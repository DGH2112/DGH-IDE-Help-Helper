(**
  
  This module contains Open Tools API code to create an editor view to display the Help.

  @Author  David Hoyle
  @Version 2.087
  @Date    15 Jan 2022

**)
Unit DGHIDEHelpHelper.HelpEditorView;

Interface

Uses
  ToolsAPI,
  DesignIntf,
  Vcl.Forms,
  Vcl.ComCtrls,
  WinAPI.Windows;
   
{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to implement an editor view for displaying module metrics. @nometrics MissingCONSTInParam **)
  TIHHHelpEditorView = Class(TInterfacedObject, IInterface, INTACustomEditorView,
    INTACustomEditorView150 {: @todo , INTACustomEditorViewStatusPanel})
  Strict Private
    Class Var
      (** A single class var reference to the editor view. **)
      FEditorViewRef : INTACustomEditorView;
  Strict Private
    FImageIndex       : Integer;
    FViewIdent        : String;
  Strict Protected
    // INTACustomEditorView
    Function  CloneEditorView: INTACustomEditorView;
    Procedure CloseAllCalled(Var ShouldClose: Boolean);
    Procedure DeselectView;
    Function  EditAction(Action: TEditAction): Boolean;
    Procedure FrameCreated(AFrame: TCustomFrame);
    Function  GetCanCloneView: Boolean;
    Function  GetCaption: String;
    Function  GetEditState: TEditState;
    Function  GetEditorWindowCaption: String;
    Function  GetFrameClass: TCustomFrameClass;
    Function  GetViewIdentifier: String;
    Procedure SelectView;
    // INTACustomEditorView150
    Procedure Close(Var Allowed: Boolean);
    Function  GetImageIndex: Integer;
    Function  GetTabHintText: String;
    // INTACustomEditorViewStatusPanel
    Procedure ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);
    Procedure DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);
    Function  GetStatusPanelCount: Integer;
    // General Methods
    {: @debug Procedure ParseAndRender(Const strFileName : String);
    Procedure UpdateStatusPanels;
    Procedure ExtractSourceFromModule(Const Module : IOTAModule);
    Procedure ExtractSourceFromFile(Const strFileName: String);
    Procedure LastModifiedDateFromModule(Const Module: IOTAModule);
    Procedure LastModifiedDateFromFile(Const strFileName: String);
    Function  CurrentEditWindow : String;
    Procedure ProcessModule(Const strFileName: String);
    Procedure RenderedList : TBADIModuleMetrics;
    Function  CheckSettings: Boolean;
    Procedure UpdateSettings; }
  Public
    Class Function CreateEditorView: INTACustomEditorView;
    Constructor Create(Const strViewIdentifier : String);
    Destructor Destroy; Override;
  End;

  Procedure RegisterIDEHelpEditorView;
  Procedure UnregisterIDEHelpEditorView;
  
Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils, 
  Controls, 
  Vcl.Graphics,
  DGHIDEHelpHelper.WebBrowserFrame;

Const
  (** A unique name for the editor view. **)
  strBADIMetricsEditorView = 'BADIMetricsEditorView';
  (** A caption for the editor view. **)
  strBADIMetrics = 'BADI Metrics';
  (** A default edit window name if one cannot be determined. **)
  strUnknown = 'Unknown';

(**

  This method returns an instance of the custom editor view and passed in the registration of the view
  so that a view can be created when a desktop is loaded.

  @precon  None.
  @postcon An instance of this custom editor view is returned.

  @return  an INTACustomEditorView

**)
Function RecreateBADIStatisticEditorView: INTACustomEditorView;

Begin
  CodeSite.TraceMethod('RecreateBADIStatisticEditorView', tmoTiming);
  Result := TIHHHelpEditorView.CreateEditorView;
End;

(**

  This method is called from the main wizard constructor to register this custom editor view.

  @precon  None.
  @postcon The custom editor view is registered with the IDE.

**)
Procedure RegisterIDEHelpEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  CodeSite.TraceMethod('RegisterMetricsEditorView', tmoTiming);
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.RegisterEditorView(strBADIMetricsEditorView, RecreateBADIStatisticEditorView);
End;

(**

  This method is called from the main wizard destructor to un-register this custom editor view.

  @precon  None.
  @postcon The custom editor view is unregistered from the IDE.

**)
Procedure UnregisterIDEHelpEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  CodeSite.TraceMethod('UnregisterMetricsEditorView', tmoTiming);
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorView(strBADIMetricsEditorView);
End;

(**

  This method checks whether any of the settings that affect spelling has changed.

  @precon  None.
  @postcon Returns true if any of the options have changed and the list needs to be completely rebuilt.

  @return  a Boolean

**)
{: @debug Function TIHHHelpEditorView.CheckSettings: Boolean;

Var
  Exclusions: IBADIExclusions;
  i: Integer;
  sl : TStringList;

Begin
  Result := FLastRenderedList <> RenderedList;
  Result := Result Or (FLastDocOptions <> TBADIOptions.BADIOptions.Options * setMetricsOptions);
  sl := TStringList.Create;
  Try
    sl.Duplicates := dupIgnore;
    sl.Sorted := True;
    Exclusions := TBADIOptions.BADIOptions.Exclusions;
    For i := 0 To Exclusions.Count - 1 Do
      If etSpelling In Exclusions[i].FExclusions Then
        sl.Add(Exclusions[i].FExclusionPattern);
    Result := Result Or (CompareText(sl.Text, FDocExclusions.Text) <> 0);
  Finally
    sl.Free;
  End;
End;
 }
(**

  This method is called when the IDE wants to clone the view and if the GetCanClose method returns true.

  @precon  None.
  @postcon You should return a cloned instance of your view if requested. I have not been able to get the
           IDE to ever call this method.

  @return  an INTACustomEditorView

**)
Function TIHHHelpEditorView.CloneEditorView: INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  CodeSite.TraceMethod(Self, 'CloneEditorView', tmoTiming);
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.CloseActiveEditorView;
  Result := RecreateBADIStatisticEditorView;
End;

(**

  This method is called when this view tab in the editor is being requested to close. Return true to
  allow if the close else return false for it to persist.

  @precon  None.
  @postcon I return true here so it can be closed.

  @param   Allowed as a Boolean as a reference

**)
Procedure TIHHHelpEditorView.Close(Var Allowed: Boolean);

Begin
  CodeSite.TraceMethod(Self, 'Close', tmoTiming);
  Allowed := True;
End;

(**

  This method is called when all the views in the editor are being requested to close. Return true to 
  allow if the close else return false for it to persist.

  @precon  None.
  @postcon I return true here so it can be closed.

  @param   ShouldClose as a Boolean as a reference

**)
Procedure TIHHHelpEditorView.CloseAllCalled(Var ShouldClose: Boolean);

Begin
  CodeSite.TraceMethod(Self, 'CloseAllCalled', tmoTiming);
  ShouldClose := True;
End;

(**

  This method is called when each editor status panel is created.

  @precon  None.
  @postcon References to the panels are stored for later use and each panel is configured.

  @nocheck MissingCONSTInParam
  @nohint  StatusBar

  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel

**)
Procedure TIHHHelpEditorView.ConfigurePanel(StatusBar: TStatusBar; Panel: TStatusPanel);

//Const
//  iPanelWidth = 100;

Begin
  CodeSite.TraceMethod(Self, 'ConfigurePanel', tmoTiming);
//  FModulePanels[TBADIMetricStatusPanel(Panel.Index)] := Panel;
//  FModulePanels[TBADIMetricStatusPanel(Panel.Index)].Alignment := taCenter;
//  FModulePanels[TBADIMetricStatusPanel(Panel.Index)].Width := iPanelWidth;
//  //: @note Problems with first panel if you do not explicitly set this
//  FModulePanels[TBADIMetricStatusPanel(Panel.Index)].Style := psOwnerDraw; // psText; 
End;

(**

  A constructor for the TBADIModuleMetricsEditorView class.

  @precon  None.
  @postcon Adds an image to the editor image list to be displayed against this editor view.

  @param   strViewIdentifier as a String as a constant

**)
Constructor TIHHHelpEditorView.Create(Const strViewIdentifier : String);

Const
  strBADIMetricsImage = 'BADIMetricsImage';

Var
  EVS : INTAEditorViewServices;
  ImageList : TImageList;
  BM: TBitmap;
  
Begin
  CodeSite.TraceMethod(Self, 'Create', tmoTiming);
  Inherited Create;
  FViewIdent := strViewIdentifier;
  If Supports(BorlandIDEServices, INTAEditorViewServices, EVS) Then
    Begin
      ImageList := TImageList.Create(Nil);
      Try
        BM := TBitMap.Create;
        Try
          {: @debug BM.LoadFromResourceName(HInstance, strBADIMetricsImage);
          ImageList.AddMasked(BM, clLime);
          FImageIndex := EVS.AddImages(ImageList, strBADIMetricsEditorView); }
        Finally
          BM.Free;
        End;
      Finally
        ImageList.Free;
      End;
    End;
End;

(**

  This is a class method to create a singleton instance of this editor view.

  @precon  None.
  @postcon Create the editor view is it does not already exist else returned the existing instance 
           reference.

  @return  an INTACustomEditorView

**)
Class Function TIHHHelpEditorView.CreateEditorView : INTACustomEditorView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  CodeSite.TraceMethod('TIHHHelpEditorView.CreateEditorView', tmoTiming);
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    Begin
      If Not Assigned(FEditorViewRef) Then
        FEditorViewRef := TIHHHelpEditorView.Create('');
      Result := FEditorViewRef;
      EVS.ShowEditorView(Result);
    End;
End;

(**

  This method returns the name of the current top editor window.

  @precon  None.
  @postcon The name for the top editor window is returned.

  @return  a String

**)
{: @debug Function TIHHHelpEditorView.CurrentEditWindow: String;

Var
  ES : INTAEditorServices;
  
Begin
  Result := strUnknown;
  If Supports(BorlandIDEServices, INTAEditorServices, ES) Then
    Result := ES.TopEditWindow.Form.Name;
End; }

(**

  This method is called when the editor view loses focus.

  @nocheck EmptyMethod
  
  @precon  None.
  @postcon Does nothing.

**)
Procedure TIHHHelpEditorView.DeselectView;

Begin
  CodeSite.TraceMethod(Self, 'DeselectView', tmoTiming);
  // Does nothing
End;

(**

  A destructor for the TBADIModuleMetricsEditorView class.

  @precon  None.
  @postcon Frees the memory used by the module (if not nil).

**)
Destructor TIHHHelpEditorView.Destroy;

Begin
  CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);
  Inherited Destroy;
End;

(**

  This method is called for each status panel if it is set to owner draw.

  @precon  None.
  @postcon Draw each panel with a blue number and black bold text.

  @nocheck MissingCONSTInParam
  
  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TIHHHelpEditorView.DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; Const Rect: TRect);

  (**

    This method renders the background of the status bar panel.

    @precon  None.
    @postcon The background of the status panel is rendered.

    @param   strNum        as a String as a constant

  **)
  Procedure DrawBackground(Const strNum : String);

  Var
    iColour : TColor;

  Begin
    CodeSite.TraceMethod(Self, 'DrawPanel/DrawBackground', tmoTiming);
    {: @debug Case TBADIMetricStatusPanel(Panel.Index) Of
      mspModules..mspLinesOfCode:  iColour := iLightYellow;
      mspUnderLimit..mspOverLimit: iColour := iLightGreen;
    Else
      iColour := StatusBar.Color;
    End;
    If strNum <> '' Then
      Case TBADIMetricStatusPanel(Panel.Index) Of
        mspAtLimit:
          If StrToInt(strNum) > 0 Then
            iColour := iLightAmber;
        mspOverLimit:
          If StrToInt(strNum) > 0 Then
            iColour := iLightRed;
      End;
    StatusBar.Canvas.Brush.Color := iColour;
    StatusBar.Canvas.FillRect(Rect); }
  End;

  (**

    This width of the text in the status panel is calculated.

    @precon  None.
    @postcon The width of the text in the status panel is returned taking into account the font styles.

    @param   strNum   as a String as a constant
    @param   strSpace as a String as a constant
    @param   strText  as a String as a constant
    @return  an Integer

  **)
  Function CalcWidth(Const strNum, strSpace, strText : String) : Integer;

  Begin  
    CodeSite.TraceMethod(Self, 'DrawPanel/CalcWidth', tmoTiming);
    StatusBar.Canvas.Font.Style := [];
    Result := StatusBar.Canvas.TextWidth(strNum);
    Inc(Result, StatusBar.Canvas.TextWidth(strSpace));
    StatusBar.Canvas.Font.Style := [fsBold];
    Inc(Result, StatusBar.Canvas.TextWidth(strText));
  End;
  
  (**

    This method renders the text on the status panel.

    @precon  None.
    @postcon The text of the status bar is rendered.

    @param   strNum        as a String as a reference
    @param   strSpace      as a String as a reference
    @param   strText       as a String as a reference
    @param   iWidth        as an Integer as a constant

  **)
  Procedure DrawText(Var strNum, strSpace, strText : String; Const iWidth : Integer);

  Const
    iDivisor = 2;
    iTopPadding = 4;

  Var
    R : TRect;
    
  Begin
    CodeSite.TraceMethod(Self, 'DrawPanel/DrawText', tmoTiming);
    R := Rect;
    // Draw Number
    Inc(R.Left, (R.Right - R.Left - iWidth) Div iDivisor);
    Inc(R.Top, iTopPadding);
    StatusBar.Canvas.Font.Assign(StatusBar.Font);
    StatusBar.Canvas.Font.Color := clBlue;
    StatusBar.Canvas.Font.Style := [];
    StatusBar.Canvas.TextRect(R, strNum, [tfLeft,tfBottom]);
    // Draw Space
    Inc(R.Left, StatusBar.Canvas.TextWidth(strNum));
    StatusBar.Canvas.TextRect(R, strSpace, [tfLeft,tfBottom]);
    // Draw Text Label
    StatusBar.Canvas.Font.Color := clWindowText;
    StatusBar.Canvas.Font.Style := [fsBold];
    Inc(R.Left, StatusBar.Canvas.TextWidth(strSpace));
    StatusBar.Canvas.TextRect(R, strText, [tfLeft,tfBottom]);
  End;

Var
  strNum, strSpace, strText : String;
  iPos : Integer;
  
Begin
  CodeSite.TraceMethod(Self, 'DrawPanel', tmoTiming);
  // Split text by first space
  iPos := Pos(#32, Panel.Text);
  strNum := Copy(Panel.Text, 1, Pred(iPos));
  strSpace := #32;
  strText := Copy(Panel.Text, Succ(iPos), Length(Panel.Text) - iPos);
  DrawBackground(strNum);
  DrawText(strNum, strSpace, strText, CalcWidth(strNum, strSpace, strText));
End;

(**

  This method is called for the given editor action that you have said is supported by the editor view.

  @precon  None.
  @postcon The treeview text is copied to the clipboard if that action is invoked.

  @nocheck MissingCONSTInParam
  
  @param   Action as a TEditAction
  @return  a Boolean

**)
Function TIHHHelpEditorView.EditAction(Action: TEditAction): Boolean;

Var
  AFrame: Tframe;

Begin
  CodeSite.TraceMethod(Self, 'EditAction', tmoTiming);
  Result := False;
  {: @debug Case Action Of
    eaCopy:
      Begin
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) And (AFrame Is TframeBADIModuleMetricsEditorView) Then
          (AFrame As TframeBADIModuleMetricsEditorView).CopyToClipboard;
        Result := True;
      End;
  End; }
End;

(**

  This method extracts the source code, filename and date information from a disk file.

  @precon  ModuleInfo must be a valid instance.
  @postcon The source code, filename and date information is retrieved from the disk file.

  @param   strFileName as a String as a constant

**)
{: @debug Procedure TIHHHelpEditorView.ExtractSourceFromFile(Const strFileName: String);

Begin
  FSource := '';
  If FileExists(strFileName) Then
    Begin
      FSourceStrings.LoadFromFile(strFileName);
      FSource := FSourceStrings.Text;
      FSourceStrings.Clear;
    End;
End; }

(**

  This method extracts the source code, filename and date information from an in memory module.

  @precon  Module and ModuleInfo must be a valid instance.
  @postcon The source code, filename and date information is retrieved from the in memory module.

  @param   Module as an IOTAModule as a constant

**)
{: @debug Procedure TIHHHelpEditorView.ExtractSourceFromModule(Const Module : IOTAModule);

Var
  SE: IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  FSource := TBADIToolsAPIFunctions.EditorAsString(SE);
End; }

(**

  This method is called when the frame is first created.

  @precon  None.
  @postcon Stores a reference to the frame so that a modules metrics can be rendered

  @nocheck MissingCONSTInParam
  
  @param   AFrame as a TCustomFrame

**)
Procedure TIHHHelpEditorView.FrameCreated(AFrame: TCustomFrame);

Const
  strTEditWindow = 'TEditWindow';

Var
  ES : INTAEditorServices;
  C : TWinControl;
  strEditWindowName : String;

Begin
CodeSite.TraceMethod(Self, 'FrameCreated', tmoTiming);
  If Supports(BorlandIDEServices, INTAEditorServices, ES) Then
    Begin
      strEditWindowName := strUnknown;
      C := AFrame;
      While Assigned(C) Do
        Begin
          If C.ClassName = strTEditWindow Then
            Begin
              strEditWindowName := C.Name;
              Break;
            End;
          C := C.Parent;
        End;
      {: @debug FFrameManager.Add(strEditWindowName, AFrame As TframeBADIModuleMetricsEditorView); }
    End;
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Returns false as this editor view should not be cloned (think singleton view).

  @return  a Boolean

**)
Function TIHHHelpEditorView.GetCanCloneView: Boolean;

Begin
  CodeSite.TraceMethod(Self, 'GetCanCloneView', tmoTiming);
  Result := False;
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon The method returns the caption  for the editor view. It is also used as the editor sub view
           tab description.

  @return  a String

**)
Function TIHHHelpEditorView.GetCaption: String;

Begin
  CodeSite.TraceMethod(Self, 'GetCaption', tmoTiming);
  Result := 'IDE Help Helper';
End;

(**

  This is a getter method for the Editor Window Caption property.

  @precon  None.
  @postcon Returns the text to be displayed in the Editor Window (you can only see this when the editor
           is floating).

  @return  a String

**)
Function TIHHHelpEditorView.GetEditorWindowCaption: String;

Begin
  CodeSite.TraceMethod(Self, 'GetEditorWindowCaption', tmoTiming);
  Result := strBADIMetrics;
End;

(**

  This is a getter method for the Edit State property.

  @precon  None.
  @postcon This method is called to tell the IDE what editor state can be invoked on the data in the
           view (cut, copy, paste, etc).

  @return  a TEditState

**)
Function TIHHHelpEditorView.GetEditState: TEditState;

Begin
  CodeSite.TraceMethod(Self, 'GetEditState', tmoTiming);
  Result := [esCanCopy];
End;

(**

  This is a getter method for the Frame Class property.

  @precon  None.
  @postcon The method returns the frame class that the IDE should create when creating the editor view.

  @return  a TCustomFrameClass

**)
Function TIHHHelpEditorView.GetFrameClass: TCustomFrameClass;

Begin
  CodeSite.TraceMethod(Self, 'GetFrameClass', tmoTiming);
  Result := TfmWebBrowser;
End;

(**

  This is a getter method for the Image Index property.

  @precon  None.
  @postcon Returns the image index of the image in the editor image list for this editor view.

  @return  an Integer

**)
Function TIHHHelpEditorView.GetImageIndex: Integer;

Begin
  CodeSite.TraceMethod(Self, 'GetImageIndex', tmoTiming);
  Result := FImageIndex;
End;

(**

  This is a getter method for the Status Panel Count property.

  @precon  None.
  @postcon Returns the number of status panels to create for the editor view.

  @return  an Integer

**)
Function TIHHHelpEditorView.GetStatusPanelCount: Integer;

Begin
  CodeSite.TraceMethod(Self, 'GetStatusPanelCount', tmoTiming);
  Result := 0; {: @debug Ord(High(TBADIMetricStatusPanel)) - Ord(Low(TBADIMetricStatusPanel)) + 1; }
End;

(**

  This is a getter method for the Tab Hint Text property.

  @precon  None.
  @postcon Returns the text to be displayed when the mouse is hovered over the editor tab.

  @return  a String

**)
Function TIHHHelpEditorView.GetTabHintText: String;

Begin
  CodeSite.TraceMethod(Self, 'GetTabHintText', tmoTiming);
  Result := strBADIMetrics;
End;

(**

  This is a getter method for the View Identifier property.

  @precon  None.
  @postcon Returns a unique identifier for this view (must be unique within the IDE - think singleton
           instance).

  @return  a String

**)
Function TIHHHelpEditorView.GetViewIdentifier: String;

Begin
  CodeSite.TraceMethod(Self, 'GetViewIdentifier', tmoTiming);
  Result := Format('%s.%s', [strBADIMetricsEditorView, FViewIdent]);
End;

(**

  This method retrieves the last modified date of the module from disk.

  @precon  ModuleInfo must be a valid instance.
  @postcon The FFileDate and FModified fields are updated.

  @param   strFileName as a String as a constant

**)
{: @debug Procedure TIHHHelpEditorView.LastModifiedDateFromFile(Const strFileName: String);

Begin
  FileAge(strFileName, FFileDate);
  FModified := False;
End; }

(**

  This method retrieves the last modified date of the module from the IDE.

  @precon  Module must be a valid instance.
  @postcon The FFileDate and FModified fields are updated.

  @param   Module as an IOTAModule as a constant

**)
{: @debug Procedure TIHHHelpEditorView.LastModifiedDateFromModule(Const Module: IOTAModule);

Var
  SE : IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  FModified := SE.Modified;
  If Not FModified Then
    FileAge(MOdule.FileName, FFileDate)
  Else
    FFileDate := Now();
End; }

(**

  This method parses the source code and renders the module in the metrics frame.

  @precon  None.
  @postcon The source code is parsed and rendered.

  @param   strFileName as a String as a constant

**)
{: @debug Procedure TIHHHelpEditorView.ParseAndRender(Const strFileName : String);

Var
  Module : TBaseLanguageModule;
  AFrame: Tframe;

Begin
  FFileInfoMgr.Add(strFileName, FFileDate);
  If (Length(FSource) > 0) And (Length(strFileName) > 0) Then
    Begin
      Module := TBADIDispatcher.BADIDispatcher.Dispatcher(FSource, strFileName, FModified, [moParse]);
      Try
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) And (AFrame Is TframeBADIModuleMetricsEditorView) Then
          (AFrame As TframeBADIModuleMetricsEditorView).RenderModule(
            Module,
            [roAutoExpand, roAutoExpandOnError]
          );
      Finally
        Module.Free;
      End;
    End;
End; }

(**

  This method process the module extracting the filename, date time and source code and the pass it for 
  parsing.

  @precon  ModuleInfo must be a valid instance.
  @postcon Process the module extracting the filename, date time and source code and the pass it for 
           parsing.

  @param   strFileName as a String as a constant

**)
{: @debug Procedure TIHHHelpEditorView.ProcessModule(Const strFileName : String);

Var
  Module: IOTAModule;

Begin
  FModified := False;
  Module := (BorlandIDEServices As IOTAModuleServices).FindModule(strFileName);
  If Assigned(Module) Then
    LastModifiedDateFromModule(Module)
  Else
    LastModifiedDateFromFile(strFileName);
  If FFileInfoMgr.ShouldUpdate(strFileName, FFileDate) Then
    Begin
      If Assigned(Module) Then
        ExtractSourceFromModule(Module)
      Else
        ExtractSourceFromFile(strFileName);
        ParseAndRender(strFileName);
    End; 
End; }

(**

  This method is called when the editor view is selected either when its created or when it regains
  focus.

  @precon  None.
  @postcon Renders the modules metrics in the frame.

**)
Procedure TIHHHelpEditorView.SelectView;

ResourceString
  strParsingProjectModules = 'Parsing project modules';
  strPleaseWait = 'Please wait...';
  strParsing = 'Parsing: %s...';

Const
  setModuleTypesToParse = [omtForm, omtDataModule, omtProjUnit, omtUnit];
  
{: @debug Var
  P: IOTAProject;
  iModule: Integer;
  frmProgress : TfrmProgress;
  ModuleInfo: IOTAModuleInfo;
  AFrame: Tframe; }

Begin
  CodeSite.TraceMethod(Self, 'SelectView', tmoTiming);
  {: @debug P := TBADIToolsAPIFunctions.ActiveProject;
  If Assigned(P) Then
    Begin
      If CheckSettings Then
        Begin
          FFileInfoMgr.Clear;
          UpdateSettings;
        End;
      frmProgress := TfrmProgress.Create(Application.MainForm);
      Try
        frmProgress.Init(P.GetModuleCount, strParsingProjectModules, strPleaseWait);
        For iModule := 0 To P.GetModuleCount - 1 Do
          Begin
            ModuleInfo := P.GetModule(iModule);
            If ModuleInfo.ModuleType In setModuleTypesToParse Then
              If ModuleInfo.FileName.Length > 0 Then
                Begin
                  ProcessModule(ModuleInfo.FileName);

                  frmProgress.UpdateProgress(Succ(iModule), Format(strParsing,
                    [ExtractFileName(ModuleInfo.FileName)]));
                End
          End;
        AFrame := FFrameManager.Frame[CurrentEditWindow];
        If Assigned(AFrame) And (AFrame Is TframeBADIModuleMetricsEditorView) Then
          (AFrame As TframeBADIModuleMetricsEditorView).FocusResults;
      Finally
        frmProgress.Free;
      End;
      UpdateStatusPanels;
    End; }
End; 

(**

  This method updates the internal copy of the settings that are used for rendering the editor view.

  @precon  None.
  @postcon The internal copy of the settings is updated so later renderings can check whether the
           settings have changed.

**)
{: @debug Procedure TIHHHelpEditorView.UpdateSettings;

Var
  Exclusions: IBADIExclusions;
  i: Integer;

Begin
  FLastRenderedList := RenderedList;
  FLastDocOptions := TBADIOptions.BADIOptions.Options * setMetricsOptions;
  // Build a string list of spelling exclusions
  Exclusions := TBADIOptions.BADIOptions.Exclusions;
  For i := 0 To Exclusions.Count - 1 Do
    If etSpelling In Exclusions[i].FExclusions Then
      FDocExclusions.Add(Exclusions[i].FExclusionPattern); 
End; }

(**

  This method updates the status panels with the information from the frame.

  @precon  None.
  @postcon The status panels are updated.

**)
{: @debug Procedure TIHHHelpEditorView.UpdateStatusPanels;

ResourceString
  strModules = '%d Modules';
  strMethods = '%d Methods';
  strLinesOfCode = '%d Lines';
  strUnderLimit = '%d < Limit';
  strAtLimit = '%d @ Limit';
  strOverLimit = '%d > Limit';

Var
  AFrame: Tframe;
  F: TframeBADIModuleMetricsEditorView;
  
Begin
  AFrame := FFrameManager.Frame[CurrentEditWindow];
  If Assigned(AFrame) And (AFrame Is TframeBADIModuleMetricsEditorView) Then
    Begin
      F := (AFrame As TframeBADIModuleMetricsEditorView);
      FModulePanels[mspModules].Text :=     Format(strModules,     [F.ModuleCount]);
      FModulePanels[mspMethods].Text :=     Format(strMethods,     [F.MethodCount]);
      FModulePanels[mspLinesOfCode].Text := Format(strLinesOfCode, [F.LinesOfCode]);
      FModulePanels[mspUnderLimit].Text :=  Format(strUnderLimit,  [F.UnderLimit]);
      FModulePanels[mspAtLimit].Text :=     Format(strAtLimit,     [F.AtLimit]);
      FModulePanels[mspOverLimit].Text :=   Format(strOverLimit,   [F.OverLimit]);
    End;
End; }

End.

