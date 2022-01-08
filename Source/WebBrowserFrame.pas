(**

	This module contains a frame for the web browser so that this code can be used in more
	than one application without having to conditionally compile a form for inclusion in the
  IDE or an external application.

  @Version 1.019
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit WebBrowserFrame;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ImgList,
  ActnList,
  ComCtrls,
  OleCtrls,
  SHDocVw,
  Buttons,
  ExtCtrls,
  StdCtrls, System.ImageList, System.Actions;

{$INCLUDE CompilerDefinitions.inc}
Type
  (** This is a class to represent a frame for the web browser including an address bar
      and toolbar buttons. **)
  TfmWebBrowser = Class(TFrame)
    wbBrowser: TWebBrowser;
    sbrStatus: TStatusBar;
    alActions: TActionList;
    actBack: TAction;
    actForward: TAction;
    actConfigure: TAction;
    ilImages: TImageList;
    pnlToolbar: TPanel;
    btnBack: TSpeedButton;
    btnForward: TSpeedButton;
    cbxURL: TComboBox;
    btnStop: TSpeedButton;
    btnRefresh: TSpeedButton;
    btnConfig: TSpeedButton;
    actStop: TAction;
    actRefresh: TAction;
    actOpen: TAction;
    btnOpen: TSpeedButton;
    Procedure actBackExecute(Sender: TObject);
    Procedure actForwardExecute(Sender: TObject);
    Procedure wbBrowserCommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    Procedure wbBrowserDownloadBegin(Sender: TObject);
    procedure wbBrowserDownloadComplete(Sender: TObject);
    procedure wbBrowserProgressChange(ASender: TObject; Progress, ProgressMax: Integer);
    procedure wbBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      const URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
		procedure wbBrowserStatusTextChange(ASender: TObject; const Text: WideString);
		procedure cbxURLKeyPress(Sender: TObject; var Key: Char);
		procedure actStopExecute(Sender: TObject);
		procedure actRefreshExecute(Sender: TObject);
		procedure wbBrowserDocumentComplete(ASender: TObject; const pDisp: IDispatch;
			const URL: OleVariant);
		procedure sbrStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
			const Rect: TRect);
		procedure actConfigureExecute(Sender: TObject);
		procedure cbxURLSelect(Sender: TObject);
		procedure actOpenExecute(Sender: TObject);
		procedure wbBrowserTitleChange(ASender: TObject; const Text: WideString);
	Private
		{Private declarations}
		FPercent : Double;
	Public
		{Public declarations}
		Constructor Create(AOwner : TComponent); Override;
		Procedure Navigate(strURL: String);
		Function  CurrentURL : String;
	End;

Implementation

{$R *.dfm}

Uses
	DGHIDEHelpHelper.Functions,
	DGHIDEHelphelperConfigForm,
	ApplicationsOptions,
	ShellAPI,
	ToolsAPI;

{TfmWebBrowser}

(**

	This is an on execute event handler for the Back action.

	@precon  None.
	@postcon Asks the browser to go to the previous page in the history.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.actBackExecute(Sender: TObject);

Begin
  wbBrowser.GoBack;
End;

(**

  This is an on execute event handler for the Configure action.

  @precon  None.
  @postcon Displays the configuration dialogue and if confirmed updates the application
           options and ensures that any new Permanent URLs are in the address bar list.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.actConfigureExecute(Sender: TObject);

{$IFNDEF DXE00}
Var
  iIndex : Integer;
  i: Integer;
{$ENDIF}

Begin
  {$IFNDEF DXE00}
  iIndex := AppOptions.SearchURLIndex;
  If TfrmDGHIDEHelphelperConfig.Execute(AppOptions.SearchURLs, AppOptions.PermanentURLs,
		iIndex) Then
    Begin
      AppOptions.SearchURLIndex := iIndex;
      For i := 0 To AppOptions.PermanentURLs.Count - 1 Do
        If cbxURL.Items.IndexOf(AppOptions.PermanentURLs[i]) = -1 Then
          cbxURL.Items.Add(AppOptions.PermanentURLs[i]);
    End;
  {$ELSE}
  (BorlandIDEServices As IOTAServices).GetEnvironmentOptions.EditOptions('', 'IDE Help Helper.Options');
  {$ENDIF}
End;

(**

  This is an on execute event handler for the Forward action.

  @precon  None.
  @postcon Asks the browser to go to the next page in the history list.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.actForwardExecute(Sender: TObject);
Begin
  wbBrowser.GoForward;
End;

(**

  This is an on execute event handler for the Open action.

  @precon  None.
  @postcon Asks the OS to open the current URL in an external browser.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.actOpenExecute(Sender: TObject);

Begin
  ShellExecute(Application.Handle, 'open', PChar(cbxURL.Text), '', '', SW_SHOWNORMAL);
End;

(**

  This is an on execute event handler for the Refresh action.

  @precon  None.
  @postcon Asks the browser to refresh the current page.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.actRefreshExecute(Sender: TObject);

Begin
  wbBrowser.Refresh;
End;

(**

  This is an on execute event handler for the Stop action.

  @precon  None.
	@postcon Asks the browser to stop processing the currently loading page.

	@param   Sender as a TObject

**)
Procedure TfmWebBrowser.actStopExecute(Sender: TObject);

Begin
	wbBrowser.Stop;
End;

(**

	This is an on key press event handler for the URL combo box.

	@precon  None.
	@postcon If the enter key is pressed it forces the browser to display for the current
					 URL. If this is not a qualified URL a default search will be done.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
Procedure TfmWebBrowser.cbxURLKeyPress(Sender: TObject; Var Key: Char);

Begin
  If Key = #13 Then
    Begin
      wbBrowser.Navigate(cbxURL.Text);
      Key := #0;
		End;
End;

(**

  This is an on select item event handler for the URL combo box.

  @precon  None.
  @postcon Asks the browser to display the selected URL.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.cbxURLSelect(Sender: TObject);

Begin
  wbBrowser.Navigate(cbxURL.Text);
End;

(**

  A constructor for the TfmWebBrowser class.

  @precon  None.
  @postcon Adds the permanent URLs to the URL list.

  @param   AOwner as a TComponent

**)
Constructor TfmWebBrowser.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
  cbxURL.Items.Assign(AppOptions.PermanentURLs);
End;

(**

  This method returns the current URL of the browser.

  @precon  None.
  @postcon The current URL of the browser is returned.

  @return  a String

**)
Function TfmWebBrowser.CurrentURL: String;

Begin
  Result := wbBrowser.LocationURL;
End;

(**

	This method is the only public method for the class and is used by external code to
	invoke the browser to display a URL.

	@precon  None.
	@postcon The given URL is displayed.

	@param   strURL as a String

**)
Procedure TfmWebBrowser.Navigate(strURL: String);

Begin
	wbBrowser.Navigate(strURL);
End;

(**

	This is an owner draw method for the status bar panel and is used to draw a progress bar
	in the second panel for the loading of the page in the browser.

	@precon  None.
	@postcon The progress of the browser loading is displayed as a progress with a percentage.

  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TfmWebBrowser.sbrStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  Const Rect: TRect);

Var
  R : TRect;
  strText : String;
  iX: Integer;
  iY: Integer;

Begin
  If Panel.Index = 1 Then
    Begin
      R := Rect;
      strText := Panel.Text;
      StatusBar.Canvas.Brush.Color := clBtnFace;
      StatusBar.Canvas.FillRect(R);
      iX := R.Left + ((R.Right - R.Left) - StatusBar.Canvas.TextWidth(strText)) Div 2;
      iY := R.Top + ((R.Bottom - R.Top) - StatusBar.Canvas.TextHeight(strText)) Div 2;
      StatusBar.Canvas.TextRect(R, iX, iY, strText);
      R.Right := R.Left + Trunc(FPercent / 100.0 * Int(R.Right - R.Left));
      StatusBar.Canvas.Brush.Color := clLime;
      StatusBar.Canvas.FillRect(R);
      StatusBar.Canvas.TextRect(R, iX, iY, strText);
    End;
End;

(**

	This is an on Before Navigate event handler for the browser.

	@precon  None.
	@postcon The browser toolbar buttons are updated and the URL is added to the list of
					 URLs.

	@param   ASender         as a TObject
	@param   pDisp           as an IDispatch as a constant
	@param   URL             as an OleVariant as a constant
	@param   Flags           as an OleVariant as a constant
	@param   TargetFrameName as an OleVariant as a constant
	@param   PostData        as an OleVariant as a constant
	@param   Headers         as an OleVariant as a constant
	@param   Cancel          as a WordBool as a reference

**)
Procedure TfmWebBrowser.wbBrowserBeforeNavigate2(ASender: TObject; Const pDisp: IDispatch;
	Const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; Var Cancel: WordBool);

Begin
	cbxURL.Text := VarToStr(URL);
	If cbxURL.Items.IndexOf(cbxURL.Text) = -1 Then
		cbxURL.Items.Add(cbxURL.Text);
	actStop.Enabled := True;
	actRefresh.Enabled := False;
	actOpen.Enabled := True;
End;

(**

	This is an on change event handler for the Browser control.

	@precon  None.
	@postcon This is updates the state of the Back and Forward toolbar buttons.

  @param   ASender as a TObject
  @param   Command as an Integer
  @param   Enable  as a WordBool

**)
Procedure TfmWebBrowser.wbBrowserCommandStateChange(ASender: TObject; Command: Integer;
  Enable: WordBool);

Begin
  Case Command Of
    CSC_NAVIGATEBACK:    actBack.Enabled := Enable;
    CSC_NAVIGATEFORWARD: actForward.Enabled := Enable;
  Else
    //actStop.Enabled := wbBrowser.Busy;
    //actRefresh.Enabled := Not wbBrowser.Busy;
  End;
End;

(**

  This is an on Document complete event handler for the browser.

  @precon  None.
  @postcon The stop and refresh buttons states are changed if the document is fully
           loaded.

  @param   ASender as a TObject
  @param   pDisp   as an IDispatch as a constant
  @param   URL     as an OleVariant as a constant

**)
Procedure TfmWebBrowser.wbBrowserDocumentComplete(ASender: TObject;
  Const pDisp: IDispatch; Const URL: OleVariant);

Begin
  If wbBrowser.ReadyState = READYSTATE_COMPLETE Then
    Begin
      actStop.Enabled := False;
      actRefresh.Enabled := True;
    End;
End;

(**

	This is an on Download Begin event handler for the browser.

	@precon  None.
	@postcon Updates the status panel to say loading.

	@param   Sender as a TObject

**)
Procedure TfmWebBrowser.wbBrowserDownloadBegin(Sender: TObject);

Begin
	sbrStatus.Panels[0].Text := 'Loading...';
End;

(**

	This is an on Download Complete event handler for the browser.

	@precon  None.
  @postcon Updates the status panel to say loaded.

  @param   Sender as a TObject

**)
Procedure TfmWebBrowser.wbBrowserDownloadComplete(Sender: TObject);

Begin
  sbrStatus.Panels[0].Text := 'Loaded.';
End;

(**

  This is an on change event handler for the Browser control.

  @precon  None.
  @postcon Updates the percentage complete for the progress panel.

  @param   ASender     as a TObject
  @param   Progress    as an Integer
  @param   ProgressMax as an Integer

**)
Procedure TfmWebBrowser.wbBrowserProgressChange(ASender: TObject;
  Progress, ProgressMax: Integer);

Begin
  If ProgressMax > 0 Then
    FPercent := Int(Progress) / Int(ProgressMax) * 100.0
  Else
    FPercent := 100;
  sbrStatus.Panels[1].Text := Format('%1.1n%%', [FPercent]);
  Application.ProcessMessages;
End;

(**

  This is an on change event handler for the status of the browser control.

  @precon  None.
  @postcon Updates the third panel on the status bar with the browser information.

  @param   ASender as a TObject
  @param   Text    as a WideString as a constant

**)
Procedure TfmWebBrowser.wbBrowserStatusTextChange(ASender: TObject;
  Const Text: WideString);

Begin
  sbrStatus.Panels[2].Text := Text;
End;

(**

  This is an on change event handler for the browser title control.

  @precon  None.
  @postcon Updates the host forms caption with the name of the page.

  @param   ASender as a TObject
  @param   Text    as a WideString as a constant

**)
Procedure TfmWebBrowser.wbBrowserTitleChange(ASender: TObject; Const Text: WideString);

Begin
  If Parent Is TForm Then
    (Parent As TForm).Caption := 'IDE Help Helper Browser: ' + Text;
End;

End.
