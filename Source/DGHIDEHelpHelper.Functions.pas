(**

  This module contains often used code for use through out this application.

  @Version 1.023
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit DGHIDEHelpHelper.Functions;

Interface

Uses
  ToolsAPI,
  Graphics,
  Windows;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class defined a custom message for the IDE. **)
  TCustomMessage = Class(TInterfacedObject, IOTACustomMessage, INTACustomDrawMessage)
  Private
    FMsg: String;
    FFontName: String;
    FForeColour: TColor;
    FStyle: TFontStyles;
    FBackColour: TColor;
    FMessagePntr: Pointer;
  Protected
    Procedure SetForeColour(iColour: TColor);
  Public
    Constructor Create(strMsg: String; FontName: String;
      ForeColour: TColor = clBlack; Style: TFontStyles = [];
      BackColour: TColor = clWindow);
    (**
      This allows the colour of the font to be updated.
      @precon  None.
      @postcon Updates the font colour of the message.
      @return  a TColor
    **)
    Property ForeColour: TColor Write SetForeColour;
    (**
      This property returns the message pointer to be used as the parent for
      sub messages.
      @precon  None.
      @postcon Returns the message pointer to be used as the parent for
      @return  a Pointer
    **)
    Property MessagePntr: Pointer Read FMessagePntr Write FMessagePntr;
    // IOTACustomMessage
    Function GetColumnNumber: Integer;
    Function GetFileName: String;
    Function GetLineNumber: Integer;
    Function GetLineText: String;
    Procedure ShowHelp;
    // INTACustomDrawMessage
    Function CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect;
    Procedure Draw(Canvas: TCanvas; Const Rect: TRect; Wrap: Boolean);
  End;

	(** An enumerate to define which message should be cleared from the IDE
      message window. **)
	TClearMessage = (cmCompiler, cmSearch, cmTool);
  (** A set of the above message types. **)
  TClearMessages = Set of TClearMessage;

  (** A record to hold version information. **)
  TVersionInfo = Record
    iMajor  : Integer;
    iMinor  : Integer;
    iBugfix : Integer;
    iBuild  : Integer;
  End;

  Procedure BuildNumber(var VersionInfo : TVersionInfo);
  Procedure OutputMessage(strText : String); Overload;
  Procedure OutputMessage(strFileName, strText, strPrefix : String; iLine,
    iCol : Integer); Overload;
  {$IFDEF D0006}
  Procedure OutputMessage(strText : String; strGroupName : String); Overload;
  {$ENDIF}
  Procedure ClearMessages(Msg : TClearMessages);
  {$IFDEF D0006}
  Procedure ShowMessages(strGroupName : String = '');
  {$ENDIF}
  Function ProjectGroup: IOTAProjectGroup;
  Function ActiveProject : IOTAProject;
  Function ProjectModule(Project : IOTAProject) : IOTAModule;
  Function ActiveSourceEditor : IOTASourceEditor;
  Function SourceEditor(Module : IOTAMOdule) : IOTASourceEditor;
  Function EditorAsString(SourceEditor : IOTASourceEditor) : String;
  Function AddMsg(strText: String; boolGroup, boolAutoScroll: Boolean;
    strFontName: String; iForeColour: TColor; fsStyle: TFontStyles;
    iBackColour: TColor = clWindow; ptrParent: Pointer = Nil): Pointer;
  Procedure OutputText(Writer : IOTAEditWriter; iIndent : Integer; strText : String);

Implementation

Uses
  SysUtils;

(**

	This method extracts the Major, Minor, Bug Fix and Build version numbers from
  this modules resource information.

  @precon  None.
  @postcon Returns the version information in the var parameter.

  @param   VersionInfo as a TVersionInfo as a reference

**)
Procedure BuildNumber(Var VersionInfo: TVersionInfo);

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  strBuffer: Array [0 .. MAX_PATH] Of Char;

Begin
  GetModuleFileName(hInstance, strBuffer, MAX_PATH);
  VerInfoSize := GetFileVersionInfoSize(strBuffer, Dummy);
  If VerInfoSize <> 0 Then
		Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(strBuffer, 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        With VerValue^ Do
          Begin
            VersionInfo.iMajor := dwFileVersionMS Shr 16;
            VersionInfo.iMinor := dwFileVersionMS And $FFFF;
            VersionInfo.iBugfix := dwFileVersionLS Shr 16;
            VersionInfo.iBuild := dwFileVersionLS And $FFFF;
          End;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;
End;

(**

  This method outputs the given message to the IDEs message window.

  @precon  None.
  @postcon Outputs the given message to the IDEs message window.

  @param   strText as a String

**)
Procedure OutputMessage(strText : String);

Begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(strText);
End;

(**

  This method outputs the given message with file and cursor position to the
  IDEs message window so that if the message is double clicked then the position
  in the file will be displayed by the IDE.

  @precon  None.
  @postcon Displays a tool message in the IDE.

  @param   strFileName as a String
  @param   strText     as a String
  @param   strPrefix   as a String
  @param   iLine       as an Integer
  @param   iCol        as an Integer

**)
Procedure OutputMessage(strFileName, strText, strPrefix : String; iLine, iCol : Integer);

Begin
  (BorlandIDEServices As IOTAMessageServices).AddToolMessage(strFileName,
    strText, strPrefix, iLine, iCol);
End;

{$IFDEF D0006}
(**

  This method outputs the given message to the named message group.

  @precon  None.
  @postcon Outputs the given message to the named message group.

  @param   strText      as a String
  @param   strGroupName as a String

**)
Procedure OutputMessage(strText : String; strGroupName : String);

Var
  Group : IOTAMessageGroup;

Begin
  With (BorlandIDEServices As IOTAMessageServices) Do
    Begin
      Group := GetGroup(strGroupName);
      If Group = Nil Then
        Group := AddMessageGroup(strGroupName);
      AddTitleMessage(strText, Group);
    End;
End;
{$ENDIF}

(**

  This method clears the IDE message window of the given message types.

  @precon  None.
  @postcon Clears the IDE message window of the given message types.

  @param   Msg as a TClearMessages

**)
Procedure ClearMessages(Msg : TClearMessages);

Begin
  If cmCompiler In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearCompilerMessages;
  If cmSearch In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearSearchMessages;
  If cmTool In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearToolMessages;
End;

{$IFDEF D0006}
(**

  This method displays the named message group in the IDE. If no group is
  provided then the main message window is displayed.

  @precon  None.
  @postcon Displays the named message group in the IDE.

  @param   strGroupName as a String

**)
Procedure ShowMessages(strGroupName : String = '');

Var
  G : IOTAMessageGroup;

Begin
  With (BorlandIDEServices As IOTAMessageServices) Do
    Begin
      G := GetGroup(strGroupName);
      ShowMessageView(G);
		End;
End;
{$ENDIF}

(**

  This method returns the current project group reference or nil if there is no
  project group open.

  @precon  None.
  @postcon Returns the current project group reference or nil if there is no
           project group open.

  @return  an IOTAProjectGroup

**)
Function ProjectGroup: IOTAProjectGroup;

Var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProjectGroup: IOTAProjectGroup;

Begin
  Result := Nil;
  AModuleServices := (BorlandIDEServices as IOTAModuleServices);
  For i := 0 To AModuleServices.ModuleCount - 1 Do
    Begin
      AModule := AModuleServices.Modules[i];
      If (AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK) Then
       Break;
    End;
  Result := AProjectGroup;
end;

(**

  This method returns the active project in the IDE else returns Nil if there is
  no active project.

  @precon  None.
  @postcon Returns the active project in the IDE else returns Nil if there is
           no active project.

  @return  an IOTAProject

**)
Function ActiveProject : IOTAProject;

var
  G : IOTAProjectGroup;

Begin
  Result := Nil;
  G := ProjectGroup;
  If G <> Nil Then
    Result := G.ActiveProject;
End;

(**

  This method returns the project module for the given project.

	@precon  Project must be a valid instance.
  @postcon Returns the project module for the given project.

  @param   Project as an IOTAProject
  @return  an IOTAModule

**)
Function ProjectModule(Project : IOTAProject) : IOTAModule;

Var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProject: IOTAProject;

Begin
  Result := Nil;
  AModuleServices := (BorlandIDEServices as IOTAModuleServices);
  For i := 0 To AModuleServices.ModuleCount - 1 Do
    Begin
      AModule := AModuleServices.Modules[i];
      If (AModule.QueryInterface(IOTAProject, AProject) = S_OK) And
        (Project = AProject) Then
        Break;
    End;
  Result := AProject;
End;

(**

  This method returns the Source Editor interface for the active source editor
  else returns nil.

  @precon  None.
  @postcon Returns the Source Editor interface for the active source editor
           else returns nil.

  @return  an IOTASourceEditor

**)
Function ActiveSourceEditor : IOTASourceEditor;

Var
  CM : IOTAModule;

Begin
  Result := Nil;
  If BorlandIDEServices = Nil Then
    Exit;
  CM := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Result := SourceEditor(CM);
End;

(**

  This method returns the source editor for the given module.

  @precon  Module must be a valid instance.
  @postcon Returns the source editor for the given module.

  @param   Module as an IOTAMOdule
  @return  an IOTASourceEditor

**)
Function SourceEditor(Module : IOTAMOdule) : IOTASourceEditor;

Var
  iFileCount : Integer;
  i : Integer;

Begin
  Result := Nil;
  If Module = Nil Then Exit;
  With Module Do
    Begin
      iFileCount := GetModuleFileCount;
      For i := 0 To iFileCount - 1 Do
        If GetModuleFileEditor(i).QueryInterface(IOTASourceEditor,
          Result) = S_OK Then
          Break;
    End;
End;

(**

  This method returns the editor code as a string from the given source editor
  reference.

  @precon  SourceEditor must be a valid instance.
  @postcon returns the editor code as a string from the given source editor
           reference.

  @param   SourceEditor as an IOTASourceEditor
  @return  a String

**)
Function EditorAsString(SourceEditor : IOTASourceEditor) : String;

Const
  iBufferSize : Integer = 1024;

Var
  Reader : IOTAEditReader;
  iRead : Integer;
  iPosition : Integer;
  strBuffer : AnsiString;

Begin
  Result := '';
  Reader := SourceEditor.CreateReader;
  Try
    iPosition := 0;
    Repeat
      SetLength(strBuffer, iBufferSize);
      iRead := Reader.GetText(iPosition, PAnsiChar(strBuffer), iBufferSize);
      SetLength(strBuffer, iRead);
      Result := Result + String(strBuffer);
      Inc(iPosition, iRead);
    Until iRead < iBufferSize;
  Finally
    Reader := Nil;
  End;
End;

(**

  This method adds a custom message to the IDE and returns a POINTER to that
  message.

  @precon  ptrParent must be a POINTER to another message not a reference.
  @postcon Adds a custom message to the IDE and returns a POINTER to that
           message.

  @param   strText        as a String
  @param   boolGroup      as a Boolean
  @param   boolAutoScroll as a Boolean
  @param   strFontName    as a String
  @param   iForeColour    as a TColor
  @param   fsStyle        as a TFontStyles
  @param   iBackColour    as a TColor
  @param   ptrParent      as a Pointer
  @return  a Pointer

**)
Function AddMsg(strText: String; boolGroup, boolAutoScroll: Boolean;
  strFontName: String; iForeColour: TColor; fsStyle: TFontStyles;
  iBackColour: TColor = clWindow; ptrParent: Pointer = Nil): Pointer;

Const
  strMessageGroupName = 'My Custom Messages';

Var
  M: TCustomMessage;
  {$IFDEF D0006}
  G: IOTAMessageGroup;
  {$ENDIF}

Begin
  With (BorlandIDEServices As IOTAMessageServices) Do
    Begin
      M := TCustomMessage.Create(strText, strFontName, iForeColour, fsStyle, iBackColour);
      Result := M;
      If ptrParent = Nil Then
        Begin
          {$IFDEF D0006}
          G := Nil;
          If boolGroup Then
            G := AddMessageGroup(strMessageGroupName)
          Else
            G := GetMessageGroup(0);
          {$IFDEF D2005}
          If boolAutoScroll <> G.AutoScroll Then
            G.AutoScroll := boolAutoScroll;
          {$ENDIF}
          {$IFDEF D2005}
          M.MessagePntr := AddCustomMessagePtr(M As IOTACustomMessage, G);
          {$ELSE}
          AddCustomMessage(M As IOTACustomMessage, G);
          {$ENDIF}
          {$ELSE}
          AddCustomMessage(M As IOTACustomMessage);
          {$ENDIF}
        End
      Else
        {$IFDEF D2005}
        AddCustomMessage(M As IOTACustomMessage, ptrParent);
        {$ELSE}
        AddCustomMessage(M As IOTACustomMessage);
        {$ENDIF}
    End;
End;

(**

	This method outputs text to the given Writer interface.

	@precon  Writer must be a valid instance.
	@postcon Outputs text to the given Writer interface.

	@param   Writer  as an IOTAEditWriter
	@param   iIndent as an Integer
	@param   strText as a String

**)
Procedure OutputText(Writer : IOTAEditWriter; iIndent : Integer; strText : String);

Begin
	{$IFNDEF D2009}
	Writer.Insert(PAnsiChar(StringOfChar(#32, iIndent) + strText));
	{$ELSE}
	Writer.Insert(PAnsiChar(AnsiString(StringOfChar(#32, iIndent) + strText)));
	{$ENDIF}
End;

{ TCustomMessage Methods }

(**

	Returns the column number.

	GetColumnNumber returns the column number in the associated source file. When
	the user double-clicks the message in the message view, the IDE shows the
	source file and positions the cursor at the location given by the line number
	and column number.

	@precon  None.
	@postcon We does use this in this implementation but you would return the
					 column number for your message here.

	@return  an Integer

**)
Function TCustomMessage.GetColumnNumber: Integer;

Begin
	Result := 0;
End;

(**

	Returns the source file name.

	GetFileName returns the complete path to the associated source file. When the
	user double-clicks the message in the message view, the IDE shows the source
	file and positions the cursor at the location given by the line number and
	column number.

	Return an empty string if the message is not associated with a source file.

	@precon  None.
	@postcon We return an empty string for this implementation otherwise you would
					 return the full name and path of the file associated with the
					 message.

	@return  a String

**)
Function TCustomMessage.GetFileName: String;

Begin
	Result := '';
End;

(**

	Returns the line number.

	GetLineNumber returns the line number in the associated source file. When the
	user double-clicks the message in the message view, the IDE shows the source
	file and positions the cursor at the location given by the line number and
	column number.

	@precon  None.
	@postcon We return 0 for out implementation but you would return the line
					 number of the message here.

	@return  an Integer

**)
Function TCustomMessage.GetLineNumber: Integer;

Begin
	Result := 0;
End;

(**

	Returns the text of the message.

	GetLineText returns the text of the custom message.

	@precon  None.
	@postcon Here we return the message

	@return  a String

**)
Function TCustomMessage.GetLineText: String;

Begin
	Result := FMsg;
End;

(**

	This is a setter method for the ForeColour property.

	@precon  None.
	@postcon Sets the message fore colour.

	@param   iColour as a TColor

**)
Procedure TCustomMessage.SetForeColour(iColour: TColor);

Begin
	If FForeColour <> iColour Then
		FForeColour := iColour;
End;

(**

	Provides help for the message.

	When the user selects the custom message and presses the F1 key, the IDE calls
	the ShowHelp function to provide help to the user.

	@precon  None.
	@postcon Not implemented but you would display the custom help for the message
					 here.

**)
Procedure TCustomMessage.ShowHelp;

Begin
	//
End;

(**

	This is the constructor for the TCustomMessage class.

	@precon  None.
	@postcon Creates a custom message with fore and background colours and
					 font styles.

	@param   strMsg     as a String
	@param   FontName   as a String
	@param   ForeColour as a TColor
	@param   Style      as a TFontStyles
	@param   BackColour as a TColor

**)
Constructor TCustomMessage.Create(strMsg: String; FontName: String;
	ForeColour: TColor = clBlack; Style: TFontStyles = [];
	BackColour: TColor = clWindow);

Const
	strValidChars: Set Of AnsiChar = [#10, #13, #32 .. #128];

Var
	i: Integer;
	iLength: Integer;

Begin
	SetLength(FMsg, Length(strMsg));
	iLength := 0;
	For i := 1 To Length(strMsg) Do
		{$IFDEF D2009}
		If CharInSet(strMsg[i], strValidChars) Then
		{$ELSE}
		If strMsg[i] In strValidChars Then
		{$ENDIF}
			Begin
				FMsg[iLength + 1] := strMsg[i];
				Inc(iLength);
			End;
	SetLength(FMsg, iLength);
	FFontName := FontName;
	FForeColour := ForeColour;
	FStyle := Style;
	FBackColour := BackColour;
	FMessagePntr := Nil;
End;

(**

	Calculates the bounding rectangle.

	CalcRect computes the bounding box required by the entire message. The message
	view itself always displays messages in a single line of a fixed size. If the
	user hovers the cursor over a long message, a tool tip displays the entire
	message. CalcRect returns the size of the tool tip window.

	The Canvas parameter is the canvas for drawing the message.

	The MaxWidth parameter is the maximum allowed width of the bounding box
	(e.g., the screen width).

	The Wrap Parameter is true to word-wrap the message onto multiple lines. It is
	false if the message must be kept to one line.

	The Return value is the bounding rectangle required by the message.

	@precon  None.
	@postcon We calculate the size of the message here.

	@param   Canvas   as a TCanvas
	@param   MaxWidth as an Integer
	@param   Wrap     as a Boolean
	@return  a TRect

**)
Function TCustomMessage.CalcRect(Canvas: TCanvas; MaxWidth: Integer;
	Wrap: Boolean): TRect;

Begin
	Canvas.Font.Name := FFontName;
	Canvas.Font.Style := FStyle;
	Result := Canvas.ClipRect;
	Result.Bottom := Result.Top + Canvas.TextHeight('Wp');
	Result.Right := Result.Left + Canvas.TextWidth(FMsg);
End;

(**

	Draws the message.

	Draw draws the message in the message view window or in a tool tip window.

	The Canvas parameter is the canvas on which to draw the message.

	The Rect parameter is the bounding box for the message. If you draw outside
	this rectangle, you might obscure other messages.

	The Wrap Parameter is true to word-wrap the message on multiple lines or false
	to keep the message on a single line. The message view window always uses a
	single line for each message, but the tool tip (which the user sees by hovering
	the cursor over the message) can be multiple lines.

	The drawing objects (brush, pen, and font) are set up appropriately for
	drawing messages that look like all the other messages in the message view. In
	particular, the brush and font colours are set differently depending on whether
	the message is selected. A custom-drawn message should not alter the colours or
	other graphic parameters without good reason.

	@precon  None.
	@postcon This is where we draw the message on the canvas.

	@param   Canvas as a TCanvas
	@param   Rect   as a TRect as a constant
	@param   Wrap   as a Boolean

**)
Procedure TCustomMessage.Draw(Canvas: TCanvas; Const Rect: TRect;
	Wrap: Boolean);

Begin
	If Canvas.Brush.Color = clWindow Then
		Begin
			Canvas.Font.Color := FForeColour;
			Canvas.Brush.Color := FBackColour;
			Canvas.FillRect(Rect);
		End;
	Canvas.Font.Name := FFontName;
	Canvas.Font.Style := FStyle;
	Canvas.TextOut(Rect.Left, Rect.Top, FMsg);
End;

End.
