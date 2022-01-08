(**

  This module contains a class to provide a key binding wizard to capture the F1 key press
  in the IDE and if unhandled do an internet search.

  @Version 1.059
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit KeyboardBindingInterface;

Interface

Uses
  ToolsAPI,
  Classes;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class implements the IOTAKeyboardbinding interface for handling the F1 key
      press. **)
  TKeybindingTemplate = Class(TNotifierObject, IOTAKeyboardBinding)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure ProcessKeyBinding(Const Context: IOTAKeyContext;
      KeyCode: TShortcut; Var BindingResult: TKeyBindingResult);
    Function GetWordAtCursor : String;
  Public
    Procedure BindKeyboard(Const BindingServices: IOTAKeyBindingServices);
    Function GetBindingType: TBindingType;
    Function GetDisplayName: String;
    Function GetName: String;
  End;

Implementation

Uses
  SysUtils,
  Dialogs,
  Menus,
  DGHIDEHelpHelper.Functions,
  ShellAPI,
  Windows,
  DockableBrowserForm,
  ApplicationsOptions;

{ TKeybindingTemplate }

(**

  This method is an interface method of the IOTAKeyboardBinding interface and is called by
  the IDE when the wizard is created and is used to add a key binding for the F1 key press.

  @precon  None.
  @postcon The F1 key press is bound and provided a handler in ProcessKeyBinding.

  @param   BindingServices as an IOTAKeyBindingServices as a constant

**)
Procedure TKeybindingTemplate.BindKeyboard(Const BindingServices: IOTAKeyBindingServices);

Begin
  BindingServices.AddKeyBinding([TextToShortcut('F1')], ProcessKeyBinding, Nil);
End;

(**

  This is the GetBindingType implementation for the IOTAKeyboardBinding interface.

  @precon  None.
  @postcon Returns that this is a partial key binding, i.e. adds to the main key bindings.

  @return  a TBindingType

**)
Function TKeybindingTemplate.GetBindingType: TBindingType;

Begin
  Result := btPartial;
End;

(**

  This is the getDisplayName method for the IOTAKeyboardBinding interface.

  @precon  None.
  @postcon Provides the display name for the keyboard binding which is displayed in the
           options dialogue.

  @return  a String

**)
Function TKeybindingTemplate.GetDisplayName: String;

ResourceString
  strIDEHelpHelperPartialKeybindings = 'IDE Help Helper Partial Keyboard Bindings';

Begin
  Result := strIDEHelpHelperPartialKeybindings;
End;

(**

  This is the GetName method for the IOTAKeyboardBinding interface.

  @precon  None.
  @postcon Provides a name of the wizard interface.

  @return  a String

**)
Function TKeybindingTemplate.GetName: String;

ResourceString
  strIDEHelpHelperPartialKeyboardBindings = 'IDE Help Helper Partial Keyboard Bindings';

Begin
  Result := strIDEHelpHelperPartialKeyboardBindings;
End;

(**

  This method returns the word underneath the cursor position in the active editor.

  This method gets the text from the editor using the utility function EditorAsString,
  gets the cursor position, places the text in a string list and then checks for the word
  under the cursor position.

  @precon  None.
  @postcon The word underneath he cursor in the editor is returned else an empty string
           is returned if the cursor is not under a word.

  @return  a String

**)
Function TKeybindingTemplate.GetWordAtCursor: String;

Const
  strIdentChars = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

Var
  SE: IOTASourceEditor;
  EP: TOTAEditPos;
  iPosition: Integer;
  sl: TStringList;

Begin
  Result := '';
  SE := ActiveSourceEditor;
  EP := SE.EditViews[0].CursorPos;
  sl := TStringList.Create;
  Try
    sl.Text := EditorAsString(SE);
    Result := sl[Pred(EP.Line)];
    iPosition := EP.Col;
    If (iPosition > 0) And (Length(Result) >= iPosition) And
      {$IFDEF D2009}
        CharInSet(Result[iPosition], strIdentChars)
      {$ELSE}
        (Result[iPosition] In strIdentChars)
      {$ENDIF} Then
      Begin
        While (iPosition > 1) And
          {$IFDEF D2009}
          (CharInSet(Result[Pred(iPosition)], strIdentChars))
          {$ELSE}
          (Result[iPosition] In strIdentChars)
          {$ENDIF}
          Do
          Dec(iPosition);
        Delete(Result, 1, Pred(iPosition));
        iPosition := 1;
        While {$IFDEF D2009} CharInSet(Result[iPosition], strIdentChars) {$ELSE}
          Result[iPosition] In strIdentChars {$ENDIF} Do
          Inc(iPosition);
        Delete(Result, iPosition, Length(Result) - iPosition + 1);
        If {$IFDEF D2009} CharInSet(Result[1], ['0'..'9']) {$ELSE}
           Result[1] In ['0'..'9'] {$ENDIF} Then
          Result := '';
      End Else
        Result := '';
  Finally
    sl.Free;
  End;
End;

(**

  This method intercepts the F1 key and asks the IDE whether it understands the word at
  the cursor. If so then the event does nothing else as the IDE will display the
  IDEs help however if the IDE says it does not understand the word at the cursor then
  the search URL is used to search for help on the word in the dockable browser.

  @precon  None.
  @postcon Either the IDe displays its help if it knows the word at the cursor of else
           the dockable browser is displayed with a search for the word.

  @nocheck MissingCONSTInParam
  @nohint  Context KeyCode

  @param   Context       as an IOTAKeyContext as a constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
Procedure TKeybindingTemplate.ProcessKeyBinding(Const Context: IOTAKeyContext;
  KeyCode: TShortcut; Var BindingResult: TKeyBindingResult);

Const
  strMsg = 'Your search URLs are misconfigured. Ensure there is a Search URL and that ' +
    'it is checked in the list in the configuration dialogue.';

Var
  strWordAtCursor : String;
  boolHandled: Boolean;

Begin
  strWordAtCursor := GetWordAtCursor;
  If strWordAtCursor <> '' Then
    Begin
      boolHandled :=
        (BorlandIDEServices As IOTAHelpServices).UnderstandsKeyword(strWordAtCursor);
      If boolHandled Then
        BindingResult := krUnhandled
      Else
        Begin
          BindingResult := krHandled;
          If (AppOptions.SearchURLIndex <= AppOptions.SearchURLs.Count - 1) And
            (AppOptions.SearchURLIndex >= 0) Then
            TfrmDockableBrowser.Execute(
              Format(AppOptions.SearchURLs[AppOptions.SearchURLIndex], [strWordAtCursor]))
          Else
            MessageDlg(strMsg, mtError, [mbOK], 0);
        End;
    End Else
      BindingResult := krUnhandled;
End;

End.
