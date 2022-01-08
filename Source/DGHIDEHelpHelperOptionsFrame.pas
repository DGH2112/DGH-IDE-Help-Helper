(**

  This module contains a frame which represents the controls for configuring the IDE Help
  Helper settings.

  @Version 1.144
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit DGHIDEHelpHelperOptionsFrame;

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
  StdCtrls,
  CheckLst,
  Buttons,
  ExtCtrls;

Type
  (** A class to represent a frame of controls for configuring settings. **)
  TfmIDEHelpHelperOptions = Class(TFrame)
    gpGridPanel: TGridPanel;
    pnlSearchURLs: TPanel;
    lblSearchURLs: TLabel;
    pnlSearchButtons: TPanel;
    btnAddSearch: TBitBtn;
    btnEditSearch: TBitBtn;
    btnDeleteSearch: TBitBtn;
    lbxSearchURLs: TCheckListBox;
    pnlPermanentURLs: TPanel;
    lblPermanentURLs: TLabel;
    lbxPermanentURLs: TListBox;
    pnlPermanentButtons: TPanel;
    btnAddPermanent: TBitBtn;
    btnEditPeranent: TBitBtn;
    btnDeletePermanent: TBitBtn;
    btnDefault: TBitBtn;
    Procedure lbxSearchURLsClickCheck(Sender: TObject);
    Procedure lbxSearchURLsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure lbxSearchURLsClick(Sender: TObject);
    Procedure lbxPermanentURLsClick(Sender: TObject);
    Procedure btnAddSearchClick(Sender: TObject);
    Procedure btnAddPermanentClick(Sender: TObject);
    Procedure btnDeleteSearchClick(Sender: TObject);
    Procedure btnDeletePermanentClick(Sender: TObject);
    Procedure btnEditSearchClick(Sender: TObject);
    Procedure btnEditPeranentClick(Sender: TObject);
    Procedure btnDefaultClick(Sender: TObject);
  Private
    {Private declarations}
    (** This variable is updated to the index of the clicked item in the Search URLs
        list box so that is check value can be left along when the other check marks
        are reset. **)
    FClickIndex: Integer;
  Public
    {Public declarations}
    Procedure InitialiseFrame(Const slSearchURLs, slPermanentURLs : TStringList;
      Const iSearchURL : Integer);
    Procedure FinaliseFrame(Const slSearchURLs, slPermanentURLs : TStringList;
      var iSearchURL : Integer);
  End;

Implementation

{$R *.dfm}

Const
  (** This is a message template for the removal of items from the URL lists. **)
  strMsg = 'Are you sure you want to remove "%s" from this list?';

(**

  This is an on click event handler for the Add button for Permanent URLs.

  @precon  None.
  @postcon Adds a Permanent URL to the list IF the Input Query is confirmed.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnAddPermanentClick(Sender: TObject);

ResourceString
  strNEWPermanentURL = 'NEW Permanent URL';
  strPleaseEnterNewPermanentURL = 'Please enter a new Permanent URL.';

Var
  strURL : String;

Begin
  If InputQuery(strNEWPermanentURL, strPleaseEnterNewPermanentURL, strURL) Then
    lbxPermanentURLs.Items.Add(strURL);
End;

(**

  This is an on click event handler for the Add button for Search URLs.

  @precon  None.
  @postcon Adds a Search URL to the list IF the Input Query is confirmed.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnAddSearchClick(Sender: TObject);

ResourceString
  strNEWSearchURL = 'NEW Search URL';
  strPleaseEnterNewSearchURL = 'Please enter a new search URL (place %s where the identifier should ' + 
    'be inserted).';

Var
  strURL : String;

Begin
  If InputQuery(strNEWSearchURL,
    strPleaseEnterNewSearchURL,
    strURL) Then
    lbxSearchURLs.Items.Add(strURL);
End;

(**

  This is an on click event handler for the Default button.

  @precon  None.
  @postcon Move the selected item to the top of the list to act as the home page on first
           display.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnDefaultClick(Sender: TObject);

Begin
  lbxPermanentURLs.Items.Move(lbxPermanentURLs.ItemIndex, 0);
  lbxPermanentURLs.ItemIndex := 0;
  lbxPermanentURLsClick(Sender);
End;

(**

  This is an on click event handler for the Delete button for the Permanent URLs.

  @precon  None.
  @postcon Prompts the user to delete the selected entry and if confirmed deletes the
           item.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnDeletePermanentClick(Sender: TObject);

Begin
  If lbxPermanentURLs.ItemIndex > -1 Then
    If MessageDlg(Format(strMsg, [lbxPermanentURLs.Items[lbxPermanentURLs.ItemIndex]]),
       mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
      lbxPermanentURLs.Items.Delete(lbxPermanentURLs.ItemIndex);
End;

(**

  This is an on click event handler for the Delete button for the Search URLs.

  @precon  None.
  @postcon Prompts the user to delete the selected entry and if confirmed deletes the
           item.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnDeleteSearchClick(Sender: TObject);

Begin
  If lbxSearchURLs.ItemIndex > -1 Then
    If MessageDlg(Format(strMsg, [lbxSearchURLs.Items[lbxSearchURLs.ItemIndex]]),
       mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
      lbxSearchURLs.Items.Delete(lbxSearchURLs.ItemIndex);
End;

(**

  This is an on click event handler for the Edit button for the Permanent URLs.

  @precon  None.
  @postcon Prompts the users to edit the selected permanent URL and if confirmed updates
           the URL.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnEditPeranentClick(Sender: TObject);

ResourceString
  strEditSearchURL = 'Edit Search URL';
  strPleaseEnterPermanentURL = 'Please enter a permanent URL.';

Var
  strURL : String;

Begin
  If lbxPermanentURLs.ItemIndex > -1 Then
    Begin
      strURL := lbxPermanentURLs.Items[lbxPermanentURLs.ItemIndex];
      If InputQuery(strEditSearchURL, strPleaseEnterPermanentURL, strURL) Then
        lbxPermanentURLs.Items[lbxPermanentURLs.ItemIndex] := strURL;
    End;
End;

(**

  This is an on click event handler for the Edit button for the Search URLs.

  @precon  None.
  @postcon Prompts the users to edit the selected search URL and if confirmed updates the
           URL.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.btnEditSearchClick(Sender: TObject);

ResourceString
  strEditSearchURL = 'Edit Search URL';
  strPleaseEnterSearchURL = 'Please enter a search URL (place %s where the identifier should be ' + 
    'inserted).';

Var
  strURL : String;

Begin
  If lbxSearchURLs.ItemIndex > -1 Then
    Begin
      strURL := lbxSearchURLs.Items[lbxSearchURLs.ItemIndex];
      If InputQuery(strEditSearchURL,
        strPleaseEnterSearchURL,
        strURL) Then
        lbxSearchURLs.Items[lbxSearchURLs.ItemIndex] := strURL;
    End;
End;

(**

  This method retrieves the settings from the frame and returns them to the calling code.

  @precon  The strings lists MUST be valid instances.
  @postcon The updated information in the frame is returned to the calling code through the parameters 
           of the method.

  @param   slSearchURLs    as a TStringList as a constant
  @param   slPermanentURLs as a TStringList as a constant
  @param   iSearchURL      as an Integer as a reference

**)
Procedure TfmIDEHelpHelperOptions.FinaliseFrame(Const slSearchURLs,
  slPermanentURLs: TStringList; Var iSearchURL: Integer);

Var
  i: Integer;

Begin
  slSearchURLs.Assign(lbxSearchURLs.Items);
  slPermanentURLs.Assign(lbxPermanentURLs.Items);
  iSearchURL := -1;
  For i := 0 To lbxSearchURLs.Items.Count - 1 Do
    If lbxSearchURLs.Checked[i] Then
      Begin
        iSearchURL := i;
        Break;
      End;
End;

(**

  This is an on Key Down event handler for the Search URL List box.

  @precon  None.
  @postcon If the space bar is pressed to check an item is sets the FClickIndex is updated
           to the selected item.

  @param   Sender as a TObject
  @param   Key    as a Word as a reference
  @param   Shift  as a TShiftState

**)
Procedure TfmIDEHelpHelperOptions.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);

Begin
  If (Shift = []) And (Key = VK_SPACE) Then
    Begin
      FClickIndex := lbxSearchURLs.ItemIndex;
      lbxSearchURLsClickCheck(Sender);
      Key := 0;
    End;
End;

(**

  This method initialises the controls in the frame with the information in the parameters.

  @precon  The string lists must be valid instances.
  @postcon The frame is initialised.

  @param   slSearchURLs    as a TStringList as a constant
  @param   slPermanentURLs as a TStringList as a constant
  @param   iSearchURL      as an Integer as a constant

**)
Procedure TfmIDEHelpHelperOptions.InitialiseFrame(Const slSearchURLs,
  slPermanentURLs: TStringList; Const iSearchURL: Integer);

Begin
  FClickIndex := -1;
  lbxSearchURLsClick(Nil);
  lbxPermanentURLsClick(Nil);
  lbxSearchURLs.Items.Assign(slSearchURLs);
  lbxPermanentURLs.Items.Assign(slPermanentURLs);
  If (iSearchURL > -1) And (iSearchURL <= lbxSearchURLs.Items.Count - 1) Then
    lbxSearchURLs.Checked[iSearchURL] := True;
End;

(**

  This is an on click event handler for the Permanent URLs list box.

  @precon  None.
  @postcon Updates the availability of the edit and delete buttons.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.lbxPermanentURLsClick(Sender: TObject);

Begin
  btnDefault.Enabled := lbxPermanentURLs.ItemIndex > -1;
  btnEditPeranent.Enabled := lbxPermanentURLs.ItemIndex > -1;
  btnDeletePermanent.Enabled := lbxPermanentURLs.ItemIndex > -1;
End;

(**

  This is an on click event handler for the Search URLs list box.

  @precon  None.
  @postcon Updates the availability of the edit and delete buttons.

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.lbxSearchURLsClick(Sender: TObject);

Begin
  btnEditSearch.Enabled := lbxSearchURLs.ItemIndex > -1;
  btnDeleteSearch.Enabled := lbxSearchURLs.ItemIndex > -1;
End;

(**

  This is an on click check event handler for the Search List box.

  @precon  None.
  @postcon This method un-selects all the check marks of the list box except the one with
           index FClickIndex (i.e. ensure that ONLY the selected items check mark is
           checked).

  @param   Sender as a TObject

**)
Procedure TfmIDEHelpHelperOptions.lbxSearchURLsClickCheck(Sender: TObject);

Var
  i: Integer;

Begin
  For i := 0 To lbxSearchURLs.Items.Count - 1 Do
    lbxSearchURLs.Checked[i] := (FClickIndex = i);
End;

(**

  This method is an on mouse down event handler for the Search URLs list box.

  @precon  None.
  @postcon Updates the FClickIndex when the left mouse button is clicked over a list box
           item.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfmIDEHelpHelperOptions.lbxSearchURLsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

Begin
  If Button = mbLeft Then
    FClickIndex := lbxSearchURLs.ItemAtPos(Point(X, Y), True);
End;

End.
