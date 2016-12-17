(**

  This module contains a class to represent a configuation for for the applications
  options.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Apr 2016

**)
Unit DGHIDEHelphelperConfigForm;

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
  Buttons,
  CheckLst,
  ExtCtrls,
  DGHIDEHelphelperOptionsFrame;

Type
  (** This class represents a form for editing the applications options. **)
  TfrmDGHIDEHelphelperConfig = Class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlFrame: TPanel;
    procedure FormCreate(Sender: TObject);
  Strict Private
  Private
    {Private declarations}
    FFrame : TfmIDEHelpHelperOptions;
  Public
    {Public declarations}
    Class Function Execute(slSearchURLs, slPermanentURLs : TStringList;
      var iSearchURL : Integer) : Boolean;
  End;

Implementation

{$IFDEF CodeSiteLogging}
Uses
  CodeSiteLogging;
{$ENDIF}

{$R *.dfm}

{ TfrmDGHIDEHelphelperConfig }

(**

  This is the forms main interface method for invoking the dialogue. The method will
  return true if the dialogue was confirmed and the data has changed.

  @precon  The string lists must be valid instances.
  @postcon The pass variables are updated in the dialogue is confirmed and true is
           returned else they are not modified and false is returned.

  @param   slSearchURLs    as a TStringList
  @param   slPermanentURLs as a TStringList
  @param   iSearchURL      as an Integer as a reference
  @return  a Boolean

**)
Class Function TfrmDGHIDEHelphelperConfig.Execute(slSearchURLs,
  slPermanentURLs : TStringList; var iSearchURL : Integer): Boolean;

Begin
  Result := False;
  With TfrmDGHIDEHelphelperConfig.Create(Nil) Do
    Try
      FFrame.InitialiseFrame(slSearchURLs, slPermanentURLs, iSearchURL);
      If ShowModal = mrOk Then
        Begin
          FFrame.FinaliseFrame(slSearchURLs, slPermanentURLs, iSearchURL);
          Result := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This is an OnFormCreate Event Handler for the TfrmDGHIDEHelpHelperConfig class.

  @precon  None.
  @postcon Initialises the FClickIndex variable to -1.

  @param   Sender as a TObject

**)
Procedure TfrmDGHIDEHelphelperConfig.FormCreate(Sender: TObject);

Begin
  FFrame := TfmIDEHelpHelperOptions.Create(Self);
  FFrame.Parent := pnlFrame;
  FFrame.Align := alClient;
End;

End.
