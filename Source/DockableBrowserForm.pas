(**

  This module contains a IDE derived dockable form to contain the results of the
  searches where the IDE cannot provide help.

  @Version 1.0
  @Author  David Hoyle
  @Date    19 Apr 2016

**)
Unit DockableBrowserForm;

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
  OleCtrls,
  SHDocVw,
  DockForm,
  StdCtrls,
  Buttons,
  ToolWin,
  ComCtrls,
  ActnList,
  ImgList,
  WebBrowserFrame;

Type
  (** This class is a IDE dockable form for the results of the internet searches. **)
  TfrmDockableBrowser = Class(TDockableForm)
  Strict private
    FWebBrowser: TfmWebBrowser;
  Private
    {Private declarations}
  Public
    {Public declarations}
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Procedure Execute(strURL : String);
    Class Procedure RemoveDockableBrowser;
    Class Procedure CreateDockableBrowser;
    Class Procedure ShowDockableBrowser;
    Procedure Focus;
    Function CurrentURL : String;
  End;

  (** This is a type to define a class of the dockable for required for the
      registration. **)
  TfrmDockableBrowserClass = Class Of TfrmDockableBrowser;

Implementation

{$R *.dfm}

Uses
  DeskUtil,
  ApplicationsOptions;

Var
  (** This is a private varaible to hold the singleton instance of the
      dockable form. **)
  FormInstance : TfrmDockableBrowser;

(**

  This procedure makes the dockable module explorer visible.

  @precon  None.
  @postcon Makes the dockable module explorer visible.

  @param   Form as a TfrmDockableBrowser

**)
Procedure ShowDockableForm(Form : TfrmDockableBrowser);

Begin
  If Not Assigned(Form) Then
    Exit;
  If Not Form.Floating Then
    Begin
      Form.ForceShow;
      FocusWindow(Form);
      Form.Focus;
    End Else
    Begin
      Form.Show;
      Form.Focus;
    End;
  If Form.Visible And (Form.CurrentURL = '') And (AppOptions.PermanentURLs.Count > 0) Then
    Form.Execute(AppOptions.PermanentURLs[0]);
End;

(**

  This procedure registers the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is registered with the IDE.

  @param   FormClass as a TfrmDockableBrowserClass
  @param   FormVar
  @param   FormName  as a String as a constant

**)
Procedure RegisterDockableForm(FormClass : TfrmDockableBrowserClass; var FormVar;
  Const FormName : String);

Begin
  If @RegisterFieldAddress <> Nil Then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
End;

(**

  This method unregisters the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is unregistered with the IDE.

  @param   FormVar
  @param   FormName as a String as a constant

**)
Procedure UnRegisterDockableForm(var FormVar; Const FormName : String);
Begin
  If @UnRegisterFieldAddress <> Nil Then
    UnregisterFieldAddress(@FormVar);
End;

(**

  This procedure creates an instance of the dockable form.

  @precon  FormVar is the instance reference and FormCass is the type of class
           to be created..
  @postcon The form instance is created.

  @param   FormVar   as a TfrmDockableBrowser as a reference
  @param   FormClass as a TfrmDockableBrowserClass

**)
Procedure CreateDockableForm(var FormVar : TfrmDockableBrowser;
  FormClass : TfrmDockableBrowserClass);
Begin
  TCustomForm(FormVar) := FormClass.Create(Nil);
  RegisterDockableform(FormClass, FormVar, TCustomForm(FormVar).Name);
End;

(**

  This procedure frees the instance of the dockable form.

  @precon  None.
  @postcon Free the instance of the dockable form.

  @param   FormVar as a TfrmDockableBrowser as a reference

**)
Procedure FreeDockableForm(var FormVar : TfrmDockableBrowser);
Begin
  If Assigned(FormVar) Then
    Begin
      UnRegisterDockableForm(FormVar, FormVar.Name);
      FreeAndNil(FormVar);
    End;
End;

{ TfrmDockableBrowser }

(**

  This is the constructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Sets the dockable form up for being saved within the BDS 2006 IDE and
           then creates a Module Explorer Frame and places inside the form.

  @param   AOwner as a TComponent

**)
constructor TfrmDockableBrowser.Create(AOwner: TComponent);

begin
  inherited;
  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  FWebBrowser := TfmWebBrowser.Create(Self);
  FWebBrowser.Parent := Self;
  FWebBrowser.Align := alClient;
end;

(**

  This is the destructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Destroys the Module Explorer Frame and ensures the desktop is saved.

**)
destructor TfrmDockableBrowser.Destroy;

begin
  SaveStateNecessary := True;
  inherited;
end;

(**

  This method focuses the modukle explorers tree view the be focused IF
  available.

  @precon  None.
  @postcon Focuses the modukle explorers tree view the be focused IF available.

**)
procedure TfrmDockableBrowser.Focus;

begin
  FormInstance.SetFocus;
end;

(**

  This is a class method to create the dockable form instance.

  @precon  None.
  @postcon The form instance is created if one is not already present.

**)
class procedure TfrmDockableBrowser.CreateDockableBrowser;

begin
  If Not Assigned(FormInstance) Then
    CreateDockableForm(FormInstance, TfrmDockableBrowser);
end;

(**

  This method returns the current URL of the browser frame.

  @precon  None.
  @postcon The current URL of the browser is returned.

  @return  a String

**)
Function TfrmDockableBrowser.CurrentURL: String;

Begin
  Result := FWebBrowser.CurrentURL;
End;

(**

  This is a class method to remove the dockable form.

  @precon  None.
  @postcon Removes the instance of the dockable form.

**)
class procedure TfrmDockableBrowser.RemoveDockableBrowser;

begin
  FreeDockableForm(FormInstance);
end;

(**

  This method is a class method for displaying the dockable form. If the form
  does not already exist it is created first.

  @precon  None.
  @postcon Displays the dockable form.

**)
class procedure TfrmDockableBrowser.ShowDockableBrowser;

begin
  CreateDockableBrowser;
  ShowDockableForm(FormInstance);
end;

(**

  This is th forms main method for displaying the results of the given URL.

  @precon  None.
  @postcon If the form exists (which it should) the web browser is asked to render the
           given URL.

  @param   strURL as a String

**)
Class Procedure TfrmDockableBrowser.Execute(strURL: String);

Begin
  If Assigned(FormInstance) Then
    Begin
      FormInstance.FWebBrowser.Navigate(strURL);
      If Not FormInstance.Floating Then  //: @refactor Refactor with ShowDockableForm().
        Begin
          FormInstance.ForceShow;
          FocusWindow(FormInstance);
          FormInstance.Focus;
        End Else
        Begin
          FormInstance.Show;
          FormInstance.Focus;
        End;
      End;
End;

End.
