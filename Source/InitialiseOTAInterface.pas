(**

  This module contains methods for initialising all the various wizard interfaces required
  by the application.

  @Version 1.132
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit InitialiseOTAInterface;

Interface

Uses
  ToolsAPI;

{$INCLUDE 'CompilerDefinitions.inc'}

  Procedure Register;

  Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
    RegisterProc : TWizardRegisterProc;
    var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Exports
  InitWizard Name WizardEntryPoint;

Implementation

Uses
  SysUtils,
  Forms,
  Windows,
  DGHIDEHelpHelper.Wizard,
  KeyboardBindingInterface,
  DockableBrowserForm,
  DGHIDEHelpHelper.Functions,
  DGHIDEHelpHelper.ResourceStrings,
  DGHIDEHelpHelper.Constants;

Type
  (** An enumerate to define the type of wizard to be created. **)
  TWizardType = (wtPackageWizard, wtDLLWizard);

Const
  (** This is a constant which is initially assigned to all wizard indexes to signify
      a failed initialisation state. **)
  iWizardFailState = -1;

Var
  {$IFDEF D2005}
  (** A variable to hold the version information for the application. **)
  VersionInfo            : TVersionInfo;
  (** A variable to hold the bitmap for the application. **)
  (** This is a 48x48 butmap variabes for the splash scren in RAD Studio 2005/6 and the
      about box in 2005 and above. **)
  bmSplashScreen48x48    : HBITMAP;
  {$ENDIF}
  (** A variabel to hold the wizard index for the main wizard. **)
  iWizardIndex           : Integer = iWizardFailState;
  {$IFDEF D0006}
  (** A variabel to hold the wizard index for the about pluging. **)
  iAboutPluginIndex      : Integer = iWizardFailState;
  {$ENDIF}
  (** A variabel to hold the wizard index for the key binding wizard. **)
  iKeyBindingIndex       : Integer = iWizardFailState;


(**

  This method is called when the wizard is initialised.

  @precon  None.
  @postcon Initialises all the wizard interfaces for this application.

  @param   WizardType as a TWizardType
  @return  a TWizardTemplate

**)
Function InitialiseWizard(WizardType : TWizardType) : TWizardTemplate;

Var
  Svcs : IOTAServices;

Begin
  Svcs := BorlandIDEServices As IOTAServices;
  ToolsAPI.BorlandIDEServices := BorlandIDEServices;
  Application.Handle := Svcs.GetParentHandle;
  {$IFDEF D2005}
  // Aboutbox plugin
  bmSplashScreen48x48 := LoadBitmap(hInstance, 'DGHIDEHelpHelperSplashScreenBitMap48x48');
  With VersionInfo Do
    iAboutPluginIndex := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
      Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1), Application.Title]),
      'A wizard to intercept F1 calls and look up help on the web if not handled by the IDE..',
      bmSplashScreen48x48,
      False,
      Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
      Format('SKU Build %d.%d.%d.%d', [iMajor, iMinor, iBugfix, iBuild]));
  {$ENDIF}
  // Create Wizard / Menu Wizard
  Result := TWizardTemplate.Create;
  If WizardType = wtPackageWizard Then // Only register main wizard this way if PACKAGE
    iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(Result);
  // Create Keyboard Binding Interface
  iKeyBindingIndex := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
    TKeybindingTemplate.Create);

End;

(**

  This method is required for Package experts to initialise the expert.

  @precon  None.
  @postcon The experts is initialised.

**)
procedure Register;

begin
  InitialiseWizard(wtPackageWizard);
end;

(**

  This method is required by DLL experts for the IDE to initialise the expert.

  @precon  None.
  @postcon Initialises the interfaces by calling the InitaliseWizard method and pass this
           to the RegisterProc method of the IDE.

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Begin
  Result := BorlandIDEServices <> Nil;
  If Result Then
    RegisterProc(InitialiseWizard(wtDLLWizard));
End;

(** The initialisation section creates the dockable browser form and adds an item to the
    splash screen for the application. **)
Initialization
  TfrmDockableBrowser.CreateDockableBrowser;
(** The finalisation section removes the about boc, splash screen entries and wizard
    interfaces and frees the dockable browser. **)
Finalization
  // Remove Wizard Interface
  If iWizardIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  {$IFDEF D2005}
  // Remove Aboutbox Plugin Interface
  If iAboutPluginIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutPluginIndex);
  {$ENDIF}
  // Remove Keyboard Binding Interface
  If iKeyBindingIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBindingIndex);
  TfrmDockableBrowser.RemoveDockableBrowser;
End.

