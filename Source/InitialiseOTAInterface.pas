(**

  This module contains methods for initialising all the various wizard interfaces required
  by the application.

  @Version 1.300
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit InitialiseOTAInterface;

Interface

Uses
  ToolsAPI;

{$INCLUDE 'CompilerDefinitions.inc'}

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
	(** A variable to hold the wizard index for the main wizard. **)
	iWizardIndex           : Integer = iWizardFailState;
	(** A variable to hold the wizard index for the key binding wizard. **)
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
	// Create Wizard / Menu Wizard
	Result := TWizardTemplate.Create;
	If WizardType = wtPackageWizard Then // Only register main wizard this way if PACKAGE
		iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(Result);
	// Create Keyboard Binding Interface
	iKeyBindingIndex := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
		TKeybindingTemplate.Create);

End;

(**

	This method is required by DLL experts for the IDE to initialise the expert.

	@precon  None.
	@postcon Initialises the interfaces by calling the Initialise Wizard method and pass this
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
(** The finalisation section removes the about box, splash screen entries and wizard
    interfaces and frees the dockable browser. **)
Finalization
  // Remove Wizard Interface
  If iWizardIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  // Remove Keyboard Binding Interface
  If iKeyBindingIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBindingIndex);
  TfrmDockableBrowser.RemoveDockableBrowser;
End.

