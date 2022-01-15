(**

  This module contains methods for initialising all the various wizard interfaces required
  by the application.

  @Version 1.457
  @Author  David Hoyle
  @Date    15 Jan 2022

**)
Unit DGHIDEHelpHelper.InitialiseOTAInterface;

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
  DGHIDEHelpHelper.DockableBrowserForm,
  DGHIDEHelpHelper.Functions,
  DGHIDEHelpHelper.ResourceStrings,
  DGHIDEHelpHelper.Constants;

(**

  This method is called when the wizard is initialised.

  @precon  None.
  @postcon Initialises all the wizard interfaces for this application.

  @nocheck MissingCONSTInParam

  @return  a TWizardTemplate

**)
Function InitialiseWizard : TWizardTemplate;

Var
  Svcs : IOTAServices;

Begin
  Svcs := BorlandIDEServices As IOTAServices;
  ToolsAPI.BorlandIDEServices := BorlandIDEServices;
  Application.Handle := Svcs.GetParentHandle;
  Result := TWizardTemplate.Create;
End;

(**

  This method is required by DLL experts for the IDE to initialise the expert.

  @precon  None.
  @postcon Initialises the interfaces by calling the Initialise Wizard method and pass this
           to the RegisterProc method of the IDE.

  @nocheck MissingCONSTInParam
  @nohint  Terminate

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
    RegisterProc(InitialiseWizard);
End;

(** The initialisation section creates the dockable browser form and adds an item to the
    splash screen for the application. **)
Initialization
  TfrmDockableBrowser.CreateDockableBrowser;
(** The finalisation section removes the about box, splash screen entries and wizard
    interfaces and frees the dockable browser. **)
Finalization
  TfrmDockableBrowser.RemoveDockableBrowser;
End.

