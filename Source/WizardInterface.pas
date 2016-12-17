(**

  This module contains the main wizard / menu wizard code for the IDE plugin.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Apr 2016

**)
Unit WizardInterface;

Interface

Uses
  ToolsAPI,
  Menus,
  ExtCtrls,
  IDEOptionsInterface;

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Type
  (** This class defines the main wizard interfaces for the application. **)
  TWizardTemplate = Class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF DXE00}
    FOpFrame : TIDEHelpHelperIDEOptionsInterface;
    {$ENDIF}
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    {$IFDEF DXE00}
    Constructor Create;
    Destructor Destroy; Override;
    {$ENDIF}
    // IOTAWizard
    Function GetIDString: String;
    Function GetName: String;
    Function GetState: TWizardState;
    Procedure Execute;
    // IOTAMenuWizard
    Function GetMenuText: String;
  End;

Implementation

{ TWizardTemplate }

Uses
  DockableBrowserForm;

{$IFDEF DXE00}
(**

  A constructor for the TWizardTemplate class.

  @precon  None.
  @postcon Creates and instance of the IDE Options Interface and registers it with the
           system.

**)
Constructor TWizardTemplate.Create;

Begin
  FOpFrame := TIDEHelpHelperIDEOptionsInterface.Create;
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FOpFrame);
End;

(**

  A destructor for the TWizardTemplate class.

  @precon  None.
  @postcon Unregisters the IDE Options Interface from the system.

**)
Destructor TWizardTemplate.Destroy;

Begin
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FOpFrame);
  FOpFrame := Nil;
  Inherited Destroy;
End;
{$ENDIF}

(**

  This is the Exceute method for the IOTAWizard interface.

  @precon  None.
  @postcon This is invoked when the menu item on the Help menu is selected and it displays
           the dockable browser.

**)
Procedure TWizardTemplate.Execute;

Begin
  TfrmDockableBrowser.ShowDockableBrowser;
End;

(**

  This is the GetIDString method for the IOTAWizard interface.

  @precon  None.
  @postcon Returns a ID string for the wizard.

  @return  a String

**)
Function TWizardTemplate.GetIDString: String;

Begin
  Result := 'DGH IDE HelpHelper';
End;

(**

  This is the GetMenuText method for the IOTAMenuWizard interface.

  @precon  None.
  @postcon Returns the menu text to be displayed under the Help menu.

  @return  a String

**)
Function TWizardTemplate.GetMenuText: String;

Begin
  Result := 'IDE Help Helper';
End;

(**

  This is the GetName method for the IOTAWizard interface.

  @precon  None.
  @postcon Returns the name of the wizard.

  @return  a String

**)
Function TWizardTemplate.GetName: String;

Begin
  Result := 'DGH IDE Help Helper';
End;

(**

  This is the GetState method for the IOTAWizard interface.

  @precon  None.
  @postcon Returns a set telling the IDe that this wizard is enabled.

  @return  a TWizardState

**)
Function TWizardTemplate.GetState: TWizardState;

Begin
  Result := [wsEnabled];
End;

End.
