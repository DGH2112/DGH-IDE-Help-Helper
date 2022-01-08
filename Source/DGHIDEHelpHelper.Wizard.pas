(**

  This module contains the main wizard / menu wizard code for the IDE plug-in.

  @Version 1.077
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit DGHIDEHelpHelper.Wizard;

Interface

Uses
  ToolsAPI,
  Menus,
  ExtCtrls,
  IDEOptionsInterface;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class defines the main wizard interfaces for the application. **)
  TWizardTemplate = Class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  Strict Private
    FAboutPluginIndex: Integer;
    {$IFDEF DXE00}
    FOpFrame : TIDEHelpHelperIDEOptionsInterface;
    {$ENDIF}
  Strict Protected
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
  DockableBrowserForm,
  DGHIDEHelpHelper.SplashScreen,
  DGHIDEHelpHelper.AboutBox;

{$IFDEF DXE00}
(**

  A constructor for the TWizardTemplate class.

  @precon  None.
  @postcon Creates and instance of the IDE Options Interface and registers it with the
           system.

**)
Constructor TWizardTemplate.Create;

Begin
  TIHHSplashScreen.AddSplashScreenItem;
  FAboutPluginIndex := TIHHAboutBox.InstallAboutBox;
  FOpFrame := TIDEHelpHelperIDEOptionsInterface.Create;
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FOpFrame);
End;

(**

  A destructor for the TWizardTemplate class.

  @precon  None.
  @postcon Un-registers the IDE Options Interface from the system.

**)
Destructor TWizardTemplate.Destroy;

Begin
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FOpFrame);
  FOpFrame := Nil;
  TIHHAboutBox.RemoveAboutBox(FAboutPluginIndex);
  Inherited Destroy;
End;
{$ENDIF}

(**

  This is the Execute method for the IOTAWizard interface.

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

Const
  strDGHHelpHelper = 'DGHHelpHelper';

Begin
  Result := strDGHHelpHelper;
End;

(**

  This is the GetMenuText method for the IOTAMenuWizard interface.

  @precon  None.
  @postcon Returns the menu text to be displayed under the Help menu.

  @return  a String

**)
Function TWizardTemplate.GetMenuText: String;

ResourceString
  strIDEHelpHelper = 'IDE Help Helper';

Begin
  Result := strIDEHelpHelper;
End;

(**

  This is the GetName method for the IOTAWizard interface.

  @precon  None.
  @postcon Returns the name of the wizard.

  @return  a String

**)
Function TWizardTemplate.GetName: String;

Const
  strDGHIDEHelpHelper = 'DGH IDE Help Helper';

Begin
  Result := strDGHIDEHelpHelper;
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
