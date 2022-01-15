(**

  This module contains the main wizard / menu wizard code for the IDE plug-in.

  @Version 1.174
  @Author  David Hoyle
  @Date    15 Jan 2022

**)
Unit DGHIDEHelpHelper.Wizard;

Interface

uses
  Vcl.Menus,
  Vcl.ExtCtrls,
  ToolsAPI,
  DGHIDEHelpHelper.IDEOptionsInterface,
  DGHIDEHelpHelper.HelpEditorView;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class defines the main wizard interfaces for the application. **)
  TWizardTemplate = Class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  Strict Private
    FAboutPluginIndex: Integer;
    {$IFDEF DXE00}
    FOpFrame : TIDEHelpHelperIDEOptionsInterface;
    {$ENDIF}
    FKeyBindingIndex : Integer;
  Strict Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    // IOTAWizard
    Function GetIDString: String;
    Function GetName: String;
    Function GetState: TWizardState;
    Procedure Execute;
    // IOTAMenuWizard
    Function GetMenuText: String;
  End;

Implementation

Uses
  DGHIDEHelpHelper.DockableBrowserForm,
  DGHIDEHelpHelper.SplashScreen,
  DGHIDEHelpHelper.AboutBox,
  DGHIDEHelpHelper.KeyboardBindingInterface;

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
  FKeyBindingIndex := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
    TKeybindingTemplate.Create);
  FOpFrame := TIDEHelpHelperIDEOptionsInterface.Create;
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FOpFrame);
  //: @debug TfrmDockableBrowser.CreateDockableBrowser;
  RegisterIDEHelpEditorView;
End;

(**

  A destructor for the TWizardTemplate class.

  @precon  None.
  @postcon Un-registers the IDE Options Interface from the system.

**)
Destructor TWizardTemplate.Destroy;

Begin
  UnregisterIDEHelpEditorView;
  //: @debug TfrmDockableBrowser.RemoveDockableBrowser;
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FOpFrame);
  FOpFrame := Nil;
  If FKeyBindingIndex > -1 Then
    (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(FKeyBindingIndex);
  TIHHAboutBox.RemoveAboutBox(FAboutPluginIndex);
  Inherited Destroy;
End;

(**

  This is the Execute method for the IOTAWizard interface.

  @precon  None.
  @postcon This is invoked when the menu item on the Help menu is selected and it displays
           the dockable browser.

**)
Procedure TWizardTemplate.Execute;

Begin
  //: @debug TfrmDockableBrowser.ShowDockableBrowser;
  TIHHHelpEditorView.CreateEditorView;
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
