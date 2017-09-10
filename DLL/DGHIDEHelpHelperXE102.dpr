(**

  This defines a DLL Open Tools API project to displaya web browser search for any context
  information the IDE cannot find.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Dec 2016

**)
library DGHIDEHelpHelperXE102;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R *.res}

{Generate by DGH OTA Wizard}
{$R 'ITHrVerInfo.res' 'ITHrVerInfo.RC'}
{$R 'SplashScreenIcon.res' '..\SplashScreenIcon.RC'}

uses
  InitialiseOTAInterface in '..\Source\InitialiseOTAInterface.pas',
  UtilityFunctions in '..\Source\UtilityFunctions.pas',
  WizardInterface in '..\Source\WizardInterface.pas',
  KeyboardBindingInterface in '..\Source\KeyboardBindingInterface.pas',
  DockableBrowserForm in '..\Source\DockableBrowserForm.pas' {frmDockableBrowser},
  DGHIDEHelphelperConfigForm in '..\Source\DGHIDEHelphelperConfigForm.pas' {frmDGHIDEHelphelperConfig},
  ApplicationsOptions in '..\Source\ApplicationsOptions.pas',
  DGHIDEHelpHelperOptionsFrame in '..\Source\DGHIDEHelpHelperOptionsFrame.pas' {fmIDEHelpHelperOptions: TFrame},
  IDEOptionsInterface in '..\Source\IDEOptionsInterface.pas',
  WebBrowserFrame in '..\Source\WebBrowserFrame.pas' {fmWebBrowser: TFrame};

begin
end.
