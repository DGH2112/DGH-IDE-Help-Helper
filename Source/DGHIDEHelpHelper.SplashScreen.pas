(**

  This module contains code for displaying a splash screen entry in the RAD Studio IDE splash screen when
  the plug-in is loaded.

  @Author  David Hoyle
  @Version 1.132
  @Date    08 Jan 2022

**)
Unit DGHIDEHelpHelper.SplashScreen;

Interface

Type
  (** A record to encapsulate the splash screen functionality. **)
  TIHHSplashScreen = Record
  Strict Private
  Public
    Class Procedure AddSplashScreenItem; Static;
  End;

Implementation

{$INCLUDE CompilerDefinitions.inc}

Uses
  ToolsAPI,
  System.SysUtils,
  Vcl.Forms,
  {$IFDEF RS110}
  Vcl.Graphics,
  {$ELSE}
  WinAPI.Windows,
  {$ENDIF RS110}
  DGHIDEHelpHelper.ResourceStrings,
  DGHIDEHelpHelper.Functions,
  DGHIDEHelpHelper.Constants;

(**

  This method adds a splash screen entry to the IDE splash screen.

  @precon  None.
  @postcon The splash screen entry is added to the IDE.

**)
Class Procedure TIHHSplashScreen.AddSplashScreenItem;

Const
  strSplashScreenBitMap = 'DGHIDEHelpHelperSplashScreenBitMap24x24';

Var
  bmSplashScreen : {$IFDEF RS110} TBitMap {$ELSE} HBITMAP {$ENDIF};
  SSS : IOTASplashScreenServices;
  VerInfo: TVersionInfo;

Begin
  BuildNumber(VerInfo);
  If Supports(SplashScreenServices, IOTASplashScreenServices, SSS) Then
    Begin
      // Add Splash Screen
      {$IFDEF RS110}
      bmSplashScreen := TBitMap.Create;
      Try
        bmSplashScreen.LoadFromResourceName(hInstance, strSplashScreenBitMap);
        SSS.AddPluginBitmap(
          Format(strSplashScreenName, [VerInfo.iMajor, VerInfo.iMinor, Copy(strRevision, VerInfo.iBugFix + 1, 1), Application.Title]),
          [bmSplashScreen],
          False,
          Format(strSplashScreenBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugfix, VerInfo.iBuild])
        );
      Finally
        bmSplashScreen.Free;
      End;
      {$ELSE}
      bmSplashScreen := LoadBitmap(hInstance, strSplashScreenBitMap);
      SSS.AddPluginBitmap(
        Format(strSplashScreenName, [VerInfo.iMajor, VerInfo.iMinor, Copy(strRevision, VerInfo.iBugFix + 1, 1), Application.Title]),
        bmSplashScreen,
        False,
        Format(strSplashScreenBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugfix, VerInfo.iBuild])
      );
      {$ENDIF}
    End;
End;

End.
