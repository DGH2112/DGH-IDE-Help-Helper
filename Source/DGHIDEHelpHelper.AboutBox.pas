(**

  This module contains code to install and remove about box entries for the plug.

  @Author  David Hoyle
  @Version 1.229
  @Date    08 Jan 2022

**)
Unit DGHIDEHelpHelper.AboutBox;

Interface

Type
  (** A record to encapsulate the about box functionality. **)
  TIHHAboutBox = Record
    Class Function  InstallAboutBox : Integer; Static;
    Class Procedure RemoveAboutBox(Const iIndex : Integer); Static;
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
  DGHIDEHelpHelper.Functions,
  DGHIDEHelpHelper.ResourceStrings,
  DGHIDEHelpHelper.Constants;

(**

  This method install the about box entry into the IDE and returns the index of the entry.

  @precon  None.
  @postcon The about box entry is installed.

  @return  an Integer

**)
Class Function TIHHAboutBox.InstallAboutBox: Integer;

Const
  strAboutBoxBitMap = 'DGHIDEHelpHelperSplashScreenBitMap48x48';
  strDescription = 'A wizard to intercept F1 calls and look up help on the web if not handled by the IDE.';

ResourceString
  strSKUBuild = 'SKU Build %d.%d.%d.%d';

Var
  bmAboutBox : {$IFDEF RS110} TBitMap {$ELSE} HBITMAP {$ENDIF};
  VerInfo: TVersionInfo;
  ABS : IOTAAboutBoxServices;

Begin
  Result := -1;
  BuildNumber(VerInfo);
  If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
    Begin
      {$IFDEF RS110}
      bmAboutBox := TBitMap.Create;
      Try
        bmAboutBox.LoadFromResourceName(hINstance, strAboutBoxBitMap);
        Result := ABS.AddPluginInfo(
          Format(strSplashScreenName, [VerInfo.iMajor, VerInfo.iMinor, Copy(strRevision, VerInfo.iBugFix + 1, 1),
            Application.Title]),
          strDescription,
          [bmAboutBox],
          {$IFDEF DEBUG} True {$ELSE} False {$ENDIF DEBUG},
          Format(strSplashScreenBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugfix, VerInfo.iBuild]),
          Format(strSKUBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugfix, VerInfo.iBuild])
        );
      Finally
        bmAboutBox.Free;
      End;
      {$ELSE}
      bmAboutBox := LoadBitmap(hInstance, strAboutBoxBitMap);
      Result := ABS.AddPluginInfo(
        Format(strSplashScreenName, [VerInfo.iMajor, VerInfo.iMinor, Copy(strRevision, VerInfo.iBugFix + 1, 1),
          Application.Title]),
        strDescription,
        bmAboutBox,
        {$IFDEF DEBUG} True {$ELSE} False {$ENDIF DEBUG},
        Format(strSplashScreenBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugfix, VerInfo.iBuild]),
        Format(strSKUBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugfix, VerInfo.iBuild])
      );
      {$ENDIF}
    End;
End;

(**

  This method removes the about box entry from the IDE using the given index of the entry.

  @precon  None.
  @postcon The about box entry is removed.

  @param   iIndex as an Integer as a constant

**)
Class Procedure TIHHAboutBox.RemoveAboutBox(Const iIndex: Integer);


Var
  ABS : IOTAAboutBoxServices;

Begin
  If iIndex > -1 Then
    If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
      ABS.RemovePluginInfo(iIndex);
End;

End.
