(**

  This module contains a class to handle all the applications settings (including loading
  and saving them to an INI file) and make them available to the whole application.

  @Version 1.208
  @Author  David Hoyle
  @Date    08 Jan 2022

**)
Unit ApplicationsOptions;

Interface

Uses
  Classes;

Type
  (** A class to handling the applications settings. **)
  TAppOptions = Class
  Strict Private
    FSearchURLs     : TStringList;
    FPermanentURLs  : TStringList;
    FSearchURLIndex : Integer;
    FINIFileName    : String;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    (**
      This property returns a reference to the Searches URL string list. This is a list of
      URLs with %s within then where the Identifier that needs to be looked up will be
      inserted.
      @precon  None.
      @postcon Returns a reference to the Search URL string list.
      @return  a TStringList
    **)
    Property SearchURLs : TStringList Read FSearchURLs;
    (**
      This property returns a reference to the Permanent URLs string list. This is a simple
      list of URLs which will always be available in the browser URL drop down.
      @precon  None.
      @postcon Returns a reference to the Permanent URLs string list.
      @return  a TStringList
    **)
    property PermanentURLs : TStringList Read FPermanentURLs;
    (**
      This property sets or gets the index (zero based) of the search URL to be used.
      @precon  None.
      @postcon Sets or gets the index (zero based) of the search URL to be used.
      @return  an Integer
    **)
    Property SearchURLIndex : Integer Read FSearchURLIndex Write FSearchURLIndex;
  End;

Var
  (** This is a globally visible variable to provide the application with access to the
      options class. **)
  AppOptions : TAppOptions;

Implementation

Uses
  Windows,
  {$IFDEF D2010}
  ShlObj,
  {$ELSE}
  SHFolder,
  {$ENDIF}
  //AnsiStrings,
  SysUtils,
  IniFiles;

Const
  (** A constant to define the setup INI section. **)
  strSetupINISection = 'Setup';
  (** A constant to define the Search URL Index INI Key. **)
  strSearchURLIndex = 'SearchURLIndex';
  (** A constant to define the Permanent URLs INI section. **)
  strPermanentURLsINISection = 'PermanentURLs';
  (** A constant to define the Search URLs INI section. **)
  strSearchURLsINISection = 'SearchURLs';

{ TAppOptions }

(**

  A constructor for the TAppOptions class.

  @precon  None.
  @postcon Creates the various string lists, then builds the INI file Name and finally
           loads the applications settings.

**)
Constructor TAppOptions.Create;

{$IFNDEF D2009}
Const
  // This constant doesn't exist in Delphi 2007 and below
  SHGFP_TYPE_CURRENT = 0; { current value for user, verify it exists }
  {$ENDIF}
  strSeasonFall = '\Season''s Fall\';
  strDGHIDEHelpHelperSettingsIni = 'DGH IDE Help Helper Settings.ini';

Begin
  FSearchURLs := TStringList.Create;
  FPermanentURLs := TStringList.Create;
  SetLength(FINIFileName, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT,
    PChar(FINIFileName));
  FINIFileName := StrPas(PChar(FINIFileName));
  FINIFileName := FINIFileName + strSeasonFall;
  If Not DirectoryExists(FINIFileName) Then
    ForceDirectories(FINIFileName);
  FINIFileName := FINIFileName + strDGHIDEHelpHelperSettingsIni;
  LoadSettings;
End;

(**

  A destructor for the TAppOptions class.

  @precon  None.
  @postcon saves the applications settings and frees the string lists.

**)
Destructor TAppOptions.Destroy;

Begin
  SaveSettings;
  FSearchURLs.Free;
  FPermanentURLs.Free;
  Inherited Destroy;
End;

(**

  This method loads the applications options from the already defined INI file.

  @precon  FINIFileName must be valid.
  @postcon The applications options are loaded into the class.

**)
Procedure TAppOptions.LoadSettings;

Const
  strDefaultGoogleSearch = 'www.google.com/search?q=%s';
  strDefaultMicrosoftMSDNSearch = 'https://social.msdn.microsoft.com/search/en-US?query=%s#' + 
    'refinementChanges=117';
  strDefaultGoogleURL = 'http://www.google.com';
  strDefaultMicrosftMSDNURL = 'http://msdn.microsoft.com';
  strDefaultDGHURL = 'http://www.davidghoyle.co.uk';

Var
  sl : TStringList;
  i  : Integer;
  iniFile : TMemIniFile;

Begin
  iniFile := TMemINIfile.Create(FINIFileName);
  Try
    sl := TStringList.Create;
    Try
      iniFile.ReadSection(strSearchURLsINISection, sl);
      For i := 0 To sl.Count - 1 Do
        FSearchURLs.Add(iniFile.ReadString(strSearchURLsINISection, sl[i], ''));
      If FSearchURLs.Count = 0 Then
        Begin
          FSearchURLs.Add(strDefaultGoogleSearch);
          FSearchURLs.Add(strDefaultMicrosoftMSDNSearch);
        End;
      iniFile.ReadSection(strPermanentURLsINISection, sl);
      For i := 0 To sl.Count - 1 Do
        FPermanentURLs.Add(iniFile.ReadString(strPermanentURLsINISection, sl[i], ''));
      If FPermanentURLs.Count = 0 Then
        Begin
          FPermanentURLs.Add(strDefaultGoogleURL);
          FPermanentURLs.Add(strDefaultMicrosftMSDNURL);
          FPermanentURLs.Add(strDefaultDGHURL);
        End;
      FSearchURLIndex := iniFile.ReadInteger(strSetupINISection, strSearchURLIndex, 0);
    Finally
      sl.Free;
    End;
  Finally
    iniFile.Free;
  End;
End;

(**

  This method saves the applications options to the already defined INI file.

  @precon  FINIFileName must be valid.
  @postcon The applications options are saved to the INI File.

**)
Procedure TAppOptions.SaveSettings;

Const
  strURLIdx = 'URL%4.4d';

Var
  i  : Integer;
  iniFile : TMemIniFile;

Begin
  iniFile:= TMemINIfile.Create(FINIFileName);
  Try
    For i := 0 To FSearchURLs.Count - 1 Do
      iniFile.WriteString(strSearchURLsINISection, Format(strURLIdx, [i]), FSearchURLs[i]);
    For i := 0 To FPermanentURLs.Count - 1 Do
      iniFile.WriteString(strPermanentURLsINISection, Format(strURLIdx, [i]), FPermanentURLs[i]);
    iniFile.WriteInteger(strSetupINISection, strSearchURLIndex, FSearchURLIndex);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(** The AppOptions variables is initialised as the module is loaded to provide access
    to the options as soon as possible. **)
Initialization
  AppOptions := TAppOptions.Create;
(** The AppOptions variable is destroyed as the module is unloaded to provide access to
    the options for as along as possible. **)
Finalization
  AppOptions.Free;
End.
