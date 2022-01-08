(**

  This module contains a class to handle all the applications settings (including loading
  and saving them to an INI file) and make them available to the whole application.

  @Version 1.020
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

{ TAppOptions }

(**

  A constructor for the TAppOptions class.

  @precon  None.
	@postcon Creates the various string lists, then builds the INI file Name and finally
					 loads the applications settings.

**)
Constructor TAppOptions.Create;

{$IFNDEF D2009}
// This constant doesn't exist in Delphi 2007 and below
Const
	SHGFP_TYPE_CURRENT = 0; { current value for user, verify it exists }
{$ENDIF}

Begin
	FSearchURLs := TStringList.Create;
	FPermanentURLs := TStringList.Create;
	SetLength(FINIFileName, MAX_PATH);
	SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT,
		PChar(FINIFileName));
	FINIFileName := StrPas(PChar(FINIFileName));
	FINIFileName := FINIFileName + '\Season''s Fall\';
	If Not DirectoryExists(FINIFileName) Then
		ForceDirectories(FINIFileName);
	FINIFileName := FINIFileName + 'DGH IDE Help Helper Settings.ini';
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

Var
  sl : TStringList;
  i  : Integer;

Begin
  With TMemINIfile.Create(FINIFileName) Do
    Try
      sl := TStringList.Create;
      Try
        ReadSection('SearchURLs', sl);
        For i := 0 To sl.Count - 1 Do
          FSearchURLs.Add(ReadString('SearchURLs', sl[i], ''));
        If FSearchURLs.Count = 0 Then
          Begin
            FSearchURLs.Add('www.google.com/search?q=%s');
            FSearchURLs.Add('https://social.msdn.microsoft.com/search/en-US?query=%s#refinementChanges=117');
          End;
        ReadSection('PermanentURLs', sl);
        For i := 0 To sl.Count - 1 Do
          FPermanentURLs.Add(ReadString('PermanentURLs', sl[i], ''));
        If FPermanentURLs.Count = 0 Then
          Begin
            FPermanentURLs.Add('http://www.google.com');
            FPermanentURLs.Add('http://msdn.microsoft.com');
            FPermanentURLs.Add('http://www.davidghoyle.co.uk');
          End;
        FSearchURLIndex := ReadInteger('Setup', 'SearchURLIndex', 0);
      Finally
        sl.Free;
      End;
    Finally
      Free;
    End;
End;

(**

  This method saves the applications options to the already defined INI file.

  @precon  FINIFileName must be valid.
  @postcon The applications options are saved to the INI File.

**)
Procedure TAppOptions.SaveSettings;

Var
  i  : Integer;

Begin
  With TMemINIfile.Create(FINIFileName) Do
    Try
      For i := 0 To FSearchURLs.Count - 1 Do
        WriteString('SearchURLs', Format('URL%4.4d', [i]), FSearchURLs[i]);
      For i := 0 To FPermanentURLs.Count - 1 Do
        WriteString('PermanentURLs', Format('URL%4.4d', [i]), FPermanentURLs[i]);
      WriteInteger('Setup', 'SearchURLIndex', FSearchURLIndex);
      UpdateFile;
    Finally
      Free;
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
