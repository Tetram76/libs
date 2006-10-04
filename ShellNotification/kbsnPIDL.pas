unit kbsnPIDL;

interface

uses Windows, ShlObj;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}


{*******************************************************
                 Public enumerated types
 *******************************************************}

type
  TkbSpecialLocation  = (kbslPath, kbslDesktop, kbslProgramFiles, kbslControlPanel, kbslPrinters, kbslMyDocuments,
                         kbslFavorites, kbslStartup, kbslRecent, kbslSendTo, kbslRecycleBin, kbslStartMenu,
                         kbslDesktopDirectory, kbslDrives, kbslNetwork, kbslNethoodDirectory, kbslFonts,
                         kbslTemplates, kbslCommonStartMenu, kbslCommonProgramFiles, kbslCommonStartup,
                         kbslCommonDesktopDirectory, kbslCommonAppData, kbslCommonPrinters);



{*******************************************************
                     Public set types
 *******************************************************}

type
  TkbSpecialLocations = set of TkbSpecialLocation;



{********************************************************
        Public PIDL manipulation method interfaces
 ********************************************************}

procedure FreePIDL(PIDL: PItemIDList);                                       stdcall;
function  GetPathFromPIDL(AbsolutePIDL: PItemIDList): String;                stdcall;
function  GetPIDLFromPath(ThePath: String): PItemIDList;                     stdcall;
function  GetSpecialLocationPIDL(Location: TkbSpecialLocation): PItemIDList; stdcall;



{********************************************************
 Undocumented Windows PIDL manipulation method interfaces
 ********************************************************}

procedure ILFree(PIDL: Pointer);                        stdcall;
function  ILCreateFromPath(Path: Pointer): PItemIDList; stdcall;



{*******************************************************
    Public enum/const conversion function interfaces
 *******************************************************}

function SpecialLocationEnumToConst(Location: TkbSpecialLocation): DWORD;
function SpecialLocationConstToEnum(Location: DWORD): TkbSpecialLocation;



implementation

uses SysUtils;

{*******************************************************
                  Private unit constants
 *******************************************************}

const
  Shell32 = 'shell32.dll';


{*******************************************************
     Public PIDL manipulation method implementations
 *******************************************************}


{Frees a PIDL.  Direct call to undocumented Windows function.}
procedure FreePIDL; external Shell32  index 155;


{Gets a DOS path from a PIDL. Requires an absolute PIDL.}
function  GetPathFromPIDL(AbsolutePIDL: PItemIDList): String; stdcall;
var
  PathBuffer: Array[0..MAX_PATH] of Char;
begin
  {Initialize return value.}
  Result := EmptyStr;

  {Check that the PIDL parameter is not nil.}
  if (AbsolutePIDL = nil) then begin
    Exit;
  end; {if}

  {Convert the absolute PIDL into a DOS path and return the string.}
  SHGetPathFromIDList(AbsolutePIDL, PathBuffer);
  Result := StrPas(PathBuffer);
end;


{Creates an absolute PIDL from a DOS path string.  The PIDL must be freed
 with the OLE allocator after use, as usual.}
function  GetPIDLFromPath(ThePath: String): PItemIDList; stdcall;
var
  Buffer: Array[0..MAX_PATH] of WideChar;
begin
  {If NT, convert path to UNICODE.}
  if (SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT) then begin
    StringToWideChar(ThePath, Buffer, (High(Buffer) - Low(Buffer) + 1));
  end   {if}
  {If not NT, copy path as a standard ANSI null-term string.}
  else begin
    StrPLCopy(PChar(@Buffer), ThePath, SizeOf(Buffer));
  end;

  {Convert Path into PIDL}
  Result := ILCreateFromPath(@Buffer);
end;


{Retrieves an absolute PIDL to a special folder location. The PIDL must be freed
 with the OLE allocator after use, as usual.}
function GetSpecialLocationPIDL(Location: TkbSpecialLocation): PItemIDList; stdcall;
begin
  SHGetSpecialFolderLocation(0, SpecialLocationEnumToConst(Location), Result);
end;



{********************************************************
   Undocumented Windows PIDL manipulation method imports
 ********************************************************}

procedure ILFree;                 external Shell32  index 155;
function  ILCreateFromPath;       external Shell32  index 157;



{*******************************************************
  Public enum/const conversion function implementations
 *******************************************************}

function SpecialLocationEnumToConst(Location: TkbSpecialLocation): DWORD;
begin
  case (Location) of
    kbslPath:                   Result := 0;
    kbslDesktop:                Result := CSIDL_DESKTOP;
    kbslProgramFiles:           Result := CSIDL_PROGRAMS;
    kbslControlPanel:           Result := CSIDL_CONTROLS;
    kbslPrinters:               Result := CSIDL_PRINTERS;
    kbslMyDocuments:            Result := CSIDL_PERSONAL;
    kbslFavorites:              Result := CSIDL_FAVORITES;
    kbslStartup:                Result := CSIDL_STARTUP;
    kbslRecent:                 Result := CSIDL_RECENT;
    kbslSendTo:                 Result := CSIDL_SENDTO;
    kbslRecycleBin:             Result := CSIDL_BITBUCKET;
    kbslStartMenu:              Result := CSIDL_STARTMENU;
    kbslDesktopDirectory:       Result := CSIDL_DESKTOPDIRECTORY;
    kbslDrives:                 Result := CSIDL_DRIVES;
    kbslNetwork:                Result := CSIDL_NETWORK;
    kbslNethoodDirectory:       Result := CSIDL_NETHOOD;
    kbslFonts:                  Result := CSIDL_FONTS;
    kbslTemplates:              Result := CSIDL_TEMPLATES;
    kbslCommonStartMenu:        Result := CSIDL_COMMON_STARTMENU;
    kbslCommonProgramFiles:     Result := CSIDL_COMMON_PROGRAMS;
    kbslCommonStartup:          Result := CSIDL_COMMON_STARTUP;
    kbslCommonDesktopDirectory: Result := CSIDL_COMMON_DESKTOPDIRECTORY;
    kbslCommonAppData:          Result := CSIDL_APPDATA;
    kbslCommonPrinters:         Result := CSIDL_PRINTHOOD;
    else                        Result := 0;
  end; {case}
end;


function SpecialLocationConstToEnum(Location: DWORD): TkbSpecialLocation;
begin
  case (Location) of
    CSIDL_DESKTOP:                 Result := kbslDesktop;
    CSIDL_PROGRAMS:                Result := kbslProgramFiles;
    CSIDL_CONTROLS:                Result := kbslControlPanel;
    CSIDL_PRINTERS:                Result := kbslPrinters;
    CSIDL_PERSONAL:                Result := kbslMyDocuments;
    CSIDL_FAVORITES:               Result := kbslFavorites;
    CSIDL_STARTUP:                 Result := kbslStartup;
    CSIDL_RECENT:                  Result := kbslRecent;
    CSIDL_SENDTO:                  Result := kbslSendTo;
    CSIDL_BITBUCKET:               Result := kbslRecycleBin;
    CSIDL_STARTMENU:               Result := kbslStartMenu;
    CSIDL_DESKTOPDIRECTORY:        Result := kbslDesktopDirectory;
    CSIDL_DRIVES:                  Result := kbslDrives;
    CSIDL_NETWORK:                 Result := kbslNetwork;
    CSIDL_NETHOOD:                 Result := kbslNethoodDirectory;
    CSIDL_FONTS:                   Result := kbslFonts;
    CSIDL_TEMPLATES:               Result := kbslTemplates;
    CSIDL_COMMON_STARTMENU:        Result := kbslCommonStartMenu;
    CSIDL_COMMON_PROGRAMS:         Result := kbslCommonProgramFiles;
    CSIDL_COMMON_STARTUP:          Result := kbslCommonStartup;
    CSIDL_COMMON_DESKTOPDIRECTORY: Result := kbslCommonDesktopDirectory;
    CSIDL_APPDATA:                 Result := kbslCommonAppData;
    CSIDL_PRINTHOOD:               Result := kbslCommonPrinters;
    else                           Result := kbslPath;
  end; {case}
end;



end.
