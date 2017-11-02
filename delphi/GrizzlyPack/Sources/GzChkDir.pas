unit GZChkDir;

{***************************************************************
 *
 * Unit Name: GZChkDir
 * Purpose  : Retrieve useful directories.
 * Author   : Frederic GUILLIEN
 * History  : ...
 *
 ****************************************************************}

interface

uses Windows;

function CheckPath(const APath: string): string;

function GetWindowsPath: string;
function GetSystemPath: string;
function GetTempPath: string;
function GetCommonFilesPath: string;
function GetProgramFilesPath: string;
function GetDesktopPath: string;
function GetFontsPath: string;
function GetPersonalPath: string;
function GetProgramsMenuPath: string;
function GetSendToPath: string;
function GetStartMenuPath: string;
function GetStartUpMenuPath: string;
function GetExePath: string;
function GetUserApplicationDataPath: string;
function GetUserDocumentsAndSettingsRoot: string;

type
  TPathFunction = function: string;

  TPathDescr = record
    Name: string;
    Func: TPathFunction;
  end;

const
  NbPaths = 15;
  PathFunctions: array[0..NbPaths - 1] of TPathDescr = (
    (Name: '($WINDOWS)'; Func: GetWindowsPath),
    (Name: '($SYSTEM)'; Func: GetSystemPath),
    (Name: '($TEMP)'; Func: GetTempPath),
    (Name: '($COMMONFILES)'; Func: GetCommonFilesPath),
    (Name: '($PROGRAMFILES)'; Func: GetProgramFilesPath),
    (Name: '($DESKTOP)'; Func: GetDesktopPath),
    (Name: '($FONTS)'; Func: GetFontsPath),
    (Name: '($PERSONAL)'; Func: GetPersonalPath),
    (Name: '($PROGRAMSMENU)'; Func: GetProgramsMenuPath),
    (Name: '($SENDTO)'; Func: GetSendToPath),
    (Name: '($STARTMENU)'; Func: GetStartMenuPath),
    (Name: '($STARTUPMENU)'; Func: GetStartUpMenuPath),
    (Name: '($EXE)'; Func: GetExePath),
    (Name: '($APPDATA)'; Func: GetUserApplicationDataPath),
    (Name: '($DOCSETTINGS)'; Func: GetUserDocumentsAndSettingsRoot)
    );

function GetFolderLocation(nFolder: Integer): string;

implementation

uses SysUtils, ShlObj, Registry, {AStrUtil,} FGFiles;

function CheckPath(const APath: string): string;
var
  i: Integer;
begin
  Result:= APath;
  for i:= 0 to (NbPaths - 1) do
  begin
    while Pos(UpperCase(PathFunctions[i].Name), UpperCase(Result)) > 0 do
      Result:= StringReplace(Result, PathFunctions[i].Name, RemoveSlash(PathFunctions[i].Func), [rfReplaceAll]);
  end;
end;

function GetWindowsPath: string;
var
  Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, GetWindowsDirectory(Buffer, SizeOf(Buffer)));
  Result:= AddSlash(Result);
end;

function GetSystemPath: string;
var
  Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, GetSystemDirectory(Buffer, SizeOf(Buffer)));
  Result:= AddSlash(Result);
end;

function GetTempPath: string;
begin
  Result:= AddSlash(GetTempDir);
end;

function GetCommonFilesPath: string;
var
  AReg: TRegistry;
begin
  Result:= '';
  AReg:= TRegistry.Create;
  try
    AReg.RootKey:= HKEY_LOCAL_MACHINE;
    AReg.OpenKey('Software\Microsoft\Windows\CurrentVersion', False);
    Result:= AddSlash(AReg.ReadString('CommonFilesDir'));
    AReg.CloseKey;
  finally
    AReg.Free;
  end;
end;

function GetProgramFilesPath: string;
var
  AReg: TRegistry;
begin
  Result:= '';
  AReg:= TRegistry.Create;
  try
    AReg.RootKey:= HKEY_LOCAL_MACHINE;
    AReg.OpenKey('Software\Microsoft\Windows\CurrentVersion', False);
    Result:= AddSlash(AReg.ReadString('ProgramFilesDir'));
    AReg.CloseKey;
  finally
    AReg.Free;
  end;
end;

function GetFolderLocation(nFolder: Integer): string;
var
  T: TItemIDList;
  P: PItemIDList;
  Buffer: array[0..1023] of Char;
begin
  P:= @T;
  SHGetSpecialFolderLocation(0, nFolder, P);
  FillChar(Buffer, SizeOf(Buffer), 0);
  SHGetPathFromIDList(P, Buffer);
  Result:= AddSlash(Trim(string(Buffer)));
end;

function GetDesktopPath: string;
begin
  Result:= GetFolderLocation(CSIDL_DESKTOPDIRECTORY);
end;

function GetFontsPath: string;
begin
  Result:= GetFolderLocation(CSIDL_FONTS);
end;

function GetPersonalPath: string;
begin
  Result:= GetFolderLocation(CSIDL_PERSONAL);
end;

function GetProgramsMenuPath: string;
begin
  Result:= GetFolderLocation(CSIDL_PROGRAMS);
end;

function GetSendToPath: string;
begin
  Result:= GetFolderLocation(CSIDL_SENDTO);
end;

function GetStartMenuPath: string;
begin
  Result:= GetFolderLocation(CSIDL_STARTMENU);
end;

function GetStartUpMenuPath: string;
begin
  Result:= GetFolderLocation(CSIDL_STARTUP);
end;

function GetUserApplicationDataPath: string;
begin
  Result:= GetFolderLocation(CSIDL_APPDATA);
end;

function GetUserDocumentsAndSettingsRoot: string;
begin
  Result:= GetFolderLocation(40); 
end;

function GetExePath: string;
begin
  if IsLibrary then
    Result := AddSlash(ExtractFilePath(GetModuleFileName))
  else
    Result:= AddSlash(ExtractFilePath(ParamStr(0)));
end;

end.

