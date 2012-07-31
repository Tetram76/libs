// Delphi interface unit for UnRAR.dll
// Translated from unrar.h
// Use Delphi 2.0 and higher to compile this module
//
// Ported to Delphi by Eugene Kotlyarov, fidonet: 2:5058/26.9 ek@oris.ru
// Fixed version by Alexey Torgashin <alextp@mail.ru>, 2:5020/604.24@fidonet
//
// Revisions:
// Aug 2001 - changed call convention for TChangeVolProc and TProcessDataProc
//          - added RARGetDllVersion function, see comment below
//
// Jan 2002 - Added RARSetCallback  // eugene
//
// Oct 2002 - Added RARHeaderDataEx, RAROpenArchiveDataEx // eugene

unit UnRAR;
{$D-}
{$J+}
interface

uses Windows;

const
  ERAR_END_ARCHIVE    = 10;
  ERAR_NO_MEMORY      = 11;
  ERAR_BAD_DATA       = 12;
  ERAR_BAD_ARCHIVE    = 13;
  ERAR_UNKNOWN_FORMAT = 14;
  ERAR_EOPEN          = 15;
  ERAR_ECREATE        = 16;
  ERAR_ECLOSE         = 17;
  ERAR_EREAD          = 18;
  ERAR_EWRITE         = 19;
  ERAR_SMALL_BUF      = 20;
  ERAR_UNKNOWN        = 21;

  RAR_OM_LIST         =  0;
  RAR_OM_EXTRACT      =  1;

  RAR_SKIP            =  0;
  RAR_TEST            =  1;
  RAR_EXTRACT         =  2;

  RAR_VOL_ASK         =  0;
  RAR_VOL_NOTIFY      =  1;

  RAR_DLL_VERSION     =  3;

  UCM_CHANGEVOLUME    =  0;
  UCM_PROCESSDATA     =  1;
  UCM_NEEDPASSWORD    =  2;

type
  RARHeaderData = packed record
    ArcName: packed array[0..Pred(260)] of Char;
    FileName: packed array[0..Pred(260)] of Char;
    Flags: UINT;
    PackSize: UINT;
    UnpSize: UINT;
    HostOS: UINT;
    FileCRC: UINT;
    FileTime: UINT;
    UnpVer: UINT;
    Method: UINT;
    FileAttr: UINT;
    CmtBuf: PChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
  end;

  RARHeaderDataEx = packed record
    ArcName: packed array [0..1023] of char;
    ArcNameW: packed array [0..1023] of WideChar;
    FileName: packed array [0..1023] of char;
    FileNameW: packed array [0..1023] of WideChar;
    Flags: UINT;
    PackSize: UINT;
    PackSizeHigh: UINT;
    UnpSize: UINT;
    UnpSizeHigh: UINT;
    HostOS: UINT;
    FileCRC: UINT;
    FileTime: UINT;
    UnpVer: UINT;
    Method: UINT;
    FileAttr: UINT;
    CmtBuf: PChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
    Reserved: packed array [0..1023] of UINT;
  end;

  RAROpenArchiveData = packed record
    ArcName: PChar;
    OpenMode: UINT;
    OpenResult: UINT;
    CmtBuf: PChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
  end;

  RAROpenArchiveDataEx = packed record
    ArcName: PChar;
    ArcNameW: PWideChar;
    OpenMode: UINT;
    OpenResult: UINT;
    CmtBuf: PChar;
    CmtBufSize: UINT;
    CmtSize: UINT;
    CmtState: UINT;
    Flags: UINT;
    Reserved: packed array [0..31] of UINT;
  end;

  TUnrarCallback = function (Msg: UINT; UserData, P1, P2: Integer) :Integer; stdcall;

const
  _unrar = 'unrar.dll';
  _hunrardll: THandle = 0;

function RAROpenArchive(var ArchiveData: RAROpenArchiveData): THandle;
function RAROpenArchiveEx(var ArchiveData: RAROpenArchiveDataEx): THandle;
function RARCloseArchive(hArcData: THandle): Integer;
function RARReadHeader(hArcData: THandle; var HeaderData: RARHeaderData): Integer;
function RARReadHeaderEx(hArcData: THandle; var HeaderData: RARHeaderDataEx): Integer;
function RARProcessFile(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer;
procedure RARSetCallback(hArcData: THandle; UnrarCallback: TUnrarCallback; UserData: Integer);
procedure RARSetPassword(hArcData: THandle; Password: PChar);

// Wrapper for DLL's function - old unrar.dll doesn't export RARGetDllVersion
// Returns: -1 = DLL not found; 0 = old ver. (C-style callbacks); >0 = new ver.
function RARGetDllVersion: integer;

// obsolete functions
//type
//  TChangeVolProc = function(ArcName: PChar; Mode: Integer): Integer; stdcall;
//  TProcessDataProc = function(Addr: PUChar; Size: Integer): Integer; stdcall;

//procedure RARSetChangeVolProc(hArcData: THandle; ChangeVolProc: TChangeVolProc);
//procedure RARSetProcessDataProc(hArcData: THandle; ProcessDataProc: TProcessDataProc); 

implementation

uses SysUtils;

type
  TRAROpenArchive = function (var ArchiveData: RAROpenArchiveData): THandle; stdcall;
  TRAROpenArchiveEx = function (var ArchiveData: RAROpenArchiveDataEx): THandle; stdcall;
  TRARCloseArchive = function (hArcData: THandle): Integer; stdcall;
  TRARReadHeader = function (hArcData: THandle; var HeaderData: RARHeaderData): Integer; stdcall;
  TRARReadHeaderEx = function (hArcData: THandle; var HeaderData: RARHeaderDataEx): Integer; stdcall;
  TRARProcessFile = function (hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer; stdcall;
  TRARSetCallback = procedure (hArcData: THandle; UnrarCallback: TUnrarCallback; UserData: Integer); stdcall;
  TRARSetPassword = procedure (hArcData: THandle; Password: PChar); stdcall;
//  TRARSetChangeVolProc = procedure (hArcData: THandle; ChangeVolProc: TChangeVolProc); stdcall;
//  TRARSetProcessDataProc = procedure (hArcData: THandle; ProcessDataProc: TProcessDataProc); stdcall;

const
  _RAROpenArchive: TRAROpenArchive = nil;
  _RAROpenArchiveEx: TRAROpenArchiveEx = nil;
  _RARCloseArchive: TRARCloseArchive = nil;
  _RARReadHeader: TRARReadHeader = nil;
  _RARReadHeaderEx: TRARReadHeaderEx = nil;
  _RARProcessFile: TRARProcessFile = nil;
  _RARSetCallback: TRARSetCallback = nil;
  _RARSetPassword: TRARSetPassword = nil;
//  _RARSetChangeVolProc: TRARSetChangeVolProc = nil;
//  _RARSetProcessDataProc: TRARSetProcessDataProc = nil;

type
  TRARGetDllVersion = function: integer; stdcall;

function RARGetDllVersion: integer;
var
  h: THandle;
  f: TRARGetDllVersion;
begin
  h := LoadLibrary(_unrar);
  if h = 0 then begin
    Result := -1;
    Exit
  end;
  f := GetProcAddress(h, 'RARGetDllVersion');
  if @f = nil then
    Result := 0
  else
    Result := f;
  FreeLibrary(h);
end;

procedure InitUnrarDLL;
begin
  if _hunrardll < 32 then begin
    _hunrardll := LoadLibrary(_unrar);
    if _hunrardll < 32 then raise Exception.Create('Unable to found ' + _unrar);

    _RAROpenArchive := GetProcAddress(_hunrardll, 'RAROpenArchive');
    _RAROpenArchiveEx := GetProcAddress(_hunrardll, 'RAROpenArchiveEx');
    _RARCloseArchive := GetProcAddress(_hunrardll, 'RARCloseArchive');
    _RARReadHeader := GetProcAddress(_hunrardll, 'RARReadHeader');
    _RARReadHeaderEx := GetProcAddress(_hunrardll, 'RARReadHeaderEx');
    _RARProcessFile := GetProcAddress(_hunrardll, 'RARProcessFile');
    _RARSetCallback := GetProcAddress(_hunrardll, 'RARSetCallback');
    _RARSetPassword := GetProcAddress(_hunrardll, 'RARSetPassword');
  end;
end;

function RAROpenArchive(var ArchiveData: RAROpenArchiveData): THandle;
begin
  InitUnrarDLL;
  Result := _RAROpenArchive(ArchiveData);
end;

function RAROpenArchiveEx(var ArchiveData: RAROpenArchiveDataEx): THandle;
begin
  InitUnrarDLL;
  Result := _RAROpenArchiveEx(ArchiveData);
end;

function RARCloseArchive(hArcData: THandle): Integer;
begin
  Result := _RARCloseArchive(hArcData);
end;

function RARReadHeader(hArcData: THandle; var HeaderData: RARHeaderData): Integer;
begin
  InitUnrarDLL;
  Result := _RARReadHeader(hArcData, HeaderData);
end;

function RARReadHeaderEx(hArcData: THandle; var HeaderData: RARHeaderDataEx): Integer;
begin
  InitUnrarDLL;
  Result := _RARReadHeaderEx(hArcData, HeaderData);
end;

function RARProcessFile(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer;
begin
  InitUnrarDLL;
  Result := _RARProcessFile(hArcData, Operation, DestPath, DestName);
end;

procedure RARSetCallback(hArcData: THandle; UnrarCallback: TUnrarCallback; UserData: Integer);
begin
  InitUnrarDLL;
  _RARSetCallback(hArcData, UnrarCallback, UserData);
end;

procedure RARSetPassword(hArcData: THandle; Password: PChar);
begin
  InitUnrarDLL;
  _RARSetPassword(hArcData, Password);
end;

//procedure RARSetChangeVolProc(hArcData: THandle; ChangeVolProc: TChangeVolProc);
//begin
//  InitUnrarDLL;
//  _RARSetChangeVolProc(hArcData, ChangeVolProc);
//end;

//procedure RARSetProcessDataProc(hArcData: THandle; ProcessDataProc: TProcessDataProc);
//begin
//  InitUnrarDLL;
//  _RARSetProcessDataProc(hArcData, ProcessDataProc);
//end;

procedure UnloadUnrarDLL;
begin
  _RAROpenArchive := nil;
  _RAROpenArchiveEx := nil;
  _RARCloseArchive := nil;
  _RARReadHeader := nil;
  _RARReadHeaderEx := nil;
  _RARProcessFile := nil;
  _RARSetCallback := nil;
  _RARSetPassword := nil;
  FreeLibrary(_hunrardll);
end;

initialization

finalization
  UnloadUnrarDLL;

end.
