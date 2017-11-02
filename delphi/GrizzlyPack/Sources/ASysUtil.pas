unit ASysUtil;

interface

uses Windows, SysUtils, Classes, ShellAPI;

type
  EFileMapError = class(Exception);
  TFileMapStream = class(TCustomMemoryStream)
  private
     FHandle: Integer;
     FHandleOwned: Boolean;
  public
     constructor Create(var FileId: string; Size: Integer);
     destructor Destroy; override;
     {}
     function Write(const Buffer; Count: LongInt): LongInt; override;
     property HandleOwned: Boolean read FHandleOwned;
  end;

function ExecCommandLine(CommandLine: string; AShowWindow : Word = SW_SHOWNORMAL; ADirectory: string = ''; SamePriority : Boolean = True): TProcessInformation;
function ExecAndWait(CommandLine: string; MsWaitLimit: Integer; ADirectory: string = ''; AShowWindow : Word = SW_SHOWNORMAL; SamePriority : Boolean = True): Boolean;
function ExecAndWaitOrKill(CommandLine: string; MsWaitLimit: Integer; ADirectory: string = ''; SamePriority : Boolean = True): Boolean;
function ShellExecAndWait(AWnd : hWnd; AOperation : string; AFileName : string; MsWaitLimit : Integer) : Boolean;
function ShowFileProperties(AFilename : string; AWnd: HWND): Boolean;

//This is "stream" for "NTFS stream" (syntax : filename:streamname)
function StreamExists(const AFileName, AStreamName : string) : Boolean;

implementation

uses AUtils;

constructor TFileMapStream.Create(var FileId: string; Size: Integer);
var P: Pointer;
begin
  FileId:= RemoveChars(FileId, ['\']);
  if Length(FileId) > MAX_PATH then
    SetLength(FileId, MAX_PATH);
  FHandle:= CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size, PChar(FileId));
  FHandleOwned:= GetLastError <> ERROR_ALREADY_EXISTS;
  if FHandle = 0 then raise EFileMapError.Create('Erreur ' + IntToStr(GetLastError) + ' : '#13#10
     + 'Impossible de créer un FileMapping nommé "' + FileId + '" !!!');
  P:= MapViewOfFile(FHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if P = nil then
  begin
    Size:= GetLastError;
    if HandleOwned then
      CloseHandle(FHandle);
    raise EFileMapError.Create('Erreur ' + IntToStr(Size) + ' : '#13#10
      + 'Impossible de lire le FileMapping nommé "' + FileId + '" !!!');
  end;
  SetPointer(P, Size);
end;

function TFileMapStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result:= IMin(Count, Size - Position);
  Move(Buffer, IncPointer(Memory, Position)^, Result);
  Seek(Count, soFromCurrent);
end;

destructor TFileMapStream.Destroy;
begin
  UnMapViewOfFile(Memory);
  CloseHandle(FHandle);
  inherited Destroy;
end;

  { Autres fonctions }

function ExecCommandLine(CommandLine: string; AShowWindow : Word = SW_SHOWNORMAL; ADirectory: string = ''; SamePriority : Boolean = True): TProcessInformation;
var
  StartUpInfos: TStartUpInfo;
begin
  StartUpInfos.cb:= SizeOf(StartUpInfos);
  StartUpInfos.lpReserved:= nil;
  StartUpInfos.lpDesktop:= nil;
  StartUpInfos.lpTitle:= nil;
  StartUpInfos.dwFlags:= STARTF_USESHOWWINDOW;
  StartUpInfos.cbReserved2:= 0;
  StartUpInfos.lpReserved2:= nil;
  StartUpInfos.wShowWindow:= AShowWindow;
  if not CreateProcess(nil, PChar(CommandLine), nil, nil,
    False, CREATE_DEFAULT_ERROR_MODE + CREATE_NEW_CONSOLE + NORMAL_PRIORITY_CLASS,
    nil, PChar(ADirectory), StartUpInfos, Result) then
      FillChar(Result, SizeOf(Result), 0) // if failure, result is 0
  else
    if SamePriority then
      SetPriorityClass(Result.hProcess, GetPriorityClass(GetCurrentProcess));
end;

function ExecAndWait(CommandLine: string; MsWaitLimit: Integer; ADirectory: string = '';
  AShowWindow : Word = SW_SHOWNORMAL; SamePriority : Boolean = True): Boolean;
var H: Integer;
    L: DWORD;
begin
  H:= ExecCommandLine(CommandLine, AShowWindow, ADirectory, SamePriority).hProcess;
  if H <> 0 then
  begin
    if MsWaitLimit < 0 then
      L:= INFINITE
    else
      L:= MsWaitLimit;
    Result:= WaitForSingleObject(H, L) = WAIT_OBJECT_0;
  end else
    Result:= False;
end;

{ FG 08/2000}
function ExecAndWaitOrKill(CommandLine: string; MsWaitLimit: Integer; ADirectory: string = ''; SamePriority : Boolean = True): Boolean;
var H: Integer;
    L: DWORD;
begin
  H:= ExecCommandLine(CommandLine, SW_SHOWNORMAL, ADirectory, SamePriority).hProcess;
  if H <> 0 then
  begin
    if MsWaitLimit < 0 then
      L:= INFINITE
    else
      L:= MsWaitLimit;
    if WaitForSingleObject(H, L) = WAIT_TIMEOUT then
      TerminateProcess(H, WAIT_TIMEOUT);
    Result:= True;
  end else
    Result:= False;
end;

function ShellExecAndWait(AWnd : hWnd; AOperation : string; AFileName : string; MsWaitLimit : Integer) : Boolean;
var
  H: Integer;
  //L: DWORD;
  ShExInfo : TShellExecuteInfo;
  ADirectory : string;
begin
  Result := False;
  FillChar(ShExInfo, SizeOf(ShExInfo), 0);
  ShExInfo.cbSize := SizeOf(ShExInfo);
  ShExInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ShExInfo.Wnd := AWnd;
  ShExInfo.lpVerb := PChar(AOperation);
  ShExInfo.lpFile := PChar(AFileName);
  ADirectory := ExtractFilePath(AFileName);
  ShExInfo.lpDirectory := PChar(ADirectory);
  ShExInfo.nShow := SW_SHOWNORMAL;
  if ShellExecuteEx(@ShExInfo) then
  begin
    H := ShExInfo.hProcess;
    if  H <> 0 then
    begin
      WaitForSingleObject(H, MsWaitLimit);
      Result := True;
    end;
  end
end;

function ShowFileProperties(AFilename : string; AWnd: HWND): Boolean;
var
  sei : ShellAPI.TShellExecuteInfo;
begin
  sei.cbSize := sizeof(sei);
  sei.fMask := SEE_MASK_NOCLOSEPROCESS OR SEE_MASK_INVOKEIDLIST OR SEE_MASK_FLAG_NO_UI;
  sei.wnd := AWnd;
  sei.lpVerb := PChar('properties');
  sei.lpFile := PChar(AFilename);
  sei.lpParameters := nil;
  sei.lpDirectory := nil;
  sei.nShow := 0;
  sei.hInstApp := 0;
  sei.lpIDList := nil;
  Result := ShellAPI.ShellExecuteEx(@sei);
end;

function StreamExists(const AFileName, AStreamName : string) : Boolean;
var
  hFile : THandle;
  AString : string;
begin
  AString := AFileName + ':' + AStreamName;
  hFile := CreateFile(PChar(AString), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if hFile = INVALID_HANDLE_VALUE then
    Result := False
  else
  begin
    CloseHandle(hFile);
    Result := True;
  end;
end;


end.
