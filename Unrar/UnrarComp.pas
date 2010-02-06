unit UnrarComp;

interface

uses Windows, SysUtils, Classes, UnRAR, Masks;

type
  TUnrar = class;
  TRarEntries = class;

  ERARException = class(Exception);
  ERAROpenException = class(ERARException);
  ERARListException = class(ERARException);

  THostOS = (hosMSDOS, hosOS2, hosWin32, hosUnix);
  TDictionnarySize = (ds64, ds128, ds256, ds512, ds1024, ds2048, ds4096, dsDir);

  TRarEntry = class(TCollectionItem)
  private
    aRARHeaderData: RARHeaderData;
    function GetFileName: string;
    function GetArcName: string;
    function GetCmt: string;
    function GetCmtState: UINT;
    function GetFileAttr: UINT;
    function GetFileCRC: UINT;
    function GetFileTime: TDateTime;
    function GetHostOS: THostOS;
    function GetMethod: UINT;
    function GetPachSize: UINT;
    function GetUnpackSize: UINT;
    function GetUnpackVersion: UINT;
    function GetFileCRCstr: string;
    function GetCommentPresent: Boolean;
    function GetContFromPrevVol: Boolean;
    function GetContOnNextVol: Boolean;
    function GetDictionnarySize: TDictionnarySize;
    function GetEncWithPassword: Boolean;
    function GetSolidFlag: Boolean;
    function GetIsDirectory: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArcName: string read GetArcName;
    property FileName: string read GetFileName;
    property CmtState: UINT read GetCmtState;
    property Cmt: string read GetCmt;
    property PackSize: UINT read GetPachSize;
    property UnpackSize: UINT read GetUnpackSize;
    property HostOS: THostOS read GetHostOS;
    property FileCRC: UINT read GetFileCRC;
    property FileCRCstr: string read GetFileCRCstr;
    property FileTime: TDateTime read GetFileTime;
    property UnpackVersion: UINT read GetUnpackVersion;
    property Method: UINT read GetMethod;
    property FileAttr: UINT read GetFileAttr;
    property ContFromPrevVol: Boolean read GetContFromPrevVol;
    property ContOnNextVol: Boolean read GetContOnNextVol;
    property EncWithPassword: Boolean read GetEncWithPassword;
    property CommentPresent: Boolean read GetCommentPresent;
    property SolidFlag: Boolean read GetSolidFlag;
    property DictionnarySize: TDictionnarySize read GetDictionnarySize;
    property IsDirectory: Boolean read GetIsDirectory;
  end;

  TRarEntries = class(TCollection)
  private
    FUnrar: TUnrar;
    function GetItem(Index: Integer): TRarEntry;
    procedure SetItem(Index: Integer; Value: TRarEntry);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Unrar: TUnrar);
    function Add: TRarEntry;
    function Insert(Index: Integer): TRarEntry;
    property Items[Index: Integer]: TRarEntry read GetItem write SetItem; default;
  end;

  PRAROpenArchiveDataEx = ^RAROpenArchiveDataEx;

  TRarArchive = class
    FRAROpenArchiveEx: RAROpenArchiveDataEx;
  private
    function GetArcName: string;
    procedure SetArcName(const Value: string);
    function GetOpenMode: Integer;
    procedure SetOpenMode(const Value: Integer);
    function GetOpenResult: Integer;
    function GetCmt: string;
    function GetCmtState: Integer;
    function GetArcNameW: WideString;
    procedure SetArcNameW(const Value: WideString);
    function GetFirstVolume: Boolean;
    function GetBlockHeadersEncrypted: Boolean;
    function GetRecoveryRecord: Boolean;
    function GetAuthenticityInformation: Boolean;
    function GetNewVolumeNamingScheme: Boolean;
    function GetSolidArchive: Boolean;
    function GetLockArchive: Boolean;
    function GetArchiveComment: Boolean;
    function GetArchiveVolume: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function RAROpenArchiveEx: PRAROpenArchiveDataEx;
    property ArcName: string read GetArcName write SetArcName;
    property ArcNameW: WideString read GetArcNameW write SetArcNameW;
    property OpenMode: Integer read GetOpenMode write SetOpenMode;
    property OpenResult: Integer read GetOpenResult;
    property CmtState: Integer read GetCmtState;
    property Cmt: string read GetCmt;
    property ArchiveVolume: Boolean read GetArchiveVolume;
    property ArchiveComment: Boolean read GetArchiveComment;
    property LockArchive: Boolean read GetLockArchive;
    property SolidArchive: Boolean read GetSolidArchive;
    property NewVolumeNamingScheme: Boolean read GetNewVolumeNamingScheme; // ('volname.partN.rar')
    property AuthenticityInformation: Boolean  read GetAuthenticityInformation;
    property RecoveryRecord: Boolean read GetRecoveryRecord;
    property BlockHeadersEncrypted: Boolean read GetBlockHeadersEncrypted;
    property FirstVolume: Boolean read GetFirstVolume; // (set only by RAR 3.0 and later)
  end;

  TRAROnProcessData = procedure (Sender: TUnrar; CurrentEntry: TRarEntry; CurrentPosition: PChar; SizeToProcess: Integer; var Continue: Boolean) of object;
  TRAROnNeedPassword = procedure (Sender: TUnrar; CurrentEntry: TRarEntry; var NewPassword: string; var Continue: Boolean) of object;
  TRAROnNotifyChangeVolume = procedure (Sender: TUnrar; CurrentEntry: TRarEntry; NewVolumeName: string; var Continue: Boolean) of object;
  TRAROnNotifyRequiredVolume = procedure (Sender: TUnrar; CurrentEntry: TRarEntry; var NewVolumeName: string; var Continue: Boolean) of object;
  TRAROnUnknownCallbackMsg = procedure (Sender: TUnrar; CurrentEntry: TRarEntry; Msg, P1, P2: Integer; var Continue: Boolean) of object;

  TUnrar = class(TComponent)
  private
    FRarHdl: THandle;
    FRarFile: string;
    FRarEntries: TRarEntries;
    FRarArchive: TRarArchive;
    FPathMask: string;
    FFileMask: string;
    FPathDest: string;
    FUsePath: Boolean;
    FCurrentEntry: TRarEntry;
    FOnProcessData: TRAROnProcessData;
    FPassword: string;
    FOnNeedPassword: TRAROnNeedPassword;
    FOnNotifyChangeVolume: TRAROnNotifyChangeVolume;
    FOnNotifyRequiredVolume: TRAROnNotifyRequiredVolume;
    FOnUnknownCallbackMsg: TRAROnUnknownCallbackMsg;
    procedure SetRarFile(const Value: string);
    function GetActive: Boolean;
    procedure CheckInactive;
    procedure Open(OpenMode: Integer);
    procedure ProcessList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure List;
    procedure Extract;
    procedure Close;
    function GetDllVersion: Integer;
    property RarEntries: TRarEntries read FRarEntries;
    property RarArchive: TRarArchive read FRarArchive;
  published
    property Active: Boolean read GetActive;
    property RarFile: string read FRarFile write SetRarFile;
    property Password: string read FPassword write FPassword;
    property FileMask: string read FFileMask write FFileMask;
    property PathMask: string read FPathMask write FPathMask;
    property PathDest: string read FPathDest write FPathDest;
    property UsePath: Boolean read FUsePath write FUsePath;
    property OnProcessData: TRAROnProcessData read FOnProcessData write FOnProcessData;
    property OnNeedPassword: TRAROnNeedPassword read FOnNeedPassword write FOnNeedPassword;
    property OnNotifyChangeVolume: TRAROnNotifyChangeVolume read FOnNotifyChangeVolume write FOnNotifyChangeVolume;
    property OnNotifyRequiredVolume: TRAROnNotifyRequiredVolume read FOnNotifyRequiredVolume write FOnNotifyRequiredVolume;
    property OnUnknownCallbackMsg: TRAROnUnknownCallbackMsg read FOnUnknownCallbackMsg write FOnUnknownCallbackMsg;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TUnrar]);
end;

{ TUnrar }

function UnrarCallback(Msg: UINT; UserData, P1, P2: Integer): Integer; stdcall;
var
  NextVolumeName: string;
  CanContinue: Boolean;
  NewPassword: string;
begin
  case Msg of
    UCM_CHANGEVOLUME: //     Process volume change.
      begin
        Result := -1;
        case P2 of
          RAR_VOL_ASK:
            begin
              CanContinue := False;
              NextVolumeName := StrPas(PChar(P1));
              if Assigned(TUnrar(UserData).FOnNotifyRequiredVolume) then
                TUnrar(UserData).FOnNotifyRequiredVolume(TUnrar(UserData), TUnrar(UserData).FCurrentEntry, NextVolumeName, CanContinue);
              StrPCopy(PChar(P1), NextVolumeName);
              if CanContinue then Result := 1
                             else Result := -1;
            end;
          RAR_VOL_NOTIFY:
            begin
              CanContinue := True;
              if Assigned(TUnrar(UserData).FOnNotifyChangeVolume) then
                TUnrar(UserData).FOnNotifyChangeVolume(TUnrar(UserData), TUnrar(UserData).FCurrentEntry, StrPas(PChar(P1)), CanContinue);
              if CanContinue then Result := 1
                             else Result := -1;
            end;
        end;
      end;
    UCM_PROCESSDATA:
      begin
        CanContinue := True;
        if Assigned(TUnrar(UserData).FOnProcessData) then
          TUnrar(UserData).FOnProcessData(TUnrar(UserData), TUnrar(UserData).FCurrentEntry, PChar(P1), P2, CanContinue);
        if CanContinue then Result := 1
                       else Result := -1;
      end;
    UCM_NEEDPASSWORD:
      begin
        NewPassword := '';
        if Assigned(TUnrar(UserData).FOnNeedPassword) then begin
          CanContinue := True;
          TUnrar(UserData).FOnNeedPassword(TUnrar(UserData), TUnrar(UserData).FCurrentEntry, NewPassword, CanContinue);
          StrPLCopy(PChar(P1), NewPassword, P2);
        end else
          CanContinue := False;
        if CanContinue then Result := 1
                       else Result := -1;
      end;
    else begin
      begin
        CanContinue := True;
        if Assigned(TUnrar(UserData).FOnUnknownCallbackMsg) then
          TUnrar(UserData).FOnUnknownCallbackMsg(TUnrar(UserData), TUnrar(UserData).FCurrentEntry, Msg, P1, P2, CanContinue);
        if CanContinue then Result := 1
                       else Result := -1;
      end;
    end;
  end;
end;

procedure TUnrar.CheckInactive;
begin
  if Active then Close;
end;

procedure TUnrar.Close;
begin
  RARCloseArchive(FRarHdl);
  RarEntries.Clear;
  FRarHdl := 0;
end;

constructor TUnrar.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentEntry := nil;
  FRarHdl := 0;
  FRarArchive := TRarArchive.Create;
  FRarEntries := TRarEntries.Create(Self);
  FUsePath := True;
  FPathMask := '';
  FFileMask := '';
  FPathDest := '';
  FPassword := '';
end;

destructor TUnrar.Destroy;
begin
  CheckInactive;
  FreeAndNil(FCurrentEntry);
  FreeAndNil(FRarEntries);
  FreeAndNil(FRarArchive);
  inherited;
end;

procedure TUnrar.Extract;
begin
  Open(RAR_OM_EXTRACT);
  ProcessList;
end;

function TUnrar.GetActive: Boolean;
begin
  Result := FRarHdl <> 0;
end;

function TUnrar.GetDllVersion: Integer;
begin
  Result := RARGetDllVersion;
end;

procedure TUnrar.List;
begin
  Open(RAR_OM_LIST);
  ProcessList;
end;

procedure TUnrar.Open(OpenMode: Integer);
begin
  CheckInactive;
  FRarArchive.ArcName := FRarFile;
  FRarArchive.OpenMode := OpenMode;
  FRarHdl := RAROpenArchiveEx(FRarArchive.RAROpenArchiveEx^);
  if FRarHdl = 0 then
    case FRarArchive.OpenResult of
      ERAR_NO_MEMORY: raise ERAROpenException.Create('Not enough memory to initialize data structures');
      ERAR_BAD_DATA: raise ERAROpenException.Create('Archive header broken');
      ERAR_BAD_ARCHIVE: raise ERAROpenException.Create('File is not valid RAR archive');
      ERAR_EOPEN: raise ERAROpenException.Create('File open error');
      else raise ERAROpenException.CreateFmt('Unexpected error: %d', [FRarArchive.OpenResult]);
    end;
  if FPassword <> '' then RARSetPassword(FRarHdl, PChar(FPassword));
  RARSetCallback(FRarHdl, UnrarCallback, Integer(Self));  
end;

procedure TUnrar.ProcessList;
var
  i: Integer;
  FileName: string;
  FileGood: Boolean;
  PathName: string;
  PathGood: Boolean;
  Op: Integer;
begin
  try
    FPathDest := IncludeTrailingPathDelimiter(FPathDest);
    FRarEntries.Clear;

    i := 0;
    Op := RAR_SKIP;
    FileName := '';
    while i = 0 do begin
      FCurrentEntry := RarEntries.Add;
      case RARReadHeader(FRarHdl, FCurrentEntry.aRARHeaderData) of
        0:
          begin
            if FCurrentEntry.IsDirectory then begin
              PathName := FCurrentEntry.FileName;
              FileName := '';
            end else begin
              PathName := ExcludeTrailingPathDelimiter(ExtractFilePath(FCurrentEntry.FileName));
              FileName := Trim(ExtractFileName(FCurrentEntry.FileName));
            end;
            PathGood := (PathName = '') or (PathMask = '') or (MatchesMask(PathName, PathMask));
            FileGood := (FileName = '') or (FileMask = '') or (MatchesMask(FileName, FileMask));
            if (FileGood and PathGood) then
              Op := RAR_EXTRACT
            else begin
              FreeAndNil(FCurrentEntry);
              Op := RAR_SKIP;
            end;
          end;
        ERAR_END_ARCHIVE:
          begin
            FreeAndNil(FCurrentEntry);
            Break; //      End of archive
          end;
        ERAR_BAD_DATA: raise ERARListException.Create('File header broken');
      end;

      if FUsePath then i := RARProcessFile(FRarHdl, Op, PChar(FPathDest), nil)
                  else i := RARProcessFile(FRarHdl, Op, PChar(FPathDest), PChar(FPathDest + FileName));
      case i of
        0: ; //                   Success
        ERAR_BAD_DATA: raise ERARListException.Create('File CRC error');
        ERAR_BAD_ARCHIVE: raise ERARListException.Create('Volume is not valid RAR archive');
        ERAR_UNKNOWN_FORMAT: raise ERARListException.Create('Unknown archive format');
        ERAR_EOPEN: raise ERARListException.Create('Volume open error');
        ERAR_ECREATE: raise ERARListException.Create('File create error');
        ERAR_ECLOSE: raise ERARListException.Create('File close error');
        ERAR_EREAD: raise ERARListException.Create('Read error');
        ERAR_EWRITE: raise ERARListException.Create('Write error');
        else raise ERARListException.CreateFmt('Unexpected error: %d', [i]);
      end;
    end;
  finally
    FCurrentEntry := nil;
  end;
end;

procedure TUnrar.SetRarFile(const Value: string);
begin
  CheckInactive;
  FRarFile := Value;
end;

{ TRarEntries }

function TRarEntries.Add: TRarEntry;
begin
  Result := TRarEntry(inherited Add);
end;

constructor TRarEntries.Create(Unrar: TUnrar);
begin
  inherited Create(TRarEntry);
  FUnrar := Unrar;
end;

function TRarEntries.GetItem(Index: Integer): TRarEntry;
begin
  Result := TRarEntry(inherited GetItem(Index));
end;

function TRarEntries.GetOwner: TPersistent;
begin
  Result := FUnrar;
end;

function TRarEntries.Insert(Index: Integer): TRarEntry;
begin
  Result := Add;
  Result.Index := Index;
end;

procedure TRarEntries.SetItem(Index: Integer; Value: TRarEntry);
begin
  inherited SetItem(Index, Value);
end;

{ TRarEntry }

procedure TRarEntry.Assign(Source: TPersistent);
begin
  if Source is TRarEntry then begin
    CopyMemory(@aRARHeaderData, @TRarEntry(Source).aRARHeaderData, SizeOf(RARHeaderData));
  end
  else inherited Assign(Source);
end;

constructor TRarEntry.Create(Collection: TCollection);
begin
  inherited;
  aRARHeaderData.CmtBufSize := 65535;
  GetMem(aRARHeaderData.CmtBuf, aRARHeaderData.CmtBufSize);
end;

destructor TRarEntry.Destroy;
begin
  FreeMem(aRARHeaderData.CmtBuf);
  inherited;
end;

function TRarEntry.GetArcName: string;
begin
  Result := StrPas(aRARHeaderData.ArcName);
end;

function TRarEntry.GetCmt: string;
begin
  Result := StrPas(aRARHeaderData.CmtBuf);
end;

function TRarEntry.GetCmtState: UINT;
begin
  Result := aRARHeaderData.CmtState;
end;

function TRarEntry.GetCommentPresent: Boolean;
begin
  Result := LongBool(aRARHeaderData.Flags and $08);
end;

function TRarEntry.GetContFromPrevVol: Boolean;
begin
  Result := LongBool(aRARHeaderData.Flags and $01);
end;

function TRarEntry.GetContOnNextVol: Boolean;
begin
  Result := LongBool(aRARHeaderData.Flags and $02);
end;

function TRarEntry.GetDictionnarySize: TDictionnarySize;

  function TestFlag(Value: Cardinal): Boolean;
  begin
    Result := (aRARHeaderData.Flags and Value) = Value;
  end;

begin
  Result := ds64;
  if TestFlag(              $20) then Result := ds128;
  if TestFlag(       $40       ) then Result := ds256;
  if TestFlag(       $40 or $20) then Result := ds512;
  if TestFlag($80              ) then Result := ds1024;
  if TestFlag($80        or $20) then Result := ds2048;
  if TestFlag($80 or $40       ) then Result := ds4096;
  if TestFlag($80 or $40 or $20) then Result := dsDir;
end;

function TRarEntry.GetDisplayName: string;
begin
  Result := GetFileName;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TRarEntry.GetEncWithPassword: Boolean;
begin
  Result := LongBool(aRARHeaderData.Flags and $04);
end;

function TRarEntry.GetFileAttr: UINT;
begin
  Result := aRARHeaderData.FileAttr;
end;

function TRarEntry.GetFileCRC: UINT;
begin
  Result := aRARHeaderData.FileCRC;
end;

function TRarEntry.GetFileCRCstr: string;
begin
  Result := IntToHex(FileCRC, 8);
end;

function TRarEntry.GetFileName: string;
begin
  Result := StrPas(aRARHeaderData.FileName);
end;

function TRarEntry.GetFileTime: TDateTime;
begin
  Result := FileDateToDateTime(aRARHeaderData.FileTime);
end;

function TRarEntry.GetHostOS: THostOS;
begin
  Result := THostOS(aRARHeaderData.HostOS);
end;

function TRarEntry.GetIsDirectory: Boolean;
begin
  Result := (aRARHeaderData.FileAttr and faDirectory) = faDirectory;
end;

function TRarEntry.GetMethod: UINT;
begin
  Result := aRARHeaderData.Method;
end;

function TRarEntry.GetPachSize: UINT;
begin
  Result := aRARHeaderData.PackSize;
end;

function TRarEntry.GetSolidFlag: Boolean;
begin
  Result := LongBool(aRARHeaderData.Flags and $10);
end;

function TRarEntry.GetUnpackSize: UINT;
begin
  Result := aRARHeaderData.UnpSize;
end;

function TRarEntry.GetUnpackVersion: UINT;
begin
  Result := aRARHeaderData.UnpVer;
end;

{ TRarDescription }

constructor TRarArchive.Create;
begin
  GetMem(FRAROpenArchiveEx.ArcName, MAX_PATH);
  GetMem(FRAROpenArchiveEx.ArcNameW, MAX_PATH * 2);
  FRAROpenArchiveEx.CmtBufSize := 65535;
  GetMem(FRAROpenArchiveEx.CmtBuf, FRAROpenArchiveEx.CmtBufSize);
end;

destructor TRarArchive.Destroy;
begin
  FreeMem(FRAROpenArchiveEx.ArcName, MAX_PATH);
  FreeMem(FRAROpenArchiveEx.ArcNameW, MAX_PATH);
  FreeMem(FRAROpenArchiveEx.CmtBuf);
  inherited;
end;

function TRarArchive.GetArchiveComment: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $02);
end;

function TRarArchive.GetArchiveVolume: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $01);
end;

function TRarArchive.GetArcName: string;
begin
  Result := StrPas(FRAROpenArchiveEx.ArcName);
end;

function TRarArchive.GetArcNameW: WideString;
begin
  Result := WideCharToString(FRAROpenArchiveEx.ArcNameW);
end;

function TRarArchive.GetAuthenticityInformation: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $20);
end;

function TRarArchive.GetBlockHeadersEncrypted: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $80);
end;

function TRarArchive.GetCmt: string;
begin
  Result := StrPas(FRAROpenArchiveEx.CmtBuf);
end;

function TRarArchive.GetCmtState: Integer;
begin
  Result := FRAROpenArchiveEx.CmtState;
end;

function TRarArchive.GetFirstVolume: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $100);
end;

function TRarArchive.GetLockArchive: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $04);
end;

function TRarArchive.GetNewVolumeNamingScheme: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $10);
end;

function TRarArchive.GetOpenMode: Integer;
begin
  Result := FRAROpenArchiveEx.OpenMode;
end;

function TRarArchive.GetOpenResult: Integer;
begin
  Result := FRAROpenArchiveEx.OpenResult;
end;

function TRarArchive.GetRecoveryRecord: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $40);
end;

function TRarArchive.GetSolidArchive: Boolean;
begin
  Result := LongBool(FRAROpenArchiveEx.Flags and $08);
end;

function TRarArchive.RAROpenArchiveEx: PRAROpenArchiveDataEx;
begin
  Result := @FRAROpenArchiveEx;
end;

procedure TRarArchive.SetArcName(const Value: string);
begin
  StrPCopy(FRAROpenArchiveEx.ArcName, Value);
  StringToWideChar(Value, FRAROpenArchiveEx.ArcNameW, MAX_PATH * 2);
end;

procedure TRarArchive.SetArcNameW(const Value: WideString);
begin
  StrPCopy(FRAROpenArchiveEx.ArcName, Value);
  StringToWideChar(Value, FRAROpenArchiveEx.ArcNameW, MAX_PATH * 2);
end;

procedure TRarArchive.SetOpenMode(const Value: Integer);
begin
  FRAROpenArchiveEx.OpenMode := Value;
end;

end.
