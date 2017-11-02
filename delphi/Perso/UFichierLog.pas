unit UFichierLog;

interface

uses
{$IFDEF DEBUG} FastMM4, {$ENDIF}
  Windows, SysUtils, Classes, StrUtils, SyncObjs;

type
  TTypeMessage = (tmErreurCritique, tmErreur, tmTraitement, tmInfo, tmDebugLite, tmDebugFull);
  TLogRotation = (lrMinute, lrHour, lrDay, lrMonth, lrYear);
  TLogOption = (loIncludeInstanceID, loIncludeCallStackOnError);
  TLogOptions = set of TLogOption;
  TLogFileOption = (lfoAutoClose, lfoKeepBackup);
  TLogFileOptions = set of TLogFileOption;

  TOnAppendLogEvent = procedure(const FullText, SmallText: string; TypeMessage: TTypeMessage; AddedToFile: Boolean) of object;
  TOnBuildMessageEvent = procedure(var Prefix, Message, Suffix: string; TypeMessage: TTypeMessage) of object;

  TFichierLog = class sealed
  strict private
    class var _Instance: TFichierLog;
  public
    class property Instance: TFichierLog read _Instance;
    class constructor Create;
    class destructor Destroy;

  strict private
    FLogCriticalSection, FFileCriticalSection: TCriticalSection;
  private
    FLogFileName, FLogDir: string;
    FWriter: TThread;
    FLogLevel: TTypeMessage;
    FguidInstance: string;
    FLimitSize: Integer;
    FKeepRatio: Integer;
    FBackupDir: string;
    FBackupRotation: TLogRotation;
    FEncoding: TEncoding;
    FOnAppend: TOnAppendLogEvent;
    FLogOptions: TLogOptions;
    FLogFileOptions: TLogFileOptions;
    FArchivingDuration: Integer;
    FOnBuildMessage: TOnBuildMessageEvent;
    procedure SetLimitSize(const Value: Integer);
    function GetLogDir: string;
    procedure SetLogDir(const Value: string);
    function GetLogFileName: string;
    procedure SetLogFileName(const Value: string);
    procedure SetKeepRatio(const Value: Integer);
    procedure SetBackupRotation(const Value: TLogRotation);
    procedure SetEncoding(const Value: TEncoding);
    procedure SetLogOptions(const Value: TLogOptions);
    procedure SetLogFileOptions(const Value: TLogFileOptions);
    function GetLogFullPath: string;
    function GetBackupPath: string;

    procedure CheckLogging;

    constructor Create;
  public
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    function BuildMessage(Texte: string; TypeMessage: TTypeMessage): string;

    procedure AppendLog(const Texte: string; TypeMessage: TTypeMessage); overload;
    procedure AppendLog(E: Exception); overload;
    procedure AppendCallStack;

    property LogFileName: string read GetLogFileName write SetLogFileName;
    property LogDir: string read GetLogDir write SetLogDir;
    property LogLevel: TTypeMessage read FLogLevel write FLogLevel;

    property IdInstance: string read FguidInstance;
    property Encoding: TEncoding read FEncoding write SetEncoding;
    property LogOptions: TLogOptions read FLogOptions write SetLogOptions;
    property LogFileOptions: TLogFileOptions read FLogFileOptions write SetLogFileOptions;

    // LimitSize en octets
    property LimitSize: Integer read FLimitSize write SetLimitSize;
    property KeepRatio: Integer read FKeepRatio write SetKeepRatio;
    property BackupDir: string read FBackupDir write FBackupDir;
    property BackupRotation: TLogRotation read FBackupRotation write SetBackupRotation;
    property ArchivingDuration: Integer read FArchivingDuration write FArchivingDuration;

    property LogFullPath: string read GetLogFullPath;
    property BackupPath: string read GetBackupPath;

    property OnAppend: TOnAppendLogEvent read FOnAppend write FOnAppend;
    property OnBuildMessage: TOnBuildMessageEvent read FOnBuildMessage write FOnBuildMessage;
  end;

implementation

uses
  DateUtils, TypInfo, System.IOUtils, JclCompression, JclDebug, sevenzip;

type
  TLogWriter = class sealed(TThread)
  public
    FFichierLog: TFileStream;

    lockSection: TCriticalSection;
    LstLog: TStringList;
    function isOpen: Boolean;

    function OpenLogFile: Boolean;
    procedure CloseLogFile;
    procedure AddToBackup(Data: TStream);
    procedure CleanBackupDir;
    procedure ForwardStream(Stream: TStream);
    procedure RewindStream(Stream: TStream);
    procedure WriteToLogStream(Stream: TStream; const Text: string);

    procedure AddLigne(const s: string);

    procedure Execute; override;
  end;

  { TLogWriter }

procedure TLogWriter.AddLigne(const s: string);
begin
  lockSection.Acquire;
  try
    LstLog.Add(s);
  finally
    lockSection.Leave;
  end;
end;

procedure TLogWriter.AddToBackup(Data: TStream);
var
  NomFichierBackup, NomArchive, Suffix: string;
  bckStream: TStream;
  Archive: TJclGZipUpdateArchive;
begin
  if TFichierLog.Instance.ArchivingDuration > 0 then
    CleanBackupDir;

  NomFichierBackup := TFichierLog.Instance.LogFileName;
  Suffix := '';
  if TFichierLog.Instance.BackupRotation <= lrYear then
    Suffix := Suffix + Format('_%.4d', [YearOf(Now)]);
  if TFichierLog.Instance.BackupRotation <= lrMonth then
    Suffix := Suffix + Format('_%.2d', [MonthOf(Now)]);
  if TFichierLog.Instance.BackupRotation <= lrDay then
    Suffix := Suffix + Format('_%.2d', [DayOf(Now)]);
  if TFichierLog.Instance.BackupRotation <= lrHour then
    Suffix := Suffix + Format('_%.2d', [HourOf(Now)]);
  if TFichierLog.Instance.BackupRotation <= lrMinute then
    Suffix := Suffix + Format('_%.2d', [MinuteOf(Now)]);

  NomFichierBackup := TPath.GetFileNameWithoutExtension(NomFichierBackup) + Suffix + TPath.GetExtension(NomFichierBackup);
  NomFichierBackup := TPath.Combine(TFichierLog.Instance.BackupPath, NomFichierBackup);
  TDirectory.CreateDirectory(TPath.GetDirectoryName(NomFichierBackup));

  NomArchive := NomFichierBackup + '.gz';

  Archive := TJclGZipUpdateArchive.Create(NomArchive);
  bckStream := TMemoryStream.Create;
  try
    Data.Position := 0;
    if TFile.Exists(NomArchive) then
    begin
      Archive.ListFiles;
      Archive.Items[0].Selected := True;
      Archive.Items[0].Stream := bckStream;
      Archive.Items[0].OwnsStream := False;
      Archive.ExtractSelected;
      bckStream.Seek(0, soFromEnd);
      Data.Seek(Length(TFichierLog.Instance.Encoding.GetPreamble), soFromBeginning);
    end
    else
      Archive.AddFile(TPath.GetFileName(NomFichierBackup), bckStream);

    bckStream.CopyFrom(Data, Data.Size);
    bckStream.Position := 0;

    Archive.Compress;
  finally
    bckStream.Free;
    Archive.Free;
  end;
end;

procedure TLogWriter.CleanBackupDir;
var
  fileName, rootLogFileName: string;
begin
  if not TDirectory.Exists(TFichierLog.Instance.BackupPath) then
    Exit;
  rootLogFileName := TPath.GetFileNameWithoutExtension(TFichierLog.Instance.LogFileName);
  for fileName in TDirectory.GetFiles(TFichierLog.Instance.BackupPath, rootLogFileName + '*.gz', TSearchOption.soTopDirectoryOnly,
    function(const Path: string; const SearchRec: TSearchRec): Boolean
    var
      s: string;
      v: TArray<Word>;
      i: Integer;
      tags: TArray<string>;
      backupDate: TDateTime;
    begin
      try
        s := TPath.GetFileNameWithoutExtension(SearchRec.Name);
        s := TPath.GetFileNameWithoutExtension(s).Substring(Length(rootLogFileName) + 1);
        SetLength(v, 5);
        v[0] := YearOf(Now);
        v[1] := MonthOf(Now);
        v[2] := DayOf(Now);
        v[3] := HourOf(Now);
        v[4] := MinuteOf(Now);
        tags := s.Split(['_']);
        for i := 0 to Pred(Length(tags)) do
          v[i] := StrToInt(tags[i]);
        backupDate := EncodeDateTime(v[0], v[1], v[2], v[3], v[4], 0, 0);
        case TFichierLog.Instance.BackupRotation of
          lrMinute:
            Exit(MinutesBetween(backupDate, Now) > TFichierLog.Instance.ArchivingDuration);
          lrHour:
            Exit(HoursBetween(backupDate, Now) > TFichierLog.Instance.ArchivingDuration);
          lrDay:
            Exit(DaysBetween(backupDate, Now) > TFichierLog.Instance.ArchivingDuration);
          lrMonth:
            Exit(MonthsBetween(backupDate, Now) > TFichierLog.Instance.ArchivingDuration);
          lrYear:
            Exit(YearsBetween(backupDate, Now) > TFichierLog.Instance.ArchivingDuration);
        end;
        Exit(False);
      except
        // si le nom de fichier n'est pas décodable, c'est qu'il n'est pas un fichier de backup
        Result := False;
      end;
    end) do
    TFile.Delete(fileName);
end;

procedure TLogWriter.CloseLogFile;
begin
  if not Assigned(FFichierLog) then
    Exit;

  FreeAndNil(FFichierLog);
end;

procedure TLogWriter.Execute;
var
  dumpLst: TStringList;
  s: string;
begin
  lockSection := TCriticalSection.Create;
  LstLog := TStringList.Create;
  dumpLst := TStringList.Create;
  try
    while not Terminated do
    begin
      lockSection.Acquire;
      try
        if LstLog.Count > 0 then
        begin
          dumpLst.Assign(LstLog);
          LstLog.Clear;
        end;
      finally
        lockSection.Leave;
      end;

      if dumpLst.Count > 0 then
        try
          for s in dumpLst do
            if OpenLogFile then
              WriteToLogStream(FFichierLog, s + #13#10);
        finally
          dumpLst.Clear;
          if lfoAutoClose in TFichierLog.Instance.LogFileOptions then
            CloseLogFile;
        end;

      Sleep(500);
    end;
  finally
    CloseLogFile;
    dumpLst.Free;
    LstLog.Free;
    lockSection.Free;
  end;
end;

procedure TLogWriter.ForwardStream(Stream: TStream);
var
  c: AnsiChar;
begin
  Stream.Read(c, 1);
  while (Stream.Position < Stream.Size) and not CharInSet(c, [#10, #13]) do
    Stream.Read(c, 1);
  while (Stream.Position < Stream.Size) and CharInSet(c, [#10, #13]) do
    Stream.Read(c, 1);
  Stream.Seek(-1, soFromCurrent);
end;

function TLogWriter.isOpen: Boolean;
begin
  Result := Assigned(FFichierLog);
end;

function TLogWriter.OpenLogFile: Boolean;
var
  tmpStream, bckStream: TStream;
  cutPos: Integer;
  NomFichierLog: string;
  MaxFileSize: Cardinal;
begin
  MaxFileSize := TFichierLog.Instance.LimitSize;
  Result := Assigned(FFichierLog);
  NomFichierLog := TFichierLog.Instance.GetLogFullPath;
  if not Result then
  begin
    if TFile.Exists(NomFichierLog) then
      FFichierLog := TFileStream.Create(NomFichierLog, fmOpenReadWrite or fmShareDenyWrite)
    else
    begin
      TDirectory.CreateDirectory(TPath.GetDirectoryName(NomFichierLog));
      FFichierLog := TFileStream.Create(NomFichierLog, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
    end;

    Result := True;
  end;
  if (MaxFileSize > 0) and (FFichierLog.Size > MaxFileSize) then
  begin
    bckStream := TMemoryStream.Create;
    tmpStream := TMemoryStream.Create;
    try
      FFichierLog.Position := FFichierLog.Size - MulDiv(MaxFileSize, 100 - TFichierLog.Instance.KeepRatio, 100);
      ForwardStream(FFichierLog);

      cutPos := FFichierLog.Position;

      // on copie le BOM en même temps s'il est présent
      FFichierLog.Position := 0;
      bckStream.CopyFrom(FFichierLog, cutPos);

      // FFichierLog a été repositionné sur cutPos
      tmpStream.CopyFrom(FFichierLog, FFichierLog.Size - cutPos);

      FFichierLog.Size := Length(TFichierLog.Instance.Encoding.GetPreamble);
      FFichierLog.CopyFrom(tmpStream, -1);

      if lfoKeepBackup in TFichierLog.Instance.LogFileOptions then
        AddToBackup(bckStream);
    finally
      tmpStream.Free;
      bckStream.Free;
    end;
  end;

  FFichierLog.Seek(0, soFromEnd);
end;

procedure TLogWriter.RewindStream(Stream: TStream);
var
  c: AnsiChar;
begin
  Stream.Seek(-1, soFromCurrent);
  Stream.Read(c, 1);
  while (Stream.Position > 1) and CharInSet(c, [#10, #13]) do
  begin
    Stream.Seek(-2, soFromCurrent);
    Stream.Read(c, 1);
  end;
  while (Stream.Position > 1) and not CharInSet(c, [#10, #13]) do
  begin
    Stream.Seek(-2, soFromCurrent);
    Stream.Read(c, 1);
  end;
end;

procedure TLogWriter.WriteToLogStream(Stream: TStream; const Text: string);
var
  Buffer, Preamble: TBytes;
begin
  Buffer := TFichierLog.Instance.Encoding.GetBytes(Text);
  if Stream.Size = 0 then
  begin
    Preamble := TFichierLog.Instance.Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

{ TFichierLog }

procedure TFichierLog.SetLimitSize(const Value: Integer);
begin
  FLimitSize := Value;
end;

procedure TFichierLog.AppendLog(const Texte: string; TypeMessage: TTypeMessage);
var
  s: string;
begin
  s := BuildMessage(Texte, TypeMessage);
  if (TypeMessage <= FLogLevel) then
    TLogWriter(FWriter).AddLigne(s);
  if Assigned(FOnAppend) then
    FOnAppend(s, Texte, TypeMessage, TypeMessage <= FLogLevel);
end;

procedure TFichierLog.AppendCallStack;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    JclLastExceptStackListToStrings(sl, True);
    AppendLog(sl.Text, tmErreur);
  finally
    sl.Free;
  end;
end;

procedure TFichierLog.AppendLog(E: Exception);
begin
  AppendLog(E.ClassName, tmErreur);
  AppendLog(E.Message, tmErreur);

  if loIncludeCallStackOnError in FLogOptions then
    AppendCallStack;
end;

procedure TFichierLog.BeforeDestruction;
begin
  inherited;
  LogOptions := FLogOptions - [loIncludeCallStackOnError];
end;

destructor TFichierLog.Destroy;
begin
  FWriter.Terminate;
  FWriter.WaitFor;
  FLogCriticalSection.Free;
  FFileCriticalSection.Free;
  JclStopExceptionTracking;
  inherited;
end;

procedure TFichierLog.CheckLogging;
begin
  if TLogWriter(FWriter).isOpen then
    raise Exception.Create('Impossible de modifier les propriétés');
end;

constructor TFichierLog.Create;
var
  GUID: TGUID;
begin
  FWriter := TLogWriter.Create(True);

  FLogCriticalSection := TCriticalSection.Create;
  FFileCriticalSection := TCriticalSection.Create;

  FLogLevel := tmErreur;

  CreateGUID(GUID);
  FguidInstance := GUIDToString(GUID);

  FEncoding := TEncoding.Default;

  FLimitSize := 3 * 1024 * 1024; // 3 Mo
  FKeepRatio := 30;
  FLogDir := 'Log';
  FBackupDir := 'Backup';
  FBackupRotation := lrMonth;
  FArchivingDuration := 6;

  LogOptions := [loIncludeInstanceID];
  LogFileOptions := [lfoAutoClose];
  if Load7Zip then
    LogFileOptions := LogFileOptions + [lfoKeepBackup];

  JclStartExceptionTracking;

  FWriter.Start;
end;

class destructor TFichierLog.Destroy;
begin
  _Instance.Free;
end;

function TFichierLog.GetLogFullPath: string;
begin
  Result := TPath.Combine(LogDir, LogFileName);
end;

class constructor TFichierLog.Create;
begin
  _Instance := TFichierLog.Create;
end;

function TFichierLog.GetLogDir: string;
var
  binPath: string;
begin
  if IsLibrary then
    binPath := GetModuleName(HInstance)
  else
    binPath := ParamStr(0);
  Result := TPath.Combine(TPath.GetDirectoryName(binPath), FLogDir);
end;

procedure TFichierLog.SetLogDir(const Value: string);
begin
  CheckLogging;
  FLogDir := IncludeTrailingPathDelimiter(Value);
end;

function TFichierLog.GetBackupPath: string;
begin
  Result := TPath.Combine(LogDir, FBackupDir)
end;

function TFichierLog.GetLogFileName: string;
var
  f: string;
begin
  if FLogFileName = '' then
  begin
    if IsLibrary then
      f := GetModuleName(HInstance)
    else
      f := ParamStr(0);
    Result := TPath.ChangeExtension(TPath.GetFileName(f), '.log');
  end
  else
    Result := FLogFileName;
end;

procedure TFichierLog.SetBackupRotation(const Value: TLogRotation);
begin
  CheckLogging;
  FBackupRotation := Value;
end;

procedure TFichierLog.SetEncoding(const Value: TEncoding);
begin
  CheckLogging;
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  if TEncoding.IsStandardEncoding(Value) then
    FEncoding := Value
  else if Value <> nil then
    FEncoding := Value.Clone
  else
    FEncoding := TEncoding.Default;
end;

procedure TFichierLog.SetLogFileName(const Value: string);
begin
  CheckLogging;
  FLogFileName := Value;
end;

procedure TFichierLog.SetLogFileOptions(const Value: TLogFileOptions);
begin
  FLogFileOptions := Value;
end;

procedure TFichierLog.SetKeepRatio(const Value: Integer);
begin
  if not(Value in [0 .. 99]) then
    raise Exception.Create('Le ratio doit être compris entre 0% et 99%');
  FKeepRatio := Value;
end;

function TFichierLog.BuildMessage(Texte: string; TypeMessage: TTypeMessage): string;
var
  Suffix: string;
begin
  Result := FormatDateTime('dd-mm-yyyy hh:mm:ss:zzz', Now);
  if loIncludeInstanceID in FLogOptions then
    Result := Result + ' - ' + FguidInstance;

  if Assigned(FOnBuildMessage) then
    FOnBuildMessage(Result, Texte, Suffix, TypeMessage);

  Result := Result + ' - ' + Texte + Suffix;
end;

procedure TFichierLog.SetLogOptions(const Value: TLogOptions);
begin
  FLogOptions := Value;
end;

initialization

{$IFDEF DEBUG}
  RegisterExpectedMemoryLeak(TLogWriter, 1);
{$ENDIF}

end.
