unit UFichierLog;

interface

uses
  Windows, SysUtils, Classes, StrUtils;

type
  TTypeMessage = (tmErreurCritique, tmErreur, tmTraitement, tmInfo, tmDebugLite, tmDebugFull);
  TLogRotation = (lrMinute, lrHour, lrDay, lrMonth, lrYear);

  TFichierLog = class
  strict private
    class var _Instance: TFichierLog;
  strict private
    _FileName, _Path: string;
  private
    FFichierLog: TFileStream;
    FDebugLevel: TTypeMessage;
    FguidInstance: string;
    FLimitSize: Integer;
    FKeepBackup: Boolean;
    FKeepRatio: Integer;
    FBackupDir: string;
    FLogDir: string;
    FBackupRotation: TLogRotation;
    FEncoding: TEncoding;
    FAddCallStack: Boolean;
    procedure SetLimitSize(const Value: Integer);
    function OuvreFichierLog: Boolean;
    function GetPath: string;
    procedure SetPath(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetKeepRatio(const Value: Integer);
    procedure AddToBackup(Data: TStream);
    procedure ForwardStream(Stream: TStream);
    procedure RewindStream(Stream: TStream);
    procedure WriteToLogStream(Stream: TStream; const Text: string);
    procedure SetBackupRotation(const Value: TLogRotation);
    procedure SetEncoding(const Value: TEncoding);

    constructor Create;
    procedure SetAddCallStack(const Value: Boolean);
  public
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    class function getInstance: TFichierLog;
    class destructor Destroy;

    procedure AppendLog(const Texte: string; TypeMessage: TTypeMessage); overload;
    procedure AppendLog(E: Exception); overload;
    procedure UpdateLog(const Texte: string; TypeMessage: TTypeMessage);

    procedure Close;

    property LogFileName: string read GetFileName write SetFileName;
    property LogDir: string read GetPath write SetPath;
    function GetNomFichierLog: string;
    property DebugLevel: TTypeMessage read FDebugLevel write FDebugLevel;
    property AddCallStack: Boolean read FAddCallStack write SetAddCallStack;

    property Encoding: TEncoding read FEncoding write SetEncoding;

    // LimitSize en octets
    property LimitSize: Integer read FLimitSize write SetLimitSize;
    property KeepBackup: Boolean read FKeepBackup write FKeepBackup;
    property KeepRatio: Integer read FKeepRatio write SetKeepRatio;
    property BackupDir: string read FBackupDir write FBackupDir;
    property BackupRotation: TLogRotation read FBackupRotation write SetBackupRotation;
  end;

implementation

uses
  DateUtils, TypInfo, System.IOUtils, JclCompression, JclDebug;

procedure TFichierLog.SetLimitSize(const Value: Integer);
begin
  FLimitSize := Value;
end;

procedure TFichierLog.AppendLog(const Texte: string; TypeMessage: TTypeMessage);
begin
  if (TypeMessage > FDebugLevel) then
    Exit;

  try
    if OuvreFichierLog then
      WriteToLogStream(FFichierLog, Texte);
  except
    // cette proc ne doit pas être problématique
  end;
end;

procedure TFichierLog.AddToBackup(Data: TStream);
var
  NomFichierBackup, NomArchive, suffix: string;
  bckStream: TStream;
  Archive: TJclGZipUpdateArchive;
begin
  NomFichierBackup := LogFileName;
  suffix := '';
  if FBackupRotation <= lrMonth then
    suffix := suffix + Format('_%.4d', [YearOf(Now)]);
  if FBackupRotation <= lrMonth then
    suffix := suffix + Format('_%.2d', [MonthOf(Now)]);
  if FBackupRotation <= lrDay then
    suffix := suffix + Format('_%.2d', [DayOf(Now)]);
  if FBackupRotation <= lrHour then
    suffix := suffix + Format('_%.2d', [HourOf(Now)]);
  if FBackupRotation <= lrMinute then
    suffix := suffix + Format('_%.2d', [MinuteOf(Now)]);

  NomFichierBackup := TPath.GetFileNameWithoutExtension(NomFichierBackup) + suffix + TPath.GetExtension(NomFichierBackup);
  NomFichierBackup := TPath.Combine(TPath.Combine(LogDir, FBackupDir), NomFichierBackup);
  TDirectory.CreateDirectory(TPath.GetDirectoryName(NomFichierBackup));

  NomArchive := NomFichierBackup + '.gz';

  Archive := TJclGZipUpdateArchive.Create(NomArchive);
  bckStream := TMemoryStream.Create;
  try
    if TFile.Exists(NomArchive) then
      Archive.ListFiles;
    if Archive.ItemCount = 0 then
    begin
      Archive.AddFile(NomFichierBackup, Data);
      Data.Position := 0;
    end
    else
    begin
      Archive.Items[0].Selected := True;
      Archive.Items[0].Stream := bckStream;
      Archive.Items[0].OwnsStream := False;
      Archive.ExtractSelected;
      bckStream.Seek(0, soFromEnd);
      Data.Position := Length(FEncoding.GetPreamble);
      bckStream.CopyFrom(Data, Data.Size);
      bckStream.Position := 0;
    end;

    Archive.Compress;
  finally
    bckStream.Free;
    Archive.Free;
  end;
end;

procedure TFichierLog.AppendLog(E: Exception);
var
  sl: TStringList;
begin
  AppendLog(E.ClassName, tmErreur);
  AppendLog(E.Message, tmErreur);

  if FAddCallStack then
  begin
    sl := TStringList.Create;
    try
      JclLastExceptStackListToStrings(sl, True);
      AppendLog(sl.Text, tmErreur);
    finally
      sl.Free;
    end;
  end;
end;

procedure TFichierLog.BeforeDestruction;
begin
  inherited;
  AddCallStack := False;
end;

function TFichierLog.OuvreFichierLog: Boolean;
var
  tmpStream, bckStream: TStream;
  cutPos: Integer;
  NomFichierLog: string;
  MaxFileSize: Cardinal;
begin
  MaxFileSize := FLimitSize;
  Result := Assigned(FFichierLog);
  NomFichierLog := GetNomFichierLog;
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
      FFichierLog.Position := FFichierLog.Size - MulDiv(MaxFileSize, 100 - FKeepRatio, 100);
      ForwardStream(FFichierLog);

      cutPos := FFichierLog.Position;

      // on copie le BOM en même temps s'il est présent
      FFichierLog.Position := 0;
      bckStream.CopyFrom(FFichierLog, cutPos);

      // FFichierLog a été repositionné sur cutPos
      tmpStream.CopyFrom(FFichierLog, FFichierLog.Size - cutPos);

      FFichierLog.Size := Length(FEncoding.GetPreamble);
      FFichierLog.CopyFrom(tmpStream, -1);

      if KeepBackup then
        AddToBackup(bckStream);
    finally
      tmpStream.Free;
      bckStream.Free;
    end;
  end;

  FFichierLog.Seek(0, soFromEnd);
end;

procedure TFichierLog.RewindStream(Stream: TStream);
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

procedure TFichierLog.ForwardStream(Stream: TStream);
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

destructor TFichierLog.Destroy;
begin
  FFichierLog.Free;
  inherited;
end;

constructor TFichierLog.Create;
var
  GUID: TGUID;
begin
  FDebugLevel := tmErreur;

  CreateGUID(GUID);
  FguidInstance := GUIDToString(GUID);

  FEncoding := TEncoding.Default;

  FLimitSize := 3 * 1024 * 1024; // 3 Mo
  FKeepBackup := True;
  FKeepRatio := 30;
  FLogDir := 'Log';
  FBackupDir := 'Backup';
  FBackupRotation := lrMonth;
  FAddCallStack := False;
end;

class destructor TFichierLog.Destroy;
begin
  _Instance.Free;
end;

function TFichierLog.GetNomFichierLog: string;
begin
  Result := TPath.Combine(LogDir, LogFileName);
end;

procedure TFichierLog.Close;
begin
  FreeAndNil(FFichierLog);
end;

function TFichierLog.GetPath: string;
var
  binPath: string;
begin
  if IsLibrary then
    binPath := GetModuleName(HInstance)
  else
    binPath := ParamStr(0);
  Result := TPath.Combine(TPath.GetDirectoryName(binPath), _Path);
end;

procedure TFichierLog.SetPath(const Value: string);
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(Value);

  if not SameFileName(s, _Path) then
    Close;

  _Path := s;
end;

procedure TFichierLog.UpdateLog(const Texte: string; TypeMessage: TTypeMessage);
begin
  if (TypeMessage > FDebugLevel) then
    Exit;

  try
    if OuvreFichierLog then
    begin
      RewindStream(FFichierLog);
      WriteToLogStream(FFichierLog, Texte);
      FFichierLog.Size := FFichierLog.Position;
    end;
  except
    // cette proc ne doit pas être problématique
  end;
end;

procedure TFichierLog.WriteToLogStream(Stream: TStream; const Text: string);
var
  Buffer, Preamble: TBytes;
begin
  Buffer := FEncoding.GetBytes(Format('%s - %s - %s'#13#10, [FormatDateTime('dd-mm-yyyy hh:mm:ss:zzz', Now), FguidInstance, Text]));
  if Stream.Size = 0 then
  begin
    Preamble := FEncoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

function TFichierLog.GetFileName: string;
var
  f: string;
begin
  if _FileName = '' then
  begin
    if IsLibrary then
      f := GetModuleName(HInstance)
    else
      f := ParamStr(0);
    Result := TPath.ChangeExtension(TPath.GetFileName(f), '.log');
  end
  else
    Result := _FileName;
end;

class function TFichierLog.getInstance: TFichierLog;
begin
  if _Instance = nil then
    _Instance := TFichierLog.Create;
  Result := _Instance;
end;

procedure TFichierLog.SetAddCallStack(const Value: Boolean);
begin
  FAddCallStack := Value;
  if FAddCallStack then
    JclStartExceptionTracking
  else
    JclStopExceptionTracking;
end;

procedure TFichierLog.SetBackupRotation(const Value: TLogRotation);
begin
  if FBackupRotation <> Value then
    Close;

  FBackupRotation := Value;
end;

procedure TFichierLog.SetEncoding(const Value: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  if TEncoding.IsStandardEncoding(Value) then
    FEncoding := Value
  else if Value <> nil then
    FEncoding := Value.Clone
  else
    FEncoding := TEncoding.Default;
end;

procedure TFichierLog.SetFileName(const Value: string);
begin
  if not SameFileName(Value, _FileName) then
    Close;

  _FileName := Value;
end;

procedure TFichierLog.SetKeepRatio(const Value: Integer);
begin
  if not(Value in [0 .. 100]) then
    raise Exception.Create('Le ratio doit être compris entre 0% et 100%');
  FKeepRatio := Value;
end;

end.
