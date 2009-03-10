unit GzExeUpdate;

interface

{$I GrizzlyDefine.inc}

uses Classes, SysUtils, Windows, GzVerInf, Forms, FGFiles, ASysUtil
(*{$IFDEF USE_ICS}
, HttpProt
{$ELSE}
, IdHTTP
{$ENDIF}*)
;

type

  { TExeUpdate est un objet en devenir... Son analyse reste encore à terminer... }
  TUpdateMode = (umWeb, umNetwork);
  {Si umWeb : chargement de DirectoryAdress+'/'+ExeName.txt qui contient la version
  et comparaison avec la version actuelle}
  {Si umNetwork : vérification directe sur le réseau de la version...}

  TRetrieveVersionEvent = procedure(Sender : TObject; var Version : string); 

  TExeUpdate = class(TComponent)
  private
    FDirectoryAddress: string;
    FOnMustQuit: TNotifyEvent;
    FUpdateMode: TUpdateMode;
    FOnRetrieveVersion: TRetrieveVersionEvent;
    {$IFDEF LINUX}
    FVersion : string;
    {$ENDIF}
    procedure LoadHTTPStream(AURL : string; AStream : TStream);
    function LoadWebString(AURL : string) : string;
    procedure SetDirectoryAddress(const Value: string);
    procedure SetOnMustQuit(const Value: TNotifyEvent);
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetOnRetrieveVersion(const Value: TRetrieveVersionEvent);
    procedure SetVersion(const Value: string);
    function GetVersion : string;
  protected
    function GetWebVersion : string;
    function GetNetworkVersion : string;
    procedure EndUpdate;
    procedure LoadFromWeb;
    procedure LoadFromNetwork;
    procedure CheckWeb;
    procedure CheckNetwork;
  public
    function HasNewVersion : Boolean;
    procedure UpdateExe;
    property UpdateMode : TUpdateMode read FUpdateMode write SetUpdateMode;
    property DirectoryAddress : string read FDirectoryAddress write SetDirectoryAddress;
    property OnMustQuit : TNotifyEvent read FOnMustQuit write SetOnMustQuit;
    property OnRetrieveVersion : TRetrieveVersionEvent read FOnRetrieveVersion write SetOnRetrieveVersion;
    property Version : string read GetVersion write SetVersion;
  end;

  TQueryCloseEvent = procedure(Sender : TObject; var CanKeepOn : Boolean) of object;

  TSelfUpdate = class(TComponent)
  private
    FReplacement: string;
    FQueryClose: TQueryCloseEvent;
    FWithRestart: Boolean;
  public
    function ReplacementVersion : string;
    function ActiveVersion : string;

    function CanUpdate : Boolean;
    procedure TryUpdate;
    procedure Clean;
  published
    property ReplacementFileName : string read FReplacement write FReplacement;
    property OnQueryClose : TQueryCloseEvent read FQueryClose write FQueryClose;
    property WithRestart : Boolean read FWithRestart write FWithRestart;
  end;


implementation

uses MBinToRC;

{ TExeUpdate }

procedure TExeUpdate.CheckNetwork;
begin

end;

procedure TExeUpdate.CheckWeb;
begin

end;

procedure TExeUpdate.EndUpdate;
begin

end;

function TExeUpdate.GetNetworkVersion: string;
{$IFDEF WIN32}
var
  AVI : TVersionInfo;
{$ENDIF}
begin
  {$IFDEF WIN32}
  AVI := TVersionInfo.Create(Application.ExeName);
  try
    Result := AVI.FileVersion;
  finally
    AVI.Free;
  end;
  {$ELSE}

  {$ENDIF}
end;

function TExeUpdate.GetVersion: string;
{$IFDEF WIN32}
var
  AVI : TVersionInfo;
{$ENDIF}
begin
  if Assigned(FOnRetrieveVersion) then
  begin
    FOnRetrieveVersion(Self, Result);
  end
  else
  begin
    {$IFDEF WIN32}
    AVI := TVersionInfo.Create(Application.ExeName);
    try
      Result := AVI.FileVersion;
    finally
      AVI.Free;
    end;
    {$ELSE}
    Result := FVersion;
    {$ENDIF}
  end;
end;

function TExeUpdate.GetWebVersion: string;
var
  AURL : string;
begin
  AURL := AddWebSlash(DirectoryAddress) + ChangeFileExt(ExtractFileName(Application.ExeName), '.txt');
  Result := LoadWebString(AURL);
end;

function TExeUpdate.HasNewVersion: Boolean;
begin
  Result := (Version <> GetWebVersion) and (GetWebVersion <> '');
end;

procedure TExeUpdate.LoadFromNetwork;
begin

end;

procedure TExeUpdate.LoadFromWeb;
begin

end;

procedure TExeUpdate.LoadHTTPStream(AURL: string; AStream: TStream);
begin

end;

function TExeUpdate.LoadWebString(AURL: string): string;
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    LoadHTTPStream(AURL, AStream);
    Result := PChar(AStream.Memory);
  finally
    AStream.Free;
  end;
end;

procedure TExeUpdate.SetDirectoryAddress(const Value: string);
begin
  FDirectoryAddress := Value;
end;

procedure TExeUpdate.SetOnMustQuit(const Value: TNotifyEvent);
begin
  FOnMustQuit := Value;
end;

procedure TExeUpdate.SetOnRetrieveVersion(const Value: TRetrieveVersionEvent);
begin
  FOnRetrieveVersion := Value;
end;

procedure TExeUpdate.SetUpdateMode(const Value: TUpdateMode);
begin
  FUpdateMode := Value;
end;

procedure TExeUpdate.SetVersion(const Value: string);
begin
  {$IFDEF LINUX}
  FVersion := Value;
  {$ENDIF}
end;

procedure TExeUpdate.UpdateExe;
begin

end;

{ TSelfUpdate }

function TSelfUpdate.ActiveVersion: string;
var
  AVI : TVersionInfo;
begin
  AVI := TVersionInfo.Create(Application.ExeName);
  try
    Result := AVI.FileVersion;
  finally
    AVI.Free;
  end;
end;

function TSelfUpdate.CanUpdate: Boolean;
var
  V1, V2 : string;
begin
  try
    V1 := ActiveVersion;
    V2 := ReplacementVersion;
    Result := FileExists(ExtractFilePath(Application.ExeName) + FReplacement) and (V2 <> '') and (CompareVersion(StringToLongVersion(V1), StringToLongVersion(V2)) < 0);
  except
    Result := False;
  end;
end;

function TSelfUpdate.ReplacementVersion: string;
var
  AVI : TVersionInfo;
begin
  AVI := TVersionInfo.Create(ExtractFilePath(Application.ExeName) + FReplacement);
  try
    Result := AVI.FileVersion;
  finally
    AVI.Free;
  end;
end;

procedure TSelfUpdate.TryUpdate;
var
  FExeName : string;
  KeepOn : Boolean;
begin
  if CanUpdate then
  begin
    DMBinToRC := TDMBinToRC.Create(Self);
    try
      FExeName := ExtractFilePath(Application.ExeName) + DMBinToRC.ExeStore.GetResByIndex(0).Name;
      DMBinToRC.ExeStore.GetResByIndex(0).SaveToFile(FExeName);
      FExeName := '"' + FExeName + '" ' + FReplacement + ' ' + ExtractFileName(Application.ExeName);
      if WithRestart then
        FExeName := FExeName + ' restart';
      KeepOn := True;
      if Assigned(FQueryClose) then
        FQueryClose(Self, KeepOn)
      else
        Application.Terminate;
      if KeepOn then
        ExecCommandLine(FExeName, SW_HIDE);
    finally
      DMBinToRC.Free;
    end;
  end;
end;

procedure TSelfUpdate.Clean;
var
  FExeName : string;
begin
  DMBinToRC := TDMBinToRC.Create(Self);
  try
    FExeName := ExtractFilePath(Application.ExeName) + DMBinToRC.ExeStore.GetResByIndex(0).Name;
    if FileExists(FExeName) then
      DeleteFile(PAnsiChar(FExeName));

    FExeName := ExtractFilePath(Application.ExeName) + FReplacement;
    if FileExists(FExeName) then
      DeleteFile(PAnsiChar(FExeName));

  finally
    DMBinToRC.Free;
  end;
end;

end.
