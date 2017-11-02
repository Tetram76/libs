unit GZParams;

interface

{ ATTENTION à AutoClose qui peut contrôler la destruction des TGZParams...}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, ExtCtrls;

type
  EGZParamError = class(Exception);

  TCustomParamDatabase = class;

  TGZParam = class(TObject)
  protected
    FParamDB: TCustomParamDatabase;
    FPath: string;
    FParamName: string;
    function GetAsString: string; virtual; abstract;
    procedure SetAsString(Value: string); virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    procedure SetAsInteger(Value: Integer); virtual; abstract;
    function GetAsFloat: Extended; virtual; abstract;
    procedure SetAsFloat(Value: Extended); virtual; abstract;
    function GetExists: Boolean; virtual; abstract;
  public
    constructor Create(AParamDB: TCustomParamDatabase; APath, AParamName: string); virtual;
    destructor Destroy; override;
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure WriteAsStrings(ALst : TStrings);
    procedure ReadAsStrings(ALst : TStrings);
    procedure Delete; virtual; abstract;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property Exists: Boolean read GetExists;
    property ParamDB: TCustomParamDatabase read FParamDB;
    property Path: string read FPath;
    property ParamName: string read FParamName;
  end;

  TGZPath = class(TObject)
  private
  protected
    FParamDB: TCustomParamDatabase;
    FPath: string;
  public
    constructor Create(AParamDB: TCustomParamDatabase; APath: string); virtual;
    destructor Destroy; override;
    procedure SaveToStream(Param: string; AStream: TStream);
    procedure LoadFromStream(Param: string; AStream: TStream);
    procedure WriteAsStrings(Param: string; ALst : TStrings);
    procedure ReadAsStrings(Param: string; ALst : TStrings);
    procedure WriteString(Param: string; Value: string);
    procedure WriteInteger(Param: string; Value: Integer);
    procedure WriteFloat(Param: string; Value: Extended);
    function ReadString(Param: string; Default: string): string;
    function ReadInteger(Param: string; Default: Integer): Integer;
    function ReadFloat(Param: string; Default: Extended): Extended;
    procedure Delete(Param: string); virtual; 
    property ParamDB: TCustomParamDatabase read FParamDB;
  end;

  TCustomParamDatabase = class(TComponent)
  private
    { Déclarations privées }
    FTimer: TTimer;
    FAutoClose: Boolean;
    FAutoCloseInterval: Cardinal;
    FStreamedActive: Boolean;
    FReadOnly: Boolean;
    FAutoCreateParam: Boolean;
    FAutoActive: Boolean;
    FDefaultPath: string;
    FGZPath: TGZPath;
    procedure SetActive(Value: Boolean);
  protected
    { Déclarations protégées }
    FActive: Boolean;
    procedure SetInternalActive(Value: Boolean); virtual;
    function GetActive: Boolean; virtual;

    function CreateGZParam(const APath, AParamName: string): TGZParam; virtual; abstract;
    function GetParamByName(APath, AParamName: string): TGZParam; virtual; abstract;
    function GetParams(APath: string): TGZPath; virtual;

    procedure GetPathList(BasePath: string; List: TStrings); virtual; abstract;
    procedure GetParamList(Path: string; List: TStrings); virtual; abstract;

    procedure CheckAutoClose;
    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckReadOnly;

    procedure Loaded; override;
    procedure OnTimerClose(Sender: TObject);
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function ParamByName(APath, AParamName: string): TGZParam;
    function Params(APath: string): TGZPath;
  published
    { Déclarations publiées }
    property Active: Boolean read GetActive write SetActive;
    property AutoActive: Boolean read FAutoActive write FAutoActive default True;
    property AutoClose: Boolean read FAutoClose write FAutoClose default True;
    property AutoCloseInterval: Cardinal read FAutoCloseInterval write FAutoCloseInterval default 1000;
    property DefaultPath: string read FDefaultPath write FDefaultPath;
    property AutoCreateParam: Boolean read FAutoCreateParam write FAutoCreateParam default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

implementation

{ TGZParam }

constructor TGZParam.Create(AParamDB: TCustomParamDatabase; APath, AParamName: string);
begin
  inherited Create;
  FPath:= APath;
  FParamName:= AParamName;
  FParamDB:= AParamDB;
end;

destructor TGZParam.Destroy;
begin
  FPath:= '';
  FParamName:= '';
  FParamDB:= nil;
  inherited Destroy;
end;

procedure TGZParam.WriteAsStrings(ALst: TStrings);
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    ALst.SaveToStream(AStream);
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TGZParam.ReadAsStrings(ALst: TStrings);
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    ALst.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

{ TGZPath }

constructor TGZPath.Create(AParamDB: TCustomParamDatabase; APath: string);
begin
  inherited Create;
  FPath:= APath;
  FParamDB:= AParamDB;
end;

destructor TGZPath.Destroy;
begin
  FPath:= '';
  FParamDB:= nil;
  inherited Destroy;
end;

procedure TGZPath.SaveToStream(Param: string; AStream: TStream);
begin
  FParamDB.ParamByName(FPath, Param).SaveToStream(AStream);
end;

procedure TGZPath.LoadFromStream(Param: string; AStream: TStream);
begin
  FParamDB.ParamByName(FPath, Param).LoadFromStream(AStream);
end;

procedure TGZPath.WriteString(Param: string; Value: string);
begin
  FParamDB.ParamByName(FPath, Param).AsString:= Value;
end;

procedure TGZPath.WriteInteger(Param: string; Value: Integer);
begin
  FParamDB.ParamByName(FPath, Param).AsInteger:= Value;
end;

procedure TGZPath.WriteFloat(Param: string; Value: Extended);
begin
  FParamDB.ParamByName(FPath, Param).AsFloat:= Value;
end;

function TGZPath.ReadString(Param: string; Default: string): string;
begin
  if FParamDB.ParamByName(FPath, Param).Exists then
    Result:= FParamDB.ParamByName(FPath, Param).AsString
  else
    Result:= Default;
end;

function TGZPath.ReadInteger(Param: string; Default: Integer): Integer;
begin
  if FParamDB.ParamByName(FPath, Param).Exists then
    Result:= FParamDB.ParamByName(FPath, Param).AsInteger
  else
    Result:= Default;
end;

function TGZPath.ReadFloat(Param: string; Default: Extended): Extended;
begin
  if FParamDB.ParamByName(FPath, Param).Exists then
    Result:= FParamDB.ParamByName(FPath, Param).AsFloat
  else
    Result:= Default;
end;

procedure TGZPath.WriteAsStrings(Param: string; ALst: TStrings);
begin
  FParamDB.ParamByName(FPath, Param).WriteAsStrings(ALst);
end;

procedure TGZPath.ReadAsStrings(Param: string; ALst: TStrings);
begin
  FParamDB.ParamByName(FPath, Param).ReadAsStrings(ALst);
end;

procedure TGZPath.Delete(Param: string);
begin
  if FParamDB.ParamByName(FPath, Param).Exists then
    FParamDB.ParamByName(FPath, Param).Delete;
end;

{ TCustomParamDatabase }

constructor TCustomParamDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStreamedActive:= False;
  FReadOnly:= False;
  FAutoActive:= True;
  FAutoClose:= True;
  FAutoCreateParam:= True;
  FAutoCloseInterval := 1000;
  FGZPath:= TGZPath.Create(Self, '');
end;

destructor TCustomParamDatabase.Destroy;
begin
  FGZPath.Free;
  FGZPath:= nil;
  inherited Destroy;
end;

procedure TCustomParamDatabase.Loaded;
begin
  inherited;
  if FStreamedActive then
    Active:= True;
end;

function TCustomParamDatabase.GetParams(APath: string): TGZPath;
begin
  FGZPath.FPath:= APath;
  Result:= FGZPath;
end;

function TCustomParamDatabase.Params(APath: string): TGZPath;
var
  FinalPath: string;
begin
  CheckActive;
  try
    if (APath = '') and (DefaultPath <> '') then
      FinalPath:= DefaultPath
    else
      FinalPath:= APath;

    Result:= GetParams(FinalPath);

  finally
    CheckAutoClose;
  end;
end;

function TCustomParamDatabase.ParamByName(APath, AParamName: string): TGZParam;
var
  FinalPath: string;
begin
  CheckActive;
  try
    if (APath = '') and (DefaultPath <> '') then
      FinalPath:= DefaultPath
    else
      FinalPath:= APath;

    Result:= GetParamByName(FinalPath, AParamName);

  finally
    CheckAutoClose;
  end;
end;

procedure TCustomParamDatabase.Open;
begin
  Active:= True;
end;

procedure TCustomParamDatabase.Close;
begin
  Active:= False;
end;

procedure TCustomParamDatabase.CheckAutoClose;
begin
  if FAutoClose then
  begin
    if FAutoCloseInterval = 0 then
      Close
    else
    begin
      FTimer:= TTimer.Create(Self);
      FTimer.OnTimer:= OnTimerClose;
      FTimer.Interval:= FAutoCloseInterval;
      FTimer.Enabled:= True;
    end;
  end;
end;

procedure TCustomParamDatabase.OnTimerClose(Sender: TObject);
begin
  FTimer.Free;
  FTimer:= nil;
  if FAutoClose then
    Close;
end;

function TCustomParamDatabase.GetActive: Boolean;
begin
  Result:= FActive;
end;

procedure TCustomParamDatabase.SetInternalActive(Value: Boolean);
begin
  if Value <> FActive then
    FActive:= Value;
end;

procedure TCustomParamDatabase.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
  begin
    if Value then FStreamedActive:= True;
    Exit;
  end
  else
  begin
    SetInternalActive(Value);
  end;
end;

procedure TCustomParamDatabase.CheckReadOnly;
begin
  if ReadOnly then
    raise EGZParamError.Create('Base de paramètres en lecture seule. Ecriture impossible');
end;

procedure TCustomParamDatabase.CheckActive;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer:= nil;
  end;
  if not Active then
  begin
    if FAutoActive then
      Open;
    if not Active then
      raise EGZParamError.Create('Base de paramètres inactive. Opération impossible.');
  end;
end;

procedure TCustomParamDatabase.CheckInactive;
begin
  if Active then
    raise EGZParamError.Create('Base de paramètres active. Opération impossible.');
end;

end.

