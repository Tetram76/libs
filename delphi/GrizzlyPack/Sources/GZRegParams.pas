unit GZRegParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GZParams, Registry;

type
  EGZRegParamError = class(Exception);

  TRegParamDatabase = class;

  TGZRegParam = class(TGZParam)
  private
    FRegistry : TRegistry;
    function GetParamDB : TRegParamDatabase;
  protected
    function GetAsString : string; override;
    procedure SetAsString(Value : string); override;
    function GetAsInteger : Integer; override;
    procedure SetAsInteger(Value : Integer); override;
    function GetAsFloat : Extended; override;
    procedure SetAsFloat(Value : Extended); override;
    function GetExists: Boolean; override;
    function CheckKey : Boolean;
    function CheckParam : Boolean;
  public
    constructor Create(AParamDB : TCustomParamDatabase; APath, AParamName : string); override;
    destructor Destroy; override;
    procedure SaveToStream(AStream : TStream); override;
    procedure LoadFromStream(AStream : TStream); override;
    property ParamDB : TRegParamDatabase read GetParamDB;
  end;

  TRegParamDatabase = class(TCustomParamDatabase)
  private
    { Déclarations privées }
    FRootKey : HKey;
    FRegParam : TGZRegParam;
    procedure SetRootKey(Value : HKey);
  protected
    { Déclarations protégées }
    procedure SetInternalActive(Value : Boolean); override;

    function CreateGZParam(const APath, AParamName : string): TGZParam; override;
    function GetParamByName(APath, AParamName : string) : TGZParam; override;

    procedure GetPathList(BasePath: string; List: TStrings); override;
    procedure GetParamList(Path: string; List: TStrings); override;

  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Déclarations publiées }
    property RootKey : HKey read FRootKey write SetRootKey;
  end;

implementation

uses FGUtils;

{ TGZRegParam }

constructor TGZRegParam.Create(AParamDB : TCustomParamDatabase; APath, AParamName : string);
begin
  inherited Create(AParamDB, APath, AParamName);
  FRegistry:= TRegistry.Create;
  FRegistry.RootKey:= ParamDB.RootKey;
end;

destructor TGZRegParam.Destroy;
begin
  FRegistry.Free;
  inherited Destroy;
end;

function TGZRegParam.GetParamDB : TRegParamDatabase;
begin
  Result:= TRegParamDatabase(FParamDB);
end;

function TGZRegParam.GetExists: Boolean;
begin
  Result:= (FRegistry.OpenKey(FPath, False) and FRegistry.ValueExists(FParamName));
end;

function TGZRegParam.CheckKey : Boolean;
begin
  Result:= True;
  if CompareText(FRegistry.CurrentPath, FPath) <> 0 then
    Result:= FRegistry.OpenKey(FPath, (not ParamDB.ReadOnly) and (ParamDB.AutoCreateParam));
end;

function TGZRegParam.CheckParam : Boolean;
begin
  Result:= False;
  if (CompareText(FRegistry.CurrentPath, FPath) = 0) then
  begin
    if (FRegistry.ValueExists(FParamName)) then
      Result:= True
    else
      Result:= ParamDB.AutoCreateParam;
  end;
end;

function TGZRegParam.GetAsString : string;
begin
  CheckKey;
  if not CheckKey then
    Result:= ''
  else
    Result:= FRegistry.ReadString(FParamName);
end;

function TGZRegParam.GetAsInteger : Integer;
begin
  if not CheckKey then
    Result:= 0
  else
    Result:= FRegistry.ReadInteger(FParamName);
end;

function TGZRegParam.GetAsFloat : Extended;
begin
  if not CheckKey then
    Result:= 0
  else
    Result:= FRegistry.ReadFloat(FParamName);
end;

procedure TGZRegParam.SaveToStream(AStream : TStream);
begin
  raise EGZRegParamError.Create('Méthode SaveToStream non implémentée')
end;

procedure TGZRegParam.LoadFromStream(AStream : TStream);
begin
  //ParamDB.CheckReadOnly;
  raise EGZRegParamError.Create('Méthode LoadFromStream non implémentée')
end;

procedure TGZRegParam.SetAsString(Value : string);
begin
  ParamDB.CheckReadOnly;
  if not (CheckKey and CheckParam) then
    raise EGZRegParamError.Create('Erreur à la création de la clef')
  else
    FRegistry.WriteString(FParamName, Value);
end;

procedure TGZRegParam.SetAsInteger(Value : Integer);
begin
  ParamDB.CheckReadOnly;
  if not (CheckKey and CheckParam) then
    raise EGZRegParamError.Create('Erreur à la création de la clef')
  else
    FRegistry.WriteInteger(FParamName, Value);
end;

procedure TGZRegParam.SetAsFloat(Value : Extended);
begin
  ParamDB.CheckReadOnly;
  if not (CheckKey and CheckParam) then
    raise EGZRegParamError.Create('Erreur à la création de la clef')
  else
    FRegistry.WriteFloat(FParamName, Value);
end;


{ TRegParamDatabase }

constructor TRegParamDatabase.Create(AOwner : TComponent);
begin
  inherited;
  FRootKey:= HKEY_CURRENT_USER;
  FRegParam:= TGZRegParam(CreateGZParam(DefaultPath,''));
end;

destructor TRegParamDatabase.Destroy;
begin
  FRegParam.Free;
  FRegParam:= nil;
  inherited Destroy;
end;

function TRegParamDatabase.CreateGZParam(const APath, AParamName : string): TGZParam;
begin
  Result:= TGZRegParam.Create(Self, APath, AParamName);
end;

function TRegParamDatabase.GetParamByName(APath, AParamName : string) : TGZParam;
begin
  FRegParam.FPath:= APath;
  FRegParam.FParamName:= AParamName;
  Result:= FRegParam;
end;

procedure TRegParamDatabase.GetPathList(BasePath: string; List: TStrings);
begin
end;

procedure TRegParamDatabase.GetParamList(Path: string; List: TStrings);
begin
end;

procedure TRegParamDatabase.SetRootKey(Value : HKey);
begin
  CheckInactive;
  FRootKey:= Value;
  FRegParam.FRegistry.RootKey:= Value;
end;

procedure TRegParamDatabase.SetInternalActive(Value : Boolean);
begin
  if Assigned(FRegParam) then
    FRegParam.FRegistry.RootKey:= FRootKey;
  FActive:= Value;
  if not FActive then
    FRegParam.FRegistry.CloseKey;
end;

end.
