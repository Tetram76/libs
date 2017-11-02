unit EasyDB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, BDE, DBTables;

type
	TEasyMode = (emNone, emAuto);

  TEasyDatabase = class(TComponent)
  private
    FPassWord : string;
    FEasyMode : TEasyMode;
    FExtraPath : string;
    FDatabaseName : string;
		FDesigningDatabase : string;
    FForcedPath : string;
    FRegPath: string;
    FUseReg: Boolean;
    { Champs internes du Database }
    FAliasName: string;
    FExtraFile: string;
    FDriverName: string; { Important pour l'éditeur }
    FParams: TStrings;
  protected
    FDatabase : TDatabase;
		procedure SetEasyMode(Value : TEasyMode);
		procedure SetExtraPath(Value : string);
    procedure SetExtraFile(Value: string);
    procedure SetConnected(Value : Boolean);
    function GetConnected : Boolean;
    procedure SetLoginPrompt(Value : Boolean);
    function GetLoginPrompt : Boolean;
    procedure SetParams(Value : TStrings);
    procedure SetDatabaseName(Value : string);
    procedure SetDesigningDatabase(Value : string);
		procedure SetDatabaseParams;
    procedure SetDriverName(Value : string);
    procedure SetPassword(Value : string);
    function GetPassword : string;
    procedure SetAliasName(Value : string);
    procedure SetForcedPath(Value : string);
    procedure Loaded; override;
    {}
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    procedure SetDefaultRegistry;
  public
    function EditDatabase: Boolean;
    {}
    property Database : TDatabase read FDatabase;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property AliasName : string read FAliasName write SetAliasName;
    property Connected : Boolean read GetConnected write SetConnected stored False;
    property LoginPrompt : Boolean read GetLoginPrompt write SetLoginPrompt;
    property DatabaseName : string read FDatabaseName write SetDatabaseName;
    property EasyMode : TEasyMode read FEasyMode write SetEasyMode default emAuto;
    property ExtraPath : string read FExtraPath write SetExtraPath;
    property ExtraFile: string read FExtraFile write SetExtraFile;
    property Params : TStrings read FParams write SetParams;
    property DriverName : string read FDriverName write SetDriverName;
    property DesigningDatabase : string read FDesigningDatabase write SetDesigningDatabase;
    property Password : string read GetPassword write SetPassword;
    property ForcedPath : string read FForcedPath write SetForcedPath;
    property RegistryKey: string read FRegPath write FRegPath;
    property UseRegistry: Boolean read FUseReg write FUseReg;
  end;

var
  HelpContextEditDatabase: Integer;

implementation

uses Registry, TypInfo, Encrypt, FGUtils, FEditDatabase;

function TEasyDatabase.EditDatabase: Boolean;
var F: TFenEditDatabase;
    OldConnected: Boolean;
    L: TList; i: Integer;
begin
  OldConnected:= Connected;
  L:= TList.Create;
  try
    { Mémorisation des tables actives avant la déconnection }
    if OldConnected then
      for i:= 0 to Database.DataSetCount - 1 do
        if Database.DataSets[i].Active then
          L.Add(Database.DataSets[i]);
    Connected:= False;
    try
      F:= TFenEditDatabase.Create(nil); { Si Self, ça plante mortel en ComponentEditor }
      try
        F.HelpContext:= HelpContextEditDatabase;
        F.Initialize(Self);
        Result:= F.ShowModal = mrOk;
        if Result and FUseReg then
          SaveToRegistry;
      finally
        F.Free;
      end;
    finally
      Connected:= OldConnected;
    end;
    if OldConnected then
      for i:= 0 to L.Count - 1 do
        TDataSet(L[i]).Active:= True;
  finally
    L.Free;
  end;
end;

procedure TEasyDatabase.LoadFromRegistry;
var Reg: TRegistry;
begin
  Reg:= TRegistry.Create;
  try
    try
      Reg.RootKey:= HKEY_LOCAL_MACHINE;
      if Reg.OpenKey(RegistryKey, False) then
      begin
        if not Reg.ValueExists('EasyDB_DefaultParams') then Exit;
        if Reg.ReadBool('EasyDB_DefaultParams') then Exit;
        EasyMode:= TEasyMode(GetEnumValue(TypeInfo(TEasyMode), Reg.ReadString('EasyDB_Mode')));
        DriverName:= Reg.ReadString('EasyDB_DriverName');
        AliasName:= Reg.ReadString('EasyDB_AliasName');
        ExtraPath:= Reg.ReadString('EasyDB_ExtraPath');
        ExtraFile:= Reg.ReadString('EasyDB_ExtraFile');
        ForcedPath:= Reg.ReadString('EasyDB_ForcedPath');
      end;
    except
      on E: ERegistryException do
      begin // ignorer
      end else raise;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TEasyDatabase.SaveToRegistry;
var Reg: TRegistry;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    Reg.OpenKey(RegistryKey, True);
    Reg.WriteBool('EasyDB_DefaultParams', False);
    Reg.WriteString('EasyDB_Mode', GetEnumName(TypeInfo(TEasyMode), Integer(FEasyMode)));
    Reg.WriteString('EasyDB_DriverName', DriverName);
    Reg.WriteString('EasyDB_AliasName', AliasName);
    Reg.WriteString('EasyDB_ExtraPath', ExtraPath);
    Reg.WriteString('EasyDB_ExtraFile', ExtraFile);
    Reg.WriteString('EasyDB_ForcedPath', ForcedPath);
  finally
    Reg.Free;
  end;
end;

procedure TEasyDatabase.SetDefaultRegistry;
var Reg: TRegistry;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    Reg.OpenKey(RegistryKey, True);
    Reg.WriteBool('EasyDB_DefaultParams', True);
  finally
    Reg.Free;
  end;
end;

procedure TEasyDatabase.Loaded;
begin
  inherited Loaded;
  if FUseReg and (not (csDesigning in ComponentState)) then LoadFromRegistry;
  SetDatabaseParams;
end;

  { Get/Set functions/procedures }

procedure TEasyDatabase.SetLoginPrompt(Value : Boolean);
begin
  if FDatabase = nil then Exit;
	if FDatabase.LoginPrompt <> Value then
		FDatabase.LoginPrompt:= Value;
end;

function TEasyDatabase.GetLoginPrompt : Boolean;
begin
  Result:= False;
  if FDatabase = nil then Exit;
  Result:= FDatabase.LoginPrompt;
end;

procedure TEasyDatabase.SetPassword(Value : string);
begin
	FPassword:=EncryptString(Value,25000);
end;

function TEasyDatabase.GetPassword : string;
begin
	Result:=DecryptString(FPassWord,25000);
end;

procedure TEasyDatabase.SetDesigningDatabase(Value : string);
begin
	if FDesigningDatabase <> Value then
  begin
  	FDesigningDatabase:= Trim(Value);
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetDriverName(Value : string);
begin
	if FDriverName <> Value then
  begin
    FDriverName:= Trim(Value);
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetForcedPath(Value : string);
begin
	if Value <> FForcedPath then
  begin
  	FForcedPath:= Value;
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetAliasName(Value : string);
begin
	if FAliasName <> Value then
  begin
    FAliasName:= Value;
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetDatabaseName(Value : string);
begin
	if Value <> FDatabaseName then
  begin
  	FDatabaseName:= Value;
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetParams(Value : TStrings);
begin
	FParams.Assign(Value);
  SetDatabaseParams;
end;

procedure TEasyDatabase.SetConnected(Value : Boolean);
begin
  if FDatabase = nil then Exit;
	if Value <> FDatabase.Connected then
  begin
  	try
    	if Value then
      begin
		    SetDatabaseParams;
      end;
  		FDatabase.Connected:= Value;
		except
    	raise;
    end;
  end;
end;

function TEasyDatabase.GetConnected : Boolean;
begin
  Result:= False;
  if FDatabase = nil then
    Exit;
	Result:= FDatabase.Connected;
end;

procedure TEasyDatabase.SetEasyMode(Value : TEasyMode);
begin
	if Value <> FEasyMode then
  begin
  	FEasyMode:= Value;
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetExtraPath(Value : string);
begin
	if Value <> FExtraPath then
  begin
  	FExtraPath:= Value;
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetExtraFile(Value : string);
begin
	if Value <> FExtraFile then
  begin
  	FExtraFile:= Value;
    SetDatabaseParams;
  end;
end;

procedure TEasyDatabase.SetDatabaseParams;
var AliasList: TStringList;
    Chemin: string;
  procedure EffacerParams;
  var i : Integer;
  begin
    if FDatabase.Params.Count > 0 then
    begin
      i:= 0;
      while i < FDatabase.Params.Count do
      begin
        if UpperCase(Copy(FDatabase.Params[i],1,4))='PATH' then
          FDatabase.Params.Delete(i)
        else
        if UpperCase(Copy(FDatabase.Params[i],1,13))='DATABASE NAME' then
          FDatabase.Params.Delete(i)
        else
          Inc(i);
      end;
    end;
  end;
begin
  if (csLoading in ComponentState) then Exit;
  if FDatabase = nil then Exit;
  FDatabase.Connected:= False;
  AliasList:= TStringList.Create;
  try
  	FDatabase.Session.GetAliasNames(AliasList);
    FDatabase.Params:= FParams;
    if AliasList.IndexOf(DatabaseName) >= 0 then
    begin
      EffacerParams;
      FDatabase.AliasName:= DatabaseName;
      FDatabase.DatabaseName:= DatabaseName;
    end else begin
  		FDatabase.DatabaseName:= DatabaseName;
      FDatabase.AliasName:= '';
      if FDriverName = '' then
        FDatabase.DriverName:= 'STANDARD'
      else
        FDatabase.DriverName:= FDriverName;
      case EasyMode of
        emNone: begin
          if FAliasName <> '' then
            FDatabase.AliasName:= FAliasName;
        end;
        emAuto: begin
          if csDesigning in ComponentState then
          begin
            if AliasList.IndexOf(FDesigningDatabase) = -1 then
  					  Chemin:= FDesigningDatabase
            else begin
  					  FDatabase.Params.Clear;
  					  FDatabase.AliasName:= FDesigningDatabase;
              Exit;
            end;
          end else begin
            if FForcedPath = '' then
              Chemin:= AddSlash(ExtractFilePath(Application.ExeName)) + AddSlash(ExtraPath)
            else
              Chemin:= AddSlash(FForcedPath);
          end;
          FDatabase.Params.Clear;
          FDatabase.Params.Add('PATH=' + Chemin);
          if (FDriverName = 'MSACCESS') or (FDriverName = 'INTRBASE') then
            Chemin:= Chemin + ExtraFile;
          FDatabase.Params.Add('DATABASE NAME=' + Chemin);
        end;
      end;
    end;
    if PassWord <> '' then
      FDatabase.Params.Add('PASSWORD='+Password);
  finally
    AliasList.Free;
  end;
end;

constructor TEasyDatabase.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
  FParams:= TStringList.Create;
  FDatabase:= TDatabase.Create(Self);
  if FDatabase <> nil then
    FDatabase.DriverName:= 'STANDARD';
  EasyMode:= emAuto;
end;

destructor TEasyDatabase.Destroy;
begin
  FParams.Free;
  FParams:= nil;
  FDatabase.Free;
	FDatabase:= nil;
	inherited Destroy;
end;

end.
