unit GzDBUpdate;

interface

uses Classes, SysUtils;

type
  TDBVersionUpdater = class;

  TDBVersion = Integer;

  TVersionResult = (vrOK, vrMustRestart, vrMustReboot);

  TVersionItem = class;

  //On passe Handled à True si on ne souhaite pas passer l'évènement au parent
  TCommandEvent = procedure (Sender : TObject; var Handled : Boolean) of object;

  TUpdateCommand = class(TCollectionItem)
  private
    FCommand : TStringList;
    FOnCommand: TCommandEvent;
    FAfterCommand: TNotifyEvent;
    FBeforeCommand: TNotifyEvent;
    FLegend: string;
    FActive: Boolean;
    function GetVersion: TDBVersion;
    procedure SetCommand(const Value: TStringList);
  protected
    function GetDisplayName: string; override;

    procedure DoBeforeCommand;
    procedure DoAfterCommand;
    procedure DoOnCommand;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Execute;

  published
    property Active : Boolean read FActive write FActive;
    property Version : TDBVersion read GetVersion;
    property CommandText : TStringList read FCommand write SetCommand;
    property Legend : string read FLegend write FLegend;
    property BeforeCommand : TNotifyEvent read FBeforeCommand write FBeforeCommand;
    property OnCommand : TCommandEvent read FOnCommand write FOnCommand;
    property AfterCommand : TNotifyEvent read FAfterCommand write FAfterCommand;
  end;

  TUpdateCommandClass = class of TUpdateCommand;

  TUpdateCommandList = class(TCollection)
  private
    FParent: TVersionItem;
    function GetCommand(Index: Integer): TUpdateCommand;
    procedure SetCommand(Index: Integer; Value: TUpdateCommand);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AParent: TVersionItem; AItemClass: TUpdateCommandClass);
    function Add: TUpdateCommand;
    property Parent: TVersionItem read FParent;
    property Items[Index: Integer]: TUpdateCommand read GetCommand write SetCommand; default;
  end;

  TVersionItem = class(TCollectionItem)
  private
    FAfterVersion: TNotifyEvent;
    FBeforeVersion: TNotifyEvent;
    FUpdateCommands: TUpdateCommandList;
    FEffect: TVersionResult;
    FVersionDate: TDateTime;
    FActive: Boolean;
    FVersionLog: TStringList;
    function GetVersion: TDBVersion;
    procedure SetUpdateCommands(const Value: TUpdateCommandList);
    function GetParent: TDBVersionUpdater;
    procedure SetVersionDate(const Value: TDateTime);
    procedure SetVersion(const Value: TDBVersion);
    procedure SetVersionLog(const Value: TStringList);
  protected
    function GetDisplayName: string; override;

    procedure DoBeforeVersion;
    procedure DoAfterVersion;
  public
    property Parent : TDBVersionUpdater read GetParent;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function Apply : TVersionResult;

  published
    property Active : Boolean read FActive write FActive;
    property Version : TDBVersion read GetVersion write SetVersion;
    property VersionDate : TDateTime read FVersionDate write SetVersionDate;
    property VersionLog : TStringList read FVersionLog write SetVersionLog;
    property Effect : TVersionResult read FEffect write FEffect;
    property UpdateCommands : TUpdateCommandList read FUpdateCommands write SetUpdateCommands;

    property BeforeVersion : TNotifyEvent read FBeforeVersion write FBeforeVersion;
    property AfterVersion : TNotifyEvent read FAfterVersion write FAfterVersion;
  end;

  TVersionItemClass = class of TVersionItem;

  TVersionList = class(TCollection)
  private
    FParent: TDBVersionUpdater;
    function GetVersionItem(Index: Integer): TVersionItem;
    procedure SetVersionItem(Index: Integer; Value: TVersionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AParent: TDBVersionUpdater; AItemClass: TVersionItemClass);
    function Add: TVersionItem;
    property Parent: TDBVersionUpdater read FParent;
    property Items[Index: Integer]: TVersionItem read GetVersionItem write SetVersionItem; default;
  end;

  TExecuteCommandEvent = procedure (Sender : TObject; Command : TUpdateCommand) of object;

  TReadVersionEvent = procedure (Sender : TObject; var Version : TDBVersion) of object;

  TDBVersionUpdater = class(TComponent)
  private
    FOnExecuteCommand: TExecuteCommandEvent;
    FOnWriteVersion: TNotifyEvent;
    FOnReadVersion: TReadVersionEvent;
    FVersions: TVersionList;
    FAllLogs: TStringList;
    procedure SetVersions(const Value: TVersionList);
    function GetVersion: TDBVersion;
    function GetVersionDate: TDateTime;
    procedure SetVersion(const Value: TDBVersion);
    function GetAllLogs: TStringList;
    procedure SetAllLogs(const Value: TStringList);
    procedure SetVersionDate(const Value: TDateTime);
  protected

    procedure DoOnExecuteCommand(Command : TUpdateCommand);
    procedure DoOnReadVersion(var Version : TDBVersion);
    procedure DoOnWriteVersion;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function CheckVersions : TVersionResult;

  published
    property Versions : TVersionList read FVersions write SetVersions;

    //Propriété en lecture seule résumant tout ce qui est contenu dans le composant
    property AllLogs : TStringList read GetAllLogs write SetAllLogs stored false;

    property Version : TDBVersion read GetVersion write SetVersion stored false;
    property VersionDate : TDateTime read GetVersionDate write SetVersionDate stored false;

    property OnExecuteCommand : TExecuteCommandEvent read FOnExecuteCommand write FOnExecuteCommand;
    property OnReadVersion : TReadVersionEvent read FOnReadVersion write FOnReadVersion;
    property OnWriteVersion : TNotifyEvent read FOnWriteVersion write FOnWriteVersion;
  end;

implementation

{ TDBVersionUpdater }

function TDBVersionUpdater.CheckVersions: TVersionResult;
var
  i : Integer;
  Temp : TVersionResult;
  ADBV : TDBVersion;
begin
  Result := vrOK;
  DoOnReadVersion(ADBV);
  if Version > ADBV then
  begin
    for i := 0 to Versions.Count - 1 do
    begin
      if Versions[i].Version > ADBV then
      begin
        Temp := Versions[i].Apply;
        if Temp > Result then
          Result := Temp;
      end;
    end;
    DoOnWriteVersion;
  end;
end;

constructor TDBVersionUpdater.Create(AOwner: TComponent);
begin
  inherited;
  FVersions := TVersionList.Create(Self, TVersionItem);
  FAllLogs := TStringList.Create;
end;

destructor TDBVersionUpdater.Destroy;
begin
  FAllLogs.Free;
  FVersions.Free;
  inherited;
end;

procedure TDBVersionUpdater.DoOnExecuteCommand(Command: TUpdateCommand);
begin
  if Assigned(FOnExecuteCommand) then
    FOnExecuteCommand(Self, Command);
end;

procedure TDBVersionUpdater.DoOnReadVersion(var Version: TDBVersion);
begin
  if Assigned(FOnReadVersion) then
    FOnReadVersion(Self, Version);
end;

procedure TDBVersionUpdater.DoOnWriteVersion;
begin
  if Assigned(FOnWriteVersion) then
    FOnWriteVersion(Self);
end;

function TDBVersionUpdater.GetAllLogs: TStringList;
var
  i, j : Integer;
begin
  FAllLogs.Clear;
  for i := Versions.Count - 1 downto 0 do
  begin
    FAllLogs.Add('Version ' + IntToStr(i) + ' - ' + DateToStr(Versions[i].VersionDate));
    FAllLogs.AddStrings(Versions[i].VersionLog);
    if Versions[i].UpdateCommands.Count > 0 then
    begin
      FAllLogs.Add('Commandes : ');
      for j := 0 to Versions[i].UpdateCommands.Count - 1 do
      begin
        FAllLogs.Add('  - ' + Versions[i].UpdateCommands[j].Legend);
      end;
    end;
    FAllLogs.Add('');
  end;
  Result := FAllLogs;
end;

function TDBVersionUpdater.GetVersion: TDBVersion;
var
  i : Integer;
begin
  if FVersions.Count > 0 then
  begin
    for i := FVersions.Count - 1 downto 0 do
      if FVersions[i].Active then
      begin
        Result := i;
        Exit;
      end;
  end;
  Result := -1;
end;

function TDBVersionUpdater.GetVersionDate: TDateTime;
begin
  if Version >= 0 then
    Result := Versions[Version].VersionDate
  else
    Result := Date;
end;

procedure TDBVersionUpdater.SetAllLogs(const Value: TStringList);
begin
  FAllLogs.Clear;
end;

procedure TDBVersionUpdater.SetVersion(const Value: TDBVersion);
begin
  // Pas d'action ici... en lecture seule !
end;

procedure TDBVersionUpdater.SetVersionDate(const Value: TDateTime);
begin
  // Pas d'action ici... en lecture seule !
end;

procedure TDBVersionUpdater.SetVersions(const Value: TVersionList);
begin
  FVersions.Assign(Value);
end;

{ TVersionList }

function TVersionList.Add: TVersionItem;
begin
  Result := TVersionItem(inherited Add);
end;

constructor TVersionList.Create(AParent: TDBVersionUpdater;
  AItemClass: TVersionItemClass);
begin
  inherited Create(AItemClass);
  FParent := AParent;
end;

function TVersionList.GetOwner: TPersistent;
begin
  Result := FParent;
end;

function TVersionList.GetVersionItem(Index: Integer): TVersionItem;
begin
  Result:= TVersionItem(inherited Items[Index]);
end;

procedure TVersionList.SetVersionItem(Index: Integer; Value: TVersionItem);
begin
  Items[Index].Assign(Value);
end;

procedure TVersionList.Update(Item: TCollectionItem);
begin
  inherited;
  //??? Vérifier dans l'aide l'utilité de cette procédure...
end;

{ TVersionItem }

constructor TVersionItem.Create(Collection: TCollection);
begin
  inherited;
  FUpdateCommands := TUpdateCommandList.Create(Self, TUpdateCommand);
  FVersionLog := TStringList.Create;
  FVersionDate := Date;
end;

destructor TVersionItem.Destroy;
begin
  FVersionLog.Free;
  FUpdateCommands.Free;
  inherited;
end;

function TVersionItem.GetDisplayName: string;
var
  Actif : string;
begin
  if Active then
    Actif := 'A'
  else
    Actif := 'N';
  Result := '{' + Actif + '} ' + DateToStr(FVersionDate) + ' (' + IntToStr(UpdateCommands.Count) + ' commands)';
end;

function TVersionItem.GetParent: TDBVersionUpdater;
begin
  Result := TVersionList(Collection).Parent;
end;

function TVersionItem.GetVersion: TDBVersion;
begin
  Result := Index;
end;

procedure TVersionItem.SetVersionDate(const Value: TDateTime);
begin
  FVersionDate := Trunc(Value);
end;

procedure TVersionItem.SetUpdateCommands(const Value: TUpdateCommandList);
begin
  FUpdateCommands.Assign(Value);
end;

procedure TVersionItem.SetVersion(const Value: TDBVersion);
begin
  Index := Value;
end;

procedure TVersionItem.SetVersionLog(const Value: TStringList);
begin
  FVersionLog.Assign(Value);
end;

function TVersionItem.Apply: TVersionResult;
var
  i : Integer;
begin
  if Active then
  begin
    DoBeforeVersion;
    for i := 0 to UpdateCommands.Count - 1 do
    begin
      UpdateCommands[i].Execute;
    end;
    Result := Effect;
    DoAfterVersion;
  end
  else
    Result := vrOK;
end;

procedure TVersionItem.DoAfterVersion;
begin
  if Assigned(FBeforeVersion) then
    FBeforeVersion(Self);
end;

procedure TVersionItem.DoBeforeVersion;
begin
  if Assigned(FAfterVersion) then
    FAfterVersion(Self);
end;

{ TUpdateCommand }

constructor TUpdateCommand.Create(Collection: TCollection);
begin
  inherited;
  FCommand := TStringList.Create;
  FActive := True;
end;

destructor TUpdateCommand.Destroy;
begin
  FCommand.Free;
  inherited;
end;

procedure TUpdateCommand.DoAfterCommand;
begin
  if Assigned(FAfterCommand) then
    FAfterCommand(Self);
end;

procedure TUpdateCommand.DoBeforeCommand;
begin
  if Assigned(FBeforeCommand) then
    FBeforeCommand(Self);
end;

procedure TUpdateCommand.DoOnCommand;
var
  Handled : Boolean;
begin
  Handled := False;
  if Assigned(FOnCommand) then
    FOnCommand(Self, Handled);
  if not Handled then
    TVersionList(TUpdateCommandList(Collection).Parent.Collection).Parent.DoOnExecuteCommand(Self);
end;

procedure TUpdateCommand.Execute;
begin
  if Active then
  begin
    DoBeforeCommand;
    DoOnCommand;
    DoAfterCommand;
  end;
end;

function TUpdateCommand.GetDisplayName: string;
var
  Actif : string;
begin
  if Active then
    Actif := 'A'
  else
    Actif := 'N';
  Result := '{' + Actif + '} ' +  FLegend + ' [' + FCommand.Text + ']';
end;

function TUpdateCommand.GetVersion: TDBVersion;
begin
  Result := TUpdateCommandList(Collection).Parent.Version;
end;

procedure TUpdateCommand.SetCommand(const Value: TStringList);
begin
  FCommand.Assign(Value);
end;

{ TUpdateCommandList }

function TUpdateCommandList.Add: TUpdateCommand;
begin
  Result := TUpdateCommand(inherited Add);
end;

constructor TUpdateCommandList.Create(AParent: TVersionItem;
  AItemClass: TUpdateCommandClass);
begin
  inherited Create(AItemClass);
  FParent := AParent;
end;

function TUpdateCommandList.GetCommand(Index: Integer): TUpdateCommand;
begin
  Result:= TUpdateCommand(inherited Items[Index]);
end;

function TUpdateCommandList.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TUpdateCommandList.SetCommand(Index: Integer; Value: TUpdateCommand);
begin
  Items[Index].Assign(Value);
end;

procedure TUpdateCommandList.Update(Item: TCollectionItem);
begin
  inherited;
  //??? A vérifier comme l'autre liste...
end;

end.
