unit GZRL_Composant;

{ Unités spécifiquement dépendantes :
    a) FRequetesLibres
    b) FSelModeleEtat
    c) FEditModeleEtat
    d) FEditModelesEtats
    e) RRequetesLibres [ SUPPRIMÉ Après passage sous Report-Builder ]
    f) FChoixExport
}

(*
A propos du DataModeles, les champs doivent peu ou prou avoir les types suivants :
Nom de l'édition : A(100)
Commentaires : Memo
Donnees : Blob binaire
Dates : TDateTime
*)
interface

uses SysUtils, Classes, DB, Graphics, FiltExpt;

type
  TActionRequetesLibres = class(TCollectionItem)
  private
    FActionName: string;
    FGlyph: TBitmap;
    FOnClick: TNotifyEvent;
    FHint: string;
    FNumGlyph: Integer;
    FVisible: Boolean;
    FEnabled: Boolean;
    procedure SetActionName(const Value: string);
    procedure SetGlyph(Value: TBitmap);
    procedure SetOnClick(Value: TNotifyEvent);
    procedure SetHint(const Value: string);
    procedure SetNumGlyph(const Value: Integer);
  public
    constructor Create(Owner: TCollection); override;
    destructor Destroy; override;
  published
    property ActionName: string read FActionName write SetActionName;
    property Hint: string read FHint write SetHint;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property NumGlyph: Integer read FNumGlyph write SetNumGlyph;
    property Visible: Boolean read FVisible write FVisible default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  TCustomActions = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TActionRequetesLibres;
    procedure SetItem(Index: Integer; const Value: TActionRequetesLibres);
  protected
    function GetOwner: TPersistent; override;
    property Owner: TPersistent read FOwner write FOwner;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    property Items[Index: Integer]: TActionRequetesLibres read GetItem write SetItem;
  end;

  TFieldName = string;

  TDataSetModeles = class(TPersistent)
  private
    FOwner: TComponent;
    {}
    FDataSet: TDataSet;
    {}
    FChampNom: TFieldName;
    FChampDateCreation: TFieldName;
    FChampDateModif: TFieldName;
    FChampCommentaires: TFieldName;
    FChampDonnees: TFieldName;
    {}
    procedure SetDataSet(const Value: TDataSet);
  public
    constructor Create(Owner: TComponent);
    {}
    procedure Assign(Source: TPersistent); override;
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    {}
    property ChampNom: TFieldName read FChampNom write FChampNom;
    property ChampDateCreation: TFieldName read FChampDateCreation write FChampDateCreation;
    property ChampDateModif: TFieldName read FChampDateModif write FChampDateModif;
    property ChampCommentaires: TFieldName read FChampCommentaires write FChampCommentaires;
    property ChampDonnees: TFieldName read FChampDonnees write FChampDonnees;
    {Le champ Donnees doit être un champ BINAIRE, BLOB et pas MEMO...
    Enfin à vérifier en fonction de la base de données}
  end;

  TDefaultFormat = record
    FieldName: string;
    DisplayLabel: string;
    DisplayWidth: Integer;
    Index: Integer;
  end;
  PDefaultFormat = ^TDefaultFormat;

  TBeforeExecSQLEvent = procedure(Sender: TObject; var SQL: string) of object;
  TExportEvent = procedure(Sender: TObject; Index, Count: Integer) of object;
  TOnOkClickEvent = procedure(Sender: TObject; SQL: string) of object;
  TCustomPrintEvent = procedure(Sender: TObject; var DefaultPrint: Boolean) of object;

  TRequetesLibres = class(TComponent)
  private
    FExitPanelVisible: Boolean;
    FOnOkClick: TOnOkClickEvent;
    FOnCancelClick : TNotifyEvent;
    FQuery: TDataSet;
    FCountQuery: TDataSet;
    FDefaultSQL: TStrings;
    FDefaultCountSQL: TStrings;
    FAutoCount: Boolean;
    FDataSetModeles: TDataSetModeles;
    FDataSetSnapShots: TDataSetModeles;
    FDefaultOrderBy: string;
    FWindowTitle: string;
    {}
    FDefaultFormat: TList;
    {}
    FOnEditWithButtonClick: TEditButtonClickEvent;
    FOnFillValues: TFillValuesEvent;
    FOnSelectField: TSelectFieldEvent;
    FOnBeforeExecSQL: TBeforeExecSQLEvent;
    FOnExportRec: TExportEvent;
    FBeforeExport: TNotifyEvent;
    FAfterExport: TNotifyEvent;
    FOnPrint: TCustomPrintEvent;
    FCustomActions: TCustomActions;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    {}
    procedure SetQuery(const Value: TDataSet);
    procedure SetDataSetModeles(const Value: TDataSetModeles);
    procedure SetDataSetSnapShots(const Value: TDataSetModeles);
    procedure SetDefaultSQL(const Value: TStrings);
    procedure SetDefaultCountSQL(const Value: TStrings);
    function GetDefaultFormatCount: Integer;
    function GetDefaultFormat(Idx: Integer): TDefaultFormat;
    procedure SetCountQuery(const Value: TDataSet);
    procedure SetCustomActions(const Value: TCustomActions);
  protected
    procedure ClearDefaultFormat;
    {}
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    {}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure UpdateDefaultFormat;
    {}
    property DefaultFormatCount: Integer read GetDefaultFormatCount;
    property DefaultFormat[Idx: Integer]: TDefaultFormat read GetDefaultFormat;
    {}
    procedure Execute;
  published
    property ExitPanelVisible: Boolean read FExitPanelVisible write FExitPanelVisible;
    property DefaultSQL: TStrings read FDefaultSQL write SetDefaultSQL;
    property DefaultCountSQL: TStrings read FDefaultCountSQL write SetDefaultCountSQL;
    property Query: TDataSet read FQuery write SetQuery;
    property CountQuery: TDataSet read FCountQuery write SetCountQuery;
    {}
    property AutoCount: Boolean read FAutoCount write FAutoCount default False;
    {}
    property DefaultOrderBy: string read FDefaultOrderBy write FDefaultOrderBy;
    {}
    property DataSetModeles: TDataSetModeles read FDataSetModeles write SetDataSetModeles;
    property DataSetSnapShots: TDataSetModeles read FDataSetSnapShots write SetDataSetSnapShots;
    {}
    property WindowTitle: string read FWindowTitle write FWindowTitle;
    {}
    property OnFilterExpertEditButtonClick: TEditButtonClickEvent read FOnEditWithButtonClick write FOnEditWithButtonClick;
    property OnFilterExpertFillValues: TFillValuesEvent read FOnFillValues write FOnFillValues;
    property OnFilterExpertSelectField: TSelectFieldEvent read FOnSelectField write FOnSelectField;
    {}
    property BeforeExecSQL: TBeforeExecSQLEvent read FOnBeforeExecSQL write FOnBeforeExecSQL;
    property BeforeExport: TNotifyEvent read FBeforeExport write FBeforeExport;
    property AfterExport: TNotifyEvent read FAfterExport write FAfterExport;
    property OnExportRecord: TExportEvent read FOnExportRec write FOnExportRec;
    property OnOkClick: TOnOkClickEvent read FOnOkClick write FOnOkClick;
    property OnCancelClick: TNotifyEvent read FOnCancelClick write FOnCancelClick;
    property OnPrint: TCustomPrintEvent read FOnPrint write FOnPrint;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property CustomActions: TCustomActions read FCustomActions write SetCustomActions;
  end;

implementation

uses GzConsts, GZRL_FRequetesLibres, Forms, AClasses, ADB;

{ TRequetesLibres }

constructor TRequetesLibres.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {}
  FCustomActions:= TCustomActions.Create(TActionRequetesLibres);
  FCustomActions.Owner:= Self;
  FDefaultSQL:= TStringList.Create;
  FDefaultCountSQL:= TStringList.Create;
  FAutoCount:= False;
  FDataSetModeles:= TDataSetModeles.Create(Self);
  FDataSetSnapShots:= TDataSetModeles.Create(Self);
  FDefaultFormat:= TList.Create;
end;

destructor TRequetesLibres.Destroy;
begin
  FCustomActions.Free;
  ClearDefaultFormat;
  FDefaultFormat.Free;
  FDefaultCountSQL.Free;
  FDefaultSQL.Free;
  FDataSetModeles.Free;
  FDataSetSnapShots.Free;
  {}
  inherited Destroy;
end;

procedure TRequetesLibres.Execute;
var F: TFenRequetesLibres;
begin
  if Assigned(Application) then
    F:= TFenRequetesLibres.Create(Application)
  else
    F:= TFenRequetesLibres.Create(Self);
  try
    F.Query:= Query;
    F.CountQuery:= CountQuery;
    F.DefaultSQL:= DefaultSQL;
    F.DefaultOrderBy:= DefaultOrderBy;
    F.DataSetModeles:= DataSetModeles;
    F.DataSetSnapShots:= DataSetSnapShots;
    F.Title:= WindowTitle;
    F.RequeteLibre:= Self;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TRequetesLibres.ClearDefaultFormat;
var i: Integer;
begin
  for i:= 0 to FDefaultFormat.Count - 1 do
    Dispose(FDefaultFormat[i]);
  FDefaultFormat.Clear;
end;

function TRequetesLibres.GetDefaultFormatCount: Integer;
begin
  Result:= FDefaultFormat.Count;
end;

function TRequetesLibres.GetDefaultFormat(Idx: Integer): TDefaultFormat;
begin
  Result:= PDefaultFormat(FDefaultFormat[Idx])^;
end;

procedure TRequetesLibres.UpdateDefaultFormat;
var i: Integer;
    P: PDefaultFormat;
    F: TField;
begin
  ClearDefaultFormat;
  if Assigned(Query) then
  begin
    for i:= 0 to Query.FieldCount - 1 do
    begin
      New(P);
      F:= Query.Fields[i];
      P^.FieldName:= F.FieldName;
      P^.DisplayLabel:= F.DisplayLabel;
      P^.DisplayWidth:= F.DisplayWidth;
      P^.Index:= F.Index;
      FDefaultFormat.Add(P);
    end;
  end;
end;

procedure TRequetesLibres.Loaded;
begin
  inherited Loaded;
  {}
  UpdateDefaultFormat;
end;

procedure TRequetesLibres.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if AOperation = opRemove then
  begin
    if AComponent = FQuery then
      FQuery:= nil;
    if AComponent = DataSetModeles.DataSet then
      DataSetModeles.DataSet:= nil;
    if AComponent = DataSetSnapShots.DataSet then
      DataSetSnapShots.DataSet:= nil;
  end;
  {}
  inherited Notification(AComponent, AOperation);
end;

procedure TRequetesLibres.SetDataSetModeles(const Value: TDataSetModeles);
begin
  FDataSetModeles.Assign(Value);
end;

procedure TRequetesLibres.SetDataSetSnapShots(const Value: TDataSetModeles);
begin
  FDataSetSnapShots.Assign(Value);
end;

procedure TRequetesLibres.SetDefaultSQL(const Value: TStrings);
begin
  FDefaultSQL.Assign(Value);
end;

procedure TRequetesLibres.SetDefaultCountSQL(const Value: TStrings);
begin
  FDefaultCountSQL.Assign(Value);
end;

procedure TRequetesLibres.SetQuery(const Value: TDataSet);
begin
  if Value <> FQuery then
  begin
    FQuery:= Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    UpdateDefaultFormat
  end;
end;

procedure TRequetesLibres.SetCountQuery(const Value: TDataSet);
begin
  if Value <> FCountQuery then
  begin
    FCountQuery:= Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
  end;
end;

procedure TRequetesLibres.SetCustomActions(const Value: TCustomActions);
begin
  FCustomActions.Assign(Value);
  FCustomActions.FOwner:= Self;
end;

{ TDataSetModeles }

constructor TDataSetModeles.Create(Owner: TComponent);
begin
  FOwner:= Owner;
end;

procedure TDataSetModeles.Assign(Source: TPersistent);
begin
  if TDataSetModeles(Source) is TDataSetModeles then
  begin
    GenericAssignEx(Source, Self);
  end else
    inherited Assign(Source);
end;

procedure TDataSetModeles.SetDataSet(const Value: TDataSet);
begin
  if Assigned(Value) and Assigned(FOwner) then
    Value.FreeNotification(FOwner);
  FDataSet:= Value;
end;

{ TActionRequetesLibres }

constructor TActionRequetesLibres.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  FGlyph:= TBitmap.Create;
  FEnabled := True;
  FVisible := True;
end;

destructor TActionRequetesLibres.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TActionRequetesLibres.SetActionName(const Value: string);
begin
  FActionName := Value;
end;

procedure TActionRequetesLibres.SetGlyph(Value: TBitmap);
begin
  if FGlyph <> nil then
    FGlyph.Assign(Value);
end;

procedure TActionRequetesLibres.SetHint(const Value: string);
begin
  FHint := Value;
end;

procedure TActionRequetesLibres.SetNumGlyph(const Value: Integer);
begin
  FNumGlyph := Value;
end;

procedure TActionRequetesLibres.SetOnClick(Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

{ TCustomActions }

constructor TCustomActions.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner:= nil;
end;

destructor TCustomActions.Destroy;
begin
  inherited;
end;

function TCustomActions.GetItem(Index: Integer): TActionRequetesLibres;
begin
  Result := TActionRequetesLibres(inherited GetItem(Index));
end;

function TCustomActions.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure TCustomActions.SetItem(Index: Integer;
  const Value: TActionRequetesLibres);
begin
  Inherited SetItem(Index, Value);
end;


end.
