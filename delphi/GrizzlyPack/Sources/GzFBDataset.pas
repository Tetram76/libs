unit GzFBDataset;

{$I GrizzlyDefine.INC}

interface

uses
  Windows, Classes, DB, VDataSet, uib, uiblib, uibdataset;

type
  TGzSQLStrings = class(TStringList);

  TRefreshMode = (rmUpdate, rmInsert);
  TRefreshModes = set of TRefreshMode;

  TGzFBParam = class(TParam)
  private
    function GetAsLargeInt: Int64;
    procedure SetAsLargeInt(const Value: Int64);
  public
    property AsLargeInt : Int64 read GetAsLargeInt write SetAsLargeInt;
  end;

  TGzCustomFBDataSet = class(TCustomVirtualDataSet)
  private
    FSelectText: string;
    FDataLink: TDataLink;
    FParams: TParams;
    FParamCheck: Boolean;
    FSQLSelect: TGzSQLStrings;
    FKeepAlive: Boolean;
    FOnClose: TEndTransMode;
    FOnError: TEndTransMode;
    FOpening: Boolean;

    procedure RefreshParams;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetParamsList(Value: TParams);
    function GetParamsCount: Word;
    procedure SetParamsFromDataset;

    procedure SQLSelectChanged(Sender: TObject);
    procedure SetSQLSelect(const Value: TGzSQLStrings);
    function GetDataBase: TUIBDataBase;
    function GetTransaction: TUIBTransaction;
  protected
    FDatasetSelect : TUIBDataSet;

    function GetCanModify: Boolean; override;

    procedure SetDataBase(const Value: TUIBDataBase); virtual;
    procedure SetTransaction(const Value: TUIBTransaction); virtual;

    procedure InternalInitUIB; virtual;

    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    function CanReadDesignData: Boolean; override;

    function GetDataSource: TDataSource; override;
    property DataLink: TDataLink read FDataLink;

    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

    procedure InternalInitFieldDefs; override;
    procedure SetActive(Value: Boolean); override;
    procedure InternalOpen; override;
    procedure DoAfterOpen; override;
    procedure DoAfterClose; override;
    procedure InternalSQLSelectOpen;

    property KeepAlive : Boolean read FKeepAlive write FKeepAlive default False;
    property OnClose : TEndTransMode read FOnClose write FOnClose default etmStayIn;
    property OnError : TEndTransMode read FOnError write FOnError default etmStayIn;
    procedure DoAutoTrans(ATransMode : TEndTransMode; ATransaction : TUIBTransaction);

    function DataSetEventDisabled : Boolean; virtual;

    procedure DoBeforePost; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterPost; override;
    procedure DoAfterEdit; override;
    procedure DoAfterDelete; override;
    procedure DoAfterCancel; override;
    procedure DoAfterInsert; override;
    procedure DoAfterRefresh; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    procedure DoBeforeClose; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeRefresh; override;
    procedure DoBeforeScroll; override;
    procedure DoOnCalcFields; override;
    procedure DoOnNewRecord; override;

    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;

    function ParamByName(const Value: string): TGzFBParam;
    property ParamCount: Word read GetParamsCount;

    procedure Requery;

    //Pour rendre ces éléments visibles car très utiles...
    procedure UseIndex(const IndexFieldNames: string; Options: TIndexOptions; const DescFieldNames: string; CanCreate: Boolean); override;
    property IndexFieldNames;
    property DescFieldNames;

  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Params: TParams read FParams write SetParamsList stored False;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property SQLSelect : TGzSQLStrings read FSQLSelect write SetSQLSelect;
    property DataBase  : TUIBDataBase read GetDataBase write SetDataBase;
    property Transaction: TUIBTransaction read GetTransaction write SetTransaction;
  end;

  TGzFBCachedDataSet = class(TGzCustomFBDataSet)
  private
    {}
  protected
    {}
  public
    { CachedUpdates }
    procedure ApplyUpdates; override;
    procedure CommitUpdates; override;
    procedure CancelUpdates; override;
    {}
    property UpdatePendings;
    property ChangeCount;
  published
    property KeepAlive;
    property ReadOnly;
    property CachedUpdates;
    property OnClose;
    property OnError;
    property OnUpdateRecord;
  end;

  TGzFBDataSet = class(TGzCustomFBDataSet)
  private
    FTwoPhaseUpdate : Boolean;
    FPhaseTwo : Boolean;
    FSQLInsert: TGzSQLStrings;
    FSQLUpdate: TGzSQLStrings;
    FSQLDelete: TGzSQLStrings;
    FSQLRefresh: TGzSQLStrings;
    FRefreshMode: TRefreshModes;
    FAutoApplyUpdate: Boolean;
    FOnExec: TEndTransMode;

    procedure SetUpdateTransaction(const Value: TUIBTransaction);
    function GetUpdateTransaction: TUIBTransaction;
    procedure SetSQLInsert(const Value: TGzSQLStrings);
    procedure SetSQLUpdate(const Value: TGzSQLStrings);
    procedure SetSQLDelete(const Value: TGzSQLStrings);
    procedure SetSQLRefresh(const Value: TGzSQLStrings);

    procedure SetAutoApplyUpdates(const Value: Boolean);

  protected
    FDatasetRefresh : TUIBDataSet;
    FQueryUpdate, FQueryDelete, FQueryInsert : TUIBQuery;

    procedure SetDataBase(const Value: TUIBDataBase); override;
    procedure SetTransaction(const Value: TUIBTransaction); override;

    function DataSetEventDisabled : Boolean; override;

    procedure InternalInitUIB; override;

    procedure Notification(Component: TComponent; Operation: TOperation); override;

    procedure UpdateRecord(Update: TCachedUpdate; OldValues, NewValues: TDataSet; ModifiedFields: TList); override;
    procedure DoSQLModify(OldValues, NewValues: TDataSet; AQuery : TUIBQuery); virtual;
    procedure DoSQLRefresh(OldValues, NewValues: TDataSet); virtual;

    procedure InternalPost; override;
    procedure DoAfterPost; override;
    procedure InternalDelete; override;
    procedure DoAfterClose; override;

    procedure CheckAutoApplyUpdates;
    procedure CheckAutoApplyRefresh;


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdates; override;
    property UpdatePendings;
    procedure CommitTrans;
    procedure RollbackTrans;

  published
    property SQLInsert : TGzSQLStrings read FSQLInsert write SetSQLInsert;
    property SQLUpdate : TGzSQLStrings read FSQLUpdate write SetSQLUpdate;
    property SQLDelete : TGzSQLStrings read FSQLDelete write SetSQLDelete;
    property SQLRefresh : TGzSQLStrings read FSQLRefresh write SetSQLRefresh;

    property RefreshMode : TRefreshModes read FRefreshMode write FRefreshMode default [rmUpdate, rmInsert];
    property AutoApplyUpdates : Boolean read FAutoApplyUpdate write SetAutoApplyUpdates default True;

    property UpdateTransaction: TUIBTransaction read GetUpdateTransaction write SetUpdateTransaction;

    property KeepAlive;
    property OnClose;
    property OnError;
    property OnExec : TEndTransMode read FOnExec write FOnExec default etmCommitRetaining;
  end;


function FindUIBParam(SP : TSQLParams; AName : string; var Index : integer) : Boolean;
procedure AssignParamToSQLParams(UIB: TUIBDataSet; P : TParam; SP : TSQLParams);
procedure AssignFieldToSQLParams(UIB: TUIBStatement; F : TField; SP : TSQLParams; APrefix : string = ''); overload;
procedure AssignFieldToSQLParams(UIB: TUIBDataSet; F : TField; SP : TSQLParams; APrefix : string = ''); overload;

implementation

uses
  SysUtils, DBConsts, TypInfo, GzFBFields;

resourcestring
{$IFDEF FRENCH}
  msgFieldTypeNotSupported = 'Type de données non supporté %s';
{$ENDIF}
{$IFDEF ENGLISH}
  msgFieldTypeNotSupported = 'Field type not supported %s';
{$ENDIF}

type
  TInternalDataLink = class(TDetailDataLink)
  private
    FDataset: TGzCustomFBDataSet;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ADataset: TGzCustomFBDataSet);
  end;

{ TGzCustomFBDataSet }

procedure TGzFBDataSet.ApplyUpdates;
begin
  inherited;
  if not FTwoPhaseUpdate then
    CommitUpdates; //Vide la liste des mises à jour...
end;

procedure AssignFieldToSQLParams(UIB: TUIBStatement; F: TField; SP: TSQLParams; APrefix : string = '');
var
  k : Integer;
  procedure SetTheBlob;
  var
    MS : TMemoryStream;
    Str : string;
  begin
    if F.IsBlob then
    begin
      MS := TMemoryStream.Create;
      try
        TBlobField(F).SaveToStream(MS);
        MS.Position := 0;
        UIB.ParamsSetBlob(k, MS);
      finally
        MS.Free;
      end;
    end
    else
    begin
      Str := F.AsString;
      UIB.ParamsSetBlob(k, Str);
    end;
  end;
begin
  if FindUIBParam(SP, APrefix + F.FieldName, k) then
  begin
    if F.IsBlob then
      SetTheBlob
    else if F.IsNull then
      SP.IsNull[k] := True
    else
    case F.DataType of
      ftSmallint : SP.AsSmallint[k] := F.AsInteger;
      ftInteger : SP.AsInteger[k] := F.AsInteger;
      ftFloat : SP.AsDouble[k] := F.AsFloat;
      ftCurrency : SP.AsCurrency[k] := F.AsCurrency;
      ftLargeint : SP.AsInt64[k] := Trunc(F.AsFloat); //??? AsFloat ? Les paramètres ne supportent pas les Int64 ?
      ftString : SP.AsString[k] := F.AsString;
      ftWideString : SP.AsWideString[k] := F.AsString; //??? Encore un truc bizarre...
      ftDate, ftTime, ftDateTime : SP.AsDateTime[k] := F.AsDateTime;
      ftBoolean : SP.AsBoolean[k] := F.AsBoolean;
      ftBCD : SP.AsCurrency[k] := F.AsCurrency;
      ftMemo, ftBlob : SetTheBlob;
    else
      DatabaseError(Format(msgFieldTypeNotSupported,
            [GetEnumName(TypeInfo(TFieldType), Integer(F.DataType))]));
    end;
  end;
end;

procedure AssignFieldToSQLParams(UIB: TUIBDataSet; F: TField; SP: TSQLParams; APrefix: string);
var
  k : Integer;
  procedure SetTheBlob;
  var
    MS : TMemoryStream;
    Str : string;
  begin
    if F.IsBlob then
    begin
      MS := TMemoryStream.Create;
      try
        TBlobField(F).SaveToStream(MS);
        MS.Position := 0;
        UIB.ParamsSetBlob(k, MS);
      finally
        MS.Free;
      end;
    end
    else
    begin
      Str := F.AsString;
      UIB.ParamsSetBlob(k, Str);
    end;
  end;
begin
  if FindUIBParam(SP, APrefix + F.FieldName, k) then
  begin
    if F.IsBlob then
      SetTheBlob
    else if F.IsNull then
      SP.IsNull[k] := True
    else
    case F.DataType of
      ftSmallint : SP.AsSmallint[k] := F.AsInteger;
      ftInteger : SP.AsInteger[k] := F.AsInteger;
      ftFloat : SP.AsDouble[k] := F.AsFloat;
      ftCurrency : SP.AsCurrency[k] := F.AsCurrency;
      ftLargeint : SP.AsInt64[k] := Trunc(F.AsFloat); //??? AsFloat ? Les paramètres ne supportent pas les Int64 ?
      ftString : SP.AsString[k] := F.AsString;
      ftWideString : SP.AsWideString[k] := F.AsString; //??? Encore un truc bizarre...
      ftDate, ftTime, ftDateTime : SP.AsDateTime[k] := F.AsDateTime;
      ftBoolean : SP.AsBoolean[k] := F.AsBoolean;
      ftBCD : SP.AsCurrency[k] := F.AsCurrency;
      ftMemo, ftBlob : SetTheBlob;
    else
      DatabaseError(Format(msgFieldTypeNotSupported,
            [GetEnumName(TypeInfo(TFieldType), Integer(F.DataType))]));
    end;
  end;
end;

procedure AssignParamToSQLParams(UIB: TUIBDataSet; P: TParam; SP: TSQLParams);
var
  k : Integer;
  procedure SetTheBlob;
  var
    Str : string;
  begin
    Str := P.AsBlob;
    UIB.ParamsSetBlob(k, Str);
  end;
begin
  if FindUIBParam(SP, P.Name, k) then
  begin
    if P.DataType in [ftMemo, ftBlob] then
      SetTheBlob
    else
    if P.IsNull then
      SP.IsNull[k] := True
    else
    case P.DataType of
      ftSmallint : SP.AsSmallint[k] := P.AsSmallInt;
      ftInteger : SP.AsInteger[k] := P.AsInteger;
      ftFloat : SP.AsDouble[k] := P.AsFloat;
      ftCurrency : SP.AsCurrency[k] := P.AsCurrency;
      ftLargeint : SP.AsInt64[k] := Trunc(P.AsFloat); //??? AsFloat ? Les paramètres ne supportent pas les Int64 ?
      ftString : SP.AsString[k] := P.AsString;
      ftWideString : SP.AsWideString[k] := P.AsString; //??? Encore un truc bizarre...
      ftDate, ftTime, ftDateTime : SP.AsDateTime[k] := P.AsDateTime;
      ftBoolean : SP.AsBoolean[k] := P.AsBoolean;
      ftBCD : SP.AsCurrency[k] := P.AsCurrency;
    else
      DatabaseError(Format(msgFieldTypeNotSupported,
            [GetEnumName(TypeInfo(TFieldType), Integer(P.DataType))]));
    end;
  end;
end;

function TGzCustomFBDataSet.CanReadDesignData: Boolean;
begin
  Result := False;
end;

procedure TGzFBDataSet.CheckAutoApplyRefresh;
begin
  if (not Importing) and (not ApplyingUpdates) and AutoApplyUpdates and UpdatePendings then
  begin
    FTwoPhaseUpdate := True;
    FPhaseTwo := True;
    try
      ApplyUpdates;
      CommitUpdates;
    finally
      FPhaseTwo := False;
      FTwoPhaseUpdate := False;
    end;
    DataEvent(deRecordChange, 0);
  end;
end;

procedure TGzFBDataSet.CheckAutoApplyUpdates;
begin
  if (not Importing) and (not ApplyingUpdates) and AutoApplyUpdates and UpdatePendings then
  begin
    FTwoPhaseUpdate := True;
    try
      ApplyUpdates;
    finally
      FTwoPhaseUpdate := False;
    end;
  end;
end;

procedure TGzFBDataSet.CommitTrans;
begin
  UpdateTransaction.Commit;
end;

constructor TGzCustomFBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQLSelect := TGzSQLStrings.Create;
  FSQLSelect.OnChange := SQLSelectChanged;

  FDataLink := TInternalDataLink.Create(Self);
  FParams := TParams.Create(Self);

  ParamCheck := True;

  FDatasetSelect := TUIBDataSet.Create(nil);
  FDatasetSelect.OnClose := etmStayIn;
  FDatasetSelect.OnError := etmStayIn;

  FOnClose := etmStayIn;
  FOnError := etmStayIn;

  CachedUpdates := True; 
end;

function TGzCustomFBDataSet.DataSetEventDisabled: Boolean;
begin
  Result := Importing or FOpening;
end;

procedure TGzCustomFBDataSet.DefineProperties(Filer: TFiler);
  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TGzCustomFBDataSet(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;
begin
  inherited;
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

function TGzCustomFBDataSet.GetCanModify: Boolean;
begin
  Result := DataSetEventDisabled or (not ReadOnly);
end;

destructor TGzCustomFBDataSet.Destroy;
begin
  FDatasetSelect.Free;
  FSQLSelect.Free;

  FParams.Free;
  FDataLink.Free;
  inherited Destroy;
end;

constructor TGzFBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQLRefresh := TGzSQLStrings.Create;
  FSQLInsert := TGzSQLStrings.Create;
  FSQLUpdate := TGzSQLStrings.Create;
  FSQLDelete := TGzSQLStrings.Create;

  FDatasetRefresh := TUIBDataSet.Create(nil);
  FQueryInsert := TUIBQuery.Create(nil);
  FQueryUpdate := TUIBQuery.Create(nil);
  FQueryDelete := TUIBQuery.Create(nil);

  FDatasetRefresh.OnClose := etmStayIn;
  FDatasetRefresh.OnError := etmStayIn;
  FQueryInsert.OnError := etmStayIn;
  FQueryUpdate.OnError := etmStayIn;
  FQueryDelete.OnError := etmStayIn;

  FRefreshMode := [rmUpdate, rmInsert];
  FAutoApplyUpdate := True;
  FOnExec := etmCommitRetaining;
end;

function TGzFBDataSet.DataSetEventDisabled: Boolean;
begin
  Result := (inherited DataSetEventDisabled) or ApplyingUpdates;
end;

destructor TGzFBDataSet.Destroy;
begin
  FQueryDelete.Free;
  FQueryUpdate.Free;
  FQueryInsert.Free;
  FDatasetRefresh.Free;

  FSQLDelete.Free;
  FSQLInsert.Free;
  FSQLUpdate.Free;
  FSQLRefresh.Free;
  inherited Destroy;
end;

procedure TGzCustomFBDataSet.DoAfterCancel;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterClose;
begin
  if FDatasetSelect.Active then
    FDatasetSelect.Close;
  DoAutoTrans(OnClose, Transaction);
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterDelete;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterEdit;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterInsert;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterOpen;
begin
  //On ouvre le vrai ensemble... et on le transfère !
  try
    InternalSQLSelectOpen;
  finally
    SetState(dsInactive);
    FOpening := False;
    SetState(dsBrowse);
  end;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterPost;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzFBDataSet.DoAfterClose;
begin
  DoAutoTrans(OnClose, UpdateTransaction);
  inherited;
end;

procedure TGzFBDataSet.DoAfterPost;
begin
  if DataSetEventDisabled then
    Exit;
  CheckAutoApplyRefresh;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterRefresh;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAfterScroll;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeCancel;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeClose;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeDelete;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeEdit;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeInsert;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforePost;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeRefresh;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoBeforeScroll;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoOnCalcFields;
begin
  if Importing then Exit; //Attention, même en cas de mise à jour, on doit
                          //pouvoir calculer les champs calculés !
  inherited;
end;

procedure TGzCustomFBDataSet.DoOnNewRecord;
begin
  if DataSetEventDisabled then
    Exit;
  inherited;
end;

procedure TGzCustomFBDataSet.DoAutoTrans(ATransMode: TEndTransMode;
  ATransaction: TUIBTransaction);
var
  AETM : TEndTransMode;
begin
  if ATransMode = etmDefault then
    AETM := ATransaction.DefaultAction
  else
    AETM := ATransMode;
  case AETM of
    etmCommit : ATransaction.Commit;
    etmRollback : ATransaction.RollBack;
    etmCommitRetaining : ATransaction.CommitRetaining;
    etmRollbackRetaining : ATransaction.RollBackRetaining;
  end;
end;

procedure TGzFBDataSet.DoSQLModify(OldValues, NewValues: TDataSet;
  AQuery: TUIBQuery);
  procedure AssignFields(Prefixe : string; AFields : TFields);
  var
    i : Integer;
  begin
    for i := 0 to AFields.Count - 1 do
      AssignFieldToSQLParams(AQuery, AFields[i], AQuery.Params, Prefixe);
  end;
begin
  //Ici, la requête est prête... Il n'y a qu'à assigner les Params avec
  //les bonnes valeurs.
  //On a les valeurs NEW_ et OLD_ aussi à fusionner...

  //Il faudra penser au cas où un paramètre garderait une ancienne valeur...
  //c'est à dire à tout remettre à null avant toute chose !

  if not UpdateTransaction.InTransaction then
    UpdateTransaction.StartTransaction;

  try

    AssignFields('NEW_', NewValues.Fields);
    AssignFields('OLD_', OldValues.Fields);
    AssignFields('', NewValues.Fields);

    AQuery.ExecSQL;

    DoAutoTrans(OnExec, UpdateTransaction);
  except
    DoAutoTrans(OnError, UpdateTransaction);
    raise;
  end;
end;

procedure TGzFBDataSet.DoSQLRefresh(OldValues, NewValues: TDataSet);
  procedure AssignFields(Prefixe : string; AFields : TFields);
  var
    i : Integer;
  begin
    for i := 0 to AFields.Count - 1 do
      AssignFieldToSQLParams(FDatasetRefresh, AFields[i], FDatasetRefresh.Params, Prefixe);
  end;
begin
  if Trim(FSQLRefresh.Text) = '' then
    Exit;

  AssignFields('NEW_', NewValues.Fields);
  AssignFields('OLD_', OldValues.Fields);
  AssignFields('', NewValues.Fields);

  FDatasetRefresh.Open;
  try
    if (not FDatasetRefresh.IsEmpty) then
    begin
      Edit;
      ImportCurrentRecord(FDatasetRefresh);
      Post;
    end;
  finally
    FDatasetRefresh.Close;
  end;
end;

function FindUIBParam(SP: TSQLParams; AName: string; var Index: integer): Boolean;
var
  i : Integer;
begin
  Index := -1;
  Result := False;
  for i := 0 to SP.FieldCount - 1 do
  begin
    if AnsiCompareText(SP.FieldName[i], AName) = 0 then
    begin
      Index := i;
      Result := True;
      Exit;
    end;
  end;
end;

function TGzCustomFBDataSet.GetDataBase: TUIBDataBase;
begin
  Result := FDatasetSelect.Database;
end;

function TGzCustomFBDataSet.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TGzCustomFBDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);
  function AddFieldToList(const FieldName: string; DataSet: TDataSet;
    List: TList): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if (Field <> nil) then
      List.Add(Field);
    Result := Field <> nil;
  end;
var
  i: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;

function TGzCustomFBDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftBCD then
    Result := TUIBBCDField
  else
  if FieldType = ftString then
    Result := TFBStringField
  else
    Result := inherited GetFieldClass(FieldType);
end;

function TGzCustomFBDataSet.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

function TGzCustomFBDataSet.GetTransaction: TUIBTransaction;
begin
  Result := FDatasetSelect.Transaction;
end;

function TGzFBDataSet.GetUpdateTransaction: TUIBTransaction;
begin
  Result := FQueryUpdate.Transaction;
end;

procedure TGzFBDataSet.InternalDelete;
begin
  inherited;
  CheckAutoApplyUpdates;
end;

procedure TGzCustomFBDataSet.InternalInitFieldDefs;
begin
  InternalInitUIB;
  FieldDefs.Clear;
  if (csDesigning in ComponentState) then
  begin
    try
      FDatasetSelect.Open;
      ImportFieldDefs(FDatasetSelect);
    finally
      FDatasetSelect.Close;
    end;
  end
  else
  begin
    FDatasetSelect.FieldDefs.Update;
    ImportFieldDefs(FDatasetSelect);
  end;
end;

procedure TGzCustomFBDataSet.InternalInitUIB;
begin
  FDatasetSelect.Close;
  FDatasetSelect.SQL.Assign(SQLSelect);
end;

procedure TGzFBDataSet.InternalInitUIB;
begin
  inherited;
  FDatasetRefresh.Close;
  FDatasetRefresh.SQL.Assign(SQLRefresh);
  FQueryUpdate.SQL.Assign(SQLUpdate);
  FQueryDelete.SQL.Assign(SQLDelete);
  FQueryInsert.SQL.Assign(SQLInsert);
end;

procedure TGzCustomFBDataSet.InternalOpen;
begin
  FOpening := True;
  //On s'assure de la bonne définition des champs...
  if DefaultFields then //Sinon, c'est InternalInitFieldDefs qui fait le travail...
  begin
    InternalInitUIB;
    FDatasetSelect.FieldDefs.Update;
    ImportFieldDefs(FDatasetSelect);
  end;
  //Et on ouvre...
  inherited;
  //Le transfert des données se fait dans DoAfterOpen...
end;

procedure TGzFBDataSet.InternalPost;
begin
  inherited;
  CheckAutoApplyUpdates;
end;

procedure TGzCustomFBDataSet.InternalSQLSelectOpen;
var
  i : Integer;
  OldCU : Boolean;
  
  procedure CheckFieldsProperties;
  var
    i : Integer;
    F : TField;
  begin
    for i:= 0 to FieldCount - 1 do
    begin
      F:= FDatasetSelect.FindField(Fields[i].FieldName);
      if Assigned(F) and (Fields[i].FieldKind = fkData) then
      begin
        if (Fields[i].Origin = '') then
          Fields[i].Origin := F.Origin;
        {if (not Active) and (Fields[i].DataType = ftBCD) and (TBCDField(Fields[i]).Precision <> TBCDField(F).Precision) then
          TBCDField(Fields[i]).Precision := TBCDField(F).Precision;}
      end;
    end;
  end;
  
begin
  if FDatasetSelect.Active then
    FDatasetSelect.Close;

  //Puis on s'occupe des paramètres...
  SetParamsFromDataset; // En cas de liaison maître/détail

  for i := 0 to ParamCount - 1 do
    AssignParamToSQLParams(FDatasetSelect, Params[i], FDatasetSelect.Params);

  OldCU := CachedUpdates;
  try
    CachedUpdates := False;
    try
      FDatasetSelect.Open;
    except
      DoAutoTrans(OnError, Transaction);
      raise;
    end;
    try
      DisableControls;
      try
        ImportDataSet(FDatasetSelect);
        CheckFieldsProperties;
      finally
        EnableControls;
      end;
    finally
      if not KeepAlive then
        FDatasetSelect.Close;
    end;
    First;
  finally
    CachedUpdates := OldCU;
  end;
end;

procedure TGzCustomFBDataSet.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (Component = DataSource) then
      DataSource:= nil
    else
    if (Component = Transaction) then
      Transaction := nil
    else
    if (Component = DataBase) then
      DataBase := nil;
  end;
end;

procedure TGzFBDataSet.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (Component = UpdateTransaction) then
      UpdateTransaction := nil;
  end;
end;

function TGzCustomFBDataSet.ParamByName(const Value: string): TGzFBParam;
begin
  Result := TGzFBParam(FParams.ParamByName(Value));
end;

procedure TGzCustomFBDataSet.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TGzCustomFBDataSet.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if Assigned(DataSet) then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
        begin
          Close;
          Open;
        end;
    end;
  finally
    EnableControls;
  end;
end;

procedure TGzCustomFBDataSet.Requery;
begin
  Close;
  Open;
end;

procedure TGzFBDataSet.RollbackTrans;
begin
  UpdateTransaction.RollBack;
end;

procedure TGzFBDataSet.SetAutoApplyUpdates(const Value: Boolean);
begin
  FAutoApplyUpdate := Value;
  if Value and UpdatePendings then
    ApplyUpdates;
end;

procedure TGzCustomFBDataSet.SetDataBase(const Value: TUIBDataBase);
begin
  FDatasetSelect.Database := Value;
end;

procedure TGzCustomFBDataSet.SetDataSource(const Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FDataLink.DataSource := Value;
end;

procedure TGzCustomFBDataSet.SetParamsFromDataset;
var
  I: Integer;
  DataSet: TDataSet;
  AField : TField;
begin
  if FDataLink.DataSource <> nil then
  begin
    DataSet := FDataLink.DataSource.DataSet;
    if (DataSet <> nil) then
    begin
      if not DataSet.Active then
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        with FParams[I] do
          if not Bound then
          begin
            AField := DataSet.FindField(Name);
            if Assigned(AField) then
            begin
              AssignField(AField);
              Bound := False;
            end
            else
            begin
              Clear;
            end;
          end;
    end;
  end;
end;

procedure TGzCustomFBDataSet.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

procedure TGzFBDataSet.SetDataBase(const Value: TUIBDataBase);
begin
  inherited;
  FDatasetRefresh.Database := Value;
  FQueryUpdate.Database := Value;
  FQueryInsert.Database := Value;
  FQueryDelete.Database := Value;
end;

procedure TGzFBDataSet.SetSQLDelete(const Value: TGzSQLStrings);
begin
  CheckInactive;
  FSQLDelete.Assign(Value);
end;

procedure TGzFBDataSet.SetSQLInsert(const Value: TGzSQLStrings);
begin
  CheckInactive;
  FSQLInsert.Assign(Value);
end;

procedure TGzFBDataSet.SetSQLRefresh(const Value: TGzSQLStrings);
begin
  CheckInactive;
  FSQLRefresh.Assign(Value);
end;

procedure TGzCustomFBDataSet.SetSQLSelect(const Value: TGzSQLStrings);
begin
  CheckInactive;
  FSQLSelect.Text := Value.Text;
  FDatasetSelect.SQL.Text := Value.Text;
  FDatasetSelect.FieldDefs.Clear;
  DataEvent(dePropertyChange, 0);
end;

procedure TGzFBDataSet.SetSQLUpdate(const Value: TGzSQLStrings);
begin
  CheckInactive;
  FSQLUpdate.Assign(Value);
end;

procedure TGzCustomFBDataSet.SetTransaction(const Value: TUIBTransaction);
begin
  CheckInactive; // ??? Vérifier la validité de ce truc... en cas de suppression du composant rattaché...
  FDatasetSelect.Transaction := Value;
end;

procedure TGzFBDataSet.SetTransaction(const Value: TUIBTransaction);
begin
  inherited;
  if (UpdateTransaction = nil) then
    UpdateTransaction := Value;
end;

procedure TGzFBDataSet.SetUpdateTransaction(const Value: TUIBTransaction);
begin
  CheckInactive;
  FQueryUpdate.Transaction := Value;
  FQueryDelete.Transaction := Value;
  FQueryInsert.Transaction := Value;
  FDatasetRefresh.Transaction := Value; 
end;

procedure TGzCustomFBDataSet.SQLSelectChanged(Sender: TObject);
var
  List: TParams;
begin
  if not (csReading in ComponentState) then
  begin
    Close;
    if ParamCheck or (csDesigning in ComponentState) then
    begin
      List := TParams.Create(Self);
      try
        FSelectText := List.ParseSQL(SQLSelect.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end else
      FSelectText := SQLSelect.Text;
  end else
    FSelectText := FParams.ParseSQL(SQLSelect.Text, False);
end;

procedure TGzFBDataSet.UpdateRecord(Update: TCachedUpdate; OldValues,
  NewValues: TDataSet; ModifiedFields: TList);
  procedure ForceRefresh;
  var
    OldR, OldF : Boolean;
    Bmk : TBookmarkStr;
  begin
    DisableControls;
    try
      Bmk := Bookmark;
      try
        OldF := Filtered;
        OldR := RangeActive;
        try
          CancelRange;
          Filtered := False;
          RecNo := AllRecords.IndexOf(Update.RecId) + 1;
          DoSQLRefresh(OldValues, NewValues);
          if Update.UpdateKind = ukInsert then
            Bmk := Bookmark;
        finally
          if OldR then
            ApplyRange;
          Filtered := OldF;
        end;
      finally
        Bookmark := Bmk;
      end;
    finally
      EnableControls;
    end;        
  end;
begin
  try
    case Update.UpdateKind of
      ukModify :
      begin
        if not FPhaseTwo then
          DoSQLModify(OldValues, NewValues, FQueryUpdate);
        if (rmUpdate in RefreshMode) then
        begin
          if FPhaseTwo or (not FTwoPhaseUpdate) then
            ForceRefresh;
        end;
      end;
      ukInsert :
      begin
        if not FPhaseTwo then
          DoSQLModify(OldValues, NewValues, FQueryInsert);
        if rmInsert in RefreshMode then
        begin
          if FPhaseTwo or (not FTwoPhaseUpdate) then
            ForceRefresh;
        end;
      end;
      ukDelete :
      begin
        if not FPhaseTwo then
          DoSQLModify(OldValues, NewValues, FQueryDelete);
      end;
    end;
  except
    if AutoApplyUpdates and (not FPhaseTwo) then
    begin
      SetState(dsBrowse);
      case Update.UpdateKind of
        ukModify : //On repasse en édition
        begin
          RecNo := CurrentRecords.IndexOf(Update.RecId) + 1;
          Edit;
          ImportCurrentRecord(OldValues);
          Post;
          Edit;
          ImportCurrentRecord(NewValues);
        end;
        ukInsert : //On repasse en insertion
        begin
          RecNo := CurrentRecords.IndexOf(Update.RecId) + 1;
          Delete;
          Insert;
          ImportCurrentRecord(NewValues);
        end;
        ukDelete : //On recrée l'enregistrement
        begin
          Insert;
          ImportCurrentRecord(OldValues);
          Post;
        end;
      end;
      CommitUpdates;
    end;
    //Plus tard, il faudra ajouter le support des évènements TDataSetErrorEvent
    raise;
  end;
  inherited;
end;

procedure TGzCustomFBDataSet.UseIndex(const IndexFieldNames: string;
  Options: TIndexOptions; const DescFieldNames: string;
  CanCreate: Boolean);
begin
  inherited;
end;

procedure TGzCustomFBDataSet.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TGzCustomFBDataSet.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if DataSetEventDisabled then
    Exit
  else
    inherited;
(*  case Event of
    deDatasetChange:
      //if not DataSetEventDisabled then
        inherited;
    deFieldChange:
      if not DataSetEventDisabled then
        inherited;
    {deUpdateRecord:
      if not DataSetEventDisabled then
        inherited;
    deCheckBrowseMode:
      if not DataSetEventDisabled then
        inherited;
    deUpdateState:
      if not DataSetEventDisabled then
        inherited;}
  else
    inherited;
  end;*)
end;

procedure TGzCustomFBDataSet.SetActive(Value: Boolean);
begin
  try
    inherited;
  finally
    FOpening := False; //Protection pour que les évènements ne soient pas bloqués à jamais...
  end;
end;

{ TInternalDataLink }

procedure TInternalDataLink.ActiveChanged;
begin
  if FDataset.Active then
    FDataset.RefreshParams;
end;

procedure TInternalDataLink.CheckBrowseMode;
begin
  if FDataset.Active then
    FDataset.CheckBrowseMode;
end;

constructor TInternalDataLink.Create(ADataset: TGzCustomFBDataSet);
begin
  inherited Create;
  FDataset := ADataset;
end;

function TInternalDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataset;
end;

procedure TInternalDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataset.Active then
    FDataset.RefreshParams;
end;

{ TGzFBCachedDataSet }

procedure TGzFBCachedDataSet.ApplyUpdates;
begin
  inherited;
end;

procedure TGzFBCachedDataSet.CancelUpdates;
begin
  inherited;
end;

procedure TGzFBCachedDataSet.CommitUpdates;
begin
  inherited;
end;

{ TGzFBParam }

function TGzFBParam.GetAsLargeInt: Int64;
begin
  Result := Trunc(AsFloat);
end;

procedure TGzFBParam.SetAsLargeInt(const Value: Int64);
begin
  AsFloat := Value;
end;

end.
