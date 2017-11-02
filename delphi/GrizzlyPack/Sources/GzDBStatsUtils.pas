unit GzDBStatsUtils;

{ Useful functions to calculate and manipulate stats contained in Datasets }

interface

uses Classes, SysUtils, DB, Contnrs;

type
  TCalculKind = (ckMode, ckMode3, ckMean, ckMax, ckMin, ckMedian, ckVar, ckStdDev, ckSum, ckSCE, ckCount, ckMinMaxList);
  TCalculatedSet = set of TCalculKind;

const
  CalculatedLabels : array[TCalculKind] of string =
    ('Mode', 'Mode3', 'Moyenne', 'Max', 'Min', 'Médiane', 'Variance', 'Ecart-type', 'Somme', 'SCE', 'Effectif', 'Lst min/max');

  AlwaysCalculatedSet = [ckSum, ckMean, ckMax, ckMin, ckCount];

type
  EDBStatsError = class(Exception);

  PArrayOfDouble = ^TArrayOfDouble;
  TArrayOfDouble = array[0..0] of Double;

  TStatsArray = array[TCalculKind] of Double;

  TDatasetStats = class;

  TFieldStatsCollection = class;

  TNextMinMaxAction = (nmContinue, nmNextCandidate);

  TMinMaxKind = (mkMin, mkFloor, mkMax);
  TMinMaxKinds = set of TMinMaxKind;

  TMinMaxCandidate = class
  private
    FValuesBefore: Cardinal;
    FValuesAfter: Cardinal;
    FValue: Double;
    FLastValue: Double;
    FFirstValue: Double;
    FValueIndex: Cardinal;
    FKind: TMinMaxKind;
    function GetIsFloor: Boolean;
    function GetIsMax: Boolean;
    function GetIsMin: Boolean;
    function GetFirstIndex: Integer;
    function GetLastIndex: Integer;
  public
    property IsMax : Boolean read GetIsMax;
    property IsMin : Boolean read GetIsMin;
    property IsFloor : Boolean read GetIsFloor;
    property Kind : TMinMaxKind read FKind;

    property Value : Double read FValue; //Valeur du Min/Max
    property ValueIndex : Cardinal read FValueIndex; //N° de valeur du Min/Max
    property ValuesBefore : Cardinal read FValuesBefore; //Nombre de valeurs depuis le palier précédent
    property ValuesAfter : Cardinal read FValuesAfter; //Nombre de valeurs jusqu'au palier suivant
    property FirstValue : Double read FFirstValue; //Valeur du palier précédent
    property LastValue : Double read FLastValue; //Valeur du prochain palier
    property FirstIndex : Integer read GetFirstIndex;
    property LastIndex : Integer read GetLastIndex;

    //Utilisés pour le mode "Exact"
    constructor Create(AFirstValue, ASecondValue : Double); overload;
    constructor Create(AValue : Double; ACandidate : TMinMaxCandidate; AIndex : Integer; InsertAfter : Boolean = True); overload;
    function AddValue(AValue : Double; AIndex : Integer) : TNextMinMaxAction;

    //Utilisés pour le mode "Peak"
    constructor Create(AFirstValue : Double; AKind : TMinMaxKind); overload;
  end;

  TMinMaxFilterOption = (moMinAmplitude, moMinLength, moMaxLength);
  TMinMaxFilterOptions = set of TMinMaxFilterOption;

  TMinMaxFilter = class(TPersistent)
  private
    FMinAmplitude: Double;
    FOptions: TMinMaxFilterOptions;
    FMinLength: Cardinal;
    FMaxLength: Cardinal;
    FKinds: TMinMaxKinds;
  published
    property MinAmplitude : Double read FMinAmplitude write FMinAmplitude;
    property MinLength : Cardinal read FMinLength write FMinLength;
    property MaxLength : Cardinal read FMaxLength write FMaxLength;
    property Options : TMinMaxFilterOptions read FOptions write FOptions default [];
    property Kinds : TMinMaxKinds read FKinds write FKinds default [mkMin, mkFloor, mkMax];
  end;

  TMinMaxMode = (mmExact, mmPeak);

  TMinMaxList = class(TObjectList)
  private
    FMinMaxMode: TMinMaxMode;
    FMinValue: Double;
    function GetItems(Index: Integer): TMinMaxCandidate;
    procedure SetItems(Index: Integer; const Value: TMinMaxCandidate);
    procedure ExactCalculate(Tableau : PArrayOfDouble; Taille : Cardinal);
    procedure PeakCalculate(Tableau : PArrayOfDouble; Taille : Cardinal);
  public
    property Items[Index : Integer] : TMinMaxCandidate read GetItems write SetItems; default;

    procedure Calculate(Tableau : PArrayOfDouble; Taille : Cardinal);
    procedure ApplyFilter(MinMaxFilter : TMinMaxFilter);
    property MinMaxMode : TMinMaxMode read FMinMaxMode write FMinMaxMode;
    property MinValue : Double read FMinValue write FMinValue;
  end;

  TFieldStats = class(TCollectionItem)
  private
    FArray : PArrayOfDouble;
    FCollection : TFieldStatsCollection;
    FCount : Cardinal;
    FFieldName: string;
    FField : TField;
    FCalcValues: TCalculatedSet;
    FStatsArray : TStatsArray;
    FMinMaxList: TMinMaxList;
    FMinMaxFilter: TMinMaxFilter;
    FMinMaxFiltered: Boolean;
    function GetStats(Index: TCalculKind): Double;
    function GetValues(Index: Cardinal): Double;
    procedure SetFieldName(const Value: string);
    procedure SetValues(Index: Cardinal; const Value: Double);
    procedure SetCalcValues(const Value: TCalculatedSet);
    function GetCollection: TFieldStatsCollection;
    function GetDataset: TDataset;
    function SortValues(P1, P2: Pointer): Integer;
    procedure SetMinMaxFilter(const Value: TMinMaxFilter);
  protected
    procedure CreateArray;
    procedure FreeArray;
    function GetDisplayName: string; override;
    procedure GetFieldValue(const Index : Cardinal);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure InitStats;
    procedure Calculate;


    property Collection : TFieldStatsCollection read GetCollection;
    property Stats[Index : TCalculKind] : Double read GetStats;
    property Values[Index : Cardinal] : Double read GetValues write SetValues;
    property Dataset : TDataset read GetDataset;

    property MinMaxList : TMinMaxList read FMinMaxList;
    procedure ApplyMinMaxFilter;

  published
    property FieldName : string read FFieldName write SetFieldName;
    property CalculatedValues : TCalculatedSet read FCalcValues write SetCalcValues;
    property MinMaxFilter : TMinMaxFilter read FMinMaxFilter write SetMinMaxFilter;
    property MinMaxFiltered : Boolean read FMinMaxFiltered write FMinMaxFiltered;
  end;

  TFieldStatsClass = class of TFieldStats;

  TFieldStatsCollection = class(TCollection)
  private
    FDatasetStats: TDatasetStats;
    function GetField(Index: Integer): TFieldStats;
    procedure SetField(Index: Integer; Value: TFieldStats);
    function GetFieldByName(Name: string): TFieldStats;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner : TDatasetStats);
    function Add: TFieldStats;

    procedure Calculate;

    property DatasetStats: TDatasetStats read FDatasetStats;
    property Items[Index: Integer]: TFieldStats read GetField write SetField; default;
    property Fields[Name: string]: TFieldStats read GetFieldByName;
  end;

  TDatasetStats = class(TComponent)
  private
    FDataset: TDataset;
    FFields: TFieldStatsCollection;
    FDisableControls: Boolean;
    procedure SetDataset(const Value: TDataset);
    procedure SetFields(const Value: TFieldStatsCollection);
    function GetFieldByName(Name: string): TFieldStats;
  protected
    procedure LoadDataset;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Calculate;

    property FieldStats[Name : string]: TFieldStats read GetFieldByName; default;
  published
    property Fields: TFieldStatsCollection read FFields write SetFields;
    property Dataset : TDataset read FDataset write SetDataset;
    property DisableControls : Boolean read FDisableControls write FDisableControls;
  end;

implementation

uses AClasses, Math;

{ TDatasetStats }

procedure TDatasetStats.Calculate;
begin
  {1) Chargement des données en une passe}
  LoadDataset;
  {2) Calculs pour tous les champs}
  Fields.Calculate;
end;

procedure TDatasetStats.LoadDataset;
var
  i : Integer;
  k : Integer;
begin
  if not Assigned(Dataset) then
    Exit;
  if not Dataset.Active then
    raise EDBStatsError.Create('Dataset inactif');

  for i := 0 to Fields.Count - 1 do
    Fields[i].InitStats;

  if DisableControls then
    Dataset.DisableControls;
  try
    Dataset.First;
    k := 0;
    while not Dataset.EOF do
    begin
      for i := 0 to Fields.Count - 1 do
        Fields[i].GetFieldValue(k);
      Inc(k);
      Dataset.Next;
    end;
    Dataset.First;
  finally
    if DisableControls then
      Dataset.EnableControls;
  end;
end;

constructor TDatasetStats.Create(AOwner: TComponent);
begin
  inherited;
  FFields := TFieldStatsCollection.Create(Self);
end;

destructor TDatasetStats.Destroy;
begin
  FFields.Free;
  FFields:= nil;
  inherited;
end;

procedure TDatasetStats.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(Dataset) and (AComponent = Dataset) then
  begin
    Dataset := nil;
  end;
end;

procedure TDatasetStats.SetDataset(const Value: TDataset);
begin
  FDataset := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TDatasetStats.SetFields(const Value: TFieldStatsCollection);
begin
  FFields.Assign(Value);
end;

function TDatasetStats.GetFieldByName(Name: string): TFieldStats;
begin
  Result := Fields.Fields[Name];
  if not Assigned(Result) then
    raise EDBStatsError.Create('Le champ ' + Name + ' ne fait pas partie des champs calculés !');
end;

{ TFieldStatsCollection }

function TFieldStatsCollection.Add: TFieldStats;
begin
  Result:= TFieldStats(inherited Add);
end;

procedure TFieldStatsCollection.Calculate;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Calculate;
end;

constructor TFieldStatsCollection.Create(AOwner: TDatasetStats);
begin
  inherited Create(TFieldStats);
  FDatasetStats := AOwner;
end;

function TFieldStatsCollection.GetField(Index: Integer): TFieldStats;
begin
  Result:= TFieldStats(inherited Items[Index]);
end;

function TFieldStatsCollection.GetFieldByName(Name: string): TFieldStats;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].FieldName = Name then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

function TFieldStatsCollection.GetOwner: TPersistent;
begin
  Result := FDatasetStats;
end;

procedure TFieldStatsCollection.SetField(Index: Integer; Value: TFieldStats);
begin
  Items[Index].Assign(Value);
end;

procedure TFieldStatsCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TFieldStats }

function TFieldStats.SortValues(P1, P2 : Pointer) : Integer;
begin
  Result := CompareValue(Double(P1^), Double(P2^), 1E-5);
end;

procedure TFieldStats.Calculate;
var
  i : Cardinal;
  Eff1, Eff2, Eff3 : Integer;
  AMC1, AMC3, AV1, AV2 : Double;
  AMode, AMode3, AMedian, AValue, AMax, AMin, ASum, AMean, ASCE, AVar, AStdDev : Double;
  FSortedArray : PArrayOfDouble;
begin
  FillChar(FStatsArray, SizeOf(FStatsArray), 0);
  if FCount = 0 then
    Exit;

  FStatsArray[ckCount] := FCount;

  //1) Calcul de la somme
  ASum := 0;
  AMax := FArray^[0];
  AMin := AMax;
  for i := 0 to FCount - 1 do
  begin
    AValue := FArray^[i];
    if AValue > AMax then
      AMax := AValue;
    if AValue < AMin then
      AMin := AValue;
    ASum := ASum + AValue;
  end;

  FStatsArray[ckMax] := AMax;
  FStatsArray[ckMin] := AMin;
  FStatsArray[ckSum] := ASum;

  AMean := ASum / FCount;

  FStatsArray[ckMean] := AMean;

  if (ckSCE in CalculatedValues) or (ckVar in CalculatedValues) or (ckStdDev in CalculatedValues) then
  begin
    ASCE := 0;
    for i := 0 to FCount - 1 do
      ASCE := ASCE + Sqr(FArray^[i] - AMean);

    FStatsArray[ckSCE] := ASCE;

    if (ckVar in CalculatedValues) or (ckStdDev in CalculatedValues) then
    begin
      AVar := ASCE / FCount;

      FStatsArray[ckVar] := AVar;

      if (ckStdDev in CalculatedValues) then
      begin
        AStdDev := Sqrt(AVar);

        FStatsArray[ckStdDev] := AStdDev;

      end;
    end;
  end;

  if (ckMode3 in CalculatedValues) or (ckMode in CalculatedValues) or (ckMedian in CalculatedValues) then
  begin
    {On tri les valeurs et on compte...}
    {Tri par QSort...}
    GetMem(FSortedArray, SizeOf(Double) * FCount);
    try
      Move(FArray^, FSortedArray^, SizeOf(Double) * FCount);
      GenericQuickSort(FSortedArray, SizeOf(Double), SortValues, 0, FCount - 1);


      if (ckMedian in CalculatedValues) then
      begin
        if Odd(FCount) then
          AMedian := FSortedArray^[FCount div 2 + 1 - 1]
        else
          AMedian := (FSortedArray^[FCount div 2 - 1] + FSortedArray^[FCount div 2 + 1 - 1]) / 2;
        FStatsArray[ckMedian] := AMedian;
      end;


      if (ckMode in CalculatedValues) or (ckMode3 in CalculatedValues) then
      begin
        Eff1 := 1;
        Eff2 := 1;
        Eff3 := 1;
        AMode := FSortedArray^[0];
        AMode3 := FSortedArray^[0];
        AMC1 := AMode;
        AMC3 := AMode;
        AV1 := FSortedArray^[0];
        AV2 := FSortedArray^[0];
        if FCount > 1 then
          for i := 1 to FCount - 1 do
          begin
            if FSortedArray^[i] <> FSortedArray^[i - 1] then
            begin
              if Eff1 + Eff2 + Eff3 > AMC3 then
              begin
                AMC3 := Eff1 + Eff2 + Eff3;
                AMode3 := AV2;
              end;
              if Eff1 > AMC1 then
              begin
                AMC1 := Eff1;
                AMode := AV1;
              end;

              Eff3 := Eff2;
              Eff2 := Eff1;
              AV2 := AV1;
              Eff1 := 1;
              AV1 := FSortedArray^[i];
            end
            else
              Inc(Eff1);
          end;
        FStatsArray[ckMode] := AMode;
        FStatsArray[ckMode3] := AMode3;
      end;
    finally
      FreeMem(FSortedArray, SizeOf(Double) * FCount);
    end;
  end;

  if ckMinMaxList in CalculatedValues then
  begin
    FMinMaxList.Calculate(FArray, FCount);
    if MinMaxFiltered then
      ApplyMinMaxFilter;
  end;
end;

constructor TFieldStats.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCollection := TFieldStatsCollection(Collection);

  FCalcValues := AlwaysCalculatedSet;

  FMinMaxFilter := TMinMaxFilter.Create;

  FMinMaxList := TMinMaxList.Create(True);
end;

procedure TFieldStats.CreateArray;
begin
  FreeArray;

  FCount := Collection.FDatasetStats.FDataset.RecordCount;

  if FCount > 0 then
  begin
    GetMem(FArray, SizeOf(Double) * FCount);
    FillChar(FArray^, SizeOf(Double) * FCount, 0);
  end;
end;

destructor TFieldStats.Destroy;
begin
  FreeArray;
  FMinMaxList.Free;
  FMinMaxFilter.Free;
  inherited;
end;

procedure TFieldStats.FreeArray;
begin
  if Assigned(FArray) then
  begin
    FreeMem(FArray, SizeOf(Double) * FCount);
    FArray := nil;
  end;
end;

function TFieldStats.GetCollection: TFieldStatsCollection;
begin
  Result := FCollection;
end;

function TFieldStats.GetDataset: TDataset;
begin
  Result := FCollection.FDatasetStats.FDataset;
end;

function TFieldStats.GetDisplayName: string;
begin
  Result := FFieldName;
end;

procedure TFieldStats.GetFieldValue(const Index: Cardinal);
begin
  if not Assigned(FField) then
    FField := Dataset.FieldByName(FieldName);
  if not Assigned(FField) then
    raise EDBStatsError.Create('Champ "' + FieldName + '" introuvable dans le Dataset');
  Values[Index] := FField.AsFloat;
end;

function TFieldStats.GetStats(Index: TCalculKind): Double;
begin
  if Index in CalculatedValues then
    Result := FStatsArray[Index]
  else
    raise EDBStatsError.Create('Statistique non-calculée');
end;

function TFieldStats.GetValues(Index: Cardinal): Double;
begin
  if Index < FCount then
    Result := FArray^[Index]
  else
    raise EDBStatsError.Create('Accès à un indice de tableau incorrect');
end;

procedure TFieldStats.InitStats;
begin
  CreateArray;
  FillChar(FStatsArray, SizeOf(FStatsArray), 0);
  FMinMaxList.Clear;
end;

procedure TFieldStats.SetCalcValues(const Value: TCalculatedSet);
begin
  FCalcValues := Value;
  FCalcValues := FCalcValues + AlwaysCalculatedSet;
end;

procedure TFieldStats.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TFieldStats.SetValues(Index: Cardinal; const Value: Double);
begin
  if Index < FCount then
    FArray^[Index] := Value
  else
    raise EDBStatsError.Create('Accès à un indice de tableau incorrect');
end;

procedure TFieldStats.SetMinMaxFilter(const Value: TMinMaxFilter);
begin
  FMinMaxFilter.Assign(Value);
end;

procedure TFieldStats.ApplyMinMaxFilter;
begin
  MinMaxList.ApplyFilter(MinMaxFilter);
end;

{ TMinMaxCandidate }

function TMinMaxCandidate.AddValue(AValue: Double; AIndex : Integer): TNextMinMaxAction;
begin
  Result := nmContinue;
  if IsFloor then
  begin
    if AValue = FValue then
    begin
      Inc(FValuesBefore);
      FValueIndex := AIndex;
    end
    else
    begin
      Result := nmNextCandidate;
    end;
  end
  else
  if IsMax then
  begin
    if AValue > FValue then
    begin
      FValue := AValue;
      FValueIndex := AIndex;
      FLastValue := AValue;
      Inc(FValuesBefore);
    end
    else
    begin
      Result := nmNextCandidate;
    end;
  end
  else //IsMin
  begin
    if AValue < FValue then
    begin
      FValue := AValue;
      FValueIndex := AIndex;
      FLastValue := AValue;
      Inc(FValuesBefore);
    end
    else
    begin
      Result := nmNextCandidate;
    end;
  end;
end;

constructor TMinMaxCandidate.Create(AFirstValue, ASecondValue: Double);
begin
  FValue := AFirstValue;
  FFirstValue := AFirstValue;
  FValuesBefore := 0;
  FLastValue := AFirstValue;
  FValuesAfter := 0;
  if AFirstValue > ASecondValue then
    FKind := mkMax
  else
  if AFirstValue < ASecondValue then
    FKind := mkMin
  else
    FKind := mkFloor;
end;

constructor TMinMaxCandidate.Create(AValue: Double; ACandidate: TMinMaxCandidate; AIndex : Integer; InsertAfter : Boolean = True);
begin
  if InsertAfter then //Valeur par défaut, insertion après
  begin
    FValue := AValue;
    FValueIndex := AIndex;
    FFirstValue := ACandidate.Value;
    FLastValue := AValue;
    FValuesBefore := 1;
    FValuesAfter := 0;
  end
  else
  begin
    FValue := ACandidate.FirstValue;
    FValueIndex := AIndex;
    FFirstValue := ACandidate.FirstValue;
    FLastValue := ACandidate.Value;
    FValuesAfter := ACandidate.ValuesBefore;
    FValuesBefore := 0;
  end;
  if AValue > ACandidate.LastValue then
    FKind := mkMax
  else
  if AValue < ACandidate.LastValue then
    FKind := mkMin
  else
    FKind := mkFloor;
end;

constructor TMinMaxCandidate.Create(AFirstValue: Double; AKind : TMinMaxKind);
begin
  FValue := AFirstValue;
  FFirstValue := AFirstValue;
  FValuesBefore := 0;
  FLastValue := AFirstValue;
  FValuesAfter := 0;
  FKind := AKind;
end;

function TMinMaxCandidate.GetFirstIndex: Integer;
begin
  Result := FValueIndex - FValuesBefore;
end;

function TMinMaxCandidate.GetIsFloor: Boolean;
begin
  Result := (FKind = mkFloor);
end;

function TMinMaxCandidate.GetIsMax: Boolean;
begin
  Result := (FKind = mkMax);
end;

function TMinMaxCandidate.GetIsMin: Boolean;
begin
  Result := (FKind = mkMin);
end;

function TMinMaxCandidate.GetLastIndex: Integer;
begin
  Result := FValueIndex + FValuesAfter;
end;

{ TMinMaxList }

procedure TMinMaxList.Calculate(Tableau: PArrayOfDouble; Taille : Cardinal);
begin
  case FMinMaxMode of
  mmExact : ExactCalculate(Tableau, Taille);
  mmPeak : PeakCalculate(Tableau, Taille);
  end;
end;

procedure TMinMaxList.ApplyFilter(MinMaxFilter : TMinMaxFilter);
var
  i : Integer;
begin
  //Et dernier parcours pour filtrer
  if (MinMaxFilter.Options <> []) or (MinMaxFilter.Kinds <> []) then
  begin
    i := 0;
    while i < Count do
    begin
      with MinMaxFilter do
      if ((moMinAmplitude in Options) and (Abs(Items[i].Value - Items[i].FirstValue) < MinAmplitude)) or
        ((moMinLength in Options) and (Items[i].ValuesBefore < MinLength)) or
        ((moMaxLength in Options) and (Items[i].ValuesBefore > MaxLength)) or
        ((Kinds <> []) and (not (Items[i].Kind in Kinds))) then
      begin
        Delete(i);
      end
      else
        Inc(i);
    end;
  end;
end;

function TMinMaxList.GetItems(Index: Integer): TMinMaxCandidate;
begin
  Result := TMinMaxCandidate(inherited Items[Index]);
end;

procedure TMinMaxList.SetItems(Index: Integer; const Value: TMinMaxCandidate);
begin
  inherited Items[Index] := Value;
end;

procedure TMinMaxList.ExactCalculate(Tableau: PArrayOfDouble; Taille: Cardinal);
var
  FCandidate : TMinMaxCandidate;
  i : Cardinal;
begin
  if Taille = 1 then
  begin
    FCandidate := TMinMaxCandidate.Create(Tableau^[0], Tableau^[0]);
    Add(FCandidate);
  end
  else
  if Taille > 1 then
  begin
    i := 1;
    FCandidate := TMinMaxCandidate.Create(Tableau^[0], Tableau^[i]);
    Add(FCandidate);
    while i < Taille do
    begin
      if FCandidate.AddValue(Tableau^[i], i) = nmNextCandidate then
      begin
        FCandidate := TMinMaxCandidate.Create(Tableau^[i], FCandidate, i);
        Add(FCandidate);
        Inc(i);
      end
      else
        Inc(i);
    end;
    //Traitement du dernier, c'est un max ou un min en théorie...
    if FCandidate.ValuesAfter > 0 then
    begin
      FCandidate := TMinMaxCandidate.Create(Tableau^[i], FCandidate, i);
      Add(FCandidate);
    end;
  end;
  //Puis on reparcourt pour remplir ValuesAfter
  if Count > 1 then
    for i := 0 to Count - 2 do
    begin
      if Items[i].ValuesAfter = 0 then
      begin
        Items[i].FValuesAfter := Items[i + 1].ValuesBefore;
        Items[i].FLastValue := Items[i + 1].FirstValue;
      end;
    end;
end;

procedure TMinMaxList.PeakCalculate(Tableau: PArrayOfDouble; Taille: Cardinal);
var
  FCandidate : TMinMaxCandidate;
  i : Cardinal;
  FTempIndex : Cardinal;
  //FTempValue : Double;
begin
  i := 0;
  if Tableau^[i] <= FMinValue then
  begin
    FCandidate := TMinMaxCandidate.Create(0, mkFloor);
    FCandidate.FValueIndex := i;
    FCandidate.FValuesBefore := 0;
    FCandidate.FFirstValue := 0;
    FCandidate.FLastValue := 0;
  end
  else
  begin
    FCandidate := TMinMaxCandidate.Create(Tableau^[0], mkMax);
    FCandidate.FValueIndex := i;
    FCandidate.FValuesBefore := i;
    FCandidate.FFirstValue := 0;
  end;
  Add(FCandidate);
  while i < Taille do
  begin
    if FCandidate.Kind = mkFloor then
    begin
      while (i < Taille) and (Tableau^[i] <= FMinValue) do
        Inc(i);
      FCandidate.FValuesBefore := i - FCandidate.FValueIndex - 1;
      FCandidate.FValueIndex := i - 1;
      FCandidate.FValue := 0;
      if i < Taille then
      begin
        FCandidate := TMinMaxCandidate.Create(0, mkMax);
        Add(FCandidate);
        FCandidate.FValueIndex := i;
        FCandidate.FValuesBefore := i;
      end;
    end
    else
    begin
      while (i < Taille) and (Tableau^[i] > FMinValue) do
      begin
        if (Tableau^[i] > FCandidate.Value) then
        begin
          FCandidate.FValue := Tableau^[i];
          FCandidate.FValueIndex := i;
        end;
        Inc(i);
      end;
      if (i < Taille) then
      begin
        FCandidate.FValuesBefore := FCandidate.FValueIndex - FCandidate.FValuesBefore;
        FTempIndex := FCandidate.FValueIndex;
        //FTempValue := FCandidate.FValue;

        FCandidate := TMinMaxCandidate.Create(0, mkMin);
        Add(FCandidate);
        FCandidate.FValueIndex := i;
        FCandidate.FValuesBefore := i - FTempIndex;
        FCandidate.FFirstValue := 0;

        FCandidate := TMinMaxCandidate.Create(0, mkFloor);
        Add(FCandidate);
        FCandidate.FValuesBefore := 0;
        FCandidate.FValueIndex := i;
        FCandidate.FFirstValue := 0;
      end
    end;
  end;

  //Puis on reparcourt pour remplir ValuesAfter
  if Count > 1 then
    for i := 0 to Count - 2 do
    begin
      if Items[i].ValuesAfter = 0 then
      begin
        Items[i].FValuesAfter := Items[i + 1].ValuesBefore;
        if Items[i].Kind = mkFloor then
          Items[i].FLastValue := 0
        else
          Items[i].FLastValue := Items[i + 1].FirstValue;
      end;
    end;
end;

end.
