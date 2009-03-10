unit EvalDB;

interface

{$I GrizzlyDefine.INC}

uses DB, Classes, EvalIntf
     {$IFDEF GZ_D6},Variants{$ENDIF};

type
  TGetValueProc = procedure(DataField: TField; var Value: Variant) of object;

  TDBSymbol = class(TExpression)
  private
    FDataSet: TDataSet;
    FField: string;
    FGetValue: TGetValueProc;
  protected
    function GetAsVariant: Variant; override;
  public
    constructor CreateField(DataSet: TDataSet; DataField: string; GetValueProc: TGetValueProc);
    procedure Assign(Source: TPersistent); override;
  published
    property DataSet: TDataSet read FDataSet;
    property FieldName: string read FField;
    {}
    property OnGetValue: TGetValueProc read FGetValue;
  end;

  {}

  TDBEvaluator = class;

  TDBSymbolItem = class(TCollectionItem)
  private
    FDataSet: TDataSet;
    FFieldName: string;
    FSymbolName: string;
    FUserLabel: string;
    FGetValue: TGetValueProc;
    procedure SetDataSet(DataSet: TDataSet);
    procedure SetFieldName(const FieldName: string);
    procedure SetSymbolName(Name: string);
    function GetEvaluator: TDBEvaluator;
  protected
    function GetDisplayName: string; override;
    property Evaluator: TDBEvaluator read GetEvaluator;
  public
    destructor Destroy; override;
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property FieldName: string read FFieldName write SetFieldName;
    property SymbolName: string read FSymbolName write SetSymbolName;
    property UserLabel: string read FUserLabel write FUserLabel;
    {}
    property OnGetValue: TGetValueProc read FGetValue write FGetValue;
  end;

  TDBSymbolCollection = class(TCollection)
  private
    FEvaluator: TDBEvaluator;
    function GetItem(Idx: Integer): TDBSymbolItem;
  protected
    function GetOwner: TPersistent; override;
    {}
    property Evaluator: TDBEvaluator read FEvaluator;
  public
    constructor Create(Evaluator: TDBEvaluator);
    {}
    property Items[Idx: Integer]: TDBSymbolItem read GetItem;
  end;

  TDBEvaluator = class(TEvaluator)
  private
    FDBSymbols: TDBSymbolCollection;
    procedure SetDBSymbols(Symbols: TDBSymbolCollection);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure GetSymbols(NameList, LabelList: TStrings);
  published
    property DBSymbols: TDBSymbolCollection read FDBSymbols write SetDBSymbols;
  end;

  TExpressionField = class(TField)
  private
    FEvaluator: TEvaluator;
    FStrExpr: string;
    procedure SetStrExpr(const Value: string);
  protected
    property Evaluator: TEvaluator read FEvaluator;
    {}
    procedure RegisterFieldNames;
    {}
    function GetAsVariant: Variant; override;
    function GetAsString: string; override;
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsFloat: Double; override;
    function GetAsDateTime: TDateTime; override;
    {}
    function GetIsNull: Boolean; override;
    function GetCanModify: Boolean; override;
    function GetDataSize: {$IFDEF GZ_D5}Integer{$ELSE}Word{$ENDIF}; override;
    class procedure CheckTypeSize(Value: Integer); override;
    {}
{$IFNDEF GZ_D10}
    procedure SetParentComponent(AParent: TComponent); override;
{$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF GZ_D10}
    procedure SetParentComponent(AParent: TComponent); override;
{$ENDIF}
  published
    property Expression: string read FStrExpr write SetStrExpr;
  end;

  procedure LocalRegisterDataSet(Evaluator: TEvaluator; DataSet: TDataSet);
  procedure LocalUnRegisterDataSet(Evaluator: TEvaluator; DataSet: TDataSet);

implementation

uses SharedUtils, SysUtils, TypInfo, EvalClasses, EvalUser, EvalMath;

{ TExpressionField }

constructor TExpressionField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvaluator:= TEvaluator.Create(Self);
  SetDataType(ftString);
  Size:= 1;
end;

procedure TExpressionField.RegisterFieldNames;
begin
  FEvaluator.LocalSymbols.ClearSymbols;
  LocalRegisterDataSet(FEvaluator, DataSet);
    // Remove current field name ... (to avoid infinite loop)
  TEvaluator(Evaluator).UnRegisterSymbol(FieldName);
end;

procedure TExpressionField.SetParentComponent(AParent: TComponent);
begin
  if (AParent <> DataSet) and (DataSet <> nil) then
  begin
    FEvaluator.LocalSymbols.ClearSymbols;
    FEvaluator.ExtractResult.Free;
  end;
  inherited SetParentComponent(AParent);
  if (DataSet <> nil) and not (csLoading in ComponentState) then
    RegisterFieldNames;
end;

procedure TExpressionField.Loaded;
begin
  inherited Loaded;
  if DataSet <> nil then
    RegisterFieldNames;
  FStrExpr:= Trim(FStrExpr);
  if Expression <> '' then
    FEvaluator.Evaluate(Expression)
  else
    FEvaluator.ExtractResult.Free;
end;

procedure TExpressionField.SetStrExpr(const Value: string);
begin
  try
    if (Trim(Value) <> '') and not (csLoading in ComponentState) then
    begin
      if csDesigning in ComponentState then
        RegisterFieldNames;
      FEvaluator.Evaluate(Value);
    end else
      FEvaluator.ExtractResult.Free;
  finally
    FStrExpr:= Trim(Value);
    if Assigned(DataSet) and DataSet.Active then
      DataSet.Refresh;
  end;
end;

function TExpressionField.GetCanModify: Boolean;
begin
  Result:= False;
end;

class procedure TExpressionField.CheckTypeSize(Value: Integer);
begin
  if (Value > 1) or (Value < 0) then
    inherited CheckTypeSize(Value);
end;

function TExpressionField.GetDataSize: {$IFDEF GZ_D5}Integer{$ELSE}Word{$ENDIF};
begin
  if csDesigning in ComponentState then
    Result:= 1
  else
    Result:= 0;
end;

function TExpressionField.GetIsNull: Boolean;
begin
  Result:= VarIsNull(AsVariant) or VarIsEmpty(AsVariant);
end;

function TExpressionField.GetAsVariant: Variant;
begin
  if Assigned(FEvaluator.Result) then
    Result:= FEvaluator.Result.AsVariant
  else
    Result:= Null;
end;

function TExpressionField.GetAsString: string;
begin
  if Assigned(FEvaluator.Result) then
    Result:= FEvaluator.Result.AsString
  else
    Result:= '';
end;

function TExpressionField.GetAsBoolean: Boolean;
begin
  if Assigned(FEvaluator.Result) then
    Result:= FEvaluator.Result.AsBoolean
  else
    Result:= False;
end;

function TExpressionField.GetAsInteger: Integer;
begin
  if Assigned(FEvaluator.Result) then
    Result:= FEvaluator.Result.AsInteger
  else
    Result:= 0;
end;

function TExpressionField.GetAsFloat: Double;
begin
  if Assigned(FEvaluator.Result) then
    Result:= FEvaluator.Result.AsFloat
  else
    Result:= 0;
end;

function TExpressionField.GetAsDateTime: TDateTime;
begin
  Result:= AsFloat;
end;

{ TDBEvaluator }

constructor TDBEvaluator.Create(AOwner: TComponent);
begin
  FDBSymbols:= TDBSymbolCollection.Create(Self);
  inherited Create(AOwner);
end;

destructor TDBEvaluator.Destroy;
var V: TExpression;
begin
  V:= ExtractResult;
  FreeExpression(V);
  FDBSymbols.Free; FDBSymbols:= nil;
  inherited Destroy;
end;

procedure TDBEvaluator.Notification(AComponent: TComponent; AOperation: TOperation);
var i: Integer;
begin
  if (AOperation = opRemove) and (AComponent <> nil) then
  begin
    if AComponent is TDataSet then
    begin
      for i:= 0 to DBSymbols.Count - 1 do
        if DBSymbols.Items[i].DataSet = AComponent then
          DBSymbols.Items[i].DataSet:= nil;
    end;
  end;
  inherited Notification(AComponent, AOperation);
end;

procedure TDBEvaluator.GetSymbols(NameList, LabelList: TStrings);
var i: Integer;
begin
  for i:= 0 to DBSymbols.Count - 1 do
  begin
    NameList.Add(DBSymbols.Items[i].SymbolName);
    LabelList.Add(DBSymbols.Items[i].UserLabel);
  end;
end;

procedure TDBEvaluator.SetDBSymbols(Symbols: TDBSymbolCollection);
begin
  FDBSymbols.Assign(Symbols);
end;

{ TDBSymbolItem }

destructor TDBSymbolItem.Destroy;
begin
  if Trim(SymbolName) <> '' then
  begin
    Evaluator.UnRegisterSymbol(SymbolName);
    FSymbolName:= '';
  end;
  inherited Destroy;
end;

function TDBSymbolItem.GetDisplayName: string;
begin
  if Assigned(DataSet) and (FieldName <> '') then
    Result:= Format('%s.%s', [DataSet.Name, FieldName])
  else
    Result:= inherited GetDisplayName;
end;

function TDBSymbolItem.GetEvaluator: TDBEvaluator;
begin
  Result:= TDBSymbolCollection(Collection).Evaluator;
end;

procedure TDBSymbolItem.SetDataSet(DataSet: TDataSet);
begin
  if DataSet <> nil then
    DataSet.FreeNotification(Evaluator);
  FDataSet:= DataSet;
  SymbolName:= SymbolName;
end;

procedure TDBSymbolItem.SetFieldName(const FieldName: string);
begin
  FFieldName:= FieldName;
  SymbolName:= FieldName;
end;

procedure TDBSymbolItem.SetSymbolName(Name: string);
var i: Integer;
begin
  if SymbolName <> '' then
    Evaluator.UnRegisterSymbol(SymbolName);
  if Trim(Name) <> '' then
  begin
    for i:= Length(Name) downto 1 do
      if not (Name[i] in FreeSymbolChars) then
        Delete(Name, i, 1);
    if Evaluator.SymbolExists(Name) or (Name = '') then
    begin
      i:= 1;
      while Evaluator.SymbolExists(Name + IntToStr(i)) do
        Inc(i);
      Name:= Name + IntToStr(i);
    end;
    Evaluator.RegisterVariable(Name, TDBSymbol.CreateField(DataSet, FieldName, OnGetValue));
  end;
  FSymbolName:= Name;
  FUserLabel:= Name;
end;

{ TDBSymbolCollection }

constructor TDBSymbolCollection.Create(Evaluator: TDBEvaluator);
begin
  FEvaluator:= Evaluator;
  inherited Create(TDBSymbolItem);
end;

function TDBSymbolCollection.GetOwner: TPersistent;
begin
  Result:= FEvaluator;
end;

function TDBSymbolCollection.GetItem(Idx: Integer): TDBSymbolItem;
begin
  Result:= TDBSymbolItem(inherited Items[Idx]);
end;

{ TDBSymbol }

constructor TDBSymbol.CreateField(DataSet: TDataSet; DataField: string; GetValueProc: TGetValueProc);
begin
  FDataSet:= DataSet;
  FField:= DataField;
  FGetValue:= GetValueProc;
  inherited Create(etVariant);
end;

function TDBSymbol.GetAsVariant: Variant;
begin
  if Assigned(DataSet) and (DataSet.FindField(FField) <> nil) and DataSet.Active then
  begin
    Result:= FDataSet.FieldByName(FField).AsVariant;
    if Assigned(FGetValue) then
      FGetValue(DataSet.FindField(FField), Result);
  end else
    Result:= Null;
end;

procedure TDBSymbol.Assign(Source: TPersistent);
begin
  if Source is TDBSymbol then
  begin
    FDataSet:= TDBSymbol(Source).DataSet;
    FField:= TDBSymbol(Source).FieldName;
    FGetValue:= TDBSymbol(Source).OnGetValue;
  end else
    inherited Assign(Source);
end;

{}

procedure LocalRegisterDataSet(Evaluator: TEvaluator; DataSet: TDataSet);
var i: Integer;
begin
  for i:= 0 to DataSet.FieldCount - 1 do
  begin
    Evaluator.RegisterVariable(DataSet.Fields[i].FieldName,
      TDBSymbol.CreateField(DataSet, DataSet.Fields[i].FieldName, nil));
  end;
end;

procedure LocalUnRegisterDataSet(Evaluator: TEvaluator; DataSet: TDataSet);
var i: Integer;
begin
  for i:= 0 to DataSet.FieldCount - 1 do
    if DataSet.Fields[i].Visible then
      Evaluator.UnRegisterSymbol(DataSet.Fields[i].FieldName);
end;

end.
