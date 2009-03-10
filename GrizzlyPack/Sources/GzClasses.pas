unit GzClasses;

interface

uses Classes, Contnrs;

type
  TIntegerList = class;
  
  TIntegerListSortCompare = function(List: TIntegerList; Index1, Index2: Integer): Integer;

  TIntegerList = class(TList)
  private
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    function GetItems(Index: Integer): Integer;
    procedure SetItems(Index: Integer; const Value: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TIntegerListSortCompare);
    procedure SetSorted(const Value: Boolean);
  protected
    function CompareIntegers(const Int1, Int2: Integer): Integer; virtual;
  public
    function Add(AItem: Integer) : Integer;
    procedure Insert(Index: Integer; Item: Integer);
    property Items[Index : Integer] : Integer read GetItems write SetItems; default;

    function Sum : Integer;
    function Mean : Integer;

    procedure CustomSort(Compare: TIntegerListSortCompare);
    procedure Sort;
    function Find(const Value: Integer; var Index: Integer): Boolean;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
  end;

  TDictionnary = class(TStringList)
  private
    function GetItems(Key : string): string;
    procedure SetItems(Key : string; const Value: string);
    function GetKeys(Index: Integer): string;
    function GetValues(Index: Integer): string;
    procedure SetKeys(Index: Integer; const Value: string);
    procedure SetValues(Index: Integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Key, Value : string); reintroduce;
    procedure Clear; override;
    procedure Delete(Key : string); reintroduce;
    procedure Assign(Source: TPersistent); override;
    property Items[Key : string] : string read GetItems write SetItems; default;
    property Keys[Index : Integer] : string read GetKeys write SetKeys;
    property Values[Index : Integer] : string read GetValues write SetValues;
  end;

  TNotificationProc = procedure (AObject : TObject) of object;

  TNotificationList = class(TObject)
  private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(AObject : TObject);
    procedure RemoveObject(AObject : TObject);
    procedure Notify(AProc : TNotificationProc);
  end;

implementation

uses
  FGUtils, Math;

function IntegerListCompare(List: TIntegerList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareIntegers(List.Items[Index1], List.Items[Index2]);
end;

{ TIntegerList }

function TIntegerList.Add(AItem: Integer) : Integer;
begin
  if not Sorted then
    Result := Count
  else
    if Find(AItem, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: raise EListError.Create('Elément déjà présent dans la liste.');
      end;
  Insert(Result, AItem);
end;

function TIntegerList.CompareIntegers(const Int1, Int2: Integer): Integer;
begin
  Result := CompareValue(Int1, Int2);
end;

procedure TIntegerList.CustomSort(Compare: TIntegerListSortCompare);
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, Compare);
end;

function TIntegerList.Find(const Value: Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareIntegers(Items[I], Value);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TIntegerList.GetItems(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, Pointer(Item));
end;

function TIntegerList.Mean: Integer;
begin
  if Count > 0 then
    Result := Sum div Count
  else
    Result := 0;
end;

procedure TIntegerList.QuickSort(L, R: Integer; SCompare: TIntegerListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TIntegerList.SetItems(Index: Integer; const Value: Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

procedure TIntegerList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TIntegerList.Sort;
begin
  CustomSort(IntegerListCompare);
end;

function TIntegerList.Sum: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Inc(Result, Items[i]);
end;

{ TNotificationList }

procedure TNotificationList.AddObject(AObject: TObject);
begin
  if (FList.IndexOf(AObject) < 0) then
    FList.Add(AObject);
end;

constructor TNotificationList.Create;
begin
  FList := TObjectList.Create(False);
end;

destructor TNotificationList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TNotificationList.Notify(AProc: TNotificationProc);
var
  i : Integer;
begin
  for i := 0 to FList.Count - 1 do
    AProc(FList.Items[i]);
end;

procedure TNotificationList.RemoveObject(AObject: TObject);
begin
  if FList.IndexOf(AObject) >= 0 then
    FList.Remove(AObject);
end;

{ TDictionnary }

type
  TDictionnaryValue = class(TObject)
  public
    Value : string;
    constructor Create(AValue : string);
  end;

procedure TDictionnary.Add(Key, Value: string);
begin
  Items[Key] := Value;
end;

procedure TDictionnary.Assign(Source: TPersistent);
var
  i : Integer;
begin
  if Source is TDictionnary then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TStrings(Source).Count - 1 do
        Add(TDictionnary(Source).Keys[i], TDictionnary(Source).Values[i]);
    finally
      EndUpdate;
    end;
    Exit;
  end
  else
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TStrings(Source).Count - 1 do
        Add(TStrings(Source)[i], '');
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TDictionnary.Clear;
begin
  inherited;

end;

constructor TDictionnary.Create;
begin
  inherited;
  Sorted := True;
  //Duplicates := dupIgnore;
end;

procedure TDictionnary.Delete(Key: string);
var
  k : Integer;
begin
  if Find(Key, k) then
    inherited Delete(k);
end;

destructor TDictionnary.Destroy;
begin
  //Détruit tous les objets et vide la liste...
  ClearStringList(Self);
  inherited;
end;

function TDictionnary.GetItems(Key: string): string;
var
  k : Integer;
begin
  if Find(Key, k) then
    Result := TDictionnaryValue(Objects[k]).Value
  else
    Result := '';
end;

function TDictionnary.GetKeys(Index: Integer): string;
begin
  Result := inherited Strings[Index];
end;

function TDictionnary.GetValues(Index: Integer): string;
begin
  Result := TDictionnaryValue(Objects[Index]).Value;
end;

procedure TDictionnary.SetItems(Key: string; const Value: string);
var
  k : Integer;
begin
  if Find(Key, k) then
    TDictionnaryValue(Objects[k]).Value := Value
  else
    inherited AddObject(Key, TDictionnaryValue.Create(Value));
end;

procedure TDictionnary.SetKeys(Index: Integer; const Value: string);
begin
  inherited Strings[Index] := Value;
end;

procedure TDictionnary.SetValues(Index: Integer; const Value: string);
begin
  TDictionnaryValue(Objects[Index]).Value := Value;
end;

{ TDictionnaryValue }

constructor TDictionnaryValue.Create(AValue: string);
begin
  Value := AValue;
end;

end.
