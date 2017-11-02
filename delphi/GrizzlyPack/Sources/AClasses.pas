{***************************************************************
 *
 * Unit Name: AClasses
 * Purpose  : Useful classes
 * Author   : Alexandre GUILLIEN
 * History  :
 *
 ****************************************************************}

unit AClasses;

interface

uses SysUtils, Classes, AUtils, AFiles;

type
  TCompareProc = function(P1, P2: Pointer): Integer of object;

  TAssocItem = record
    Source, Dest: string;
    SourceObj, DestObj: TObject;
  end;
  PAssocItem = ^TAssocItem;

  TAssociation = class(TPersistent)
  private
    FSource, FDest, FChoice: TStrings;
    FBeforeEdit: TNotifyEvent;
    procedure SetSource(NewSource: TStrings);
    procedure SetDest(NewDest: TStrings);
    procedure SetChoice(NewChoice: TStrings);
    function GetAssocItem(Idx: Integer): TAssocItem;
    procedure SetAssocItem(Idx: Integer; Assoc: TAssocItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function CheckIntegrity: Boolean;
    {}
    procedure Associate(Dest, Value: string);
    {}
    function AssocCount: Integer;
    property AssocItem[Idx: Integer]: TAssocItem read GetAssocItem write SetAssocItem;
    {}
    property BeforeEdit: TNotifyEvent read FBeforeEdit write FBeforeEdit;
  published
    property Source: TStrings read FSource write SetSource;
    property Dest: TStrings read FDest write SetDest;
    property Choice: TStrings read FChoice write SetChoice;
  end;

  TCustomArray = class(TPersistent)
  private
    FPArray: Pointer;
    FItemSize: Integer;
    FCount: Integer;
    procedure SetCount(NewCount: Integer);
    function GetItem(Idx: Integer): Pointer;
    procedure CheckIndex(Idx: Integer);
  protected
    procedure InitItem(P: Pointer); virtual;
    procedure FreeItem(P: Pointer); virtual;
    {}
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveItem(Item: Pointer; Stream: TStream); virtual;
    procedure LoadItem(Item: Pointer; Stream: TStream); virtual;
    {}
    procedure AssignItem(Source, Dest: Pointer); virtual;
    {}
    property ItemSize: Integer read FItemSize write FItemSize;
  public
    constructor Create; virtual; abstract;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {}
    procedure Exchange(Idx1, Idx2: Integer);
    procedure Move(Idx1, Idx2: Integer);
    procedure Insert(Idx: Integer);
    procedure Delete(Idx: Integer);
    procedure Clear;
    {}
    function ArraySize: LongInt;
    property PArray: Pointer read FPArray;
    property Items[Idx: Integer]: Pointer read GetItem;
    {}
    property Count: Integer read FCount write SetCount;
  end;

  TCustomArrayClass = class of TCustomArray;

  TPointerArray = class(TCustomArray)
  public
    constructor Create; override;
  end;

  TConstAllocArray = class(TPointerArray)
  private
    FItemsSize: Integer;
  protected
    procedure InitItem(P: Pointer); override;
    procedure FreeItem(P: Pointer); override;
    {}
    property ItemsSize: Integer read FItemsSize write FItemsSize;
  end;

  TSortedArray = class(TCustomArray)
  private
    FCompareProc: TCompareProc;
    function GenericCompare(P1, P2: Pointer): Integer;
  protected
    function InsertSorted(const Item): Integer;
    property CompareProc: TCompareProc read FCompareProc write FCompareProc;
  public
    procedure Sort;
  end;

  TCustomGridArray = class(TPersistent)
  private
    FPArray: Pointer;
    FItemSize: Integer;
    FColCount, FRowCount: Integer;
    { Get/Set }
    procedure SetColCount(NewColCount: Integer);
    procedure SetRowCount(NewRowCount: Integer);
    {}
    function PItem(ACol, ARow: Integer): Pointer;
    function GetItem(ACol, ARow: Integer): Pointer;
    procedure CheckIndex(CR: Integer; Idx: Integer);
  protected
    function Size(Cols, Rows: LongInt): LongInt;
    procedure InitItem(P: Pointer); virtual;
    procedure FreeItem(P: Pointer); virtual;
    property ItemSize: Integer read FItemSize write FItemSize;
    {}
    procedure AssignItem(Source, Dest: Pointer); virtual;
  public
    constructor Create; virtual; abstract;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {}
    procedure AdjustSize(NewColCount, NewRowCount: Integer);
    procedure ExchangeCols(Index1, Index2: Integer);
    procedure ExchangeRows(Index1, Index2: Integer);
    procedure InsertCol(Idx: Integer);
    procedure InsertRow(Idx: Integer);
    procedure DeleteCol(Idx: Integer);
    procedure DeleteRow(Idx: Integer);
    procedure MoveCol(OldIndex, NewIndex: Integer);
    procedure MoveRow(OldIndex, NewIndex: Integer);
    {}
    procedure Clear;
    {}
    function ArraySize: LongInt;
    property PArray: Pointer read FPArray;
    property Items[ACol, ARow: Integer]: Pointer read GetItem;
    {}
    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
  end;

  TCharGridArray = class(TCustomGridArray)
  private
    function GetCell(ACol, ARow: Integer): Char;
    procedure SetCell(ACol, ARow: Integer; Value: Char);
  public
    constructor Create; override;
    property Cells[ACol, ARow: Integer]: Char read GetCell write SetCell;
  end;

  TPointerGridArray = class(TCustomGridArray)
  private
    function GetCell(ACol, ARow: Integer): Pointer;
    procedure SetCell(ACol, ARow: Integer; Value: Pointer);
  public
    constructor Create; override;
    property Cells[ACol, ARow: Integer]: Pointer read GetCell write SetCell;
  end;

  TStringGridArray = class(TPointerGridArray)
  private
     function GetCell(ACol, ARow: Integer): string;
     procedure SetCell(ACol, ARow: Integer; Value: string);
  protected
    procedure InitItem(P: Pointer); override;
    procedure FreeItem(P: Pointer); override;
  public
    property Items[ACol, ARow: Integer]: string read GetCell write SetCell;
  end;

  { Miscellaneous functions }

  function RandomRange(Low, High: Extended): Extended;

    { Generic Sort/Search procs }
  procedure GenericQuickSort(P: Pointer; SizeInfos: Integer;
     CompareProc: TCompareProc; Deb, Fin: Integer);
  function GenericDichoSearch(P: Pointer; SizeInfos: Integer; { Note : résultat négatif }
     CompareProc: TCompareProc; Deb, Fin: Integer; const Item; var Index: Integer): Boolean; { ==> non trouvé }
  function IntegerCompare(P1, P2: Pointer): Integer;

  procedure GenericAssign(Source, Destination: TPersistent);
  procedure GenericAssignEx(Source, Destination: TPersistent);

  procedure QuickSortStrings(Strings: TStrings);

  function Quote(const S: string): string;

implementation

uses TypInfo;

{ Contrôles }

function RandomRange(Low, High: Extended): Extended;
begin
  Result:= Abs(High - Low);
  if Low > High then Low:= High;
  Result:= Random * Result + Low;
end;

procedure QuickSortStrings(Strings: TStrings);
var
  S: string;
  procedure LocalQSort(iLo, iHi: Integer);
  var
     Lo, Hi: Integer;
     Med: Integer;
  begin
     Lo:= iLo;
     Hi:= iHi;
     { Élément du milieu }
     Med:= (Lo + Hi) div 2;
     repeat
        while CompareStr(Strings[Lo], Strings[Med]) < 0 do Inc(Lo);
        while CompareStr(Strings[Hi], Strings[Med]) > 0 do Dec(Hi);
        if Lo <= Hi then
        begin
          { Exchange }
          S:= Strings[Lo];
          Strings[Lo]:= Strings[Hi];
          Strings[Hi]:= S;
          if (Lo = Med) then
            Med:= Hi
          else if (Hi = Med) then
            Med:= Lo;
          Inc(Lo);
          Dec(Hi);
        end;
     until Lo > Hi;
     if iLo < Hi then LocalQSort(iLo, Hi);
     if Lo < iHi then LocalQSort(Lo, iHi);
  end;
begin
  LocalQSort(0, IMax(0, Strings.Count - 1));
end;

  { P: Pointeur sur le début du tableau, SizeInfos = taille d'un élément,
    CompareProc: fonction de comparaison, Deb, Fin: Index début & fin }
procedure GenericQuickSort(P: Pointer; SizeInfos: Integer;
  CompareProc: TCompareProc; Deb, Fin: Integer);
var
  TempItem: Pointer;
  function PItem(Index: Integer): Pointer;
  begin
     Result:= Pointer(LongInt(P) + Index * SizeInfos);
  end;
  procedure LocalQSort(iLo, iHi: Integer);
  var
     Lo, Hi: Integer;
     Med: Integer;
  begin
     Lo:= iLo;
     Hi:= iHi;
     { Élément du milieu }
     Med:= (Lo + Hi) div 2;
     repeat
        while CompareProc(PItem(Lo), PItem(Med)) < 0 do Inc(Lo);
        while CompareProc(PItem(Hi), PItem(Med)) > 0 do Dec(Hi);
        if Lo <= Hi then
        begin
          { Exchange }
          Move(PItem(Lo)^, TempItem^, SizeInfos);
          Move(PItem(Hi)^, PItem(Lo)^, SizeInfos);
          Move(TempItem^, PItem(Hi)^, SizeInfos);
          if (Lo = Med) then
            Med:= Hi
          else if (Hi = Med) then
            Med:= Lo;
          Inc(Lo);
          Dec(Hi);
        end;
     until Lo > Hi;
     if iLo < Hi then LocalQSort(iLo, Hi);
     if Lo < iHi then LocalQSort(Lo, iHi);
  end;
begin
  GetMem(TempItem, SizeInfos);
  LocalQSort(Deb, Fin);
  FreeMem(TempItem, SizeInfos);
end;

{ Note : résultat négatif ==> non trouvé }
function GenericDichoSearch(P: Pointer; SizeInfos: Integer;
  CompareProc: TCompareProc; Deb, Fin: Integer; const Item; var Index: Integer): Boolean;
var
  Cmp, Med, InitialEnd: Integer;
begin
  InitialEnd:= Fin;
  while (Deb < Fin) do
  begin
     Med:= (Deb + Fin) div 2;
     Cmp:= CompareProc(Pointer(LongInt(P) + Med * SizeInfos), @Item);
     if Cmp = 0 then
     begin
        Deb:= Med;
        Fin:= Med;
     end
     else if Cmp > 0 then
        Fin:= Med
     else Deb:= Med + 1;
  end;
  Cmp:= CompareProc(Pointer(LongInt(P) + Deb * SizeInfos), @Item);
  if Cmp = 0 then
    Index:= Deb
  else if (Cmp < 0) and (Fin = InitialEnd) then
    Index:= - Deb - 1
  else
    Index:= - Deb;
  Result:= Cmp = 0;
end;

function IntegerCompare(P1, P2: Pointer): Integer;
begin
  Result:= PInteger(P1)^ - PInteger(P2)^;
end;

{ ************************** TAssociation ************************ }

constructor TAssociation.Create;
begin
  FChoice:= TStringList.Create;
  FSource:= TStringList.Create;
  FDest:= TStringList.Create;
  inherited Create;
end;

destructor TAssociation.Destroy;
begin
  FChoice.Free;
  FSource.Free;
  FDest.Free;
  inherited Destroy;
end;

procedure TAssociation.Associate(Dest, Value: string);
var Idx: Integer;
begin
  Idx:= Self.Dest.IndexOf(Dest);
  if Idx = -1 then
    raise Exception.CreateFmt('Champs %s non trouvé', [Dest]);
  Source[Idx]:= Value;
end;

function TAssociation.CheckIntegrity: Boolean;
var i: Integer;
begin
  Result:= True; i:= 0;
  while Result and (i < Source.Count) do
  begin
     Result:= Choice.IndexOf(Source[i]) <> -1;
     Inc(i);
  end;
end;

procedure TAssociation.Assign(Source: TPersistent);
begin
  if Source is TAssociation then
     with TAssociation(Source) do
     begin
        Self.Choice:= Choice;
        Self.Dest:= Dest;
        Self.Source:= Source;
     end
  else inherited Assign(Source);
end;

function TAssociation.GetAssocItem(Idx: Integer): TAssocItem;
var i: Integer;
begin
  i:= 0;
  repeat
     if (Source[i] = '') and (Source.Objects[i] = nil) then Inc(Idx);
     Inc(i);
  until (i = Dest.Count) or (i > Idx);
  Result.Source:= Source[Idx]; Result.Dest:= Dest[Idx];
  Result.SourceObj:= Source.Objects[Idx]; Result.DestObj:= Dest.Objects[Idx];
end;

procedure TAssociation.SetAssocItem(Idx: Integer; Assoc: TAssocItem);
var i: Integer;
begin
  i:= 0;
  repeat
     if (Source[i] = '') and (Source.Objects[i] = nil) then Inc(Idx);
     Inc(i);
  until (i = Dest.Count) or (i > Idx);
  Source[Idx]:= Assoc.Source; Dest[Idx]:= Assoc.Dest;
  Source.Objects[Idx]:= Assoc.SourceObj; Dest.Objects[Idx]:= Assoc.DestObj;
end;

function TAssociation.AssocCount: Integer;
var i: Integer;
begin
  Result:= Dest.Count;
  for i:= 0 to Result - 1 do
     if (Source[i] = '') and (Source.Objects[i] = nil) then Dec(Result);
end;

procedure TAssociation.SetSource(NewSource: TStrings);
begin
  if NewSource.Count <> FDest.Count then
    raise Exception.Create('Les éléments source d''une association doivent ' +
      'être aussi nombreux que ceux de la destination')
  else FSource.Assign(NewSource);
end;

procedure TAssociation.SetDest(NewDest: TStrings);
var i, j: Integer;
    P: PAssocItem;
    L: TList;
    function IndexOf(S: string; Obj: TObject): Integer;
    var i: Integer;
    begin
      Result:= -1;
      for i:= 0 to L.Count - 1 do
        if (PAssocItem(L[i])^.Dest = S) and (PAssocItem(L[i])^.DestObj = Obj) then
        begin
          Result:= i;
          Exit;
        end;
    end;
begin
    { Il faut conserver l'association telle qu'elle était }
  L:= TList.Create;
  try
      { Préparation pour conserver les associations existantes }
    for i:= 0 to AssocCount - 1 do
    begin
      New(P);
      P^:= AssocItem[i];
      L.Add(P);
    end;
    FDest.Assign(NewDest);
      { Ajoût des élements source manquant }
    for i:= FSource.Count to FDest.Count - 1 do
      FSource.AddObject('', nil);
      { Destruction des éléments source en surplus }
    for i:= FSource.Count - 1 downto FDest.Count do
      FSource.Delete(i);
      { Conservation des associations existantes }
    for i:= 0 to FDest.Count - 1 do
    begin
      j:= IndexOf(FDest[i], FDest.Objects[i]);
      if j > -1 then
      begin
        FSource[i]:= PAssocItem(L[j])^.Source;
        FSource.Objects[i]:= PAssocItem(L[j])^.SourceObj;
      end else begin
        FSource[i]:= '';
        FSource.Objects[i]:= nil;
      end;
    end;
  finally
    try
      for i:= L.Count - 1 downto 0 do
      begin
        Dispose(PAssocItem(L[i]));
        L.Delete(i);
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure TAssociation.SetChoice(NewChoice: TStrings);
begin
  FChoice.Assign(NewChoice);
end;

procedure TAssociation.Clear;
begin
  Source.Clear;
  Dest.Clear;
  Choice.Clear;
end;

{ ***************************** TCustomArray ******************************** }
{ TCustomArray }

procedure TCustomArray.Assign(Source: TPersistent);
var i: Integer;
begin
  if Source is ClassType then
  begin
    with TCustomArray(Source) do
    begin
      Self.Count:= Count;
      for i:= 0 to Count - 1 do
        AssignItem(Items[i], Self.Items[i]);
    end;
  end else
    inherited Assign(Source);
end;

procedure TCustomArray.AssignItem(Source, Dest: Pointer);
begin
  System.Move(Source^, Dest^, ItemSize);
end;

procedure TCustomArray.SaveToStream(Stream: TStream);
var i: Integer;
    P: Pointer;
begin
  Stream.Write(FItemSize, SizeOf(Integer));
  Stream.Write(FCount, SizeOf(Integer));
  P:= PArray;
  for i:= 0 to Count - 1 do
  begin
     SaveItem(P, Stream);
     Inc(LongInt(P), FItemSize);
  end;
end;

procedure TCustomArray.LoadFromStream(Stream: TStream);
var i: Integer;
    P: Pointer;
begin
  Count:= 0;
  Stream.Read(FItemSize, SizeOf(Integer));
  Stream.Read(i, SizeOf(Integer)); Count:= i;
  P:= PArray;
  for i:= 0 to Count - 1 do
  begin
     LoadItem(P, Stream);
     Inc(LongInt(P), FItemSize);
  end;
end;

procedure TCustomArray.SaveItem(Item: Pointer; Stream: TStream);
begin
  Stream.Write(Item^, FItemSize);
end;

procedure TCustomArray.LoadItem(Item: Pointer; Stream: TStream);
begin
  Stream.Read(Item^, FItemSize);
end;

function TCustomArray.ArraySize: LongInt;
begin
  Result:= Count * ItemSize;
end;

procedure TCustomArray.FreeItem(P: Pointer);
begin
end;

procedure TCustomArray.InitItem(P: Pointer);
begin
  FillChar(P^, ItemSize, 0);
end;

procedure TCustomArray.CheckIndex(Idx: Integer);
begin
  if (Idx < 0) or (Idx >= Count) then raise
        Exception.Create('Index tableau hors bornes !');
end;

function TCustomArray.GetItem(Idx: Integer): Pointer;
begin
  CheckIndex(Idx);
  Result:= IncPointer(PArray, Idx * ItemSize);
end;

destructor TCustomArray.Destroy;
begin
  Count:= 0;
  inherited Destroy;
end;

procedure TCustomArray.SetCount(NewCount: Integer);
var i: Integer;
begin
  if NewCount > Count then
  begin
     ReallocMem(FPArray, NewCount * ItemSize);
     i:= FCount; FCount:= NewCount;
     for i:= i to NewCount - 1 do
        InitItem(GetItem(i));
  end else if NewCount < Count then
  begin
     for i:= FCount - 1 downto NewCount do
        FreeItem(GetItem(i));
     ReallocMem(FPArray, NewCount * ItemSize);
     FCount:= NewCount;
  end;
end;

procedure TCustomArray.Clear;
var i: Integer;
begin
  for i:= 0 to Count - 1 do
  begin
     FreeItem(GetItem(i));
     InitItem(GetItem(i));
  end;
end;

procedure TCustomArray.Exchange(Idx1, Idx2: Integer);
var P: Pointer;
begin
  CheckIndex(Idx1); CheckIndex(Idx2);
  GetMem(P, ItemSize);
  System.Move(GetItem(Idx1)^, P^, ItemSize);
  System.Move(GetItem(Idx2)^, GetItem(Idx1)^, ItemSize);
  System.Move(P^, GetItem(Idx2)^, ItemSize);
  FreeMem(P, ItemSize);
end;

procedure TCustomArray.Move(Idx1, Idx2: Integer);
var P: Pointer;
begin
  CheckIndex(Idx1); CheckIndex(Idx2);
  GetMem(P, ItemSize);
  System.Move(GetItem(Idx1)^, P^, ItemSize);
  if Idx2 > Idx1 then
     System.Move(GetItem(Idx1 + 1)^, GetItem(Idx1)^, ItemSize * (Idx2 - Idx1))
  else
     System.Move(GetItem(Idx1)^, GetItem(Idx1 + 1)^, ItemSize * (Idx1 - Idx2));
  System.Move(P^, GetItem(Idx2)^, ItemSize);
  FreeMem(P, ItemSize);
end;
procedure TCustomArray.Insert(Idx: Integer);
var P: Pointer;
begin
  if Idx >= Count then
    Count:= Idx + 1
  else begin
    Count:= Count + 1;
    GetMem(P, ItemSize);
    try
      System.Move(GetItem(Count - 1)^, P^, ItemSize);
      System.Move(GetItem(Idx)^, GetItem(Idx + 1)^, ItemSize * (Count - 1 - Idx));
      System.Move(P^, GetItem(Idx)^, ItemSize);
    finally
      FreeMem(P, ItemSize);
    end;
  end;
end;
procedure TCustomArray.Delete(Idx: Integer);
var P: Pointer;
begin
  CheckIndex(Idx);
  if Idx = (Count - 1) then
     Count:= Idx
  else begin
        { Transferre de l'élement à détruire à la fin }
     GetMem(P, ItemSize);
     System.Move(GetItem(Idx)^, P^, ItemSize);
     System.Move(GetItem(Idx + 1)^, GetItem(Idx)^, ItemSize * (Count - 1 - Idx));
     System.Move(P^, GetItem(Count - 1)^, ItemSize);
     FreeMem(P, ItemSize);
     Count:= Count - 1;
  end;
end;

{ ***************************** TSortedArray ******************************* }

function TSortedArray.InsertSorted(const Item): Integer;
var Proc: TCompareProc;
begin
  Result:= 0;
  if Count > 0 then
  begin
    if Assigned(CompareProc) then Proc:= CompareProc else Proc:= GenericCompare;
    GenericDichoSearch(PArray, ItemSize, Proc, 0, Count - 1, Item, Result);
    Result:= Abs(Result);
    Insert(Result);
  end else Count:= 1;
end;

procedure TSortedArray.Sort;
var Proc: TCompareProc;
begin
  if Assigned(CompareProc) then Proc:= CompareProc else Proc:= GenericCompare;
  GenericQuickSort(PArray, ItemSize, Proc, 0, Count - 1);
end;

function TSortedArray.GenericCompare(P1, P2: Pointer): Integer;
var i: Integer;
begin
  Result:= 0; i:= 0;
  while (Result = 0) and (i < ItemSize) do
  begin
     Result:= PByteArray(P1)^[i] - PByteArray(P2)^[i];
     Inc(i);
  end;
end;

{ *************************** TCustomGridArray ****************************** }

destructor TCustomGridArray.Destroy;
begin
  AdjustSize(0, 0);
  inherited;
end;

function TCustomGridArray.ArraySize: LongInt;
begin
  Result:= ColCount * RowCount * FItemSize;
end;

procedure TCustomGridArray.Assign(Source: TPersistent);
var i, j: Integer;
begin
  if Source is TCustomGridArray then
  with TCustomGridArray(Source) do
  begin
    Self.AdjustSize(ColCount, RowCount);
    for i:= 0 to ColCount - 1 do
      for j:= 0 to RowCount - 1 do
        AssignItem(Items[i, j], Self.Items[i, j]); 
  end else
    inherited Assign(Source);
end;

procedure TCustomGridArray.AssignItem(Source, Dest: Pointer);
begin
  Move(Source^, Dest^, ItemSize);
end;

  { Pointers & co }
procedure TCustomGridArray.AdjustSize(NewColCount, NewRowCount: Integer);
var i, j: Integer;
     { Réalloue la mémoire à la nouvelle taille }
  procedure Realloc(Cols, Rows: LongInt);
  var S: Integer;
  begin
     S:= Size(Cols, Rows);
     ReallocMem(FPArray, S);
  end;
     { Transferre les données de leur ANCIENNE vers leur NOUVELLE position }
  procedure TransferRow(Row: Integer);
  begin
     Move(IncPointer(PArray, Row * ColCount * FItemSize)^,    { OldPos }
          IncPointer(PArray, Row * NewColCount * FItemSize)^, { NewPos }
          ColCount * FItemSize); { Size }
  end;
begin
  if NewColCount < 0 then NewColCount:= 0;
  if NewRowCount < 0 then NewRowCount:= 0;
  if (ColCount <> NewColCount) then
  begin
     { Si on réduit ColCount,
           - Libération
           - Transfert à partir du début
           - Réallocation }
     if NewColCount < ColCount then
     begin
        for i:= 0 to RowCount - 1 do
        begin
           { Libération des items situés à la fin de chaque rows }
           for j:= NewColCount to ColCount - 1 do
              FreeItem(PItem(j, i)); { On veut l'Item AVANT transfert }
           TransferRow(i);
        end;
        Realloc(NewColCount, RowCount);
     { Si on augmente ColCount,
           - Réallocation
           - Transfert à partir de la fin }
     end else if NewColCount > ColCount then
     begin
        Realloc(NewColCount, RowCount);
        for i:= RowCount - 1 downto 0 do
        begin
           TransferRow(i);
           { Initialisation des nouveaux Items situés à la fin de chaque row }
           for j:= ColCount to NewColCount - 1 do { Item sur la NOUVELLE pos }
              InitItem(IncPointer(PArray, (i * NewColCount + j) * FItemSize));
        end;
     end;
     FColCount:= NewColCount; { La modification des colonnes est effective }
  end;
  if (RowCount <> NewRowCount) then
  begin
     if NewRowCount < RowCount then
     begin
        for i:= NewRowCount to RowCount - 1 do
           for j:= 0 to ColCount - 1 do
              FreeItem(PItem(j, i));
        Realloc(NewColCount, NewRowCount);
     end else if NewRowCount > RowCount then
     begin
        Realloc(NewColCount, NewRowCount);
        for i:= RowCount to NewRowCount - 1 do
           for j:= 0 to ColCount - 1 do
              InitItem(PItem(j, i));
     end;
     FRowCount:= NewRowCount;
  end;
end;

procedure TCustomGridArray.CheckIndex(CR: Integer; Idx: Integer);
begin
  if CR = 0 then CR:= ColCount else CR:= RowCount;
  if (Idx < 0) or (Idx >= CR) then raise
        Exception.Create('Index grille hors bornes !');
end;

procedure TCustomGridArray.ExchangeCols(Index1, Index2: Integer);
var i, S: Integer;
    P: Pointer;
begin
  CheckIndex(0, Index1); CheckIndex(0, Index2);
  S:= Size(RowCount, 1);
  GetMem(P, S);
  for i:= 0 to RowCount - 1 do
     Move(PItem(Index1, i)^, IncPointer(P, i * FItemSize)^, FItemSize);
  for i:= 0 to RowCount - 1 do
     Move(PItem(Index2, i)^, PItem(Index1, i)^, FItemSize);
  for i:= 0 to RowCount - 1 do
     Move(IncPointer(P, i * FItemSize)^, PItem(Index2, i)^, FItemSize);
  FreeMem(P, S);
end;

procedure TCustomGridArray.ExchangeRows(Index1, Index2: Integer);
var S: Integer;
    P: Pointer;
begin
  CheckIndex(1, Index1); CheckIndex(1, Index2);
  S:= Size(ColCount, 1);
  GetMem(P, S);
  Move(PItem(0, Index1)^, P^, S);
  Move(PItem(0, Index2)^, PItem(0, Index1)^, S);
  Move(P^, PItem(0, Index2)^, S);
  FreeMem(P, S);
end;

procedure TCustomGridArray.InsertCol(Idx: Integer);
var i: Integer;
begin
  CheckIndex(0, IMin(Idx, Abs(Idx - 1)));  { Si 0, alors -1, ... }
  AdjustSize(ColCount + 1, RowCount);
  for i:= ColCount - 1 downto Idx + 1 do
     ExchangeCols(i, i - 1);
end;

procedure TCustomGridArray.InsertRow(Idx: Integer);
var i: Integer;
begin
  CheckIndex(1, IMin(Idx, Abs(Idx - 1)));
  AdjustSize(ColCount, RowCount + 1);
  for i:= RowCount - 1 downto Idx + 1 do
     ExchangeRows(i, i - 1);
end;

procedure TCustomGridArray.DeleteCol(Idx: Integer);
var i: Integer;
begin
  CheckIndex(0, Idx);
  for i:= Idx + 1 to ColCount - 1 do
     ExchangeCols(i - 1, i);
  AdjustSize(ColCount - 1, RowCount); { S'occupe automatique du FreeItem }
end;

procedure TCustomGridArray.DeleteRow(Idx: Integer);
var i: Integer;
begin
  CheckIndex(1, Idx);
  for i:= Idx + 1 to RowCount - 1 do
     ExchangeRows(i - 1, i);
  AdjustSize(ColCount, RowCount - 1); { S'occupe automatique du FreeItem }
end;

procedure TCustomGridArray.MoveCol(OldIndex, NewIndex: Integer);
var i: Integer;
begin
  CheckIndex(0, OldIndex); CheckIndex(0, NewIndex);
  if OldIndex < NewIndex then
     for i:= OldIndex to NewIndex - 1 do
        ExchangeCols(i, i + 1)
  else
     for i:= OldIndex downto NewIndex + 1 do
        ExchangeCols(i, i - 1)
end;

procedure TCustomGridArray.MoveRow(OldIndex, NewIndex: Integer);
var i: Integer;
begin
  CheckIndex(1, OldIndex); CheckIndex(1, NewIndex);
  if OldIndex < NewIndex then
     for i:= OldIndex to NewIndex - 1 do
        ExchangeRows(i, i + 1)
  else
     for i:= OldIndex downto NewIndex + 1 do
        ExchangeRows(i, i - 1)
end;

procedure TCustomGridArray.Clear;
var i, j: Integer;
begin
  for i:= 0 to RowCount - 1 do
     for j:= 0 to ColCount - 1 do
     begin
        Self.FreeItem(PItem(j, i));
        Self.InitItem(PItem(j, i));
     end;
end;

function TCustomGridArray.PItem(ACol, ARow: Integer): Pointer;
begin
  Result:= IncPointer(PArray, (ARow * ColCount + ACol) * FItemSize);
end;

function TCustomGridArray.Size(Cols, Rows: LongInt): LongInt;
begin
  Result:= Cols * Rows * FItemSize;
end;

function TCustomGridArray.GetItem(ACol, ARow: Integer): Pointer;
begin
  if (ACol < 0) or (ACol >= ColCount) or (ARow < 0) or (ARow >= RowCount) then
     raise EListError.Create('Index grille (' + IntToStr(ACol) + ', ' +
        IntToStr(ARow) + ') hors borne !');
  Result:= PItem(ACol, ARow);
end;

  { Get/Set }
procedure TCustomGridArray.SetColCount(NewColCount: Integer);
begin
  AdjustSize(NewColCount, RowCount);
end;

procedure TCustomGridArray.SetRowCount(NewRowCount: Integer);
begin
  AdjustSize(ColCount, NewRowCount);
end;

  { Free/Init }
procedure TCustomGridArray.FreeItem(P: Pointer);
begin
end;

procedure TCustomGridArray.InitItem(P: Pointer);
begin
  FillChar(P^, FItemSize, 0);
end;

{ ****************************** TCharGridArray *************************** }

constructor TCharGridArray.Create;
begin
  FItemSize:= SizeOf(Char);
end;

function TCharGridArray.GetCell(ACol, ARow: Integer): Char;
begin
  Result:= PChar(Items[ACol, ARow])^;
end;

procedure TCharGridArray.SetCell(ACol, ARow: Integer; Value: Char);
begin
  PChar(Items[ACol, ARow])^:= Value;
end;

{ ****************************** TPointerGridArray *************************** }

constructor TPointerGridArray.Create;
begin
  FItemSize:= SizeOf(Pointer);
end;

function TPointerGridArray.GetCell(ACol, ARow: Integer): Pointer;
begin
  Result:= PPointer(Items[ACol, ARow])^;
end;

procedure TPointerGridArray.SetCell(ACol, ARow: Integer; Value: Pointer);
begin
  PPointer(Items[ACol, ARow])^:= Value;
end;

{ ****************************** TStringGridArray *************************** }

function TStringGridArray.GetCell(ACol, ARow: Integer): string;
begin
  Result:= PString(inherited Items[ACol, ARow]^)^;
end;

procedure TStringGridArray.SetCell(ACol, ARow: Integer; Value: string);
begin
  PString(inherited Items[ACol, ARow]^)^:= Value;
end;

procedure TStringGridArray.InitItem(P: Pointer);
begin
  New(PString(P^));
end;

procedure TStringGridArray.FreeItem(P: Pointer);
begin
  Dispose(PString(P^));
end;

{ ************************** Various Arrays ************************** }

constructor TPointerArray.Create;
begin
  ItemSize:= SizeOf(Pointer);
end;

procedure TConstAllocArray.InitItem(P: Pointer);
begin
  GetMem(PPointer(P)^, FItemsSize);
end;

procedure TConstAllocArray.FreeItem(P: Pointer);
begin
  FreeMem(PPointer(P)^, FItemsSize);
end;

{ Misc }

function Quote(const S: string): string;
begin
  Result:= '"' + S + '"';
end;

{}

procedure GenericAssign(Source, Destination: TPersistent);
const OrdType     = [tkChar, tkSet, tkInteger, tkEnumeration];
      StrType     = [tkString, tkWChar, tkLString, tkWString];
      FloatType   = [tkFloat];
      MethodType  = [tkMethod];
      VariantType = [tkVariant];
      MiscType    = [tkUnknown, tkArray, tkRecord, tkInterface, tkClass];
var PSource: PPropList;
    SourceProp, DestProp: PPropInfo;
    i: Integer;
begin
  GetMem(PSource, GetTypeData(Source.ClassInfo)^.PropCount * SizeOf(PPropInfo));
  try
    i:= GetPropList(Source.ClassInfo, tkProperties, PSource);
    for i:= 0 to i - 1 do
    begin
      SourceProp:= PSource[i];
      if Assigned(SourceProp) and (not (SourceProp^.PropType^.Kind in MiscType)) and
        Assigned(SourceProp^.GetProc) then
      begin
        DestProp:= GetPropInfo(Destination.ClassInfo, SourceProp^.Name);
        if (DestProp <> nil) and (DestProp^.PropType^.Kind = SourceProp^.PropType^.Kind) and
          Assigned(DestProp^.SetProc) then
        begin
          if DestProp^.PropType^.Kind in OrdType then
            SetOrdProp(Destination, DestProp, GetOrdProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in StrType then
            SetStrProp(Destination, DestProp, GetStrProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in FloatType then
            SetFloatProp(Destination, DestProp, GetFloatProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in MethodType then
            SetMethodProp(Destination, DestProp, GetMethodProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in VariantType then
            SetVariantProp(Destination, DestProp, GetVariantProp(Source, SourceProp));
        end;
      end;
    end;
  finally
    FreeMem(PSource);
  end;
end;

procedure GenericAssignEx(Source, Destination: TPersistent);
const OrdType     = [tkChar, tkSet, tkInteger, tkEnumeration];
      StrType     = [tkString, tkWChar, tkLString, tkWString];
      FloatType   = [tkFloat];
      MethodType  = [tkMethod];
      VariantType = [tkVariant];
      ClassType   = [tkClass];
      MiscType    = [tkUnknown, tkArray, tkRecord, tkInterface];
var PSource: PPropList;
    SourceProp, DestProp: PPropInfo;
    i: Integer;
    ClassProp: Integer;
begin
  if (Source = nil) or (Destination = nil) then Exit; 
  GetMem(PSource, GetTypeData(Source.ClassInfo)^.PropCount * SizeOf(PPropInfo));
  try
    i:= GetPropList(Source.ClassInfo, tkProperties, PSource);
    for i:= 0 to i - 1 do
    begin
      SourceProp:= PSource[i];
      if Assigned(SourceProp) and (not (SourceProp^.PropType^.Kind in MiscType)) and
        Assigned(SourceProp^.GetProc) then
      begin
        DestProp:= GetPropInfo(Destination.ClassInfo, SourceProp^.Name);
        if (DestProp <> nil) and (DestProp^.PropType^.Kind = SourceProp^.PropType^.Kind) and
          Assigned(DestProp^.SetProc) then
        begin
          if DestProp^.PropType^.Kind in OrdType then
            SetOrdProp(Destination, DestProp, GetOrdProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in StrType then
            SetStrProp(Destination, DestProp, GetStrProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in FloatType then
            SetFloatProp(Destination, DestProp, GetFloatProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in MethodType then
            SetMethodProp(Destination, DestProp, GetMethodProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in VariantType then
            SetVariantProp(Destination, DestProp, GetVariantProp(Source, SourceProp))
          else if DestProp^.PropType^.Kind in ClassType then
          begin
            ClassProp:= GetOrdProp(Source, SourceProp);
            if TObject(ClassProp) is TPersistent then
            begin
              if TObject(ClassProp) is TComponent then
                SetOrdProp(Destination, DestProp, ClassProp)
              else
                TPersistent(GetOrdProp(Destination, DestProp)).Assign(TPersistent(ClassProp));
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(PSource);
  end;
end;

initialization

end.
