unit GzFileList;

interface

{$I GrizzlyDefine.INC}

uses
  SysUtils, Classes, Contnrs, GzSumInfo;

type
  TCustomFileList = class;
  TThreadedFileList = class;

  TFileListThread = class(TThread)
  private
    { Déclarations privées }
  protected
    procedure Execute; override;
  public
    List : TStringList;
    ItemThread : TCustomFileInformation;
    IndexSource : Integer;
    SourceList : TThreadedFileList;

    constructor Create(AList : TThreadedFileList);
    destructor Destroy; override;
  end;

  TCustomFileList = class(TObjectList)
  private
    FAutoLoad: Boolean;
    FSortIndex: Integer;
    FDescending: Boolean;
    FFileClass: TFileClass;
    FUpdateCount : Integer;
    FOnChange: TNotifyEvent;
    function GetItems(Index: Integer): TCustomFileInformation;
    procedure SetItems(Index: Integer; const Value: TCustomFileInformation);
    procedure SetSortIndex(const Value: Integer);
    procedure SetDescending(const Value: Boolean);
  protected
    procedure DoChange;
    procedure DoCreate; virtual;
  public
    {Méthodes de liste}
    constructor Create; overload;
    constructor Create(AFileClass : TFileClass); overload;
    constructor Create(AOwnsObjects : Boolean); overload;
    constructor Create(AFileClass : TFileClass; AOwnsObjects : Boolean); overload;
    function Add(AFileName : string; MustLoad : Boolean; ASearchRec : Pointer = nil) : TCustomFileInformation; overload;
    function Add(AFileName : string; ASearchRec : Pointer = nil) : TCustomFileInformation; overload;
    procedure Add(AFileInfo : TCustomFileInformation); overload;
    procedure Insert(Index : Integer; Item : TCustomFileInformation);
    property Items[Index : Integer] : TCustomFileInformation read GetItems write SetItems; default;
    function Extract(Item: TCustomFileInformation): TCustomFileInformation;
    function Remove(Item: TCustomFileInformation): Integer;
    procedure Delete(Index: Integer);
    function IndexOf(Item: TCustomFileInformation): Integer; overload;
    function IndexOf(AFileName : TFileName): Integer; overload;
    function First: TCustomFileInformation;
    function Last: TCustomFileInformation;
    procedure Clear; override;

    property AutoLoad : Boolean read FAutoLoad write FAutoLoad;

    function FindByName(AFileName : TFileName) : TCustomFileInformation;

    property SortIndex : Integer read FSortIndex write SetSortIndex;
    property SortDescending : Boolean read FDescending write SetDescending;
    procedure Sort;

    //C'est la classe de fichiers qui sera lue et créée à partir de la fonction Add
    property FileClass : TFileClass read FFileClass write FFileClass;

    procedure BeginUpdate;
    procedure EndUpdate;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    procedure Assign(ALst : TStrings); overload;
    procedure Assign(ALst : TCustomFileList); overload;

    procedure AssignTo(ALst : TStrings);

  end;

  TThreadedFileList = class(TCustomFileList)
  private
    FDirectory: string;
    FRecursive: Boolean;
    FBackgroundThread : TFileListThread;
    FOnFillingTerminate: TNotifyEvent;
    procedure SetDirectory(const Value: string);
    procedure SetRecursive(const Value: Boolean);
    function GetFilling: Boolean;
  protected
    procedure OnTerminate(Sender : TObject);
    procedure DoCreate; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function Extract(Item: TCustomFileInformation): TCustomFileInformation;

    procedure BackgroundFilling;
    procedure HandleThreadedItem;

    procedure Fill;

    property Directory : string read FDirectory write SetDirectory;
    property Recursive : Boolean read FRecursive write SetRecursive;

    property OnBackgroundFillingTerminate : TNotifyEvent read FOnFillingTerminate write FOnFillingTerminate;
    property Filling : Boolean read GetFilling;

  end;

implementation

uses FGFiles;

constructor TFileListThread.Create(AList: TThreadedFileList);
var
  i : Integer;
begin
  inherited Create(True);
  SourceList := AList;
  List := TStringList.Create;
  for i := 0 to SourceList.Count - 1 do
    List.Add(SourceList[i].FileName);
  ItemThread := AList.FileClass.Create('', False);
end;

destructor TFileListThread.Destroy;
begin
  List.Free;
  ItemThread.Free;
  inherited;
end;

procedure TFileListThread.Execute;
var
  i : Integer;
begin
  for i := List.Count - 1 downto 0 do
  begin
    if Terminated then
      Exit;
    try
      if i >= List.Count then
        Exit;
      ItemThread.FileName := List[i];
    except
    end;
    IndexSource := i;
    if ItemThread.Loaded then
      Synchronize(SourceList.HandleThreadedItem);
  end;
end;

procedure FillList(ALst : TCustomFileList; ADirectory : string; Recursive : Boolean; AutoLoad : Boolean);
var
  AFichier: TSearchRec;
  AReponse: Integer;
  FDirs: TStringList;
  i: Integer;
begin
  AReponse:= FindFirst(AddSlash(ADirectory) + '*.*', faAnyFile, AFichier);
  try
    while AReponse = 0 do
    begin
      if not ((AFichier.Name = '.') or (AFichier.Name = '..')) then
      begin
        if (AFichier.Attr and faDirectory) = 0 then
          ALst.Add(AddSlash(ADirectory) + AFichier.Name, AutoLoad, @AFichier);
      end;
      AReponse:= FindNext(AFichier);
    end;
  finally
    FindClose(AFichier);
  end;
  if Recursive then
  begin
    FDirs:= TStringList.Create;
    try
      if FindFiles(AddSlash(ADirectory), '*.*', faDirectory or faHidden or faArchive or faReadOnly or faSysFile, False, FDirs) then
      begin
        for i:= 0 to FDirs.Count - 1 do
        begin
          if DirectoryExists(FDirs[i]) then
            FillList(ALst, FDirs[i], Recursive, AutoLoad);
        end;
      end;
    finally
      FDirs.Free;
    end;
  end;
end;

{ TCustomFileList }

function TCustomFileList.Add(AFileName : string; MustLoad : Boolean; ASearchRec : Pointer = nil): TCustomFileInformation;
begin
  Result := FFileClass.Create(AFileName, MustLoad, ASearchRec);
  inherited Add(Result);
  DoChange;
end;

function TCustomFileList.Add(AFileName: string; ASearchRec : Pointer = nil): TCustomFileInformation;
begin
  Result := FFileClass.Create(AFileName, AutoLoad, ASearchRec);
  inherited Add(Result);
  DoChange;
end;

procedure TCustomFileList.Add(AFileInfo: TCustomFileInformation);
begin
  inherited Add(AFileInfo);
  DoChange;
end;

procedure TCustomFileList.Assign(ALst: TStrings);
var
  i : Integer;
begin
  BeginUpdate;
  try
    Clear;
    for i := 0 to ALst.Count - 1 do
      Add(ALst[i]);
  finally
    EndUpdate;
  end;
end;

procedure TCustomFileList.Assign(ALst: TCustomFileList);
var
  i : Integer;
begin
  BeginUpdate;
  try
    Clear;
    for i := 0 to ALst.Count - 1 do
      Add(ALst[i]);
  finally
    EndUpdate;
  end;
end;

procedure TCustomFileList.AssignTo(ALst: TStrings);
var
  i : Integer;
begin
  ALst.BeginUpdate;
  try
    ALst.Clear;
    for i := 0 to Count - 1 do
      ALst.Add(Items[i].FileName);
  finally
    ALst.EndUpdate;
  end;
end;

procedure TCustomFileList.Clear;
begin
  inherited;
  DoChange;
end;

constructor TCustomFileList.Create;
begin
  inherited;
  FileClass := TCustomFileInformation;
end;

constructor TCustomFileList.Create(AOwnsObjects: Boolean);
begin
  inherited;
  FileClass := TCustomFileInformation;
end;

constructor TCustomFileList.Create(AFileClass: TFileClass);
begin
  inherited;
  FileClass := AFileClass;
end;

procedure TCustomFileList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TCustomFileList.Create(AFileClass: TFileClass; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FileClass := AFileClass;
end;

procedure TCustomFileList.SetDescending(const Value: Boolean);
begin
  FDescending := Value;
end;

procedure TCustomFileList.DoChange;
begin
  if (FUpdateCount <= 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomFileList.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChange;
end;

function TCustomFileList.Extract(Item: TCustomFileInformation): TCustomFileInformation;
begin
  Result := TCustomFileInformation(inherited Extract(Item));
  DoChange;
end;

function TCustomFileList.FindByName(AFileName: TFileName): TCustomFileInformation;
var
  i : Integer;
begin
  i := IndexOf(AFileName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

function TCustomFileList.First: TCustomFileInformation;
begin
  Result := TCustomFileInformation(inherited First);
end;

function TCustomFileList.GetItems(Index: Integer): TCustomFileInformation;
begin
  Result := TCustomFileInformation(inherited Items[Index]);
end;

function TCustomFileList.IndexOf(Item: TCustomFileInformation): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TCustomFileList.IndexOf(AFileName: TFileName): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].FileName, AFileName) = 0 then
    begin
      Result := i;
      Exit;
    end
end;

procedure TCustomFileList.Insert(Index: Integer; Item: TCustomFileInformation);
begin
  inherited Insert(Index, Item);
  DoChange;
end;

function TCustomFileList.Last: TCustomFileInformation;
begin
  Result := TCustomFileInformation(inherited Last);
end;

function TCustomFileList.Remove(Item: TCustomFileInformation): Integer;
begin
  Result := inherited Remove(Item);
  DoChange;
end;

procedure TCustomFileList.SetItems(Index: Integer;
  const Value: TCustomFileInformation);
begin
  inherited Items[Index] := Value;
end;

procedure TCustomFileList.SetSortIndex(const Value: Integer);
begin
  if Value = FSortIndex then
    FDescending := not FDescending
  else
    FDescending := False;
  FSortIndex := Value;
end;

procedure TCustomFileList.Sort;
  function Compare(F1, F2 : TCustomFileInformation) : Integer;
  begin
    if FSortIndex >= 0 then
    begin
      if F1.Values[FSortIndex] > F2.Values[FSortIndex] then
      begin
        Result := 1;
        if FDescending then
          Result := -1;
      end
      else
      if F1.Values[FSortIndex] < F2.Values[FSortIndex] then
      begin
        Result := -1;
        if FDescending then
          Result := 1;
      end
      else
        Result := CompareText(F1.FileName, F2.FileName);
    end
    else
    begin
      Result := CompareText(F1.FileName, F2.FileName);
      if FDescending then
        Result := - Result;
    end;
  end;
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P{, T}: TCustomFileInformation;
  begin
    repeat
      I := L;
      J := R;
      P := Items[(L + R) shr 1];
      repeat
        while Compare(Items[I], P) < 0 do
          Inc(I);
        while Compare(Items[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          {T := Items[I];
          Items[I] := Items[J];
          Items[J] := T;}
          Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;
begin
  if (Count > 0) then
    QuickSort(0, Count - 1);
end;

procedure TCustomFileList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  DoChange;
end;

procedure TCustomFileList.DoCreate;
begin
  FSortIndex := -1;
end;


{ TThreadedFileList }

procedure TThreadedFileList.BackgroundFilling;
begin
  if Assigned(FBackgroundThread) then
    FBackGroundThread.Terminate;
  FBackgroundThread := TFileListThread.Create(Self);
  FBackgroundThread.FreeOnTerminate := True;
  FBackgroundThread.OnTerminate := OnTerminate;
  FBackgroundThread.Resume;
end;

procedure TThreadedFileList.Clear;
begin
  if Assigned(FBackgroundThread) then
    FBackGroundThread.Terminate;
  inherited;
end;

procedure TThreadedFileList.Delete(Index: Integer);
begin
  if Assigned(FBackgroundThread) then
    FBackGroundThread.Terminate;
  inherited Delete(Index);
end;

destructor TThreadedFileList.Destroy;
begin
  inherited;
end;

procedure TThreadedFileList.DoCreate;
begin
  inherited;
end;

function TThreadedFileList.Extract(Item: TCustomFileInformation): TCustomFileInformation;
begin
  if Assigned(FBackgroundThread) then
    FBackGroundThread.Terminate;
  Result := inherited Extract(Item);
end;

procedure TThreadedFileList.Fill;
begin
  BeginUpdate;
  try
    Clear;
    if not DirectoryExists(Directory) then
      Exit;
    FillList(Self, Directory, Recursive, AutoLoad);
    SortIndex := -1;
    SortDescending := False;
  finally
    EndUpdate;
  end;
end;

function TThreadedFileList.GetFilling: Boolean;
begin
  Result := Assigned(FBackgroundThread);
end;

procedure TThreadedFileList.HandleThreadedItem;
var
  Item : TCustomFileInformation;
begin
  if Assigned(FBackgroundThread) then
  begin
    if FBackgroundThread.IndexSource >= Count then
      Exit;
    Item := Items[FBackgroundThread.IndexSource];
    if not Item.Loaded then
      Item.Assign(FBackgroundThread.ItemThread);
  end;
end;

procedure TThreadedFileList.OnTerminate(Sender: TObject);
begin
  if Sender = FBackgroundThread then
    FBackgroundThread := nil;
  if Assigned(FOnFillingTerminate) then
    FOnFillingTerminate(Self);
end;

procedure TThreadedFileList.SetDirectory(const Value: string);
begin
  FDirectory := Value;
  Fill;
end;

procedure TThreadedFileList.SetRecursive(const Value: Boolean);
begin
  FRecursive := Value;
  //On n'appelle pas Fill pour éviter le remplissage intempestif...
end;

end.
