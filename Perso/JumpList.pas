unit JumpList;

interface

uses SysUtils, Windows, Classes, Contnrs, ComObj, ShlObj, PropSys, ObjectArray, ActiveX;

type
  TJumpListKnowCategory = (jlkcFrequent, jlkcRecent);
  TJumpListKnowCategories = set of TJumpListKnowCategory;

const
  KNOWN_CATEGORIES_DEFAULT: TJumpListKnowCategories = [jlkcFrequent, jlkcRecent];
  PKEY_Title: TPropertyKey = (fmtid: (D1: $F29F85E0; D2: $4FF9; D3: $1068; D4: ($AB, $91, $08, $00, $2B, $27, $B3, $D9)); pid: 2);
  PKEY_AppUserModel_IsDestListSeparator: TPropertyKey = (fmtid: (D1: $9F4C2855; D2: $9F79; D3: $4B39; D4: ($A8, $D0, $E1, $D4, $2D, $E1, $D5, $F3));
    pid: 6);

type
  // on redéclare: il y a des erreurs dans la version de la VCL
  ICustomDestinationList = interface
    [SID_ICustomDestinationList]
    function SetAppID(pszAppID: LPCWSTR): HRESULT; stdcall;
    function BeginList(var pcMaxSlots: UINT; const riid: TIID; out ppv): HRESULT; stdcall;
    // dans la VCL, poa est déclaré var ce qui est une erreur
    function AppendCategory(pszCategory: LPCWSTR; const poa: IObjectArray): HRESULT; stdcall;
    function AppendKnownCategory(category: Integer): HRESULT; stdcall;
    function AddUserTasks(const poa: IObjectArray): HRESULT; stdcall;
    function CommitList: HRESULT; stdcall;
    function GetRemovedDestinations(const riid: TIID; out ppv): HRESULT; stdcall;
    function DeleteList(pszAppID: LPCWSTR): HRESULT; stdcall;
    function AbortList: HRESULT; stdcall;
  end;

  TLinkObjectType = (lotShellLink, lotShellItem, lotSeparator);

  TLinkObjectItem = class;
  TLinkObjectList = class;
  TLinkCategoryItem = class;
  TLinkCategoryList = class;
  TJumpList = class;

  TShellItem = class(TPersistent)
  private
    FFilename: WideString;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    function AsIShellItem: IShellItem;
  published
    property Filename: WideString read FFilename write FFilename;
  end;

  TShellLink = class(TShellItem)
  private
    FDisplayName: WideString;
    FArguments: WideString;
    FIconFilename: WideString;
    FIconIndex: Integer;
    FDescription: WideString;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    function AsIShellLink: IShellLinkW;
    function AsSeparator: IShellLinkW;
  published
    property Description: WideString read FDescription write FDescription;
    property Arguments: WideString read FArguments write FArguments;
    property DisplayName: WideString read FDisplayName write FDisplayName;
    property IconFilename: WideString read FIconFilename write FIconFilename;
    property IconIndex: Integer read FIconIndex write FIconIndex;
  end;

  TLinkObjectItem = class(TCollectionItem)
  private
    FObjectType: TLinkObjectType;
    FShellLink: TShellLink;
    procedure SetShellItem(const Value: TShellItem);
    procedure SetShellLink(const Value: TShellLink);
    function GetShellItem: TShellItem;
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property ObjectType: TLinkObjectType read FObjectType write FObjectType default lotShellItem;
    property ShellItem: TShellItem read GetShellItem write SetShellItem;
    property ShellLink: TShellLink read FShellLink write SetShellLink;
  end;

  TLinkObjectList = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(index: Integer): TLinkObjectItem;
    procedure SetItem(index: Integer; Value: TLinkObjectItem);
    function GetObjectArray(DeletedObjects: IObjectArray): IObjectArray;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TPersistent);
    destructor Destroy; override;

    function Add: TLinkObjectItem;
    function AddShellItem(const Filename: WideString): TLinkObjectItem;
    function AddShellLink(const DisplayName, Arguments: WideString; const Description: WideString = ''; const IconFilename: WideString = '';
      IconIndex: Integer = 0; const Filename: WideString = ''): TLinkObjectItem;
    function AddSeparator: TLinkObjectItem;
    function AddItem(Item: TLinkObjectItem; index: Integer): TLinkObjectItem;
    function Insert(index: Integer): TLinkObjectItem;

    property Items[index: Integer]: TLinkObjectItem read GetItem write SetItem; default;
  end;

  TLinkCategoryItem = class(TCollectionItem)
  private
    FTitle: WideString;
    FItems: TLinkObjectList;
    procedure SetItems(const Value: TLinkObjectList);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Title: WideString read FTitle write FTitle;
    property Items: TLinkObjectList read FItems write SetItems;
  end;

  TLinkCategoryList = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(index: Integer): TLinkCategoryItem;
    procedure SetItem(index: Integer; Value: TLinkCategoryItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Owner: TPersistent);
    destructor Destroy; override;

    function Add: TLinkCategoryItem;
    function AddItem(Item: TLinkCategoryItem; index: Integer): TLinkCategoryItem;
    function Insert(index: Integer): TLinkCategoryItem;

    property Items[index: Integer]: TLinkCategoryItem read GetItem write SetItem; default;
  end;

  TJumpList = class(TComponent)
  private
    FDisplayKnowCategories: TJumpListKnowCategories;
    FDestinationList: ICustomDestinationList;
    FIsSupported: Boolean;
    FCategories: TLinkCategoryList;
    FTasks: TLinkObjectList;
    FAppID: WideString;

    function DoStoreDisplayKnowCategories: Boolean;
    procedure SetCategories(const Value: TLinkCategoryList);
    procedure SetTasks(const Value: TLinkObjectList);
    procedure SetAppID(const Value: WideString);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetMaxJumpListEntryCount: Integer;
    function Commit: Boolean;

    property DestinationList: ICustomDestinationList read FDestinationList;
    property IsSupported: Boolean read FIsSupported;
  published
    property DisplayKnowCategories: TJumpListKnowCategories read FDisplayKnowCategories write FDisplayKnowCategories stored
      DoStoreDisplayKnowCategories;
    property Categories: TLinkCategoryList read FCategories write SetCategories;
    property Tasks: TLinkObjectList read FTasks write SetTasks;
    property AppID: WideString read FAppID write SetAppID;
  end;

implementation

type
  TObjectArray = class(TInterfacedObject, IObjectArray)
  private
    FObjectList: TInterfaceList;

    procedure LoadObjectList(ObjectList: TLinkObjectList; DeletedObjects: IObjectArray);
  protected
  public
    constructor Create(ObjectList: TLinkObjectList; DeletedObjects: IObjectArray);
    destructor Destroy; override;

    function GetCount(var pcObjects: UINT): HRESULT; stdcall;
    function GetAt(uiIndex: UINT; const riid: TIID; out ppv): HRESULT; stdcall;
  end;

constructor TObjectArray.Create(ObjectList: TLinkObjectList; DeletedObjects: IObjectArray);
begin
  inherited Create;

  FObjectList := TInterfaceList.Create;
  LoadObjectList(ObjectList, DeletedObjects);
end;

destructor TObjectArray.Destroy;
begin
  FObjectList.Free;

  inherited;
end;

function TObjectArray.GetAt(uiIndex: UINT; const riid: TIID; out ppv): HRESULT; stdcall;
var
  obj: IUnknown;
begin
  obj := FObjectList[uiIndex];
  Result := obj.QueryInterface(riid, ppv);
end;

function TObjectArray.GetCount(var pcObjects: UINT): HRESULT; stdcall;
begin
  pcObjects := FObjectList.Count;
  Result := S_OK;
end;

procedure TObjectArray.LoadObjectList(ObjectList: TLinkObjectList; DeletedObjects: IObjectArray);
var
  I: Integer;
  ObjectItem: TLinkObjectItem;
begin
  for I := 0 to ObjectList.Count - 1 do
  begin
    ObjectItem := ObjectList.Items[I];
    case ObjectItem.FObjectType of
      lotShellLink:
        FObjectList.Add(ObjectItem.ShellLink.AsIShellLink);
      lotSeparator:
        FObjectList.Add(ObjectItem.ShellLink.AsSeparator);
      lotShellItem:
        FObjectList.Add(ObjectItem.ShellItem.AsIShellItem);
    end;
  end;
end;

{ TShellItem }

function TShellItem.AsIShellItem: IShellItem;
begin
  SHCreateItemFromParsingName(PWideChar(Filename), nil, StringToGUID(SID_IShellItem), Result);
end;

procedure TShellItem.Assign(Source: TPersistent);
begin
  if Source is TShellItem then
    Self.FFilename := (Source as TShellItem).FFilename
  else
    inherited Assign(Source);
end;

constructor TShellItem.Create;
begin
  inherited Create;

  FFilename := '';
end;

{ TShellLink }

function TShellLink.AsIShellLink: IShellLink;
var
  PPS: IPropertyStore;
  P: tagPROPVARIANT;
  icoFile: string;
begin
  CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkW, Result);
  if FFilename <> '' then
    Result.SetPath(PWideChar(FFilename))
  else
    Result.SetPath(PWideChar(WideString(GetModuleName(HInstance))));
  Result.SetArguments(PWideChar(FArguments));
  if FIconFilename <> '' then
    icoFile := FIconFilename
  else
    icoFile := GetModuleName(HInstance);
  Result.SetIconLocation(PWideChar(icoFile), FIconIndex);
  Result.SetDescription(PWideChar(FDescription));
  PPS := Result as IPropertyStore;
  P.vt := VT_LPWSTR;
  P.pwszVal := PWideChar(FDisplayName);
  PPS.SetValue(PKEY_Title, P);
  PPS.Commit;
end;

function TShellLink.AsSeparator: IShellLinkW;
var
  PPS: IPropertyStore;
  P: tagPROPVARIANT;
begin
  CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkW, Result);
  PPS := Result as IPropertyStore;
  P.vt := VT_BOOL;
  P.boolVal := True;
  PPS.SetValue(PKEY_AppUserModel_IsDestListSeparator, P);
  PPS.Commit;
end;

procedure TShellLink.Assign(Source: TPersistent);
begin
  if Source is TShellLink then
  begin
    Self.FDescription := (Source as TShellLink).FDescription;
    Self.FDisplayName := (Source as TShellLink).FDisplayName;
    Self.FArguments := (Source as TShellLink).FArguments;
    Self.FIconFilename := (Source as TShellLink).FIconFilename;
    Self.FIconIndex := (Source as TShellLink).FIconIndex;
  end;
  inherited Assign(Source);
end;

constructor TShellLink.Create;
begin
  inherited Create;

  FDescription := '';
  FDisplayName := '';
  FArguments := '';
  FIconFilename := '';
  FIconIndex := 0;
end;

{ TLinkObjectItem }

procedure TLinkObjectItem.Assign(Source: TPersistent);
begin
  if Source is TLinkObjectItem then
    Self.FObjectType := (Source as TLinkObjectItem).FObjectType
  else
    inherited Assign(Source);
end;

constructor TLinkObjectItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FObjectType := lotShellItem;
  FShellLink := TShellLink.Create();
end;

destructor TLinkObjectItem.Destroy;
begin
  FShellLink.Free;

  inherited;
end;

function TLinkObjectItem.GetShellItem: TShellItem;
begin
  Result := FShellLink;
end;

procedure TLinkObjectItem.SetShellItem(const Value: TShellItem);
begin
  FShellLink.Assign(Value);
end;

procedure TLinkObjectItem.SetShellLink(const Value: TShellLink);
begin
  FShellLink.Assign(Value);
end;

{ TLinkObjectList }

function TLinkObjectList.Add: TLinkObjectItem;
begin
  Result := AddItem(nil, -1);
end;

function TLinkObjectList.AddItem(Item: TLinkObjectItem; index: Integer): TLinkObjectItem;
begin
  if Item = nil then
  begin
    Result := TLinkObjectItem.Create(Self);
  end
  else
  begin
    Result := Item;
    if Assigned(Item) then
    begin
      Result.Collection := Self;
      if index < Count then
        index := Count - 1;
      Result.index := index;
    end;
  end;
end;

function TLinkObjectList.AddSeparator: TLinkObjectItem;
begin
  Result := Add;

  Result.FObjectType := lotSeparator;
end;

function TLinkObjectList.AddShellItem(const Filename: WideString): TLinkObjectItem;
begin
  Result := Add;

  Result.FObjectType := lotShellItem;
  Result.ShellItem.FFilename := Filename;
end;

function TLinkObjectList.AddShellLink(const DisplayName, Arguments, Description, IconFilename: WideString; IconIndex: Integer;
  const Filename: WideString): TLinkObjectItem;
begin
  Result := Add;
  Result.FObjectType := lotShellLink;
  Result.ShellLink.FDisplayName := DisplayName;
  Result.ShellLink.FDescription := Description;
  Result.ShellLink.FFilename := Filename;
  Result.ShellLink.FArguments := Arguments;
  Result.ShellLink.FIconFilename := IconFilename;
  Result.ShellLink.FIconIndex := IconIndex;
end;

constructor TLinkObjectList.Create(Owner: TPersistent);
begin
  inherited Create(TLinkObjectItem);
  FOwner := Owner;
end;

destructor TLinkObjectList.Destroy;
begin

  inherited;
end;

function TLinkObjectList.GetItem(index: Integer): TLinkObjectItem;
begin
  Result := TLinkObjectItem( inherited GetItem(index));
end;

function TLinkObjectList.GetObjectArray(DeletedObjects: IObjectArray): IObjectArray;
begin
  Result := TObjectArray.Create(Self, DeletedObjects) as IObjectArray;
end;

function TLinkObjectList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TLinkObjectList.Insert(index: Integer): TLinkObjectItem;
begin
  Result := AddItem(nil, index);
end;

procedure TLinkObjectList.SetItem(index: Integer; Value: TLinkObjectItem);
begin
  inherited SetItem(index, Value);
end;

procedure TLinkObjectList.Update(Item: TCollectionItem);
begin
  // nothing to do
end;

{ TLinkCategoryItem }

procedure TLinkCategoryItem.Assign(Source: TPersistent);
begin
  if Source is TLinkCategoryItem then
    Self.FTitle := (Source as TLinkCategoryItem).FTitle
  else
    inherited Assign(Source);
end;

constructor TLinkCategoryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FTitle := '';
  FItems := TLinkObjectList.Create(Self);
end;

destructor TLinkCategoryItem.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

procedure TLinkCategoryItem.SetItems(const Value: TLinkObjectList);
begin
  FItems.Assign(Value);
end;

{ TLinkCategoryList }

function TLinkCategoryList.Add: TLinkCategoryItem;
begin
  Result := AddItem(nil, -1);
end;

function TLinkCategoryList.AddItem(Item: TLinkCategoryItem; index: Integer): TLinkCategoryItem;
begin
  if Item = nil then
  begin
    Result := TLinkCategoryItem.Create(Self);
  end
  else
  begin
    Result := Item;
    if Assigned(Item) then
    begin
      Result.Collection := Self;
      if index < Count then
        index := Count - 1;
      Result.index := index;
    end;
  end;
end;

constructor TLinkCategoryList.Create(Owner: TPersistent);
begin
  inherited Create(TLinkCategoryItem);
  FOwner := Owner;
end;

destructor TLinkCategoryList.Destroy;
begin
  inherited Destroy;
end;

function TLinkCategoryList.GetItem(index: Integer): TLinkCategoryItem;
begin
  Result := TLinkCategoryItem( inherited GetItem(index));
end;

function TLinkCategoryList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TLinkCategoryList.Insert(index: Integer): TLinkCategoryItem;
begin
  Result := AddItem(nil, index);
end;

procedure TLinkCategoryList.SetItem(index: Integer; Value: TLinkCategoryItem);
begin
  inherited SetItem(index, Value);
end;

procedure TLinkCategoryList.Update(Item: TCollectionItem);
begin
  // nothing to do
end;

{ TJumpList }

function TJumpList.Commit: Boolean;
var
  MaxSlots: Cardinal;
  IdxCat: Integer;
  DeletedObjects: IObjectArray;
  category: TLinkCategoryItem;
begin
  if IsSupported then
    try
      FDestinationList.BeginList(MaxSlots, IUnknown, DeletedObjects);

      for IdxCat := 0 to FCategories.Count - 1 do
      begin
        category := FCategories.Items[IdxCat];
        if category.Items.Count > 0 then
          FDestinationList.AppendCategory(PWideChar(category.FTitle), category.Items.GetObjectArray(DeletedObjects));
      end;

      if FTasks.Count > 0 then
        FDestinationList.AddUserTasks(FTasks.GetObjectArray(DeletedObjects));

      if jlkcFrequent in FDisplayKnowCategories then
        FDestinationList.AppendKnownCategory(KDC_FREQUENT);
      if jlkcRecent in FDisplayKnowCategories then
        FDestinationList.AppendKnownCategory(KDC_RECENT);

      FDestinationList.CommitList;
      Result := True;
    except
      Result := False;
    end
  else
  begin
    Result := False;
  end;
end;

constructor TJumpList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if CheckWin32Version(6, 1) then
  begin
    FDisplayKnowCategories := KNOWN_CATEGORIES_DEFAULT;
    FDestinationList := CreateComObject(CLSID_DestinationList) as ICustomDestinationList;
  end
  else
  begin
    FDestinationList := nil;
  end;
  FIsSupported := FDestinationList <> nil;
  AppID := GetModuleName(HInstance);

  FCategories := TLinkCategoryList.Create(Self);
  FTasks := TLinkObjectList.Create(Self);
end;

destructor TJumpList.Destroy;
begin
  FDestinationList := nil;
  FCategories.Free;
  FTasks.Free;

  inherited Destroy;
end;

function TJumpList.DoStoreDisplayKnowCategories: Boolean;
begin
  Result := FDisplayKnowCategories <> KNOWN_CATEGORIES_DEFAULT;
end;

function TJumpList.GetMaxJumpListEntryCount: Integer;
var
  Objects: IObjectArray;
  MaxSlots: Cardinal;
begin
  if not IsSupported then
  begin
    Result := -1;
  end
  else
  begin
    FDestinationList.BeginList(MaxSlots, IUnknown, Objects);
    FDestinationList.AbortList;
    Result := MaxSlots;
  end;
end;

procedure TJumpList.SetAppID(const Value: WideString);
begin
  FAppID := Value;
  if IsSupported then
    FDestinationList.SetAppID(PWideChar(Value));
end;

procedure TJumpList.SetCategories(const Value: TLinkCategoryList);
begin
  FCategories.Assign(Value);
end;

procedure TJumpList.SetTasks(const Value: TLinkObjectList);
begin
  FTasks.Assign(Value);
end;

end.
