unit FGDBMDTV;

{$I GrizzlyDefine.INC}

interface

uses Windows, SysUtils, Classes, Messages, CommCtrl, Controls, DB, ExtCtrls,
  ComCtrls;

type
  TCustomDBMDTreeView = class;

  TTreeViewLevels = class;

  TTreeViewLevel = class;

  TLookupMethod = (lmBookmark, lmKeyFields);

  TDBTreeNode = class(TTreeNode)
  private
    FBookmark: TBookmark;
    FIsTemporary: Boolean;
    FTreeLevel : TTreeViewLevel;
    FStringKey: string;
    FValue: Variant;
    FKeyFields: string;

    function GetDataset : TDataset;

    procedure SetBookmark(const Value: TBookmark);
    procedure FreeBookmark;

    procedure SetValue(const Value: Variant);

    function GetLookupMethod: TLookupMethod;

    procedure GetLookupValue(ADataset : TDataset = nil);
    procedure GetKeyFieldsLookupValue(ADataset : TDataset);
    procedure GetBookmarkLookupValue(ADataset : TDataset);

    procedure LookupRecord(ADataset : TDataset = nil);
    procedure LookupWithBookmark(ADataset : TDataset);
    procedure LookupWithKeyFields(ADataset : TDataset);

    function IsSynchronized(ADataset : TDataset = nil) : Boolean;
    function IsSynchronizedWithBookmark(ADataset : TDataset) : Boolean;
    function IsSynchronizedWithKeyFields(ADataset : TDataset) : Boolean;
  protected
    procedure SynchronizeDataset(SetSynchro: Boolean);
  public
    procedure FillNode;
    destructor Destroy; override;
    property Bookmark: TBookmark read FBookmark write SetBookmark;
    property KeyValue : Variant read FValue write SetValue;
    property TreeLevel : TTreeViewLevel read FTreeLevel;
    property KeyFields : string read FKeyFields;
    property StringKey : string read FStringKey write FStringKey;
    property LookupMethod : TLookupMethod read GetLookupMethod;
  end;

  TLevelDatalink = class(TDataLink)
  private
    FLevel: TTreeViewLevel;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(ALevel: TTreeViewLevel);
    destructor Destroy; override;
  end;

  TAutoRecreateItems = (arOnActiveChanged, arOnEditingChanged);
  TAutoRecreateSet = set of TAutoRecreateItems;

  TStringsFillingEvent = procedure (Sender : TObject; AParentNode : TDBTreeNode; AKeyList : TStringList) of object; 

  TTreeViewLevel = class(TCollectionItem)
  private
    FField: TField;
    FFieldName: string;
    FDatalink: TLevelDatalink;
    FLevels: TTreeViewLevels;
    FAutoExpand: Boolean;
    FStateIndex,
      FSelectedIndex,
      FImageIndex: Integer;
    FRunTimeFilling: Boolean;
    FReadOnly: Boolean;
    FSubLevels: TTreeViewLevels;
    FAutoRecreate: TAutoRecreateSet;
    FStringsFilling: Boolean;
    FOnStringsNeeded: TStringsFillingEvent;
    FLookupMethod: TLookupMethod;
    FKeyFields: string;
    function GetField: TField;
    procedure SetField(Value: TField);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFieldName(const Value: string);
    function GetDataSource: TDataSource;
    procedure SetSubLevels(const Value: TTreeViewLevels);
    procedure SetLookupMethod(const Value: TLookupMethod);
    procedure SetKeyFields(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure EditingChanged;
    procedure DataSetChanged;
    procedure ActiveChanged;
    procedure DataSetScrolled(Distance: Integer);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function SynchroniseTreeView : TDBTreeNode;
    property Field: TField read GetField write SetField;
    property Datalink : TLevelDatalink read FDatalink;
  published
    property FieldName: string read FFieldName write SetFieldName;
    property KeyFields: string read FKeyFields write SetKeyFields;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property AutoExpand: Boolean read FAutoExpand write FAutoExpand default False;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property SelectedIndex: Integer read FSelectedIndex write FSelectedIndex default -1;
    property StateIndex: Integer read FStateIndex write FStateIndex default -1;
    property RunTimeFilling: Boolean read FRunTimeFilling write FRunTimeFilling;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default True;
    property SubLevels : TTreeViewLevels read FSubLevels write SetSubLevels;
    property AutoRecreate : TAutoRecreateSet read FAutoRecreate write FAutoRecreate;
    property StringsFilling : Boolean read FStringsFilling write FStringsFilling;
    property OnStringsNeeded : TStringsFillingEvent read FOnStringsNeeded write FOnStringsNeeded;
    property LookupMethod : TLookupMethod read FLookupMethod write SetLookupMethod default lmBookmark;
  end;

  TTreeViewLevelClass = class of TTreeViewLevel;

  TTreeViewLevels = class(TCollection)
  private
    FTreeView: TCustomDBMDTreeView;
    FParentLevel: TTreeViewLevel;
    function GetLevel(Index: Integer): TTreeViewLevel;
    procedure SetLevel(Index: Integer; Value: TTreeViewLevel);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ATreeView: TCustomDBMDTreeView; ALevelClass: TTreeViewLevelClass; AParentLevel : TTreeViewLevel = nil);
    function Add: TTreeViewLevel;
    property TreeView: TCustomDBMDTreeView read FTreeView;
    property ParentLevel: TTreeViewLevel read FParentLevel;
    property Items[Index: Integer]: TTreeViewLevel read GetLevel write SetLevel; default;
  end;

  TAddDBTreeNodeEvent = procedure(Sender: TObject; Node: TDBTreeNode) of object;
  TLevelFillEvent = procedure(Sender: TObject; Level: TTreeViewLevel) of object;

  TGetStringCaptionEvent = procedure(Sender : TObject; Node : TDBTreeNode; var ACaption : string) of object;

  TCustomDBMDTreeView = class(TCustomTreeView)
  private
    FLevels: TTreeViewLevels;
    FTimer: TTimer;
    FFilling: Boolean;
    FCreating : Boolean;
    FSynchronizing: Integer;
    FOnAddNode: TAddDBTreeNodeEvent;
    FAfterCreate: TNotifyEvent;
    FBeforeFill: TLevelFillEvent;
    FAfterFill: TLevelFillEvent;
    FUseSubLevels: Boolean;
    FOnGetStringCaption: TGetStringCaptionEvent;
    FBeforeEditing: TNotifyEvent;
    FAfterEditing: TNotifyEvent;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure DoAddDBTreeNode(ANode: TDBTreeNode); virtual;
    function GetSynchronizing: Boolean;
    procedure BeginSynchronize;
    procedure EndSynchronize;
    function CreateNode: TTreeNode; override;
    procedure SetLevels(Value: TTreeViewLevels);
    procedure UpdateNode(ATreeLevel: TTreeViewLevel; ANode: TDBTreeNode); virtual;
    procedure FillNode(ATreeLevel: TTreeViewLevel; ANodeParent: TDBTreeNode; EmptyLevel : Boolean = False); virtual;
    procedure QueryRecreate(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change(Node: TTreeNode); override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure Edit(const Item: TTVItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure CreateTreeView;
    {}
    property Levels: TTreeViewLevels read FLevels write SetLevels;
    property Filling: Boolean read FFilling;
    property Synchronizing: Boolean read GetSynchronizing;
    property OnAddNode: TAddDBTreeNodeEvent read FOnAddNode write FOnAddNode;
    property OnGetStringCaptionEvent : TGetStringCaptionEvent read FOnGetStringCaption write FOnGetStringCaption;
  published
    //property Items;
    property AfterCreate: TNotifyEvent read FAfterCreate write FAfterCreate;
    property BeforeFilling: TLevelFillEvent read FBeforeFill write FBeforeFill;
    property AfterFilling: TLevelFillEvent read FAfterFill write FAfterFill;
    property UseSubLevels : Boolean read FUseSubLevels write FUseSubLevels default False;

    property BeforeEditing: TNotifyEvent read FBeforeEditing write FBeforeEditing;
    property AfterEditing: TNotifyEvent read FAfterEditing write FAfterEditing;
  end;

  TDBMDTreeView = class(TCustomDBMDTreeView)
  private
  protected
  public
    property Items;
  published
    property Align;
    property Levels;
    property HideSelection;
    property ReadOnly default True;
    property Color;
    property Font;
    property Images;
    property StateImages;
    property OnAddNode;
    property OnChange;
    property OnChanging;
    property OnCollapsed;
    property OnCollapsing;
    property OnExpanded;
    property OnExpanding;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnGetStringCaptionEvent;
    property Visible;
    property Enabled;
    property ParentFont;
  end;

implementation

uses GzConsts, Variants;

{ TDBTreeNode }

destructor TDBTreeNode.Destroy;
begin
  FreeBookmark;
  inherited;
end;

procedure TDBTreeNode.FillNode;
begin
  TCustomDBMDTreeView(TreeView).FillNode(nil,Self);
end;

procedure TDBTreeNode.FreeBookmark;
var
  ADataset : TDataset;
begin
  try
    ADataset := GetDataset;
    if Assigned(ADataset) and Assigned(FBookmark) then
        ADataSet.FreeBookmark(FBookmark);
  finally
    FBookmark := nil;
  end;
end;

procedure TDBTreeNode.GetBookmarkLookupValue(ADataset : TDataset);
begin
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;
  Bookmark := ADataset.GetBookmark;
end;

function TDBTreeNode.GetDataset: TDataset;
begin
  if Assigned(TreeLevel)
    and Assigned(TreeLevel.DataSource)
    and Assigned(TreeLevel.DataSource.DataSet) then
      Result := TreeLevel.Datasource.Dataset
  else
    Result := nil;
end;

procedure TDBTreeNode.GetKeyFieldsLookupValue(ADataset : TDataset);
  function FieldsToVariant : Variant;
  var
    AFields : TList;
    i : Integer;
  begin
    AFields:= TList.Create;
    try
      ADataset.GetFieldList(AFields, KeyFields);
      if AFields.Count = 0 then
        Result := Null
      else
      if AFields.Count = 1 then
        Result:= TField(AFields[0]).AsVariant
      else
      begin
        Result:= VarArrayCreate([0, AFields.Count - 1], varVariant);
        for i := 0 to AFields.Count - 1 do
          Result[i]:= TField(AFields[i]).AsVariant;
      end;
    finally
      AFields.Free;
    end;
  end;
begin
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;

  FValue := FieldsToVariant;
end;

function TDBTreeNode.GetLookupMethod: TLookupMethod;
begin
  if Assigned(TreeLevel) then
    Result := TreeLevel.LookupMethod
  else
    Result := lmBookmark;
end;

procedure TDBTreeNode.GetLookupValue(ADataset : TDataset);
begin
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;
  case LookupMethod of
  lmBookmark : GetBookmarkLookupValue(ADataset);
  lmKeyFields : GetKeyFieldsLookupValue(ADataset);
  end;
end;

function TDBTreeNode.IsSynchronized(ADataset : TDataset = nil) : Boolean;
begin
  Result := False;
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;
  case LookupMethod of
  lmBookmark : Result := IsSynchronizedWithBookmark(ADataset);
  lmKeyFields : Result := IsSynchronizedWithKeyFields(ADataset);
  end;
end;

function TDBTreeNode.IsSynchronizedWithBookmark(ADataset: TDataset): Boolean;
var
  ABmk : TBookmark;
begin
  Result := False;
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;
  ABmk := ADataset.GetBookmark;
  if Assigned(ABmk) then
  begin
    try
      if (ADataset.CompareBookmarks(Bookmark, ABmk) = 0) then
        Result := True;
    finally
      ADataset.FreeBookmark(ABmk);
    end;
  end;
end;

function TDBTreeNode.IsSynchronizedWithKeyFields(ADataset: TDataset): Boolean;
  function FieldsAreEqual : Boolean;
  var
    AFields : TList;
    i : Integer;
  begin
    Result := False;
    AFields:= TList.Create;
    try
      ADataset.GetFieldList(AFields, KeyFields);
      if AFields.Count = 0 then
        Exit
      else
      if (AFields.Count = 1) then
      begin
        if not VarIsArray(FValue) then
          if (TField(AFields[0]).AsVariant <> FValue) then
            Exit;
      end
      else
      if VarIsArray(FValue) then
      begin
        if (VarArrayHighBound(FValue, 1) <> AFields.Count - 1) then
          Exit;
        for i := 0 to AFields.Count - 1 do
          if FValue[i] <> TField(AFields[i]).AsVariant then
            Exit;
      end;
      Result := True;
    finally
      AFields.Free;
    end;
  end;
begin
  Result := False;
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;

  Result := FieldsAreEqual;

end;

procedure TDBTreeNode.LookupRecord(ADataset : TDataset);
begin
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;
  case LookupMethod of
  lmBookmark : LookupWithBookmark(ADataset);
  lmKeyFields : LookupWithKeyFields(ADataset);
  end;
end;

procedure TDBTreeNode.LookupWithBookmark(ADataset : TDataset);
var
  ABmk : TBookmark;
begin
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;
  try
    if Assigned(Bookmark) and ADataset.BookmarkValid(Bookmark) then
    begin
      ABmk := ADataset.GetBookmark;
      try
        if (ADataset.CompareBookmarks(ABmk, Bookmark) <> 0) then
          ADataset.GotoBookmark(Bookmark);
      finally
        ADataset.FreeBookmark(ABmk);
      end;
    end
    else
      Text := '<Bookmark invalide>';
  except
  end;
end;

procedure TDBTreeNode.LookupWithKeyFields(ADataset : TDataset);
begin
  if not Assigned(ADataset) then
  begin
    ADataset := GetDataset;
    if not Assigned(ADataset) then
      Exit;
  end;

  if not ADataset.Locate(KeyFields, KeyValue, []) then
    Text := '<Clé introuvable>';
end;

procedure TDBTreeNode.SetBookmark(const Value: TBookmark);
begin
  FreeBookmark;
  FBookmark := Value;
end;

procedure TDBTreeNode.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

procedure TDBTreeNode.SynchronizeDataset(SetSynchro: Boolean);
var
  AParent: TDBTreeNode;
  ADataset: TDataset;
begin
  AParent:= TDBTreeNode(Parent);
  if Assigned(AParent) then
  begin
    if SetSynchro then
      TCustomDBMDTreeView(TreeView).BeginSynchronize;
    try
      AParent.SynchronizeDataset(False);
    finally
      if SetSynchro then
        TCustomDBMDTreeView(TreeView).EndSynchronize;
    end;
  end;
  ADataset := GetDataset;
  if Assigned(ADataset) then
    LookupRecord(ADataset);
end;


{ TLevelDatalink }

constructor TLevelDatalink.Create(ALevel: TTreeViewLevel);
begin
  inherited Create;
  FLevel:= ALevel;
end;

destructor TLevelDatalink.Destroy;
begin
  FLevel:= nil;
  inherited Destroy;
end;

procedure TLevelDatalink.EditingChanged;
begin
  if FLevel <> nil then FLevel.EditingChanged;
end;

procedure TLevelDatalink.DataSetChanged;
begin
  if FLevel <> nil then FLevel.DataSetChanged;
end;

procedure TLevelDatalink.ActiveChanged;
begin
  if FLevel <> nil then FLevel.ActiveChanged;
end;

procedure TLevelDatalink.DataSetScrolled(Distance: Integer);
begin
  if FLevel <> nil then FLevel.DatasetScrolled(Distance);
end;

{ TTreeViewLevel }

constructor TTreeViewLevel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStateIndex := -1;
  FSelectedIndex := -1;
  FImageIndex := -1;
  FReadOnly := True;
  FAutoExpand := False;
  FLevels := TTreeViewLevels(Collection);
  FDatalink := TLevelDatalink.Create(Self);
  FSubLevels:= TTreeViewLevels.Create(TTreeViewLevels(Collection).TreeView, TTreeViewLevel, Self);
  FLookupMethod := lmBookmark;
end;

destructor TTreeViewLevel.Destroy;
begin
  FDataLink.Free;
  FDataLink:= nil;
  FSubLevels.Free;
  FSubLevels:= nil;
  inherited Destroy;
end;

function TTreeViewLevel.SynchroniseTreeView : TDBTreeNode;
var
  ATV : TCustomDBMDTreeView;
  AN, AN2 : TDBTreeNode;
  ATVL : TTreeViewLevel;
  function TrouverNode(ANodeParent : TDBTreeNode) : TDBTreeNode;
  var
    ANode : TDBTreeNode;
    OkUSL : Boolean;
  begin
    Result := nil;
    if Assigned(ANodeParent) then
      ANode := TDBTreeNode(ANodeParent.getFirstChild)
    else
      ANode := TDBTreeNode(ATV.Items.GetFirstNode);
    while Assigned(ANode) do
    begin
      if Assigned(ANode.TreeLevel) and Assigned(ANode.TreeLevel.DataSource)
        and Assigned(ANode.TreeLevel.DataSource.Dataset) then
      begin
        OkUSL := (not ATV.UseSubLevels);
        if not OkUSL then
          OkUSL := ((not Assigned(ANodeParent)) and (ANode.TreeLevel = ATVL)) or Assigned(ANodeParent);
        if OkUSL then
        begin
          if ANode.IsSynchronized then
          begin
            Result := ANode;
            Exit;
          end;
        end;
      end;
      ANode := TDBTreeNode(ANode.getNextSibling);
    end;
  end;
  function TrouverTVLRacine(ATVLEnfant : TTreeViewLevel) : TTreeViewLevel;
  begin
    if Assigned(ATVLEnfant.FLevels.ParentLevel) then
      Result := TrouverTVLRacine(ATVLEnfant.FLevels.ParentLevel)
    else
      Result := ATVLEnfant;
  end;
begin
  Result := nil;
  ATV := FLevels.TreeView;
  ATV.BeginSynchronize;
  try
    {On synchronise par rapport au DataSet de ce TreeLevel}
    if ATV.UseSubLevels then
    begin
      {1) Identification du TVLevel racine de ce TVLevel.
      2) Récupération du Node correspondant à ce TVLevel.
      3) Parcours habituel en remontant jusqu'à la destination.}
      ATVL := TrouverTVLRacine(Self);
      if not Assigned(ATVL) then
        Exit;
    end
    else
    begin
      if FLevels.Count = 0 then
        Exit;
    end;

    AN := nil;
    AN2 := TrouverNode(nil);
    while Assigned(AN2) do
    begin
      AN := AN2;
      if AN.TreeLevel = Self then
        Break;
      AN2 := TrouverNode(AN2);
    end;
    if Assigned(AN) then
    begin
      if AN.TreeLevel = Self then
        Result := AN;
      AN.MakeVisible;
      {$IFDEF GZ_D6}ATV.Select(AN);{$ENDIF}
    end;
  finally
    ATV.EndSynchronize;
  end;
end;

procedure TTreeViewLevel.EditingChanged;
var
  ANode: TDBTreeNode;
  ADataset: TDataset;
  Ok: Boolean;
begin
  if not (arOnEditingChanged in AutoRecreate) then
    Exit;
  {En théorie, c'est qu'on sort d'édition dans le Dataset}
  {
  1) Recherche du noeud correspondant...
  1.1) Si on le trouve
    On modifie son libellé... et on le remplit à nouveau...
  1.2) Si on ne le trouve pas
    On recherche son parent... et on le remplit à nouveau...
  2) On le sélectionne pour finir...
  }

  {
    1) On vérifie que le noeud actif soit sur le bon dataset et le bon bookmark
    1.1) Si oui, on fait la modification
    1.2) Si non, on synchronise le TV, puis on fait la modification
  }

  {On ne synchronise que si on sort de modification (Editing=False donc...)}
  if (not FDataLink.Editing) then
  begin

    //1) On vérifie que le noeud actif soit sur le bon dataset et le bon bookmark
    //1.1) Si oui, on fait la modification
    Ok := False;
    ADataset := Self.DataSource.Dataset;
    ANode:= TDBTreeNode(FLevels.FTreeView.Selected);
    if Assigned(ANode) and Assigned(ANode.TreeLevel) and (ANode.TreeLevel = Self) then
    begin
      if Assigned(ANode.TreeLevel.DataSource)
         and (ANode.TreeLevel.DataSource.DataSet = ADataset) then
      begin
        if ANode.IsSynchronized(ADataset) then
          Ok := True;
      end;
    end;

    //1.2) Si non, on synchronise le TV, puis on fait la modification
    if not Ok then
      ANode := SynchroniseTreeView;

    if Assigned(ANode) then
      FLevels.FTreeView.UpdateNode(Self, ANode)
    else
    begin
      ANode := nil;//TDBTreeNode(FLevels.FTreeView.Selected);
      if not Assigned(ANode) then
      begin
        FLevels.FTreeView.QueryRecreate(nil);
        //SynchroniseTreeView;
      end
      else
        ANode.FillNode;
    end;

  end;

end;

procedure TTreeViewLevel.DataSetChanged;
begin
end;

procedure TTreeViewLevel.ActiveChanged;
begin
  if not (arOnActiveChanged in AutoRecreate) then
    Exit;
  if (DataSource <> nil) and (DataSource.Dataset <> nil) {and (DataSource.DataSet.Active)} then
    FLevels.FTreeView.QueryRecreate(Self);
end;

procedure TTreeViewLevel.DataSetScrolled(Distance: Integer);
begin

end;

procedure TTreeViewLevel.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : Integer;
begin
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = FDataLink.Datasource) then
    begin
      FDataLink.Datasource:= nil;
      FField:= nil;
    end;
    if (AComponent is TDataSource) and (FSubLevels.Count > 0) then
    begin
      for i:= 0 to FSubLevels.Count - 1 do
        FSubLevels[i].Notification(AComponent, Operation);
    end;
  end;
end;

function TTreeViewLevel.GetDisplayName: string;
begin
  Result:= '';
  if FDataLink.DataSource <> nil then
  begin
    Result:= FDataLink.DataSource.Name;
    if FFieldName <> '' then
      Result:= Result + ' : ' + FFieldName;
  end
  else
    Result:= FFieldName;
  if StringsFilling then
    if Result = '' then
      Result := 'Strings filling'
    else
      Result := 'SF - ' + Result;
  if Result = '' then
    Result:= inherited GetDisplayName;
end;

procedure TTreeViewLevel.SetDataSource(Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
  begin
    FDataLink.DataSource:= Value;
    if Value <> nil then
      Value.FreeNotification(FLevels.FTreeView);
  end;
end;

function TTreeViewLevel.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) then
    Result:= FDataLink.DataSource
  else
    Result := nil;
end;

function TTreeViewLevel.GetField: TField;
begin
  if FieldName = '' then
    SetField(nil)
  else if (FField = nil) and (FDataLink.DataSet <> nil) then
    SetField(FDataLink.DataSet.FindField(FieldName));
  Result:= FField;
end;

procedure TTreeViewLevel.SetField(Value: TField);
begin
  if Value <> FField then
  begin
    FField:= Value;
  end;
end;

procedure TTreeViewLevel.SetFieldName(const Value: string);
begin
  if Value <> FFieldName then
  begin
    FFieldName:= Value;
    if (FFieldName = '') then
      SetField(nil)
    else if (FDataLink.Dataset <> nil) then
      SetField(FDataLink.Dataset.FindField(Value));
  end;
end;

procedure TTreeViewLevel.SetSubLevels(const Value: TTreeViewLevels);
begin
  FSubLevels.Assign(Value);
end;

procedure TTreeViewLevel.SetLookupMethod(const Value: TLookupMethod);
begin
  FLookupMethod := Value;
end;

procedure TTreeViewLevel.SetKeyFields(const Value: string);
begin
  FKeyFields := Value;
end;

{ TTreeViewLevels }

constructor TTreeViewLevels.Create(ATreeView: TCustomDBMDTreeView; ALevelClass: TTreeViewLevelClass; AParentLevel : TTreeViewLevel = nil);
begin
  inherited Create(ALevelClass);
  FTreeView := ATreeView;
  FParentLevel := AParentLevel;
end;

function TTreeViewLevels.Add: TTreeViewLevel;
begin
  Result:= TTreeViewLevel(inherited Add);
end;

function TTreeViewLevels.GetLevel(Index: Integer): TTreeViewLevel;
begin
  Result:= TTreeViewLevel(inherited Items[Index]);
end;

function TTreeViewLevels.GetOwner: TPersistent;
begin
  Result:= FTreeView;
end;

procedure TTreeViewLevels.SetLevel(Index: Integer; Value: TTreeViewLevel);
begin
  Items[Index].Assign(Value);
end;

procedure TTreeViewLevels.Update(Item: TCollectionItem);
begin
  if (csLoading in FTreeView.ComponentState) then Exit;
  {Effectuer des modifications...}
end;

{ TCustomDBMDTreeView }

constructor TCustomDBMDTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilling:= False;
  ReadOnly:= True;
  FUseSubLevels:= False;
  FLevels:= TTreeViewLevels.Create(Self, TTreeViewLevel);
end;

destructor TCustomDBMDTreeView.Destroy;
begin
  FLevels.Free;
  FLevels:= nil;
  inherited Destroy;
end;

procedure TCustomDBMDTreeView.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TDataSource) and (FLevels.Count > 0) then
  begin
    for i:= 0 to FLevels.Count - 1 do
      FLevels[i].Notification(AComponent, Operation);
  end;
end;

function TCustomDBMDTreeView.CreateNode: TTreeNode;
begin
  Result:= TDBTreeNode.Create(Items);
end;

procedure TCustomDBMDTreeView.DoAddDBTreeNode(ANode: TDBTreeNode);
begin
  if Assigned(FOnAddNode) then
    FOnAddNode(Self, ANode);
end;

procedure TCustomDBMDTreeView.CNNotify(var Message: TWMNotify);
var
  Node: TTreeNode;
  ChildNode: TDBTreeNode;
begin
  inherited;
  with Message.NMHdr^ do
  begin
    case code of
      TVN_ITEMEXPANDING:
        begin
          with PNMTreeView(Pointer(Message.NMHdr))^ do
          begin
            with ItemNew do
              if (state and TVIF_PARAM) <> 0 then
                Node:= Pointer(lParam)
              else
                Node:= Items.GetNode(hItem);
            if (action = TVE_EXPAND) and CanExpand(Node) then
            begin
              ChildNode:= TDBTreeNode(Node.GetFirstChild);
              if ChildNode.FIsTemporary then
              begin
                ChildNode.Delete;
                TDBTreeNode(Node).SynchronizeDataset(False);
                if UseSubLevels then
                begin
                  if TDBTreeNode(Node).TreeLevel.SubLevels.Count > 0 then
                    FillNode(TDBTreeNode(Node).TreeLevel.SubLevels[0], TDBTreeNode(Node));
                end
                else
                begin
                  if Node.Level < Levels.Count - 1 then
                    FillNode(Levels[Node.Level + 1], TDBTreeNode(Node));
                end;
                if TDBTreeNode(Node).TreeLevel.StringsFilling then
                begin
                  FillNode(TDBTreeNode(Node).TreeLevel, TDBTreeNode(Node));
                end;
                Node.Selected := True;
              end;
            end;
          end;
        end;
    end;
  end;
end;

function TCustomDBMDTreeView.CanEdit(Node: TTreeNode): Boolean;
var
  ALevel: TTreeViewLevel;
begin
  if TDBTreeNode(Node).FIsTemporary then
  begin
    Result:= False;
    Exit;
  end;
  ALevel:= TDBTreeNode(Node).FTreeLevel;
  if (ALevel.DataSource <> nil) and (ALevel.DataSource.DataSet <> nil)
    and (ALevel.Field <> nil) then
  begin
    Result:= (ALevel.DataSource.DataSet.CanModify) and (not ALevel.ReadOnly) and (not ALevel.Field.ReadOnly);
  end
  else
    Result:= False;
  Result:= Result and inherited CanEdit(Node);
  if Result then
  begin
    if Assigned(FBeforeEditing) then
      FBeforeEditing(Self);
  end;
end;

procedure TCustomDBMDTreeView.Edit(const Item: TTVItem);
var
  ALevel: TTreeViewLevel;
  //FTexte: string;
begin
  inherited Edit(Item);
  if Item.pszText <> nil then
  begin
    ALevel:= TDBTreeNode(Selected).TreeLevel;
    if (ALevel.DataSource <> nil) and (ALevel.DataSource.DataSet <> nil)
      and (ALevel.Field <> nil) and (Item.pszText <> ALevel.Field.AsString) then
    begin
      ALevel.DataSource.Edit;
      if ALevel.DataSource.State = dsEdit then
      begin
        ALevel.Field.AsString:= Item.pszText;
      end;
    end;
  end
  else //Le texte est à nil, c'est donc qu'on souhaite annuler l'édition...
  begin
    ALevel:= TDBTreeNode(Selected).TreeLevel;
    if (ALevel.DataSource <> nil) and (ALevel.DataSource.DataSet <> nil)
      and (ALevel.Field <> nil) and (ALevel.DataSource.State in [dsEdit, dsInsert]) then
    begin
      ALevel.DataSource.DataSet.Cancel;
      TDBTreeNode(Selected).Text := ALevel.DataSource.Dataset.FieldByName(ALevel.FieldName).AsString;
    end;
  end;
  if Assigned(FAfterEditing) then
    FAfterEditing(Self);
end;

procedure TCustomDBMDTreeView.Change(Node: TTreeNode);
begin
  if Assigned(Node) and (not FCreating) and (not FFilling) and (not Synchronizing) and (not (csDestroying in ComponentState)) then
    TDBTreeNode(Node).SynchronizeDataset(True);
  inherited Change(Node);
end;

function TCustomDBMDTreeView.GetSynchronizing: Boolean;
begin
  Result:= FSynchronizing > 0;
end;

procedure TCustomDBMDTreeView.BeginSynchronize;
begin
  if FSynchronizing < 0 then
    FSynchronizing:= 0;
  Inc(FSynchronizing);
end;

procedure TCustomDBMDTreeView.EndSynchronize;
begin
  if FSynchronizing < 0 then
    FSynchronizing:= 0;
  if FSynchronizing > 0 then
    Dec(FSynchronizing);
end;

procedure TCustomDBMDTreeView.SetLevels(Value: TTreeViewLevels);
begin
  FLevels.Assign(Value);
end;

procedure TCustomDBMDTreeView.QueryRecreate(Sender: TObject);
begin
  if FTimer = nil then
  begin
    FTimer:= TTimer.Create(Self);
    FTimer.Enabled:= False;
    FTimer.OnTimer:= QueryRecreate;
    FTimer.Interval:= 1;
    FTimer.Enabled:= True;
  end else if (Sender is TTimer) then
  begin
    TTimer(Sender).Free;
    FTimer:= nil;
    CreateTreeView;
  end;
end;

procedure TCustomDBMDTreeView.UpdateNode(ATreeLevel: TTreeViewLevel; ANode: TDBTreeNode);
var
  ACaption : string;
  ADataset : TDataset;
begin
  if Assigned(FOnGetStringCaption) and (ANode.StringKey <> '') then
  begin
    FOnGetStringCaption(Self, ANode, ACaption);
    ANode.Text := ACaption;
  end
  else
  ANode.FTreeLevel := ATreeLevel;
  ANode.FKeyFields := ATreeLevel.KeyFields;
  ANode.ImageIndex:= ATreeLevel.ImageIndex;
  ANode.SelectedIndex:= ATreeLevel.SelectedIndex;
  ANode.StateIndex:= ATreeLevel.StateIndex;

  ADataset := ANode.GetDataset;
  if Assigned(ADataset) then
  begin
    ANode.Text := ADataset.FieldByName(ATreeLevel.FieldName).AsString;
    ANode.GetLookupValue(ADataset);
  end;
end;

procedure TCustomDBMDTreeView.FillNode(ATreeLevel: TTreeViewLevel; ANodeParent: TDBTreeNode; EmptyLevel : Boolean = False);
var
  ADataset: TDataset;
  ANode2: TDBTreeNode;
  ANode3: TDBTreeNode;
  ABookmark: string;
  AField: TField;
  FAlreadyFilling: Boolean;
  ALst : TStringList;
  i : Integer;
begin
  //Au cas où, on supprime les éventuels enfants existants...
  if Assigned(ANodeParent) and EmptyLevel and (not UseSubLevels) then
    ANodeParent.DeleteChildren;
  if (not Assigned(ATreeLevel)) then
  begin
    //Tentative de détermination du TreeLevel automatiquement...
    if not Assigned(ANodeParent.TreeLevel) then
      Exit;
    if UseSubLevels then
    begin
      if ANodeParent.TreeLevel.SubLevels.Count > 0 then
        ATreeLevel := ANodeParent.TreeLevel.SubLevels[0]
    end
    else
    begin //Méthode standard de remplissage...
      if (ANodeParent.TreeLevel.Index < FLevels.Count - 1) then
        ATreeLevel := FLevels[ANodeParent.TreeLevel.Index + 1]
    end;
  end;
  if (not Assigned(ATreeLevel)) then
    Exit;
  if Assigned(FBeforeFill) then
    FBeforeFill(Self, ATreeLevel);
  if FFilling then
    FAlreadyFilling := True
  else
  begin
    FAlreadyFilling := False;
    FFilling:= True;
  end;
  try
    if ATreeLevel.Datasource <> nil then
    begin
      ADataset:= ATreeLevel.Datasource.Dataset;
      if (ADataset <> nil) and (ADataset.Active) then
      begin
        ABookmark:= ADataset.Bookmark;
        try
          AField:= ADataset.FieldByName(ATreeLevel.FieldName);
          ADataset.First;
          while not ADataSet.EOF do
          begin
            ANode2:= TDBTreeNode(Items.AddChildObject(ANodeParent, AField.AsString, nil));

            UpdateNode(ATreeLevel, ANode2);

            {On appelle l'évènement permettant de personnaliser le noeud.}
            DoAddDBTreeNode(ANode2);

            if UseSubLevels then //Méthode récursive (et "nouvelle")
            begin
              if ATreeLevel.SubLevels.Count > 0 then
              begin
                if not ATreeLevel.RunTimeFilling then
                begin
                  FillNode(ATreeLevel.SubLevels[0], ANode2);
                  if ATreeLevel.AutoExpand then
                    ANode2.Expand(False);
                end else
                begin
                  ANode3:= TDBTreeNode(Items.AddChildObject(ANode2, 'Attente', nil));
                  ANode3.FIsTemporary:= True;
                end;
              end;
            end
            else
            begin //Méthode standard de remplissage...
              if (ATreeLevel.Index < FLevels.Count - 1) then
              begin
                if not ATreeLevel.RunTimeFilling then
                begin
                  FillNode(FLevels[ATreeLevel.Index + 1], ANode2);
                  if ATreeLevel.AutoExpand then
                    ANode2.Expand(False);
                end else
                begin
                  ANode3:= TDBTreeNode(Items.AddChildObject(ANode2, 'Attente', nil));
                  ANode3.FIsTemporary:= True;
                end;
              end;
            end;
            ADataSet.Next;
          end;
        finally
          ADataset.Bookmark:= ABookmark;
        end;
      end;
    end;
    if ATreeLevel.StringsFilling then
    begin
      if Assigned(ATreeLevel.FOnStringsNeeded) then
      begin
        ALst := TStringList.Create;
        try
          ATreeLevel.FOnStringsNeeded(ATreeLevel, ANodeParent, ALst);
          if ALst.Count > 0 then
          begin
            for i := 0 to ALst.Count - 1 do
            begin
              ANode2:= TDBTreeNode(Items.AddChildObject(ANodeParent, ALst[i], nil));
              ANode2.StringKey := ALst[i];
              UpdateNode(ATreeLevel, ANode2);
              {On appelle l'évènement permettant de personnaliser le noeud.}
              DoAddDBTreeNode(ANode2);

              //On continue le remplissage jusqu'à ce que la liste renvoyée soit vide...
              if not ATreeLevel.RunTimeFilling then
              begin
                FillNode(ATreeLevel, ANode2);
                if ATreeLevel.AutoExpand then
                  ANode2.Expand(False);
              end else
              begin
                ANode3:= TDBTreeNode(Items.AddChildObject(ANode2, 'Attente', nil));
                ANode3.FIsTemporary:= True;
              end;
            end;
          end;
        finally
          ALst.Free;
        end;
      end;
    end;
    if UseSubLevels then
    begin
      if ATreeLevel.Index < ATreeLevel.FLevels.Count - 1 then
        FillNode(ATreeLevel.FLevels[ATreeLevel.Index + 1], ANodeParent);
    end;
  finally
    if not FAlreadyFilling then
      FFilling:= False;
    if Assigned(FAfterFill) then
      FAfterFill(Self, ATreeLevel);
  end;
end;

procedure TCustomDBMDTreeView.CreateTreeView;
var
  ADataSet: TDataSet;
begin
  FCreating := True;
  try
    Items.BeginUpdate;
    try
      Items.Clear;
      if FLevels.Count > 0 then
      begin
        ADataset:= nil;
        if not UseSubLevels then
        begin
          //Désactivation du dernier niveau
          if FLevels[FLevels.Count - 1].DataSource <> nil then
            ADataSet:= FLevels[FLevels.Count - 1].DataSource.DataSet;
          if ADataSet <> nil then
            ADataSet.DisableControls;
        end;
        try
          FillNode(FLevels[0], nil);
        finally
          if ADataSet <> nil then
            ADataSet.EnableControls;
        end;
      end;
      if Assigned(FAfterCreate) then
        FAfterCreate(Self);
    finally
      Items.EndUpdate;
    end;
  finally
    FCreating := False;
  end;
end;


end.

