unit GZGLMgr;

{$I GrizzlyDefine.INC}

interface

{Les listes génériques
Leur mise en place impose les points suivant :
- Deux tables existent : GGroups et GItems.
- GGroups a la structure suivante :
    GroupName   A 50 *
    AllowAdd    I
    AllowDelete I
    AllowValue  I
    AllowOrder  I
    ItemReadOnly I
    ValueName   A 20
    ValueUnit   A 5

- GItems a la structure suivante :
    GroupName   A 50  *
    Order       I     *
    Item        A 255 *
    Value       A 255
}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB {$IFDEF GZ_D6}, Variants{$ENDIF};

type
  EGenericListError = class(Exception);

  TGenGroupFields = class(TPersistent)
  private
    FGroupName, FAllowAdd, FAllowDelete, FAllowValue: string;
    FAllowOrder, FItemReadOnly, FValueName, FValueUnit: string;
  public
    constructor Create;
  published
    property GroupName: string read FGroupName write FGroupName;
    property AllowAdd: string read FAllowAdd write FAllowAdd;
    property AllowDelete: string read FAllowDelete write FAllowDelete;
    property AllowValue: string read FAllowValue write FAllowValue;
    property AllowOrder: string read FAllowOrder write FAllowOrder;
    property ItemReadOnly: string read FItemReadOnly write FItemReadOnly;
    property ValueName: string read FValueName write FValueName;
    property ValueUnit: string read FValueUnit write FValueUnit;
  end;

  TGenItemFields = class(TPersistent)
  private
    FGroupName, FOrder, FItem, FValue: string;
  public
    constructor Create;
  published
    property GroupName: string read FGroupName write FGroupName;
    property Order: string read FOrder write FOrder;
    property Item: string read FItem write FItem;
    property Value: string read FValue write FValue;
  end;

  TGenGroup = class
  private
    FItems: TStringList;
    FValues: TStringList;
    FName: string;
    FAllowAdd,
      FAllowDelete,
      FAllowValue,
      FAllowOrder,
      FItemReadOnly: Boolean;
    FValueName,
      FValueUnit: string;
    function GetItems(index: Integer): string;
    function GetValues(AItem: string): string;
    function GetDefaultItem: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ValueList: TStringList read FValues;
    property ItemList: TStringList read FItems;
    property Items[index: Integer]: string read GetItems;
    property Values[AItem: string]: string read GetValues;
    property DefaultItem: string read GetDefaultItem;
    property AllowAdd: Boolean read FAllowAdd;
    property AllowDelete: Boolean read FAllowDelete;
    property AllowValue: Boolean read FAllowValue;
    property AllowOrder: Boolean read FAllowOrder;
    property Name: string read FName;
    property ValueName: string read FValueName;
    property ValueUnit: string read FValueUnit;
  end;

  TGLBeforeDeleteEvent = procedure(GenGroup, GenItem: string;
    var AllowDelete: Boolean) of object;

  TCustomGenListManager = class(TComponent)
  private
    { Déclarations privées }
    FLastUpdate: TDateTime;
    FGroupFields: TGenGroupFields;
    FItemFields: TGenItemFields;
    FGroups: TStringList;
    FCached: Boolean;
    //
    FBeforeDelete: TGLBeforeDeleteEvent;
  protected
    { Déclarations protégées }
    function CreateGroupDataset(AReadOnly: Boolean): TDataSet; virtual; abstract;
    function CreateItemDataset(AReadOnly: Boolean): TDataSet; virtual; abstract;
    procedure FreeGroupDataset(ADataset : TDataset); virtual; abstract;
    procedure FreeItemDataset(ADataset : TDataset); virtual; abstract;
    procedure Loaded; override;
    function GetGroupByName(AName: string): TGenGroup;
    function GetGroups(index: Integer): TGenGroup;
    procedure SetCached(Value: Boolean);
    procedure SetGroupFields(Value: TGenGroupFields);
    procedure SetItemFields(Value: TGenItemFields);
    function CanInitializeGL : Boolean; virtual; abstract;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Edit; virtual; abstract;
    procedure Update;
    property LastUpDate: TDateTime read FLastUpDate;
    function QueryAddItem(AGroupName: string; AItem, AValue: string): string;
    procedure AddItem(AGroupName: string; AItem, AValue: string);
    procedure GetItemList(AGroupName: string; AStrLst: TStrings);
    procedure GetGroupList(AStrLst: TStrings);
    function GetItemValue(AGroupName, AItem: string): string;
    procedure SetItemValue(AGroupName, AItem, ANewValue: string);
    function GetDefaultItem(AGroupName: string): string;
    function GetGroupValueUnit(AGroupName: string): string;
    function GetGroupValueName(AGroupName: string): string;
    property GroupByName[AName: string]: TGenGroup read GetGroupByName;
    property Groups[index: Integer]: TGenGroup read GetGroups;
    function FillStrings(AStrings: TStrings; ACanAdd: Boolean; AGroupName: string): Boolean;
  published
    { Déclarations publiées }
    property GroupFields: TGenGroupFields read FGroupFields write SetGroupFields;
    property ItemFields: TGenItemFields read FItemFields write SetItemFields;
    property Cached: Boolean read FCached write SetCached;
    {}
    property BeforeDeleteItem: TGLBeforeDeleteEvent read FBeforeDelete write FBeforeDelete;
  end;

  TWithDatabaseParamsGenListManager = class(TCustomGenListManager)
  private
    FDatabaseName: string;
    FGroupTableName: string;
    FItemTableName: string;
  protected
    procedure FreeGroupDataset(ADataset : TDataset); override;
    procedure FreeItemDataset(ADataset : TDataset); override;
    procedure SetDatabaseName(Value: string);
    procedure SetGroupTableName(Value: string);
    procedure SetItemTableName(Value: string);
    function CanInitializeGL : Boolean; override;
  public
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property GroupTableName: string read FGroupTableName write SetGroupTableName;
    property ItemTableName: string read FItemTableName write SetItemTableName;
  end;

var
  HelpContextEditGL: Integer;

implementation

uses GzConsts, FGUtils, FFGGLAdd, {FFGGLApp,} UFGGLMgr, AClasses;

{ TGenGroup }

constructor TGenGroup.Create;
begin
  inherited Create;
  FItems:= TStringList.Create;
  FValues:= TStringList.Create;
end;

destructor TGenGroup.Destroy;
begin
  FItems.Free;
  FValues.Free;
  inherited Destroy;
end;

function TGenGroup.GetItems(index: Integer): string;
begin
  if (index >= 0) and (index < FItems.Count) then
    Result:= FItems[index]
  else
    Result:= '';
end;

function TGenGroup.GetValues(AItem: string): string;
begin
  Result:= FValues.Values[AItem];
end;

function TGenGroup.GetDefaultItem: string;
begin
  if FItems.Count > 0 then
    Result:= FItems[0];
end;

{ TCustomGenListManager }

constructor TCustomGenListManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGroups:= TStringList.Create;
  FGroups.Sorted:= True;
  FCached:= True;
  FGroupFields:= TGenGroupFields.Create;
  FItemFields:= TGenItemFields.Create;
end;

destructor TCustomGenListManager.Destroy;
begin
  ClearStringList(FGroups);
  FGroups.Free;
  FGroups:= nil;
  FGroupFields.Free;
  FItemFields.Free;
  inherited Destroy;
end;

procedure TCustomGenListManager.Loaded;
begin
  inherited Loaded;
  //try
    Update;
  //except
  //end;
end;

function TCustomGenListManager.FillStrings(AStrings: TStrings; ACanAdd: Boolean; AGroupName: string): Boolean;
var
  AGroup: TGenGroup;
begin
  Result:= False;
  AStrings.Clear;
  AGroup:= GroupByName[AGroupName];
  if Assigned(AGroup) then
  begin
    AStrings.Assign(AGroup.ItemList);
    if ACanAdd and AGroup.AllowAdd then
      //Items.Insert(0, msgItemNewForAdding);
      AStrings.Add(msgItemNewForAdding);
    Result:= True;
  end;
end;

procedure TCustomGenListManager.SetCached(Value: Boolean);
begin
  if FCached <> Value then
  begin
    FCached:= Value;
    UpDate;
  end;
end;

function TCustomGenListManager.GetGroupByName(AName: string): TGenGroup;
var
  k: Integer;
begin
  if FGroups.Find(AName, k) then
    Result:= Groups[k]
  else
    Result:= nil;
end;

function TCustomGenListManager.QueryAddItem(AGroupName: string; AItem, AValue: string): string;
var
  ADlg: TDlgGenListAddItem;
  AGroup: TGenGroup;
begin
  Result:= '';
  AGroup:= GroupByName[AGroupName];
  if (not Assigned(AGroup)) or (not AGroup.AllowAdd) then
    Exit;
  ADlg:= TDlgGenListAddItem.Create(Self);
  try
    ADlg.LabelGroupName.Caption:= AGroupName;
    ADlg.LabelValue.Caption:= AGroup.ValueName;
    if not AGroup.AllowValue then
      ADlg.SetWithoutValue;
    ADlg.EditItem.Text:= AItem;
    ADlg.EditValue.Text:= AValue;
    if (ADlg.ShowModal = mrOk) and (ADlg.EditItem.Text <> '') then
    begin
      AddItem(AGroupName, ADlg.EditItem.Text, ADlg.EditValue.Text);
      Result:= ADlg.EditItem.Text;
    end;
  finally
    ADlg.Free;
  end;
end;

procedure TCustomGenListManager.AddItem(AGroupName: string; AItem, AValue: string);
var
  AGroup: TGenGroup;
  ATable: TDataset;
  AOrder: Integer;
begin
  AGroup:= GroupByName[AGroupName];
  if (not Assigned(AGroup)) or (not AGroup.AllowAdd) then
    Exit;
  ATable:= CreateItemDataset(False);
  try
    try
      ATable.Open;
    except
      on E: Exception do
      begin
        Application.ShowException(E);
        Exit;
      end;
    end;
    try
      if (AGroup.ItemList.Count > 0) then
        AOrder:= Longint(AGroup.ItemList.Objects[AGroup.ItemList.Count - 1])
      else
        AOrder:= 0;
      if ATable.Locate(ItemFields.GroupName+';'+ItemFields.Order+';'+ItemFields.Item, VarArrayOf([AGroupName, AOrder, AItem]), []) then
      begin
        if ATable.FieldByName(ItemFields.Value).AsString <> AValue then
          ATable.Edit;
      end
      else
        ATable.Insert;
      if ATable.State in [dsInsert, dsEdit] then
      begin
        try
          ATable.FieldByName(ItemFields.GroupName).AsString:= AGroupName;
          ATable.FieldByName(ItemFields.Order).AsInteger:= AOrder;
          ATable.FieldByName(ItemFields.Item).AsString:= AItem;
          ATable.FieldByName(ItemFields.Value).AsString:= AValue;
          ATable.Post;
        except
          ATable.Cancel;
          raise;
        end;
        Update;
      end;
    finally
      ATable.Close;
    end;
  finally
    FreeItemDataset(ATable);
  end;
end;

function TCustomGenListManager.GetGroups(index: Integer): TGenGroup;
begin
  Result:= TGenGroup(FGroups.Objects[index])
end;

(*Procedure TCustomGenListManager.Edit;
{Var
  ADlg : TFenGLMgrEdit;}
Begin
{  Try
    ADlg:=TFenGLMgrEdit.Create(Self);
    Try
      ADlg.HelpContext:= HelpContextEditGL;
      ADlg.TableElements.DatabaseName:=DatabaseName;
      ADlg.TableElements.TableName:=ItemTableName;
      ADlg.TableGroupes.DatabaseName:=DatabaseName;
      ADlg.TableGroupes.TableName:=GroupTableName;
      ADlg.GLMgr:= Self;
      ADlg.ShowModal;
    Finally
      ADlg.Free;
    End;
  Finally
    Update;
  End;}
End;*)

procedure TCustomGenListManager.Update;
var
  k: Integer;
  ATable: TDataset;
  AGroup: TGenGroup;
  AOrder: Longint;
begin
  if csLoading in ComponentState then
    Exit;
  FLastUpdate:= Now;
  {Vider la liste}
  ClearStringList(FGroups);
  if not CanInitializeGL then
    Exit;
  {Remplissage des groupes}
  ATable:= CreateGroupDataset(True);
  try
    try
      ATable.Open;
    except
      on E: Exception do
      begin
//        Application.ShowException(E); // est embêtant sur le Loaded
        Exit;
      end;
    end;
    ATable.DisableControls;
    try
      ATable.First;
      while not ATable.EOF do
      begin
        if FGroups.IndexOf(ATable.FieldByName(GroupFields.GroupName).AsString) < 0 then
        begin
          AGroup:= TGenGroup.Create;
          AGroup.FName:= ATable.FieldByName(GroupFields.GroupName).AsString;
          AGroup.FAllowAdd:= (ATable.FieldByName(GroupFields.AllowAdd).AsInteger = 1);
          AGroup.FAllowDelete:= (ATable.FieldByName(GroupFields.AllowDelete).AsInteger = 1);
          AGroup.FAllowValue:= (ATable.FieldByName(GroupFields.AllowValue).AsInteger = 1);
          AGroup.FAllowOrder:= (ATable.FieldByName(GroupFields.AllowOrder).AsInteger = 1);
          AGroup.FItemReadOnly:= (ATable.FieldByName(GroupFields.ItemReadOnly).AsInteger = 1);
          AGroup.FValueName:= ATable.FieldByName(GroupFields.ItemReadOnly).AsString;
          AGroup.FValueUnit:= ATable.FieldByName(GroupFields.ValueUnit).AsString;
          FGroups.AddObject(AGroup.Name, AGroup);
        end;
        ATable.Next;
      end;
    finally
      ATable.EnableControls;
    end;
    ATable.Close;
    FreeGroupDataset(ATable);
    {Remplissage des éléments}
    {Création de la table Item}
    ATable:= CreateItemDataset(True);
    try
      ATable.Open;
    except
      on E: Exception do
      begin
        Application.ShowException(E);
        Exit;
      end;
    end;
    ATable.DisableControls;
    try
      ATable.First;
      while not ATable.EOF do
      begin
        if FGroups.Find(ATable.FieldByName(ItemFields.GroupName).AsString, k) then
        begin
          AOrder:= StrToIntDef(ATable.FieldByName(ItemFields.Order).AsString, 0);
          TGenGroup(FGroups.Objects[k]).ItemList.AddObject(ATable.FieldByName(ItemFields.Item).AsString, Pointer(AOrder));
          TGenGroup(FGroups.Objects[k]).ValueList.Values[ATable.FieldByName(ItemFields.Item).AsString]:= ATable.FieldByName(ItemFields.Value).AsString;
        end;
        ATable.Next;
      end;
    finally
      ATable.EnableControls;
    end;
    ATable.Close;
  finally
    FreeItemDataset(ATable);
  end;
end;

procedure TCustomGenListManager.GetItemList(AGroupName: string; AStrLst: TStrings);
var
  AGroup: TGenGroup;
begin
  if not Cached then
    UpDate;
  AGroup:= GroupByName[AGroupName];
  if Assigned(AGroup) then
    AStrLst.Assign(AGroup.ItemList);
end;

procedure TCustomGenListManager.GetGroupList(AStrLst: TStrings);
var
  k: Integer;
begin
  if not Cached then
    UpDate;
  AStrLst.BeginUpDate;
  try
    AStrLst.Clear;
    for k:= 0 to FGroups.Count - 1 do
      AStrLst.Add(FGroups[k]);
  finally
    AStrLst.EndUpDate;
  end;
end;

function TCustomGenListManager.GetItemValue(AGroupName, AItem: string): string;
var
  AGroup: TGenGroup;
begin
  if not Cached then
    UpDate;
  Result:= '';
  AGroup:= GroupByName[AGroupName];
  if Assigned(AGroup) then
    Result:= AGroup.Values[AItem];
end;

procedure TCustomGenListManager.SetItemValue(AGroupName, AItem, ANewValue: string);
var
  ATable: TDataset;
begin
  if not CanInitializeGL then
    Exit;
  {On ne vérifie pas l'opportunité du changement, du fait de
  l'environnement multi-utilisateur potentiel}
  ATable:= CreateItemDataset(False);
  try
    try
      ATable.Open;
    except
      on E: Exception do
      begin
        Application.ShowException(E);
        Exit;
      end;
    end;
    ATable.Filter:= Format('%s = ''%s'' AND %s = ''%s''',
      [ItemFields.GroupName, AGroupName, ItemFields.Item, AItem]);
    if ATable.FindFirst then
    begin
      ATable.Edit;
      try
        ATable.FieldByName(ItemFields.Value).AsString:= ANewValue;
        ATable.Post;
      except
        ATable.Cancel;
        raise;
      end;
    end;
  finally
    FreeItemDataset(ATable);
  end;
  UpDate;
end;

procedure TCustomGenListManager.SetGroupFields(Value: TGenGroupFields);
begin
  GenericAssign(Value, FGroupFields);
end;

procedure TCustomGenListManager.SetItemFields(Value: TGenItemFields);
begin
  GenericAssign(Value, FItemFields);
end;

function TCustomGenListManager.GetDefaultItem(AGroupName: string): string;
var
  AGroup: TGenGroup;
begin
  if not Cached then
    UpDate;
  Result:= '';
  AGroup:= GroupByName[AGroupName];
  if Assigned(AGroup) then
    Result:= AGroup.DefaultItem;
end;

function TCustomGenListManager.GetGroupValueUnit(AGroupName: string): string;
var
  AGroup: TGenGroup;
begin
  Result:= '';
  if not Cached then
    UpDate;
  AGroup:= GroupByName[AGroupName];
  if Assigned(AGroup) then
    Result:= AGroup.ValueUnit;
end;

function TCustomGenListManager.GetGroupValueName(AGroupName: string): string;
var
  AGroup: TGenGroup;
begin
  Result:= '';
  if not Cached then
    UpDate;
  AGroup:= GroupByName[AGroupName];
  if Assigned(AGroup) then
    Result:= AGroup.ValueName;
end;

{}

constructor TGenGroupFields.Create;
begin
  GroupName:= 'GroupName';
  AllowAdd:= 'AllowAdd';
  AllowDelete:= 'AllowDelete';
  AllowValue:= 'AllowValue';
  AllowOrder:= 'AllowOrder';
  ItemReadOnly:= 'ItemReadOnly';
  ValueName:= 'ValueName';
  ValueUnit:= 'ValueUnit';
end;

constructor TGenItemFields.Create;
begin
  GroupName:= 'GroupName';
  Order:= 'Order';
  Item:= 'Item';
  Value:= 'Value';
end;

function TWithDatabaseParamsGenListManager.CanInitializeGL: Boolean;
begin
  Result := (FItemTableName <> '') and (FGroupTableName <> '');
end;

procedure TWithDatabaseParamsGenListManager.FreeGroupDataset(ADataset: TDataset);
begin
  ADataset.Free;
end;

procedure TWithDatabaseParamsGenListManager.FreeItemDataset(ADataset: TDataset);
begin
  ADataset.Free;
end;

procedure TWithDatabaseParamsGenListManager.SetDatabaseName(Value: string);
begin
  if FDatabaseName <> Value then
  begin
    FDatabaseName:= Value;
    Update;
  end;
end;

procedure TWithDatabaseParamsGenListManager.SetGroupTableName(Value: string);
begin
  if FGroupTableName <> Value then
  begin
    FGroupTableName:= Value;
    Update;
  end;
end;

procedure TWithDatabaseParamsGenListManager.SetItemTableName(Value: string);
begin
  if FItemTableName <> Value then
  begin
    FItemTableName:= Value;
    UpDate;
  end;
end;

end.

