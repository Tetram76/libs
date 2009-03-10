unit GzPnlExpt;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  Buttons, Dialogs, FGSelec, ComCtrls, RXSpin, Mask, ToolEdit, CurrEdit;

type
  TFilterFieldType = (fftString, fftDateTime, fftBoolean, fftInteger);

const
  FilterFieldTypeString : array[TFilterFieldType] of string = ('String', 'DateTime', 'Boolean', 'Integer');
  CM_AUTODELETESELECTIONITEM = WM_USER + 1;
  CM_FIELDCHANGE = WM_USER + 2;

type
  TSQLDialect = (sdStandard, sdFullText, sddtSearch);

  TFilterComboBox = class(TComboBox)
  protected
    procedure CreateWnd; override;
  public
  end;

  EPanelFilterExpert = class(Exception);

  TPanelFilterExpert = class;
  TFilterFieldList = class;
  TSelectionItem = class;
  TSelectionPanel = class;

  TFilterField = class(TCollectionItem)
  private
    FFieldType: TFilterFieldType;
    FFieldName: string;
    FFieldLabel: string;
    procedure SetFieldName(const Value: string);
    function GetFieldLabel: string;
    function GetParent: TFilterFieldList;
    procedure SetFieldLabel(const Value: string);
    procedure SetFieldType(const Value: TFilterFieldType);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    property Parent : TFilterFieldList read GetParent;
  published
    property FieldType : TFilterFieldType read FFieldType write SetFieldType;
    property FieldName : string read FFieldName write SetFieldName;
    property FieldLabel : string read GetFieldLabel write SetFieldLabel;
  end;

  TFilterFieldList = class(TCollection)
  private
    FParent: TPanelFilterExpert;
  protected
    function GetItem(Index: Integer): TFilterField;
    procedure SetItem(Index: Integer; const Value: TFilterField);

    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AParent: TPanelFilterExpert);
    function Add(AFieldName : string; AFieldType : TFilterFieldType = fftString; AFieldLabel : string = '') : TFilterField; 
    property Parent: TPanelFilterExpert read FParent;
    property Items[Index: Integer]: TFilterField read GetItem write SetItem; default;

    function FindByName(AFieldName : string) : TFilterField;
  end;

  TFieldSQLEvent = procedure (Sender : TObject; AFieldName, AOperator, AValue1, AValue2 : string; var ASQL : string) of object;

  TFieldTypePanel = class(TCustomPanel)
  private
    function GetParent: TSelectionPanel;
    procedure SetParent(const Value: TSelectionPanel);
    function GetSelectionItem: TSelectionItem;
    procedure AutoMove;
  protected
    procedure Loaded; override;
    procedure CreateControls; virtual;
    procedure OnSomethingChange(Sender : TObject);
    procedure NotifyChange; virtual;
    procedure SaveToStream(AStream : TStream); virtual; abstract;
    procedure LoadFromStream(AStream : TStream); virtual; abstract;
    procedure ForceRefresh;
    function GetSQL: string; virtual;
    procedure DoSQLEvent(AFieldName, AOperator, AValue1, AValue2 : string; var ASQL : string);
  public
    constructor Create(AOwner : TComponent); override;
    property Parent : TSelectionPanel read GetParent write SetParent;
    property SelectionItem : TSelectionItem read GetSelectionItem;
    property SQL : string read GetSQL;
  end;

  TFieldTypePanelClass = class of TFieldTypePanel;

  TStringFieldPanel = class(TFieldTypePanel)
  private
    FRBContains,
    FRBNotContains : TRadioButton;
    FEditValue : TFilterComboBox;
    procedure GetValues;
  protected
    function GetSQL: string; override;
    procedure CreateControls; override;
    procedure Loaded; override;
    procedure SaveToStream(AStream : TStream); override;
    procedure LoadFromStream(AStream : TStream); override;
  public
    property SQL : string read GetSQL;
  end;

  TBooleanFieldPanel = class(TFieldTypePanel)
  private
    FLabel : TLabel;
    FCBValue : TComboBox;
  protected
    function GetSQL: string; override;
    procedure CreateControls; override;
    procedure Loaded; override;
    procedure SaveToStream(AStream : TStream); override;
    procedure LoadFromStream(AStream : TStream); override;
  public
    property SQL : string read GetSQL;
  end;

  TIntegerFieldPanel = class(TFieldTypePanel)
  private
    FCBOperators : TComboBox;
    FEditValue : TRxCalcEdit;
    procedure OnEditValueChange(Sender: TObject);
  protected
    function GetSQL: string; override;
    procedure CreateControls; override;
    procedure SaveToStream(AStream : TStream); override;
    procedure LoadFromStream(AStream : TStream); override;
  public
    property SQL : string read GetSQL;
  end;

  TDateTimeFieldPanel = class(TFieldTypePanel)
  private
    FRBMois,
    FRBJours,
    FRBEntre : TRadioButton;
    FLabelMois,
    FLabelJours,
    FLabelEtLe : TLabel;
    FEditMois,
    FEditJours : TRxSpinEdit;
    FCBDate1, FCBDate2 : TDateEdit;
    procedure OnRBClick(Sender: TObject);
    procedure OnLabelEntreClick(Sender: TObject);
  protected
    function GetSQL: string; override;
    procedure CreateControls; override;
    procedure SaveToStream(AStream : TStream); override;
    procedure LoadFromStream(AStream : TStream); override;
  public
    property SQL : string read GetSQL;
  end;

  TSelectionPanel = class(TCustomPanel)
  private
    FFieldChanging : Boolean;
    FCBLinks : TComboBox;
    FCBFields : TFilterComboBox;
    FBtnAutoDelete : TSpeedButton;
    FBtnUpDown : TRxSpinButton;
    FSelectionItem: TSelectionItem;
    FPanelValue: TFieldTypePanel;
    function GetParent: TPanelFilterExpert;
    procedure SetParent(const Value: TPanelFilterExpert);
    function GetSQL: string;
    function GetField: TFilterField;
    procedure OnCBFieldsChange(Sender: TObject);
    procedure OnBtnAutoDeleteClick(Sender: TObject);
    procedure OnCBLinksChange(Sender: TObject);
    procedure OnBtnBottomClick(Sender: TObject);
    procedure OnBtnTopClick(Sender: TObject);
    procedure ForceRefresh;
  protected
    procedure NotifyChange;
    procedure CreateControls;
    procedure MoveControls;
    procedure UpdateFields;
    procedure Resize; override;
    procedure FieldChange;
    procedure CheckFirstRow;
    procedure NeedValues(ALst : TStrings);
  public
    constructor Create(AParentItem : TSelectionItem);
    procedure CreateWnd; override;
    property Parent : TPanelFilterExpert read GetParent write SetParent;
    property SelectionItem : TSelectionItem read FSelectionItem;
    property Field : TFilterField read GetField;
    property PanelValue : TFieldTypePanel read FPanelValue;

    property SQL : string read GetSQL;
  end;

  TSelectionItem = class(TCollectionItem)
  private
    FField : TFilterField;
    FFieldName: string;
    FPanel: TSelectionPanel;
    function GetField: TFilterField;
    procedure SetField(const Value: TFilterField);
    procedure SetFieldName(const Value: string);
    procedure FindField;
    function GetParent: TPanelFilterExpert;
    function GetSQL: string;
  protected
    function GetDisplayName: string; override;
    procedure UpdateFields;
    procedure FieldChange;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Field : TFilterField read GetField write SetField;
    property Panel : TSelectionPanel read FPanel;
    property Parent : TPanelFilterExpert read GetParent;

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    property SQL : string read GetSQL;
  published
    property FieldName : string read FFieldName write SetFieldName;
  end;

  TSelectionList = class(TCollection)
  private
    FParent: TPanelFilterExpert;
    function GetSQL: string;
  protected
    function GetItem(Index: Integer): TSelectionItem;
    procedure SetItem(Index: Integer; const Value: TSelectionItem);

    procedure Loaded;

    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    procedure UpdateFields;
  public
    constructor Create(AParent: TPanelFilterExpert);
    function Add: TSelectionItem;
    property Parent: TPanelFilterExpert read FParent;
    property Items[Index: Integer]: TSelectionItem read GetItem write SetItem; default;
    property SQL : string read GetSQL;

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);
  end;

  TNeedValuesEvent = procedure(Sender : TObject; Item : TSelectionItem; ALst : TStrings) of object;

  TPanelFilterExpert = class(TScrollingWinControl)
  private
    FFieldList: TFilterFieldList;
    FSelectionList: TSelectionList;
    FOnSQLChange: TNotifyEvent;
    FOnNeedValues: TNeedValuesEvent;
    FSQLDialect: TSQLDialect;
    FOnGetSQL: TFieldSQLEvent;

    procedure SetFieldList(const Value: TFilterFieldList);
    procedure SetSelectionList(const Value: TSelectionList);
    function GetSQL: string;
    procedure SetSQLDialect(const Value: TSQLDialect);
  protected
    procedure NotifyChange;
    procedure MovePanels;
    procedure Loaded; override;
    procedure Resize; override;
    procedure CMDeleteSelectionItem(var Msg : TMessage); message CM_AUTODELETESELECTIONITEM;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SQL : string read GetSQL;

  published
    property SQLDialect : TSQLDialect read FSQLDialect write SetSQLDialect;
    property Fields : TFilterFieldList read FFieldList write SetFieldList;
    property Selections : TSelectionList read FSelectionList write SetSelectionList;
    property OnSQLChange : TNotifyEvent read FOnSQLChange write FOnSQLChange;
    property OnNeedValues : TNeedValuesEvent read FOnNeedValues write FOnNeedValues;
    property OnGetSQL : TFieldSQLEvent read FOnGetSQL write FOnGetSQL;

    property Align;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property OnEnter;
    property OnExit;
  end;



implementation

{$R GzPnlExpt.res}

uses Math, DateUtil, AFiles;

{ TPanelFilterExpert }

procedure TPanelFilterExpert.CMDeleteSelectionItem(var Msg: TMessage);
begin
  Msg.Result := 1;
  if Msg.Msg = CM_AUTODELETESELECTIONITEM then
    Selections.Delete(Msg.LParam);
end;

constructor TPanelFilterExpert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  HorzScrollBar.Visible := False;
  VertScrollBar.Tracking := True; 

  ParentFont := True;
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];
  Caption := '';

  BevelInner := bvLowered;

  FFieldList := TFilterFieldList.Create(Self);
  FSelectionList := TSelectionList.Create(Self);
end;

destructor TPanelFilterExpert.Destroy;
begin
  FSelectionList.Free;
  FFieldList.Free;
  inherited;
end;

function TPanelFilterExpert.GetSQL: string;
begin
  Result := Selections.SQL;
end;

procedure TPanelFilterExpert.Loaded;
begin
  inherited;
  Selections.Loaded;
  MovePanels;
  NotifyChange;
end;

procedure TPanelFilterExpert.MovePanels;
var
  i, aTop : Integer;
  aPanel : TSelectionPanel;
begin
  if VertScrollBar.IsScrollBarVisible then
    aTop := - VertScrollBar.Position
  else
    aTop := 0;
  for i := 0 to Selections.Count - 1 do
  begin
    aPanel := Selections[i].Panel;
    if Assigned(APanel) then
    begin
      aPanel.TabOrder := i;
      aPanel.Top := aTop;
      aPanel.Left := 0;
      aPanel.Width := ClientWidth;
      Inc(ATop, aPanel.Height);
      aPanel.CheckFirstRow;
    end;
  end;
end;

procedure TPanelFilterExpert.NotifyChange;
begin
  if Assigned(FOnSQLChange) then
    FOnSQLChange(Self);
end;

procedure TPanelFilterExpert.Resize;
begin
  inherited;
  MovePanels;
end;

procedure TPanelFilterExpert.SetFieldList(const Value: TFilterFieldList);
begin
  FFieldList.Assign(Value);
end;

procedure TPanelFilterExpert.SetSelectionList(const Value: TSelectionList);
begin
  FSelectionList.Assign(Value);
end;

procedure TPanelFilterExpert.SetSQLDialect(const Value: TSQLDialect);
begin
  FSQLDialect := Value;
  NotifyChange;
end;

{ TSelectionPanel }

constructor TSelectionPanel.Create(AParentItem : TSelectionItem);
begin
  inherited Create(AParentItem.Parent);
  Top := -1000;
  Height := 40;
  Width := AParentItem.Parent.ClientWidth;

  Parent := AParentItem.Parent;

  FSelectionItem := AParentItem;

  ParentFont := True;
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Caption:= '';

  CreateControls;

  MoveControls;

  UpdateFields;

end;

procedure TSelectionPanel.CreateControls;
begin

  FCBLinks := TComboBox.Create(Self);
  FCBLinks.Parent:= Self;
  FCBLinks.Width:= 40;
  FCBLinks.Top := 3;
  FCBLinks.Left := 3;
  FCBLinks.Anchors := [akLeft, akTop];
  FCBLinks.Style:= csDropDownList;
  FCBLinks.OnChange:= OnCBLinksChange;
  FCBLinks.HandleNeeded;
  FCBLinks.Items.Clear;
  FCBLinks.Items.Add('ET');
  FCBLinks.Items.Add('OU');
  FCBLinks.ItemIndex := 0;

  FCBFields := TFilterComboBox.Create(Self);
  FCBFields.Parent:= Self;
  FCBFields.Width:= 40;
  FCBFields.Top := 3;
  FCBFields.Left := FCBLinks.Left + FCBLinks.Width + 2;
  FCBFields.Anchors := [akLeft, akTop];
  FCBFields.Style:= csDropDownList;
  FCBFields.OnChange:= OnCBFieldsChange;

  FBtnUpDown := TRxSpinButton.Create(Self);
  FBtnUpDown.Parent:= Self;
  FBtnUpDown.Hint := 'Monter/descendre la condition';
  FBtnUpDown.ShowHint := True;
  FBtnUpDown.Width:= 20;
  FBtnUpDown.Height:= 20;
  FBtnUpDown.Top := 3;
  FBtnUpDown.Left := ClientWidth - FBtnUpDown.Width - 3;
  FBtnUpDown.Anchors := [akRight, akTop];
  FBtnUpDown.OnTopClick:= OnBtnTopClick;
  FBtnUpDown.OnBottomClick:= OnBtnBottomClick;

  FBtnAutoDelete := TSpeedButton.Create(Self);
  FBtnAutoDelete.Parent:= Self;
  FBtnAutoDelete.Glyph.LoadFromResourceName(HInstance, 'BTNDELETEPANEL');
  FBtnAutoDelete.Hint := 'Supprimer la condition';
  FBtnAutoDelete.ShowHint := True;
  FBtnAutoDelete.Width:= 20;
  FBtnAutoDelete.Height:= 20;
  FBtnAutoDelete.Top := 3;
  FBtnAutoDelete.Left := FBtnUpDown.Left - FBtnAutoDelete.Width - 1;
  FBtnAutoDelete.Anchors := [akRight, akTop];
  FBtnAutoDelete.OnClick:= OnBtnAutoDeleteClick;

end;

procedure TSelectionPanel.CreateWnd;
begin
  inherited;
end;

function TSelectionPanel.GetField: TFilterField;
begin
  Result := SelectionItem.Field;
end;

function TSelectionPanel.GetParent: TPanelFilterExpert;
begin
  Result := TPanelFilterExpert(inherited Parent);
end;

function TSelectionPanel.GetSQL: string;
begin
  if Assigned(FPanelValue) then
  begin
    if (SelectionItem.Index > 0) then
    begin
      if FCBLinks.ItemIndex = 0 then
        Result := 'AND '
      else
        Result := 'OR ';
    end
    else
      Result := '';
    Result := Result + FPanelValue.SQL;
  end
  else
    Result := '';
end;

procedure TSelectionPanel.OnBtnAutoDeleteClick(Sender: TObject);
begin
  if MessageDlg('Confirmez-vous l''effacement de la condition ?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    PostMessage(Parent.Handle, CM_AUTODELETESELECTIONITEM, 0, SelectionItem.Index);
end;

procedure TSelectionPanel.OnBtnTopClick(Sender: TObject);
begin
  if SelectionItem.Index > 0 then
    SelectionItem.Index := SelectionItem.Index - 1;
  NotifyChange;
end;

procedure TSelectionPanel.OnBtnBottomClick(Sender: TObject);
begin
  if SelectionItem.Index < SelectionItem.Collection.Count - 1 then
    SelectionItem.Index := SelectionItem.Index + 1;
  NotifyChange;
end;

procedure TSelectionPanel.OnCBFieldsChange(Sender: TObject);
begin
  FFieldChanging := True;
  try
    if (FCBFields.ItemIndex >= 0) and Assigned(FCBFields.Items.Objects[FCBFields.ItemIndex]) then
      SelectionItem.FieldName := TFilterField(FCBFields.Items.Objects[FCBFields.ItemIndex]).FieldName
    else
      SelectionItem.FieldName := '';
  finally
    FFieldChanging := False;
  end;
  Parent.NotifyChange;
end;

procedure TSelectionPanel.OnCBLinksChange(Sender: TObject);
begin
  NotifyChange;
end;

procedure TSelectionPanel.SetParent(const Value: TPanelFilterExpert);
begin
  inherited Parent := Value;
end;

procedure TSelectionPanel.UpdateFields;
var
  k, i : Integer;
  OldField : TFilterField;
begin
  FCBFields.Items.BeginUpdate;
  try
    k := -1;
    if (FCBFields.ItemIndex >= 0) and Assigned(FCBFields.Items.Objects[FCBFields.ItemIndex]) then
      OldField := TFilterField(FCBFields.Items.Objects[FCBFields.ItemIndex])
    else
      OldField := nil;
    FCBFields.Items.Clear;
    for i := 0 to Parent.Fields.Count - 1 do
    begin
      FCBFields.Items.AddObject(Parent.Fields[i].FieldLabel, Parent.Fields[i]);
      if (Assigned(OldField) and (Parent.Fields[i] = OldField))  then
        k := i;
    end;
    if k < 0 then
    begin
      OldField := Parent.Fields.FindByName(SelectionItem.FieldName);
      if Assigned(OldField) then
        FCBFields.ItemIndex := FCBFields.Items.IndexOfObject(OldField)
      else
        FCBFields.ItemIndex := -1;
    end
    else
      FCBFields.ItemIndex := k;
  finally
    FCBFields.Items.EndUpdate;
  end;
end;

procedure TSelectionPanel.Resize;
begin
  inherited;
  MoveControls;
  Parent.MovePanels;
  ForceRefresh;
  if Assigned(FPanelValue) then
    FPanelValue.ForceRefresh;
end;

procedure TSelectionPanel.FieldChange;
var
  AClass : TFieldTypePanelClass;
  i : Integer;
begin
  if not FFieldChanging then
  begin
    for i := 0 to FCBFields.Items.Count - 1 do
    begin
      if FCBFields.Items.Objects[i] = Field then
      begin
        FCBFields.ItemIndex := i;
        Break;
      end;
    end;
  end;

  AClass := TStringFieldPanel;
  if Assigned(Field) then
  begin
    case Field.FieldType of
    fftString : AClass := TStringFieldPanel;
    fftDateTime : AClass := TDateTimeFieldPanel;
    fftBoolean : AClass := TBooleanFieldPanel;
    fftInteger : AClass := TIntegerFieldPanel;
    else
      AClass := TStringFieldPanel;
    end;
  end;

  if Assigned(FPanelValue) then
  begin
    if (FPanelValue.ClassType <> AClass) then
    begin
      FPanelValue.Free;
      FPanelValue := nil;
      FPanelValue := AClass.Create(Self);
    end;
  end
  else
    FPanelValue := AClass.Create(Self);
  MoveControls;
  FPanelValue.Loaded;
  ForceRefresh;
  //FPanelValue.ForceRefresh; //Obligatoire pour tout afficher...
end;

procedure TSelectionPanel.ForceRefresh;
var
  i : Integer;
begin
  for i := 0 to ControlCount - 1 do
    Controls[i].Refresh;
end;

procedure TSelectionPanel.MoveControls;
begin
  if Assigned(FCBFields) then
  begin
    if FCBLinks.Visible then
      FCBFields.Width := Trunc(ClientWidth * 0.18)
    else
      FCBFields.Width := Trunc(ClientWidth * 0.18) + FCBLinks.Width + 2;
    if Assigned(FPanelValue) then
    begin
      FPanelValue.AutoMove;
      {FPanelValue.Left := FCBFields.Left + FCBFields.Width + 2;
      FPanelValue.Width := FBtnAutoDelete.Left - FPanelValue.Left - 2;}
    end;
  end;
end;

procedure TSelectionPanel.NotifyChange;
begin
  Parent.NotifyChange;
end;

procedure TSelectionPanel.CheckFirstRow;
begin
  if FCBLinks.Visible and (SelectionItem.Index = 0) then
  begin //Il faut le cacher
    FCBLinks.Visible := False;
    FCBFields.SetBounds(FCBLinks.Left, FCBFields.Top, FCBLinks.Width + FCBFields.Width + 2, FCBFields.Height);
  end
  else
  if (not FCBLinks.Visible) and (SelectionItem.Index <> 0) then
  begin //Il faut le remontrer
    FCBLinks.Visible := True;
    FCBFields.SetBounds(FCBLinks.Left + FCBLinks.Width + 2, FCBFields.Top, FCBFields.Width  - FCBLinks.Width - 2, FCBFields.Height);
  end;
end;

procedure TSelectionPanel.NeedValues(ALst: TStrings);
begin
  if Assigned(Parent.FOnNeedValues) then
    Parent.FOnNeedValues(Parent, SelectionItem, ALst);
end;

{ TFilterField }

constructor TFilterField.Create(Collection: TCollection);
begin
  inherited;

end;

function TFilterField.GetDisplayName: string;
begin
  Result := Format('%s (%s) : %s', [FieldName, FilterFieldTypeString[FieldType], FieldLabel]);
end;

function TFilterField.GetFieldLabel: string;
begin
  if FFieldLabel <> '' then
    Result := FFieldLabel
  else
    Result := FFieldName;
end;

function TFilterField.GetParent: TFilterFieldList;
begin
  Result := TFilterFieldList(Collection);
end;

procedure TFilterField.SetFieldLabel(const Value: string);
begin
  FFieldLabel := Value;
  Parent.Parent.Selections.UpdateFields;
end;

procedure TFilterField.SetFieldName(const Value: string);
begin
  FFieldName := Trim(Value);
  Parent.Parent.Selections.UpdateFields;
end;

procedure TFilterField.SetFieldType(const Value: TFilterFieldType);
begin
  FFieldType := Value;
  Parent.Parent.Selections.UpdateFields;
end;

{ TFilterFieldList }

function TFilterFieldList.Add(AFieldName : string; AFieldType : TFilterFieldType = fftString; AFieldLabel : string = ''): TFilterField;
begin
  Result := TFilterField(inherited Add);
  Result.FFieldType := AFieldType;
  Result.FFieldLabel := AFieldLabel;
  Result.FFieldName := AFieldName;     
  Parent.Selections.UpdateFields;
end;

constructor TFilterFieldList.Create(AParent: TPanelFilterExpert);
begin
  inherited Create(TFilterField);
  FParent := AParent;
end;

function TFilterFieldList.FindByName(AFieldName: string): TFilterField;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].FieldName, Trim(AFieldName)) = 0 then
    begin
      Result := Items[i];
      Exit;
    end;
end;

function TFilterFieldList.GetItem(Index: Integer): TFilterField;
begin
  Result := TFilterField(inherited GetItem(Index));
end;

function TFilterFieldList.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TFilterFieldList.SetItem(Index: Integer; const Value: TFilterField);
begin
  inherited SetItem(Index, Value);
end;

procedure TFilterFieldList.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TSelectionItem }

constructor TSelectionItem.Create(Collection: TCollection);
begin
  inherited;
  FPanel := TSelectionPanel.Create(Self);
  Parent.MovePanels;
  {if Assigned(FPanel.PanelValue) then
    FPanel.PanelValue.ForceRefresh;}
end;

destructor TSelectionItem.Destroy;
begin
  FPanel.Free;
  inherited;
end;

procedure TSelectionItem.FieldChange;
begin
  Panel.FieldChange;
end;

procedure TSelectionItem.FindField;
begin
  FField := Parent.Fields.FindByName(FieldName);
end;

function TSelectionItem.GetDisplayName: string;
begin
  Result := FFieldName;
end;

function TSelectionItem.GetField: TFilterField;
begin
  if (not Assigned(FField)) and (FFieldName <> '') then
    FindField;
  Result := FField;
end;

function TSelectionItem.GetParent: TPanelFilterExpert;
begin
  Result := TSelectionList(Collection).Parent;
end;

function TSelectionItem.GetSQL: string;
begin
  if Assigned(Panel) then
    Result := Panel.SQL
  else
    Result := '';
end;

procedure TSelectionItem.LoadFromStream(AStream: TStream);
var
  k : Integer;
  T : TFilterFieldType;
begin
  FieldName := TUsefulStream(AStream).ReadString;
  //On écrit le type actuel du champ, et on écrit ensuite le
  //contenu du PanelValue. A la lecture, si le type est le même
  //on pourra relire sans danger... sinon, tant pis, mais on lira tout de même :-)

  if Assigned(Panel) then
    Panel.FCBLinks.ItemIndex := TUsefulStream(AStream).ReadLongInt
  else
    TUsefulStream(AStream).ReadLongint;

  AStream.Read(T, SizeOf(T));
  k := TUsefulStream(AStream).ReadLongint;
  if k = 0 then
    Exit;
  if (Assigned(Field) and (Field.FieldType = T)) and Assigned(Panel) and Assigned(Panel.PanelValue) then
    Panel.PanelValue.LoadFromStream(AStream)
  else
    AStream.Position := AStream.Position + k;
end;

procedure TSelectionItem.SaveToStream(AStream: TStream);
var
  k : Integer;
  T : TFilterFieldType;
  AMS : TMemoryStream;
begin
  TUsefulStream(AStream).WriteString(FieldName);
  //On écrit le type actuel du champ, et on écrit ensuite le
  //contenu du PanelValue. A la lecture, si le type est le même
  //on pourra relire sans danger... sinon, tant pis, mais on lira tout de même :-)

  if Assigned(Panel) then
    TUsefulStream(AStream).WriteLongInt(Panel.FCBLinks.ItemIndex)
  else
    TUsefulStream(AStream).WriteLongInt(0);

  if Assigned(Field) then
    T := Field.FieldType
  else
    T := fftString;
  AStream.Write(T, SizeOf(T));
  if (not Assigned(Field)) or (not Assigned(Panel)) or (not Assigned(Panel.PanelValue)) then
  begin
    k := 0;
    AStream.Write(k, SizeOf(k));
  end
  else
  begin
    AMS := TMemoryStream.Create;
    try
      Panel.PanelValue.SaveToStream(AMS);
      AMS.Position := 0;
      k := AMS.Size;
      AStream.Write(k, SizeOf(k));
      AStream.CopyFrom(AMS, k);
    finally
      AMS.Free;
    end;
  end;

end;

procedure TSelectionItem.SetField(const Value: TFilterField);
begin
  FField := Value;
  FFieldName := FField.FieldName;
  FieldChange;
end;

procedure TSelectionItem.SetFieldName(const Value: string);
begin
  FFieldName := Trim(Value);
  FindField;
  FieldChange;
end;

procedure TSelectionItem.UpdateFields;
begin
  Panel.UpdateFields;
end;

{ TSelectionList }

function TSelectionList.Add: TSelectionItem;
begin
  Result := TSelectionItem(inherited Add);
end;

constructor TSelectionList.Create(AParent: TPanelFilterExpert);
begin
  inherited Create(TSelectionItem);
  FParent := AParent;
end;

function TSelectionList.GetItem(Index: Integer): TSelectionItem;
begin
  Result := TSelectionItem(inherited GetItem(Index));
end;

function TSelectionList.GetOwner: TPersistent;
begin
  Result := Parent;
end;

function TSelectionList.GetSQL: string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Result = '' then
      Result := Items[i].SQL
    else
      Result := Result + ' ' + Items[i].SQL;
  end;
end;

procedure TSelectionList.Loaded;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Assigned(Items[i].Panel) then
      Items[i].Panel.Loaded;
  end;
end;

procedure TSelectionList.LoadFromStream(AStream: TStream);
var
  i, k : Integer;
begin
  Clear;
  AStream.Read(k, SizeOf(k));
  if k > 0 then
    for i := 0 to k - 1 do
      Add.LoadFromStream(AStream);

  //Parent.Refresh;
end;

procedure TSelectionList.SaveToStream(AStream: TStream);
var
  i, k : Integer;
begin
  k := Count;
  AStream.Write(k, SizeOf(k));
  for i := 0 to Count - 1 do
    Items[i].SaveToStream(AStream);
end;

procedure TSelectionList.SetItem(Index: Integer; const Value: TSelectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSelectionList.Update(Item: TCollectionItem);
begin
  inherited;
  Parent.MovePanels;
  Parent.NotifyChange;
end;

procedure TSelectionList.UpdateFields;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].UpdateFields;
  end;
end;

{ TFieldTypePanel }

constructor TFieldTypePanel.Create(AOwner: TComponent);
begin
  inherited;
  Parent := TSelectionPanel(AOwner);

  Top := -1000;

  ParentFont := True;
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Caption:= '';

  BevelOuter := bvNone;
  BorderWidth := 1;

  Font.Size := 7;
  Font.Name := 'Small Fonts';

  CreateControls;

  AutoMove;
end;

procedure TFieldTypePanel.CreateControls;
begin
end;

procedure TFieldTypePanel.AutoMove;
begin
  Left := Parent.FCBFields.Left + Parent.FCBFields.Width + 2;
  Top := 2;
  Width := Parent.FBtnAutoDelete.Left - Left - 2;
  Parent.Height := Height + 4;
  //ForceRefresh;
end;

function TFieldTypePanel.GetParent: TSelectionPanel;
begin
  Result := TSelectionPanel(inherited Parent);
end;

function TFieldTypePanel.GetSelectionItem: TSelectionItem;
begin
  Result := Parent.SelectionItem;
end;

function TFieldTypePanel.GetSQL: string;
begin
  Result := '%FIELDNAME%';
end;

procedure TFieldTypePanel.SetParent(const Value: TSelectionPanel);
begin
  inherited SetParent(Value);
end;

procedure TFieldTypePanel.NotifyChange;
begin
  Parent.NotifyChange;
end;

procedure TFieldTypePanel.OnSomethingChange(Sender: TObject);
begin
  NotifyChange;
end;

procedure TFieldTypePanel.Loaded;
begin
  inherited;
  //Vide, mais appelé par le Panel au moment où tout est chargé...
end;

procedure TFieldTypePanel.ForceRefresh;
var
  i : Integer;
begin
  for i := 0 to ControlCount - 1 do
    Controls[i].Refresh;
end;

procedure TFieldTypePanel.DoSQLEvent(AFieldName, AOperator, AValue1,
  AValue2: string; var ASQL: string);
begin
  if Assigned(Parent.Parent.FOnGetSQL) then
    Parent.Parent.FOnGetSQL(Parent.Parent, AFieldName, AOperator, AValue1, AValue2, ASQL);
end;

{ TStringFieldPanel }

procedure TStringFieldPanel.CreateControls;
begin
  inherited;

  Canvas.Font := Font;
  FRBContains := TRadioButton.Create(Self);
  FRBContains.Parent:= Self;
  FRBContains.Height:= Max(13, Canvas.TextHeight('Òq') + 2);
  FRBContains.Top := 1;
  FRBContains.Left := 1;
  FRBContains.Anchors := [akLeft, akTop];
  FRBContains.Caption := 'contient';
  FRBContains.Checked := True;
  FRBContains.OnClick:= OnSomethingChange;

  FRBNotContains := TRadioButton.Create(Self);
  FRBNotContains.Parent:= Self;
  FRBNotContains.Height:= Max(13, Canvas.TextHeight('Òq') + 2);
  FRBNotContains.Top := FRBContains.Top + FRBContains.Height + 2;
  FRBNotContains.Left := 1;
  FRBNotContains.Anchors := [akLeft, akTop];
  FRBNotContains.Caption := 'ne contient pas';
  FRBNotContains.OnClick:= OnSomethingChange;

  FRBContains.Width := Max(Canvas.TextWidth(FRBContains.Caption), Canvas.TextWidth(FRBNotContains.Caption)) + 20;
  FRBNotContains.Width := FRBContains.Width;

  FEditValue := TFilterComboBox.Create(Self);
  FEditValue.Parent:= Self;
  FEditValue.Font := Parent.Font;
  FEditValue.Top := 1;
  FEditValue.Left := FRBContains.Left + FRBContains.Width + 2;
  FEditValue.Width:= Width - FEditValue.Left - 1;
  FEditValue.Anchors := [akLeft, akRight, akTop];
  FEditValue.OnChange := OnSomethingChange;
  FEditValue.Style := csDropDown;

  Height := FRBNotContains.Top + FRBNotContains.Height + 1;

end;

procedure TStringFieldPanel.Loaded;
begin
  GetValues;
end;

procedure TStringFieldPanel.GetValues;
var
  ALst : TStringList;
begin
  ALst := TStringList.Create;
  try
    Parent.NeedValues(ALst);
    FEditValue.Items.Assign(ALst);
  finally
    ALst.Free;
  end;
end;

function TStringFieldPanel.GetSQL: string;
var
  FValeur : string;
begin
  if FEditValue.Text = '' then
  begin
    if FRBContains.Checked then
      Result := Format('((%s is null) or (%s=''''))', [SelectionItem.FieldName, SelectionItem.FieldName])
    else
      Result := Format('(%s is not null)', [SelectionItem.FieldName]);
  end
  else
  if Parent.Parent.SQLDialect = sdStandard then
  begin
    FValeur := AnsiQuotedStr('%' + FEditValue.Text + '%', '''');
    if FRBContains.Checked then
      Result := Format('(%s like %s)', [SelectionItem.FieldName, FValeur])
    else
      Result := Format('(not (%s like %s))', [SelectionItem.FieldName, FValeur]);
  end
  else
  if Parent.Parent.SQLDialect = sdFullText then
  begin
    FValeur := AnsiQuotedStr(FEditValue.Text, '''');
    if FRBContains.Checked then
      Result := Format('FREETEXT(%s, %s)', [SelectionItem.FieldName, FValeur])
    else
      Result := Format('(not FREETEXT(%s, %s))', [SelectionItem.FieldName, FValeur]);
  end;
  if Parent.Parent.SQLDialect = sddtSearch then
  begin
    FValeur := FEditValue.Text;
    if FRBContains.Checked then
      Result := Format('(%s contains %s)', [SelectionItem.FieldName, FValeur])
    else
      Result := Format('(not (%s contains %s))', [SelectionItem.FieldName, FValeur]);
  end;
  if FRBContains.Checked then
    DoSQLEvent(SelectionItem.FieldName, '', FValeur, '', Result)
  else
    DoSQLEvent(SelectionItem.FieldName, 'not', FValeur, '', Result)
end;

procedure TStringFieldPanel.LoadFromStream(AStream: TStream);
begin
  if TUsefulStream(AStream).ReadChar = 'Y' then
    FRBContains.Checked := True
  else
    FRBNotContains.Checked := True;
  FEditValue.Text := TUsefulStream(AStream).ReadString;
end;

procedure TStringFieldPanel.SaveToStream(AStream: TStream);
begin
  if FRBContains.Checked then
    TUsefulStream(AStream).WriteChar('Y')
  else
    TUsefulStream(AStream).WriteChar('N');
  TUsefulStream(AStream).WriteString(FEditValue.Text);
end;

{ TBooleanFieldPanel }

procedure TBooleanFieldPanel.CreateControls;
begin
  inherited;

  FLabel := TLabel.Create(Self);
  FLabel.Parent:= Self;
  FLabel.Top := 1;
  FLabel.Left := 1;
  FLabel.Caption := ' est  ';
  FLabel.Anchors := [akLeft, akTop];

  FCBValue := TComboBox.Create(Self);
  FCBValue.Parent:= Self;
  FCBValue.Top := 1;
  FCBValue.Left := FLabel.Width + 2;
  FCBValue.Width:= Width - FCBValue.Left - 1;
  FCBValue.Style := csDropDownList;
  FCBValue.Anchors := [akLeft, akRight, akTop];
  FCBValue.OnChange := OnSomethingChange;
  FCBValue.HandleNeeded;
  FCBValue.Items.Add('Vrai');
  FCBValue.Items.Add('Faux');
  FCBValue.ItemIndex := 0;
  FCBValue.Font := Parent.Font;

  FLabel.Top := FCBValue.Top + (FCBValue.Height - FLabel.Height) div 2;

  Height := FCBValue.Top + FCBValue.Height + 1;

end;

function TBooleanFieldPanel.GetSQL: string;
var
  FValeur : string;
begin
  if FCBValue.ItemIndex = 0 then
    FValeur := SQLBoolTrue
  else
    FValeur := SQLBoolFalse;
  if SQLBoolQuote <> #0 then
    FValeur := AnsiQuotedStr(FValeur, SQLBoolQuote);
  if Parent.Parent.SQLDialect <> sddtSearch then
    Result := Format('(%s=%s)', [SelectionItem.FieldName, FValeur])
  else
    Result := Format('(%s contains %s)', [SelectionItem.FieldName, FValeur]);
  DoSQLEvent(SelectionItem.FieldName, '', FValeur, '', Result);
end;

procedure TBooleanFieldPanel.Loaded;
var
  ALst : TStringList;
begin
  inherited;
  ALst := TStringList.Create;
  try
    Parent.NeedValues(ALst);
    if ALst.Count = 2 then
      FCBValue.Items.Assign(ALst);
  finally
    ALst.Free;
  end;
end;

procedure TBooleanFieldPanel.LoadFromStream(AStream: TStream);
begin
  FCBValue.ItemIndex := TUsefulStream(AStream).ReadLongInt;
end;

procedure TBooleanFieldPanel.SaveToStream(AStream: TStream);
begin
  TUsefulStream(AStream).WriteLongInt(FCBValue.ItemIndex);
end;

{ TDateTimeFieldPanel }

procedure TDateTimeFieldPanel.CreateControls;
var
  FWidth : Integer;
begin
  inherited;
  {FRBMois,
  FRBJours,
  FRBEntre : TRadioButton;
  FEditMois,
  FEditJours : TRxSpinEdit;
  FCBDate1, FCBDate2 : TDateEdit;}

  Canvas.Font := Font;

  FRBMois := TRadioButton.Create(Self);
  FRBMois.Parent:= Self;
  FRBMois.Top := 1;
  FRBMois.Left := 1;
  FRBMois.Caption := 'dans les';
  FRBMois.Height:= Max(13, Canvas.TextHeight('Òq'));
  FRBMois.Width:= Canvas.TextWidth(FRBMois.Caption);
  FRBMois.Anchors := [akLeft, akTop];
  FRBMois.OnClick := OnRBClick;

  FRBJours := TRadioButton.Create(Self);
  FRBJours.Parent:= Self;
  FRBJours.Top := FRBMois.Top + FRBMois.Height + 2;
  FRBJours.Left := 1;
  FRBJours.Caption := 'dans les';
  FRBJours.Height:= FRBMois.Height;
  FRBJours.Width:= Canvas.TextWidth(FRBJours.Caption);
  FRBJours.Anchors := [akLeft, akTop];
  FRBJours.OnClick := OnRBClick;

  FRBEntre := TRadioButton.Create(Self);
  FRBEntre.Parent:= Self;
  FRBEntre.Top := FRBJours.Top + FRBJours.Height + 2;
  FRBEntre.Left := 1;
  FRBEntre.Caption := 'entre le';
  FRBEntre.Height:= FRBMois.Height;
  FRBEntre.Width:= Canvas.TextWidth(FRBEntre.Caption);
  FRBEntre.Anchors := [akLeft, akTop];
  FRBEntre.OnClick := OnRBClick;

  FWidth := Max(FRBEntre.Width, Max(FRBMois.Width, FRBJours.Width)) + 20;
  FRBEntre.Width := FWidth;
  FRBMois.Width := FWidth;
  FRBJours.Width := FWidth;

  FEditMois := TRxSpinEdit.Create(Self);
  FEditMois.Parent:= Self;
  FEditMois.Font := Parent.Font;
  FEditMois.Top := 1;
  FEditMois.Left := FRBMois.Left + FRBMois.Width + 2;
  FEditMois.Width:= 50;
  FEditMois.Anchors := [akLeft, akRight, akTop];
  FEditMois.OnChange := OnSomethingChange;

  FEditJours := TRxSpinEdit.Create(Self);
  FEditJours.Parent:= Self;
  FEditJours.Font := Parent.Font;
  FEditJours.Top := FEditMois.Top + FEditMois.Height + 2;
  FEditJours.Left := FRBMois.Left + FRBMois.Width + 2;
  FEditJours.Width:= 50;
  FEditJours.Anchors := [akLeft, akRight, akTop];
  FEditJours.OnChange := OnSomethingChange;

  FRBMois.Top := FEditMois.Top + (FEditMois.Height - FRBMois.Height) div 2;
  FRBJours.Top := FEditJours.Top + (FEditJours.Height - FRBJours.Height) div 2;

  FLabelMois := TLabel.Create(Self);
  FLabelMois.Parent:= Self;
  FLabelMois.Caption := 'derniers mois';
  FLabelMois.Top := FEditMois.Top + (FEditMois.Height - FLabelMois.Height) div 2;
  FLabelMois.Left := FEditMois.Left + FEditMois.Width + 2;
  FLabelMois.Anchors := [akRight, akTop];

  FLabelJours := TLabel.Create(Self);
  FLabelJours.Parent:= Self;
  FLabelJours.Caption := 'derniers jours';
  FLabelJours.Top := FEditJours.Top + (FEditJours.Height - FLabelJours.Height) div 2;
  FLabelJours.Left := FEditJours.Left + FEditJours.Width + 2;
  FLabelJours.Anchors := [akRight, akTop];

  FWidth := Max(FLabelMois.Width, FLabelJours.Width);
  FLabelMois.Left := Width - 2 - FWidth;
  FLabelJours.Left := FLabelMois.Left;

  FEditJours.Width := FLabelJours.Left - FEditJours.Left - 1;
  FEditMois.Width := FLabelMois.Left - FEditMois.Left - 1;

  FCBDate1 := TDateEdit.Create(Self);
  FCBDate1.Parent:= Self;
  FCBDate1.Font := Parent.Font;
  FCBDate1.Top := FEditJours.Top + FEditJours.Height + 2;
  FCBDate1.Left := FEditMois.Left;
  FCBDate1.Width:= FEditMois.Width + FWidth + 2;
  FCBDate1.Anchors := [akLeft, akRight, akTop];
  FCBDate1.OnChange := OnSomethingChange;
  FCBDate1.Date := Now - 30;

  FRBEntre.Top := FCBDate1.Top + (FCBDate1.Height - FRBEntre.Height) div 2;

  FCBDate2 := TDateEdit.Create(Self);
  FCBDate2.Parent:= Self;
  FCBDate2.Font := Parent.Font;
  FCBDate2.Top := FCBDate1.Top + FCBDate1.Height + 2;
  FCBDate2.Left := FEditMois.Left;
  FCBDate2.Width:= FEditMois.Width + FWidth + 2;
  FCBDate2.Anchors := [akLeft, akRight, akTop];
  FCBDate2.OnChange := OnSomethingChange;
  FCBDate2.Date := Now;

  FLabelEtLe := TLabel.Create(Self);
  FLabelEtLe.Parent:= Self;
  FLabelEtLe.Caption := 'et le   ';
  FLabelEtLe.Top := FCBDate2.Top + (FCBDate2.Height - FLabelEtLe.Height) div 2;
  FLabelEtLe.Left := FCBDate2.Left - 2 - FLabelEtLe.Width;
  FLabelEtLe.Anchors := [akLeft, akTop];
  FLabelEtLe.OnClick := OnLabelEntreClick;

  FRBEntre.Checked := True;

  Height := FCBDate2.Top + FCBDate2.Height + 1;
end;

function TDateTimeFieldPanel.GetSQL: string;
var
  ADate1, ADate2 : string;
  function FormatTheDate(ADate : TDateTime) : string;
  begin
    if Frac(ADate) = 0 then
      Result := FormatDateTime(SQLDateFormat, ADate)
    else
      Result := FormatDateTime(SQLDateTimeFormat, ADate);
    if SQLDateQuote <> #0 then
      Result := AnsiQuotedStr(Result, SQLDateQuote);
  end;
begin
  if FRBMois.Checked then
  begin
    ADate1 := FormatTheDate(IncMonth(Date, - FEditMois.AsInteger));
    ADate2 := FormatTheDate(IncYear(Date, 50));
    if Parent.Parent.SQLDialect <> sddtSearch then
      Result := Format('(%s>=%s)', [SelectionItem.FieldName, ADate1])
    else
      Result := Format('(%s contains %s~~%s)', [SelectionItem.FieldName, ADate1, ADate2])
  end
  else
  if FRBJours.Checked then
  begin
    ADate1 := FormatTheDate(IncDay(Date, - FEditJours.AsInteger));
    ADate2 := FormatTheDate(IncYear(Date, 50));
    if Parent.Parent.SQLDialect <> sddtSearch then
      Result := Format('(%s>=%s)', [SelectionItem.FieldName, ADate1])
    else
      Result := Format('(%s contains %s~~%s)', [SelectionItem.FieldName, ADate1, ADate2]);
  end
  else
  begin
    ADate1 := FormatTheDate(Trunc(FCBDate1.Date));
    if Parent.Parent.SQLDialect <> sddtSearch then
    begin
      ADate2 := FormatTheDate(Trunc(FCBDate2.Date) + 1);
      Result := Format('((%s>=%s) AND (%s<%s))', [SelectionItem.FieldName, ADate1, SelectionItem.FieldName, ADate2]);
    end
    else
    begin
      ADate2 := FormatTheDate(Trunc(FCBDate2.Date));
      Result := Format('(%s contains %s~~%s)', [SelectionItem.FieldName, ADate1, ADate2]);
    end;
  end;
  DoSQLEvent(SelectionItem.FieldName, 'between', ADate1, ADate2, Result);
end;

procedure TDateTimeFieldPanel.OnRBClick(Sender: TObject);
begin
  FEditMois.Enabled := FRBMois.Checked;
  FEditJours.Enabled := FRBJours.Checked;
  FCBDate1.Enabled := FRBEntre.Checked;
  FCBDate2.Enabled := FRBEntre.Checked;
  NotifyChange;
end;

procedure TDateTimeFieldPanel.OnLabelEntreClick(Sender: TObject);
begin
  FRBEntre.Checked := True;
end;

procedure TDateTimeFieldPanel.LoadFromStream(AStream: TStream);
begin
  case TUsefulStream(AStream).ReadChar of
  'E' : FRBEntre.Checked := True;
  'J' : FRBJours.Checked := True;
  'M' : FRBMois.Checked := True;
  else
    FRBEntre.Checked := True;
  end;

  FEditMois.AsInteger := TUsefulStream(AStream).ReadLongInt;
  FEditJours.AsInteger := TUsefulStream(AStream).ReadLongInt;

  FCBDate1.Date := TUsefulStream(AStream).ReadDateTime;
  FCBDate2.Date := TUsefulStream(AStream).ReadDateTime;
end;

procedure TDateTimeFieldPanel.SaveToStream(AStream: TStream);
begin
  if FRBMois.Checked then
    TUsefulStream(AStream).WriteChar('M')
  else
  if FRBJours.Checked then
    TUsefulStream(AStream).WriteChar('J')
  else
    TUsefulStream(AStream).WriteChar('E');


  TUsefulStream(AStream).WriteLongInt(FEditMois.AsInteger);
  TUsefulStream(AStream).WriteLongInt(FEditJours.AsInteger);

  TUsefulStream(AStream).WriteDateTime(FCBDate1.Date);
  TUsefulStream(AStream).WriteDateTime(FCBDate2.Date);
end;

{ TIntegerFieldPanel }

procedure TIntegerFieldPanel.CreateControls;
begin
  inherited;

  FCBOperators := TComboBox.Create(Self);
  FCBOperators.Parent:= Self;
  FCBOperators.Top := 1;
  FCBOperators.Left := 1;
  FCBOperators.Width:= 50;
  FCBOperators.Style := csDropDownList;
  FCBOperators.Anchors := [akLeft, akTop];
  FCBOperators.OnChange := OnSomethingChange;
  FCBOperators.HandleNeeded;
  if Parent.Parent.SQLDialect <> sddtSearch then
  begin
    FCBOperators.Items.Add('=');
    FCBOperators.Items.Add('<>');
    FCBOperators.Items.Add('>');
    FCBOperators.Items.Add('>=');
    FCBOperators.Items.Add('<');
    FCBOperators.Items.Add('<=');
  end
  else
  begin
    FCBOperators.Items.Add('=');
    FCBOperators.Items.Add('<>');
    FCBOperators.Items.Add('>');
    FCBOperators.Items.Add('<');
  end;
  FCBOperators.ItemIndex := 0;
  FCBOperators.Font := Parent.Font;

  FEditValue := TRxCalcEdit.Create(Self);
  FEditValue.Parent:= Self;
  FEditValue.Font := Parent.Font;
  FEditValue.Top := 1;
  FEditValue.Left := FCBOperators.Left + FCBOperators.Width + 2;
  FEditValue.Width:= Width - FEditValue.Left - 1;
  FEditValue.Anchors := [akLeft, akRight, akTop];
  FEditValue.DisplayFormat := '0';
  FEditValue.DecimalPlaces := 0;
  FEditValue.OnChange := OnEditValueChange;

  Height := FEditValue.Top + FEditValue.Height + 1;

end;

function TIntegerFieldPanel.GetSQL: string;
var
  AOp : string;
  AVal1, AVal2 : string;
begin
  if Parent.Parent.SQLDialect <> sddtSearch then
  begin
    Result := Format('(%s%s%s)', [SelectionItem.FieldName, FCBOperators.Items[FCBOperators.ItemIndex], IntToStr(FEditValue.AsInteger)]);
    AVal1 := IntToStr(FEditValue.AsInteger);
    AVal2 := AVal1;
  end
  else
  begin
    AOp := FCBOperators.Items[FCBOperators.ItemIndex];
    if AOp = '=' then
    begin
      Result := Format('(%s contains %s)', [SelectionItem.FieldName, IntToStr(FEditValue.AsInteger)]);
      AVal1 := IntToStr(FEditValue.AsInteger);
      AVal2 := AVal1;
    end
    else
    if AOp = '>' then
    begin
      Result := Format('(%s contains %s~~%s)', [SelectionItem.FieldName, IntToStr(FEditValue.AsInteger), IntToStr(High(Integer))]);
      AVal1 := IntToStr(FEditValue.AsInteger);
      AVal2 := IntToStr(High(Integer));
    end
    else
    if AOp = '<' then
    begin
      Result := Format('(%s contains %s~~%s)', [SelectionItem.FieldName, '0', IntToStr(FEditValue.AsInteger)]);
      AVal1 := '0';
      AVal2 := IntToStr(FEditValue.AsInteger);
    end
    else
    if AOp = '<>' then
    begin
      Result := Format('(not (%s contains %s))', [SelectionItem.FieldName, IntToStr(FEditValue.AsInteger)]);
      AVal1 := IntToStr(FEditValue.AsInteger);
      AVal2 := AVal1;
    end;
  end;
  DoSQLEvent(SelectionItem.FieldName, FCBOperators.Items[FCBOperators.ItemIndex], AVal1, AVal2, Result);
end;

procedure TIntegerFieldPanel.LoadFromStream(AStream: TStream);
begin
  FCBOperators.ItemIndex := TUsefulStream(AStream).ReadLongInt;
  FEditValue.AsInteger := TUsefulStream(AStream).ReadLongInt;
end;

procedure TIntegerFieldPanel.OnEditValueChange(Sender: TObject);
begin
  if Frac(FEditValue.Value) <> 0 then
    FEditValue.AsInteger := FEditValue.AsInteger;
  NotifyChange;
end;


procedure TIntegerFieldPanel.SaveToStream(AStream: TStream);
begin
  TUsefulStream(AStream).WriteLongInt(FCBOperators.ItemIndex);
  TUsefulStream(AStream).WriteLongInt(FEditValue.AsInteger);
end;

{ TFilterComboBox }

procedure TFilterComboBox.CreateWnd;
begin
  inherited;
  SendMessage(Handle, CB_SETDROPPEDWIDTH, 200, 0);
end;

end.

