unit FFGGLEd;

{Fenêtre d'édition du composant GenListManager}

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

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls, DB,
  DBTables, Dbnavbtn, Dbinfo, DBFind, GZGlMgr;

type
  TFenGLMgrEdit = class(TForm)
    TableElements: TTable;
    DSElements: TDataSource;
    PanelGlobal: TPanel;
    PanelBoutons: TPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    PanelBoutonFermer: TPanel;
    FermerBtn: TBitBtn;
    PanelDBNav: TPanel;
    DBNavPrincipal: TDBNavigator;
    TableGroupes: TTable;
    DSGroupes: TDataSource;
    TableElementsGroupName: TStringField;
    TableElementsItem: TStringField;
    TableGroupesGroupName: TStringField;
    TableGroupesAllowAdd: TIntegerField;
    TableGroupesAllowDelete: TIntegerField;
    TableGroupesAllowValue: TIntegerField;
    TableElementsValue: TStringField;
    TableGroupesAllowOrder: TIntegerField;
    TableElementsOrder: TIntegerField;
    TableGroupesValueName: TStringField;
    TableGroupesValueUnit: TStringField;
    TableGroupesItemReadOnly: TIntegerField;
    PanelItems: TPanel;
    DBGElements: TDBGrid;
    FindPanelPrincipal: TFindPanel;
    PanelGroupes: TPanel;
    DBGGroupes: TDBGrid;
    PanelTitreGroupe: TPanel;
    DBRecordCount1: TDBRecordCount;
    DBInfoPrincipal: TDBRecordCount;
    Splitter1: TSplitter;
    procedure FermerBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TableElementsBeforeDelete(DataSet: TDataSet);
    procedure TableElementsBeforeInsert(DataSet: TDataSet);
    procedure DSGroupesDataChange(Sender: TObject; Field: TField);
    procedure TableElementsValueGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
  private
    { Déclarations private }
    FMgr: TCustomGenListManager;
    Function GetGroupeEnCours : String;
    Procedure SetGroupeEnCours(Value : String);
    Function GetAllowAdd : Boolean;
    Function GetAllowDelete : Boolean;
    Function GetAllowValue : Boolean;
    Function GetAllowOrder : Boolean;
  public
    { Déclarations public }
    Property AllowAdd : Boolean Read GetAllowAdd;
    Property AllowDelete : Boolean Read GetAllowDelete;
    Property AllowValue : Boolean Read GetAllowValue;
    Property AllowOrder : Boolean Read GetAllowOrder;
    Property Group : String Read GetGroupeEnCours Write SetGroupeEnCours;
    {}
    property GLMgr: TCustomGenListManager read FMgr write FMgr;
  end;

var
  FenGLMgrEdit: TFenGLMgrEdit;

implementation

{$R *.DFM}

Uses FGUtils, UFGGLMgr;

Function TFenGLMgrEdit.GetGroupeEnCours : String;
Begin
  Result:=TableGroupesGroupName.AsString;
End;

Procedure TFenGLMgrEdit.SetGroupeEnCours(Value : String);
Begin
  If Not TableGroupes.Locate('GroupName',Value,[]) then
    raise EGenericListError.Create(Format(msgGroupNotExist,[Value]));
End;

Function TFenGLMgrEdit.GetAllowAdd : Boolean;
Begin
  Result:=(TableGroupesAllowAdd.AsInteger<>0);
End;

Function TFenGLMgrEdit.GetAllowDelete : Boolean;
Begin
  Result:=(TableGroupesAllowDelete.AsInteger<>0);
End;

Function TFenGLMgrEdit.GetAllowValue : Boolean;
Begin
  Result:=(TableGroupesAllowValue.AsInteger<>0);
End;

Function TFenGLMgrEdit.GetAllowOrder : Boolean;
Begin
  Result:=(TableGroupesAllowOrder.AsInteger<>0);
End;

procedure TFenGLMgrEdit.FormCreate(Sender: TObject);
begin
  {Translations}
  TableGroupesGroupName.DisplayLabel := msgGroupName;
  TableElementsGroupName.DisplayLabel := msgGroupName;
  TableElementsOrder.DisplayLabel := msgOrder;
  TableElementsItem.DisplayLabel := msgItem;
  TableElementsValue.DisplayLabel := msgValue;
  FindPanelPrincipal.Caption := #32+msgSearchItem+#32;
  PanelTitreGroupe.Caption:=msgGroups;
  FermerBtn.Caption:=msgClose;
end;

procedure TFenGLMgrEdit.FormDestroy(Sender: TObject);
begin
	TableGroupes.Close;
	TableElements.Close;
end;

procedure TFenGLMgrEdit.FormShow(Sender: TObject);
var i: Integer;
begin
  for i:= 0 to 7 do
    TableGroupes.Fields[i].FieldName:= '$xxx$' + IntToStr(i);
  for i:= 0 to 3 do
    TableElements.Fields[i].FieldName:= '$xxx$' + IntToStr(i);
  TableGroupes.Fields[0].FieldName:= GLMgr.GroupFields.GroupName;
  TableGroupes.Fields[1].FieldName:= GLMgr.GroupFields.AllowAdd;
  TableGroupes.Fields[2].FieldName:= GLMgr.GroupFields.AllowDelete;
  TableGroupes.Fields[3].FieldName:= GLMgr.GroupFields.AllowValue;
  TableGroupes.Fields[4].FieldName:= GLMgr.GroupFields.AllowOrder;
  TableGroupes.Fields[5].FieldName:= GLMgr.GroupFields.ValueName;
  TableGroupes.Fields[6].FieldName:= GLMgr.GroupFields.ValueUnit;
  TableGroupes.Fields[7].FieldName:= GLMgr.GroupFields.ItemReadOnly;
  TableElements.Fields[0].FieldName:= GLMgr.ItemFields.GroupName;
  TableElements.Fields[1].FieldName:= GLMgr.ItemFields.Order;
  TableElements.Fields[2].FieldName:= GLMgr.ItemFields.Item;
  TableElements.Fields[3].FieldName:= GLMgr.ItemFields.Value;
	TableGroupes.Open;
	TableElements.Open;
end;

procedure TFenGLMgrEdit.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	If DBNavBarrePrincipal.Editing then
  Begin
  	MessageDlg(msgEndUpdateBeforeClose,mtInformation,[mbOk],0);
    Canclose:=False;
  End;
end;

procedure TFenGLMgrEdit.TableElementsBeforeInsert(DataSet: TDataSet);
begin
	If Group='' then
		raise EGenericListError.Create(msgGroupHasNoName);
	If Not AllowAdd then
		raise EGenericListError.Create(msgNotAllowedToAddItem);
end;

procedure TFenGLMgrEdit.TableElementsBeforeDelete(DataSet: TDataSet);
var B: Boolean;
begin
	If Not AllowDelete then
		raise EGenericListError.Create(msgNotAllowedToDeleteItem);
  if Assigned(GLMgr.BeforeDeleteItem) then
  begin
    B:= True;
    GLMgr.BeforeDeleteItem(Group, TableElementsItem.AsString, B);
    if not B then
      Abort;
  end;
end;

procedure TFenGLMgrEdit.FermerBtnClick(Sender: TObject);
begin
	Close;
end;


procedure TFenGLMgrEdit.DSGroupesDataChange(Sender: TObject;
  Field: TField);
begin
  TableElementsItem.ReadOnly:=(TableGroupesItemReadOnly.AsInteger=1);
  TableElementsValue.Visible:=AllowValue;
  TableElementsOrder.Visible:=AllowOrder;
  If AllowValue and (TableGroupesValueName.AsString<>'') then
  Begin
    TableElementsValue.DisplayLabel:=TableGroupesValueName.AsString;
  End
  Else
    TableElementsValue.DisplayLabel:=msgValue;
end;

procedure TFenGLMgrEdit.TableElementsValueGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  If DisplayText and (TableGroupesValueUnit.AsString<>'') then
    Text:=Format('%s %s',[TableElementsValue.AsString,TableGroupesValueUnit.AsString])
  Else
    Text:=TableElementsValue.AsString;
end;

end.
