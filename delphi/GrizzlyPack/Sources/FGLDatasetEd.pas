unit FGLDatasetEd;

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
    GroupName   A 50 *
    Order       I
    Item        A 255
    Value       A 255

}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls, DB,
  DBTables, Dbnavbtn, Dbinfo, DBFind, GZGlMgr, HintCtrl;

type
  TFenGLMgrDatasetEdit = class(TForm)
    DSElements: TDataSource;
    PanelBoutons: TPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    PanelBoutonFermer: TPanel;
    FermerBtn: TBitBtn;
    DSGroupes: TDataSource;
    PanelGlobal: TPanel;
    Splitter1: TSplitter;
    PanelDBNav: TPanel;
    DBNavPrincipal: TDBNavigator;
    PanelItems: TPanel;
    DBGElements: THDBGrid;
    FindPanelPrincipal: TFindPanel;
    DBInfoPrincipal: TDBRecordCount;
    PanelGroupes: TPanel;
    DBGGroupes: THDBGrid;
    PanelTitreGroupe: TPanel;
    DBRecordCount1: TDBRecordCount;
    procedure FermerBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TableElementsBeforeDelete(DataSet: TDataSet);
    procedure TableElementsBeforeInsert(DataSet: TDataSet);
    procedure DSGroupesDataChange(Sender: TObject; Field: TField);
    procedure TableElementsValueGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure PanelGlobalResize(Sender: TObject);
  private
    { Déclarations private }
    FMgr: TCustomGenListManager;
    FGroupDataset: TDataset;
    FItemDataset: TDataset;
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
    property GroupDataset : TDataset Read FGroupDataset Write FGroupDataset;
    property ItemDataset : TDataset Read FItemDataset Write FItemDataset;
    {}
    property GLMgr: TCustomGenListManager read FMgr write FMgr;
  end;

var
  FenGLMgrDatasetEdit: TFenGLMgrDatasetEdit;

implementation

{$R *.DFM}

Uses FGUtils, UFGGLMgr;

Function TFenGLMgrDatasetEdit.GetGroupeEnCours : String;
Begin
  Result:=GroupDataset.FieldByName(GLMgr.GroupFields.GroupName).AsString;
End;

Procedure TFenGLMgrDatasetEdit.SetGroupeEnCours(Value : String);
Begin
  If Not GroupDataset.Locate('GroupName',Value,[]) then
    raise EGenericListError.Create(Format(msgGroupNotExist,[Value]));
End;

Function TFenGLMgrDatasetEdit.GetAllowAdd : Boolean;
Begin
  Result:= (GroupDataset.FieldByName(GLMgr.GroupFields.AllowAdd).AsInteger<>0);
End;

Function TFenGLMgrDatasetEdit.GetAllowDelete : Boolean;
Begin
  Result:=(GroupDataset.FieldByName(GLMgr.GroupFields.AllowDelete).AsInteger<>0);
End;

Function TFenGLMgrDatasetEdit.GetAllowValue : Boolean;
Begin
  Result:=(GroupDataset.FieldByName(GLMgr.GroupFields.AllowValue).AsInteger<>0);
End;

Function TFenGLMgrDatasetEdit.GetAllowOrder : Boolean;
Begin
  Result:=(GroupDataset.FieldByName(GLMgr.GroupFields.AllowOrder).AsInteger<>0);
End;

procedure TFenGLMgrDatasetEdit.FormCreate(Sender: TObject);
begin
  {Translations}
  FindPanelPrincipal.Caption := #32+msgSearchItem+#32;
  PanelTitreGroupe.Caption:=msgGroups;
  FermerBtn.Caption:=msgClose;
end;

procedure TFenGLMgrDatasetEdit.FormShow(Sender: TObject);
begin
  DSGroupes.DataSet := GroupDataset;
  DSElements.DataSet := ItemDataset;

  FindPanelPrincipal.DataSet := ItemDataset;
  FindPanelPrincipal.SearchField := GLMgr.ItemFields.Item;

  DBGGroupes.Columns[0].FieldName := GLMgr.GroupFields.GroupName;

  DBGElements.Columns[0].FieldName := GLMgr.ItemFields.Order;
  DBGElements.Columns[1].FieldName := GLMgr.ItemFields.Item;
  DBGElements.Columns[2].FieldName := GLMgr.ItemFields.Value;

  DBGGroupes.Columns[0].Title.Caption := msgGroupName;
  
  DBGElements.Columns[0].Title.Caption := msgOrder;
  DBGElements.Columns[1].Title.Caption := msgItem;
  DBGElements.Columns[2].Title.Caption := msgValue;

  if not GroupDataset.Active then
    GroupDataset.Open;
  if not ItemDataset.Active then
    ItemDataset.Open;

  DBGElements.Columns[0].Width := DBGElements.Columns[0].Width + 1; 
end;

procedure TFenGLMgrDatasetEdit.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	If DBNavBarrePrincipal.Editing then
  Begin
  	MessageDlg(msgEndUpdateBeforeClose,mtInformation,[mbOk],0);
    Canclose:=False;
  End;
end;

procedure TFenGLMgrDatasetEdit.TableElementsBeforeInsert(DataSet: TDataSet);
begin
	If Group='' then
		raise EGenericListError.Create(msgGroupHasNoName);
	If Not AllowAdd then
		raise EGenericListError.Create(msgNotAllowedToAddItem);
end;

procedure TFenGLMgrDatasetEdit.TableElementsBeforeDelete(DataSet: TDataSet);
var B: Boolean;
begin
	If Not AllowDelete then
		raise EGenericListError.Create(msgNotAllowedToDeleteItem);
  if Assigned(GLMgr.BeforeDeleteItem) then
  begin
    B:= True;
    GLMgr.BeforeDeleteItem(Group, ItemDataset.FieldByName(GLMgr.ItemFields.Item).AsString, B);
    if not B then
      Abort;
  end;
end;

procedure TFenGLMgrDatasetEdit.FermerBtnClick(Sender: TObject);
begin
	Close;
end;


procedure TFenGLMgrDatasetEdit.DSGroupesDataChange(Sender: TObject;
  Field: TField);
begin
  ItemDataset.FieldByName(GLMgr.ItemFields.Item).ReadOnly:=(GroupDataset.FieldByName(GLMgr.GroupFields.ItemReadOnly).AsInteger=1);
  DBGElements.Columns[2].Visible := AllowValue;
  ItemDataset.FieldByName(GLMgr.ItemFields.Value).Visible:=AllowValue;
  DBGElements.Columns[0].Visible := AllowOrder;
  ItemDataset.FieldByName(GLMgr.ItemFields.Order).Visible:=AllowOrder;
  If AllowValue and (GroupDataset.FieldByName(GLMgr.GroupFields.ValueName).AsString<>'') then
  Begin
    ItemDataset.FieldByName(GLMgr.ItemFields.Value).DisplayLabel:=GroupDataset.FieldByName(GLMgr.GroupFields.ValueName).AsString;
  End
  Else
    ItemDataset.FieldByName(GLMgr.ItemFields.Value).DisplayLabel:=msgValue;
end;

procedure TFenGLMgrDatasetEdit.TableElementsValueGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  If DisplayText and (GroupDataset.FieldByName(GLMgr.GroupFields.ValueUnit).AsString<>'') then
    Text:=Format('%s %s',[ItemDataset.FieldByName(GLMgr.ItemFields.Value).AsString, GroupDataset.FieldByName(GLMgr.GroupFields.ValueUnit).AsString])
  Else
    Text:=ItemDataset.FieldByName(GLMgr.ItemFields.Value).AsString;
end;

procedure TFenGLMgrDatasetEdit.PanelGlobalResize(Sender: TObject);
begin
  PanelGroupes.Width := (4 * PanelGlobal.Width) div 10;
end;

end.
