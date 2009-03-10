unit FFGGLApp;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls, DB,
  Dbnavbtn, Dbinfo, DBFind, FGAliasD, DBTables, GzGLMgr;

type
	EGenericListError = Class(Exception);

  TGLFenAppGenericList = class(TForm)
    TableElements: TTable;
    DSElements: TDataSource;
    PanelGlobal: TPanel;
    DBGElements: TDBGrid;
    PanelBoutons: TPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    PanelBoutonFermer: TPanel;
    FermerBtn: TBitBtn;
    PanelDBNav: TPanel;
    DBNavPrincipal: TDBNavigator;
    DBInfoPrincipal: TDBRecordCount;
    FindPanelPrincipal: TFindPanel;
    TableGroupes: TTable;
    DSGroupes: TDataSource;
    DBGGroupes: TDBGrid;
    TableElementsGroupName: TStringField;
    TableElementsItem: TStringField;
    TableGroupesGroupName: TStringField;
    TableGroupesAllowAdd: TIntegerField;
    TableGroupesAllowDelete: TIntegerField;
    TableGroupesAllowValue: TIntegerField;
    TableElementsValue: TStringField;
    TableGroupesAllowOrder: TIntegerField;
    TableElementsOrder: TIntegerField;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BtnLien: TSpeedButton;
    DSLienGroupes: TDataSource;
    Button1: TButton;
    DatabaseNameDialog1: TDatabaseNameDialog;
    TableGroupesValueName: TStringField;
    TableGroupesValueUnit: TStringField;
    TableGroupesItemReadOnly: TIntegerField;
    LabelTables: TLabel;
    procedure FermerBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TableElementsBeforeInsert(DataSet: TDataSet);
    procedure DSGroupesDataChange(Sender: TObject; Field: TField);
    procedure BtnLienClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TableElementsValueGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure TableGroupesAllowAddGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
  private
    { Déclarations private }
    LargeurMin : Integer;
    FItemList : TStringList;
    FValueList : TStringList;
    Function GetGroupeEnCours : String;
    Procedure SetGroupeEnCours(Value : String);
    Function GetAllowAdd : Boolean;
    Function GetAllowDelete : Boolean;
    Function GetAllowValue : Boolean;
    Function GetAllowOrder : Boolean;
    Procedure WMGetMinMaxInfo(Var Msg : TWMGetMinMaxInfo); Message WM_GetMinMaxInfo;
  public
    { Déclarations public }
    procedure OuvrirAppli(AD, AG, AI : String; GroupFields: TGenGroupFields;
      ItemFields: TGenItemFields);
    Procedure ChoisirDatabase;
    Property AllowAdd : Boolean Read GetAllowAdd;
    Property AllowDelete : Boolean Read GetAllowDelete;
    Property AllowValue : Boolean Read GetAllowValue;
    Property AllowOrder : Boolean Read GetAllowOrder;
    Property Group : String Read GetGroupeEnCours Write SetGroupeEnCours;
  end;

var
  GLFenAppGenericList: TGLFenAppGenericList;

implementation

{$R *.DFM}

Uses FGUtils, FGLChTb;


Function TGLFenAppGenericList.GetGroupeEnCours : String;
Begin
  Result:=TableGroupesGroupName.AsString;
End;

Procedure TGLFenAppGenericList.SetGroupeEnCours(Value : String);
Begin
  If Not TableGroupes.Locate(TableGroupes.Fields[0].FieldName,Value,[]) then
    raise EGenericListError.Create('Le groupe "'+Value+'" n''existe pas');
End;

Function TGLFenAppGenericList.GetAllowAdd : Boolean;
Begin
  Result:=(TableGroupesAllowAdd.AsInteger<>0);
End;

Function TGLFenAppGenericList.GetAllowDelete : Boolean;
Begin
  Result:=(TableGroupesAllowDelete.AsInteger<>0);
End;

Function TGLFenAppGenericList.GetAllowValue : Boolean;
Begin
  Result:=(TableGroupesAllowValue.AsInteger<>0);
End;

Function TGLFenAppGenericList.GetAllowOrder : Boolean;
Begin
  Result:=(TableGroupesAllowOrder.AsInteger<>0);
End;

Procedure TGLFenAppGenericList.WMGetMinMaxInfo(Var Msg : TWMGetMinMaxInfo);
Begin
	If LargeurMin=0 then
  	LargeurMin:=Width;
	Msg.MinMaxInfo^.ptMinTrackSize.x:=LargeurMin;
End;

procedure TGLFenAppGenericList.OuvrirAppli(AD, AG, AI : String;
  GroupFields: TGenGroupFields; ItemFields: TGenItemFields);
//var i: Integer;
begin
//  for i:= 0 to 7 do
//    TableGroupes.Fields[i].FieldName:= IntToStr(i);
//  for i:= 0 to 3 do
//    TableElements.Fields[i].FieldName:= IntToStr(i);
  TableGroupes.Fields[0].FieldName:= GroupFields.GroupName;
  TableGroupes.Fields[1].FieldName:= GroupFields.AllowAdd;
  TableGroupes.Fields[2].FieldName:= GroupFields.AllowDelete;
  TableGroupes.Fields[3].FieldName:= GroupFields.AllowOrder;
  TableGroupes.Fields[4].FieldName:= GroupFields.AllowValue;
  TableGroupes.Fields[5].FieldName:= GroupFields.ItemReadOnly;
  TableGroupes.Fields[6].FieldName:= GroupFields.ValueName;
  TableGroupes.Fields[7].FieldName:= GroupFields.ValueUnit;
  TableElements.Fields[0].FieldName:= ItemFields.GroupName;
  TableElements.Fields[1].FieldName:= ItemFields.Order;
  TableElements.Fields[2].FieldName:= ItemFields.Item;
  TableElements.Fields[3].FieldName:= ItemFields.Value;
  if (AD<>'') and (AG<>'') and (AI<>'') then
  begin
    try
      DatabaseNameDialog1.DatabaseName:= AD;
      TableGroupes.DatabaseName:= AD;
      TableGroupes.TableName:= AG;
      TableElements.DatabaseName:= AD;
      TableElements.TableName:= AI;
      TableGroupes.Open;
      TableElements.Open;
      Caption:='Liste Génériques - '+DatabaseNameDialog1.DatabaseName;
    except
      on E: Exception do
      begin
        Application.ShowException(E);
        ChoisirDatabase;
      end;
    end;
  end else
    ChoisirDatabase;
end;

procedure TGLFenAppGenericList.FormCreate(Sender: TObject);
{$IFDEF APPGENLISTS}
var
  FD, FG, FI : String;
{$ENDIF}
begin
	LargeurMin:=Width;
  FItemList:=TStringList.Create;
  FValueList:=TStringList.Create;
  {$IFDEF APPGENLISTS}
  FD:='';
  FG:='';
  FI:='';
  If ParamCount>0 then
  begin
    FD:=ParamStr(1);
    If FD<>'' then
    begin
      DatabaseNameDialog1.DatabaseName:=FD;
      If ParamCount>1 then
      begin
        FG:=ParamStr(2);
        FI:=ParamStr(3);
      end;
    end;
  end;
  OuvrirAppli(FD, FG, FI);
  {$ENDIF}
end;

Procedure TGLFenAppGenericList.ChoisirDatabase;
Begin
  If DatabaseNameDialog1.Execute then
  Begin
    TableGroupes.Close;
    TableElements.Close;
    TableGroupes.DatabaseName:=DatabaseNameDialog1.DatabaseName;
    TableElements.DatabaseName:=DatabaseNameDialog1.DatabaseName;
    Try
      If DatabaseNameDialog1.AliasName='' then
        Caption:='Liste Génériques - '+DatabaseNameDialog1.DatabaseName
      Else
        Caption:='Liste Génériques - '+DatabaseNameDialog1.AliasName;
      LabelTables.Caption:='';
      GLDlgChoixTables:=TGLDlgChoixTables.Create(Self);
      Try
        Session.GetTableNames(TableGroupes.DatabaseName,'*.*',False,False,GLDlgChoixTables.LBGroups.Items);
        GLDlgChoixTables.LBGroups.ItemIndex:=GLDlgChoixTables.LBGroups.Items.IndexOf('GGroups');
        Session.GetTableNames(TableGroupes.DatabaseName,'*.*',False,False,GLDlgChoixTables.LBItems.Items);
        GLDlgChoixTables.LBItems.ItemIndex:=GLDlgChoixTables.LBItems.Items.IndexOf('GItems');
        If (GLDlgChoixTables.LBGroups.Items.Count>0) and
          (GLDlgChoixTables.LBItems.Items.Count>0) and
         (GLDlgChoixTables.ShowModal=mrOk) then
        begin
          With GLDlgChoixTables do
          begin
            If LBGroups.ItemIndex>=0 then
              TableGroupes.TableName:=LBGroups.Items[LBGroups.ItemIndex];
            If LBItems.ItemIndex>=0 then
              TableElements.TableName:=LBItems.Items[LBItems.ItemIndex];
          end;
        end;
      Finally
        GLDlgChoixTables.Free;
      End;
      LabelTables.Caption:='Groupes : ['+TableGroupes.TableName+'] Eléments : ['+TableElements.TableName+']';
      TableGroupes.Open;
      TableElements.Open;
    Except
      Caption:='Liste Génériques';
      LabelTables.Caption:='';
      raise;
    End;
  End;
End;

procedure TGLFenAppGenericList.FormDestroy(Sender: TObject);
begin
	TableGroupes.Close;
	TableElements.Close;
  ClearStringList(FItemList);
  FItemList.Free;
  FItemList:=Nil;
  ClearStringList(FValueList);
  FValueList.Free;
  FValueList:=Nil;
end;

procedure TGLFenAppGenericList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	If DBNavBarrePrincipal.Editing then
  Begin
  	MessageDlg('Terminez vos modifications avant de quitter',mtInformation,[mbOk],0);
    Canclose:=False;
  End;
end;

procedure TGLFenAppGenericList.TableElementsBeforeInsert(DataSet: TDataSet);
begin
	If Group='' then
		raise EGenericListError.Create('Le groupe n''a pas de nom');
end;

procedure TGLFenAppGenericList.FermerBtnClick(Sender: TObject);
begin
	Close;
end;


procedure TGLFenAppGenericList.DSGroupesDataChange(Sender: TObject;
  Field: TField);
begin
  If AllowValue and (TableGroupesValueName.AsString<>'') then
  Begin
    TableElementsValue.DisplayLabel:=TableGroupesValueName.AsString;
  End
  Else
    TableElementsValue.DisplayLabel:='Valeur';
end;

procedure TGLFenAppGenericList.BtnLienClick(Sender: TObject);
begin
  DSLienGroupes.Enabled:=Not BtnLien.Down;
  TableGroupes.Refresh;
  TableElements.Refresh;
end;

procedure TGLFenAppGenericList.Button1Click(Sender: TObject);
begin
  ChoisirDatabase;
end;


procedure TGLFenAppGenericList.TableElementsValueGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  If DisplayText and (TableGroupesValueUnit.AsString<>'') then
    Text:=Format('%s %s',[TableElementsValue.AsString,TableGroupesValueUnit.AsString])
  Else
    Text:=TableElementsValue.AsString;
end;

procedure TGLFenAppGenericList.TableGroupesAllowAddGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  If DisplayText then
  Begin
    If Sender.AsInteger=1 then
      Text:='Oui'
    Else
      Text:='Non';
  End
  Else
  Begin
    Text:=Sender.AsString;
  End;
end;

end.
