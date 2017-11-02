unit FFGEdUse;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls, DB,
  Dbnavbtn, Dbinfo, DBFind, Mask, FGUsers, HintCtrl;


type
  TFenEditUsers = class(TForm)
    Panel1: TPanel;
    PanelBoutons: TPanel;
    DBNavBarre1: TDBNavBarre;
    Panel2: TPanel;
    FermerBtn: TBitBtn;
    PanelPages: TPanel;
    Panel4: TPanel;
    DBGrid1: THDBGrid;
    Panel5: TPanel;
    DBNavigator1: TDBNavigator;
    DBInformation1: TDBRecordCount;
    FindPanel1: TFindPanel;
    DSUsers: TDataSource;
    Label3: TLabel;
    BtnNiveau: TBitBtn;
    DBLongName: TDBEdit;
    Label1: TLabel;
    BtnChangePasse: TBitBtn;
    DBShortName: TDBEdit;
    Label2: TLabel;
    btnDroitsAcces: TBitBtn;
    DBLevel: TDBComboBox;
    procedure FermerBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure BtnChangePasseClick(Sender: TObject);
    procedure BtnNiveauClick(Sender: TObject);
    procedure DSUsersDataChange(Sender: TObject; Field: TField);
    procedure FormDestroy(Sender: TObject);
    procedure btnDroitsAccesClick(Sender: TObject);
  private
    FOldBeforePost, FOldBeforeDelete: TDataSetNotifyEvent;
    FUManager: TUsersManager;
    FCurrentShortName: string;
    procedure SetUManager(UManager: TUsersManager);
    procedure DoBeforePost(DataSet: TDataSet);
    procedure DoBeforeDelete(DataSet: TDataSet);
    procedure ChangeUserName(const OldName, NewName: string; Delete: Boolean);
  public
    { Déclarations public }
    idSuperviseur : String;
    FieldPassword,
    FieldShortName,
    FieldLongName,
    FieldRetryCount,
    FieldLevel : TField;
    Dataset : TDataset;
    property UManager: TUsersManager read FUManager write SetUManager;
  end;

var
  FenEditUsers: TFenEditUsers;

implementation

uses ADB, FFGEdMDP, UFGDBCtl, UFGUsers, FEditElements;

{$R *.DFM}

procedure TFenEditUsers.BtnChangePasseClick(Sender: TObject);
Var
	LaDlg : TDlgModifierMotDePasse;
begin
 	LaDlg:=TDlgModifierMotDePasse.Create(Self);
  Try
    LaDlg.HelpContext:= HelpContextModifyPassword;
    LaDlg.UManager:=UManager;
  	If (LaDlg.ShowModal=mrOk) then
    Begin
    	If (LaDlg.Accepter(FieldShortName.AsString,FieldPassword.AsString)) then
	    Begin
  	    If Not (Dataset.State in [dsEdit, dsInsert]) then
	      	Dataset.Edit;
	    	FieldPassword.AsString:=LaDlg.MotDePasse(FieldShortName.AsString);
        FieldRetryCount.AsInteger:=0;
	    End
      Else
      Begin
  	    If Not (Dataset.State in [dsEdit, dsInsert]) then
	      	Dataset.Edit;
        FieldRetryCount.AsInteger:=FieldRetryCount.AsInteger+1;
        Dataset.Post;
      End;
    End;
  Finally
  	LaDlg.Free;
  End;
end;

procedure TFenEditUsers.FermerBtnClick(Sender: TObject);
begin
	Close;
end;

procedure TFenEditUsers.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	If Dataset.State in [dsEdit,dsInsert] then
  Begin
  	MessageDlg(msgSaveBeforeExit,mtInformation,[mbOk],0);
    Canclose:=False;
  End;
end;

procedure TFenEditUsers.FormCreate(Sender: TObject);
begin
  idSuperviseur := 'SUPERVISEUR';
  Caption:=msgUsersFile;
  Label1.Caption:=msgLongName;
  Label2.Caption:=msgShortName;
  Label3.Caption:=msgUserLevel;
  BtnChangePasse.Caption:=msgChangePassword;
  BtnNiveau.Caption:=msgChangeLevel;
  FermerBtn.Caption:=msgClose;
end;

procedure TFenEditUsers.BtnNiveauClick(Sender: TObject);
begin
  If FieldShortName.AsString=IdSuperviseur then
  	raise Exception.Create(msgCantModifySupervisorLevel);
  DBLevel.Enabled:=True;
  If Not (Dataset.State in [dsEdit,dsInsert]) then
    Dataset.Edit;
  ActiveControl:=DBLevel;
end;

procedure TFenEditUsers.DSUsersDataChange(Sender: TObject;
  Field: TField);
begin
  DBShortName.ReadOnly:=FieldShortName.AsString=IdSuperviseur;
  If Not (Dataset.State in [dsEdit,dsInsert]) then
    DBLevel.Enabled:=False;
  if DataSet.State = dsBrowse then
    FCurrentShortName:= DataSet.FieldByName(UManager.FieldShortName).AsString;
end;

procedure TFenEditUsers.ChangeUserName(const OldName, NewName: string; Delete: Boolean);
var OldActive: Boolean;
    L: TStringList;
    Idx: Integer;
begin
  { Intégrité référentielle avec AccessDataSet ... }
  if Assigned(UManager.AccessDataSet) then
  with UManager do
  begin
    OldActive:= AccessDataSet.Active;
    AccessDataSet.Active:= True;
    try
      StoreDataSet(AccessDataSet);
      try
        AccessDataSet.First;
        L:= TStringList.Create;
        try
          while not AccessDataSet.EOF do
          begin
            L.Assign(AccessDataSet.FieldByName(AccessFieldUsers));
            Idx:= L.IndexOf(OldName);
            if Idx > -1 then
            begin
              if Delete then
                L.Delete(Idx)
              else
                L[Idx]:= NewName;
              AccessDataSet.Edit;
              AccessDataSet.FieldByName(AccessFieldUsers).Assign(L);
              AccessDataSet.Post;
            end;
            AccessDataSet.Next;
          end;
        finally
          L.Free;
        end;
      finally
        RestoreDataSet(AccessDataSet);
      end;
    finally
      AccessDataSet.Active:= OldActive;
    end;
  end;
end;

procedure TFenEditUsers.DoBeforeDelete(DataSet: TDataSet);
begin
  if Assigned(FOldBeforeDelete) then FOldBeforeDelete(DataSet);
  if FieldShortName.AsString=IdSuperviseur then
    raise EFGUsersError.Create(msgCantDeleteSupervisor);
  ChangeUserName(FCurrentShortName, '', True);
end;

procedure TFenEditUsers.DoBeforePost(DataSet: TDataSet);
begin
  if Assigned(FOldBeforePost) then FOldBeforePost(DataSet);
 	if FieldPassword.AsString = '' then
    raise EFGUsersError.Create(msgCantSaveWithoutPassword);
  DBLevel.Enabled:= False;
  if DataSet.State = dsInsert then Exit;
  { Vérification du nom de l'utilisateur }
  if (FCurrentShortName <> DataSet.FieldByName(UManager.FieldShortName).AsString) then
    ChangeUserName(FCurrentShortName, DataSet.FieldByName(UManager.FieldShortName).AsString, False);
end;

procedure TFenEditUsers.SetUManager(UManager: TUsersManager);
begin
  FUManager:= UManager;
  FOldBeforePost:= FUManager.Dataset.BeforePost;
  FUManager.DataSet.BeforePost:= DoBeforePost;
  FOldBeforeDelete:= FUManager.Dataset.BeforeDelete;
  FUManager.DataSet.BeforeDelete:= DoBeforeDelete;
end;

procedure TFenEditUsers.FormDestroy(Sender: TObject);
begin
  FUManager.Dataset.BeforePost:= FOldBeforePost;
  FUManager.Dataset.BeforeDelete:= FOldBeforeDelete;
end;

procedure TFenEditUsers.btnDroitsAccesClick(Sender: TObject);
begin
  FUManager.EditAccessRights(False);
end;

end.
