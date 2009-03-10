unit FEditElements;

{$I GrizzlyDefine.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, DBCtrls, DBGrids, Hintctrl, Grids, Buttons,
  DBNavBtn, DBFind, DBInfo, ExtCtrls, Db, FGUsers
  {$IFDEF GZ_D6}, Variants{$ENDIF};

type
  TFenEditElements = class(TForm)
    DSAccess: TDataSource;
    Panel1: TPanel;
    PanelPages: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    DBNavigator1: TDBNavigator;
    DBInformation1: TDBRecordCount;
    FindPanel1: TFindPanel;
    PanelBoutons: TPanel;
    DBNavBarre1: TDBNavBarre;
    Panel2: TPanel;
    FermerBtn: TBitBtn;
    GridElem: THDBGrid;
    LabelElemName: TLabel;
    EditElement: TDBEdit;
    LabelReqAccessRight: TLabel;
    LabelReqPassword: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnCrossEdit: TBitBtn;
    AccessKindSelecter: TDBRadioGroup;
    Bevel3: TBevel;
    btnChangePassword: TBitBtn;
    Bevel4: TBevel;
    MemoUsers: TDBMemo;
    RadioButtonYes: TRadioButton;
    RadioButtonNo: TRadioButton;
    btnUsersFile: TBitBtn;
    EditLevel: TDBComboBox;
    procedure btnCrossEditClick(Sender: TObject);
    procedure FermerBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DSAccessDataChange(Sender: TObject; Field: TField);
    procedure btnChangePasswordClick(Sender: TObject);
    procedure RadioButtonYesClick(Sender: TObject);
    procedure btnUsersFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FUserManager: TUsersManager;
    OldNewRecord, OldBeforePost: TDataSetNotifyEvent;
    procedure SetUserManager(UM: TUsersManager);
    procedure AccessDataSetNewRecord(DataSet: TDataSet);
    procedure AccessDataSetBeforePost(DataSet: TDataSet);
  public
    property UserManager: TUsersManager read FUserManager write SetUserManager;
  end;

var
  FenEditElements: TFenEditElements;

implementation

uses FCrossEditAccessRights, FFGEdMDP, UFGUsers, Encrypt, AUtils;

{$R *.DFM}

procedure TFenEditElements.FormCreate(Sender: TObject);
begin
  Caption:= msgAccessManagement;
  AccessKindSelecter.Items[0]:= msgPerLevel;
  AccessKindSelecter.Values[0]:= msgShortLevel;
  AccessKindSelecter.Items[1]:= msgPerUser;
  AccessKindSelecter.Values[1]:= msgShortUser;
  AccessKindSelecter.Items[2]:= msgPerBoth;
  AccessKindSelecter.Values[2]:= msgShortBoth;
  LabelElemName.Caption:= msgShortElemName;
  LabelReqAccessRight.Caption:= msgRequiredAccessRight;
  LabelReqPassword.Caption:= msgRequiredPassword;
  btnChangePassword.Caption:= msgBtnModifyPassword;
  btnUsersFile.Caption:= msgShortUsersFile;
  btnCrossEdit.Caption:= msgUsersRights;
end;

procedure TFenEditElements.SetUserManager(UM: TUsersManager);
  procedure SetFieldWidth(FieldName: string; Width: Integer);
  var i: Integer;
      C: TColumn;
  begin
    C:= nil;
    for i:= 0 to GridElem.Columns.Count - 1 do
      if FieldName = GridElem.Columns[i].FieldName then
        C:= GridElem.Columns[i];
    if Assigned(C) then
      C.Width:= Width;
  end;
begin
  FUserManager:= UM;
  if Assigned(UM) then
  begin
    GridElem.Columns[0].FieldName:= UM.AccessFieldElem;
    GridElem.Columns[1].FieldName:= UM.AccessFieldAccessKind;
    GridElem.Columns[2].FieldName:= UM.AccessFieldLevel;
    GridElem.Columns[0].Width:= 200;
    GridElem.Columns[1].Width:= 35;
    GridElem.Columns[2].Width:= 50;
    GridElem.Columns[0].Title.Caption:= msgAccessFieldElem;
    GridElem.Columns[1].Title.Caption:= msgAccessFieldAccessKind;
    GridElem.Columns[2].Title.Caption:= msgAccessFieldLevel;
    DSAccess.DataSet:= UM.AccessDataSet;
    FindPanel1.DataSet:= UM.AccessDataSet;
    FindPanel1.SearchField:= UM.AccessFieldElem;
    EditElement.DataField:= UM.AccessFieldElem;
    AccessKindSelecter.DataField:= UM.AccessFieldAccessKind;
    EditLevel.DataField:= UM.AccessFieldLevel;
    MemoUsers.DataField:= UM.AccessFieldUsers;
    {}
    OldNewRecord:= UM.AccessDataSet.OnNewRecord;
    UM.AccessDataSet.OnNewRecord:= AccessDataSetNewRecord;
    OldBeforePost:= UM.AccessDataSet.BeforePost;
    UM.AccessDataSet.BeforePost:= AccessDataSetBeforePost;
  end;
end;

procedure TFenEditElements.btnCrossEditClick(Sender: TObject);
var OldDelay: Integer;
begin
  if (UserManager.AccessDataSet.Lookup(UserManager.AccessFieldAccessKind, 'U',
    UserManager.AccessFieldElem) = Null) and
    (UserManager.AccessDataSet.Lookup(UserManager.AccessFieldAccessKind, 'B',
    UserManager.AccessFieldElem) = Null) then
  begin
    MessageDlg(msgNoUserLimitedElement, mtError, [mbOk], 0);
    Exit;
  end;
  FenCrossEditAccessRights:= TFenCrossEditAccessRights.Create(Self);
  try
    FenCrossEditAccessRights.UserManager:= UserManager;
    OldDelay:= BaseShowHintDelay;
    BaseShowHintDelay:= 50;
  try
    FenCrossEditAccessRights.HelpContext:= HelpContextEditUsersRights;
    FenCrossEditAccessRights.ShowModal;
    finally
      BaseShowHintDelay:= OldDelay;
    end;
  finally
    FenCrossEditAccessRights.Free;
  end;
end;

procedure TFenEditElements.AccessDataSetNewRecord(DataSet: TDataSet);
begin
  with UserManager do
    AccessDataSet.FieldByName(AccessFieldAccessKind).AsString:= msgShortUser;
  if Assigned(OldNewRecord) then OldNewRecord(DataSet);
end;

procedure TFenEditElements.AccessDataSetBeforePost(DataSet: TDataSet);
var Kind: string;
begin
  with UserManager do
  begin
    Kind:= AccessDataSet.FieldByName(AccessFieldAccessKind).AsString;
    if (Kind <> msgShortUser) and (not Bounded(AccessDataSet.FieldByName(AccessFieldLevel).AsInteger, 1, 4)) then
    begin
      EditLevel.SetFocus;
      raise EFGUsersError.Create(msgAccessRightIncorrect);
    end;
    if Kind = msgShortUser then AccessDataSet.FieldByName(AccessFieldLevel).Clear;
    if Kind = msgShortLevel then AccessDataSet.FieldByName(AccessFieldUsers).Clear;
  end;
  if Assigned(OldBeforePost) then OldBeforePost(DataSet);
end;

procedure TFenEditElements.FormDestroy(Sender: TObject);
begin
  if Assigned(UserManager) and Assigned(UserManager.AccessDataSet) then
  begin
    UserManager.AccessDataSet.OnNewRecord:= OldNewRecord;
    UserManager.AccessDataSet.BeforePost:= OldBeforePost;
  end;
end;

procedure TFenEditElements.FermerBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFenEditElements.DSAccessDataChange(Sender: TObject;
  Field: TField);
begin
  if (DSAccess.State = dsBrowse) or ((Field <> nil) and (Field.FieldName = UserManager.AccessFieldPassword)) then
  begin
    if DSAccess.DataSet.IsEmpty then
    begin
      RadioButtonYes.Checked:= False;
      RadioButtonNo.Checked:= False;
    end else with UserManager do
    begin
      RadioButtonYes.Checked:= AccessDataSet.FieldByName(AccessFieldPassword).AsString <> '';
      RadioButtonNo.Checked:= not RadioButtonYes.Checked;
    end;
    btnChangePassword.Enabled:= RadioButtonYes.Checked;
  end;
end;

procedure TFenEditElements.btnChangePasswordClick(Sender: TObject);
var ADlg: TDlgModifierMotDePasse;
    PW: string;
begin
 	ADlg:= TDlgModifierMotDePasse.Create(Self);
  with ADlg, UserManager do
  try
    UManager:= UserManager;
    EditAncien.PasswordChar:= #0;
    EditAncien.Enabled:= False;
    EditAncien.Text:= DecryptFunction(AccessDataSet.FieldByName(AccessFieldPassword).AsString, CommonPo);
    ActiveControl:= EditNouveau;
  	if ShowModal = mrOk then
    begin
      if EditNouveau.Text = '' then
        PW:= ''
      else
        PW:= EncryptFunction(EditNouveau.Text, CommonPo);
  	  if not (AccessDataset.State in [dsEdit, dsInsert]) then
        AccessDataset.Edit;
	    AccessDataSet.FieldByName(AccessFieldPassword).AsString:= PW;
    end;
  finally
  	ADlg.Free;
  end;
end;

procedure TFenEditElements.RadioButtonYesClick(Sender: TObject);
begin
  btnChangePassword.Enabled:= RadioButtonYes.Checked;
end;

procedure TFenEditElements.btnUsersFileClick(Sender: TObject);
begin
  UserManager.EditUsers;
end;

end.
