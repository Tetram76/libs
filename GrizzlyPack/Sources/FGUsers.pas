unit FGUsers;

interface

{
Table : Users.DB
  Liste des champs
    ShortName                          	String         	  11
    LongName                           	String         	  50
    Password                           	String         	 100
    Level                              	Integer
    Try                                	Integer
  Liste des index
    Sans nom : ShortName 	Options : Primaire, Unique
Table : Rights.DB
  Liste des champs
    Element                            	String         	  30
    AccessKind                         	String         	   1
    RequiredLevel                      	Integer
    RequiredPassword                   	String         	 100
    AllowedUsers                       	Memo           	  30
  Liste des index
    Sans nom : Element 	Options : Primaire, Unique
}

uses
  SysUtils, Classes, DB, Controls, Dialogs;

type
  EFGUsersError = class(Exception);

  TEncryptDecryptFunction = function(const S: string; AKey: Word): string;

  TUsersManager = class(TComponent)
  private
    { Déclarations privées }
    FCurrentLongName,
      FCurrentShortName: string;
    FCurrentLevel: Integer;
    FDataset: TDataset;
    FFieldShortName,
      FFieldLongName,
      FFieldLevel,
      FFieldRetryCount,
      FFieldPassword: string;
    FSupervisorName: string;
    FShowRetryCount: Boolean;
    FShowFailure: Boolean;
    FSupervisorEditOnly: Boolean;
    {}
    FAccessDataSet: TDataSet;
    FAccessFieldElem,
      FAccessFieldAccessKind,
      FAccessFieldPassword,
      FAccessFieldLevel,
      FAccessFieldUsers: string;
    {}
    FEditingUsers: Boolean;
    FEditingAccessRights: Boolean;
    procedure SetDataset(Value: TDataset);
    procedure SetAccessDataSet(DataSet: TDataSet);
    {}
    function GetUseRights: Boolean;
  protected
    { Déclarations protégées }
    LastMessage: string;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure VerifyFieldValidity;
    {}
    property EditingUsers: Boolean read FEditingUsers;
    property EditingAccessRights: Boolean read FEditingAccessRights;
  public
    { Déclarations publiques }
    EncryptFunction: TEncryptDecryptFunction;
    DecryptFunction: TEncryptDecryptFunction;
    procedure AddDefault;
    function QueryIdentification: Boolean;
    function QueryPassword: Boolean;
    function QuerySupervisorPassword: Boolean;
    function CanAccess(AAccessElem: string; AskUserId, NotifyUser: Boolean): Boolean;
    procedure EditUsers;
    procedure EditAccessRights(Design: Boolean);
    function IsGoodId(AName, APassword: string; UpdateUser: Boolean): Boolean;
    property CurrentLevel: Integer read FCurrentLevel write FCurrentLevel;
    property CurrentShortName: string read FCurrentShortName write FCurrentShortName;
    property CurrentLongName: string read FCurrentLongName write FCurrentLongName;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure NotifyAccessDenied(UserName, Element: string);
    {}
    property UseAccessRights: Boolean read GetUseRights;
  published
    { Déclarations publiées }
    property SupervisorName: string read FSupervisorName write FSupervisorName;
    property Dataset: TDataset read FDataset write SetDataset;
    property FieldShortName: string read FFieldShortName write FFieldShortName;
    property FieldLongName: string read FFieldLongName write FFieldLongName;
    property FieldLevel: string read FFieldLevel write FFieldLevel;
    property FieldRetryCount: string read FFieldRetryCount write FFieldRetryCount;
    property FieldPassword: string read FFieldPassword write FFieldPassword;
    property ShowRetryCount: Boolean read FShowRetryCount write FShowRetryCount;
    property ShowFailure: Boolean read FShowFailure write FShowFailure;
    {}
    property SupervisorEditOnly: Boolean read FSupervisorEditOnly write FSupervisorEditOnly default True;
    {}
    property AccessDataSet: TDataSet read FAccessDataSet write SetAccessDataSet;
    property AccessFieldElem: string read FAccessFieldElem write FAccessFieldElem;
    property AccessFieldAccessKind: string read FAccessFieldAccessKind write FAccessFieldAccessKind;
    property AccessFieldLevel: string read FAccessFieldLevel write FAccessFieldLevel;
    property AccessFieldPassword: string read FAccessFieldPassword write FAccessFieldPassword;
    property AccessFieldUsers: string read FAccessFieldUsers write FAccessFieldUsers;
  end;

var
  HelpContextEditUsers: Integer;
  HelpContextIdentify: Integer;
  HelpContextModifyPassword: Integer;
  HelpContextEditAccessRights: Integer;
  HelpContextEditUsersRights: Integer;

implementation

uses UFGUsers, FFGMDP, Encrypt, FFGEdUse, FEditElements, ADB, DBCtrls,
  DBNavBtn;

procedure TUsersManager.AddDefault;
var
  AOuvert: Boolean;
begin
  if not Dataset.Active then
  begin
    Dataset.Open;
    AOuvert:= True;
  end
  else
    AOuvert:= False;
  if not Dataset.Locate(FieldShortName, SupervisorName, []) then
  begin
    Dataset.AppendRecord([SupervisorName, SupervisorName, '22366=7990133<:6=:;252<<689997163', 4, 0]);
  end;
  if AOuvert then
    Dataset.Close;
end;

function TUsersManager.IsGoodId(AName, APassword: string; UpdateUser: Boolean): Boolean;
var
  AOuvert: Boolean;
  ThePo: Word;
begin
  if Dataset = nil then
  begin
    raise EFGUsersError.Create(msgNoDatasetDefined);
  end
  else
  begin
    if APassword = '' then
    begin
      Result:= False;
      LastMessage:= msgBadPasswordEmptyString;
      Exit;
    end;
    if not Dataset.Active then
    begin
      Dataset.Open;
      AOuvert:= True;
    end
    else
      AOuvert:= False;
    try
      AName:= UpperCase(AName);
      if Dataset.Locate(FieldShortName, AName, []) then
      begin
        if Dataset.FieldByName(FieldShortName).AsString = SupervisorName then
          ThePo:= OtherPo
        else
          ThePo:= CommonPo;
        Result:= (EncryptFunction(APassword, ThePo) = Dataset.FieldByName(FieldPassword).AsString);
        if Result then
        begin
          LastMessage:= msgGoodPassword;
          if UpdateUser then
          begin
            CurrentShortName:= Dataset.FieldByName(FieldShortName).AsString;
            CurrentLongName:= Dataset.FieldByName(FieldLongName).AsString;
            if CurrentLongName = '' then
              CurrentLongName:= CurrentShortName;
            CurrentLevel:= Dataset.FieldByName(FieldLevel).AsInteger;
          end;
          if Dataset.FieldByName(FieldRetryCount).AsInteger > 0 then
          begin
            if ShowRetryCount then
              MessageDlg(Format(msgTriesSinceLastGood, [Dataset.FieldByName(FieldRetryCount).AsString]),
                mtWarning, [mbOk], 0);
            Dataset.Edit;
            Dataset.FieldByName(FieldRetryCount).AsInteger:= 0;
            Dataset.Post;
          end;
        end
        else
        begin
          LastMessage:= msgBadPassword;
          Dataset.Edit;
          Dataset.FieldByName(FieldRetryCount).AsInteger:= Dataset.FieldByName(FieldRetryCount).AsInteger + 1;
          Dataset.Post;
        end;
      end
      else
      begin
        LastMessage:= msgUnknownUser;
        Result:= False;
      end;
    finally
      if AOuvert then
        Dataset.Close;
    end;
  end;
end;

procedure TUsersManager.VerifyFieldValidity;
begin
  if (Dataset = nil) then
    Exit;
  {Si une ligne lance une exception, c'est ce qu'il faut}
  if (Dataset.FindField(FieldShortName) = nil) then
    raise EFGUsersError.Create('Field name error : FieldShortName');
  if (Dataset.FindField(FieldLongName) = nil) then
    raise EFGUsersError.Create('Field name error : FieldLongName');
  if (Dataset.FindField(FieldPassword) = nil) then
    raise EFGUsersError.Create('Field name error : FieldPassword');
  if (Dataset.FindField(FieldRetryCount) = nil) then
    raise EFGUsersError.Create('Field name error : FieldRetryCount');
  if (Dataset.FindField(FieldLevel) = nil) then
    raise EFGUsersError.Create('Field name error : FieldLevel');
  if not UseAccessRights then Exit;
  if AccessDataSet.FindField(AccessFieldElem) = nil then
    raise EFGUsersError.Create('Field name error : AccessFieldElem');
  if AccessDataSet.FindField(AccessFieldAccessKind) = nil then
    raise EFGUsersError.Create('Field name error : AccessFieldAccessKind');
  if AccessDataSet.FindField(AccessFieldLevel) = nil then
    raise EFGUsersError.Create('Field name error : AccessFieldLevel');
  if AccessDataSet.FindField(AccessFieldPassword) = nil then
    raise EFGUsersError.Create('Field name error : AccessFieldPassword');
  if AccessDataSet.FindField(AccessFieldUsers) = nil then
    raise EFGUsersError.Create('Field name error : AccessFieldUsers');
end;

function TUsersManager.QueryIdentification: Boolean;
var
  ADlg: TDlgIdentification;
begin
  Result:= False;
  if Dataset = nil then
  begin
    raise EFGUsersError.Create(msgNoDatasetDefined);
  end
  else
  begin
    VerifyFieldValidity;
    ADlg:= TDlgIdentification.Create(Self);
    try
      ADlg.HelpContext:= HelpContextIdentify;
      ADlg.Caption:= msgIdentification;
      ADlg.EditOperateur.Text:= '';
      ADlg.EditOperateur.Enabled:= True;
      if ADlg.ShowModal = mrOk then
      begin
        if IsGoodId(ADlg.EditOperateur.Text, ADlg.EditPasse.Text, True) then
          Result:= True
        else
        begin
          Result:= False;
          if ShowFailure then
            MessageDlg(LastMessage, mtWarning, [mbOk], 0);
        end;
      end;
    finally
      ADlg.Free;
    end;
  end;
end;

function TUsersManager.QueryPassword: Boolean;
var
  ADlg: TDlgIdentification;
begin
  Result:= False;
  if Dataset = nil then
  begin
    raise EFGUsersError.Create(msgNoDatasetDefined);
  end
  else
  begin
    VerifyFieldValidity;
    ADlg:= TDlgIdentification.Create(Self);
    try
      ADlg.HelpContext:= HelpContextIdentify;
      ADlg.Caption:= msgPassword;
      ADlg.EditOperateur.Text:= CurrentShortName;
      ADlg.EditOperateur.Enabled:= False;
      ADlg.ActiveControl:= ADlg.EditPasse;
      if ADlg.ShowModal = mrOk then
      begin
        if IsGoodId(ADlg.EditOperateur.Text, ADlg.EditPasse.Text, True) then
          Result:= True
        else
        begin
          Result:= False;
          if ShowFailure then
            MessageDlg(LastMessage, mtWarning, [mbOk], 0);
        end;
      end;
    finally
      ADlg.Free;
    end;
  end;
end;

function TUsersManager.QuerySupervisorPassword: Boolean;
var
  ADlg: TDlgIdentification;
begin
  Result:= False;
  if Dataset = nil then
  begin
    raise EFGUsersError.Create(msgNoDatasetDefined);
  end
  else
  begin
    VerifyFieldValidity;
    ADlg:= TDlgIdentification.Create(Self);
    try
      ADlg.HelpContext:= HelpContextIdentify;
      ADlg.Caption:= msgSupervisorPassword;
      ADlg.EditOperateur.Text:= SupervisorName;
      ADlg.EditOperateur.Enabled:= False;
      ADlg.ActiveControl:= ADlg.EditPasse;
      if ADlg.ShowModal = mrOk then
      begin
        if IsGoodId(ADlg.EditOperateur.Text, ADlg.EditPasse.Text, False) then
          Result:= True
        else
        begin
          Result:= False;
          if ShowFailure then
            MessageDlg(LastMessage, mtWarning, [mbOk], 0);
        end;
      end;
    finally
      ADlg.Free;
    end;
  end;
end;

procedure TUsersManager.EditUsers;
var
  ADlg: TFenEditUsers;
  AOuvert: Boolean;
begin
  if Dataset = nil then
  begin
    raise EFGUsersError.Create(msgNoDatasetDefined);
  end
  else
  begin
    VerifyFieldValidity;
    if FSupervisorEditOnly or FEditingAccessRights or QuerySupervisorPassword then
    begin
      ADlg:= TFenEditUsers.Create(Self);
      try
        ADlg.HelpContext:= HelpContextEditUsers;
        Dataset.FieldByName(FieldPassword).Visible:= False;
        ADlg.FindPanel1.Dataset:= Dataset;
        ADlg.FindPanel1.SearchField:= FieldShortName;
        ADlg.DBShortName.DataField:= FieldShortName;
        ADlg.DBLongName.DataField:= FieldLongName;
        ADlg.DBLevel.DataField:= FieldLevel;
        ADlg.IdSuperviseur:= SupervisorName;
        ADlg.FieldShortName:= Dataset.FieldByName(FieldShortName);
        ADlg.FieldLongName:= Dataset.FieldByName(FieldLongName);
        ADlg.FieldLevel:= Dataset.FieldByName(FieldLevel);
        ADlg.FieldPassword:= Dataset.FieldByName(FieldPassword);
        ADlg.FieldRetryCount:= Dataset.FieldByName(FieldRetryCount);
        ADlg.UManager:= Self;
        ADlg.Dataset:= Dataset;
        ADlg.DSUsers.Dataset:= Dataset;
        ADlg.btnDroitsAcces.Visible:= UseAccessRights and (not FEditingAccessRights);
        FEditingUsers:= True;
        if not Dataset.Active then
        begin
          Dataset.Open;
          AOuvert:= True;
        end
        else
          AOuvert:= False;
        try
          ADlg.ShowModal;
        finally
          if AOuvert then
            Dataset.Close;
        end;
      finally
        FEditingUsers:= False;
        ADlg.Free;
      end;
    end;
  end;
end;

procedure TUsersManager.NotifyAccessDenied(UserName, Element: string);
begin
  MessageDlg(Format(msgAccessDenied, [Element]), mtError, [mbOk], 0);
end;

function TUsersManager.CanAccess(AAccessElem: string; AskUserId, NotifyUser: Boolean): Boolean;
var
  AccessKind: string;
  L: TStringList;
  ADlg: TDlgIdentification;
  OldActive1, OldActive2: Boolean;
begin
  if Dataset = nil then
    raise EFGUsersError.Create(msgNoDatasetDefined)
  else if not UseAccessRights then
    raise EFGUsersError.Create(msgNoAccessDatasetDefined)
  else
  begin
    OldActive1:= DataSet.Active;
    DataSet.Active:= True;
    try
      OldActive2:= AccessDataSet.Active;
      AccessDataSet.Active:= True;
      try
        VerifyFieldValidity;
        Result:= True;
        if CompareText(FCurrentShortName, FSupervisorName) = 0 then
          Exit;
        if not AccessDataSet.Locate(AccessFieldElem, AAccessElem, [loCaseInsensitive]) then
          raise EFGUsersError.Create(Format(msgAccessElemDoesNotExist, [AAccessElem]))
        else
        begin
          AccessKind:= AccessDataSet.FieldByName(AccessFieldAccessKind).AsString;
          if Result and (AccessKind <> msgShortUser) then
            Result:= CurrentLevel >= AccessDataSet.FieldByName(AccessFieldLevel).AsInteger;
          if Result and (AccessKind <> msgShortLevel) then
          begin
            L:= TStringList.Create;
            try
              L.Assign(AccessDataSet.FieldByName(AccessFieldUsers));
              Result:= L.IndexOf(CurrentShortName) <> -1;
            finally
              L.Free;
            end;
          end;
        end;
        if Result and AskUserId then
          Result:= QueryPassword;
        if Result and (AccessDataSet.FieldByName(AccessFieldPassword).AsString <> '') then
        begin
          ADlg:= TDlgIdentification.Create(Self);
          try
            with ADlg do
            begin
              HelpContext:= HelpContextIdentify;
              Caption:= msgEnterElement;
              Label1.Caption:= msgElementName;
              Label1.Left:= Label2.Left + Label2.Width - Label1.Width; { Aligner Label1 avec Label2 }
              EditOperateur.Text:= AAccessElem;
              EditOperateur.Enabled:= False;
              ActiveControl:= EditPasse;
              Result:= ShowModal = mrOk;
              if Result then
                Result:= EditPasse.Text = DecryptFunction(AccessDataSet.FieldByName(AccessFieldPassword).AsString, CommonPo);
            end;
          finally
            ADlg.Free;
          end;
        end;
        if (not Result) and NotifyUser then
          NotifyAccessDenied(FCurrentLongName, AAccessElem);
      finally
        AccessDataSet.Active:= OldActive2;
      end;
    finally
      DataSet.Active:= OldActive1;
    end;
  end;
end;

procedure TUsersManager.EditAccessRights(Design: Boolean);
var
  F: TFenEditElements;
  OldActive: Boolean;
begin
  if Dataset = nil then
    raise EFGUsersError.Create(msgNoDatasetDefined)
  else if not UseAccessRights then
    raise EFGUsersError.Create(msgNoAccessDatasetDefined)
  else if FSupervisorEditOnly or FEditingUsers or QuerySupervisorPassword then
  begin
    VerifyFieldValidity;
    F:= TFenEditElements.Create(nil);
    FEditingAccessRights:= True;
    try
      F.UserManager:= Self;
      F.EditElement.ReadOnly:= not Design;
      F.btnUsersFile.Enabled:= not FEditingUsers;
      if Design then
      begin
        F.DBNavigator1.VisibleButtons:= F.DBNavigator1.VisibleButtons + [nbDelete];
        F.DBNavBarre1.VisibleBtn:= F.DBNavBarre1.VisibleBtn + [navInsert];
      end;
      F.HelpContext:= HelpContextEditAccessRights;
      OldActive:= AccessDataSet.Active;
      try
        if not OldActive then
          AccessDataSet.Open;
        F.ShowModal;
      finally
        AccessDataSet.Active:= OldActive;
      end;
    finally
      FEditingAccessRights:= False;
      F.Free;
    end;
  end;
end;

procedure TUsersManager.SetDataset(Value: TDataset);
begin
  FDataset:= Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TUsersManager.SetAccessDataSet(DataSet: TDataSet);
begin
  FAccessDataSet:= DataSet;
  if Assigned(DataSet) then
    DataSet.FreeNotification(Self);
end;

procedure TUsersManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent = DataSet then DataSet:= nil;
    if AComponent = AccessDataSet then AccessDataSet:= nil;
  end;
end;

function TUsersManager.GetUseRights: Boolean;
begin
  Result:= AccessDataSet <> nil;
end;

constructor TUsersManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EncryptFunction:= EncryptString;
  DecryptFunction:= DecryptString;
  FSupervisorName:= 'SUPERVISEUR';
  FSupervisorEditOnly:= True;
end;

destructor TUsersManager.Destroy;
begin
  inherited Destroy;
end;


end.

