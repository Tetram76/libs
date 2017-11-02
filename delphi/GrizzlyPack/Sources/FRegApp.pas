unit FRegApp;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, ShwrMgr, Mask;

type
  TFenRegApp = class(TForm)
    Bevel1: TBevel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    EditCustomerName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditReleaseKey: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    EditLicenseNumber: TMaskEdit;
    EditRegistrationCode: TMaskEdit;
    EditMasterNumber: TMaskEdit;
    LabelMasterNumber: TLabel;
    procedure FormShow(Sender: TObject);
    procedure EditRegistrationCodeChange(Sender: TObject);
    procedure EditLicenseNumberChange(Sender: TObject);
    procedure EditMasterNumberChange(Sender: TObject);
    procedure EditCustomerNameChange(Sender: TObject);
  private
    procedure SetCustomerName(CustomerName: string);
    procedure SetLicenseNumber(LicenseNumber: string);
    procedure SetMasterNumber(MasterNumber: string);
    procedure SetRegCode(RegCode: string);
    function GetCustomerName: string;
    function GetLicenseNumber: string;
    function GetMasterNumber: string;
    function GetRegCode: string;
  protected
    procedure OnChange;
  public
    Manager: TSharewareManager;
    property CustomerName: string read GetCustomerName write SetCustomerName;
    property LicenseNumber: string read GetLicenseNumber write SetLicenseNumber;
    property MasterNumber: string read GetMasterNumber write SetMasterNumber;
    property RegCode: string read GetRegCode write SetRegCode;
  end;

implementation

uses AUtils;

{$R *.DFM}

procedure TFenRegApp.FormShow(Sender: TObject);
begin
  Caption:= Caption + Application.Title;
  EditReleaseKey.Text:= Manager.KeyToString(Manager.ReleaseKey);
  OnChange;
end;

procedure TFenRegApp.SetCustomerName(CustomerName: string);
begin
  EditCustomerName.Text:= CustomerName;
end;

procedure TFenRegApp.SetLicenseNumber(LicenseNumber: string);
var i: Integer;
begin
  for i:= Manager.LicenseLength div 4 - 1 downto 1 do
    Insert('-', LicenseNumber, i * 4 + 1);
  EditLicenseNumber.Text:= LicenseNumber;
end;

procedure TFenRegApp.SetMasterNumber(MasterNumber: string);
var i: Integer;
begin
  for i:= Manager.LicenseLength div 4 - 1 downto 1 do
    Insert('-', MasterNumber, i * 4 + 1);
  EditMasterNumber.Text:= MasterNumber;
end;

procedure TFenRegApp.SetRegCode(RegCode: string);
var i: Integer;
begin
  for i:= Manager.RegCodeLength div 4 - 1 downto 1 do
    Insert('-', RegCode, i * 4 + 1);
  EditRegistrationCode.Text:= RegCode;
end;

function TFenRegApp.GetCustomerName: string;
begin
  Result:= EditCustomerName.Text;
end;

function TFenRegApp.GetLicenseNumber: string;
var i: Integer;
begin
  Result:= EditLicenseNumber.Text;
  for i:= 1 to Manager.LicenseLength div 4 - 1 do
    Delete(Result, i * 4 + 1, 1);
end;

function TFenRegApp.GetMasterNumber: string;
var i: Integer;
begin
  if EditMasterNumber.Enabled then
  begin
    Result:= EditMasterNumber.Text;
    for i:= 1 to Manager.LicenseLength div 4 - 1 do
      Delete(Result, i * 4 + 1, 1);
  end else
    Result:= '';
end;

function TFenRegApp.GetRegCode: string;
var i: Integer;
begin
  Result:= EditRegistrationCode.Text;
  for i:= 1 to Manager.RegCodeLength div 4 - 1 do
    Delete(Result, i * 4 + 1, 1);
end;

procedure TFenRegApp.EditCustomerNameChange(Sender: TObject);
var S: string;
begin
  with Manager do
  begin
    S:= TEdit(Sender).Text;
    if Assigned(OnEditing) then OnEditing(Manager, seCustName, S);
    TEdit(Sender).Text:= S;
    OnChange;
  end;
end;

procedure TFenRegApp.EditLicenseNumberChange(Sender: TObject);
var S: string;
begin
  with Manager do
  begin
    S:= TEdit(Sender).Text;
    if Assigned(OnEditing) then OnEditing(Manager, seLicenseNo, S);
    TEdit(Sender).Text:= S;
    OnChange;
  end;
end;

procedure TFenRegApp.EditMasterNumberChange(Sender: TObject);
var S: string;
begin
  with Manager do
  begin
    S:= TEdit(Sender).Text;
    if Assigned(OnEditing) then OnEditing(Manager, seMasterNo, S);
    TEdit(Sender).Text:= S;
    OnChange;
  end;
end;

procedure TFenRegApp.EditRegistrationCodeChange(Sender: TObject);
var S: string;
begin
  with Manager do
  begin
    S:= TEdit(Sender).Text;
    if Assigned(OnEditing) then OnEditing(Manager, seRegKey, S);
    TEdit(Sender).Text:= S;
    OnChange;
  end;
end;

procedure TFenRegApp.OnChange;
var Enable: Boolean;
begin
  with Manager do
  begin
    Enable:= (EditCustomerName.Text <> '') and (Length(KillSpaces(EditLicenseNumber.Text)) = (LicenseLength + (LicenseLength div 4) - 1)) and
      (Length(KillSpaces(EditRegistrationCode.Text)) = (RegCodeLength + (RegCodeLength div 4) - 1));
    btnOk.Enabled:= Enable;
    EditMasterNumber.Enabled:= MasterLicense;
    LabelMasterNumber.Enabled:= MasterLicense;
  end;
end;

end.
