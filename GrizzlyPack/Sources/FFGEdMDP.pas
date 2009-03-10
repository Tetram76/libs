unit FFGEdMDP;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, FGUsers;

type
  TDlgModifierMotDePasse = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    EditAncien: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditNouveau: TEdit;
    EditConfirmation: TEdit;
    Label3: TLabel;
    procedure EditAncienKeyPress(Sender: TObject; var Key: Char);
    procedure EditNouveauKeyPress(Sender: TObject; var Key: Char);
    procedure OKBtnClick(Sender: TObject);
    procedure EditConfirmationKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    UManager : TUsersManager;
    Function Accepter(Operateur, Old : String) : Boolean;
    Function MotDePasse(Operateur : String) : String;
  end;

var
  DlgModifierMotDePasse: TDlgModifierMotDePasse;

implementation

{$R *.DFM}

Uses UFGUsers, Encrypt;

Function TDlgModifierMotDePasse.MotDePasse(Operateur : String) : String;
Begin
	If Operateur=UManager.SupervisorName then
		Result:=UManager.EncryptFunction(EditNouveau.Text, OtherPo)
  Else
		Result:=UManager.EncryptFunction(EditNouveau.Text, CommonPo);
End;

Function TDlgModifierMotDePasse.Accepter(Operateur, Old : String) : Boolean;
Var
	LaCle : Word;
Begin
	Result:=False;
  If Operateur=UManager.SupervisorName then
  	LaCle:=OtherPO
  Else
  	LaCle:=CommonPO;
	If UManager.EncryptFunction(EditAncien.Text, LaCle)=Old then
  	Result:=True
  Else
  	ShowMessage(msgBadPassword);
End;

procedure TDlgModifierMotDePasse.EditAncienKeyPress(Sender: TObject;
  var Key: Char);
begin
	If Key=#13 then
  	EditNouveau.SetFocus;
end;

procedure TDlgModifierMotDePasse.EditNouveauKeyPress(Sender: TObject;
  var Key: Char);
begin
	If Key=#13 then
  	EditConfirmation.SetFocus;
end;

procedure TDlgModifierMotDePasse.OKBtnClick(Sender: TObject);
begin
  If EditNouveau.Text<>EditConfirmation.Text then
  Begin
    MessageDlg(msgBadConfirmation,mtWarning,[mbOk],0);
    EditNouveau.Text:='';
    EditConfirmation.Text:='';
    EditNouveau.SetFocus;
  End
  Else
    ModalResult:=mrOk;
end;

procedure TDlgModifierMotDePasse.EditConfirmationKeyPress(Sender: TObject;
  var Key: Char);
begin
	If Key=#13 then
		OkBtn.Click;
end;

procedure TDlgModifierMotDePasse.FormCreate(Sender: TObject);
begin
  Caption:=msgModifyPassword;
  Label1.Caption:=msgOldPassword;
  Label2.Caption:=msgNewPassword;
  Label3.Caption:=msgConfirmationPassword;
  OKBtn.Caption:=msgOk;
  CancelBtn.Caption:=msgCancel;
end;

end.
