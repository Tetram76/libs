unit FFGMDP;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, DB, Dialogs;

type
  TDlgIdentification = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    EditOperateur: TEdit;
    Label1: TLabel;
    EditPasse: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure EditOperateurKeyPress(Sender: TObject; var Key: Char);
    procedure EditPasseKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgIdentification: TDlgIdentification;

implementation

{$R *.DFM}

Uses UFGUsers;

procedure TDlgIdentification.EditOperateurKeyPress(Sender: TObject;
  var Key: Char);
begin
	If Key=#13 then
  	EditPasse.SetFocus;
end;

procedure TDlgIdentification.EditPasseKeyPress(Sender: TObject; var Key: Char);
begin
	If Key=#13 then
		OkBtn.Click;
end;

procedure TDlgIdentification.FormCreate(Sender: TObject);
begin
  Label3.Caption:=Format(msgIdApplication,[Application.Title]);
  Label1.Caption:=msgUserShortName;
  Label2.Caption:=msgUserPassword;
  OKBtn.Caption:=msgOk;
  CancelBtn.Caption:=msgCancel;
end;

end.
