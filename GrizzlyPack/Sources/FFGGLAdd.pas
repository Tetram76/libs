unit FFGGLAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TDlgGenListAddItem = class(TForm)
    EditItem: TEdit;
    LabelItem: TLabel;
    EditValue: TEdit;
    LabelValue: TLabel;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    LabelTitre: TLabel;
    LabelGroupName: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Procedure SetWithoutValue;
  end;

var
  DlgGenListAddItem: TDlgGenListAddItem;

implementation

{$R *.DFM}

Uses UFGGLMgr;

Procedure TDlgGenListAddItem.SetWithoutValue;
Begin
  LabelValue.Visible:=False;
  EditValue.Visible:=False;
  LabelItem.Top:=Bevel2.Top+(Bevel2.Height - EditItem.Height - LabelItem.Height - 2) div 2;
  EditItem.Top:=LabelItem.Top+LabelItem.Height+2;
End;

procedure TDlgGenListAddItem.BtnOkClick(Sender: TObject);
begin
  If EditItem.Text='' then
    raise Exception.Create(msgEmptyItem);
  ModalResult:=mrOk;
end;

procedure TDlgGenListAddItem.FormCreate(Sender: TObject);
begin
  LabelTitre.Caption := msgAddItemToGroup;
  LabelItem.Caption := msgNewItem;
  Caption := msgNewItem;
  LabelValue.Caption := msgItemValue;
  BtnOk.Caption := msgOk;
  BtnCancel.Caption := msgCancel;
end;

end.
