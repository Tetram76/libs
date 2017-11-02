unit GZRL_FEditModeleEtat;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Mask, GZRL_Composant, GZRL_FRequetesLibres,
  Hintctrl;

type
  TFenEditModeleEtat = class(TForm)
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    LabelNom: TLabel;
    EditNom: THEdit;
    Label2: TLabel;
    MemoCommentaires: TMemo;
    Bevel1: TBevel;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDataSetModeles: TDataSetModeles;
    FModeEdition: TModeEditionModele;
    FMsgNomExistant: string;
    procedure SetModeEdition(const Value: TModeEditionModele);
  public
    property DataSetModeles: TDataSetModeles read FDataSetModeles write FDataSetModeles;
    {}
    property ModeEdition: TModeEditionModele read FModeEdition write SetModeEdition;
  end;

var
  FenEditModeleEtat: TFenEditModeleEtat;

implementation

{$R *.DFM}

procedure TFenEditModeleEtat.BtnOkClick(Sender: TObject);
begin
  with DataSetModeles do
  begin
    DataSet.Open;
    if DataSet.Locate(ChampNom, EditNom.Text, []) then
    begin
      if MessageDlg(FMsgNomExistant, mtWarning, [mbYes, mbNo], 0) = mrYes then
        DataSet.Edit
      else
        Exit;
    end else
      DataSet.Append;
  end;
  {}
  ModalResult:= mrOk;
end;

procedure TFenEditModeleEtat.FormShow(Sender: TObject);
begin
  with DataSetModeles do
  begin
    DataSet.Open;
    if DataSet.Locate(ChampNom, EditNom.Text, []) then
      MemoCommentaires.Lines.Text:= DataSet.FieldByName(ChampCommentaires).AsString;
  end;
end;

procedure TFenEditModeleEtat.BtnCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TFenEditModeleEtat.SetModeEdition(const Value: TModeEditionModele);
begin
  FModeEdition:= Value;
  case Value of
    emModele:
    begin
      Caption:= 'Enregistrer une édition';
      LabelNom.Caption:= 'Nom de l''édition';
      FMsgNomExistant:= 'Une édition porte déjà ce nom. Voulez-vous l''écraser ?';
    end;
    emSnapShot:
    begin
      Caption:= 'Enregistrer une image des données';
      LabelNom.Caption:= 'Nom de l''image';
      FMsgNomExistant:= 'Une image des données porte déjà ce nom. Voulez-vous l''écraser ?';
    end;
  end;
end;

end.
