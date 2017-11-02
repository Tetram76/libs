unit GZRL_FSelModeleEtat;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls,
  DB, DBTables, Dbnavbtn, Dbinfo, DBFind, ExitPnl, GZRL_Composant,
  GZRL_FRequetesLibres;

type
  TFenSelModeleEtat = class(TForm)
    DSPrincipal: TDataSource;
    PanelGlobal: TPanel;
    DBGridPrincipal: TDBGrid;
    PanelDBNavDBInfo: TPanel;
    DBNavPrincipal: TDBNavigator;
    DBInfoPrincipal: TDBRecordCount;
    FindPanelPrincipal: TFindPanel;
    ExitPanel1: TExitPanel;
    Panel1: TPanel;
    LabelCommentaires: TLabel;
    MemoCommentaires: TDBMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBGridPrincipalDblClick(Sender: TObject);
  private
    FDataSetModeles: TDataSetModeles;
    FModeEdition: TModeEditionModele;
    procedure SetModeEdition(const Value: TModeEditionModele);
  public
    property DataSetModeles: TDataSetModeles read FDataSetModeles write FDataSetModeles;
    {}
    property ModeEdition: TModeEditionModele read FModeEdition write SetModeEdition;
  end;

var
  FenSelModeleEtat: TFenSelModeleEtat;

implementation

{$R *.DFM}

procedure TFenSelModeleEtat.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth:= Width;
  Constraints.MaxWidth:= Width;
end;

procedure TFenSelModeleEtat.FormShow(Sender: TObject);
begin
  with DataSetModeles do
  begin
    DBGridPrincipal.Columns[0].FieldName:= ChampNom;
    DBGridPrincipal.Columns[1].FieldName:= ChampDateCreation;
    DBGridPrincipal.Columns[2].FieldName:= ChampDateModif;
    MemoCommentaires.DataField:= ChampCommentaires;
    DSPrincipal.DataSet:= DataSet;
    FindPanelPrincipal.SearchField:= ChampNom;
    FindPanelPrincipal.DataSet:= DataSet;
    DataSet.Open;
  end;
end;

procedure TFenSelModeleEtat.SetModeEdition(const Value: TModeEditionModele);
begin
  FModeEdition:= Value;
  case Value of
    emModele:
    begin
      Caption:= 'Sélection d''une édition';
      LabelCommentaires.Caption:= 'Commentaires pour l''édition sélectionnée';
    end;
    emSnapShot:
    begin
      Caption:= 'Sélection d''une image des données';
      LabelCommentaires.Caption:= 'Commentaires pour l''image sélectionnée';
    end;
  end;
end;

procedure TFenSelModeleEtat.DBGridPrincipalDblClick(Sender: TObject);
begin
  ExitPanel1.BtnOk.Btn.Click;
end;

end.

