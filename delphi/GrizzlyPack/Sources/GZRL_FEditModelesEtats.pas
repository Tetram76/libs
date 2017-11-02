unit GZRL_FEditModelesEtats;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls, DB,
  DBTables, DBNavBtn, DBInfo, DBFind, Mask, GZRL_Composant, GZRL_FRequetesLibres;

type
  TFenEditModelesEtats = class(TForm)
    DSPrincipal: TDataSource;
    PanelGlobal: TPanel;
    PanelBoutons: TPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    PanelBoutonFermer: TPanel;
    FermerBtn: TBitBtn;
    PanelDonnees: TPanel;
    PanelDBGrid: TPanel;
    DBGridPrincipal: TDBGrid;
    PanelDBNav: TPanel;
    DBNavPrincipal: TDBNavigator;
    DBInfoPrincipal: TDBRecordCount;
    FindPanelPrincipal: TFindPanel;
    LabelNom: TLabel;
    DBEditNom: TDBEdit;
    Label2: TLabel;
    DBMemoCommentaires: TDBMemo;
    Label3: TLabel;
    DBEditDateCreation: TDBEdit;
    Label4: TLabel;
    DBEditDateModification: TDBEdit;
    procedure FermerBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBNavBarrePrincipalBeforeAction(Sender: TObject;
      Button: TNavButtons; var AllowAction: Boolean);
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
  FenEditModelesEtats: TFenEditModelesEtats;

implementation

{$R *.DFM}

procedure TFenEditModelesEtats.FermerBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFenEditModelesEtats.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if DBNavBarrePrincipal.Editing then
  begin
    MessageDlg('Terminez vos modifications avant de quitter', mtInformation, [mbOk], 0);
    Canclose:= False;
  end;
end;

procedure TFenEditModelesEtats.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth:= Width;
  Constraints.MaxWidth:= Width;
end;

procedure TFenEditModelesEtats.FormShow(Sender: TObject);
begin
  with DataSetModeles do
  begin
    DBGridPrincipal.Columns[0].FieldName:= ChampNom;
    DBEditNom.DataField:= ChampNom;
    DBMemoCommentaires.DataField:= ChampCommentaires;
    DBEditDateCreation.DataField:= ChampDateCreation;
    DBEditDateModification.DataField:= ChampDateModif;
    DSPrincipal.DataSet:= DataSet;
    FindPanelPrincipal.SearchField:= ChampNom;
    FindPanelPrincipal.DataSet:= DataSet;
    DataSet.Open;
  end;
end;

procedure TFenEditModelesEtats.DBNavBarrePrincipalBeforeAction(Sender: TObject;
  Button: TNavButtons; var AllowAction: Boolean);
begin
  if (Button = navEdit) and (DSPrincipal.DataSet.RecordCount = 0) then
    AllowAction:= False;
end;

procedure TFenEditModelesEtats.SetModeEdition(const Value: TModeEditionModele);
begin
  FModeEdition:= Value;
  case Value of
    emModele:
    begin
      Caption:= 'Edition des modèles d''édition';
      LabelNom.Caption:= 'Nom de l''édition';
    end;
    emSnapShot:
    begin
      Caption:= 'Edition des images des données';
      LabelNom.Caption:= 'Nom de l''image';
    end;
  end;
end;

end.

