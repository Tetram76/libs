unit FDBGridEtPanel;

interface

uses
  Windows, SysUtils, DB, StdCtrls, Buttons, DBNavBtn, DBFind, DBInfo, DBCtrls,
  Grids, DBGrids, Classes, Controls, ExtCtrls, HintCtrl;

type
  TFenDBGridPanel1 = class(TForm)
    DSPrincipal: TDataSource;
    PanelGlobal: TPanel;
    PanelBoutons: TPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    PanelBoutonFermer: TPanel;
    btnFermer: TBitBtn;
    PanelDonnees: TPanel;
    PanelDBGrid: TPanel;
    DBGridPrincipal: THDBGrid;
    PanelDBNav: TPanel;
    DBNavPrincipal: TDBNavigator;
    DBInfoPrincipal: TDBRecordCount;
    PanelHaut: TPanel;
    FindPanelPrincipal: TFindPanel;
    procedure btnFermerClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    {}
  public
    {}
  end;

const
  msgModificationsASauvegarder = 'Attention ! Vous avez fait des modifications. Souhaitez-vous ' +
    'les sauvegarder ?';

var
  FenDBGridPanel1: TFenDBGridPanel1;

implementation

{$R *.DFM}

procedure TFenDBGridPanel1.btnFermerClick(Sender: TObject);
begin
	Close;
end;

procedure TFenDBGridPanel1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Action: TModalResult;
begin
	if DSPrincipal.DataSet.State in [dsInsert, dsEdit] then
  begin
    if DSPrincipal.DataSet.Modified then
    begin
  	  Action:= MessageDlg(msgModificationsASauvegarder, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      case Action of
        mrYes: DSPrincipal.DataSet.Post;
        mrNo: DSPrincipal.DataSet.Cancel;
        else
          Canclose:= False;
      end;
    end else
      DSPrincipal.DataSet.Cancel;
  end;
end;

procedure TFenDBGridPanel1.FormCreate(Sender: TObject);
begin
  FindPanelPrincipal.DataSet := DSPrincipal.DataSet;
end;

end.
