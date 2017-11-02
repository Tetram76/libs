unit FSelectionDBGrid;

interface

uses
  Windows, SysUtils, Forms, Dialogs, DB, ExitPnl, DBNavBtn, DBFind, DBInfo,
  DBCtrls, Grids, DBGrids, Classes, Controls, ExtCtrls, HintCtrl;

type
  TFenSelDBGrid1 = class(TForm)
    DSPrincipal: TDataSource;
    PanelGlobal: TPanel;
    DBGridPrincipal: THDBGrid;
    PanelDBNavDBInfo: TPanel;
    DBNavPrincipal: TDBNavigator;
    DBInfoPrincipal: TDBRecordCount;
    FindPanelPrincipal: TFindPanel;
    PanelHaut: TPanel;
    ExitPanel1: TExitPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBGridPrincipalDblClick(Sender: TObject);
  private
  public
  end;

const
  msgModificationsASauvegarder = 'Attention ! Vous avez fait des modifications. Souhaitez-vous ' +
    'les sauvegarder ?';

var
  FenSelDBGrid1: TFenSelDBGrid1;

implementation

{$R *.DFM}

procedure TFenSelDBGrid1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Action: TModalResult;
begin
  if not Assigned(DSPrincipal.DataSet) then
  begin
    CanClose := True;
    Exit;
  end;
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

procedure TFenSelDBGrid1.FormCreate(Sender: TObject);
begin
  FindPanelPrincipal.DataSet := DSPrincipal.DataSet;
end;

procedure TFenSelDBGrid1.FormShow(Sender: TObject);
begin
  FindPanelPrincipal.Text:= '';
  FindPanelPrincipal.SetFocus;
end;

procedure TFenSelDBGrid1.DBGridPrincipalDblClick(Sender: TObject);
begin
  if not DSPrincipal.DataSet.IsEmpty then
    ModalResult:= mrOk;
end;

end.
