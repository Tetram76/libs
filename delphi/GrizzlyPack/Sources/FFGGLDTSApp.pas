unit FFGGLDTSApp;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, DBCtrls, Grids, DBGrids, ExtCtrls, DB,
  Dbnavbtn, Dbinfo, DBFind, GzGLMgr;

type
	EGenericListError = Class(Exception);

  TGLDTSFenAppGenericList = class(TForm)
    DSElements: TDataSource;
    PanelGlobal: TPanel;
    DBGElements: TDBGrid;
    PanelBoutons: TPanel;
    DBNavBarrePrincipal: TDBNavBarre;
    PanelBoutonFermer: TPanel;
    FermerBtn: TBitBtn;
    PanelDBNav: TPanel;
    DBNavPrincipal: TDBNavigator;
    DBInfoPrincipal: TDBRecordCount;
    FindPanelPrincipal: TFindPanel;
    DSGroupes: TDataSource;
    DBGGroupes: TDBGrid;
    Splitter1: TSplitter;
    DSLienGroupes: TDataSource;
    procedure FermerBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations private }
    FGroupsOpened, FItemsOpened : Boolean;
  public
    { Déclarations public }
    procedure Init(DtsGroupes, DtsElements : TDataset);
  end;

var
  GLDTSFenAppGenericList: TGLDTSFenAppGenericList;

implementation

{$R *.DFM}

procedure TGLDTSFenAppGenericList.Init(DtsGroupes, DtsElements : TDataset);
begin
  DSGroupes.Dataset := DtsGroupes;
  DSElements.Dataset := DtsElements;
  DSLienGroupes.Dataset := DtsGroupes;
  FindPanelPrincipal.DataSet := DtsElements;
  if not DtsGroupes.Active then
  begin
    DtsGroupes.Open;
    FGroupsOpened := True;
  end
  else
    FGroupsOpened := False;
  if not DtsElements.Active then
  begin
    DtsElements.Open;
    FItemsOpened := True;
  end
  else
    FItemsOpened := False;
end;

procedure TGLDTSFenAppGenericList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
	If DBNavBarrePrincipal.Editing then
  Begin
  	MessageDlg('Terminez vos modifications avant de quitter',mtInformation,[mbOk],0);
    Canclose:=False;
  End;
end;

procedure TGLDTSFenAppGenericList.FermerBtnClick(Sender: TObject);
begin
	Close;
end;

procedure TGLDTSFenAppGenericList.FormDestroy(Sender: TObject);
begin
  if FGroupsOpened then
    DSGroupes.DataSet.Close;
  if FItemsOpened then
    DSElements.DataSet.Close;
end;

end.
