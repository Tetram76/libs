unit FEditFileGridCols;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF GZ_D6}Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, ExitPnl, StdCtrls, Buttons, ExtCtrls, GzFileGrid;

type
  TDlgEditFileGridColumns = class(TForm)
    Panel1: TPanel;
    btnMonter: TBitBtn;
    btnDescendre: TBitBtn;
    btnAjouter: TBitBtn;
    btnRetirer: TBitBtn;
    LBActuels: TListBox;
    LBDispos: TListBox;
    Panel2: TPanel;
    ExitPanel1: TExitPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure ExitPanel1OkClick(Sender: TObject);
    procedure btnMonterClick(Sender: TObject);
    procedure btnDescendreClick(Sender: TObject);
    procedure btnAjouterClick(Sender: TObject);
    procedure btnRetirerClick(Sender: TObject);
    procedure LBActuelsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBActuelsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LBActuelsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LBDisposMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    FFileGrid : TFileGrid;
    FDragged : Integer;
  public
    { Déclarations publiques }
    procedure Init(AFileGrid : TFileGrid; CheckTitle : Boolean = False);
    procedure Apply;
  end;

var
  DlgEditFileGridColumns: TDlgEditFileGridColumns;

implementation

{$R *.dfm}

uses
  GzSumInfo;

{ TDlgEditFileGridColumns }

procedure TDlgEditFileGridColumns.Apply;
var
  i : Integer;
  AColParam : TFilePropertyParams;
  AColumn : TFileColumnItem;
begin
  FFileGrid.Columns.BeginUpdate;
  try
    FFileGrid.Columns.Clear;
    for i := 0 to LBActuels.Items.Count - 1 do
    begin
      AColParam := TFilePropertyParams(LBActuels.Items.Objects[i]);
      AColumn := FFileGrid.Columns.Add;
      AColumn.PropertyName := AColParam.Name;
      AColumn.DisplayName := AColParam.Title;
      AColumn.Width := AColParam.ColWidth;
      AColumn.Alignment := AColParam.Alignment;
    end;
  finally
    FFileGrid.Columns.EndUpdate;
  end;
end;

procedure TDlgEditFileGridColumns.Init(AFileGrid: TFileGrid; CheckTitle : Boolean = False);
var
  i, k : Integer;
  ALst : TStringList;
  AColParam : TFilePropertyParams;
begin
  FFileGrid := AFileGrid;
  ALst := TStringList.Create;
  try
    GetFilePropertyNames(FFileGrid.FileClass.ClassName, ALst);
    for i := 0 to ALst.Count - 1 do
    begin
      AColParam := GetFilePropertyParams(FFileGrid.FileClass, ALst[i]);
      if (not CheckTitle) or (CompareStr(AnsiUpperCase(AColParam.Title), AColParam.Title) <> 0) then
        LBDispos.Items.AddObject(AColParam.Title, AColParam);
    end;
    for i := 0 to FFileGrid.Columns.Count - 1 do
    begin
      AColParam := GetFilePropertyParams(FFileGrid.FileClass, FFileGrid.Columns[i].PropertyName);
      k := LBDispos.Items.IndexOfObject(AColParam);
      LBDispos.Items.Delete(k);
      LBActuels.Items.AddObject(AColParam.Title, AColParam);
    end;
  finally
    ALst.Free;
  end;
end;

procedure TDlgEditFileGridColumns.ExitPanel1OkClick(Sender: TObject);
begin
  Apply;
  ModalResult := mrOk;
end;

procedure TDlgEditFileGridColumns.btnMonterClick(Sender: TObject);
begin
  if LBActuels.ItemIndex > 0 then
    LBActuels.Items.Exchange(LBActuels.ItemIndex, LBActuels.ItemIndex - 1);
end;

procedure TDlgEditFileGridColumns.btnDescendreClick(Sender: TObject);
begin
  if LBActuels.ItemIndex < LBActuels.Items.Count - 1 then
    LBActuels.Items.Exchange(LBActuels.ItemIndex, LBActuels.ItemIndex + 1);
end;

procedure TDlgEditFileGridColumns.btnAjouterClick(Sender: TObject);
var
  k : Integer;
begin
  if LBDispos.ItemIndex >= 0 then
  begin
    LBActuels.Items.AddObject(LBDispos.Items[LBDispos.ItemIndex],LBDispos.Items.Objects[LBDispos.ItemIndex]);
    LBActuels.ItemIndex := LBActuels.Items.Count - 1;
    k := LBDispos.ItemIndex;
    LBDispos.Items.Delete(LBDispos.ItemIndex);
    if k >= LBDispos.Items.Count then
      LBDispos.ItemIndex := LBDispos.Items.Count - 1
    else
      LBDispos.ItemIndex := k;
  end;
end;

procedure TDlgEditFileGridColumns.btnRetirerClick(Sender: TObject);
var
  AColParam : TFilePropertyParams;
  k : Integer;
begin
  if LBActuels.ItemIndex >= 0 then
  begin
    AColParam := TFilePropertyParams(LBActuels.Items.Objects[LBActuels.ItemIndex]);
    LBDispos.Items.AddObject(LBActuels.Items[LBActuels.ItemIndex], AColParam);
    LBDispos.ItemIndex := LBDispos.Items.IndexOfObject(AColParam);
    k := LBActuels.ItemIndex;
    LBActuels.Items.Delete(LBActuels.ItemIndex);
    if k >= LBActuels.Items.Count then
      LBActuels.ItemIndex := LBActuels.Items.Count - 1
    else
      LBActuels.ItemIndex := k;
  end;
end;

procedure TDlgEditFileGridColumns.LBActuelsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (LBActuels.ItemIndex >= 0) then
  begin
    FDragged := LBActuels.ItemIndex;
    LBActuels.BeginDrag(False, 1);
  end;
end;

procedure TDlgEditFileGridColumns.LBActuelsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  k : Integer;
begin
  if (Source = LBActuels) and (FDragged >= 0) then
  begin
    k := LBActuels.ItemAtPos(Point(X, Y), False);
    Accept := (k >= 0);
    if Accept then
      LBActuels.ItemIndex := k;
  end
  else
  if (Source = LBDispos) and (FDragged >= 0) then
  begin
    k := LBActuels.ItemAtPos(Point(X, Y), False);
    Accept := (k >= 0);
    if Accept then
      LBActuels.ItemIndex := k;
  end;
end;

procedure TDlgEditFileGridColumns.LBActuelsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  k : Integer;
begin
  if (Source = LBActuels) and (FDragged >= 0) then
  begin
    k := LBActuels.ItemAtPos(Point(X, Y), False);
    if k >= 0 then
    begin
      LBActuels.Items.Move(FDragged, k);
      LBActuels.ItemIndex := k;
    end;
    FDragged := 0;
  end
  else
  if (Source = LBDispos) and (FDragged >= 0) then
  begin
    k := LBActuels.ItemAtPos(Point(X, Y), False);
    if k >= 0 then
    begin
      LBActuels.Items.InsertObject(k, LBDispos.Items[FDragged],LBDispos.Items.Objects[FDragged]);
      LBDispos.Items.Delete(FDragged);
      LBActuels.ItemIndex := k;
    end
    else
    begin
      LBActuels.Items.AddObject(LBDispos.Items[FDragged],LBDispos.Items.Objects[FDragged]);
      LBDispos.Items.Delete(FDragged);
      LBActuels.ItemIndex := LBActuels.Items.Count - 1;
    end;
    FDragged := 0;
  end;
end;

procedure TDlgEditFileGridColumns.LBDisposMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (LBDispos.ItemIndex >= 0) then
  begin
    FDragged := LBDispos.ItemIndex;
    LBDispos.BeginDrag(False, 1);
  end;
end;

end.
