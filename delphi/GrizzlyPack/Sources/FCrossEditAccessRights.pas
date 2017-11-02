unit FCrossEditAccessRights;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, VDataSet, Grids, DBGrids, FGUsers, Hintctrl, ExtCtrls, ExitPnl,
  StdCtrls, DBFind, Mask, DBCtrls;

type
  { Notes :
    Les colonnes sont les éléments
    Les lignes sont les utilisateurs
  }

  TFenCrossEditAccessRights = class(TForm)
    FVDataSet: TVirtualDataSet;
    FInternalSource: TDataSource;
    DataGrid: THDBGrid;
    ExitPanel1: TExitPanel;
    PanelSearch: TPanel;
    TypeSearchSelecter: TRadioGroup;
    Panel1: TPanel;
    EditElem: THEdit;
    LabelSelectedElem: TLabel;
    EditElemNumber: THEdit;
    LabelCurrentUser: TLabel;
    EditUser: THDBEdit;
    AccessSelecter: TRadioGroup;
    FindPanelUsers: TFindPanel;
    LabelTypeSearchString: TLabel;
    ElemSelecter: TComboBox;
    Bevel1: TBevel;
    procedure ExitPanel1OkClick(Sender: TObject);
    procedure DataGridShowHint(ACol, ARow: Integer; var Show: Boolean;
      var Text: String);
    procedure DataGridDblClick(Sender: TObject);
    procedure DataGridSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure DataGridKeyPress(Sender: TObject; var Key: Char);
    procedure AccessSelecterClick(Sender: TObject);
    procedure TypeSearchSelecterClick(Sender: TObject);
    procedure ElemSelecterClick(Sender: TObject);
  private
    FUM: TUsersManager;
    FSelecting: Boolean;
    procedure SetUM(UM: TUsersManager);
    procedure InitGrid;
    function GetItem(ACol, ARow: Integer): string;
    procedure SetItem(ACol, ARow: Integer; Value: string);
    procedure SelectCell(ACol, ARow: Integer);
  protected
    procedure SaveAccessRights;
  public
    property UserManager: TUsersManager read FUM write SetUM;
    property Items[ACol, ARow: Integer]: string read GetItem write SetItem;
  end;

const
  Selected = 'X';
  UnSelected = '';

var
  FenCrossEditAccessRights: TFenCrossEditAccessRights;

implementation

uses ADB, UFGUsers;

{$R *.DFM}

procedure TFenCrossEditAccessRights.InitGrid;
var OldActive: Boolean;
    i: Integer;
    procedure InitValues;
    var L: TStringList;
        i, Idx: Integer;
        procedure FindUser(User: string);
        var Found: Boolean;
        begin
          Found:= False;
          FVDataSet.First;
          while (not FVDataSet.EOF) and (not Found) do
          begin
            Found:= CompareText(FVDataSet.Fields[0].AsString, User) = 0;
            if not Found then FVDataSet.Next;
          end;
        end;
    begin
      L:= TStringList.Create;
      with FUM do
      try
        StoreDataSet(AccessDataSet);
        try
          AccessDataSet.First;
          while not AccessDataSet.EOF do
          begin
            Idx:= FVDataSet.FieldDefs.IndexOf(AccessDataSet.FieldByName(AccessFieldElem).AsString);
            if Idx > -1 then
            begin
              L.Assign(AccessDataSet.FieldByName(AccessFieldUsers));
              for i:= 0 to L.Count - 1 do
              begin
                FindUser(L[i]);
                FVDataSet.Edit;
                FVDataSet.Fields[Idx].AsBoolean:= True;
                FVDataSet.Post;
              end;
            end;
            AccessDataSet.Next;
          end;
        finally
          RestoreDataSet(AccessDataSet);
        end;
      finally
        L.Free;
      end;
    end;
begin
  with FUM do
  begin
    { Initialisation des colonnes }
    ElemSelecter.Clear;
    AccessDataSet.First;
    FVDataSet.FieldDefs.Clear;
    FVDataSet.FieldDefs.Add(msgUsers, ftString, 255, False);
    while not AccessDataSet.EOF do
    begin
      if AccessDataSet.FieldByName(AccessFieldAccessKind).AsString <> msgShortLevel then
      begin
        FVDataSet.FieldDefs.Add(AccessDataSet.FieldByName(AccessFieldElem).AsString, ftBoolean, 0, True);
        ElemSelecter.Items.Add(AccessDataSet.FieldByName(AccessFieldElem).AsString + ' - ' + FormatFloat('000', FVDataSet.FieldDefs.Count - 1));
      end;
      AccessDataSet.Next;
    end;
      { Initialisation interface des colonnes }
    FVDataSet.Active:= True;
    DataGrid.Columns[0].ReadOnly:= True;
    DataGrid.Columns[0].Width:= 80;
    for i:= 1 to FVDataSet.FieldCount - 1 do
    begin
      TBooleanField(FVDataSet.Fields[i]).DisplayValues:= Selected + ';' + Unselected;
      DataGrid.Columns[i].Width:= 40;
      DataGrid.Columns[i].Title.Alignment:= taCenter;
      DataGrid.Columns[i].Title.Caption:= FormatFloat('000', i);
      DataGrid.Columns[i].Alignment:= taCenter;
    end;
    { Initialisation des lignes }
    OldActive:= DataSet.Active;
    DataSet.Active:= True;
    try
      DataSet.First;
      while not DataSet.EOF do
      begin
        FVDataSet.Append;
        FVDataSet.Fields[0].AsString:= DataSet.FieldByName(FieldShortName).AsString;
        for i:= 1 to FVDataSet.FieldCount - 1 do
          TBooleanField(FVDataSet.Fields[i]).AsBoolean:= False;
        FVDataSet.Post;
        DataSet.Next;
      end;
    finally
      DataSet.Active:= OldActive;
    end;
      { Initialisation des valeurs }
    StoreDataSet(FVDataSet);
    try
      InitValues;
    finally
      RestoreDataSet(FVDataSet);
    end;
  end;
end;

type
  TPDBGrid = class(THDBGrid);

procedure TFenCrossEditAccessRights.SetUM(UM: TUsersManager);
begin
  FUM:= UM;
  if Assigned(UM) then
  begin
    StoreDataSet(UM.Dataset);
    try
      StoreDataSet(UM.AccessDataSet);
      try
        EditUser.DataField:= msgUsers;
        InitGrid;
        TPDBGrid(DataGrid).FixedCols:= 2;
        FindPanelUsers.SearchField:= FVDataSet.Fields[0].FieldName;
        {}
        LabelSelectedElem.Caption:= msgSelectedElem;
        LabelCurrentUser.Caption:= msgUser;
        AccessSelecter.Caption:= msgAccessEnabled;
        AccessSelecter.Items[0]:= msgYes;
        AccessSelecter.Items[1]:= msgNo;
        TypeSearchSelecter.Caption:= msgSearch;
        TypeSearchSelecter.Items[0]:= msgAUser;
        TypeSearchSelecter.Items[1]:= msgAnElem;
        LabelTypeSearchString.Caption:= msgTypeSearchString;
        ExitPanel1.BtnOk.Caption:= msgOk;
        ExitPanel1.BtnCancel.Caption:= msgCancel;
      finally
        RestoreDataSet(UM.AccessDataSet);
      end;
    finally
      RestoreDataSet(UM.Dataset);
    end;
  end;
end;

{ Pour le retour, que faudra-t-il faire ?
  1 - On annihile ce qu'il y avait avant dans AccessFieldUsers
  2 - On parcoure notre table :
    Chaque fois qu'on tombe sur un Ok, on ajoûte ce qu'il faut là où il faut
      dans AccessDataSet ... C'est LENT, mais bon !
}

procedure TFenCrossEditAccessRights.SaveAccessRights;
var L: TStringList;
    i: Integer;
begin
  { Ecriture dans la base de données }
  L:= TStringList.Create;
  with FVDataSet do
  try
    for i:= 1 to FieldDefs.Count - 1 do
    begin
      L.Clear;
      First;
      while not EOF do
      begin
        if Fields[i].AsBoolean then L.Add(Fields[0].AsString);
        Next;
      end;
      with FUM, FUM.AccessDataSet do
        if Locate(AccessFieldElem, FVDataSet.FieldDefs[i].Name, []) then
        begin
          Edit;
          FieldByName(AccessFieldUsers).Assign(L);
          Post;
        end;
    end;
  finally
    L.Free;
  end;
end;

procedure TFenCrossEditAccessRights.ExitPanel1OkClick(Sender: TObject);
begin
  StoreDataSet(FVDataSet);
  try
    StoreDataSet(FUM.AccessDataSet);
    try
      SaveAccessRights;
    finally
      RestoreDataSet(FUM.AccessDataSet);
    end;
  finally
    RestoreDataSet(FVDataSet);
  end;
  ModalResult:= mrOk;
end;

procedure TFenCrossEditAccessRights.DataGridShowHint(ACol, ARow: Integer;
  var Show: Boolean; var Text: String);
begin
  if (ARow = 0) and (ACol > 1) then
  begin
    Show:= True;
    Text:= FVDataSet.FieldDefs[ACol - 1].Name;
  end;
end;

function TFenCrossEditAccessRights.GetItem(ACol, ARow: Integer): string;
begin
  StoreDataSet(FVDataSet);
  try
    FVDataSet.RecNo:= ARow + 1;
    Result:= FVDataSet.Fields[ACol].AsString;
  finally
    RestoreDataSet(FVDataSet);
  end;
end;

procedure TFenCrossEditAccessRights.SetItem(ACol, ARow: Integer; Value: string);
begin
  StoreDataSet(FVDataSet);
  try
    FVDataSet.RecNo:= ARow + 1;
    FVDataSet.Edit;
    FVDataSet.Fields[ACol].AsString:= Value;
    FVDataSet.Post;
  finally
    RestoreDataSet(FVDataSet);
  end;
end;

procedure TFenCrossEditAccessRights.SelectCell(ACol, ARow: Integer);
var CV: string;
begin
  if (ACol > 1) and (ARow > 0) then
  begin
    CV:= GetItem(ACol - 1, ARow - 1);
    if CV = Selected then CV:= Unselected else CV:= Selected;
    SetItem(ACol - 1, ARow - 1, CV);
    if CV = Selected then
      AccessSelecter.ItemIndex:= 0
    else
      AccessSelecter.ItemIndex:= 1;
  end;
end;

procedure TFenCrossEditAccessRights.AccessSelecterClick(Sender: TObject);
var S: string;
begin
  if FSelecting then Exit;
  case AccessSelecter.ItemIndex of
    0: S:= Selected;
    1: S:= UnSelected;
  end;
  SetItem(TPDBGrid(DataGrid).Selection.Left - 1, TPDBGrid(DataGrid).Selection.Top - 1, S);
end;

procedure TFenCrossEditAccessRights.DataGridDblClick(Sender: TObject);
var G: TGridCoord;
    P: TPoint;
begin
  GetCursorPos(P);
  P:= DataGrid.ScreenToClient(P);
  G:= DataGrid.MouseCoord(P.x, P.y);
  SelectCell(G.X, G.Y);
end;

procedure TFenCrossEditAccessRights.DataGridSelectCell(Sender: TObject;
  Col, Row: Integer; var CanSelect: Boolean);
begin
  CanSelect:= CanSelect and (Col <> 1);
  if FSelecting then Exit;
  if Col > 1 then
  begin
    FSelecting:= True;
    try
      EditElem.Text:= FVDataSet.FieldDefs[Col - 1].Name;
      EditElemNumber.Text:= FormatFloat('000', Col - 1);
      if GetItem(Col - 1, Row - 1) = Selected then
        AccessSelecter.ItemIndex:= 0
      else
        AccessSelecter.ItemIndex:= 1;
    finally
      FSelecting:= False;
    end;
  end;
end;

procedure TFenCrossEditAccessRights.DataGridKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key in [#13, ' '] then
    SelectCell(TPDBGrid(DataGrid).Selection.Left, TPDBGrid(DataGrid).Selection.Top);
end;

procedure TFenCrossEditAccessRights.TypeSearchSelecterClick(Sender: TObject);
begin
  ElemSelecter.Visible:= TRadioGroup(Sender).ItemIndex = 1;
  FindPanelUsers.Visible:= not ElemSelecter.Visible;
end;

procedure TFenCrossEditAccessRights.ElemSelecterClick(Sender: TObject);
var R: TRect;
begin
  R.Top:= TPDBGrid(DataGrid).Selection.Top;
  R.Left:= ElemSelecter.ItemIndex + 2;
  R:= TPDBGrid(DataGrid).CellRect(R.Left, R.Top);
  DataGrid.Perform(WM_LBUTTONDOWN, 0, Word(R.Top + 1) shl 16 + R.Left + 1);
  DataGrid.Perform(WM_LBUTTONUP, 0, R.Left + 1 + Word(R.Top + 1) shl 16);
end;

end.
