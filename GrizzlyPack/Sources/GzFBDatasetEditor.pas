unit GzFBDatasetEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, GzFBDataset, StdCtrls, Buttons, ComCtrls,
  SynEditHighlighter, SynHighlighterSQL, SynEdit, SynMemo, 
  uib, uibdataset;

type
  TDlgFBDatasetEditor = class(TForm)
    btnOK: TBitBtn;
    BitBtn1: TBitBtn;
    PageControl1: TPageControl;
    tsSQL: TTabSheet;
    tsGenerer: TTabSheet;
    btnGenerer: TBitBtn;
    LBUpdateKey: TListBox;
    Label1: TLabel;
    CBTable: TComboBox;
    LabelErreurs: TLabel;
    LBRefreshKey: TListBox;
    LBUpdateFields: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoSQL: TSynMemo;
    SynSQLSyn1: TSynSQLSyn;
    tbSQL: TTabControl;
    CBGuillemets: TCheckBox;
    CBIndexUpdate: TComboBox;
    CBIndexRefresh: TComboBox;
    transReadOnly: TUIBTransaction;
    dtsIndices: TUIBQuery;
    dtsCheckSQL: TUIBDataSet;
    dtsGetFields: TUIBDataSet;
    procedure tbSQLChange(Sender: TObject);
    procedure MemoSQLChange(Sender: TObject);
    procedure tsGenererShow(Sender: TObject);
    procedure CBTableChange(Sender: TObject);
    procedure CBIndexUpdateChange(Sender: TObject);
    procedure CBIndexRefreshChange(Sender: TObject);
    procedure btnGenererClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Déclarations privées }
    FLastSQLSelect : string;
    FSQLSelect, FSQLUpdate, FSQLInsert, FSQLDelete, FSQLRefresh : string;
    FDataset: TGzFBDataSet;
    procedure FillTableCombo;
    procedure FillFieldLists;
    procedure SelectIndexFields(ALB : TListBox; F : string);

    function GetFieldsString(ALB : TListBox; AFormat, ASeparator : string): string;
    function GetSelect(ALB : TListBox) : string;
    function GetUpdate(ALB : TListBox) : string;
    function GetValues(ALB : TListBox) : string;
    function GetWhere(ALB : TListBox; WithOld : Boolean) : string;

  public
    { Déclarations publiques }
    property Dataset : TGzFBDataSet read FDataset write FDataset;
    function Execute(ADataset : TGzFBDataset) : Boolean;
    procedure Apply;

    property SQLSelect : string read FSQLSelect;
    property SQLInsert : string read FSQLInsert;
    property SQLUpdate : string read FSQLUpdate;
    property SQLDelete : string read FSQLDelete;
    property SQLRefresh : string read FSQLRefresh;
  end;

var
  DlgFBDatasetEditor: TDlgFBDatasetEditor;

implementation

{$R *.dfm}

procedure TDlgFBDatasetEditor.FillTableCombo;
var
  i, k, n : Integer;
  ATable : string;
  AFields : TFields;
  ALst : TStrings;
begin
  CBGuillemets.Checked := (Pos('"', FSQLSelect) > 0);
  //Récupération des champs...
  ALst := CBTable.Items;
  ALst.Clear;
  AFields := nil;
  dtsCheckSQL.SQL.Text := FSQLSelect;
  try
    try
      dtsCheckSQL.Open;
      AFields := dtsCheckSQL.Fields;
      FLastSQLSelect := FSQLSelect;
    except
      on E: Exception do
        MessageDlg(E.Message, mtError, [mbOk], 0);
    end;

    if Assigned(AFields) then
    begin
      for i := 0 to AFields.Count - 1 do
      begin
        with AFields[i] do
        begin
          k := Pos('.', Origin);
          if (k > 0) then
          begin
            ATable := Copy(Origin, 1, k - 1);
            n := ALst.IndexOf(ATable);
            if n < 0 then
              ALst.AddObject(ATable, Pointer(0))
            else
              ALst.Objects[n] := Pointer(Longint(ALst.Objects[n]) + 1);
          end;
        end;
      end;
    end;
  finally
    if dtsCheckSQL.Active then
      dtsCheckSQL.Close;
  end;
  SynSQLSyn1.TableNames.Assign(ALst);
  if ALst.Count = 0 then
    LabelErreurs.Caption := 'Aucunes tables disponibles'
  else
  begin
    //Recherche du plus grand effectif pour les tables...
    n := -1;
    k := 0;
    for i := 0 to ALst.Count - 1 do
    begin
      if Longint(ALst.Objects[i]) > k then
      begin
        n := i;
        k := Longint(ALst.Objects[n]);
      end;
    end;
    LabelErreurs.Caption := '';
    CBTable.ItemIndex := n;
    FillFieldLists;
  end;
end;

function TDlgFBDatasetEditor.Execute(ADataset: TGzFBDataset): Boolean;
begin
  FDataset := ADataset;

  //Attention, si on tente d'affecter une seconde fois, ça plante...
  //A cause sûrement d'un souci avec le composant Database qui est dans une
  //autre fenêtre...
  transReadOnly.DataBase := FDataset.DataBase;
  dtsIndices.DataBase := FDataset.DataBase;
  dtsCheckSQL.DataBase := FDataset.DataBase;
  dtsGetFields.DataBase := FDataset.DataBase;

  FLastSQLSelect := FDataset.SQLSelect.Text;
  FSQLSelect := FDataset.SQLSelect.Text;
  FSQLUpdate := FDataset.SQLUpdate.Text;
  FSQLInsert := FDataset.SQLInsert.Text;
  FSQLDelete := FDataset.SQLDelete.Text;
  FSQLRefresh := FDataset.SQLRefresh.Text;

  FillTableCombo;

  Result := (ShowModal = mrOk);

end;

procedure TDlgFBDatasetEditor.tbSQLChange(Sender: TObject);
begin
  case tbSQL.TabIndex of
  0 : MemoSQL.Text := FSQLSelect;
  1 : MemoSQL.Text := FSQLUpdate;
  2 : MemoSQL.Text := FSQLInsert;
  3 : MemoSQL.Text := FSQLDelete;
  4 : MemoSQL.Text := FSQLRefresh;
  end;
end;

procedure TDlgFBDatasetEditor.MemoSQLChange(Sender: TObject);
begin
  case tbSQL.TabIndex of
  0 : FSQLSelect := MemoSQL.Text;
  1 : FSQLUpdate := MemoSQL.Text;
  2 : FSQLInsert := MemoSQL.Text;
  3 : FSQLDelete := MemoSQL.Text;
  4 : FSQLRefresh := MemoSQL.Text;
  end;
end;

procedure TDlgFBDatasetEditor.Apply;
begin
  FDataset.SQLSelect.Text := FSQLSelect;
  FDataset.SQLUpdate.Text := FSQLUpdate;
  FDataset.SQLInsert.Text := FSQLInsert;
  FDataset.SQLDelete.Text := FSQLDelete;
  FDataset.SQLRefresh.Text := FSQLRefresh;
end;

procedure TDlgFBDatasetEditor.tsGenererShow(Sender: TObject);
begin
  if FLastSQLSelect <> FSQLSelect then
    FillTableCombo;
end;

procedure TDlgFBDatasetEditor.FillFieldLists;
var
  i, k : Integer;
  ATable, AChamp : string;
  AFields : TFields;
  ALst : TStrings;
begin
  CBIndexUpdate.Items.Clear;
  CBIndexRefresh.Items.Clear;
  LBUpdateKey.Items.Clear;
  LBRefreshKey.Items.Clear;
  LBUpdateFields.Items.Clear;

  dtsGetFields.SQL.Text := FSQLSelect;
  dtsGetFields.Open;
  try

    AFields := dtsGetFields.Fields;
  ALst := LBUpdateKey.Items;

  for i := 0 to AFields.Count - 1 do
    with AFields[i] do
    begin
      k := Pos('.', Origin);
      if (k > 0) then
      begin
        ATable := Copy(Origin, 1, k - 1);
          if (ATable = CBTable.Text) and (FieldKind = fkData) then
        begin
          AChamp := Copy(Origin, k + 1, Length(Origin) - k);
          if ALst.IndexOf(AChamp) < 0 then
            ALst.Add(AChamp);
        end;
      end;
    end;
  finally
    dtsGetFields.Close;
  end;

  LBRefreshKey.Items.Assign(ALst);
  LBUpdateFields.Items.Assign(ALst);

  dtsIndices.Params.AsString[0] := CBTable.Text;
  dtsIndices.Open;
  try
    ALst := CBIndexUpdate.Items;
    ATable := Trim(dtsIndices.Fields.AsString[0]);
    AChamp := '';
    while not dtsIndices.Eof do
    begin
      if Trim(dtsIndices.Fields.AsString[0]) <> ATable then
      begin
        if AChamp <> '' then
          ALst.Add(AChamp);
        ATable := Trim(dtsIndices.Fields.AsString[0]);
        AChamp := Trim(dtsIndices.Fields.AsString[1]);
      end
      else
      begin
        if AChamp = '' then
          AChamp := Trim(dtsIndices.Fields.AsString[1])
        else
          AChamp := AChamp + ';' + Trim(dtsIndices.Fields.AsString[1]);
      end;
      dtsIndices.Next;
    end;
    if AChamp <> '' then
      ALst.Add(AChamp);
    CBIndexRefresh.Items.Assign(ALst);

    CBIndexUpdate.ItemIndex := -1;
    CBIndexRefresh.ItemIndex := -1;
  finally
    dtsIndices.Close;
  end;
  if CBIndexUpdate.Items.Count > 0 then
  begin
    CBIndexUpdate.ItemIndex := 0;
    if CBIndexRefresh.Items.Count > 1 then
      CBIndexRefresh.ItemIndex := 1
    else
      CBIndexRefresh.ItemIndex := 0;
    SelectIndexFields(LBUpdateKey, CBIndexUpdate.Text);
    SelectIndexFields(LBRefreshKey, CBIndexRefresh.Text);
  end;
  LBUpdateFields.SelectAll;
end;

procedure TDlgFBDatasetEditor.CBTableChange(Sender: TObject);
begin
  FillFieldLists;
end;

procedure TDlgFBDatasetEditor.SelectIndexFields(ALB: TListBox; F: string);
var
  ALst : TStringList;
  i : Integer;
begin
  ALst := TStringList.Create;
  try
    ALst.Delimiter := ';';
    ALst.DelimitedText := F;
    if ALst.Count > 0 then
    begin
      for i := 0 to ALB.Items.Count - 1 do
        ALB.Selected[i] := (ALst.IndexOf(ALB.Items[i]) >= 0);
    end;
  finally
    ALst.Free;
  end;
end;

procedure TDlgFBDatasetEditor.CBIndexUpdateChange(Sender: TObject);
begin
  SelectIndexFields(LBUpdateKey, CBIndexUpdate.Text);
end;

procedure TDlgFBDatasetEditor.CBIndexRefreshChange(Sender: TObject);
begin
  SelectIndexFields(LBRefreshKey, CBIndexRefresh.Text);
end;

procedure TDlgFBDatasetEditor.btnGenererClick(Sender: TObject);
var
  ASQL : string;
  function GetTable : string;
  begin
    if CBGuillemets.Checked then
      Result := '"' + CBTable.Text + '"'
    else
      Result := CBTable.Text;
  end;
begin
  //Remplissage de l'update
  ASQL := 'UPDATE ' + GetTable + #13#10 +
    'SET ' + #13#10 +
    GetUpdate(LBUpdateFields) + #13#10 +
    'WHERE ' + #13#10 +
    GetWhere(LBUpdateKey, True) + ';';
  FSQLUpdate := ASQL;
  //Remplissage de l'insert
  ASQL := 'INSERT INTO ' + GetTable + #13#10 +
    '(' + #13#10 +
    GetSelect(LBUpdateFields) + #13#10 +
    ') ' + #13#10 +
    'VALUES ' + #13#10 +
    '(' + #13#10 +
    GetValues(LBUpdateFields) + #13#10 +
    ');';
  FSQLInsert := ASQL;
  //Remplissage du delete
  ASQL := 'DELETE FROM ' + GetTable + #13#10 +
    'WHERE ' + #13#10 +
    GetWhere(LBUpdateKey, True) + ';';
  FSQLDelete := ASQL;
  //Remplissage de l'update
  ASQL := 'SELECT ' + #13#10 +
    GetSelect(LBUpdateFields) + #13#10 +
    'FROM ' + CBTable.Text + #13#10 +
    'WHERE ' + #13#10 +
    GetWhere(LBRefreshKey, False) + ';';
  FSQLRefresh := ASQL;

  tbSQLChange(nil);
end;


function TDlgFBDatasetEditor.GetFieldsString(ALB : TListBox; AFormat, ASeparator : string): string;
var
  i : Integer;
  ALst : TStrings;
  function GetFieldString(FieldName: string): string;
  begin
    Result := '  ' + StringReplace(AFormat, '%FIELD%', FieldName, [rfReplaceAll, rfIgnoreCase]);
    if CBGuillemets.Checked then
      Result := StringReplace(Result, '%Q%', '"', [rfReplaceAll, rfIgnoreCase])
    else
      Result := StringReplace(Result, '%Q%', '', [rfReplaceAll, rfIgnoreCase]);
  end;
begin
  ALst := ALB.Items;
  Result := '';
  for i := 0 to ALst.Count - 1 do
    if ALB.Selected[i] then
      if Result = '' then
        Result := GetFieldString(ALst[i])
      else
        Result := Result + ASeparator + #13#10 + GetFieldString(ALst[i]);
end;

function TDlgFBDatasetEditor.GetSelect(ALB: TListBox): string;
begin
  Result := GetFieldsString(ALB, '%Q%%FIELD%%Q%', ', ');
end;

function TDlgFBDatasetEditor.GetUpdate(ALB: TListBox): string;
begin
  Result := GetFieldsString(ALB, '%Q%%FIELD%%Q% = :%Q%%FIELD%%Q%', ', ');
end;

function TDlgFBDatasetEditor.GetValues(ALB: TListBox): string;
begin
  Result := GetFieldsString(ALB, ':%Q%%FIELD%%Q%', ', ');
end;

function TDlgFBDatasetEditor.GetWhere(ALB: TListBox; WithOld : Boolean): string;
begin
  if WithOld then
    Result := GetFieldsString(ALB, '%Q%%FIELD%%Q% = :%Q%OLD_%FIELD%%Q%', ' and ')
  else
    Result := GetFieldsString(ALB, '%Q%%FIELD%%Q% = :%Q%%FIELD%%Q%', ' and ');
end;

procedure TDlgFBDatasetEditor.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  tbSQL.TabIndex := 0;
end;

procedure TDlgFBDatasetEditor.btnOKClick(Sender: TObject);
begin
  if ((FSQLUpdate = '') or (FSQLInsert = '') or (FSQLDelete = '') or (FSQLRefresh = ''))
    and (MessageDlg('Vous n''avez pas généré tous les ordres SQL, voulez-vous vraiment fermer l''éditeur ?', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel) then
    Exit;
  ModalResult := mrOk;
end;

end.
