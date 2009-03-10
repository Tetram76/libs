unit FFieldMapEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Db, DBTables, Grids, DBGrids, Hintctrl, ExtCtrls, ExitPnl,
  VDataSet;

type
  TFieldMapEditor = class(TForm)
    DS: TDataSource;
    Panel1: TPanel;
    FGrid: THDBGrid;
    VDataSet: TVirtualDataSet;
    VDataSetSource: TStringField;
    VDataSetDest: TStringField;
    Panel2: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnClear: TBitBtn;
    btnAuto: TBitBtn;
    procedure CancelBtnClick(Sender: TObject);
    procedure FGridExit(Sender: TObject);
    procedure FGridColEnter(Sender: TObject);
    procedure FGridColExit(Sender: TObject);
    procedure VDataSetBeforeInsert(DataSet: TDataSet);
    procedure ExitPanel1OkClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure VDataSetBeforeDelete(DataSet: TDataSet);
    procedure btnClearClick(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
  private
    FFieldMap: TStrings;
    FDestFields: TStrings;
    FSourceFields: TStrings;
    {}
    procedure SetFieldMap(const Value: TStrings);
    procedure SetDestFields(const Value: TStrings);
    procedure SetSourceFields(const Value: TStrings);
  protected
    procedure Initialize;
    procedure Clear;
    procedure AutoAssign;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: TModalResult;
    {}
    property FieldMap: TStrings read FFieldMap write SetFieldMap;
    property SourceFields: TStrings read FSourceFields write SetSourceFields;
    property DestFields: TStrings read FDestFields write SetDestFields;
  end;

var
  FieldMapEditor: TFieldMapEditor;

implementation

{$R *.DFM}

constructor TFieldMapEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldMap:= TStringList.Create;
  FSourceFields:= TStringList.Create;
  FDestFields:= TStringList.Create;
  TStringList(FSourceFields).Sorted:= True;
  TStringList(FDestFields).Sorted:= True;
end;

destructor TFieldMapEditor.Destroy;
begin
  FDestFields.Free;
  FSourceFields.Free;
  FFieldMap.Free;
  inherited Destroy;
end;

procedure TFieldMapEditor.Initialize;
  procedure ReverseMapping;
    procedure ExtractItems(const S: string; var Name, Value: string);
    var P: Integer;
    begin
      P:= AnsiPos('=', S);
      Name:= System.Copy(S, 1, P - 1);
      Value:= System.Copy(S, P + 1, Length(S));
    end;
  var S1, S2: string;
      i: Integer;
  begin
    for i:= 0 to FieldMap.Count - 1 do
    begin
      ExtractItems(FieldMap[i], S1, S2);
      FieldMap[i]:= Format('%s=%s', [S2, S1]);
    end;
  end;
var i: Integer;
begin
  { Open Table }
  VDataSet.Open;
  { Fill List }
  FGrid.Columns[0].PickList.Assign(SourceFields);
  { Fill DataSet }
  ReverseMapping;
  for i:= 0 to DestFields.Count - 1 do
  begin
    VDataSet.Append;
    VDataSetSource.AsString:= FieldMap.Values[DestFields[i]];
    VDataSetDest.AsString:= DestFields[i];
    VDataSet.Post;
  end;
  VDataSet.BeforeInsert:= VDataSetBeforeInsert;
  VDataSet.First;
end;

procedure TFieldMapEditor.ExitPanel1OkClick(Sender: TObject);
begin
  VDataSet.DisableControls;
  try
    FieldMap.Clear;
    {}
    VDataSet.First;
    while not VDataSet.EOF do
    begin
      if VDataSetSource.AsString <> '' then
        FieldMap.Add(Format('%s=%s', [VDataSetSource.AsString, VDataSetDest.AsString]));
      VDataSet.Next;
    end;
  except
    VDataSet.EnableControls;
    raise;
  end;
  ModalResult:= mrOk;
end;

procedure TFieldMapEditor.btnClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TFieldMapEditor.btnAutoClick(Sender: TObject);
begin
  AutoAssign;
end;

procedure TFieldMapEditor.Clear;
var OldBookmark: TBookmarkStr;
begin
  VDataSet.DisableControls;
  try
    OldBookmark:= VDataSet.Bookmark;
    try
      VDataSet.First;
      while not VDataSet.EOF do
      begin
        VDataSet.Edit;
        VDataSetSource.Clear;
        VDataSet.Post;
        VDataSet.Next;
      end;
    finally
      VDataSet.Bookmark:= OldBookmark;
    end;
  finally
    VDataSet.EnableControls;
  end;
end;

procedure TFieldMapEditor.AutoAssign;
var OldBookmark: TBookmarkStr;
    Idx: Integer;
begin
  VDataSet.DisableControls;
  try
    OldBookmark:= VDataSet.Bookmark;
    try
      VDataSet.First;
      while not VDataSet.EOF do
      begin
        Idx:= SourceFields.IndexOf(VDataSetDest.AsString);
        if Idx > -1 then
        begin
          VDataSet.Edit;
          VDataSetSource.AsString:= SourceFields[Idx];
          VDataSet.Post;
        end;
        VDataSet.Next;
      end;
    finally
      VDataSet.Bookmark:= OldBookmark;
    end;
  finally
    VDataSet.EnableControls;
  end;
end;

function TFieldMapEditor.Execute: TModalResult;
begin
  Initialize;
  Result:= Self.ShowModal;
end;

procedure TFieldMapEditor.VDataSetBeforeInsert(DataSet: TDataSet);
begin
  Abort;
end;

procedure TFieldMapEditor.CancelBtnClick(Sender: TObject);
begin
  FieldMap.Clear;
end;

procedure TFieldMapEditor.FGridExit(Sender: TObject);
begin
  if VDataSet.State in [dsInsert, dsEdit] then
    VDataSet.Post;
end;

procedure TFieldMapEditor.FGridColEnter(Sender: TObject);
begin
  if VDataSet.State in [dsInsert, dsEdit] then
    VDataSet.Post;
end;

procedure TFieldMapEditor.FGridColExit(Sender: TObject);
begin
  if VDataSet.State in [dsInsert, dsEdit] then
    VDataSet.Post;
end;

procedure TFieldMapEditor.Panel1Resize(Sender: TObject);
var W: Integer;
begin
  W:= (FGrid.Width - 35) div 2;
  FGrid.Columns[0].Width:= W;
  FGrid.Columns[1].Width:= W;
end;

procedure TFieldMapEditor.VDataSetBeforeDelete(DataSet: TDataSet);
begin
  Abort;
end;

procedure TFieldMapEditor.SetFieldMap(const Value: TStrings);
begin
  FFieldMap.Assign(Value);
end;

procedure TFieldMapEditor.SetDestFields(const Value: TStrings);
begin
  FDestFields.Assign(Value);
end;

procedure TFieldMapEditor.SetSourceFields(const Value: TStrings);
begin
  FSourceFields.Assign(Value);
end;

end.
