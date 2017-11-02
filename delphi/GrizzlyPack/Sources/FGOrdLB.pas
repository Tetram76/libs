unit FGOrdLB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB;

const
  obDescending = 'D';
  obAscending = 'A';

type
  TOrderByFieldOption = (ofFieldName, ofOrigin);

  TOrderChangeEvent = procedure(Sender: TObject; Ascending: Boolean) of object;

  TOrderByListBox = class(TCustomListBox)
  private
    FOrderChange: TOrderChangeEvent;
    FOrderByFieldOption: TOrderByFieldOption;
  protected
    FAddOrderBy: Boolean;
    DragStartIndex: Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure Click; override;
    procedure SetSelectedField(AFi: TField);
    function GetSelectedField: TField;
    function GetOrderBy: string;
    procedure DoOrderChange;
    function GetIndexFieldNames: string;
  public
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream; ADataset: TDataset);
    procedure AddField(AFi: TField);
    procedure RemoveField(AFi: TField);
    constructor Create(AOwner: TComponent); override;
    property SelectedField: TField read GetSelectedField write SetSelectedField;
    procedure ChangeSort;
    property OrderBy: string read GetOrderBy;
    property IndexFieldNames: string read GetIndexFieldNames;
  published
    property AddOrderBy: Boolean read FAddOrderBy write FAddOrderBy default True;
    property OnOrderChange: TOrderChangeEvent read FOrderChange write FOrderChange;
    property FieldProperty: TOrderByFieldOption read FOrderByFieldOption write FOrderByFieldOption default ofFieldName;
    {}
    property Align;
    property BorderStyle;
    property Cursor;
    property DragCursor;
    property Enabled;
    property HelpContext;
    property PopUpMenu;
    property Visible;
    property TabOrder;
    property Ctl3D;
    property Font;
    property Color;
    property ParentColor;
    property ParentFont;
    property ParentCtl3D;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

var
  BitmapAscendingSort, BitmapDescendingSort: TBitmap;

implementation

{$R FGORDLB.R32}

uses GzConsts, FGUtils;

constructor TOrderByListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAddOrderBy:= True;
  DragCursor:= crSizeNS;
  Style:= lbOwnerDrawFixed;
  ItemHeight:= 16;
end;

procedure TOrderByListBox.SaveToStream(AStream: TStream);
var
  i, k: Integer;
  b: Byte;
  AFi: TField;
  procedure WriteString(AChaine: string);
  begin
    k:= Length(AChaine);
    AStream.Write(k, SizeOf(k));
    if k > 0 then
      AStream.Write(Pointer(AChaine)^, k);
  end;
begin
  k:= Items.Count;
  AStream.Write(k, SizeOf(k));
  if k > 0 then
    for i:= 0 to k - 1 do
    begin
      AFi:= TField(Items.Objects[i]);
      WriteString(AFi.FieldName);
      if Items[i] = 'A' then
        b:= 0
      else
        b:= 1;
      AStream.Write(b, SizeOf(b));
    end;
end;

procedure TOrderByListBox.LoadFromStream(AStream: TStream; ADataset: TDataset);
var
  i, k: Integer;
  b: Byte;
  AFi: TField;
  AChaine: string;
  procedure ReadStreamString(var AChaine: string; AStream: TStream);
  var
    k: Integer;
  begin
    AStream.ReadBuffer(k, SizeOf(k));
    if k > 0 then
    begin
      SetLength(AChaine, k);
      AStream.ReadBuffer(Pointer(AChaine)^, k);
    end;
  end;
begin
  AStream.Read(k, SizeOf(k));
  if k > 0 then
    for i:= 0 to k - 1 do
    begin
      ReadStreamString(AChaine, AStream);
      AStream.Read(b, SizeOf(b));
      AFi:= ADataset.FindField(AChaine);
      if Assigned(AFi) then
      begin
        AddField(AFi);
        if b <> 0 then
          ChangeSort;
      end;
    end;
end;

procedure TOrderByListBox.AddField(AFi: TField);
begin
  { On ne l'ajoute que s'il n'est pas déjà là }
  if (Assigned(AFi)) and (Items.IndexOfObject(AFi) < 0) then
    Items.AddObject('A', AFi);
end;

procedure TOrderByListBox.RemoveField(AFi: TField);
var
  k: Integer;
begin
  k:= Items.IndexOfObject(AFi);
  if k >= 0 then
    Items.Delete(k);
end;

procedure TOrderByListBox.ChangeSort;
begin
  if (ItemIndex < 0) and (Items.Count > 0) then
    ItemIndex:= 0;
  if ItemIndex >= 0 then
  begin
    if Items[ItemIndex] = obDescending then
      Items[ItemIndex]:= obAscending
    else
      Items[ItemIndex]:= obDescending;
    DoOrderChange;
  end;
end;

procedure TOrderByListBox.DoOrderChange;
begin
  if Assigned(FOrderChange) then
  begin
    if ItemIndex >= 0 then
      FOrderChange(Self, Items[ItemIndex] = 'A');
  end;
end;

procedure TOrderByListBox.SetSelectedField(AFi: TField);
begin
  ItemIndex:= Items.IndexOfObject(AFi);
end;

function TOrderByListBox.GetSelectedField: TField;
begin
  if ItemIndex >= 0 then
    Result:= TField(Items.Objects[ItemIndex])
  else
    Result:= nil;
end;

procedure TOrderByListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AFi: TField;
  procedure DrawBM(ABitmap: TBitmap);
  begin
    if Assigned(ABitmap) then
    begin
      //Canvas.BrushCopy(Bounds(Rect.Left, Rect.Top,16, 16), ABitmap, Classes.Rect(0,0,15,15), ABitmap.TransparentColor);
      Canvas.Draw(Rect.Left, Rect.Top, ABitmap);
    end;
  end;
begin
  inherited DrawItem(Index, Rect, State);
  if Items[Index] = obDescending then
    DrawBM(BitmapDescendingSort)
  else
    DrawBM(BitmapAscendingSort);
  AFi:= TField(Items.Objects[Index]);
  Canvas.TextOut(Rect.Left + 20, Rect.Top, AFi.DisplayLabel);
end;

procedure TOrderByListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key = VK_DELETE) and (ItemIndex >= 0) then
    Items.Delete(ItemIndex);
end;

procedure TOrderByListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, x, y);
  if ItemIndex >= 0 then
  begin
    DragStartIndex:= ItemIndex;
    BeginDrag(False);
  end;
end;

procedure TOrderByListBox.DragDrop(Source: TObject; X, Y: Integer);
var
  k: Integer;
begin
  inherited DragDrop(Source, X, Y);
  if (Source = Self) and (DragStartIndex >= 0) then
  begin
    k:= ItemAtPos(Point(X, Y), True);
    if (k < 0) then
      k:= Items.Count - 1;
    Items.Move(DragStartIndex, k);
    ItemIndex:= k;
    DragStartIndex:= -1;
  end;
end;

procedure TOrderByListBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  k: Integer;
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if (Source = Self) then
  begin
    k:= ItemAtPos(Point(X, Y), True);
    if k >= 0 then
      ItemIndex:= k;
    Accept:= True;
  end;
end;

procedure TOrderByListBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  try
    inherited DoEndDrag(Target, X, Y);
  finally
    DragStartIndex:= -1;
  end;
end;

procedure TOrderByListBox.DblClick;
begin
  inherited DblClick;
  ChangeSort;
end;

procedure TOrderByListBox.Click;
begin
  inherited Click;
  DoOrderChange;
end;

function TOrderByListBox.GetIndexFieldNames: string;
var i: Integer;
begin
  Result:= '';
  for i:= 0 to Items.Count - 1 do
  begin
    if i <> 0 then
      Result:= Result + ';';
    if FOrderByFieldOption = ofFieldName then
      Result:= Result + TField(Items.Objects[i]).FieldName
    else
      Result:= Result + TField(Items.Objects[i]).Origin;
  end;
end;

function TOrderByListBox.GetOrderBy: string;
var
  AFi: TField;
  i: Integer;
begin
  Result:= '';
  if Items.Count > 0 then
  begin
    for i:= 0 to Items.Count - 1 do
    begin
      AFi:= TField(Items.Objects[i]);
      if Assigned(AFi) then
      begin
        if Result <> '' then
          Result:= Result + ',' + #13 + #10;
        if FOrderByFieldOption = ofFieldName then
          Result:= Result + AFi.FieldName
        else
        if AFi.Origin <> '' then
          Result:= Result + AFi.Origin
        else
          Result:= Result + AFi.FieldName;
        if (result <> '') then
        begin
          if (Items[i] = obDescending) then
            Result:= Result + ' DESC'
          else
            Result:= Result + ' ASC';
        end;
      end;
    end;
    if (Result <> '') and (FAddOrderBy) then
      Result:= ' ORDER BY ' + Result;
  end;
end;

initialization
  BitmapAscendingSort:= TBitmap.Create;
  BitmapAscendingSort.LoadFromResourceName(HInstance, 'FGORDERASC');
  BitmapDescendingSort:= TBitmap.Create;
  BitmapDescendingSort.LoadFromResourceName(HInstance, 'FGORDERDES');
finalization
  BitmapAscendingSort.Free;
  BitmapAscendingSort:= nil;
  BitmapDescendingSort.Free;
  BitmapDescendingSort:= nil;
end.

