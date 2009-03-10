unit QRCustomGrid;

{$I GrizzlyDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, QuickRpt, DB, QRCtrls;

type
  TLabelKind = (lkFieldLabel, lkFieldValue, lkCustomLabel);

  TVertAlign = (vaTop, vaCenter, vaBottom);

  EQRCustomGridError = class(Exception);

  TQRCustomGrid = class;

  TQRGridStoredValues = set of (svWidth, svAlignment, svCaption);

  TQRGridColumn = class(TCollectionItem)
  private
    FDataField: string;
    FCaption: string;
    FKind: TLabelKind;
    FWidth: Integer;
    FAlignment: TAlignment;
    FStoredValues: TQRGridStoredValues;
    FVisible: Boolean;
    FVertAlign: TVertAlign;
    FWordWrap: Boolean;
    FFont: TFont;
    FDefaultFont: Boolean;
    {}
    function GetField: TField;
    function GetGrid: TQRCustomGrid;
    function GetLeft: Integer;
    procedure SetKind(Value: TLabelKind);
    procedure SetVisible(const Value: Boolean);
    {}
    function IsWidthStored: Boolean;
    function IsAlignmentStored: Boolean;
    function IsCaptionStored: Boolean;
    function GetDefaultWidth: Integer;
    function GetDefaultAlignment: TAlignment;
    function GetDefaultCaption: string;
    function GetDefaultValues: Boolean;
    procedure SetWidth(const Width: Integer);
    procedure SetAlignment(const Alignment: TAlignment);
    procedure SetVertAlign(const VertAlign: TVertAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetCaption(const Caption: string);
    procedure SetField(const FieldName: string);
    procedure SetDefaultValues(const Value: Boolean);
    {}
    function Loading: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetDefaultFont(const Value: Boolean);
    procedure FontChange(Sender : TObject);
  protected
    function GetDisplayName: string; override;
    procedure RestoreDefaults;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    {}
    procedure Assign(Source: TPersistent); override;
    property Field: TField read GetField;
    property Grid: TQRCustomGrid read GetGrid;
    {}
    property Left: Integer read GetLeft;
  published
    property DataField: string read FDataField write SetField;
    property LabelKind: TLabelKind read FKind write SetKind default lkCustomLabel;
    property Width: Integer read FWidth write SetWidth stored IsWidthStored;
    property Alignment: TAlignment read FAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: string read FCaption write SetCaption stored IsCaptionStored;
    property DefaultValues: Boolean read GetDefaultValues write SetDefaultValues;
    property Visible: Boolean read FVisible write SetVisible default True;
    property VerticalAlignment: TVertAlign read FVertAlign write SetVertAlign default vaCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property Font : TFont read FFont write SetFont;
    property DefaultFont : Boolean read FDefaultFont write SetDefaultFont default True;
  end;

  TQRGridColumns = class(TCollection)
  private
    FGrid: TQRCustomGrid;
    function GetItem(Idx: Integer): TQRGridColumn;
    function GetVisibleWidth: Integer;
  protected
    property Grid: TQRCustomGrid read FGrid write FGrid;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    property VisibleWidth: Integer read GetVisibleWidth;
  public
    function ColumnByName(const FieldName: string): TQRGridColumn;
    {}
    property Items[Idx: Integer]: TQRGridColumn read GetItem; default;
  end;

  TQRGridPrintEvent = procedure(Sender: TObject; Field: TField; var Text: string) of object;

  TQRCustomGrid = class(TQRPrintable)
  private
    FEmptyKind: TLabelKind;
    FAlignUpdating: Boolean;
    FColumns: TQRGridColumns;
    FMargin: Extended;
    FSeparator: Boolean;
    FDataSet: TDataSet;
    FAutoSize: Boolean;
    FOnPrint: TQRGridPrintEvent;
    FOldFontChange: TNotifyEvent;
    procedure SetColumns(Columns: TQRGridColumns);
    procedure SetDrawSeparator(const Value: Boolean);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetAutoSize(Value: Boolean);
    procedure FontChanged(Sender: TObject);
    function GetKind: TLabelKind;
    procedure SetKind(Kind: TLabelKind);
  protected
    procedure AlignIt; override;
    procedure Paint; override;
    procedure Print(OfsX, OfsY : Integer); override;
    procedure PaintToCanvas(aCanvas: TCanvas; aRect: TRect; aAl: TAlignment;
      aVertAlign: TVertAlign; aCap: string; WordWrap: Boolean);
    procedure PrintToCanvas(aCanvas: TCanvas; aLeft, aTop, aWidth, aHeight: Extended;
      aAl: TAlignment; aVertAlign: TVertAlign; aCap: string; WordWrap: Boolean);
    procedure Loaded; override;
    {}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;
    procedure AutoAdjustSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment;
    property AlignToBand;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property LabelKind: TLabelKind read GetKind write SetKind default lkCustomLabel;
    {}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Columns: TQRGridColumns read FColumns write SetColumns;
    property Color;
    property Height default 16;
    property Width default 65;
    property DrawSeparator: Boolean read FSeparator write SetDrawSeparator default True;
    property Font;
    property ParentFont;
    {}
    property OnPrint: TQRGridPrintEvent read FOnPrint write FOnPrint;
  end;

const
  msgExportImpossible = 'Export impossible';

implementation

uses AClasses, ADB;

{ TQRCustomGrid }

procedure TQRCustomGrid.Print(OfsX, OfsY: Integer);
var i, AYPos, AErr : Integer;
    Column : TQRGridColumn;
    AxPos, aWidth: Extended;
    Text : string;
    Pos: Extended;
    Ratio: Extended;
    procedure DestroyCRLF(var S: string);
    var i: Integer;
      function HasCRLF: Boolean;
      begin
        i:= System.Pos(#13#10, S);
        Result:= i <> 0;
      end;
    begin
      while HasCRLF do
      begin
        Delete(S, i, 1);
        S[i]:= ' ';
      end;
    end;
begin
  if ParentReport.FinalPass and Enabled then
  begin
    with QRPrinter do
    begin
      with QRPrinter.Canvas do
      begin
        AYPos:= YPos(OfsY + Size.Top + Size.Height);
        Brush.Color:= Self.Color;
          { Rempli de vide }
        FillRect(Classes.Rect(XPos(OfsX + Size.Left), YPos(OfsY + Size.Top),
            XPos(OfsX + Size.Left + Size.Width), AYPos));
        Pos:= 0;
          // On ne peint que les colonnes visibles !
        if Width > 0 then
        begin
          QRPrinter.Canvas.Font.Assign(Self.Font);
          Ratio:= Size.Width / Width;
          for i:= 0 to FColumns.Count - 1 do
          begin
            Column:= FColumns.Items[i];
            if Column.Visible and (Pos < Size.Width) then
            begin
              if Column.LabelKind = lkFieldValue then
              begin
                if Column.Field = nil then
                  Text:= ''
                else if Column.Field.IsBlob then
                  Text:= Column.Field.AsString
                else
                  Text:= Column.Field.DisplayText;
              end else
                Text:= Column.Caption;
              aWidth:= Column.Width * Ratio;
              if (Pos + aWidth) > Size.Width then
                aWidth:= Size.Width - Pos;
              DestroyCRLF(Text);
              if Column.DefaultFont then
                QRPrinter.Canvas.Font.Assign(Self.Font)
              else
                QRPrinter.Canvas.Font.Assign(Column.Font);
              if Assigned(OnPrint) then
                OnPrint(Self, Column.Field, Text);
              PrintToCanvas(QRPrinter.Canvas,
                OfsX + Size.Left + Pos, OfsY + Size.Top,
                aWidth, Size.Height,
                Column.Alignment, Column.VerticalAlignment, Text, Column.WordWrap);
              AxPos:= Pos + aWidth;
                { Trace la ligne de droite }
              if DrawSeparator and (AxPos >= 0) and (AxPos <= Size.Width) and (i <> (FColumns.Count - 1)) then
              begin
                Pen.Color:= Self.Frame.Color;
                Pen.Width := Self.Frame.Width;
                AErr:= XPos(OfsX + Size.Left + AxPos);
                MoveTo(AErr, YPos(OfsY + Size.Top));
                LineTo(AErr, AYPos - 1);
              end;
              Pos:= Pos + aWidth;
            end;
          end;
        end;
      end;
    end;
  end;
  inherited Print(OfsX, OfsY);
end;

procedure TQRCustomGrid.PrintToCanvas(aCanvas: TCanvas; aLeft, aTop, aWidth, aHeight: Extended;
  aAl: TAlignment; aVertAlign: TVertAlign; aCap: string; WordWrap: Boolean);
var aRect: TRect;
    R: TRect;
    DrawTextAlign: Integer;
begin
  aRect.Top:= QRPrinter.YPos(aTop);
  aRect.Left:= QRPrinter.XPos(aLeft);
  aRect.Bottom:= QRPrinter.YPos(aTop + aHeight);
  aRect.Right:= QRPrinter.XPos(aLeft + aWidth);

  FMargin:= QRPrinter.Canvas.TextWidth('W') / 3;

  DrawTextAlign:= 0;

  case aAl of
    taLeftJustify:
    begin
      Inc(DrawTextAlign, DT_LEFT);
      Inc(aRect.Left, Round(FMargin));
    end;
    taRightJustify:
    begin
      Inc(DrawTextAlign, DT_RIGHT);
      Dec(aRect.Right, Round(FMargin));
    end;
    taCenter: Inc(DrawTextAlign, DT_CENTER);
  end;
  case aVertAlign of
    vaCenter: Inc(DrawTextAlign, DT_VCENTER);
    vaBottom: Inc(DrawTextAlign, DT_BOTTOM);
    vaTop: Inc(DrawTextAlign, DT_TOP);
  end;
  if WordWrap then
  begin
{    Inc(DrawTextAlign, DT_WORDBREAK);
    Inc(aRect.Top, Round(FMargin));}
    R:= aRect;
    DrawText(aCanvas.Handle, PChar(aCap), Length(aCap), R, DT_NOPREFIX +
      DrawTextAlign + DT_CALCRECT + DT_WORDBREAK);
    aRect.Top:= aRect.Top + Round(Abs(aRect.Bottom - aRect.Top) / 2 - Abs(R.Top - R.Bottom) / 2);
    SetTextAlign(aCanvas.Handle, 0);
    DrawText(aCanvas.Handle, PChar(aCap), Length(aCap), aRect, DT_NOPREFIX +
      DrawTextAlign + DT_WORDBREAK);
  end else
  begin
    Inc(DrawTextAlign, DT_SINGLELINE);
    // Nécessaire pour réinitialiser comme il faut (les composants QR standards ont
    // la facheuse habitude de faire des SetTextAlign à tout va, ce qui fout le
    // bordel de belle manière .......)
    SetTextAlign(aCanvas.Handle, 0);
    DrawText(aCanvas.Handle, PChar(aCap), Length(aCap), aRect, DT_NOPREFIX +
      DrawTextAlign);
  end;

  if ParentReport.Exporting then
    raise EQRCustomGridError.Create(msgExportImpossible);
end;

procedure TQRCustomGrid.Paint;
var i, AxPos: Integer;
    Rect: TRect;
    Column: TQRGridColumn;
    Ratio: Extended;
    Text: string;
    Pos: Integer;
    function Ranged(Value: Integer): Integer;
    begin
      Result:= Round(Value * Ratio);
    end;
begin
  with Canvas do
  begin
    Brush.Color:= Self.Color;
      { On efface tout }
    FillRect(Classes.Rect(0, 0, Width, Height));
    if FColumns.Count > 0 then
    begin
      Ratio:= Zoom / 100;
      Canvas.Font.Assign(Self.Font);
      if Canvas.Font.Size <> Round(Self.Font.Size * Ratio) then
        Canvas.Font.Size:= Round(Self.Font.Size * Ratio);
      Pos:= 0;
      for i:= 0 to FColumns.Count - 1 do
      begin
        Column:= FColumns.Items[i];
        if Column.Visible then
        begin
            { Tracer le trait vertical à droite de la colonne }
          AxPos:= Pos + Ranged(Column.Width);
          if DrawSeparator and (AxPos >= 0) and (AxPos <= Width) and (i <> (FColumns.Count - 1)) then
          begin
            MoveTo(AxPos, 0);
            LineTo(AxPos, Height - 1);
          end;
            { détermine le rectangle et son contenu }
          Rect:= Bounds(Pos, 0, Ranged(Column.Width), Height);
          if Rect.Left < Width then
          begin
            if Rect.Right > Width then
              Rect.Right:= Width;
            if csDesigning in ComponentState then
            begin
              Text:= Column.Caption;
              if Text = '' then
                Text:= Column.DataField;
            end else if Column.LabelKind = lkFieldValue then
            begin
              if Column.Field.IsBlob then
                Text:= Column.Field.AsString
              else
                Text:= Column.Field.DisplayText;
            end else
              Text:= Column.Caption;
            if Column.DefaultFont then
              Canvas.Font.Assign(Self.Font)
            else
              Canvas.Font.Assign(Column.Font);
            PaintToCanvas(Canvas, Rect, Column.Alignment, Column.VerticalAlignment,
              Text, Column.WordWrap);
            Pos:= Pos + Ranged(Column.Width);
          end;
        end;
      end;
    end;
  end;
  inherited Paint;
end;

procedure TQRCustomGrid.PaintToCanvas(aCanvas: TCanvas; aRect: TRect; aAl: TAlignment;
  aVertAlign: TVertAlign; aCap: string; WordWrap: Boolean);
var AlignOpt: Integer;
    VAlign: Integer;
    R: TRect;
begin
  FMargin:= Canvas.TextWidth('W') * (Size.Width / Width) / 3;
  case aAl of
    taLeftJustify:
    begin
      AlignOpt:= DT_LEFT;
      Inc(aRect.Left, Round((Width / Size.Width) * FMargin));
    end;
    taRightJustify:
    begin
      AlignOpt:= DT_RIGHT;
      Dec(aRect.Right, Round((Width / Size.Width) * FMargin));
    end;
    taCenter: AlignOpt:= DT_CENTER;
    else AlignOpt:= 0;
  end;
  case aVertAlign of
    vaCenter: VAlign:= DT_VCENTER;
    vaBottom: VAlign:= DT_BOTTOM;
    vaTop: VAlign:= DT_TOP;
    else VAlign:= 0;
  end;
  if WordWrap then
  begin
//    Inc(aRect.Top, Round((Width / Size.Width) * FMargin));
    R:= aRect;
    DrawText(aCanvas.Handle, PChar(aCap), Length(aCap), R, DT_NOPREFIX +
      VAlign + AlignOpt + DT_CALCRECT + DT_WORDBREAK);
    aRect.Top:= aRect.Top + Round(Abs(aRect.Bottom - aRect.Top) / 2 - Abs(R.Top - R.Bottom) / 2);
    DrawText(aCanvas.Handle, PChar(aCap), Length(aCap), aRect, DT_NOPREFIX +
      VAlign + AlignOpt + DT_WORDBREAK);
  end else
  begin
    Inc(AlignOpt, DT_SINGLELINE);
    DrawText(aCanvas.Handle, PChar(aCap), Length(aCap), aRect, DT_NOPREFIX +
      VAlign + AlignOpt);
  end;
end;

constructor TQRCustomGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns:= TQRGridColumns.Create(TQRGridColumn);
  FColumns.Grid:= Self;
  Frame.DrawTop:= True;
  Frame.DrawBottom:= True;
  Frame.DrawLeft:= True;
  Frame.DrawRight:= True;
  FSeparator:= True;
  FOldFontChange:= Font.OnChange;
  Font.OnChange:= FontChanged;
  FEmptyKind:= lkCustomLabel;
  FAutoSize:= True;
end;

destructor TQRCustomGrid.Destroy;
begin
  FColumns.Free;
  FColumns:= nil;
  inherited Destroy;
end;

procedure TQRCustomGrid.FontChanged(Sender: TObject);
var i: Integer;
begin
  if Assigned(FOldFontChange) then
    FOldFontChange(Sender);
  for i:= 0 to Columns.Count - 1 do
    if not (svWidth in Columns[i].FStoredValues) then
      Columns[i].FWidth:= Columns[i].GetDefaultWidth; 
end;

procedure TQRCustomGrid.AlignIt;
begin
  if AlignToBand and (not FAlignUpdating) then
  begin
    if Parent is TQRCustomBand then
    begin
      FAlignUpdating:= True;
      try
        Top:= 0;
        Height:= TQRCustomBand(Parent).Height;
        case Alignment of
          taLeftJustify: Left:= 0;
          taRightJustify: Left:= TQRCustomBand(Parent).Width - Width;
          taCenter: Left:= (TQRCustomBand(Parent).Width - Width) div 2;
        end;
      finally
        FAlignUpdating:= False;
      end;
    end;
  end;
end;

procedure TQRCustomGrid.SetDataSet(const Value: TDataSet);
var i: Integer;
begin
  if Value <> FDataSet then
  begin
    FDataSet:= Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then
    begin
      for i:= 0 to Columns.Count - 1 do
      begin
        with Columns.Items[i] do
        begin
          if not (svCaption in FStoredValues) then
            FCaption:= GetDefaultCaption;
          if not (svAlignment in FStoredValues) then
            FAlignment:= GetDefaultAlignment;
          if not (svWidth in FStoredValues) then
            FWidth:= GetDefaultWidth;
          Changed(False);
        end;
      end;
    end;
  end;
end;

procedure TQRCustomGrid.SetDrawSeparator(const Value: Boolean);
begin
  if FSeparator <> Value then
  begin
    FSeparator:= Value;
    Invalidate;
  end;
end;

function TQRCustomGrid.GetKind: TLabelKind;
var i: Integer;
begin
  if Columns.Count > 0 then
  begin
    Result:= Columns[0].LabelKind;
    for i:= 1 to Columns.Count - 1 do
      if Columns[i].LabelKind <> Result then
      begin
        if Result = lkCustomLabel then
          Result:= Columns[i].LabelKind
        else begin
          Result:= lkCustomLabel;
          Exit;
        end;
      end;
  end else
    Result:= FEmptyKind;
end;

procedure TQRCustomGrid.SetKind(Kind: TLabelKind);
var i: Integer;
begin
  if not (csLoading in ComponentState) then
    for i:= 0 to Columns.Count - 1 do
      Columns[i].LabelKind:= Kind;
  FEmptyKind:= Kind;
end;

procedure TQRCustomGrid.Loaded;
var i: Integer;
    Column: TQRGridColumn;
begin
  inherited Loaded;
  for i:= 0 to FColumns.Count - 1 do
  begin
    Column:= FColumns.Items[i];
    if not (svCaption in Column.FStoredValues) then
      Column.FCaption:= Column.GetDefaultCaption;
    if not (svAlignment in Column.FStoredValues) then
      Column.FAlignment:= Column.GetDefaultAlignment;
    if not (svWidth in Column.FStoredValues) then
      Column.FWidth:= Column.GetDefaultWidth;
    if Column.DefaultFont then
    begin
      Column.FFont.Assign(Font);
      Column.FDefaultFont := True;
    end;
  end;
  if AutoSize then
    AutoAdjustSize;
  Invalidate;
end;

procedure TQRCustomGrid.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    if Value then
      AutoAdjustSize;
    FAutoSize:= Value;
  end;
end;

procedure TQRCustomGrid.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  if AutoSize and not (csLoading in ComponentState) then
  begin
    AWidth:= Columns.VisibleWidth;
    if (AWidth = 0) and (csDesigning in ComponentState) then
      AWidth:= 64;
    inherited SetBounds(ALeft, ATop, AWidth, AHeight)
  end else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TQRCustomGrid.AutoAdjustSize;
begin
  Width:= Columns.VisibleWidth;
end;

procedure TQRCustomGrid.SetColumns(Columns: TQRGridColumns);
begin
  FColumns.Assign(Columns);
end;

{ TQRGridColumns }

function TQRGridColumns.GetVisibleWidth: Integer;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to Count - 1 do
    if Items[i].Visible then
      Result:= Result + Items[i].Width;
end;

function TQRGridColumns.GetOwner: TPersistent;
begin
  Result:= FGrid;
end;

function TQRGridColumns.GetItem(Idx: Integer): TQRGridColumn;
begin
  Result:= TQRGridColumn(inherited Items[Idx]);
end;

procedure TQRGridColumns.Update(Item: TCollectionItem);
begin
  if Assigned(FGrid) and (csDesigning in FGrid.ComponentState) then
    FGrid.Invalidate;
  if FGrid.AutoSize then
    FGrid.AutoAdjustSize;
end;

function TQRGridColumns.ColumnByName(const FieldName: string): TQRGridColumn;
var i: Integer;
begin
  for i:= 0 to Count - 1 do
    if CompareText(Items[i].DataField, FieldName) = 0 then
    begin
      Result:= Items[i];
      Exit;
    end;
  Result:= nil;
end;

{ TQRGridColumn }

constructor TQRGridColumn.Create(Collection: TCollection);
begin
  FVisible:= True;
  FVertAlign:= vaCenter;
  inherited Create(Collection);
  FKind:= Grid.LabelKind;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  DefaultFont := True;
end;

destructor TQRGridColumn.Destroy;
begin
  FFont.Free;
  FFont := nil;
  inherited;
end;

procedure TQRGridColumn.Assign(Source: TPersistent);
begin
  if Source is TQRGridColumn then
  begin
    GenericAssign(Source, Self);
    FFont.Assign(TQRGridColumn(Source).Font);
  end
  else
    inherited Assign(Self);
end;

function TQRGridColumn.Loading: Boolean;
begin
  Result:= Assigned(Collection) and Assigned(TQRGridColumns(Collection).FGrid) and
    (csLoading in TQRGridColumns(Collection).FGrid.ComponentState);
end;

procedure TQRGridColumn.RestoreDefaults;
begin
  FCaption:= GetDefaultCaption;
  FAlignment:= GetDefaultAlignment;
  FWidth:= GetDefaultWidth;
  FStoredValues:= [];
  DefaultFont := True;
  Changed(False);
end;

procedure TQRGridColumn.SetKind(Value: TLabelKind);
begin
  if Value <> FKind then
  begin
    FKind:= Value;
    if (Value = lkFieldValue) and (not (svCaption in FStoredValues)) and (not Loading) then
      FCaption:= '';
    if (Value = lkFieldLabel) and (not Loading) then
    begin
      FCaption:= GetDefaultCaption;
      Exclude(FStoredValues, svCaption);
    end;
    Changed(False);
  end;
end;

procedure TQRGridColumn.SetField(const FieldName: string);
begin
  if FieldName <> FDataField then
  begin
    FDataField:= FieldName;
    if not Loading then
    begin
      if not (svCaption in FStoredValues) then
        FCaption:= GetDefaultCaption;
      if not (svAlignment in FStoredValues) then
        FAlignment:= GetDefaultAlignment;
      if not (svWidth in FStoredValues) then
        FWidth:= GetDefaultWidth;
    end;
    Changed(False);
  end;
end;

function TQRGridColumn.GetField: TField;
begin
  Result:= nil;
  if Assigned(Grid.DataSet) then
    Result:= Grid.DataSet.FindField(DataField);
end;

function TQRGridColumn.GetGrid: TQRCustomGrid;
begin
  Result:= TQRCustomGrid(TQRGridColumns(Collection).GetOwner);
end;

function TQRGridColumn.GetLeft: Integer;
var i: Integer;
begin
  Result:= 0;
  if Visible then
    for i:= 0 to Self.Index - 1 do
      if Grid.Columns[i].Visible then
        Result:= Result + Grid.Columns[i].Width;
end;

function TQRGridColumn.GetDisplayName: string;
begin
  if Assigned(Grid.DataSet) and (DataField <> '') then
    Result:= Grid.DataSet.Name + '.' + DataField
  else
    Result:= inherited GetDisplayName;
end;

procedure TQRGridColumn.SetVisible(const Value: Boolean);
begin
  FVisible:= Value;
  Changed(False);
end;

{}

function TQRGridColumn.IsWidthStored: Boolean;
begin
  Result:= (svWidth in FStoredValues) and (FWidth <> GetDefaultWidth);
end;

function TQRGridColumn.IsAlignmentStored: Boolean;
begin
  Result:= (svAlignment in FStoredValues) and (FAlignment <> GetDefaultAlignment);
end;

function TQRGridColumn.IsCaptionStored: Boolean;
begin
  Result:= (svCaption in FStoredValues) and (FCaption <> GetDefaultCaption);
end;

function TQRGridColumn.GetDefaultWidth: Integer;
var W: Integer;
    RestoreCanvas: Boolean;
    TM: TTextMetric;
begin
  if (Grid = nil) or not Assigned(Field) then
  begin
    Result := 64;
    Exit;
  end;
  with Grid do
  begin
    RestoreCanvas := not HandleAllocated;
    if RestoreCanvas then
      Canvas.Handle := GetDC(0);
    try
      Canvas.Font := Font;
      GetTextMetrics(Canvas.Handle, TM);
      Result := (Field.DisplayWidth + 2) * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
      W := Canvas.TextWidth(Field.DisplayLabel) + 2 * Canvas.TextWidth('0');
      if Result < W then
        Result := W;
    finally
      if RestoreCanvas then
      begin
        ReleaseDC(0,Canvas.Handle);
        Canvas.Handle := 0;
      end;
    end;
  end;
end;

function TQRGridColumn.GetDefaultAlignment: TAlignment;
begin
  if Assigned(Field) then
    Result:= Field.Alignment
  else
    Result:= taLeftJustify;
end;

function TQRGridColumn.GetDefaultCaption: string;
begin
  if (LabelKind = lkFieldLabel) and Assigned(Field) then
    Result:= Field.DisplayLabel
  else
    Result:= '';
end;

function TQRGridColumn.GetDefaultValues: Boolean;
begin
  Result:= FStoredValues = [];
end;

procedure TQRGridColumn.SetWidth(const Width: Integer);
begin
  if (Width <> FWidth) or Loading then
  begin
    FWidth := Width;
    Include(FStoredValues, svWidth);
  end;
  Changed(False);
end;

procedure TQRGridColumn.SetAlignment(const Alignment: TAlignment);
begin
  if (Alignment <> FAlignment) or Loading then
  begin
    FAlignment := Alignment;
    Include(FStoredValues, svAlignment);
  end;
  Changed(False);
end;

procedure TQRGridColumn.SetVertAlign(const VertAlign: TVertAlign);
begin
  FVertAlign:= VertAlign;
  Changed(False);
end;

procedure TQRGridColumn.SetWordWrap(const Value: Boolean);
begin
  FWordWrap:= Value;
  Changed(False);
end;

procedure TQRGridColumn.SetCaption(const Caption: string);
begin
  if (Caption <> FCaption) or Loading then
  begin
    FCaption := Caption;
    Include(FStoredValues, svCaption);
  end;
  Changed(False);
end;

procedure TQRGridColumn.SetDefaultValues(const Value: Boolean);
begin
  if Value then
    RestoreDefaults;
end;

procedure TQRGridColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if csLoading in Grid.ComponentState then
    Exit;
  FDefaultFont := False;
  Changed(False);
end;

procedure TQRGridColumn.SetDefaultFont(const Value: Boolean);
begin
  if Value then
    FFont.Assign(Grid.Font);
  FDefaultFont := Value;
  Changed(False);
end;

procedure TQRGridColumn.FontChange(Sender: TObject);
begin
  if csLoading in Grid.ComponentState then
    Exit;
  DefaultFont := False;
  Changed(False);
end;

end.
