unit RoundButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, System.Types,
  StdCtrls, Menus, Buttons, dialogs;

type
  TEtatsButton = (ebDesactive, ebRepos, ebEnfonce, ebDestroying);
  TEtatButton = set of TEtatsButton;
  TCoinButton = (cbOutCoin, cbInCoin, cbOutRond, cbInRond, cbPlat);
  TMenuPosition = (mpLeft, mpRight, mpLienLeft, mpLienRight);
  TOrdreAction = (oaClickFirst, oaMenuFirst);
  TPriorityAction = (paAll, paClickOnly, paMenuOnly);

  TRoundButton = class(TGraphicControl)
  private
    FEtat: TEtatButton;
    FCaption, FRollOverCaption: string;
    FFont,    FRollOverFont: TFont;
    FLayout: TButtonLayout;
    FPopup: TPopupMenu;
    FGlyph: TBitmap;
    FNumGlyphs: TNumGlyphs;
    FTransparentColor: TColor;
    FLstBMP: Tlist;
    FMargin: Integer;
    FSpacing: Integer;
    FAngle: Integer;
    FOrdre: TOrdreAction;
    FPriority: TPriorityAction;
    FTopLeft, FTopRight, FBottomLeft, FBottomRight: TCoinButton;
    FLien: TRoundButton;
    SenderEvent: TRoundButton;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FMenuPosition: TMenuPosition;
    FRgnBtn: HRGN;
    FColor: TColor;
    FTransparent: Boolean;
    procedure SetAngle(Value: Integer);
    procedure SetCaption(Index: Integer; Value: string);
    procedure SetFont(Index: Integer; Value: TFont);
    procedure SetGlyph(Value: TBitmap);
    procedure SetMargin(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetListeBMP(Value: TBitmap; NbGlyph: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetCoins(Index: Integer; Value: TCoinButton);
    procedure SetLien(Value: TRoundButton);
    procedure SetColor(Value: TColor);
    procedure SetTransparent(Value: Boolean);
    procedure GetPos(const Caption: string; Img: TBitmap; var GlyphPos: TPoint; var TextBounds: TRect);
    function DrawState: integer;
    procedure PictureChanged(Sender: TObject);
    procedure DoMouseMove(Sender: TRoundButton; Shift: TShiftState; X, Y: Integer);
    procedure DoCMMouseEnter(Sender: TRoundButton; var Message: TMessage);
    procedure DoCMMouseLeave(Sender: TRoundButton; var Message: TMessage);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function PtOnBtn(Position: TPoint): Boolean;
    function MouseOnBtn: Boolean;
  public
    UsedAngle: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property CoinTopLeft: TCoinButton index 0 read FTopLeft write SetCoins;
    property CoinTopRight: TCoinButton index 1 read FTopRight write SetCoins;
    property CoinBottomLeft: TCoinButton index 2 read FBottomLeft write SetCoins;
    property CoinBottomRight: TCoinButton index 3 read FBottomRight write SetCoins;
    property Rayon: Integer read FAngle write SetAngle default 10;
    property Caption: string index 0 read FCaption write SetCaption;
    property RollOverCaption: string index 1 read FRollOverCaption write SetCaption;
    property Font: TFont index 0 read FFont write SetFont;
    property RollOverFont: TFont index 1 read FRollOverFont write SetFont;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Popup: TPopupMenu read FPopup write FPopup;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default -1;
    property Ordre: TOrdreAction read FOrdre write FOrdre default oaMenuFirst;
    property Priority: TPriorityAction read FPriority write FPriority default paAll;
    property Lien: TRoundButton read FLien write SetLien default nil;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property MenuPosition: TMenuPosition read FMenuPosition write FMenuPosition;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TRoundButton]);
end;

procedure DisposeLst(Liste: TList);
var
  i: Integer;
  B: TBitmap;
begin
  for i := Liste.Count -1 downto 0 do begin
    if Assigned(Liste.Items[i]) then begin
      B := TBitmap(Liste.Items[i]);
      B.Free;
    end;
    Liste.Delete(i);
  end;
end;

constructor TRoundButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDesignInteractive, csSetCaption, csReplicatable];
  ControlStyle := ControlStyle - [csOpaque];
  Height := 25;
  Width := 75;
  FAngle := 10;
  FCaption := Name;
  FRollOverCaption := '';
  FFont := TFont.Create;
  FRollOverFont := TFont.Create;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := PictureChanged;
  FLstBMP := TList.Create;
  FEtat := [ebRepos];
  FMargin := -1;
  FSpacing := -1;
  FOrdre := oaMenuFirst;
  FPriority := paAll;
  FTopLeft := cbOutRond;
  FTopRight := cbOutRond;
  FBottomLeft := cbOutRond;
  FBottomRight := cbOutRond;
  FColor := clBtnFace;
  FTransparent := True;
end;

destructor TRoundButton.Destroy;
begin
  SetLien(nil);
  DisposeLst(FLstBMP);
  FLstBMP.Free;
  FGlyph.Free;
  FRollOverFont.Free;
  FFont.Free;
  inherited Destroy;
end;

function TRoundButton.PtOnBtn(Position: TPoint): Boolean;
begin
  Result := False;
  if FRgnBtn = 0 then Exit;
  Result := PtInRegion(FRgnBtn, Position.x, Position.y);
end;

function TRoundButton.MouseOnBtn: Boolean;
begin
  Result := PtOnBtn(ScreenToClient(Mouse.CursorPos));
end;

procedure TRoundButton.SetColor(Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  if FTransparent then Invalidate;
end;

procedure TRoundButton.SetTransparent(Value: Boolean);
begin
  if FTransparent = Value then Exit;
  FTransparent := Value;
  Invalidate;
end;

procedure TRoundButton.SetCaption(Index: Integer; Value: string);
begin
  case index of
    0:
      begin
        if FCaption = Value then Exit;
        FCaption := Value;
      end;
    1:
      begin
        if FRollOverCaption = Value then Exit;
        FRollOverCaption := Value;
      end;
  end;
  invalidate;
end;

procedure TRoundButton.SetCoins(Index: Integer; Value: TCoinButton);
var
  f: ^TCoinButton;
begin
  case Index of
    0: f := @FTopLeft;
    1: f := @FTopRight;
    2: f := @FBottomLeft;
    3: f := @FBottomRight;
    else raise Exception.Create('Erreur interne');
  end;
  if f^ = Value then Exit;
  f^ := Value;
  invalidate;
end;

procedure TRoundButton.SetLien(Value: TRoundButton);
begin
  if Value = FLien then Exit;
  if (Value = Self) then raise Exception.Create('Références circulaires interdites');
  if Assigned(FLien) then FLien.FLien := nil;
  FLien := Value;
  if Assigned(FLien) then FLien.Lien := Self;
end;

procedure TRoundButton.SetAngle(Value: Integer);
begin
  // aucune vérification car c'est une valeur calculée sur FAngle qui est utilisée
  if {(Value < 0) or (Value > Height div 2) or (Value > Width div 2) or }(Value = FAngle) then Exit;
  FAngle := Value;
  invalidate;
end;

procedure TRoundButton.SetFont(Index: Integer; Value: TFont);
begin
  case index of
    0: FFont.Assign(Value);
    1: FRollOverFont.Assign(Value);
  end;
  invalidate;
end;

procedure TRoundButton.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  PictureChanged(FGlyph);
end;

procedure TRoundButton.PictureChanged(Sender: TObject);
var
  g: Integer;
begin
  FTransparentColor := TBitmap(Sender).TransparentColor;
  g := TBitmap(Sender).Width div TBitmap(Sender).Height;
  if g > 4 then g := 4;
  if g < 1 then g := 1;
  SetNumGlyphs(g);
end;

procedure TRoundButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  FNumGlyphs := Value;
  SetListeBMP(FGlyph, FNumGlyphs);
  invalidate;
end;

procedure TRoundButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout = Value then Exit;
  FLayout := Value;
  Invalidate;
end;

procedure TRoundButton.SetMargin(Value: Integer);
begin
  if Value = FMargin then Exit;
  FMargin := value;
  invalidate;
end;

procedure TRoundButton.SetSpacing(Value: Integer);
begin
  if Value = FSpacing then Exit;
  FSpacing := value;
  invalidate;
end;

procedure TRoundButton.SetListeBMP(Value: TBitmap; NbGlyph: Integer);
var
  i: Integer;
  Target, Source: TRect;
  B: TBitmap;
begin
  DisposeLst(FLstBMP);
  for i := 1 to NbGlyph do begin
    B := TBitmap.Create;
    B.Height := Value.Height;
    B.Width := Value.Width div NbGlyph;
    Target := Rect(0, 0, B.Width, B.Height);
    Source := Rect(B.Width * (i - 1), 0, B.Width * i, B.Height);
    B.Canvas.CopyRect(Target, Value.Canvas, Source);
    FLstBMP.Add(B);
  end;
end;

procedure TRoundButton.GetPos(const Caption: string; Img: TBitmap; var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  mrg, spc: integer;
begin
  mrg := FMargin;
  spc := FSpacing;
  { calculate the item sizes }
  ClientSize := Point(ClientRect.Right - ClientRect.Left, ClientRect.Bottom - ClientRect.Top);

  if Assigned(img) then GlyphSize := Point(img.Width, img.Height)
                   else GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then begin
    TextBounds := Rect(0, 0, ClientRect.Right - ClientRect.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end else begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then Spacing := 0;

  { adjust Margin and Spacing }
  if Mrg = -1 then begin
    if Spc = -1 then begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then Mrg := (ClientSize.X - TotalSize.X) div 3
                                               else Mrg := (ClientSize.Y - TotalSize.Y) div 3;
      Spc := Mrg;
    end else begin
        TotalSize := Point(GlyphSize.X + Spc + TextSize.X, GlyphSize.Y + Spc + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then Mrg := (ClientSize.X - TotalSize.X + 1) div 2
                                               else Mrg := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end else begin
    if Spc = -1 then begin
      TotalSize := Point(ClientSize.X - (Mrg + GlyphSize.X), ClientSize.Y - Mrg + GlyphSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then Spc := (TotalSize.X - TextSize.X) div 2
                                               else Spc := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Mrg;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spc;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Mrg - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spc - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Mrg;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spc;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Mrg - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spc - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  with GlyphPos do begin
    Inc(X, ClientRect.Left);
    Inc(Y, ClientRect.Top);
  end;
  OffsetRect(TextBounds, TextPos.X + ClientRect.Left, TextPos.Y + ClientRect.Top);
  if (ebEnfonce in FEtat) then OffsetRect(TextBounds, 1, 1);
end;

procedure TRoundButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Include(FEtat, ebEnfonce);
  if Enabled then invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRoundButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Exclude(FEtat, ebEnfonce);
  if Enabled then invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TRoundButton.DoMouseMove(Sender: TRoundButton; Shift: TShiftState; X, Y: Integer);
begin
  SenderEvent := Sender;
  MouseMove(Shift, X, Y);
  SenderEvent := nil;
end;

procedure TRoundButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Invalid: Boolean;
  Source: TRoundButton;
begin
  if Assigned(SenderEvent) then Source := SenderEvent
                           else Source := Self;

  Invalid := ebRepos in FEtat;
  if Source.MouseOnBtn then Exclude(FEtat, ebRepos)
                       else begin
                         Invalid := not Invalid;
                         Include(FEtat, ebRepos);
                       end;

//  if ebRepos in FEtat then begin
//    Exclude(FEtat, ebRepos);
    if Enabled and Invalid then Invalidate;
    if Assigned(FLien) and not Assigned(SenderEvent) then FLien.DoMouseMove(Self, Shift, X, Y);
//    if Assigned(FLien) and not MouseCapture then FLien.MouseMove(Shift, X, Y);
//  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TRoundButton.DoCMMouseEnter(Sender: TRoundButton; var Message: TMessage);
begin
  SenderEvent := Sender;
  CMMouseEnter(Message);
  SenderEvent := nil;
end;

procedure TRoundButton.CMMouseEnter(var Message: TMessage);
var
  Source: TRoundButton;
begin
  if Assigned(SenderEvent) then Source := SenderEvent
                           else Source := Self;
  if Source.MouseOnBtn then Exclude(FEtat, ebRepos);
  if Enabled and not (ebRepos in FEtat) then Invalidate;
  if not Assigned(SenderEvent) then begin
    if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    if Assigned(FLien) then FLien.DoCMMouseEnter(Self, Message);
  end;
//  if Assigned(FLien) and not MouseCapture then FLien.CMMouseEnter(Message);
  inherited;
end;

procedure TRoundButton.DoCMMouseLeave(Sender: TRoundButton; var Message: TMessage);
begin
  SenderEvent := Sender;
  CMMouseLeave(Message);
  SenderEvent := nil;
end;

procedure TRoundButton.CMMouseLeave(var Message: TMessage);
begin
  Include(FEtat, ebRepos);
  if Enabled then invalidate;
  if not Assigned(SenderEvent) then begin
    if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
    if Assigned(FLien) then FLien.DoCMMouseLeave(Self, Message);
  end;
  inherited;
end;

procedure TRoundButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Enabled then Exclude(FEtat, ebDesactive)
             else Include(FEtat, ebDesactive);
  Invalidate;
end;

function TRoundButton.DrawState: integer;
begin
  if ebDesactive in FEtat then Result := 1
  else begin
    if ebRepos in FEtat then Result := 2
                        else Result := 3;
    if ebEnfonce in FEtat then Result := 4;
  end;
end;

procedure TRoundButton.Paint;

  procedure DessineBouton(Coul1, Coul2: TColor);
  var
    p1, p2: TPoint;
    TopLeft, TopRight, BottomLeft, BottomRight: Integer;
    RgnTemp: HRgn;
    RgnTopLeft, RgnTopRight, RgnBottomLeft, RgnBottomRight: HRgn;
    ArrayPoint: array of TPoint;
  begin
    UsedAngle := FAngle;
    if (UsedAngle > Height div 2) then UsedAngle := Height div 2;
    if (UsedAngle > Width div 2) then UsedAngle := Width div 2;

    p1 := ClientRect.TopLeft;
    p2.x := ClientRect.BottomRight.x - 1;
    p2.y := ClientRect.BottomRight.y - 1;

    if FRgnBtn <> 0 then DeleteObject(FRgnBtn);
    FRgnBtn := CreateRectRgn(p1.x + UsedAngle, p1.y, p2.x - UsedAngle, p2.y);
    RgnTemp := CreateRectRgn(p1.x, p1.y + UsedAngle, p2.x, p2.y - UsedAngle);
    CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);

    RgnTopLeft := CreateRectRgn(p1.x, p1.y, p1.x + UsedAngle, p1.y + UsedAngle);
    RgnTopRight := CreateRectRgn(p2.x, p1.y, p2.x - UsedAngle, p1.y + UsedAngle);
    RgnBottomLeft := CreateRectRgn(p1.x, p2.y, p1.x + UsedAngle, p2.y - UsedAngle);
    RgnBottomRight := CreateRectRgn(p2.x, p2.y, p2.x - UsedAngle, p2.y - UsedAngle);

    if FTopLeft = cbOutCoin then TopLeft := 0
                            else TopLeft := 1;
    if FTopRight = cbOutCoin then TopRight := 0
                             else TopRight := 1;
    if FBottomLeft = cbOutCoin then BottomLeft := 0
                               else BottomLeft := 1;
    if FBottomRight = cbOutCoin then BottomRight := 0
                                else BottomRight := 1;

    with Canvas do begin
      Pen.Color := Coul1;
      MoveTo(p1.x, p2.y - UsedAngle * BottomLeft);
      LineTo(p1.x, p1.y + UsedAngle * TopLeft - 1);
      MoveTo(p1.x + UsedAngle * TopLeft, p1.y);
      LineTo(p2.x - UsedAngle * TopRight, p1.y);

      if FTopLeft = cbOutRond then begin
        Arc(p1.x, p1.y, p1.x + 2 * (UsedAngle + 1), p1.y + 2 * (UsedAngle + 1), p1.x + (UsedAngle + 1), p1.y, p1.x, p1.y + (UsedAngle + 1) + 1);
        RgnTemp := CreateEllipticRgn(p1.x, p1.y, p1.x + 2 * (UsedAngle + 1), p1.y + 2 * (UsedAngle + 1));
        CombineRgn(RgnTemp, RgnTemp, RgnTopLeft, RGN_AND);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FTopRight = cbOutRond then begin
        Arc(p2.x + 1, p1.y, p2.x - 2 * UsedAngle, p1.y + 2 * (UsedAngle + 1), p2.x + 1, p1.y, p2.x - UsedAngle, p1.y);
        RgnTemp := CreateEllipticRgn(p2.x + 1, p1.y, p2.x - 2 * UsedAngle, p1.y + 2 * (UsedAngle + 1));
        CombineRgn(RgnTemp, RgnTemp, RgnTopRight, RGN_AND);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FBottomLeft = cbOutRond then begin
        Arc(p1.x, p2.y + 1, p1.x + 2 * (UsedAngle + 1), p2.y - 2 * UsedAngle, p1.x, p2.y - UsedAngle, p1.x, p2.y + 1);
        RgnTemp := CreateEllipticRgn(p1.x, p2.y + 1, p1.x + 2 * (UsedAngle + 1), p2.y - 2 * UsedAngle);
        CombineRgn(RgnTemp, RgnTemp, RgnBottomLeft, RGN_AND);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;

      if FTopLeft = cbInCoin then begin
        MoveTo(p1.x + UsedAngle, p1.y);
        LineTo(p1.x + UsedAngle, p1.y + UsedAngle);
        LineTo(p1.x, p1.y + UsedAngle);
      end;
      if FBottomLeft = cbInCoin then begin
        MoveTo(p1.x + UsedAngle, p2.y);
        LineTo(p1.x + UsedAngle, p2.y - UsedAngle);
      end;
      if FTopRight = cbInCoin then begin
        MoveTo(p2.x, p1.y + UsedAngle);
        LineTo(p2.x - UsedAngle, p1.y + UsedAngle);
      end;

      if FTopLeft = cbPlat then begin
        MoveTo(p1.x + UsedAngle, p1.y);
        LineTo(p1.x, p1.y + UsedAngle);
        SetLength(ArrayPoint, 3);
        ArrayPoint[0] := Point(p1.x + UsedAngle, p1.y + UsedAngle);
        ArrayPoint[1] := Point(p1.x + UsedAngle, p1.y);
        ArrayPoint[2] := Point(p1.x, p1.y + UsedAngle);
        RgnTemp := CreatePolygonRgn(ArrayPoint[0], 3, WINDING);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;

      if FTopLeft = cbInRond then begin
        Arc(p1.x - UsedAngle + 1, p1.y - UsedAngle + 1, p1.x + UsedAngle + 1, p1.y + UsedAngle + 1, p1.x, p1.y + (UsedAngle + 1) + 1, p1.x + (UsedAngle + 1), p1.y);
        RgnTemp := CreateEllipticRgn(p1.x - UsedAngle + 1, p1.y - UsedAngle + 1, p1.x + UsedAngle + 1, p1.y + UsedAngle + 1);
        CombineRgn(RgnTemp, RgnTopLeft, RgnTemp, RGN_DIFF);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FTopRight = cbInRond then begin
        Arc(p2.x + UsedAngle, p1.y - UsedAngle + 1, p2.x - UsedAngle, p1.y + UsedAngle + 1, p2.x - UsedAngle, p1.y + UsedAngle + 1, p2.x, p1.y + UsedAngle + 1);
        RgnTemp := CreateEllipticRgn(p2.x + UsedAngle, p1.y - UsedAngle + 1, p2.x - UsedAngle, p1.y + UsedAngle + 1);
        CombineRgn(RgnTemp, RgnTopRight, RgnTemp, RGN_DIFF);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FBottomLeft = cbInRond then begin
        Arc(p1.x - UsedAngle + 1, p2.y + UsedAngle, p1.x + UsedAngle + 1, p2.y - UsedAngle, p1.x + UsedAngle, p2.y, p1.x + UsedAngle, p2.y - UsedAngle);
        RgnTemp := CreateEllipticRgn(p1.x - UsedAngle + 1, p2.y + UsedAngle, p1.x + UsedAngle + 1, p2.y - UsedAngle);
        CombineRgn(RgnTemp, RgnBottomLeft, RgnTemp, RGN_DIFF);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;

      Pen.Color := Coul2;
      MoveTo(p2.x, p2.y - UsedAngle * BottomRight);
      LineTo(p2.x, p1.y + UsedAngle * TopRight);
      MoveTo(p1.x + UsedAngle * BottomLeft, p2.y);
      LineTo(p2.x - UsedAngle * BottomRight, p2.y);

      if FBottomRight = cbOutRond then begin
        Arc(p2.x - 2 * UsedAngle, p2.y - 2 * UsedAngle, p2.x + 1, p2.y + 1, p2.x - UsedAngle, p2.y, p2.x, p2.y - UsedAngle);
        RgnTemp := CreateEllipticRgn(p2.x - 2 * UsedAngle, p2.y - 2 * UsedAngle, p2.x + 1, p2.y + 1);
        CombineRgn(RgnTemp, RgnTemp, RgnBottomRight, RGN_AND);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FTopRight = cbOutRond then Arc(p2.x + 1, p1.y, p2.x - 2 * UsedAngle, p1.y + 2 * (UsedAngle + 1), p2.x + 1, p1.y + (UsedAngle + 1), p2.x, p1.y);
      if FBottomLeft = cbOutRond then Arc(p1.x, p2.y + 1, p1.x + 2 * (UsedAngle + 1), p2.y - 2 * UsedAngle, p1.x, p2.y, p1.x + (UsedAngle + 1), p2.y + 1);

      if FBottomRight = cbInCoin then begin
        MoveTo(p2.x - UsedAngle, p2.y);
        LineTo(p2.x - UsedAngle, p2.y - UsedAngle);
        LineTo(p2.x, p2.y - UsedAngle);
      end;
      if FBottomLeft = cbInCoin then begin
        MoveTo(p1.x, p2.y - UsedAngle);
        LineTo(p1.x + UsedAngle, p2.y - UsedAngle);
      end;
      if FTopRight = cbInCoin then begin
        MoveTo(p2.x - UsedAngle, p1.y);
        LineTo(p2.x - UsedAngle, p1.y + UsedAngle);
      end;

      if FBottomRight = cbPlat then begin
        MoveTo(p2.x - UsedAngle, p2.y);
        LineTo(p2.x, p2.y - UsedAngle);
        SetLength(ArrayPoint, 3);
        ArrayPoint[0] := Point(p2.x - UsedAngle, p2.y - UsedAngle);
        ArrayPoint[1] := Point(p2.x - UsedAngle, p2.y);
        ArrayPoint[2] := Point(p2.x, p2.y + UsedAngle);
        RgnTemp := CreatePolygonRgn(ArrayPoint[0], 3, WINDING);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FTopRight = cbPlat then begin
        MoveTo(p2.x, p1.y + UsedAngle);
        LineTo(p2.x - UsedAngle, p1.y);
        SetLength(ArrayPoint, 3);
        ArrayPoint[0] := Point(p2.x - UsedAngle, p1.y + UsedAngle);
        ArrayPoint[1] := Point(p2.x - UsedAngle, p1.y);
        ArrayPoint[2] := Point(p2.x, p1.y + UsedAngle);
        RgnTemp := CreatePolygonRgn(ArrayPoint[0], 3, WINDING);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FBottomLeft = cbPlat then begin
        MoveTo(p1.x, p2.y - UsedAngle);
        LineTo(p1.x + UsedAngle, p2.y);
        SetLength(ArrayPoint, 3);
        ArrayPoint[0] := Point(p1.x + UsedAngle, p2.y - UsedAngle);
        ArrayPoint[1] := Point(p1.x + UsedAngle, p2.y);
        ArrayPoint[2] := Point(p1.x, p2.y - UsedAngle);
        RgnTemp := CreatePolygonRgn(ArrayPoint[0], 3, WINDING);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;

      if FBottomRight = cbInRond then begin
        Arc(p2.x - UsedAngle, p2.y - UsedAngle, p2.x + UsedAngle + 1, p2.y + UsedAngle + 1, p2.x, p2.y - UsedAngle, p2.x - UsedAngle, p2.y + 1);
        RgnTemp := CreateEllipticRgn(p2.x - UsedAngle, p2.y - UsedAngle, p2.x + UsedAngle + 1, p2.y + UsedAngle + 1);
        CombineRgn(RgnTemp, RgnBottomRight, RgnTemp, RGN_DIFF);
        CombineRgn(FRgnBtn, FRgnBtn, RgnTemp, RGN_OR);
      end;
      if FTopRight = cbInRond then Arc(p2.x + UsedAngle, p1.y - UsedAngle + 1, p2.x - UsedAngle, p1.y + UsedAngle + 1, p2.x - UsedAngle, p1.y, p2.x - UsedAngle, p1.y + UsedAngle + 1);
      if FBottomLeft = cbInRond then Arc(p1.x - UsedAngle + 1, p2.y + UsedAngle, p1.x + UsedAngle + 1, p2.y - UsedAngle, p1.x + UsedAngle, p2.y - UsedAngle, p1.x, p2.y - UsedAngle);


      if FTopLeft = cbOutCoin then CombineRgn(FRgnBtn, FRgnBtn, RgnTopLeft, RGN_OR);
      if FTopRight = cbOutCoin then CombineRgn(FRgnBtn, FRgnBtn, RgnTopRight, RGN_OR);
      if FBottomLeft = cbOutCoin then CombineRgn(FRgnBtn, FRgnBtn, RgnBottomLeft, RGN_OR);
      if FBottomRight = cbOutCoin then CombineRgn(FRgnBtn, FRgnBtn, RgnBottomRight, RGN_OR);

      Brush.Style := bsSolid;
      Brush.Color := FColor;
      if not FTransparent then FillRgn(Handle, FRgnBtn, Brush.Handle);
    end;  // with Canvas
    if RgnTemp <> 0 then DeleteObject(RgnTemp);
    if RgnTopLeft <> 0 then DeleteObject(RgnTopLeft);
    if RgnTopRight <> 0 then DeleteObject(RgnTopRight);
    if RgnBottomLeft <> 0 then DeleteObject(RgnBottomLeft);
    if RgnBottomRight <> 0 then DeleteObject(RgnBottomRight);
  end;

  procedure DrawButtonText(const Caption: string; TextBounds: TRect);
  begin
    with Canvas do
    begin
      Brush.Style := bsClear;
      if not Enabled then begin
        OffsetRect(TextBounds, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER);
        OffsetRect(TextBounds, -1, -1);
        Font.Color := clBtnShadow;
        DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER);
      end else
        DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER);
    end;
  end;

var
  PosGlyph: TPoint;
  SCaption: String;
  imIndex: Integer;
  GlyphToDraw: TBitmap;
  TargetText, Target, Source: TRect;
begin
  SCaption := '';
  GlyphToDraw := nil;
  if FLstBMP.Count > 0 then imIndex := 0
                       else ImIndex := -1;
  with Canvas do begin
    Brush.Style := bsClear;
    if csDesigning in ComponentState then begin
      SCaption := FCaption;
      DessineBouton(clWhite, clDkGray);
      Font.Assign(FFont);
      if ebDesactive in FEtat then begin
        Font.Color := clInactiveCaption;
        if FLstBMP.Count > 1 then imIndex := 1;
      end else
        if FLstBMP.Count > 0 then imIndex := 0;
    end else
    case DrawState of
      1:  // désactivé
        begin
          if FLstBMP.Count > 1 then imIndex := 1;
          SCaption := FCaption;
          Font.Assign(FFont);
          Font.Color := clInactiveCaption;
        end;
      2:  // repos
        begin
          if FLstBMP.Count > 0 then imIndex := 0;
          SCaption := FCaption;
          Font.Assign(FFont);
        end;
      3:  // actif
        begin
          if FLstBMP.Count > 2 then imIndex := 2;
          DessineBouton(clWhite, clDkGray);
          SCaption := FRollOverCaption;
          Font.Assign(FRollOverFont);
        end;
      else  // appuyé
        begin
          if FLstBMP.Count > 3 then imIndex := 3;
          DessineBouton(clDkGray, clWhite);
          SCaption := FRollOverCaption;
          Font.Assign(FRollOverFont);
        end;
    end;
    if SCaption = '' then SCaption := FCaption;
    if (imIndex <> -1) then GlyphToDraw := TBitmap(FLstBMP.Items[imIndex]);
    GetPos(SCaption, GlyphToDraw, PosGlyph, TargetText);
    DrawButtonText(SCaption, TargetText);
    Brush.Color := FTransparentColor;
    Brush.Style := bsClear;
    if Assigned(GlyphToDraw) and (not GlyphToDraw.Empty) then begin
      Source := Rect(0, 0, GlyphToDraw.Width, GlyphToDraw.Height);
      Target := Rect(PosGlyph.x, PosGlyph.y, PosGlyph.x + GlyphToDraw.Width, PosGlyph.y + GlyphToDraw.Height);
      BrushCopy(Target, GlyphToDraw, Source, FTransparentColor);
    end;
  end;
end;

procedure TRoundButton.Click;
  procedure DoPopup;
  var
    p: TPoint;
    x, y: integer;
  begin
    if not Assigned(FPopup) then Exit;
    Include(FEtat, ebRepos);
    case FMenuPosition of
//      mpRight:
//        begin
//          x := 0;
//          y := Height;
//        end;
      mpLienLeft:
        begin
          if Assigned(FLien) then begin
            x := FLien.Left - Left;
            y := FLien.Top - Top + FLien.Height;
          end else begin
            x := 0;
            y := Height;
          end;
        end;
//      mpLienRight:
//        begin
//          x := 0;
//          y := Height;
//        end;
      else
        begin
          x := 0;
          y := Height;
        end;
    end;
    p := ClientToScreen(Point(x, y));
    FPopup.PopupComponent := Self;
    FPopup.Popup(p.x, p.y);
    if Assigned(FLien) then begin
      Include(FLien.FEtat, ebRepos);
      Flien.Invalidate;
    end;
  end;
begin
  if FOrdre = oaClickFirst then begin
    if FPriority in [paAll, paClickOnly] then inherited Click;
    if FPriority in [paAll, paMenuOnly] then DoPopup;
  end else begin  // oaMenuFirst
    if FPriority in [paAll, paMenuOnly] then DoPopup;
    if FPriority in [paAll, paClickOnly] then inherited Click;
  end;
end;

end.
