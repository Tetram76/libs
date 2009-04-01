unit ProgressBar;

interface

uses Windows, Controls, Graphics, Classes, Messages, SysUtils;

type
  TMKBevelOuter = (bvNone, bvLowered, bvRaised);
  TMKPBOrientation = (orVertical, orHorizontal);
  TMKShowTextStyle = (stsPercent, stsPosition);
  TMKPBStyle = (sSolid, sRectangles, sExSolid, sExRectangles, sPictures);

  TMKProgressBar = class(TCustomControl)
  private
    FBevelOuter: TMKBevelOuter;
    FBarBevelOuter: TMKBevelOuter;
    FOrientation: TMKPBOrientation;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FShowText: Boolean;
    FShowTextStyle: TMKShowTextStyle;
    FBeginColor: TColor;
    FEndColor: TColor;
    FStyle: TMKPBStyle;
    FGlyph: TBitmap;
    FStep: Integer;
    FTransparentGlyph: Boolean;
    procedure SetBevelOuter(Value: TMKBevelOuter);
    procedure SetBarBevelOuter(Value: TMKBevelOuter);
    procedure SetOrientation(Value: TMKPBOrientation);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetShowText(Value: Boolean);
    procedure SetShowTextStyle(Value: TMKShowTextStyle);
    procedure SetBeginColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetStyle(Value: TMKPBStyle);
    procedure SetGlyph(Value: TBitmap);
    procedure SetStep(Value: Integer);
    procedure InvalidatePosition(OldPosition: Integer);
    procedure InvalidateText;
    procedure SetTransparentGlyph(Value: Boolean);
  protected
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StepBy(Delta: Integer);
    procedure StepIt;
  published
    property BarBevelOuter: TMKBevelOuter read FBarBevelOuter write SetBarBevelOuter;
    property BevelOuter: TMKBevelOuter read FBevelOuter write SetBevelOuter;
    property Orientation: TMKPBOrientation read FOrientation write SetOrientation;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Position: Integer read FPosition write SetPosition;
    property ShowText: Boolean read FShowText write SetShowText default False;
    property ShowTextStyle: TMKShowTextStyle read FShowTextStyle write SetShowTextStyle;
    property BeginColor: TColor read FBeginColor write SetBeginColor;
    property EndColor: TColor read FEndColor write SetEndColor;
    property Style: TMKPBStyle read FStyle write SetStyle;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Step: Integer read FStep write SetStep;
    property TransparentGlyph: Boolean read FTransparentGlyph write SetTransparentGlyph;

    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Hint;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TMKProgressBar]);
end;

function GetColor(BeginColor, EndColor: TColor; N, H: Integer): TColor;
var
  BColor, EColor: Longint;
begin
  BColor := ColorToRGB(BeginColor); // sinon, les calculs ne marchent pas avec clInactiveCaption et consors
  EColor := ColorToRGB(EndColor); // id.
  Result := RGB(Trunc(GetRValue(BColor) + (GetRValue(EColor) - GetRValue(BColor)) * N / H),
    Trunc(GetGValue(BColor) + (GetGValue(EColor) - GetGValue(BColor)) * N / H),
    Trunc(GetBValue(BColor) + (GetBValue(EColor) - GetBValue(BColor)) * N / H));
end;

{TMKProgressBar}

constructor TMKProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 16;
  FBevelOuter := bvLowered;
  FBarBevelOuter := bvNone;
  FOrientation := orHorizontal;
  FMax := 100;
  FMin := 0;
  FPosition := 0;
  FShowTextStyle := stsPercent;
  FShowText := False;
  FBeginColor := clNavy;
  FEndColor := clWhite;
  FStyle := sRectangles;
  FStep := 10;
  FGlyph := TBitmap.Create;
  FTransparentGlyph := True;
end;

destructor TMKProgressBar.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TMKProgressBar.SetTransparentGlyph(Value: Boolean);
begin
  if Value = FTransparentGlyph then Exit;
  FTransparentGlyph := Value;
  FGlyph.Transparent := Value;
  Invalidate;
end;

procedure TMKProgressBar.Loaded;
begin
  inherited;
  if not FGlyph.Empty then FGlyph.Transparent := FTransparentGlyph;
  Invalidate;
end;

procedure TMKProgressBar.SetBevelOuter;
begin
  if FBevelOuter = Value then Exit;
  FBevelOuter := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetBarBevelOuter;
begin
  if FBarBevelOuter = Value then Exit;
  FBarBevelOuter := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetOrientation(Value: TMKPBOrientation);
begin
  if Value = FOrientation then Exit;
  FOrientation := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetMax(Value: Integer);
begin
  if (Value = FMax) or (Value < FMin) then Exit;
  FMax := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetMin(Value: Integer);
begin
  if (Value = FMin) or (Value > FMax) then Exit;
  FMin := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetPosition(Value: Integer);
var
  OldPosition: Integer;
begin
  if Value = FPosition then Exit;
  OldPosition := FPosition;
  FPosition := Value;
  InvalidatePosition(OldPosition);
  if FShowText then InvalidateText;
end;

procedure TMKProgressBar.SetShowText(Value: Boolean);
begin
  if Value = FShowText then Exit;
  FShowText := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetShowTextStyle(Value: TMKShowTextStyle);
begin
  if Value = FShowTextStyle then Exit;
  FShowTextStyle := Value;
  if FShowText then Invalidate;
end;

procedure TMKProgressBar.SetBeginColor(Value: TColor);
begin
  if FBeginColor = Value then Exit;
  FBeginColor := Value;
  if FStyle <> sPictures then Invalidate;
end;

procedure TMKProgressBar.SetEndColor(Value: TColor);
begin
  if FEndColor = Value then Exit;
  FEndColor := Value;
  if (FStyle = sExSolid) or (FStyle = sExRectangles) then Invalidate;
end;

procedure TMKProgressBar.SetStyle(Value: TMKPBStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
  Invalidate;
end;

procedure TMKProgressBar.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  if not FGlyph.Empty then
  begin
    if FOrientation = orVertical then Width := FGlyph.Width + 4;
    if FOrientation = orHorizontal then Height := FGlyph.Height + 4;
    FGlyph.Transparent := FTransparentGlyph;
  end;
  if FStyle = sPictures then Invalidate;
end;

procedure TMKProgressBar.SetStep(Value: Integer);
begin
  if FStep <> Value then FStep := Value;
end;

procedure TMKProgressBar.InvalidatePosition(OldPosition: Integer);
var
  ARect: TRect;
  InvRect: TRect;
  RectWidth, RectHeight: Integer;
begin
  ARect := GetClientRect;
  InvRect := ARect;
  if FMax = FMin then Exit;

  if FOrientation = orHorizontal then
  begin
    if OldPosition < FPosition then
    begin
      InvRect.Right := ARect.Left + Trunc(MulDiv(FPosition, ARect.Right - ARect.Left, FMax - FMin)) + 2;
      InvRect.Left := ARect.Left + Trunc(MulDiv(OldPosition, ARect.Right - ARect.Left, FMax - FMin)) - 2;
    end
    else
    begin
      InvRect.Left := ARect.Left + Trunc(MulDiv(FPosition, ARect.Right - ARect.Left, FMax - FMin)) - 2;
      InvRect.Right := ARect.Left + Trunc(MulDiv(OldPosition, ARect.Right - ARect.Left, FMax - FMin)) + 2;
    end;
  end
  else
  begin
    if OldPosition < FPosition then
    begin
      InvRect.Top := ARect.Bottom - Trunc(MulDiv(FPosition, ARect.Bottom - ARect.Top, FMax - FMin)) - 2;
      InvRect.Bottom := ARect.Bottom - Trunc(MulDiv(OldPosition, ARect.Bottom - ARect.Top, FMax - FMin)) + 2;
    end
    else
    begin
      InvRect.Bottom := ARect.Bottom - Trunc(MulDiv(FPosition, ARect.Bottom - ARect.Top, FMax - FMin)) + 2;
      InvRect.Top := ARect.Bottom - Trunc(MulDiv(OldPosition, ARect.Bottom - ARect.Top, FMax - FMin)) - 2;
    end;
  end;

  ARect := InvRect;
  if ARect.Left > ARect.Right then
  begin
    InvRect.Left := ARect.Right;
    InvRect.Right := ARect.Left;
  end;
  if ARect.Top > ARect.Bottom then
  begin
    InvRect.Top := ARect.Top;
    InvRect.Bottom := ARect.Bottom;
  end;

  RectWidth := Trunc(MulDiv(ARect.Bottom - ARect.Top, 2, 3)) + 2;
  RectHeight := Trunc(MulDiv(ARect.Right - ARect.Left, 2, 3)) + 2;
  if FStyle = sPictures then
  begin
    RectWidth := FGlyph.Width;
    RectHeight := FGlyph.Height;
  end;
  if (FStyle = sRectangles) or (FStyle = sExRectangles) or (FStyle = sPictures) then
  begin
    if FOrientation = orHorizontal then
      InflateRect(InvRect, RectWidth, 0)
    else
      InflateRect(InvRect, 0, RectHeight);
  end;

  if (InvRect.Left <> InvRect.Right) and (InvRect.Top <> InvRect.Bottom) then
    InvalidateRect(Handle, @InvRect, False);
  Update;
end;

procedure TMKProgressBar.InvalidateText;
var
  InvRect: TRect;
  S: string;
begin
  if FShowTextStyle = stsPosition then S := IntToStr(FMax);
  if FShowTextStyle = stsPercent then S := '100 %';
  if FOrientation = orHorizontal then
  begin
    Canvas.Font := Font;
    InvRect.Left := (Width - Canvas.TextWidth(S)) div 2;
    InvRect.Top := (Height - Canvas.TextHeight(S)) div 2;
    InvRect.Right := InvRect.Left + Canvas.TextWidth(S);
    InvRect.Bottom := InvRect.Top + Canvas.TextHeight(S);
  end;
  if FOrientation = orVertical then
  begin
    Canvas.Font := Font;
    InvRect.Left := (Width - Canvas.TextHeight(S)) div 2;
    InvRect.Top := (Height - Canvas.TextWidth(S)) div 2;
    InvRect.Right := InvRect.Left + Canvas.TextHeight(S);
    InvRect.Bottom := InvRect.Top + Canvas.TextWidth(S);
  end;

  if (InvRect.Left <> InvRect.Right) and (InvRect.Top <> InvRect.Bottom) then
    InvalidateRect(Handle, @InvRect, False);
end;

procedure TMKProgressBar.Paint;
var
  ARect: TRect;
  SolidRect: TRect;
  i, RectWidth, RectHeight, Old: Integer;
  S: string;
  Bmp: TBitmap;
  W, H: Integer;
  TTM: TTextMetric;
  TLF: TLogFont;
  AFont: TFont;
  Ft: hFont;

  procedure PaintBarBevelOuter(BRect: TRect);
  begin
    if FBarBevelOuter = bvLowered then
    begin
      DrawEdge(Bmp.Canvas.Handle, BRect, BDR_SUNKENOUTER, BF_TOPLEFT);
      DrawEdge(Bmp.Canvas.Handle, BRect, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);
    end;
    if FBarBevelOuter = bvRaised then
    begin
      DrawEdge(Bmp.Canvas.Handle, BRect, BDR_RAISEDINNER, BF_TOPLEFT);
      DrawEdge(Bmp.Canvas.Handle, BRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    end;
  end;

begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Width;
    Bmp.Height := Height;
    ARect := GetClientRect;
    Bmp.Canvas.Brush.Color := Color;
    Bmp.Canvas.FillRect(ARect);

    if FBevelOuter = bvLowered then
    begin
      DrawEdge(Bmp.Canvas.Handle, ARect, BDR_SUNKENOUTER, BF_TOPLEFT);
      DrawEdge(Bmp.Canvas.Handle, ARect, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);
    end;
    if FBevelOuter = bvRaised then
    begin
      DrawEdge(Bmp.Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
      DrawEdge(Bmp.Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    end;

    if FBevelOuter <> bvNone then InflateRect(ARect, -2, -2);
    SolidRect := ARect;

    if FPosition <> FMin then
    begin

      if FOrientation = orHorizontal then
        SolidRect.Right := SolidRect.Left + Trunc(FPosition * (SolidRect.Right - SolidRect.Left) / (FMax - FMin))
      else
        SolidRect.Top := SolidRect.Bottom - Trunc(FPosition * (SolidRect.Bottom - SolidRect.Top) / (FMax - FMin));

      if SolidRect.Right > ARect.Right then SolidRect.Right := AREct.Right;
      if SolidRect.Top < ARect.Top then SolidRect.Top := AREct.Top;

      if FStyle = sSolid then
      begin
        Bmp.Canvas.Brush.Color := FBeginColor;
        Bmp.Canvas.FillRect(SolidRect);
        PaintBarBevelOuter(SolidRect);
      end;

      if FStyle = sExSolid then
      begin
        if FOrientation = orHorizontal then
          for i := SolidRect.Left to SolidRect.Right - 1 do
          begin
            Bmp.Canvas.Pen.Color := GetColor(FBeginColor, FEndColor, i - SolidRect.Left, ARect.Right - ARect.Left);
            Bmp.Canvas.MoveTo(i, SolidRect.Top);
            Bmp.Canvas.LineTo(i, SolidRect.Bottom);
          end
        else
          for i := SolidRect.Bottom - 1 downto SolidRect.Top do
          begin
            Bmp.Canvas.Pen.Color := GetColor(FBeginColor, FEndColor, SolidRect.Bottom - i, ARect.Bottom - ARect.Top);
            Bmp.Canvas.MoveTo(SolidRect.Left, i);
            Bmp.Canvas.LineTo(SolidRect.Right, i);
          end;
        PaintBarBevelOuter(SolidRect);
      end;

      if FStyle = sRectangles then
      begin
        Old := 0;
        if FOrientation = orHorizontal then
        begin
          RectWidth := Trunc((ARect.Bottom - ARect.Top) * 2 / 3) + 2;
          for i := SolidRect.Left to SolidRect.Right - 1 do
            if (((i - SolidRect.Left) mod RectWidth) = 0) and (i <> SolidRect.Left) then
            begin
              Bmp.Canvas.Pen.Color := FBeginColor;
              Bmp.Canvas.Brush.Color := FBeginColor;
              Bmp.Canvas.FillRect(Rect(i - RectWidth, SolidRect.Top, i - 2, SolidRect.Bottom));
              PaintBarBevelOuter(Rect(i - RectWidth, SolidRect.Top, i - 2, SolidRect.Bottom));
              Old := i;
            end;
          if FPosition >= Max then
          begin
            Bmp.Canvas.Pen.Color := FBeginColor;
            Bmp.Canvas.Brush.Color := FBeginColor;
            Bmp.Canvas.FillRect(Rect(Old, SolidRect.Top, SolidRect.Right, SolidRect.Bottom));
            PaintBarBevelOuter(Rect(Old, SolidRect.Top, SolidRect.Right, SolidRect.Bottom));
          end;
        end
        else
        begin
          RectHeight := Trunc((ARect.Right - ARect.Left) * 2 / 3) + 2;
          for i := SolidRect.Bottom - 1 downto SolidRect.Top do
            if (((SolidRect.Bottom - i - 1) mod RectHeight) = 0) and (i <> SolidRect.Bottom - 1) then
            begin
              Bmp.Canvas.Pen.Color := FBeginColor;
              Bmp.Canvas.Brush.Color := FBeginColor;
              Bmp.Canvas.FillRect(Rect(SolidRect.Left, i + 2, SolidRect.Right, i + RectHeight));
              PaintBarBevelOuter(Rect(SolidRect.Left, i + 2, SolidRect.Right, i + RectHeight));
              Old := i;
            end;
          if FPosition >= Max then
          begin
            Bmp.Canvas.Pen.Color := FBeginColor;
            Bmp.Canvas.Brush.Color := FBeginColor;
            Bmp.Canvas.FillRect(Rect(SolidRect.Left, SolidRect.Top, SolidRect.Right, Old));
            PaintBarBevelOuter(Rect(SolidRect.Left, SolidRect.Top, SolidRect.Right, Old));
          end;
        end;
      end;

      if FStyle = sExRectangles then
      begin
        Old := 0;
        if FOrientation = orHorizontal then
        begin
          RectWidth := Trunc((ARect.Bottom - ARect.Top) * 2 / 3) + 2;
          for i := SolidRect.Left to SolidRect.Right - 1 do
            if (((i - SolidRect.Left) mod RectWidth) = 0) and (i <> SolidRect.Left) then
            begin
              Bmp.Canvas.Pen.Color := GetColor(FBeginColor, FEndColor, i - SolidRect.Left, ARect.Right - ARect.Left);
              Bmp.Canvas.Brush.Color := Bmp.Canvas.Pen.Color;
              Bmp.Canvas.FillRect(Rect(i - RectWidth, SolidRect.Top, i - 2, SolidRect.Bottom));
              PaintBarBevelOuter(Rect(i - RectWidth, SolidRect.Top, i - 2, SolidRect.Bottom));
              Old := i;
            end;
          if FPosition >= Max then
          begin
            Bmp.Canvas.Pen.Color := GetColor(FBeginColor, FEndColor, Old - SolidRect.Left, ARect.Right - ARect.Left);
            Bmp.Canvas.Brush.Color := Bmp.Canvas.Pen.Color;
            Bmp.Canvas.FillRect(Rect(Old, SolidRect.Top, SolidRect.Right, SolidRect.Bottom));
            PaintBarBevelOuter(Rect(Old, SolidRect.Top, SolidRect.Right, SolidRect.Bottom));
          end;
        end
        else
        begin
          RectHeight := Trunc((ARect.Right - ARect.Left) * 2 / 3) + 2;
          for i := SolidRect.Bottom - 1 downto SolidRect.Top do
            if (((SolidRect.Bottom - i - 1) mod RectHeight) = 0) and (i <> SolidRect.Bottom - 1) then
            begin
              Bmp.Canvas.Pen.Color := GetColor(FBeginColor, FEndColor, SolidRect.Bottom - i, ARect.Bottom - ARect.Top);
              Bmp.Canvas.Brush.Color := Bmp.Canvas.Pen.Color;
              Bmp.Canvas.FillRect(Rect(SolidRect.Left, i + 2, SolidRect.Right, i + RectHeight));
              PaintBarBevelOuter(Rect(SolidRect.Left, i + 2, SolidRect.Right, i + RectHeight));
              Old := i;
            end;
          if FPosition >= Max then
          begin
            Bmp.Canvas.Pen.Color := GetColor(FBeginColor, FEndColor, SolidRect.Bottom - Old, ARect.Bottom - ARect.Top);
            Bmp.Canvas.Brush.Color := Bmp.Canvas.Pen.Color;
            Bmp.Canvas.FillRect(Rect(SolidRect.Left, SolidRect.Top, SolidRect.Right, Old));
            PaintBarBevelOuter(Rect(SolidRect.Left, SolidRect.Top, SolidRect.Right, Old));
          end;
        end;
      end;

      if (FStyle = sPictures) and (FGlyph.Width > 0) and (FGlyph.Height > 0) then
      begin
        if FOrientation = orHorizontal then
        begin
          Old := 0;
          RectWidth := FGlyph.Width;
          for i := SolidRect.Left to SolidRect.Right - 1 do
            if (((i - SolidRect.Left) mod RectWidth) = 0) and (i <> SolidRect.Left) then
            begin
              Bmp.Canvas.Draw(i - RectWidth, SolidRect.Top, FGlyph);
              Old := i;
            end;
          if FPosition = FMax then Bmp.Canvas.Draw(Old, SolidRect.Top, FGlyph);
        end
        else
        begin
          Old := 0;
          RectHeight := FGlyph.Height;
          for i := SolidRect.Bottom - 1 downto SolidRect.Top do
            if (((SolidRect.Bottom - i - 1) mod RectHeight) = 0) and (i <> SolidRect.Bottom - 1) then
            begin
              Bmp.Canvas.Draw(SolidRect.Left, i, FGlyph);
              Old := i;
            end;
          if FPosition = FMax then Bmp.Canvas.Draw(SolidRect.Left, Old - FGlyph.Height, FGlyph);
        end;
      end;
    end;

    if FShowText then
    begin
      if FShowTextStyle = stsPosition then S := IntToStr(FPosition);
      if FShowTextStyle = stsPercent then S := IntToStr(Round(100 * FPosition / (FMax - FMin))) + ' %';
      Bmp.Canvas.Font.Assign(Font);
      W := Bmp.Canvas.TextWidth(S);
      H := Bmp.Canvas.TextHeight(S);
      if FOrientation = orHorizontal then
      begin
        Bmp.Canvas.Brush.Style := bsClear;
        Bmp.Canvas.TextOut((Width - W) div 2, (Height - H) div 2, S);
      end;
      if FOrientation = orVertical then
      begin
        AFont := TFont.Create;
        try
          AFont.Assign(Font);
          GetTextMetrics(AFont.Handle, TTM);
          if (TTM.tmPitchAndFamily and TMPF_TRUETYPE) = 0 then
            AFont.Name := 'Arial';
          GetObject(AFont.Handle, SizeOf(TLF), @TLF);
          TLF.lfEscapement := 2700;

          Ft := CreateFontIndirect(TLF);
          Bmp.Canvas.Font.Handle := Ft;
          Bmp.Canvas.Brush.Style := bsClear;
          Bmp.Canvas.TextOut(H + (Width - H) div 2, (Height - W) div 2, S);
        finally
          AFont.Free;
        end;
      end;

    end;
    BitBlt(Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    Bmp.Free;
  end;
end;

procedure TMKProgressBar.StepBy(Delta: Integer);
begin
  if Delta <> 0 then Position := Position + Delta;
end;

procedure TMKProgressBar.StepIt;
begin
  if Step <> 0 then Position := Position + Step;
end;

end.

