unit CRFurtif;

interface

{$D-}
{$B-}
{$R-}

uses
  Windows, SysUtils, Messages, Classes, Controls, Forms, Graphics, StdCtrls, ExtCtrls, CommCtrl, Buttons, Menus;

type
  TButtonState = (bsUp, bsDisabled, bsDown, bsMouseOver);

  TCRFurtifLight = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: Pointer;
    FDown: Boolean;
    FClicking: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FMouseInControl: Boolean;
    FPopupLst: TPopupMenu;
    FWithBorder: Boolean;
    FModalResult: TModalResult;
    FDisableGrayed: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetWithBorder(const Value: Boolean);
    procedure SetDisableGrayed(const Value: Boolean);
    function GetDisableGrayed: Boolean;
  protected
    FState: TButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    UsedAngle: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Canvas;
  published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property PopupLst: TPopupMenu read FPopupLst write FPopupLst default nil;
    property WithBorder: Boolean read FWithBorder write SetWithBorder default False;
    property DisableGrayed: Boolean read GetDisableGrayed write SetDisableGrayed default True;
  end;

  TCRFurtifLightPush = class(TCRFurtifLight)
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  end;

  //  TFurtifLight = class(TCRFurtifLight);
  //  TFurtifLightPush = class(TCRFurtifLightPush);

procedure Register;

implementation

uses
  Math, ActnList, ImgList;

procedure Register;
begin
  RegisterComponents('Cardio', [{TFurtifLight, TFurtifLightPush, }TCRFurtifLight, TCRFurtifLightPush]);
end;

type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FDisableGrayed: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint; const WithBorder: Boolean; State: TButtonState);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string; TextBounds: TRect; const WithBorder: Boolean; State: TButtonState; BiDiFlags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect; const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect; BiDiFlags: Longint);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Caption: string; Layout: TButtonLayout; const WithBorder: Boolean; Margin, Spacing: Integer; State: TButtonState; BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DisableGrayed: Boolean read FDisableGrayed write FDisableGrayed default True;
  end;

  { TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;
  ButtonCount: Integer = 0;

  { TButtonGlyph }

constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
  FDisableGrayed := True;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
type
  TRGBArray = array[0..0] of TRGBTriple; // élément de bitmap (API windows)
  pRGBArray = ^TRGBArray; // type pointeur vers tableau 3 octets 24 bits
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;

  x, y: Integer;
  R, G, B, R0, G0, B0, Gris: Integer;
  p: pRGBArray;
  cl: TColor;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown, bsMouseOver: begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, FOriginal.TransparentColor);
        end;
      bsDisabled:
        if FDisableGrayed then begin
          // on prend l'icone "normal" pour la rendre désactivée: plus joli avec les nouvelles icones de CardioXP
          I := bsUp;
          ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);

          TmpImage.PixelFormat := pf24bit;
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);

          //          if FOriginal.TransparentMode = tmFixed then cl := FOriginal.TransparentColor
          //                                                 else cl := clDefault;
          cl := FOriginal.TransparentColor;

          R0 := GetRValue(cl);
          G0 := GetGValue(cl);
          B0 := GetBValue(cl);

          for y := 0 to Pred(TmpImage.Height) do begin
            p := TmpImage.ScanLine[y];
            for x := 0 to Pred(TmpImage.Width) do begin
              R := p[x].rgbtRed;
              G := p[x].rgbtGreen;
              B := p[x].rgbtBlue;
              if (R <> R0) or (G <> G0) or (B <> B0) then begin
                Gris := (R + G + B) div 3;
                p[x].rgbtRed := Gris;
                p[x].rgbtGreen := Gris;
                p[x].rgbtBlue := Gris;
              end;
            end;
          end;

          FIndexs[State] := FGlyphList.AddMasked(TmpImage, cl);
        end
        else begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, FOriginal.TransparentColor);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint; const WithBorder: Boolean; State: TButtonState);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  if WithBorder and (State = bsDown) then
    with GlyphPos do
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, Succ(X), Succ(Y), 0, 0, clNone, clNone, ILD_Transparent)
  else
    with GlyphPos do
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0, clNone, clNone, ILD_Transparent);
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string; TextBounds: TRect; const WithBorder: Boolean; State: TButtonState; BiDiFlags: LongInt);
begin
  if WithBorder and (State = bsDown) then begin
    Inc(TextBounds.Left);
    Inc(TextBounds.Right);
  end;
  with Canvas do begin
    Brush.Style := bsClear;
    if State = bsDisabled then begin
      //      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or BiDiFlags or DT_WORDBREAK);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or BiDiFlags or DT_WORDBREAK);
    end
    else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or DT_VCENTER or BiDiFlags or DT_WORDBREAK);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas;
  const Client: TRect;
  const Caption: string;
  Layout: TButtonLayout;
  Margin,
  Spacing: Integer;
  var GlyphPos: TPoint;
  var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    case Layout of
      blGlyphLeft: Layout := blGlyphRight;
      blGlyphRight: Layout := blGlyphLeft;
    end;
  { calculate the item sizes }

  ClientSize := Point(Client.Right - Client.Left - Max(0, 2 * Margin), Client.Bottom - Client.Top - Max(0, 2 * Margin));

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then begin
    TextBounds := Rect(0, 0, ClientSize.x, 0);
    if Layout in [blGlyphLeft, blGlyphRight] then Dec(TextBounds.Right, GlyphSize.x);
    if (Layout in [blGlyphLeft, blGlyphRight]) and (Spacing = -2) then Dec(TextBounds.Right, GlyphSize.x);
    if (Spacing > -1) and (Layout in [blGlyphLeft, blGlyphRight]) and (GlyphSize.x > 0) then Dec(TextBounds.Right, Spacing);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT or BiDiFlags or DT_WORDBREAK);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  end
  else begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  { If the layout has the glyph on the right or the left, then both the text and the glyph are centered vertically.
    If the glyph is on the top or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then begin
    GlyphPos.Y := Max(0, Margin) + (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := Max(0, Margin) + (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else begin
    GlyphPos.X := Max(0, Margin) + (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := Max(0, Margin) + (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then begin
    if Spacing = -1 then begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else begin
    if Spacing = -1 then begin
      TotalSize := Point(ClientSize.X - GlyphSize.X, ClientSize.Y - GlyphSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end
    else if Spacing = -2 then begin
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (ClientSize.X - TextSize.X) div 2 - GlyphSize.X
      else
        Spacing := (ClientSize.Y - TextSize.Y) div 2 - GlyphSize.Y;
    end;
  end;

  case Layout of
    blGlyphLeft: begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight: begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop: begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom: begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  with GlyphPos do begin
    Inc(X, Client.Left);
    Inc(Y, Client.Top);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

function TButtonGlyph.Draw(Canvas: TCanvas;
  const Client: TRect;
  const Caption: string;
  Layout: TButtonLayout;
  const WithBorder: Boolean;
  Margin,
  Spacing: Integer;
  State: TButtonState;
  BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Caption, Layout, Margin, Spacing, GlyphPos, Result, BiDiFlags);
  DrawButtonGlyph(Canvas, GlyphPos, WithBorder, State);
  DrawButtonText(Canvas, Caption, Result, WithBorder, State, BiDiFlags);
end;

constructor TCRFurtifLight.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  Cursor := crHandPoint;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  FWithBorder := False;
  FDisableGrayed := True;
  Inc(ButtonCount);
end;

destructor TCRFurtifLight.Destroy;
begin
  Dec(ButtonCount);
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
end;

procedure TCRFurtifLight.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);

var
  PaintRect: TRect;
begin
  if not Enabled then begin
    FState := bsDisabled;
    FClicking := False;
  end;

  Canvas.Font := Self.Font;
  PaintRect := Rect(0, 0, Width, Height);
  if (csDesigning in ComponentState) or (WithBorder and (FMouseInControl or FDown)) then
    DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState = bsDown], BF_RECT);
  InflateRect(PaintRect, -1, -1);
  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Caption, FLayout, FWithBorder, FMargin, FSpacing, FState, DrawTextBiDiModeFlags(0));
end;

procedure TCRFurtifLight.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = PopupLst then PopupLst := nil;
end;

procedure TCRFurtifLight.Loaded;
begin
  inherited Loaded;
  if Enabled then
    if FDown then
      FState := bsDown
    else
      FState := bsUp
  else
    FState := bsDisabled;
  Invalidate;
end;

procedure TCRFurtifLight.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then begin
    FState := bsDown;
    FClicking := True;
    if Assigned(FPopupLst) and not Bool(FGroupIndex) then begin
      pt := ClientToScreen(Point(0, Height - 1));
      FPopupLst.PopupComponent := Self;
      if Layout = blGlyphRight then begin
        FPopupLst.Alignment := paRight;
        Inc(pt.X, Width);
      end
      else
        FPopupLst.Alignment := paLeft;
      Repaint;
      FPopupLst.Popup(pt.X, pt.Y);
      FState := bsUp;
      FClicking := False;
      FMouseInControl := False;
    end
    else begin
      FMouseInControl := True;
    end;
    Invalidate;
  end;
end;

procedure TCRFurtifLight.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Enabled and FClicking then begin
    FClicking := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then begin
      if FState <> bsUp then Invalidate;
      FState := bsUp;
      FMouseInControl := False;
    end
    else if DoClick then begin
      SetDown(not FDown);
      if FDown then Repaint;
    end
    else begin
      if FDown then FState := bsUp;
      Repaint;
    end;
    if DoClick then Click;
  end;
end;

function GetParentForm(Control: TControl): TCustomForm;
begin
  while (Control.Parent <> nil) and not (Control.Parent is TCustomForm) do
    Control := Control.Parent;
  if Control.Parent is TCustomForm then
    Result := TCustomForm(Control.Parent)
  else
    Result := nil;
end;

procedure TCRFurtifLight.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  inherited Click;
end;

function TCRFurtifLight.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TCRFurtifLight.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TCRFurtifLight.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

function TCRFurtifLight.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TCRFurtifLight.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > 4 then
    Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TCRFurtifLight.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCRFurtifLight.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TCRFurtifLight.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then begin
      if FState <> bsDown then Invalidate;
      FState := bsDown;
    end
    else begin
      if FMouseInControl then
        FState := bsMouseOver
      else
        FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TCRFurtifLight.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TCRFurtifLight.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TCRFurtifLight.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCRFurtifLight.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCRFurtifLight.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp <> Value then begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TCRFurtifLight.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TCRFurtifLight.CMEnabledChanged(var Message: TMessage);
var
  NewState: TButtonState;
begin
  if Enabled then
    if FDown or FClicking then
      NewState := bsDown
    else
      NewState := bsUp
  else
    NewState := bsDisabled;
  if NewState <> FState then Invalidate;
  FState := NewState;
  //  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState);
end;

procedure TCRFurtifLight.CMButtonPressed(var Message: TMessage);
var
  Sender: TCRFurtifLight;
begin
  if Message.WParam = WPARAM(FGroupIndex) then begin
    Sender := TCRFurtifLight(Message.LParam);
    if Sender <> Self then begin
      if Sender.Down and FDown then begin
        FDown := False;
        if FMouseInControl then
          FState := bsMouseOver
        else
          FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
  Message.Result := 0;
end;

procedure TCRFurtifLight.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCRFurtifLight.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCRFurtifLight.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCRFurtifLight.CMSysColorChange(var Message: TMessage);
begin
  with TButtonGlyph(FGlyph) do begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;

procedure TCRFurtifLight.CMMouseEnter(var Message: TMessage);
var
  NewState: TButtonState;
begin
  inherited;
  if not FClicking and Enabled then begin
    if FDown then
      NewState := bsDown
    else
      NewState := bsMouseOver;
    if FState <> NewState then Invalidate;
    FState := NewState;
  end;
  FMouseInControl := True;
end;

procedure TCRFurtifLight.CMMouseLeave(var Message: TMessage);
var
  NewState: TButtonState;
begin
  inherited;
  if Enabled and not FClicking then begin
    if FDown then
      NewState := bsDown
    else
      NewState := bsUp;
    if FState <> NewState then Invalidate;
    FState := NewState;
  end;
  FMouseInControl := False;
end;

procedure TCRFurtifLight.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia; //! for lack of a better color
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do begin
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

constructor TCRFurtifLightPush.Create(AOwner: TComponent);
begin
  inherited;
  GroupIndex := 1;
end;

procedure TCRFurtifLight.SetWithBorder(const Value: Boolean);
begin
  if FWithBorder <> Value then begin
    FWithBorder := Value;
    Invalidate;
  end;
end;

function TCRFurtifLight.GetDisableGrayed: Boolean;
begin
  Result := TButtonGlyph(FGlyph).DisableGrayed;
end;

procedure TCRFurtifLight.SetDisableGrayed(const Value: Boolean);
begin
  TButtonGlyph(FGlyph).DisableGrayed := Value;
  Invalidate;
end;

end.

