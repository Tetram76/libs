unit Handles;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Menus, StdCtrls;

type
  TDragStyle = (dsMove, dsSizeTopLeft, dsSizeTopRight, dsSizeBottomLeft, dsSizeBottomRight, dsSizeTop, dsSizeLeft, dsSizeBottom, dsSizeRight);
  EBadChild = class(Exception);

  GridValues = 1..100;

  TStretchHandle = class(TCustomControl)
  private
    FDragOffset: TPoint;
    FDragStyle: TDragStyle;
    FDragging: Boolean;
    FDragRect: TRect;
    FLocked: Boolean;
    FColor: TColor;
    FPrimaryColor: TColor;
    FSecondaryColor: TColor;
    FGridX, FGridY: GridValues;
    FChildControl: TControl;
    FGridState: Boolean;
    FSizeable: Boolean;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDLGCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure SetChildControl(ChildControl: TControl);
//    procedure MoveSiblings(XOffset, YOffset: integer);
    procedure Rubberband(NewRect: TRect);
    function HasSiblings: Boolean;
    function GetChildControl: TControl;
    function GetModifiedRect(XPos, YPos: Integer): TRect;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BringToFront;
    procedure SendToBack;
    procedure Attach(ChildControl: TControl);
    procedure Detach;
    procedure SetColors(Color1, Color2: TColor);
    procedure SwitchColor;
    function IsAttached: Boolean;

    function IsDragging: Boolean;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    property Canvas;

    property Child: TControl read GetChildControl write SetChildControl;
    property Attached: Boolean read IsAttached;
  published
    property Color: TColor read FPrimaryColor write FPrimaryColor default clBlack;
    property SecondaryColor: TColor read FSecondaryColor write FSecondaryColor default clGray;
    property Locked: Boolean read FLocked write FLocked default False;
    property GridX: GridValues read FGridX write FGridX default 8;
    property GridY: GridValues read FGridY write FGridY default 8;
    property SnapToGrid: Boolean read FGridState write FGridState default False;
    property Sizeable: Boolean read FSizeable write FSizeable default True;
    property DragCursor;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TStretchHandle]);
end;

constructor TStretchHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 24;
  Height := 24;
  FChildControl := nil;
  FPrimaryColor := clBlack;
  FSecondaryColor := clGray;

  FGridX := 8;
  FGridY := 8;
  FGridState := False;
  FSizeable := True;

  Enabled := False;
  Visible := False;
end;

destructor TStretchHandle.Destroy;
begin
  inherited Destroy;
end;

procedure TStretchHandle.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;
end;

procedure TStretchHandle.Attach(ChildControl: TControl);
var
  i: Integer;
begin
  if ChildControl is TForm then raise EBadChild.Create('Handles can not be attached to this component');
  if Attached then Detach;
  if Assigned(ChildControl) then begin
    FChildControl := ChildControl;
    Parent := ChildControl.Parent;
    SetBounds(ChildControl.Left - 2, ChildControl.Top - 2, ChildControl.Width + 5, ChildControl.Height + 5);
    FColor := FPrimaryColor;
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] is TStretchHandle then
        if (TStretchHandle(Parent.Controls[i]).Attached) and (TStretchHandle(Parent.Controls[i]) <> Self) then begin
          FColor := FSecondaryColor;
          TStretchHandle(Parent.Controls[i]).SwitchColor;
        end;
    FDragRect := Rect(0, 0, 0, 0);
    Enabled := True;
    Visible := True;
    if not (csDesigning in ComponentState) then begin
      inherited BringToFront;
      SetFocus;
    end;
  end;
  Invalidate;
end;

procedure TStretchHandle.Detach;
begin
  FChildControl := nil;
  FLocked := False;
  Enabled := False;
  Visible := False;
  Parent := nil;
  FDragRect := Rect(0, 0, 0, 0);
end;

procedure TStretchHandle.SetColors(Color1, Color2: TColor);
begin
  FPrimaryColor := Color1;
  FSecondaryColor := Color2;
  if HasSiblings then FColor := FSecondaryColor
                 else FColor := FPrimaryColor;
  Invalidate;
end;

procedure TStretchHandle.SwitchColor;
begin
  FColor := FSecondaryColor;
  Invalidate;
end;

procedure TStretchHandle.SetChildControl(ChildControl: TControl);
begin
  if Assigned(ChildControl) then Attach(ChildControl)
                            else Detach;
end;

function TStretchHandle.GetChildControl: TControl;
begin
  Result := FChildControl;
end;

function TStretchHandle.HasSiblings: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Parent.ControlCount - 1 do
    if (Parent.Controls[i] is TStretchHandle) and (TStretchHandle(Parent.Controls[i]) <> Self) and (TStretchHandle(Parent.Controls[i]).IsAttached) then begin
      Result := True;
      Break;
    end;
end;

function TStretchHandle.IsAttached: Boolean;
begin
  Result := Assigned(FChildControl);
end;

procedure TStretchHandle.WMGetDLGCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TStretchHandle.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TStretchHandle.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ARect: TRect;
  DragStyle: TDragStyle;
begin
  DragStyle := dsMove;
  Cursor := DragCursor;
  if Sizeable and not HasSiblings then begin
    ARect := GetClientRect;
    if ((Abs(X - ARect.Left) < 5) and (Abs(Y - ARect.Top) < 5)) then begin
      DragStyle := dsSizeTopLeft;
      Cursor := crSizeNWSE;
    end;
    if ((Abs(X - ARect.Right) < 5) and (Abs(Y - ARect.Bottom) < 5)) then begin
      DragStyle := dsSizeBottomRight;
      Cursor := crSizeNWSE;
    end;
    if ((Abs(X - ARect.Right) < 5) and (Abs(Y - ARect.Top) < 5)) then begin
      DragStyle := dsSizeTopRight;
      Cursor := crSizeNESW;
    end;
    if ((Abs(X - ARect.Left) < 5) and (Abs(Y - ARect.Bottom) < 5)) then begin
      DragStyle := dsSizeBottomLeft;
      Cursor := crSizeNESW;
    end;
    if ((Abs(X - Trunc(ARect.Right - ARect.Left) / 2) < 3) and (Abs(Y - ARect.Top) < 5)) then begin
      DragStyle := dsSizeTop;
      Cursor := crSizeNS;
    end;
    if ((Abs(X - Trunc(ARect.Right - ARect.Left) / 2) < 3) and (Abs(Y - ARect.Bottom) < 5)) then begin
      DragStyle := dsSizeBottom;
      Cursor := crSizeNS;
    end;
    if ((Abs(Y - Trunc(ARect.Bottom - ARect.Top) / 2) < 3) and (Abs(X - ARect.Left) < 5)) then begin
      DragStyle := dsSizeLeft;
      Cursor := crSizeWE;
    end;
    if ((Abs(Y - Trunc(ARect.Bottom - ARect.Top) / 2) < 3) and (Abs(X - ARect.Right) < 5)) then begin
      DragStyle := dsSizeRight;
      Cursor := crSizeWE;
    end;
  end;
  if FDragging then begin
    if SnapToGrid then begin
      if FGridX > 1 then X := (X div FGridX) * FGridX - 2;
      if FGridY > 1 then Y := (Y div FGridY) * FGridY - 2;
    end;
    if (Left + X) < 0 then X := -Left;
    if (Top + Y) < 0 then Y := -Top;
    if (Left + X) > Parent.Width then X := Parent.Width - Left;
    if (Top + Y) > Parent.Height then Y := Parent.Height - Top;
    RubberBand(GetModifiedRect(X, Y));
  end else
    FDragStyle := DragStyle;
  if FLocked then Cursor := crNoDrop;
  inherited MouseMove(Shift, X, Y);
end;

procedure TStretchHandle.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and not FLocked then begin
    if SnapToGrid then begin
      if FGridX > 1 then X := (X div FGridX) * FGridX - 2;
      if FGridY > 1 then Y := (Y div FGridY) * FGridY - 2;
    end;
    FDragOffset := Point(X, Y);
    FDragging := True;
    RubberBand(GetModifiedRect(X, Y));
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TStretchHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ARect: TRect;
begin
  if Button = mbLeft then begin
    if SnapToGrid then begin
      if FGridX > 1 then X := (X div FGridX) * FGridX - 2;
      if FGridY > 1 then Y := (Y div FGridY) * FGridY - 2;
    end;
    if (Left + X) < 0 then X := -Left;
    if (Top + Y) < 0 then Y := -Top;
    if (Left + X) > Parent.Width then X := Parent.Width - Left;
    if (Top + Y) > Parent.Height then Y := Parent.Height - Top;
    if FDragging then begin
      ARect := GetModifiedRect(X, Y);
      RubberBand(Rect(0, 0, 0, 0));
      if (ARect.Left <> Left) or (ARect.Top <> Top) then Invalidate;
//      if FDragStyle = dsMove then MoveSiblings(ARect.Left - Left, ARect.Top - Top);
      SetBounds(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      FDragging := False;
      SetFocus;
    end;
    ReleaseCapture;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TStretchHandle.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        if IsAttached then FChildControl.Invalidate;
        Invalidate;
        SetBounds(Left, Top - 1, Width, Height);
//        MoveSiblings(0, -1);
      end;
    VK_DOWN:
      begin
        if IsAttached then FChildControl.Invalidate;
        Invalidate;
        SetBounds(Left, Top + 1, Width, Height);
//        MoveSiblings(0, 1);
      end;
    VK_LEFT:
      begin
        if IsAttached then FChildControl.Invalidate;
        Invalidate;
        SetBounds(Left - 1, Top, Width, Height);
//        MoveSiblings(-1, 0);
      end;
    VK_RIGHT:
      begin
        if IsAttached then FChildControl.Invalidate;
        Invalidate;
        SetBounds(Left + 1, Top, Width, Height);
//        MoveSiblings(1, 0);
      end;
  end;
  inherited KeyDown(Key, Shift);
end;

function TStretchHandle.GetModifiedRect(XPos, YPos: Integer): TRect;
var
  ARect: TRect;
begin
  case FDragStyle of
    dsSizeTopLeft:
      begin
        ARect.Left := Left + (XPos - FDragOffset.X);
        ARect.Top := Top + (YPos - FDragOffset.Y);
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height - (ARect.Top - Top);
      end;
    dsSizeTopRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top + (YPos - FDragOffset.Y);
        ARect.Right := Width + (XPos - FDragOffset.X);
        ARect.Bottom := Height - (ARect.Top - Top);
      end;
    dsSizeBottomLeft:
      begin
        ARect.Left := Left + (XPos - FDragOffset.X);
        ARect.Top := Top;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height + (YPos - FDragOffset.Y);
      end;
    dsSizeBottomRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width + (XPos - FDragOffset.X);
        ARect.Bottom := Height + (YPos - FDragOffset.Y);
      end;
    dsSizeTop:
      begin
        ARect.Left := Left;
        ARect.Top := Top + (YPos - FDragOffset.Y);
        ARect.Right := Width;
        ARect.Bottom := Height - (ARect.Top - Top);
      end;
    dsSizeBottom:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width;
        ARect.Bottom := Height + (YPos - FDragOffset.Y);
      end;
    dsSizeLeft:
      begin
        ARect.Left := Left + (XPos - FDragOffset.X);
        ARect.Top := Top;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height;
      end;
    dsSizeRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width + (XPos - FDragOffset.X);
        ARect.Bottom := Height;
      end;
  else
    ARect.Left := Left + (XPos - FDragOffset.X);
    ARect.Top := Top + (YPos - FDragOffset.Y);
    ARect.Right := Width;
    ARect.Bottom := Height;
  end;
  if ARect.Right < 5 then ARect.Right := 5;
  if ARect.Bottom < 5 then ARect.Bottom := 5;
  Result := ARect;
end;

procedure TStretchHandle.Rubberband(NewRect: TRect);
var
  PtA, PtB: TPoint;
  ScreenDC: HDC;
begin
  ScreenDC := GetDC(0);
  PtA := Parent.ClientToScreen(Point(FDragRect.Left + 2, FDragRect.Top + 2));
  PtB := Parent.ClientToScreen(Point(FDragRect.Left + FDragRect.Right - 3, FDragRect.Top + FDragRect.Bottom - 3));
  if (FDragRect.Left <> 0) or (FDragRect.Top <> 0) or (FDragRect.Right <> 0) or (FDragRect.Bottom <> 0) then DrawFocusRect(ScreenDC, Rect(PtA.X, PtA.Y, PtB.X, PtB.Y));
  PtA := Parent.ClientToScreen(Point(NewRect.Left + 2, NewRect.Top + 2));
  PtB := Parent.ClientToScreen(Point(NewRect.Left + NewRect.Right - 3, NewRect.Top + NewRect.Bottom - 3));
  if (NewRect.Left <> 0) or (NewRect.Top <> 0) or (NewRect.Right <> 0) or (NewRect.Bottom <> 0) then DrawFocusRect(ScreenDC, Rect(PtA.X, PtA.Y, PtB.X, PtB.Y));
  FDragRect := NewRect;
  ReleaseDC(0, ScreenDC);
end;

procedure TStretchHandle.BringToFront;
begin
  if IsAttached then FChildControl.BringToFront;
  inherited BringToFront;
  SetFocus;
end;

procedure TStretchHandle.SendToBack;
begin
  if IsAttached then FChildControl.SendToBack;
  inherited SendToBack;
  SetFocus;
end;

procedure TStretchHandle.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  WasVisible: Boolean;
begin
  WasVisible := Visible;
  if csDesigning in ComponentState then begin
    Visible := False;
    inherited SetBounds(ALeft, ATop, 24, 24);
  end else
    if not FLocked then begin
      if IsAttached then FChildControl.SetBounds(ALeft + 2, ATop + 2, AWidth - 5, AHeight - 5);
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    end;
  if Visible = False then Visible := WasVisible;
end;

//procedure TStretchHandle.MoveSiblings(XOffset, YOffset: Integer);
//var
//  i, T, L, W, H: Integer;
//begin
//  for i := 0 to Parent.ControlCount - 1 do
//    if Parent.Controls[i] is TStretchHandle then
//      if (TStretchHandle(Parent.Controls[i]).Attached) and (TStretchHandle(Parent.Controls[i]) <> Self) then begin
//        L := TStretchHandle(Parent.Controls[i]).Left + XOffset;
//        T := TStretchHandle(Parent.Controls[i]).Top + YOffset;
//        W := TStretchHandle(Parent.Controls[i]).Width;
//        H := TStretchHandle(Parent.Controls[i]).Height;
//        TStretchHandle(Parent.Controls[i]).Invalidate;
//        TStretchHandle(Parent.Controls[i]).SetBounds(L, T, W, H);
//      end;
//end;

procedure TStretchHandle.Paint;
var
  ARect, BoxRect: TRect;
begin
  inherited Paint;
  ARect := Rect(0, 0, Width - 1, Height - 1);
  with Canvas do begin
    Brush.Color := FColor;
    BoxRect := Rect(ARect.Left, ARect.Top, ARect.Left + 5, ARect.Top + 5); FillRect(BoxRect);
    BoxRect := Rect(ARect.Right - 5, ARect.Top, ARect.Right, ARect.Top + 5); FillRect(BoxRect);
    BoxRect := Rect(ARect.Right - 5, ARect.Bottom - 5, ARect.Right, ARect.Bottom); FillRect(BoxRect);
    BoxRect := Rect(ARect.Left, ARect.Bottom - 5, ARect.Left + 5, ARect.Bottom); FillRect(BoxRect);
    BoxRect := Rect(Trunc((ARect.Right - ARect.Left) / 2) - 2, ARect.Top, Trunc((ARect.Right - ARect.Left) / 2) + 3, ARect.Top + 5); FillRect(BoxRect);
    BoxRect := Rect(Trunc((ARect.Right - ARect.Left) / 2) - 2, ARect.Bottom - 5, Trunc((ARect.Right - ARect.Left) / 2) + 3, ARect.Bottom); FillRect(BoxRect);
    BoxRect := Rect(ARect.Left, Trunc((ARect.Bottom - ARect.Top) / 2) - 2, ARect.Left + 5, Trunc((ARect.Bottom - ARect.Top) / 2) + 3); FillRect(BoxRect);
    BoxRect := Rect(ARect.Right - 5, Trunc((ARect.Bottom - ARect.Top) / 2) - 2, ARect.Right, Trunc((ARect.Bottom - ARect.Top) / 2) + 3); FillRect(BoxRect);
  end;
end;

function TStretchHandle.IsDragging: Boolean;
begin
  Result := FDragging;
end;

end.


