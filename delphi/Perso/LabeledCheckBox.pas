unit LabeledCheckBox;

interface

uses
  Windows, SysUtils, Classes, Messages, Controls, Graphics, StdCtrls, ExtCtrls, System.Types;

type
  TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);

  { TCustomLabeledEdit }

  TCustomLabeledCheckBox = class(TCustomCheckBox)
  private
    FReadOnly: Boolean;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    function GetParentFont: Boolean;
    procedure SetParentFont(const Value: Boolean);
    type
      TBoundLabel = class(ExtCtrls.TBoundLabel)
        protected
          procedure AdjustBounds; override;
      end;

    var
    FEditLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    OldValue: TCheckBoxState;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
    function GetAlignment: TLeftRight;
    procedure SetAlignment(const Value: TLeftRight);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
    property EditLabel: TBoundLabel read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpRight;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 8;
    property Alignment: TLeftRight read GetAlignment write SetAlignment default taRightJustify;
    property Font: TFont read GetFont write SetFont;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Caption: TCaption read GetText write SetText;
    property ParentFont: Boolean read GetParentFont write SetParentFont;
  end;

  TLabeledCheckBox = class(TCustomLabeledCheckBox)
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property EditLabel;
    // property LabelPosition;
    property LabelSpacing;
    property ReadOnly;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TLabeledCheckBox]);
end;

{ TCustomLabeledCheckBox }

procedure TCustomLabeledCheckBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;
  if FEditLabel <> nil then
    FEditLabel.BiDiMode := BiDiMode;
end;

procedure TCustomLabeledCheckBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;
  if FEditLabel <> nil then
    FEditLabel.Enabled := Enabled;
end;

procedure TCustomLabeledCheckBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;
  if FEditLabel <> nil then
    FEditLabel.Visible := Visible;
end;

constructor TCustomLabeledCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabelPosition := lpRight;
  FLabelSpacing := 8;
  Width := 13;
  Height := 13;
  FReadOnly := False;
  SetupInternalLabel;
end;

function TCustomLabeledCheckBox.GetAlignment: TLeftRight;
begin
  Result := inherited Alignment;
end;

function TCustomLabeledCheckBox.GetFont: TFont;
begin
  if Assigned(FEditLabel) then
    Result := FEditLabel.Font
  else
    Result := inherited Font;
end;

function TCustomLabeledCheckBox.GetParentFont: Boolean;
begin
  if Assigned(FEditLabel) then
    Result := FEditLabel.ParentFont
  else
    Result := inherited ParentFont;
end;

function TCustomLabeledCheckBox.GetText: TCaption;
begin
  if Assigned(FEditLabel) then
    Result := FEditLabel.Caption
  else
    Result := inherited Caption;
end;

procedure TCustomLabeledCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FReadOnly and (Key = 32) then Key := 0;
  inherited;
end;

procedure TCustomLabeledCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldValue := State;
  inherited;
end;

procedure TCustomLabeledCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FReadOnly then State := OldValue;
end;

procedure TCustomLabeledCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TCustomLabeledCheckBox.SetAlignment(const Value: TLeftRight);
begin
  case Value of
    taLeftJustify: LabelPosition := lpLeft;
    taRightJustify: LabelPosition := lpRight;
  end;
  inherited Alignment := Value;
end;

procedure TCustomLabeledCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TCustomLabeledCheckBox.SetFont(const Value: TFont);
begin
  if Assigned(FEditLabel) then
    FEditLabel.Font.Assign(Value)
  else
    inherited Font.Assign(Value);
end;

function AdjustedAlignment(RightToLeftAlignment: Boolean; Alignment: TAlignment): TAlignment;
begin
  Result := Alignment;
  if RightToLeftAlignment then
    case Result of
      taLeftJustify: Result := taRightJustify;
      taRightJustify: Result := taLeftJustify;
    end;
end;

procedure TCustomLabeledCheckBox.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then Exit;
  FLabelPosition := Value;
  case Value of
    lpAbove:
      case AdjustedAlignment(UseRightToLeftAlignment, Alignment) of
        taLeftJustify: P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FEditLabel.Width,
          Top - FEditLabel.Height - FLabelSpacing);
        taCenter: P := Point(Left + (Width - FEditLabel.Width) div 2,
          Top - FEditLabel.Height - FLabelSpacing);
      end;
    lpBelow:
      case AdjustedAlignment(UseRightToLeftAlignment, Alignment) of
        taLeftJustify: P := Point(Left, Top + Height + FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FEditLabel.Width,
          Top + Height + FLabelSpacing);
        taCenter: P := Point(Left + (Width - FEditLabel.Width) div 2,
          Top + Height + FLabelSpacing);
      end;
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                    Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TCustomLabeledCheckBox.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

procedure TCustomLabeledCheckBox.SetName(const Value: TComponentName);
var
  LClearText: Boolean;
begin
  if (csDesigning in ComponentState) and (FEditLabel <> nil) and
     ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
    FEditLabel.Caption := Value;
  LClearText := (csDesigning in ComponentState) and (Text = '');
  inherited SetName(Value);
  if LClearText then
    Text := '';
end;

procedure TCustomLabeledCheckBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEditLabel = nil then exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TCustomLabeledCheckBox.SetParentFont(const Value: Boolean);
begin
  if Assigned(FEditLabel) then
    FEditLabel.ParentFont := Value
  else
    inherited ParentFont := Value;
end;

procedure TCustomLabeledCheckBox.SetText(const Value: TCaption);
begin
  if Assigned(FEditLabel) then
    FEditLabel.Caption := Value
  else
    inherited Caption := Value;
end;

procedure TCustomLabeledCheckBox.SetupInternalLabel;
begin
  if Assigned(FEditLabel) then exit;
  FEditLabel := TBoundLabel.Create(Self);
  FEditLabel.FreeNotification(Self);
  FEditLabel.FocusControl := Self;
end;

{ TCustomLabeledCheckBox.TBoundLabel }

procedure TCustomLabeledCheckBox.TBoundLabel.AdjustBounds;
begin
  inherited AdjustBounds;
  if Owner is TCustomLabeledCheckBox then
    with Owner as TCustomLabeledCheckBox do
      SetLabelPosition(LabelPosition);
end;

end.
