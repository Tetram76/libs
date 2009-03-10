unit ExitPnl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons;

type
  TExitPanel = class;

  TAutoPosKind = (pkCenter, pkBorder);

  TBtnExitPanel = class(TPersistent)
  private
    { Déclarations privées }
    FExitPanel : TExitPanel;
    FBtn: TBitBtn;
    FOldGlyphChanged: TNotifyEvent;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  protected
    { Déclarations protégées }
    procedure GlyphChanged(Sender: TObject);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetDefault: Boolean;
    procedure SetDefault(Value: Boolean);
    function GetCancel: Boolean;
    procedure SetCancel(Value: Boolean);
    function GetModalResult: TModalResult;
    procedure SetModalResult(Value: TModalResult);
    function GetKind: TBitBtnKind;
    procedure SetKind(Value: TBitBtnKind);
    procedure SetCaption(Value: TCaption);
    function GetCaption: TCaption;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TExitPanel; ACaption: TCaption);
    destructor Destroy; override;
    {}
    procedure Assign(Source: TPersistent); override;
    {}
    property Btn: TBitBtn read FBtn;
  published
    { Déclarations publiées }
    property Caption: TCaption read GetCaption write SetCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
    property Cancel: Boolean read GetCancel write SetCancel;
    property Default: Boolean read GetDefault write SetDefault;
    property Kind: TBitBtnKind read GetKind write SetKind;
    property Visible : Boolean read GetVisible write SetVisible;
  end;

  TExitPanel = class(TCustomPanel)
  private
    { Déclarations privées }
    FBtnOk,
      FBtnCancel: TBtnExitPanel;
    FSpacing: Integer;
    FAutoPosKind: TAutoPosKind;
  protected
    { Déclarations protégées }
    procedure SetBtnHeight(Value: Integer);
    function GetBtnHeight: Integer;
    procedure SetBtnWidth(Value: Integer);
    function GetBtnWidth: Integer;
    procedure SetSpacing(Value: Integer);
    procedure MoveControls;
    procedure Resize; override;
    procedure Loaded; override;
    procedure SetOnOkClick(Value: TNotifyEvent);
    function GetOnOkClick: TNotifyEvent;
    procedure SetOnCancelClick(Value: TNotifyEvent);
    function GetOnCancelClick: TNotifyEvent;
    procedure SetBtnOk(const Btn: TBtnExitPanel);
    procedure SetBtnCancel(const Btn: TBtnExitPanel);
    procedure SetAutoPosKind(const Value: TAutoPosKind);
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Déclarations publiées }
    property BtnHeight: Integer read GetBtnHeight write SetBtnHeight default 29;
    property BtnWidth: Integer read GetBtnWidth write SetBtnWidth default 89;
    property BtnOk: TBtnExitPanel read FBtnOk write SetBtnOk;
    property BtnCancel: TBtnExitPanel read FBtnCancel write SetBtnCancel;
    property Spacing: Integer read FSpacing write SetSpacing default 8;
    property AutoPosKind : TAutoPosKind read FAutoPosKind write SetAutoPosKind default pkCenter;
    property Align;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property Font;
    property ParentFont;
    property Color;
    property ParentColor;
    property OnOkClick: TNotifyEvent read GetOnOkClick write SetOnOkClick;
    property OnCancelClick: TNotifyEvent read GetOnCancelClick write SetOnCancelClick;
    property OnEnter;
    property OnExit;
  end;


implementation


constructor TBtnExitPanel.Create(AOwner: TExitPanel; ACaption: TCaption);
begin
  inherited Create;

  FExitPanel := AOwner;

  FBtn:= TBitBtn.Create(AOwner);
  FBtn.Parent:= AOwner;
  FBtn.Width:= 89;
  FBtn.Height:= 29;
  FBtn.Caption:= ACaption;
  FOldGlyphChanged:= FBtn.Glyph.OnChange;
  FBtn.Glyph.OnChange:= GlyphChanged;
end;

destructor TBtnExitPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TBtnExitPanel.Assign(Source: TPersistent);
begin
  if Source is TBtnExitPanel then
    with TBtnExitPanel(Source) do
    begin
      Self.Kind:= Kind;
      Self.Caption:= Caption;
      Self.Glyph:= Glyph;
      Self.ModalResult:= ModalResult;
      Self.Cancel:= Cancel;
      Self.Default:= Default;
    end
  else
    inherited Assign(Source);
end;

function TBtnExitPanel.GetDefault: Boolean;
begin
  Result:= FBtn.Default;
end;

procedure TBtnExitPanel.SetDefault(Value: Boolean);
begin
  FBtn.Default:= Value;
end;

function TBtnExitPanel.GetCancel: Boolean;
begin
  Result:= FBtn.Cancel;
end;

procedure TBtnExitPanel.SetCancel(Value: Boolean);
begin
  FBtn.Cancel:= Value;
end;

function TBtnExitPanel.GetModalResult: TModalResult;
begin
  Result:= FBtn.ModalResult;
end;

procedure TBtnExitPanel.SetModalResult(Value: TModalResult);
begin
  FBtn.ModalResult:= Value;
end;

function TBtnExitPanel.GetKind: TBitBtnKind;
begin
  Result:= FBtn.Kind;
end;

procedure TBtnExitPanel.SetKind(Value: TBitBtnKind);
begin
  FBtn.Kind:= Value;
end;

function TBtnExitPanel.GetGlyph: TBitmap;
begin
  Result:= FBtn.Glyph;
end;

procedure TBtnExitPanel.SetGlyph(Value: TBitmap);
begin
  FBtn.Glyph.Assign(Value);
end;

procedure TBtnExitPanel.GlyphChanged(Sender: TObject);
var
  Glyphs: Integer;
begin
  if (FBtn.Glyph <> nil) and (FBtn.Glyph.Height > 0) then
  begin
    if FBtn.Glyph.Width mod FBtn.Glyph.Height = 0 then
    begin
      Glyphs:= FBtn.Glyph.Width div FBtn.Glyph.Height;
      if Glyphs > 4 then Glyphs:= 1;
      if Glyphs <> FBtn.NumGlyphs then
        FBtn.NumGlyphs:= Glyphs;
    end;
  end;
  if Assigned(FOldGlyphChanged) then
    FOldGlyphChanged(Sender);
end;

procedure TBtnExitPanel.SetCaption(Value: TCaption);
begin
  FBtn.Caption:= Value;
end;

function TBtnExitPanel.GetCaption: TCaption;
begin
  Result:= FBtn.Caption;
end;

function TBtnExitPanel.GetVisible: Boolean;
begin
  Result := FBtn.Visible;
end;

procedure TBtnExitPanel.SetVisible(const Value: Boolean);
begin
  FBtn.Visible := Value;
  FExitPanel.MoveControls;
end;

{ TExitPanel }

constructor TExitPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoPosKind := pkCenter;

  FSpacing:= 8;

  FBtnOk:= TBtnExitPanel.Create(Self, '&OK');
  FBtnOk.Btn.Kind:= bkOk;
  FBtnOk.Btn.Caption:= '&OK';
  FBtnCancel:= TBtnExitPanel.Create(Self, '&Annuler');
  FBtnCancel.Btn.Kind:= bkCancel;
  FBtnCancel.Btn.Caption:= '&Annuler';

  BevelOuter:= bvLowered;
  Caption:= '';
  ControlStyle:= ControlStyle - [csSetCaption];
  Width:= 300;
  Height := 37;
  Align:= alBottom;
end;

destructor TExitPanel.Destroy;
begin
  FBtnOk.Free;
  FBtnCancel.Free;
  inherited Destroy;
end;

procedure TExitPanel.Loaded;
begin
  inherited Loaded;
  MoveControls;
  Caption:= '';
end;

procedure TExitPanel.SetOnOkClick(Value: TNotifyEvent);
begin
  if Assigned(FBtnOk) and Assigned(FBtnOk.FBtn) then
    FBtnOk.FBtn.OnClick:= Value;
end;

function TExitPanel.GetOnOkClick: TNotifyEvent;
begin
  if Assigned(FBtnOk) and Assigned(FBtnOk.FBtn) then
    Result:= FBtnOk.FBtn.OnClick
  else
    Result:= nil;
end;

procedure TExitPanel.SetOnCancelClick(Value: TNotifyEvent);
begin
  if Assigned(FBtnCancel) and Assigned(FBtnCancel.FBtn) then
    FBtnCancel.FBtn.OnClick:= Value;
end;

function TExitPanel.GetOnCancelClick: TNotifyEvent;
begin
  if Assigned(FBtnCancel) and Assigned(FBtnCancel.FBtn) then
    Result:= FBtnCancel.FBtn.OnClick
  else
    Result:= nil;
end;

procedure TExitPanel.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing:= Value;
    MoveControls;
  end;
end;

procedure TExitPanel.SetBtnHeight(Value: Integer);
begin
  FBtnOk.Btn.Height:= Value;
  FBtnCancel.Btn.Height:= Value;
  MoveControls;
end;

function TExitPanel.GetBtnHeight: Integer;
begin
  Result:= FBtnOk.Btn.Height;
end;

procedure TExitPanel.SetBtnWidth(Value: Integer);
begin
  FBtnOk.Btn.Width:= Value;
  FBtnCancel.Btn.Width:= Value;
  MoveControls;
end;

function TExitPanel.GetBtnWidth: Integer;
begin
  Result:= FBtnOk.Btn.Width;
end;

procedure TExitPanel.SetBtnOk(const Btn: TBtnExitPanel);
begin
  FBtnOk.Assign(Btn);
end;

procedure TExitPanel.SetBtnCancel(const Btn: TBtnExitPanel);
begin
  FBtnCancel.Assign(Btn);
end;

procedure TExitPanel.MoveControls;
var
  TailleW : Integer;
begin
  if FBtnOk.Visible then
    FBtnOk.Btn.Top:= (Height - FBtnOk.Btn.Height) div 2
  else
    FBtnOk.Btn.Top := -100;
  if FBtnCancel.Visible then
    FBtnCancel.Btn.Top:= (Height - FBtnCancel.Btn.Height) div 2
  else
    FBtnCancel.Btn.Top := -100;

  if FAutoPosKind = pkCenter then
  begin
    TailleW := 0;
    if FBtnOk.Visible then
      TailleW := FBtnOk.Btn.Width;
    if FBtnCancel.Visible then
      Inc(TailleW, FBtnCancel.Btn.Width);
    if FBtnOk.Visible and FBtnCancel.Visible then
      Inc(TailleW, FSpacing);

    if FBtnOk.Visible and FBtnCancel.Visible then
    begin
      FBtnOk.Btn.Left:= (Width - (TailleW)) div 2;
      FBtnCancel.Btn.Left:= FBtnOk.Btn.Left + FSpacing + FBtnOk.Btn.Width;
    end
    else
    if FBtnOk.Visible then
      FBtnOk.Btn.Left:= (Width - (TailleW)) div 2
    else
    if FBtnCancel.Visible then
      FBtnCancel.Btn.Left:= (Width - (TailleW)) div 2;
  end
  else
  begin
    FBtnOK.Btn.Left := FSpacing;
    FBtnCancel.Btn.Left := Width - FBtnCancel.Btn.Width - FSpacing;
  end;
end;

procedure TExitPanel.Resize;
begin
  inherited Resize;
  MoveControls;
end;

procedure TExitPanel.SetAutoPosKind(const Value: TAutoPosKind);
begin
  if Value <> FAutoPosKind then
  begin
    FAutoPosKind := Value;
    MoveControls;
  end;
end;

end.

