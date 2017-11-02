unit ComboCheck;

{.$D-}
{$J+}
interface

uses
  Dialogs, Windows, Messages, SysUtils, Classes, Graphics, Controls, Menus, StdCtrls, Math, ExtCtrls, IniFiles, Forms,
{$IFDEF CLR}
  WinUtils,
{$ENDIF}
  Types;

type
  TFBorderChk = (CCBNone, CCBflat, CCB3d);
  TVerticalAlign = (alTop, alBottom, alCenter);
  TComboChangeEvent = TNotifyEvent;
  TBeforeShowPopEvent = procedure(Sender: TObject; Menu: TPopupMenu; var Continue: Boolean) of object;
  TValidValueOptions = (vvoChecked, vvoUnchecked, vvoMissing, vvoUnknown, vvoOthers);
  TValidValueOption = set of TValidValueOptions;

  TValidValue = class(TPersistent)
  private
    FOtherValues: TStrings;
    FValueOption: TValidValueOption;
    procedure SetOtherValues(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property OtherValues: TStrings read FOtherValues write SetOtherValues;
    property ValueOption: TValidValueOption read FValueOption write FValueOption;
  end;

  TCustomComboCheck = class;
  TSubItems = class;

  TCollectionOwnedByCustomComboCheck = class(TCollection)
  private
    FCustomComboCheck: TCustomComboCheck;
    FCaptionComplet: Boolean;
    FCaptionSeparateur: String;
    procedure SetCaptionComplet(const Value: Boolean);
    procedure SetCaptionSeparateur(const Value: String);
  public
    constructor Create(CustomComboCheck: TCustomComboCheck);
  published
    property CaptionComplet: Boolean read FCaptionComplet write SetCaptionComplet;
    property Separateur: String read FCaptionSeparateur write SetCaptionSeparateur;
  end;

  TSubItem = class(TCollectionItem)
  private
    FValeur: Integer;
    FCaption: String;
    FSubItems: TSubItems;
    FVisible: Boolean;
    FEnabled: Boolean;
    procedure SetValeur(Value: Integer);
    procedure SetCaption(const Value: String);
    procedure SetVisible(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetSubItems(Value: TSubItems);
    function ComboParent: TCollectionOwnedByCustomComboCheck;
  protected
    function GetDisplayName: String; override;
  public
    Data: TObject;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Valeur: Integer read FValeur write SetValeur;
    property Caption: String read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property SubItems: TSubItems read FSubItems write SetSubItems;
  end;

  TSubItems = class(TCollectionOwnedByCustomComboCheck)
  private
    FOwner: TSubItem;
    function GetItem(Index: Integer): TSubItem;
    procedure SetItem(Index: Integer; Value: TSubItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(CustomComboCheck: TCustomComboCheck; AOwner: TSubItem); reintroduce;
    procedure Assign(Source: TPersistent); override;
    function Add: TSubItem; overload;
    function Add(Chaine: String): TSubItem; overload;
    procedure Insert(Index: Integer; Chaine: String);
    function IndexOf(Chaine: String): Integer;
    property Items[Index: Integer]: TSubItem read GetItem write SetItem; default;
  end;

  TItems = class(TCollectionOwnedByCustomComboCheck)
  private
    function GetItem(Index: Integer): TSubItem;
    procedure SetItem(Index: Integer; Value: TSubItem);
  protected
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Add: TSubItem; overload;
    function Add(Chaine: String): TSubItem; overload;
    procedure Insert(Index: Integer; Chaine: String);
    function IndexOf(Chaine: String): Integer;
    property Items[Index: Integer]: TSubItem read GetItem write SetItem; default;
  end;

  TCustomComboCheck = class(TCustomLabel)
  private
    TextTop, TextLeft, TextW: Integer;
    HintToShow, HintToShowFinTimer, HintShown: Boolean;
    FShowCaptionHint: boolean;
    FAssignHint: boolean;
    FCheckVisible: Boolean;
    FVerticalAlign: TVerticalAlign;
    FDefaultValueUnchecked: Integer;
    FDefaultValueChecked: Integer;
    FHintWindow: THintWindow;
    FValeur: Integer;
    FOldValeur: Integer;
    FBmpChk: TBitmap;
    FBmpDown: TBitmap;
    FChecked: Boolean;
    FTransparentColor: TColor;
    FBorderChk: TFBorderChk;
    FValueMissing: Integer;
    FValueUnknown: Integer;
    FCaptionMissing: String;
    FCaptionUnknown: String;
    FCaptionUnchecked: String;
    FFillCaption: Boolean;
    FBackColor: TColor;
    FCheckedBold: Boolean;
    FTextClick: Boolean;
    FOnChange: TComboChangeEvent;
    FOnPopEmpty: TNotifyEvent;
    FTimer: TTimer;
    FInterval: Cardinal;
    FStore: Boolean;
    FOptionValidValue: TValidValue;
    FItems: TItems;
    FBeforeShowPop: TBeforeShowPopEvent;
    procedure SetItems(Values: TItems);
    procedure CleanUp;
    procedure UpdateData(Sender: TObject); dynamic;
    procedure SetVerticalAlign(Values: TVerticalAlign);
    procedure SetFillCaption(Values: Boolean);
    procedure SetBackColor(Values: TColor);
    procedure SetCaption; dynamic;
    function GetChecked: Boolean;
    procedure DoSetChecked(const Values: Boolean); dynamic;
    procedure SetChecked(const Values: Boolean);
    procedure SetBorderChk(Values: TFBorderChk);
    procedure SetCaptionEx(Index: Integer; Values: String);
    procedure SetCheckVisible(Values: Boolean);
    procedure SetDefaultValueChecked(Value: Integer);
    procedure SetDefaultValueUnchecked(Value: Integer);
    procedure SetValue(Values: Integer);
    function GetValue: Integer;
    procedure FinTimer(Sender: TObject);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure DoMouseLeave;
    procedure ShowHintWindow;
    class procedure InitImg;
    class procedure CleanImg;
    function FillText(const Texte: String; Canvas: TCanvas; Longueur: Integer): String;
  public
    LastItemData: TObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure AssignItemPop;
    procedure PopDwn(Sender: TObject);
    procedure ClickOnItem(Sender: TObject); dynamic;
    procedure Paint; override;
    procedure LoadChkDW;
    procedure LoadImage;
    function GetCaption(Value: Integer; out Data: TObject): string; overload;
  published
    procedure ShowPop;
    procedure Repaint; override;
    function GetCaption(Value: Integer): string; overload;
    property Visible;
    property Checked: Boolean read GetChecked write SetChecked;
    function PossibleValue(Value: Integer): Boolean;
    function ValidValue(Value: Integer): Boolean;
    function ValidCurrentValue: Boolean;
    property Anchors;
    property Alignment;
    property Border: TFBorderChk read FBorderChk write SetBorderChk default CCBNone;
    property CaptionMissing: String index 1 read FCaptionMissing write SetCaptionEx;
    property CaptionUnknown: String index 2 read FCaptionUnknown write SetCaptionEx;
    property CaptionUnchecked: String index 3 read FCaptionUnchecked write SetCaptionEx;
    property ColorTrans: TColor read FTransparentColor;
    property DefaultValueChecked: Integer read FDefaultValueChecked write SetDefaultValueChecked default 1;
    property DefaultValueUnchecked: Integer read FDefaultValueUnchecked write SetDefaultValueUnchecked default -1;
    property ValueMissing: Integer read FValueMissing write FValueMissing default 99;
    property ValueUnknown: Integer read FValueUnknown write FValueUnknown default 98;
    property Enabled;
    property Font;
    property PropertiesStored: Boolean read FStore write FStore;
    property Value: Integer read GetValue write SetValue stored False;
    property OldValue: Integer read FOldValeur stored False;
    property ShowHint;
    property ParentShowHint;
    property VerticalAlignement: TVerticalAlign read FVerticalAlign write SetVerticalAlign default alCenter;
    property Transparent;
    property CheckVisible: Boolean read FCheckVisible write SetCheckVisible default True;
    property FillCaption: Boolean read FFillCaption write SetFillCaption default True;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property CheckedCaptionBold: Boolean read FCheckedBold write FCheckedBold default True;
    property TextClick: Boolean read FTextClick write FTextClick default True;
    property OnChange: TComboChangeEvent read FOnChange write FOnChange;
    property BeforeShowPop: TBeforeShowPopEvent read FBeforeShowPop write FBeforeShowPop;
    property OnPopEmpty: TNotifyEvent read FOnPopEmpty write FOnPopEmpty;
    property CaptionHintInterval: Cardinal read FInterval write FInterval default 1000;
    property ShowCaptionHint: boolean read FShowCaptionHint write FShowCaptionHint;
    property AssignHint: boolean read FAssignHint write FAssignHint;
    property OptionValidValue: TValidValue read FOptionValidValue write FOptionValidValue;

    property Items: TItems read FItems write SetItems;
  end;

  TLightComboCheck = class(TCustomComboCheck)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
  published
    { Déclarations publiées }
  end;

procedure Register;

implementation

uses
  System.UITypes;

{$R CheckCombo.res}

const
  Offset = 2;  // bordure de dessin des bitmaps

type
  TMenuItem = class(Menus.TMenuItem)
    SubItem: TSubItem;
  end;

procedure Register;
begin
  RegisterComponents('Tetram', [TLightComboCheck]);
end;

{TKPopupMenu}

type
  TKPopupSplitType = (pstBalanced, pstFillFirst);

  TPopupMenu = class(Menus.TPopupMenu)
  private
    FBreakType: TMenuBreak;
    FPopupSplitType: TKPopupSplitType;
  protected
  public
    procedure Popup(X, Y: integer); override;
    constructor Create(AOwner: TComponent); override;
  published
    property BreakType: TMenuBreak read FBreakType write FBreakType default mbBarBreak;
    property PopupSplitType: TKPopupSplitType read FPopupSplitType write FPopupSplitType default pstBalanced;
  end;

constructor TPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  BreakType := mbBarBreak;
  PopupSplitType := pstBalanced;
end;

procedure TPopupMenu.Popup(X, Y: integer);
var
  ACanvas: TCanvas;
  ItemsOnScreen: integer;

  procedure ClearBreakMark(MenuItems: Menus.TMenuItem);
  var
    I: integer;
  begin
    for I := 0 to MenuItems.Count - 1 do
    begin
      MenuItems[I].Break := mbNone;
      if MenuItems[I].Count > 0 then
        ClearBreakMark(MenuItems[I]);
    end;
  end;

  procedure BreakItems(MenuItems: Menus.TMenuItem);
  var
    I, RCount, BreakCount: integer;
  begin
    BreakCount := MenuItems.Count div ItemsOnScreen;

    if PopupSplitType = pstBalanced then
    begin
      RCount := MenuItems.Count;
      if MenuItems.Count mod (BreakCount + 1) <> 0 then
        RCount := RCount + (BreakCount + 1) - MenuItems.Count mod (BreakCount + 1);
      for I := 1 to BreakCount do
        MenuItems[I * (RCount div (BreakCount + 1))].Break := BreakType;
    end
    else
    begin
      for I := 1 to (MenuItems.Count div ItemsOnScreen) do
        MenuItems[I * ItemsOnScreen].Break := BreakType;
    end;
    for I := 0 to MenuItems.Count - 1 do
      if MenuItems[I].Count > 0 then
        BreakItems(MenuItems[I]);
  end;

begin
  ACanvas := TControlCanvas.Create;
  with ACanvas do
    try
      try
        Handle := GetDC(WindowHandle);
        Font := Screen.MenuFont;
        {4 is magic number = diference between TextHeight method result and real TMenuItem Height}
        ItemsOnScreen := Screen.Height div (ACanvas.TextHeight('I') + 4);
      finally
        Handle := 0;
      end;
    finally
      ACanvas.Free;
    end;
  ClearBreakMark(Items);
  BreakItems(Items);
  inherited Popup(X, Y);
end;

function TItems.Add: TSubItem;
begin
  Result := TSubItem(inherited Add);
end;

function TItems.Add(Chaine: String): TSubItem;
var
  i: Integer;
begin
  Result := Add;
  i := Pos('=', Chaine);
  if i = 0 then
    Result.Caption := Chaine
  else
  begin
    Result.Caption := Copy(Chaine, 1, i - 1);
    Result.Valeur := StrToInt(Copy(Chaine, i + 1, Length(Chaine)));
  end;
end;

function TItems.IndexOf(Chaine: String): Integer;

  function Process(Item: TSubItem): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if Item.SubItems.Count > 0 then
    begin
      for i := 0 to Item.SubItems.Count - 1 do
      begin
        Result := Process(Item);
        if Result <> -1 then
          Exit;
      end;
    end
    else if Format('%s=%d', [Item.Caption, Item.Valeur]) = Chaine then
      Result := Item.ID;
  end;

var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    Result := Process(Items[i]);
    if Result <> -1 then
      Exit;
  end;
end;

procedure TItems.Insert(Index: Integer; Chaine: String);
begin
  if (Index < 0) or (Index > Count) then
    Exit;
  Add(Chaine).Index := Index;
end;

function TItems.GetItem(Index: Integer): TSubItem;
begin
  Result := TSubItem(inherited GetItem(Index));
end;

function TItems.GetOwner: TPersistent;
begin
  Result := FCustomComboCheck;
end;

procedure TItems.SetItem(Index: Integer; Value: TSubItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TItems.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TStrings(Source).Count - 1 do
        Add(TStrings(Source)[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end
  else
    inherited Assign(Source);
end;

procedure TItems.AssignTo(Dest: TPersistent);

  procedure Process(Level: Integer; Avant: string; Item: TSubItem);
  var
    s: string;
    i: Integer;
  begin
    s := Trim(Item.Caption);
    if Avant <> '' then
      Avant := Avant + Separateur;
    if Item.SubItems.Count = 0 then
    begin
      if (s <> '-') then
        s := s + '=' + IntToStr(Item.Valeur);
      if (s <> '-') or (Level > 0) then
      begin
        if CaptionComplet then
          TStrings(Dest).Add(Avant + s)
        else
          TStrings(Dest).Add(s);
      end;
    end
    else
      for i := 0 to Item.SubItems.Count - 1 do
      begin
        Process(Level + 1, Avant + s, Item.SubItems[i]);
      end;
  end;

var
  i: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      for i := 0 to Count - 1 do
        Process(0, '', Items[i]);
    finally
      TStrings(Dest).EndUpdate;
    end;
    Exit;
  end
  else
    inherited AssignTo(Dest);
end;

function TSubItems.Add: TSubItem;
begin
  Result := TSubItem(inherited Add);
end;

function TSubItems.Add(Chaine: String): TSubItem;
var
  i: Integer;
begin
  Result := Add;
  i := Pos('=', Chaine);
  if i = 0 then
    Result.Caption := Chaine
  else
  begin
    Result.Caption := Copy(Chaine, 1, i - 1);
    Result.Valeur := StrToInt(Copy(Chaine, i + 1, Length(Chaine)));
  end;
end;

function TSubItems.IndexOf(Chaine: String): Integer;

  function Process(Item: TSubItem): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if Item.SubItems.Count > 0 then
    begin
      for i := 0 to Item.SubItems.Count - 1 do
      begin
        Result := Process(Item);
        if Result <> -1 then
          Exit;
      end;
    end
    else if Format('%s=%d', [Item.Caption, Item.Valeur]) = Chaine then
      Result := Item.ID;
  end;

var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    Result := Process(Items[i]);
    if Result <> -1 then
      Exit;
  end;
end;

procedure TSubItems.Insert(Index: Integer; Chaine: String);
begin
  if (Index < 0) or (Index > Count) then
    Exit;
  Add(Chaine).Index := Index;
end;

constructor TSubItems.Create(CustomComboCheck: TCustomComboCheck; AOwner: TSubItem);
begin
  inherited Create(CustomComboCheck);
  FOwner := AOwner;
end;

procedure TSubItems.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to TStrings(Source).Count - 1 do
        Add(TStrings(Source)[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end
  else
    inherited Assign(Source);
end;

function TSubItems.GetItem(Index: Integer): TSubItem;
begin
  Result := TSubItem(inherited GetItem(Index));
end;

function TSubItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSubItems.SetItem(Index: Integer; Value: TSubItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSubItem.Assign;
begin
  if Source is TSubItem then
  begin
    Caption := TSubItem(Source).Caption;
    Valeur := TSubItem(Source).Valeur;
    Data := TSubItem(Source).Data;
    Exit;
  end;
  inherited Assign(Source);
end;

constructor TSubItem.Create(Collection: TCollection);
begin
  if not (Collection is TCollectionOwnedByCustomComboCheck) then
    raise Exception.Create('Erreur');
  inherited Create(Collection);
  with Collection as TCollectionOwnedByCustomComboCheck do
  begin
    FSubItems := TSubItems.Create(FCustomComboCheck, Self);
    FSubItems.FCaptionComplet := FCaptionComplet;
    FSubItems.FCaptionSeparateur := FCaptionSeparateur;
  end;
  Caption := 'Item' + IntToStr(Index);
  Valeur := Index;
  Visible := True;
  Enabled := True;
end;

destructor TSubItem.Destroy;
begin
  FSubItems.Free;
  inherited;
end;

function TSubItem.GetDisplayName;
begin
  Result := FCaption;
  if Result = '-' then
    Result := '-------------------'
  else
    Result := Result + ' = ' + IntToStr(FValeur);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TSubItem.SetValeur(Value: Integer);
begin
  if FValeur = Value then
    Exit;
  FValeur := Value;
  ComboParent.FCustomComboCheck.Invalidate;
end;

procedure TSubItem.SetCaption(const Value: string);
begin
  if FCaption = Value then
    Exit;
  FCaption := Value;
  ComboParent.FCustomComboCheck.Invalidate;
end;

procedure TSubItem.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then
    Exit;
  FVisible := Value;
  ComboParent.FCustomComboCheck.Invalidate;
end;

procedure TSubItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;
  FEnabled := Value;
  ComboParent.FCustomComboCheck.Invalidate;
end;

procedure TSubItem.SetSubItems(Value: TSubItems);
begin
  FSubItems.Assign(Value);
end;

function TSubItem.ComboParent: TCollectionOwnedByCustomComboCheck;
begin
  Result := (Collection as TCollectionOwnedByCustomComboCheck);
end;

function MinimizeText(const Texte: String; Canvas: TCanvas; Longueur: Integer; var Retour: Boolean): String;
begin
  Result := Texte;
  Retour := False;
  if Canvas.TextWidth(Result) <= Longueur then
    Exit;
  while (Result <> '') and (Canvas.TextWidth(Result + '...') > Longueur) do
    Delete(Result, Length(Result), 1);
  Result := Result + '...';
  Retour := True;
end;

const
  CountCombo: Integer = 0;

var
  FImgMissing: TBitmap;
  FImgUnknown: TBitmap;
  FImgNotVisible: TBitmap;
  FImgOnEnable: TBitmap;
  FImgOnDisable: TBitmap;
  FImgOffEnable: TBitmap;
  FImgOffDisable: TBitmap;
  FImgDw: TBitmap;
  FPopup: TPopupMenu;

constructor TValidValue.Create;
begin
  inherited;
  FOtherValues := TStringList.Create;
  FValueOption := [vvoChecked, vvoUnchecked, vvoMissing, vvoUnknown, vvoOthers];
end;

destructor TValidValue.Destroy;
begin
  FOtherValues.Free;
  inherited;
end;

procedure TValidValue.SetOtherValues(Value: TStrings);
begin
  FOtherValues.Assign(Value);
end;

function TCustomComboCheck.FillText(const Texte: String; Canvas: TCanvas; Longueur: Integer): String;
var
  CharFill: Char;
  b: Boolean;
begin
  if FFillCaption then
    CharFill := '.'
  else
    CharFill := ' ';
  Result := '';
  if Alignment <> taLeftJustify then
    Result := Texte;
  b := False;
  case Alignment of
    taRightJustify:
      while Canvas.TextWidth(Result) < Longueur do
        Result := CharFill + Result;
    taCenter:
      while Canvas.TextWidth(Result) < Longueur do
      begin
        if b then
          Result := CharFill + Result
        else
          Result := Result + CharFill;
        b := not b;
      end;
    else
      while Canvas.TextWidth(Texte + Result) < Longueur do
        Result := Result + CharFill;
  end;
end;

class procedure TCustomComboCheck.InitImg;

  procedure InitImage(var i: TBitmap; Res: String);
  begin
    if not Assigned(i) then
      i := TBitmap.Create;
    with i do
    begin
      Transparent := True;
      LoadFromResourceName(HInstance, Res);
    end;
  end;

begin
  InitImage(FImgMissing, 'CheckMissing');
  InitImage(FImgUnknown, 'CheckUnknown');
  InitImage(FImgNotVisible, 'CheckNotVisible');
  InitImage(FImgOnEnable, 'CheckOnEnable');
  InitImage(FImgOnDisable, 'CheckOnDisable');
  InitImage(FImgOffEnable, 'CheckOffEnable');
  InitImage(FImgOffDisable, 'CheckOffDisable');
  InitImage(FImgDw, 'CheckDw');
end;

constructor TCustomComboCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Inc(CountCombo);
  if CountCombo = 1 then
    InitImg;

  FItems := TItems.Create(Self);
  LastItemData := nil;
  FValeur := -999999999;
  Color := clBtnFace;
  SetBounds(0, 0, 120, 20);
  FCheckVisible := True;
  FFillCaption := True;
  FBackColor := clBtnFace;
  AutoSize := False;
  Enabled := True;
  FBorderChk := CCBNone;
  FCheckedBold := True;
  FTextClick := True;
  FChecked := False;
  FVerticalAlign := alCenter;
  FTimer := nil;
  FInterval := 1000;
  FHintWindow := THintWindowClass.Create(Self);
  HintToShow := False;
  HintToShowFinTimer := False;
  FShowCaptionHint := False;
  HintShown := False;
  FDefaultValueChecked := 1;
  FDefaultValueUnchecked := -1;
  FValueMissing := 99;
  FValueUnknown := 98;
  FCaptionMissing := '';
  FCaptionUnknown := '';
  FOptionValidValue := TValidValue.Create;
  SetCaption;
  LoadChkDW;
  LoadImage;
end;

class procedure TCustomComboCheck.CleanImg;
  procedure Libere(var i: TBitmap); overload;
  begin
    if Assigned(i) then
      i.Free;
    i := nil;
  end;

  procedure Libere(var i: TPopupMenu); overload;
  begin
    if Assigned(i) then
      i.Free;
    i := nil;
  end;

begin
  Libere(FImgMissing);
  Libere(FImgUnknown);
  Libere(FImgNotVisible);
  Libere(FImgOnEnable);
  Libere(FImgOnDisable);
  Libere(FImgOffEnable);
  Libere(FImgOffDisable);
  Libere(FImgDw);
  Libere(FPopup);
end;

destructor TCustomComboCheck.Destroy;
begin
  FHintWindow.Free;
  FOptionValidValue.Free;
  FItems.Free;
  inherited Destroy;

  Dec(CountCombo);
  if CountCombo = 0 then
    CleanImg;
end;

procedure TCustomComboCheck.LoadImage;
begin
  if FCheckVisible then
  begin
    if Value = FValueMissing then
      FBmpChk := FImgMissing
    else if Value = FValueUnknown then
      FBmpChk := FImgUnknown
    else
    begin
      if FChecked then
      begin
        if Enabled then
          FBmpChk := FImgOnEnable
        else
          FBmpChk := FImgOnDisable;
      end
      else
      begin
        if Enabled then
          FBmpChk := FImgOffEnable
        else
          FBmpChk := FImgOffDisable;
      end;
    end;
  end
  else
    FBmpChk := FImgNotVisible;
end;

procedure TCustomComboCheck.LoadChkDW;
begin
  FBmpDown := FImgDw;
end;

procedure TCustomComboCheck.SetCaption;
var
  s: string;
begin
  s := GetCaption(Value, LastItemData);
  if not FChecked and (FCaptionUnchecked <> '') then
    s := FCaptionUnchecked;
  if (Value = FValueMissing) and (FCaptionMissing <> '') then
    s := FCaptionMissing;
  if (Value = FValueUnknown) and (FCaptionUnknown <> '') then
    s := FCaptionUnknown;
  if (csDesigning in ComponentState) then
    s := FCaptionUnchecked;
  if (Trim(s) = '') and (csDesigning in ComponentState) then
    s := Name;
  if Caption <> s then
    Caption := s;
end;

function TCustomComboCheck.GetCaption(Value: Integer): string;
var
  dummy: TObject;
begin
  Result := GetCaption(Value, dummy);
end;

function TCustomComboCheck.GetCaption(Value: Integer; out Data: TObject): string;

  function Process(Item: TSubItem; var Retour: String; out outData: TObject): Boolean;
  var
    s: string;
    i: Integer;
  begin
    Result := False;
    s := Trim(Item.Caption);
    if s <> '-' then
    begin
      if Item.SubItems.Count = 0 then
      begin
        if (Item.Valeur = Value) then
        begin
          Retour := s;
          outData := Item.Data;
          Result := True;
        end;
      end
      else
        for i := 0 to Item.SubItems.Count - 1 do
        begin
          Result := Process(Item.SubItems[i], Retour, outData);
          if Result then
          begin
            if Item.ComboParent.CaptionComplet then
              Retour := s + Item.ComboParent.Separateur + Retour;
            Exit;
          end;
        end;
    end;
  end;

var
  i: Integer;
  Retour: String;
begin
  Data := nil;
  Result := '';
  Retour := '';
  for i := 0 to Items.Count - 1 do
    if Process(Items[i], Retour, Data) then
    begin
      Result := Retour;
      Exit;
    end;
end;

function TCustomComboCheck.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TCustomComboCheck.DoSetChecked(const Values: Boolean);
begin
  if Values = FChecked then
    Exit;
  //  FOldValeur := Value;
  //  FChecked := Values;
  if Values then
    Value := FDefaultValueChecked
  else
    Value := FDefaultValueUnchecked;
  //  SetCaption;
  //  UpdateData(Self);
end;

procedure TCustomComboCheck.SetChecked(const Values: Boolean);
begin
  DoSetChecked(Values);
end;

procedure TCustomComboCheck.SetItems(Values: TItems);
begin
  FItems.Assign(Values);
end;

procedure TCustomComboCheck.SetBorderChk(Values: TFBorderChk);
begin
  if Values = FBorderChk then
    Exit;
  FBorderChk := Values;
  Invalidate;
end;

procedure TCustomComboCheck.SetCaptionEx(Index: Integer; Values: String);
begin
  case Index of
    1: FCaptionMissing := Values;
    2: FCaptionUnknown := Values;
    3: FCaptionUnchecked := Values;
  end;
  Invalidate;
end;

function TCustomComboCheck.ValidValue(Value: Integer): Boolean;
begin
  Result := ((vvoChecked in FOptionValidValue.ValueOption) and PossibleValue(Value)) or ((vvoMissing in FOptionValidValue.ValueOption) and (Value = FValueMissing)) or ((vvoUnknown in FOptionValidValue.ValueOption) and (Value = FValueUnknown)) or ((vvoUnchecked in FOptionValidValue.ValueOption) and not FChecked) or ((vvoOthers in FOptionValidValue.ValueOption) and (FOptionValidValue.OtherValues.IndexOf(IntToStr(Value)) <> -1));
end;

function TCustomComboCheck.ValidCurrentValue: Boolean;
begin
  Result := ValidValue(Value);
end;

function TCustomComboCheck.PossibleValue(Value: Integer): Boolean;

  function Process(Item: TSubItem): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if (Trim(Item.Caption) <> '-') then
      if Item.SubItems.Count = 0 then
        Result := (Item.Valeur = Value) and (Value <> FValueMissing) and (Value <> FValueUnknown)
      else
        for i := 0 to Item.SubItems.Count - 1 do
        begin
          Result := Process(Item.SubItems[i]);
          if Result then
            Exit;
        end;
  end;

var
  i: Integer;
begin
  Result := False;
  for i := 0 to Items.Count - 1 do
  begin
    Result := Process(Items[i]);
    if Result then
      Exit;
  end;
end;

procedure TCustomComboCheck.SetFillCaption(Values: Boolean);
begin
  if Values = FFillCaption then
    Exit;
  FFillCaption := Values;
  Invalidate;
end;

procedure TCustomComboCheck.SetVerticalAlign(Values: TVerticalAlign);
begin
  if Values = FVerticalAlign then
    Exit;
  FVerticalAlign := Values;
  Invalidate;
end;

procedure TCustomComboCheck.SetDefaultValueChecked(Value: Integer);
begin
  if Value = FDefaultValueChecked then
    Exit;
  FChecked := FValeur <> FDefaultValueUnchecked;
  FDefaultValueChecked := Value;
  Invalidate;
end;

procedure TCustomComboCheck.SetDefaultValueUnchecked(Value: Integer);
begin
  if Value = FDefaultValueUnchecked then
    Exit;
  FChecked := FValeur <> Value;
  FDefaultValueUnchecked := Value;
  Invalidate;
end;

procedure TCustomComboCheck.SetCheckVisible(Values: Boolean);
begin
  if Values = FCheckVisible then
    Exit;
  FCheckVisible := Values;
  Invalidate;
end;

procedure TCustomComboCheck.SetBackColor(Values: TColor);
begin
  if Values = FBackColor then
    Exit;
  FBackColor := Values;
  if not Transparent then
    Invalidate;
end;

procedure TCustomComboCheck.SetValue(Values: Integer);
begin
  if (Values = FValeur) then
    Exit;
  FOldValeur := Value;
  FChecked := Values <> FDefaultValueUnchecked;
  FValeur := Values;
  UpdateData(Self);
end;

function TCustomComboCheck.GetValue: Integer;
begin
  if Checked then
    Result := FValeur
  else
    Result := DefaultValueUnchecked;
end;

procedure TCustomComboCheck.UpdateData(Sender: TObject);
begin
  Invalidate;
  SetCaption;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomComboCheck.Repaint;
begin
  Invalidate;
  inherited;
end;

procedure TCustomComboCheck.CleanUp;
var
  i: integer;
begin
  with FPopup do
  begin
    for i := Items.Count - 1 downto 0 do
      Items[i].Free;
  end;
end;

procedure TCustomComboCheck.FinTimer(Sender: TObject);
begin
  DoMouseLeave;
  HintToShowFinTimer := False;
end;

procedure TCustomComboCheck.DoMouseLeave;
begin
  FHintWindow.ReleaseHandle;
  HintShown := False;
  FTimer.Free;
  FTimer := nil;
end;

procedure TCustomComboCheck.Paint;
var
  R: TRect;
  Target: TPoint;
  OldBkMode, TextH: Integer;
  S, sFill: String;
begin
  SetCaption;
  with Canvas do
  begin
    LoadImage;
    Pen.Color := FBackColor;
    Brush.Color := FBackColor;
    if not Transparent then
      FillRect(ClientRect);

    Font.Assign(Self.Font);
    if FCheckedBold and FChecked then
      Font.Style := Font.Style + [fsBold];
    OldBkMode := GetBkMode(Canvas.Handle);
    if Transparent then
      SetBkMode(Canvas.Handle, 1)
    else
      SetBkMode(Canvas.Handle, 2);
    FTransparentColor := FBmpChk.Canvas.Pixels[0, 15];

    TextH := Max(TextHeight(Caption), FBmpDown.Height);
    if FCheckVisible then
      TextH := Max(TextH, FBmpChk.Height);
    case FVerticalAlign of
      alCenter: TextTop := (ClientHeight - TextHeight(Caption)) div 2;
      alBottom: TextTop := ClientHeight - Offset - (TextHeight(Caption) + TextH) div 2;
      else TextTop := Offset + (TextH - TextHeight(Caption)) div 2;
    end;
    TextW := ClientWidth - 3 * Offset - FBmpDown.Width;
    TextLeft := Offset * 2;
    if FCheckVisible then
    begin
      Dec(TextW, FBmpChk.Width + Offset);
      Inc(TextLeft, FBmpChk.Width);
    end;
    if Enabled then
      Font.Color := Self.Font.Color
    else
      Font.Color := clInactiveCaption;
    S := MinimizeText(Caption, Canvas, TextW, HintToShow);
    HintToShowFinTimer := HintToShow;
    sFill := FillText(S, Canvas, TextW);
    if Alignment <> taLeftJustify then
      TextOut(TextLeft, TextTop, sFill)
    else
    begin
      TextOut(TextLeft + TextW - Canvas.TextWidth(sFill), TextTop, sFill);
      TextOut(TextLeft, TextTop, S);
    end;

    if FCheckVisible then
    begin
      Target.x := Offset;
      case FVerticalAlign of
        alCenter: Target.y := (ClientHeight - FBmpChk.Height) div 2;
        alTop: Target.y := Offset;
        alBottom: Target.y := ClientHeight - Offset - FBmpChk.Height;
      end;
      Draw(Target.x, Target.y, FBmpChk);
    end;

    Target.x := ClientWidth - FBmpDown.Width - Offset;
    case FVerticalAlign of
      alCenter: Target.y := (ClientHeight - FBmpDown.Height) div 2;
      alTop: Target.y := Offset;
      alBottom: Target.y := ClientHeight - Offset - FBmpDown.Height;
    end;
    Draw(Target.x, Target.y, FBmpDown);

    case FBorderChk of
      //CCBNone:
      CCBflat:
      begin
        Pen.Color := clBtnShadow;
        R := ClientRect;
        InflateRect(R, -1, -1);
        MoveTo(R.Left, R.Top);
        LineTo(R.Right, R.Top);
        LineTo(R.Right, R.Bottom);
        LineTo(R.Left, R.Bottom);
        LineTo(R.Left, R.Top);
      end;
      CCB3d:
      begin
        Pen.Color := clBtnShadow;
        R := ClientRect;
        InflateRect(R, -1, -1);
        MoveTo(R.Left, R.Top);
        LineTo(R.Right, R.Top);
        LineTo(R.Right, R.Bottom);
        Pen.Color := clWhite;
        LineTo(R.Left, R.Bottom);
        LineTo(R.Left, R.Top);
      end;
    end;
    SetBkMode(Canvas.Handle, OldBkMode);
  end;
end;

procedure TCustomComboCheck.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (csDesigning in ComponentState) then
    Exit;
  if (FCheckVisible and (x < FBmpChk.Width)) or (x > (ClientWidth - FBmpDown.Width)) then
    DoMouseLeave;
  if Enabled then
  begin
    if (FCheckVisible and (x < FBmpChk.Width)) then
      SetChecked(not FChecked);
    if (x > (ClientRect.Right - FBmpDown.Width)) or (FTextClick and ((x > FBmpChk.Width) or not FCheckVisible)) then
      ShowPop;
  end;
  Invalidate;
end;

procedure TCustomComboCheck.ShowPop;
var
  p: TPoint;
  CanContinue: Boolean;
begin
  AssignItemPop;
  p := ClientToScreen(Point(self.width, Height));
  if Assigned(FPopup) then
  begin
    CanContinue := True;
    if Assigned(FBeforeShowPop) then
      FBeforeShowPop(Self, FPopup, CanContinue);
    if CanContinue then
      FPopup.Popup(p.x, p.y - Height);
  end;
end;

procedure TCustomComboCheck.AssignItemPop;

  procedure Change(var chaine: String);
  var
    i: Integer;
  begin
    i := 0;
    while i < Length(chaine) do
    begin
      if chaine[i] = '&' then
      begin
        Insert('&', chaine, i);
        Inc(i);
      end;
      Inc(i);
    end;
  end;

  procedure Process(Item: TSubItem; ParentItem: TMenuItem);
  var
    NewItem: TMenuItem;
    i: Integer;
    dummy: String;
  begin
    NewItem := TMenuItem.Create(FPopup);
    NewItem.Caption := Item.Caption;
    NewItem.Visible := Item.Visible;
    NewItem.Enabled := Item.Enabled;
    NewItem.SubItem := Item; // surtout pas Integer(@Item)
    dummy := NewItem.Caption; // pas de trim sinon une chaine vide sera transformée en ligne de séparation
    Change(dummy);
    NewItem.Caption := dummy;
    NewItem.Bitmap := nil;
    if Item.Valeur = FValueMissing then
      NewItem.Bitmap.Assign(FImgMissing);
    if Item.Valeur = FValueUnknown then
      NewItem.Bitmap.Assign(FImgUnknown);
    if Assigned(ParentItem) then
      ParentItem.Add(NewItem)
    else
      FPopup.Items.Add(NewItem);
    if (Item.SubItems.Count = 0) then
      NewItem.OnClick := ClickOnItem
    else
      for i := 0 to Item.SubItems.Count - 1 do
        Process(Item.SubItems[i], NewItem);
  end;

var
  i: Integer;
begin
  if not Assigned(FPopUp) then
  begin
    FPopup := TPopupMenu.Create(nil);
    FPopup.Alignment := paleft;//paRight;
    FPopup.AutoPopup := False;
  end;
  with FPopup do
  begin
    CleanUp;
    OnPopup := PopDwn;
    for i := 0 to FItems.Count - 1 do
      Process(FItems[i], nil);
  end;
end;

procedure TCustomComboCheck.PopDwn(Sender: TObject);

  procedure Process(MenuItem: Menus.TMenuItem);
  var
    i: Integer;
  begin
    if MenuItem.Count = 0 then
      MenuItem.Checked := (Value = TMenuItem(MenuItem).SubItem.Valeur)
    else
      for i := 0 to MenuItem.Count - 1 do
      begin
        Process(MenuItem[i]);
      end;
    if MenuItem.Checked then
      MenuItem.Parent.Checked := True;
  end;

var
  i: Integer;
begin
  with FPopup do
  begin
    if Items.Count > 0 then
    begin
      for i := 0 to Items.Count - 1 do
        Process(Items[i]);
    end
    else if Assigned(FOnPopEmpty) then
      FOnPopEmpty(Self);//si le popup est vide
  end;
end;

procedure TCustomComboCheck.ClickOnItem(Sender: TObject);
begin
  if (Sender is TMenuItem) and Enabled then
    SetValue(TMenuItem(Sender).SubItem.Valeur);
  Invalidate;
  CleanUp;
end;

procedure TCustomComboCheck.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited Mousemove(Shift, X, Y);
  if x < FBmpChk.Width then
    Cursor := crHandPoint
  else if (x > (ClientRect.Right - FBmpDown.Width)) or
    (FTextClick and (x > FBmpChk.Width)) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  if FShowCaptionHint then
    ShowHintWindow
  else if FAssignHint and ShowHint then
    Hint := Self.Text;
end;

procedure TCustomComboCheck.ShowHintWindow;
var
  RAff: TRect;
begin
  if (csDesigning in ComponentState) then
    Exit;
  if (HintToShow and HintToShowFinTimer) and not HintShown then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.Interval := FInterval;
    FTimer.OnTimer := FinTimer;
    RAff := FHintWindow.CalcHintRect(TextW, Caption, nil);
    Inc(RAff.Right, TextLeft - RAff.Left);
    RAff.Left := TextLeft;
    Inc(RAff.Bottom, TextTop - RAff.Top);
    RAff.Top := TextTop;
    RAff.TopLeft := ClientToScreen(RAff.TopLeft);
    RAff.BottomRight := ClientToScreen(RAff.BottomRight);
    FHintWindow.ActivateHint(RAff, Caption);
    HintShown := True;
    FTimer.Enabled := True;
  end;
end;

procedure TCustomComboCheck.CMMouseLeave(var Message: TMessage);
begin
  if (csDesigning in ComponentState) then
    Exit;
  DoMouseLeave;
end;

procedure TCustomComboCheck.CMMouseEnter(var Message: TMessage);
begin
  if (csDesigning in ComponentState) then
    Exit;
  HintToShowFinTimer := True;
end;

{ TCollectionOwnedByCustomComboCheck }

constructor TCollectionOwnedByCustomComboCheck.Create(CustomComboCheck: TCustomComboCheck);
begin
  inherited Create(TSubItem);
  FCustomComboCheck := CustomComboCheck;
  FCaptionComplet := True;
  FCaptionSeparateur := ' ';
end;

procedure TCollectionOwnedByCustomComboCheck.SetCaptionComplet(const Value: Boolean);
begin
  if FCaptionComplet = Value then
    Exit;
  FCaptionComplet := Value;
  FCustomComboCheck.Invalidate;
end;

procedure TCollectionOwnedByCustomComboCheck.SetCaptionSeparateur(const Value: String);
begin
  if FCaptionSeparateur = Value then
    Exit;
  FCaptionSeparateur := Value;
  if FCaptionComplet then
    FCustomComboCheck.Invalidate;
end;

initialization

finalization
  TCustomComboCheck.CleanImg;

end.
