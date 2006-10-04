unit DBEditLabeled;
{.$D-}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ScanEdit, StdCtrls, Mask, Spin, DBCtrls, comctrls, ExtCtrls,
  VDTButton;

type
  TControlItem = class(TCollectionItem)
  private
    FOldColor: TColor;
    FControl: TControl;
    procedure SetControl(const Value: TControl);
  public
    procedure DoEnter;
    procedure DoExit;
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  protected
    function GetDisplayName: string; override;
  published
    property Control: TControl read FControl write SetControl;
  end;

  TControlList = class(TCollection)
  private
    function GetItem(Index: Integer): TControlItem;
    procedure SetItem(Index: Integer; const Value: TControlItem);
  public
    constructor Create;
    procedure DoEnter;
    procedure DoExit;
    function Add(Control: TControl): TControlItem; overload;
    function Add: TControlItem; overload;
    property Items[Index: Integer]: TControlItem read GetItem write SetItem; default;
  end;

  TRadioGroupLabeled = class(TRadioGroup)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDateTimePickerLabeled = class(TDateTimePicker)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TVDTListViewLabeled = class(TVDTListView)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TScanEditLVLabeled = class(TScanEditLV)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBEditLabeled = class(TDBEdit)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBCheckBoxLabeled = class(TDBCheckBox)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TCheckBoxLabeled = class(TCheckBox)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBMemoLabeled = class(TDBMemo)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TMemoLabeled = class(TMemo)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TDBComboBoxLabeled = class(TDBComboBox)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TListBoxLabeled = class(TListBox)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TTypeDonnee = (tdChaine, tdNumeric, tdEntier, tdNumericSigne, tdEntierSigne, tdDate, tdHeure, tdDateHeure, tdNomFichier, tdISBN, tdCurrency);

  TEditLabeled = class(TEdit)
  private
    FTypeDonnee: TTypeDonnee;
    FCurrencyChar: Char;
    FLinkControls: TControlList;
    procedure ValidChar(var Key: Char; PressValid: Boolean = False);
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
    property TypeDonnee: TTypeDonnee read FTypeDonnee write FTypeDonnee default tdChaine;
    property CurrencyChar: Char read FCurrencyChar write FCurrencyChar;
  end;

  TSpinEditLabeled = class(TSpinEdit)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BorderStyle;
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

  TMaskEditLabeled = class(TMaskEdit)
  private
    FLinkControls: TControlList;
    procedure SetLinkControls(const Value: TControlList);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LinkControls: TControlList read FLinkControls write SetLinkControls;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Medi@ Kit', [TDateTimePickerLabeled, TDBCheckBoxLabeled, TCheckBoxLabeled, TScanEditLVLabeled, TVDTListViewLabeled, TDBEditLabeled, TDBMemoLabeled, TMemoLabeled, TEditLabeled, TSpinEditLabeled, TMaskEditLabeled, TDBComboBoxLabeled, TListBoxLabeled, TRadioGroupLabeled]);
end;

type
  TCrackWinControl = class(TWinControl);
  TCrackControl = class(TControl);

procedure TVDTListViewLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TVDTListViewLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TVDTListViewLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TDateTimePickerLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDateTimePickerLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDateTimePickerLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TScanEditLVLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TScanEditLVLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TScanEditLVLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TDBEditLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBEditLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDBEditLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TDBCheckBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBCheckBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDBCheckBoxLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TCheckBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TCheckBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TCheckBoxLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TDBMemoLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBMemoLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TDBMemoLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TEditLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TEditLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TEditLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTypeDonnee := tdChaine;
  FLinkControls := TControlList.Create;
end;

procedure TSpinEditLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TSpinEditLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TSpinEditLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

procedure TMaskEditLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TMaskEditLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TMaskEditLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

{ TDBComboBoxLabeled }

constructor TDBComboBoxLabeled.Create(AOwner: TComponent);
begin
  inherited;
  FLinkControls := TControlList.Create;
end;

destructor TDBComboBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBComboBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TDBComboBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

procedure TDBComboBoxLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

{ TMemoLabeled }

constructor TMemoLabeled.Create(AOwner: TComponent);
begin
  inherited;
  FLinkControls := TControlList.Create;
end;

destructor TMemoLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TMemoLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TMemoLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

procedure TMemoLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

{ TRadioGroupLabeled }

constructor TRadioGroupLabeled.Create(AOwner: TComponent);
begin
  inherited;
  FLinkControls := TControlList.Create;
end;

destructor TRadioGroupLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TRadioGroupLabeled.DoEnter;
begin
  inherited;
  FLinkControls.DoEnter;
end;

procedure TRadioGroupLabeled.DoExit;
begin
  inherited;
  FLinkControls.DoExit;
end;

procedure TEditLabeled.ValidChar(var Key: Char; PressValid: Boolean = False);
type
  SetOfChar = set of Char;

  procedure SameChar(Chars: SetOfChar);
  begin
    if PressValid and (Key in Chars) and (SelStart < Length(Text)) and (Text[SelStart + 1] = Key) then begin
      Key := #0;
      SelStart := SelStart + 1;
    end;
  end;

  function Count(DecimalSeparator: Char; Text: string; TestCount: Integer = 1): Boolean;
  var
    c: Integer;
    p: PChar;
  begin
    Result := False;
    if Text = '' then Exit;
    p := @Text[1];
    c := 0;
    while Assigned(p) and (p^ <> #0) do begin
      if p^ = DecimalSeparator then Inc(c);
      Inc(p);
    end;
    Result := c > TestCount;
  end;

var
  ValidChars: SetOfChar;
begin
  if (Key <> #8) then
    case FTypeDonnee of
      tdNumeric, tdNumericSigne, tdCurrency: begin
          if Key = '.' then Key := DecimalSeparator;
          ValidChars := ['-', '0'..'9', DecimalSeparator];
          if (FTypeDonnee = tdCurrency) then
            ValidChars := ValidChars + [' ', FCurrencyChar];
          if not (Key in ValidChars) then Key := #0;
          if PressValid then begin
            if (Key = DecimalSeparator) and Bool(Pos(DecimalSeparator, Text)) and (SelStart < Length(Text)) and (Text[SelStart + 1] <> DecimalSeparator) then Key := #0;
            if (Key = '-') and ((FTypeDonnee = tdNumeric) or (SelStart <> 0)) then Key := #0;
          end
          else begin
            if (FTypeDonnee = tdCurrency) then begin
              if (Key = FCurrencyChar) and Count(FCurrencyChar, Text) then Key := #0;
              if (Key = ' ') and Count(' ', Text) then Key := #0;
            end;
            if (Key = DecimalSeparator) and Count(DecimalSeparator, Text) then Key := #0;
            if (Key = '-') and ((FTypeDonnee = tdNumeric) or Count('-', Text)) then Key := #0;
          end;
          SameChar(['-', DecimalSeparator]);
        end;
      tdEntier, tdEntierSigne: begin
          ValidChars := ['-', '0'..'9'];
          if not (Key in ValidChars) then Key := #0;
          if PressValid then begin
            if (Key = '-') and ((FTypeDonnee = tdEntier) or (SelStart <> 0)) then Key := #0;
          end
          else begin
            if (Key = '-') and ((FTypeDonnee = tdEntier) or Count('-', Text)) then Key := #0;
          end;
          SameChar(['-']);
        end;
      tdISBN: begin
          if (Key = 'x') then Key := 'X';
          ValidChars := ['0'..'9', 'X'];
          if not (Key in ValidChars) then Key := #0;
          if PressValid then begin
            if (Key = 'X') and not (Succ(SelStart) in [13, 10]) then Key := #0;
          end
          else begin
            if (Key = 'X') and Count('X', Text) then Key := #0;
          end;
          SameChar(['-', 'X']);
        end;
      tdDate: begin
          if not (Key in [DateSeparator, '0'..'9']) then Key := #0;
          SameChar([DateSeparator]);
        end;
      tdHeure: begin
          if not (Key in [TimeSeparator, '0'..'9']) then Key := #0;
          SameChar([TimeSeparator]);
        end;
      tdDateHeure: begin
          if not (Key in [DateSeparator, TimeSeparator, ' ', '0'..'9']) then Key := #0;
          SameChar([DateSeparator, TimeSeparator, ' ']);
        end;
      tdNomFichier:
        if Key in ['<', '>', '*', '"', '|', '?'] then Key := #0;
    end;
  inherited;
end;

procedure TEditLabeled.KeyPress(var Key: Char);
begin
  if Ord(Key) >= 32 then
    if not (csDesigning in ComponentState) then ValidChar(Key, True);
  inherited;
end;

procedure TEditLabeled.Change;

  function IsVisible(Ctrl: TWinControl): Boolean;
  begin
    Result := True;
    while Result and Assigned(Ctrl) do begin
      Result := Ctrl.Visible;
      Ctrl := Ctrl.Parent;
    end;
  end;

var
  i, LastSelStart: Integer;
  c: Char;
  s: string;
begin
  if not (csDesigning in ComponentState) then begin
    s := Text;
    LastSelStart := SelStart;
    for i := Length(s) downto 1 do begin
      c := s[i];
      if c <> #0 then begin
        ValidChar(c);
        if c = #0 then begin
          Delete(s, i, 1);
          if i <= LastSelStart then Dec(LastSelStart);
        end;
      end;
    end;
    Text := s;
    SelStart := LastSelStart;
  end;
  inherited;
  if not (csDesigning in ComponentState) and Bool(MaxLength) and (Length(Text) >= MaxLength) and Assigned(Parent) and IsVisible(Parent) then
    TCrackWinControl(Parent).SelectNext(Self, True, True);
end;

procedure TRadioGroupLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

{ TListBoxLabeled }

constructor TListBoxLabeled.Create(AOwner: TComponent);
begin
  inherited;
  FLinkControls := TControlList.Create;
end;

destructor TListBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TListBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TListBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

procedure TListBoxLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

{ TControlItem }

procedure TControlItem.Assign(Source: TPersistent);
begin
  if Source is TControlItem then begin
    Control := TControlItem(Source).Control;
    Exit;
  end;
  inherited Assign(Source);
end;

constructor TControlItem.Create(Collection: TCollection);
begin
  inherited;
  FControl := nil;
end;

procedure TControlItem.DoEnter;
begin
  if Assigned(FControl) then begin
    FOldColor := TCrackControl(FControl).Font.Color;
    TCrackControl(FControl).Font.Color := $00DD22BB;
  end;
end;

procedure TControlItem.DoExit;
begin
  if Assigned(FControl) then TCrackControl(FControl).Font.Color := FOldColor;
end;

function TControlItem.GetDisplayName: string;
begin
  if Assigned(FControl) then
    Result := Format('%s (%s)', [FControl.Name, FControl.ClassName])
  else
    Result := ClassName;
end;

procedure TControlItem.SetControl(const Value: TControl);
var
  i: Integer;
begin
  if Assigned(Value) then
    for i := 0 to Pred(Collection.Count) do
      if TControlItem(Collection.Items[i]).Control = Value then
        raise Exception.Create('Ce control fait déjà parti de la liste');
  FControl := Value;
end;

{ TControlList }

function TControlList.Add(Control: TControl): TControlItem;
begin
  Result := Add;
  Result.Control := Control;
end;

function TControlList.Add: TControlItem;
begin
  Result := TControlItem(inherited Add);
end;

constructor TControlList.Create;
begin
  inherited Create(TControlItem);
end;

procedure TControlList.DoEnter;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    Items[i].DoEnter;
end;

procedure TControlList.DoExit;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    Items[i].DoExit;
end;

function TControlList.GetItem(Index: Integer): TControlItem;
begin
  Result := TControlItem(inherited GetItem(Index));
end;

procedure TControlList.SetItem(Index: Integer; const Value: TControlItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TEditLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

destructor TMaskEditLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TMaskEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TSpinEditLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TSpinEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDBMemoLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBMemoLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TCheckBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TCheckBoxLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDBCheckBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBCheckBoxLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDBEditLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDBEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TScanEditLVLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TScanEditLVLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TVDTListViewLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TVDTListViewLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TDateTimePickerLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDateTimePickerLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

end.

