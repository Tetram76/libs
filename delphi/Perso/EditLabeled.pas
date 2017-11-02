unit EditLabeled;

{$D-}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ScanEdit, StdCtrls, Mask, Spin, ComCtrls, ExtCtrls,
  VDTButton, CheckLst, LinkControls;

  type
  TTypeDonnee = (tdChaine, tdNumeric, tdEntier, tdNumericSigne, tdEntierSigne, tdDate, tdHeure, tdDateHeure, tdNomFichier, tdISBN, tdCurrency);

  TEditLabeled = class(TEdit)
  private
    FTypeDonnee: TTypeDonnee;
    FCurrencyChar: Char;
    FLinkControls: TControlList;
    procedure ValidChar(var Key: Char; PressValid: Boolean = False);
    procedure SetLinkControls(const Value: TControlList);
    type TCrackWinControl = class(TWinControl);
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

  TRadioButtonLabeled = class(TRadioButton)
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

  TCheckListBoxLabeled = class(TCheckListBox)
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

implementation

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

procedure TEditLabeled.ValidChar(var Key: Char; PressValid: Boolean = False);

  procedure SameChar(Chars: TSysCharSet);
  begin
    if PressValid and CharInSet(Key, Chars) and (SelStart < Length(Text)) and (Text[SelStart + 1] = Key) then
    begin
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
    if Text = '' then
      Exit;
    p := @Text[1];
    c := 0;
    while Assigned(p) and (p^ <> #0) do
    begin
      if p^ = DecimalSeparator then
        Inc(c);
      Inc(p);
    end;
    Result := c > TestCount;
  end;

var
  ValidChars: TSysCharSet;
begin
  if (Key <> #8) then
    case FTypeDonnee of
      tdNumeric, tdNumericSigne, tdCurrency:
      begin
        if Key = '.' then
          Key := FormatSettings.DecimalSeparator;
        ValidChars := ['-', '0'..'9', FormatSettings.DecimalSeparator];
        if (FTypeDonnee = tdCurrency) then
          ValidChars := ValidChars + [' ', FCurrencyChar];
        if not CharInSet(Key, ValidChars) then
          Key := #0;
        if PressValid then
        begin
          if (Key = FormatSettings.DecimalSeparator) and Bool(Pos(FormatSettings.DecimalSeparator, Text)) and (SelStart < Length(Text)) and (Text[SelStart + 1] <> FormatSettings.DecimalSeparator) then
            Key := #0;
          if (Key = '-') and ((FTypeDonnee = tdNumeric) or (SelStart <> 0)) then
            Key := #0;
        end
        else
        begin
          if (FTypeDonnee = tdCurrency) then
          begin
            if (Key = FCurrencyChar) and Count(FCurrencyChar, Text) then
              Key := #0;
            if (Key = ' ') and Count(' ', Text) then
              Key := #0;
          end;
          if (Key = FormatSettings.DecimalSeparator) and Count(FormatSettings.DecimalSeparator, Text) then
            Key := #0;
          if (Key = '-') and ((FTypeDonnee = tdNumeric) or Count('-', Text)) then
            Key := #0;
        end;
        SameChar(['-', FormatSettings.DecimalSeparator]);
      end;
      tdEntier, tdEntierSigne:
      begin
        ValidChars := ['-', '0'..'9'];
        if not CharInSet(Key, ValidChars) then
          Key := #0;
        if PressValid then
        begin
          if (Key = '-') and ((FTypeDonnee = tdEntier) or (SelStart <> 0)) then
            Key := #0;
        end
        else
        begin
          if (Key = '-') and ((FTypeDonnee = tdEntier) or Count('-', Text)) then
            Key := #0;
        end;
        SameChar(['-']);
      end;
      tdISBN:
      begin
        if (Key = 'x') then
          Key := 'X';
        ValidChars := ['0'..'9', 'X'];
        if not CharInSet(Key, ValidChars) then
          Key := #0;
        if PressValid then
        begin
          if (Key = 'X') and not (Succ(SelStart) in [13, 10]) then
            Key := #0;
        end
        else
        begin
          if (Key = 'X') and Count('X', Text) then
            Key := #0;
        end;
        SameChar(['-', 'X']);
      end;
      tdDate:
      begin
        if not CharInSet(Key, [FormatSettings.DateSeparator, '0'..'9']) then
          Key := #0;
        SameChar([FormatSettings.DateSeparator]);
      end;
      tdHeure:
      begin
        if not CharInSet(Key, [FormatSettings.TimeSeparator, '0'..'9']) then
          Key := #0;
        SameChar([FormatSettings.TimeSeparator]);
      end;
      tdDateHeure:
      begin
        if not CharInSet(Key, [FormatSettings.DateSeparator, FormatSettings.TimeSeparator, ' ', '0'..'9']) then
          Key := #0;
        SameChar([FormatSettings.DateSeparator, FormatSettings.TimeSeparator, ' ']);
      end;
      tdNomFichier:
        if CharInSet(Key, ['<', '>', '*', '"', '|', '?']) then
          Key := #0;
    end;
  inherited;
end;

procedure TEditLabeled.KeyPress(var Key: Char);
begin
  if Ord(Key) >= 32 then
    if not (csDesigning in ComponentState) then
      ValidChar(Key, True);
  inherited;
end;

procedure TEditLabeled.Change;

  function IsVisible(Ctrl: TWinControl): Boolean;
  begin
    Result := True;
    while Result and Assigned(Ctrl) do
    begin
      Result := Ctrl.Visible;
      Ctrl := Ctrl.Parent;
    end;
  end;

var
  i, LastSelStart: Integer;
  c: Char;
  s: string;
begin
  if not (csDesigning in ComponentState) then
  begin
    s := Text;
    LastSelStart := SelStart;
    for i := Length(s) downto 1 do
    begin
      c := s[i];
      if c <> #0 then
      begin
        ValidChar(c);
        if c = #0 then
        begin
          Delete(s, i, 1);
          if i <= LastSelStart then
            Dec(LastSelStart);
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

procedure TEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

destructor TEditLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

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

destructor TDateTimePickerLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TDateTimePickerLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
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

procedure TRadioGroupLabeled.SetLinkControls(const Value: TControlList);
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
  FLinkControls.Add(Self);
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

procedure TSpinEditLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
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

procedure TRadioButtonLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TRadioButtonLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TRadioButtonLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

destructor TRadioButtonLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TRadioButtonLabeled.SetLinkControls(const Value: TControlList);
begin
  FLinkControls.Assign(Value);
end;

procedure TCheckListBoxLabeled.DoEnter;
begin
  FLinkControls.DoEnter;
  inherited;
end;

procedure TCheckListBoxLabeled.DoExit;
begin
  FLinkControls.DoExit;
  inherited;
end;

constructor TCheckListBoxLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkControls := TControlList.Create;
end;

destructor TCheckListBoxLabeled.Destroy;
begin
  FLinkControls.Free;
  inherited;
end;

procedure TCheckListBoxLabeled.SetLinkControls(const Value: TControlList);
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

end.
