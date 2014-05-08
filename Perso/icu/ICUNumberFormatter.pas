unit ICUNumberFormatter;

{$I icu.inc}

interface

uses
  System.SysUtils, System.Classes, _unum, _uloc, _utypes, ICULocale;

type
  TICUNumberFormatterWrapper = class(TICUObject)
  private type
    TICUNumberFormatterChild = class
    private
      FFormatter: TICUNumberFormatterWrapper;
    public
      constructor Create(Formatter: TICUNumberFormatterWrapper);
    end;

  private type
    TAttributes = class(TICUNumberFormatterChild)
    protected
      function GetAttribute(Attr: UNumberFormatAttribute): Int32;
      procedure SetAttribute(Attr: UNumberFormatAttribute; const Value: Int32);
      function GetBoolAttribute(Attr: UNumberFormatAttribute): Boolean;
      procedure SetBoolAttribute(Attr: UNumberFormatAttribute; const Value: Boolean);
      function GetRoundingMode(Attr: UNumberFormatAttribute): UNumberFormatRoundingMode;
      procedure SetRoundingMode(Attr: UNumberFormatAttribute; const Value: UNumberFormatRoundingMode);
      function GetTextAttribute(Tag: UNumberFormatTextAttribute): WideString;
      procedure SetTextAttribute(Tag: UNumberFormatTextAttribute; const Value: WideString);
      function GetDoubleAttribute(Attr: UNumberFormatAttribute): Double;
      procedure SetDoubleAttribute(Attr: UNumberFormatAttribute; const Value: Double);
    public
      property ParseIntOnly: Boolean index UNUM_PARSE_INT_ONLY read GetBoolAttribute write SetBoolAttribute;
      property GroupingUsed: Boolean index UNUM_GROUPING_USED read GetBoolAttribute write SetBoolAttribute;
      property LenientParse: Boolean index UNUM_LENIENT_PARSE read GetBoolAttribute write SetBoolAttribute;

      property DecimalAlwaysShown: Int32 index UNUM_DECIMAL_ALWAYS_SHOWN read GetAttribute write SetAttribute;
      property MaxIntegerDigits: Int32 index UNUM_MAX_INTEGER_DIGITS read GetAttribute write SetAttribute;
      property MinIntegerDigits: Int32 index UNUM_MIN_INTEGER_DIGITS read GetAttribute write SetAttribute;
      property IntegerDigits: Int32 index UNUM_INTEGER_DIGITS read GetAttribute write SetAttribute;
      property MaxFractionDigits: Int32 index UNUM_MAX_FRACTION_DIGITS read GetAttribute write SetAttribute;
      property MinFractionDigits: Int32 index UNUM_MIN_FRACTION_DIGITS read GetAttribute write SetAttribute;
      property FractionDigits: Int32 index UNUM_FRACTION_DIGITS read GetAttribute write SetAttribute;
      property Multiplier: Int32 index UNUM_MULTIPLIER read GetAttribute write SetAttribute;
      property GroupingSize: Int32 index UNUM_GROUPING_SIZE read GetAttribute write SetAttribute;
      property RoundingMode: UNumberFormatRoundingMode index UNUM_ROUNDING_MODE read GetRoundingMode write SetRoundingMode;
      property FormatWidth: Int32 index UNUM_FORMAT_WIDTH read GetAttribute write SetAttribute;
      property PaddingPosition: Int32 index UNUM_PADDING_POSITION read GetAttribute write SetAttribute;
      property SecondaryGroupingSize: Int32 index UNUM_SECONDARY_GROUPING_SIZE read GetAttribute write SetAttribute;
{$IFNDEF U_HIDE_DRAFT_API}
      property Scale: Int32 index UNUM_SCALE read GetAttribute write SetAttribute;
{$ENDIF ~U_HIDE_DRAFT_API}
      property SignificantDigitsUsed: Int32 index UNUM_SIGNIFICANT_DIGITS_USED read GetAttribute write SetAttribute;
      property MinSignificantDigits: Int32 index UNUM_MIN_SIGNIFICANT_DIGITS read GetAttribute write SetAttribute;
      property MaxSignificantDigits: Int32 index UNUM_MAX_SIGNIFICANT_DIGITS read GetAttribute write SetAttribute;

      property PositivePrefix: WideString index UNUM_POSITIVE_PREFIX read GetTextAttribute write SetTextAttribute;
      property PositiveSuffix: WideString index UNUM_POSITIVE_SUFFIX read GetTextAttribute write SetTextAttribute;
      property NegativePrefix: WideString index UNUM_NEGATIVE_PREFIX read GetTextAttribute write SetTextAttribute;
      property NegativeSuffix: WideString index UNUM_NEGATIVE_SUFFIX read GetTextAttribute write SetTextAttribute;
      property PaddingCharacter: WideString index UNUM_PADDING_CHARACTER read GetTextAttribute write SetTextAttribute;
      property Currency: WideString index UNUM_CURRENCY_CODE read GetTextAttribute write SetTextAttribute;
      property DefaultRuleset: WideString index UNUM_DEFAULT_RULESET read GetTextAttribute write SetTextAttribute;
      property PublicRuleset: WideString index UNUM_PUBLIC_RULESETS read GetTextAttribute write SetTextAttribute;

      property RoundingIncrement: Double index UNUM_ROUNDING_INCREMENT read GetDoubleAttribute write SetDoubleAttribute;
    end;

    TSymbols = class(TICUNumberFormatterChild)
    protected
      function GetSymbol(Symbol: UNumberFormatSymbol): WideString;
      procedure SetSymbol(Symbol: UNumberFormatSymbol; const Value: WideString);
    public
      property DecimalSeparator: WideString index UNUM_DECIMAL_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property GroupingSeparator: WideString index UNUM_GROUPING_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property PatternSeparator: WideString index UNUM_PATTERN_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property Percent: WideString index UNUM_PERCENT_SYMBOL read GetSymbol write SetSymbol;
      property ZeroDigit: WideString index UNUM_ZERO_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property Digit: WideString index UNUM_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property MinusSign: WideString index UNUM_MINUS_SIGN_SYMBOL read GetSymbol write SetSymbol;
      property PlusSign: WideString index UNUM_PLUS_SIGN_SYMBOL read GetSymbol write SetSymbol;
      property Currency: WideString index UNUM_CURRENCY_SYMBOL read GetSymbol write SetSymbol;
      property IntlCurrency: WideString index UNUM_INTL_CURRENCY_SYMBOL read GetSymbol write SetSymbol;
      property MonetarySeparator: WideString index UNUM_MONETARY_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property ExponentialSeparator: WideString index UNUM_EXPONENTIAL_SYMBOL read GetSymbol write SetSymbol;
      property Permill: WideString index UNUM_PERMILL_SYMBOL read GetSymbol write SetSymbol;
      property PadEscape: WideString index UNUM_PAD_ESCAPE_SYMBOL read GetSymbol write SetSymbol;
      property Infinity: WideString index UNUM_INFINITY_SYMBOL read GetSymbol write SetSymbol;
      property NAN: WideString index UNUM_NAN_SYMBOL read GetSymbol write SetSymbol;
      property SignificantDigit: WideString index UNUM_SIGNIFICANT_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property MonetaryGroupingSeparator: WideString index UNUM_MONETARY_GROUPING_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property OneDigit: WideString index UNUM_ONE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property TwoDigit: WideString index UNUM_TWO_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property ThreeDigit: WideString index UNUM_THREE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property FourDigit: WideString index UNUM_FOUR_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property FiveDigit: WideString index UNUM_FIVE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property SixDigit: WideString index UNUM_SIX_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property SevenDigit: WideString index UNUM_SEVEN_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property EightDigit: WideString index UNUM_EIGHT_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property NineDigit: WideString index UNUM_NINE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property Format: WideString index UNUM_FORMAT_SYMBOL_COUNT read GetSymbol write SetSymbol;
    end;

  private
    FFormat: PUNumberFormat;
    FAttributes: TAttributes;
    FSymbols: TSymbols;

    function GetLocale(aType: ULocDataLocaleType = ULOC_ACTUAL_LOCALE): AnsiString;
  protected
    procedure ReleaseFormatter; virtual;
  public
    constructor Create(UNumberFormat: PUNumberFormat); reintroduce;
    destructor Destroy; override;

    function Format(Value: Int32): WideString; overload;
    function Format(Value: Int64): WideString; overload;
    function Format(Value: Double): WideString; overload;
    function Format(Value: Double; const CurrencyCode: WideString): WideString; overload;
    function Format(Value: AnsiString): WideString; overload;

    function ParseInt32(const Value: WideString): Int32;
    function ParseInt64(const Value: WideString): Int64;
    function ParseDouble(const Value: WideString): Double;
    function ParseCurrency(const Value: WideString; const CurrencyCode: WideString): Double;
    function ParseDecimal(const Value: WideString): AnsiString;

    property UNumberFormat: PUNumberFormat read FFormat;

    property ActualLocale: AnsiString index ULOC_ACTUAL_LOCALE read GetLocale;
    property ValidLocale: AnsiString index ULOC_VALID_LOCALE read GetLocale;

    property Attributes: TAttributes read FAttributes;
    property Symbols: TSymbols read FSymbols;
  end;

  TICUNumberFormatter = class(TICUNumberFormatterWrapper)
  private
    FLocale: AnsiString;
    FPattern: WideString;
    FStyle: UNumberFormatStyle;
    procedure SetLocale(const Value: AnsiString);
    procedure SetPattern(const Value: WideString);
    procedure SetStyle(const Value: UNumberFormatStyle);
  protected
    procedure BuildFormatter; virtual;
  public
    constructor Create(const Locale: AnsiString = ''; Style: UNumberFormatStyle = UNUM_DEFAULT; const Pattern: string = ''); reintroduce;
    destructor Destroy; override;

    property Locale: AnsiString read FLocale write SetLocale;
    property Style: UNumberFormatStyle read FStyle write SetStyle;
    property Pattern: WideString read FPattern write SetPattern;
  end;

function ICUCurrencyToStr(const Value: Double; const Locale: AnsiString = ''; const CurrencySymbol: string = ''): string;
function ICUCurrencyToStrShort(const Value: Double; const Locale: AnsiString = ''): string;
function ICUStrToCurrency(const Value: string; const Locale: AnsiString = ''; const CurrencySymbol: string = ''): Double;
function ICUStrToCurrencyDef(const Value: string; const Default: Double; const Locale: AnsiString = ''; const CurrencySymbol: string = ''): Double;

function ICUDoubleToStr(const Value: Double; const Locale: AnsiString = ''): string;
function ICUStrToDouble(const Value: string; const Locale: AnsiString = ''): Double;
function ICUStrToDoubleDef(const Value: string; const Default: Double; const Locale: AnsiString = ''): Double;

implementation

uses
  icu_globals, System.AnsiStrings, _umachine;

{ TICUNumberFormatter.TICUNumberFormatterChild }

constructor TICUNumberFormatterWrapper.TICUNumberFormatterChild.Create(Formatter: TICUNumberFormatterWrapper);
begin
  FFormatter := Formatter;
end;

{ TICUNumberFormatter.TSymbols }

function TICUNumberFormatterWrapper.TSymbols.GetSymbol(Symbol: UNumberFormatSymbol): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FFormatter.FStatus);
  // à priori, le symbole le plus long est le code monétaire internationnal (3)
  // tous les autres sont à 1 (sauf pour UNUM_FORMAT_SYMBOL_COUNT mais il demande un traitement spécial)
  bufNeeded := 3;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_getSymbol(FFormatter.FFormat, Symbol, @buffer[1], bufNeeded, FFormatter.FStatus);
  if FFormatter.FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FFormatter.FStatus);
    bufNeeded := unum_getSymbol(FFormatter.FFormat, Symbol, @buffer[1], bufNeeded, FFormatter.FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

procedure TICUNumberFormatterWrapper.TSymbols.SetSymbol(Symbol: UNumberFormatSymbol; const Value: WideString);
begin
  ResetErrorCode(FFormatter.FStatus);
  unum_setSymbol(FFormatter.FFormat, Symbol, @Value[1], Length(Value), FFormatter.FStatus);
end;

{ TICUNumberFormatter.TAttributes }

function TICUNumberFormatterWrapper.TAttributes.GetAttribute(Attr: UNumberFormatAttribute): Int32;
begin
  Result := unum_getAttribute(FFormatter.FFormat, Attr);
end;

function TICUNumberFormatterWrapper.TAttributes.GetBoolAttribute(Attr: UNumberFormatAttribute): Boolean;
begin
  Result := unum_getAttribute(FFormatter.FFormat, Attr) <> 0;
end;

function TICUNumberFormatterWrapper.TAttributes.GetDoubleAttribute(Attr: UNumberFormatAttribute): Double;
begin
  Result := unum_getDoubleAttribute(FFormatter.FFormat, Attr);
end;

function TICUNumberFormatterWrapper.TAttributes.GetRoundingMode(Attr: UNumberFormatAttribute): UNumberFormatRoundingMode;
begin
  Result := UNumberFormatRoundingMode(GetAttribute(Attr));
end;

function TICUNumberFormatterWrapper.TAttributes.GetTextAttribute(Tag: UNumberFormatTextAttribute): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FFormatter.FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_getTextAttribute(FFormatter.FFormat, Tag, @buffer[1], bufNeeded, FFormatter.FStatus);
  if FFormatter.FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FFormatter.FStatus);
    bufNeeded := unum_getTextAttribute(FFormatter.FFormat, Tag, @buffer[1], bufNeeded, FFormatter.FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

procedure TICUNumberFormatterWrapper.TAttributes.SetAttribute(Attr: UNumberFormatAttribute; const Value: Int32);
begin
  unum_setAttribute(FFormatter.FFormat, Attr, Value);
end;

procedure TICUNumberFormatterWrapper.TAttributes.SetBoolAttribute(Attr: UNumberFormatAttribute; const Value: Boolean);
const
  boolVals: array [False .. True] of Byte = (0, 1);
begin
  unum_setAttribute(FFormatter.FFormat, Attr, boolVals[Value]);
end;

procedure TICUNumberFormatterWrapper.TAttributes.SetDoubleAttribute(Attr: UNumberFormatAttribute; const Value: Double);
begin
  unum_setDoubleAttribute(FFormatter.FFormat, Attr, Value);
end;

procedure TICUNumberFormatterWrapper.TAttributes.SetRoundingMode(Attr: UNumberFormatAttribute; const Value: UNumberFormatRoundingMode);
begin
  SetAttribute(Attr, Ord(Value));
end;

procedure TICUNumberFormatterWrapper.TAttributes.SetTextAttribute(Tag: UNumberFormatTextAttribute; const Value: WideString);
begin
  ResetErrorCode(FFormatter.FStatus);
  unum_setTextAttribute(FFormatter.FFormat, Tag, @Value[1], Length(Value), FFormatter.FStatus);
end;

{ TICUNumberFormatterWrapper }

constructor TICUNumberFormatterWrapper.Create(UNumberFormat: PUNumberFormat);
begin
  FFormat := UNumberFormat;
  FAttributes := TAttributes.Create(Self);
  FSymbols := TSymbols.Create(Self);
end;

destructor TICUNumberFormatterWrapper.Destroy;
begin
  FAttributes.Free;
  FSymbols.Free;
  ReleaseFormatter;
  inherited;
end;

function TICUNumberFormatterWrapper.Format(Value: Double; const CurrencyCode: WideString): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_formatDoubleCurrency(FFormat, Value, @CurrencyCode[1], @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := unum_formatDoubleCurrency(FFormat, Value, @CurrencyCode[1], @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatterWrapper.Format(Value: Double): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_formatDouble(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := unum_formatDouble(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatterWrapper.Format(Value: Int32): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_format(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := unum_format(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatterWrapper.Format(Value: Int64): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_formatInt64(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := unum_formatInt64(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatterWrapper.GetLocale(aType: ULocDataLocaleType): AnsiString;
begin
  ResetErrorCode(FStatus);
  Result := unum_getLocaleByType(FFormat, aType, FStatus);
end;

function TICUNumberFormatterWrapper.ParseCurrency(const Value, CurrencyCode: WideString): Double;
begin
  ResetErrorCode(FStatus);
  Result := unum_parseDoubleCurrency(FFormat, @Value[1], Length(Value), nil, PUChar(CurrencyCode), FStatus);
end;

function TICUNumberFormatterWrapper.ParseDecimal(const Value: WideString): AnsiString;
var
  buffer: AnsiString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_parseDecimal(FFormat, @Value[1], Length(Value), nil, @buffer[1], bufNeeded, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := unum_parseDecimal(FFormat, @Value[1], Length(Value), nil, @buffer[1], bufNeeded, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatterWrapper.ParseDouble(const Value: WideString): Double;
var
  p: Int32;
begin
  ResetErrorCode(FStatus);
  p := 0;
  Result := unum_parseDouble(FFormat, @Value[1], Length(Value), @p, FStatus);
  if p < Length(Value) then
    FStatus := U_PARSE_ERROR;
end;

function TICUNumberFormatterWrapper.ParseInt32(const Value: WideString): Int32;
begin
  ResetErrorCode(FStatus);
  Result := unum_parse(FFormat, @Value[1], Length(Value), nil, FStatus);
end;

function TICUNumberFormatterWrapper.ParseInt64(const Value: WideString): Int64;
begin
  ResetErrorCode(FStatus);
  Result := unum_parseInt64(FFormat, @Value[1], Length(Value), nil, FStatus);
end;

procedure TICUNumberFormatterWrapper.ReleaseFormatter;
begin
  if FFormat <> nil then
    unum_close(FFormat);
  FFormat := nil;
end;

function TICUNumberFormatterWrapper.Format(Value: AnsiString): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := unum_formatDecimal(FFormat, @Value[1], Length(Value), @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := unum_formatDecimal(FFormat, @Value[1], Length(Value), @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

{ TICUNumberFormatter }

procedure TICUNumberFormatter.BuildFormatter;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  ReleaseFormatter;

  ResetErrorCode(FStatus);
  FFormat := unum_open(Style, @Pattern[1], Length(Pattern), @Locale[1], nil, FStatus);
end;

constructor TICUNumberFormatter.Create(const Locale: AnsiString; Style: UNumberFormatStyle; const Pattern: string);
begin
  inherited Create(nil);
  FLocale := Locale;
  FStyle := Style;
  FPattern := Pattern;
  BuildFormatter;
end;

destructor TICUNumberFormatter.Destroy;
begin
  inherited;
end;

procedure TICUNumberFormatter.SetLocale(const Value: AnsiString);
begin
  FLocale := System.AnsiStrings.Trim(Value);
  BuildFormatter;
end;

procedure TICUNumberFormatter.SetPattern(const Value: WideString);
begin
  FPattern := Trim(Value);
  BuildFormatter;
end;

procedure TICUNumberFormatter.SetStyle(const Value: UNumberFormatStyle);
begin
  FStyle := Value;
  BuildFormatter;
end;

function ICUCurrencyToStr(const Value: Double; const Locale: AnsiString = ''; const CurrencySymbol: string = ''): string;
var
  Formatter: TICUNumberFormatter;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_CURRENCY);
  try
    if CurrencySymbol <> '' then
      Formatter.Symbols.Currency := CurrencySymbol;

    Result := Formatter.Format(Value);
  finally
    Formatter.Free;
  end;
end;

function ICUCurrencyToStrShort(const Value: Double; const Locale: AnsiString = ''): string;
var
  Formatter: TICUNumberFormatter;
  s: string;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_CURRENCY);
  try
    Formatter.Symbols.Currency := '';
    s := Formatter.Format(Value);
    Result := s.Trim([#32, #160]);
  finally
    Formatter.Free;
  end;
end;

function ICUStrToCurrency(const Value: string; const Locale: AnsiString = ''; const CurrencySymbol: string = ''): Double;
var
  Formatter: TICUNumberFormatter;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_CURRENCY);
  try
    if CurrencySymbol <> '' then
      Formatter.Symbols.Currency := CurrencySymbol;
    Formatter.Attributes.LenientParse := True;

    Result := Formatter.ParseDouble(StringReplace(Value.Trim, #32, #160, [rfReplaceAll]));
  finally
    Formatter.Free;
  end;
end;

function ICUStrToCurrencyDef(const Value: string; const Default: Double; const Locale: AnsiString = ''; const CurrencySymbol: string = ''): Double;
var
  Formatter: TICUNumberFormatter;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_CURRENCY);
  try
    if CurrencySymbol <> '' then
      Formatter.Symbols.Currency := CurrencySymbol;
    Formatter.Attributes.LenientParse := True;

    Result := Formatter.ParseDouble(StringReplace(Value.Trim, #32, #160, [rfReplaceAll]));
    if U_FAILURE(Formatter.GetErrorCode) then
      Result := Default;
  finally
    Formatter.Free;
  end;
end;

function ICUDoubleToStr(const Value: Double; const Locale: AnsiString = ''): string;
var
  Formatter: TICUNumberFormatter;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_DECIMAL);
  try
    Formatter.Attributes.RoundingMode := UNUM_ROUND_HALFEVEN;
    Result := Formatter.Format(Value);
    ICUCheck(Formatter.GetErrorCode);
  finally
    Formatter.Free;
  end;
end;

function ICUStrToDouble(const Value: string; const Locale: AnsiString = ''): Double;
var
  Formatter: TICUNumberFormatter;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_DECIMAL);
  try
    Result := Formatter.ParseDouble(StringReplace(Value.Trim, #32, #160, [rfReplaceAll]));
  finally
    Formatter.Free;
  end;
end;

function ICUStrToDoubleDef(const Value: string; const Default: Double; const Locale: AnsiString = ''): Double;
var
  Formatter: TICUNumberFormatter;
begin
  Formatter := TICUNumberFormatter.Create(ProperLocale(Locale), UNUM_DECIMAL);
  try
    Formatter.Attributes.ParseIntOnly := False;
    Formatter.Attributes.LenientParse := False;
    Result := Formatter.ParseDouble(StringReplace(Value.Trim, #32, #160, [rfReplaceAll]));
    if U_FAILURE(Formatter.GetErrorCode) then
      Result := Default;
  finally
    Formatter.Free;
  end;
end;

end.
