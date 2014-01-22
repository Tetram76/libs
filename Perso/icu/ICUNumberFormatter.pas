unit ICUNumberFormatter;

interface

uses
  System.SysUtils, System.Classes, unum, uloc, utypes;

type
  TICUNumberFormatter = class
  private type
    TICUNumberFormatterChild = class
    private
      FFormatter: TICUNumberFormatter;
    protected
      function GetSymbol(Symbol: UNumberFormatSymbol): string;
      procedure SetSymbol(Symbol: UNumberFormatSymbol; const Value: string);
    public
      constructor Create(Formatter: TICUNumberFormatter);

      property DecimalSeparator: string index UNUM_DECIMAL_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property GroupingSeparator: string index UNUM_GROUPING_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property PatternSeparator: string index UNUM_PATTERN_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property Percent: string index UNUM_PERCENT_SYMBOL read GetSymbol write SetSymbol;
      property ZeorDigit: string index UNUM_ZERO_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property Digit: string index UNUM_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property MinusSign: string index UNUM_MINUS_SIGN_SYMBOL read GetSymbol write SetSymbol;
      property PlusSign: string index UNUM_PLUS_SIGN_SYMBOL read GetSymbol write SetSymbol;
      property Currency: string index UNUM_CURRENCY_SYMBOL read GetSymbol write SetSymbol;
      property IntlCurrency: string index UNUM_INTL_CURRENCY_SYMBOL read GetSymbol write SetSymbol;
      property MonetarySeparator: string index UNUM_MONETARY_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property ExponentialSeparator: string index UNUM_EXPONENTIAL_SYMBOL read GetSymbol write SetSymbol;
      property Permill: string index UNUM_PERMILL_SYMBOL read GetSymbol write SetSymbol;
      property PadEscape: string index UNUM_PAD_ESCAPE_SYMBOL read GetSymbol write SetSymbol;
      property Infinity: string index UNUM_INFINITY_SYMBOL read GetSymbol write SetSymbol;
      property NAN: string index UNUM_NAN_SYMBOL read GetSymbol write SetSymbol;
      property SignificantDigit: string index UNUM_SIGNIFICANT_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property MonetaryGroupingSeparator: string index UNUM_MONETARY_GROUPING_SEPARATOR_SYMBOL read GetSymbol write SetSymbol;
      property OneDigit: string index UNUM_ONE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property TwoDigit: string index UNUM_TWO_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property ThreeDigit: string index UNUM_THREE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property FourDigit: string index UNUM_FOUR_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property FiveDigit: string index UNUM_FIVE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property SixDigit: string index UNUM_SIX_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property SevenDigit: string index UNUM_SEVEN_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property EightDigit: string index UNUM_EIGHT_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property NineDigit: string index UNUM_NINE_DIGIT_SYMBOL read GetSymbol write SetSymbol;
      property Format: string index UNUM_FORMAT_SYMBOL_COUNT read GetSymbol write SetSymbol;
    end;

  private type
    TAttributes = class(TICUNumberFormatterChild)
    protected
      function GetAttribute(Attr: UNumberFormatAttribute): Int32;
      procedure SetAttribute(Attr: UNumberFormatAttribute; const Value: Int32);
      function GetTextAttribute(Tag: UNumberFormatTextAttribute): string;
      procedure SetTextAttribute(Tag: UNumberFormatTextAttribute; const Value: string);
      function GetDoubleAttribute(Attr: UNumberFormatAttribute): Double;
      procedure SetDoubleAttribute(Attr: UNumberFormatAttribute; const Value: Double);
    public
      property ParseIntOnly: Int32 index UNUM_PARSE_INT_ONLY read GetAttribute write SetAttribute;
      property GroupingUsed: Int32 index UNUM_GROUPING_USED read GetAttribute write SetAttribute;
      property DecimalAlwaysShown: Int32 index UNUM_DECIMAL_ALWAYS_SHOWN read GetAttribute write SetAttribute;
      property MaxIntegerDigits: Int32 index UNUM_MAX_INTEGER_DIGITS read GetAttribute write SetAttribute;
      property MinIntegerDigits: Int32 index UNUM_MIN_INTEGER_DIGITS read GetAttribute write SetAttribute;
      property IntegerDigits: Int32 index UNUM_INTEGER_DIGITS read GetAttribute write SetAttribute;
      property MaxFractionDigits: Int32 index UNUM_MAX_FRACTION_DIGITS read GetAttribute write SetAttribute;
      property MinFractionDigits: Int32 index UNUM_MIN_FRACTION_DIGITS read GetAttribute write SetAttribute;
      property FractionDigits: Int32 index UNUM_FRACTION_DIGITS read GetAttribute write SetAttribute;
      property Multiplier: Int32 index UNUM_MULTIPLIER read GetAttribute write SetAttribute;
      property GroupingSize: Int32 index UNUM_GROUPING_SIZE read GetAttribute write SetAttribute;
      property RoundingMode: Int32 index UNUM_ROUNDING_MODE read GetAttribute write SetAttribute;
      property FormatWidth: Int32 index UNUM_FORMAT_WIDTH read GetAttribute write SetAttribute;
      property PaddingPosition: Int32 index UNUM_PADDING_POSITION read GetAttribute write SetAttribute;
      property SecondaryGroupingSize: Int32 index UNUM_SECONDARY_GROUPING_SIZE read GetAttribute write SetAttribute;
      property Scale: Int32 index UNUM_SCALE read GetAttribute write SetAttribute;

      property PositivePrefix: string index UNUM_POSITIVE_PREFIX read GetTextAttribute write SetTextAttribute;
      property PositiveSuffix: string index UNUM_POSITIVE_SUFFIX read GetTextAttribute write SetTextAttribute;
      property NegativePrefix: string index UNUM_NEGATIVE_PREFIX read GetTextAttribute write SetTextAttribute;
      property NegativeSuffix: string index UNUM_NEGATIVE_SUFFIX read GetTextAttribute write SetTextAttribute;
      property PaddingCharacter: string index UNUM_PADDING_CHARACTER read GetTextAttribute write SetTextAttribute;
      property Currency: string index UNUM_CURRENCY_CODE read GetTextAttribute write SetTextAttribute;
      property DefaultRuleset: string index UNUM_DEFAULT_RULESET read GetTextAttribute write SetTextAttribute;
      property PublicRuleset: string index UNUM_PUBLIC_RULESETS read GetTextAttribute write SetTextAttribute;

      property RoundingIncrement: Double index UNUM_ROUNDING_INCREMENT read GetDoubleAttribute write SetDoubleAttribute;
    end;

    TSymbols = class(TICUNumberFormatterChild)

    end;

  private
    FFormat: PUNumberFormat;
    FStatus: UErrorCode;
    FLocale: AnsiString;
    FPattern: string;
    FStyle: UNumberFormatStyle;
    FAttributes: TAttributes;
    FSymbols: TSymbols;

    procedure BuildFormatter;
    procedure ReleaseFormatter;

    function GetLocale(aType: ULocDataLocaleType = ULOC_ACTUAL_LOCALE): string;
    procedure SetLocale(const Value: AnsiString);
    procedure SetPattern(const Value: string);
    procedure SetStyle(const Value: UNumberFormatStyle);
    function GetPattern: string;

    function GetErrorCode: UErrorCode;
    function GetErrorMessage: string;
  public
    constructor Create(Locale: AnsiString; Style: UNumberFormatStyle; Pattern: string = '');
    destructor Destroy; override;

    function Format(Value: Int32): string; overload;
    function Format(Value: Int64): string; overload;
    function Format(Value: Double): string; overload;
    function Format(Value: Double; const CurrencyCode: string): string; overload;
    function Format(Value: AnsiString): string; overload;

    function ParseInt32(const Value: string): Int32;
    function ParseInt64(const Value: string): Int64;
    function ParseDouble(const Value: string): Double;
    function ParseCurrency(const Value: string; const CurrencyCode: string): Double;
    function ParseDecimal(const Value: string): AnsiString;

    property Locale: AnsiString read FLocale write SetLocale;
    property ActualLocale: string index ULOC_ACTUAL_LOCALE read GetLocale;
    property ValidLocale: string index ULOC_VALID_LOCALE read GetLocale;
    property Style: UNumberFormatStyle read FStyle write SetStyle;
    property Pattern: string read GetPattern write SetPattern;

    property Attributes: TAttributes read FAttributes;
    property Symbols: TSymbols read FSymbols;
  end;

implementation

uses
  icu_globals;

const
  DEFAULT_BUFFER_SIZE = 256;

  { TICUNumberFormatter.TICUNumberFormatterChild }

constructor TICUNumberFormatter.TICUNumberFormatterChild.Create(Formatter: TICUNumberFormatter);
begin
  FFormatter := Formatter;
end;

function TICUNumberFormatter.TICUNumberFormatterChild.GetSymbol(Symbol: UNumberFormatSymbol): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FFormatter.FStatus := U_ZERO_ERROR;
  // à priori, le symbole le plus long est le code monétaire internationnal (3)
  // tous les autres sont à 1 (sauf pour UNUM_FORMAT_SYMBOL_COUNT mais il demande un traitement spécial)
  bufNeeded := 3;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumGetSymbol(FFormatter.FFormat, Symbol, @buffer[1], bufNeeded, FFormatter.FStatus);
  if FFormatter.FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FFormatter.FStatus := U_ZERO_ERROR;
    bufNeeded := UnumGetSymbol(FFormatter.FFormat, Symbol, @buffer[1], bufNeeded, FFormatter.FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

procedure TICUNumberFormatter.TICUNumberFormatterChild.SetSymbol(Symbol: UNumberFormatSymbol; const Value: string);
begin
  FFormatter.FStatus := U_ZERO_ERROR;
  UnumSetSymbol(FFormatter.FFormat, Symbol, @WideString(Value)[1], Length(Value), FFormatter.FStatus);
end;

{ TICUNumberFormatter.TAttributes }

function TICUNumberFormatter.TAttributes.GetAttribute(Attr: UNumberFormatAttribute): Int32;
begin
  FFormatter.FStatus := U_ZERO_ERROR;
  Result := UnumGetAttribute(FFormatter.FFormat, Attr);
end;

function TICUNumberFormatter.TAttributes.GetDoubleAttribute(Attr: UNumberFormatAttribute): Double;
begin
  FFormatter.FStatus := U_ZERO_ERROR;
  Result := UnumGetDoubleAttribute(FFormatter.FFormat, Attr);
end;

function TICUNumberFormatter.TAttributes.GetTextAttribute(Tag: UNumberFormatTextAttribute): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FFormatter.FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumGetTextAttribute(FFormatter.FFormat, Tag, @buffer[1], bufNeeded, FFormatter.FStatus);
  if FFormatter.FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FFormatter.FStatus := U_ZERO_ERROR;
    bufNeeded := UnumGetTextAttribute(FFormatter.FFormat, Tag, @buffer[1], bufNeeded, FFormatter.FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

procedure TICUNumberFormatter.TAttributes.SetAttribute(Attr: UNumberFormatAttribute; const Value: Int32);
begin
  UnumSetAttribute(FFormatter.FFormat, Attr, Value);
end;

procedure TICUNumberFormatter.TAttributes.SetDoubleAttribute(Attr: UNumberFormatAttribute; const Value: Double);
begin
  UnumSetDoubleAttribute(FFormatter.FFormat, Attr, Value);
end;

procedure TICUNumberFormatter.TAttributes.SetTextAttribute(Tag: UNumberFormatTextAttribute; const Value: string);
begin
  FFormatter.FStatus := U_ZERO_ERROR;
  UnumSetTextAttribute(FFormatter.FFormat, Tag, @WideString(Value)[1], Length(Value), FFormatter.FStatus);
end;

{ TICUNumberFormat }

procedure TICUNumberFormatter.BuildFormatter;
var
  unumStatus: UErrorCode;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  if FFormat <> nil then
    ReleaseFormatter;

  unumStatus := U_ZERO_ERROR;
  FFormat := UnumOpen(Style, @WideString(Pattern)[1], Length(Pattern), PAnsiChar(Locale), nil, unumStatus);
end;

constructor TICUNumberFormatter.Create(Locale: AnsiString; Style: UNumberFormatStyle; Pattern: string = '');
begin
  FAttributes := TAttributes.Create(Self);
  FSymbols := TSymbols.Create(Self);
  FLocale := Locale;
  FStyle := Style;
  FPattern := Pattern;
  BuildFormatter;
end;

destructor TICUNumberFormatter.Destroy;
begin
  FAttributes.Free;
  FSymbols.Free;
  inherited;
end;

function TICUNumberFormatter.Format(Value: Double; const CurrencyCode: string): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumFormatDoubleCurrency(FFormat, Value, @WideString(CurrencyCode)[1], @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := UnumFormatDoubleCurrency(FFormat, Value, @WideString(CurrencyCode)[1], @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatter.Format(Value: Double): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumFormatDouble(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := UnumFormatDouble(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatter.Format(Value: Int32): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumFormat(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := UnumFormat(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatter.Format(Value: Int64): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumFormatInt64(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := UnumFormatInt64(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatter.GetErrorCode: UErrorCode;
begin
  Result := FStatus;
end;

function TICUNumberFormatter.GetErrorMessage: string;
begin
  Result := u_errorName(FStatus);
end;

function TICUNumberFormatter.GetLocale(aType: ULocDataLocaleType): string;
begin
  FStatus := U_ZERO_ERROR;
  Result := UnumGetLocaleByType(FFormat, aType, FStatus);
end;

function TICUNumberFormatter.GetPattern: string;
begin
  Result := FPattern;
end;

function TICUNumberFormatter.ParseCurrency(const Value, CurrencyCode: string): Double;
begin
  FStatus := U_ZERO_ERROR;
  Result := UnumParseDoubleCurrency(FFormat, @WideString(Value)[1], Length(Value), nil, @WideString(CurrencyCode)[1], FStatus);
end;

function TICUNumberFormatter.ParseDecimal(const Value: string): AnsiString;
var
  buffer: AnsiString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumParseDecimal(FFormat, @WideString(Value)[1], Length(Value), nil, @buffer[1], bufNeeded, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := UnumParseDecimal(FFormat, @WideString(Value)[1], Length(Value), nil, @buffer[1], bufNeeded, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUNumberFormatter.ParseDouble(const Value: string): Double;
begin
  FStatus := U_ZERO_ERROR;
  Result := UnumParseDouble(FFormat, @WideString(Value)[1], Length(Value), nil, FStatus);
end;

function TICUNumberFormatter.ParseInt32(const Value: string): Int32;
begin
  FStatus := U_ZERO_ERROR;
  Result := UnumParse(FFormat, @WideString(Value)[1], Length(Value), nil, FStatus);
end;

function TICUNumberFormatter.ParseInt64(const Value: string): Int64;
begin
  FStatus := U_ZERO_ERROR;
  Result := UnumParseInt64(FFormat, @WideString(Value)[1], Length(Value), nil, FStatus);
end;

procedure TICUNumberFormatter.ReleaseFormatter;
begin
  UnumClose(FFormat);
  FFormat := nil;
end;

procedure TICUNumberFormatter.SetLocale(const Value: AnsiString);
begin
  FLocale := Value;
  BuildFormatter;
end;

procedure TICUNumberFormatter.SetPattern(const Value: string);
begin
  FPattern := Value;
  BuildFormatter;
end;

procedure TICUNumberFormatter.SetStyle(const Value: UNumberFormatStyle);
begin
  FStyle := Value;
  BuildFormatter;
end;

function TICUNumberFormatter.Format(Value: AnsiString): string;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := UnumFormatDecimal(FFormat, PAnsiChar(Value), Length(Value), @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := UnumFormatDecimal(FFormat, PAnsiChar(Value), Length(Value), @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

initialization

finalization

UnloadICU;

end.
