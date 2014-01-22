unit unum;

interface

{$I icu.inc}

uses
  icu_globals, JclSysUtils, umachine, utypes, umisc, parseerr, formattable,
  uloc;

type
  // A number formatter.
  PUNumberFormat = ^UNumberFormat;

  UNumberFormat = packed record
  end;

  // The possible number format styles.
  UNumberFormatStyle = (
    UNUM_PATTERN_DECIMAL = 0,
    UNUM_DECIMAL = 1,
    UNUM_CURRENCY,
    UNUM_PERCENT,
    UNUM_SCIENTIFIC,
    UNUM_SPELLOUT,
    UNUM_ORDINAL,
    UNUM_DURATION,
    UNUM_NUMBERING_SYSTEM,
    UNUM_PATTERN_RULEBASED,
    UNUM_CURRENCY_ISO,
    UNUM_CURRENCY_PLURAL,
    UNUM_FORMAT_STYLE_COUNT,
    UNUM_DEFAULT = UNUM_DECIMAL,
    UNUM_IGNORE = UNUM_PATTERN_DECIMAL
  );

  // The possible number format rounding modes.
  UNumberFormatRoundingMode = (
    UNUM_ROUND_CEILING,
    UNUM_ROUND_FLOOR,
    UNUM_ROUND_DOWN,
    UNUM_ROUND_UP,
    UNUM_ROUND_HALFEVEN,
    UNUM_FOUND_HALFEVEN = UNUM_ROUND_HALFEVEN,
    UNUM_ROUND_HALFDOWN = UNUM_ROUND_HALFEVEN + 1,
    UNUM_ROUND_HALFUP,
    UNUM_ROUND_UNNECESSARY
  );

  // The possible number format pad positions.
  UNumberFormatPadPosition = (
    UNUM_PAD_BEFORE_PREFIX,
    UNUM_PAD_AFTER_PREFIX,
    UNUM_PAD_BEFORE_SUFFIX,
    UNUM_PAD_AFTER_SUFFIX
  );

  // Constants for specifying short or long format.
  UNumberCompactStyle = (
    UNUM_SHORT,
    UNUM_LONG
  );

  // Constants for specifying currency spacing.
  UCurrencySpacing = (
    UNUM_CURRENCY_MATCH,
    UNUM_CURRENCY_SURROUNDING_MATCH,
    UNUM_CURRENCY_INSERT,
    UNUM_CURRENCY_SPACING_COUNT
  );

  // FieldPosition and UFieldPosition selectors for format fields defined by NumberFormat and UNumberFormat.
  UNumberFormatFields = (
    UNUM_INTEGER_FIELD,
    UNUM_FRACTION_FIELD,
    UNUM_DECIMAL_SEPARATOR_FIELD,
    UNUM_EXPONENT_SYMBOL_FIELD,
    UNUM_EXPONENT_SIGN_FIELD,
    UNUM_EXPONENT_FIELD,
    UNUM_GROUPING_SEPARATOR_FIELD,
    UNUM_CURRENCY_FIELD,
    UNUM_PERCENT_FIELD,
    UNUM_PERMILL_FIELD,
    UNUM_SIGN_FIELD,
    UNUM_FIELD_COUNT
  );

  // The possible UNumberFormat numeric attributes.
  UNumberFormatAttribute = (
    UNUM_PARSE_INT_ONLY,
    UNUM_GROUPING_USED,
    UNUM_DECIMAL_ALWAYS_SHOWN,
    UNUM_MAX_INTEGER_DIGITS,
    UNUM_MIN_INTEGER_DIGITS,
    UNUM_INTEGER_DIGITS,
    UNUM_MAX_FRACTION_DIGITS,
    UNUM_MIN_FRACTION_DIGITS,
    UNUM_FRACTION_DIGITS,
    UNUM_MULTIPLIER,
    UNUM_GROUPING_SIZE,
    UNUM_ROUNDING_MODE,
    UNUM_ROUNDING_INCREMENT,
    UNUM_FORMAT_WIDTH,
    UNUM_PADDING_POSITION,
    UNUM_SECONDARY_GROUPING_SIZE,
    UNUM_SIGNIFICANT_DIGITS_USED,
    UNUM_MIN_SIGNIFICANT_DIGITS,
    UNUM_MAX_SIGNIFICANT_DIGITS,
    UNUM_LENIENT_PARSE,
    UNUM_SCALE = UNUM_LENIENT_PARSE + 2,
    UNUM_NUMERIC_ATTRIBUTE_COUNT = UNUM_LENIENT_PARSE + 3,
    UNUM_MAX_NONBOOLEAN_ATTRIBUTE = $0FFF,
    UNUM_FORMAT_FAIL_IF_MORE_THAN_MAX_DIGITS = $1000,
    UNUM_PARSE_NO_EXPONENT,
    UNUM_LIMIT_BOOLEAN_ATTRIBUTE
  );

  // The possible UNumberFormat text attributes.
  UNumberFormatTextAttribute = (
    UNUM_POSITIVE_PREFIX,
    UNUM_POSITIVE_SUFFIX,
    UNUM_NEGATIVE_PREFIX,
    UNUM_NEGATIVE_SUFFIX,
    UNUM_PADDING_CHARACTER,
    UNUM_CURRENCY_CODE,
    UNUM_DEFAULT_RULESET,
    UNUM_PUBLIC_RULESETS
  );

  // Constants for specifying a number format symbol.
  UNumberFormatSymbol = (
    UNUM_DECIMAL_SEPARATOR_SYMBOL = 0,
    UNUM_GROUPING_SEPARATOR_SYMBOL = 1,
    UNUM_PATTERN_SEPARATOR_SYMBOL = 2,
    UNUM_PERCENT_SYMBOL = 3,
    UNUM_ZERO_DIGIT_SYMBOL = 4,
    UNUM_DIGIT_SYMBOL = 5,
    UNUM_MINUS_SIGN_SYMBOL = 6,
    UNUM_PLUS_SIGN_SYMBOL = 7,
    UNUM_CURRENCY_SYMBOL = 8,
    UNUM_INTL_CURRENCY_SYMBOL = 9,
    UNUM_MONETARY_SEPARATOR_SYMBOL = 10,
    UNUM_EXPONENTIAL_SYMBOL = 11,
    UNUM_PERMILL_SYMBOL = 12,
    UNUM_PAD_ESCAPE_SYMBOL = 13,
    UNUM_INFINITY_SYMBOL = 14,
    UNUM_NAN_SYMBOL = 15,
    UNUM_SIGNIFICANT_DIGIT_SYMBOL = 16,
    UNUM_MONETARY_GROUPING_SEPARATOR_SYMBOL = 17,
    UNUM_ONE_DIGIT_SYMBOL = 18,
    UNUM_TWO_DIGIT_SYMBOL = 19,
    UNUM_THREE_DIGIT_SYMBOL = 20,
    UNUM_FOUR_DIGIT_SYMBOL = 21,
    UNUM_FIVE_DIGIT_SYMBOL = 22,
    UNUM_SIX_DIGIT_SYMBOL = 23,
    UNUM_SEVEN_DIGIT_SYMBOL = 24,
    UNUM_EIGHT_DIGIT_SYMBOL = 25,
    UNUM_NINE_DIGIT_SYMBOL = 26,
    UNUM_FORMAT_SYMBOL_COUNT = 27
  );

{$IFDEF ICU_LINKONREQUEST}
  // Create and return a new UNumberFormat for formatting and parsing numbers.
  TUnumOpenFunc = function(Style: UNumberFormatStyle; Pattern: PUChar; PatternLength: Int32; Locale: PAnsiChar; parseerr: PUParseError; out Status: UErrorCode): PUNumberFormat; cdecl;
  // Close a UNumberFormat.
  TUnumCloseProc = procedure(Fmt: PUNumberFormat); cdecl;
  // Open a copy of a UNumberFormat.
  TUnumCloneFunc = function(const Fmt: PUNumberFormat; var Status: UErrorCode): PUNumberFormat; cdecl;
  // Format an integer using a UNumberFormat.
  TUnumFormatFunc = function(const Fmt: PUNumberFormat; Number: Int32; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Format an int64 using a UNumberFormat.
  TUnumFormatInt64Func = function(const Fmt: PUNumberFormat; Number: Int64; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Format a double using a UNumberFormat.
  TUnumFormatDouble = function(const Fmt: PUNumberFormat; NNumber: Double; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Format a decimal number using a UNumberFormat.
  TUnumFormatDecimal = function (const Fmt: PUNumberFormat; const Number: PAnsiChar; Length: Int32; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Format a double currency amount using a UNumberFormat.
  TUnumFormatDoubleCurrency = function(const Fmt: PUNumberFormat; Number: Double; Currency: PUChar; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Format a UFormattable into a string.
  TUnumFormatUFormattable = function(const Fmt: PUNumberFormat; const Number: PUFormattable; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Parse a string into an integer using a UNumberFormat.
  TUnumParse = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): Int32; cdecl;
  // Parse a string into an int64 using a UNumberFormat.
  TUnumParseInt64 = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): Int64; cdecl;
  // Parse a string into a double using a UNumberFormat.
  TUnumParseDouble = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): Double; cdecl;
  // Parse a number from a string into an unformatted numeric string using a UNumberFormat.
  TUnumParseDecimal = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; outBuf: PAnsiChar; outBufLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Parse a string into a double and a currency using a UNumberFormat.
  TUnumParseDoubleCurrency = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; Currency: PUChar; var Status: UErrorCode): Double; cdecl;
  // Parse a UChar string into a UFormattable.
  TUnumParseToUFormattable = function(const Fmt: PUNumberFormat; Result: PUFormattable; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): PUFormattable; cdecl;
  // Set the pattern used by a UNumberFormat.
  TUnumApplyPattern = procedure(Format: PUNumberFormat; Localized: UBool; const Pattern: PUChar; PatternLength: Int32; parseError: PUParseError; var Status: UErrorCode); cdecl;
  // Get a locale for which decimal formatting patterns are available.
  TUnumGetAvailable = function(LocaleIndex: Int32): PAnsiChar; cdecl;
  // Determine how many locales have decimal formatting patterns available.
  TUnumCountAvailable = function: Int32; cdecl;
  // Get a numeric attribute associated with a UNumberFormat.
  TUnumGetAttribute = function(const Fmt: PUNumberFormat; Attr: UNumberFormatAttribute): Int32; cdecl;
  // Set a numeric attribute associated with a UNumberFormat.
  TUnumSetAttribute = procedure(Fmt: PUNumberFormat; Attr: UNumberFormatAttribute; NewValue: Int32); cdecl;
  // Get a numeric attribute associated with a UNumberFormat.
  TUnumGetDoubleAttribute = function(const Fmt: PUNumberFormat; Attr: UNumberFormatAttribute): Double; cdecl;
  // Set a numeric attribute associated with a UNumberFormat.
  TUnumSetDoubleAttribute = procedure(Fmt: PUNumberFormat; Attr: UNumberFormatAttribute; NewValue: Double); cdecl;
  // Get a text attribute associated with a UNumberFormat.
  TUnumGetTextAttribute = function(const Fmt: PUNumberFormat; Tag: UNumberFormatTextAttribute; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Set a text attribute associated with a UNumberFormat.
  TUnumSetTextAttribute = procedure(Fmt: PUNumberFormat; Tag: UNumberFormatTextAttribute; const NewValue: PUChar; NewValueLength: Int32; var Status: UErrorCode); cdecl;
  // Extract the pattern from a UNumberFormat.
  TUnumToPattern = function(const Fmt: PUNumberFormat; IsPatternLocalized: UBool; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Get a symbol associated with a UNumberFormat.
  TUnumGetSymbol = function(const Fmt: PUNumberFormat; Symbol: UNumberFormatSymbol; Buffer: PUChar; Size: Int32; var Status: UErrorCode): Int32; cdecl;
  // Set a symbol associated with a UNumberFormat.
  TUnumSetSymbol = procedure(Fmt: PUNumberFormat; Symbol: UNumberFormatSymbol; const Value: PUChar; Length: Int32; var Status: UErrorCode); cdecl;
  // Get the locale for this number format object.
  TUnumGetLocaleByType = function(const Fmt: PUNumberFormat; aType: ULocDataLocaleType; var Status: UErrorCode): PAnsiChar; cdecl;

var
  UnumOpen: TUnumOpenFunc = nil;
  UnumClose: TUnumCloseProc = nil;
  UnumClone: TUnumCloneFunc = nil;
  UnumFormat: TUnumFormatFunc = nil;
  UnumFormatInt64: TUnumFormatInt64Func = nil;
  UnumFormatDouble: TUnumFormatDouble = nil;
  UnumFormatDecimal: TUnumFormatDecimal = nil;
  UnumFormatDoubleCurrency: TUnumFormatDoubleCurrency = nil;
  UnumFormatUFormattable: TUnumFormatUFormattable = nil;
  UnumParse: TUnumParse = nil;
  UnumParseInt64: TUnumParseInt64 = nil;
  UnumParseDouble: TUnumParseDouble = nil;
  UnumParseDecimal: TUnumParseDecimal = nil;
  UnumParseDoubleCurrency: TUnumParseDoubleCurrency = nil;
  UnumParseToUFormattable: TUnumParseToUFormattable = nil;
  UnumApplyPattern: TUnumApplyPattern = nil;
  UnumGetAvailable: TUnumGetAvailable = nil;
  UnumCountAvailable: TUnumCountAvailable = nil;
  UnumGetAttribute: TUnumGetAttribute = nil;
  UnumSetAttribute: TUnumSetAttribute = nil;
  UnumGetDoubleAttribute: TUnumGetDoubleAttribute = nil;
  UnumSetDoubleAttribute: TUnumSetDoubleAttribute = nil;
  UnumGetTextAttribute: TUnumGetTextAttribute = nil;
  UnumSetTextAttribute: TUnumSetTextAttribute = nil;
  UnumToPattern: TUnumToPattern = nil;
  UnumGetSymbol: TUnumGetSymbol = nil;
  UnumSetSymbol: TUnumSetSymbol = nil;
  UnumGetLocaleByType: TUnumGetLocaleByType = nil;
{$ELSE ~ICU_LINKONREQUEST}
  UnumOpen = function(Style: UNumberFormatStyle; Pattern: PUChar; PatternLength: Int32; Locale: PAnsiChar; parseerr: PUParseError; out Status: UErrorCode): PUNumberFormat; cdecl;
  UnumClose = procedure(Fmt: PUNumberFormat); cdecl;
  UnumClone = function(const Fmt: PUNumberFormat; var Status: UErrorCode): PUNumberFormat; cdecl;
  UnumFormat = function(Fmt: PUNumberFormat; Number: Int32; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  UnumFormatInt64 = function(const Fmt: PUNumberFormat; Number: int64; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  UnumFormatDouble = function(const Fmt: PUNumberFormat; NNumber: Double; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  UnumFormatDecimal = function (const Fmt: PUNumberFormat; const Number: PAnsiChar; Length: Int32; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  UnumFormatDoubleCurrency = function(const Fmt: PUNumberFormat; Number: Double; Currency: PUChar; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  UnumFormatUFormattable = function(const Fmt: PUNumberFormat; const Number: PUFormattable; Result: PUChar; ResultLength: Int32; Pos: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  UnumParse = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): Int32; cdecl;
  UnumParseInt64 = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): Int64; cdecl;
  UnumParseDouble = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): Double; cdecl;
  UnumParseDecimal = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; outBuf: PAnsiChar; outBufLength: Int32; var Status: UErrorCode): Int32; cdecl;
  UnumParseDoubleCurrency = function(const Fmt: PUNumberFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; Currency: PUChar; var Status: UErrorCode): Double; cdecl;
  UnumParseToUFormattable = function(const Fmt: PUNumberFormat; Result: PUFormattable; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): PUFormattable; cdecl;
  UnumApplyPattern = procedure(Format: PUNumberFormat; Localized: UBool; const Pattern: PUChar; PatternLength: Int32; parseError: PUParseError; var Status: UErrorCode); cdecl;
  UnumGetAvailable = function(LocaleIndex: Int32): PAnsiChar; cdecl;
  UnumCountAvailable = function: Int32; cdecl;
  UnumGetAttribute = function(const Fmt: PUNumberFormat; Attr: UNumberFormatAttribute): Int32; cdecl;
  UnumSetAttribute = procedure(Fmt: PUNumberFormat; Attr: UNumberFormatAttribute; NewValue: Int32); cdecl;
  UnumGetDoubleAttribute = function(const Fmt: PUNumberFormat; Attr: UNumberFormatAttribute): Double; cdecl;
  UnumSetDoubleAttribute = procedure(Fmt: PUNumberFormat; Attr: UNumberFormatAttribute; NewValue: Double); cdecl;
  UnumGetTextAttribute = function(const Fmt: PUNumberFormat; Tag: UNumberFormatTextAttribute; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  UnumSetTextAttribute = procedure(Fmt: PUNumberFormat; Tag: UNumberFormatTextAttribute; const NewValue: PUChar; NewValueLength: Int32; var Status: UErrorCode); cdecl;
  UnumToPattern = function(const Fmt: PUNumberFormat; IsPatternLocalized: UBool; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  UnumGetSymbol = function(const Fmt: PUNumberFormat; Symbol: UNumberFormatSymbol; Buffer: PUChar; Size: Int32; var Status: UErrorCode): Int32; cdecl;
  UnumSetSymbol = procedure(Fmt: PUNumberFormat; Symbol: UNumberFormatSymbol; const Value: PUChar; Length: Int32; var Status: UErrorCode); cdecl;
  UnumGetLocaleByType = function(const Fmt: PUNumberFormat; aType: ULocDataLocaleType; var Status: UErrorCode): PAnsiChar; cdecl;
{$ENDIF ~ICU_LINKONREQUEST}

const
  UnumOpenDefaultExportName = 'unum_open' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumCloseDefaultExportName = 'unum_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumCloneDefaultExportName = 'unum_clone' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumFormatDefaultExportName = 'unum_format' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumFormatInt64DefaultExportName = 'unum_formatInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumFormatDoubleDefaultExportName = 'unum_formatDouble' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumFormatDecimalDefaultExportName = 'unum_formatDecimal' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumFormatDoubleCurrencyDefaultExportName = 'unum_formatDoubleCurrency' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumFormatUFormattableDefaultExportName = 'unum_formatUFormattable' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumParseDefaultExportName = 'unum_parse' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumParseInt64DefaultExportName = 'unum_parseInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumParseDoubleDefaultExportName = 'unum_parseDouble' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumParseDecimalDefaultExportName = 'unum_parseDecimal' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumParseDoubleCurrencyDefaultExportName = 'unum_parseDoubleCurrency' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumParseToUFormattableDefaultExportName = 'unum_parseToUFormattable' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumApplyPatternDefaultExportName = 'unum_applyPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumGetAvailableDefaultExportName = 'unum_getAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumCountAvailableDefaultExportName = 'unum_countAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumGetAttributeDefaultExportName = 'unum_getAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumSetAttributeDefaultExportName = 'unum_setAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumGetDoubleAttributeDefaultExportName = 'unum_getDoubleAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumSetDoubleAttributeDefaultExportName = 'unum_setDoubleAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumGetTextAttributeDefaultExportName = 'unum_getTextAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumSetTextAttributeDefaultExportName = 'unum_setTextAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumToPatternDefaultExportName = 'unum_toPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumGetSymbolDefaultExportName = 'unum_getSymbol' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumSetSymbolDefaultExportName = 'unum_setSymbol' + ICU_DEFAULT_EXPORT_SUFFIX;
  UnumGetLocaleByTypeDefaultExportName = 'unum_getLocaleByType' + ICU_DEFAULT_EXPORT_SUFFIX;

{$IFDEF ICU_LINKONREQUEST}

var
  UnumOpenExportName: string = UnumOpenDefaultExportName;
  UnumCloseExportName: string = UnumCloseDefaultExportName;
  UnumCloneExportName: string = UnumCloneDefaultExportName;
  UnumFormatExportName: string = UnumFormatDefaultExportName;
  UnumFormatInt64ExportName: string = UnumFormatInt64DefaultExportName;
  UnumFormatDoubleExportName: string = UnumFormatDoubleDefaultExportName;
  UnumFormatDecimalExportName: string = UnumFormatDecimalDefaultExportName;
  UnumFormatDoubleCurrencyExportName: string = UnumFormatDoubleCurrencyDefaultExportName;
  UnumFormatUFormattableExportName: string = UnumFormatUFormattableDefaultExportName;
  UnumParseExportName: string = UnumParseDefaultExportName;
  UnumParseInt64ExportName: string = UnumParseInt64DefaultExportName;
  UnumParseDoubleExportName: string = UnumParseDoubleDefaultExportName;
  UnumParseDecimalExportName: string = UnumParseDecimalDefaultExportName;
  UnumParseDoubleCurrencyExportName: string = UnumParseDoubleCurrencyDefaultExportName;
  UnumParseToUFormattableExportName: string = UnumParseToUFormattableDefaultExportName;
  UnumApplyPatternExportName: string = UnumApplyPatternDefaultExportName;
  UnumGetAvailableExportName: string = UnumGetAvailableDefaultExportName;
  UnumCountAvailableExportName: string = UnumCountAvailableDefaultExportName;
  UnumGetAttributeExportName: string = UnumGetAttributeDefaultExportName;
  UnumSetAttributeExportName: string = UnumSetAttributeDefaultExportName;
  UnumGetDoubleAttributeExportName: string = UnumGetDoubleAttributeDefaultExportName;
  UnumSetDoubleAttributeExportName: string = UnumSetDoubleAttributeDefaultExportName;
  UnumGetTextAttributeExportName: string = UnumGetTextAttributeDefaultExportName;
  UnumSetTextAttributeExportName: string = UnumSetTextAttributeDefaultExportName;
  UnumToPatternExportName: string = UnumToPatternDefaultExportName;
  UnumGetSymbolExportName: string = UnumGetSymbolDefaultExportName;
  UnumSetSymbolExportName: string = UnumSetSymbolDefaultExportName;
  UnumGetLocaleByTypeExportName: string = UnumGetLocaleByTypeDefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

implementation

{$IFNDEF ICU_LINKONREQUEST}
function UnumOpen; external ICU_I18N_DEFAULT_MODULE_NAME name UnumOpenDefaultExportName;
procedure UnumClose; external ICU_I18N_DEFAULT_MODULE_NAME name UnumCloseDefaultExportName;
function UnumClone; external ICU_I18N_DEFAULT_MODULE_NAME name UnumCloneDefaultExportName;
function UnumFormat; external ICU_I18N_DEFAULT_MODULE_NAME name UnumFormatDefaultExportName;
function UnumFormatInt64; external ICU_I18N_DEFAULT_MODULE_NAME name UnumFormatInt64DefaultExportName;
function UnumFormatDouble; external ICU_I18N_DEFAULT_MODULE_NAME name UnumFormatDoubleDefaultExportName;
function UnumFormatDecimal; external ICU_I18N_DEFAULT_MODULE_NAME name UnumFormatDecimalDefaultExportName;
function UnumFormatDoubleCurrency; external ICU_I18N_DEFAULT_MODULE_NAME name UnumFormatDoubleCurrencyDefaultExportName;
function UnumFormatUFormattable; external ICU_I18N_DEFAULT_MODULE_NAME name UnumFormatUFormattableDefaultExportName;
function UnumParse; external ICU_I18N_DEFAULT_MODULE_NAME name UnumParseDefaultExportName;
function UnumParseInt64; external ICU_I18N_DEFAULT_MODULE_NAME name UnumParseInt64DefaultExportName;
function UnumParseDouble; external ICU_I18N_DEFAULT_MODULE_NAME name UnumParseDoubleDefaultExportName;
function UnumParseDecimal; external ICU_I18N_DEFAULT_MODULE_NAME name UnumParseDecimalDefaultExportName;
function UnumParseDoubleCurrency; external ICU_I18N_DEFAULT_MODULE_NAME name UnumParseDoubleCurrencyDefaultExportName;
function UnumParseToUFormattable; external ICU_I18N_DEFAULT_MODULE_NAME name UnumParseToUFormattableDefaultExportName;
procedure UnumApplyPattern; external ICU_I18N_DEFAULT_MODULE_NAME name UnumApplyPatternDefaultExportName;
function UnumGetAvailable; external ICU_I18N_DEFAULT_MODULE_NAME name UnumGetAvailableDefaultExportName;
function UnumCountAvailable; external ICU_I18N_DEFAULT_MODULE_NAME name UnumCountAvailableDefaultExportName;
function UnumGetAttribute; external ICU_I18N_DEFAULT_MODULE_NAME name UnumGetAttributeDefaultExportName;
procedure UnumSetAttribute; external ICU_I18N_DEFAULT_MODULE_NAME name UnumSetAttributeDefaultExportName;
function UnumGetDoubleAttribute; external ICU_I18N_DEFAULT_MODULE_NAME name UnumGetDoubleAttributeDefaultExportName;
procedure UnumSetDoubleAttribute; external ICU_I18N_DEFAULT_MODULE_NAME name UnumSetDoubleAttributeDefaultExportName;
function UnumGetTextAttribute; external ICU_I18N_DEFAULT_MODULE_NAME name UnumGetTextAttributeDefaultExportName;
procedure UnumSetTextAttribute; external ICU_I18N_DEFAULT_MODULE_NAME name UnumSetTextAttributeDefaultExportName;
function UnumToPattern; external ICU_I18N_DEFAULT_MODULE_NAME name UnumToPatternDefaultExportName;
function UnumGetSymbol; external ICU_I18N_DEFAULT_MODULE_NAME name UnumGetSymbolDefaultExportName;
procedure UnumSetSymbol; external ICU_I18N_DEFAULT_MODULE_NAME name UnumSetSymbolDefaultExportName;
function UnumGetLocaleByType; external ICU_I18N_DEFAULT_MODULE_NAME name UnumGetLocaleByTypeDefaultExportName;

{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @UnumOpen := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumOpenExportName);
  @UnumClose := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumCloseExportName);
  @UnumClone := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumCloneExportName);
  @UnumFormat := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumFormatExportName);
  @UnumFormatInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumFormatInt64ExportName);
  @UnumFormatDouble := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumFormatDoubleExportName);
  @UnumFormatDecimal := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumFormatDecimalExportName);
  @UnumFormatDoubleCurrency := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumFormatDoubleCurrencyExportName);
  @UnumFormatUFormattable := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumFormatUFormattableExportName);
  @UnumParse := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumParseExportName);
  @UnumParseInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumParseInt64ExportName);
  @UnumParseDouble := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumParseDoubleExportName);
  @UnumParseDecimal := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumParseDecimalExportName);
  @UnumParseDoubleCurrency := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumParseDoubleCurrencyExportName);
  @UnumParseToUFormattable := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumParseToUFormattableExportName);
  @UnumApplyPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumApplyPatternExportName);
  @UnumGetAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumGetAvailableExportName);
  @UnumCountAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumCountAvailableExportName);
  @UnumGetAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumGetAttributeExportName);
  @UnumSetAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumSetAttributeExportName);
  @UnumGetDoubleAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumGetDoubleAttributeExportName);
  @UnumSetDoubleAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumSetDoubleAttributeExportName);
  @UnumGetTextAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumGetTextAttributeExportName);
  @UnumSetTextAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumSetTextAttributeExportName);
  @UnumToPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumToPatternExportName);
  @UnumGetSymbol := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumGetSymbolExportName);
  @UnumSetSymbol := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumSetSymbolExportName);
  @UnumGetLocaleByType := GetModuleSymbol(ICU_I18N_LibraryHandle, UnumGetLocaleByTypeExportName);

  Result := Assigned(@UnumOpen) and Assigned(@UnumClose) and Assigned(@UnumClone) and Assigned(@UnumFormat) and Assigned(@UnumFormatInt64) and
    Assigned(@UnumFormatDouble) and Assigned(@UnumFormatDecimal) and Assigned(@UnumFormatDoubleCurrency) and Assigned(@UnumFormatUFormattable) and
    Assigned(UnumParse) and Assigned(@UnumParseInt64) and Assigned(@UnumParseDouble) and Assigned(@UnumParseDecimal) and Assigned(@UnumParseDoubleCurrency) and
    Assigned(@UnumParseToUFormattable) and Assigned(@UnumApplyPattern) and Assigned(@UnumGetAvailable) and Assigned(@UnumCountAvailable) and
    Assigned(@UnumGetAttribute) and Assigned(@UnumSetAttribute) and Assigned(@UnumGetDoubleAttribute) and Assigned(@UnumSetDoubleAttribute) and
    Assigned(@UnumGetTextAttribute) and Assigned(@UnumSetTextAttribute) and Assigned(@UnumToPattern) and Assigned(@UnumGetSymbol) and Assigned(@UnumSetSymbol)
    and Assigned(@UnumGetLocaleByType);
end;

procedure UnloadICU;
begin
  @UnumOpen := nil;
  @UnumClose := nil;
  @UnumClone := nil;
  @UnumFormat := nil;
  @UnumFormatDouble := nil;
  @UnumFormatDecimal := nil;
  @UnumFormatDoubleCurrency := nil;
  @UnumFormatUFormattable := nil;
  @UnumParse := nil;
  @UnumParseInt64 := nil;
  @UnumParseDouble := nil;
  @UnumParseDecimal := nil;
  @UnumParseDoubleCurrency := nil;
  @UnumParseToUFormattable := nil;
  @UnumApplyPattern := nil;
  @UnumGetAvailable := nil;
  @UnumCountAvailable := nil;
  @UnumGetAttribute := nil;
  @UnumSetAttribute := nil;
  @UnumGetDoubleAttribute := nil;
  @UnumSetDoubleAttribute := nil;
  @UnumGetTextAttribute := nil;
  @UnumSetTextAttribute := nil;
  @UnumToPattern := nil;
  @UnumGetSymbol := nil;
  @UnumSetSymbol := nil;
  @UnumGetLocaleByType := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
