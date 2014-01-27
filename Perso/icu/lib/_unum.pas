unit _unum;

{$I icu.inc}

interface

uses
  icu_globals, JclSysUtils, _umachine, _utypes, _umisc, parseerr, _uformattable,
  _uloc;

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
{$IFNDEF U_HIDE_DEPRECATED_API}
    UNUM_FOUND_HALFEVEN = UNUM_ROUND_HALFEVEN,
{$ENDIF ~U_HIDE_DEPRECATED_API}
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

{$IFNDEF U_HIDE_DRAFT_API}
  // Constants for specifying short or long format.
  UNumberCompactStyle = (
    UNUM_SHORT,
    UNUM_LONG
  );
{$ENDIF ~U_HIDE_DRAFT_API}

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
{$IFNDEF U_HIDE_DRAFT_API}
    UNUM_SCALE = UNUM_LENIENT_PARSE + 2,
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
    UNUM_NUMERIC_ATTRIBUTE_COUNT = UNUM_LENIENT_PARSE + 3,
    UNUM_MAX_NONBOOLEAN_ATTRIBUTE = $0FFF,
{$ENDIF ~U_HIDE_INTERNAL_API}
    UNUM_FORMAT_FAIL_IF_MORE_THAN_MAX_DIGITS = $1000,
    UNUM_PARSE_NO_EXPONENT
{$IFNDEF U_HIDE_INTERNAL_API}
    , UNUM_LIMIT_BOOLEAN_ATTRIBUTE
{$ENDIF ~U_HIDE_INTERNAL_API}
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
  Tunum_open = function(style: UNumberFormatStyle; pattern: PUChar; patternLength: Int32; locale: PAnsiChar; parseErr: PUParseError; var status: UErrorCode): PUNumberFormat; cdecl;
  // Close a UNumberFormat.
  Tunum_close = procedure(fmt: PUNumberFormat); cdecl;
  // Open a copy of a UNumberFormat.
  Tunum_clone = function(const fmt: PUNumberFormat; var status: UErrorCode): PUNumberFormat; cdecl;
  // Format an integer using a UNumberFormat.
  Tunum_format = function(const fmt: PUNumberFormat; number: Int32; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  // Format an int64 using a UNumberFormat.
  Tunum_formatInt64 = function(const fmt: PUNumberFormat; number: Int64; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  // Format a double using a UNumberFormat.
  Tunum_formatDouble = function(const fmt: PUNumberFormat; number: Double; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  // Format a decimal number using a UNumberFormat.
  Tunum_formatDecimal = function (const fmt: PUNumberFormat; const number: PAnsiChar; length: Int32; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  // Format a double currency amount using a UNumberFormat.
  Tunum_formatDoubleCurrency = function(const fmt: PUNumberFormat; number: Double; currency: PUChar; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  // Format a UFormattable into a string.
  Tunum_formatUFormattable = function(const fmt: PUNumberFormat; const number: PUFormattable; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
  // Parse a string into an integer using a UNumberFormat.
  Tunum_parse = function(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): Int32; cdecl;
  // Parse a string into an int64 using a UNumberFormat.
  Tunum_parseInt64 = function(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): Int64; cdecl;
  // Parse a string into a double using a UNumberFormat.
  Tunum_parseDouble = function(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): Double; cdecl;
  // Parse a number from a string into an unformatted numeric string using a UNumberFormat.
  Tunum_parseDecimal = function(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; outBuf: PAnsiChar; outBufLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Parse a string into a double and a currency using a UNumberFormat.
  Tunum_parseDoubleCurrency = function(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; currency: PUChar; var status: UErrorCode): Double; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  // Parse a UChar string into a UFormattable.
  Tunum_parseToUFormattable = function(const fmt: PUNumberFormat; result: PUFormattable; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): PUFormattable; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
  // Set the pattern used by a UNumberFormat.
  Tunum_applyPattern = procedure(format: PUNumberFormat; localized: UBool; const pattern: PUChar; patternLength: Int32; parseError: PUParseError; var status: UErrorCode); cdecl;
  // Get a locale for which decimal formatting patterns are available.
  Tunum_getAvailable = function(localeIndex: Int32): PAnsiChar; cdecl;
  // Determine how many locales have decimal formatting patterns available.
  Tunum_countAvailable = function: Int32; cdecl;
  // Get a numeric attribute associated with a UNumberFormat.
  Tunum_getAttribute = function(const fmt: PUNumberFormat; attr: UNumberFormatAttribute): Int32; cdecl;
  // Set a numeric attribute associated with a UNumberFormat.
  Tunum_setAttribute = procedure(fmt: PUNumberFormat; attr: UNumberFormatAttribute; newValue: Int32); cdecl;
  // Get a numeric attribute associated with a UNumberFormat.
  Tunum_getDoubleAttribute = function(const fmt: PUNumberFormat; attr: UNumberFormatAttribute): Double; cdecl;
  // Set a numeric attribute associated with a UNumberFormat.
  Tunum_setDoubleAttribute = procedure(fmt: PUNumberFormat; attr: UNumberFormatAttribute; newValue: Double); cdecl;
  // Get a text attribute associated with a UNumberFormat.
  Tunum_getTextAttribute = function(const fmt: PUNumberFormat; tag: UNumberFormatTextAttribute; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Set a text attribute associated with a UNumberFormat.
  Tunum_setTextAttribute = procedure(fmt: PUNumberFormat; tag: UNumberFormatTextAttribute; const newValue: PUChar; newValueLength: Int32; var status: UErrorCode); cdecl;
  // Extract the pattern from a UNumberFormat.
  Tunum_toPattern = function(const fmt: PUNumberFormat; isPatternLocalized: UBool; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Get a symbol associated with a UNumberFormat.
  Tunum_getSymbol = function(const fmt: PUNumberFormat; symbol: UNumberFormatSymbol; buffer: PUChar; size: Int32; var status: UErrorCode): Int32; cdecl;
  // Set a symbol associated with a UNumberFormat.
  Tunum_setSymbol = procedure(fmt: PUNumberFormat; symbol: UNumberFormatSymbol; const value: PUChar; length: Int32; var status: UErrorCode); cdecl;
  // Get the locale for this number format object.
  Tunum_getLocaleByType = function(const fmt: PUNumberFormat; _type: ULocDataLocaleType; var status: UErrorCode): PAnsiChar; cdecl;

var
  unum_open: Tunum_open = nil;
  unum_close: Tunum_close = nil;
  unum_clone: Tunum_clone = nil;
  unum_format: Tunum_format = nil;
  unum_formatInt64: Tunum_formatInt64 = nil;
  unum_formatDouble: Tunum_formatDouble = nil;
  unum_formatDecimal: Tunum_formatDecimal = nil;
  unum_formatDoubleCurrency: Tunum_formatDoubleCurrency = nil;
{$IFNDEF U_HIDE_DRAFT_API}
  unum_formatUFormattable: Tunum_formatUFormattable = nil;
{$ENDIF ~U_HIDE_DRAFT_API}
  unum_parse: Tunum_parse = nil;
  unum_parseInt64: Tunum_parseInt64 = nil;
  unum_parseDouble: Tunum_parseDouble = nil;
  unum_parseDecimal: Tunum_parseDecimal = nil;
  unum_parseDoubleCurrency: Tunum_parseDoubleCurrency = nil;
{$IFNDEF U_HIDE_DRAFT_API}
  unum_parseToUFormattable: Tunum_parseToUFormattable = nil;
{$ENDIF ~U_HIDE_DRAFT_API}
  unum_applyPattern: Tunum_applyPattern = nil;
  unum_getAvailable: Tunum_getAvailable = nil;
  unum_countAvailable: Tunum_countAvailable = nil;
  unum_getAttribute: Tunum_getAttribute = nil;
  unum_setAttribute: Tunum_setAttribute = nil;
  unum_getDoubleAttribute: Tunum_getDoubleAttribute = nil;
  unum_setDoubleAttribute: Tunum_setDoubleAttribute = nil;
  unum_getTextAttribute: Tunum_getTextAttribute = nil;
  unum_setTextAttribute: Tunum_setTextAttribute = nil;
  unum_toPattern: Tunum_toPattern = nil;
  unum_getSymbol: Tunum_getSymbol = nil;
  unum_setSymbol: Tunum_setSymbol = nil;
  unum_getLocaleByType: Tunum_getLocaleByType = nil;
{$ELSE ~ICU_LINKONREQUEST}
  function unum_open(style: UNumberFormatStyle; pattern: PUChar; patternLength: Int32; locale: PAnsiChar; parseErr: PUParseError; var status: UErrorCode): PUNumberFormat; cdecl;
  procedure unum_close(fmt: PUNumberFormat); cdecl;
  function unum_clone(const fmt: PUNumberFormat; var status: UErrorCode): PUNumberFormat; cdecl;
  function unum_format(const fmt: PUNumberFormat; number: Int32; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  function unum_formatInt64(const fmt: PUNumberFormat; number: Int64; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  function unum_formatDouble(const fmt: PUNumberFormat; number: Double; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  function  unum_formatDecimal(const fmt: PUNumberFormat; const number: PAnsiChar; length: Int32; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  function unum_formatDoubleCurrency(const fmt: PUNumberFormat; number: Double; currency: PUChar; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  function unum_formatUFormattable(const fmt: PUNumberFormat; const number: PUFormattable; result: PUChar; resultLength: Int32; pos: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
  function unum_parse(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): Int32; cdecl;
  function unum_parseInt64(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): Int64; cdecl;
  function unum_parseDouble(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): Double; cdecl;
  function unum_parseDecimal(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; outBuf: PAnsiChar; outBufLength: Int32; var status: UErrorCode): Int32; cdecl;
  function unum_parseDoubleCurrency(const fmt: PUNumberFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; currency: PUChar; var status: UErrorCode): Double; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  function unum_parseToUFormattable(const fmt: PUNumberFormat; result: PUFormattable; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): PUFormattable; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
  procedure unum_applyPattern(format: PUNumberFormat; localized: UBool; const pattern: PUChar; patternLength: Int32; parseError: PUParseError; var status: UErrorCode); cdecl;
  function unum_getAvailable(localeIndex: Int32): PAnsiChar; cdecl;
  function unum_countAvailable: Int32; cdecl;
  function unum_getAttribute(const fmt: PUNumberFormat; attr: UNumberFormatAttribute): Int32; cdecl;
  procedure unum_setAttribute(fmt: PUNumberFormat; attr: UNumberFormatAttribute; newValue: Int32); cdecl;
  function unum_getDoubleAttribute(const fmt: PUNumberFormat; attr: UNumberFormatAttribute): Double; cdecl;
  procedure unum_setDoubleAttribute(fmt: PUNumberFormat; attr: UNumberFormatAttribute; newValue: Double); cdecl;
  function unum_getTextAttribute(const fmt: PUNumberFormat; tag: UNumberFormatTextAttribute; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  procedure unum_setTextAttribute(fmt: PUNumberFormat; tag: UNumberFormatTextAttribute; const newValue: PUChar; newValueLength: Int32; var status: UErrorCode); cdecl;
  function unum_toPattern(const fmt: PUNumberFormat; isPatternLocalized: UBool; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  function unum_getSymbol(const fmt: PUNumberFormat; symbol: UNumberFormatSymbol; buffer: PUChar; size: Int32; var status: UErrorCode): Int32; cdecl;
  procedure unum_setSymbol(fmt: PUNumberFormat; symbol: UNumberFormatSymbol; const value: PUChar; length: Int32; var status: UErrorCode); cdecl;
  function unum_getLocaleByType(const fmt: PUNumberFormat; _type: ULocDataLocaleType; var status: UErrorCode): PAnsiChar; cdecl;
{$ENDIF ~ICU_LINKONREQUEST}

const
  unum_openDefaultExportName = 'unum_open' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_closeDefaultExportName = 'unum_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_cloneDefaultExportName = 'unum_clone' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_formatDefaultExportName = 'unum_format' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_formatInt64DefaultExportName = 'unum_formatInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_formatDoubleDefaultExportName = 'unum_formatDouble' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_formatDecimalDefaultExportName = 'unum_formatDecimal' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_formatDoubleCurrencyDefaultExportName = 'unum_formatDoubleCurrency' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFNDEF U_HIDE_DRAFT_API}
  unum_formatUFormattableDefaultExportName = 'unum_formatUFormattable' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_DRAFT_API}
  unum_parseDefaultExportName = 'unum_parse' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_parseInt64DefaultExportName = 'unum_parseInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_parseDoubleDefaultExportName = 'unum_parseDouble' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_parseDecimalDefaultExportName = 'unum_parseDecimal' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_parseDoubleCurrencyDefaultExportName = 'unum_parseDoubleCurrency' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFNDEF U_HIDE_DRAFT_API}
  unum_parseToUFormattableDefaultExportName = 'unum_parseToUFormattable' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_DRAFT_API}
  unum_applyPatternDefaultExportName = 'unum_applyPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_getAvailableDefaultExportName = 'unum_getAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_countAvailableDefaultExportName = 'unum_countAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_getAttributeDefaultExportName = 'unum_getAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_setAttributeDefaultExportName = 'unum_setAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_getDoubleAttributeDefaultExportName = 'unum_getDoubleAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_setDoubleAttributeDefaultExportName = 'unum_setDoubleAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_getTextAttributeDefaultExportName = 'unum_getTextAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_setTextAttributeDefaultExportName = 'unum_setTextAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_toPatternDefaultExportName = 'unum_toPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_getSymbolDefaultExportName = 'unum_getSymbol' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_setSymbolDefaultExportName = 'unum_setSymbol' + ICU_DEFAULT_EXPORT_SUFFIX;
  unum_getLocaleByTypeDefaultExportName = 'unum_getLocaleByType' + ICU_DEFAULT_EXPORT_SUFFIX;

{$IFDEF ICU_LINKONREQUEST}

var
  unum_openExportName: string = unum_openDefaultExportName;
  unum_closeExportName: string = unum_closeDefaultExportName;
  unum_cloneExportName: string = unum_cloneDefaultExportName;
  unum_formatExportName: string = unum_formatDefaultExportName;
  unum_formatInt64ExportName: string = unum_formatInt64DefaultExportName;
  unum_formatDoubleExportName: string = unum_formatDoubleDefaultExportName;
  unum_formatDecimalExportName: string = unum_formatDecimalDefaultExportName;
  unum_formatDoubleCurrencyExportName: string = unum_formatDoubleCurrencyDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
  unum_formatUFormattableExportName: string = unum_formatUFormattableDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
  unum_parseExportName: string = unum_parseDefaultExportName;
  unum_parseInt64ExportName: string = unum_parseInt64DefaultExportName;
  unum_parseDoubleExportName: string = unum_parseDoubleDefaultExportName;
  unum_parseDecimalExportName: string = unum_parseDecimalDefaultExportName;
  unum_parseDoubleCurrencyExportName: string = unum_parseDoubleCurrencyDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
  unum_parseToUFormattableExportName: string = unum_parseToUFormattableDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
  unum_applyPatternExportName: string = unum_applyPatternDefaultExportName;
  unum_getAvailableExportName: string = unum_getAvailableDefaultExportName;
  unum_countAvailableExportName: string = unum_countAvailableDefaultExportName;
  unum_getAttributeExportName: string = unum_getAttributeDefaultExportName;
  unum_setAttributeExportName: string = unum_setAttributeDefaultExportName;
  unum_getDoubleAttributeExportName: string = unum_getDoubleAttributeDefaultExportName;
  unum_setDoubleAttributeExportName: string = unum_setDoubleAttributeDefaultExportName;
  unum_getTextAttributeExportName: string = unum_getTextAttributeDefaultExportName;
  unum_setTextAttributeExportName: string = unum_setTextAttributeDefaultExportName;
  unum_toPatternExportName: string = unum_toPatternDefaultExportName;
  unum_getSymbolExportName: string = unum_getSymbolDefaultExportName;
  unum_setSymbolExportName: string = unum_setSymbolDefaultExportName;
  unum_getLocaleByTypeExportName: string = unum_getLocaleByTypeDefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

implementation

{$IFNDEF ICU_LINKONREQUEST}
function unum_open; external ICU_DEFAULT_I18N_MODULE_NAME name unum_openDefaultExportName;
procedure unum_close; external ICU_DEFAULT_I18N_MODULE_NAME name unum_closeDefaultExportName;
function unum_clone; external ICU_DEFAULT_I18N_MODULE_NAME name unum_cloneDefaultExportName;
function unum_format; external ICU_DEFAULT_I18N_MODULE_NAME name unum_formatDefaultExportName;
function unum_formatInt64; external ICU_DEFAULT_I18N_MODULE_NAME name unum_formatInt64DefaultExportName;
function unum_formatDouble; external ICU_DEFAULT_I18N_MODULE_NAME name unum_formatDoubleDefaultExportName;
function unum_formatDecimal; external ICU_DEFAULT_I18N_MODULE_NAME name unum_formatDecimalDefaultExportName;
function unum_formatDoubleCurrency; external ICU_DEFAULT_I18N_MODULE_NAME name unum_formatDoubleCurrencyDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
function unum_formatUFormattable; external ICU_DEFAULT_I18N_MODULE_NAME name unum_formatUFormattableDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
function unum_parse; external ICU_DEFAULT_I18N_MODULE_NAME name unum_parseDefaultExportName;
function unum_parseInt64; external ICU_DEFAULT_I18N_MODULE_NAME name unum_parseInt64DefaultExportName;
function unum_parseDouble; external ICU_DEFAULT_I18N_MODULE_NAME name unum_parseDoubleDefaultExportName;
function unum_parseDecimal; external ICU_DEFAULT_I18N_MODULE_NAME name unum_parseDecimalDefaultExportName;
function unum_parseDoubleCurrency; external ICU_DEFAULT_I18N_MODULE_NAME name unum_parseDoubleCurrencyDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
function unum_parseToUFormattable; external ICU_DEFAULT_I18N_MODULE_NAME name unum_parseToUFormattableDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
procedure unum_applyPattern; external ICU_DEFAULT_I18N_MODULE_NAME name unum_applyPatternDefaultExportName;
function unum_getAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name unum_getAvailableDefaultExportName;
function unum_countAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name unum_countAvailableDefaultExportName;
function unum_getAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name unum_getAttributeDefaultExportName;
procedure unum_setAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name unum_setAttributeDefaultExportName;
function unum_getDoubleAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name unum_getDoubleAttributeDefaultExportName;
procedure unum_setDoubleAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name unum_setDoubleAttributeDefaultExportName;
function unum_getTextAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name unum_getTextAttributeDefaultExportName;
procedure unum_setTextAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name unum_setTextAttributeDefaultExportName;
function unum_toPattern; external ICU_DEFAULT_I18N_MODULE_NAME name unum_toPatternDefaultExportName;
function unum_getSymbol; external ICU_DEFAULT_I18N_MODULE_NAME name unum_getSymbolDefaultExportName;
procedure unum_setSymbol; external ICU_DEFAULT_I18N_MODULE_NAME name unum_setSymbolDefaultExportName;
function unum_getLocaleByType; external ICU_DEFAULT_I18N_MODULE_NAME name unum_getLocaleByTypeDefaultExportName;

{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @unum_open := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_openExportName);
  @unum_close := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_closeExportName);
  @unum_clone := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_cloneExportName);
  @unum_format := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_formatExportName);
  @unum_formatInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_formatInt64ExportName);
  @unum_formatDouble := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_formatDoubleExportName);
  @unum_formatDecimal := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_formatDecimalExportName);
  @unum_formatDoubleCurrency := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_formatDoubleCurrencyExportName);
{$IFNDEF U_HIDE_DRAFT_API}
  @unum_formatUFormattable := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_formatUFormattableExportName);
{$ENDIF ~U_HIDE_DRAFT_API}
  @unum_parse := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_parseExportName);
  @unum_parseInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_parseInt64ExportName);
  @unum_parseDouble := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_parseDoubleExportName);
  @unum_parseDecimal := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_parseDecimalExportName);
  @unum_parseDoubleCurrency := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_parseDoubleCurrencyExportName);
{$IFNDEF U_HIDE_DRAFT_API}
  @unum_parseToUFormattable := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_parseToUFormattableExportName);
{$ENDIF ~U_HIDE_DRAFT_API}
  @unum_applyPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_applyPatternExportName);
  @unum_getAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_getAvailableExportName);
  @unum_countAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_countAvailableExportName);
  @unum_getAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_getAttributeExportName);
  @unum_setAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_setAttributeExportName);
  @unum_getDoubleAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_getDoubleAttributeExportName);
  @unum_setDoubleAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_setDoubleAttributeExportName);
  @unum_getTextAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_getTextAttributeExportName);
  @unum_setTextAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_setTextAttributeExportName);
  @unum_toPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_toPatternExportName);
  @unum_getSymbol := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_getSymbolExportName);
  @unum_setSymbol := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_setSymbolExportName);
  @unum_getLocaleByType := GetModuleSymbol(ICU_I18N_LibraryHandle, unum_getLocaleByTypeExportName);

  result := Assigned(@unum_open) and Assigned(@unum_close) and Assigned(@unum_clone) and Assigned(@unum_format) and Assigned(@unum_formatInt64) and
    Assigned(@unum_formatDouble) and Assigned(@unum_formatDecimal) and Assigned(@unum_formatDoubleCurrency) {$IFNDEF U_HIDE_DRAFT_API}
    and Assigned(@unum_formatUFormattable) {$ENDIF ~U_HIDE_DRAFT_API}
    and Assigned(unum_parse) and Assigned(@unum_parseInt64) and Assigned(@unum_parseDouble) and Assigned(@unum_parseDecimal) and
    Assigned(@unum_parseDoubleCurrency) {$IFNDEF U_HIDE_DRAFT_API}
    and Assigned(@unum_parseToUFormattable) {$ENDIF ~U_HIDE_DRAFT_API}
    and Assigned(@unum_applyPattern) and Assigned(@unum_getAvailable) and Assigned(@unum_countAvailable) and Assigned(@unum_getAttribute) and
    Assigned(@unum_setAttribute) and Assigned(@unum_getDoubleAttribute) and Assigned(@unum_setDoubleAttribute) and Assigned(@unum_getTextAttribute) and
    Assigned(@unum_setTextAttribute) and Assigned(@unum_toPattern) and Assigned(@unum_getSymbol) and Assigned(@unum_setSymbol) and
    Assigned(@unum_getLocaleByType);
end;

procedure UnloadICU;
begin
  @unum_open := nil;
  @unum_close := nil;
  @unum_clone := nil;
  @unum_format := nil;
  @unum_formatDouble := nil;
  @unum_formatDecimal := nil;
  @unum_formatDoubleCurrency := nil;
{$IFNDEF U_HIDE_DRAFT_API}
  @unum_formatUFormattable := nil;
{$ENDIF ~U_HIDE_DRAFT_API}
  @unum_parse := nil;
  @unum_parseInt64 := nil;
  @unum_parseDouble := nil;
  @unum_parseDecimal := nil;
  @unum_parseDoubleCurrency := nil;
{$IFNDEF U_HIDE_DRAFT_API}
  @unum_parseToUFormattable := nil;
{$ENDIF ~U_HIDE_DRAFT_API}
  @unum_applyPattern := nil;
  @unum_getAvailable := nil;
  @unum_countAvailable := nil;
  @unum_getAttribute := nil;
  @unum_setAttribute := nil;
  @unum_getDoubleAttribute := nil;
  @unum_setDoubleAttribute := nil;
  @unum_getTextAttribute := nil;
  @unum_setTextAttribute := nil;
  @unum_toPattern := nil;
  @unum_getSymbol := nil;
  @unum_setSymbol := nil;
  @unum_getLocaleByType := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
