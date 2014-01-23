unit _udat;

{$I icu.inc}

interface

uses
  icu_globals, JclSysUtils, _ucal, _umachine, _utypes, _unum, _umisc, _uloc,
  _udisplaycontext;

type
  // A date formatter.
  PUDateFormat = ^UDateFormat;
  UDateFormat = packed record
  end;

  // The possible date/time format styles.
  UDateFormatStyle = (
    UDAT_FULL,
    UDAT_LONG,
    UDAT_MEDIUM,
    UDAT_SHORT,
    UDAT_DEFAULT = UDAT_MEDIUM,
    UDAT_RELATIVE = (1 shl 7),
    UDAT_FULL_RELATIVE = UDAT_FULL or UDAT_RELATIVE,
    UDAT_LONG_RELATIVE = UDAT_LONG or UDAT_RELATIVE,
    UDAT_MEDIUM_RELATIVE = UDAT_MEDIUM or UDAT_RELATIVE,
    UDAT_SHORT_RELATIVE = UDAT_SHORT or UDAT_RELATIVE,
    UDAT_NONE = -1,
    UDAT_PATTERN = -2,
    UDAT_IGNORE = UDAT_PATTERN
  );

const
  // Constant for date skeleton with year.
  UDAT_YEAR = 'y';
  // Constant for date skeleton with quarter.
  UDAT_QUARTER = 'QQQQ';
  // Constant for date skeleton with abbreviated quarter.
  UDAT_ABBR_QUARTER = 'QQQ';
  // Constant for date skeleton with year and quarter.
  UDAT_YEAR_QUARTER = 'yQQQQ';
  // Constant for date skeleton with year and abbreviated quarter.
  UDAT_YEAR_ABBR_QUARTER = 'yQQQ';
  // Constant for date skeleton with month.
  UDAT_MONTH = 'MMMM';
  // Constant for date skeleton with abbreviated month.
  UDAT_ABBR_MONTH = 'MMM';
  // Constant for date skeleton with numeric month.
  UDAT_NUM_MONTH = 'M';
  // Constant for date skeleton with year and month.
  UDAT_YEAR_MONTH = 'yMMMM';
  // Constant for date skeleton with year and abbreviated month.
  UDAT_YEAR_ABBR_MONTH = 'yMMM';
  // Constant for date skeleton with year and numeric month.
  UDAT_YEAR_NUM_MONTH = 'yM';
  // Constant for date skeleton with day.
  UDAT_DAY = 'd';
  // Constant for date skeleton with year, month, and day.
  UDAT_YEAR_MONTH_DAY = 'yMMMMd';
  // Constant for date skeleton with year, abbreviated month, and day.
  UDAT_YEAR_ABBR_MONTH_DAY = 'yMMMd';
  // Constant for date skeleton with year, numeric month, and day.
  UDAT_YEAR_NUM_MONTH_DAY = 'yMd';
  // Constant for date skeleton with weekday.
  UDAT_WEEKDAY = 'EEEE';
  // Constant for date skeleton with abbreviated weekday.
  UDAT_ABBR_WEEKDAY = 'E';
  // Constant for date skeleton with year, month, weekday, and day.
  UDAT_YEAR_MONTH_WEEKDAY_DAY = 'yMMMMEEEEd';
  // Constant for date skeleton with year, abbreviated month, weekday, and day.
  UDAT_YEAR_ABBR_MONTH_WEEKDAY_DAY = 'yMMMEd';
  // Constant for date skeleton with year, numeric month, weekday, and day.
  UDAT_YEAR_NUM_MONTH_WEEKDAY_DAY = 'yMEd';
  // Constant for date skeleton with long month and day.
  UDAT_MONTH_DAY = 'MMMMd';
  // Constant for date skeleton with abbreviated month and day.
  UDAT_ABBR_MONTH_DAY = 'MMMd';
  // Constant for date skeleton with numeric month and day.
  UDAT_NUM_MONTH_DAY = 'Md';
  // Constant for date skeleton with month, weekday, and day.
  UDAT_MONTH_WEEKDAY_DAY = 'MMMMEEEEd';
  // Constant for date skeleton with abbreviated month, weekday, and day.
  UDAT_ABBR_MONTH_WEEKDAY_DAY = 'MMMEd';
  // Constant for date skeleton with numeric month, weekday, and day.
  UDAT_NUM_MONTH_WEEKDAY_DAY = 'MEd';
  // Constant for date skeleton with hour, with the locale's preferred hour format (12 or 24).
  UDAT_HOUR = 'j';
  // Constant for date skeleton with hour in 24-hour presentation.
  UDAT_HOUR24 = 'H';
  // Constant for date skeleton with minute.
  UDAT_MINUTE = 'm';
  // Constant for date skeleton with hour and minute, with the locale's preferred hour format (12 or 24).
  UDAT_HOUR_MINUTE = 'jm';
  // Constant for date skeleton with hour and minute in 24-hour presentation.
  UDAT_HOUR24_MINUTE = 'Hm';
  // Constant for date skeleton with second.
  UDAT_SECOND = 's';
  // Constant for date skeleton with hour, minute, and second, with the locale's preferred hour format (12 or 24).
  UDAT_HOUR_MINUTE_SECOND = 'jms';
  // Constant for date skeleton with hour, minute, and second in 24-hour presentation.
  UDAT_HOUR24_MINUTE_SECOND = 'Hms';
  // Constant for date skeleton with minute and second.
  UDAT_MINUTE_SECOND = 'ms';
  // Constant for generic location format, such as Los Angeles Time; used in combinations date + time + zone, or time + zone.
  UDAT_LOCATION_TZ = 'VVVV';
  // Constant for generic non-location format, such as Pacific Time; used in combinations date + time + zone, or time + zone.
  UDAT_GENERIC_TZ = 'vvvv';
  // Constant for generic non-location format, abbreviated if possible, such as PT; used in combinations date + time + zone, or time + zone.
  UDAT_ABBR_GENERIC_TZ = 'v';
  // Constant for specific non-location format, such as Pacific Daylight Time; used in combinations date + time + zone, or time + zone.
  UDAT_SPECIFIC_TZ = 'zzzz';
  // Constant for specific non-location format, abbreviated if possible, such as PDT; used in combinations date + time + zone, or time + zone.
  UDAT_ABBR_SPECIFIC_TZ = 'z';
  // Constant for localized GMT/UTC format, such as GMT+8:00 or HPG-8:00; used in combinations date + time + zone, or time + zone.
  UDAT_ABBR_UTC_TZ = 'ZZZZ';
  // Constant for date skeleton with standalone month.
  UDAT_STANDALONE_MONTH = 'LLLL' deprecated;
  // Constant for date skeleton with standalone abbreviated month.
  UDAT_ABBR_STANDALONE_MONTH = 'LLL' deprecated;
  // Constant for date skeleton with hour, minute, and generic timezone.
  UDAT_HOUR_MINUTE_GENERIC_TZ = 'jmv' deprecated;
  // Constant for date skeleton with hour, minute, and timezone.
  UDAT_HOUR_MINUTE_TZ = 'jmz' deprecated;
  // Constant for date skeleton with hour and generic timezone.
  UDAT_HOUR_GENERIC_TZ = 'jv' deprecated;
  // Constant for date skeleton with hour and timezone.
  UDAT_HOUR_TZ = 'jz' deprecated;

type
  // FieldPosition and UFieldPosition selectors for format fields defined by DateFormat and UDateFormat.
  UDateFormatField = (
    UDAT_ERA_FIELD = 0,
    UDAT_YEAR_FIELD = 1,
    UDAT_MONTH_FIELD = 2,
    UDAT_DATE_FIELD = 3,
    UDAT_HOUR_OF_DAY1_FIELD = 4,
    UDAT_HOUR_OF_DAY0_FIELD = 5,
    UDAT_MINUTE_FIELD = 6,
    UDAT_SECOND_FIELD = 7,
    UDAT_FRACTIONAL_SECOND_FIELD = 8,
    UDAT_DAY_OF_WEEK_FIELD = 9,
    UDAT_DAY_OF_YEAR_FIELD = 10,
    UDAT_DAY_OF_WEEK_IN_MONTH_FIELD = 11,
    UDAT_WEEK_OF_YEAR_FIELD = 12,
    UDAT_WEEK_OF_MONTH_FIELD = 13,
    UDAT_AM_PM_FIELD = 14,
    UDAT_HOUR1_FIELD = 15,
    UDAT_HOUR0_FIELD = 16,
    UDAT_TIMEZONE_FIELD = 17,
    UDAT_YEAR_WOY_FIELD = 18,
    UDAT_DOW_LOCAL_FIELD = 19,
    UDAT_EXTENDED_YEAR_FIELD = 20,
    UDAT_JULIAN_DAY_FIELD = 21,
    UDAT_MILLISECONDS_IN_DAY_FIELD = 22,
    UDAT_TIMEZONE_RFC_FIELD = 23,
    UDAT_TIMEZONE_GENERIC_FIELD = 24,
    UDAT_STANDALONE_DAY_FIELD = 25,
    UDAT_STANDALONE_MONTH_FIELD = 26,
    UDAT_QUARTER_FIELD = 27,
    UDAT_STANDALONE_QUARTER_FIELD = 28,
    UDAT_TIMEZONE_SPECIAL_FIELD = 29,
    UDAT_YEAR_NAME_FIELD = 30,
    UDAT_TIMEZONE_LOCALIZED_GMT_OFFSET_FIELD = 31,
    UDAT_TIMEZONE_ISO_FIELD = 32,
    UDAT_TIMEZONE_ISO_LOCAL_FIELD = 33,
    UDAT_FIELD_COUNT = 34
  );

  // The possible types of date format symbols.
  UDateFormatSymbolType = (
    UDAT_ERAS,
    UDAT_MONTHS,
    UDAT_SHORT_MONTHS,
    UDAT_WEEKDAYS,
    UDAT_SHORT_WEEKDAYS,
    UDAT_AM_PMS,
    UDAT_LOCALIZED_CHARS,
    UDAT_ERA_NAMES,
    UDAT_NARROW_MONTHS,
    UDAT_NARROW_WEEKDAYS,
    UDAT_STANDALONE_MONTHS,
    UDAT_STANDALONE_SHORT_MONTHS,
    UDAT_STANDALONE_NARROW_MONTHS,
    UDAT_STANDALONE_WEEKDAYS,
    UDAT_STANDALONE_SHORT_WEEKDAYS,
    UDAT_STANDALONE_NARROW_WEEKDAYS,
    UDAT_QUARTERS,
    UDAT_SHORT_QUARTERS,
    UDAT_STANDALONE_QUARTERS,
    UDAT_STANDALONE_SHORT_QUARTERS,
    UDAT_SHORTER_WEEKDAYS,
    UDAT_STANDALONE_SHORTER_WEEKDAYS
  );

  // Date format symbols.
  UDateFormatSymbols = packed record
  end;

{$IFDEF ICU_LINKONREQUEST}

  // Maps from a UDateFormatField to the corresponding UCalendarDateFields.
  TUdatToCalendarDateField = function(Field: UDateFormatField): UCalendarDateFields; cdecl;
  // Open a new UDateFormat for formatting and parsing dates and times.
  TUdatOpen = function(TimeStyle: UDateFormatStyle; DateStyle: UDateFormatStyle; const Locale: PAnsiChar; const TzID: PUChar; TzIDLength: Int32; const Pattern: PUChar; PatternLength: Int32; var Status: UErrorCode): PUDateFormat; cdecl;
  // Close a UDateFormat.
  TUdatClose = procedure(Format: PUDateFormat); cdecl;
  // Open a copy of a UDateFormat.
  TUdatClone = function(const Format: PUDateFormat; var Status: UErrorCode): PUDateFormat; cdecl;
  // Format a date using an UDateFormat.
  TUdatFormat = function(const Format: PUDateFormat; DateToFormat: UDate; Result: PUChar; ResultLength: Int32; Position: PUFieldPosition; var Status: UErrorCode): Int32; cdecl;
  // Parse a string into an date/time using a UDateFormat.
  TUdatParse = function(const Format: PUDateFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): UDate; cdecl;
  // Parse a string into an date/time using a UDateFormat.
  TUdatParseCalendar = procedure(const Format: PUDateFormat; Calendar: PUCalendar; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode); cdecl;
  // Determine if an UDateFormat will perform lenient parsing.
  TUdatIsLenient = function(const Format: PUDateFormat): UBool; cdecl;
  // Specify whether an UDateFormat will perform lenient parsing.
  TUdatSetLenient = procedure(Format: PUDateFormat; isLenient: UBool); cdecl;
  // Get the UCalendar associated with an UDateFormat.
  TUdatGetCalendar = function(const Format: PUDateFormat): PUCalendar; cdecl;
  // Set the UCalendar associated with an UDateFormat.
  TUdatSetCalendar = procedure(Format: PUDateFormat; const CalendarToSet: PUCalendar); cdecl;
  // Get the UNumberFormat associated with an UDateFormat.
  TUdatGetNumberFormat = function(const Format: PUDateFormat): PUNumberFormat; cdecl;
  // Set the UNumberFormat associated with an UDateFormat.
  TUdatSetNumberFormat = procedure(Format: PUDateFormat; const NumberFormatToSet: PUNumberFormat); cdecl;
  // Get a locale for which date/time formatting patterns are available.
  TUdatGetAvailable = function(LocaleIndex: Int32): PAnsiChar; cdecl;
  // Determine how many locales have date/time formatting patterns available.
  TUdatCountAvailable = function: Int32; cdecl;
  // Get the year relative to which all 2-digit years are interpreted.
  TUdatGet2DigitYearStart = function(const Format: PUDateFormat; var Status: UErrorCode): UDate; cdecl;
  // Set the year relative to which all 2-digit years will be interpreted.
  TUdatSet2DigitYearStart = procedure(Format: PUDateFormat; d: UDate; var Status: UErrorCode); cdecl;
  // Extract the pattern from a UDateFormat.
  TUdatToPattern = function(const Format: PUDateFormat; Localized: UBool; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Set the pattern used by an UDateFormat.
  TUdatApplyPattern = procedure(Format: PUDateFormat; Localized: UBool; const Pattern: PUChar; PatternLength: Int32); cdecl;
  // Get the symbols associated with an UDateFormat.
  TUdatGetSymbols = function(const Format: PUDateFormat; aType: UDateFormatSymbolType; SymbolIndex: Int32; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Count the number of particular symbols for an UDateFormat.
  TUdatCountSymbols = function(const Format: PUDateFormat; aType: UDateFormatSymbolType): Int32; cdecl;
  // Set the symbols associated with an UDateFormat.
  TUdatSetSymbols = procedure(Format: PUDateFormat; aType: UDateFormatSymbolType; SymbolIndex: Int32; Value: PUChar; ValueLength: Int32; var Status: UErrorCode); cdecl;
  // Get the locale for this date format object.
  TUdatGetLocaleByType = function(const Format: PUDateFormat; aType: ULocDataLocaleType; var Status: UErrorCode): PAnsiChar; cdecl;
  // Set a particular UDisplayContext value in the formatter, such as UDISPCTX_CAPITALIZATION_FOR_STANDALONE.
  TUdatSetContext = procedure(Format: PUDateFormat; Value: UDisplayContext; var Status: UErrorCode); cdecl;
  // Get the formatter's UDisplayContext value for the specified UDisplayContextType, such as UDISPCTX_TYPE_CAPITALIZATION.
  TUdatGetContext = function(Format: PUDateFormat; aType: UDisplayContextType; var Status: UErrorCode): UDisplayContext; cdecl;
  // Extract the date pattern from a UDateFormat set for relative date formatting.
  TUdatToPatternRelativeDate = function(const Format: PUDateFormat; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Extract the time pattern from a UDateFormat set for relative date formatting.
  TUdatToPatternRelativeTime = function(const Format: PUDateFormat; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
  // Set the date & time patterns used by a UDateFormat set for relative date formatting.
  TUdatApplyPatternRelative = procedure(Format: PUDateFormat; const DatePattern: PUChar; DatePatternLength: Int32; const TimePattern: PUChar; TimePatternLength: Int32; var Status: UErrorCode); cdecl;

var
  UdatToCalendarDateField: TUdatToCalendarDateField = nil;
  UdatOpen: TUdatOpen = nil;
  UdatClose: TUdatClose = nil;
  UdatClone: TUdatClone = nil;
  UdatFormat: TUdatFormat = nil;
  UdatParse: TUdatParse = nil;
  UdatParseCalendar: TUdatParseCalendar = nil;
  UdatIsLenient: TUdatIsLenient = nil;
  UdatSetLenient: TUdatSetLenient = nil;
  UdatGetCalendar: TUdatGetCalendar = nil;
  UdatSetCalendar: TUdatSetCalendar = nil;
  UdatGetNumberFormat: TUdatGetNumberFormat = nil;
  UdatSetNumberFormat: TUdatSetNumberFormat = nil;
  UdatGetAvailable: TUdatGetAvailable = nil;
  UdatCountAvailable: TUdatCountAvailable = nil;
  UdatGet2DigitYearStart: TUdatGet2DigitYearStart = nil;
  UdatSet2DigitYearStart: TUdatSet2DigitYearStart = nil;
  UdatToPattern: TUdatToPattern = nil;
  UdatApplyPattern: TUdatApplyPattern = nil;
  UdatGetSymbols: TUdatGetSymbols = nil;
  UdatCountSymbols: TUdatCountSymbols = nil;
  UdatSetSymbols: TUdatSetSymbols = nil;
  UdatGetLocaleByType: TUdatGetLocaleByType = nil;
  UdatSetContext: TUdatSetContext = nil;
  UdatGetContext: TUdatGetContext = nil;
  UdatToPatternRelativeDate: TUdatToPatternRelativeDate = nil;
  UdatToPatternRelativeTime: TUdatToPatternRelativeTime = nil;
  UdatApplyPatternRelative: TUdatApplyPatternRelative = nil;
{$ELSE ~ICU_LINKONREQUEST}
function UdatToCalendarDateField(Field: UDateFormatField): UCalendarDateFields; cdecl;
function UdatOpen(TimeStyle: UDateFormatStyle; DateStyle: UDateFormatStyle; const Locale: PAnsiChar; const TzID: PUChar; TzIDLength: Int32; const Pattern: PUChar; PatternLength: Int32; var Status: UErrorCode): PUDateFormat; cdecl;
procedure UdatClose(Format: PUDateFormat); cdecl;
function UdatClone(const Format: PUDateFormat; var Status: UErrorCode): PUDateFormat; cdecl;
function UdatFormat(const Format: PUDateFormat; DateToFormat: UDate; Result: PUChar; ResultLength: Int32; Position: PUFieldPosition; var Status: UErrorCode) Int32; cdecl;
function UdatParse(const Format: PUDateFormat; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode): UDate; cdecl;
procedure UdatParseCalendar(const Format: PUDateFormat; Calendar: PUCalendar; const Text: PUChar; TextLength: Int32; ParsePos: PInt32; var Status: UErrorCode); cdecl;
function UdatIsLenient(const Format: PUDateFormat): UBool; cdecl;
procedure UdatSetLenient(Format: PUDateFormat; isLenient: UBool); cdecl;
function UdatGetCalendar(const Format: PUDateFormat): PUCalendar; cdecl;
procedure UdatSetCalendar(Format: PUDateFormat; const CalendarToSet: PUCalendar); cdecl;
function UdatGetNumberFormat(const Format: PUDateFormat): PUNumberFormat; cdecl;
procedure UdatSetNumberFormat(Format: PUDateFormat; const NumberFormatToSet: PUNumberFormat); cdecl;
function UdatGetAvailable(LocaleIndex: Int32): PAnsiChar; cdecl;
function UdatCountAvailable: Int32; cdecl;
function UdatGet2DigitYearStart(const Format: PUDateFormat; var Status: UErrorCode): UDate; cdecl;
procedure UdatSet2DigitYearStart(Format: PUDateFormat; d: UDate; var Status: UErrorCode); cdecl;
function UdatToPattern(const Format: PUDateFormat; Localized: UBool; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
procedure UdatApplyPattern(Format: PUDateFormat; Localized: UBool; const Pattern: PUChar; PatternLength: Int32); cdecl;
function UdatGetSymbols(const Format: PUDateFormat; aType: UDateFormatSymbolType; SymbolIndex: Int32; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
function UdatCountSymbols(const Format: PUDateFormat; aType: UDateFormatSymbolType): Int32; cdecl;
procedure UdatSetSymbols(Format: PUDateFormat; aType: UDateFormatSymbolType; SymbolIndex: Int32; Value: PUChar; ValueLength: Int32; var Status: UErrorCode); cdecl;
function UdatGetLocaleByType(const Format: PUDateFormat; aType: ULocDataLocaleType; var Status: UErrorCode): PAnsiChar; cdecl;
procedure UdatSetContext(Format: PUDateFormat; Value: UDisplayContext; var Status: UErrorCode); cdecl;
function UdatGetContext(Format: PUDateFormat; aType: UDisplayContextType; var Status: UErrorCode): UDisplayContext; cdecl;
function UdatToPatternRelativeDate(const Format: PUDateFormat; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
function UdatToPatternRelativeTime(const Format: PUDateFormat; Result: PUChar; ResultLength: Int32; var Status: UErrorCode): Int32; cdecl;
procedure UdatApplyPatternRelative(Format: PUDateFormat; const DatePattern: PUChar; DatePatternLength: Int32; const TimePattern: PUChar; TimePatternLength: Int32; var Status: UErrorCode); cdecl;
{$ENDIF ~ICU_LINKONREQUEST}

const
  UdatToCalendarDateFieldDefaultExportName = 'udat_toCalendarDateField' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatOpenDefaultExportName = 'udat_open' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatCloseDefaultExportName = 'udat_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatCloneDefaultExportName = 'udat_clone' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatFormatDefaultExportName = 'udat_format' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatParseDefaultExportName = 'udat_parse' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatParseCalendarDefaultExportName = 'udat_parseCalendar' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatIsLenientDefaultExportName = 'udat_isLenient' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatSetLenientDefaultExportName = 'udat_setLenient' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGetCalendarDefaultExportName = 'udat_getCalendar' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatSetCalendarDefaultExportName = 'udat_setCalendar' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGetNumberFormatDefaultExportName = 'udat_getNumberFormat' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatSetNumberFormatDefaultExportName = 'udat_setNumberFormat' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGetAvailableDefaultExportName = 'udat_getAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatCountAvailableDefaultExportName = 'udat_countAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGet2DigitYearStartDefaultExportName = 'udat_get2DigitYearStart' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatSet2DigitYearStartDefaultExportName = 'udat_set2DigitYearStart' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatToPatternDefaultExportName = 'udat_toPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatApplyPatternDefaultExportName = 'udat_applyPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGetSymbolsDefaultExportName = 'udat_getSymbols' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatCountSymbolsDefaultExportName = 'udat_countSymbols' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatSetSymbolsDefaultExportName = 'udat_setSymbols' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGetLocaleByTypeDefaultExportName = 'udat_getLocaleByType' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatSetContextDefaultExportName = 'udat_setContext' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatGetContextDefaultExportName = 'udat_getContext' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatToPatternRelativeDateDefaultExportName = 'udat_toPatternRelativeDate' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatToPatternRelativeTimeDefaultExportName = 'udat_toPatternRelativeTime' + ICU_DEFAULT_EXPORT_SUFFIX;
  UdatApplyPatternRelativeDefaultExportName = 'udat_applyPatternRelative' + ICU_DEFAULT_EXPORT_SUFFIX;

{$IFDEF ICU_LINKONREQUEST}

var
  UdatToCalendarDateFieldExportName: string = UdatToCalendarDateFieldDefaultExportName;
  UdatOpenExportName: string = UdatOpenDefaultExportName;
  UdatCloseExportName: string = UdatCloseDefaultExportName;
  UdatCloneExportName: string = UdatCloneDefaultExportName;
  UdatFormatExportName: string = UdatFormatDefaultExportName;
  UdatParseExportName: string = UdatParseDefaultExportName;
  UdatParseCalendarExportName: string = UdatParseCalendarDefaultExportName;
  UdatIsLenientExportName: string = UdatIsLenientDefaultExportName;
  UdatSetLenientExportName: string = UdatSetLenientDefaultExportName;
  UdatGetCalendarExportName: string = UdatGetCalendarDefaultExportName;
  UdatSetCalendarExportName: string = UdatSetCalendarDefaultExportName;
  UdatGetNumberFormatExportName: string = UdatGetNumberFormatDefaultExportName;
  UdatSetNumberFormatExportName: string = UdatSetNumberFormatDefaultExportName;
  UdatGetAvailableExportName: string = UdatGetAvailableDefaultExportName;
  UdatCountAvailableExportName: string = UdatCountAvailableDefaultExportName;
  UdatGet2DigitYearStartExportName: string = UdatGet2DigitYearStartDefaultExportName;
  UdatSet2DigitYearStartExportName: string = UdatSet2DigitYearStartDefaultExportName;
  UdatToPatternExportName: string = UdatToPatternDefaultExportName;
  UdatApplyPatternExportName: string = UdatApplyPatternDefaultExportName;
  UdatGetSymbolsExportName: string = UdatGetSymbolsDefaultExportName;
  UdatCountSymbolsExportName: string = UdatCountSymbolsDefaultExportName;
  UdatSetSymbolsExportName: string = UdatSetSymbolsDefaultExportName;
  UdatGetLocaleByTypeExportName: string = UdatGetLocaleByTypeDefaultExportName;
  UdatSetContextExportName: string = UdatSetContextDefaultExportName;
  UdatGetContextExportName: string = UdatGetContextDefaultExportName;
  UdatToPatternRelativeDateExportName: string = UdatToPatternRelativeDateDefaultExportName;
  UdatToPatternRelativeTimeExportName: string = UdatToPatternRelativeTimeDefaultExportName;
  UdatApplyPatternRelativeExportName: string = UdatApplyPatternRelativeDefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

implementation

{$IFNDEF ICU_LINKONREQUEST}
function UdatToCalendarDateField; external ICU_DEFAULT_I18N_MODULE_NAME name UdatToCalendarDateFieldDefaultExportName;
function UdatOpen; external ICU_DEFAULT_I18N_MODULE_NAME name UdatOpenDefaultExportName;
procedure UdatClose; external ICU_DEFAULT_I18N_MODULE_NAME name UdatCloseDefaultExportName;
function UdatClone; external ICU_DEFAULT_I18N_MODULE_NAME name UdatCloneDefaultExportName;
function UdatFormat; external ICU_DEFAULT_I18N_MODULE_NAME name UdatFormatDefaultExportName;
function UdatParse; external ICU_DEFAULT_I18N_MODULE_NAME name UdatParseDefaultExportName;
procedure UdatParseCalendar; external ICU_DEFAULT_I18N_MODULE_NAME name UdatParseCalendarDefaultExportName;
function UdatIsLenient; external ICU_DEFAULT_I18N_MODULE_NAME name UdatIsLenientDefaultExportName;
procedure UdatSetLenient; external ICU_DEFAULT_I18N_MODULE_NAME name UdatSetLenientDefaultExportName;
function UdatGetCalendar; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGetCalendarDefaultExportName;
procedure UdatSetCalendar; external ICU_DEFAULT_I18N_MODULE_NAME name UdatSetCalendarDefaultExportName;
function UdatGetNumberFormat; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGetNumberFormatDefaultExportName;
procedure UdatSetNumberFormat; external ICU_DEFAULT_I18N_MODULE_NAME name UdatSetNumberFormatDefaultExportName;
function UdatGetAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGetAvailableDefaultExportName;
function UdatCountAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name UdatCountAvailableDefaultExportName;
function UdatGet2DigitYearStart; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGet2DigitYearStartDefaultExportName;
procedure UdatSet2DigitYearStart; external ICU_DEFAULT_I18N_MODULE_NAME name UdatSet2DigitYearStartDefaultExportName;
function UdatToPattern; external ICU_DEFAULT_I18N_MODULE_NAME name UdatToPatternDefaultExportName;
procedure UdatApplyPattern; external ICU_DEFAULT_I18N_MODULE_NAME name UdatApplyPatternDefaultExportName;
function UdatGetSymbols; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGetSymbolsDefaultExportName;
function UdatCountSymbols; external ICU_DEFAULT_I18N_MODULE_NAME name UdatCountSymbolsDefaultExportName;
procedure UdatSetSymbols; external ICU_DEFAULT_I18N_MODULE_NAME name UdatSetSymbolsDefaultExportName;
function UdatGetLocaleByType; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGetLocaleByTypeDefaultExportName;
procedure UdatSetContext; external ICU_DEFAULT_I18N_MODULE_NAME name UdatSetContextDefaultExportName;
function UdatGetContext; external ICU_DEFAULT_I18N_MODULE_NAME name UdatGetContextDefaultExportName;
function UdatToPatternRelativeDate; external ICU_DEFAULT_I18N_MODULE_NAME name UdatToPatternRelativeDateDefaultExportName;
function UdatToPatternRelativeTime; external ICU_DEFAULT_I18N_MODULE_NAME name UdatToPatternRelativeTimeDefaultExportName;
procedure UdatApplyPatternRelative; external ICU_DEFAULT_I18N_MODULE_NAME name UdatApplyPatternRelativeDefaultExportName;

{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @UdatToCalendarDateField := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatToCalendarDateFieldExportName);
  @UdatOpen := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatOpenExportName);
  @UdatClose := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatCloseExportName);
  @UdatClone := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatCloneExportName);
  @UdatFormat := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatFormatExportName);
  @UdatParse := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatParseExportName);
  @UdatParseCalendar := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatParseCalendarExportName);
  @UdatIsLenient := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatIsLenientExportName);
  @UdatSetLenient := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatSetLenientExportName);
  @UdatGetCalendar := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGetCalendarExportName);
  @UdatSetCalendar := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatSetCalendarExportName);
  @UdatGetNumberFormat := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGetNumberFormatExportName);
  @UdatSetNumberFormat := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatSetNumberFormatExportName);
  @UdatGetAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGetAvailableExportName);
  @UdatCountAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatCountAvailableExportName);
  @UdatGet2DigitYearStart := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGet2DigitYearStartExportName);
  @UdatSet2DigitYearStart := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatSet2DigitYearStartExportName);
  @UdatToPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatToPatternExportName);
  @UdatApplyPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatApplyPatternExportName);
  @UdatGetSymbols := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGetSymbolsExportName);
  @UdatCountSymbols := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatCountSymbolsExportName);
  @UdatSetSymbols := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatSetSymbolsExportName);
  @UdatGetLocaleByType := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGetLocaleByTypeExportName);
  @UdatSetContext := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatSetContextExportName);
  @UdatGetContext := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatGetContextExportName);
  @UdatToPatternRelativeDate := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatToPatternRelativeDateExportName);
  @UdatToPatternRelativeTime := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatToPatternRelativeTimeExportName);
  @UdatApplyPatternRelative := GetModuleSymbol(ICU_I18N_LibraryHandle, UdatApplyPatternRelativeExportName);

  Result := Assigned(@UdatToCalendarDateField) and Assigned(@UdatOpen) and Assigned(@UdatClose) and Assigned(@UdatClone) and Assigned(@UdatFormat) and
    Assigned(@UdatParse) and Assigned(@UdatParseCalendar) and Assigned(@UdatIsLenient) and Assigned(@UdatSetLenient) and Assigned(@UdatGetCalendar) and
    Assigned(@UdatSetCalendar) and Assigned(@UdatGetNumberFormat) and Assigned(@UdatSetNumberFormat) and Assigned(@UdatGetAvailable) and
    Assigned(@UdatCountAvailable) and Assigned(@UdatGet2DigitYearStart) and Assigned(@UdatSet2DigitYearStart) and Assigned(@UdatToPattern) and
    Assigned(@UdatApplyPattern) and Assigned(@UdatGetSymbols) and Assigned(@UdatCountSymbols) and Assigned(@UdatSetSymbols) and Assigned(@UdatGetLocaleByType)
    and Assigned(@UdatSetContext) and Assigned(@UdatGetContext) and Assigned(@UdatToPatternRelativeDate) and Assigned(@UdatToPatternRelativeTime) and
    Assigned(@UdatApplyPatternRelative);
end;

procedure UnloadICU;
begin
  @UdatToCalendarDateField := nil;
  @UdatOpen := nil;
  @UdatClose := nil;
  @UdatClone := nil;
  @UdatFormat := nil;
  @UdatParse := nil;
  @UdatParseCalendar := nil;
  @UdatIsLenient := nil;
  @UdatSetLenient := nil;
  @UdatGetCalendar := nil;
  @UdatSetCalendar := nil;
  @UdatGetNumberFormat := nil;
  @UdatSetNumberFormat := nil;
  @UdatGetAvailable := nil;
  @UdatCountAvailable := nil;
  @UdatGet2DigitYearStart := nil;
  @UdatSet2DigitYearStart := nil;
  @UdatToPattern := nil;
  @UdatApplyPattern := nil;
  @UdatGetSymbols := nil;
  @UdatCountSymbols := nil;
  @UdatSetSymbols := nil;
  @UdatGetLocaleByType := nil;
  @UdatSetContext := nil;
  @UdatGetContext := nil;
  @UdatToPatternRelativeDate := nil;
  @UdatToPatternRelativeTime := nil;
  @UdatApplyPatternRelative := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
