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
    UDAT_FULL_RELATIVE = UDAT_FULL + UDAT_RELATIVE, // or
    UDAT_LONG_RELATIVE = UDAT_LONG + UDAT_RELATIVE, // or
    UDAT_MEDIUM_RELATIVE = UDAT_MEDIUM + UDAT_RELATIVE, // or
    UDAT_SHORT_RELATIVE = UDAT_SHORT + UDAT_RELATIVE, // or
    UDAT_NONE = -1,
    UDAT_PATTERN = -2,
    UDAT_IGNORE = UDAT_PATTERN
  );

const
  // Constant for date skeleton with year.
  UDAT_YEAR = 'y';
{$IFNDEF U_HIDE_DRAFT_API}
  // Constant for date skeleton with quarter.
  UDAT_QUARTER = 'QQQQ';
  // Constant for date skeleton with abbreviated quarter.
  UDAT_ABBR_QUARTER = 'QQQ';
{$ENDIF ~U_HIDE_DRAFT_API}
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
{$IFNDEF U_HIDE_DRAFT_API}
  // Constant for date skeleton with weekday.
  UDAT_WEEKDAY = 'EEEE';
  // Constant for date skeleton with abbreviated weekday.
  UDAT_ABBR_WEEKDAY = 'E';
{$ENDIF ~U_HIDE_DRAFT_API}
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
{$IFNDEF U_HIDE_DRAFT_API}
  // Constant for date skeleton with hour in 24-hour presentation.
  UDAT_HOUR24 = 'H';
  // Constant for date skeleton with minute.
  UDAT_MINUTE = 'm';
{$ENDIF ~U_HIDE_DRAFT_API}
  // Constant for date skeleton with hour and minute, with the locale's preferred hour format (12 or 24).
  UDAT_HOUR_MINUTE = 'jm';
  // Constant for date skeleton with hour and minute in 24-hour presentation.
  UDAT_HOUR24_MINUTE = 'Hm';
{$IFNDEF U_HIDE_DRAFT_API}
  // Constant for date skeleton with second.
  UDAT_SECOND = 's';
{$ENDIF ~U_HIDE_DRAFT_API}
  // Constant for date skeleton with hour, minute, and second, with the locale's preferred hour format (12 or 24).
  UDAT_HOUR_MINUTE_SECOND = 'jms';
  // Constant for date skeleton with hour, minute, and second in 24-hour presentation.
  UDAT_HOUR24_MINUTE_SECOND = 'Hms';
  // Constant for date skeleton with minute and second.
  UDAT_MINUTE_SECOND = 'ms';
{$IFNDEF U_HIDE_DRAFT_API}
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
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_DEPRECATED_API}
  // Constant for date skeleton with standalone month.
  UDAT_STANDALONE_MONTH = 'LLLL';
  // Constant for date skeleton with standalone abbreviated month.
  UDAT_ABBR_STANDALONE_MONTH = 'LLL';
  // Constant for date skeleton with hour, minute, and generic timezone.
  UDAT_HOUR_MINUTE_GENERIC_TZ = 'jmv';
  // Constant for date skeleton with hour, minute, and timezone.
  UDAT_HOUR_MINUTE_TZ = 'jmz';
  // Constant for date skeleton with hour and generic timezone.
  UDAT_HOUR_GENERIC_TZ = 'jv';
  // Constant for date skeleton with hour and timezone.
  UDAT_HOUR_TZ = 'jz';
{$ENDIF ~U_HIDE_DEPRECATED_API}

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
{$IFNDEF U_HIDE_DRAFT_API}
    UDAT_TIMEZONE_LOCALIZED_GMT_OFFSET_FIELD = 31,
    UDAT_TIMEZONE_ISO_FIELD = 32,
    UDAT_TIMEZONE_ISO_LOCAL_FIELD = 33,
{$ENDIF ~U_HIDE_DRAFT_API}
    UDAT_FIELD_COUNT = 34
  );

  UDateFormatBooleanAttribute = (
    UDAT_PARSE_ALLOW_WHITESPACE,
    UDAT_PARSE_ALLOW_NUMERIC,
    UDAT_BOOLEAN_ATTRIBUTE_COUNT
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
    UDAT_STANDALONE_SHORT_QUARTERS
{$IFNDEF U_HIDE_DRAFT_API}
    ,UDAT_SHORTER_WEEKDAYS
    ,UDAT_STANDALONE_SHORTER_WEEKDAYS
{$ENDIF ~U_HIDE_DRAFT_API}
  );

  // Date format symbols.
  UDateFormatSymbols = packed record
  end;

{$IFNDEF U_HIDE_INTERNAL_API}
  PUDateFormatOpener = ^UDateFormatOpener;
  UDateFormatOpener = function(timeStyle: UDateFormatStyle; dateStyle: UDateFormatStyle; const locale: PAnsiChar; const tzID: PUChar; tzIDLength: Int32; const pattern: PUChar; patternLength: Int32; var status: UErrorCode): PUDateFormat; cdecl;
{$ENDIF ~U_HIDE_INTERNAL_API}

{$IFDEF ICU_LINKONREQUEST}
{$IFNDEF U_HIDE_INTERNAL_API}
  // Get a boolean attribute associated with a UDateFormat.
  //  Tudat_getBooleanAttribute = function(const format: PUDateFormat; attr: UDateFormatBooleanAttribute; var status: UErrorCode): UBool; cdecl;
  // Set a boolean attribute associated with a UDateFormat.
  //  Tudat_setBooleanAttribute = procedure(format: PUDateFormat; attr: UDateFormatBooleanAttribute; value: UBool; var status: UErrorCode); cdecl;
{$ENDIF ~U_HIDE_INTERNAL_API}
  // Maps from a UDateFormatField to the corresponding UCalendarDateFields.
  Tudat_toCalendarDateField = function(field: UDateFormatField): UCalendarDateFields; cdecl;
  // Open a new UDateFormat for formatting and parsing dates and times.
  Tudat_open = function(timeStyle: UDateFormatStyle; dateStyle: UDateFormatStyle; const locale: PAnsiChar; const tzID: PUChar; tzIDLength: Int32; const pattern: PUChar; patternLength: Int32; var status: UErrorCode): PUDateFormat; cdecl;
  // Close a UDateFormat.
  Tudat_close = procedure(format: PUDateFormat); cdecl;
  // Open a copy of a UDateFormat.
  Tudat_clone = function(const format: PUDateFormat; var status: UErrorCode): PUDateFormat; cdecl;
  // Format a date using an UDateFormat.
  Tudat_format = function(const format: PUDateFormat; dateToFormat: UDate; result: PUChar; resultLength: Int32; position: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  // Parse a string into an date/time using a UDateFormat.
  Tudat_parse = function(const format: PUDateFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): UDate; cdecl;
  // Parse a string into an date/time using a UDateFormat.
  Tudat_parseCalendar = procedure(const format: PUDateFormat; calendar: PUCalendar; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode); cdecl;
  // Determine if an UDateFormat will perform lenient parsing.
  Tudat_isLenient = function(const format: PUDateFormat): UBool; cdecl;
  // Specify whether an UDateFormat will perform lenient parsing.
  Tudat_setLenient = procedure(format: PUDateFormat; isLenient: UBool); cdecl;
  // Get the UCalendar associated with an UDateFormat.
  Tudat_getCalendar = function(const format: PUDateFormat): PUCalendar; cdecl;
  // Set the UCalendar associated with an UDateFormat.
  Tudat_setCalendar = procedure(format: PUDateFormat; const calendarToSet: PUCalendar); cdecl;
  // Get the UNumberFormat associated with an UDateFormat.
  Tudat_getNumberFormat = function(const format: PUDateFormat): PUNumberFormat; cdecl;
  // Set the UNumberFormat associated with an UDateFormat.
  Tudat_setNumberFormat = procedure(format: PUDateFormat; const numberFormatToSet: PUNumberFormat); cdecl;
  // Get a locale for which date/time formatting patterns are available.
  Tudat_getAvailable = function(localeIndex: Int32): PAnsiChar; cdecl;
  // Determine how many locales have date/time formatting patterns available.
  Tudat_countAvailable = function: Int32; cdecl;
  // Get the year relative to which all 2-digit years are interpreted.
  Tudat_get2DigitYearStart = function(const format: PUDateFormat; var status: UErrorCode): UDate; cdecl;
  // Set the year relative to which all 2-digit years will be interpreted.
  Tudat_set2DigitYearStart = procedure(format: PUDateFormat; d: UDate; var status: UErrorCode); cdecl;
  // Extract the pattern from a UDateFormat.
  Tudat_toPattern = function(const format: PUDateFormat; localized: UBool; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Set the pattern used by an UDateFormat.
  Tudat_applyPattern = procedure(format: PUDateFormat; localized: UBool; const pattern: PUChar; patternLength: Int32); cdecl;
  // Get the symbols associated with an UDateFormat.
  Tudat_getSymbols = function(const format: PUDateFormat; _type: UDateFormatSymbolType; symbolIndex: Int32; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Count the number of particular symbols for an UDateFormat.
  Tudat_countSymbols = function(const format: PUDateFormat; _type: UDateFormatSymbolType): Int32; cdecl;
  // Set the symbols associated with an UDateFormat.
  Tudat_setSymbols = procedure(format: PUDateFormat; _type: UDateFormatSymbolType; symbolIndex: Int32; value: PUChar; valueLength: Int32; var status: UErrorCode); cdecl;
  // Get the locale for this date format object.
  Tudat_getLocaleByType = function(const format: PUDateFormat; _type: ULocDataLocaleType; var status: UErrorCode): PAnsiChar; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  // Set a particular UDisplayContext value in the formatter, such as UDISPCTX_CAPITALIZATION_FOR_STANDALONE.
  Tudat_setContext = procedure(format: PUDateFormat; value: UDisplayContext; var status: UErrorCode); cdecl;
  // Get the formatter's UDisplayContext value for the specified UDisplayContextType, such as UDISPCTX_TYPE_CAPITALIZATION.
  Tudat_getContext = function(format: PUDateFormat; _type: UDisplayContextType; var status: UErrorCode): UDisplayContext; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  // Extract the date pattern from a UDateFormat set for relative date formatting.
  Tudat_toPatternRelativeDate = function(const format: PUDateFormat; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Extract the time pattern from a UDateFormat set for relative date formatting.
  Tudat_toPatternRelativeTime = function(const format: PUDateFormat; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Set the date & time patterns used by a UDateFormat set for relative date formatting.
  Tudat_applyPatternRelative = procedure(format: PUDateFormat; const datePattern: PUChar; datePatternLength: Int32; const timePattern: PUChar; timePatternLength: Int32; var status: UErrorCode); cdecl;
  // Register a provider factory.
  Tudat_registerOpener = procedure(opener: UDateFormatOpener; var status: UErrorCode); cdecl;
  // Un-Register a provider factory.
  Tudat_unregisterOpener = function(opener: UDateFormatOpener; var status: UErrorCode): UDateFormatOpener; cdecl;
{$ENDIF ~U_HIDE_INTERNAL_API}

var
{$IFNDEF U_HIDE_INTERNAL_API}
//  udat_getBooleanAttribute: Tudat_getBooleanAttribute = nil;
//  udat_setBooleanAttribute: Tudat_setBooleanAttribute = nil;
{$ENDIF ~U_HIDE_INTERNAL_API}
  udat_toCalendarDateField: Tudat_toCalendarDateField = nil;
  udat_open: Tudat_open = nil;
  udat_close: Tudat_close = nil;
  udat_clone: Tudat_clone = nil;
  udat_format: Tudat_format = nil;
  udat_parse: Tudat_parse = nil;
  udat_parseCalendar: Tudat_parseCalendar = nil;
  udat_isLenient: Tudat_isLenient = nil;
  udat_setLenient: Tudat_setLenient = nil;
  udat_getCalendar: Tudat_getCalendar = nil;
  udat_setCalendar: Tudat_setCalendar = nil;
  udat_getNumberFormat: Tudat_getNumberFormat = nil;
  udat_setNumberFormat: Tudat_setNumberFormat = nil;
  udat_getAvailable: Tudat_getAvailable = nil;
  udat_countAvailable: Tudat_countAvailable = nil;
  udat_get2DigitYearStart: Tudat_get2DigitYearStart = nil;
  udat_set2DigitYearStart: Tudat_set2DigitYearStart = nil;
  udat_toPattern: Tudat_toPattern = nil;
  udat_applyPattern: Tudat_applyPattern = nil;
  udat_getSymbols: Tudat_getSymbols = nil;
  udat_countSymbols: Tudat_countSymbols = nil;
  udat_setSymbols: Tudat_setSymbols = nil;
  udat_getLocaleByType: Tudat_getLocaleByType = nil;
{$IFNDEF U_HIDE_DRAFT_API}
  udat_setContext: Tudat_setContext = nil;
  udat_getContext: Tudat_getContext = nil;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  udat_toPatternRelativeDate: Tudat_toPatternRelativeDate = nil;
  udat_toPatternRelativeTime: Tudat_toPatternRelativeTime = nil;
  udat_applyPatternRelative: Tudat_applyPatternRelative = nil;
  udat_registerOpener: Tudat_registerOpener = nil;
  udat_unregisterOpener: Tudat_unregisterOpener = nil;
{$ENDIF ~U_HIDE_INTERNAL_API}
{$ELSE ~ICU_LINKONREQUEST}
{$IFNDEF U_HIDE_INTERNAL_API}
//  function udat_getBooleanAttribute(const format: PUDateFormat; attr: UDateFormatBooleanAttribute; var status: UErrorCode): UBool; cdecl;
//  procedure udat_setBooleanAttribute(format: PUDateFormat; attr: UDateFormatBooleanAttribute; value: UBool; var status: UErrorCode); cdecl;
{$ENDIF ~U_HIDE_INTERNAL_API}
  function udat_toCalendarDateField(field: UDateFormatField): UCalendarDateFields; cdecl;
  function udat_open(timeStyle: UDateFormatStyle; dateStyle: UDateFormatStyle; const locale: PAnsiChar; const tzID: PUChar; tzIDLength: Int32; const pattern: PUChar; patternLength: Int32; var status: UErrorCode): PUDateFormat; cdecl;
  procedure udat_close(format: PUDateFormat); cdecl;
  function udat_clone(const format: PUDateFormat; var status: UErrorCode): PUDateFormat; cdecl;
  function udat_format(const format: PUDateFormat; dateToFormat: UDate; result: PUChar; resultLength: Int32; position: PUFieldPosition; var status: UErrorCode): Int32; cdecl;
  function udat_parse(const format: PUDateFormat; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode): UDate; cdecl;
  procedure udat_parseCalendar(const format: PUDateFormat; calendar: PUCalendar; const text: PUChar; textLength: Int32; parsePos: PInt32; var status: UErrorCode); cdecl;
  function udat_isLenient(const format: PUDateFormat): UBool; cdecl;
  procedure udat_setLenient(format: PUDateFormat; isLenient: UBool); cdecl;
  function udat_getCalendar(const format: PUDateFormat): PUCalendar; cdecl;
  procedure udat_setCalendar(format: PUDateFormat; const calendarToSet: PUCalendar); cdecl;
  function udat_getNumberFormat(const format: PUDateFormat): PUNumberFormat; cdecl;
  procedure udat_setNumberFormat(format: PUDateFormat; const numberFormatToSet: PUNumberFormat); cdecl;
  function udat_getAvailable(localeIndex: Int32): PAnsiChar; cdecl;
  function udat_countAvailable: Int32; cdecl;
  function udat_get2DigitYearStart(const format: PUDateFormat; var status: UErrorCode): UDate; cdecl;
  procedure udat_set2DigitYearStart(format: PUDateFormat; d: UDate; var status: UErrorCode); cdecl;
  function udat_toPattern(const format: PUDateFormat; localized: UBool; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  procedure udat_applyPattern(format: PUDateFormat; localized: UBool; const pattern: PUChar; patternLength: Int32); cdecl;
  function udat_getSymbols(const format: PUDateFormat; _type: UDateFormatSymbolType; symbolIndex: Int32; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  function udat_countSymbols(const format: PUDateFormat; _type: UDateFormatSymbolType): Int32; cdecl;
  procedure udat_setSymbols(format: PUDateFormat; _type: UDateFormatSymbolType; symbolIndex: Int32; value: PUChar; valueLength: Int32; var status: UErrorCode); cdecl;
  function udat_getLocaleByType(const format: PUDateFormat; _type: ULocDataLocaleType; var status: UErrorCode): PAnsiChar; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  procedure udat_setContext(format: PUDateFormat; value: UDisplayContext; var status: UErrorCode); cdecl;
  function udat_getContext(format: PUDateFormat; _type: UDisplayContextType; var status: UErrorCode): UDisplayContext; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  function udat_toPatternRelativeDate(const format: PUDateFormat; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  function udat_toPatternRelativeTime(const format: PUDateFormat; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  procedure udat_applyPatternRelative(format: PUDateFormat; const datePattern: PUChar; datePatternLength: Int32; const timePattern: PUChar; timePatternLength: Int32; var status: UErrorCode); cdecl;
  procedure udat_registerOpener(opener: UDateFormatOpener; var status: UErrorCode); cdecl;
  function udat_unregisterOpener(opener: UDateFormatOpener; var status: UErrorCode): UDateFormatOpener; cdecl;
{$ENDIF ~U_HIDE_INTERNAL_API}
{$ENDIF ~ICU_LINKONREQUEST}

const
{$IFNDEF U_HIDE_INTERNAL_API}
//  udat_getBooleanAttributeDefaultExportName = 'udat_getBooleanAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
//  udat_setBooleanAttributeDefaultExportName = 'udat_setBooleanAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_INTERNAL_API}
  udat_toCalendarDateFieldDefaultExportName = 'udat_toCalendarDateField' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_openDefaultExportName = 'udat_open' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_closeDefaultExportName = 'udat_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_cloneDefaultExportName = 'udat_clone' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_formatDefaultExportName = 'udat_format' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_parseDefaultExportName = 'udat_parse' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_parseCalendarDefaultExportName = 'udat_parseCalendar' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_isLenientDefaultExportName = 'udat_isLenient' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_setLenientDefaultExportName = 'udat_setLenient' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_getCalendarDefaultExportName = 'udat_getCalendar' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_setCalendarDefaultExportName = 'udat_setCalendar' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_getNumberFormatDefaultExportName = 'udat_getNumberFormat' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_setNumberFormatDefaultExportName = 'udat_setNumberFormat' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_getAvailableDefaultExportName = 'udat_getAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_countAvailableDefaultExportName = 'udat_countAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_get2DigitYearStartDefaultExportName = 'udat_get2DigitYearStart' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_set2DigitYearStartDefaultExportName = 'udat_set2DigitYearStart' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_toPatternDefaultExportName = 'udat_toPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_applyPatternDefaultExportName = 'udat_applyPattern' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_getSymbolsDefaultExportName = 'udat_getSymbols' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_countSymbolsDefaultExportName = 'udat_countSymbols' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_setSymbolsDefaultExportName = 'udat_setSymbols' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_getLocaleByTypeDefaultExportName = 'udat_getLocaleByType' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFNDEF U_HIDE_DRAFT_API}
  udat_setContextDefaultExportName = 'udat_setContext' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_getContextDefaultExportName = 'udat_getContext' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  udat_toPatternRelativeDateDefaultExportName = 'udat_toPatternRelativeDate' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_toPatternRelativeTimeDefaultExportName = 'udat_toPatternRelativeTime' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_applyPatternRelativeDefaultExportName = 'udat_applyPatternRelative' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_registerOpenerDefaultExportName = 'udat_registerOpener' + ICU_DEFAULT_EXPORT_SUFFIX;
  udat_unregisterOpenerDefaultExportName = 'udat_unregisterOpener' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_INTERNAL_API}

{$IFDEF ICU_LINKONREQUEST}

var
{$IFNDEF U_HIDE_INTERNAL_API}
//  udat_getBooleanAttributeExportName: string = udat_getBooleanAttributeDefaultExportName;
//  udat_setBooleanAttributeExportName: string = udat_setBooleanAttributeDefaultExportName;
{$ENDIF ~U_HIDE_INTERNAL_API}
  udat_toCalendarDateFieldExportName: string = udat_toCalendarDateFieldDefaultExportName;
  udat_openExportName: string = udat_openDefaultExportName;
  udat_closeExportName: string = udat_closeDefaultExportName;
  udat_cloneExportName: string = udat_cloneDefaultExportName;
  udat_formatExportName: string = udat_formatDefaultExportName;
  udat_parseExportName: string = udat_parseDefaultExportName;
  udat_parseCalendarExportName: string = udat_parseCalendarDefaultExportName;
  udat_isLenientExportName: string = udat_isLenientDefaultExportName;
  udat_setLenientExportName: string = udat_setLenientDefaultExportName;
  udat_getCalendarExportName: string = udat_getCalendarDefaultExportName;
  udat_setCalendarExportName: string = udat_setCalendarDefaultExportName;
  udat_getNumberFormatExportName: string = udat_getNumberFormatDefaultExportName;
  udat_setNumberFormatExportName: string = udat_setNumberFormatDefaultExportName;
  udat_getAvailableExportName: string = udat_getAvailableDefaultExportName;
  udat_countAvailableExportName: string = udat_countAvailableDefaultExportName;
  udat_get2DigitYearStartExportName: string = udat_get2DigitYearStartDefaultExportName;
  udat_set2DigitYearStartExportName: string = udat_set2DigitYearStartDefaultExportName;
  udat_toPatternExportName: string = udat_toPatternDefaultExportName;
  udat_applyPatternExportName: string = udat_applyPatternDefaultExportName;
  udat_getSymbolsExportName: string = udat_getSymbolsDefaultExportName;
  udat_countSymbolsExportName: string = udat_countSymbolsDefaultExportName;
  udat_setSymbolsExportName: string = udat_setSymbolsDefaultExportName;
  udat_getLocaleByTypeExportName: string = udat_getLocaleByTypeDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
  udat_setContextExportName: string = udat_setContextDefaultExportName;
  udat_getContextExportName: string = udat_getContextDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  udat_toPatternRelativeDateExportName: string = udat_toPatternRelativeDateDefaultExportName;
  udat_toPatternRelativeTimeExportName: string = udat_toPatternRelativeTimeDefaultExportName;
  udat_applyPatternRelativeExportName: string = udat_applyPatternRelativeDefaultExportName;
  udat_registerOpenerExportName: string = udat_registerOpenerDefaultExportName;
  udat_unregisterOpenerExportName: string = udat_unregisterOpenerDefaultExportName;
{$ENDIF ~U_HIDE_INTERNAL_API}
{$ENDIF ~ICU_LINKONREQUEST}

implementation

{$IFNDEF ICU_LINKONREQUEST}
{$IFNDEF U_HIDE_INTERNAL_API}
// function udat_getBooleanAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getBooleanAttributeDefaultExportName;
// procedure udat_setBooleanAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name udat_setBooleanAttributeDefaultExportName;
{$ENDIF ~U_HIDE_INTERNAL_API}
function udat_toCalendarDateField; external ICU_DEFAULT_I18N_MODULE_NAME name udat_toCalendarDateFieldDefaultExportName;
function udat_open; external ICU_DEFAULT_I18N_MODULE_NAME name udat_openDefaultExportName;
procedure udat_close; external ICU_DEFAULT_I18N_MODULE_NAME name udat_closeDefaultExportName;
function udat_clone; external ICU_DEFAULT_I18N_MODULE_NAME name udat_cloneDefaultExportName;
function udat_format; external ICU_DEFAULT_I18N_MODULE_NAME name udat_formatDefaultExportName;
function udat_parse; external ICU_DEFAULT_I18N_MODULE_NAME name udat_parseDefaultExportName;
procedure udat_parseCalendar; external ICU_DEFAULT_I18N_MODULE_NAME name udat_parseCalendarDefaultExportName;
function udat_isLenient; external ICU_DEFAULT_I18N_MODULE_NAME name udat_isLenientDefaultExportName;
procedure udat_setLenient; external ICU_DEFAULT_I18N_MODULE_NAME name udat_setLenientDefaultExportName;
function udat_getCalendar; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getCalendarDefaultExportName;
procedure udat_setCalendar; external ICU_DEFAULT_I18N_MODULE_NAME name udat_setCalendarDefaultExportName;
function udat_getNumberFormat; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getNumberFormatDefaultExportName;
procedure udat_setNumberFormat; external ICU_DEFAULT_I18N_MODULE_NAME name udat_setNumberFormatDefaultExportName;
function udat_getAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getAvailableDefaultExportName;
function udat_countAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name udat_countAvailableDefaultExportName;
function udat_get2DigitYearStart; external ICU_DEFAULT_I18N_MODULE_NAME name udat_get2DigitYearStartDefaultExportName;
procedure udat_set2DigitYearStart; external ICU_DEFAULT_I18N_MODULE_NAME name udat_set2DigitYearStartDefaultExportName;
function udat_toPattern; external ICU_DEFAULT_I18N_MODULE_NAME name udat_toPatternDefaultExportName;
procedure udat_applyPattern; external ICU_DEFAULT_I18N_MODULE_NAME name udat_applyPatternDefaultExportName;
function udat_getSymbols; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getSymbolsDefaultExportName;
function udat_countSymbols; external ICU_DEFAULT_I18N_MODULE_NAME name udat_countSymbolsDefaultExportName;
procedure udat_setSymbols; external ICU_DEFAULT_I18N_MODULE_NAME name udat_setSymbolsDefaultExportName;
function udat_getLocaleByType; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getLocaleByTypeDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
procedure udat_setContext; external ICU_DEFAULT_I18N_MODULE_NAME name udat_setContextDefaultExportName;
function udat_getContext; external ICU_DEFAULT_I18N_MODULE_NAME name udat_getContextDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
function udat_toPatternRelativeDate; external ICU_DEFAULT_I18N_MODULE_NAME name udat_toPatternRelativeDateDefaultExportName;
function udat_toPatternRelativeTime; external ICU_DEFAULT_I18N_MODULE_NAME name udat_toPatternRelativeTimeDefaultExportName;
procedure udat_applyPatternRelative; external ICU_DEFAULT_I18N_MODULE_NAME name udat_applyPatternRelativeDefaultExportName;
procedure udat_registerOpener; external ICU_DEFAULT_I18N_MODULE_NAME name udat_registerOpenerDefaultExportName;
function udat_unregisterOpener; external ICU_DEFAULT_I18N_MODULE_NAME name udat_unregisterOpenerDefaultExportName;
{$ENDIF ~U_HIDE_INTERNAL_API}

{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
{$IFNDEF U_HIDE_INTERNAL_API}
//  @udat_getBooleanAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getBooleanAttributeExportName);
//  @udat_setBooleanAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_setBooleanAttributeExportName);
{$ENDIF ~U_HIDE_INTERNAL_API}
  @udat_toCalendarDateField := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_toCalendarDateFieldExportName);
  @udat_open := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_openExportName);
  @udat_close := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_closeExportName);
  @udat_clone := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_cloneExportName);
  @udat_format := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_formatExportName);
  @udat_parse := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_parseExportName);
  @udat_parseCalendar := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_parseCalendarExportName);
  @udat_isLenient := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_isLenientExportName);
  @udat_setLenient := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_setLenientExportName);
  @udat_getCalendar := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getCalendarExportName);
  @udat_setCalendar := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_setCalendarExportName);
  @udat_getNumberFormat := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getNumberFormatExportName);
  @udat_setNumberFormat := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_setNumberFormatExportName);
  @udat_getAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getAvailableExportName);
  @udat_countAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_countAvailableExportName);
  @udat_get2DigitYearStart := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_get2DigitYearStartExportName);
  @udat_set2DigitYearStart := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_set2DigitYearStartExportName);
  @udat_toPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_toPatternExportName);
  @udat_applyPattern := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_applyPatternExportName);
  @udat_getSymbols := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getSymbolsExportName);
  @udat_countSymbols := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_countSymbolsExportName);
  @udat_setSymbols := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_setSymbolsExportName);
  @udat_getLocaleByType := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getLocaleByTypeExportName);
{$IFNDEF U_HIDE_DRAFT_API}
  @udat_setContext := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_setContextExportName);
  @udat_getContext := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_getContextExportName);
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  @udat_toPatternRelativeDate := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_toPatternRelativeDateExportName);
  @udat_toPatternRelativeTime := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_toPatternRelativeTimeExportName);
  @udat_applyPatternRelative := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_applyPatternRelativeExportName);
  @udat_registerOpener := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_registerOpenerExportName);
  @udat_unregisterOpener := GetModuleSymbol(ICU_I18N_LibraryHandle, udat_unregisterOpenerExportName);
{$ENDIF ~U_HIDE_INTERNAL_API}

  Result :=
{$IFNDEF U_HIDE_INTERNAL_API}
//    Assigned(@udat_getBooleanAttribute) and Assigned(@udat_setBooleanAttribute) and
{$ENDIF ~U_HIDE_INTERNAL_API}
    Assigned(@udat_toCalendarDateField) and Assigned(@udat_open) and Assigned(@udat_close) and Assigned(@udat_clone) and Assigned(@udat_format) and
    Assigned(@udat_parse) and Assigned(@udat_parseCalendar) and Assigned(@udat_isLenient) and Assigned(@udat_setLenient) and Assigned(@udat_getCalendar) and
    Assigned(@udat_setCalendar) and Assigned(@udat_getNumberFormat) and Assigned(@udat_setNumberFormat) and Assigned(@udat_getAvailable) and
    Assigned(@udat_countAvailable) and Assigned(@udat_get2DigitYearStart) and Assigned(@udat_set2DigitYearStart) and Assigned(@udat_toPattern) and
    Assigned(@udat_applyPattern) and Assigned(@udat_getSymbols) and Assigned(@udat_countSymbols) and Assigned(@udat_setSymbols) and
    Assigned(@udat_getLocaleByType)
{$IFNDEF U_HIDE_DRAFT_API}
    and Assigned(@udat_setContext) and Assigned(@udat_getContext)
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
    and Assigned(@udat_toPatternRelativeDate) and Assigned(@udat_toPatternRelativeTime) and Assigned(@udat_applyPatternRelative) and
    Assigned(@udat_registerOpener) and Assigned(@udat_unregisterOpener)
{$ENDIF ~U_HIDE_INTERNAL_API}
    ;
end;

procedure UnloadICU;
begin
{$IFNDEF U_HIDE_INTERNAL_API}
//  @udat_getBooleanAttribute := nil;
//  @udat_setBooleanAttribute := nil;
{$ENDIF ~U_HIDE_INTERNAL_API}
  @udat_toCalendarDateField := nil;
  @udat_open := nil;
  @udat_close := nil;
  @udat_clone := nil;
  @udat_format := nil;
  @udat_parse := nil;
  @udat_parseCalendar := nil;
  @udat_isLenient := nil;
  @udat_setLenient := nil;
  @udat_getCalendar := nil;
  @udat_setCalendar := nil;
  @udat_getNumberFormat := nil;
  @udat_setNumberFormat := nil;
  @udat_getAvailable := nil;
  @udat_countAvailable := nil;
  @udat_get2DigitYearStart := nil;
  @udat_set2DigitYearStart := nil;
  @udat_toPattern := nil;
  @udat_applyPattern := nil;
  @udat_getSymbols := nil;
  @udat_countSymbols := nil;
  @udat_setSymbols := nil;
  @udat_getLocaleByType := nil;
{$IFNDEF U_HIDE_DRAFT_API}
  @udat_setContext := nil;
  @udat_getContext := nil;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFNDEF U_HIDE_INTERNAL_API}
  @udat_toPatternRelativeDate := nil;
  @udat_toPatternRelativeTime := nil;
  @udat_applyPatternRelative := nil;
  @udat_registerOpener := nil;
  @udat_unregisterOpener := nil;
{$ENDIF ~U_HIDE_INTERNAL_API}
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
