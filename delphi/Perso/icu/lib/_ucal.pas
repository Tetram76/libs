unit _ucal;

{$I icu.inc}

interface

uses
  _utypes, _umachine, _enum, _uloc, icu_globals;

const
  UCAL_UNKNOWN_ZONE_ID = 'Etc/Unknown';

type
  PUCalendar = ^UCalendar;
  UCalendar = packed record
  end;

  UCalendarType = (
    UCAL_TRADITIONAL,
    UCAL_DEFAULT = UCAL_TRADITIONAL,
    UCAL_GREGORIAN
  );

  UCalendarDateFields = (
    UCAL_ERA,
    UCAL_YEAR,
    UCAL_MONTH,
    UCAL_WEEK_OF_YEAR,
    UCAL_WEEK_OF_MONTH,
    UCAL_DATE,
    UCAL_DAY_OF_YEAR,
    UCAL_DAY_OF_WEEK,
    UCAL_DAY_OF_WEEK_IN_MONTH,
    UCAL_AM_PM,
    UCAL_HOUR,
    UCAL_HOUR_OF_DAY,
    UCAL_MINUTE,
    UCAL_SECOND,
    UCAL_MILLISECOND,
    UCAL_ZONE_OFFSET,
    UCAL_DST_OFFSET,
    UCAL_YEAR_WOY,
    UCAL_DOW_LOCAL,
    UCAL_EXTENDED_YEAR,
    UCAL_JULIAN_DAY,
    UCAL_MILLISECONDS_IN_DAY,
    UCAL_IS_LEAP_MONTH,
    UCAL_FIELD_COUNT,
    UCAL_DAY_OF_MONTH=UCAL_DATE
  );

  UCalendarDaysOfWeek = (
    UCAL_SUNDAY = 1,
    UCAL_MONDAY,
    UCAL_TUESDAY,
    UCAL_WEDNESDAY,
    UCAL_THURSDAY,
    UCAL_FRIDAY,
    UCAL_SATURDAY
  );

  UCalendarMonths = (
    UCAL_JANUARY,
    UCAL_FEBRUARY,
    UCAL_MARCH,
    UCAL_APRIL,
    UCAL_MAY,
    UCAL_JUNE,
    UCAL_JULY,
    UCAL_AUGUST,
    UCAL_SEPTEMBER,
    UCAL_OCTOBER,
    UCAL_NOVEMBER,
    UCAL_DECEMBER,
    UCAL_UNDECIMBER
  );

  UCalendarAMPMs = (
    UCAL_AM,
    UCAL_PM
  );

  USystemTimeZoneType = (
    UCAL_ZONE_TYPE_ANY,
    UCAL_ZONE_TYPE_CANONICAL,
    UCAL_ZONE_TYPE_CANONICAL_LOCATION
  );

  UCalendarDisplayNameType = (
    UCAL_STANDARD,
    UCAL_SHORT_STANDARD,
    UCAL_DST,
    UCAL_SHORT_DST
  );

  UCalendarAttribute = (
    UCAL_LENIENT,
    UCAL_FIRST_DAY_OF_WEEK,
    UCAL_MINIMAL_DAYS_IN_FIRST_WEEK,
    UCAL_REPEATED_WALL_TIME,
    UCAL_SKIPPED_WALL_TIME
  );

  // Options for handling ambiguous wall time at time zone offset transitions.
  UCalendarWallTimeOption = (
    UCAL_WALLTIME_LAST,
    UCAL_WALLTIME_FIRST,
    UCAL_WALLTIME_NEXT_VALID
  );

  // Possible limit values for a UCalendar.
  UCalendarLimitType = (
    UCAL_MINIMUM,
    UCAL_MAXIMUM,
    UCAL_GREATEST_MINIMUM,
    UCAL_LEAST_MAXIMUM,
    UCAL_ACTUAL_MINIMUM,
    UCAL_ACTUAL_MAXIMUM
  );

  // Weekday types, as returned by ucal_getDayOfWeekType().
  UCalendarWeekdayType = (
    UCAL_WEEKDAY,
    UCAL_WEEKEND,
    UCAL_WEEKEND_ONSET,
    UCAL_WEEKEND_CEASE
  );

  // Time zone transition types for ucal_getTimeZoneTransitionDate.
  UTimeZoneTransitionType = (
    UCAL_TZ_TRANSITION_NEXT,
    UCAL_TZ_TRANSITION_NEXT_INCLUSIVE,
    UCAL_TZ_TRANSITION_PREVIOUS,
    UCAL_TZ_TRANSITION_PREVIOUS_INCLUSIVE
  );

{$IFDEF ICU_LINKONREQUEST}

type
  // Create an enumeration over system time zone IDs with the given filter conditions.
  Tucal_openTimeZoneIDEnumeration = function(zoneType: USystemTimeZoneType; region: PAnsiChar; const rawOffset: PInt32; var ec: UErrorCode): PUEnumeration; cdecl;
  // Create an enumeration over all time zones.
  Tucal_openTimeZones = function(var ec: UErrorCode): PUEnumeration; cdecl;
  // Create an enumeration over all time zones associated with the given country.
  Tucal_openCountryTimeZones = function(country: PAnsiChar; var ec: UErrorCode): PUEnumeration; cdecl;
  // Return the default time zone.
  Tucal_getDefaultTimeZone = function(result: PUChar; resultCapacity: Int32; var ec: UErrorCode): Int32; cdecl;
  // Set the default time zone.
  Tucal_setDefaultTimeZone = procedure(const zoneID: PUChar; var ec: UErrorCode); cdecl;
  // Return the amount of time in milliseconds that the clock is advanced during daylight savings time for the given time zone, or zero if the time zone does not observe daylight savings time.
  Tucal_getDSTSavings = function(const zoneID: PUChar; var ec: UErrorCode): Int32; cdecl;
  // Get the current date and time.
  Tucal_getNow = function: UDate; cdecl;
  // Open a UCalendar.
  Tucal_open = function(const zoneID: PUChar; len: Int32; const locale: PAnsiChar; _type: UCalendarType; var status: UErrorCode): PUCalendar; cdecl;
  // Close a UCalendar.
  Tucal_close = procedure(cal: PUCalendar); cdecl;
  // Open a copy of a UCalendar.
  Tucal_clone = function(const cal: PUCalendar; var status: UErrorCode): PUCalendar; cdecl;
  // Set the TimeZone used by a UCalendar.
  Tucal_setTimeZone = procedure(cal: PUCalendar; const zoneID: PUChar; len: Int32; var status: UErrorCode); cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  // Get the ID of the UCalendar's time zone.
  Tucal_getTimeZoneID = function(const cal: PUCalendar; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
  // Get the display name for a UCalendar's TimeZone.
  Tucal_getTimeZoneDisplayName = function(const cal: PUCalendar; _type: UCalendarDisplayNameType; const locale: PAnsiChar; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
  // Determine if a UCalendar is currently in daylight savings time.
  Tucal_inDaylightTime = function(const cal: PUCalendar; var status: UErrorCode): UBool; cdecl;
  // Sets the GregorianCalendar change date.
  Tucal_setGregorianChange = procedure(cal: PUCalendar; date: UDate; var pErrorCode: UErrorCode); cdecl;
  // Gets the Gregorian Calendar change date.
  Tucal_getGregorianChange = function(const cal: PUCalendar; var pErrorCode: UErrorCode): UDate; cdecl;
  // Get a numeric attribute associated with a UCalendar.
  Tucal_getAttribute = function(const cal: PUCalendar; attr: UCalendarAttribute): Int32; cdecl;
  // Set a numeric attribute associated with a UCalendar.
  Tucal_setAttribute = procedure(cal: PUCalendar; attr: UCalendarAttribute; newValue: Int32); cdecl;
  // Get a locale for which calendars are available.
  Tucal_getAvailable = function(localeIndex: Int32): PAnsiChar; cdecl;
  // Determine how many locales have calendars available.
  Tucal_countAvailable = function: Int32; cdecl;
  // Get a UCalendar's current time in millis.
  Tucal_getMillis = function(const cal: PUCalendar; var status: UErrorCode): UDate; cdecl;
  // Set a UCalendar's current time in millis.
  Tucal_setMillis = procedure(cal: PUCalendar; dateTime: UDate; var status: UErrorCode); cdecl;
  // Set a UCalendar's current date.
  Tucal_setDate = procedure(cal: PUCalendar; year, month, date: Int32; var status: UErrorCode); cdecl;
  // Set a UCalendar's current date.
  Tucal_setDateTime = procedure(cal: PUCalendar; year, month, date, hour, minute, second: Int32; var status: UErrorCode); cdecl;
  // Returns TRUE if two UCalendars are equivalent.
  Tucal_equivalentTo = function(const cal1, cal2: PUCalendar): UBool;
  // Add a specified signed amount to a particular field in a UCalendar.
  Tucal_add = procedure(cal: PUCalendar; field: UCalendarDateFields; amount: Int32; var status: UErrorCode); cdecl;
  // Add a specified signed amount to a particular field in a UCalendar.
  Tucal_roll = procedure(cal: PUCalendar; field: UCalendarDateFields; amount: Int32; var status: UErrorCode); cdecl;
  // Get the current value of a field from a UCalendar.
  Tucal_get = function(const cal: PUCalendar; field: UCalendarDateFields; var status: UErrorCode): Int32; cdecl;
  // Set the value of a field in a UCalendar.
  Tucal_set = procedure(cal: PUCalendar; field: UCalendarDateFields; value: Int32); cdecl;
  // Determine if a field in a UCalendar is set.
  Tucal_isSet = function(const cal: PUCalendar; field: UCalendarDateFields): UBool; cdecl;
  // Clear a field in a UCalendar.
  Tucal_clearField = procedure(cal: PUCalendar; field: UCalendarDateFields); cdecl;
  // Clear all fields in a UCalendar.
  Tucal_clear = procedure(calendar: PUCalendar); cdecl;
  // Determine a limit for a field in a UCalendar.
  Tucal_getLimit = function(const cal: PUCalendar; field: UCalendarDateFields; _type: UCalendarLimitType; var status: UErrorCode): Int32; cdecl;
  // Get the locale for this calendar object.
  Tucal_getLocaleByType = function(const cal: PUCalendar; _type: ULocDataLocaleType; var status: UErrorCode): PAnsiChar; cdecl;
  // Returns the timezone data version currently used by ICU.
  Tucal_getTZDataVersion = function(var status: UErrorCode): PAnsiChar; cdecl;
  // Returns the canonical system timezone ID or the normalized custom time zone ID for the given time zone ID.
  Tucal_getCanonicalTimeZoneID = function(const id: PUChar; len: Int32; result: PUChar; resultCapacity: Int32; var isSystemID: UBool; var status: UErrorCode): Int32; cdecl;
  // Get the resource keyword value string designating the calendar type for the UCalendar.
  Tucal_getType = function(const cal: PUCalendar; var status: UErrorCode): PAnsiChar; cdecl;
  // Given a key and a locale, returns an array of string values in a preferred order that would make a difference.
  Tucal_getKeywordValuesForLocale = function(const key, locale: PAnsiChar; commonlyUsed: UBool; var status: UErrorCode): PUEnumeration; cdecl;
  // Returns whether the given day of the week is a weekday, a weekend day, or a day that transitions from one to the other, for the locale and calendar system associated with this UCalendar (the locale's region is often the most determinant factor).
  Tucal_getDayOfWeekType = function(const cal: PUCalendar; dayOfWeek: UCalendarDaysOfWeek; var status: UErrorCode): UCalendarWeekdayType; cdecl;
  // Returns the time during the day at which the weekend begins or ends in this calendar system.
  Tucal_getWeekendTransition = function(const cal: PUCalendar; dayOfWeek: UCalendarDaysOfWeek; var status: UErrorCode): Int32; cdecl;
  // Returns TRUE if the given UDate is in the weekend in this calendar system.
  Tucal_isWeekend = function(const cal: PUCalendar; date: UDate; var status: UErrorCode): UBool; cdecl;
  // Return the difference between the target time and the time this calendar object is currently set to.
  Tucal_getFieldDifference = function(cal: PUCalendar; target: UDate; field: UCalendarDateFields; var status: UErrorCode): Int32; cdecl;
  // Get the UDate for the next/previous time zone transition relative to the calendar's current date, in the time zone to which the calendar is currently set.
  Tucal_getTimeZoneTransitionDate = function(const cal: PUCalendar; _type: UTimeZoneTransitionType; var transition: UDate; var status: UErrorCode): UBool; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
  // Converts a system time zone ID to an equivalent Windows time zone ID.
  Tucal_getWindowsTimeZoneID = function(const id: PUChar; len: Int32; winid: PUChar; winidCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Converts a Windows time zone ID to an equivalent system time zone ID for a region.
  Tucal_getTimeZoneIDForWindowsID = function(const winid: PUChar; len: Int32; const region: PAnsiChar; id: PUChar; idCapacity: Int32; var status: UErrorCode): Int32; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}

var
  ucal_openTimeZoneIDEnumeration: Tucal_openTimeZoneIDEnumeration = nil;
  ucal_openTimeZones: Tucal_openTimeZones = nil;
  ucal_openCountryTimeZones: Tucal_openCountryTimeZones = nil;
  ucal_getDefaultTimeZone: Tucal_getDefaultTimeZone = nil;
  ucal_setDefaultTimeZone: Tucal_setDefaultTimeZone = nil;
  ucal_getDSTSavings: Tucal_getDSTSavings = nil;
  ucal_getNow: Tucal_getNow = nil;
  ucal_open: Tucal_open = nil;
  ucal_close: Tucal_close = nil;
  ucal_clone: Tucal_clone = nil;
  ucal_setTimeZone: Tucal_setTimeZone = nil;
{$IFNDEF U_HIDE_DRAFT_API}
  ucal_getTimeZoneID: Tucal_getTimeZoneID = nil;
{$ENDIF ~U_HIDE_DRAFT_API}
  ucal_getTimeZoneDisplayName: Tucal_getTimeZoneDisplayName = nil;
  ucal_inDaylightTime: Tucal_inDaylightTime = nil;
  ucal_setGregorianChange: Tucal_setGregorianChange = nil;
  ucal_getGregorianChange: Tucal_getGregorianChange = nil;
  ucal_getAttribute: Tucal_getAttribute = nil;
  ucal_setAttribute: Tucal_setAttribute = nil;
  ucal_getAvailable: Tucal_getAvailable = nil;
  ucal_countAvailable: Tucal_countAvailable = nil;
  ucal_getMillis: Tucal_getMillis = nil;
  ucal_setMillis: Tucal_setMillis = nil;
  ucal_setDate: Tucal_setDate = nil;
  ucal_setDateTime: Tucal_setDateTime = nil;
  ucal_equivalentTo: Tucal_equivalentTo = nil;
  ucal_add: Tucal_add = nil;
  ucal_roll: Tucal_roll = nil;
  ucal_get: Tucal_get = nil;
  ucal_set: Tucal_set = nil;
  ucal_isSet: Tucal_isSet = nil;
  ucal_clearField: Tucal_clearField = nil;
  ucal_clear: Tucal_clear = nil;
  ucal_getLimit: Tucal_getLimit = nil;
  ucal_getLocaleByType: Tucal_getLocaleByType = nil;
  ucal_getTZDataVersion: Tucal_getTZDataVersion = nil;
  ucal_getCanonicalTimeZoneID: Tucal_getCanonicalTimeZoneID = nil;
  ucal_getType: Tucal_getType = nil;
  ucal_getKeywordValuesForLocale: Tucal_getKeywordValuesForLocale = nil;
  ucal_getDayOfWeekType: Tucal_getDayOfWeekType = nil;
  ucal_getWeekendTransition: Tucal_getWeekendTransition = nil;
  ucal_isWeekend: Tucal_isWeekend = nil;
  ucal_getFieldDifference: Tucal_getFieldDifference = nil;
  ucal_getTimeZoneTransitionDate: Tucal_getTimeZoneTransitionDate = nil;
{$IFNDEF U_HIDE_DRAFT_API}
  ucal_getWindowsTimeZoneID: Tucal_getWindowsTimeZoneID = nil;
  ucal_getTimeZoneIDForWindowsID: Tucal_getTimeZoneIDForWindowsID = nil;
{$ENDIF ~U_HIDE_DRAFT_API}
{$ELSE ~ICU_LINKONREQUEST}
function ucal_openTimeZoneIDEnumeration(zoneType: USystemTimeZoneType; region: PAnsiChar; const rawOffset: PInt32; var ec: UErrorCode): PUEnumeration; cdecl;
function ucal_openTimeZones(var ec: UErrorCode): PUEnumeration; cdecl;
function ucal_openCountryTimeZones(country: PAnsiChar; var ec: UErrorCode): PUEnumeration; cdecl;
function ucal_getDefaultTimeZone(result: PUChar; resultCapacity: Int32; var ec: UErrorCode): Int32; cdecl;
procedure ucal_setDefaultTimeZone(const zoneID: PUChar; var ec: UErrorCode); cdecl;
function ucal_getDSTSavings(const zoneID: PUChar; var ec: UErrorCode): Int32; cdecl;
function ucal_getNow: UDate; cdecl;
function ucal_open(const zoneID: PUChar; len: Int32; const locale: PAnsiChar; _type: UCalendarType; var status: UErrorCode): PUCalendar; cdecl;
procedure ucal_close(cal: PUCalendar); cdecl;
function ucal_clone(const cal: PUCalendar; var status: UErrorCode): PUCalendar; cdecl;
procedure ucal_setTimeZone(cal: PUCalendar; const zoneID: PUChar; len: Int32; var status: UErrorCode); cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
function ucal_getTimeZoneID(const cal: PUCalendar; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
function ucal_getTimeZoneDisplayName(const cal: PUCalendar; _type: UCalendarDisplayNameType; const locale: PAnsiChar; result: PUChar; resultLength: Int32; var status: UErrorCode): Int32; cdecl;
function ucal_inDaylightTime(const cal: PUCalendar; var status: UErrorCode): UBool; cdecl;
procedure ucal_setGregorianChange(cal: PUCalendar; date: UDate; var pErrorCode: UErrorCode); cdecl;
function ucal_getGregorianChange(const cal: PUCalendar; var pErrorCode: UErrorCode): UDate; cdecl;
function ucal_getAttribute(const cal: PUCalendar; attr: UCalendarAttribute): Int32; cdecl;
procedure ucal_setAttribute(cal: PUCalendar; attr: UCalendarAttribute; newValue: Int32); cdecl;
function ucal_getAvailable(localeIndex: Int32): PAnsiChar; cdecl;
function ucal_countAvailable: Int32; cdecl;
function ucal_getMillis(const cal: PUCalendar; var status: UErrorCode): UDate; cdecl;
procedure ucal_setMillis(cal: PUCalendar; dateTime: UDate; var status: UErrorCode); cdecl;
procedure ucal_setDate(cal: PUCalendar; year, month, date: Int32; var status: UErrorCode); cdecl;
procedure ucal_setDateTime(cal: PUCalendar; year, month, date, hour, minute, second: Int32; var status: UErrorCode); cdecl;
function ucal_equivalentTo(const cal1, cal2: PUCalendar): UBool;
procedure ucal_add(cal: PUCalendar; field: UCalendarDateFields; amount: Int32; var status: UErrorCode); cdecl;
procedure ucal_roll(cal: PUCalendar; field: UCalendarDateFields; amount: Int32; var status: UErrorCode); cdecl;
function ucal_get(const cal: PUCalendar; field: UCalendarDateFields; var status: UErrorCode): Int32; cdecl;
procedure ucal_set(cal: PUCalendar; field: UCalendarDateFields; value: Int32); cdecl;
function ucal_isSet(const cal: PUCalendar; field: UCalendarDateFields): UBool; cdecl;
procedure ucal_clearField(cal: PUCalendar; field: UCalendarDateFields); cdecl;
procedure ucal_clear(calendar: PUCalendar); cdecl;
function ucal_getLimit(const cal: PUCalendar; field: UCalendarDateFields; _type: UCalendarLimitType; var status: UErrorCode): Int32; cdecl;
function ucal_getLocaleByType(const cal: PUCalendar; _type: ULocDataLocaleType; var status: UErrorCode): PAnsiChar; cdecl;
function ucal_getTZDataVersion(var status: UErrorCode): PAnsiChar; cdecl;
function ucal_getCanonicalTimeZoneID(const id: PUChar; len: Int32; result: PUChar; resultCapacity: Int32; var isSystemID: UBool; var status: UErrorCode): Int32; cdecl;
function ucal_getType(const cal: PUCalendar; var status: UErrorCode): PAnsiChar; cdecl;
function ucal_getKeywordValuesForLocale(const key, locale: PAnsiChar; commonlyUsed: UBool; var status: UErrorCode): PUEnumeration; cdecl;
function ucal_getDayOfWeekType(const cal: PUCalendar; dayOfWeek: UCalendarDaysOfWeek; var status: UErrorCode): UCalendarWeekdayType; cdecl;
function ucal_getWeekendTransition(const cal: PUCalendar; dayOfWeek: UCalendarDaysOfWeek; var status: UErrorCode): Int32; cdecl;
function ucal_isWeekend(const cal: PUCalendar; date: UDate; var status: UErrorCode): UBool; cdecl;
function ucal_getFieldDifference(cal: PUCalendar; target: UDate; field: UCalendarDateFields; var status: UErrorCode): Int32; cdecl;
function ucal_getTimeZoneTransitionDate(const cal: PUCalendar; _type: UTimeZoneTransitionType; var transition: UDate; var status: UErrorCode): UBool; cdecl;
{$IFNDEF U_HIDE_DRAFT_API}
function ucal_getWindowsTimeZoneID(const id: PUChar; len: Int32; winid: PUChar; winidCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function ucal_getTimeZoneIDForWindowsID(const winid: PUChar; len: Int32; const region: PAnsiChar; id: PUChar; idCapacity: Int32; var status: UErrorCode): Int32; cdecl;
{$ENDIF ~U_HIDE_DRAFT_API}
{$ENDIF ~ICU_LINKONREQUEST}

const
  ucal_openTimeZoneIDEnumerationDefaultExportName = 'ucal_openTimeZoneIDEnumeration' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_openTimeZonesDefaultExportName = 'ucal_openTimeZones' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_openCountryTimeZonesDefaultExportName = 'ucal_openCountryTimeZones' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getDefaultTimeZoneDefaultExportName = 'ucal_getDefaultTimeZone' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setDefaultTimeZoneDefaultExportName = 'ucal_setDefaultTimeZone' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getDSTSavingsDefaultExportName = 'ucal_getDSTSavings' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getNowDefaultExportName = 'ucal_getNow' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_openDefaultExportName = 'ucal_open' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_closeDefaultExportName = 'ucal_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_cloneDefaultExportName = 'ucal_clone' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setTimeZoneDefaultExportName = 'ucal_setTimeZone' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFNDEF U_HIDE_DRAFT_API}
  ucal_getTimeZoneIDDefaultExportName = 'ucal_getTimeZoneID' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_DRAFT_API}
  ucal_getTimeZoneDisplayNameDefaultExportName = 'ucal_getTimeZoneDisplayName' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_inDaylightTimeDefaultExportName = 'ucal_inDaylightTime' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setGregorianChangeDefaultExportName = 'ucal_setGregorianChange' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getGregorianChangeDefaultExportName = 'ucal_getGregorianChange' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getAttributeDefaultExportName = 'ucal_getAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setAttributeDefaultExportName = 'ucal_setAttribute' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getAvailableDefaultExportName = 'ucal_getAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_countAvailableDefaultExportName = 'ucal_countAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getMillisDefaultExportName = 'ucal_getMillis' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setMillisDefaultExportName = 'ucal_setMillis' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setDateDefaultExportName = 'ucal_setDate' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setDateTimeDefaultExportName = 'ucal_setDateTime' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_equivalentToDefaultExportName = 'ucal_equivalentTo' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_addDefaultExportName = 'ucal_add' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_rollDefaultExportName = 'ucal_roll' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getDefaultExportName = 'ucal_get' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_setDefaultExportName = 'ucal_set' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_isSetDefaultExportName = 'ucal_isSet' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_clearFieldDefaultExportName = 'ucal_clearField' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_clearDefaultExportName = 'ucal_clear' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getLimitDefaultExportName = 'ucal_getLimit' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getLocaleByTypeDefaultExportName = 'ucal_getLocaleByType' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getTZDataVersionDefaultExportName = 'ucal_getTZDataVersion' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getCanonicalTimeZoneIDDefaultExportName = 'ucal_getCanonicalTimeZoneID' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getTypeDefaultExportName = 'ucal_getType' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getKeywordValuesForLocaleDefaultExportName = 'ucal_getKeywordValuesForLocale' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getDayOfWeekTypeDefaultExportName = 'ucal_getDayOfWeekType' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getWeekendTransitionDefaultExportName = 'ucal_getWeekendTransition' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_isWeekendDefaultExportName = 'ucal_isWeekend' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getFieldDifferenceDefaultExportName = 'ucal_getFieldDifference' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getTimeZoneTransitionDateDefaultExportName = 'ucal_getTimeZoneTransitionDate' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFNDEF U_HIDE_DRAFT_API}
  ucal_getWindowsTimeZoneIDDefaultExportName = 'ucal_getWindowsTimeZoneID' + ICU_DEFAULT_EXPORT_SUFFIX;
  ucal_getTimeZoneIDForWindowsIDDefaultExportName = 'ucal_getTimeZoneIDForWindowsID' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_DRAFT_API}
{$IFDEF ICU_LINKONREQUEST}
var
  ucal_openTimeZoneIDEnumerationExportName: string = ucal_openTimeZoneIDEnumerationDefaultExportName;
  ucal_openTimeZonesExportName: string = ucal_openTimeZonesDefaultExportName;
  ucal_openCountryTimeZonesExportName: string = ucal_openCountryTimeZonesDefaultExportName;
  ucal_getDefaultTimeZoneExportName: string = ucal_getDefaultTimeZoneDefaultExportName;
  ucal_setDefaultTimeZoneExportName: string = ucal_setDefaultTimeZoneDefaultExportName;
  ucal_getDSTSavingsExportName: string = ucal_getDSTSavingsDefaultExportName;
  ucal_getNowExportName: string = ucal_getNowDefaultExportName;
  ucal_openExportName: string = ucal_openDefaultExportName;
  ucal_closeExportName: string = ucal_closeDefaultExportName;
  ucal_cloneExportName: string = ucal_cloneDefaultExportName;
  ucal_setTimeZoneExportName: string = ucal_setTimeZoneDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
  ucal_getTimeZoneIDExportName: string = ucal_getTimeZoneIDDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
  ucal_getTimeZoneDisplayNameExportName: string = ucal_getTimeZoneDisplayNameDefaultExportName;
  ucal_inDaylightTimeExportName: string = ucal_inDaylightTimeDefaultExportName;
  ucal_setGregorianChangeExportName: string = ucal_setGregorianChangeDefaultExportName;
  ucal_getGregorianChangeExportName: string = ucal_getGregorianChangeDefaultExportName;
  ucal_getAttributeExportName: string = ucal_getAttributeDefaultExportName;
  ucal_setAttributeExportName: string = ucal_setAttributeDefaultExportName;
  ucal_getAvailableExportName: string = ucal_getAvailableDefaultExportName;
  ucal_countAvailableExportName: string = ucal_countAvailableDefaultExportName;
  ucal_getMillisExportName: string = ucal_getMillisDefaultExportName;
  ucal_setMillisExportName: string = ucal_setMillisDefaultExportName;
  ucal_setDateExportName: string = ucal_setDateDefaultExportName;
  ucal_setDateTimeExportName: string = ucal_setDateTimeDefaultExportName;
  ucal_equivalentToExportName: string = ucal_equivalentToDefaultExportName;
  ucal_addExportName: string = ucal_addDefaultExportName;
  ucal_rollExportName: string = ucal_rollDefaultExportName;
  ucal_getExportName: string = ucal_getDefaultExportName;
  ucal_setExportName: string = ucal_setDefaultExportName;
  ucal_isSetExportName: string = ucal_isSetDefaultExportName;
  ucal_clearFieldExportName: string = ucal_clearFieldDefaultExportName;
  ucal_clearExportName: string = ucal_clearDefaultExportName;
  ucal_getLimitExportName: string = ucal_getLimitDefaultExportName;
  ucal_getLocaleByTypeExportName: string = ucal_getLocaleByTypeDefaultExportName;
  ucal_getTZDataVersionExportName: string = ucal_getTZDataVersionDefaultExportName;
  ucal_getCanonicalTimeZoneIDExportName: string = ucal_getCanonicalTimeZoneIDDefaultExportName;
  ucal_getTypeExportName: string = ucal_getTypeDefaultExportName;
  ucal_getKeywordValuesForLocaleExportName: string = ucal_getKeywordValuesForLocaleDefaultExportName;
  ucal_getDayOfWeekTypeExportName: string = ucal_getDayOfWeekTypeDefaultExportName;
  ucal_getWeekendTransitionExportName: string = ucal_getWeekendTransitionDefaultExportName;
  ucal_isWeekendExportName: string = ucal_isWeekendDefaultExportName;
  ucal_getFieldDifferenceExportName: string = ucal_getFieldDifferenceDefaultExportName;
  ucal_getTimeZoneTransitionDateExportName: string = ucal_getTimeZoneTransitionDateDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
  ucal_getWindowsTimeZoneIDExportName: string = ucal_getWindowsTimeZoneIDDefaultExportName;
  ucal_getTimeZoneIDForWindowsIDExportName: string = ucal_getTimeZoneIDForWindowsIDDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
{$ENDIF ~ICU_LINKONREQUEST}

implementation

uses
  JclSysUtils;

{$IFNDEF ICU_LINKONREQUEST}
function ucal_openTimeZoneIDEnumeration; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_openTimeZoneIDEnumerationDefaultExportName;
function ucal_openTimeZones; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_openTimeZonesDefaultExportName;
function ucal_openCountryTimeZones; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_openCountryTimeZonesDefaultExportName;
function ucal_getDefaultTimeZone; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getDefaultTimeZoneDefaultExportName;
procedure ucal_setDefaultTimeZone; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setDefaultTimeZoneDefaultExportName;
function ucal_getDSTSavings; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getDSTSavingsDefaultExportName;
function ucal_getNow; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getNowDefaultExportName;
function ucal_open; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_openDefaultExportName;
procedure ucal_close; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_closeDefaultExportName;
function ucal_clone; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_cloneDefaultExportName;
procedure ucal_setTimeZone; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setTimeZoneDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
function ucal_getTimeZoneID; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getTimeZoneIDDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
function ucal_getTimeZoneDisplayName; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getTimeZoneDisplayNameDefaultExportName;
function ucal_inDaylightTime; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_inDaylightTimeDefaultExportName;
procedure ucal_setGregorianChange; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setGregorianChangeDefaultExportName;
function ucal_getGregorianChange; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getGregorianChangeDefaultExportName;
function ucal_getAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getAttributeDefaultExportName;
procedure ucal_setAttribute; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setAttributeDefaultExportName;
function ucal_getAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getAvailableDefaultExportName;
function ucal_countAvailable; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_countAvailableDefaultExportName;
function ucal_getMillis; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getMillisDefaultExportName;
procedure ucal_setMillis; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setMillisDefaultExportName;
procedure ucal_setDate; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setDateDefaultExportName;
procedure ucal_setDateTime; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setDateTimeDefaultExportName;
function ucal_equivalentTo; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_equivalentToDefaultExportName;
procedure ucal_add; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_addDefaultExportName;
procedure ucal_roll; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_rollDefaultExportName;
function ucal_get; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getDefaultExportName;
procedure ucal_set; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_setDefaultExportName;
function ucal_isSet; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_isSetDefaultExportName;
procedure ucal_clearField; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_clearFieldDefaultExportName;
procedure ucal_clear; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_clearDefaultExportName;
function ucal_getLimit; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getLimitDefaultExportName;
function ucal_getLocaleByType; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getLocaleByTypeDefaultExportName;
function ucal_getTZDataVersion; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getTZDataVersionDefaultExportName;
function ucal_getCanonicalTimeZoneID; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getCanonicalTimeZoneIDDefaultExportName;
function ucal_getType; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getTypeDefaultExportName;
function ucal_getKeywordValuesForLocale; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getKeywordValuesForLocaleDefaultExportName;
function ucal_getDayOfWeekType; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getDayOfWeekTypeDefaultExportName;
function ucal_getWeekendTransition; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getWeekendTransitionDefaultExportName;
function ucal_isWeekend; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_isWeekendDefaultExportName;
function ucal_getFieldDifference; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getFieldDifferenceDefaultExportName;
function ucal_getTimeZoneTransitionDate; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getTimeZoneTransitionDateDefaultExportName;
{$IFNDEF U_HIDE_DRAFT_API}
function ucal_getWindowsTimeZoneID; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getWindowsTimeZoneIDDefaultExportName;
function ucal_getTimeZoneIDForWindowsID; external ICU_DEFAULT_I18N_MODULE_NAME name ucal_getTimeZoneIDForWindowsIDDefaultExportName;
{$ENDIF ~U_HIDE_DRAFT_API}
{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @ucal_openTimeZoneIDEnumeration := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_openTimeZoneIDEnumerationExportName);
  @ucal_openTimeZones := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_openTimeZonesExportName);
  @ucal_openCountryTimeZones := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_openCountryTimeZonesExportName);
  @ucal_getDefaultTimeZone := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getDefaultTimeZoneExportName);
  @ucal_setDefaultTimeZone := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setDefaultTimeZoneExportName);
  @ucal_getDSTSavings := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getDSTSavingsExportName);
  @ucal_getNow := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getNowExportName);
  @ucal_open := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_openExportName);
  @ucal_close := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_closeExportName);
  @ucal_clone := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_cloneExportName);
  @ucal_setTimeZone := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setTimeZoneExportName);
{$IFNDEF U_HIDE_DRAFT_API}
  @ucal_getTimeZoneID := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getTimeZoneIDExportName);
{$ENDIF ~U_HIDE_DRAFT_API}
  @ucal_getTimeZoneDisplayName := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getTimeZoneDisplayNameExportName);
  @ucal_inDaylightTime := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_inDaylightTimeExportName);
  @ucal_setGregorianChange := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setGregorianChangeExportName);
  @ucal_getGregorianChange := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getGregorianChangeExportName);
  @ucal_getAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getAttributeExportName);
  @ucal_setAttribute := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setAttributeExportName);
  @ucal_getAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getAvailableExportName);
  @ucal_countAvailable := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_countAvailableExportName);
  @ucal_getMillis := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getMillisExportName);
  @ucal_setMillis := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setMillisExportName);
  @ucal_setDate := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setDateExportName);
  @ucal_setDateTime := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setDateTimeExportName);
  @ucal_equivalentTo := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_equivalentToExportName);
  @ucal_add := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_addExportName);
  @ucal_roll := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_rollExportName);
  @ucal_get := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getExportName);
  @ucal_set := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_setExportName);
  @ucal_isSet := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_isSetExportName);
  @ucal_clearField := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_clearFieldExportName);
  @ucal_clear := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_clearExportName);
  @ucal_getLimit := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getLimitExportName);
  @ucal_getLocaleByType := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getLocaleByTypeExportName);
  @ucal_getTZDataVersion := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getTZDataVersionExportName);
  @ucal_getCanonicalTimeZoneID := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getCanonicalTimeZoneIDExportName);
  @ucal_getType := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getTypeExportName);
  @ucal_getKeywordValuesForLocale := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getKeywordValuesForLocaleExportName);
  @ucal_getDayOfWeekType := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getDayOfWeekTypeExportName);
  @ucal_getWeekendTransition := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getWeekendTransitionExportName);
  @ucal_isWeekend := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_isWeekendExportName);
  @ucal_getFieldDifference := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getFieldDifferenceExportName);
  @ucal_getTimeZoneTransitionDate := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getTimeZoneTransitionDateExportName);
{$IFNDEF U_HIDE_DRAFT_API}
  @ucal_getWindowsTimeZoneID := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getWindowsTimeZoneIDExportName);
  @ucal_getTimeZoneIDForWindowsID := GetModuleSymbol(ICU_I18N_LibraryHandle, ucal_getTimeZoneIDForWindowsIDExportName);
{$ENDIF ~U_HIDE_DRAFT_API}

  Result :=
    Assigned(@ucal_openTimeZoneIDEnumeration) and Assigned(@ucal_openTimeZones) and Assigned(@ucal_openCountryTimeZones) and Assigned(@ucal_getDefaultTimeZone)
    and Assigned(@ucal_setDefaultTimeZone) and Assigned(@ucal_getDSTSavings) and Assigned(@ucal_getNow) and Assigned(@ucal_open) and Assigned(@ucal_close) and
    Assigned(@ucal_clone) and Assigned(@ucal_setTimeZone) and
{$IFNDEF U_HIDE_DRAFT_API}
    Assigned(@ucal_getTimeZoneID) and
{$ENDIF ~U_HIDE_DRAFT_API}
    Assigned(@ucal_getTimeZoneDisplayName) and Assigned(@ucal_inDaylightTime) and Assigned(@ucal_setGregorianChange) and Assigned(@ucal_getGregorianChange) and
    Assigned(@ucal_getAttribute) and Assigned(@ucal_setAttribute) and Assigned(@ucal_getAvailable) and Assigned(@ucal_countAvailable) and
    Assigned(@ucal_getMillis) and Assigned(@ucal_setMillis) and Assigned(@ucal_setDate) and Assigned(@ucal_setDateTime) and Assigned(@ucal_equivalentTo) and
    Assigned(@ucal_add) and Assigned(@ucal_roll) and Assigned(@ucal_get) and Assigned(@ucal_set) and Assigned(@ucal_isSet) and Assigned(@ucal_clearField) and
    Assigned(@ucal_clear) and Assigned(@ucal_getLimit) and Assigned(@ucal_getLocaleByType) and Assigned(@ucal_getTZDataVersion) and
    Assigned(@ucal_getCanonicalTimeZoneID) and Assigned(@ucal_getType) and Assigned(@ucal_getKeywordValuesForLocale) and Assigned(@ucal_getDayOfWeekType) and
    Assigned(@ucal_getWeekendTransition) and Assigned(@ucal_isWeekend) and Assigned(@ucal_getFieldDifference) and Assigned(@ucal_getTimeZoneTransitionDate)
{$IFNDEF U_HIDE_DRAFT_API}
    and Assigned(@ucal_getWindowsTimeZoneID) and Assigned(@ucal_getTimeZoneIDForWindowsID)
{$ENDIF ~U_HIDE_DRAFT_API}
    ;
end;

procedure UnloadICU;
begin
  @ucal_openTimeZoneIDEnumeration := nil;
  @ucal_openTimeZones := nil;
  @ucal_openCountryTimeZones := nil;
  @ucal_getDefaultTimeZone := nil;
  @ucal_setDefaultTimeZone := nil;
  @ucal_getDSTSavings := nil;
  @ucal_getNow := nil;
  @ucal_open := nil;
  @ucal_close := nil;
  @ucal_clone := nil;
  @ucal_setTimeZone := nil;
{$IFNDEF U_HIDE_DRAFT_API}
  @ucal_getTimeZoneID := nil;
{$ENDIF ~U_HIDE_DRAFT_API}
  @ucal_getTimeZoneDisplayName := nil;
  @ucal_inDaylightTime := nil;
  @ucal_setGregorianChange := nil;
  @ucal_getGregorianChange := nil;
  @ucal_getAttribute := nil;
  @ucal_setAttribute := nil;
  @ucal_getAvailable := nil;
  @ucal_countAvailable := nil;
  @ucal_getMillis := nil;
  @ucal_setMillis := nil;
  @ucal_setDate := nil;
  @ucal_setDateTime := nil;
  @ucal_equivalentTo := nil;
  @ucal_add := nil;
  @ucal_roll := nil;
  @ucal_get := nil;
  @ucal_set := nil;
  @ucal_isSet := nil;
  @ucal_clearField := nil;
  @ucal_clear := nil;
  @ucal_getLimit := nil;
  @ucal_getLocaleByType := nil;
  @ucal_getTZDataVersion := nil;
  @ucal_getCanonicalTimeZoneID := nil;
  @ucal_getType := nil;
  @ucal_getKeywordValuesForLocale := nil;
  @ucal_getDayOfWeekType := nil;
  @ucal_getWeekendTransition := nil;
  @ucal_isWeekend := nil;
  @ucal_getFieldDifference := nil;
  @ucal_getTimeZoneTransitionDate := nil;
{$IFNDEF U_HIDE_DRAFT_API}
  @ucal_getWindowsTimeZoneID := nil;
  @ucal_getTimeZoneIDForWindowsID := nil;
{$ENDIF ~U_HIDE_DRAFT_API}
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
