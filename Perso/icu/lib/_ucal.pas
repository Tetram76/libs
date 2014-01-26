unit _ucal;

{$I icu.inc}

interface

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

(*
// Create an enumeration over system time zone IDs with the given filter conditions.
UEnumeration * 	ucal_openTimeZoneIDEnumeration (USystemTimeZoneType zoneType, const char *region, const int32_t *rawOffset, UErrorCode *ec)
// Create an enumeration over all time zones.
UEnumeration * 	ucal_openTimeZones (UErrorCode *ec)
// Create an enumeration over all time zones associated with the given country.
UEnumeration * 	ucal_openCountryTimeZones (const char *country, UErrorCode *ec)
// Return the default time zone.
int32_t 	ucal_getDefaultTimeZone (UChar *result, int32_t resultCapacity, UErrorCode *ec)
// Set the default time zone.
void 	ucal_setDefaultTimeZone (const UChar *zoneID, UErrorCode *ec)
// Return the amount of time in milliseconds that the clock is advanced during daylight savings time for the given time zone, or zero if the time zone does not observe daylight savings time.
int32_t 	ucal_getDSTSavings (const UChar *zoneID, UErrorCode *ec)
// Get the current date and time.
UDate 	ucal_getNow (void)
// Open a UCalendar.
UCalendar * 	ucal_open (const UChar *zoneID, int32_t len, const char *locale, UCalendarType type, UErrorCode *status)
// Close a UCalendar.
void 	ucal_close (UCalendar *cal)
// Open a copy of a UCalendar.
UCalendar * 	ucal_clone (const UCalendar *cal, UErrorCode *status)
// Set the TimeZone used by a UCalendar.
void 	ucal_setTimeZone (UCalendar *cal, const UChar *zoneID, int32_t len, UErrorCode *status)
// Get the ID of the UCalendar's time zone.
int32_t 	ucal_getTimeZoneID (const UCalendar *cal, UChar *result, int32_t resultLength, UErrorCode *status)
// Get the display name for a UCalendar's TimeZone.
int32_t 	ucal_getTimeZoneDisplayName (const UCalendar *cal, UCalendarDisplayNameType type, const char *locale, UChar *result, int32_t resultLength, UErrorCode *status)
// Determine if a UCalendar is currently in daylight savings time.
UBool 	ucal_inDaylightTime (const UCalendar *cal, UErrorCode *status)
// Sets the GregorianCalendar change date.
void 	ucal_setGregorianChange (UCalendar *cal, UDate date, UErrorCode *pErrorCode)
// Gets the Gregorian Calendar change date.
UDate 	ucal_getGregorianChange (const UCalendar *cal, UErrorCode *pErrorCode)
// Get a numeric attribute associated with a UCalendar.
int32_t 	ucal_getAttribute (const UCalendar *cal, UCalendarAttribute attr)
// Set a numeric attribute associated with a UCalendar.
void 	ucal_setAttribute (UCalendar *cal, UCalendarAttribute attr, int32_t newValue)
// Get a locale for which calendars are available.
const char * 	ucal_getAvailable (int32_t localeIndex)
// Determine how many locales have calendars available.
int32_t 	ucal_countAvailable (void)
// Get a UCalendar's current time in millis.
UDate 	ucal_getMillis (const UCalendar *cal, UErrorCode *status)
// Set a UCalendar's current time in millis.
void 	ucal_setMillis (UCalendar *cal, UDate dateTime, UErrorCode *status)
// Set a UCalendar's current date.
void 	ucal_setDate (UCalendar *cal, int32_t year, int32_t month, int32_t date, UErrorCode *status)
// Set a UCalendar's current date.
void 	ucal_setDateTime (UCalendar *cal, int32_t year, int32_t month, int32_t date, int32_t hour, int32_t minute, int32_t second, UErrorCode *status)
// Returns TRUE if two UCalendars are equivalent.
UBool 	ucal_equivalentTo (const UCalendar *cal1, const UCalendar *cal2)
// Add a specified signed amount to a particular field in a UCalendar.
void 	ucal_add (UCalendar *cal, UCalendarDateFields field, int32_t amount, UErrorCode *status)
// Add a specified signed amount to a particular field in a UCalendar.
void 	ucal_roll (UCalendar *cal, UCalendarDateFields field, int32_t amount, UErrorCode *status)
// Get the current value of a field from a UCalendar.
int32_t 	ucal_get (const UCalendar *cal, UCalendarDateFields field, UErrorCode *status)
// Set the value of a field in a UCalendar.
void 	ucal_set (UCalendar *cal, UCalendarDateFields field, int32_t value)
// Determine if a field in a UCalendar is set.
UBool 	ucal_isSet (const UCalendar *cal, UCalendarDateFields field)
// Clear a field in a UCalendar.
void 	ucal_clearField (UCalendar *cal, UCalendarDateFields field)
// Clear all fields in a UCalendar.
void 	ucal_clear (UCalendar *calendar)
// Determine a limit for a field in a UCalendar.
int32_t 	ucal_getLimit (const UCalendar *cal, UCalendarDateFields field, UCalendarLimitType type, UErrorCode *status)
// Get the locale for this calendar object.
const char * 	ucal_getLocaleByType (const UCalendar *cal, ULocDataLocaleType type, UErrorCode *status)
// Returns the timezone data version currently used by ICU.
const char * 	ucal_getTZDataVersion (UErrorCode *status)
// Returns the canonical system timezone ID or the normalized custom time zone ID for the given time zone ID.
int32_t 	ucal_getCanonicalTimeZoneID (const UChar *id, int32_t len, UChar *result, int32_t resultCapacity, UBool *isSystemID, UErrorCode *status)
// Get the resource keyword value string designating the calendar type for the UCalendar.
const char * 	ucal_getType (const UCalendar *cal, UErrorCode *status)
// Given a key and a locale, returns an array of string values in a preferred order that would make a difference.
UEnumeration * 	ucal_getKeywordValuesForLocale (const char *key, const char *locale, UBool commonlyUsed, UErrorCode *status)
// Returns whether the given day of the week is a weekday, a weekend day, or a day that transitions from one to the other, for the locale and calendar system associated with this UCalendar (the locale's region is often the most determinant factor).
UCalendarWeekdayType 	ucal_getDayOfWeekType (const UCalendar *cal, UCalendarDaysOfWeek dayOfWeek, UErrorCode *status)
// Returns the time during the day at which the weekend begins or ends in this calendar system.
int32_t 	ucal_getWeekendTransition (const UCalendar *cal, UCalendarDaysOfWeek dayOfWeek, UErrorCode *status)
// Returns TRUE if the given UDate is in the weekend in this calendar system.
UBool 	ucal_isWeekend (const UCalendar *cal, UDate date, UErrorCode *status)
// Return the difference between the target time and the time this calendar object is currently set to.
int32_t 	ucal_getFieldDifference (UCalendar *cal, UDate target, UCalendarDateFields field, UErrorCode *status)
// Get the UDate for the next/previous time zone transition relative to the calendar's current date, in the time zone to which the calendar is currently set.
UBool 	ucal_getTimeZoneTransitionDate (const UCalendar *cal, UTimeZoneTransitionType type, UDate *transition, UErrorCode *status)
// Converts a system time zone ID to an equivalent Windows time zone ID.
int32_t 	ucal_getWindowsTimeZoneID (const UChar *id, int32_t len, UChar *winid, int32_t winidCapacity, UErrorCode *status)
// Converts a Windows time zone ID to an equivalent system time zone ID for a region.
int32_t 	ucal_getTimeZoneIDForWindowsID (const UChar *winid, int32_t len, const char *region, UChar *id, int32_t idCapacity, UErrorCode *status)
*)
implementation

end.
