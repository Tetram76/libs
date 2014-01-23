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

implementation

end.
