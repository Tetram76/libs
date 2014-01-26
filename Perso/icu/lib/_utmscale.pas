unit _utmscale;

interface

type
  UDateTimeScale = (
    UDTS_JAVA_TIME = 0,
    UDTS_UNIX_TIME,
    UDTS_ICU4C_TIME,
    UDTS_WINDOWS_FILE_TIME,
    UDTS_DOTNET_DATE_TIME,
    UDTS_MAC_OLD_TIME,
    UDTS_MAC_TIME,
    UDTS_EXCEL_TIME,
    UDTS_DB2_TIME,
    UDTS_UNIX_MICROSECONDS_TIME,
    UDTS_MAX_SCALE
  );

  UTimeScaleValue = (
    UTSV_UNITS_VALUE = 0,
    UTSV_EPOCH_OFFSET_VALUE = 1,
    UTSV_FROM_MIN_VALUE = 2,
    UTSV_FROM_MAX_VALUE = 3,
    UTSV_TO_MIN_VALUE = 4,
    UTSV_TO_MAX_VALUE = 5,
(*
#ifndef U_HIDE_INTERNAL_API
    UTSV_EPOCH_OFFSET_PLUS_1_VALUE=6,
    UTSV_EPOCH_OFFSET_MINUS_1_VALUE=7,
    UTSV_UNITS_ROUND_VALUE=8,
    UTSV_MIN_ROUND_VALUE=9,
    UTSV_MAX_ROUND_VALUE=10,
#endif /* U_HIDE_INTERNAL_API */
*)
    UTSV_MAX_SCALE_VALUE = 11
  );

(*
// Get a value associated with a particular time scale.
int64_t 	utmscale_getTimeScaleValue (UDateTimeScale timeScale, UTimeScaleValue value, UErrorCode *status)
// Convert a int64_t datetime from the given time scale to the universal time scale.
int64_t 	utmscale_fromInt64 (int64_t otherTime, UDateTimeScale timeScale, UErrorCode *status)
// Convert a datetime from the universal time scale to a int64_t in the given time scale.
int64_t 	utmscale_toInt64 (int64_t universalTime, UDateTimeScale timeScale, UErrorCode *status)
*)
implementation

end.
