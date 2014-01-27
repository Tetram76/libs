unit _utmscale;

{$I icu.inc}

interface

uses
  _utypes, icu_globals;

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
{$IFNDEF U_HIDE_INTERNAL_API}
    UTSV_EPOCH_OFFSET_PLUS_1_VALUE = 6,
    UTSV_EPOCH_OFFSET_MINUS_1_VALUE = 7,
    UTSV_UNITS_ROUND_VALUE = 8,
    UTSV_MIN_ROUND_VALUE = 9,
    UTSV_MAX_ROUND_VALUE = 10,
{$ENDIF ~U_HIDE_INTERNAL_API}
    UTSV_MAX_SCALE_VALUE = 11
  );


{$IFDEF ICU_LINKONREQUEST}

type
  // Get a value associated with a particular time scale.
  Tutmscale_getTimeScaleValue = function(timeScale: UDateTimeScale; value: UTimeScaleValue; var status: UErrorCode): Int64; cdecl;
  // Convert a int64_t datetime from the given time scale to the universal time scale.
  Tutmscale_fromInt64 = function(otherTime: Int64; timeScale: UDateTimeScale; var status: UErrorCode): Int64; cdecl;
  // Convert a datetime from the universal time scale to a int64_t in the given time scale.
  Tutmscale_toInt64 = function(universalTime: Int64; timeScale: UDateTimeScale; var status: UErrorCode): Int64; cdecl;

var
  utmscale_getTimeScaleValue: Tutmscale_getTimeScaleValue = nil;
  utmscale_fromInt64: Tutmscale_fromInt64 = nil;
  utmscale_toInt64: Tutmscale_toInt64 = nil;
{$ELSE ~ICU_LINKONREQUEST}
function utmscale_getTimeScaleValue(timeScale: UDateTimeScale; value: UTimeScaleValue; var status: UErrorCode): Int64; cdecl;
function utmscale_fromInt64(otherTime: Int64; timeScale: UDateTimeScale; var status: UErrorCode): Int64; cdecl;
function utmscale_toInt64(universalTime: Int64; timeScale: UDateTimeScale; var status: UErrorCode): Int64; cdecl;
{$ENDIF ~ICU_LINKONREQUEST}

const
  utmscale_getTimeScaleValueDefaultExportName = 'utmscale_getTimeScaleValue' + ICU_DEFAULT_EXPORT_SUFFIX;
  utmscale_fromInt64DefaultExportName = 'utmscale_fromInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
  utmscale_toInt64DefaultExportName = 'utmscale_toInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFDEF ICU_LINKONREQUEST}

var
  utmscale_getTimeScaleValueExportName: string = utmscale_getTimeScaleValueDefaultExportName;
  utmscale_fromInt64ExportName: string = utmscale_fromInt64DefaultExportName;
  utmscale_toInt64ExportName: string = utmscale_toInt64DefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

implementation

uses
  JclSysUtils;

{$IFNDEF ICU_LINKONREQUEST}
function utmscale_getTimeScaleValue; external ICU_DEFAULT_I18N_MODULE_NAME name utmscale_getTimeScaleValueDefaultExportName;
function utmscale_fromInt64; external ICU_DEFAULT_I18N_MODULE_NAME name utmscale_fromInt64DefaultExportName;
function utmscale_toInt64; external ICU_DEFAULT_I18N_MODULE_NAME name utmscale_toInt64DefaultExportName;
{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @utmscale_getTimeScaleValue := GetModuleSymbol(ICU_I18N_LibraryHandle, utmscale_getTimeScaleValueExportName);
  @utmscale_fromInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, utmscale_fromInt64ExportName);
  @utmscale_toInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, utmscale_toInt64ExportName);

  Result := Assigned(@utmscale_getTimeScaleValue) and Assigned(@utmscale_fromInt64) and Assigned(@utmscale_toInt64);
end;

procedure UnloadICU;
begin
  @utmscale_getTimeScaleValue := nil;
  @utmscale_fromInt64 := nil;
  @utmscale_toInt64 := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
