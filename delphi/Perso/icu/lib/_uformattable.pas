unit _uformattable;

{$I icu.inc}

interface

uses
  _utypes, _umachine, icu_globals;
{$IFNDEF U_HIDE_DRAFT_API}

type
  PUFormattable = ^UFormattable;
  UFormattable = packed record
  end;

 	// Enum designating the type of a UFormattable instance.
  UFormattableType = (
    UFMT_DATE = 0,
    UFMT_DOUBLE,
    UFMT_LONG,
    UFMT_STRING,
    UFMT_ARRAY,
    UFMT_INT64,
    UFMT_OBJECT,
    UFMT_COUNT
  );

{$IFDEF ICU_LINKONREQUEST}

type
  // Initialize a UFormattable, to type UNUM_LONG, value 0 may return error if memory allocation failed.
  Tufmt_open = function(var status: UErrorCode): PUFormattable; cdecl;
  // Cleanup any additional memory allocated by this UFormattable.
  Tufmt_close = procedure(fmt: PUFormattable); cdecl;
  // Return the type of this object.
  Tufmt_getType = function(const fmt: PUFormattable; var status: UErrorCode): UFormattableType; cdecl;
  // Return whether the object is numeric.
  Tufmt_isNumeric = function(const fmt: PUFormattable): UBool; cdecl;
  // Gets the UDate value of this object.
  Tufmt_getDate = function(const fmt: PUFormattable; var status: UErrorCode): UDate; cdecl;
  // Gets the double value of this object.
  Tufmt_getDouble = function(fmt: PUFormattable; var status: UErrorCode): double; cdecl;
  // Gets the long (int32_t) value of this object.
  Tufmt_getLong = function(fmt: PUFormattable; var status: UErrorCode): Int32; cdecl;
  // Gets the int64_t value of this object.
  Tufmt_getInt64 = function(fmt: PUFormattable; var status: UErrorCode): Int64; cdecl;
  // Returns a pointer to the UObject contained within this formattable (as a const void* ), or NULL if this object is not of type UFMT_OBJECT.
  Tufmt_getObject = function(const fmt: PUFormattable; var status: UErrorCode): Pointer; cdecl;
  // Gets the string value of this object as a UChar string.
  Tufmt_getUChars = function(fmt: PUFormattable; var len: Int32; var status: UErrorCode): PUChar; cdecl;
  // Get the number of array objects contained, if an array type UFMT_ARRAY.
  Tufmt_getArrayLength = function(const fmt: PUFormattable; var status: UErrorCode): Int32; cdecl;
  // Get the specified value from the array of UFormattables.
  Tufmt_getArrayItemByIndex = function(fmt: PUFormattable; n: Int32; var status: UErrorCode): PUFormattable; cdecl;
  // Returns a numeric string representation of the number contained within this formattable, or NULL if this object does not contain numeric type.
  Tufmt_getDecNumChars = function(fmt: PUFormattable; var len: Int32; var status: UErrorCode): PAnsiChar; cdecl;

var
  ufmt_open: Tufmt_open = nil;
  ufmt_close: Tufmt_close = nil;
  ufmt_getType: Tufmt_getType = nil;
  ufmt_isNumeric: Tufmt_isNumeric = nil;
  ufmt_getDate: Tufmt_getDate = nil;
  ufmt_getDouble: Tufmt_getDouble = nil;
  ufmt_getLong: Tufmt_getLong = nil;
  ufmt_getInt64: Tufmt_getInt64 = nil;
  ufmt_getObject: Tufmt_getObject = nil;
  ufmt_getUChars: Tufmt_getUChars = nil;
  ufmt_getArrayLength: Tufmt_getArrayLength = nil;
  ufmt_getArrayItemByIndex: Tufmt_getArrayItemByIndex = nil;
  ufmt_getDecNumChars: Tufmt_getDecNumChars = nil;
{$ELSE ~ICU_LINKONREQUEST}
function ufmt_open(var status: UErrorCode): PUFormattable; cdecl;
procedure ufmt_close(fmt: PUFormattable); cdecl;
function ufmt_getType(const fmt: PUFormattable; var status: UErrorCode): UFormattableType; cdecl;
function ufmt_isNumeric(const fmt: PUFormattable): UBool; cdecl;
function ufmt_getDate(const fmt: PUFormattable; var status: UErrorCode): UDate; cdecl;
function ufmt_getDouble(fmt: PUFormattable; var status: UErrorCode): double; cdecl;
function ufmt_getLong(fmt: PUFormattable; var status: UErrorCode): Int32; cdecl;
function ufmt_getInt64(fmt: PUFormattable; var status: UErrorCode): Int64; cdecl;
function ufmt_getObject(const fmt: PUFormattable; var status: UErrorCode): Pointer; cdecl;
function ufmt_getUChars(fmt: PUFormattable; var len: Int32; var status: UErrorCode): PUChar; cdecl;
function ufmt_getArrayLength(const fmt: PUFormattable; var status: UErrorCode): Int32; cdecl;
function ufmt_getArrayItemByIndex(fmt: PUFormattable; n: Int32; var status: UErrorCode): PUFormattable; cdecl;
function ufmt_getDecNumChars(fmt: PUFormattable; var len: Int32; var status: UErrorCode): PAnsiChar; cdecl;
{$ENDIF ~ICU_LINKONREQUEST}

const
  ufmt_openDefaultExportName = 'ufmt_open' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_closeDefaultExportName = 'ufmt_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getTypeDefaultExportName = 'ufmt_getType' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_isNumericDefaultExportName = 'ufmt_isNumeric' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getDateDefaultExportName = 'ufmt_getDate' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getDoubleDefaultExportName = 'ufmt_getDouble' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getLongDefaultExportName = 'ufmt_getLong' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getInt64DefaultExportName = 'ufmt_getInt64' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getObjectDefaultExportName = 'ufmt_getObject' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getUCharsDefaultExportName = 'ufmt_getUChars' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getArrayLengthDefaultExportName = 'ufmt_getArrayLength' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getArrayItemByIndexDefaultExportName = 'ufmt_getArrayItemByIndex' + ICU_DEFAULT_EXPORT_SUFFIX;
  ufmt_getDecNumCharsDefaultExportName = 'ufmt_getDecNumChars' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFDEF ICU_LINKONREQUEST}

var
  ufmt_openExportName: string = ufmt_openDefaultExportName;
  ufmt_closeExportName: string = ufmt_closeDefaultExportName;
  ufmt_getTypeExportName: string = ufmt_getTypeDefaultExportName;
  ufmt_isNumericExportName: string = ufmt_isNumericDefaultExportName;
  ufmt_getDateExportName: string = ufmt_getDateDefaultExportName;
  ufmt_getDoubleExportName: string = ufmt_getDoubleDefaultExportName;
  ufmt_getLongExportName: string = ufmt_getLongDefaultExportName;
  ufmt_getInt64ExportName: string = ufmt_getInt64DefaultExportName;
  ufmt_getObjectExportName: string = ufmt_getObjectDefaultExportName;
  ufmt_getUCharsExportName: string = ufmt_getUCharsDefaultExportName;
  ufmt_getArrayLengthExportName: string = ufmt_getArrayLengthDefaultExportName;
  ufmt_getArrayItemByIndexExportName: string = ufmt_getArrayItemByIndexDefaultExportName;
  ufmt_getDecNumCharsExportName: string = ufmt_getDecNumCharsDefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

{$ENDIF ~U_HIDE_DRAFT_API}

implementation

{$IFNDEF U_HIDE_DRAFT_API}

uses
  JclSysUtils;

{$IFNDEF ICU_LINKONREQUEST}
function ufmt_open; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_openDefaultExportName;
procedure ufmt_close; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_closeDefaultExportName;
function ufmt_getType; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getTypeDefaultExportName;
function ufmt_isNumeric; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_isNumericDefaultExportName;
function ufmt_getDate; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getDateDefaultExportName;
function ufmt_getDouble; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getDoubleDefaultExportName;
function ufmt_getLong; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getLongDefaultExportName;
function ufmt_getInt64; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getInt64DefaultExportName;
function ufmt_getObject; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getObjectDefaultExportName;
function ufmt_getUChars; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getUCharsDefaultExportName;
function ufmt_getArrayLength; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getArrayLengthDefaultExportName;
function ufmt_getArrayItemByIndex; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getArrayItemByIndexDefaultExportName;
function ufmt_getDecNumChars; external ICU_DEFAULT_I18N_MODULE_NAME name ufmt_getDecNumCharsDefaultExportName;
{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @ufmt_open := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_openExportName);
  @ufmt_close := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_closeExportName);
  @ufmt_getType := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getTypeExportName);
  @ufmt_isNumeric := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_isNumericExportName);
  @ufmt_getDate := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getDateExportName);
  @ufmt_getDouble := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getDoubleExportName);
  @ufmt_getLong := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getLongExportName);
  @ufmt_getInt64 := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getInt64ExportName);
  @ufmt_getObject := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getObjectExportName);
  @ufmt_getUChars := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getUCharsExportName);
  @ufmt_getArrayLength := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getArrayLengthExportName);
  @ufmt_getArrayItemByIndex := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getArrayItemByIndexExportName);
  @ufmt_getDecNumChars := GetModuleSymbol(ICU_I18N_LibraryHandle, ufmt_getDecNumCharsExportName);

  Result :=
    Assigned(@ufmt_open) and Assigned(@ufmt_close) and Assigned(@ufmt_getType) and Assigned(@ufmt_isNumeric) and Assigned(@ufmt_getDate) and
    Assigned(@ufmt_getDouble) and Assigned(@ufmt_getLong) and Assigned(@ufmt_getInt64) and Assigned(@ufmt_getObject) and Assigned(@ufmt_getUChars) and
    Assigned(@ufmt_getArrayLength) and Assigned(@ufmt_getArrayItemByIndex) and Assigned(@ufmt_getDecNumChars);
end;

procedure UnloadICU;
begin
  @ufmt_open := nil;
  @ufmt_close := nil;
  @ufmt_getType := nil;
  @ufmt_isNumeric := nil;
  @ufmt_getDate := nil;
  @ufmt_getDouble := nil;
  @ufmt_getLong := nil;
  @ufmt_getInt64 := nil;
  @ufmt_getObject := nil;
  @ufmt_getUChars := nil;
  @ufmt_getArrayLength := nil;
  @ufmt_getArrayItemByIndex := nil;
  @ufmt_getDecNumChars := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}
{$ENDIF ~U_HIDE_DRAFT_API}

end.
