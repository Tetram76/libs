unit _enum;

{$I icu.inc}

interface

uses
  _utypes, _umachine, icu_globals;

type
  // structure representing an enumeration object instance
  PUEnumeration = ^UEnumeration;

  UEnumeration = packed record
  end;

{$IFDEF ICU_LINKONREQUEST}

type
  // Disposes of resources in use by the iterator.
  Tuenum_close = procedure(en: PUEnumeration); cdecl;
  // Returns the number of elements that the iterator traverses.
  Tuenum_count = function(en: PUEnumeration; var status: UErrorCode): Int32; cdecl;
  // Returns the next element in the iterator's list.
  Tuenum_unext = function(en: PUEnumeration; var resultLength: Int32; var status: UErrorCode): PUChar; cdecl;
  // Returns the next element in the iterator's list.
  Tuenum_next = function(en: PUEnumeration; var resultLength: Int32; var status: UErrorCode): PAnsiString; cdecl;
  // Resets the iterator to the current list of service IDs.
  Tuenum_reset = procedure(en: PUEnumeration; var status: UErrorCode); cdecl;
  // Given an array of const UChar* strings, return a UEnumeration.
  Tuenum_openUCharStringsEnumeration = function(const strings: PPUChar; count: Int32; var ec: UErrorCode): PUEnumeration; cdecl;
  // Given an array of const char* strings (invariant chars only), return a UEnumeration.
  Tuenum_openCharStringsEnumeration = function(const strings: PPAnsiChar; count: Int32; var ec: UErrorCode): PUEnumeration; cdecl;

var
  uenum_close: Tuenum_close = nil;
  uenum_count: Tuenum_count = nil;
  uenum_unext: Tuenum_unext = nil;
  uenum_next: Tuenum_next  = nil;
  uenum_reset: Tuenum_reset = nil;
  uenum_openUCharStringsEnumeration: Tuenum_openUCharStringsEnumeration = nil;
  uenum_openCharStringsEnumeration: Tuenum_openCharStringsEnumeration = nil;

{$ELSE ~ICU_LINKONREQUEST}
procedure uenum_close(en: PUEnumeration); cdecl;
function uenum_count(en: PUEnumeration; var status: UErrorCode): Int32; cdecl;
function uenum_unext(en: PUEnumeration; var resultLength: Int32; var status: UErrorCode): PUChar; cdecl;
function uenum_next(en: PUEnumeration; var resultLength: Int32; var status: UErrorCode): PAnsiChar; cdecl;
procedure uenum_reset(en: PUEnumeration; var status: UErrorCode); cdecl;
function uenum_openUCharStringsEnumeration(const strings: PPUChar; count: Int32; var ec: UErrorCode): PUEnumeration; cdecl;
function uenum_openCharStringsEnumeration(const strings: PPAnsiChar; count: Int32; var ec: UErrorCode): PUEnumeration; cdecl;
{$ENDIF ~ICU_LINKONREQUEST}

const
  uenum_closeDefaultExportName = 'uenum_close' + ICU_DEFAULT_EXPORT_SUFFIX;
  uenum_countDefaultExportName = 'uenum_count' + ICU_DEFAULT_EXPORT_SUFFIX;
  uenum_unextDefaultExportName = 'uenum_unext' + ICU_DEFAULT_EXPORT_SUFFIX;
  uenum_nextDefaultExportName = 'uenum_next' + ICU_DEFAULT_EXPORT_SUFFIX;
  uenum_resetDefaultExportName = 'uenum_reset' + ICU_DEFAULT_EXPORT_SUFFIX;
  uenum_openUCharStringsEnumerationDefaultExportName = 'uenum_openUCharStringsEnumeration' + ICU_DEFAULT_EXPORT_SUFFIX;
  uenum_openCharStringsEnumerationDefaultExportName = 'uenum_openCharStringsEnumeration' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFDEF ICU_LINKONREQUEST}

var
  uenum_closeExportName: string = uenum_closeDefaultExportName;
  uenum_countExportName: string = uenum_countDefaultExportName;
  uenum_unextExportName: string = uenum_unextDefaultExportName;
  uenum_nextExportName: string = uenum_nextDefaultExportName;
  uenum_resetExportName: string = uenum_resetDefaultExportName;
  uenum_openUCharStringsEnumerationExportName: string = uenum_openUCharStringsEnumerationDefaultExportName;
  uenum_openCharStringsEnumerationExportName: string = uenum_openCharStringsEnumerationDefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

implementation

uses
  JclSysUtils;

{$IFNDEF ICU_LINKONREQUEST}
procedure uenum_close; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_closeDefaultExportName;
function uenum_count; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_countDefaultExportName;
function uenum_unext; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_unextDefaultExportName;
function uenum_next; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_nextDefaultExportName;
procedure uenum_reset; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_resetDefaultExportName;
function uenum_openUCharStringsEnumeration; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_openUCharStringsEnumerationDefaultExportName;
function uenum_openCharStringsEnumeration; external ICU_DEFAULT_COMMON_MODULE_NAME name uenum_openCharStringsEnumerationDefaultExportName;
{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
  @uenum_close := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_closeExportName);
  @uenum_count := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_countExportName);
  @uenum_unext := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_unextExportName);
  @uenum_next := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_nextExportName);
  @uenum_reset := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_resetExportName);
  @uenum_openUCharStringsEnumeration := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_openUCharStringsEnumerationExportName);
  @uenum_openCharStringsEnumeration := GetModuleSymbol(ICU_COMMON_LibraryHandle, uenum_openCharStringsEnumerationExportName);

  Result := Assigned(@uenum_close) and Assigned(@uenum_count) and Assigned(@uenum_unext) and Assigned(@uenum_next) and Assigned(@uenum_reset) and
    Assigned(@uenum_openUCharStringsEnumeration) and Assigned(@uenum_openCharStringsEnumeration);
end;

procedure UnloadICU;
begin
  @uenum_close := nil;
  @uenum_count := nil;
  @uenum_unext := nil;
  @uenum_next := nil;
  @uenum_reset := nil;
  @uenum_openUCharStringsEnumeration := nil;
  @uenum_openCharStringsEnumeration := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
