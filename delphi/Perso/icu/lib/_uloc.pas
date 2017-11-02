unit _uloc;

{$I icu.inc}

interface

uses
  _utypes, _umachine, _enum, icu_globals;

const
  ULOC_CHINESE = 'zh';
  ULOC_ENGLISH = 'en';
  ULOC_FRENCH = 'fr';
  ULOC_GERMAN = 'de';
  ULOC_ITALIAN = 'it';
  ULOC_JAPANESE = 'ja';
  ULOC_KOREAN = 'ko';
  ULOC_SIMPLIFIED_CHINESE = 'zh_CN';
  ULOC_TRADITIONAL_CHINESE = 'zh_TW';
  ULOC_CANADA = 'en_CA';
  ULOC_CANADA_FRENCH = 'fr_CA';
  ULOC_CHINA = 'zh_CN';
  ULOC_PRC = 'zh_CN';
  ULOC_FRANCE = 'fr_FR';
  ULOC_GERMANY = 'de_DE';
  ULOC_ITALY = 'it_IT';
  ULOC_JAPAN = 'ja_JP';
  ULOC_KOREA = 'ko_KR';
  ULOC_TAIWAN = 'zh_TW';
  ULOC_UK = 'en_GB';
  ULOC_US = 'en_US';
  ULOC_LANG_CAPACITY = 12;
  ULOC_COUNTRY_CAPACITY = 4;
  ULOC_FULLNAME_CAPACITY = 157;
  ULOC_SCRIPT_CAPACITY = 6;
  ULOC_KEYWORDS_CAPACITY = 50;
  ULOC_KEYWORD_AND_VALUES_CAPACITY = 100;
  ULOC_KEYWORD_SEPARATOR = '@';
  ULOC_KEYWORD_SEPARATOR_UNICODE = $40;
  ULOC_KEYWORD_ASSIGN = '=';
  ULOC_KEYWORD_ASSIGN_UNICODE = $3D;
  ULOC_KEYWORD_ITEM_SEPARATOR = ';';
  ULOC_KEYWORD_ITEM_SEPARATOR_UNICODE = $3B;

type
  ULocDataLocaleType = (
    ULOC_ACTUAL_LOCALE = 0,
    ULOC_VALID_LOCALE = 1,
{$IFNDEF U_HIDE_DEPRECATED_API}
    ULOC_REQUESTED_LOCALE = 2,
{$ENDIF ~U_HIDE_DEPRECATED_API}
    ULOC_DATA_LOCALE_TYPE_LIMIT = 3
  );

type
  ULayoutType = (
    ULOC_LAYOUT_LTR = 0, // left-to-right.
    ULOC_LAYOUT_RTL = 1, // right-to-left.
    ULOC_LAYOUT_TTB = 2, // top-to-bottom.
    ULOC_LAYOUT_BTT = 3, // bottom-to-top.
    ULOC_LAYOUT_UNKNOWN
  );

  UAcceptResult = (
    ULOC_ACCEPT_FAILED = 0, // No exact match was found.
    ULOC_ACCEPT_VALID = 1, // An exact match was found.
    ULOC_ACCEPT_FALLBACK = 2 // A fallback was found, for example, Accept list contained 'ja_JP' which matched available locale 'ja'.
    );


  {$IFDEF ICU_LINKONREQUEST}

{$IFNDEF U_HIDE_SYSTEM_API}
  // Gets ICU's default locale.
 	Tuloc_getDefault = function: PAnsiChar; cdecl;
  // Sets ICU's default locale.
 	Tuloc_setDefault = procedure(const localeID: PAnsiChar; var status: UErrorCode); cdecl;
{$ENDIF ~U_HIDE_SYSTEM_API}
  // Gets the language code for the specified locale.
  Tuloc_getLanguage = function(const localeID: PAnsiChar; language: PAnsiChar; languageCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the script code for the specified locale.
  Tuloc_getScript = function(const localeID: PAnsiChar; script: PAnsiChar; scriptCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the country code for the specified locale.
  Tuloc_getCountry = function(const localeID: PAnsiChar; country: PAnsiChar; countryCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the variant code for the specified locale.
  Tuloc_getVariant = function(const localeID: PAnsiChar; variant: PAnsiChar; variantCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the full name for the specified locale.
  Tuloc_getName = function(const localeID: PAnsiChar; name: PAnsiChar; nameCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the full name for the specified locale.
  Tuloc_canonicalize = function(const localeID: PAnsiChar; name: PAnsiChar; nameCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the ISO language code for the specified locale.
  Tuloc_getISO3Language = function(const localeID: PAnsiChar): PAnsiChar; cdecl;
  // Gets the ISO country code for the specified locale.
  Tuloc_getISO3Country = function(const localeID: PAnsiChar): PAnsiChar; cdecl;
  // Gets the Win32 LCID value for the specified locale.
  Tuloc_getLCID = function(const localeID: PAnsiChar): UInt32; cdecl;
  // Gets the language name suitable for display for the specified locale.
  Tuloc_getDisplayLanguage = function(const locale: PAnsiChar; const displayLocale: PAnsiChar; language: PUChar; languageCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Gets the script name suitable for display for the specified locale.
  Tuloc_getDisplayScript = function(const locale: PAnsiChar; const displayLocale: PAnsiChar; script: PUChar; scriptCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Gets the country name suitable for display for the specified locale.
  Tuloc_getDisplayCountry = function(const locale: PAnsiChar; const displayLocale: PAnsiChar; country: PUChar; countryCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Gets the variant name suitable for display for the specified locale.
  Tuloc_getDisplayVariant = function(const locale: PAnsiChar; const displayLocale: PAnsiChar; variant: PUChar; variantCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Gets the keyword name suitable for display for the specified locale.
  Tuloc_getDisplayKeyword = function(const keyword: PAnsiChar; const displayLocale: PAnsiChar; dest: PUChar; destCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Gets the value of the keyword suitable for display for the specified locale.
  Tuloc_getDisplayKeywordValue = function(const locale: PAnsiChar; const keyword: PAnsiChar; const displayLocale: PAnsiChar; dest: PUChar; destCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Gets the full name suitable for display for the specified locale.
  Tuloc_getDisplayName = function(const localeID: PAnsiChar; const inLocaleID: PAnsiChar; result: PUChar; maxResultSize: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the specified locale from a list of all available locales.
  Tuloc_getAvailable = function(n: Int32): PAnsiChar; cdecl;
  // Gets the size of the all available locale list.
  Tuloc_countAvailable = function: Int32; cdecl;
  // Gets a list of all available 2-letter language codes defined in ISO 639, plus additional 3-letter codes determined to be useful for locale generation as defined by Unicode CLDR.
  Tuloc_getISOLanguages = function: PAnsiChar; cdecl;
  // Gets a list of all available 2-letter country codes defined in ISO 639.
  Tuloc_getISOCountries = function: PAnsiChar; cdecl;
  // Truncate the locale ID string to get the parent locale ID.
  Tuloc_getParent = function(const localeID: PAnsiChar; parent: PAnsiChar; parentCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets the full name for the specified locale, like uloc_getName(), but without keywords.
  Tuloc_getBaseName = function(const localeID: PAnsiChar; name: PAnsiChar; nameCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Gets an enumeration of keywords for the specified locale.
  Tuloc_openKeywords = function(const localeID: PAnsiChar; var status: UErrorCode): PUEnumeration; cdecl;
  // Get the value for a keyword.
  Tuloc_getKeywordValue = function(const localeID: PAnsiChar; const keywordName: PAnsiChar; buffer: PAnsiChar; bufferCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Sets or removes the value of the specified keyword.
  Tuloc_setKeywordValue = function(const keywordName: PAnsiChar; const keywordValue: PAnsiChar; buffer: PAnsiChar; bufferCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Get the layout character orientation for the specified locale.
  Tuloc_getCharacterOrientation = function(const localeID: PAnsiChar; var status: UErrorCode): ULayoutType; cdecl;
  // Get the layout line orientation for the specified locale.
  Tuloc_getLineOrientation = function(const localeID: PAnsiChar; var status: UErrorCode): ULayoutType; cdecl;
  // Based on a HTTP header from a web browser and a list of available locales, determine an acceptable locale for the user.
  Tuloc_acceptLanguageFromHTTP = function(result: PAnsiChar; resultAvailable: Int32; var outResult: UAcceptResult; const httpAcceptLanguage: PAnsiChar; availableLocales: PUEnumeration; var status: UErrorCode): Int32; cdecl;
  // Based on a list of available locales, determine an acceptable locale for the user.
  // const char **acceptList
  Tuloc_acceptLanguage = function(result: PAnsiChar; resultAvailable: Int32; var outResult: UAcceptResult; acceptList: PPAnsiChar; acceptListCount: Int32; availableLocales: PUEnumeration; var status: UErrorCode): Int32; cdecl;
  // Gets the ICU locale ID for the specified Win32 LCID value.
  Tuloc_getLocaleForLCID = function(hostID: UInt32; locale: PAnsiChar; localeCapacity: Int32; var status: UErrorCode): Int32; cdecl;
  // Add the likely subtags for a provided locale ID, per the algorithm described in the following CLDR technical report:
  Tuloc_addLikelySubtags = function(const localeID: PAnsiChar; maximizedLocaleID: PAnsiChar; maximizedLocaleIDCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Minimize the subtags for a provided locale ID, per the algorithm described in the following CLDR technical report:
  Tuloc_minimizeSubtags = function(const localeID: PAnsiChar; minimizedLocaleID: PAnsiChar; minimizedLocaleIDCapacity: Int32; var err: UErrorCode): Int32; cdecl;
  // Returns a locale ID for the specified BCP47 language tag string.
  Tuloc_forLanguageTag = function(const langtag: PAnsiChar; localeID: PAnsiChar; localeIDCapacity: Int32; var parsedLength: Int32; var err: UErrorCode): Int32; cdecl;
  // Returns a well-formed language tag for this locale ID.
  Tuloc_toLanguageTag = function(const localeID: PAnsiChar; langtag: PAnsiChar; langtagCapacity: Int32; strict: UBool; var err: UErrorCode): Int32; cdecl;

var
{$IFNDEF U_HIDE_SYSTEM_API}
 	uloc_getDefault: Tuloc_getDefault = nil;
 	uloc_setDefault: Tuloc_setDefault = nil;
{$ENDIF ~U_HIDE_SYSTEM_API}
  uloc_getLanguage: Tuloc_getLanguage = nil;
  uloc_getScript: Tuloc_getScript = nil;
  uloc_getCountry: Tuloc_getCountry = nil;
  uloc_getVariant: Tuloc_getVariant = nil;
  uloc_getName: Tuloc_getName = nil;
  uloc_canonicalize: Tuloc_canonicalize = nil;
  uloc_getISO3Language: Tuloc_getISO3Language = nil;
  uloc_getISO3Country: Tuloc_getISO3Country = nil;
  uloc_getLCID: Tuloc_getLCID = nil;
  uloc_getDisplayLanguage: Tuloc_getDisplayLanguage = nil;
  uloc_getDisplayScript: Tuloc_getDisplayScript = nil;
  uloc_getDisplayCountry: Tuloc_getDisplayCountry = nil;
  uloc_getDisplayVariant: Tuloc_getDisplayVariant = nil;
  uloc_getDisplayKeyword: Tuloc_getDisplayKeyword = nil;
  uloc_getDisplayKeywordValue: Tuloc_getDisplayKeywordValue = nil;
  uloc_getDisplayName: Tuloc_getDisplayName = nil;
  uloc_getAvailable: Tuloc_getAvailable = nil;
  uloc_countAvailable: Tuloc_countAvailable = nil;
  uloc_getISOLanguages: Tuloc_getISOLanguages = nil;
  uloc_getISOCountries: Tuloc_getISOCountries = nil;
  uloc_getParent: Tuloc_getParent = nil;
  uloc_getBaseName: Tuloc_getBaseName = nil;
  uloc_openKeywords: Tuloc_openKeywords = nil;
  uloc_getKeywordValue: Tuloc_getKeywordValue = nil;
  uloc_setKeywordValue: Tuloc_setKeywordValue = nil;
  uloc_getCharacterOrientation: Tuloc_getCharacterOrientation = nil;
  uloc_getLineOrientation: Tuloc_getLineOrientation = nil;
  uloc_acceptLanguageFromHTTP: Tuloc_acceptLanguageFromHTTP = nil;
  uloc_acceptLanguage: Tuloc_acceptLanguage = nil;
  uloc_getLocaleForLCID: Tuloc_getLocaleForLCID = nil;
  uloc_addLikelySubtags: Tuloc_addLikelySubtags = nil;
  uloc_minimizeSubtags: Tuloc_minimizeSubtags = nil;
  uloc_forLanguageTag: Tuloc_forLanguageTag = nil;
  uloc_toLanguageTag: Tuloc_toLanguageTag = nil;
{$ELSE ~ICU_LINKONREQUEST}
{$IFNDEF U_HIDE_SYSTEM_API}
function uloc_getDefault: PAnsiChar; cdecl;
procedure uloc_setDefault(const localeID: PAnsiChar; var status: UErrorCode); cdecl;
{$ENDIF ~U_HIDE_SYSTEM_API}
function uloc_getLanguage(const localeID: PAnsiChar; language: PAnsiChar; languageCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getScript(const localeID: PAnsiChar; script: PAnsiChar; scriptCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getCountry(const localeID: PAnsiChar; country: PAnsiChar; countryCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getVariant(const localeID: PAnsiChar; variant: PAnsiChar; variantCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getName(const localeID: PAnsiChar; name: PAnsiChar; nameCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_canonicalize(const localeID: PAnsiChar; name: PAnsiChar; nameCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getISO3Language(const localeID: PAnsiChar): PAnsiChar; cdecl;
function uloc_getISO3Country(const localeID: PAnsiChar): PAnsiChar; cdecl;
function uloc_getLCID(const localeID: PAnsiChar): UInt32; cdecl;
function uloc_getDisplayLanguage(const locale: PAnsiChar; const displayLocale: PAnsiChar; language: PUChar; languageCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getDisplayScript(const locale: PAnsiChar; const displayLocale: PAnsiChar; script: PUChar; scriptCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getDisplayCountry(const locale: PAnsiChar; const displayLocale: PAnsiChar; country: PUChar; countryCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getDisplayVariant(const locale: PAnsiChar; const displayLocale: PAnsiChar; variant: PUChar; variantCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getDisplayKeyword(const keyword: PAnsiChar; const displayLocale: PAnsiChar; dest: PUChar; destCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getDisplayKeywordValue(const locale: PAnsiChar; const keyword: PAnsiChar; const displayLocale: PAnsiChar; dest: PUChar; destCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getDisplayName(const localeID: PAnsiChar; const inLocaleID: PAnsiChar; result: PUChar; maxResultSize: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getAvailable(n: Int32): PAnsiChar; cdecl;
function uloc_countAvailable: Int32; cdecl;
function uloc_getISOLanguages: PAnsiChar; cdecl;
function uloc_getISOCountries: PAnsiChar; cdecl;
function uloc_getParent(const localeID: PAnsiChar; parent: PAnsiChar; parentCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_getBaseName(const localeID: PAnsiChar; name: PAnsiChar; nameCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_openKeywords(const localeID: PAnsiChar; var status: UErrorCode): PUEnumeration; cdecl;
function uloc_getKeywordValue(const localeID: PAnsiChar; const keywordName: PAnsiChar; buffer: PAnsiChar; bufferCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_setKeywordValue(const keywordName: PAnsiChar; const keywordValue: PAnsiChar; buffer: PAnsiChar; bufferCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_getCharacterOrientation(const localeID: PAnsiChar; var status: UErrorCode): ULayoutType; cdecl;
function uloc_getLineOrientation(const localeID: PAnsiChar; var status: UErrorCode): ULayoutType; cdecl;
function uloc_acceptLanguageFromHTTP(result: PAnsiChar; resultAvailable: Int32; var outResult: UAcceptResult; const httpAcceptLanguage: PAnsiChar; availableLocales: PUEnumeration; var status: UErrorCode): Int32; cdecl;
// const char **acceptList
function uloc_acceptLanguage(result: PAnsiChar; resultAvailable: Int32; var outResult: UAcceptResult; acceptList: PPAnsiChar; acceptListCount: Int32; availableLocales: PUEnumeration; var status: UErrorCode): Int32; cdecl;
function uloc_getLocaleForLCID(hostID: UInt32; locale: PAnsiChar; localeCapacity: Int32; var status: UErrorCode): Int32; cdecl;
function uloc_addLikelySubtags(const localeID: PAnsiChar; maximizedLocaleID: PAnsiChar; maximizedLocaleIDCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_minimizeSubtags(const localeID: PAnsiChar; minimizedLocaleID: PAnsiChar; minimizedLocaleIDCapacity: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_forLanguageTag(const langtag: PAnsiChar; localeID: PAnsiChar; localeIDCapacity: Int32; var parsedLength: Int32; var err: UErrorCode): Int32; cdecl;
function uloc_toLanguageTag(const localeID: PAnsiChar; langtag: PAnsiChar; langtagCapacity: Int32; strict: UBool; var err: UErrorCode): Int32; cdecl;
{$ENDIF ~ICU_LINKONREQUEST}
const
{$IFNDEF U_HIDE_SYSTEM_API}
 	uloc_getDefaultDefaultExportName = 'uloc_getDefault' + ICU_DEFAULT_EXPORT_SUFFIX;
 	uloc_setDefaultDefaultExportName = 'uloc_setDefault' + ICU_DEFAULT_EXPORT_SUFFIX;
{$ENDIF ~U_HIDE_SYSTEM_API}
  uloc_getLanguageDefaultExportName = 'uloc_getLanguage' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getScriptDefaultExportName = 'uloc_getScript' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getCountryDefaultExportName = 'uloc_getCountry' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getVariantDefaultExportName = 'uloc_getVariant' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getNameDefaultExportName = 'uloc_getName' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_canonicalizeDefaultExportName = 'uloc_canonicalize' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getISO3LanguageDefaultExportName = 'uloc_getISO3Language' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getISO3CountryDefaultExportName = 'uloc_getISO3Country' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getLCIDDefaultExportName = 'uloc_getLCID' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayLanguageDefaultExportName = 'uloc_getDisplayLanguage' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayScriptDefaultExportName = 'uloc_getDisplayScript' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayCountryDefaultExportName = 'uloc_getDisplayCountry' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayVariantDefaultExportName = 'uloc_getDisplayVariant' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayKeywordDefaultExportName = 'uloc_getDisplayKeyword' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayKeywordValueDefaultExportName = 'uloc_getDisplayKeywordValue' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getDisplayNameDefaultExportName = 'uloc_getDisplayName' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getAvailableDefaultExportName = 'uloc_getAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_countAvailableDefaultExportName = 'uloc_countAvailable' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getISOLanguagesDefaultExportName = 'uloc_getISOLanguages' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getISOCountriesDefaultExportName = 'uloc_getISOCountries' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getParentDefaultExportName = 'uloc_getParent' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getBaseNameDefaultExportName = 'uloc_getBaseName' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_openKeywordsDefaultExportName = 'uloc_openKeywords' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getKeywordValueDefaultExportName = 'uloc_getKeywordValue' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_setKeywordValueDefaultExportName = 'uloc_setKeywordValue' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getCharacterOrientationDefaultExportName = 'uloc_getCharacterOrientation' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getLineOrientationDefaultExportName = 'uloc_getLineOrientation' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_acceptLanguageFromHTTPDefaultExportName = 'uloc_acceptLanguageFromHTTP' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_acceptLanguageDefaultExportName = 'uloc_acceptLanguage' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_getLocaleForLCIDDefaultExportName = 'uloc_getLocaleForLCID' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_addLikelySubtagsDefaultExportName = 'uloc_addLikelySubtags' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_minimizeSubtagsDefaultExportName = 'uloc_minimizeSubtags' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_forLanguageTagDefaultExportName = 'uloc_forLanguageTag' + ICU_DEFAULT_EXPORT_SUFFIX;
  uloc_toLanguageTagDefaultExportName = 'uloc_toLanguageTag' + ICU_DEFAULT_EXPORT_SUFFIX;
{$IFDEF ICU_LINKONREQUEST}

var
{$IFNDEF U_HIDE_SYSTEM_API}
 	uloc_getDefaultExportName: string = uloc_getDefaultDefaultExportName;
 	uloc_setDefaultExportName: string = uloc_setDefaultDefaultExportName;
{$ENDIF ~U_HIDE_SYSTEM_API}
  uloc_getLanguageExportName: string = uloc_getLanguageDefaultExportName;
  uloc_getScriptExportName: string = uloc_getScriptDefaultExportName;
  uloc_getCountryExportName: string = uloc_getCountryDefaultExportName;
  uloc_getVariantExportName: string = uloc_getVariantDefaultExportName;
  uloc_getNameExportName: string = uloc_getNameDefaultExportName;
  uloc_canonicalizeExportName: string = uloc_canonicalizeDefaultExportName;
  uloc_getISO3LanguageExportName: string = uloc_getISO3LanguageDefaultExportName;
  uloc_getISO3CountryExportName: string = uloc_getISO3CountryDefaultExportName;
  uloc_getLCIDExportName: string = uloc_getLCIDDefaultExportName;
  uloc_getDisplayLanguageExportName: string = uloc_getDisplayLanguageDefaultExportName;
  uloc_getDisplayScriptExportName: string = uloc_getDisplayScriptDefaultExportName;
  uloc_getDisplayCountryExportName: string = uloc_getDisplayCountryDefaultExportName;
  uloc_getDisplayVariantExportName: string = uloc_getDisplayVariantDefaultExportName;
  uloc_getDisplayKeywordExportName: string = uloc_getDisplayKeywordDefaultExportName;
  uloc_getDisplayKeywordValueExportName: string = uloc_getDisplayKeywordValueDefaultExportName;
  uloc_getDisplayNameExportName: string = uloc_getDisplayNameDefaultExportName;
  uloc_getAvailableExportName: string = uloc_getAvailableDefaultExportName;
  uloc_countAvailableExportName: string = uloc_countAvailableDefaultExportName;
  uloc_getISOLanguagesExportName: string = uloc_getISOLanguagesDefaultExportName;
  uloc_getISOCountriesExportName: string = uloc_getISOCountriesDefaultExportName;
  uloc_getParentExportName: string = uloc_getParentDefaultExportName;
  uloc_getBaseNameExportName: string = uloc_getBaseNameDefaultExportName;
  uloc_openKeywordsExportName: string = uloc_openKeywordsDefaultExportName;
  uloc_getKeywordValueExportName: string = uloc_getKeywordValueDefaultExportName;
  uloc_setKeywordValueExportName: string = uloc_setKeywordValueDefaultExportName;
  uloc_getCharacterOrientationExportName: string = uloc_getCharacterOrientationDefaultExportName;
  uloc_getLineOrientationExportName: string = uloc_getLineOrientationDefaultExportName;
  uloc_acceptLanguageFromHTTPExportName: string = uloc_acceptLanguageFromHTTPDefaultExportName;
  uloc_acceptLanguageExportName: string = uloc_acceptLanguageDefaultExportName;
  uloc_getLocaleForLCIDExportName: string = uloc_getLocaleForLCIDDefaultExportName;
  uloc_addLikelySubtagsExportName: string = uloc_addLikelySubtagsDefaultExportName;
  uloc_minimizeSubtagsExportName: string = uloc_minimizeSubtagsDefaultExportName;
  uloc_forLanguageTagExportName: string = uloc_forLanguageTagDefaultExportName;
  uloc_toLanguageTagExportName: string = uloc_toLanguageTagDefaultExportName;
{$ENDIF ~ICU_LINKONREQUEST}

implementation
uses
  JclSysUtils;

{$IFNDEF ICU_LINKONREQUEST}
{$IFNDEF U_HIDE_SYSTEM_API}
function uloc_getDefault; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDefaultDefaultExportName;
procedure uloc_setDefault; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_setDefaultDefaultExportName;
{$ENDIF ~U_HIDE_SYSTEM_API}
function uloc_getLanguage; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getLanguageDefaultExportName;
function uloc_getScript; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getScriptDefaultExportName;
function uloc_getCountry; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getCountryDefaultExportName;
function uloc_getVariant; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getVariantDefaultExportName;
function uloc_getName; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getNameDefaultExportName;
function uloc_canonicalize; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_canonicalizeDefaultExportName;
function uloc_getISO3Language; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getISO3LanguageDefaultExportName;
function uloc_getISO3Country; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getISO3CountryDefaultExportName;
function uloc_getLCID; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getLCIDDefaultExportName;
function uloc_getDisplayLanguage; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayLanguageDefaultExportName;
function uloc_getDisplayScript; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayScriptDefaultExportName;
function uloc_getDisplayCountry; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayCountryDefaultExportName;
function uloc_getDisplayVariant; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayVariantDefaultExportName;
function uloc_getDisplayKeyword; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayKeywordDefaultExportName;
function uloc_getDisplayKeywordValue; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayKeywordValueDefaultExportName;
function uloc_getDisplayName; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getDisplayNameDefaultExportName;
function uloc_getAvailable; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getAvailableDefaultExportName;
function uloc_countAvailable; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_countAvailableDefaultExportName;
function uloc_getISOLanguages; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getISOLanguagesDefaultExportName;
function uloc_getISOCountries; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getISOCountriesDefaultExportName;
function uloc_getParent; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getParentDefaultExportName;
function uloc_getBaseName; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getBaseNameDefaultExportName;
function uloc_openKeywords; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_openKeywordsDefaultExportName;
function uloc_getKeywordValue; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getKeywordValueDefaultExportName;
function uloc_setKeywordValue; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_setKeywordValueDefaultExportName;
function uloc_getCharacterOrientation; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getCharacterOrientationDefaultExportName;
function uloc_getLineOrientation; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getLineOrientationDefaultExportName;
function uloc_acceptLanguageFromHTTP; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_acceptLanguageFromHTTPDefaultExportName;
function uloc_acceptLanguage; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_acceptLanguageDefaultExportName;
function uloc_getLocaleForLCID; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_getLocaleForLCIDDefaultExportName;
function uloc_addLikelySubtags; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_addLikelySubtagsDefaultExportName;
function uloc_minimizeSubtags; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_minimizeSubtagsDefaultExportName;
function uloc_forLanguageTag; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_forLanguageTagDefaultExportName;
function uloc_toLanguageTag; external ICU_DEFAULT_COMMON_MODULE_NAME name uloc_toLanguageTagDefaultExportName;
{$ELSE ~ICU_LINKONREQUEST}

function LoadICU: Boolean;
begin
{$IFNDEF U_HIDE_SYSTEM_API}
 	@uloc_getDefault := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDefaultExportName);
 	@uloc_setDefault := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_setDefaultExportName);
{$ENDIF ~U_HIDE_SYSTEM_API}
  @uloc_getLanguage := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getLanguageExportName);
  @uloc_getScript := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getScriptExportName);
  @uloc_getCountry := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getCountryExportName);
  @uloc_getVariant := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getVariantExportName);
  @uloc_getName := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getNameExportName);
  @uloc_canonicalize := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_canonicalizeExportName);
  @uloc_getISO3Language := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getISO3LanguageExportName);
  @uloc_getISO3Country := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getISO3CountryExportName);
  @uloc_getLCID := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getLCIDExportName);
  @uloc_getDisplayLanguage := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayLanguageExportName);
  @uloc_getDisplayScript := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayScriptExportName);
  @uloc_getDisplayCountry := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayCountryExportName);
  @uloc_getDisplayVariant := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayVariantExportName);
  @uloc_getDisplayKeyword := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayKeywordExportName);
  @uloc_getDisplayKeywordValue := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayKeywordValueExportName);
  @uloc_getDisplayName := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getDisplayNameExportName);
  @uloc_getAvailable := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getAvailableExportName);
  @uloc_countAvailable := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_countAvailableExportName);
  @uloc_getISOLanguages := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getISOLanguagesExportName);
  @uloc_getISOCountries := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getISOCountriesExportName);
  @uloc_getParent := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getParentExportName);
  @uloc_getBaseName := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getBaseNameExportName);
  @uloc_openKeywords := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_openKeywordsExportName);
  @uloc_getKeywordValue := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getKeywordValueExportName);
  @uloc_setKeywordValue := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_setKeywordValueExportName);
  @uloc_getCharacterOrientation := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getCharacterOrientationExportName);
  @uloc_getLineOrientation := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getLineOrientationExportName);
  @uloc_acceptLanguageFromHTTP := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_acceptLanguageFromHTTPExportName);
  @uloc_acceptLanguage := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_acceptLanguageExportName);
  @uloc_getLocaleForLCID := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_getLocaleForLCIDExportName);
  @uloc_addLikelySubtags := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_addLikelySubtagsExportName);
  @uloc_minimizeSubtags := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_minimizeSubtagsExportName);
  @uloc_forLanguageTag := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_forLanguageTagExportName);
  @uloc_toLanguageTag := GetModuleSymbol(ICU_COMMON_LibraryHandle, uloc_toLanguageTagExportName);

  Result :=
{$IFNDEF U_HIDE_SYSTEM_API}
    Assigned(@uloc_getDefault) and Assigned(@uloc_setDefault) and
{$ENDIF ~U_HIDE_SYSTEM_API}
    Assigned(@uloc_getLanguage) and Assigned(@uloc_getScript) and Assigned(@uloc_getCountry) and
    Assigned(@uloc_getVariant) and Assigned(@uloc_getName) and Assigned(@uloc_canonicalize) and
    Assigned(@uloc_getISO3Language) and Assigned(@uloc_getISO3Country) and Assigned(@uloc_getLCID) and
    Assigned(@uloc_getDisplayLanguage) and Assigned(@uloc_getDisplayScript) and Assigned(@uloc_getDisplayCountry) and
    Assigned(@uloc_getDisplayVariant) and Assigned(@uloc_getDisplayKeyword) and Assigned(@uloc_getDisplayKeywordValue) and
    Assigned(@uloc_getDisplayName) and Assigned(@uloc_getAvailable) and Assigned(@uloc_countAvailable) and
    Assigned(@uloc_getISOLanguages) and Assigned(@uloc_getISOCountries) and Assigned(@uloc_getParent) and
    Assigned(@uloc_getBaseName) and Assigned(@uloc_openKeywords) and Assigned(@uloc_getKeywordValue) and
    Assigned(@uloc_setKeywordValue) and Assigned(@uloc_getCharacterOrientation) and Assigned(@uloc_getLineOrientation) and
    Assigned(@uloc_acceptLanguageFromHTTP) and Assigned(@uloc_acceptLanguage) and Assigned(@uloc_getLocaleForLCID) and
    Assigned(@uloc_addLikelySubtags) and Assigned(@uloc_minimizeSubtags) and Assigned(@uloc_forLanguageTag) and
    Assigned(@uloc_toLanguageTag);
end;

procedure UnloadICU;
begin
{$IFNDEF U_HIDE_SYSTEM_API}
 	@uloc_getDefault := nil;
 	@uloc_setDefault := nil;
{$ENDIF ~U_HIDE_SYSTEM_API}
  @uloc_getLanguage := nil;
  @uloc_getScript := nil;
  @uloc_getCountry := nil;
  @uloc_getVariant := nil;
  @uloc_getName := nil;
  @uloc_canonicalize := nil;
  @uloc_getISO3Language := nil;
  @uloc_getISO3Country := nil;
  @uloc_getLCID := nil;
  @uloc_getDisplayLanguage := nil;
  @uloc_getDisplayScript := nil;
  @uloc_getDisplayCountry := nil;
  @uloc_getDisplayVariant := nil;
  @uloc_getDisplayKeyword := nil;
  @uloc_getDisplayKeywordValue := nil;
  @uloc_getDisplayName := nil;
  @uloc_getAvailable := nil;
  @uloc_countAvailable := nil;
  @uloc_getISOLanguages := nil;
  @uloc_getISOCountries := nil;
  @uloc_getParent := nil;
  @uloc_getBaseName := nil;
  @uloc_openKeywords := nil;
  @uloc_getKeywordValue := nil;
  @uloc_setKeywordValue := nil;
  @uloc_getCharacterOrientation := nil;
  @uloc_getLineOrientation := nil;
  @uloc_acceptLanguageFromHTTP := nil;
  @uloc_acceptLanguage := nil;
  @uloc_getLocaleForLCID := nil;
  @uloc_addLikelySubtags := nil;
  @uloc_minimizeSubtags := nil;
  @uloc_forLanguageTag := nil;
  @uloc_toLanguageTag := nil;
end;

initialization

RegisterLoadICUProc(LoadICU, UnloadICU);
{$ENDIF ~ICU_LINKONREQUEST}

end.
