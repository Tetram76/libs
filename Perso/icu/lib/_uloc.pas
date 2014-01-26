unit _uloc;

{$I icu.inc}

interface

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
    ULOC_DEPRECATED2 = 2,
    ULOC_DATA_LOCALE_TYPE_LIMIT = 3
  );

const
  ULOC_REQUESTED_LOCALE = ULOC_DEPRECATED2 deprecated;

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


(*
// Gets ICU's default locale.
const char * 	uloc_getDefault (void)
// Sets ICU's default locale.
void 	uloc_setDefault (const char *localeID, UErrorCode *status)
// Gets the language code for the specified locale.
int32_t 	uloc_getLanguage (const char *localeID, char *language, int32_t languageCapacity, UErrorCode *err)
// Gets the script code for the specified locale.
int32_t 	uloc_getScript (const char *localeID, char *script, int32_t scriptCapacity, UErrorCode *err)
// Gets the country code for the specified locale.
int32_t 	uloc_getCountry (const char *localeID, char *country, int32_t countryCapacity, UErrorCode *err)
// Gets the variant code for the specified locale.
int32_t 	uloc_getVariant (const char *localeID, char *variant, int32_t variantCapacity, UErrorCode *err)
// Gets the full name for the specified locale.
int32_t 	uloc_getName (const char *localeID, char *name, int32_t nameCapacity, UErrorCode *err)
// Gets the full name for the specified locale.
int32_t 	uloc_canonicalize (const char *localeID, char *name, int32_t nameCapacity, UErrorCode *err)
// Gets the ISO language code for the specified locale.
const char * 	uloc_getISO3Language (const char *localeID)
// Gets the ISO country code for the specified locale.
const char * 	uloc_getISO3Country (const char *localeID)
// Gets the Win32 LCID value for the specified locale.
uint32_t 	uloc_getLCID (const char *localeID)
// Gets the language name suitable for display for the specified locale.
int32_t 	uloc_getDisplayLanguage (const char *locale, const char *displayLocale, UChar *language, int32_t languageCapacity, UErrorCode *status)
// Gets the script name suitable for display for the specified locale.
int32_t 	uloc_getDisplayScript (const char *locale, const char *displayLocale, UChar *script, int32_t scriptCapacity, UErrorCode *status)
// Gets the country name suitable for display for the specified locale.
int32_t 	uloc_getDisplayCountry (const char *locale, const char *displayLocale, UChar *country, int32_t countryCapacity, UErrorCode *status)
// Gets the variant name suitable for display for the specified locale.
int32_t 	uloc_getDisplayVariant (const char *locale, const char *displayLocale, UChar *variant, int32_t variantCapacity, UErrorCode *status)
// Gets the keyword name suitable for display for the specified locale.
int32_t 	uloc_getDisplayKeyword (const char *keyword, const char *displayLocale, UChar *dest, int32_t destCapacity, UErrorCode *status)
// Gets the value of the keyword suitable for display for the specified locale.
int32_t 	uloc_getDisplayKeywordValue (const char *locale, const char *keyword, const char *displayLocale, UChar *dest, int32_t destCapacity, UErrorCode *status)
// Gets the full name suitable for display for the specified locale.
int32_t 	uloc_getDisplayName (const char *localeID, const char *inLocaleID, UChar *result, int32_t maxResultSize, UErrorCode *err)
// Gets the specified locale from a list of all available locales.
const char * 	uloc_getAvailable (int32_t n)
// Gets the size of the all available locale list.
int32_t 	uloc_countAvailable (void)
// Gets a list of all available 2-letter language codes defined in ISO 639, plus additional 3-letter codes determined to be useful for locale generation as defined by Unicode CLDR.
const char *const * 	uloc_getISOLanguages (void)
// Gets a list of all available 2-letter country codes defined in ISO 639.
const char *const * 	uloc_getISOCountries (void)
// Truncate the locale ID string to get the parent locale ID.
int32_t 	uloc_getParent (const char *localeID, char *parent, int32_t parentCapacity, UErrorCode *err)
// Gets the full name for the specified locale, like uloc_getName(), but without keywords.
int32_t 	uloc_getBaseName (const char *localeID, char *name, int32_t nameCapacity, UErrorCode *err)
// Gets an enumeration of keywords for the specified locale.
UEnumeration * 	uloc_openKeywords (const char *localeID, UErrorCode *status)
// Get the value for a keyword.
int32_t 	uloc_getKeywordValue (const char *localeID, const char *keywordName, char *buffer, int32_t bufferCapacity, UErrorCode *status)
// Sets or removes the value of the specified keyword.
int32_t 	uloc_setKeywordValue (const char *keywordName, const char *keywordValue, char *buffer, int32_t bufferCapacity, UErrorCode *status)
// Get the layout character orientation for the specified locale.
ULayoutType 	uloc_getCharacterOrientation (const char *localeId, UErrorCode *status)
// Get the layout line orientation for the specified locale.
ULayoutType 	uloc_getLineOrientation (const char *localeId, UErrorCode *status)
// Based on a HTTP header from a web browser and a list of available locales, determine an acceptable locale for the user.
int32_t 	uloc_acceptLanguageFromHTTP (char *result, int32_t resultAvailable, UAcceptResult *outResult, const char *httpAcceptLanguage, UEnumeration *availableLocales, UErrorCode *status)
// Based on a list of available locales, determine an acceptable locale for the user.
int32_t 	uloc_acceptLanguage (char *result, int32_t resultAvailable, UAcceptResult *outResult, const char **acceptList, int32_t acceptListCount, UEnumeration *availableLocales, UErrorCode *status)
// Gets the ICU locale ID for the specified Win32 LCID value.
int32_t 	uloc_getLocaleForLCID (uint32_t hostID, char *locale, int32_t localeCapacity, UErrorCode *status)
// Add the likely subtags for a provided locale ID, per the algorithm described in the following CLDR technical report:
int32_t 	uloc_addLikelySubtags (const char *localeID, char *maximizedLocaleID, int32_t maximizedLocaleIDCapacity, UErrorCode *err)
// Minimize the subtags for a provided locale ID, per the algorithm described in the following CLDR technical report:
int32_t 	uloc_minimizeSubtags (const char *localeID, char *minimizedLocaleID, int32_t minimizedLocaleIDCapacity, UErrorCode *err)
// Returns a locale ID for the specified BCP47 language tag string.
int32_t 	uloc_forLanguageTag (const char *langtag, char *localeID, int32_t localeIDCapacity, int32_t *parsedLength, UErrorCode *err)
// Returns a well-formed language tag for this locale ID.
int32_t 	uloc_toLanguageTag (const char *localeID, char *langtag, int32_t langtagCapacity, UBool strict, UErrorCode *err)
*)
implementation

end.
