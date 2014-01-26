unit _enum;

interface

type
  // structure representing an enumeration object instance
  PUEnumeration = ^UEnumeration;

  UEnumeration = packed record
  end;

  (*
    // Disposes of resources in use by the iterator.
    void 	uenum_close (UEnumeration *en)
    // Returns the number of elements that the iterator traverses.
    int32_t 	uenum_count (UEnumeration *en, UErrorCode *status)
    // Returns the next element in the iterator's list.
    const UChar * 	uenum_unext (UEnumeration *en, int32_t *resultLength, UErrorCode *status)
    // Returns the next element in the iterator's list.
    const char * 	uenum_next (UEnumeration *en, int32_t *resultLength, UErrorCode *status)
    // Resets the iterator to the current list of service IDs.
    void 	uenum_reset (UEnumeration *en, UErrorCode *status)
    // Given a StringEnumeration, wrap it in a UEnumeration.
    UEnumeration * 	uenum_openFromStringEnumeration (icu::StringEnumeration *adopted, UErrorCode *ec)
    // Given an array of const UChar* strings, return a UEnumeration.
    UEnumeration * 	uenum_openUCharStringsEnumeration (const UChar *const strings[], int32_t count, UErrorCode *ec)
    // Given an array of const char* strings (invariant chars only), return a UEnumeration.
    UEnumeration * 	uenum_openCharStringsEnumeration (const char *const strings[], int32_t count, UErrorCode *ec)
  *)

implementation

end.
