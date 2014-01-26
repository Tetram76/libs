unit _uformattable;

{$I icu.inc}

interface

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

(*
// Initialize a UFormattable, to type UNUM_LONG, value 0 may return error if memory allocation failed.
UFormattable * 	ufmt_open (UErrorCode *status)
// Cleanup any additional memory allocated by this UFormattable.
void 	ufmt_close (UFormattable *fmt)
// Return the type of this object.
UFormattableType 	ufmt_getType (const UFormattable *fmt, UErrorCode *status)
// Return whether the object is numeric.
UBool 	ufmt_isNumeric (const UFormattable *fmt)
// Gets the UDate value of this object.
UDate 	ufmt_getDate (const UFormattable *fmt, UErrorCode *status)
// Gets the double value of this object.
double 	ufmt_getDouble (UFormattable *fmt, UErrorCode *status)
// Gets the long (int32_t) value of this object.
int32_t 	ufmt_getLong (UFormattable *fmt, UErrorCode *status)
// Gets the int64_t value of this object.
int64_t 	ufmt_getInt64 (UFormattable *fmt, UErrorCode *status)
// Returns a pointer to the UObject contained within this formattable (as a const void* ), or NULL if this object is not of type UFMT_OBJECT.
const void * 	ufmt_getObject (const UFormattable *fmt, UErrorCode *status)
// Gets the string value of this object as a UChar string.
const UChar * 	ufmt_getUChars (UFormattable *fmt, int32_t *len, UErrorCode *status)
// Get the number of array objects contained, if an array type UFMT_ARRAY.
int32_t 	ufmt_getArrayLength (const UFormattable *fmt, UErrorCode *status)
// Get the specified value from the array of UFormattables.
UFormattable * 	ufmt_getArrayItemByIndex (UFormattable *fmt, int32_t n, UErrorCode *status)
// Returns a numeric string representation of the number contained within this formattable, or NULL if this object does not contain numeric type.
const char * 	ufmt_getDecNumChars (UFormattable *fmt, int32_t *len, UErrorCode *status)
*)

implementation

end.
