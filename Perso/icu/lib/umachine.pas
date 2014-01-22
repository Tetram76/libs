unit umachine;

{$I icu.inc}

interface

const
  // The TRUE value of a UBool.
  UTRUE = 1;
  // The FALSE value of a UBool.
  UFALSE = 0;

type
  PInt32 = ^Int32;

  // The ICU boolean type.
  UBool = int8;
  // Define UChar to be UCHAR_TYPE, if that is #defined (for example, to char16_t), or wchar_t if that is 16 bits wide; always assumed to be unsigned.
  UChar = WideChar;
  PUChar = ^UChar;
  // Define UChar32 as a type for single Unicode code points.
  UChar32 = int32;

implementation

end.
