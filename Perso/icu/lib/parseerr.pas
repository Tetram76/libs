unit parseerr;

{$I icu.inc}

interface

uses
  umachine;

const
  U_PARSE_CONTEXT_LEN = 16;

type
  PUParseError = ^UParseError;

  UParseError = packed record
    // The line on which the error occured.
    line: int32;
    // The character offset to the error.
    offset: int32;
    // Textual context before the error.
    preContext: array [0 .. U_PARSE_CONTEXT_LEN - 1] of UChar;
    // The error itself and/or textual context after the error.
    postContext: array [0 .. U_PARSE_CONTEXT_LEN - 1] of UChar;
  end;

implementation

end.
