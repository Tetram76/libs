unit _umisc;

{$I icu.inc}

interface

type
  PUFieldPosition = ^UFieldPosition;

  UFieldPosition = packed record
    // The field.
    field: int32;
    // The start of the text range containing field.
    beginIndex: int32;
    // The limit of the text range containing field.
    endIndex: int32;
  end;

implementation

end.
