unit uloc;

{$I icu.inc}

interface

type
  ULocDataLocaleType = (
    ULOC_ACTUAL_LOCALE = 0,
    ULOC_VALID_LOCALE = 1,
    ULOC_DEPRECATED2 = 2,
    ULOC_DATA_LOCALE_TYPE_LIMIT = 3
  );

const
  ULOC_REQUESTED_LOCALE = ULOC_DEPRECATED2 deprecated;

implementation

end.
