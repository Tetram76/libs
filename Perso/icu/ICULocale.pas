unit ICULocale;

interface

uses SysUtils, Classes, _uloc;

function ProperLocale(const Locale: AnsiString): AnsiString;

implementation

function ProperLocale(const Locale: AnsiString): AnsiString;
begin
  if Trim(Locale) = '' then
    Result := uloc_getDefault
  else
    Result := Locale;
end;

end.
