unit ICUDateFormatter;

{$I icu.inc}

interface

uses
  System.SysUtils, System.Classes, _udat, _utypes, _uloc, _udisplaycontext,
  ICUNumberFormatter;

type
  TICUDateFormatter = class
  private
    FFormat: PUDateFormat;
    FStatus: UErrorCode;
    FLocale: AnsiString;
    FTimeStyle: UDateFormatStyle;
    FDateStyle: UDateFormatStyle;
    FTimeZone: WideString;
    FPattern: WideString;
    procedure SetLocale(const Value: AnsiString);
    procedure SetDateStyle(const Value: UDateFormatStyle);
    procedure SetTimeZone(const Value: WideString);
    procedure SetPattern(const Value: WideString);
    procedure SetTimeStyle(const Value: UDateFormatStyle);
    function GetLenient: Boolean;
    procedure SetLenient(const Value: Boolean);
  protected
    procedure BuildFormatter; virtual;
    procedure ReleaseFormatter; virtual;
  public
  public
    constructor Create(Locale: AnsiString; timeStyle, dateStyle: UDateFormatStyle; timeZone: WideString = ''; Pattern: WideString = '');
    destructor Destroy; override;

    function GetErrorCode: UErrorCode;
    function GetErrorMessage: AnsiString;

    function Format(Value: TDateTime): WideString;
    function Parse(const Value: WideString): TDateTime;

    property Locale: AnsiString read FLocale write SetLocale;
    property timeStyle: UDateFormatStyle read FTimeStyle write SetTimeStyle;
    property dateStyle: UDateFormatStyle read FDateStyle write SetDateStyle;
    property timeZone: WideString read FTimeZone write SetTimeZone;
    property Pattern: WideString read FPattern write SetPattern;

    property Lenient: Boolean read GetLenient write SetLenient;
  end;

function ICUDateToStr(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUDateToStrFull(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUDateToStrShort(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUDateToStrLong(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUTimeToStr(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUTimeToStrFull(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUTimeToStrShort(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
function ICUTimeToStrLong(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;

implementation

uses
  System.AnsiStrings, icu_globals, _umachine;

{ TICUDateFormatter }

procedure TICUDateFormatter.BuildFormatter;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  if FFormat <> nil then
    ReleaseFormatter;

  FStatus := U_ZERO_ERROR;
  FFormat := udat_open(timeStyle, dateStyle, @Locale[1], @timeZone[1], Length(FTimeZone), PUChar(Pattern), Length(Pattern), FStatus);
end;

constructor TICUDateFormatter.Create(Locale: AnsiString; timeStyle, dateStyle: UDateFormatStyle; timeZone: WideString; Pattern: WideString);
begin
  FLocale := Locale;
  FTimeStyle := timeStyle;
  FDateStyle := dateStyle;
  FTimeZone := timeZone;
  FPattern := Pattern;
  BuildFormatter;
end;

destructor TICUDateFormatter.Destroy;
begin

  inherited;
end;

function TICUDateFormatter.Format(Value: TDateTime): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  FStatus := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := udat_format(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    FStatus := U_ZERO_ERROR;
    bufNeeded := udat_format(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUDateFormatter.GetErrorCode: UErrorCode;
begin
  Result := FStatus;
end;

function TICUDateFormatter.GetErrorMessage: AnsiString;
begin
  Result := u_errorName(FStatus);
end;

function TICUDateFormatter.GetLenient: Boolean;
begin
  Result := udat_isLenient(FFormat) <> 0;
end;

function TICUDateFormatter.Parse(const Value: WideString): TDateTime;
begin
  FStatus := U_ZERO_ERROR;
  Result := udat_parse(FFormat, @Value[1], Length(Value), nil, FStatus);
end;

procedure TICUDateFormatter.ReleaseFormatter;
begin
  udat_close(FFormat);
  FFormat := nil;
end;

procedure TICUDateFormatter.SetDateStyle(const Value: UDateFormatStyle);
begin
  FDateStyle := Value;
  BuildFormatter;
end;

procedure TICUDateFormatter.SetLenient(const Value: Boolean);
const
  boolVals: array [False .. True] of Byte = (0, 1);
begin
  udat_setLenient(FFormat, boolVals[Value]);
end;

procedure TICUDateFormatter.SetLocale(const Value: AnsiString);
begin
  FLocale := System.AnsiStrings.Trim(Value);
  BuildFormatter;
end;

procedure TICUDateFormatter.SetPattern(const Value: WideString);
begin
  FPattern := Trim(Value);
  BuildFormatter;
end;

procedure TICUDateFormatter.SetTimeStyle(const Value: UDateFormatStyle);
begin
  FTimeStyle := Value;
  BuildFormatter;
end;

procedure TICUDateFormatter.SetTimeZone(const Value: WideString);
begin
  FTimeZone := Value;
  BuildFormatter;
end;

function FormatDateTime(Value: TDateTime; DateFormat, TimeFormat: UDateFormatStyle; LocalToGMT: Boolean; Locale: AnsiString): string;
var
  Formatter: TICUDateFormatter;
  timeZone: string;
begin
  if LocalToGMT then
    timeZone := 'GMT+1'
  else
    timeZone := '';

  Formatter := TICUDateFormatter.Create(Locale, TimeFormat, DateFormat, timeZone);
  try
    Result := Formatter.Format(Value);
  finally
    Formatter.Free;
  end;
end;

function ICUDateToStr(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_DEFAULT, UDAT_NONE, LocalToGMT, Locale);
end;

function ICUDateToStrShort(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_SHORT, UDAT_NONE, LocalToGMT, Locale);
end;

function ICUDateToStrLong(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_LONG, UDAT_NONE, LocalToGMT, Locale);
end;

function ICUDateToStrFull(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_FULL, UDAT_NONE, LocalToGMT, Locale);
end;

function ICUTimeToStr(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_DEFAULT, LocalToGMT, Locale);
end;

function ICUTimeToStrShort(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_SHORT, LocalToGMT, Locale);
end;

function ICUTimeToStrLong(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_LONG, LocalToGMT, Locale);
end;

function ICUTimeToStrFull(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_FULL, LocalToGMT, Locale);
end;

end.
