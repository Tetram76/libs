unit ICUDateFormatter;

{$I icu.inc}

interface

uses
  System.SysUtils, System.Classes, _udat, _utypes, _uloc, _udisplaycontext,
  ICUNumberFormatter;

type
  TICUDateFormatterWrapper = class(TICUObject)
  private
    FFormat: PUDateFormat;
    FNumberFormat: TICUNumberFormatterWrapper;
    function GetLenient: Boolean;
    procedure SetLenient(const Value: Boolean);
  protected
    function GetNumberFormat: TICUNumberFormatterWrapper;
    function GetDateFormat: PUDateFormat;
    procedure ReleaseFormatter; virtual;
  public
    constructor Create(UDateFormat: PUDateFormat); reintroduce;
    destructor Destroy; override;

    function Format(Value: TDateTime): WideString;
    function Parse(const Value: WideString): TDateTime;
    property NumberFormat: TICUNumberFormatterWrapper read GetNumberFormat;

    property Lenient: Boolean read GetLenient write SetLenient;
  end;

  TICUDateFormatter = class(TICUDateFormatterWrapper)
  private
    FLocale: AnsiString;
    FTimeStyle: UDateFormatStyle;
    FDateStyle: UDateFormatStyle;
    FTimeZone: WideString;
    FPattern: WideString;
    procedure SetLocale(const Value: AnsiString);
    procedure SetTimeStyle(const Value: UDateFormatStyle);
    procedure SetDateStyle(const Value: UDateFormatStyle);
    procedure SetTimeZone(const Value: WideString);
    procedure SetPattern(const Value: WideString);
  protected
    procedure BuildFormatter; virtual;
  public
    constructor Create(const Locale: AnsiString = ''; timeStyle: UDateFormatStyle = UDAT_DEFAULT; dateStyle: UDateFormatStyle = UDAT_DEFAULT;
      const timeZone: WideString = ''; const Pattern: WideString = ''); reintroduce;
    destructor Destroy; override;

    property Locale: AnsiString read FLocale write SetLocale;
    property timeStyle: UDateFormatStyle read FTimeStyle write SetTimeStyle;
    property dateStyle: UDateFormatStyle read FDateStyle write SetDateStyle;
    property timeZone: WideString read FTimeZone write SetTimeZone;
    property Pattern: WideString read FPattern write SetPattern;
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
  System.AnsiStrings, icu_globals, _umachine, System.DateUtils, ICULocale;

{ TICUDateFormatterWrapper }

constructor TICUDateFormatterWrapper.Create(UDateFormat: PUDateFormat);
begin
  FFormat := UDateFormat;
end;

destructor TICUDateFormatterWrapper.Destroy;
begin
  ReleaseFormatter;
  inherited;
end;

function TICUDateFormatterWrapper.Format(Value: TDateTime): WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  if FNumberFormat <> nil then
    udat_setNumberFormat(FFormat, FNumberFormat.UNumberFormat);

  ResetErrorCode(FStatus);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := udat_format(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  if FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FStatus);
    bufNeeded := udat_format(FFormat, Value, @buffer[1], bufNeeded, nil, FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUDateFormatterWrapper.GetDateFormat: PUDateFormat;
begin
  Result := FFormat;
end;

function TICUDateFormatterWrapper.GetLenient: Boolean;
begin
  Result := udat_isLenient(FFormat);
end;

function TICUDateFormatterWrapper.GetNumberFormat: TICUNumberFormatterWrapper;
begin
  if FNumberFormat = nil then
    FNumberFormat := TICUNumberFormatterWrapper.Create(udat_getNumberFormat(FFormat));
  Result := FNumberFormat;
end;

function TICUDateFormatterWrapper.Parse(const Value: WideString): TDateTime;
begin
  if FNumberFormat <> nil then
    udat_setNumberFormat(FFormat, FNumberFormat.UNumberFormat);

  ResetErrorCode(FStatus);
  Result := udat_parse(FFormat, @Value[1], Length(Value), nil, FStatus);
end;

procedure TICUDateFormatterWrapper.ReleaseFormatter;
begin
  FreeAndNil(FNumberFormat);
  if FFormat <> nil then
    udat_close(FFormat);
  FFormat := nil;
end;

procedure TICUDateFormatterWrapper.SetLenient(const Value: Boolean);
const
  boolVals: array [False .. True] of Byte = (0, 1);
begin
  udat_setLenient(FFormat, boolVals[Value]);
end;
{ TICUDateFormatter }

constructor TICUDateFormatter.Create(const Locale: AnsiString; timeStyle, dateStyle: UDateFormatStyle; const timeZone, Pattern: WideString);
begin
  inherited Create(nil);
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

procedure TICUDateFormatter.BuildFormatter;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');
  ReleaseFormatter;
  ResetErrorCode(FStatus);
  FFormat := udat_open(timeStyle, dateStyle, @Locale[1], @timeZone[1], Length(FTimeZone), PUChar(Pattern), Length(Pattern), FStatus);
end;

function FormatDateTime(Value: TDateTime; DateFormat, TimeFormat: UDateFormatStyle; LocalToGMT: Boolean; Locale: AnsiString): string;
var
  Formatter: TICUDateFormatter;
  timeZone: string;
begin
  if LocalToGMT then
    timeZone := 'GMT'
  else
    timeZone := TTimeZone.Local.Abbreviation;

  Formatter := TICUDateFormatter.Create(Locale, TimeFormat, DateFormat, timeZone);
  try
    Result := Formatter.Format(TTimeZone.Local.ToUniversalTime(Value));
  finally
    Formatter.Free;
  end;
end;

function ICUDateToStr(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_DEFAULT, UDAT_NONE, LocalToGMT, ProperLocale(Locale));
end;

function ICUDateToStrShort(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_SHORT, UDAT_NONE, LocalToGMT, ProperLocale(Locale));
end;

function ICUDateToStrLong(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_LONG, UDAT_NONE, LocalToGMT, ProperLocale(Locale));
end;

function ICUDateToStrFull(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_FULL, UDAT_NONE, LocalToGMT, ProperLocale(Locale));
end;

function ICUTimeToStr(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_DEFAULT, LocalToGMT, ProperLocale(Locale));
end;

function ICUTimeToStrShort(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_SHORT, LocalToGMT, ProperLocale(Locale));
end;

function ICUTimeToStrLong(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_LONG, LocalToGMT, ProperLocale(Locale));
end;

function ICUTimeToStrFull(const Value: TDateTime; LocalToGMT: Boolean = False; const Locale: AnsiString = ''): string;
begin
  Result := FormatDateTime(Value, UDAT_NONE, UDAT_FULL, LocalToGMT, ProperLocale(Locale));
end;

procedure TICUDateFormatter.SetLocale(const Value: AnsiString);
begin
  FLocale := System.AnsiStrings.Trim(Value);
  BuildFormatter;
end;

procedure TICUDateFormatter.SetTimeStyle(const Value: UDateFormatStyle);
begin
  FTimeStyle := Value;
  BuildFormatter;
end;

procedure TICUDateFormatter.SetDateStyle(const Value: UDateFormatStyle);
begin
  FDateStyle := Value;
  BuildFormatter;
end;

procedure TICUDateFormatter.SetTimeZone(const Value: WideString);
begin
  FTimeZone := Value;
  BuildFormatter;
end;

procedure TICUDateFormatter.SetPattern(const Value: WideString);
begin
  FPattern := Trim(Value);
  BuildFormatter;
end;

end.
