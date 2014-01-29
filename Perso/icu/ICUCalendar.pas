unit ICUCalendar;

interface

uses
  System.SysUtils, System.Classes, _ucal, _utypes, icu_globals;

type
  TICUCalendarWrapper = class(TICUObject)
  private
    FCalendar: PUCalendar;
    class function GetDefaultTimeZone: WideString; static;
    class procedure SetDefaultTimeZone(const Value: WideString); static;
  public
    constructor Create(UCalendar: PUCalendar); overload;
    destructor Destroy; override;

    class property DefaultTimeZone: WideString read GetDefaultTimeZone write SetDefaultTimeZone;
  end;

  TICUCalendar = class(TICUCalendarWrapper)
  private
    FTimeZone: WideString;
    FLocale: AnsiString;
    FCalendarType: UCalendarType;
    procedure SetTimeZone(const Value: WideString);
    procedure SetLocale(const Value: AnsiString);
    procedure SetCalendarType(const Value: UCalendarType);
  protected
    procedure BuildCalendar; virtual;
    procedure ReleaseCalendar; virtual;
  public
    destructor Destroy; override;

    property Locale: AnsiString read FLocale write SetLocale;
    property timeZone: WideString read FTimeZone write SetTimeZone;
    property CalendarType: UCalendarType read FCalendarType write SetCalendarType;
  end;

implementation

{ TICUCalendarWrapper }

constructor TICUCalendarWrapper.Create(UCalendar: PUCalendar);
begin
  FCalendar := UCalendar;
end;

destructor TICUCalendarWrapper.Destroy;
begin

  inherited;
end;

class function TICUCalendarWrapper.GetDefaultTimeZone: WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
  ec: UErrorCode;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  ec := U_ZERO_ERROR;
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := ucal_getDefaultTimeZone(@buffer[1], bufNeeded, ec);
  if ec = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ec := U_ZERO_ERROR;
    bufNeeded := ucal_getDefaultTimeZone(@buffer[1], bufNeeded, ec);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

class procedure TICUCalendarWrapper.SetDefaultTimeZone(const Value: WideString);
var
  ec: UErrorCode;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  ec := U_ZERO_ERROR;
  ucal_setDefaultTimeZone(@Value[1], ec);
  ICUCheck(ec);
end;

{ TICUCalendar }

procedure TICUCalendar.BuildCalendar;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  ReleaseCalendar;

  FStatus := U_ZERO_ERROR;
  FCalendar := ucal_open(@FTimeZone[1], Length(FTimeZone), @FLocale[1], FCalendarType, FStatus);
end;

destructor TICUCalendar.Destroy;
begin
  ReleaseCalendar;
  inherited;
end;

procedure TICUCalendar.ReleaseCalendar;
begin
  ucal_close(FCalendar);
end;

procedure TICUCalendar.SetCalendarType(const Value: UCalendarType);
begin
  FCalendarType := Value;
  BuildCalendar;
end;

procedure TICUCalendar.SetLocale(const Value: AnsiString);
begin
  FLocale := Value;
  BuildCalendar;
end;

procedure TICUCalendar.SetTimeZone(const Value: WideString);
begin
  FTimeZone := Value;
  BuildCalendar;
end;

end.
