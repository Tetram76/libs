unit ICUCalendar;

interface

uses
  System.SysUtils, System.Classes, _ucal, _utypes, icu_globals, _uloc;

type
  TICUCalendarWrapper = class(TICUObject)
  private type
    TICUCalendarChild = class
    private
      FCalendar: TICUCalendarWrapper;
    public
      constructor Create(Calendar: TICUCalendarWrapper);
    end;

  private type
    TAttributes = class(TICUCalendarChild)
    private
      function GetAttribute(const Index: UCalendarAttribute): Int32;
      procedure SetAttribute(const Index: UCalendarAttribute; const Value: Int32);
    public
      property isLenient: Int32 index UCAL_LENIENT read GetAttribute write SetAttribute;
      property FirstDayOfWeek: Int32 index UCAL_FIRST_DAY_OF_WEEK read GetAttribute write SetAttribute;
      property MinimalDaysInFirstWeek: Int32 index UCAL_MINIMAL_DAYS_IN_FIRST_WEEK read GetAttribute write SetAttribute;
      property RepeatedWallTime: Int32 index UCAL_REPEATED_WALL_TIME read GetAttribute write SetAttribute;
      property SkippedWallTime: Int32 index UCAL_SKIPPED_WALL_TIME read GetAttribute write SetAttribute;
    end;

  private type
    TTimeZone = class(TICUCalendarChild)
    private
      function GetDisplayName(const Index: UCalendarDisplayNameType): WideString;
      function GetId: WideString;
      procedure SetId(const Value: WideString);
    public
      property Id: WideString read GetId write SetId;
      property StandardDisplayName: WideString index UCAL_STANDARD read GetDisplayName;
      property StandardShortDisplayName: WideString index UCAL_SHORT_STANDARD read GetDisplayName;
      property DSTDisplayName: WideString index UCAL_DST read GetDisplayName;
      property DSTShortDisplayName: WideString index UCAL_SHORT_DST read GetDisplayName;
    end;

  private type
    TAbstractDateField = class(TICUCalendarChild)
    private
      FIndexValue: UCalendarDateFields;
    public
      constructor Create(Calendar: TICUCalendarWrapper; Index: UCalendarDateFields); reintroduce;
    end;

  private type
    TDateFieldLimit = class(TAbstractDateField)
    private
      function GetLimit(const Index: UCalendarLimitType): Int32;
    public
      property ActualMinimum: Int32 index UCAL_ACTUAL_MINIMUM read GetLimit;
      property ActualMaximum: Int32 index UCAL_ACTUAL_MAXIMUM read GetLimit;
      property CommonMinimum: Int32 index UCAL_ACTUAL_MINIMUM read GetLimit;
      property CommonMaximum: Int32 index UCAL_ACTUAL_MAXIMUM read GetLimit;
      property AbsolutMinimum: Int32 index UCAL_GREATEST_MINIMUM read GetLimit;
      property AbsolutMaximum: Int32 index UCAL_LEAST_MAXIMUM read GetLimit;
    end;

  private type
    TDateField<T> = class(TAbstractDateField)
    private
      FLimits: TDateFieldLimit;
      function GetValueGeneric<TD>: TD;
      procedure SetValueGeneric<TD>(const Value: TD);
      function GetValue: T; virtual;
      procedure SetValue(const Value: T); virtual;
      function GetIsSet: Boolean;
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;

      procedure Clear;
      procedure Add(Value: Int32);

      property Value: T read GetValue write SetValue;
      property IsSet: Boolean read GetIsSet;
      property Limits: TDateFieldLimit read FLimits;
    end;

  private type
    TBoolDateField = class(TDateField<Boolean>)
    private
      function GetValue: Boolean; override;
      procedure SetValue(const Value: Boolean); override;
    end;

  private type
    TDateFields = class(TICUCalendarChild)
    private
      FFields: array [UCalendarDateFields] of TAbstractDateField;
      function GetField(const Index: UCalendarDateFields): TDateField<Int32>;
      function GetMonthField(const Index: UCalendarDateFields): TDateField<UCalendarMonths>;
      function GetWeekField(const Index: UCalendarDateFields): TDateField<UCalendarDaysOfWeek>;
      function GetAMPMField(const Index: UCalendarDateFields): TDateField<UCalendarAMPMs>;
      function GetBoolField(const Index: UCalendarDateFields): TBoolDateField;
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;

      procedure Clear;

      property Era: TDateField<Int32> index UCAL_ERA read GetField;
      property Year: TDateField<Int32> index UCAL_YEAR read GetField;
      property Month: TDateField<UCalendarMonths> index UCAL_MONTH read GetMonthField;
      property WeekOfYear: TDateField<Int32> index UCAL_WEEK_OF_YEAR read GetField;
      property WeekOfMonth: TDateField<Int32> index UCAL_WEEK_OF_MONTH read GetField;
      property DayOfMonth: TDateField<Int32> index UCAL_DATE read GetField;
      property DayOfYear: TDateField<Int32> index UCAL_DAY_OF_YEAR read GetField;
      property DayOfWeek: TDateField<UCalendarDaysOfWeek> index UCAL_DAY_OF_WEEK read GetWeekField;
      property DayOfWeekInMonth: TDateField<Int32> index UCAL_DAY_OF_WEEK_IN_MONTH read GetField;
      property AMPM: TDateField<UCalendarAMPMs> index UCAL_AM_PM read GetAMPMField;
      property Hour: TDateField<Int32> index UCAL_HOUR read GetField;
      property HourOfDay: TDateField<Int32> index UCAL_HOUR_OF_DAY read GetField;
      property Minute: TDateField<Int32> index UCAL_MINUTE read GetField;
      property Second: TDateField<Int32> index UCAL_SECOND read GetField;
      property Millisecond: TDateField<Int32> index UCAL_MILLISECOND read GetField;
      property ZoneOffset: TDateField<Int32> index UCAL_ZONE_OFFSET read GetField;
      property DSTOffset: TDateField<Int32> index UCAL_DST_OFFSET read GetField;
      property YearWoy: TDateField<Int32> index UCAL_YEAR_WOY read GetField;
      property DowLocal: TDateField<Int32> index UCAL_DOW_LOCAL read GetField;
      property ExtendedYear: TDateField<Int32> index UCAL_EXTENDED_YEAR read GetField;
      property JulianDay: TDateField<Int32> index UCAL_JULIAN_DAY read GetField;
      property MillisecondsInDay: TDateField<Int32> index UCAL_MILLISECONDS_IN_DAY read GetField;
      property IsLeapMonth: TBoolDateField index UCAL_IS_LEAP_MONTH read GetBoolField;
    end;

  private
    class function GetDefaultTimeZone: WideString; static;
    class procedure SetDefaultTimeZone(const Value: WideString); static;
  private
    FCalendar: PUCalendar;
    FAttributes: TAttributes;
    FTimeZone: TTimeZone;
    FDateFields: TDateFields;
    function GetLocale(const Index: ULocDataLocaleType): AnsiString;
    function GetValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
    function GetInDaylightTime: Boolean;
    function GetWeekdayType: UCalendarWeekdayType;
    function GetIsWeekEnd: Boolean;
    function GetWeekendTransition: Int32;
    function GetTransitionDate(TransitionType: UTimeZoneTransitionType): TDateTime;
  protected
    procedure ReleaseCalendar; virtual;
  public
    constructor Create(UCalendar: PUCalendar); reintroduce;
    destructor Destroy; override;

    function IsEquivalentTo(Calendar: TICUCalendarWrapper): Boolean;
    function GetNextTransitionDate(ExcludeCurrentDate: Boolean): TDateTime;
    function GetPreviousTransitionDate(ExcludeCurrentDate: Boolean): TDateTime;
    function SpanWith(TargetDate: TDateTime; ResultUnit: UCalendarDateFields): Int32;

    property ActualLocale: AnsiString index ULOC_ACTUAL_LOCALE read GetLocale;
    property ValidLocale: AnsiString index ULOC_VALID_LOCALE read GetLocale;

    property Date: TDateTime read GetValue write SetValue;
    property TimeZone: TTimeZone read FTimeZone;
    property InDaylightTime: Boolean read GetInDaylightTime;
    property IsWeekEnd: Boolean read GetIsWeekEnd;
    property WeekdayType: UCalendarWeekdayType read GetWeekdayType;
    property WeekendTransition: Int32 read GetWeekendTransition;

    property Attributes: TAttributes read FAttributes;
    property DateFields: TDateFields read FDateFields;
    class property DefaultTimeZone: WideString read GetDefaultTimeZone write SetDefaultTimeZone;
  end;

  TICUCalendar = class(TICUCalendarWrapper)
  private
    FLocale: AnsiString;
    FCalendarType: UCalendarType;
    procedure SetLocale(const Value: AnsiString);
    procedure SetCalendarType(const Value: UCalendarType);
  protected
    procedure BuildCalendar; virtual;
  public
    constructor Create(const Locale: AnsiString = ''; CalendarType: UCalendarType = UCAL_DEFAULT); reintroduce;
    destructor Destroy; override;

    property Locale: AnsiString read FLocale write SetLocale;
    property CalendarType: UCalendarType read FCalendarType write SetCalendarType;
  end;

function ICUIsWeekend(Locale: AnsiString; Date: TDateTime): Boolean;
function ICUWeekdayType(Locale: AnsiString; Day: UCalendarDaysOfWeek): UCalendarWeekdayType;
function ICUWeekendTransition(Locale: AnsiString; Day: UCalendarDaysOfWeek): Int32;

implementation

uses
  Winapi.Windows, _umachine, ICULocale;

{ TICUCalendarWrapper.TICUNumberFormatterChild }

constructor TICUCalendarWrapper.TICUCalendarChild.Create(Calendar: TICUCalendarWrapper);
begin
  FCalendar := Calendar;
end;

{ TICUCalendarWrapper.TAttributes }

function TICUCalendarWrapper.TAttributes.GetAttribute(const Index: UCalendarAttribute): Int32;
begin
  Result := ucal_getAttribute(FCalendar.FCalendar, Index);
end;

procedure TICUCalendarWrapper.TAttributes.SetAttribute(const Index: UCalendarAttribute; const Value: Int32);
begin
  ucal_setAttribute(FCalendar.FCalendar, Index, Value);
end;

{ TICUCalendarWrapper.TTimeZone }

function TICUCalendarWrapper.TTimeZone.GetDisplayName(const Index: UCalendarDisplayNameType): WideString;
begin

end;

function TICUCalendarWrapper.TTimeZone.GetId: WideString;
var
  buffer: WideString;
  bufNeeded: Int32;
begin
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  ResetErrorCode(FCalendar.FStatus);
  bufNeeded := ucal_getTimeZoneID(FCalendar.FCalendar, @buffer[1], bufNeeded, FCalendar.FStatus);
  if FCalendar.FStatus = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(FCalendar.FStatus);
    bufNeeded := ucal_getTimeZoneID(FCalendar.FCalendar, @buffer[1], bufNeeded, FCalendar.FStatus);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

procedure TICUCalendarWrapper.TTimeZone.SetId(const Value: WideString);
begin
  ResetErrorCode(FCalendar.FStatus);
  ucal_setTimeZone(FCalendar.FCalendar, @Value[1], Length(Value), FCalendar.FStatus);
end;

{ TICUCalendarWrapper.TAbstractDateField }

constructor TICUCalendarWrapper.TAbstractDateField.Create(Calendar: TICUCalendarWrapper; Index: UCalendarDateFields);
begin
  inherited Create(Calendar);
  FIndexValue := Index;
end;

{ TICUCalendarWrapper.TDateFieldLimit }

function TICUCalendarWrapper.TDateFieldLimit.GetLimit(const Index: UCalendarLimitType): Int32;
begin
  ResetErrorCode(FCalendar.FStatus);
  Result := ucal_getLimit(FCalendar.FCalendar, FIndexValue, Index, FCalendar.FStatus);
end;

{ TICUCalendarWrapper.TDateField<T> }

procedure TICUCalendarWrapper.TDateField<T>.Add(Value: Int32);
begin
  ResetErrorCode(FCalendar.FStatus);
  ucal_add(FCalendar.FCalendar, FIndexValue, Value, FCalendar.FStatus);
end;

procedure TICUCalendarWrapper.TDateField<T>.AfterConstruction;
begin
  inherited;
  FLimits := TDateFieldLimit.Create(FCalendar, FIndexValue);
end;

procedure TICUCalendarWrapper.TDateField<T>.BeforeDestruction;
begin
  inherited;
  FLimits.Free;
end;

procedure TICUCalendarWrapper.TDateField<T>.Clear;
begin
  ucal_clearField(FCalendar.FCalendar, FIndexValue);
end;

function TICUCalendarWrapper.TDateField<T>.GetIsSet: Boolean;
begin
  Result := ucal_isSet(FCalendar.FCalendar, FIndexValue);
end;

function TICUCalendarWrapper.TDateField<T>.GetValue: T;
begin
  Result := GetValueGeneric<T>;
end;

function TICUCalendarWrapper.TDateField<T>.GetValueGeneric<TD>: TD;
var
  v: Int32;
begin
  ResetErrorCode(FCalendar.FStatus);
  v := ucal_get(FCalendar.FCalendar, FIndexValue, FCalendar.FStatus);
  CopyMemory(@Result, @v, SizeOf(TD));
end;

procedure TICUCalendarWrapper.TDateField<T>.SetValue(const Value: T);
begin
  SetValueGeneric<T>(Value);
end;

procedure TICUCalendarWrapper.TDateField<T>.SetValueGeneric<TD>(const Value: TD);
var
  v: Int32;
begin
  CopyMemory(@v, @Value, SizeOf(T));
  ucal_set(FCalendar.FCalendar, FIndexValue, v);
end;

{ TICUCalendarWrapper.TBoolDateField }

function TICUCalendarWrapper.TBoolDateField.GetValue: Boolean;
begin
  Result := inherited GetValueGeneric<UBool>;
end;

procedure TICUCalendarWrapper.TBoolDateField.SetValue(const Value: Boolean);
begin
  inherited SetValueGeneric<UBool>(Value);
end;

{ TICUCalendarWrapper.TDateFields }

procedure TICUCalendarWrapper.TDateFields.AfterConstruction;
var
  f: UCalendarDateFields;
begin
  inherited;
  for f := Low(UCalendarDateFields) to High(UCalendarDateFields) do
  begin
    case f of
      UCAL_MONTH:
        FFields[f] := TDateField<UCalendarMonths>.Create(FCalendar, f);
      UCAL_DAY_OF_WEEK:
        FFields[f] := TDateField<UCalendarDaysOfWeek>.Create(FCalendar, f);
      UCAL_AM_PM:
        FFields[f] := TDateField<UCalendarAMPMs>.Create(FCalendar, f);
      UCAL_IS_LEAP_MONTH:
        FFields[f] := TBoolDateField.Create(FCalendar, f);
      UCAL_FIELD_COUNT:
        FFields[f] := nil;
    else
      FFields[f] := TDateField<Int32>.Create(FCalendar, f);
    end;
  end;
end;

procedure TICUCalendarWrapper.TDateFields.BeforeDestruction;
var
  f: TAbstractDateField;
begin
  inherited;
  for f in FFields do
    f.Free;
end;

procedure TICUCalendarWrapper.TDateFields.Clear;
begin
  ucal_clear(FCalendar.FCalendar);
end;

function TICUCalendarWrapper.TDateFields.GetAMPMField(const Index: UCalendarDateFields): TDateField<UCalendarAMPMs>;
begin
  Result := TDateField<UCalendarAMPMs>(FFields[Index]);
end;

function TICUCalendarWrapper.TDateFields.GetBoolField(const Index: UCalendarDateFields): TBoolDateField;
begin
  Result := TBoolDateField(FFields[Index]);
end;

function TICUCalendarWrapper.TDateFields.GetField(const Index: UCalendarDateFields): TDateField<Int32>;
begin
  Result := TDateField<Int32>(FFields[Index]);
end;

function TICUCalendarWrapper.TDateFields.GetMonthField(const Index: UCalendarDateFields): TDateField<UCalendarMonths>;
begin
  Result := TDateField<UCalendarMonths>(FFields[Index]);
end;

function TICUCalendarWrapper.TDateFields.GetWeekField(const Index: UCalendarDateFields): TDateField<UCalendarDaysOfWeek>;
begin
  Result := TDateField<UCalendarDaysOfWeek>(FFields[Index]);
end;

{ TICUCalendarWrapper }

constructor TICUCalendarWrapper.Create(UCalendar: PUCalendar);
begin
  FCalendar := UCalendar;
  FAttributes := TAttributes.Create(Self);
  FTimeZone := TTimeZone.Create(Self);
  FDateFields := TDateFields.Create(Self);
end;

destructor TICUCalendarWrapper.Destroy;
begin
  FAttributes.Free;
  FTimeZone.Free;
  FDateFields.Free;
  ReleaseCalendar;
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

  ResetErrorCode(ec);
  bufNeeded := DEFAULT_BUFFER_SIZE;
  SetLength(buffer, bufNeeded);
  bufNeeded := ucal_getDefaultTimeZone(@buffer[1], bufNeeded, ec);
  if ec = U_BUFFER_OVERFLOW_ERROR then
  begin
    SetLength(buffer, bufNeeded);
    ResetErrorCode(ec);
    bufNeeded := ucal_getDefaultTimeZone(@buffer[1], bufNeeded, ec);
  end;

  SetLength(buffer, bufNeeded);
  Result := buffer;
end;

function TICUCalendarWrapper.GetInDaylightTime: Boolean;
begin
  ResetErrorCode(FStatus);
  Result := ucal_inDaylightTime(FCalendar, FStatus);
end;

function TICUCalendarWrapper.GetLocale(const Index: ULocDataLocaleType): AnsiString;
begin
  ResetErrorCode(FStatus);
  Result := ucal_getLocaleByType(FCalendar, Index, FStatus);
end;

function TICUCalendarWrapper.GetNextTransitionDate(ExcludeCurrentDate: Boolean): TDateTime;
begin
  if ExcludeCurrentDate then
    Result := GetTransitionDate(UCAL_TZ_TRANSITION_NEXT)
  else
    Result := GetTransitionDate(UCAL_TZ_TRANSITION_NEXT_INCLUSIVE);
end;

function TICUCalendarWrapper.GetPreviousTransitionDate(ExcludeCurrentDate: Boolean): TDateTime;
begin
  if ExcludeCurrentDate then
    Result := GetTransitionDate(UCAL_TZ_TRANSITION_PREVIOUS)
  else
    Result := GetTransitionDate(UCAL_TZ_TRANSITION_PREVIOUS_INCLUSIVE);
end;

function TICUCalendarWrapper.GetTransitionDate(TransitionType: UTimeZoneTransitionType): TDateTime;
var
  v: UDate;
begin
  Result := -1;
  ResetErrorCode(FStatus);
  if Boolean(ucal_getTimeZoneTransitionDate(FCalendar, TransitionType, v, FStatus)) then
    Result := v;
end;

function TICUCalendarWrapper.GetValue: TDateTime;
begin
  ResetErrorCode(FStatus);
  Result := ucal_getMillis(FCalendar, FStatus);
end;

function TICUCalendarWrapper.GetWeekdayType: UCalendarWeekdayType;
begin
  ResetErrorCode(FStatus);
  Result := ucal_getDayOfWeekType(FCalendar, DateFields.DayOfWeek.Value, FStatus);
end;

function TICUCalendarWrapper.GetWeekendTransition: Int32;
begin
  ResetErrorCode(FStatus);
  Result := ucal_getWeekendTransition(FCalendar, DateFields.DayOfWeek.Value, FStatus);
end;

function TICUCalendarWrapper.IsEquivalentTo(Calendar: TICUCalendarWrapper): Boolean;
begin
  Result := ucal_equivalentTo(FCalendar, Calendar.FCalendar);
end;

procedure TICUCalendarWrapper.ReleaseCalendar;
begin
  ucal_close(FCalendar);
end;

function TICUCalendarWrapper.GetIsWeekEnd: Boolean;
begin
  ResetErrorCode(FStatus);
  Result := ucal_isWeekend(FCalendar, Date, FStatus);
end;

class procedure TICUCalendarWrapper.SetDefaultTimeZone(const Value: WideString);
var
  ec: UErrorCode;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  ResetErrorCode(ec);
  ucal_setDefaultTimeZone(@Value[1], ec);
  ICUCheck(ec);
end;

procedure TICUCalendarWrapper.SetValue(const Value: TDateTime);
begin
  ResetErrorCode(FStatus);
  ucal_setMillis(FCalendar, Value, FStatus);
end;

function TICUCalendarWrapper.SpanWith(TargetDate: TDateTime; ResultUnit: UCalendarDateFields): Int32;
var
  calClone: PUCalendar;
  tmpCal: TICUCalendarWrapper;
begin
  ResetErrorCode(FStatus);
  ucal_clone(FCalendar, FStatus);
  ICUCheck(FStatus);
  tmpCal := TICUCalendarWrapper.Create(calClone);
  try
    Result := ucal_getFieldDifference(tmpCal.FCalendar, TargetDate, ResultUnit, tmpCal.FStatus);
  finally
    tmpCal.Free;
  end;
end;

{ TICUCalendar }

procedure TICUCalendar.BuildCalendar;
begin
  if not(IsICULoaded or LoadICU) then
    raise Exception.Create('Impossible de charger ICU');

  ReleaseCalendar;

  ResetErrorCode(FStatus);
  FCalendar := ucal_open(nil, 0, @FLocale[1], FCalendarType, FStatus);
end;

constructor TICUCalendar.Create(const Locale: AnsiString; CalendarType: UCalendarType);
begin
  inherited Create(nil);
  Self.Locale := Locale;
  Self.CalendarType := CalendarType;
  BuildCalendar;
end;

destructor TICUCalendar.Destroy;
begin
  inherited;
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

function ICUIsWeekend(Locale: AnsiString; Date: TDateTime): Boolean;
var
  cal: TICUCalendar;
begin
  cal := TICUCalendar.Create(ProperLocale(Locale), UCAL_DEFAULT);
  try
    ResetErrorCode(cal.FStatus);
    Result := ucal_isWeekend(cal.FCalendar, Date, cal.FStatus);
    ICUCheck(cal.FStatus);
  finally
    cal.Free;
  end;
end;

function ICUWeekdayType(Locale: AnsiString; Day: UCalendarDaysOfWeek): UCalendarWeekdayType;
var
  cal: TICUCalendar;
begin
  cal := TICUCalendar.Create(ProperLocale(Locale), UCAL_DEFAULT);
  try
    ResetErrorCode(cal.FStatus);
    Result := ucal_getDayOfWeekType(cal.FCalendar, Day, cal.FStatus);
    ICUCheck(cal.FStatus);
  finally
    cal.Free;
  end;
end;

function ICUWeekendTransition(Locale: AnsiString; Day: UCalendarDaysOfWeek): Int32;
var
  cal: TICUCalendar;
begin
  cal := TICUCalendar.Create(ProperLocale(Locale), UCAL_DEFAULT);
  try
    ResetErrorCode(cal.FStatus);
    Result := ucal_getWeekendTransition(cal.FCalendar, Day, cal.FStatus);
    ICUCheck(cal.FStatus);
  finally
    cal.Free;
  end;
end;

end.
