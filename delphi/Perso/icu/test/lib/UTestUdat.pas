unit UTestUdat;

interface

uses
  Classes, SysUtils, TestFramework, ICUTest;

type
  TTestUdat = class(TICUTest)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFormat;
  end;

implementation

uses
  _udat, _utypes, _umachine;

{ TTestUdat }

procedure TTestUdat.SetUp;
begin
  inherited;

end;

procedure TTestUdat.TearDown;
begin
  inherited;

end;

procedure TTestUdat.TestFormat;
var
  Status: UErrorCode;
  fmt: PUDateFormat;
  Result: WideString;
  bufNeeded: Integer;
  dateToFormat: UDate;
  d: TDateTime;
  timeZone: UChar;
begin
  timeZone := 'GMT';
  d := EncodeDate(2015, 3, 25) + EncodeTime(18, 21, 46, 157);
  dateToFormat := d;

  ResetErrorCode(Status);
  fmt := udat_open(UDAT_DEFAULT, UDAT_DEFAULT, 'en-US', @timeZone[1], Length(timeZone), nil, 0, Status);
  Check(U_SUCCESS(Status), 'udat_open failed: ' + u_errorName(Status));
  try
    bufNeeded := udat_format(fmt, dateToFormat, @Result[1], 0, nil, Status);
    Check(U_SUCCESS(Status) or (Status = U_BUFFER_OVERFLOW_ERROR), 'udat_format 1, unexpected: ' + u_errorName(Status));
    SetLength(Result, bufNeeded);
    ResetErrorCode(Status);
    udat_format(fmt, dateToFormat, @Result[1], Length(Result), nil, Status);
    Check(U_SUCCESS(Status), 'udat_format 2, unexpected: ' + u_errorName(Status));

    CheckEquals('Mar 25, 2015, 6:21:46 PM', Result);
  finally
    udat_close(fmt);
  end;

  ResetErrorCode(Status);
  fmt := udat_open(UDAT_DEFAULT, UDAT_DEFAULT, 'fr-FR', @timeZone[1], Length(timeZone), nil, 0, Status);
  Check(U_SUCCESS(Status), 'udat_open failed: ' + u_errorName(Status));
  try
    bufNeeded := udat_format(fmt, dateToFormat, @Result[1], 0, nil, Status);
    Check(U_SUCCESS(Status) or (Status = U_BUFFER_OVERFLOW_ERROR), 'udat_format 1, unexpected: ' + u_errorName(Status));
    SetLength(Result, bufNeeded);
    ResetErrorCode(Status);
    udat_format(fmt, dateToFormat, @Result[1], Length(Result), nil, Status);
    Check(U_SUCCESS(Status), 'udat_format 2, unexpected: ' + u_errorName(Status));

    CheckEquals('25 mars 2015 18:21:46', Result);
  finally
    udat_close(fmt);
  end;
end;

initialization

RegisterTest('ICU', TTestUdat.Suite);

end.
