unit UTestUnum;

interface

uses
  Classes, SysUtils, TestFramework, ICUTest;

type
  TTestUnum = class(TICUTest)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFormat;
  end;

implementation

uses
  _unum, parseerr, _utypes, _umachine, _umisc, icu_globals;

{ TTestUnum }

procedure TTestUnum.SetUp;
begin
  inherited;
end;

procedure TTestUnum.TearDown;
begin
  inherited;
end;

procedure TTestUnum.TestFormat;
var
  Status: UErrorCode;
  fmt: PUNumberFormat;
  Result: WideString;
  bufNeeded: int32;
begin
  ResetErrorCode(Status);
  fmt := unum_open(UNUM_SPELLOUT, nil, 0, 'en-US', nil, Status);
  Check(U_SUCCESS(Status), 'unum_open failed: ' + u_errorName(Status));
  try
    bufNeeded := unum_format(fmt, 150, @Result[1], 0, nil, Status);
    Check(U_SUCCESS(Status) or (Status = U_BUFFER_OVERFLOW_ERROR), 'unum_format 1, unexpected: ' + u_errorName(Status));
    SetLength(Result, bufNeeded);
    ResetErrorCode(Status);
    unum_format(fmt, 150, @Result[1], Length(Result), nil, Status);
    Check(U_SUCCESS(Status), 'unum_format 2, unexpected: ' + u_errorName(Status));

    CheckEquals('one hundred fifty', Result);
  finally
    unum_close(fmt);
  end;

  ResetErrorCode(Status);
  fmt := unum_open(UNUM_SPELLOUT, nil, 0, 'fr-FR', nil, Status);
  Check(U_SUCCESS(Status), 'unum_open failed: ' + u_errorName(Status));
  try
    bufNeeded := unum_format(fmt, 150, @Result[1], 0, nil, Status);
    Check(U_SUCCESS(Status) or (Status = U_BUFFER_OVERFLOW_ERROR), 'unum_format 1, unexpected: ' + u_errorName(Status));
    SetLength(Result, bufNeeded);
    ResetErrorCode(Status);
    unum_format(fmt, 150, @Result[1], Length(Result), nil, Status);
    Check(U_SUCCESS(Status), 'unum_format 2, unexpected: ' + u_errorName(Status));

    CheckEquals('cent cinquante', Result);
  finally
    unum_close(fmt);
  end;
end;

initialization

RegisterTest('ICU', TTestUnum.Suite);

end.
