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
  unumStatus: UErrorCode;
  fmt: PUNumberFormat;
  Result: WideString;
  bufNeeded: int32;
begin
  unumStatus := U_ZERO_ERROR;
  fmt := unum_open(UNUM_SPELLOUT, nil, 0, 'en-US', nil, unumStatus);
  Check(fmt <> nil, 'unum_open failed: ' + u_errorName(unumStatus));
  try
    bufNeeded := unum_format(fmt, 150, @Result[1], 0, nil, unumStatus);
    Check((unumStatus = U_BUFFER_OVERFLOW_ERROR) or (unumStatus = U_STRING_NOT_TERMINATED_WARNING), 'unum_format 1, unexpected: ' + u_errorName(unumStatus));
    SetLength(Result, bufNeeded);
    unumStatus := U_ZERO_ERROR;
    unum_format(fmt, 150, @Result[1], Length(Result), nil, unumStatus);
    Check(unumStatus = U_STRING_NOT_TERMINATED_WARNING, 'unum_format 1, unexpected: ' + u_errorName(unumStatus));

    CheckEquals('one hundred fifty', Result);
  finally
    unum_close(fmt);
  end;

  unumStatus := U_ZERO_ERROR;
  fmt := unum_open(UNUM_SPELLOUT, nil, 0, 'fr-FR', nil, unumStatus);
  Check(fmt <> nil, 'unum_open failed: ' + u_errorName(unumStatus));
  try
    bufNeeded := unum_format(fmt, 150, @Result[1], 0, nil, unumStatus);
    Check((unumStatus = U_BUFFER_OVERFLOW_ERROR) or (unumStatus = U_STRING_NOT_TERMINATED_WARNING), 'unum_format 1, unexpected: ' + u_errorName(unumStatus));
    SetLength(Result, bufNeeded);
    unumStatus := U_ZERO_ERROR;
    unum_format(fmt, 150, @Result[1], Length(Result), nil, unumStatus);
    Check(unumStatus = U_STRING_NOT_TERMINATED_WARNING, 'unum_format 1, unexpected: ' + u_errorName(unumStatus));

    CheckEquals('cent cinquante', Result);
  finally
    unum_close(fmt);
  end;
end;

initialization

RegisterTest('ICU', TTestUnum.Suite);

end.
