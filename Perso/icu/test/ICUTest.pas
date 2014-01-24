unit ICUTest;

interface

uses
  System.SysUtils, System.Classes, TestFramework;

type
  TICUTest = class(TTestCase)
  public
    procedure CheckEquals(expected, actual: extended; msg: string = ''); override;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  icu_globals, System.Math;

{ TICUTest }

procedure TICUTest.CheckEquals(expected, actual: extended; msg: string);
const
  FuzzFactor = 1000;
  SingleResolution = 1E-7 * FuzzFactor;
  DoubleResolution = 1E-15 * FuzzFactor;
{$IFDEF EXTENDEDIS10BYTES}
  ExtendedResolution = 1E-19 * FuzzFactor;
{$ELSE  EXTENDEDIS10BYTES}
  ExtendedResolution = DoubleResolution;
{$ENDIF EXTENDEDIS10BYTES}
begin
  CheckEquals(expected, actual, Max(Min(Abs(expected), Abs(actual)) * ExtendedResolution, ExtendedResolution));
end;

procedure TICUTest.SetUp;
begin
  inherited;
  Check(LoadICU, 'Impossible de charger ICU');
end;

procedure TICUTest.TearDown;
begin
  inherited;

end;

end.
