unit ICUTest;

interface

uses
  System.SysUtils, System.Classes, TestFramework;

type
  TICUTest = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure CheckEquals(expected, actual: extended; msg: string = ''); override;
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
  CheckEquals(expected, actual, Max(Min(Abs(expected), Abs(actual)) * ExtendedResolution, ExtendedResolution), msg);
end;

procedure TICUTest.SetUp;
begin
  inherited;
  CheckTrue(LoadICU, 'Cannot load ICU');
end;

procedure TICUTest.TearDown;
begin
  // pas besoin de les décharger, l'unité icu_globals s'en charge et ça fait gagner du temps sur les tests
  // UnloadICU;
  inherited;
end;

end.
