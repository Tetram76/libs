unit TestICUNumberFormatFunctions;

interface

uses
  System.Classes, System.SysUtils, TestFramework, ICUNumberFormatter;

type
  // Méthodes de test pour les fonctions globales de l'unité ICUNumberFormat

  TestTICUNumberFormat = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckEquals(expected, actual: extended; msg: string = ''); override;
  published
    procedure TestCurrencyToStr;
    procedure TestCurrencyToStrShort;
    procedure TestDoubleToStr;
    procedure TestStrToDouble;
    procedure TestStrToDoubleDef;
    procedure TestStrToCurrency;
    procedure TestStrToCurrencyDef;
  end;

implementation

uses
  System.Math;

{ TestTICUNumberFormat }

procedure TestTICUNumberFormat.CheckEquals(expected, actual: extended; msg: string);
const
  FuzzFactor = 1000;
  SingleResolution   = 1E-7 * FuzzFactor;
  DoubleResolution   = 1E-15 * FuzzFactor;
{$IFDEF EXTENDEDIS10BYTES}
  ExtendedResolution = 1E-19 * FuzzFactor;
{$ELSE  EXTENDEDIS10BYTES}
  ExtendedResolution = DoubleResolution;
{$ENDIF EXTENDEDIS10BYTES}
begin
  CheckEquals(expected, actual, Max(Min(Abs(expected), Abs(actual)) * ExtendedResolution, ExtendedResolution), msg);
end;

procedure TestTICUNumberFormat.SetUp;
begin
  inherited;

end;

procedure TestTICUNumberFormat.TearDown;
begin
  inherited;

end;

procedure TestTICUNumberFormat.TestCurrencyToStr;
begin
  // ATTENTION: icu-fr utilise #160 (espace insécable) au lieu de #32 comme espace
  CheckEquals('2'#160'145,46'#160'€', ICUCurrencyToStr(2145.456, 'fr-FR'), 'fr-FR');
  CheckEquals('$2,145.46', ICUCurrencyToStr(2145.456, 'en-US'), 'en-US');
end;

procedure TestTICUNumberFormat.TestCurrencyToStrShort;
begin
  // ATTENTION: icu-fr utilise #160 (espace insécable) au lieu de #32 comme espace
  CheckEquals('2'#160'145,46', ICUCurrencyToStrShort(2145.456, 'fr-FR'), 'fr-FR');
  CheckEquals('2,145.46', ICUCurrencyToStrShort(2145.456, 'en-US'), 'en-US');
end;

procedure TestTICUNumberFormat.TestDoubleToStr;
begin
  // ATTENTION: icu-fr utilise #160 (espace insécable) au lieu de #32 comme espace
  CheckEquals('2'#160'145,456', ICUDoubleToStr(2145.456, 'fr-FR'), 'fr-FR');
  CheckEquals('2,145.456', ICUDoubleToStr(2145.456, 'en-US'), 'en-US');
end;

procedure TestTICUNumberFormat.TestStrToCurrency;
begin
  CheckEquals(2145.456, ICUStrToCurrency('2 145,456 €', 'fr-FR'), 'fr-FR - "2 145,456 €"');
  CheckEquals(2145.456, ICUStrToCurrency('2'#160'145,456 €', 'fr-FR'), 'fr-FR - "2''#160''145,456 €"');
  CheckEquals(2145.456, ICUStrToCurrency('2145,456 €', 'fr-FR'), 'fr-FR - "2145,456 €"');
  CheckEquals(2145.456, ICUStrToCurrency('$2,145.456', 'en-US'), 'en-US - "$2,145.456"');
end;

procedure TestTICUNumberFormat.TestStrToCurrencyDef;
begin
  CheckEquals(2145.456, ICUStrToCurrencyDef('2 145,456 €', -1, 'fr-FR'), 'fr-FR - "2 145,456 €"');
  CheckEquals(2145.456, ICUStrToCurrencyDef('2'#160'145,456 €', -1, 'fr-FR'), 'fr-FR - "2''#160''145,456 €"');
  CheckEquals(2145.456, ICUStrToCurrencyDef('2145,456 €', -1, 'fr-FR'), 'fr-FR - "2145,456 €"');
  CheckEquals(-1, ICUStrToCurrencyDef('2 145.456 €', -1, 'fr-FR'), 'fr-FR - "2 145.456 €"');
  CheckEquals(2145, ICUStrToCurrencyDef('2 145 € ', -1, 'fr-FR'), 'fr-FR - "2 145 € "');
  CheckEquals(2145.456, ICUStrToCurrencyDef('$2,145.456', -1, 'en-US'), 'en-US - "$2,145.456"');
end;

procedure TestTICUNumberFormat.TestStrToDouble;
begin
  CheckEquals(2145.456, ICUStrToDouble('2 145,456', 'fr-FR'), 'fr-FR - "2 145,456"');
  CheckEquals(2145.456, ICUStrToDouble('2'#160'145,456', 'fr-FR'), 'fr-FR - "2''#160''145,456"');
  CheckEquals(2145.456, ICUStrToDouble('2145,456', 'fr-FR'), 'fr-FR - "2145,456"');
  CheckEquals(2145.456, ICUStrToDouble('2,145.456', 'en-US'), 'en-US - "2,145.456"');
end;

procedure TestTICUNumberFormat.TestStrToDoubleDef;
begin
  CheckEquals(2145.456, ICUStrToDoubleDef('2 145,456', -1, 'fr-FR'), 'fr-FR - "2 145,456"');
  CheckEquals(2145.456, ICUStrToDoubleDef('2'#160'145,456', -1, 'fr-FR'), 'fr-FR - "2''#160''145,456"');
  CheckEquals(2145.456, ICUStrToDoubleDef('2145,456', -1, 'fr-FR'), 'fr-FR - "2145,456"');
  CheckEquals(-1, ICUStrToDoubleDef('2 145.456', -1, 'fr-FR'), 'fr-FR - "2 145.456"');
  CheckEquals(2145, ICUStrToDoubleDef('2 145 ', -1, 'fr-FR'), 'fr-FR - "2 145 "');
  CheckEquals(2145.456, ICUStrToDoubleDef('2,145.456', -1, 'en-US'), 'en-US - "2,145.456"');
end;

initialization

RegisterTest(TestTICUNumberFormat.Suite);

end.
