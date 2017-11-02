unit TestICUNumberFormatFunctions;

interface

uses
  System.Classes, System.SysUtils, TestFramework, ICUNumberFormatter, ICUTest;

type
  // Méthodes de test pour les fonctions globales de l'unité ICUNumberFormat

  TestTICUNumberFormat = class(TICUTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCurrencyToStr;
    procedure TestCurrencyToStrDefaultLocale;
    procedure TestCurrencyToStrShort;
    procedure TestDoubleToStr;
    procedure TestStrToDouble;
    procedure TestStrToDoubleDef;
    procedure TestStrToCurrency;
    procedure TestStrToCurrencyDef;
  end;

implementation

{ TestTICUNumberFormat }

uses _uloc;

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

procedure TestTICUNumberFormat.TestCurrencyToStrDefaultLocale;
begin
  CheckEquals('fr_FR', uloc_getDefault);
  CheckEquals('2'#160'145,46'#160'€', ICUCurrencyToStr(2145.456, uloc_getDefault), 'uloc_getDefault');
  CheckEquals('2'#160'145,46'#160'€', ICUCurrencyToStr(2145.456), 'default');
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

  CheckEquals('64,899', ICUDoubleToStr(59 / 0.9091, uloc_getDefault), 'uloc_getDefault');
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

RegisterTest('ICU', TestTICUNumberFormat.Suite);

end.
