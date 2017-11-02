unit TestICUNumberFormat;

interface

uses
  System.Classes, System.SysUtils, TestFramework, ICUNumberFormatter, _unum,
  ICUTest;

type
  // Méthodes de test pour la classe TICUNumberFormat

  TestTICUNumberFormat = class(TICUTest)
  strict private
    FICUNumberFormat: TICUNumberFormatter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFormatNaturalInteger;
    procedure TestFormatNaturalDouble;
    procedure TestFormatNaturalCurrency;
    procedure TestFormatNaturalCurrencyCode;
    procedure TestFormatCurrency;
    procedure TestSetSymbol;
    procedure TestParseAsDecimal;

    procedure TestSymbols;
  end;

implementation

procedure TestTICUNumberFormat.SetUp;
begin
  inherited;
end;

procedure TestTICUNumberFormat.TearDown;
begin
  inherited;
end;

procedure TestTICUNumberFormat.TestFormatNaturalInteger;
var
  Value: Integer;
begin
  Value := 153;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_SPELLOUT);
  try
    CheckEquals('cent cinquante-trois', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;

  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_SPELLOUT);
  try
    CheckEquals('one hundred fifty-three', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
end;

procedure TestTICUNumberFormat.TestParseAsDecimal;
begin
  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_SPELLOUT);
  CheckEquals('NaN', string(FICUNumberFormat.ParseDecimal('NaN')));
  CheckEquals('153.45', string(FICUNumberFormat.ParseDecimal('153,45')));
end;

procedure TestTICUNumberFormat.TestSetSymbol;
var
  Value: Currency;
begin
  Value := 153.45;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_CURRENCY);
  try
    FICUNumberFormat.Symbols.OneDigit := 'A';
    FICUNumberFormat.Symbols.TwoDigit := 'B';
    FICUNumberFormat.Symbols.ThreeDigit := 'C';
    FICUNumberFormat.Symbols.FourDigit := 'D';
    FICUNumberFormat.Symbols.FiveDigit := 'E';
    CheckEquals('AEC,DE €', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_CURRENCY);
  try
    FICUNumberFormat.Symbols.OneDigit := 'A';
    FICUNumberFormat.Symbols.TwoDigit := 'B';
    FICUNumberFormat.Symbols.ThreeDigit := 'C';
    FICUNumberFormat.Symbols.FourDigit := 'D';
    FICUNumberFormat.Symbols.FiveDigit := 'E';
    CheckEquals('$AEC.DE', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_CURRENCY);
  try
    FICUNumberFormat.Symbols.Currency := '*';
    CheckEquals('153,45 *', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_CURRENCY);
  try
    FICUNumberFormat.Symbols.Currency := '%';
    CheckEquals('%153.45', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
end;

procedure TestTICUNumberFormat.TestSymbols;
begin
  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_CURRENCY);
  try
    CheckEquals(FICUNumberFormat.Symbols.GroupingSeparator, FICUNumberFormat.Symbols.MonetaryGroupingSeparator);
    FICUNumberFormat.Symbols.MonetaryGroupingSeparator := '_';
    CheckNotEquals(FICUNumberFormat.Symbols.GroupingSeparator, FICUNumberFormat.Symbols.MonetaryGroupingSeparator);
  finally
    FICUNumberFormat.Free;
  end;
end;

procedure TestTICUNumberFormat.TestFormatNaturalDouble;
var
  Value: Double;
begin
  Value := 153.459;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_SPELLOUT);
  try
    CheckEquals('cent cinquante-trois virgule quatre cinq neuf', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_SPELLOUT);
  try
    CheckEquals('one hundred fifty-three point four five nine', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
end;

procedure TestTICUNumberFormat.TestFormatCurrency;
var
  Value: Currency;
begin
  Value := 153.45;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_CURRENCY);
  try
    CheckEquals('153,45 €', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_CURRENCY);
  try
    CheckEquals('$153.45', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_CURRENCY);
  try
    CheckEquals('153,45 $US', FICUNumberFormat.Format(Value, 'USD'));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_CURRENCY);
  try
    CheckEquals('€153.45', FICUNumberFormat.Format(Value, 'EUR'));
  finally
    FICUNumberFormat.Free;
  end;
end;

procedure TestTICUNumberFormat.TestFormatNaturalCurrency;
var
  Value: Currency;
begin
  Value := 153.45;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_SPELLOUT);
  try
    CheckEquals('cent cinquante-trois virgule quatre cinq', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_SPELLOUT);
  try
    CheckEquals('one hundred fifty-three point four five', FICUNumberFormat.Format(Value));
  finally
    FICUNumberFormat.Free;
  end;
end;

procedure TestTICUNumberFormat.TestFormatNaturalCurrencyCode;
var
  Value: Currency;
begin
  Value := 153.45;

  FICUNumberFormat := TICUNumberFormatter.Create('fr-FR', UNUM_SPELLOUT);
  try
    CheckEquals('cent cinquante-trois virgule quatre cinq', FICUNumberFormat.Format(Value, 'USD'));
  finally
    FICUNumberFormat.Free;
  end;
  FICUNumberFormat := TICUNumberFormatter.Create('en-US', UNUM_SPELLOUT);
  try
    CheckEquals('one hundred fifty-three point four five', FICUNumberFormat.Format(Value, 'EUR'));
  finally
    FICUNumberFormat.Free;
  end;
end;

initialization

RegisterTest('ICU', TestTICUNumberFormat.Suite);

end.
