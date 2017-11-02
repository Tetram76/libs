program ICUTests;
{

  Projet de test DUnit Delphi
  -------------------------
  Ce projet contient le framework de test DUnit et les exécuteurs de test GUI/Console.
  Ajoutez "CONSOLE_TESTRUNNER" à l'entrée des définitions conditionnelles des options
  de projet pour utiliser l'exécuteur de test console.  Sinon, l'exécuteur de test GUI sera
  utilisé par défaut.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  UTestUnum in 'lib\UTestUnum.pas',
  icu_globals in '..\lib\icu_globals.pas',
  parseerr in '..\lib\parseerr.pas',
  _umachine in '..\lib\_umachine.pas',
  _umisc in '..\lib\_umisc.pas',
  _unum in '..\lib\_unum.pas',
  _utypes in '..\lib\_utypes.pas',
  _uformattable in '..\lib\_uformattable.pas',
  _uloc in '..\lib\_uloc.pas',
  ICUNumberFormatter in '..\ICUNumberFormatter.pas',
  TestICUNumberFormat in 'TestICUNumberFormat.pas',
  TestICUNumberFormatFunctions in 'TestICUNumberFormatFunctions.pas',
  _udat in '..\lib\_udat.pas',
  _ucal in '..\lib\_ucal.pas',
  _udisplaycontext in '..\lib\_udisplaycontext.pas',
  UTestUdat in 'lib\UTestUdat.pas',
  ICUTest in 'ICUTest.pas',
  ICUDateFormatter in '..\ICUDateFormatter.pas',
  TestICUDateFormat in 'TestICUDateFormat.pas',
  TestICUDateFormatFunctions in 'TestICUDateFormatFunctions.pas',
  _enum in '..\lib\_enum.pas',
  _utmscale in '..\lib\_utmscale.pas',
  ICUCalendar in '..\ICUCalendar.pas',
  ICUEnumeration in '..\ICUEnumeration.pas',
  ICULocale in '..\ICULocale.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

