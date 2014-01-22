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
  umachine in '..\lib\umachine.pas',
  umisc in '..\lib\umisc.pas',
  unum in '..\lib\unum.pas',
  utypes in '..\lib\utypes.pas',
  formattable in '..\lib\formattable.pas',
  uloc in '..\lib\uloc.pas',
  ICUNumberFormatter in '..\ICUNumberFormatter.pas',
  TestICUNumberFormat in 'TestICUNumberFormat.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

