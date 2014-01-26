unit TestICUDateFormat;

interface

uses
  System.SysUtils, System.Classes, ICUTest, ICUDateFormatter;

type
  TestTICUDateFormat = class(TICUTest)
  published
    procedure TestFormat;
    procedure TestParse;
  end;

implementation

uses
  _udat, TestFramework;

{ TestTICUDateFormat }

procedure TestTICUDateFormat.TestFormat;
var
  Value: TDateTime;
  ICUDateFormat: TICUDateFormatter;
begin
  Value := EncodeDate(2015, 3, 25) + EncodeTime(18, 45, 36, 753);

  ICUDateFormat := TICUDateFormatter.Create('fr-FR', UDAT_DEFAULT, UDAT_DEFAULT, '');
  try
    CheckEquals('25 mars 2015 18:45:36', ICUDateFormat.Format(Value));
  finally
    ICUDateFormat.Free;
  end;
  ICUDateFormat := TICUDateFormatter.Create('fr-FR', UDAT_DEFAULT, UDAT_DEFAULT, 'GMT');
  try
    CheckEquals('25 mars 2015 18:45:36', ICUDateFormat.Format(Value));
  finally
    ICUDateFormat.Free;
  end;
  ICUDateFormat := TICUDateFormatter.Create('fr-FR', UDAT_DEFAULT, UDAT_DEFAULT, 'GMT+1');
  try
    CheckEquals('25 mars 2015 19:45:36', ICUDateFormat.Format(Value));
  finally
    ICUDateFormat.Free;
  end;

  ICUDateFormat := TICUDateFormatter.Create('en-US', UDAT_DEFAULT, UDAT_DEFAULT, 'GMT');
  try
    CheckEquals('Mar 25, 2015, 6:45:36 PM', ICUDateFormat.Format(Value));
  finally
    ICUDateFormat.Free;
  end;
  ICUDateFormat := TICUDateFormatter.Create('en-US', UDAT_DEFAULT, UDAT_DEFAULT, 'GMT+1');
  try
    CheckEquals('Mar 25, 2015, 7:45:36 PM', ICUDateFormat.Format(Value));
  finally
    ICUDateFormat.Free;
  end;
end;

procedure TestTICUDateFormat.TestParse;
begin

end;

initialization

RegisterTest('ICU', TestTICUDateFormat.Suite);

end.
