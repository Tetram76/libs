unit TestICUDateFormatFunctions;

interface

uses
  System.SysUtils, System.Classes, ICUTest, ICUDateFormatter;

type
  TestTICUDateFormat = class(TICUTest)
  private
    FDate: TDateTime;
  protected
    procedure SetUp; override;
  published
    procedure TestDateToStr;
    procedure TestDateToStrShort;
    procedure TestDateToStrLong;
    procedure TestDateToStrFull;
    procedure TestTimeToStr;
    procedure TestTimeToStrShort;
    procedure TestTimeToStrLong;
    procedure TestTimeToStrFull;
  end;

implementation

uses
  TestFramework;

{ TestTICUDateFormat }

procedure TestTICUDateFormat.SetUp;
begin
  inherited;
  FDate := EncodeDate(2015, 3, 25) + EncodeTime(18, 45, 36, 753);
end;

procedure TestTICUDateFormat.TestDateToStr;
begin
  CheckEquals('25 mars 2015', ICUDateToStr(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('25 mars 2015', ICUDateToStr(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('Mar 25, 2015', ICUDateToStr(FDate, False, 'en-US'), 'en-US');
  CheckEquals('Mar 25, 2015', ICUDateToStr(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestDateToStrFull;
begin
  CheckEquals('mercredi 25 mars 2015', ICUDateToStrFull(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('mercredi 25 mars 2015', ICUDateToStrFull(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('Wednesday, March 25, 2015', ICUDateToStrFull(FDate, False, 'en-US'), 'en-US');
  CheckEquals('Wednesday, March 25, 2015', ICUDateToStrFull(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestDateToStrLong;
begin
  CheckEquals('25 mars 2015', ICUDateToStrLong(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('25 mars 2015', ICUDateToStrLong(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('March 25, 2015', ICUDateToStrLong(FDate, False, 'en-US'), 'en-US');
  CheckEquals('March 25, 2015', ICUDateToStrLong(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestDateToStrShort;
begin
  CheckEquals('25/03/2015', ICUDateToStrShort(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('25/03/2015', ICUDateToStrShort(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('3/25/15', ICUDateToStrShort(FDate, False, 'en-US'), 'en-US');
  CheckEquals('3/25/15', ICUDateToStrShort(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestTimeToStr;
begin
  CheckEquals('18:45:36', ICUTimeToStr(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('17:45:36', ICUTimeToStr(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('6:45:36 PM', ICUTimeToStr(FDate, False, 'en-US'), 'en-US');
  CheckEquals('5:45:36 PM', ICUTimeToStr(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestTimeToStrFull;
begin
  CheckEquals('18:45:36 UTC+01:00', ICUTimeToStrFull(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('17:45:36 UTC', ICUTimeToStrFull(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('6:45:36 PM GMT+01:00', ICUTimeToStrFull(FDate, False, 'en-US'), 'en-US');
  CheckEquals('5:45:36 PM GMT', ICUTimeToStrFull(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestTimeToStrLong;
begin
  CheckEquals('18:45:36 UTC+1', ICUTimeToStrLong(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('17:45:36 UTC', ICUTimeToStrLong(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('6:45:36 PM GMT+1', ICUTimeToStrLong(FDate, False, 'en-US'), 'en-US');
  CheckEquals('5:45:36 PM GMT', ICUTimeToStrLong(FDate, True, 'en-US'), 'en-US GMT');
end;

procedure TestTICUDateFormat.TestTimeToStrShort;
begin
  CheckEquals('18:45', ICUTimeToStrShort(FDate, False, 'fr-FR'), 'fr-FR');
  CheckEquals('17:45', ICUTimeToStrShort(FDate, True, 'fr-FR'), 'fr-FR GMT');
  CheckEquals('6:45 PM', ICUTimeToStrShort(FDate, False, 'en-US'), 'en-US');
  CheckEquals('5:45 PM', ICUTimeToStrShort(FDate, True, 'en-US'), 'en-US GMT');
end;

initialization

RegisterTest('ICU', TestTICUDateFormat.Suite);

end.
