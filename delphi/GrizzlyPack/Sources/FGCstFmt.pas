unit FGCstFmt;

{
Written by Frederic GUILLIEN - All rights reserved 1998
e mail : fguillien@grizzlydev.com
}

interface

uses
  Windows, SysUtils, Classes, Dialogs;

type
  TConstFormatsList = (
    cfCurrencyString,
    cfCurrencyFormat,
    cfNegCurrFormat,
    cfThousandSeparator,
    cfDecimalSeparator,
    cfCurrencyDecimals,
    cfDateSeparator,
    cfShortDateFormat,
    cfLongDateFormat,
    cfTimeSeparator,
    cfTimeAMString,
    cfTimePMString,
    cfShortTimeFormat,
    cfLongTimeFormat);

  TConstFormatsSet = set of TConstFormatsList;

  TConstFormats = class(TComponent)
  private
    { Déclarations privées }
    FAllowSetting: TConstFormatsSet;
    FCurrencyString: string;
    FCurrencyFormat: Byte;
    FNegCurrFormat: Byte;
    FThousandSeparator: Char;
    FDecimalSeparator: Char;
    FCurrencyDecimals: Byte;
    FDateSeparator: Char;
    FShortDateFormat: string;
    FLongDateFormat: string;
    FTimeSeparator: Char;
    FTimeAMString: string;
    FTimePMString: string;
    FShortTimeFormat: string;
    FLongTimeFormat: string;
  protected
    { Déclarations protégées }
    procedure SetAllowSetting(Value: TConstFormatsSet);
    procedure SetCurrencyString(Value: string);
    procedure SetCurrencyFormat(Value: Byte);
    procedure SetNegCurrFormat(Value: Byte);
    procedure SetThousandSeparator(Value: Char);
    procedure SetDecimalSeparator(Value: Char);
    procedure SetCurrencyDecimals(Value: Byte);
    procedure SetDateSeparator(Value: Char);
    procedure SetShortDateFormat(Value: string);
    procedure SetLongDateFormat(Value: string);
    procedure SetTimeSeparator(Value: Char);
    procedure SetTimeAMString(Value: string);
    procedure SetTimePMString(Value: string);
    procedure SetShortTimeFormat(Value: string);
    procedure SetLongTimeFormat(Value: string);
    function GetCurrencyString: string;
    function GetCurrencyFormat: Byte;
    function GetNegCurrFormat: Byte;
    function GetThousandSeparator: Char;
    function GetDecimalSeparator: Char;
    function GetCurrencyDecimals: Byte;
    function GetDateSeparator: Char;
    function GetShortDateFormat: string;
    function GetLongDateFormat: string;
    function GetTimeSeparator: Char;
    function GetTimeAMString: string;
    function GetTimePMString: string;
    function GetShortTimeFormat: string;
    function GetLongTimeFormat: string;
    procedure Loaded; override;
  public
    { Déclarations publiques }
  published
    { Déclarations publiées }
    procedure RestoreDefaults;
    procedure ApplySettings;
    property AllowSetting: TConstFormatsSet read FAllowSetting write SetAllowSetting;
    property CurrencyString: string read GetCurrencyString write SetCurrencyString;
    property CurrencyFormat: Byte read GetCurrencyFormat write SetCurrencyFormat;
    property NegCurrFormat: Byte read GetNegCurrFormat write SetNegCurrFormat;
    property ThousandSeparator: Char read GetThousandSeparator write SetThousandSeparator;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator;
    property CurrencyDecimals: Byte read GetCurrencyDecimals write SetCurrencyDecimals;
    property DateSeparator: Char read GetDateSeparator write SetDateSeparator;
    property ShortDateFormat: string read GetShortDateFormat write SetShortDateFormat;
    property LongDateFormat: string read GetLongDateFormat write SetLongDateFormat;
    property TimeSeparator: Char read GetTimeSeparator write SetTimeSeparator;
    property TimeAMString: string read GetTimeAMString write SetTimeAMString;
    property TimePMString: string read GetTimePMString write SetTimePMString;
    property ShortTimeFormat: string read GetShortTimeFormat write SetShortTimeFormat;
    property LongTimeFormat: string read GetLongTimeFormat write SetLongTimeFormat;
  end;

implementation

procedure TConstFormats.Loaded;
begin
  inherited Loaded;
  ApplySettings;
end;

procedure TConstFormats.RestoreDefaults;
begin
  SysUtils.GetFormatSettings;
end;

procedure TConstFormats.ApplySettings;
var
  OldSettings: TConstFormatsSet;
begin
  OldSettings:= FAllowSetting;
  SetAllowSetting([]);
  SetAllowSetting(OldSettings);
end;

procedure TConstFormats.SetAllowSetting(Value: TConstFormatsSet);
  function NbElem(ASetCF: TConstFormatsSet): Integer;
  var
    i: TConstFormatsList;
  begin
    Result:= 0;
    for i:= Low(TConstFormatsList) to High(TConstFormatsList) do
      if i in ASetCF then
        Inc(Result);
  end;
begin
  if (Value <> FAllowSetting) then
  begin
    FAllowSetting:= Value;
    {Restauration à partir de WinIni}
    SysUtils.GetFormatSettings;
    {Restauration des valeurs utilisateur}
    if cfCurrencyString in FAllowSetting then
      SysUtils.CurrencyString:= FCurrencyString;
    if cfCurrencyFormat in FAllowSetting then
      SysUtils.CurrencyFormat:= FCurrencyFormat;
    if cfNegCurrFormat in FAllowSetting then
      SysUtils.NegCurrFormat:= FNegCurrFormat;
    if cfThousandSeparator in FAllowSetting then
      SysUtils.ThousandSeparator:= FThousandSeparator;
    if cfDecimalSeparator in FAllowSetting then
      SysUtils.DecimalSeparator:= FDecimalSeparator;
    if cfCurrencyDecimals in FAllowSetting then
      SysUtils.CurrencyDecimals:= FCurrencyDecimals;
    if cfDateSeparator in FAllowSetting then
      SysUtils.DateSeparator:= FDateSeparator;
    if cfShortDateFormat in FAllowSetting then
      SysUtils.ShortDateFormat:= FShortDateFormat;
    if cfLongDateFormat in FAllowSetting then
      SysUtils.LongDateFormat:= FLongDateFormat;
    if cfTimeSeparator in FAllowSetting then
      SysUtils.TimeSeparator:= FTimeSeparator;
    if cfTimeAMString in FAllowSetting then
      SysUtils.TimeAMString:= FTimeAMString;
    if cfTimePMString in FAllowSetting then
      SysUtils.TimePMString:= FTimePMString;
    if cfShortTimeFormat in FAllowSetting then
      SysUtils.ShortTimeFormat:= FShortTimeFormat;
    if cfLongTimeFormat in FAllowSetting then
      SysUtils.LongTimeFormat:= FLongTimeFormat;
  end;
end;

procedure TConstFormats.SetCurrencyString(Value: string);
begin
  FCurrencyString:= Value;
  if ((cfCurrencyString in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.CurrencyString) then
    SysUtils.CurrencyString:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfCurrencyString];
end;

procedure TConstFormats.SetCurrencyFormat(Value: Byte);
begin
  FCurrencyFormat:= Value;
  if ((cfCurrencyFormat in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.CurrencyFormat) then
    SysUtils.CurrencyFormat:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfCurrencyFormat];
end;

procedure TConstFormats.SetNegCurrFormat(Value: Byte);
begin
  FNegCurrFormat:= Value;
  if ((cfNegCurrFormat in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.NegCurrFormat) then
    SysUtils.NegCurrFormat:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfNegCurrFormat];
end;

procedure TConstFormats.SetThousandSeparator(Value: Char);
begin
  FThousandSeparator:= Value;
  if ((cfThousandSeparator in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.ThousandSeparator) then
    SysUtils.ThousandSeparator:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfThousandSeparator];
end;

procedure TConstFormats.SetDecimalSeparator(Value: Char);
begin
  FDecimalSeparator:= Value;
  if ((cfDecimalSeparator in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.DecimalSeparator) then
    SysUtils.DecimalSeparator:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfDecimalSeparator];
end;

procedure TConstFormats.SetCurrencyDecimals(Value: Byte);
begin
  FCurrencyDecimals:= Value;
  if ((cfCurrencyDecimals in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.CurrencyDecimals) then
    SysUtils.CurrencyDecimals:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfCurrencyDecimals];
end;

procedure TConstFormats.SetDateSeparator(Value: Char);
begin
  FDateSeparator:= Value;
  if ((cfDateSeparator in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.DateSeparator) then
    SysUtils.DateSeparator:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfDateSeparator];
end;

procedure TConstFormats.SetShortDateFormat(Value: string);
begin
  FShortDateFormat:= Value;
  if ((cfShortDateFormat in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.ShortDateFormat) then
    SysUtils.ShortDateFormat:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfShortDateFormat];
end;

procedure TConstFormats.SetLongDateFormat(Value: string);
begin
  FLongDateFormat:= Value;
  if ((cfLongDateFormat in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.LongDateFormat) then
    SysUtils.LongDateFormat:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfLongDateFormat];
end;

procedure TConstFormats.SetTimeSeparator(Value: Char);
begin
  FTimeSeparator:= Value;
  if ((cfTimeSeparator in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.TimeSeparator) then
    SysUtils.TimeSeparator:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfTimeSeparator];
end;

procedure TConstFormats.SetTimeAMString(Value: string);
begin
  FTimeAMString:= Value;
  if ((cfTimeAMString in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.TimeAMString) then
    SysUtils.TimeAMString:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfTimeAMString];
end;

procedure TConstFormats.SetTimePMString(Value: string);
begin
  FTimePMString:= Value;
  if ((cfTimePMString in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.TimePMString) then
    SysUtils.TimePMString:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfTimePMString];
end;

procedure TConstFormats.SetShortTimeFormat(Value: string);
begin
  FShortTimeFormat:= Value;
  if ((cfShortTimeFormat in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> SysUtils.ShortTimeFormat) then
    SysUtils.ShortTimeFormat:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfShortTimeFormat];
end;

procedure TConstFormats.SetLongTimeFormat(Value: string);
begin
  FLongTimeFormat:= Value;
  if ((cfLongTimeFormat in AllowSetting) or (not (csLoading in ComponentState)))
    and (Value <> LongTimeFormat) then
    SysUtils.LongTimeFormat:= Value;
  if (not (csLoading in ComponentState)) then
    FAllowSetting:= FAllowSetting + [cfLongTimeFormat];
end;

function TConstFormats.GetCurrencyString: string;
begin
  if csWriting in ComponentState then
    Result:= FCurrencyString
  else
    Result:= SysUtils.CurrencyString;
end;

function TConstFormats.GetCurrencyFormat: Byte;
begin
  if csWriting in ComponentState then
    Result:= FCurrencyFormat
  else
    Result:= SysUtils.CurrencyFormat;
end;

function TConstFormats.GetNegCurrFormat: Byte;
begin
  if csWriting in ComponentState then
    Result:= FNegCurrFormat
  else
    Result:= SysUtils.NegCurrFormat;
end;

function TConstFormats.GetThousandSeparator: Char;
begin
  if csWriting in ComponentState then
    Result:= FThousandSeparator
  else
    Result:= SysUtils.ThousandSeparator;
end;

function TConstFormats.GetDecimalSeparator: Char;
begin
  if csWriting in ComponentState then
    Result:= FDecimalSeparator
  else
    Result:= SysUtils.DecimalSeparator;
end;

function TConstFormats.GetCurrencyDecimals: Byte;
begin
  if csWriting in ComponentState then
    Result:= FCurrencyDecimals
  else
    Result:= SysUtils.CurrencyDecimals;
end;

function TConstFormats.GetDateSeparator: Char;
begin
  if csWriting in ComponentState then
    Result:= FDateSeparator
  else
    Result:= SysUtils.DateSeparator;
end;

function TConstFormats.GetShortDateFormat: string;
begin
  if csWriting in ComponentState then
    Result:= FShortDateFormat
  else
    Result:= SysUtils.ShortDateFormat;
end;

function TConstFormats.GetLongDateFormat: string;
begin
  if csWriting in ComponentState then
    Result:= FLongDateFormat
  else
    Result:= SysUtils.LongDateFormat;
end;

function TConstFormats.GetTimeSeparator: Char;
begin
  if csWriting in ComponentState then
    Result:= FTimeSeparator
  else
    Result:= SysUtils.TimeSeparator;
end;

function TConstFormats.GetTimeAMString: string;
begin
  if csWriting in ComponentState then
    Result:= FTimeAMString
  else
    Result:= SysUtils.TimeAMString;
end;

function TConstFormats.GetTimePMString: string;
begin
  if csWriting in ComponentState then
    Result:= FTimePMString
  else
    Result:= SysUtils.TimePMString;
end;

function TConstFormats.GetShortTimeFormat: string;
begin
  if csWriting in ComponentState then
    Result:= FShortTimeFormat
  else
    Result:= SysUtils.ShortTimeFormat;
end;

function TConstFormats.GetLongTimeFormat: string;
begin
  if csWriting in ComponentState then
    Result:= FLongTimeFormat
  else
    Result:= SysUtils.LongTimeFormat;
end;

end.

