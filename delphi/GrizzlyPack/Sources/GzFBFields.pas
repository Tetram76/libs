unit GzFBFields;

{$I GrizzlyDefine.INC}

interface

uses
  SysUtils, Classes, DB, DBConsts;

type
  TBooleanCharsArray = array[Boolean] of Char;
  TBooleanStringsArray = array[Boolean] of string;

var
  BooleanChars : TBooleanCharsArray;
  BooleanStrings : TBooleanStringsArray;

type
  TFBStringField = class(TStringField)
  private
    FIsBoolean: Boolean;
    FDisplayValues: string;
    FTextValues: TBooleanStringsArray;
    FCharValues: TBooleanCharsArray;
    FStoredValues: string;
    FTrueValues: set of Char;
    procedure LoadTextValues;
    procedure LoadCharValues;
    procedure LoadTrueValues;
    procedure SetDisplayValues(const Value: string);
    procedure SetStoredValues(const Value: string);
    procedure SetIsBoolean(const Value: Boolean);
  protected
    function GetAsBoolean: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsBoolean(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property IsBoolean : Boolean read FIsBoolean write SetIsBoolean default False;
    property BoolDisplayValues: string read FDisplayValues write SetDisplayValues;
    property BoolStoredValues: string read FStoredValues write SetStoredValues;
  end;

implementation

{ TFBStringField }

constructor TFBStringField.Create(AOwner: TComponent);
begin
  inherited;
  LoadTextValues;
  LoadCharValues;
end;

function TFBStringField.GetAsBoolean: Boolean;
var
  S: string;
begin
  S := GetAsString;
  if IsBoolean then
    Result := (Length(S) > 0) and (S[1] in FTrueValues)
  else
    Result := (Length(S) > 0) and (S[1] in ['T', 't', 'Y', 'y'] + FTrueValues);
end;

procedure TFBStringField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) or (not IsBoolean) or (not DisplayText) then
    inherited GetText(Text, DisplayText)
  else
    Text := FTextValues[AsBoolean];
end;

procedure TFBStringField.LoadCharValues;
begin
  FCharValues := BooleanChars;
  LoadTrueValues;
end;

procedure TFBStringField.LoadTextValues;
begin
  FTextValues := BooleanStrings;
end;

procedure TFBStringField.LoadTrueValues;
begin
  FTrueValues := [UpperCase(FCharValues[True])[1], LowerCase(FCharValues[True])[1]];
end;

procedure TFBStringField.SetAsBoolean(Value: Boolean);
begin
  SetAsString(FCharValues[Value]);
end;

procedure TFBStringField.SetDisplayValues(const Value: string);
var
  P: Integer;
begin
  if FDisplayValues <> Value then
  begin
    FDisplayValues := Value;
    if Value = '' then LoadTextValues else
    begin
      P := Pos(';', Value);
      if P = 0 then P := 256;
      FTextValues[False] := Copy(Value, P + 1, 255);
      FTextValues[True] := Copy(Value, 1, P - 1);
    end;
    PropertyChanged(True);
  end;
end;

procedure TFBStringField.SetIsBoolean(const Value: Boolean);
begin
  if FIsBoolean <> Value then
  begin
    FIsBoolean := Value;
    PropertyChanged(True);
  end;
end;

procedure TFBStringField.SetStoredValues(const Value: string);
var
  P: Integer;
begin
  if FStoredValues <> Value then
  begin
    FStoredValues := Value;
    if Value = '' then LoadCharValues else
    begin
      P := Pos(';', Value);
      if P = 0 then P := 256;
      FCharValues[False] := Copy(Value, P + 1, 255)[1];
      FCharValues[True] := Copy(Value, 1, P - 1)[1];
      LoadTrueValues;
    end;
    PropertyChanged(True);
  end;
end;

initialization
  BooleanChars[True] := 'T';
  BooleanChars[False] := 'F';
  BooleanStrings[True] := STextTrue;
  BooleanStrings[False] := STextFalse;
end.
