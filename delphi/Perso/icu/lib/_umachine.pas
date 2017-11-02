unit _umachine;

{$I icu.inc}

interface

const
  // The TRUE value of a UBool.
  UTRUE = 1;
  // The FALSE value of a UBool.
  UFALSE = 0;

type
  PInt32 = ^Int32;

  // The ICU boolean type.
  UBool = record
  private
    Value: int8;
  public
    class operator Implicit(Value: int8): UBool;
    class operator Implicit(Value: Boolean): UBool;
    class operator Implicit(Value: UBool): int8;
    class operator Implicit(Value: UBool): Boolean;
  end;

  // Define UChar to be UCHAR_TYPE, if that is #defined (for example, to char16_t), or wchar_t if that is 16 bits wide; always assumed to be unsigned.
  UChar = WideString;

  PPUChar = ^PUChar;
  PUChar = ^UChar;
  // Define UChar32 as a type for single Unicode code points.
  UChar32 = int32;

implementation

{ UBool }

class operator UBool.Implicit(Value: Boolean): UBool;
begin
  if Value then
    Result.Value := UTRUE
  else
    Result.Value := UFALSE;
end;

class operator UBool.Implicit(Value: int8): UBool;
begin
  Result.Value := Value;
end;

class operator UBool.Implicit(Value: UBool): Boolean;
begin
  Result := Value.Value = UTRUE;
end;

class operator UBool.Implicit(Value: UBool): int8;
begin
  Result := Value.Value;
end;

end.
