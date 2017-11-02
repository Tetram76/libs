unit TetraClasses;

interface

uses Classes, SysUtils;

type
  TUsefulStream = class(TStream)
  public
     { Read functions }
     function ReadLn: string;
     function ReadChar: Char;
     function ReadString: string;
     function ReadLongInt: LongInt;
     function ReadSingle: Single;
     function ReadDouble: Double;
     function ReadExtended: Extended;
     function ReadPointer: Pointer;
     { Write functions }
     procedure WriteLn(const Str: string);
     procedure WriteChar(Char: Char);
     procedure WriteString(const Str: string);
     procedure WriteStr(const Str: string);
     procedure WriteLongInt(Int: LongInt);
     procedure WriteSingle(Value: Single);
     procedure WriteDouble(Value: Double);
     procedure WriteExtended(Value: Extended);
     procedure WritePointer(Value: Pointer);
     { General usage functions }
     function BOF: Boolean;
     function EOF: Boolean;
  end;

implementation

{ ******************************* TUsefulStream ******************************* }

  { Read functions }
  
function TUsefulStream.ReadLn: string;
var C: Char;
begin
  Result:= '';
  C:= ReadChar;
  while (not (C = #13)) and (Position <> Size) do
  begin
     Result:= Result + C;
     C:= ReadChar;
  end;
  if C = #13 then
     ReadChar
  else
     Result:= Result + C;
end;

function TUsefulStream.ReadChar: Char;
begin
  Read(Result, SizeOf(Char));
end;

function TUsefulStream.ReadString: string;
begin
  SetLength(Result, ReadLongInt);
  Read(PChar(Result)^, Length(Result));
end;

function TUsefulStream.ReadLongInt: LongInt;
begin
  Read(Result, SizeOf(LongInt));
end;

function TUsefulStream.ReadSingle: Single;
begin
  Read(Result, SizeOf(Single));
end;

function TUsefulStream.ReadDouble: Double;
begin
  Read(Result, SizeOf(Double));
end;

function TUsefulStream.ReadExtended: Extended;
begin
  Read(Result, SizeOf(Extended));
end;

function TUsefulStream.ReadPointer: Pointer;
begin
  Read(Result, SizeOf(Pointer));
end;

     { Write functions }

procedure TUsefulStream.WriteLn(const Str: string);
begin
  Write(PChar(Str+#13#10)^, Length(Str) + 2 * SizeOf(Char));
end;

procedure TUsefulStream.WriteChar(Char: Char);
begin
  Write(Char, SizeOf(Char));
end;

procedure TUsefulStream.WriteString(const Str: string);
begin
  WriteLongInt(Length(Str));
  Write(PChar(Str)^, Length(Str));
end;

procedure TUsefulStream.WriteStr(const Str: string);
begin
  Write(PChar(Str)^, Length(Str));
end;

procedure TUsefulStream.WriteLongInt(Int: LongInt);
begin
  Write(Int, SizeOf(LongInt));
end;

procedure TUsefulStream.WriteSingle(Value: Single);
begin
  Write(Value, SizeOf(Single));
end;

procedure TUsefulStream.WriteDouble(Value: Double);
begin
  Write(Value, SizeOf(Double));
end;

procedure TUsefulStream.WriteExtended(Value: Extended);
begin
  Write(Value, SizeOf(Extended));
end;

procedure TUsefulStream.WritePointer(Value: Pointer);
begin
  Write(Value, SizeOf(Pointer));
end;

  { General usage functions }

function TUsefulStream.BOF: Boolean;
begin
  Result:= Position = 0;
end;

function TUsefulStream.EOF: Boolean;
begin
  Result:= Position = Size;
end;

end.
