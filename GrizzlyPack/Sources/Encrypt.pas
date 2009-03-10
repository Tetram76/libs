unit Encrypt;

interface

var
	CommonPo, OtherPo : Word;

function EncryptString(const S: string; Key: Word): string;

function DecryptString(const S: string; Key: Word): string;

implementation

uses SysUtils;

var
  b,g : Word;

function EncryptString(const S: string; Key: Word): string;
var
  I : byte;
  Ch : string;
begin
	if Length(S)=0 then
  begin
  	Result:='';
    Exit;
  end;
  Result:=S;
  for I := 1 to Length(S) do
  begin
    Result[I] := char(byte(S[I]) xor (Key shr 8));
    Key := (byte(Result[I]) + Key) * g + b;
  end;
  Ch := '';
  for i := 1 to Length(Result) do
  begin
		Ch := Ch + FormatFloat('000',Ord(Result[i]));
  end;
  for i := 1 to Length(Ch) do
  begin
  	Ch[i]:=Char(Byte(Ch[i])+i mod 10);
  end;
  Result:=Ch;
end;

function DecryptString(const S: string; Key: Word): string;
var
  I: byte;
  Ch, Ch2 : string;
begin
	if Length(S)=0 then
  begin
  	Result:='';
    Exit;
  end;
  Ch2:=S;
  for i := 1 to Length(Ch2) do
  begin
  	Ch2[i]:=Char(Byte(Ch2[i])-(i mod 10));
  end;
	for i:=1 to Length(Ch2) div 3 do
  begin
  	Ch:=Ch+Chr(StrToInt(Copy(Ch2,(i-1)*3+1,3)));
  end;
  Result:=Ch;
  for I := 1 to Length(Ch) do
  begin
    Result[I] := char(byte(Ch[I]) xor (Key shr 8));
    Key := (byte(Ch[I]) + Key) * g + b;
  end;
end;

begin
	CommonPo:=25695;
  OtherPo:=14253;
  b:=11255;
  g:=4586;
end.
