{***************************************************************
 *
 * Unit Name: AUtils
 * Purpose  : All and nothing ...
 * Author   : Alexandre GUILLIEN
 * History  : So much ...
 *
 ****************************************************************}

unit AUtils;

interface

uses TypInfo, SysUtils;

const
  MaxArray = 4096; { Sert juste pour le débogage }
  CRLF = #13#10;

type
  TByteArray    = array [0..MaxArray] of Byte;     PByteArray    = ^TByteArray;
  TCharArray    = array [0..MaxArray] of Char;     PCharArray    = ^TCharArray;
  TBoolArray    = array [0..MaxArray] of Boolean;  PBoolArray    = ^TBoolArray;
  TIntegerArray = array [0..MaxArray] of Integer;  PIntegerArray = ^TIntegerArray;
  TPointerArray = array [0..MaxArray] of Pointer;  PPointerArray = ^TPointerArray;
  TSingleArray  = array [0..MaxArray] of Single;   PSingleArray  = ^TSingleArray;
	TDoubleArray  = array [0..MaxArray] of Double;   PDoubleArray  = ^TDoubleArray;
  PBool       = ^Boolean;
  PInteger    = ^Integer;
  PSmallInt   = ^SmallInt;
  PWord       = ^Word;
  PPointer    = ^Pointer;
  PLongInt    = ^LongInt;
  PSingle     = ^Single;
  PDouble     = ^Double;
  PDateTime   = ^TDateTime;
  PExtended   = ^Extended;
  PByte       = ^Byte;
  PObject     = ^TObject;

  TObjProc = procedure of object;
  TParamProc = procedure(P: Pointer) of object;
  TArrayCompareProc = function(P1, P2: Pointer; L: Integer): Boolean;

  { Numeric }
  function Bounded(Number, Min, Max: Integer): Boolean;
  function IMin(I1, I2: Integer): Integer;
  function IMax(I1, I2: Integer): Integer;
  function Sign(V: Extended): Boolean;

  { Object Control }
  function GetStrPropByName(Instance: TObject; PropName: string): string;
  procedure SetStrPropByName(Instance: TObject; PropName: string; Value: string);
  function GetOrdPropByName(Instance: TObject; PropName: string): LongInt;
  procedure SetOrdPropByName(Instance: TObject; PropName: string; Value: LongInt);
  function GetFloatPropByName(Instance: TObject; PropName: string): Extended;
  procedure SetFloatPropByName(Instance: TObject; PropName: string; Value: Extended);
  procedure FreeObjects(Objects: array of TObject);

  { Memory Control }
  function IncPointer(P: Pointer; I: Integer): Pointer;
  function CompareArraysWithoutCase(P1, P2: Pointer; L: Integer): Boolean;
  function CompareUpCaseArrayWithoutCase(P1, P2: Pointer; L: Integer): Boolean;
  function CompareLargeArrays(P1, P2: Pointer; L: Integer): Boolean;
  function CompareArrays(P1, P2: Pointer; L: Integer): Boolean;
  function PosArray(SubArray, MainArray: Pointer; LSub, LMain: Integer;
     CompareProc: TArrayCompareProc): Integer;
  procedure ReverseArray(const P: Pointer; L: Integer);
  procedure GenericSwap(const Var1, Var2; Size: Integer);

  { String/Type Conversion }
     { Hexadecimal format Handling }
	function HToI(Nb: Char): Byte;
	function HexToInt(HexStr: string): Integer;
  function ByteToHex(Value: Byte): string;
	function ArrayToHex(MainArray: PCharArray; Len: Integer): string;
  function PtrToStr(P: Pointer): string;
  function ByteToBin(Value: Byte): string;
     { String Control }
  function LeftStr(const Str: string; Count: Integer): string;
  function RightStr(const Str: string; Count: Integer): string;
  function KillSpaces(const Str: string): string;
  function ReplaceInStr(SubStr, S, Replacer: string): string;
  function ReplaceInStrUpperCase(SubStr, S, Replacer: string): string;
  function RemoveChars(const Str: string; Chars: array of Char): string;
  function ConvertAccentuated(const Str: string): string;
  function Standardize(const Str: string): string;
  function ReverseStr(const Str: string): string;
  function ExtractQuotedStr(Src: string; Quote: Char): string;

  function CaseLessPos(SubStr: string; S: string): Integer;
     { Numeric Conversion }
  function DoubleCode(Value: Extended): string;
  function TriCode(Value: Extended): string;
  function QuadCode(Value: Extended): string;
  function PentaCode(Value: Extended): string;

  { Date Control }
     { Date Conversion }
  function FullDate(Date: TDateTime): string;     { Lundi 1er Janvier 2000 }
  function SuperiorDate(Date: TDateTime): string; { 1er Janvier 2000 }
  function HalfDate(Date: TDateTime): string;     { Lundi 1er Janvier }
  function SmallDate(Date: TDateTime): string;    { 1er Janvier }
     { Time Conversion }
  function FullTime(Time: TDateTime): string;  { 18 heure 00 }
  function HalfTime(Time: TDateTime): string;  { 18H00 }
  function Temps(Time: TDateTime; MilliS: Boolean): string;
  function HalfTemps(Time: TDateTime): string;
  function HourToTime(H: string): TDateTime; { '00h00' => TDateTime }
     { Date manipulation }
  function IncMonth(Date: TDateTime; NbMonths: Integer): TDateTime;
  function ExtractYear(Date: TDateTime): Integer;
  function ExtractMonth(Date: TDateTime): Integer;
  function ExtractDay(Date: TDateTime): Integer;
  function DateTimeToSec(Date: TDateTime): Extended;
  function WeekOfDate(Date: TDateTime): Integer;
     { Time Control }
  function RDTSC: Double; assembler;
  function SecToDateTime(NbSeconds: Extended): TDateTime;

  function ExtractParam(const S: string; Separator: Char; Param: Integer): string;

implementation

function ExtractParam(const S: string; Separator: Char; Param: Integer): string;
var i, j, k, l: Integer;
begin
  Result:= '';
  i:= 1;
  j:= 1;
  l:= 1;
  k:= -1;
  while (k <> Param) do
  begin
    if i > Length(S) then
      Exit;
    j:= i;
    while (i <= Length(S)) and (S[i] <> Separator) do
      Inc(i);
    l:= i;
    while (i <= Length(S)) and (S[i] = Separator) do
      Inc(i);
    Inc(k);
  end;
  Result:= Trim(Copy(S, j, l - j));
end;

function Bounded(Number, Min, Max: Integer): Boolean;
begin
  Result:= (Number >= Min) and (Number <= Max);
end;

function IMin(I1, I2: Integer): Integer;
begin
  if I1 > I2 then Result:= I2 else Result:= I1;
end;

function IMax(I1, I2: Integer): Integer;
begin
  if I1 < I2 then Result:= I2 else Result:= I1;
end;

function Sign(V: Extended): Boolean;
begin
  Result:= V >= 0;
end;

{ Type Info Procs }

function GetPropByName(Instance: TObject; PropName: string): PPropInfo;
begin
  Result:= GetPropInfo(Instance.ClassInfo, PropName);
  if Result = nil then raise Exception.Create('La propriété ' + PropName + ' n''existe' +
     ' pas dans la classe ' + Instance.ClassName);
end;

function GetStrPropByName(Instance: TObject; PropName: string): string;
var P: PPropInfo;
begin
  P:= GetPropByName(Instance, PropName);
  Result:= GetStrProp(Instance, P);
end;

procedure SetStrPropByName(Instance: TObject; PropName: String; Value: String);
var P: PPropInfo;
begin
  P:= GetPropByName(Instance, PropName);
  SetStrProp(Instance, P, Value);
end;

function GetOrdPropByName(Instance: TObject; PropName: string): LongInt;
var P: PPropInfo;
begin
  P:= GetPropByName(Instance, PropName);
  Result:= GetOrdProp(Instance, P);
end;

procedure SetOrdPropByName(Instance: TObject; PropName: String; Value: LongInt);
var P: PPropInfo;
begin
  P:= GetPropByName(Instance, PropName);
  SetOrdProp(Instance, P, Value);
end;

function GetFloatPropByName(Instance: TObject; PropName: string): Extended;
var P: PPropInfo;
begin
  P:= GetPropByName(Instance, PropName);
  Result:= GetFloatProp(Instance, P);
end;

procedure SetFloatPropByName(Instance: TObject; PropName: string; Value: Extended);
var P: PPropInfo;
begin
  P:= GetPropByName(Instance, PropName);
  SetFloatProp(Instance, P, Value);
end;

procedure FreeObjects(Objects: array of TObject);
var i: Integer;
begin
  for i:= Low(Objects) to High(Objects) do
    Objects[i].Free;
end;


{ Memory Control }

function IncPointer(P: Pointer; I: Integer): Pointer;
begin
  Result:= Pointer(LongInt(P) + I);
end;

function PosArray(SubArray, MainArray: Pointer; LSub, LMain: Integer;
  CompareProc: TArrayCompareProc): Integer;
{$IFDEF WIN32}
asm
 // Note : EAX = SubTab, EDX = Tab, ECX = LSub
  push EBX
  push ESI
  push EDI

  mov EBX, LMain
  sub EBX, ECX     // EBX = Limit:= LMain - LSub
  push EBX

  cmp ECX, 0
  jna @NotFound
  cmp EBX, 0
  jnae @NotFound

  inc EBX          // pour pouvoir processer EBX = 0
  mov EDI, MainArray
  add EDI, EBX
  neg EBX          // EDI + EBX = Tab[0]

  mov ESI, CompareProc // Prevents the use of "call CompareProc" that is slower than "call Register"

  mov CompareProc, EAX // CompareProc = SubTab (pas de place allouée pour SubTab !)
  mov LMain, ECX       // LMain = LSub (pas de place allouée pour LSub !)

@Bcl:
  // CallProc
     mov EAX, CompareProc   // EAX = SubTab
     lea EDX, [EDI + EBX]
     mov ECX, LMain         // ECX = LSub
        // Using this instruction leads to reduce the speed instead of
        // a Call on a "const" function .... but this way, it is far more flexible
     call ESI
  or EAX, EAX
  jne @Found
  // Boucle
  inc EBX
  jne @Bcl
@NotFound:
  mov EAX, -1
  jmp @End
@Found:
  // On met dans EAX la position actuelle ......
  mov EAX, [ESP] // [ESP] = Limit
  inc EBX  // EBX a une valeur "normalisée"
  add EAX, EBX
@End:
  pop EBX { On a pushé Limit }
  pop EDI
  pop ESI
  pop EBX
end;
{$ELSE}
var
  i, Limit: Integer;
begin
  Result:= -1;
  Limit:= LMain - LSub;
  if (Limit >= 0) and (LSub > 0) then
  begin
     for i:= 0 to Limit do
        if CompareProc(SubArray, Pointer(Integer(MainArray) + i), LSub) then
        begin
           Result:= i;
           Exit;
        end;
  end;
end;
{$ENDIF}

procedure ReverseArray(const P: Pointer; L: Integer);
var i: Integer;
    C: Char;
begin
  for i:= 0 to (L - 1) div 2 do
  begin
     C:= PCharArray(P)^[i];
     PCharArray(P)^[i]:= PCharArray(P)^[L - i];
     PCharArray(P)^[L - i]:= C;
  end;
end;

procedure GenericSwap(const Var1, Var2; Size: Integer);
var P: Pointer;
begin
  GetMem(P, Size);
  try
    Move(Var1, P^, Size);
    Move(Var2, (@Var1)^, Size);
    Move(P^, (@Var2)^, Size);
  finally
    FreeMem(P, Size);
  end;
end;

{ Rapide : compare d'abord par dword puis par byte }
{ ATTENTION, ne procure d'accéleration que si tout est ALIGNÉ ... !!! }
function CompareLargeArrays(P1, P2: Pointer; L: Integer): Boolean;
{$IFDEF WIN32}
asm
	push EBX
  push ESI

  mov ESI, ECX
  and ECX, $FFFFFFFC { on vire les bits sans importance }
  je @ShortSearch

  add EAX, ECX      { On fait pointer EAX tel que EAX - ECX = 0 }
  add EDX, ECX      { On fait pointer EAX tel que EAX - ECX = 0 }
  neg ECX
@Bcl1:
  mov EBX, [EAX + ECX]
  cmp [EDX + ECX], EBX
  jne @PasOk
  add ECX, 4
  jne @Bcl1
@ShortSearch:
  and ESI, 3
  je @Ok
@Bcl2:
  mov BL, [EAX + ECX]
  cmp [EDX + ECX], BL
  jne @PasOk
  inc ECX
  dec ESI
  jne @Bcl2
@Ok:
	xor EAX, EAX
  inc EAX
  jmp @End
@PasOk:
	xor EAX, EAX
@End:
  pop ESI
	pop EBX
end;
{$ELSE}
var i, SmallL: Integer;
begin
  Result:= True;
  SmallL:= L mod SizeOf(Integer);
  L:= L div SizeOf(Integer);
  for i:= 0 to L - 1 do
  begin
     Result:= PIntegerArray(P1)^[i] = PIntegerArray(P2)^[i];
     if not Result then Exit;
  end;
  for SmallL:= 0 to SmallL - 1 do
  begin
     Result:= PCharArray(P1)^[L * 4 + SmallL] = PCharArray(P2)^[L * 4 + SmallL];
     if not Result then Exit;
  end;
end;
{$ENDIF}

function CompareArrays(P1, P2: Pointer; L: Integer): Boolean;
{$IFDEF WIN32}
asm              { Attention, compare à partir de la FIN !!! }
	push EBX
@Bcl:
  dec ECX
  js @Ok
  mov BL, [EAX + ECX]
  cmp [EDX + ECX], BL
	je @Bcl
@PasOk:
  xor EAX, EAX
  jmp @End
@Ok:
	mov EAX, 1
@End:
	pop EBX
end;
{$ELSE}
begin
  Result:= True;
  for L:= 0 to L - 1 do
  begin
     Result:= PCharArray(P1)^[L] = PCharArray(P2)^[L];
     if Result then Break;
  end;
end;
{$ENDIF}

function CompareArraysWithoutCase(P1, P2: Pointer; L: Integer): Boolean;
{$IFDEF WIN32}
asm              { Attention, compare à partir de la FIN !!! }
	{ EAX = P1, EDX = P2, ECX = L }
	push EBX
  mov EBX, EAX
@Bcl:
  dec ECX
  js @Found
  mov AL, [EBX + ECX]
  call UpCase
  xchg AH, AL
  mov AL, [EDX + ECX]
  call UpCase
  cmp AH, AL
  je @Bcl
@PasBon:
  xor EAX,EAX
  jmp @Fini
@Found:
  mov EAX, 1
@Fini:
  pop EBX
end;
{$ELSE}
begin
  Result:= True;
  for L:= 0 to L - 1 do
  begin
     Result:= UpCase(PCharArray(P1)^[L]) = UpCase(PCharArray(P2)^[L]);
     if not Result then Exit;
  end;
end;
{$ENDIF}

function CompareUpCaseArrayWithoutCase(P1, P2: Pointer; L: Integer): Boolean;
{$IFDEF WIN32}
asm                 { Attention, compare à partir de la FIN !!! }
  { EAX = P1, EDX = P2, ECX = L }
  push EBX
@Bcl:
  dec ECX
  js @Found
  mov BL, [EDX + ECX]
  cmp [EAX + ECX], BL { P1 est TOUJOURS en UpperCase en théorie }
  je @Bcl
  cmp BL, 'a'
  jnae @PasBon
  cmp BL, 'z'
  jnbe @PasBon
  sub BL, 20h
  cmp [EAX + ECX], BL
  je @Bcl
@PasBon:
  xor EAX,EAX
  jmp @Fini
@Found:
  mov EAX, 1
@Fini:
  pop EBX
end;
{$ELSE}
begin
  Result:= True;
  for L:= 0 to L - 1 do
  begin
     Result:= PCharArray(P1)^[L] = UpCase(PCharArray(P2)^[L]);
     if not Result then Exit;
  end;
end;
{$ENDIF}

{ String/Type Conversion }

function HToI(Nb: Char): Byte;
begin
	case Nb of
		'0'..'9': Result:= Ord(Nb) - $30;
     'A'..'F': Result:= Ord(Nb) - 55;
     'a'..'f': Result:= Ord(Nb) - 87;
     else raise EConvertError.Create('Erreur dans la chaine hexadécimale');
  end;
end;

function HexToInt(HexStr: string): Integer;
var
	i, Scale: Integer;
begin
	Result:= 0;
	Scale:= 1;
	for i:= Length(HexStr) downto 1 do
  begin
     Result:= Result + Scale * HToI(HexStr[i]);
     Scale:= Scale * 16;
  end;
end;

function ByteToHex(Value: Byte): string;
var B: Byte;
begin
  SetLength(Result, 2);
  B:= Value shr 4;
  Value:= Value and $0F;
  case B of
     0..9: B:= B + Ord('0')
     else B:= B + Ord('A') - $0A;
  end;
  case Value of
     0..9: Value:= Value + Ord('0')
     else Value:= Value + Ord('A') - $0A;
  end;
  Result[1]:= Char(B);
  Result[2]:= Char(Value);
end;

function ArrayToHex(MainArray: PCharArray; Len: Integer): string;
var
	i: Integer;
begin
	Result:= '';
  for i:= 0 to Len - 1 do
  	Result:= Result + ByteToHex(Byte(MainArray^[i]));
end;

function PtrToStr(P: Pointer): string;
begin
  {$IFDEF WIN32}
  Result:= ArrayToHex(IncPointer(@P, 3), 1) + ArrayToHex(IncPointer(@P, 2), 1) +
     ArrayToHex(IncPointer(@P, 1), 1) + ArrayToHex(IncPointer(@P, 0), 1);
  {$ELSE}
  Result:= ArrayToHex(IncPointer(@P, 2), 2) + ':' + ArrayToHex(@P, 2);
  {$ENDIF}
end;

function ByteToBin(Value: Byte): string;
var i: Integer;
    B: Byte;
begin
  Result:= '';
  for i:= 7 downto 0 do
  begin
    B:= 1 shl i;
    Result:= Result + Char(Ord('0') or Byte((B and Value) > 0));
    if i = 4 then Result:= Result + ' ';   
  end;
end;

  { Numeric Conversion }

function DoubleCode(Value: Extended): string;
begin
  Result:= FormatFloat('00', Value);
end;

function TriCode(Value: Extended): string;
begin
  Result:= FormatFloat('000', Value);
end;

function QuadCode(Value: Extended): string;
begin
  Result:= FormatFloat('0000', Value);
end;

function PentaCode(Value: Extended): string;
begin
  Result:= FormatFloat('00000', Value);
end;

  { String Control }

function LeftStr(const Str: string; Count: Integer): string;
begin
  Result:= Copy(Str, 1, Count);
end;

function RightStr(const Str: string; Count: Integer): string;
begin
  if Count >= Length(Str) then Result:= Str
  else Result:= Copy(Str, Length(Str) - Count + 1, Count);
end;

function KillSpaces(const Str: string): string;
var
	i: Integer;
begin
	Result:= '';
	for i:= 1 to Length(Str) do
		if Str[i] <> ' ' then Result:= Result + Str[i];
end;

function ReplaceInStr(SubStr, S, Replacer: string): string;
var
  i: Integer;
begin
  i:= Pos(SubStr, S);
  if i <> 0 then
     Result:= Copy(S, 1, i-1) + Replacer + Copy(S, i + Length(SubStr), Length(S))
  else
     Result:= S;
end;

function ReplaceInStrUpperCase(SubStr, S, Replacer: string): string;
var
  k: Integer;
begin
  k:= Pos(UpperCase(SubStr), UpperCase(S));
  if k <> 0 then
     Result:= Copy(S, 1, k-1) + Replacer + Copy(S, k + Length(SubStr), Length(S))
  else
     Result:= S;
end;

function RemoveChars(const Str: string; Chars: array of Char): string;
var i: Integer;
    C: string;
begin
  C:= ''; Result:= '';
  for i:= Low(Chars) to High(Chars) do C:= C + Chars[i];
  for i:= 1 to Length(Str) do
     if Pos(Str[i], C) = 0 then Result:= Result + Str[i];
end;

function ConvertAccentuated(const Str: string): string;
var i, j: Integer;
    C: Char;
    procedure AjouterDoublon(C1, C2: Char);
    begin
      C:= #0;
      Result[j]:= C1;
      Insert(C2, Result, j + 1);
      Inc(j);
    end;
begin
  Result:= Str;
  i:= 1;
  j:= 1;
  while i <= Length(Str) do
  begin
    case Str[i] of
      'À', 'Á', 'Ã', 'Å', 'Â', 'Ä': C:= 'A';
      'à', 'â', 'ä', 'á', 'ã', 'å': C:= 'a';
      'É', 'È', 'Ê', 'Ë': C:= 'E';
      'è', 'é', 'ê', 'ë': C:= 'e';
      'Î', 'Ï', 'Ì', 'Í': C:= 'I';
      'î', 'ï', 'ì', 'í': C:= 'i';
      'Ô', 'Ö', 'Ò', 'Ó', 'Õ': C:= 'O';
      'ò', 'ó', 'ô', 'õ', 'ö': C:= 'o';
      'Ù', 'Ú', 'Û', 'Ü': C:= 'U';
      'ù', 'ú', 'û', 'ü': C:= 'u';
      'Ç': C:= 'C';
      'ç': C:= 'c';
      'Ý', 'Ÿ': C:= 'Y';
      'ý', 'ÿ': C:= 'y';
      'Æ': AjouterDoublon('A', 'E');
      'æ': AjouterDoublon('a', 'e');
      'Œ': AjouterDoublon('O', 'E');
      'œ': AjouterDoublon('o', 'e');
      'ñ': C:= 'n';
      else
        C:= Str[i];
    end;
    if C <> #0 then
      Result[j]:= C;
    Inc(i);
    Inc(j);
  end;
end;

function Standardize(const Str: string): string;
var
  i: Integer;
begin
  SetLength(Result, Length(Str));
  for i:= 1 to Length(Str) do
  begin
    if Str[i] > 'z' then
    begin
      case Str[i] of
        'À', 'Á', 'Ã', 'Å', 'Â', 'Ä', 'à', 'â', 'ä', 'á', 'ã', 'å': Result[i]:= 'A';
        'É', 'È', 'Ê', 'Ë', 'è', 'é', 'ê', 'ë': Result[i]:= 'E';
        'Î', 'Ï', 'Ì', 'Í', 'î', 'ï', 'ì', 'í': Result[i]:= 'I';
        'Ô', 'Ö', 'Ò', 'Ó', 'Õ', 'ò', 'ó', 'ô', 'õ', 'ö': Result[i]:= 'O';
        'Ù', 'Ú', 'Û', 'Ü', 'ù', 'ú', 'û', 'ü': Result[i]:= 'U';
        'Ç', 'ç': Result[i]:= 'C';
        'Ý', 'Ÿ', 'ý', 'ÿ': Result[i]:= 'Y';
        'æ', 'Æ':
        begin
          Result[i]:= 'A';
          Insert('E', Result, i + 1);
        end;
        'Œ', 'œ':
        begin
          Result[i]:= 'O';
          Insert('E', Result, i + 1);
        end;
        'ñ': Result[i]:= 'N';
      end;
    end else if Str[i] >= 'a' then
      Result[i]:= UpCase(Str[i]);
  end;
end;

function ReverseStr(const Str: string): string;
var i, L: Integer;
begin
  L:= Length(Str);
  SetLength(Result, L);
  for i:= 1 to L do
     Result[L + 1 - i]:= Str[i];
end;

function ExtractQuotedStr(Src: string; Quote: Char): string;
var i: Integer;
begin
  Result:= '';
  if (Src = '') or (Src[1] <> Quote) or (Src[Length(Src)] <> Quote) then Exit;
  Src:= Copy(Src, 2, Length(Src) - 2);
  i:= 1;
  while i <= Length(Src) do
  begin
    if Src[i] = Quote then
    begin
      Inc(i);
      if (i > Length(Src)) or (Src[i] <> Quote) then
      begin
        Result:= '';
        Exit;
      end else
        Result:= Result + Src[i];
    end else
      Result:= Result + Src[i];
    Inc(i);
  end;
end;

function CaseLessPos(SubStr: string; S: string): Integer;
begin
  Result:= Pos(UpperCase(SubStr), UpperCase(S));
end;

{ Date Control }

  { Date Conversion }
function FullDate(Date: TDateTime): string;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  if (M <= 0) or (J <= 0) then begin Result:= ''; Exit; end;
  if J = 1 then
     Result:= LongDayNames[DayOfWeek(Date)] + ' 1er '
         + LongMonthNames[M] + ' ' + IntToStr(Y)
  else
     Result:= LongDayNames[DayOfWeek(Date)] + ' ' + IntToStr(J)
         + ' ' + LongMonthNames[M] + ' ' + IntToStr(Y);
end;

function SuperiorDate(Date: TDateTime): string;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  if (M <= 0) or (J <= 0) then begin Result:= ''; Exit; end;
  if J = 1 then
     Result:= '1er ' + LongMonthNames[M] + ' ' + IntToStr(Y)
  else
     Result:= IntToStr(J) + ' ' + LongMonthNames[M] + ' ' + IntToStr(Y);
end;

function HalfDate(Date: TDateTime): string;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  if (M <= 0) or (J <= 0) then begin Result:= ''; Exit; end;
  if J = 1 then
     Result:= LongDayNames[DayOfWeek(Date)] + ' 1er ' + LongMonthNames[M]
  else
     Result:= LongDayNames[DayOfWeek(Date)] + ' ' + IntToStr(J) + ' ' + LongMonthNames[M];
end;

function SmallDate(Date: TDateTime): string;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  if J = 1 then
     Result:= '1er ' + LongMonthNames[M]
  else
     Result:= IntToStr(J) + ' ' + LongMonthNames[M];
end;

  { Time Conversion }
function FullTime(Time: TDateTime): string;
var
  H, M, S: Word;
begin
  DecodeTime(Time, H, M, S, S);
  Result:= IntToStr(H) + ' heure ' + DoubleCode(M);
end;

function HalfTime(Time: TDateTime): string;
var
  H, M, S: Word;
begin
  DecodeTime(Time, H, M, S, S);
  Result:= IntToStr(H) + 'H' + DoubleCode(M);
end;

function Temps(Time: TDateTime; MilliS: Boolean): string;
var
  H, M, S, MS: Word;
  function SpFormat(S: string; V: Integer): string;
  begin
     Result:= S;
     if V > 1 then Result:= Result + 's';
  end;
begin
  DecodeTime(Time, H, M, S, MS);
  Result:= IntToStr(H) + SpFormat(' heure', H);
  Result:= Result + ', ' + IntToStr(M) + SpFormat(' minute', M);
  Result:= Result + ', ' + IntToStr(S) + SpFormat(' seconde', S);
  if MilliS then
     Result:= Result + ', ' + IntToStr(MS) + SpFormat(' milliseconde', MS);
end;

function HalfTemps(Time: TDateTime): string;
var
  H, M, S, MS: Word;
begin
  DecodeTime(Time, H, M, S, MS);
  Result:= IntToStr(H) + ' h ' + IntToStr(M) + ' m ' +
     IntToStr(S) + ' s ' + IntToStr(MS) + ' ms';
end;

function HourToTime(H: String): TDateTime;
var i: Integer;
begin
  if H = '' then Result:= 0
  else begin
     for i:= 1 to Length(H) do
     begin if H[i] in ['h', 'H'] then H[i]:= ':' end;
     Result:= StrToTime(H);
  end;
end;

  { Date Manipulation }
function IncMonth(Date: TDateTime; NbMonths: Integer): TDateTime;
var
  Y, M, J: Word;
  EndOfMonth: Boolean;
begin
  DecodeDate(Date, Y, M, J);
  EndOfMonth:= ExtractMonth(Date + 1) <> M; 
  if NbMonths >= 0 then
  begin
    Inc(M, NbMonths);
    while M >= 13 do
    begin
      Inc(Y);
      Dec(M, 12);
    end;
  end else begin
    Inc(M, NbMonths); { un WORD est toujours Positif ! }
    while (M > 12) or (M = 0) do
    begin
      Dec(Y);
      Inc(M, 12);
    end;
  end;
  case M of
    4, 6, 9, 11: if J = 31 then J:= 30;
  end;
  if (M = 2) and (J > 28) then
  begin
    try
      Result:= EncodeDate(Y, M, J);
    except
      Result:= EncodeDate(Y, M, 28);
    end;
  end else
    Result:= EncodeDate(Y, M, J);
      { Préservation de la fin du mois }
  if EndOfMonth then
    Result:= IncMonth(EncodeDate(ExtractYear(Result), ExtractMonth(Result), 1), 1) - 1;
end;

function ExtractYear(Date: TDateTime): Integer;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  Result:= Y;
end;

function ExtractMonth(Date: TDateTime): Integer;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  Result:= M;
end;

function ExtractDay(Date: TDateTime): Integer;
var
  Y, M, J: Word;
begin
  DecodeDate(Date, Y, M, J);
  Result:= J;
end;

function DateTimeToSec(Date: TDateTime): Extended;
begin
  Result:= Date * 24 * 3600;
end;

function WeekOfDate(Date: TDateTime): Integer;
var
  Y, T: Word;
  DebYear, SecondWeek: TDateTime;
begin
  DecodeDate(Date, Y, T, T);
  DebYear:= EncodeDate(Y, 1, 1);
  T:= DayOfWeek(DebYear) - 2;
  if SmallInt(T) = -1 then T:= 6;
{ Si Lundi 1er janvier, alors Week 2 = Lundi 8 janvier
  Si Dimanche 1er janvier, alors Week 2 = Lundi 2 janvier }
  SecondWeek:= DebYear + 7 - T;
  T:= Trunc(Date - SecondWeek); { Nombre de jours depuis le début de Week 2 }
  if T > 32000 then { T est un WORD ! => < 0 => très grand ... }
     Result:= 1
  else
     Result:= T div 7 + 2;  { Ex : 5 => 2, 8 => 3, 363 => 53 }
end;

  { Time Control }
function SecToDateTime(NbSeconds: Extended): TDateTime;
var H, M, S, MS: Word;
begin
  H:= Trunc(NbSeconds / 3600);
  M:= Trunc(NbSeconds / 60);
  S:= Trunc(NbSeconds);
  MS:= Round(Frac(NbSeconds) * 1000);
  Result:= EncodeTime(H, M, S, MS);
end;

function RDTSC: Double;
asm
 	DB 0Fh,31h
  Mov DWORD PTR [Result],EAX
  Mov DWORD PTR [Result + 4],EDX
  Fild QWORD PTR [Result]
  Fstp QWORD PTR [Result]
end;

end.
