Unit FGFindFn;

Interface

function StrCmp(FullStr, StrToFind: PChar): Boolean;

function Purify(StrToPurify: PChar): PChar;

Function PurifyString(AStr : String) : String;

Implementation

Uses SysUtils;

function Purify(StrToPurify: PChar): PChar;
var
	i, j: Integer;
  Resultat: Array [0..255] of Char;
begin
	i:= 0;
  j:= 0;
  StrPCopy(Resultat,'');
  repeat
  	if StrToPurify[i] = ' ' then
    begin
    	repeat
      	i:= i + 1;
      until (StrToPurify[i] <> ' ');
    	Resultat[j]:= ' ';
      j:= j + 1;
      Resultat[j]:= Chr(0);
    end
    else if StrToPurify[i] = Chr(0) then
    	i:= 32000
    else
    begin
    	Resultat[j]:= StrToPurify[i];
      j:= j + 1;
      Resultat[j]:= Chr(0);
      i:= i + 1;
    end;
  until i = 32000;
  Purify:= Resultat;
end;

Function PurifyString(AStr : String) : String;
var
	PrecedentEstSpc : Boolean;
  i : Integer;
Begin
	If Length(AStr)=0 then
  	Exit;
	PrecedentEstSpc := False;
  i:=1;
  While i<=Length(AStr) do
	Begin
   	If (AStr[i]=#32) then
    Begin
    	If PrecedentEstSpc then
      Begin
	      Delete(AStr,i,1);
  	    Dec(i);
      End
      Else
      	PrecedentEstSpc:=True;
    End
    Else
    	PrecedentEstSpc:=False;
    Inc(i);
  End;
  Result:=AStr;
End;

function StrCmp(FullStr, StrToFind: PChar): Boolean;
begin
{ Cette fonction nécessite un StrToFind épuré ... }
    asm
    	mov ECX,FullStr    { [ECX] = Chaîne principale }
     	mov EDX,StrToFind   { [EDX] = Chaîne secondaire }
@Bcl:
     	mov AL,[ECX]
     	cmp AL,[EDX] { FullStr[i] = StrToFind[i] ? non = pas bon}
     	je @FindSCSecond
      cmp AL,5Ah    { 'Z' }
     	ja @Minuscule
      add AL,40h    { + 40h - 20h = + 20h = Min }
@Minuscule:
			sub AL,20h     { - 20h = Maj }
     	cmp AL,[EDX]
     	jne @PasBon
@FindSCSecond:
     	cmp BYTE PTR [EDX+1],20h
     	je @FindCPSecond    { StrToFind[i] = ' ' ? oui => ' ' FullStr ?}
     	cmp BYTE PTR [EDX+1],0
     	je @ItsOK        { StrToFind[i] = Fini ? oui => OK}
      inc EDX
     	cmp BYTE PTR [ECX+1],20h
     	je @PasBon           { FullStr[i] = ' ' ? oui => Pas bon}
      cmp BYTE PTR [ECX+1],0
     	je @PasBon        { FullStr[i] = Fini ? oui => Pas bon}
     	inc ECX
     	jmp @Bcl
@FindCPSecond:
     	add EDX,2    { StrToFind[i] = Next Cara of next substr }
@BclFindCPSecond:
			cmp BYTE PTR [ECX],0
     	je @PasBon    { Fin FullStr => Pas Bon }
      inc ECX
     	cmp BYTE PTR [ECX],20h
     	jne @BclFindCpSecond  { Bcl pour trouver ' ' }
     	inc ECX
     	jmp @Bcl
@PasBon:
      mov Result,0
     	jmp @Fin
@ItsOK:
      mov Result,1
@Fin:
	End;
end;

End.
