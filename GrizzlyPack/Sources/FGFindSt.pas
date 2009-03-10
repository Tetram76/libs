unit FGFindSt;

interface

Uses DB, SysUtils;

Type
	TFindDirection = (fdFirst,fdLast,fdNext,fdPrior);

Function FindString(ProposedString : String; TheDataSet : TDataSet;
  LabelField : TStringField; TheDirection : TFindDirection) : Boolean;
function MultiWordMatch(Pattern, Str: string): Boolean;

implementation

Uses FGFindFn;

Function FindString(ProposedString : String;TheDataSet : TDataSet;
  LabelField : TStringField;TheDirection : TFindDirection) : Boolean;
Var
  BM : TBookMark;
  TailleProposition : Longint;
  Sortie : Boolean;
  szProposition : Array[0..255] of Char;
  Function Trouve(ImgCh : String) : Boolean;
  Var
  	sz2 : PChar;
  Begin
  	sz2:=PChar(ImgCh);
  	Result:=StrCmp(sz2,szProposition);
  End;
begin
	Result:=False;
  If Length(ProposedString)=0 then
    Exit;
	ProposedString:=PurifyString(UpperCase(ProposedString));
  StrPCopy(szProposition,ProposedString);
	TailleProposition:=Length(ProposedString);
  If TailleProposition>0 then
  Begin
		TheDataSet.DisableControls;
		Try
		 	BM:=TheDataSet.GetBookMark;
		  Try
      	Case TheDirection of
	        fdFirst : TheDataSet.First;
	        fdNext : TheDataSet.Next;
	        fdPrior : TheDataSet.Prior;
	        fdLast : TheDataSet.Last;
        End;
				If Assigned(LabelField) then
			  Begin
					Result:=Trouve(LabelField.AsString);
	      	Case TheDirection of
		        fdFirst,fdNext : Sortie:=TheDataSet.EOF
          Else
		        Sortie:=TheDataSet.BOF;
  	      End;
					While (Not Result) and (Not Sortie) do
		    	Begin
		      	Case TheDirection of
			        fdFirst,fdNext : TheDataSet.Next
						Else
			        TheDataSet.Prior;
	  	      End;
						Result:=Trouve(LabelField.AsString);
		      	Case TheDirection of
			        fdFirst,fdNext : Sortie:=TheDataSet.EOF
            Else
			        Sortie:=TheDataSet.BOF;
	  	      End;
			    End;
	      End
	      Else
	      Begin
					Result:=Trouve(TheDataSet.FieldByName('Libelle').AsString);
	      	Case TheDirection of
		        fdFirst,fdNext : Sortie:=TheDataSet.EOF;
          Else
		        Sortie:=TheDataSet.BOF;
  	      End;
					While (Not Result) and (Not Sortie) do
		    	Begin
		      	Case TheDirection of
			        fdFirst,fdNext : TheDataSet.Next;
			        fdPrior,fdLast : TheDataSet.Prior;
	  	      End;
						Result:=Trouve(TheDataSet.FieldByName('Libelle').AsString);
		      	Case TheDirection of
			        fdFirst,fdNext : Sortie:=TheDataSet.EOF;
			        fdPrior,fdLast : Sortie:=TheDataSet.BOF;
  		      End;
			    End;
	      End;
	  	  If Not Result then
		    	TheDataSet.GotoBookMark(BM);
	    Finally
	    	TheDataSet.FreeBookMark(BM);
	    End;
		Finally
	  	TheDataSet.EnableControls;
	  End;
  End;
end;

function MultiWordMatch(Pattern, Str: string): Boolean;
var Idx1, Idx2: Integer;
  function GotoNextStrWord: Boolean;
  begin
    while (Idx2 <= Length(Str)) and (Str[Idx2] <> ' ') do
      Inc(Idx2);
    while (Idx2 <= Length(Str)) and (Str[Idx2] <> ' ') do
      Inc(Idx2);
    Result:= Idx2 <= Length(Str);
    Dec(Idx2);
  end;
begin
  Pattern:= UpperCase(Pattern);
  Str:= UpperCase(Str);
  Result:= Length(Pattern) * Length(Str) <> 0;
  while Pos('  ', Pattern) <> 0 do
    Delete(Pattern, Pos('  ', Pattern), 1);
  Idx1:= 1; Idx2:= 1;
  while (Idx1 <= Length(Pattern)) and Result do
  begin
    if Pattern[Idx1] = ' ' then
    begin
      if not GotoNextStrWord then
        Exit;
    end else
      Result:= Pattern[Idx1] = Str[Idx2];
    Inc(Idx1);
    Inc(Idx2);
  end;
end;

end.
