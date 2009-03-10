{***************************************************************
 *
 * Unit Name: AClasses
 * Purpose  : Useful classes
 * Author   : Alexandre GUILLIEN
 * History  :
 *
 ****************************************************************}

unit SharedUtils;

interface

uses SysUtils, Classes;

type
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

    { Generic Sort/Search procs }
  TCompareProc = function(P1, P2: Pointer): Integer of object;

  procedure GenericQuickSort(P: Pointer; SizeInfos: Integer;
     CompareProc: TCompareProc; StartIndex, EndIndex: Integer);
  function GenericDichoSearch(P: Pointer; SizeInfos: Integer;
     CompareProc: TCompareProc; StartIndex, EndIndex: Integer;
     const Item; var Index: Integer; Duplicates: Boolean): Boolean; { ==> non trouvé }
//  function IntegerCompare(P1, P2: Pointer): Integer;

  function IMin(I1, I2: Integer): Integer;
  function IMax(I1, I2: Integer): Integer;

implementation

  { P: Pointeur sur le début du tableau, SizeInfos = taille d'un élément,
    CompareProc: fonction de comparaison, Deb, Fin: Index début & fin }
procedure GenericQuickSort(P: Pointer; SizeInfos: Integer;
  CompareProc: TCompareProc; StartIndex, EndIndex: Integer);
var
  TempItem: Pointer;
  function PItem(Index: Integer): Pointer;
  begin
    Result:= Pointer(LongInt(P) + Index * SizeInfos);
  end;
  procedure LocalQSort(iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Med: Integer;
  begin
    Lo:= iLo;
    Hi:= iHi;
    { Middle element }
    Med:= (Lo + Hi) div 2;
    repeat
      while CompareProc(PItem(Lo), PItem(Med)) < 0 do Inc(Lo);
      while CompareProc(PItem(Hi), PItem(Med)) > 0 do Dec(Hi);
      if Lo <= Hi then
      begin
        { Exchange }
        Move(PItem(Lo)^, TempItem^, SizeInfos);
        Move(PItem(Hi)^, PItem(Lo)^, SizeInfos);
        Move(TempItem^, PItem(Hi)^, SizeInfos);
        if (Lo = Med) then
          Med:= Hi
        else if (Hi = Med) then
          Med:= Lo;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if iLo < Hi then LocalQSort(iLo, Hi);
    if Lo < iHi then LocalQSort(Lo, iHi);
  end;
begin
  GetMem(TempItem, SizeInfos);
  LocalQSort(StartIndex, EndIndex);
  FreeMem(TempItem, SizeInfos);
end;

{ Note : résultat négatif ==> non trouvé }
function GenericDichoSearch(P: Pointer; SizeInfos: Integer;
  CompareProc: TCompareProc; StartIndex, EndIndex: Integer; const Item;
  var Index: Integer; Duplicates: Boolean): Boolean;
var
  Cmp, Med, InitialEnd: Integer;
begin
  InitialEnd:= EndIndex;
  while (StartIndex < EndIndex) do
  begin
    Med:= (StartIndex + EndIndex) div 2;
    Cmp:= CompareProc(Pointer(LongInt(P) + Med * SizeInfos), @Item);
    if Cmp = 0 then
    begin
      if Duplicates then // if there are duplicates, the search must continue until the first
        EndIndex:= Med
      else
      begin
        StartIndex:= Med;
        EndIndex:= Med;
      end;
    end
    else if Cmp > 0 then
      EndIndex:= Med
    else
      StartIndex:= Med + 1;
  end;
  Cmp:= CompareProc(Pointer(LongInt(P) + StartIndex * SizeInfos), @Item);
  if Cmp = 0 then
    Index:= StartIndex
  else if (Cmp < 0) and (EndIndex = InitialEnd) then
    Index:= - StartIndex - 1
  else
    Index:= - StartIndex;
  Result:= Cmp = 0;
end;

function IntegerCompare(P1, P2: Pointer): Integer;
begin
  Result:= PInteger(P1)^ - PInteger(P2)^;
end;

function IMin(I1, I2: Integer): Integer;
begin
  if I1 > I2 then Result:= I2 else Result:= I1;
end;

function IMax(I1, I2: Integer): Integer;
begin
  if I1 < I2 then Result:= I2 else Result:= I1;
end;

initialization

end.
