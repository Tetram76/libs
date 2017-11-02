unit FGUtils;

{Auteur : Frédéric GUILLIEN}
{Date : Août 1997}

interface

uses Classes{$IFDEF LINUX}, Types{$ELSE}, Windows{$ENDIF};

function GetDelimitedString(StartStr, EndStr, Source : string) : string;
function GetDelimitedInteger(StartStr, EndStr, Source : string) : Integer;

procedure Wait(MilliSeconds: Cardinal);

function SecondToDateTime(ASecondCount: Longint): TDateTime;
function HourToDateTime(AnHourCount: Longint): TDateTime;
function MinuteToDateTime(AMinuteCount: Longint): TDateTime;

function DateTimeToSecond(ADateTime: TDateTime): Longint;
function DateTimeToMinute(ADateTime: TDateTime): Longint;
function DateTimeToHour(ADateTime: TDateTime): Longint;

{$J+}
const
  WaitFormat : string = 'hhh:nn:ss zzz';
{$J-}

function FormatWaitTime(BeginningDate, EndDate: TDateTime) : string;

function FormatHistoTime(ADateTime : TDateTime) : string;

function EstimateEndDate(Progress, Total : Extended; BeginningDate : TDateTime) : TDateTime;

{$J+}
const
  AgeFormat : string = '%d ans';
{$J-}

function AgeToFloat(BeginningDate, EndDate: TDateTime): Extended;
function AgeToStr(BeginningDate, EndDate: TDateTime): string;

function BoolToStr(ABool: Boolean): string;

function StrToBool(AString: string): Boolean;

function WordByPos(APos: Integer; ASeparator: Char; AStr: string): string;

{Compte le nombre de caractères AChar dans le texte ATxt}
function CharCount(ATxt: string; AChar: Char): Longint;

function StrWordByPos(APos: Integer; ASeparator: Char; AStr: PChar): PChar;

{Compte les mots, et les met dans le tableau AArray, en prenant en compte les ""}
function ParseWords(ATxt : string; ALst : TStrings) : Integer;

{Compte le nombre de caractères AChar dans le texte ATxt}
function StrCharCount(ATxt: PChar; AChar: Char): Longint;

{Remplace tous les caractères de contrôle (<#32) par des espaces}
function WashedString(const AString: string): string;
{Supprime tous les caractères de contrôle (<#32)}
function VeryWashedString(const AString: string): string;

{Ajoute s'il n'y est pas le / final d'un chemin}
function AddSlash(Ch: string): string;

{Enlève le / final d'un chemin}
function RemoveSlash(Ch: string): string;

{Pour mettre en liste les répertoires d'un chemin}
procedure ExplodePath(const APath : string; ALst : TStrings);

{Pour vérifier que le caractère est bien numérique et que la virgule est correcte}
function NumericCheck(ASource: string; var Key: Char; WithDecimal: Boolean): Boolean;

{Pluriel automatique}
function AutomaticPlural(ANombre: Double; AChaine: string; ALettre: string): string;

{Encadrement SQL automatique correct}
function GoodSQLFrame(AString: string): string;

function ReplaceText(Old, New, Source: string): string;
function DeleteText(Text, Source: string): string;

{Pour extraire un élément de date aisément...}
function ExtractYear(const ADate: TDateTime): Word;
function ExtractMonth(const ADate: TDateTime): Word;
function ExtractDay(const ADate: TDateTime): Word;
function TodayYear: Word;
function TodayMonth: Word;
function TodayDay: Word;
function DaysInMonth(const ADate: TDateTime): Word;
function GetAge(const ADate: TDateTime): Integer;
function FirstDayOfMonth(const ADate: TDateTime): TDateTime;
function LastDayOfMonth(const ADate: TDateTime): TDateTime;

function WeekNumber(ADate: TDateTime): Integer;
function BeginningOfWeek(AYear, AWeek: Integer): TDateTime;
function HasWeek53(AYear: Integer): Boolean;

(*{$IFNDEF WIN32}
function Trim(const AStr: string): string;
{$ENDIF}*)

{Appele Free des objets présents dans la liste et appele Clear}
procedure ClearStringList(AStringList: TStrings);
{Appele Free des objets présents dans la liste et appele Clear}
procedure ClearList(AList: TList);

function StretchProportionate(ARealSize, AFinalSize: TPoint): TPoint;

{$IFDEF WIN32}
type
  TProfileInfo = record
    dwSize : DWORD;
    dwFlags : DWORD;
    lpUserName : LPTSTR;
    lpProfilePath : LPTSTR;
    lpDefaultPath : LPTSTR;
    lpServerName : LPTSTR;
    lpPolicyPath : LPTSTR;
    hProfile : THandle;
  end;

const
  PI_NOUI : DWORD = 1;  //Empêche il d'afficher message d'erreur de profil.
  PI_APPLYPOLICY : DWORD = 2; //Applique une stratégie Windows NT 4.0.

function LoadUserProfile(hToken : THandle; var lpProfileInfo : TProfileInfo): BOOL; stdcall; external 'userenv.dll' name 'LoadUserProfileA';
function UnloadUserProfile(hToken : THandle;hProfile : THandle): BOOL ; stdcall; external 'userenv.dll';

procedure RegisterProtocolHandler(Protocol, Icon, Command : string);
function ExpandEnvironmentStrings(AEnvVar : string): string;
function GetEnvironmentVariable(AEnvVar : string): string;
function GetClientName: string;
function GetComputerName: string;
function GetUserName: string;
function GetWindowCaption(AHWnd: HWnd): string;
{Pour trouver le Handle Windows d'une fenêtre par son titre}
function GetWindowByCaption(ACaption: string): Integer;
{Pour décompresser un fichier compresser avec COMPRESS.EXE}
procedure LZExpandFile(AFileName, Destination: string);
procedure RegistryAutoRunProgram(AExeName, AExePath: string);
{$ENDIF}

function HTMLToPlainText(const HTML: String): String;// Taken from Gary Williams in the newsgroups

function ComplexeLike(FieldName : string; Words : string; OrClause : Boolean = False) : string;
function ComplexeLikeFilter(FieldName : string; Words : string) : string;

type
  TCSVParsingEvent = procedure(ACol : Integer; AColName : string; ARow : Integer; AValue : string) of object;

procedure ParseCSVClipboard(const AString : string; const AFieldSeparator : string; const AWithHeaders : Boolean; ParseEvent : TCSVParsingEvent);

function FormatFileSize(ASize : Int64) : string;

implementation

uses {$IFNDEF LINUX}LZExpand, {$ENDIF}SysUtils, IniFiles{$IFDEF WIN32}, Registry{$ENDIF},
  UFGUtils, FGFiles, Math;

procedure Wait(MilliSeconds: Cardinal);
var
  Arrivee: TDateTime;
begin
  Arrivee:= Now + MilliSeconds / 1000 / 24 / 60 / 60;
  repeat
    Sleep(0);
  until (Now > Arrivee);
end;

function SecondToDateTime(ASecondCount: Longint): TDateTime;
begin
  Result:= ASecondCount / 24 / 60 / 60;
end;

function MinuteToDateTime(AMinuteCount: Longint): TDateTime;
begin
  Result:= AMinuteCount / 24 / 60;
end;

function HourToDateTime(AnHourCount: Longint): TDateTime;
begin
  Result:= AnHourCount / 24;
end;

function DateTimeToSecond(ADateTime: TDateTime): Longint;
begin
  Result:= Trunc(ADateTime * 24 * 60 * 60);
end;

function DateTimeToMinute(ADateTime: TDateTime): Longint;
begin
  Result:= Trunc(ADateTime * 24 * 60);
end;

function DateTimeToHour(ADateTime: TDateTime): Longint;
begin
  Result:= Trunc(ADateTime * 24);
end;

function AgeToFloat(BeginningDate, EndDate: TDateTime): Extended;
begin
  Result:= (EndDate - BeginningDate) / 365.25;
end;

function AgeToStr(BeginningDate, EndDate: TDateTime): string;
var
  Age: Extended;
begin
  Age:= (EndDate - BeginningDate) / 365.25;
  Result:= Format(AgeFormat, [trunc(Age), trunc(12 * (Age - trunc(Age)))]);
end;

function FormatWaitTime(BeginningDate, EndDate: TDateTime) : string;
begin
  Result:= FormatDateTime(WaitFormat, EndDate - BeginningDate);
end;

function FormatHistoTime(ADateTime : TDateTime) : string;
begin
  //Aujourd'hui, on ne met que l'heure
  if Trunc(ADateTime) = 0 then
    Result := ''
  else
  if Trunc(ADateTime) = Trunc(Now) then
    Result := 'Ce jour à ' + FormatDateTime('hh:nn', ADateTime)
  else
  //Hier, on écrit "Hier" avec l'heure
  if Trunc(ADateTime) = Trunc(Now - 1) then
    Result := 'Hier à ' + FormatDateTime('hh:nn', ADateTime)
  else
  //Sinon, si on est le même mois... On ne met que le jour avec l'heure
  if ExtractMonth(ADateTime) = ExtractMonth(Now) then
    Result := 'Le ' + FormatDateTime('d à hh:nn', ADateTime)
  else
  //Sinon, si on est la même année... On ne met que le jour et le mois avec l'heure
  if ExtractYear(ADateTime) = ExtractYear(Now) then
    Result := 'Le ' + FormatDateTime('d/mm à hh:nn', ADateTime)
  else
  //Sinon, on met tout
    Result := 'Le ' + FormatDateTime('d/mm/yyyy à hh:nn', ADateTime);
end;

//APos > 0
function WordByPos(APos: Integer; ASeparator: Char; AStr: string): string;
var
  i, k: Integer;
  MotEnCours: string;
begin
  Result:= '';
  if (AStr = '') or (APos <= 0) then
    Exit;
  i:= 0;
  repeat
    Inc(i);
    k:= Pos(ASeparator, AStr);
    if k > 0 then
    begin
      if i <> APos then
        Delete(AStr, 1, k)
      else
        MotEnCours:= Copy(AStr, 1, k - 1);
    end
    else
    begin
      if i <> APos then
        Exit
      else
      begin
        MotEnCours:= AStr;
        Break;
      end;
    end;
  until (i = APos) or (AStr = '');
  if i = APos then
    Result:= Trim(MotEnCours);
end;

function StrWordByPos(APos: Integer; ASeparator: Char; AStr: PChar): PChar;
var
  i, Debut, ATaille, NoMot: Integer;
begin
  Result:= nil;
  if APos <= 0 then
    Exit;
  NoMot:= 1;
  Debut:= 0;
  i:= 0;
  ATaille:= StrLen(AStr);
  while (i < ATaille) do
  begin
    if AStr[i] = ASeparator then
    begin
      if NoMot = APos then
      begin
        ATaille:= i - Debut;
        GetMem(Result, ATaille + 1);
        try
          StrLCopy(Result, AStr + Debut, ATaille);
        except
          FreeMem(Result, ATaille + 1);
          raise;
        end;
        Exit;
      end
      else
      begin
        Inc(NoMot);
        Debut:= i + 1;
      end;
    end;
    Inc(i);
  end;
  if (i = ATaille) and (APos = NoMot) then
  begin
    ATaille:= i - Debut;
    GetMem(Result, ATaille + 1);
    try
      StrLCopy(Result, AStr + Debut, ATaille);
    except
      FreeMem(Result, ATaille + 1);
      raise;
    end;
  end
end;

function CharCount(ATxt: string; AChar: Char): Longint;
var
  i: Longint;
begin
  Result:= 0;
  for i:= 1 to Length(ATxt) do
  begin
    if ATxt[i] = AChar then
      Inc(Result);
  end;
end;

function StrCharCount(ATxt: PChar; AChar: Char): Longint;
var
  i: Longint;
begin
  Result:= 0;
  for i:= 0 to StrLen(ATxt) - 1 do
  begin
    if ATxt[i] = AChar then
      Inc(Result);
  end;
end;

function BoolToStr(ABool: Boolean): string;
begin
  Result:= IntToStr(Ord(ABool));
end;

function StrToBool(AString: string): Boolean;
begin
  Result:= Boolean(StrToInt(AString));
end;

function WashedString(const AString: string): string;
var
  i: Integer;
begin
  Result:= AString;
  if Length(Result) > 0 then
    for i:= 1 to Length(Result) do
    begin
      if Result[i] < #32 then
        Result[i]:= #32;
    end;
end;

function VeryWashedString(const AString: string): string;
var
  i: Integer;
begin
  Result:= AString;
  i := Length(Result);
  while (Length(Result) > 0) and (i > 0) do
  begin
    if Result[i] < #32 then
      Delete(Result, i, 1);
    Dec(i);
  end;
end;

procedure ClearStringList(AStringList: TStrings);
var
  LObject: TObject;
  i: Integer;
begin
  if Assigned(AStringList) then
  begin
    if AStringList.Count > 0 then
    begin
      for i:= 0 to AStringList.Count - 1 do
      begin
        LObject:= AStringList.Objects[i];
        try
          if LObject <> nil then
          begin
            LObject.Free;
            AStringList.Objects[i]:= nil;
          end;
        except
          {On fait le silence au cas où le pointeur soit mauvais...}
          raise; {pour faire les tests on le laisse...}
        end;
      end;
      AStringList.Clear;
    end;
  end;
end;

procedure ClearList(AList: TList);
var
  LObject: TObject;
  i: Integer;
begin
  if Assigned(AList) then
  begin
    if AList.Count > 0 then
    begin
      for i:= 0 to AList.Count - 1 do
      begin
        LObject:= AList.Items[i];
        try
          if Assigned(LObject) then
          begin
            LObject.Free;
            AList.Items[i]:= nil;
          end;
        except
          {On fait le silence au cas où le pointeur soit mauvais...}
          raise; {pour faire les tests on le laisse...}
        end;
      end;
      AList.Clear;
    end;
  end;
end;

(*
{$IFNDEF WIN32}
function Trim(const AStr: string): string;
begin
  Result:= AStr;
  while (Length(Result) > 0) and (Result[Length(Result)] <= #32) do
    Delete(Result, Length(Result), 1);
  while (Length(Result) > 0) and (Result[1] <= #32) do
    Delete(Result, 1, 1);
end;
{$ENDIF}
*)

{$IFDEF LINUX}
const OSSlash = '/';
{$ELSE}
const OSSlash = '\';
{$ENDIF}

function AddSlash(Ch: string): string;
begin
  if (Ch <> '') and (Ch[Length(Ch)] <> OSSlash) then
    Result:= Ch + OSSlash
  else
    Result:= Ch;
end;

function RemoveSlash(Ch: string): string;
begin
  if (Ch <> '') and (Ch[Length(Ch)] = OSSlash) then
    Result:= Copy(Ch, 1, Length(Ch) - 1)
  else
    Result:= Ch;
end;

procedure ExplodePath(const APath : string; ALst : TStrings);
var
  ADossier, ANom : string;
  function NomDernier(const ARep : string) : string;
  begin
    Result := ExtractFileName(RemoveSlash(ARep));
  end;
begin
  ALst.Clear;
  ADossier := APath;
  ANom := NomDernier(ADossier);
  while ANom <> '' do
  begin
    if ALst.Count > 0 then
      ALst.Insert(0, ANom)
    else
      ALst.Add(ANom);
    ADossier := ExtractFilePath(RemoveSlash(ADossier));
    ANom := NomDernier(ADossier);
  end;
end;

function NumericCheck(ASource: string; var Key: Char; WithDecimal: Boolean): Boolean;
var
  AResultKey: Char;
begin
  AResultKey:= Key;
  if (not (Key in [#1..#31, '-', '0'..'9'])) then
  begin
    if WithDecimal and ((Key in [',', '.']) or (Key = DecimalSeparator)) then
    begin
      if (Pos(DecimalSeparator, ASource) > 0) then
        AResultKey:= #0
      else
      begin
        AResultKey:= DecimalSeparator;
        Key:= AResultKey;
      end;
    end
    else
      AResultKey:= #0;
  end;
  Result:= (Key = AResultKey);
  Key:= AResultKey;
end;

function AutomaticPlural(ANombre: Double; AChaine: string; ALettre: string): string;
var
  LastChar: Char;
begin
  if ALettre = '' then
    ALettre:= 's';
  if Length(AChaine) > 0 then
    LastChar:= UpperCase(AChaine[Length(AChaine)])[1]
  else
    LastChar:= ' ';
  if (ANombre <> 1) and (ANombre <> 0) and (not (LastChar in ['S', 'X'])) and (LastChar in ['A'..'Z', 'é', 'è', 'ê']) then
    Result:= AChaine + ALettre
  else
    Result:= AChaine;
  Result:= FloatToStr(ANombre) + ' ' + Result;
end;

function ExtractYear(const ADate: TDateTime): Word;
var
  mm, jj: Word;
begin
  DecodeDate(ADate, Result, mm, jj);
end;

function ExtractMonth(const ADate: TDateTime): Word;
var
  aa, jj: Word;
begin
  DecodeDate(ADate, aa, Result, jj);
end;

function ExtractDay(const ADate: TDateTime): Word;
var
  mm, aa: Word;
begin
  DecodeDate(ADate, aa, mm, Result);
end;

function TodayYear: Word;
var
  mm, jj: Word;
begin
  DecodeDate(Date, Result, mm, jj);
end;

function TodayMonth: Word;
var
  aa, jj: Word;
begin
  DecodeDate(Date, aa, Result, jj);
end;

function TodayDay: Word;
var
  mm, aa: Word;
begin
  DecodeDate(Date, aa, mm, Result);
end;

function DaysInMonth(const ADate: TDateTime): Word;
begin
{Reste à remplir les lignes...}
  Result:= 0;
end;

function GetAge(const ADate: TDateTime): Integer;
begin
  Result:= Trunc(AgeToFloat(ADate, Date));
end;

function FirstDayOfMonth(const ADate: TDateTime): TDateTime;
var
  aa, mm, jj: Word;
begin
  DecodeDate(ADate, aa, mm, jj);
  Result:= EncodeDate(aa, mm, 1);
end;

function LastDayOfMonth(const ADate: TDateTime): TDateTime;
var
  aa, mm, jj: Word;
  ADay: TDateTime;
begin
  DecodeDate(ADate, aa, mm, jj);
  case mm of
    1, 3, 5, 7, 8, 10, 12: jj:= 31;
    4, 6, 9, 11: jj:= 30;
    2:
      begin
        ADay:= EncodeDate(aa, mm, 28);
        while ExtractMonth(ADay) = mm do
          ADay:= ADay + 1;
        Result:= ADay - 1;
        Exit;
      end;
  end;
  Result:= EncodeDate(aa, mm, jj);
end;

function WeekNumber(ADate: TDateTime): Integer;
var
  aa, mm, jj: Word;
  A1, D1: TDateTime;
begin
  DecodeDate(ADate, aa, mm, jj);
  { Note : la première semaine est la semaine qui contient le premier Jeudi de l'année ... ! }

    { Si on est en Décembre ET que la fin du mois engendre une semaine 1 qui
      commence le premier Janvier, alors, la dernière semaine de l'année est la n° 1. }
  if (mm = 12) then
  begin
    A1:= EncodeDate(aa, 12, 31);
      { Si la fin du mois est un Lun, Mar, Mer, alors fin du mois = S1 ! }
    if DayOfWeek(A1) in [2..4] then
    begin
      D1:= A1;
      while not (DayOfWeek(D1) = 2) do
        D1:= D1 - 1;
      if (ADate >= D1) and (ADate <= A1) then
      begin
        Result:= 1;
        Exit;
      end;
    end;
  end;

    { => Si le premier jour est entre le Lundi et le Jeudi, alors Semaine 1 }
    { => Si le premier jour est entre le Vendredi et le Dimanche, alors Semaine d'avant }
  A1:= EncodeDate(aa, 1, 1);
  D1:= A1;
  if DayOfWeek(D1) in [2..5] then
  begin
    if DayOfWeek(D1) = 2 then
      Result:= ((Trunc(ADate - D1) div 7) mod 53) + 1
    else
    begin
      while DayOfWeek(D1) <> 2 do
        D1:= D1 + 1;
      if ADate < D1 then
        Result:= ((Trunc(ADate - D1) div 7) mod 53) + 1
      else
        Result:= ((Trunc(ADate - D1) div 7) mod 53) + 2;
    end;
  end
  else
  begin
    while DayOfWeek(D1) <> 2 do
      D1:= D1 + 1;
    if ADate < D1 then
      Result:= WeekNumber(A1 - 1)
    else
      Result:= ((Trunc(ADate - D1) div 7) mod 53) + 1;
  end;
end;

function BeginningOfWeek(AYear, AWeek: Integer): TDateTime;
var FirstWeek: Integer;
begin
  Result:= EncodeDate(AYear, 1, 1);
  FirstWeek:= WeekNumber(Result);
    { Le 1er janvier peut être soit Semaine 53, soit Semaine 1 }
  if FirstWeek in [52, 53] then
    while DayOfWeek(Result) <> 2 do
      Result:= Result + 1
  else
    while DayOfWeek(Result) <> 2 do
      Result:= Result - 1;
  Result:= Result + (AWeek - 1) * 7;
end;

function HasWeek53(AYear: Integer): Boolean;
begin
  Result:= WeekNumber(EncodeDate(AYear, 12, 31)) = 53;
end;

{$IFDEF WIN32}

//Registers a protocol handler allowing you to use
// protocol:myparameters in any url handler in Windows
// - Protocol : only the name without :
// - Icon : the name of a file containing the icon
// - Command : the command line with %1 for the parameter used in the url.
// The parameter will contain the protocol name.
// http://msdn.microsoft.com/library/default.asp?url=/workshop/networking/pluggable/overview/appendix_a.asp
procedure RegisterProtocolHandler(Protocol, Icon, Command : string);
var
  AReg : TRegistry;
  function Check : Boolean;
  begin
    Result := False;
    if (AReg.OpenKeyReadOnly(Protocol + '\DefaultIcon')) then
    begin
      Result := True;
      AReg.CloseKey;
    end;
  end;
  procedure LaunchException;
  begin
    raise ERegistryException.Create('Cannot register protocol handler');
  end;
begin
  AReg := TRegistry.Create;
  try
    AReg.RootKey := HKEY_CLASSES_ROOT;
    if not Check then 
    begin
      if AReg.OpenKey('gedeon', True) then
      begin
        try
          AReg.WriteString('', 'Protocol Handler for ' + Protocol);
          AReg.WriteString('URL Protocol', '');
        finally
          AReg.CloseKey;
        end;
        if AReg.OpenKey(Protocol + '\DefaultIcon', True) then
        begin
          try
            AReg.WriteString('', Command);
          finally
            AReg.CloseKey;
          end;
        end
        else
          LaunchException;
        if AReg.OpenKey(Protocol + '\shell\open\command', True) then
        begin
          try
            AReg.WriteString('', Command);
          finally
            AReg.CloseKey;
          end;
        end
        else
          LaunchException;
      end
      else
        LaunchException;
    end
    else
      AReg.CloseKey;
  finally
    AReg.Free;
  end;
end;

function ExpandEnvironmentStrings(AEnvVar : string): string;
var
  AString: array[0..255] of Char;
  AL: DWord;
begin
  AL:= 255;
  Windows.ExpandEnvironmentStrings(PChar(AEnvVar), AString, AL);
  Result:= StrPas(AString);
end;

function GetEnvironmentVariable(AEnvVar : string): string;
var
  AString: array[0..255] of Char;
  AL: DWord;
begin
  AL:= 255;
  FillChar(AString, AL, 0);
  Windows.GetEnvironmentVariable(PChar(AEnvVar), AString, AL);
  Result:= StrPas(AString);
end;

function GetClientName: string;
begin
  Result := GetEnvironmentVariable('CLIENTNAME');
  if Result = '' then
    Result := GetComputerName;
end;

function GetComputerName: string;
var
  AString: array[0..255] of Char;
  AL: DWord;
begin
  AL:= 255;
  FillChar(AString, AL, 0);
  Windows.GetComputerName(AString, AL);
  Result:= StrPas(AString);
end;

function GetUserName: string;
var
  AString: array[0..255] of Char;
  AL: DWord;
begin
  AL:= 255;
  FillChar(AString, AL, 0);
  Windows.GetUserName(AString, AL);
  Result:= StrPas(AString);
end;

function GetWindowCaption(AHWnd: HWnd): string;
var
  ACaption: array[0..255] of Char;
begin
  GetWindowText(AHWnd, ACaption, 255);
  Result:= StrPas(ACaption);
end;

procedure RegistryAutoRunProgram(AExeName, AExePath: string);
begin
  with TRegistry.Create do
  begin
    Rootkey:= HKEY_LOCAL_MACHINE;
    OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', True);
    WriteString(AExeName, AExePath);
    Free;
  end;
end;

procedure LZExpandFile(AFileName, Destination: string);
var
  ToFile: file;
  Depart: Integer;
  LEOfStruct: TOFSTRUCT;
  ChDepart, ChFin: array[0..255] of Char;
begin
  if Length(AFileName) = 0 then
    raise Exception.Create(msgBadFileName);
  StrPCopy(ChDepart, AFileName);
  GetExpandedName(ChDepart, ChFin);
  StrPCopy(ChFin, AddSlash(Destination) + ExtractFileName(StrPas(ChFin)));
  AssignFile(ToFile, ChFin);
  LeOfStruct.cBytes:= SizeOf(TOFStruct);
  Depart:= LZOpenFile(ChDepart, LeOfStruct, of_Read);
  try
    Rewrite(ToFile);
    try
      if LZCopy(Depart, TFileRec(ToFile).Handle) < 0 then
        raise EInOutError.Create(msgLZCopyError)
    finally
      CloseFile(ToFile);
    end;
  finally
    LZClose(Depart);
  end;
end;

function GetWindowByCaption(ACaption: string): Integer;
var
  szTitre: array[0..255] of Char;
begin
  StrPCopy(szTitre, ACaption);
  Result:= FindWindow(nil, szTitre);
end;

{$ENDIF}

function StretchProportionate(ARealSize, AFinalSize: TPoint): TPoint;
var
  Larg, Haut: Longint;
  DLarg, DHaut: Extended;
begin
  Haut:= AFinalSize.y;
  Larg:= AFinalSize.x;
  DHaut:= ARealSize.y / Haut;
  DLarg:= ARealSize.x / Larg;
  if (DHaut <> 1) or (DLarg <> 1) then
  begin
    if DHaut > DLarg then
    begin
      Result.y:= Haut;
      Result.x:= Trunc(ARealSize.x / DHaut);
    end
    else
    begin
      Result.x:= Larg;
      Result.y:= Trunc(ARealSize.y / DLarg);
    end;
  end
  else
  begin
    Result.x:= ARealSize.x;
    Result.y:= ARealSize.y;
  end;
end;

function GoodSQLFrame(AString: string): string;
const
  GoodChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789$';
var
  WithF: Boolean;
  i: Integer;
begin
  WithF:= False;
  if Length(AString) = 0 then
    Exit;
  Result:= AnsiUpperCase(AString);
  for i:= 1 to Length(AString) do
    if Pos(Result[i], GoodChars) <= 0 then
    begin
      WithF:= True;
      Break;
    end;
  if not WithF then
  begin
    if Result = 'DATE' then
      WithF:= True
    else if Result = 'ORDER' then
      WithF:= True
    else if Result = 'SELECT' then
      WithF:= True
    else if Result = 'FROM' then
      WithF:= True
    else if Result = 'WHERE' then
      WithF:= True;
  end;
  if WithF then
    Result:= '"' + AString + '"'
  else
    Result:= AString;
end;

function ReplaceText(Old, New, Source: string): string;
var
  p: Integer;
begin
  if Old = '' then
    Exit;
  Result:= Source;
  p:= 1;
  while p < Length(Result) do
  begin
    if Copy(Result, p, Length(Old)) = Old then
    begin
      Delete(Result, p, Length(Old));
      Insert(New, Result, p);
      Inc(p, Length(New));
    end
    else
      Inc(p);
  end;
end;

function DeleteText(Text, Source: string): string;
var
  p: Integer;
begin
  if Text = '' then
    Exit;
  Result:= Source;
  p:= 1;
  while p < Length(Result) do
  begin
    if Copy(Result, p, Length(Text)) = Text then
      Delete(Result, p, Length(Text))
    else
      inc(p);
  end;
end;

const
  HTML_MAPPINGS: array[160..255] of String = (
    '&nbsp;',
    '&iexcl;',
    '&cent;',
    '&pound;',
    '&curren;',
    '&yen;',
    '&brvbar;',
    '&sect;',
    '&uml;',
    '&copy;',
    '&ordf;',
    '&laquo;',
    '&not;',
    '&shy;',
    '&reg;',
    '&macr;',
    '&deg;',
    '&plusmn;',
    '&sup2;',
    '&sup3;',
    '&acute;',
    '&micro;',
    '&para;',
    '&middot;',
    '&cedil;',
    '&sup1;',
    '&ordm;',
    '&raquo;',
    '&frac14;',
    '&frac12;',
    '&frac34;',
    '&iquest;',
    '&Agrave;',
    '&Aacute;',
    '&Acirc;',
    '&Atilde;',
    '&Auml;',
    '&Aring;',
    '&AElig;',
    '&Ccedil;',
    '&Egrave;',
    '&Eacute;',
    '&Ecirc;',
    '&Euml;',
    '&Igrave;',
    '&Iacute;',
    '&Icirc;',
    '&Iuml;',
    '&ETH;',
    '&Ntilde;',
    '&Ograve;',
    '&Oacute;',
    '&Ocirc;',
    '&Otilde;',
    '&Ouml;',
    '&times;',
    '&Oslash;',
    '&Ugrave;',
    '&Uacute;',
    '&Ucirc;',
    '&Uuml;',
    '&Yacute;',
    '&THORN;',
    '&szlig;',
    '&agrave;',
    '&aacute;',
    '&acirc;',
    '&atilde;',
    '&auml;',
    '&aring;',
    '&aelig;',
    '&ccedil;',
    '&egrave;',
    '&eacute;',
    '&ecirc;',
    '&euml;',
    '&igrave;',
    '&iacute;',
    '&icirc;',
    '&iuml;',
    '&eth;',
    '&ntilde;',
    '&ograve;',
    '&oacute;',
    '&ocirc;',
    '&otilde;',
    '&ouml;',
    '&divide;',
    '&oslash;',
    '&ugrave;',
    '&uacute;',
    '&ucirc;',
    '&uuml;',
    '&yacute;',
    '&thorn;',
    '&yuml;');

function HTMLToPlainText(const HTML: String): String;// Taken from Gary Williams in the newsgroups
var
  P1: PChar;
  P2: PChar;
  InsideTag: Boolean;
  I: Integer;

begin
  SetLength(Result, Length(HTML));

  P1 := PChar(HTML);
  P2 := PChar(Result);

  InsideTag := False;

  repeat
    case P1^ of
      #0:
        Break;
      '<':
        InsideTag := True;
      '>':
        InsideTag := False;
      else
        if not(InsideTag) then
        begin
          P2^ := P1^;
          Inc(P2);
        end;
    end;

    Inc(P1);
  until (False);

  SetLength(Result, P2 - PChar(Result));

  for I := High(HTML_MAPPINGS) downto Low(HTML_MAPPINGS) do
    Result := StringReplace(Result, HTML_MAPPINGS[I], Chr(I),
[rfReplaceAll]);

  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

function ComplexeLike(FieldName : string; Words : string; OrClause : Boolean = False) : string;
var
  i, k : Integer;
  AClause, AMot : string;
  procedure AjouterCondition(ACond : string);
  begin
    if Result = '' then
      Result := FieldName + ' like ' + QuotedStr('%' + ACond + '%')
    else
      Result := Result + AClause + FieldName + ' like ' + QuotedStr('%' + ACond + '%');
  end;
begin
  if OrClause then
    AClause := ' or '
  else
    AClause := ' and ';
  Result := '';
  k := CharCount(Words, #32);
  if k > 0 then
  begin
    for i := 1 to k + 1 do
    begin
      AMot := Trim(WordByPos(i, #32, Words));
      if AMot <> '' then
        AjouterCondition(AMot);
    end;
  end
  else
  begin
    if Trim(Words) = '' then
      Result := '(' + FieldName + ' like ' + QuotedStr('%') + ' or ' + FieldName + ' is null)'
    else
      AjouterCondition(Words);
  end;
end;

function ComplexeLikeFilter(FieldName : string; Words : string) : string;
var
  i, k : Integer;
  AMot : string;
  procedure AjouterCondition(ACond : string);
  begin
    if Result = '' then
      Result := FieldName + '=' + QuotedStr('*' + ACond + '*')
    else
      Result := Result + ' and ' + FieldName + '=' + QuotedStr('*' + ACond + '*');
  end;
begin
  Result := '';
  k := CharCount(Words, #32);
  if k > 0 then
  begin
    for i := 1 to k + 1 do
    begin
      AMot := Trim(WordByPos(i, #32, Words));
      if AMot <> '' then
        AjouterCondition(AMot);
    end;
  end
  else
  begin
    if Trim(Words) <> '' then
      AjouterCondition(Words);
  end;
end;

function GetDelimitedString(StartStr, EndStr, Source : string) : string;
var
  i,j,k : Integer;
begin
  Result := '';
  i := Pos(StartStr, Source);
  if i > 0 then
  begin
    j := i + Length(StartStr);
    k := Pos(EndStr, Copy(Source, j, Length(Source) - j + 1));
    if k > 0 then
      Result := Copy(Source, j, k - 1);
  end;
end;

function GetDelimitedInteger(StartStr, EndStr, Source : string) : Integer;
var
  e,i : Integer;
begin
  Val(Trim(GetDelimitedString(StartStr, EndStr, Source)), i, e);
  if e = 0 then
    Result := i
  else
    Result := 0;
end;

function EstimateEndDate(Progress, Total : Extended; BeginningDate : TDateTime) : TDateTime;
begin
  Result := (Total - Progress) * (Now - BeginningDate) / Progress + BeginningDate;
end;

procedure ParseCSVClipboard(const AString : string; const AFieldSeparator : string; const AWithHeaders : Boolean; ParseEvent : TCSVParsingEvent);
var
  AStr : TStringList;
  AFields : TStringList;
  AValues : TStringList;
  AValue : string;
  i, j, Debut, Correction : Integer;
const
  msgFieldSeparatorManquant = 'Séparateur de champ non-renseigné, importation impossible';
  msgFieldSeparatorAuMilieu = 'Erreur dans le fichier (présence du séparateur de champs en milieu d''enregistrement)';
  msgLineSeparatorAuMilieu = 'Erreur dans le fichier (présence d''un saut de ligne en milieu d''enregistrement)';
begin
  {Le texte doit contenir les noms de colonnes dans la première ligne}
  {Si un nom de colonne n'est pas trouvé dans le Dataset, la colonne est ignorée}

  {Le séparateur de champ doit au moins avoir un caractère}
  if Length(AFieldSeparator) = 0 then
    raise Exception.Create(msgFieldSeparatorManquant);

  AStr := TStringList.Create;
  AFields := TStringList.Create;
  AValues := TStringList.Create;
  try
    AStr.Text := AString;

    AFields.Text := StringReplace(AStr.Strings[0], AFieldSeparator, #13#10, [rfReplaceAll]);
    {Les guillemets sont ignorées et supprimées}
    AFields.Text := StringReplace(AFields.Text, '"', '', [rfReplaceAll]);
    if AWithHeaders then
    begin
      Debut := 1;
      Correction := -1;
    end
    else
    begin
      Debut := 0;
      Correction := 0;
      for j := 0 to AFields.Count - 1 do
      begin
        AFields[j] := IntToStr(j);
      end;
    end;

    for i := Debut to AStr.Count - 1 do
    begin
      AValues.Text := StringReplace(AStr.Strings[i], AFieldSeparator, #13#10, [rfReplaceAll]);

      if AWithHeaders then
      begin
        {Il ne faut pas de saut de ligne en milieu d'enregistrement}
        if AValues.Count < AFields.Count then
          raise Exception.Create(msgLineSeparatorAuMilieu);

        {Il ne faut pas de FieldSeparator sur-numéraires}
        if AValues.Count > AFields.Count then
          raise Exception.Create(msgFieldSeparatorAuMilieu);
      end;

      for j := 0 to AFields.Count - 1 do
      begin
        AValue := StringReplace(AValues[j], '"', '', [rfReplaceAll]);
        ParseEvent(j, Trim(AFields[j]), i + Correction, AValue)
      end;
    end;
  finally
    AValues.Free;
    AFields.Free;
    AStr.Free;
  end;
end;

function FormatFileSize(ASize : Int64) : string;
var
  ASizeD : Double;
begin
  ASizeD := ASize / 1024;
  if ASizeD > 1024 then
  begin
    ASizeD := ASizeD / 1024;
    if ASizeD > 1024 then
    begin
      ASizeD := ASizeD / 1024;
      if ASizeD > 1024 then
      begin
        ASizeD := ASizeD / 1024;
        Result := FormatFloat(',0.# To', ASizeD);
      end
      else
        Result := FormatFloat(',0.# Go', ASizeD);
    end
    else
      Result := FormatFloat(',0.# Mo', ASizeD);
  end
  else
    Result := FormatFloat(',0.# Ko', ASizeD);
end;

function ParseWords(ATxt : string; ALst : TStrings) : Integer;
var
  d,k: Integer;
  Guillemets: Boolean;
  AValeur: string;
begin
  Result:= 0;
  k:= 1;
  while (k < Length(ATxt)) and (ATxt[k] = #32) do
    inc(k);
  d:= k;
  Guillemets:= False;
  while k <= Length(ATxt) do
  begin
    if not Guillemets then
    begin
      if (ATxt[k] = #32) or (k = Length(ATxt)) then {fin de paramètre}
      begin
        Inc(Result);
        ALst.Add(trim(Copy(ATxt, d, k-d+1)));
        {Déplacement jusqu'au prochain caractère non-espace}
        inc(k);
        while (k < Length(ATxt)) and (ATxt[k] = #32) do
          inc(k);
        d:= k;
      end
      else
      begin
        if ATxt[k] = #34 then {Début de guillemets}
          Guillemets:= True;
        inc(k);
      end;
    end
    else
    begin
      if (ATxt[k] = #34) or (k = Length(ATxt)) then {fin de paramètre}
      begin
        {Traitement des doubles "}
        if (k<Length(ATxt)) and (ATxt[k+1] = #34) then
          inc(k,2)
        else
        begin
          Guillemets:= False;
          Inc(Result);
          AValeur := Trim(Copy(ATxt, d, k-d+1));
          {Suppression guillemet gauche}
          if (AValeur <> '') and (AValeur[1] = #34) then
            AValeur := Copy(AValeur, 2, Length(AValeur)-1);
          {Suppression guillemet droite}
          if (AValeur <> '') and (AValeur[Length(AValeur)] = #34) then
            AValeur := Copy(AValeur, 1, Length(AValeur)-1);
          AValeur := ReplaceText('""', '"', AValeur);
          ALst.Add(AValeur);
          {Déplacement jusqu'au prochain caractère non-espace}
          inc(k);
          while (k < Length(ATxt)) and (ATxt[k] = #32) do
            inc(k);
          d:= k;
        end;
      end
      else
        inc(k);
    end;
  end;
end;

end.

