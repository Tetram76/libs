unit Divers;

{ .$D- }
interface

uses
  Winapi.Windows, System.SysUtils, VCL.Forms, System.Classes, VCL.Dialogs, VCL.Controls, VCL.StdCtrls;

type
  IMesurePerf = interface
    ['{46EF7EF7-D12F-4AF0-9A29-321ABCA2A6B7}']
  end;

  TMesurePerf = class(TInterfacedObject, IMesurePerf)
  private
    FStart: TDateTime;
    FLabel: string;
  public
    constructor Create(const Texte: string = '');
    destructor Destroy; override;
  end;

  RCommandLine = record
  private
    value: string;
  public
    procedure Add(Param: string);
    class operator Implicit(value: string): RCommandLine;
    class operator Implicit(value: RCommandLine): string;
  end;

function IsAdmin: Boolean;
function CanUseTaskDialog: Boolean;
procedure RemplaceChaine(var Chaine: string; Quoi, parQuoi: string);
procedure Split(var Chaine: string; const Sep: string);
procedure Collapse(var Chaine: string; const Sep: string = '');
procedure ShowFormOnTaskBar(Handle: Integer);
procedure HideFormOnTaskBar(Handle: Integer);
{$IFDEF WIN32}
procedure Swap(a, b: Variant);
{$ENDIF}
function PosMulti(const Substr: array of string; const S: string): Integer;
function PosInTextEx(const Debut: Integer; const Texte: string; const AChercher: array of string): Integer;
function IIf(Test: Boolean; Retour_Vrai, Retour_Faux: Variant): Variant;
function Choose(Val: Integer; const Retour: array of Variant): Variant;
function IsPrevInstance: HWND;
function BaseSoundex(in_str: string; const language: string): string;
function Soundex(const in_str, language: string): string;
function NumericSoundex(in_str: string; const language: string): SmallInt;
function ExtendedSoundex(in_str: string): string;
procedure ReplaceString(var str: string; const fr_str, to_str: string);
function MakeInitiales(str: string): string;
function SansAccents(str: string): string; overload;
function OnlyAlphaNum(const str: string; NoDblSpace: Boolean = True): string;
function ListOfResName(Module, TypeRes: PChar; var ListRes: TStringList): Boolean;
function MessageGetLastError: string; overload;
function MessageGetLastError(ErrorCode: Integer): string; overload;
function GetPosteName: string;
function SetPosteName(const value: string): Boolean;
procedure DoInvisible(Form: TForm);
procedure Visible(Form: TForm);
function ExtractLongPathName(const FileName: string): string;
function ValidFileName(const FileName: string): string;
function FormatFileSize(ASize: Integer): string;
function FormalizeNom(const Nom: string): string;

type
  TSysInfoRec = record
    VolumeName, VolumeSerial, FileSystemName: string;
    MaxComponentLength: Cardinal;
  end;

function GetVolumeInfo(Drive: Char; var SysInfoRec: TSysInfoRec): Boolean;
procedure ChangeCurseur(Index: TCursor; Nom, Rubrique: PChar);
function Base64(valueSTR: string): string;
function MessageDlgEx(const Caption, Msg: string; AType: TMsgDlgType; const AButtons: array of string): Word;

type
  TNotifyProc = procedure(Sender: TObject);

  TNotifyList = class(TObject)
  private
    FCode: TList;
    FData: TList;
  protected
    function GetIsProc(Index: Integer): Boolean;
    function GetMethods(Index: Integer): TNotifyEvent;
    function GetProcs(Index: Integer): TNotifyProc;
    procedure SetMethods(Index: Integer; const value: TNotifyEvent);
    procedure SetProcs(Index: Integer; const value: TNotifyProc);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const NotifyProc: TNotifyEvent); overload;
    procedure Add(const NotifyProc: TNotifyProc); overload;
    procedure Clear;
    function Count: Integer;
    procedure Delete(Index: Integer);
    function IndexOf(const NotifyProc: TNotifyEvent): Integer; overload;
    function IndexOf(const NotifyProc: TNotifyProc): Integer; overload;
    procedure Notify(Sender: TObject);
    procedure Remove(const NotifyProc: TNotifyEvent); overload;
    procedure Remove(const NotifyProc: TNotifyProc); overload;
    property IsProc[index: Integer]: Boolean read GetIsProc;
    property Methods[index: Integer]: TNotifyEvent read GetMethods write SetMethods;
    property Procs[index: Integer]: TNotifyProc read GetProcs write SetProcs;
  end;

function IsToogleKey(nVirtKey: Integer): Boolean;
function IsDownKey(nVirtKey: Integer): Boolean;

type
  TVersionNumber = record
  private type
    TArrayOfInteger = array of Integer;

  class procedure DecodeVer(const Ver: string; var AVer: TArrayOfInteger; Sep: Char); static;
  function GetIndexVersion(const Index: Integer): Integer;
  public
    value: string;

    property MajorVersion: Integer index 0 read GetIndexVersion;
    property MinorVersion: Integer index 1 read GetIndexVersion;
    property Release: Integer index 2 read GetIndexVersion;
    property Build: Integer index 3 read GetIndexVersion;

    class operator Subtract(a, b: TVersionNumber): Integer;
    class operator Equal(a, b: TVersionNumber): Boolean;
    class operator NotEqual(a, b: TVersionNumber): Boolean;
    class operator GreaterThan(a, b: TVersionNumber): Boolean;
    class operator GreaterThanOrEqual(a, b: TVersionNumber): Boolean;
    class operator LessThan(a, b: TVersionNumber): Boolean;
    class operator LessThanOrEqual(a, b: TVersionNumber): Boolean;
    class operator Implicit(a: string): TVersionNumber;
    class operator Implicit(a: TVersionNumber): string;

    class function CompareVersionNum(Ver1, Ver2: String; Sep: Char = '.'): Integer; static;
  end;

function GetFichierVersion(const Fichier: string): TVersionNumber;
function MulDiv(Number, Numerator, Denominator: Integer; Default: Real = 0): Real;
procedure ClearList(List: TList);
function IsRemoteSession: Boolean;

function MAKELANGID(p, S: Word): Word;
function MAKELCID(lgid, srtid: Word): dword;

implementation

uses
  System.AnsiStrings, System.Math, VCL.Themes;

const
  VALIDCHARPOSTE: TSysCharSet = ['a' .. 'z', 'A' .. 'Z', '1' .. '0', '!', '@', '#', '$', '%', '^', '&', '''', ')', '(', '.', '-', '_', '{', '}', '~', '.'];

constructor TMesurePerf.Create(const Texte: string);
begin
  inherited Create;
  FStart := Now;
  FLabel := Texte;
end;

destructor TMesurePerf.Destroy;
begin
  ShowMessage('Durée ' + FLabel + ': ' + FormatDateTime('hh:mm:ss:zzz', Now - FStart));
  inherited;
end;

{ TNotifyList }

constructor TNotifyList.Create;
begin
  inherited;
  FCode := TList.Create;
  FData := TList.Create;
end;

destructor TNotifyList.Destroy;
begin
  FData.Free;
  FCode.Free;
  inherited;
end;

procedure TNotifyList.Add(const NotifyProc: TNotifyEvent);
var
  m: TMethod;
begin
  if Assigned(NotifyProc) and (IndexOf(NotifyProc) < 0) then
  begin
    m := TMethod(NotifyProc);
    FCode.Add(m.Code);
    FData.Add(m.Data);
  end;
end;

procedure TNotifyList.Add(const NotifyProc: TNotifyProc);
begin
  if Assigned(NotifyProc) and (IndexOf(NotifyProc) < 0) then
  begin
    FCode.Add(@NotifyProc);
    FData.Add(nil);
  end;
end;

procedure TNotifyList.Clear;
begin
  FCode.Clear;
  FData.Clear;
end;

function TNotifyList.Count: Integer;
begin
  Result := FCode.Count;
end;

procedure TNotifyList.Delete(Index: Integer);
begin
  FCode.Delete(index);
  FData.Delete(index);
end;

function TNotifyList.GetIsProc(Index: Integer): Boolean;
begin
  Result := (not Assigned(FData[index]));
end;

function TNotifyList.GetMethods(Index: Integer): TNotifyEvent;
begin
  TMethod(Result).Code := FCode[index];
  TMethod(Result).Data := FData[index];
end;

function TNotifyList.GetProcs(Index: Integer): TNotifyProc;
begin
  Result := FCode[index];
end;

function TNotifyList.IndexOf(const NotifyProc: TNotifyEvent): Integer;
var
  m: TMethod;
begin
  if Assigned(NotifyProc) and (FCode.Count > 0) then
  begin
    m := TMethod(NotifyProc);
    Result := 0;
    while (Result < FCode.Count) and ((FCode[Result] <> m.Code) or (FData[Result] <> m.Data)) do
      Inc(Result);
    if Result >= FCode.Count then
      Result := -1;
  end
  else
    Result := -1;
end;

function TNotifyList.IndexOf(const NotifyProc: TNotifyProc): Integer;
var
  prt: ^TNotifyProc;
begin
  prt := @NotifyProc;
  if Assigned(NotifyProc) and (FCode.Count > 0) then
  begin
    Result := 0;
    while (Result < FCode.Count) and ((FCode[Result] <> prt) or (FData[Result] <> nil)) do
      Inc(Result);
    if Result >= FCode.Count then
      Result := -1;
  end
  else
    Result := -1;
end;

procedure TNotifyList.Notify(Sender: TObject);
var
  i: Integer;
  evnt: TNotifyEvent;
  proc: TNotifyProc;
begin
  for i := 0 to FCode.Count - 1 do
    if (FData[i] = nil) then
    begin
      proc := FCode[i];
      if (Assigned(proc)) then
        proc(Sender);
    end
    else
    begin
      TMethod(evnt).Code := FCode[i];
      TMethod(evnt).Data := FData[i];
      if (Assigned(evnt)) then
        evnt(Sender);
    end;
end;

procedure TNotifyList.Remove(const NotifyProc: TNotifyProc);
var
  idx: Integer;
begin
  idx := IndexOf(NotifyProc);
  if (idx >= 0) then
  begin
    FCode.Delete(idx);
    FData.Delete(idx);
  end;
end;

procedure TNotifyList.Remove(const NotifyProc: TNotifyEvent);
var
  idx: Integer;
begin
  idx := IndexOf(NotifyProc);
  if (idx >= 0) then
  begin
    FCode.Delete(idx);
    FData.Delete(idx);
  end;
end;

procedure TNotifyList.SetMethods(Index: Integer; const value: TNotifyEvent);
begin
  FCode[index] := TMethod(value).Code;
  FData[index] := TMethod(value).Data;
end;

procedure TNotifyList.SetProcs(Index: Integer; const value: TNotifyProc);
begin
  FCode[index] := @value;
  FData[index] := nil;
end;

procedure RemplaceChaine(var Chaine: string; Quoi, parQuoi: string);
var
  i: Integer;
begin
  i := Pos(Quoi, Chaine);
  while i > 0 do
  begin
    Delete(Chaine, i, Length(Quoi));
    Insert(parQuoi, Chaine, i);
    i := Pos(Quoi, Chaine);
  end;
end;

procedure Split(var Chaine: string; const Sep: string);
begin
  RemplaceChaine(Chaine, Sep, #13#10);
end;

procedure Collapse(var Chaine: string; const Sep: string = '');
begin
  RemplaceChaine(Chaine, #13#10, Sep);
end;

// PROCEDURE SHOWFORMONTASKBAR
//
// Description: Shows a form on the TaskBar
// Input   : Handle: Integer - Handle of form
// Units   : Windows

procedure ShowFormOnTaskBar(Handle: Integer);
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_APPWINDOW and not WS_EX_TOOLWINDOW);
end;

// PROCEDURE HIDEFORMONTASKBAR
//
// Description: Hides a form from the TaskBar
// Input   : Handle: Integer - Handle of form
// Notes   : Handle = Application.Handle, No application on the TaskBar.
// Units   : Windows

procedure HideFormOnTaskBar(Handle: Integer);
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
end;

function PosMulti(const Substr: array of string; const S: string): Integer;
var
  i, p: Integer;
  trouve: Boolean;
begin
  Result := 0;
  trouve := False;
  for i := low(Substr) to High(Substr) do
  begin
    p := Pos(Substr[i], S);
    if p > 0 then
    begin
      if (p < Result) or not trouve then
        Result := p;
      trouve := True;
    end;
  end;
end;

function PosInTextEx(const Debut: Integer; const Texte: string; const AChercher: array of string): Integer;
var
  Temp, p: PChar;
  i: Integer;
  trouve: Boolean;
begin
  Result := 0;
  if not Debut in [1 .. Length(Texte)] then
    Exit;
  trouve := False;
  Temp := @Texte[Debut];
  for i := low(AChercher) to High(AChercher) do
  begin
    p := StrPos(Temp, PChar(AChercher[i]));
    if p <> nil then
    begin
      if (Debut + p - Temp < Result) or not trouve then
        Result := Debut + p - Temp;
      trouve := True;
    end;
  end;
end;

{$IFDEF WIN32}

procedure Swap;
asm
  MOV EAX, &a;
  MOV ECX, &b;
  MOV &a, ECX;
  MOV &b, EAX;
end;
{$ENDIF}

function IIf(Test: Boolean; Retour_Vrai, Retour_Faux: Variant): Variant;
begin
  if Test then
    Result := Retour_Vrai
  else
    Result := Retour_Faux;
end;

function Choose(Val: Integer; const Retour: array of Variant): Variant;
begin
  Result := varNull;
  if Val in [0 .. High(Retour)] then
    Result := Retour[Val];
end;

{$HINTS OFF}

function IsPrevInstance: HWND;
// Retourne 0 si aucune instance n'est trouvée
// sinon, retourne le handle de l'application trouvée.
var
  ClassName: array [0 .. 255] of Char;
  TitreApplication: string;
begin
  Result := 0;
  TitreApplication := Application.Title;
  Application.Title := '';
  try
    GetClassName(Application.Handle, ClassName, 254);
    Result := FindWindow(ClassName, PChar(TitreApplication));
  finally
    Application.Title := TitreApplication;
  end;
end;

{$HINTS ON}

type
  Langue = string

    [26];

const
  Francais: Langue = '01230970072455012683090808';
  Anglais: Langue = '01230120022455012623010202';

procedure PrepareSoundex(var in_str: string; const language: string);
begin
  in_str := Trim(AnsiUpperCase(in_str));
  ReplaceString(in_str, ' ', '');
  ReplaceString(in_str, '''', '');
  if language = 'FR' then
  begin
    ReplaceString(in_str, 'CHR', 'CR');
    ReplaceString(in_str, 'PH', 'F');
    // ReplaceString(in_str, 'Z', 'S');
  end
  else if language = 'EN' then
  begin
    ReplaceString(in_str, 'TH', 'Z');
  end;
end;

function BaseSoundex(in_str: string; const language: string): string;
var
  lg: Langue;
  no_vowels, coded, out_str: string;
  ch: Char;
  i: Integer;
begin
  if UpperCase(language) = 'FR' then
    lg := Francais
  else
    lg := Anglais;
  PrepareSoundex(in_str, language);
  for i := 1 to Length(in_str) do
  begin
    ch := in_str[i];
    case ch of
      'A', 'E', 'I', 'O', 'U', 'H', 'W', 'Y':
        ;
    else
      if CharInSet(ch, ['A' .. 'Z']) then
        no_vowels := no_vowels + ch;
    end;
  end;
  for i := 1 to Length(no_vowels) do
    coded := coded + string(lg[Ord(no_vowels[i]) - Ord('A') + 1]);
  // out_str := coded[1];
  for i := 1 to Length(no_vowels) do
    if (coded[i] <> '0') and (coded[i] <> coded[i - 1]) then
      out_str := out_str + coded[i];
  Result := out_str;
end;

function Soundex(const in_str, language: string): string;
var
  out_str: string;
begin
  out_str := BaseSoundex(in_str, language);
  Result := AnsiUpperCase(in_str[1]) + Copy(out_str, 2, Length(out_str) - 1);
end;

function NumericSoundex(in_str: string; const language: string): SmallInt;
var
  value: Integer;
begin
  in_str := Soundex(in_str, language);
  value := (Ord(in_str[1]) - Ord('A')) * 1000;
  if (Length(in_str) > 1) then
    value := value + StrToInt(Copy(in_str, 2, Length(in_str) - 1));
  Result := value;
end;

procedure ReplaceString(var str: string; const fr_str, to_str: string);
var
  fr_len, i: Integer;
begin
  fr_len := Length(fr_str);
  i := Pos(fr_str, str);
  while (i > 0) do
  begin
    str := Copy(str, 1, i - 1) + to_str + Copy(str, i + fr_len, Length(str) - i - fr_len + 1);
    i := Pos(fr_str, str);
  end;
end;

function ExtendedSoundex(in_str: string): string;
var
  no_vowels: string;
  ch, last_ch: Char;
  i: Integer;
begin
  in_str := Trim(AnsiUpperCase(in_str));
  ReplaceString(in_str, ' ', '');
  ReplaceString(in_str, 'CHR', 'CR');
  ReplaceString(in_str, 'PH', 'F');
  ReplaceString(in_str, 'Z', 'S');
  last_ch := in_str[1];
  Result := last_ch;
  for i := 2 to Length(in_str) do
  begin
    ch := in_str[i];
    case ch of
      'A', 'E', 'I', 'O', 'U':
        ;
    else
      if (ch <> last_ch) then
      begin
        if CharInSet(ch, ['A' .. 'Z']) then
          no_vowels := no_vowels + ch;
        last_ch := ch;
      end;
    end;
  end;
  Result := Result + no_vowels;
end;

function MakeInitiales(str: string): string;
var
  i: Integer;
  ch: string;
begin
  if Length(str) = 0 then
    Exit;
  str := Trim(str);
  Result := '';
  i := 0;
  repeat
    ch := AnsiUpperCase(str[i + 1]);
    if CharInSet(ch[1], ['A' .. 'Z', '0' .. '9']) then
      Result := Result + str[i + 1];
    i := PosInTextEx(i + 1, str, [' ', '''']);
  until (i = 0) or (i >= Length(str));
end;

function SansAccents(str: string): string;
const
  Tab1: string = 'àáâãäåèéêëìíîïòóôõöùúûüýÿçñ';
  Tab2: string = 'aaaaaaeeeeiiiiooooouuuuyycn';
var
  i, p: Integer;
  Dummy: string;
begin
  Dummy := AnsiLowerCase(str);
  for i := 1 to Length(Tab1) do
  begin
    p := Pos(Tab1[i], Dummy);
    while p > 0 do
    begin
      str[p] := Chr(Ord(Tab2[i]) - (Ord(Dummy[p]) - Ord(str[p])));
      Dummy[p] := Tab2[i];
      p := Pos(Tab1[i], str);
    end;
  end;
  Result := str;
end;

function OnlyAlphaNum(const str: string; NoDblSpace: Boolean = True): string;
var
  i: Integer;
  c, cPrec: Char;
begin
  Result := '';
  cPrec := #0;
  for i := 1 to Length(str) do
  begin
    c := str[i];
    if not IsCharAlphaNumeric(c) then
      c := ' ';
    if not NoDblSpace or (c <> ' ') or not CharInSet(cPrec, [#0, ' ']) then
    begin
      cPrec := c;
      Result := Result + c;
    end;
  end;
end;

var
  ListOfRes: TStringList;

function EnumResNameProc(hModule: Integer; lpszType, lpszName: PChar; lParam: Integer): Boolean;
begin
  ListOfRes.Add(StrPas(lpszName));
  Result := True;
end;

function ListOfResName(Module, TypeRes: PChar; var ListRes: TStringList): Boolean;
var
  h: Integer;
begin
  Result := True;
  ListOfRes := ListRes;
  try
    h := LoadLibrary(Module);
    if h > 32 then
    begin
      EnumResourceNames(h, TypeRes, @EnumResNameProc, 0);
      FreeLibrary(h);
    end;
  except
    Result := False;
  end;
end;

function MessageGetLastError: string;
begin
  Result := MessageGetLastError(GetLastError);
end;

function MessageGetLastError(ErrorCode: Integer): string;
var
  Buf: array [Byte] of Char;
begin
  Result := '';
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, LOCALE_USER_DEFAULT, Buf, SizeOf(Buf), nil) <> 0) then
    Result := StrPas(Buf);
end;

function GetPosteName: string;
// retourne le nom du poste de travail
var
  lpBuffer: array [0 .. MAX_COMPUTERNAME_LENGTH - 1] of Char;
  ASize: dword;
begin
  ASize := SizeOf(lpBuffer);
  GetComputerName(lpBuffer, ASize);
  Result := StrPas(lpBuffer);
end;

function SetPosteName(const value: string): Boolean;
// Change le nom du poste de travail
var
  i: Integer;
  Avalue: string;
begin
  Result := False;
  Avalue := '';
  for i := 1 to Length(value) do
    if CharInSet(value[i], VALIDCHARPOSTE) then
      Avalue := Avalue + value[i];
  if Avalue = '' then
    Exit;
  Avalue := Copy(Avalue, 1, MAX_COMPUTERNAME_LENGTH);
  Result := SetComputerName(PChar(Avalue));
end;

procedure DoInvisible(Form: TForm);
var
  FullRgn, ClientRgn, CtlRgn: HRGN;
  AControl: TControl;
  a, Margin, X, Y, CtlX, CtlY: Integer;
begin
  with Form do
  begin
    Margin := (Width - ClientWidth) div 2;
    FullRgn := CreateRectRgn(0, 0, Width, Height);
    X := Margin;
    Y := Height - ClientHeight - Margin;
    ClientRgn := CreateRectRgn(X, Y, X + ClientWidth, Y + ClientHeight);
    CombineRgn(FullRgn, FullRgn, ClientRgn, RGN_DIFF);

    for a := 0 to ControlCount - 1 do
    begin
      AControl := Controls[a];
      if (AControl is TWinControl) or (AControl is TGraphicControl) then
        with AControl do
        begin
          if Visible then
          begin
            CtlX := X + Left;
            CtlY := Y + Top;
            CtlRgn := CreateRectRgn(CtlX, CtlY, CtlX + Width, CtlY + Height);
            CombineRgn(FullRgn, FullRgn, CtlRgn, RGN_OR);
          end;
        end;
    end;
    SetWindowRgn(Handle, FullRgn, True);
  end;
end;

procedure Visible(Form: TForm);
var
  FullRgn: HRGN;
begin
  with Form do
  begin
    FullRgn := CreateRectRgn(0, 0, Width, Height);
    CombineRgn(FullRgn, FullRgn, FullRgn, RGN_COPY);
    SetWindowRgn(Handle, FullRgn, True);
  end;
end;

function GetVolumeInfo(Drive: Char; var SysInfoRec: TSysInfoRec): Boolean;
var
  lpRootPathName: PChar; // address of root directory of the file system
  lpVolumeNameBuffer: PChar; // address of name of the volume
  nVolumeNameSize: dword; // length of lpVolumeNameBuffer
  lpVolumeSerialNumber: dword; // address of volume serial number
  lpMaximumComponentLength: dword; // address of system's maximum filename length
  lpFileSystemFlags: dword; // address of file system flags
  lpFileSystemNameBuffer: PChar; // address of name of file system
  nFileSystemNameSize: dword; // length of lpFileSystemNameBuffer
begin
  GetMem(lpVolumeNameBuffer, MAX_PATH + 1);
  GetMem(lpFileSystemNameBuffer, MAX_PATH + 1);
  try

    nVolumeNameSize := MAX_PATH + 1;
    nFileSystemNameSize := MAX_PATH + 1;

    lpRootPathName := PChar(Drive + ':\');
    Result := Winapi.Windows.GetVolumeInformation(lpRootPathName, lpVolumeNameBuffer, nVolumeNameSize, @lpVolumeSerialNumber, lpMaximumComponentLength,
      lpFileSystemFlags, lpFileSystemNameBuffer, nFileSystemNameSize);
    if Result then
      with SysInfoRec do
      begin
        VolumeName := lpVolumeNameBuffer;
        VolumeSerial := IntToHex(HiWord(lpVolumeSerialNumber), 4) + '-' + IntToHex(LoWord(lpVolumeSerialNumber), 4);
        FileSystemName := lpFileSystemNameBuffer;
        MaxComponentLength := lpMaximumComponentLength;
      end;
  finally
    FreeMem(lpFileSystemNameBuffer);
    FreeMem(lpVolumeNameBuffer);
  end;
end;

type
  TUnite = (uOctets, uKo, uMo, uGo, uTo);

const
  sUnites: array [TUnite] of string = ('o', 'Ko', 'Mo', 'Go', 'To');

function FormatFileSize(ASize: Integer): string;
var
  Unite: TUnite;
  Taille: Real;
begin
  Unite := uOctets;
  Taille := ASize;
  while (ASize > 1024) and (Integer(Unite) < Integer(High(TUnite))) do
  begin
    Inc(Unite);
    Taille := Taille / 1024;
  end;
  Result := FormatFloat(',0 ' + sUnites[Unite], ASize);
end;

procedure ChangeCurseur(Index: TCursor; Nom, Rubrique: PChar);
var
  lpFileName: array [0 .. MAX_PATH] of Char;
  buffer: array [0 .. MAX_PATH] of Char;
  res: TResourceStream;
begin
  ZeroMemory(@buffer, Length(buffer) * SizeOf(Char));
  ZeroMemory(@lpFileName, Length(lpFileName) * SizeOf(Char));

  GetTempPath(Length(buffer), buffer);
  GetTempFileName(buffer, 'CSR', 0, lpFileName);
  try
    res := TResourceStream.Create(HInstance, Nom, Rubrique);
    try
      res.SaveToFile(lpFileName);
      DestroyCursor(Screen.Cursors[Index]);
      Screen.Cursors[Index] := LoadCursorFromFile(lpFileName);
      DeleteFile(lpFileName);
    finally
      res.Free;
    end;
  except
    // tant pis si on ne peut pas charger le nouveau curseur
  end;
end;

function Base64(valueSTR: string): string;
const
  v: string = 'e7dRaTuV5AGZ1YzKhl34L9ONc0DMPq6rbCi8UXpgs2oEwIvtfHBmjFSJkWxnQy';
var
  i, j: Integer;
begin
  Result := valueSTR;
  for i := 1 to Length(Result) do
  begin
    if Pos(Result[i], v) = 0 then
      Continue;
    j := Pos(Result[i], v) - 1;
    j := (j + 31) mod 62;
    Result[i] := v[j + 1];
  end;
end;

function MessageDlgEx(const Caption, Msg: string; AType: TMsgDlgType; const AButtons: array of string): Word;
var
  oForm: TForm;
  oLabel: TLabel;
  oButton: TButton;
  nButtonWidth: Integer;
  nAllButtonsWidth: Integer;
  nCtrlHeight: Integer;
  nMessageWidth: Integer;
  ii: Integer;
begin
  // Create the form.
  oForm := TForm.Create(Application);
  oForm.BorderStyle := bsDialog;
  oForm.BorderIcons := oForm.BorderIcons - [biSystemMenu];
  oForm.PopupMode := pmAuto;
  oForm.Height := 185;
  oForm.Width := 450;
  oForm.Position := poScreenCenter;
  oForm.Caption := Caption;
  // Loop through buttons to determine the longest caption.
  nButtonWidth := 0;
  for ii := 0 to High(AButtons) do
    nButtonWidth := Max(nButtonWidth, oForm.Canvas.TextWidth(AButtons[ii]));
  // Add padding for the button's caption.
  nButtonWidth := nButtonWidth + 10;
  // Determine space required for all buttons.
  nAllButtonsWidth := nButtonWidth * (High(AButtons) + 1);
  // Each button has padding on each side.
  nAllButtonsWidth := nAllButtonsWidth + (10 * (High(AButtons) + 2));
  // The form has to be at least as wide as the buttons.
  if nAllButtonsWidth > oForm.Width then
    oForm.Width := nAllButtonsWidth;
  // Determine if the message can fit in the form's width,
  // or if it must be word wrapped.
  nCtrlHeight := oForm.Canvas.TextHeight('A') * 3;
  nMessageWidth := oForm.Canvas.TextWidth(Msg);
  // If the message can fit with just the width of the
  // buttons, adjust the form width.
  if nMessageWidth < nAllButtonsWidth then
    oForm.Width := nAllButtonsWidth;
  if nMessageWidth > oForm.ClientWidth then
  begin
    // Determine how many lines are required.
    nCtrlHeight := Trunc(nMessageWidth / oForm.ClientWidth);
    // Add 3 more lines as padding.
    nCtrlHeight := nCtrlHeight + 3;
    // Convert to pixels.
    nCtrlHeight := nCtrlHeight * oForm.Canvas.TextHeight('A');
  end;
  // Adjust the form's height accomodating the message,
  // padding and the buttons.
  oForm.Height := nCtrlHeight + (oForm.Canvas.TextHeight('A') * 4) + 22;
  // Create the message control.
  oLabel := TLabel.Create(oForm);
  oLabel.AutoSize := False;
  oLabel.Left := 10;
  oLabel.Top := 10;
  oLabel.Height := nCtrlHeight;
  oLabel.Width := oForm.ClientWidth - 20;
  oLabel.WordWrap := True;
  oLabel.Caption := Msg;
  oLabel.Parent := oForm;
  // Create the pusbuttons.
  for ii := 0 to High(AButtons) do
  begin
    oButton := TButton.Create(oForm);
    oButton.Height := 25;
    oButton.Width := nButtonWidth;
    oButton.Left := oForm.Width - ((ii + 1) * (nButtonWidth + 10));
    oButton.Top := oForm.ClientHeight - 35;
    oButton.Caption := AButtons[ii];
    oButton.ModalResult := ii + 1;
    oButton.Parent := oForm;
  end;
  Result := oForm.ShowModal;
end;

type
  ELongFilename = Exception;

function ExtractLongPathName(const FileName: string): string;
var
  buffer: array [0 .. MAX_PATH - 1] of WideChar;
  PBuffer: PWideChar;
  DriveLen, Position: Integer;
  FindData: TWin32FindData;
  FindHandle: THandle;

  function FindLongname(Path: PChar): string;
  var
    FindData: TWin32FindData;
    FindHandle: THandle;
  begin
    FindHandle := FindFirstFile(Path, FindData);
    Result := FindData.cFileName;
    Winapi.Windows.FindClose(FindHandle);
  end;

begin
  Result := '';
  FindHandle := FindFirstFile(PChar(FileName), FindData);
  if FindHandle <> INVALID_HANDLE_VALUE then
    Winapi.Windows.FindClose(FindHandle)
  else
    raise ELongFilename.CreateFmt('Path or file not found (%s) !', [FileName]);

  DriveLen := Length(System.SysUtils.ExtractFileDrive(FileName));
  PBuffer := buffer;
  StrPCopy(PBuffer, FileName);
  Position := Length(PBuffer) - 1;
  while Position <> DriveLen do
  begin
    if Result <> '' then
      Result := PathDelim + Result;
    Result := FindLongname(PBuffer) + Result;
    while (PBuffer[Position] <> PathDelim) do
      Dec(Position);
    PBuffer[Position] := #0;
  end;
  Result := string(PBuffer) + PathDelim + Result;
end;

function IsToogleKey(nVirtKey: Integer): Boolean;
begin
  Result := Bool(Lo(GetAsyncKeyState(nVirtKey)));
end;

function IsDownKey(nVirtKey: Integer): Boolean;
begin
  Result := Bool(Hi(GetAsyncKeyState(nVirtKey)));
end;

function ValidFileName(const FileName: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(FileName) do
    if CharInSet(FileName[i], ['/', '\', ':', '*', '?', '"', '<', '>', '|']) then
      Result := Result + '_'
    else
      Result := Result + FileName[i];
end;

function GetFichierVersion(const Fichier: string): TVersionNumber;
var
  dump, size: Cardinal;
  buffer: array of Char;
  vallen, Translen: Cardinal;
  VersionPointer, TransBuffer: PByte;
  Temp: Integer;
  CalcLangCharSet, tmpResult: string;
begin
  tmpResult := '';
  try
    size := GetFileVersionInfoSize(PChar(Fichier), dump);
    SetLength(buffer, size);

    GetFileVersionInfo(PChar(Fichier), 0, size, buffer);
    VerQueryValue(buffer, '\VarFileInfo\Translation', Pointer(TransBuffer), Translen);
    if Translen >= 4 then
    begin
      System.AnsiStrings.StrLCopy(@Temp, PAnsiChar(TransBuffer), 2);
      CalcLangCharSet := IntToHex(Temp and $FFFF, 4);
      System.AnsiStrings.StrLCopy(@Temp, PAnsiChar(TransBuffer + 2), 2);
      CalcLangCharSet := CalcLangCharSet + IntToHex(Temp and $FFFF, 4);
    end
    else
      Exit;

    if VerQueryValue(buffer, PChar('\StringFileInfo\' + CalcLangCharSet + '\FileVersion'), Pointer(VersionPointer), vallen) then
    begin
      SetLength(tmpResult, vallen - 1);
      StrLCopy(PChar(tmpResult), PChar(VersionPointer), vallen - 1);
    end;
  finally
    Result := tmpResult;
  end;
end;

function MulDiv(Number, Numerator, Denominator: Integer; Default: Real = 0): Real;
begin
  if Denominator = 0 then
    Result := Default
  else
    Result := (Int64(Number) * Numerator) / Denominator;
end;

procedure ClearList(List: TList);
var
  i: Integer;
  p: Pointer;
begin
  try
    for i := 0 to Pred(List.Count) do
    begin
      p := List[i];
      Dispose(p);
    end;
  finally
    List.Clear;
  end;
end;

function IsRemoteSession: Boolean;
const
  SM_REMOTESESSION = $1000;
begin
  Result := LongBool(GetSystemMetrics(SM_REMOTESESSION));
end;

function FormalizeNom(const Nom: string): string;
var
  p: PChar;
begin
  Result := Nom;
  if Result = '' then
    Exit;
  Result[1] := UpCase(Result[1]);
  p := @Result[2];
  while (p^ <> #0) do
  begin
    if CharInSet((p - 1)^, ['-', ' ']) then
      p^ := UpCase(p^);
    Inc(p);
  end;
end;

{ TFileVersion }

class operator TVersionNumber.Subtract(a, b: TVersionNumber): Integer;
begin
  Result := CompareVersionNum(a.value, b.value);
end;

class procedure TVersionNumber.DecodeVer(const Ver: string; var AVer: TArrayOfInteger; Sep: Char);
var
  Index: Integer;
  S: string;
begin
  Index := 1;
  while (Index <= Length(Ver)) do
  begin
    S := '';
    while (Index <= Length(Ver)) and (Ver[Index] <> Sep) do
    begin
      if CharInSet(Ver[Index], ['0' .. '9']) then
        S := S + Ver[Index]
      else
        raise Exception.Create('"' + Ver + '" n''est pas un numéro de version valide');
      Inc(Index);
    end;
    SetLength(AVer, Length(AVer) + 1);
    AVer[Length(AVer) - 1] := StrToIntDef(S, 0);
    Inc(Index);
  end;
end;

class operator TVersionNumber.Equal(a, b: TVersionNumber): Boolean;
begin
  Result := CompareVersionNum(a.value, b.value) = 0;
end;

class operator TVersionNumber.NotEqual(a, b: TVersionNumber): Boolean;
begin
  Result := not(a = b);
end;

function TVersionNumber.GetIndexVersion(const Index: Integer): Integer;
var
  AVer: TArrayOfInteger;
begin
  DecodeVer(value, AVer, '.');
  if (Index >= 0) and (Index < Length(AVer)) then
    Result := AVer[Index]
  else
    Result := 0;
end;

class operator TVersionNumber.GreaterThan(a, b: TVersionNumber): Boolean;
begin
  Result := CompareVersionNum(a.value, b.value) > 0;
end;

class operator TVersionNumber.GreaterThanOrEqual(a, b: TVersionNumber): Boolean;
begin
  Result := CompareVersionNum(a.value, b.value) >= 0;
end;

class operator TVersionNumber.LessThan(a, b: TVersionNumber): Boolean;
begin
  Result := CompareVersionNum(a.value, b.value) < 0;
end;

class operator TVersionNumber.LessThanOrEqual(a, b: TVersionNumber): Boolean;
begin
  Result := CompareVersionNum(a.value, b.value) <= 0;
end;

class operator TVersionNumber.Implicit(a: TVersionNumber): string;
begin
  Result := a.value;
end;

class operator TVersionNumber.Implicit(a: string): TVersionNumber;
begin
  Result.value := a;
end;

class function TVersionNumber.CompareVersionNum(Ver1, Ver2: string; Sep: Char): Integer;

  procedure AjusteArray(var A1: TArrayOfInteger; const A2: TArrayOfInteger);
  begin
    while Length(A1) < Length(A2) do
    begin
      SetLength(A1, Length(A1) + 1);
      A1[Length(A1) - 1] := 0;
    end;
  end;

var
  AVer1, AVer2: TArrayOfInteger;
  Index: Integer;
begin
  Result := 0;
  DecodeVer(Ver1, AVer1, Sep);
  DecodeVer(Ver2, AVer2, Sep);
  AjusteArray(AVer1, AVer2);
  AjusteArray(AVer2, AVer1);
  Index := 0;
  while (Index < Length(AVer1)) and (Result = 0) do
  begin
    Result := AVer1[Index] - AVer2[Index];
    Inc(Index);
  end;
  if Result <> 0 then
    Result := Result div Abs(Result);
end;

function CanUseTaskDialog: Boolean;
begin
  Result := (Win32MajorVersion >= 6) and UseLatestCommonDialogs and StyleServices.Enabled;
end;

function MAKELANGID(p, S: Word): Word;
begin
  Result := (S shl 10) + (p);
end;

function MAKELCID(lgid, srtid: Word): dword;
begin
  Result := (srtid shl 16) + (lgid);
end;

const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_ENABLED = $00000004;

function IsAdmin: Boolean;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: dword;
  psidAdministrators: PSID;
  X: Integer;
  bSuccess: Bool;
begin
  // RunAsAdmin
  // if CheckWin32Version(6, 0) then ShellExecute(Handle, 'runas', PChar(Application.Exename), '/registerHandlers', nil, SW_SHOWNORMAL)

  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  if bSuccess then
  begin
    GetTokenInformation(hAccessToken, TokenGroups, nil, 0, dwInfoBufferSize);
    ptgGroups := GetMemory(dwInfoBufferSize);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, dwInfoBufferSize, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
{$R-}
      for X := 0 to ptgGroups.GroupCount - 1 do
      begin
        if (SE_GROUP_ENABLED = (ptgGroups.Groups[X].Attributes and SE_GROUP_ENABLED)) and EqualSid(psidAdministrators, ptgGroups.Groups[X].Sid) then
        begin
          Result := True;
          Break;
        end;
      end;
{$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

{ RCommandLine }

procedure RCommandLine.Add(Param: string);
begin
  if (Param.Contains(' ') or Param.Contains('"')) and not (Param.StartsWith('"') and Param.EndsWith('"')) then
    Param := '"' + Param.Replace('"', '""', [rfReplaceAll]) + '"';
  if value <> string.Empty then
    Param.Insert(0, ' ');
  value := value + Param;
end;

class operator RCommandLine.Implicit(value: RCommandLine): string;
begin
  Result := value.value;
end;

class operator RCommandLine.Implicit(value: string): RCommandLine;
begin
  Result.value := value;
end;

end.
