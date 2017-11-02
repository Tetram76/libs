{***************************************************************
 *
 * Unit Name: AFiles
 * Purpose  : Various functions to simplify and enhance file manipulation
 * Author   : Alexandre GUILLIEN
 * History  : Much
 *
 ****************************************************************}

{ Classes :

  TFileBufferStream : enhanced TFileStream. It uses a buffer to speed-up reading.
    It uses the TBufferStream abstract class to handle the buffer.

  TUsefulStream : this class is used as a TStream enhanced functions library.
    The class will never be instanciated by itself but any TStream can be
    transtyped to a TUsefulStream to benefit from its new functions.
  Example :
    You have a TFileStream, you want to use ReadString and WriteString from
    TUsefulStream. Just do :
      TUsefulStream(MyStream).ReadString(...);
      TUsefulStream(MyStream).WriteString(...);

  FAST stream search functions :
        // TypeSearch : 1 CaseLess, 2 CaseSensitive. -1 or -2 : search reversed string
  function FindStrInStream(Stream: TStream; Str: string; TypeSearch: ShortInt): Boolean;
        // TypeSearch : 1 = BYTE, 2 = WORD, 3 = DWORD
  function FindIntegerInStream(Stream: TStream; Int: LongInt; TypeSearch: ShortInt): Boolean;
        // TypeSearch : 1 = Single, 2 = Double, 3 = Extended
  function FindFloatInStream(Stream: TStream; Float: Extended; TypeSearch: ShortInt): Boolean;
  function FindArrayInStream(Stream: TStream; MainArray: Pointer; Length: Integer): Boolean;

}

unit AFiles;

{$I GrizzlyDefine.INC}

interface

uses {$IFNDEF LINUX}Windows, {$ENDIF}SysUtils, Classes, AUtils;

type
  EFileError = Exception;
  
  TBufferStream = class(TStream)
  private
    { Buffer }
    FCurrentSize: LongInt;
    FBufferSize: LongInt;
    FBuffer: PCharArray;
		FBufferPos: LongInt;
    FReadSize: LongInt;
    FVirtualPos: LongInt;
    FHasWritten: Boolean;
    FFilled: Boolean;
    FHasReadSize: Boolean;
    procedure CheckSize;
    function GetCurrentPos: LongInt;
    function GetBOF: Boolean;
    function GetEOF: Boolean;
  protected
  	constructor Create(BufferSize: Integer);
    function DirectRead(var Buffer; Count: LongInt): LongInt; virtual; abstract;
    procedure DirectWrite(const Buffer; Count: LongInt); virtual; abstract;
  {$IFNDEF GZ_D10}
    function GetSize: LongInt; reintroduce; virtual; abstract;
  {$ENDIF}
    procedure WriteBuffer;
    procedure FillBuffer;
    procedure CheckBuffer(Length: LongInt);
    procedure CheckEnd(Length: LongInt);
    {}
    procedure SetBufferSize(NewSize: Integer);
    procedure SetSize(NewSize: LongInt); override;
    {}
    property BufferPos: LongInt read FBufferPos;
    property VirtualPos: LongInt read FVirtualPos;
  public
		destructor Destroy; override;
		function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    {}
    procedure FlushBuffer;
    procedure InvalidateBuffer;
    {}
    function ReadChar: Char;
    function ReadLn: string;
    procedure WriteChar(Char: Char);
    procedure WriteLn(const Str: string);
    {}
    property BOF: Boolean read GetBOF;
    property EOF: Boolean read GetEOF;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  end;

  TFileBufferStream = class(TBufferStream)
  private
    FHandle: Integer;
  protected
    function DirectRead(var Buffer; Count: LongInt): LongInt; override;
    procedure DirectWrite(const Buffer; Count: LongInt); override;
  {$IFDEF GZ_D10}
    function GetSize: Int64; override;
  {$ELSE}
    function GetSize: LongInt; override;
  {$ENDIF}
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;

  TUsefulStream = class(TStream)
  public
     { Read functions }
     function ReadLn: string;
     function ReadChar: Char;
     function ReadString: string;
     function ReadLongInt: LongInt;
     function ReadLargeInt: Int64;
     function ReadSingle: Single;
     function ReadDouble: Double;
     function ReadExtended: Extended;
     function ReadPointer: Pointer;
     function ReadDateTime: TDateTime;
     { Write functions }
     procedure WriteLn(const Str: string);
     procedure WriteChar(Char: Char);
     procedure WriteString(const Str: string);
     procedure WriteStr(const Str: string);
     procedure WriteLongInt(Int: LongInt);
     procedure WriteLargeInt(LargeInt: Int64);
     procedure WriteSingle(Value: Single);
     procedure WriteDouble(Value: Double);
     procedure WriteExtended(Value: Extended);
     procedure WritePointer(Value: Pointer);
     procedure WriteDateTime(Value: TDateTime);
  end;

  { Generic File Procs }

     { Various checking }
  function IsOpen(FileName: string): Boolean;
  function DirExists(Dir: string): Boolean;
     { File Copy }
  procedure CopyFile(NFSource, NFDest: string);
  procedure CopyFiles(SourceFiles: TStringList; DestPath: string);
     { File/Dir Search/Browse }
  procedure GetDirFiles(Directory, SearchPattern: string; Reccurse: Boolean; List: TStrings);
     { Mass destruction }
  procedure DeleteFiles(Files: TStringList);
     { Miscellaneous }
  {$IFDEF LINUX}
  {$ELSE}
  function GetFileTime(FileName: string): TDateTime;
  procedure SetFileTime(FileName: string; Date: TDateTime);
  function ShortPathName(Path: string): string;
  function FullPathName(Path: string): string;
  {$ENDIF}
  function ExtractFileShortName(FileName: string): string;

     { Special Stream Search functions }
  function FindInStream(Stream: TStream; P: Pointer; L: Integer; Proc: TArrayCompareProc): Boolean;
  function FindStrInStream(Stream: TStream; Str: string; TypeSearch: ShortInt): Boolean;
        { TypeSearch : 1 = BYTE, 2 = WORD, 3 = DWORD }
  function FindIntegerInStream(Stream: TStream; Int: LongInt; TypeSearch: ShortInt): Boolean;
        { TypeSearch : 1 = Single, 2 = Double, 3 = Extended }
  function FindFloatInStream(Stream: TStream; Float: Extended; TypeSearch: ShortInt): Boolean;
  function FindArrayInStream(Stream: TStream; MainArray: Pointer; Length: Integer): Boolean;

const faFile = faReadOnly + faHidden + faSysFile + faArchive;

var DefaultBufferSize: Integer = 32768;

implementation

uses FGUtils, FGFiles;

{ ***************************************************************************** }
{ ******************************* TBufferStream ******************************* }
{ ***************************************************************************** }

{ ******************** Création, ouverture, fermeture ********************** }
constructor TBufferStream.Create(BufferSize: Integer);
begin
	inherited Create;
  FBufferSize:= BufferSize;
  GetMem(FBuffer, FBufferSize);
  FCurrentSize:= GetSize;
end;

destructor TBufferStream.Destroy;
begin
  if FHasWritten then WriteBuffer;
	FreeMem(FBuffer, FBufferSize);
  inherited Destroy;
end;

{ ************************* Contrôle position ************************** }

procedure TBufferStream.SetSize(NewSize: LongInt);
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  FCurrentSize:= NewSize;
end;

function TBufferStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  if (Offset = 0) and (Origin = 2) then
  begin
    if FHasWritten then
      WriteBuffer;
    Result:= FCurrentSize;
    FHasReadSize:= True;
    FBufferPos:= FCurrentSize;
    FVirtualPos:= 0;
    Exit;
  end;
  FHasReadSize:= False;
  { Absolute Offset calculation }
  case Origin of
     0: begin end; { No Change : Offset:= Offset because already absolute }
     1: Offset:= GetCurrentPos + Offset;
     2: Offset:= FCurrentSize - Offset;
     else raise EStreamError.Create('L''origine de déplacement ' +
        IntToStr(Origin) + ' est indéterminée');
  end;
  if Offset < 0 then Offset:= 0;
  { Do Seek }
  if FFilled then
  begin
     if (Offset >= FBufferPos) and (Offset <= (FBufferPos + FBufferSize)) then
     begin
        FVirtualPos:= Offset - FBufferPos;
        if FVirtualPos > FReadSize then
          FVirtualPos:= FReadSize;
     end else
     begin
	      if FHasWritten then
          WriteBuffer;
        FBufferPos:= Offset;
        FFilled:= False;
     end
  end else FBufferPos:= Offset;
  Result:= GetCurrentPos;
end;

function TBufferStream.GetBOF: Boolean;
begin
  Result:= GetCurrentPos = 0;
end;

function TBufferStream.GetEOF: Boolean;
begin
  Result:= GetCurrentPos = FCurrentSize;
end;

{ ********************************** Buffer ********************************** }

procedure TBufferStream.FlushBuffer;
begin
  WriteBuffer;
end;

procedure TBufferStream.InvalidateBuffer;
begin
  FFilled:= False;
  FCurrentSize:= GetSize;
end;

procedure TBufferStream.WriteBuffer;
var OldPos: Integer;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  if FFilled then
  begin
     OldPos:= FVirtualPos;
     FVirtualPos:= 0;
     DirectWrite(FBuffer^, FReadSize);
     FVirtualPos:= OldPos;
     FCurrentSize:= GetSize; { Mise à jour de la taille si l'écriture a changé qqchose }
  end;
  FHasWritten:= False;
end;

procedure TBufferStream.FillBuffer;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  if FHasWritten then WriteBuffer; { Flush }
  { Pas besoin de remplir si c'est déjà fait }
  if FFilled and (FVirtualPos = 0) then Exit;
  FReadSize:= DirectRead(FBuffer^, FBufferSize);
  FBufferPos:= GetCurrentPos;
  FVirtualPos:= 0;
  FFilled:= True;
end;

procedure TBufferStream.CheckSize;
begin
  { Si la VirtualPos est supérieure à la ReadSize, on met à jour }
	if (FVirtualPos > FReadSize) then FReadSize:= FVirtualPos;
end;

procedure TBufferStream.CheckEnd(Length: LongInt);
begin
  { Si on dépasse le buffer ou que celui-ci est vide ..., on le rerempli }
	if (((FVirtualPos + Length) >= FReadSize) and (FReadSize = FBufferSize)) or (not FFilled) then
		FillBuffer;
end;

procedure TBufferStream.CheckBuffer(Length: LongInt);
begin
  { Si on dépasse le buffer, alors on Reset }
	if ((FVirtualPos + Length) >= FBufferSize) or (not FFilled) then FillBuffer;
end;

function TBufferStream.GetCurrentPos: LongInt;
begin
  if FFilled then
     Result:= FBufferPos + FVirtualPos
  else Result:= FBufferPos;
end;

procedure TBufferStream.SetBufferSize(NewSize: Integer);
var OldPos: Integer;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  if NewSize <= 0 then Exit;
  {$IFDEF VER80}
  ReallocMem(FBuffer, FBufferSize, NewSize);
  {$ELSE}
  ReallocMem(FBuffer, NewSize);
  {$ENDIF}
  { Si on est déjà rempli à mort, alors on complète ... sinon rien à faire }
  if FFilled and (NewSize > FBufferSize) and (FReadSize = FBufferSize) then
  begin
     FFilled:= False;
     OldPos:= FBufferPos;
     FBufferPos:= FBufferPos + FReadSize;
     Inc(FReadSize, DirectRead(Pointer(LongInt(FBuffer) + FBufferSize)^, NewSize - FBufferSize));
     FFilled:= True;
     FBufferPos:= OldPos;
  end;
  FBufferSize:= NewSize;
end;

{ ***************************** Lecture ****************************** }

function TBufferStream.Read(var Buffer; Count: LongInt): LongInt;
var
  L, NewPos: Integer;
  PBuff: Pointer;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  Count:= IMin(FCurrentSize - GetCurrentPos, Count);{ Calcul du transfert réel }
  NewPos:= GetCurrentPos + Count;           { Nouvelle position }
  if Count >= FBufferSize then
  begin
    if FHasWritten then WriteBuffer;        { Et ce afin de ne pas oublier d'éventuelles
                                              modifications présentes dans le buffer }
    Result:= DirectRead(Buffer, Count);     { On lit directement }
  end else
  begin
    if not FFilled then FillBuffer;
    { Flush du buffer }
    PBuff:= Pointer(LongInt(FBuffer) + FVirtualPos);
    L:= IMin(FReadSize - FVirtualPos, Count); { RS - VP = Taille restante du buffer }
    Move(PBuff^, Buffer, L);               { Transfert des données }
    Inc(FVirtualPos, L);                   { On place VP là où on est }
    Result:= L;                            { Result = Count octets transférés }
    Dec(Count, L);                         { Il y a moins de truc à récupérer }
    if Count > 0 then
    begin
      FillBuffer;                            { Remplissage du buffer }
      PBuff:= Pointer(LongInt(@Buffer) + L); { Et on écrira là où il faut (+ loin) }
      Move(FBuffer^, PBuff^, Count);         { Transfert de ce qui reste }
      Inc(Result, Count);
    end;
  end;
  Seek(NewPos, 0);                          { Mise à jour de la position }
end;

{ ********************************* Écriture ********************************* }

function TBufferStream.Write(const Buffer; Count: LongInt): LongInt;
var
  L, NewPos: Integer;
  PBuff: Pointer;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  NewPos:= Position + Count;                  
  FCurrentSize:= IMax(Position + Count, GetSize);        { Mise à jour initiale }
  Result:= Count;
  if Count >= FBufferSize then
  begin
    if FHasWritten then WriteBuffer;
    DirectWrite(Buffer, Count);      { Note : le Seek final réglera la Pos }
    FCurrentSize:= GetSize; { Si la taille du truc a changée, alors, on la met à jour }
  end else
  begin
    if not FFilled then FillBuffer;
    { Là, on transferre tout ce qui peut/doit l'être }
    PBuff:= Pointer(LongInt(FBuffer) + FVirtualPos);
    L:= IMin(FBufferSize - FVirtualPos, Count);  { BS - VP = Taille restante du buffer }
    Move(Buffer, PBuff^, L);                  { Transfert }
    Inc(FVirtualPos, L);                      { On place VP là où on est }
    Dec(Count, L);                            { Il y a moins de truc à écrire }
    CheckSize;                                { On met à jour RS si nécessaire }
    if Count > 0 then
    begin
      FillBuffer;
      PBuff:= Pointer(Integer(@Buffer) + L);    { Ce qui reste à écrire }
      Move(PBuff^, FBuffer^, Count);            { Transfert du reste }
      Inc(FVirtualPos, Count);                  { On place VP là où on est }
      CheckSize;
    end;
    FHasWritten:= True;                    { De toute façon, on a écrit ! }
  end;
  Seek(NewPos, 0);                          { Mise à jour de la position }
end;

{ **************************** Fonctions I/O natives *************************** }

function TBufferStream.ReadChar: Char;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  if not EOF then
  begin
	   CheckEnd(1);                           { Reremplie le Buffer si nécessaire }
     Result:= FBuffer^[FVirtualPos];
     Inc(FVirtualPos);
  end else
    Result:= Chr(0);
end;

function TBufferStream.ReadLn: string;
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  if not FFilled then FillBuffer;
	Result:= '';
  while not (FBuffer^[FVirtualPos] in [Chr(0), Chr(13)]) do
  begin
     CheckEnd(1);
     if EOF then Exit;
     Result:= Result + FBuffer^[FVirtualPos];
     Inc(FVirtualPos);
  end;
  ReadChar;
  ReadChar;
end;

procedure TBufferStream.WriteChar(Char: Char);
begin
  if FHasReadSize then Seek(FCurrentSize, 0);
  CheckBuffer(1);
  FBuffer^[FVirtualPos]:= Char;
  Inc(FVirtualPos);
  FHasWritten:= True;
  if EOF then
    Inc(FCurrentSize);
  CheckSize;
end;

procedure TBufferStream.WriteLn(const Str: string);
begin
  Write(PChar(Str + #13#10)^, Length(Str) + 2);
end;

{ **************************************************************************** }
{ **************************** TFileBufferStream ***************************** }
{ **************************************************************************** }

constructor TFileBufferStream.Create(const FileName: string; Mode: Word);
begin
  if Mode = fmCreate then
    FHandle:= FileCreate(FileName)
  else
    FHandle:= FileOpen(FileName, Mode);
  if FHandle < 0 then
    raise EFileError.Create('Impossible d''ouvrir le fichier ' + FileName);
  inherited Create(DefaultBufferSize);
end;

destructor TFileBufferStream.Destroy;
begin
  if FHasWritten then WriteBuffer;
  if FCurrentSize < GetSize then
  begin
    FileSeek(FHandle, FCurrentSize, 0);
    {$IFDEF VER130}
    Win32Check(SetEndOfFile(FHandle));
    {$ELSE}
      {$IFDEF LINUX}
      raise Exception.Create('Fonction non implémentée sous LINUX');
      {$ELSE}
      SetEndOfFile(FHandle);
      {$ENDIF}
    {$ENDIF}
  end;
  FileClose(FHandle);
  inherited;
end;

function TFileBufferStream.DirectRead(var Buffer; Count: LongInt): LongInt;
begin
  FileSeek(FHandle, Position, 0);
  Result:= FileRead(FHandle, Buffer, Count);
end;

procedure TFileBufferStream.DirectWrite(const Buffer; Count: LongInt);
begin
  FileSeek(FHandle, Position, 0);
  FileWrite(FHandle, Buffer, Count);
end;

{$IFDEF GZ_D10}
function TFileBufferStream.GetSize: Int64;
{$ELSE}
function TFileBufferStream.GetSize: LongInt;
{$ENDIF}
begin
  Result:= FileSeek(FHandle, 0, 2);
end;

{ ************************ Generic File Procs ************************ }

procedure CopyFile(NFSource, NFDest: string);
var
  Tampon: Pointer;
  Handle1, Handle2, TRead: Integer;
begin
  if NFSource = NFDest then
     raise EInOutError.Create('Les fichiers source et destination sont les mêmes !!!');
  GetMem(Tampon, 16384);
  Handle1:= FileOpen(NFSource, fmOpenRead or fmShareDenyNone);
  Handle2:= FileCreate(NFDest);
  try
     if Handle1 <= 0 then
        raise EInOutError.Create('Le fichier source n''existe pas !!!');
     if Handle2 <= 0 then
        raise EInOutError.Create('Erreur ' + IntToStr(Handle2) + ' lors de la création du fichier destination !!!');
     repeat
        TRead:= FileRead(Handle1, Tampon^, 16384);
        FileWrite(Handle2, Tampon^, TRead);
     until TRead <> 16384;
  finally
     if Handle1 > 0 then FileClose(Handle1);
     if Handle2 > 0 then FileClose(Handle2);
     FreeMem(Tampon, 16384);
  end;
end;

procedure CopyFiles(SourceFiles: TStringList; DestPath: string);
var
  i: Integer;
begin
  DestPath:= AddSlash(DestPath);
  for i:= 0 to SourceFiles.Count - 1 do
     CopyFile(SourceFiles[i], DestPath + ExtractFileName(SourceFiles[i]));
end;

{$IFDEF LINUX}
procedure GetDirFiles(Directory, SearchPattern: string; Reccurse: Boolean; List: TStrings);
var S: TSearchRec;
  procedure AddFile(const FileName: string);
  begin
    List.Add(Directory + FileName);
  end;
  procedure ProcessDir(const SearchRec: TSearchRec);
  begin
//    if ((SearchRec.Attr and faAnyFile) <> 0) and (SearchRec.Name[1] <> '.') then
//      GetDirFiles(Directory + SearchRec.Name, SearchPattern, Reccurse, List);
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      GetDirFiles(Directory + SearchRec.Name, SearchPattern, Reccurse, List);
  end;
begin
  Directory:= AddSlash(Directory);
  if FindFirst(Directory + SearchPattern, $FF, S) = 0 then
  begin
    AddFile(S.Name);
    while FindNext(S) = 0 do
      AddFile(S.Name);
  end;
  if Reccurse then
  begin
    if FindFirst(Directory + '*', $FF, S) = 0 then
    begin
      ProcessDir(S);
      while FindNext(S) = 0 do
        ProcessDir(S);
    end;
  end;
  FindClose(S);
end;
{$ELSE}
procedure GetDirFiles(Directory, SearchPattern: string; Reccurse: Boolean; List: TStrings);
var S: TSearchRec;
  procedure AddFile(const FileName: string);
  begin
    List.Add(Directory + FileName);
  end;
  procedure ProcessDir(const SearchRec: TSearchRec);
  begin
    if (SearchRec.Attr = faDirectory) and (SearchRec.Name[1] <> '.') then
      GetDirFiles(Directory + SearchRec.Name, SearchPattern, Reccurse, List);
  end;
begin
  Directory:= AddSlash(Directory);
  if FindFirst(Directory + SearchPattern, faFile, S) = 0 then
  begin
    AddFile(S.Name);
    while FindNext(S) = 0 do
      AddFile(S.Name);
  end;
  if Reccurse then
  begin
    if FindFirst(Directory + '*.*', faDirectory, S) = 0 then
    begin
      ProcessDir(S);
      while FindNext(S) = 0 do
        ProcessDir(S);
    end;
  end;
  FindClose(S);
end;
{$ENDIF}

procedure DeleteFiles(Files: TStringList);
var
  i: Integer;
  Dirs: TStringList;
begin
  Dirs:=TStringList.Create;
  try
    Dirs.Sorted:= True;
    for i:= 0 to Files.Count - 1 do
    begin
      if FileExists(Files[i]) then
        DeleteFile(Files[i]);
      if DirectoryExists(Files[i]) then
        Dirs.Add(Files[i]);
    end;
    for i:= Dirs.Count - 1 downto 0 do
    begin
      if DirectoryExists(Dirs[i]) then
        RemoveDir(Dirs[i]);
    end;
  finally
    Dirs.Free;
  end;
end;

function IsOpen(FileName: string): Boolean;
var
  H: Integer;
begin
  H:= FileOpen(FileName, fmShareExclusive);
  Result:= H < 0;
  FileClose(H);
end;

{$IFDEF LINUX}
{$ELSE}
function GetFileTime(FileName: string): TDateTime;
{$IFNDEF GZ_D10}
var I{,Line}: Integer;
    {W1, W2: Word;
    FileTime: TFileTime;}
{$ENDIF}
begin

{$IFDEF GZ_D10}
  if not FileAge(FileName,Result) then
    Result := 0;
{$ELSE}
  I:=FileAge(FileName);
  If I < 0 then
    Result:= 0
  else
    Result:= FileDateToDateTime(I);
{$ENDIF}

  {H:= FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if H < 0 then
    Result:= 0
  else
  begin
    Windows.GetFileTime(H, nil, nil, @FileTime);
    FileClose(H);
    FileTimeToDosDateTime(FileTime, W1, W2);
    H:= W1;
    H:= H shl 16;
    PWord(@H)^:= W2;
    Result:= FileDateToDateTime(H);
  end;}
end;

procedure SetFileTime(FileName: string; Date: TDateTime);
var H{,Line}: Integer;
    {W1, W2: Word;}
    //FileTime: TFileTime;
    FileTime: Integer;
begin
  FileTime:= DateTimeToFileDate(Date);
  H:= FileOpen(FileName, fmOpenReadWrite or fmShareDenyNone);
  if H >= 0  then
    FileSetDate(H,FileTime);
  FileClose(H);
  {H:= DateTimeToFileDate(Date);
  W1:= H shr 16;
  W2:= PWord(@H)^;
  DosDateTimeToFileTime(W1, W2, FileTime);
  H:= FileOpen(FileName, fmOpenReadWrite or fmShareDenyNone);
  Windows.SetFileTime(H, nil, nil, @FileTime);
  FileClose(H);}
end;

function ShortPathName(Path: string): string;
var L: Integer;
begin
  SetLength(Result, MAX_PATH);
  L:= GetShortPathName(PChar(Path), PChar(Result), MAX_PATH);
  SetLength(Result, L);
end;

function FullPathName(Path: string): string;
var L: Integer;
    P: PChar;
begin
  SetLength(Result, MAX_PATH);
  L:= GetFullPathName(PChar(Path), MAX_PATH, PChar(Result), P);
  SetLength(Result, L);
end;
{$ENDIF}

function ExtractFileShortName(FileName: string): string;
var L: Integer;
begin
  FileName:= ExtractFileName(FileName);
  L:= Length(ExtractFileExt(FileName));
  Result:= LeftStr(FileName, Length(FileName) - L);
end;

function DirExists(Dir: string): Boolean;
var S: TSearchRec;
begin
  Result:= False;
  if FindFirst(Dir, faDirectory, S) = 0 then
     Result:= (S.Attr and faDirectory) = faDirectory;
end;

  { Special Stream Search functions }

  {Et ça fait quoi ces choses ?
  {Ca change la prop Position au moins ? ou ça se contente de renvoyer T ou F ???}
  {Et ici, le 1 et le 2 ça veut dire quoi ?...}
  {C'est pas du tout documenté ce truc}
function FindStrInStream(Stream: TStream; Str: string; TypeSearch: ShortInt): Boolean;
var
	L: Integer;
begin
  if not (Abs(TypeSearch) in [1..2]) then raise Exception.Create('Le type de recherche ' + IntToStr(TypeSearch) + ' est invalide');
  Result:= False;
  L:= Length(Str);
  if TypeSearch < 0 then Str:= ReverseStr(Str);
  case Abs(TypeSearch) of
     1: begin
        Str:= UpperCase(Str);
        Result:= FindInStream(Stream,{$IFDEF WIN32} PChar(Str) {$ELSE} Pointer(LongInt(@Str) + 1){$ENDIF}, L, CompareUpCaseArrayWithoutCase);
     end;
     2: Result:= FindArrayInStream(Stream,{$IFDEF WIN32} PChar(Str) {$ELSE} Pointer(LongInt(@Str) + 1){$ENDIF}, L);
  end;
end;

  { TypeSearch : 1 = BYTE, 2 = WORD, 3 = DWORD }
function FindIntegerInStream(Stream: TStream; Int: LongInt; TypeSearch: ShortInt): Boolean;
const LI: array [1..3] of Integer = (SizeOf(Byte), SizeOf(Word), SizeOf(LongInt));
begin
	{ de toute façon, la valeur est stockée en Low .. High }
  if not (Abs(TypeSearch) in [1..3]) then raise Exception.Create('Le type de recherche ' + IntToStr(TypeSearch) + ' est invalide');
  if TypeSearch < 0 then ReverseArray(@Int, LI[Abs(TypeSearch)]);
  Result:= FindArrayInStream(Stream, @Int, LI[Abs(TypeSearch)]);
end;
  { TypeSearch : 1 = Single, 2 = Double, 3 = Extended }
function FindFloatInStream(Stream: TStream; Float: Extended; TypeSearch: ShortInt): Boolean;
const LF: array [1..3] of Integer = (SizeOf(Single), SizeOf(Double), SizeOf(Extended));
var
  V: array [1..SizeOf(Extended)] of Char;
begin
  if not (Abs(TypeSearch) in [1..3]) then raise Exception.Create('Le type de recherche ' + IntToStr(TypeSearch) + ' est invalide');
  case Abs(TypeSearch) of
     1: PSingle(@V)^:= Float;
     2: PDouble(@V)^:= Float;
     3: PExtended(@V)^:= Float;
  end;
  if TypeSearch < 0 then ReverseArray(@V, LF[Abs(TypeSearch)]);
  Result:= FindArrayInStream(Stream, @V, LF[Abs(TypeSearch)]);
end;

function FindArrayInStream(Stream: TStream; MainArray: Pointer; Length: Integer): Boolean;
begin
  Result:= FindInStream(Stream, MainArray, Length, CompareArrays);
end;

  { Fonctions de recherche }
function FindInStream(Stream: TStream; P: Pointer; L: Integer; Proc: TArrayCompareProc): Boolean;
const BaseBufferSize = 32768;
var
  Buffer: Pointer;
  CurPos, OldPos, BufferSize, ReadSize, FoundPos: Integer;
begin
  Result:= False;
  with Stream do
  begin
     FoundPos:= Position; OldPos:= Position;
     if L > (Size - Position) then Exit; { Si il n'y a rien à trouver, fin }
     { On crée le Buffer si nécessaire }
     if Stream is TCustomMemoryStream then
     begin
        CurPos:= 0;
        Buffer:= IncPointer(TCustomMemoryStream(Stream).Memory, Position);
        FoundPos:= PosArray(P, Buffer, L, Size - Position, Proc);
        Result:= FoundPos <> -1;
     end else begin
        BufferSize:= IMax(BaseBufferSize, L);
        GetMem(Buffer, BufferSize);
        CurPos:= Position;
        ReadSize:= Read(Buffer^, BufferSize);
        { Tant que la longueur de l'objectif est inf à ce qui reste }
        while (L <= (Size - CurPos)) and (not Result) do
        begin
           FoundPos:= PosArray(P, Buffer, L, ReadSize, Proc);
           Result:= FoundPos <> -1;
           if not Result then
           begin
              CurPos:= Position;
              ReadSize:= Read(Buffer^, BufferSize);
           end;
	      end;
        FreeMem(Buffer, BufferSize);
     end;
	   if Result then Seek(CurPos + FoundPos, 0)
     else Seek(OldPos, 0);
  end;
end;

{ ******************************* TUsefulStream ******************************* }

  { Read functions }
  
function TUsefulStream.ReadLn: string;
var C, T: Char;
begin
  Result:= '';
  C:= ReadChar;
  while (not (C in [#13, #10])) and (Position <> Size) do
  begin
     Result:= Result + C;
     C:= ReadChar;
  end;
  if C = #13 then
  begin
     T:= ReadChar;
     if T <> #10 then
       Seek(-1, soFromCurrent);
  end else if C <> #10 then
     Result:= Result + C;
end;

function TUsefulStream.ReadChar: Char;
begin
  Read(Result, SizeOf(Char));
end;

function TUsefulStream.ReadString: string;
begin
  SetLength(Result, ReadLongInt);
  Read(Result[1], Length(Result));
end;

function TUsefulStream.ReadLongInt: LongInt;
begin
  Read(Result, SizeOf(LongInt));
end;

function TUsefulStream.ReadLargeInt: Int64;
begin
  Read(Result, SizeOf(Int64));
end;

function TUsefulStream.ReadSingle: Single;
begin
  Read(Result, SizeOf(Single));
end;

function TUsefulStream.ReadDouble: Double;
begin
  Read(Result, SizeOf(Double));
end;

function TUsefulStream.ReadDateTime: TDateTime;
begin
  Read(Result, SizeOf(TDateTime));
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
  Write(Str[1], Length(Str));
end;

procedure TUsefulStream.WriteStr(const Str: string);
begin
  Write(Str[1], Length(Str));
end;

procedure TUsefulStream.WriteLongInt(Int: LongInt);
begin
  Write(Int, SizeOf(LongInt));
end;

procedure TUsefulStream.WriteLargeInt(LargeInt: Int64);
begin
  Write(LargeInt, SizeOf(Int64));
end;

procedure TUsefulStream.WriteSingle(Value: Single);
begin
  Write(Value, SizeOf(Single));
end;

procedure TUsefulStream.WriteDouble(Value: Double);
begin
  Write(Value, SizeOf(Double));
end;

procedure TUsefulStream.WriteDateTime(Value: TDateTime);
begin
  Write(Value, SizeOf(TDateTime));
end;

procedure TUsefulStream.WriteExtended(Value: Extended);
begin
  Write(Value, SizeOf(Extended));
end;

procedure TUsefulStream.WritePointer(Value: Pointer);
begin
  Write(Value, SizeOf(Pointer));
end;

end.
