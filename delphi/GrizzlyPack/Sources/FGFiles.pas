unit FGFiles;

{***************************************************************
 *
 * Unit Name: FGFiles
 * Purpose  : Functions to manipulate files...
 * Author   : Frederic GUILLIEN
 * History  : ...
 *
 ****************************************************************}

{$I GrizzlyDefine.INC}

interface

uses {$IFDEF WIN32}Windows, {$IFDEF GZ_D6}DateUtils,{$ENDIF} {$ENDIF}SysUtils, Classes;


function DirIsEmpty(const Directory : string) : Boolean;

procedure DeleteFiles(Files: TStringList; DeleteDirs: Boolean);

function ReconcileDir(const AFile, OldBaseDir, NewBaseDir: string): string;

const
  faAnyFile = FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN
    or FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_DIRECTORY
    or FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_NORMAL
    or FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_COMPRESSED
    or FILE_ATTRIBUTE_OFFLINE;

const
  FileNameBadChars : string = '\/:*?"<>|';

function FindFiles(APath, AMask: string; Attr: Integer; Recurse: Boolean; ALst: TStrings): Boolean;

function LoadTextFile(AFileName : string) : string;

procedure EmptyDirectory(ADirectory : string);

function GetFileSize(AFile: string): Int64; 

{$IFDEF WIN32}
function FileTimeToDateTime(ADate : TFileTime): TDateTime;
function FileTimeToLocalDateTime(ADate : TFileTime): TDateTime;
function DateTimeToFileTime(ADate : TDateTime): TFileTime;
function LocalDateTimeToFileTime(ADate : TDateTime): TFileTime;

function GetFileCreationAge(AFileName : string) : Integer;
function GetFileModificationDate(AFileName : string) : TDateTime;

function CopyFile(Source, Dest: string; Exclusive: Boolean): Boolean;
function CopyFileSilent(Source, Dest: string; Exclusive: Boolean; var Error : string): Boolean;

function CheckFileNameCase(var AFileName : string) : Boolean;
function CanGetExclusiveAccess(AFile : string) : Boolean;
function CanGetWriteAccess(AFile : string) : Boolean;

function GetEllipsisPath(AHandle : HDC; ARect : TRect; APath : string) : string;

function GetModuleFileName : string;

{$ENDIF}

{Existent dans FileCtrl ou SysUtils (>D7)}
{$IFNDEF GZ_D6}function DirectoryExists(const Name: string): Boolean;{$ENDIF}
{procedure ForceDirectories(Dir: string);}

{Ajoute s'il n'y est pas le / final d'un chemin}
function AddSlash(const Ch: string): string;
function AddWebSlash(const Ch: string): string;

{Enlève le / final d'un chemin}
function RemoveSlash(const Ch: string): string;
function RemoveWebSlash(const Ch: string): string;
function ConcatUrls(const Urls : array of string) : string;

var
  {$IFDEF LINUX}
  DefaultTempDir: string = '/tmp';
  {$ELSE}
  DefaultTempDir: string = 'C:\';
  {$ENDIF}

function GetTempDir: string; overload
function GetTempDir(const ABaseDir, APrefixe: string): string; overload;

function GetTempFileName(const APrefixe: string): string; overload;
function GetTempFileName(const ABaseDir, APrefixe: string): string; overload;


{$IFDEF LINUX}
const OSSlash = '/';
{$ELSE}
const OSSlash = '\';
{$ENDIF}
const WebSlash = '/';

implementation

uses
  Dialogs,
  {$IFDEF LINUX}QConsts
  {$ELSE}Consts{$ENDIF}
  {$IFDEF GZ_D6}, SysConst{$ENDIF}
  {$IFNDEF GZ_D6}, FileCtrl{$ENDIF};


function AddSlash(const Ch: string): string;
begin
  if (Ch <> '') and (Ch[Length(Ch)] <> OSSlash) then
    Result:= Ch + OSSlash
  else
    Result:= Ch;
end;

function RemoveSlash(const Ch: string): string;
begin
  if (Ch <> '') and (Ch[Length(Ch)] = OSSlash) then
    Result:= Copy(Ch, 1, Length(Ch) - 1)
  else
    Result:= Ch;
end;

function AddWebSlash(const Ch: string): string;
begin
  if (Ch <> '') and (Ch[Length(Ch)] <> WebSlash) then
    Result:= Ch + WebSlash
  else
    Result:= Ch;
end;

function RemoveWebSlash(const Ch: string): string;
begin
  if (Ch <> '') and (Ch[Length(Ch)] = WebSlash) then
    Result:= Copy(Ch, 1, Length(Ch) - 1)
  else
    Result:= Ch;
end;

function ConcatUrls(const Urls : array of string) : string;
var
  i : Integer;
  AChaine : string;
begin
  Result := '';
  for i := 0 to High(Urls) do
  begin
    AChaine := Urls[i];
    if i + 1 <= High(Urls) then
    begin
      if Copy(Urls[i+1], 1, 1) = WebSlash then
        AChaine := RemoveWebSlash(AChaine)
      else
        AChaine := AddWebSlash(AChaine);
    end
    else
      AChaine := AddWebSlash(AChaine);
    Result := Result + AChaine;
  end;
end;

function GetTempDir: string;
var
  Buffer: array[0..1023] of Char;
begin
  {$IFDEF LINUX}
  Result:= '/tmp';
  {$ELSE}
  SetString(Result, Buffer, GetTempPath(SizeOf(Buffer), Buffer));
  {$ENDIF}
  Result:= Trim(AddSlash(Result));
  if not DirectoryExists(Result) then
    Result:= AddSlash(DefaultTempDir);
end;

function GetTempDir(const ABaseDir, APrefixe: string): string; overload;
var
  i: Integer;
  AChemin, AProp: string;
begin
  AChemin:= AddSlash(ABaseDir);
  Result:= '';
  for i:= 0 to 999 do
  begin
    AProp:= AChemin + APrefixe + FormatFloat('000', i);
    if not DirectoryExists(AProp) then
    begin
      Result:= AddSlash(AProp);
      Exit;
    end;
  end;
end;

function GetTempFileName(const APrefixe: string): string;
var
  i: Integer;
  AChemin, AProp: string;
begin
  AChemin:= AddSlash(GetTempDir);
  Result:= '';
  for i:= 0 to 999 do
  begin
    AProp:= AChemin + APrefixe + FormatFloat('000', i) + '.TMP';
    if not FileExists(AProp) then
    begin
      Result:= AProp;
      Exit;
    end;
  end;
end;

function GetTempFileName(const ABaseDir, APrefixe: string): string;
var
  i: Integer;
  AChemin, AProp: string;
begin
  AChemin:= AddSlash(ABaseDir);
  Result:= '';
  for i:= 0 to 999 do
  begin
    AProp:= AChemin + APrefixe + FormatFloat('000', i) + '.TMP';
    if not FileExists(AProp) then
    begin
      Result:= AProp;
      Exit;
    end;
  end;
end;

{$IFDEF WIN32}

function FileTimeToDateTime(ADate : TFileTime): TDateTime;
var
  ASysTime : TSystemTime;
begin
  FileTimeToSystemTime(ADate, ASysTime);
  Result := EncodeDate(ASysTime.wYear, ASysTime.wMonth, ASysTime.wDay)
              + EncodeTime(ASysTime.wHour, ASysTime.wMinute, ASysTime.wSecond, ASysTime.wMilliseconds);
end;

function FileTimeToLocalDateTime(ADate : TFileTime): TDateTime;
var
  ALocalTime : TFileTime;
  ASysTime : TSystemTime;
begin
  FileTimeToLocalFileTime(ADate, ALocalTime);
  FileTimeToSystemTime(ALocalTime, ASysTime);
  Result := EncodeDate(ASysTime.wYear, ASysTime.wMonth, ASysTime.wDay)
              + EncodeTime(ASysTime.wHour, ASysTime.wMinute, ASysTime.wSecond, ASysTime.wMilliseconds);
end;

function DateTimeToFileTime(ADate : TDateTime): TFileTime;
var
  ASysTime : TSystemTime;
  //yy, mm, dd, hh, nn, ss, ms : Word;
begin
  {DecodeDateTime(ADate, yy, mm, dd, hh, nn, ss, ms);
  ASysTime.wYear := yy;
  ASysTime.wMonth := mm;
  ASysTime.wDay := dd;
  ASysTime.wHour := hh;
  ASysTime.wMinute := nn;
  ASysTime.wSecond := ss;
  ASysTime.wMilliseconds := ms;}
  DateTimeToSystemTime(ADate, ASysTime);
  SystemTimeToFileTime(ASysTime, Result);
end;

function LocalDateTimeToFileTime(ADate : TDateTime): TFileTime;
var
  ALocalTime : TFileTime;
begin
  ALocalTime := DateTimeToFileTime(ADate);
  LocalFileTimeToFileTime(ALocalTime, Result);
end;

function CopyFile(Source, Dest: string; Exclusive: Boolean): Boolean;
var
  Handle1: Integer;
begin
  if not FileExists(Source) then
    raise EInOutError.Create('Le fichier source n''existe pas');
  if Exclusive then
  begin
    Handle1:= FileOpen(Source, fmOpenRead + fmShareExclusive);
    if Handle1 <= 0 then
      raise EInOutError.Create('Ouverture en exclusif impossible, fichier déjà utilisé')
    else
    begin
      FileClose(Handle1);
      Handle1:= FileOpen(Source, fmOpenRead + fmShareDenyWrite);
    end;
  end
  else
    Handle1:= FileOpen(Source, fmOpenRead + fmShareDenyWrite);
  try
    Result:= Windows.CopyFile(PChar(Source), PChar(Dest), False);
    if not Result then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
  finally
    if Handle1 > 0 then
      FileClose(Handle1);
  end;
end;

function CanGetExclusiveAccess(AFile : string) : Boolean;
var
  Handle1: Integer;
begin
  Handle1:= FileOpen(AFile, fmOpenRead + fmShareExclusive);
  Result := (Handle1 > 0);
  FileClose(Handle1);
end;

function CanGetWriteAccess(AFile : string) : Boolean;
var
  Handle1: Integer;
begin
  Handle1:= FileOpen(AFile, fmOpenWrite + fmShareDenyWrite);
  Result := (Handle1 > 0);
  FileClose(Handle1);
end;

procedure EmptyDirectory(ADirectory : string);
var
  ALst : TStringList;
begin
  ALst := TStringList.Create;
  try
    if FindFiles(ADirectory, '*.*', faAnyFile, True, ALst) then
      DeleteFiles(ALst, True);
  finally
    ALst.Free;
  end;
end;

function CheckFileNameCase(var AFileName : string) : Boolean;
var
  ASR : TSearchRec;
  ARes : Integer;
begin
  Result := False;
  ARes := SysUtils.FindFirst(AFileName, faAnyFile, ASR);
  try
    if ARes = 0 then
    begin
      Result := True;
      AFileName := AddSlash(ExtractFilePath(AFileName)) + ASR.Name;
    end;
  finally
    SysUtils.FindClose(ASR);
  end;
end;

function CopyFileSilent(Source, Dest: string; Exclusive: Boolean; var Error : string): Boolean;
begin
  try
    Result := FGFiles.CopyFile(Source, Dest, Exclusive);
  except
    on E : Exception do
    begin
      Result := False;
      Error := E.ClassName + ' : ' + E.Message;
    end;
  end;
end;

function GetEllipsisPath(AHandle : HDC; ARect : TRect; APath : string) : string;
var
  szPath : array[0..MAX_PATH] of Char;
begin
  FillChar(szPath, MAX_PATH, 0);
  StrPCopy(szPath, APath);
  DrawText(AHandle, szPath, -1, ARect, DT_CALCRECT or DT_PATH_ELLIPSIS	or DT_MODIFYSTRING);
  Result := StrPas(szPath);
end;

function GetModuleFileName : string;
var
  szPath : array[0..MAX_PATH] of Char;
begin
  FillChar(szPath, MAX_PATH, 0);
  Windows.GetModuleFileName(HInstance, szPath, MAX_PATH);
  Result := StrPas(szPath);
end;

function GetFileCreationAge(AFileName : string) : Integer;
var
  ASR : TSearchRec;
begin
  Result := SysUtils.FindFirst(AFileName, faAnyFile, ASR);
  try
    if Result = 0 then
      Result := DateTimeToFileDate(FileTimeToLocalDateTime(ASR.FindData.ftCreationTime))
    else
      Result := 0;
  finally
    SysUtils.FindClose(ASR);
  end;
end;

function GetFileModificationDate(AFileName : string) : TDateTime;
var
  ASR : TSearchRec;
begin
  Result := SysUtils.FindFirst(AFileName, faAnyFile, ASR);
  try
    if Result = 0 then
      Result := DateTimeToFileDate(FileTimeToLocalDateTime(ASR.FindData.ftLastWriteTime))
    else
      Result := 0;
  finally
    SysUtils.FindClose(ASR);
  end;
end;

{$ENDIF}

procedure DeleteFiles(Files: TStringList; DeleteDirs: Boolean);
var
  i: Integer;
  Dirs: TStringList;
begin
  Dirs:= TStringList.Create;
  try
    Dirs.Sorted:= True;
    for i:= 0 to Files.Count - 1 do
    begin
      if FileExists(Files[i]) then
        DeleteFile(Files[i]);
      if DeleteDirs and DirectoryExists(Files[i]) then
        Dirs.Add(Files[i]);
    end;
    if DeleteDirs then
      for i:= Dirs.Count - 1 downto 0 do
      begin
        if DirectoryExists(Dirs[i]) then
          RemoveDir(Dirs[i]);
      end;
  finally
    Dirs.Free;
  end;
end;

function ReconcileDir(const AFile, OldBaseDir, NewBaseDir: string): string;
var
  Old, New : string;
begin
  Result:= AFile;
  Old := AddSlash(OldBaseDir);
  if CompareText(Copy(AFile, 1, Length(Old)), Old) <> 0 then
    Exit;
  New := AddSlash(NewBaseDir);
  Result:= New + Copy(AFile, Length(Old) + 1, Length(AFile) - Length(Old));
end;

function LoadTextFile(AFileName : string) : string;
begin
  with TStringList.Create do
  begin
    LoadFromFile(AFileName);
    Result := Text;
    Free;
  end;
end;

function FindFiles(APath, AMask: string; Attr: Integer; Recurse: Boolean; ALst: TStrings): Boolean;
var
  ASR: TSearchRec;
  ARep: Integer;
  LstDirs: TStringList;
  i: Integer;
begin
  ALst.BeginUpdate;
  try
    Result:= False;
    ARep:= FindFirst(AddSlash(APath) + AMask, Attr, ASR);
    try
      while ARep = 0 do
      begin
        if not ((ASR.Name = '.') or (ASR.Name = '..')) then
        begin
          if (ASR.Attr and Attr) <> 0 then
          begin
            ALst.Add(AddSlash(APath) + ASR.Name);
            Result:= True;
          end;
        end;
        ARep:= FindNext(ASR);
      end;
    finally
      FindClose(ASR);
    end;
    if Recurse then
    begin
      LstDirs:= TStringList.Create;
      try
        if FindFiles(AddSlash(APath), '*.*', faDirectory or faHidden or faArchive or faReadOnly or faSysFile, False, LstDirs) then
        begin
          for i:= 0 to LstDirs.Count - 1 do
          begin
            if DirectoryExists(LstDirs[i]) then
              Result:= FindFiles(LstDirs[i], AMask, Attr, Recurse, ALst) or Result;
          end;
        end;
      finally
        LstDirs.Free;
      end;
    end;
  finally
    ALst.EndUpdate;
  end;
end;



{Existent dans FileCtrl ou SysUtils (>D7)}
{$IFNDEF GZ_D6}
function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code:= GetFileAttributes(PChar(Name));
  Result:= (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}
(*
procedure ForceDirectories(Dir: string);
begin
  if Length(Dir) = 0 then
    raise Exception.Create(SCannotCreateDir);
  if (AnsiLastChar(Dir) <> nil) and (AnsiLastChar(Dir)^ = '\') then
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;
*)

function GetFileSize(AFile: string): Int64;
var
  ASR : TSearchRec;
  ALI : LARGE_INTEGER;
begin
  try
    if FindFirst(AFile, faAnyFile, ASR) = 0 then
    begin
      ALI.LowPart := ASR.FindData.nFileSizeLow;
      ALI.HighPart := ASR.FindData.nFileSizeHigh;
      Result := ALI.QuadPart;
      FindClose(ASR);
    end
    else
      Result := -1;
  except
    Result := -1;
  end;
end;

function DirIsEmpty(const Directory : string) : Boolean;
var
  ASR : TSearchRec;
begin
  if not DirectoryExists(Directory) then
    Result := False
  else
  begin
    Result := (not (FindFirst(AddSlash(Directory) + '*.*', faAnyFile - faDirectory, ASR) = 0));
    FindClose(ASR);
  end;
end;

end.

