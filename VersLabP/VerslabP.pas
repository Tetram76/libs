{
Unit        : verslab.pas
Description : A TCustomLabel derivative that displays Win32 VersionInfo data
Version     : 1.10, 11 March 2000
Status      : Freeware.
Copyright   : ©1997-2000, First Internet Software House
Contact     : http://www.fishouse.com (email: support@fishouse.com)

History:
    v1.01   : fixed bug stopping LangCharSet from actually doing anything
              at all on a non-UK system.

    v1.02   : Fixed resource leak bug. Thanks to Peter Stromblad for finding it.
              1 July 1997

    v1.03   : Added display of flags, tidied up code. Made LangCharSet
              autodetect. Thanks to Fabrice Marguerie for the flags code.
              29 July 1997

    v1.04   : Added $H+ directive to turn on long strings, otherwise
              the cast to PChar fails.
              07 October 1997

    v1.05   : Amended Application.Exename to GetModuleFileName(). This allows
              TfshVersionLabel components on forms in DLLs to display the
              version information in that DLL, not the parent app. Added
              InfoString property for reading of version info. Couldn't just
              publish caption because that is writeable which breaks things.
              Also added filename property. '' = current application or DLL,
              any other Value is filename to get version info from.
              13 July 1998.

    v1.06, 1.07
            : Amended for compatibility with Delphi 4. Thanks to Richard
              Winston for a solution that remains compatible with D3!
              Added ShortFileVersion for standard 'v1.01' type display of major
              and minor version numbers. Useful for splash screens etc. Thanks
              to Peter Harris for the idea.
              24 November 1998

    v1.08   : Tidied up code for finding the language and character set. Found
              new side effect of GetModuleFilename introduced in 1.05 - if
              the app is compiled with packgages, the version info returned is
              that of the package the component resides in! Set Filename
              property to Application.Exename in main application to overcome
              this.

    v1.09   : Added FileDateTime and FileSize version displays plus some
              properties to control the output formatting.
              4 June 1999

    v1.10   : Rebranded for FISH
              11 March 2000

}

unit verslabp;

{$H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  pPChar = ^PChar;

  TVersionResources = (vrCompanyName, vrFileDescription, vrFileVersion,
    vrInternalName, vrLegalCopyright, vrOriginalFilename,
    vrProductName, vrProductVersion, vrComments, vrFlags,
    vrShortFileVersion, vrDateTime, vrFileSize);

  TfshVersionLabel = class(TLabel)
  private
    { Private declarations }
    FVersionResource: TVersionResources;
    FInfoPrefix: string;
    FShowInfoPrefix: Boolean;
    FVersionResourceKey: string;
    FLangCharset: string;
    FInfoString: string; // read only caption - current version info
    FFilename: string; // '' = current exe, DLL whatever.
    FDateTimeFormat: string;
    FFileSizeFormat: string;

    function CalcLangCharset(Buffer: pointer; Buflen: UINT): string;
    function Slice(var S: string; Delimiter: string): string;
    procedure SetupInfoPrefix;
    procedure SetupResourceKey;
    function GetStringFileInfo(Buffer: PChar; size: Integer): string;
    function GetFixedFileInfo(Buffer: PChar; size: Integer): string;
    function GetDateTimeInfo: string;
    function GetFileSizeInfo: string;
    function GetInfo: string;
    procedure SetupCaption;
  protected
    { Protected declarations }
    procedure SetFileSizeFormat(Value: string);
    procedure SetDateTimeFormat(Value: string);
    procedure SetFilename(Value: string);
    procedure SetInfoPrefix(Value: string);
    function GetInfoPrefix: string;
    procedure SetVersionResource(Value: TVersionResources);
    procedure SetShowInfoPrefix(Value: Boolean);
    procedure SetVersionResourceKey(Value: string);
    procedure SetLangCharset(Value: string);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property VersionResource: TVersionResources read FVersionResource write SetVersionResource;
    property VersionResourceKey: string read FVersionResourceKey write SetVersionResourceKey;
    property InfoPrefix: string read GetInfoPrefix write SetInfoPrefix;
    property ShowInfoPrefix: Boolean read FShowInfoPrefix write SetShowInfoPrefix;
    property LangCharset: string read FLangCharset write SetLangCharset;
    property WordWrap;
    property Align;
    property Color;
    property Font;
    property AutoSize;
    property Alignment;
    property ParentFont;
    property Visible;
    property Transparent;
    property InfoString: string read FInfoString; // Read only copy of captoion
    property Filename: string read FFilename write SetFilename;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property FileSizeFormat: string read FFileSizeFormat write SetFileSizeFormat;
  end;

const
    {The order of this array must be the same as the VersionResources
    enum type as that is used for the index lookup}
  VersionLookup: array[TVersionResources, 0..1] of string = (
    ('CompanyName', 'Organisation:'),
    ('FileDescription', 'Description:'),
    ('FileVersion', 'Version de fichier:'),
    ('InternalName', 'Nom interne:'),
    ('LegalCopyright', 'Copyright:'),
    ('OriginalFilename', 'Nom du fichier original:'),
    ('ProductName', 'Nom du produit:'),
    ('ProductVersion', 'Version du produit:'),
    ('Comments', 'Commentaires:'),
    ('Flags', 'Flags:'),
    ('FileVersion', 'v'),
    ('DateTime', 'File Date/Time:'),
    ('Filesize', 'File Size:'));

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TfshVersionLabel]);
end;

constructor TfshVersionLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInfoString := '';
  FFilename := '';
  FDateTimeFormat := ShortDateFormat;
  FFileSizeFormat := '#,#0" Bytes"';
  WordWrap := False;
  Autosize := True;
  ShowInfoPrefix := True;
  LangCharset := '-1'; {-1 = auto detect}
  VersionResource := vrFileVersion;
end;

destructor TfshVersionLabel.Destroy;
begin
  inherited Destroy;
end;

procedure TfshVersionLabel.SetVersionResource(Value: TVersionResources);
begin
  FVersionResource := Value;
  SetupResourceKey;
  SetupInfoPrefix;
end;

procedure TfshVersionLabel.SetFilename(Value: string);
begin
  FFilename := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetDateTimeFormat(Value: string);
begin
  if Value = '' then Exit;

  FDateTimeFormat := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetFileSizeFormat(Value: string);
begin
  if Value = '' then Exit;

  FFileSizeFormat := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetupInfoPrefix;
var
  s: string;
begin
  s := VersionLookup[FVersionResource, 1];
  InfoPrefix := s;
end;

procedure TfshVersionLabel.SetupResourceKey;
var
  s: string;
begin
  s := VersionLookup[FVersionResource, 0];
  VersionResourceKey := s;
end;

function TfshVersionLabel.GetFixedFileInfo(Buffer: PChar; size: Integer):
  string;
var
//  ValLen: Integer;
  ValLen: UINT;
  FixedFileInfo: PVSFixedFileInfo;
begin
  if VerQueryValue(buffer, '\', Pointer(FixedFileInfo), ValLen) then
  begin
    Result := '';
    if (ValLen > 1) then
    begin
      if FixedFileInfo.dwFileFlags and VS_FF_DEBUG <> 0 then
        Result := Result + ', Debug';
      if FixedFileInfo.dwFileFlags and VS_FF_PRERELEASE <> 0 then
        Result := Result + ', Pre-Release';
      if FixedFileInfo.dwFileFlags and VS_FF_PATCHED <> 0 then
        Result := Result + ', Patchée';
      if FixedFileInfo.dwFileFlags and VS_FF_PRIVATEBUILD <> 0 then
        Result := Result + ', Privée';
      if FixedFileInfo.dwFileFlags and VS_FF_INFOINFERRED <> 0 then
        Result := Result + ', InfoInferred';
      if FixedFileInfo.dwFileFlags and VS_FF_SPECIALBUILD <> 0 then
        Result := Result + ', Spéciale';
      if Result <> '' then
        if Result[1] = ',' then Delete(Result, 1, 2);
    end;
  end
  else
    Result := '< Impossible de récupérer les infos >';
end;

function TfshVersionLabel.GetStringFileInfo(Buffer: PChar; size: Integer):
  string;
//var vallen, Translen: Integer;
var
  vallen, Translen: UINT;
  VersionPointer: pointer;
  TransBuffer: pointer;
  Major, Minor: Integer;
begin
  if FLangCharSet = '-1' then
  begin
    VerQueryValue(buffer, '\VarFileInfo\Translation',
      TransBuffer, TransLen);
    if TransLen >= 4 then
    begin
      FLangCharSet := CalcLangCharSet(TransBuffer, TransLen);
    end else begin
      Result := '< Impossible de récupérer les infos de langue >';
      Exit;
    end;
  end;

  if VerQueryValue(buffer, PChar('\StringFileInfo\' + FLangCharSet + '\' +
    VersionResourceKey),
    VersionPointer, vallen) then
  begin
    if (Vallen > 1) then
    begin
      SetLength(Result, vallen);
      StrLCopy(PChar(Result), VersionPointer, vallen);
            // special case for 'short' file versions
      if FVersionResource = vrShortFileVersion then
      begin
        try
          Major := StrtoInt(Slice(Result, '.'));
        except
          Major := 0;
        end;
        try
          Minor := StrtoInt(Slice(Result, '.'));
        except
          Minor := 0;
        end;
        Result := Format('%d.%.2d', [Major, Minor]);
      end;
    end
    else
      Result := '< Pas d''info de version >';
  end
  else
    Result := '< Impossible de récupérer les infos de version >';
end;

function TfshVersionLabel.GetDateTimeInfo: string;
var
  DT: TDateTime;
  FileHandle: Longint;
  Filecheck: array[0..255] of Char;
begin

  Filecheck := '';

  if FFilename = '' then
    GetModuleFileName(HInstance, Filecheck, 255)
  else
    StrPCopy(Filecheck, FFilename);

  FileHandle := CreateFile(Filecheck, GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, 0, 0);
  if FileHandle = -1 then
  begin
    Result := '<Impossible de récuperer un handle de fichier>';
    Exit;
  end;

  DT := FileDateToDateTime(FileGetDate(FileHandle));
  CloseHandle(FileHandle);
  Result := FOrmatDateTime(FDateTimeFormat, DT);
end;

function TfshVersionLabel.GetFileSizeInfo: string;
var
  fs: Longint;
  FileHandle: Longint;
  Filecheck: array[0..255] of Char;
begin

  Filecheck := '';

  if FFilename = '' then
    GetModuleFileName(HInstance, Filecheck, 255)
  else
    StrPCopy(Filecheck, FFilename);

  FileHandle := CreateFile(Filecheck, GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, 0, 0);
  if FileHandle = -1 then
  begin
    Result := '<Impossible de récuperer un handle de fichier>';
    Exit;
  end;

  fs := GetFileSize(FileHandle, nil);
  CloseHandle(FileHandle);
  Result := FormatFloat(FFileSizeFormat, fs);
end;

// Looks at the translation structure and returns the hex formatted string
// containing the language and code page identifier

function TfshVersionLabel.CalcLangCharset(Buffer: pointer; Buflen: UINT):
  string;
begin
(*
Buffer points to the following structure? Not as far as I can tell it doesn't!
Oh well, go with what works...
WORD=16 bit Value
WCHAR=WideChar
Var {
      WORD  wLength;
      WORD  wValueLength;
      WORD  wType;
      WCHAR szKey[];
      WORD  Padding[];
      DWORD Value[];
    };
*)

  Result := Format('%4.4x%4.4x', [LoWord(UInt(Buffer^)), HiWord(UInt(Buffer^))]);

end;

{Called at run time to get version information}

function TfshVersionLabel.GetInfo: string;
//var dump, size: Integer;
var
  dump: DWORD;
  size: Integer;
  buffer: PChar;
  FileCheck: array[0..255] of Char;
begin
  if csDesigning in Self.ComponentState then
    Result := '< Pas d''information en mode design >'
  else
  begin
    Filecheck := '';

    if FFilename = '' then
      GetModuleFileName(HInstance, Filecheck, 255)
    else
      StrPCopy(Filecheck, FFilename);

    size := GetFileVersionInfoSize(Filecheck, dump);
    if size = 0 then
    begin
      Result := '< Aucunes données disponibles >';
    end
    else
    begin
      buffer := StrAlloc(size + 1);
      try
        if not GetFileVersionInfo(FileCheck, 0, size, buffer) then
          Result := '< Impossible de récupérer les infos de version >'
        else
        begin
          case FVersionResource of
            vrFlags: Result := GetFixedFileInfo(buffer, size);
            vrDateTime: Result := GetDateTimeInfo;
            vrFileSize: Result := GetFileSizeInfo;
          else
            Result := GetStringFileInfo(buffer, size);
          end;
        end;
      finally
        StrDispose(Buffer);
      end;
    end;
  end;
  if ShowInfoPrefix then Result := InfoPrefix + ' ' + Result;
end;

procedure TfshVersionLabel.SetInfoPrefix(Value: string);
begin
  if FInfoPrefix = Value then Exit;
  FInfoPrefix := Value;
    {The caption needs to be recalculated everytime the prefix is
    changed, otherwise the detaults override the user specified one}
  SetupCaption;
end;

procedure TfshVersionLabel.SetVersionResourceKey(Value: string);
begin
  if FVersionResourceKey = Value then Exit;
  FVersionResourceKey := Value;
  InfoPrefix := Value;
end;

function TfshVersionLabel.GetInfoPrefix: string;
begin
  Result := FInfoPrefix;
end;

procedure TfshVersionLabel.SetShowInfoPrefix(Value: Boolean);
begin
  if FShowInfoPrefix = Value then Exit;
  FShowInfoPrefix := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetLangCharset(Value: string);
begin
  if FLangCharSet = Value then Exit;
  FLangCharSet := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetupCaption;
begin
  Caption := GetInfo;
  FInfoString := Caption;
end;

// Find first section of string - remove from front of string and return it

function TfshVersionLabel.Slice(var S: string; Delimiter: string): string;
var
  p: Integer;
begin
  p := Pos(Delimiter, S);
  if p = 0 then
  begin
    Result := S;
    S := '';
  end else begin
    Result := Copy(S, 1, p - 1);
    Delete(S, 1, p + Length(Delimiter) - 1);
  end;
end;

end.

