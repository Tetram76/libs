unit verslabp;

{$H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvVersionInfo;

type
  pPChar = ^PChar;

  TVersionResources = (vrCompanyName, vrFileDescription, vrFileVersion, vrInternalName, vrLegalCopyright, vrOriginalFilename, vrProductName, vrProductVersion,
    vrComments, vrFlags, vrShortFileVersion, vrDateTime, vrFileSize);

  TfshVersionLabel = class(TLabel)
  private
    { Private declarations }
    FVersionResource: TVersionResources;
    FInfoPrefix: string;
    FShowInfoPrefix: Boolean;
    FVersionResourceKey: string;
    FLangCharset: string;
    FInfoString: string; // read only caption - current version info
    FDateTimeFormat: string;
    FFileSizeFormat: string;
    FInfoSuffix: string;
    FVersionInfo: TJvVersionInfo;

    procedure SetupInfoPrefix;
    procedure SetupResourceKey;
    function GetFixedFileInfo: string;
    function GetDateTimeInfo: string;
    function GetFileSizeInfo: string;
    function GetInfo: string;
    procedure SetupCaption;
    procedure SetInfoSuffix(const Value: string);
    function GetFilename: string;
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
    property InfoSuffix: string read FInfoSuffix write SetInfoSuffix;
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
    property Filename: string read GetFilename write SetFilename stored False;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property FileSizeFormat: string read FFileSizeFormat write SetFileSizeFormat;
  end;

const
  { The order of this array must be the same as the VersionResources
    enum type as that is used for the index lookup }
  VersionLookup: array [TVersionResources, 0 .. 1] of string = (('CompanyName', 'Organisation:'), ('FileDescription', 'Description:'),
    ('FileVersion', 'Version de fichier:'), ('InternalName', 'Nom interne:'), ('LegalCopyright', 'Copyright:'),
    ('OriginalFilename', 'Nom du fichier original:'), ('ProductName', 'Nom du produit:'), ('ProductVersion', 'Version du produit:'),
    ('Comments', 'Commentaires:'), ('Flags', 'Flags:'), ('FileVersion', 'v'), ('DateTime', 'File Date/Time:'), ('Filesize', 'File Size:'));

procedure Register;

implementation

uses
  JvJCLUtils, System.IOUtils, JclFileUtils, Divers;

procedure Register;
begin
  RegisterComponents('Tetram', [TfshVersionLabel]);
end;

constructor TfshVersionLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionInfo := AppVerInfo;
  FInfoString := '';
  FDateTimeFormat := FormatSettings.ShortDateFormat;
  FFileSizeFormat := '#,#0" Bytes"';
  WordWrap := False;
  AutoSize := True;
  ShowInfoPrefix := True;
  LangCharset := '-1'; { -1 = auto detect }
  VersionResource := vrFileVersion;
end;

destructor TfshVersionLabel.Destroy;
begin
  inherited;
  FVersionInfo.Free;
end;

procedure TfshVersionLabel.SetVersionResource(Value: TVersionResources);
begin
  FVersionResource := Value;
  SetupResourceKey;
  SetupInfoPrefix;
end;

procedure TfshVersionLabel.SetFilename(Value: string);
begin
  FVersionInfo.Filename := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetDateTimeFormat(Value: string);
begin
  if Value = '' then
    Exit;

  FDateTimeFormat := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetFileSizeFormat(Value: string);
begin
  if Value = '' then
    Exit;

  FFileSizeFormat := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetupInfoPrefix;
var
  S: string;
begin
  S := VersionLookup[FVersionResource, 1];
  InfoPrefix := S;
end;

procedure TfshVersionLabel.SetupResourceKey;
var
  S: string;
begin
  S := VersionLookup[FVersionResource, 0];
  VersionResourceKey := S;
end;

function TfshVersionLabel.GetFixedFileInfo: string;
var
  FixedFileInfo: PVSFixedFileInfo;
begin
  Result := '';
  FixedFileInfo := FVersionInfo.FixedFileInfo;
  if FixedFileInfo <> nil then
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
      if Result[1] = ',' then
        Delete(Result, 1, 2);
  end;
end;

function TfshVersionLabel.GetDateTimeInfo: string;
begin
  if FVersionInfo.VerFileDate = NullDate then
    Result := ''
  else
    Result := FormatDateTime(FDateTimeFormat, FVersionInfo.VerFileDate);
end;

function TfshVersionLabel.GetFilename: string;
begin
  Result := FVersionInfo.Filename;
end;

function TfshVersionLabel.GetFileSizeInfo: string;
begin
  Result := FormatFloat(FFileSizeFormat, FileGetSize(Filename));
end;

function TfshVersionLabel.GetInfo: string;
var
  v: TVersionNumber;
begin
  Result := '';
  if csDesigning in Self.ComponentState then
  begin
    Result := Name;
    Exit;
  end;

  case FVersionResource of
    vrFlags:
      Result := GetFixedFileInfo;
    vrDateTime:
      Result := GetDateTimeInfo;
    vrFileSize:
      Result := GetFileSizeInfo;
    vrCompanyName:
      Result := FVersionInfo.CompanyName;
    vrFileDescription:
      Result := FVersionInfo.FileDescription;
    vrFileVersion:
      Result := FVersionInfo.FileVersion;
    vrInternalName:
      Result := FVersionInfo.InternalName;
    vrLegalCopyright:
      Result := FVersionInfo.LegalCopyright;
    vrOriginalFilename:
      Result := FVersionInfo.OriginalFilename;
    vrProductName:
      Result := FVersionInfo.ProductName;
    vrProductVersion:
      Result := FVersionInfo.ProductVersion;
    vrComments:
      Result := FVersionInfo.Comments;
    vrShortFileVersion:
      begin
        v := FVersionInfo.FileVersion;
        Result := Format('%d.%.2d', [v.MajorVersion, v.MinorVersion]);
      end;
  end;

  if ShowInfoPrefix then
    Result := InfoPrefix + ' ' + Result;
  Result := Result + ' ' + InfoSuffix;
end;

procedure TfshVersionLabel.SetInfoPrefix(Value: string);
begin
  if FInfoPrefix = Value then
    Exit;
  FInfoPrefix := Value;
  { The caption needs to be recalculated everytime the prefix is
    changed, otherwise the detaults override the user specified one }
  SetupCaption;
end;

procedure TfshVersionLabel.SetInfoSuffix(const Value: string);
begin
  if FInfoSuffix = Value then
    Exit;
  FInfoSuffix := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetVersionResourceKey(Value: string);
begin
  if FVersionResourceKey = Value then
    Exit;
  FVersionResourceKey := Value;
  InfoPrefix := Value;
end;

function TfshVersionLabel.GetInfoPrefix: string;
begin
  Result := FInfoPrefix;
end;

procedure TfshVersionLabel.SetShowInfoPrefix(Value: Boolean);
begin
  if FShowInfoPrefix = Value then
    Exit;
  FShowInfoPrefix := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetLangCharset(Value: string);
begin
  if FLangCharset = Value then
    Exit;
  FLangCharset := Value;
  SetupCaption;
end;

procedure TfshVersionLabel.SetupCaption;
begin
  Caption := GetInfo;
  FInfoString := Caption;
end;

end.
