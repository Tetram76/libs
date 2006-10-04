unit verslabp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
    TVersionResources = (vrOrganisation, vrDescription, vrVersionFichier,
                         vrNomInterne, vrCopyright, vrNomFichierOriginal,
                         vrNomProduit, vrVersionProduit, vrCommentaires, vrAttributs);

  TVersionLabelP = class(TLabel)
  private
    { Private declarations }
    FVersionResource: TVersionResources;
    FInfoPrefix: string;
    FShowInfoPrefix: boolean;
    FVersionResourceKey: string;
    FLangCharset: string;

    procedure SetupInfoPrefix;
    procedure SetupResourceKey;
    function GetStringFileInfo(Buffer: Pchar; size: integer): string;
    function GetFixedFileInfo(Buffer: PChar; size: integer): string;
    function GetInfo: string;
    procedure SetupCaption;
  protected
    { Protected declarations }
    procedure SetInfoPrefix(Value: String);
    function GetInfoPrefix: string;
    procedure SetVersionResource(Value: TVersionResources);
    procedure SetShowInfoPrefix(Value: boolean);
    procedure SetVersionResourceKey(Value: string);
    procedure SetLangCharset(Value: string);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property VersionResource: TVersionResources read FVersionResource
                                                write SetVersionResource;
    property VersionResourceKey: string read FVersionResourceKey
                                         write SetVersionResourceKey;
    property InfoPrefix: String read GetInfoPrefix write SetInfoPrefix;
    property ShowInfoPrefix: boolean read FShowInfoPrefix write SetShowInfoPrefix;
    property LangCharset: string read FLangCharset write SetLangCharset;
    property WordWrap;
    property Align;
    property Color;
    property Font;
    property AutoSize;
    property Alignment;
    property ParentFont;
  end;

const
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
                    ('Flags', 'Attributs:'));


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Medi@ Kit', [TVersionLabelP]);
end;

constructor TVersionLabelP.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    WordWrap := false;
    Autosize := true;
    ShowInfoPrefix := true;
    LangCharset :='-1';   {-1 = auto detect}
    VersionResource := vrVersionFichier;
end;

destructor TVersionLabelP.Destroy;
begin
    inherited Destroy;
end;

procedure TVersionLabelP.SetVersionResource(Value: TVersionResources);
begin
    FVersionResource := Value;
    SetupResourceKey;
    SetupInfoPrefix;
end;

procedure TVersionLabelP.SetupInfoPrefix;
var s: string;
begin
    s := VersionLookup[FVersionResource, 1];
    InfoPrefix := s;
end;

procedure TVersionLabelP.SetupResourceKey;
var s: string;
begin
    s := VersionLookup[FVersionResource, 0];
    VersionResourceKey := s;
end;

function TVersionLabelP.GetFixedFileInfo(Buffer: PChar; size: integer): string;
var
  ValLen: Cardinal;
  FixedFileInfo: PVSFixedFileInfo;
begin
    if VerQueryValue(buffer, '\', Pointer(FixedFileInfo), ValLen) then
    begin
        Result := '';
        if (ValLen > 1) then
        begin
            if FixedFileInfo.dwFileFlags and VS_FF_DEBUG <> 0 then
                Result := Result+', Version Debug';
            if FixedFileInfo.dwFileFlags and VS_FF_PRERELEASE <> 0 then
                Result := Result+', Version Pre-Release';
            if FixedFileInfo.dwFileFlags and VS_FF_PATCHED <> 0 then
                Result := Result+', Version Patchée';
            if FixedFileInfo.dwFileFlags and VS_FF_PRIVATEBUILD <> 0  then
                Result := Result+', Version Privée';
            if FixedFileInfo.dwFileFlags and VS_FF_INFOINFERRED <> 0  then
                Result := Result+', Version InfoInferred';
            if FixedFileInfo.dwFileFlags and VS_FF_SPECIALBUILD <> 0  then
                Result := Result+', Version Spéciale';

            if result <> '' then
                if Result[1] = ',' then Delete(Result, 1, 2);
        end;
    end
    else Result := '< Impossible de récupérer les infos >';
end;

function TVersionLabelP.GetStringFileInfo(Buffer: Pchar; size: integer): string;
var vallen, Translen: Cardinal;
    VersionPointer, TransBuffer: pchar;
    Temp: integer;
    CalcLangCharSet: string;
begin
    if FLangCharSet = '-1' then
    begin
        VerQueryValue(buffer, '\VarFileInfo\Translation',
                        pointer(TransBuffer), TransLen);
        if TransLen >= 4 then
        begin
            StrLCopy(@temp, TransBuffer, 2);
            CalcLangCharSet:=IntToHex(temp, 4);
            StrLCopy(@temp, TransBuffer+2, 2);
            CalcLangCharSet := CalcLangCharSet+IntToHex(temp, 4);
            FLangCharSet := CalcLangCharSet;
        end
        else
        begin
            Result := '< Impossible de récupérer les infos de langue >';
            exit;
        end;
    end;

    if VerQueryValue(buffer, pchar('\StringFileInfo\'+FLangCharSet+'\'+
                     VersionResourceKey),
                     pointer(VersionPointer), vallen) then
    begin
        if (Vallen > 1) then
        begin
            SetLength(Result, vallen);
            StrLCopy(Pchar(Result), VersionPointer, vallen);
        end
        else Result := '< Pas d''info de version >';
    end
    else result := '< Impossible de récupérer les infos de version >';
end;

function TVersionLabelP.GetInfo: string;
var dump, size: Cardinal;
    buffer: pchar;
begin
    if csDesigning in Self.ComponentState then result := '< Pas d''infos en conception >'
    else
    begin
        size := GetFileVersionInfoSize(pchar(Application.Exename), dump);
        if  size = 0 then
        begin
            Result := '< Aucunes données disponibles >';
        end
        else
        begin
            buffer := StrAlloc(size+1);
            try
                if not GetFileVersionInfo(Pchar(Application.Exename), 0,
                        size, buffer) then
                    result := '< Impossible de récupérer les infos de version >'
                else
                begin
                    if FVersionResource = vrAttributs then
                        Result := GetFixedFileInfo(buffer, size)
                    else Result := GetStringFileInfo(buffer, size);
                end;
            finally
                StrDispose(Buffer);
            end;
        end;
    end;
    if ShowInfoPrefix then Result := InfoPrefix+' '+Result;
end;

procedure TVersionLabelP.SetInfoPrefix(Value: String);
begin
    if FInfoPrefix = Value then exit;
    FInfoPrefix := Value;
    {The caption needs to be recalculated everytime the prefix is
    changed, otherwise the detaults override the user specified one}
    SetupCaption;
end;

procedure TVersionLabelP.SetVersionResourceKey(Value: string);
begin
    if FVersionResourceKey = Value then exit;
    FVersionResourceKey := Value;
    InfoPrefix := Value;
end;

function TVersionLabelP.GetInfoPrefix: string;
begin
    result := FInfoPrefix;
end;

procedure TVersionLabelP.SetShowInfoPrefix(Value: boolean);
begin
    if FShowInfoPrefix = value then exit;
    FShowInfoPrefix := Value;
    SetupCaption;
end;

procedure TVersionLabelP.SetLangCharset(Value: string);
begin
    if FLangCharSet = Value then exit;
    FLangCharSet := Value;
    SetupCaption;
end;

procedure TVersionLabelP.SetupCaption;
begin
    Caption := GetInfo;
end;


end.
