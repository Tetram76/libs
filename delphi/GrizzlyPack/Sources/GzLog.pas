{***************************************************************
 *
 * Unit Name: FGLog
 * Purpose  : Logs component to easily log anything in a file or
              a dataset.
 * Author   : Frederic GUILLIEN fguillien@grizzlydev.com
 * History  : Beta release 99-08-26
 *
 ****************************************************************}

unit GzLog;

interface

uses
  SysUtils, Classes;

type
  EFileLogException = class(Exception);

  TCustomLog = class(TComponent)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    procedure LogEvent(AContexte, ADescription: string); virtual; abstract;
    procedure Clear; virtual; abstract;
  published
    { Déclarations publiées }
  end;

  TFileLog = class(TCustomLog)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
    FFileName: TFileName;
    FFieldSeparator: Char;
  public
    { Déclarations publiques }
    procedure LogEvent(AContexte, ADescription: string); override;
    procedure Clear; override;
    constructor Create(AOwner: TComponent); override;
  published
    { Déclarations publiées }
    property Filename: TFileName read FFileName write FFileName;
    property FieldSeparator: Char read FFieldSeparator write FFieldSeparator default ';';
  end;

implementation

uses FGUtils;

procedure TFileLog.Clear;
var
  TheName: string;
begin
  if FileName <> '' then
  begin
    if ExtractFileName(FileName) = FileName then
      TheName:= AddSlash(ExtractFilePath(ParamStr(0))) + FileName
    else
      TheName:= FileName;
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

constructor TFileLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName:= 'APPLI.LOG';
  FFieldSeparator:= #9;
end;

procedure TFileLog.LogEvent(AContexte, ADescription: string);
var
  AFile: TextFile;
  TheName: string;
  ACh: string;
begin
  if FileName <> '' then
  begin
    if ExtractFileName(FileName) = FileName then
      TheName:= AddSlash(ExtractFilePath(ParamStr(0))) + FileName
    else
      TheName:= FileName;
    AssignFile(AFile, TheName);
    try
      if FileExists(TheName) then
        Append(AFile)
      else
        Rewrite(AFile);
      ACh:= DateTimeToStr(Now) + FieldSeparator;
{$IFDEF WIN32}
      ACh:= ACh + GetComputerName + FieldSeparator + GetUserName + FieldSeparator;
{$ENDIF}
      ACh:= ACh + ParamStr(0) + FieldSeparator + WashedString(AContexte) + FieldSeparator + WashedString(ADescription);
      WriteLn(AFile, ACh);
    finally
      CloseFile(AFile);
    end;
  end
  else
    raise EFileLogException.Create('Le fichier ''' + FFileName + ''' n''existe pas');
end;

end.

