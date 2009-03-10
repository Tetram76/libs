unit FEditDatabase;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, EasyDB;

type
  TFenEditDatabase = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AliasSelecter: TComboBox;
    PathSelecter: TEdit;
    btnPath: TButton;
    LabelDefaultName: TLabel;
    btnBDE: TButton;
    Bevel1: TBevel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnDefault: TBitBtn;
    Label4: TLabel;
    DriverSelecter: TComboBox;
    procedure btnPathClick(Sender: TObject);
    procedure ExitPanel1OkClick(Sender: TObject);
    procedure btnBDEClick(Sender: TObject);
    procedure AliasSelecterChange(Sender: TObject);
    procedure PathSelecterChange(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure DriverSelecterChange(Sender: TObject);
  private
    FEasyDB: TEasyDataBase;
  public
    procedure Initialize(EasyDB: TEasyDataBase);
  end;

var
  FenEditDatabase: TFenEditDatabase;

implementation

uses Registry, FileCtrl, DBTables, FGUtils, ASysUtil;

{$R *.DFM}

procedure TFenEditDatabase.Initialize(EasyDB: TEasyDataBase);
var OM: TConfigMode;
begin
  FEasyDB:= EasyDB;
  if FEasyDB.EasyMode = emNone then
  begin
    PathSelecter.Enabled:= False;
    btnPath.Enabled:= False;
    Label3.Enabled:= False;
  end;
  btnDefault.Enabled:= FEasyDB.UseRegistry;
  LabelDefaultName.Caption:= EasyDB.DatabaseName;
  OM:= Session.ConfigMode;
  Session.ConfigMode:= [cfmPersistent];
  try
    Session.GetAliasNames(AliasSelecter.Items);
  finally
    Session.ConfigMode:= OM;
  end;
  if AliasSelecter.Items.IndexOf(EasyDB.AliasName) <> -1 then
  begin
    PathSelecter.Text:= '';
    AliasSelecter.Text:= EasyDB.AliasName;
  end else begin
    AliasSelecter.Text:= '';
    PathSelecter.Text:= RemoveSlash(EasyDB.ForcedPath);
    if PathSelecter.Text = '' then
      PathSelecter.Text:= RemoveSlash(EasyDB.Params.Values['PATH']);
  end;
  if UpperCase(EasyDB.DriverName) = 'STANDARD' then
    DriverSelecter.ItemIndex:= 0
  else if UpperCase(EasyDB.DriverName) = 'MSACCESS' then
  begin
    OpenDialog1.Filter:= 'Fichiers MSAccess|*.mdb|Tous fichiers|*.*';
    DriverSelecter.ItemIndex:= 1;
  end else if UpperCase(EasyDB.DriverName) = 'INTRBASE' then
  begin
    OpenDialog1.Filter:= 'Fichiers Interbase|*.gdb|Tous fichiers|*.*';
    DriverSelecter.ItemIndex:= 2;
  end else begin
    if Trim(EasyDB.DriverName) <> '' then
    begin
      PathSelecter.Enabled:= False;
      btnPath.Enabled:= False;
      Label3.Enabled:= False;
    end;
  end;
end;

procedure TFenEditDatabase.btnPathClick(Sender: TObject);
var S: string;
begin
  if UpperCase(FEasyDB.DriverName) = 'STANDARD' then
  begin
    S:= PathSelecter.Text;
    if SelectDirectory('Sélection du chemin de base de données', '', S) then
    begin
      PathSelecter.Text:= S;
      AliasSelecter.Text:= '';
    end;
  end else if (UpperCase(FEasyDB.DriverName) = 'MSACCESS') or (UpperCase(FEasyDB.DriverName) = 'INTRBASE') then
  begin
    if OpenDialog1.Execute then
    begin
      PathSelecter.Text:= OpenDialog1.FileName;
      AliasSelecter.Text:= '';
    end;
  end;
end;

procedure TFenEditDatabase.AliasSelecterChange(Sender: TObject);
begin
  PathSelecter.Text:= '';
end;

procedure TFenEditDatabase.PathSelecterChange(Sender: TObject);
begin
  AliasSelecter.Text:= '';
end;

procedure TFenEditDatabase.ExitPanel1OkClick(Sender: TObject);
begin
//  FEasyDB.ExtraPath:= '';
  if Trim(AliasSelecter.Text) <> '' then
  begin
    FEasyDB.ForcedPath:= '';
    FEasyDB.Params.Clear;
    FEasyDB.AliasName:= AliasSelecter.Text;
  end else begin
    FEasyDB.AliasName:= '';
    FEasyDB.Params.Clear;
    FEasyDB.Params.Add('PATH='+RemoveSlash(PathSelecter.Text));
    FEasyDB.Params.Add('DATABASE NAME='+RemoveSlash(PathSelecter.Text));
    if (UpperCase(FEasyDB.DriverName) = 'MSACCESS') or (UpperCase(FEasyDB.DriverName) = 'INTRBASE') then
    begin
      FEasyDB.ForcedPath:= ExtractFilePath(PathSelecter.Text);
      FEasyDB.ExtraFile:= ExtractFileName(PathSelecter.Text);
    end else
      FEasyDB.ForcedPath:= PathSelecter.Text;
  end;
  ModalResult:= mrOk;
end;

procedure TFenEditDatabase.btnBDEClick(Sender: TObject);
var Reg: TRegistry;
    Path: string;
begin
  Reg:= TRegistry.Create;
  try
    Reg.RootKey:= HKEY_LOCAL_MACHINE;
    Reg.OpenKey('\Software\Borland\Database Engine', False);
    Path:= AddSlash(ExtractFilePath(Reg.ReadString('CONFIGFILE01'))) + 'BDEAdmin.exe';
    if not FileExists(Path) then
      raise Exception.Create('BDEAdmin.exe introuvable !');
    ExecCommandLine(Path);
  finally
    Reg.Free;
  end;
end;

type TPEasyDB = class(TEasyDatabase);

procedure TFenEditDatabase.btnDefaultClick(Sender: TObject);
begin
  TPEasyDB(FEasyDB).SetDefaultRegistry;
  MessageDlg('Vous devez redémarrer l''application afin que les modifications ' +
    'prennent effet', mtInformation, [mbOk], 0);
  Application.Terminate;
end;

procedure TFenEditDatabase.DriverSelecterChange(Sender: TObject);
begin
  PathSelecter.Enabled:= DriverSelecter.ItemIndex > -1;
  btnPath.Enabled:= DriverSelecter.ItemIndex > -1;
  Label3.Enabled:= DriverSelecter.ItemIndex > -1;
  case DriverSelecter.ItemIndex of
    0: FEasyDB.DriverName:= 'STANDARD';
    1: FEasyDB.DriverName:= 'MSACCESS';
    2: FEasyDB.DriverName:= 'INTRBASE';
  end;
  if UpperCase(FEasyDB.DriverName) = 'MSACCESS' then
    OpenDialog1.Filter:= 'Fichiers MSAccess|*.mdb|Tous fichiers|*.*'
  else if UpperCase(FEasyDB.DriverName) = 'INTRBASE' then
    OpenDialog1.Filter:= 'Fichiers Interbase|*.gdb|Tous fichiers|*.*';
end;

end.
