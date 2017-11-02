unit FChAlias;

{(c) Frederic GUILLIEN 05/1998}

interface

{$I GrizzlyDefine.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, StdCtrls, Buttons, DB, DBTables;

type
  TDlgChoixAlias = class(TForm)
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    BtnOk: TBitBtn;
    BtnAnnuler: TBitBtn;
    CBAlias: TComboBox;
    Label1: TLabel;
    LBTables: TListBox;
    LabelBD: TLabel;
    Database1: TDatabase;
    procedure FormCreate(Sender: TObject);
    procedure CBAliasChange(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
    FDatabaseName, FDirectory, FAliasName : String;
    DirChanging : Boolean;
    procedure SetDatabaseName(Value : String);
    procedure SetDirectory(Value : String);
    procedure SetAliasName(Value : String);
  public
    { Déclarations publiques }
    Property DatabaseName : String Read FDatabaseName Write SetDatabaseName;
    Property Directory : String Read FDirectory Write SetDirectory;
    Property AliasName : String Read FAliasName Write SetAliasName;
  end;

var
  DlgChoixAlias: TDlgChoixAlias;

implementation

{$R *.DFM}

Uses BDE;

procedure TDlgChoixAlias.FormCreate(Sender: TObject);
begin
  Session.GetDatabaseNames(CBAlias.Items);
  DirChanging:=False;
  DirectoryListBox1.OnChange(Self);
end;

procedure TDlgChoixAlias.SetDatabaseName(Value : String);
begin
  {Recherche dans les alias}
  If CBAlias.Items.IndexOf(Value)>=0 then
  begin
    SetAliasName(Value);
  end
  else
  begin
    if DirectoryExists(Value) then
    begin
      SetDirectory(Value)
    end
    else
    if Value<>'' then
      raise Exception.Create(Format('Base de donnée [%s] invalide',[Value]));
  end;
end;

procedure TDlgChoixAlias.SetDirectory(Value : String);
begin
  If DirectoryExists(Value) then
  begin
    DirectoryListBox1.Directory:=Value;
    FDirectory:=Value;
    FDatabaseName:=Value;
    FAliasName:='';
  end
  else
  if Value<>'' then
    raise Exception.Create(Format('Répertoire [%s] invalide',[Value]));
end;

procedure TDlgChoixAlias.SetAliasName(Value : String);
begin
  If CBAlias.Items.IndexOf(Value)>=0 then
  begin
    FDirectory:='';
    FDatabaseName:=Value;
    FAliasName:=Value;
    CBAlias.ItemIndex:=CBAlias.Items.IndexOf(Value);
    CBAlias.OnChange(CBAlias);
  end
  else
  if Value<>'' then
    raise Exception.Create(Format('Alias [%s] invalide',[Value]));
end;

procedure TDlgChoixAlias.CBAliasChange(Sender: TObject);
Var
  ADir : String;
  {$IFNDEF WIN32}
	function fDbiGetDirectory(hDB: hDbiDb): String;
	var
	  Dir: Array[0..dbiMaxPathLen + 1] of Char;
	begin
	  Check(DbiGetDirectory(hDB, False, Dir));
	  Result:= StrPas(Dir);
	end;
  {$ENDIF}
begin
  If DirChanging then
    Exit;
  DirChanging:=True;
  Try
    If CBAlias.Text='' then
      Exit;
    LabelBD.Caption:=CBAlias.Text;
    FAliasName:=LabelBD.Caption;
    FDatabaseName:=LabelBD.Caption;
    FDirectory:='';
    Session.GetTableNames(LabelBD.Caption,'',True,False,LBTables.Items);
    Try
      Database1.AliasName:=FAliasName;
      Database1.DatabaseName:='Essais'+IntToStr(Trunc(Now));
      Try
        Database1.Connected:=True;
      Except
      End;
      If Database1.Connected then
      begin
        Try
          If Not Database1.IsSQLBased then
          Begin
            {$IFDEF WIN32}
            ADir:=Database1.Directory;
            {$ELSE}
            ADir:=fdbiGetDirectory(Database1.Handle);
            {$ENDIF}
            If ADir<>'' then
              DirectoryListBox1.Directory:=ADir;
            FDirectory:=ADir;
          End;
        Finally
          Database1.Connected:=False;
        End;
      end;
    Except
    End;
  Finally
    DirChanging:=False;
  End;
end;

procedure TDlgChoixAlias.DirectoryListBox1Change(Sender: TObject);
begin
  If DirChanging then
    Exit;
  DirChanging:=True;
  Try
    LabelBD.Caption:=DirectoryListBox1.Directory;
    FDirectory:=LabelBD.Caption;
    FDatabaseName:=LabelBD.Caption;
    FAliasName:='';
    Session.GetTableNames(LabelBD.Caption,'*.*',True,False,LBTables.Items);
  Finally
    DirChanging:=False;
  End;
end;

procedure TDlgChoixAlias.FormShow(Sender: TObject);
  Function AddSlash(ADir : String) : String;
  begin
    If (ADir<>'') and (ADir[Length(ADir)]<>'\') then
      Result:=ADir+'\'
    Else
      Result:=ADir;
  end;
begin
  If (Directory<>'') and (CompareText(AddSlash(DirectoryListBox1.Directory),Directory)<>0) then
  Begin
    DirectoryListBox1.Directory:=Directory;
    DirectoryListBox1.OnChange(Self);
  End;
end;

end.
