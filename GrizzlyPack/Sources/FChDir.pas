unit FChDir;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, Buttons, ExtCtrls;

type
  TDlgChoixDirectory = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    DriveComboBox1: TDriveComboBox;
    Bevel1: TBevel;
    Panel1: TPanel;
    Label1: TLabel;
  private
    { Déclarations privées }
    Procedure SetDirectory(Value : String);
    Function GetDirectory : String;
  public
    { Déclarations publiques }
    Property Directory : String Read GetDirectory Write SetDirectory;
  end;

var
  DlgChoixDirectory: TDlgChoixDirectory;

implementation

{$R *.DFM}

Procedure TDlgChoixDirectory.SetDirectory(Value : String);
Begin
  DirectoryListBox1.Directory:=Value;
End;

Function TDlgChoixDirectory.GetDirectory : String;
Begin
  Result:=DirectoryListBox1.Directory;
End;

end.
