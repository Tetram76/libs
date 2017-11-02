unit FGDirD;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDirectoryDialog = class(TComponent)
  private
    { Déclarations privées }
    FDirectory : String;
    FMask : String;
    FCaption : TCaption;
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
  published
    { Déclarations publiées }
    Property Directory : String Read FDirectory Write FDirectory;
    Property Mask : String Read FMask Write FMask;
    Property Caption  : TCaption Read FCaption Write FCaption;
    Function Execute : Boolean;
  end;

procedure Register;

implementation

Uses GzConsts, FChDir;

Function TDirectoryDialog.Execute : Boolean;
Var
  ADlg : TDlgChoixDirectory;
Begin
  Result:=False;
  ADlg:=TDlgChoixDirectory.Create(Self);
  Try
    If FCaption<>'' then
      ADlg.Caption:=FCaption;
    If FDirectory<>'' then
      ADlg.Directory:=FDirectory;
    If FMask<>'' then
      ADlg.FileListBox1.Mask:=FMask;
    If ADlg.ShowModal=mrOk then
    Begin
      Result:=True;
      FDirectory:=ADlg.Directory;
    End;
  Finally
    ADlg.Free;
  End;
End;

procedure Register;
begin
  RegisterComponents(SGrizzlyMisc, [TDirectoryDialog]);
end;

end.
