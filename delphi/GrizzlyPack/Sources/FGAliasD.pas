unit FGAliasD;

{(c) Frederic GUILLIEN 05/1998}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDatabaseNameDialog = class(TComponent)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
    FCaption : TCaption;
    FDatabaseName, FDirectory, FAliasName : String;
    Function GetDatabaseName : String;
  public
    { Déclarations publiques }
    Property DatabaseName : String Read GetDatabaseName Write FDatabaseName;
    Property Directory : String Read FDirectory Write FDirectory;
    Property AliasName : String Read FAliasName Write FAliasName;
    Function Execute : Boolean;
  Published
    { Déclarations publiées }
    Property Caption : TCaption Read FCaption Write FCaption;
  end;

implementation

Uses FChAlias;

Function TDatabaseNameDialog.GetDatabaseName : String;
begin
  If FDatabaseName<>'' then
    Result:=FDatabaseName
  Else
  If FAliasName<>'' then
    Result:=FAliasName
  Else
    Result:=FDirectory;
end;

Function TDatabaseNameDialog.Execute : Boolean;
Var
  ADlg : TDlgChoixAlias;
Begin
  Result:=False;
  ADlg:=TDlgChoixAlias.Create(Self);
  Try
    If FCaption<>'' then
      ADlg.Caption:=FCaption;
    ADlg.DatabaseName:=DatabaseName;
    If ADlg.ShowModal=mrOk then
    Begin
      Result:=True;
      FDatabaseName:=ADlg.DatabaseName;
      FAliasName:=ADlg.AliasName;
      FDirectory:=ADlg.Directory;
    End;
  Finally
    ADlg.Free;
  End;
End;

end.
