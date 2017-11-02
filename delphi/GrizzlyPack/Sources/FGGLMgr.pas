unit FGGLMgr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, GZGLMgr;

type
  TGenListManager = class(TWithDatabaseParamsGenListManager)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
    function CreateGroupDataset(AReadOnly: Boolean): TDataSet; override;
    function CreateItemDataset(AReadOnly: Boolean): TDataSet; override;
  public
    { Déclarations publiques }
    procedure Edit; override;
  published
    { Déclarations publiées }
  end;

{procedure Register;}

implementation

Uses GzConsts, FGUtils, FFGGLEd, FFGGLAdd, {FFGGLApp,} UFGGLMgr;



{ TGenListManager }

function TGenListManager.CreateGroupDataset(AReadOnly: Boolean): TDataSet;
var
  ATable: TTable;
begin
  ATable:= TTable.Create(Self);
  ATable.DatabaseName:= DatabaseName;
  ATable.TableName:= GroupTableName;
  ATable.ReadOnly:= AReadOnly;
  Result:= ATable;
end;

function TGenListManager.CreateItemDataset(AReadOnly: Boolean): TDataSet;
var
  ATable: TTable;
begin
  ATable:= TTable.Create(Self);
  ATable.DatabaseName:= DatabaseName;
  ATable.TableName:= ItemTableName;
  ATable.ReadOnly:= AReadOnly;
  Result:= ATable;
end;

procedure TGenListManager.Edit;
var
  ADlg : TFenGLMgrEdit;
begin
  try
    ADlg:=TFenGLMgrEdit.Create(Self);
    try
      ADlg.HelpContext:= HelpContextEditGL;
      ADlg.TableElements.DatabaseName:=DatabaseName;
      ADlg.TableElements.TableName:=ItemTableName;
      ADlg.TableGroupes.DatabaseName:=DatabaseName;
      ADlg.TableGroupes.TableName:=GroupTableName;
      ADlg.GLMgr:= Self;
      ADlg.ShowModal;
    finally
      ADlg.Free;
    end;
  finally
    Update;
  end;
end;

end.
