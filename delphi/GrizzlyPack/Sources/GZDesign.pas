unit GZDesign;

{$I GrizzlyDefine.inc}

interface

uses
  SysUtils, Classes, DB
  {$IFDEF GZ_D10}
  ,WideStrings
  {$ENDIF}
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};

type
  TFieldProperty = class(TStringProperty)
  public
    function GetDataSet: TDataSet; virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  
implementation

function TFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paMultiSelect, paSortList];
end;

procedure TFieldProperty.GetValues(Proc: TGetStrProc);
var
{$IFDEF GZ_D10}
  L: TWideStringList;
{$ELSE}
  L: TStringList;
{$ENDIF}
  DataSet: TDataSet;
  i: Integer;
begin
  DataSet := GetDataSet;
  if DataSet = nil then Exit;
{$IFDEF GZ_D10}
  L := TWideStringList.Create;
{$ELSE}
  L := TStringList.Create;
{$ENDIF}
  try
    DataSet.GetFieldNames(L);
    for i := 0 to L.Count - 1 do
      Proc(L[i]);
  finally
    L.Free;
  end;
end;


end.
