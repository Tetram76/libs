unit GzMacroComponents_Reg;

{$I GrizzlyDefine.inc}

interface

procedure Register;

implementation

uses
  SysUtils, Classes, GzConsts, DB, GZDesign,
  GZRL_Composant, GzExeUpdate, GzADOEx, GzPnlExpt
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};

type
  TDataSetModelesFieldProperty = class(TFieldProperty)
  public
    function GetDataSet: TDataSet; override;
  end;

function TDataSetModelesFieldProperty.GetDataSet: TDataSet;
var F: TDataSetModeles;
begin
  F:= Pointer(GetComponent(0));
  if F = nil then
    Result:= nil
  else
    Result:= F.DataSet;
end;

{ TDBStringProperty }

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

type
  TPanelSelectionFieldNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TPanelSelectionFieldNameProperty.GetValueList(List: TStrings);
var
  i : Integer;
  AFlds : GzPnlExpt.TFilterFieldList;
  AF : string;
begin
  AFlds := (GetComponent(0) as TSelectionItem).Parent.Fields;
  for i := 0 to AFlds.Count - 1 do
  begin
    AF := Trim(AFlds[i].FieldName);
    if (AF <> '') and (List.IndexOf(AF) <> 0) then
      List.Add(AF);
  end;
end;

procedure Register;
begin
  RegisterComponents(SGrizzlyTools, [TRequetesLibres]);
  RegisterPropertyEditor(TypeInfo(TFieldName), TDataSetModeles, '', TDataSetModelesFieldProperty);

  RegisterComponents(SGrizzlyTools, [TExeUpdate]);
  RegisterComponents(SGrizzlyTools, [TSelfUpdate]);

  RegisterComponents(SGrizzlyDB, [TPanelFilterExpert]);
  RegisterPropertyEditor(TypeInfo(string), TSelectionItem, 'FieldName', TPanelSelectionFieldNameProperty);
end;

end.
