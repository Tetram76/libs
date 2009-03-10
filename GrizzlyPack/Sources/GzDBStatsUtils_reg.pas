unit GzDBStatsUtils_reg;

{$I GrizzlyDefine.INC}

interface

procedure Register;

implementation

Uses Classes, DB, TypInfo, GzConsts, Controls
  , GzDBStatsUtils
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};

type
  TStatsFieldNameProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{	TLevelFieldNameProperty }

function TStatsFieldNameProperty.GetValue: string;
begin
  Result:= GetStrValue;
end;

procedure TStatsFieldNameProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TStatsFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  LaSource: TPersistent;
  LeDataset: TDataSet;
  i: Integer;
begin
  try
    LaSource:= GetComponent(0);
    if LaSource is TFieldStats then
      LeDataSet:= TFieldStats(LaSource).Dataset
    else
      LeDataSet:= nil;
    if LeDataSet <> nil then
      LeDataSet.FieldDefs.Update;
    if (LeDataset <> nil) and (LeDataSet.Fields.Count > 0) then
    begin
      for i:= 0 to LeDataSet.Fields.Count - 1 do
      begin
        if LeDataSet.Fields[i].DataType
          in [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD,
          ftAutoInc, ftLargeInt] then
        begin
          Proc(LeDataSet.Fields[i].FieldName);
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TStatsFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
end;


procedure Register;
begin
  RegisterComponents(SGrizzlyDB, [TDatasetStats]);
  RegisterPropertyEditor(TypeInfo(string), TFieldStats, 'FieldName', TStatsFieldNameProperty);
end;

end.
