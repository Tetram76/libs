unit GzUsers_Reg;

{$I GrizzlyDefine.inc}

interface

procedure Register;

implementation

uses
  Classes, GzConsts, DB,
  FGUsers
  {$IFDEF GZ_D10}
  ,WideStrings
  {$ENDIF}
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};

  { TUsersFieldProperty }

type
  TUsersFieldProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{	TUsersFieldProperty }

function TUsersFieldProperty.GetValue: string;
begin
  Result:= GetStrValue;
end;

procedure TUsersFieldProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TUsersFieldProperty.Edit;
begin
end;

procedure TUsersFieldProperty.GetValues(Proc: TGetStrProc);
var
  LaSource: TPersistent;
  LeDataset: TDataSet;
  i: Integer;
begin
  try
    LaSource:= GetComponent(0);
    if LaSource is TUsersManager then
      LeDataSet:= TUsersManager(LaSource).DataSet
    else
      LeDataSet:= nil;
    if LeDataSet <> nil then
      LeDataSet.FieldDefs.UpDate;
    if (LeDataset <> nil) and (LeDataSet.FieldDefs.Count > 0) then
    begin
      for i:= 0 to LeDataSet.FieldDefs.Count - 1 do
      begin
        Proc(LeDataSet.FieldDefs.Items[i].Name);
    {If LeDataSet.FieldDefs.Items[i].DataType=ftString then
       Begin
        Proc(LeDataSet.FieldDefs.Items[i].Name);
       End;}
      end;
    end;
  except
    raise;
  end;
end;

function TUsersFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
end;

type
  TFieldProperty = class(TStringProperty)
  public
    function GetDataSet: TDataSet; virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

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
    for i:= 0 to L.Count - 1 do
      Proc(L[i]);
  finally
    L.Free;
  end;
end;

type
  TAccessFieldsProperty = class(TFieldProperty)
  public
    function GetDataSet: TDataSet; override;
  end;

function TAccessFieldsProperty.GetDataSet: TDataSet;
begin
  Result:= TUsersManager(GetComponent(0)).AccessDataSet;
end;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'AccessFieldElem', TAccessFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'AccessFieldAccessKind', TAccessFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'AccessFieldPassword', TAccessFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'AccessFieldLevel', TAccessFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'AccessFieldUsers', TAccessFieldsProperty);

  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'FieldShortName', TUsersFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'FieldLongName', TUsersFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'FieldRetryCount', TUsersFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'FieldLevel', TUsersFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TUsersManager, 'FieldPassword', TUsersFieldProperty);

  RegisterComponents(SGrizzlyTools, [TUsersManager]);
end;

end.
