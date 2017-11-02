unit GzDBCtrls_Reg;

{$I GrizzlyDefine.INC}

interface

procedure Register;

implementation

Uses Classes, SysUtils, DB, TypInfo, GzConsts, Controls
  {$IFDEF WIN32} {Seulement avec le BDE ça...}
  , EasyDB
  {$ENDIF}
  , DBNavBtn, DBNavMgr, DBFind, DBInfo, DBTransfer, FiltExpt, FGDBMDTV, FGOrdLB
  , FFieldMapEditor
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};

type
  TFindSearchFieldProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{	TFindSearchFieldProperty }

function TFindSearchFieldProperty.GetValue: string;
begin
  Result:= GetStrValue;
end;

procedure TFindSearchFieldProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TFindSearchFieldProperty.Edit;
begin
end;

procedure TFindSearchFieldProperty.GetValues(Proc: TGetStrProc);
var Source: TPersistent;
  Dataset: TDataSet;
  Field: TField;
  i: Integer;
  FieldTagValue: Integer;
  AllowCalcFields: Boolean;

    (* DB de D7
    TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd);*)

  function AllowField(Field: TField): Boolean;
  begin
    Result:= Field.Tag = FieldTagValue;
    Result:= Result and ((Field.DataType in [ftString, ftSmallint, ftInteger,
      ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
      ftAutoInc, ftFixedChar, ftWideString, ftLargeInt, ftVariant
      {$IFDEF GZ_D6}, ftTimeStamp, ftFMTBcd{$ENDIF}
      ]) or ((Field.DataType = ftMemo) and AllowCalcFields));
    Result:= Result and ((Field.FieldKind in [fkData, fkInternalCalc]) or AllowCalcFields);
  end;
begin
  try
    Source:= GetComponent(0);
    if Source is TFindEdit then
      DataSet:= TFindEdit(Source).DataSet
    else if Source is TFindPanel then
      DataSet:= TFindPanel(Source).DataSet
    else
      DataSet:= nil;
    if Dataset <> nil then
    begin
      if Source is TFindEdit then
      begin
        FieldTagValue:= TFindEdit(Source).FieldTagValue;
        AllowCalcFields:= TFindEdit(Source).AllowCalcFields;
      end
      else if Source is TFindPanel then
      begin
        FieldTagValue:= TFindPanel(Source).FieldTagValue;
        AllowCalcFields:= TFindPanel(Source).AllowCalcFields;
      end;
      for i:= 0 to DataSet.FieldCount - 1 do
      begin
        Field:= DataSet.Fields[i];
        if AllowField(Field) then
          Proc(Field.FieldName);
      end;
    end;
  except
    raise;
  end;
end;

function TFindSearchFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
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

{$IFDEF WIN32} {Seulement avec le BDE ça...}
type
  TEasyDriverNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TEasyDriverNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TEasyDatabase).Database.Session.GetDriverNames(List);
end;

type
  TEasyAliasNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TEasyAliasNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TEasyDatabase).Database.Session.GetAliasNames(List);
end;

type
  TEasyDBEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TEasyDBEditor.GetVerbCount: Integer;
begin
  Result:= 1;
end;

function TEasyDBEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result:= 'Editeur de chemin de BD';
end;

procedure TEasyDBEditor.ExecuteVerb(Index: Integer);
begin
  TEasyDataBase(Component).EditDatabase;
  Designer.Modified;
end;
{$ENDIF}

type
  TLevelFieldNameProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{	TLevelFieldNameProperty }

function TLevelFieldNameProperty.GetValue: string;
begin
  Result:= GetStrValue;
end;

procedure TLevelFieldNameProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TLevelFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  LaSource: TPersistent;
  LeDataset: TDataSet;
  i: Integer;
begin
  try
    LaSource:= GetComponent(0);
    if LaSource is TTreeViewLevel then
      LeDataSet:= TTreeViewLevel(LaSource).DataLink.DataSet
    else
      LeDataSet:= nil;
    if LeDataSet <> nil then
      LeDataSet.FieldDefs.Update;
    if (LeDataset <> nil) and (LeDataSet.Fields.Count > 0) then
    begin
      for i:= 0 to LeDataSet.Fields.Count - 1 do
      begin
        if LeDataSet.Fields[i].DataType
          in [ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat,
          ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftAutoInc,
          ftWideString, ftLargeInt] then
        begin
          Proc(LeDataSet.Fields[i].FieldName);
        end;
      end;
    end;
  except
    raise;
  end;
end;

function TLevelFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
end;

{ TFGDBMDTreeViewComponentEditor }
type
  TFGDBMDTreeViewComponentEditor = class(TComponentEditor)
  private
    {$IFDEF GZ_D6}
    procedure DoForEach(const PropertyEditor: IProperty);
    {$ELSE}
    procedure DoForEach(PropertyEditor: TPropertyEditor);
    {$ENDIF}
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure ExecuteOnClick;
  end;

function TFGDBMDTreeViewComponentEditor.GetVerbCount: Integer;
begin
  Result:= 1;
end;

function TFGDBMDTreeViewComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    result:= 'Editeur de Niveaux Maître/Détail'
end;

procedure TFGDBMDTreeViewComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
  begin
    ExecuteOnClick;
  end;
end;

{$IFDEF GZ_D6}
procedure TFGDBMDTreeViewComponentEditor.ExecuteOnClick;
var
  Components: IDesignerSelections;
begin
  Components:= TDesignerSelections.Create;
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, DoForEach);
end;

procedure TFGDBMDTreeViewComponentEditor.DoForEach(const PropertyEditor: IProperty);
var
  PropName: string;
begin
  PropName:= PropertyEditor.GetName;
  if (PropName = 'Levels') or (PropName = 'SubLevels') then
    PropertyEditor.Edit;
end;

{$ELSE}
  {$IFDEF GZ_D5}
procedure TFGDBMDTreeViewComponentEditor.ExecuteOnClick;
var
  Components: TDesignerSelectionList;
begin
  Components:= TDesignerSelectionList.Create;
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, DoForEach);
  Components.Free;
end;
  {$ELSE}
procedure TFGDBMDTreeViewComponentEditor.ExecuteOnClick;
var
  Components: TComponentList;
begin
  Components:= TComponentList.Create;
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, DoForEach);
  Components.Free;
end;
  {$ENDIF}

procedure TFGDBMDTreeViewComponentEditor.DoForEach(PropertyEditor: TPropertyEditor);
var
  PropName: string;
begin
  PropName:= PropertyEditor.GetName;
  if (PropName = 'Levels') or (PropName = 'SubLevels') then
    PropertyEditor.Edit;
  PropertyEditor.Free;
end;
{$ENDIF}

const
  lblTransferFieldsEditor = 'Editeur des champs de transfert';
  lblLinkFieldsEditor = 'Editeur des champs de liaison';

procedure GetAvailableFields(DataSet: TDataSet; List: TStrings; Calculated: Boolean);
var i: Integer;
begin
  if DataSet = nil then
  begin
    List.Clear;
    Exit;
  end;
  {}
  List.Clear;
  if DataSet.FieldCount = 0 then
  begin
    DataSet.FieldDefs.Update;
    for i:= 0 to DataSet.FieldDefs.Count - 1 do
      List.Add(DataSet.FieldDefs[i].Name);
  end else
  begin
    for i:= 0 to DataSet.FieldCount - 1 do
      if Calculated or (DataSet.Fields[i].FieldKind in [fkData, fkInternalCalc]) then
        List.Add(DataSet.Fields[i].FieldName);
  end;
end;

{ TDBTransferEditor }

type
  TDBTransferEditor = class(TComponentEditor)
  public
    procedure Edit; override;
		procedure ExecuteVerb(Index: Integer); override;
		function GetVerb(Index: Integer): string; override;
		function GetVerbCount: Integer; override;
    {}
    procedure EditTransferFields;
  end;

procedure TDBTransferEditor.EditTransferFields;
var F: TFieldMapEditor;
begin
  F:= TFieldMapEditor.Create(nil);
  try
    F.FieldMap:= TDBTransfer(Component).TransferFields;
    GetAvailableFields(TDBTransfer(Component).Source, F.SourceFields, True);
    GetAvailableFields(TDBTransfer(Component).Destination, F.DestFields, False);
    if F.Execute = mrOk then
      TDBTransfer(Component).TransferFields.Assign(F.FieldMap);
  finally
    F.Free;
  end;
end;

procedure TDBTransferEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TDBTransferEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    EditTransferFields;
end;

function TDBTransferEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result:= lblTransferFieldsEditor;
end;

function TDBTransferEditor.GetVerbCount: Integer;
begin
  Result:= 1;
end;

type
  TFieldMapProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TFieldMapProperty.GetValue: string;
begin
  Result:= '(TStrings)';
end;

function TFieldMapProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paReadOnly, paDialog];
end;

procedure TFieldMapProperty.Edit;
var F: TFieldMapEditor;
    Component: TComponent;
    S: string;
begin
  Component:= TComponent(GetComponent(0));
  F:= TFieldMapEditor.Create(nil);
  try
    if GetName = 'TransferFields' then
      S:= lblTransferFieldsEditor
    else
      S:= lblLinkFieldsEditor;
    F.Caption:= S;
    F.FieldMap:= Pointer(GetOrdValue);
    GetAvailableFields(TDBTransfer(Component).Source, F.SourceFields, True);
    GetAvailableFields(TDBTransfer(Component).Destination, F.DestFields, False);
    if F.Execute = mrOk then
    begin
      SetOrdValue(Integer(F.FieldMap));
      Modified;
      Self.Designer.Modified;
    end;
  finally
    F.Free;
  end;
end;

procedure Register;
begin
  {$IFDEF WIN32} {Seulement avec le BDE ça...}
  { EasyDB }
  RegisterPropertyEditor(TypeInfo(string), TEasyDatabase, 'DesigningDatabase', TEasyAliasNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TEasyDatabase, 'AliasName', TEasyAliasNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TEasyDatabase, 'DriverName', TEasyDriverNameProperty);
  {Dépendance avec la RX là...
  RegisterPropertyEditor(TypeInfo(string), TEasyDatabase, 'ExtraPath', TPathProperty);
  RegisterPropertyEditor(TypeInfo(string), TEasyDatabase, 'ExtraFile', TFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TEasyDatabase, 'ForcedPath', TPathProperty);}
  RegisterComponentEditor(TEasyDatabase, TEasyDBEditor);
  RegisterComponents(SGrizzlyDB, [TEasyDatabase]);
  {$ENDIF}

  { DBNav }
  RegisterComponents(SGrizzlyDB, [TDBNavBarre]);
  RegisterComponents(SGrizzlyDB, [TDBNavManager]);

  { DBFind }
  RegisterComponents(SGrizzlyDB, [TFindEdit]);
  RegisterComponents(SGrizzlyDB, [TFindPanel]);
  RegisterPropertyEditor(TypeInfo(string), TFindEdit, 'SearchField', TFindSearchFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TFindPanel, 'SearchField', TFindSearchFieldProperty);

  { FilterExpert }
  RegisterComponents(SGrizzlyDB, [TFilterExpert]);

  { FGDBMDTV }
  RegisterComponents(SGrizzlyDB, [TDBMDTreeView]);
  RegisterComponentEditor(TCustomDBMDTreeView, TFGDBMDTreeViewComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TTreeViewLevel, 'FieldName', TLevelFieldNameProperty);

  RegisterComponents(SGrizzlyDB, [TOrderByListBox]);

  { DBTransfer }
  RegisterComponents(SGrizzlyDB, [TDBTransfer]);
  RegisterPropertyEditor(TStrings.ClassInfo, TDBTransfer, 'TransferFields', TFieldMapProperty);
  RegisterPropertyEditor(TStrings.ClassInfo, TDBTransfer, 'LinkFields', TFieldMapProperty);
  RegisterComponentEditor(TDBTransfer, TDBTransferEditor);

end;


end.
