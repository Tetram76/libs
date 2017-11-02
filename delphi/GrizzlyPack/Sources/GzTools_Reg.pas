unit GzTools_Reg;

{$I GrizzlyDefine.inc}

interface

procedure Register;

implementation

uses
  SysUtils, Classes, GzConsts, DB,
  GzLog, FGLog, FGUseCnt, DBToTxt, FGCstFmt, ShwrMgr, StrCont, FGGLMgr,
  FGAliasD, GZDBParams, GzDBUpdate, TypInfo
  {$IFDEF WIN32}, SingInst, ResStore, Windows, Registry, GZRegParams,
  GzMemoDlg, Dialogs, HotKeyMgr{$ENDIF}
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};


{	TLogFieldProperty }

type
  TLogFieldProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

function TLogFieldProperty.GetValue: string;
begin
  Result:= GetStrValue;
end;

procedure TLogFieldProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TLogFieldProperty.Edit;
begin
end;

procedure TLogFieldProperty.GetValues(Proc: TGetStrProc);
var
  LaSource: TPersistent;
  LeDataset: TDataSet;
  i: Integer;
begin
  try
    LaSource:= GetComponent(0);
    if LaSource is TDatasetLog then
      LeDataSet:= TDatasetLog(LaSource).DataSet
    else
      LeDataSet:= nil;
    if LeDataSet <> nil then
      LeDataSet.FieldDefs.UpDate;
    if (LeDataset <> nil) and (LeDataSet.FieldDefs.Count > 0) then
    begin
      for i:= 0 to LeDataSet.FieldDefs.Count - 1 do
      begin
        Proc(LeDataSet.FieldDefs.Items[i].Name);
      end;
    end;
  except
    raise;
  end;
end;

function TLogFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
end;

{	TGZDBParamFieldProperty }

type
  TGZDBParamFieldProperty = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

function TGZDBParamFieldProperty.GetValue: string;
begin
  Result:= GetStrValue;
end;

procedure TGZDBParamFieldProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TGZDBParamFieldProperty.Edit;
begin
end;

procedure TGZDBParamFieldProperty.GetValues(Proc: TGetStrProc);
var
  LaSource: TPersistent;
  LeDataset: TDataSet;
  i: Integer;
begin
  try
    LaSource:= GetComponent(0);
    if LaSource is TDBParamDatabase then
      LeDataSet:= TDBParamDatabase(LaSource).DataSet
    else
      LeDataSet:= nil;
    if LeDataSet <> nil then
      LeDataSet.FieldDefs.UpDate;
    if (LeDataset <> nil) and (LeDataSet.FieldDefs.Count > 0) then
    begin
      for i:= 0 to LeDataSet.FieldDefs.Count - 1 do
      begin
        Proc(LeDataSet.FieldDefs.Items[i].Name);
      end;
    end;
  except
    raise;
  end;
end;

function TGZDBParamFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
end;

{$IFDEF WIN32}

{	TGZRootKeyProperty }

Type
	TGZRootKeyProperty = Class(TPropertyEditor)
  public
  	function GetValue : String; override;
		Procedure GetValues(Proc: TGetStrProc); override;
    Procedure SetValue(Const Value : String) ; override;
    Procedure Edit; override;
    Function GetAttributes : TPropertyAttributes; override;
  End;

const
  ListeClefChaine : array[0..5] of string = ('HKEY_CLASSES_ROOT', 'HKEY_CURRENT_USER', 'HKEY_LOCAL_MACHINE', 'HKEY_USERS', 'HKEY_CURRENT_CONFIG', 'HKEY_DYN_DATA');
  ListeClefValeur : array[0..5] of HKey = (HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS, HKEY_CURRENT_CONFIG, HKEY_DYN_DATA);

function TGZRootKeyProperty.GetValue : String;
var
  i, k: Cardinal;
begin
  k:= GetOrdValue;
  for i:= 0 to 5 do
  begin
    if ListeClefValeur[i] = k then
    begin
    	Result:= ListeClefChaine[i];
      Exit;
    end;
  end;
  Result:= GetStrValue;
end;

Procedure TGZRootKeyProperty.SetValue(Const Value : String);
var
  i: Integer;
begin
  for i:= 0 to 5 do
  begin
    if CompareText(ListeClefChaine[i], Value)=0 then
    begin
      SetOrdValue(ListeClefValeur[i]);
      Exit;
    end;
  end;
  SetOrdValue(StrToInt(Value));
end;


Procedure TGZRootKeyProperty.Edit;
Begin
End;

Procedure TGZRootKeyProperty.GetValues(Proc: TGetStrProc);
Var
  i : Integer;
Begin
	Try
    for i:= 0 to 5 do
    begin
      Proc(ListeClefChaine[i]);
    end;
  Except
  	raise;
  End;
End;

Function TGZRootKeyProperty.GetAttributes : TPropertyAttributes;
Begin
	Result:=[paValueList, paSortList];
End;

type
  TROIntProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

function TROIntProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paReadOnly];
end;

type
  TRODateProperty = class(TDateProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

function TRODateProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paReadOnly];
end;

type
  TPackedItemName = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TPackedItemName.GetAttributes: TPropertyAttributes;
begin
  Result:= inherited GetAttributes + [paDialog];
end;

procedure TPackedItemName.Edit;
var Dialog: TOpenDialog;
    F: TFileStream;
  procedure SetUniqueName(Name: string);
  var i: Integer;
      Base: string;
  begin
    try
      SetStrValue(Name);
    except
      Base:= Name; i:= 0;
      while Name <> GetStrValue do
      begin
        Inc(i);
        Name:= Base + IntToStr(i);
        try
          SetStrValue(Name);
        except end;
      end;
    end;
  end;
begin
  Dialog:= TOpenDialog.Create(nil);
  try
    Dialog.Options:= Dialog.Options - [ofHideReadOnly];
    if Dialog.Execute then
    begin
      F:= TFileStream.Create(Dialog.FileName, fmOpenRead or fmShareDenyNone);
      try
        SetUniqueName(ExtractFileName(Dialog.FileName));
        TPackedItem(GetComponent(0)).LoadFromStream(F);
        Modified;
      finally
        F.Free;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

type
  TResStoreEditor = class(TComponentEditor)
  protected
    {$IFDEF GZ_D6}
    procedure GetPropEdit(const Editor: IProperty);
    {$ELSE}
    procedure GetPropEdit(Editor: TPropertyEditor);
    {$ENDIF}
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Idx: Integer): string; override;
    procedure ExecuteVerb(Idx: Integer); override;
    procedure Edit; override;
  end;

function TResStoreEditor.GetVerbCount: Integer;
begin
  Result:= 1;
end;

function TResStoreEditor.GetVerb(Idx: Integer): string;
begin
  Result:= 'Add Files ...';
end;

procedure TResStoreEditor.ExecuteVerb(Idx: Integer);
var Dialog: TOpenDialog;
    F: TFileStream;
    i: Integer;
    Item: TPackedItem;
  procedure SetUniqueName(Name: string);
  var i: Integer;
      Base: string;
  begin
    try
      Item.Name:= Name;
    except
      Base:= Name; i:= 0;
      while Name <> Item.Name do
      begin
        Inc(i);
        Name:= Base + IntToStr(i);
        try
          Item.Name:= Name;
        except end;
      end;
    end;
  end;
begin
  Dialog:= TOpenDialog.Create(nil);
  try
    Dialog.Options:= Dialog.Options - [ofHideReadOnly] + [ofAllowMultiSelect];
    if Dialog.Execute then
    begin
      for i:= 0 to Dialog.Files.Count - 1 do
      begin
        F:= TFileStream.Create(Dialog.Files[i], fmOpenRead or fmShareDenyNone);
        try
          Item:= TPackedItem(TRessourceStore(Component).Items.Add);
          SetUniqueName(ExtractFileName(Dialog.Files[i]));
          Item.LoadFromStream(F);
        finally
          F.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

{$IFDEF GZ_D6}
procedure TResStoreEditor.GetPropEdit(const Editor: IProperty);
begin
  if Editor.GetName = 'Items' then
    Editor.Edit;
end;
{$ELSE}
procedure TResStoreEditor.GetPropEdit(Editor: TPropertyEditor);
begin
  if Editor.GetName = 'Items' then
    Editor.Edit;
end;
{$ENDIF}

{$IFDEF GZ_D6}
procedure TResStoreEditor.Edit;
var L: IDesignerSelections;
begin
  L:= TDesignerSelections.Create;
  L.Add(Component);
  GetComponentProperties(L, [tkClass], Designer, GetPropEdit);
end;
{$ELSE}
  {$IFDEF GZ_D5}
  procedure TResStoreEditor.Edit;
  var L: TDesignerSelectionList;
  begin
    L:= TDesignerSelectionList.Create;
    try
      L.Add(Component);
      GetComponentProperties(L, [tkClass], Designer, GetPropEdit);
    finally
      L.Free;
    end;
  end;
  {$ELSE}
    procedure TResStoreEditor.Edit;
    var L: TComponentList;
    begin
      L:= TComponentList.Create;
      try
        L.Add(Component);
        GetComponentProperties(L, [tkClass], Designer, GetPropEdit);
      finally
        L.Free;
      end;
    end;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

{ HotKeyMgr property editors }

{$IFDEF WIN32}

type
  THotKeyProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function THotKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paMultiSelect];
end;

procedure THotKeyProperty.GetValues(Proc: TGetStrProc);
var i: Integer;
begin
  for i:= 1 to High(KeyLabels) do
    Proc(KeyLabels[i]);
end;

type
  THotKeyManagerEditor = class(TComponentEditor)
  protected
    {$IFDEF GZ_D6}
    procedure GetPropEdit(const Editor: IProperty);
    {$ELSE}
    procedure GetPropEdit(Editor: TPropertyEditor);
    {$ENDIF}
  public
    procedure Edit; override;
  end;

{$IFDEF GZ_D6}
procedure THotKeyManagerEditor.GetPropEdit(const Editor: IProperty);
begin
  if Editor.GetName = 'HotKeys' then
    Editor.Edit;
end;
{$ELSE}
procedure THotKeyManagerEditor.GetPropEdit(Editor: TPropertyEditor);
begin
  if Editor.GetName = 'HotKeys' then
    Editor.Edit;
end;
{$ENDIF}

{$IFDEF GZ_D6}
procedure THotKeyManagerEditor.Edit;
var L: IDesignerSelections;
begin
  L:= TDesignerSelections.Create;
  L.Add(Component);
  GetComponentProperties(L, [tkClass], Designer, GetPropEdit);
end;
{$ELSE}
  {$IFDEF GZ_D5}
  procedure THotKeyManagerEditor.Edit;
  var L: TDesignerSelectionList;
  begin
    L:= TDesignerSelectionList.Create;
    try
      L.Add(Component);
      GetComponentProperties(L, [tkClass], Designer, GetPropEdit);
    finally
      L.Free;
    end;
  end;
  {$ELSE}
  procedure THotKeyManagerEditor.Edit;
  var L: TComponentList;
  begin
    L:= TComponentList.Create;
    try
      L.Add(Component);
      GetComponentProperties(L, [tkClass], Designer, GetPropEdit);
    finally
      L.Free;
    end;
  end;
  {$ENDIF}
{$ENDIF}
{$ENDIF}


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TLogField), TDatasetLog, '', TLogFieldProperty);
  RegisterComponents(SGrizzlyTools, [TFileLog]);
  RegisterComponents(SGrizzlyTools, [TDatasetLog]);
  RegisterComponents(SGrizzlyTools, [TFileErrorLog]);
  RegisterComponents(SGrizzlyTools, [TDatasetErrorLog]);

  RegisterComponents(SGrizzlyTools, [TUseCount]);

  RegisterComponents(SGrizzlyTools, [TDBExportToText]);

  RegisterComponents(SGrizzlyTools, [TConstFormats]);

  RegisterComponents(SGrizzlyTools, [TSharewareManager]);

  RegisterComponents(SGrizzlyTools, [TStringsContainer]);

  RegisterComponents(SGrizzlyTools, [TDatabaseNameDialog]);


  RegisterPropertyEditor(TypeInfo(TGZDBParamField), TDBParamDatabase, 'ParamNameFieldName', TGZDBParamFieldProperty);
  RegisterPropertyEditor(TypeInfo(TGZDBParamField), TDBParamDatabase, 'PathFieldName', TGZDBParamFieldProperty);
  RegisterPropertyEditor(TypeInfo(TGZDBParamField), TDBParamDatabase, 'ValueFieldName', TGZDBParamFieldProperty);
  RegisterComponents(SGrizzlyTools, [TDBParamDatabase]);

  RegisterComponents(SGrizzlyTools, [TMemoEditDialog]);

  RegisterComponents(SGrizzlyTools, [TDBVersionUpdater]);
  RegisterPropertyEditor(TypeInfo(Integer), TDBVersionUpdater, 'Version', TROIntProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), TDBVersionUpdater, 'VersionDate', TRODateProperty);

  {$IFDEF WIN32}
  RegisterPropertyEditor(TypeInfo(HKEY), TRegParamDatabase, 'RootKey', TGZRootKeyProperty);
  RegisterComponents(SGrizzlyTools, [TRegParamDatabase]);

  RegisterComponents(SGrizzlyTools, [TRessourceStore]);

  RegisterPropertyEditor(TypeInfo(Integer), TRessourceStore, 'Size', TROIntProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TPackedItem, 'Size', TROIntProperty);
  RegisterPropertyEditor(TypeInfo(string), TPackedItem, 'Name', TPackedItemName);
  RegisterComponentEditor(TRessourceStore, TResStoreEditor);

  RegisterComponents(SGrizzlyTools, [TSingleInstance]);

  RegisterComponents(SGrizzlyTools, [THotKeyManager]);
  RegisterPropertyEditor(TypeInfo(string), THotKeyItem, 'HotKey', THotKeyProperty);
  RegisterComponentEditor(THotKeyManager, THotKeyManagerEditor);
  {$ENDIF}
end;

end.
