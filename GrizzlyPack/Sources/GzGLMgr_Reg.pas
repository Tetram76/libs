unit GzGLMgr_Reg;

{$I GrizzlyDefine.inc}

interface

procedure Register;

implementation

uses
  SysUtils, Classes, Controls, GzConsts, DB, DBTables, Dialogs,
  GZGLMgr, FGGLMgr, GzGLMgrDataset, FGGLCtrl,
  FFGGLApp, FFGGLDTSApp
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors, TypInfo
  {$ELSE}
  , DsgnIntf, TypInfo
  {$ENDIF};

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


{ TGenListDatabaseNameProperty }

type
  TGenListDatabaseNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TGenListDatabaseNameProperty.GetValueList(List: TStrings);
begin
  Session.GetDatabaseNames(List);
end;


{ TGenListTableNameProperty }

type
  TGenListTableNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TGenListTableNameProperty.GetValueList(List: TStrings);
Var
  AGLM : TGenListManager;
begin
  AGLM:=TGenListManager(GetComponent(0));
  If AGLM<>Nil then
  Begin
    Try
      Session.GetTableNames(AGLM.DatabaseName,'*.*',False,False,List);
    Except
      On E : Exception do
        ShowMessage(E.Message);
    End;
  End;
end;


{ TFGGLMgrComponentEditor }

Type
	TFGGLMgrComponentEditor = Class(TComponentEditor)
  Private
  Public
  	Function GetVerbCount : Integer; override;
    Function GetVerb(Index : Integer) : String; Override;
    Procedure ExecuteVerb(Index : Integer); Override;
    Procedure ExecuteOnClick;
    {Procedure Edit; Override;}
  End;

Function TFGGLMgrComponentEditor.GetVerbCount : Integer;
Begin
	Result:=1;
End;

Function TFGGLMgrComponentEditor.GetVerb(Index : Integer) : String;
Begin
	If Index=0 then
  	result:='Editeur de liste génériques'
End;

Procedure TFGGLMgrComponentEditor.ExecuteVerb(Index : Integer);
Begin
	If Index=0 then
  Begin
		ExecuteOnClick;
	End;
End;

Procedure TFGGLMgrComponentEditor.ExecuteOnClick;
Var
  ADlg : TGLFenAppGenericList;
  AGLM : TGenListManager;
begin
  ADlg:=TGLFenAppGenericList.Create(nil);
  Try
    AGLM:=TGenListManager(Component);
    With AGLM do
      ADlg.OuvrirAppli(DatabaseName, GroupTableName, ItemTableName, GroupFields, ItemFields);
    ADlg.ShowModal;
  Finally
    ADlg.Free;
  End;
End;

{ TGroupNameProperty }

type
  TGroupNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TGroupNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure TGroupNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  ACompo: TPersistent;
  AGLMgr: TCustomGenListManager;
  ALst: TStringList;
  {Une façon bien commode de trouver une propriété
  (en l'occurence GenListManager) sans connaître
  le type exact de l'objet...}
  function GetGLMgr: TCustomGenListManager;
  var
    I: Integer;
    Props: PPropList;
    TypeData: PTypeData;
  begin
    Result:= nil;
    if (ACompo = nil) or (ACompo.ClassInfo = nil) then Exit;
    TypeData:= GetTypeData(ACompo.ClassInfo);
    if (TypeData = nil) or (TypeData^.PropCount = 0) then Exit;
    GetMem(Props, TypeData^.PropCount * sizeof(Pointer));
    try
      GetPropInfos(ACompo.ClassInfo, Props);
      for I:= 0 to TypeData^.PropCount - 1 do
      begin
        with Props^[I]^ do
          if (PropType^.Kind = tkClass) and
            (TObject(GetOrdProp(ACompo, Props^[I])) is TCustomGenListManager) then
          begin
            Result:= TCustomGenListManager(GetOrdProp(ACompo, Props^[I]));
            Exit;
          end;
      end;
    finally
      Freemem(Props);
    end;
  end;
begin
  ACompo:= GetComponent(0);
  AGLMgr:= GetGLMgr;
  if not Assigned(AGLMgr) then
    Exit;
  ALst:= TStringList.Create;
  try
    ALst.Sorted:= True;
    AGLMgr.Update;
    AGLMgr.GetGroupList(ALst);
    for i:= 0 to ALst.Count - 1 do
      Proc(ALst[i]);
  finally
    ALst.Free;
  end;
end;


{ TDatasetGLMgrComponentEditor }

Type
	TGLMgrComponentEditor = Class(TComponentEditor)
  Private
  Public
  	Function GetVerbCount : Integer; override;
    Function GetVerb(Index : Integer) : String; Override;
    Procedure ExecuteVerb(Index : Integer); Override;
    Procedure ExecuteOnClick;
    {Procedure Edit; Override;}
  End;

Function TGLMgrComponentEditor.GetVerbCount : Integer;
Begin
	Result := 1;
End;

Function TGLMgrComponentEditor.GetVerb(Index : Integer) : String;
Begin
	If Index = 0 then
  	Result := 'Edition des listes...'
End;

Procedure TGLMgrComponentEditor.ExecuteVerb(Index : Integer);
Begin
	If Index = 0 then
		ExecuteOnClick;
End;

Procedure TGLMgrComponentEditor.ExecuteOnClick;
Var
  ABDEDlg : TGLFenAppGenericList;
  AGLDB : TWithDatabaseParamsGenListManager;
  AdtsDlg : TGLDTSFenAppGenericList;
  AGLdts : TDatasetGenListManager;
begin
  if Component is TWithDatabaseParamsGenListManager then
  begin
    ABDEDlg:=TGLFenAppGenericList.Create(nil);
    try
      AGLDB:=TWithDatabaseParamsGenListManager(Component);
      with AGLDB do
        ABDEDlg.OuvrirAppli(DatabaseName, GroupTableName, ItemTableName, GroupFields, ItemFields);
      ABDEDlg.ShowModal;
    finally
      ABDEDlg.Free;
    end;
  end;
  if Component is TDatasetGenListManager then
  begin
    AGLdts := TDatasetGenListManager(Component);
    if (not Assigned(AGLdts.GroupDataset)) or (not Assigned(AGLdts.ItemDataset)) then
    begin
      MessageDlg('Veuillez définir GroupDataset et ItemDataset', mtError, [mbOk], 0);
      Exit;
    end;
    AdtsDlg:=TGLDTSFenAppGenericList.Create(nil);
    try
      with AGLDts do
        AdtsDlg.Init(GroupDataset, ItemDataset);
      AdtsDlg.ShowModal;
    finally
      AdtsDlg.Free;
    end;
  end;
End;


procedure Register;
begin
	RegisterComponentEditor(TCustomGenListManager, TGLMgrComponentEditor);

  RegisterPropertyEditor(TypeInfo(string), TGenListManager, 'DatabaseName', TGenListDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TGenListManager, 'ItemTableName', TGenListTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TGenListManager, 'GroupTableName', TGenListTableNameProperty);

  RegisterComponents(SGrizzlyGL, [TGenListManager, TDatasetGenListManager]);

  RegisterPropertyEditor(TypeInfo(string), TWinControl, 'GroupName', TGroupNameProperty);

  RegisterComponents(SGrizzlyGL, [TGLComboBox]);
  RegisterComponents(SGrizzlyGL, [TDBGLComboBox]);
  RegisterComponents(SGrizzlyGL, [TGLListBox]);
  RegisterComponents(SGrizzlyGL, [TDBGLListBox]);
  RegisterComponents(SGrizzlyGL, [TGLRadioGroup]);
  RegisterComponents(SGrizzlyGL, [TDBGLRadioGroup]);

end;

end.
