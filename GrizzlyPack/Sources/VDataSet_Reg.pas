unit VDataSet_Reg;

interface

{$I GrizzlyDefine.INC}

uses
  TypInfo, Classes, Contnrs, DB, EvalDB, EvalIntf, VDataset, GZDesign,
  {$IFDEF GZ_D6}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF};

{ Property Editors }

type
  TDBSymbolProperty = class(TFieldProperty)
  public
    function GetDataSet: TDataSet; override;
  end;

type
  TDBEvaluatorEditor = class(TComponentEditor)
  private
    {$IFDEF GZ_D6}
    FEditor: IProperty;
    {$ELSE}
    FEditor: TPropertyEditor;
    {$ENDIF}
  protected
    {$IFDEF GZ_D6}
    procedure GetEditor(const Editor: IProperty);
    {$ELSE}
    procedure GetEditor(Editor: TPropertyEditor);
    {$ENDIF}
  public
    procedure Edit; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  { Composants }
  RegisterComponents('VDataset', [TEvaluator,TDBEvaluator,TVirtualDataSet]);

  { Editeurs }
  RegisterPropertyEditor(TypeInfo(string), TDBSymbolItem, 'FieldName', TDBSymbolProperty);
  RegisterComponentEditor(TDBEvaluator, TDBEvaluatorEditor);
  RegisterFields([TExpressionField]);
end;

{ Property Editors }

function TDBSymbolProperty.GetDataSet: TDataSet;
begin
  if GetComponent(0) is TDBSymbolItem then
    Result:= TDBSymbolItem(GetComponent(0)).DataSet
  else Result:= nil;
end;

{$IFDEF GZ_D6}
procedure TDBEvaluatorEditor.GetEditor(const Editor: IProperty);
begin
  if Editor.GetName = 'DBSymbols' then
    FEditor:= Editor;
end;

procedure TDBEvaluatorEditor.Edit;
var L: IDesignerSelections;
begin
  L:= TDesignerSelections.Create;
  L.Add(Component);
  GetComponentProperties(L, [tkClass], Designer, GetEditor);
  FEditor.Edit;
end;

{$ELSE}

procedure TDBEvaluatorEditor.GetEditor(Editor: TPropertyEditor);
begin
  if Editor.GetName = 'DBSymbols' then
    FEditor:= Editor;
end;

procedure TDBEvaluatorEditor.Edit;
var L: TDesignerSelectionList;
begin
  L:= TDesignerSelectionList.Create;
  try
    L.Add(Component);
    GetComponentProperties(L, [tkClass], Designer, GetEditor);
    FEditor.Edit;
  finally
    L.Free;
  end;
end;

{$ENDIF}

end.
