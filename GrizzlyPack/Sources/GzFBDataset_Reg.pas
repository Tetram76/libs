unit GzFBDataset_Reg;

interface

{$I GrizzlyDefine.INC}

uses
  SysUtils, TypInfo, Classes, Contnrs, DB, Controls,
  GzFBDataset, GzFBFields, GzFBDatasetEditor, GZDesign,
  {$IFDEF GZ_D6}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF};

procedure Register;

implementation

type
  TGzFBDatasetEditor = class(TComponentEditor)
  public
    {$IFNDEF GZ_D6}
    DefaultEditor: TComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TFormDesigner); override;
    {$ELSE}
    DefaultEditor: IComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    {$ENDIF}
  	function GetVerbCount : Integer; override;
    function GetVerb(Index : Integer) : string; override;
    procedure ExecuteVerb(Index : Integer); override;
  end;

function TGzFBDatasetEditor.GetVerbCount : Integer;
begin
	Result := DefaultEditor.GetVerbCount + 1;
end;

function TGzFBDatasetEditor.GetVerb(Index : Integer) : string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
	if Index - DefaultEditor.GetVerbCount = 0 then
  	Result:='GzFBDataSet Editor'
end;

procedure TGzFBDatasetEditor.ExecuteVerb(Index : Integer);
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
  	DlgFBDatasetEditor := TDlgFBDatasetEditor.Create(nil);
    try
	  	if DlgFBDatasetEditor.Execute(Component as TGzFBDataSet) then
      begin
        DlgFBDatasetEditor.Apply;
        Designer.Modified;
      end;
    finally
    	DlgFBDatasetEditor.Free;
    end;
	end;
end;

procedure Register;
begin
  { Composants }
  RegisterComponents('UIB', [TGzFBCachedDataSet, TGzFBDataSet]);

  { Editeurs }
  RegisterComponentEditor(TGzFBDataset, TGzFBDatasetEditor);

  { Champs }
  RegisterFields([TFBStringField]);

  { Propriétés }
  RegisterPropertiesInCategory('Transaction',TGzFBCachedDataSet,['OnClose', 'OnError']);
  RegisterPropertiesInCategory('Transaction',TGzFBDataSet,['OnClose', 'OnError', 'OnExec']);
end;


type
  PClass = ^TClass;

{$IFNDEF GZ_D6}
constructor TGzFBDatasetEditor.Create(AComponent: TComponent; ADesigner: TFormDesigner); override;
{$ELSE}
constructor TGzFBDatasetEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
{$ENDIF}
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

end.
