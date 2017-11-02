unit GzGLMgrDataset;

interface

uses SysUtils, Classes, GzGLMgr, DB;

type
  TDatasetGenListManager = class(TCustomGenListManager)
  private
    FGroupDataset: TDataset;
    FItemDataset: TDataset;
    procedure SetGroupDataset(const Value: TDataset);
    procedure SetItemDataset(const Value: TDataset);
  protected
    function CreateGroupDataset(AReadOnly: Boolean): TDataSet; override;
    function CreateItemDataset(AReadOnly: Boolean): TDataSet; override;
    procedure FreeGroupDataset(ADataset : TDataset); override;
    procedure FreeItemDataset(ADataset : TDataset); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanInitializeGL : Boolean; override;
  public
    { Déclarations publiques }
    procedure Edit; override;
  published
    property GroupDataset : TDataset read FGroupDataset write SetGroupDataset;
    property ItemDataset : TDataset read FItemDataset write SetItemDataset;
  end;

implementation

uses FGLDatasetEd;

{ TDatasetGenListManager }

function TDatasetGenListManager.CanInitializeGL: Boolean;
begin
  Result := Assigned(FGroupDataset) and Assigned(FItemDataset); 
end;

function TDatasetGenListManager.CreateGroupDataset(AReadOnly: Boolean): TDataSet;
begin
  Result := FGroupDataset;
end;

function TDatasetGenListManager.CreateItemDataset(AReadOnly: Boolean): TDataSet;
begin
  Result := FItemDataset;
end;

procedure TDatasetGenListManager.Edit;
var
  ADlg : TFenGLMgrDatasetEdit;
begin
  try
    ADlg:=TFenGLMgrDatasetEdit.Create(nil);
    try
      ADlg.HelpContext:= HelpContextEditGL;
      ADlg.GroupDataset := GroupDataset;
      ADlg.ItemDataset := ItemDataset;
      ADlg.GLMgr:= Self;
      ADlg.ShowModal;
      Update;
    finally
      ADlg.Free;
    end;
  finally
    Update;
  end;
end;

procedure TDatasetGenListManager.FreeGroupDataset(ADataset: TDataset);
begin
  inherited;
end;

procedure TDatasetGenListManager.FreeItemDataset(ADataset: TDataset);
begin
  inherited;
end;

procedure TDatasetGenListManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Assigned(AComponent) and (Operation = opRemove) then
  begin
    if AComponent = FGroupDataset then
      FGroupDataset := nil
    else
    if AComponent = FItemDataset then
      FItemDataset := nil
  end;
end;

procedure TDatasetGenListManager.SetGroupDataset(const Value: TDataset);
begin
  FGroupDataset := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TDatasetGenListManager.SetItemDataset(const Value: TDataset);
begin
  FItemDataset := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

end.
