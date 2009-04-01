unit LinkControls;

{$D-}

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics;

type
  TControlItem = class(TCollectionItem)
  private
    FOldColor: TColor;
    FControl: TControl;
    procedure SetControl(const Value: TControl);
    type TCrackControl = class(TControl);
  public
    procedure DoEnter;
    procedure DoExit;
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  protected
    function GetDisplayName: string; override;
  published
    property Control: TControl read FControl write SetControl;
  end;

  TControlList = class(TCollection)
  private
    function GetItem(Index: Integer): TControlItem;
    procedure SetItem(Index: Integer; const Value: TControlItem);
  public
    constructor Create;
    procedure DoEnter;
    procedure DoExit;
    function Add(Control: TControl): TControlItem; overload;
    function Add: TControlItem; overload;
    property Items[Index: Integer]: TControlItem read GetItem write SetItem; default;
  end;

implementation

{ TControlItem }

procedure TControlItem.Assign(Source: TPersistent);
begin
  if Source is TControlItem then
  begin
    Control := TControlItem(Source).Control;
    Exit;
  end;
  inherited Assign(Source);
end;

constructor TControlItem.Create(Collection: TCollection);
begin
  inherited;
  FControl := nil;
end;

procedure TControlItem.DoEnter;
begin
  if Assigned(FControl) then
  begin
    FOldColor := TCrackControl(FControl).Font.Color;
    TCrackControl(FControl).Font.Color := $00DD22BB;
  end;
end;

procedure TControlItem.DoExit;
begin
  if Assigned(FControl) then
    TCrackControl(FControl).Font.Color := FOldColor;
end;

function TControlItem.GetDisplayName: string;
begin
  if Assigned(FControl) then
    Result := Format('%s (%s)', [FControl.Name, FControl.ClassName])
  else
    Result := ClassName;
end;

procedure TControlItem.SetControl(const Value: TControl);
var
  i: Integer;
begin
  if Assigned(Value) then
    for i := 0 to Pred(Collection.Count) do
      if TControlItem(Collection.Items[i]).Control = Value then
        raise Exception.Create('Ce control fait déjà parti de la liste');
  FControl := Value;
end;

{ TControlList }

function TControlList.Add(Control: TControl): TControlItem;
begin
  Result := Add;
  Result.Control := Control;
end;

function TControlList.Add: TControlItem;
begin
  Result := TControlItem(inherited Add);
end;

constructor TControlList.Create;
begin
  inherited Create(TControlItem);
end;

procedure TControlList.DoEnter;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    Items[i].DoEnter;
end;

procedure TControlList.DoExit;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    Items[i].DoExit;
end;

function TControlList.GetItem(Index: Integer): TControlItem;
begin
  Result := TControlItem(inherited GetItem(Index));
end;

procedure TControlList.SetItem(Index: Integer; const Value: TControlItem);
begin
  inherited SetItem(Index, Value);
end;

end.
