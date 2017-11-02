unit FGGLCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, GZGLMgr;

type
  TGLComboBox = class(TComboBox)
  private
    { Déclarations privées }
    FAllowAdd: Boolean;
    FLastFilling: TDateTime;
    FGenListManager: TCustomGenListManager;
    FGroupName: string;
    FCaseSensitive: Boolean;
    procedure SetGenListManager(Value: TCustomGenListManager);
    procedure SetGroupName(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDateItems; virtual;
  published
    { Déclarations publiées }
    property GenListManager: TCustomGenListManager read FGenListManager write SetGenListManager;
    property GroupName: string read FGroupName write SetGroupName;
    property AllowAdd: Boolean read FAllowAdd write FAllowAdd default True;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  TDBGLComboBox = class(TDBComboBox)
  private
    { Déclarations privées }
    FAllowAdd: Boolean;
    FLastFilling: TDateTime;
    FGenListManager: TCustomGenListManager;
    FGroupName: string;
    FCaseSensitive: Boolean;
    procedure SetGenListManager(Value: TCustomGenListManager);
    procedure SetGroupName(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDateItems; virtual;
  published
    { Déclarations publiées }
    property GenListManager: TCustomGenListManager read FGenListManager write SetGenListManager;
    property GroupName: string read FGroupName write SetGroupName;
    property AllowAdd: Boolean read FAllowAdd write FAllowAdd default True;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  TGLListBox = class(TListBox)
  private
    { Déclarations privées }
    FAllowAdd: Boolean;
    FLastFilling: TDateTime;
    FGenListManager: TCustomGenListManager;
    FGroupName: string;
    procedure SetGenListManager(Value: TCustomGenListManager);
    procedure SetGroupName(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDateItems; virtual;
  published
    { Déclarations publiées }
    property GenListManager: TCustomGenListManager read FGenListManager write SetGenListManager;
    property GroupName: string read FGroupName write SetGroupName;
    property AllowAdd: Boolean read FAllowAdd write FAllowAdd default True;
  end;

  TDBGLListBox = class(TDBListBox)
  private
    { Déclarations privées }
    FAllowAdd: Boolean;
    FLastFilling: TDateTime;
    FGenListManager: TCustomGenListManager;
    FGroupName: string;
    procedure SetGenListManager(Value: TCustomGenListManager);
    procedure SetGroupName(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDateItems; virtual;
  published
    { Déclarations publiées }
    property GenListManager: TCustomGenListManager read FGenListManager write SetGenListManager;
    property GroupName: string read FGroupName write SetGroupName;
    property AllowAdd: Boolean read FAllowAdd write FAllowAdd default True;
  end;

  TGLRadioGroup = class(TRadioGroup)
  private
    { Déclarations privées }
    FAllowAdd: Boolean;
    FLastFilling: TDateTime;
    FGenListManager: TCustomGenListManager;
    FGroupName: string;
    procedure SetGenListManager(Value: TCustomGenListManager);
    procedure SetGroupName(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDateItems; virtual;
  published
    { Déclarations publiées }
    property GenListManager: TCustomGenListManager read FGenListManager write SetGenListManager;
    property GroupName: string read FGroupName write SetGroupName;
    property AllowAdd: Boolean read FAllowAdd write FAllowAdd default True;
  end;

  TDBGLRadioGroup = class(TDBRadioGroup)
  private
    { Déclarations privées }
    FAllowAdd: Boolean;
    FLastFilling: TDateTime;
    FGenListManager: TCustomGenListManager;
    FGroupName: string;
    procedure SetGenListManager(Value: TCustomGenListManager);
    procedure SetGroupName(const Value: string);
  protected
    { Déclarations protégées }
    procedure DoEnter; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDateItems; virtual;
  published
    { Déclarations publiées }
    property GenListManager: TCustomGenListManager read FGenListManager write SetGenListManager;
    property GroupName: string read FGroupName write SetGroupName;
    property AllowAdd: Boolean read FAllowAdd write FAllowAdd default True;
  end;

implementation
              
uses UFGGLMgr;

{ TGLComboBox }

constructor TGLComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastFilling:= 0;
  FAllowAdd:= True;
  FCaseSensitive:= False;
end;

destructor TGLComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TGLComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGenListManager) then
    FGenListManager:= nil;
end;

procedure TGLComboBox.SetGenListManager(Value: TCustomGenListManager);
begin
  if Value <> FGenListManager then
  begin
    FGenListManager:= Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TGLComboBox.UpDateItems;
begin
  if Assigned(FGenListManager) and
    (FLastFilling < FGenListManager.LastUpDate) and
    FGenListManager.FillStrings(Items, FAllowAdd, FGroupName) then
  begin
    FLastFilling:= FGenListManager.LastUpDate;
  end;
end;

procedure TGLComboBox.Loaded;
begin
  inherited Loaded;
  UpDateItems;
end;

procedure TGLComboBox.DoEnter;
begin
  inherited DoEnter;
  UpDateItems;
end;

procedure TGLComboBox.DoExit;
var
  AGroup: TGenGroup;
  OldText: string;
  function ItemExists: Boolean;
  var Idx: Integer;
  begin
    Idx:= Items.IndexOf(Text);
    Result:= Idx  >= 0;
    if CaseSensitive and Result then
    begin
      Result:= False;
      while (not Result) and (Idx < Items.Count) and (CompareText(Items[Idx], Text) = 0) do
      begin
        Result:= Items[Idx] = Text;
        Inc(Idx);
      end;
    end;
  end;
begin
  inherited DoExit;
  if FAllowAdd and Assigned(FGenListManager) and (Style <> csDropDownList) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd
      and (Text <> '') and (not ItemExists) then
    begin
      if MessageDlg(Format(msgUnknownValueQueryAdd, [Text, FGroupName]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        if AGroup.AllowValue then
        begin
          OldText:= FGenListManager.QueryAddItem(FGroupName, Text, '');
        end
        else
        begin
          OldText:= Text;
          FGenListManager.AddItem(FGroupName, Text, '');
        end;
        UpDateItems;
        ItemIndex:= Items.IndexOf(OldText);
      end;
    end;
  end;
end;

procedure TGLComboBox.Change;
var
  AGroup: TGenGroup;
  NewItem: string;
begin
  inherited Change;
  if FAllowAdd and (Text = msgItemNewForAdding) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd then
    begin
      NewItem:= FGenListManager.QueryAddItem(FGroupName, '', '');
      if NewItem <> '' then
      begin
        UpDateItems;
        ItemIndex:= Items.IndexOf(NewItem);
      end
      else
      begin
        ItemIndex:= -1;
      end;
    end;
  end;
end;

procedure TGLComboBox.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    FLastFilling := 0;
    UpDateItems;
  end;
end;

{ TDBGLComboBox }

constructor TDBGLComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastFilling:= 0;
  FAllowAdd:= True;
  FCaseSensitive:= False;
end;

destructor TDBGLComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TDBGLComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGenListManager) then
    FGenListManager:= nil;
end;

procedure TDBGLComboBox.SetGenListManager(Value: TCustomGenListManager);
begin
  if Value <> FGenListManager then
  begin
    FGenListManager:= Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  end;
end;

procedure TDBGLComboBox.UpDateItems;
begin
  if Assigned(FGenListManager) and
    (FLastFilling < FGenListManager.LastUpDate) and
    FGenListManager.FillStrings(Items, FAllowAdd, FGroupName) then
  begin
    FLastFilling:= FGenListManager.LastUpDate;
  end;
end;

procedure TDBGLComboBox.Loaded;
begin
  inherited Loaded;
  UpDateItems;
end;

procedure TDBGLComboBox.DoEnter;
begin
  inherited DoEnter;
  UpDateItems;
end;

procedure TDBGLComboBox.DoExit;
var
  AGroup: TGenGroup;
  OldText: string;
  function ItemExists: Boolean;
  var Idx: Integer;
  begin
    Idx:= Items.IndexOf(Text);
    Result:= Idx  >= 0;
    if CaseSensitive and Result then
    begin
      Result:= False;
      while (not Result) and (Idx < Items.Count) and (CompareText(Items[Idx], Text) = 0) do
      begin
        Result:= Items[Idx] = Text;
        Inc(Idx);
      end;
    end;
  end;
begin
  inherited DoExit;
  if FAllowAdd and Assigned(FGenListManager) and (Style <> csDropDownList) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd
      and (Text <> '') and (not ItemExists) then
    begin
      if MessageDlg(Format(msgUnknownValueQueryAdd, [Text, FGroupName]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        OldText:= Text;
        FGenListManager.AddItem(FGroupName, Text, '');
        UpDateItems;
        ItemIndex:= Items.IndexOf(OldText);
      end;
    end;
  end;
end;

procedure TDBGLComboBox.Change;
var
  AGroup: TGenGroup;
  NewItem: string;
begin
  inherited Change;
  if FAllowAdd and (Text = msgItemNewForAdding) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd then
    begin
      NewItem:= FGenListManager.QueryAddItem(FGroupName, '', '');
      if NewItem <> '' then
      begin
        UpDateItems;
        ItemIndex:= Items.IndexOf(NewItem);
      end
      else
      begin
        ItemIndex:= -1;
      end;
    end;
  end;
end;

procedure TDBGLComboBox.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    FLastFilling := 0;
    UpDateItems;
  end;
end;

{ TGLListBox }

constructor TGLListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastFilling:= 0;
  FAllowAdd:= True;
end;

destructor TGLListBox.Destroy;
begin
  inherited Destroy;
end;

procedure TGLListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGenListManager) then
    FGenListManager:= nil;
end;

procedure TGLListBox.SetGenListManager(Value: TCustomGenListManager);
begin
  if Value <> FGenListManager then
  begin
    FGenListManager:= Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  end;
end;

procedure TGLListBox.UpDateItems;
begin
  if Assigned(FGenListManager) and
    (FLastFilling < FGenListManager.LastUpDate) and
    FGenListManager.FillStrings(Items, FAllowAdd, FGroupName) then
  begin
    FLastFilling:= FGenListManager.LastUpDate;
  end;
end;

procedure TGLListBox.Loaded;
begin
  inherited Loaded;
  UpDateItems;
end;

procedure TGLListBox.DoEnter;
begin
  inherited DoEnter;
  UpDateItems;
end;

procedure TGLListBox.Click;
var
  AGroup: TGenGroup;
  NewItem: string;
begin
  inherited Click;
  if FAllowAdd and (ItemIndex >= 0) and (Items[ItemIndex] = msgItemNewForAdding) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd then
    begin
      NewItem:= FGenListManager.QueryAddItem(FGroupName, '', '');
      if NewItem <> '' then
      begin
        UpDateItems;
        ItemIndex:= Items.IndexOf(NewItem);
      end
      else
      begin
        ItemIndex:= -1;
      end;
    end;
  end;
end;

procedure TGLListBox.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    FLastFilling := 0;
    UpDateItems;
  end;
end;

{ TDBGLListBox }

constructor TDBGLListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastFilling:= 0;
  FAllowAdd:= True;
end;

destructor TDBGLListBox.Destroy;
begin
  inherited Destroy;
end;

procedure TDBGLListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGenListManager) then
    FGenListManager:= nil;
end;

procedure TDBGLListBox.SetGenListManager(Value: TCustomGenListManager);
begin
  if Value <> FGenListManager then
  begin
    FGenListManager:= Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  end;
end;

procedure TDBGLListBox.UpDateItems;
begin
  if Assigned(FGenListManager) and
    (FLastFilling < FGenListManager.LastUpDate) and
    FGenListManager.FillStrings(Items, FAllowAdd, FGroupName) then
  begin
    FLastFilling:= FGenListManager.LastUpDate;
  end;
end;

procedure TDBGLListBox.Loaded;
begin
  inherited Loaded;
  UpDateItems;
end;

procedure TDBGLListBox.DoEnter;
begin
  inherited DoEnter;
  UpDateItems;
end;

procedure TDBGLListBox.Click;
var
  AGroup: TGenGroup;
  NewItem: string;
begin
  inherited Click;
  if FAllowAdd and (ItemIndex >= 0) and (Items[ItemIndex] = msgItemNewForAdding) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd then
    begin
      NewItem:= FGenListManager.QueryAddItem(FGroupName, '', '');
      if NewItem <> '' then
      begin
        UpDateItems;
        ItemIndex:= Items.IndexOf(NewItem);
      end
      else
      begin
        ItemIndex:= -1;
      end;
    end;
  end;
end;

procedure TDBGLListBox.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    FLastFilling := 0;
    UpDateItems;
  end;
end;

{ TGLRadioGroup }

constructor TGLRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastFilling:= 0;
  FAllowAdd:= True;
end;

destructor TGLRadioGroup.Destroy;
begin
  inherited Destroy;
end;

procedure TGLRadioGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGenListManager) then
    FGenListManager:= nil;
end;

procedure TGLRadioGroup.SetGenListManager(Value: TCustomGenListManager);
begin
  if Value <> FGenListManager then
  begin
    FGenListManager:= Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  end;
end;

procedure TGLRadioGroup.UpDateItems;
begin
  if Assigned(FGenListManager) and
    (FLastFilling < FGenListManager.LastUpDate) and
    FGenListManager.FillStrings(Items, FAllowAdd, FGroupName) then
  begin
    FLastFilling:= FGenListManager.LastUpDate;
  end;
end;

procedure TGLRadioGroup.Loaded;
begin
  inherited Loaded;
  UpDateItems;
end;

procedure TGLRadioGroup.DoEnter;
begin
  inherited DoEnter;
  UpDateItems;
end;

procedure TGLRadioGroup.Click;
var
  AGroup: TGenGroup;
  NewItem: string;
begin
  inherited Click;
  if FAllowAdd and (ItemIndex >= 0) and (Items[ItemIndex] = msgItemNewForAdding) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd then
    begin
      NewItem:= FGenListManager.QueryAddItem(FGroupName, '', '');
      if NewItem <> '' then
      begin
        UpDateItems;
        ItemIndex:= Items.IndexOf(NewItem);
      end
      else
      begin
        ItemIndex:= -1;
      end;
    end;
  end;
end;

procedure TGLRadioGroup.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    FLastFilling := 0;
    UpDateItems;
  end;
end;

{ TDBGLRadioGroup }

constructor TDBGLRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastFilling:= 0;
  FAllowAdd:= True;
end;

destructor TDBGLRadioGroup.Destroy;
begin
  inherited Destroy;
end;

procedure TDBGLRadioGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FGenListManager) then
    FGenListManager:= nil;
end;

procedure TDBGLRadioGroup.SetGenListManager(Value: TCustomGenListManager);
begin
  if Value <> FGenListManager then
  begin
    FGenListManager:= Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  end;
end;

procedure TDBGLRadioGroup.UpDateItems;
begin
  if Assigned(FGenListManager) and
    (FLastFilling < FGenListManager.LastUpDate) and
    FGenListManager.FillStrings(Items, FAllowAdd, FGroupName) then
  begin
    FLastFilling:= FGenListManager.LastUpDate;
  end;
end;

procedure TDBGLRadioGroup.Loaded;
begin
  inherited Loaded;
  UpDateItems;
end;

procedure TDBGLRadioGroup.DoEnter;
begin
  inherited DoEnter;
  UpDateItems;
end;

procedure TDBGLRadioGroup.Click;
var
  AGroup: TGenGroup;
  NewItem: string;
begin
  inherited Click;
  if FAllowAdd and (ItemIndex >= 0) and (Items[ItemIndex] = msgItemNewForAdding) then
  begin
    AGroup:= FGenListManager.GroupByName[FGroupName];
    if Assigned(AGroup) and AGroup.AllowAdd then
    begin
      NewItem:= FGenListManager.QueryAddItem(FGroupName, '', '');
      if NewItem <> '' then
      begin
        UpDateItems;
        ItemIndex:= Items.IndexOf(NewItem);
      end
      else
      begin
        ItemIndex:= -1;
      end;
    end;
  end;
end;


procedure TDBGLRadioGroup.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    FLastFilling := 0;
    UpDateItems;
  end;
end;

end.

