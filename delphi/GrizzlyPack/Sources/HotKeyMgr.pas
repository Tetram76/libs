unit HotKeyMgr;

{ Designed by Alexandre Guillien 11/11/1999

  This component allows you to easily use hot keys in your project. These hot keys
  are active even if your application is not active.

  Component use :
    Drop it on your Form/DataModule
    Dbl click on it or Edit the HotKeys property.
    In the editor, select the key and the optional shifts (Shift, Alt or Ctrl)
    Give a name to the hot key (optional).
    Finally, double click on the OnHotKeyActivation event in the object inspector.
    The will create an event where you will be able to make your code.
}

{$I GrizzlyDefine.inc}

interface

uses
  Classes, Windows, Messages, Forms;

type
  TShiftState = set of (ssShift, ssAlt, ssCtrl);

  THotKeyManager = class;

  THotKeyItem = class(TCollectionItem)
  private
    FHotKeyId: Integer;
    FOnHotKey: TNotifyEvent;
    FName: string;
    {}
    FHotKey: Integer;
    FShiftState: TShiftState;
    procedure SetShiftState(State: TShiftState);
    function GetHotKey: string;
    procedure SetHotKey(Key: string);
    function GetManager: THotKeyManager;
  protected
    function GetDisplayName: string; override;
    {}
    procedure RegisterHotKey;
    procedure UnRegisterHotKey;
    {}
    property HotKeyId: Integer read FHotKeyId;
    property Manager: THotKeyManager read GetManager;
  public
    function GetNamePath: string; override;
    {}
    procedure Assign(Item: TPersistent); override;
  published
    property Name: string read FName write FName;
    property ShiftState: TShiftState read FShiftState write SetShiftState;
    property HotKey: string read GetHotKey write SetHotKey;
    property OnHotKeyActivation: TNotifyEvent read FOnHotKey write FOnHotKey;
  end;

  THotKeyCollection = class(TCollection)
  private
    FOwner: THotKeyManager;
    function GetItem(Idx: Integer): THotKeyItem;
    procedure SetItem(Idx: Integer; Item: THotKeyItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Manager: THotKeyManager);
    {}
    property Items[Idx: Integer]: THotKeyItem read GetItem write SetItem; default;
  end;

  THotKeyManager = class(TComponent)
  private
    FHotKeys: THotKeyCollection;
    procedure SetHotKeys(HotKeys: THotKeyCollection);
    {}
    function WMHotKey(var Msg: TMessage): Boolean;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    function HotKeyByName(const Name: string): THotKeyItem;
  published
    property HotKeys: THotKeyCollection read FHotKeys write SetHotKeys;
  end;

const
  KeyCodes: array[0..60] of Integer =
    (0,
     Ord('A'), Ord('B'), Ord('C'), Ord('D'), Ord('E'), Ord('F'), Ord('G'), Ord('H'),
     Ord('I'), Ord('J'), Ord('K'), Ord('L'), Ord('M'), Ord('N'), Ord('O'), Ord('P'),
     Ord('Q'), Ord('R'), Ord('S'), Ord('T'), Ord('U'), Ord('V'), Ord('W'), Ord('X'),
     Ord('Y'), Ord('Z'),
     Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5'), Ord('6'), Ord('7'), Ord('8'),
     Ord('9'), Ord('0'),
     VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10, VK_F11, VK_F12,
     VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ESCAPE, VK_TAB, VK_INSERT, VK_DELETE,
     VK_HOME, VK_END, VK_PRIOR, VK_NEXT);

  KeyLabels: array[0..60] of string =
    ('',
     'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
     'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
     'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
     'Y', 'Z',
     '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
     'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12',
     'Up', 'Down', 'Left', 'Right', 'Echap', 'Tab', 'Insert', 'Delete',
     'Home', 'End', 'Page up', 'Page down');

implementation

uses SysUtils;

function THotKeyManager.WMHotKey(var Msg: TMessage): Boolean;
var i: Integer;
begin
  if Msg.Msg = WM_HOTKEY then
  begin
    for i:= 0 to HotKeys.Count - 1 do
      if Msg.wParam = HotKeys[i].HotKeyId then
      begin
        Result:= True;
        if Assigned(HotKeys[i].OnHotKeyActivation) then
          HotKeys[i].OnHotKeyActivation(HotKeys[i]);
        Exit;
      end;
  end;
  Result:= False;
end;

constructor THotKeyManager.Create(AOwner: TComponent);
begin
  FHotKeys:= THotKeyCollection.Create(Self);
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
    Application.HookMainWindow(WMHotKey);
end;

destructor THotKeyManager.Destroy;
var i: Integer;
begin
  if not (csDesigning in ComponentState) then
    Application.UnHookMainWindow(WMHotKey);
  for i:= 0 to HotKeys.Count - 1 do
    HotKeys[i].UnRegisterHotKey;
  FHotKeys.Free; FHotKeys:= nil;
  inherited Destroy;
end;

procedure THotKeyManager.Loaded;
var i: Integer;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    for i:= 0 to HotKeys.Count - 1 do
      HotKeys[i].RegisterHotKey;
end;

function THotKeyManager.HotKeyByName(const Name: string): THotKeyItem;
var i: Integer;
begin
  for i:= 0 to HotKeys.Count - 1 do
    if CompareText(Name, HotKeys[i].Name) = 0 then
    begin
      Result:= HotKeys[i];
      Exit;
    end;
  Result:= nil;
end;

procedure THotKeyManager.SetHotKeys(HotKeys: THotKeyCollection);
begin
  FHotKeys.Assign(HotKeys);
end;

{ THotKeyCollection }

constructor THotKeyCollection.Create(Manager: THotKeyManager);
begin
  FOwner:= Manager;
  inherited Create(THotKeyItem);
end;

function THotKeyCollection.GetItem(Idx: Integer): THotKeyItem;
begin
  Result:= THotKeyItem(inherited Items[Idx]);
end;

function THotKeyCollection.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure THotKeyCollection.SetItem(Idx: Integer; Item: THotKeyItem);
begin
  inherited Items[Idx]:= Item;
end;

{ THotKeyItem }

procedure THotKeyItem.RegisterHotKey;
var Shift: Integer;
    Key: Integer;
begin
  if FHotKeyId <> 0 then
    UnRegisterHotKey;
  if FHotKey <> 0 then
  begin
    Shift:= 0;
    if ssAlt in ShiftState then
      Shift:= Shift + MOD_ALT;
    if ssCtrl in ShiftState then
      Shift:= Shift + MOD_CONTROL;
    if ssShift in ShiftState then
      Shift:= Shift + MOD_SHIFT;
    Key:= KeyCodes[FHotKey];
    FHotKeyId:= $A000 + ID;
    Windows.RegisterHotKey(Application.Handle, FHotKeyId, Shift, Key);
  end;
end;

procedure THotKeyItem.UnRegisterHotKey;
begin
  if FHotKeyId <> 0 then
  begin
    Windows.UnRegisterHotKey(Application.Handle, FHotKeyId);
    FHotKeyId:= 0;
  end;
end;

procedure THotKeyItem.Assign(Item: TPersistent);
begin
  if Item is THotKeyItem then
  begin
    HotKey:= THotKeyItem(Item).HotKey;
    ShiftState:= THotKeyItem(Item).ShiftState;
  end else
    inherited Assign(Item);
end;

function THotKeyItem.GetDisplayName: string;
  procedure AddShift(const S: string);
  begin
    if Result <> '' then
      Result:= Result + ' + ';
    Result:= Result + S;
  end;
begin
  if HotKey = '' then
    Result:= inherited GetDisplayName
  else if Name <> '' then
    Result:= Name
  else begin
    if ssAlt in ShiftState then
      AddShift('Alt');
    if ssCtrl in ShiftState then
      AddShift('Ctrl');
    if ssShift in ShiftState then
      AddShift('Shift');
    AddShift(HotKey);
  end;
end;

function THotKeyItem.GetNamePath: string;
begin
  if Collection <> nil then
  begin
    if Name <> '' then
      Result:= Format('%s[%s]',[Collection.GetNamePath, Name])
    else
      Result:= Format('%s[%d]',[Collection.GetNamePath, Index]);
  end else
    Result:= ClassName;
end;

function THotKeyItem.GetHotKey: string;
begin
  Result:= KeyLabels[FHotKey];
end;

procedure THotKeyItem.SetHotKey(Key: string);
var i: Integer;
begin
  i:= 0;
  while i < High(KeyLabels) do
  begin
    Inc(i);
    if CompareText(Key, KeyLabels[i]) = 0 then
      Break;
  end;
  if i > High(KeyLabels) then
    i:= 0;
  FHotKey:= i;
  if not (csDesigning in Manager.ComponentState) then
    RegisterHotKey;
end;

procedure THotKeyItem.SetShiftState(State: TShiftState);
begin
  FShiftState:= State;
  if not (csDesigning in Manager.ComponentState) then
    RegisterHotKey;
end;

function THotKeyItem.GetManager: THotKeyManager;
begin
  Result:= THotKeyCollection(Collection).FOwner;
end;

end.
