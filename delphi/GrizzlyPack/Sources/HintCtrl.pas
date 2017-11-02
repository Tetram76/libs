unit HintCtrl;

{ These Hint Controls have been designed by Alexandre GUILLIEN
     They all show a Hint to complete a clipped Text in a control. The most
     useful are the THListBox and the THDBGrid.

  E-Mail = aguillien@mageos.com

History :

20/12/98
OnShowHint property Added

12/02/99
Thanks to Stas Antonov, the THDBGrid supports the ObjectView property
}

{$I GrizzlyDefine.inc}

interface

uses WinTypes, WinProcs, SysUtils, Classes, Controls, StdCtrls, Messages,
  Graphics, Forms, DBCtrls, Grids, DBGrids, DB;

type
  TListHintEvent = procedure(Index : Integer; var ShowHint : Boolean; var Text : string) of object;

  THListBox = class(TListBox)
  private
    FSpHint : Boolean;
    FHintedItem : Integer;
    FOnShowHint : TListHintEvent;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
     { CurItem Managing }
    function CheckCurItem : Integer;
    function GetCurStr : string;
    procedure SetCurStr(NewStr : string);
    function GetCurObj : TObject;
    procedure SetCurObj(NewObj : TObject);
  protected
     { SpHint }
    function HintProc(CurPos : TPoint) : Boolean; virtual;
    function Clipped(Text : string) : Boolean;
    function ItemTopLeft(Idx : Integer) : TPoint;
    function SpItemHeight : Integer;
     { Events }
    procedure DoExit; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
     { Usefull methods/properties with LB }
    procedure DeleteCurIndex;
    property CurString : string read GetCurStr write SetCurStr;
    property CurObject : TObject read GetCurObj write SetCurObj;
  published
    property SpHint : Boolean read FSpHint write FSpHint;
    property OnShowHint : TListHintEvent read FOnShowHint write FOnShowHint;
  end;

  THintEvent = procedure(var ShowHint : Boolean; var Text : string) of object;

  THEdit = class(TEdit)
  private
    FSpHint : Boolean;
    FOnShowHint : THintEvent;
    procedure DoMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    function HintProc(CurPos : TPoint) : Boolean; virtual;
    function Clipped(Text : string) : Boolean;
     { Events }
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property SpHint : Boolean read FSpHint write FSpHint;
    property OnShowHint : THintEvent read FOnShowHint write FOnShowHint;
     { Just because I use it }
    property Align;
  end;

  THDBEdit = class(TDBEdit)
  private
    FSpHint : Boolean;
    FOnShowHint : THintEvent;
    procedure DoMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    function HintProc(CurPos : TPoint) : Boolean; virtual;
    function Clipped(Text : string) : Boolean;
     { Events }
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property SpHint : Boolean read FSpHint write FSpHint;
    property OnShowHint : THintEvent read FOnShowHint write FOnShowHint;
    property Align;
  end;

  THLabel = class(TLabel)
  private
    FSpHint : Boolean;
    FOnShowHint : THintEvent;
    procedure DoMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    function HintProc(CurPos : TPoint) : Boolean; virtual;
    function Clipped(Text : string) : Boolean;
     { Events }
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property SpHint : Boolean read FSpHint write FSpHint;
    property OnShowHint : THintEvent read FOnShowHint write FOnShowHint;
  end;

  THDBText = class(TDBText)
  private
    FSpHint : Boolean;
    FOnShowHint : THintEvent;
    procedure DoMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    function HintProc(CurPos : TPoint) : Boolean; virtual;
    function Clipped(Text : string) : Boolean;
     { Events }
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property SpHint : Boolean read FSpHint write FSpHint;
    property OnShowHint : THintEvent read FOnShowHint write FOnShowHint;
  end;

  TGridHintEvent = procedure(ACol, ARow : LongInt; var Show : Boolean; var Text : string) of object;
  TGridFontEvent = procedure(Sender: TObject; Font : TFont) of object;

  THDBGrid = class(TDBGrid)
  private
    FAutoSizeLastColumn : Boolean;
    FAutoResizing : Boolean;
    FSpHint : Boolean;
    FHintedItem : TGridCoord;
    FHintEvent : TGridHintEvent;
    FOnGetFont : TGridFontEvent;
    FOnSelectCell : TSelectCellEvent;
     {}
    procedure SetAutoSizeLastColumn(Value : Boolean);
    function RectAtPos(Pos : TPoint) : TRect;
{$IFDEF VER120}function CalcD4GridTitleOffset : Integer;
{$ENDIF}
    function ValueAtPos(Pos : TPoint) : string;
    function FontAtPos(Pos : TPoint) : TFont;
    procedure DoMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    function HintProc(CurPos : TPoint) : Boolean; virtual;
    function Clipped(Text : string; CurPos : TPoint) : Boolean;
     { Events }
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Click; override;
    procedure DoExit; override;
    function SelectCell(ACol, ARow : LongInt) : Boolean; override;
    procedure ColWidthsChanged; override;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure DoAutoSize;
    procedure Loaded; override;
  public
     //procedure Invalidate; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property SpHint : Boolean read FSpHint write FSpHint;
    property OnShowHint : TGridHintEvent read FHintEvent write FHintEvent;
    property OnGetFont : TGridFontEvent read FOnGetFont write FOnGetFont;
    property OnSelectCell : TSelectCellEvent read FOnSelectCell write FOnSelectCell;
    property OnMouseDown;
    property OnMouseUp;
    property AutoSizeLastColumn : Boolean read FAutoSizeLastColumn write SetAutoSizeLastColumn default False;
  end;

{$IFNDEF WIN32}
procedure FreeAll;
{$ENDIF}

{ Design note : The following var is the delay required to show the Hint }
var
  BaseShowHintDelay : Integer;

implementation

uses Dialogs, ExtCtrls;

type
  THintProc = function(CurPos : TPoint) : Boolean of object;
  PHintProc = ^THintProc;

  THintProcList = class(TList)
  private
    function GetItem(Index : Integer) : THintProc;
    procedure SetItem(Index : Integer; Item : THintProc);
  public
    procedure Add(Item : THintProc);
    procedure DeleteItem(Index : Integer);
    destructor Destroy; override;
    property Items[Idx : Integer] : THintProc read GetItem write SetItem;
  end;

{$IFNDEF WIN32}
{ Design Notes :
     The only way I found to make it work under D1 was to recreate a new
     HintWindow from the D2 Source Code ........ The following TzHintWindow
     is this new THintWindow. If someone has a better idea !
     }
  TzHintWindow = class(TCustomControl)
  private
    procedure WMNCHitTest(var Message : TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message : TMessage); message WM_NCPAINT;
    procedure CMTextChanged(var Message : TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params : TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure ActivateHint(Rect : TRect; const AHint : string); virtual;
    procedure ActivateHintData(Rect : TRect; const AHint : string; AData : Pointer); virtual;
    function CalcHintRect(MaxWidth : Integer; const AHint : string;
      AData : Pointer) : TRect; virtual;
    function IsHintMsg(var Msg : TMsg) : Boolean; virtual;
    procedure ReleaseHandle;
    property Caption;
    property Color;
    property Canvas;
  end;
{$ENDIF}

{$IFNDEF WIN32}
  THintController = class(TzHintWindow)
{$ELSE}
  THintController = class(THintWindow)
{$ENDIF}
  private
{$IFDEF WIN32}
    procedure WMNCPaint(var Message : TMessage); message WM_NCPAINT;
{$ENDIF}
    procedure LeftMouseDown(var Msg : TMessage); message WM_MOUSEMOVE;
  protected
    Controls : TList;
    HintProcs : THintProcList;
    Timer : TTimer;
    Counter : Integer;
    procedure AddControl(Control : TControl; HintProc : THintProc);
    procedure RemoveControl(Control : TControl);
    procedure DoTimer(Sender : TObject);
    procedure DisplayHint(Control : TControl);
{$IFDEF WIN32}
    procedure Paint; override;
{$ENDIF}
  public
    Captive : TControl;
    CurStatus : Integer;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ResetCaptive;
    procedure CheckMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
  end;

var
  Controller : THintController;

{ THintProcList }

procedure THintProcList.Add(Item : THintProc);
begin
  inherited Add(nil);
  SetItem(Count - 1, Item);
end;

procedure THintProcList.DeleteItem(Index : Integer);
begin
  Dispose(PHintProc(inherited Items[Index]));
  Delete(Index);
end;

function THintProcList.GetItem(Index : Integer) : THintProc;
begin
  Result:= PHintProc(inherited Items[Index])^;
end;

procedure THintProcList.SetItem(Index : Integer; Item : THintProc);
var
  P : PHintProc;
begin
  New(P);
  try
    P^:= Item;
    inherited Items[Index]:= P;
  except
    Dispose(P);
    raise;
  end;
end;

destructor THintProcList.Destroy;
var
  i : Integer;
begin
  for i:= 0 to Count - 1 do
    Dispose(PHintProc(inherited Items[i]));
end;

{ THintController }

procedure THintController.DoTimer(Sender : TObject);
var
  Point : TPoint;
begin
  GetCursorPos(Point);
  Point:= Captive.ScreenToClient(Point);
  { If it is not showing and it is time to show, we show }
  if (CurStatus = 1) and (Counter > 0) then
    DisplayHint(Captive)
  { If it is not showing and it is not time to show, we Inc(Counter) }
  else
    if CurStatus = 1 then
      Inc(Counter);
end;

procedure THintController.DisplayHint(Control : TControl);
var
  Point : TPoint;
  Proc : THintProc;
begin
  Counter:= 0;
  Proc:= HintProcs.Items[Controls.IndexOf(Control)];
  GetCursorPos(Point);
  { Check if we must go to Mode 2 }
  if Proc(Point) then
    CurStatus:= 2
  else { If not hint required, we reset }
    ResetCaptive;
end;

procedure THintController.ResetCaptive;
begin
  Timer.Enabled:= False;
  Timer.Interval:= BaseShowHintDelay;
  Counter:= 0;
  if CurStatus = 2 then ReleaseHandle;
  Captive:= nil;
  CurStatus:= 0;
end;

procedure THintController.CheckMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
begin
  if Captive <> Sender then ResetCaptive; { I doubt it can happen with use of CMMouseLeave }
  if Captive = nil then
  begin
    Captive:= TControl(Sender);
    CurStatus:= 1;
    Timer.Enabled:= True;
  end;
end;

procedure THintController.AddControl(Control : TControl; HintProc : THintProc);
begin
  Controls.Add(Control);
  HintProcs.Add(HintProc);
end;

procedure THintController.RemoveControl(Control : TControl);
var
  Idx : Integer;
begin
  if Control = Captive then ResetCaptive;
  Idx:= Controls.IndexOf(Control);
  if Idx <> -1 then
  begin
    Controls.Delete(Idx);
    HintProcs.DeleteItem(Idx);
  end;
end;

procedure THintController.LeftMouseDown(var Msg : TMessage);
begin
  ResetCaptive;
  inherited;
end;

constructor THintController.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
{$IFDEF WIN32}Color:= clInfoBk;
{$ENDIF}
  Controls:= TList.Create;
  HintProcs:= THintProcList.Create;
  Timer:= TTimer.Create(Self);
  ResetCaptive;
  Timer.OnTimer:= DoTimer;
end;

destructor THintController.Destroy;
begin
  Timer.Free;
  HintProcs.Free;
  Controls.Free;
  inherited Destroy;
end;

{$IFDEF WIN32}
  { Note : these repaint functions allow the window to be more beautiful :
        a full black rectangle instead of an half white/half black one }

procedure THintController.WMNCPaint(var Message : TMessage);
var
  R : TRect;
begin
  Canvas.Handle:= GetWindowDC(Handle);
  with Canvas do
  try
    R:= Rect(0, 0, Width, Height);
    WinProcs.Rectangle(Canvas.Handle, 0, 0, Width, Height);
  finally
    Canvas.Handle:= 0;
  end;
end;

procedure THintController.Paint;
var
  R : TRect;
  C : PChar;
begin
  R:= ClientRect;
  Inc(R.Left, 1);
  Inc(R.Top, 1);
  Dec(R.Right, 1);
  Dec(R.Bottom, 1);
  Canvas.Font.Color:= clInfoText;
  GetMem(C, Length(Caption) + 1);
  DrawText(Canvas.Handle, StrPCopy(C, Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK);
  FreeMem(C, Length(Caption) + 1);
end;
{$ENDIF}

{ THListBox }

function THListBox.SpItemHeight : Integer;
begin
  if Style = lbOwnerDrawVariable then
    MeasureItem(0, Result)
  else
    Result:= ItemHeight;
end;

function THListBox.HintProc(CurPos : TPoint) : Boolean;
var
  Idx : Integer;
  Rect : TRect;
  Text : string;
begin
  Result:= False;
  CurPos:= ScreenToClient(CurPos);
  Idx:= ItemAtPos(CurPos, True);
  if Idx <> -1 then
  begin
     { Check if must show the thing & if yes, WE show it ... }
    Text:= Items[Idx];
    Result:= Clipped(Text);
    if Assigned(FOnShowHint) then FOnShowHint(Idx, Result, Text);
    if Result then
    begin
      Controller.Canvas.Font.Assign(Font);
      CurPos:= ItemTopLeft(Idx);
      with Rect do
      begin
        TopLeft:= ClientToScreen(CurPos);
        Right:= CurPos.X + Controller.Canvas.TextWidth(Text) + 5;
        Bottom:= CurPos.Y + SpItemHeight;
        BottomRight:= ClientToScreen(BottomRight);
      end;
      Controller.ActivateHint(Rect, Text);
      FHintedItem:= Idx;
    end;
  end;
end;

function THListBox.ItemTopLeft(Idx : Integer) : TPoint;
begin
  Idx:= Idx - TopIndex;
  Result.X:= 0;
  Result.Y:= Idx * SpItemHeight - 2;
end;

function THListBox.Clipped(Text : string) : Boolean;
var
  Modifier : Integer;
begin
  Canvas.Font.Assign(Font);
{$IFDEF WIN32}
  if Ctl3D then
    Modifier:= 4
  else
    Modifier:= 2;
  if (Items.Count * SpItemHeight) > (Height - Modifier) then
    Modifier:= GetSystemMetrics(SM_CXVSCROLL) + 2; { = Width of the ScrollBar }
{$ELSE}
  Modifier:= 0;
  if (Items.Count * SpItemHeight) > (Height - 4) then
    Modifier:= GetSystemMetrics(SM_CXVSCROLL) - 1;
{$ENDIF}
  Result:= Canvas.TextWidth(Text) > (Width - 4 - Modifier);
end;

procedure THListBox.CMMouseLeave(var Msg : TMessage);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited;
end;

procedure THListBox.DoExit;
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited DoExit;
end;

procedure THListBox.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited KeyDown(Key, Shift);
end;

procedure THListBox.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THListBox.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  Idx : Integer;
begin
  if SpHint and (Shift = []) then
  begin
    if (Controller.Captive = Self) and (Controller.CurStatus = 2) then
    begin
      Idx:= ItemAtPos(Point(X, Y), True);
      if Idx = -1 then
        Controller.ResetCaptive
      else
        if Idx <> FHintedItem then
        begin
          Controller.ReleaseHandle;
          if not HintProc(ClientToScreen(Point(X, Y))) then
            Controller.ResetCaptive;
        end;
    end;
    Controller.CheckMouseMove(Self, Shift, X, Y);
  end;
  inherited MouseMove(Shift, X, Y);
end;

constructor THListBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Controller.AddControl(Self, HintProc);
  FHintedItem:= -1;
  FSpHint:= True;
end;

destructor THListBox.Destroy;
begin
  if Assigned(Controller) then
    Controller.RemoveControl(Self);
  inherited Destroy;
end;

procedure THListBox.DeleteCurIndex;
var
  i : Integer;
begin
  i:= ItemIndex;
  if i <> -1 then
  begin
    Items.Delete(i);
    if Items.Count = -1 then Exit;
    if Items.Count > i then
      ItemIndex:= i
    else
      ItemIndex:= i - 1;
  end;
end;

function THListBox.CheckCurItem : Integer;
begin
  Result:= ItemIndex;
  if Result = -1 then
    raise EListError.Create('No item selected !!!');
end;

function THListBox.GetCurStr : string;
begin CheckCurItem;
  Result:= Items[ItemIndex];
end;

procedure THListBox.SetCurStr(NewStr : string);
begin CheckCurItem;
  Items[ItemIndex]:= NewStr;
end;

function THListBox.GetCurObj : TObject;
begin CheckCurItem;
  Result:= Items.Objects[ItemIndex];
end;

procedure THListBox.SetCurObj(NewObj : TObject);
begin CheckCurItem;
  Items.Objects[ItemIndex]:= NewObj;
end;

{ THEdit }

function THEdit.HintProc(CurPos : TPoint) : Boolean;
var
  Text : string;
  Rect : TRect;
begin
  CurPos:= ScreenToClient(CurPos);
  Text:= Self.Text;
  Result:= Clipped(Text);
  if Assigned(FOnShowHint) then FOnShowHint(Result, Text);
  if Result then
  begin
    Controller.Canvas.Font.Assign(Font);
    CurPos:= Point(0, -2);
    with Rect do
    begin
      TopLeft:= ClientToScreen(CurPos);
      Right:= CurPos.X + Controller.Canvas.TextWidth(Text) + 5;
      Bottom:= CurPos.Y + Controller.Canvas.TextHeight(Text);
      BottomRight:= ClientToScreen(BottomRight);
      Inc(Top, 2);
      Bottom:= Top + Height - 4;
{$IFDEF WIN32}
      if (Ctl3D = True) and (BorderStyle = bsSingle) then
      begin Dec(Top, 1);
        Dec(Left, 1);
        Dec(Bottom, 3);
      end;
{$ENDIF}
      if BorderStyle = bsNone then
      begin Dec(Top, 2);
        Dec(Left, 2)
      end;
    end;
    Controller.ActivateHint(Rect, Text);
  end;
end;

function THEdit.Clipped(Text : string) : Boolean;
var
  Modifier : Integer;
begin
  Controller.Canvas.Font.Assign(Font);
  if BorderStyle = bsSingle then
    Modifier:= 4
  else
    Modifier:= 0;
  Result:= Controller.Canvas.TextWidth(Text) > (Width - Modifier);
end;

procedure THEdit.DoMouseLeave(var Msg : TMessage);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited;
end;

procedure THEdit.DoExit;
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited DoExit;
end;

procedure THEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited KeyDown(Key, Shift);
end;

procedure THEdit.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THEdit.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  if SpHint and (Shift = []) then
    Controller.CheckMouseMove(Self, Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

constructor THEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Controller.AddControl(Self, HintProc);
  FSpHint:= True;
end;

destructor THEdit.Destroy;
begin
  if Assigned(Controller) then
    Controller.RemoveControl(Self);
  inherited Destroy;
end;

{ THDBEdit }

function THDBEdit.HintProc(CurPos : TPoint) : Boolean;
var
  Text : string;
  Rect : TRect;
begin
  CurPos:= ScreenToClient(CurPos);
  Text:= Self.Text;
  Result:= Clipped(Text);
  if Assigned(FOnShowHint) then FOnShowHint(Result, Text);
  if Result then
  begin
    Controller.Canvas.Font.Assign(Font);
    CurPos:= Point(0, 0);
    with Rect do
    begin
      TopLeft:= ClientToScreen(CurPos);
      Right:= CurPos.X + Controller.Canvas.TextWidth(Text) + 5;
      Bottom:= CurPos.Y + Controller.Canvas.TextHeight(Text);
      BottomRight:= ClientToScreen(BottomRight);
      Inc(Top, 2);
      Bottom:= Top + Height - 4;
{$IFDEF WIN32}
      if (Ctl3D = True) and (BorderStyle = bsSingle) then
      begin Dec(Top, 3);
        Dec(Left, 1);
        Dec(Bottom, 5);
      end;
{$ENDIF}
      if BorderStyle = bsNone then
      begin Dec(Top, 2);
        Dec(Left, 2)
      end;
    end;
    Controller.ActivateHint(Rect, Text);
  end;
end;

function THDBEdit.Clipped(Text : string) : Boolean;
var
  Modifier : Integer;
begin
  Controller.Canvas.Font.Assign(Font);
  if BorderStyle = bsSingle then
    Modifier:= 4
  else
    Modifier:= 0;
  Result:= Controller.Canvas.TextWidth(Text) > (Width - Modifier);
end;

procedure THDBEdit.DoMouseLeave(var Msg : TMessage);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited;
end;

procedure THDBEdit.DoExit;
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited DoExit;
end;

procedure THDBEdit.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited KeyDown(Key, Shift);
end;

procedure THDBEdit.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THDBEdit.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  if SpHint and (Shift = []) then
    Controller.CheckMouseMove(Self, Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

constructor THDBEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Controller.AddControl(Self, HintProc);
  FSpHint:= True;
end;

destructor THDBEdit.Destroy;
begin
  if Assigned(Controller) then
    Controller.RemoveControl(Self);
  inherited Destroy;
end;

{ THLabel }

function THLabel.HintProc(CurPos : TPoint) : Boolean;
var
  Text : string;
  Rect : TRect;
  L : TStringList;
begin
  CurPos:= ScreenToClient(CurPos);
  Text:= Caption;
  Result:= Clipped(Text);
  if Assigned(FOnShowHint) then FOnShowHint(Result, Text);
  if Result then
  begin
    Controller.Canvas.Font.Assign(Font);
    CurPos:= Point(-2, -2);
    with Rect do
    begin
      TopLeft:= ClientToScreen(CurPos);
      Right:= CurPos.X + Controller.Canvas.TextWidth(Text) + 5;
      Bottom:= CurPos.Y + Controller.Canvas.TextHeight(Text);
      BottomRight:= ClientToScreen(BottomRight);
    end;
    if Pos(#13, Text) <> 0 then
    begin
      L:= TStringList.Create;
      try
        L.Text:= Text;
        Rect.Bottom:= Rect.Top + (Rect.Bottom - Rect.Top) * L.Count;
      finally
        L.Free;
      end;
    end;
    Controller.ActivateHint(Rect, Text);
  end;
end;

function THLabel.Clipped(Text : string) : Boolean;
var
  L : TStringList;
  i : Integer;
begin
  Controller.Canvas.Font.Assign(Font);
  if Pos(#13, Text) <> 0 then
  begin
    L:= TStringList.Create;
    try
      L.Text:= Text;
      Result:= False;
      for i:= 0 to L.Count - 1 do
        Result:= Result or (Controller.Canvas.TextWidth(L[i]) > Width);
    finally
      L.Free;
    end;
  end
  else
    Result:= Controller.Canvas.TextWidth(Text) > Width;
end;

procedure THLabel.DoMouseLeave(var Msg : TMessage);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited;
end;

procedure THLabel.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THLabel.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  if SpHint and (Shift = []) then
    Controller.CheckMouseMove(Self, Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

constructor THLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Controller.AddControl(Self, HintProc);
  FSpHint:= True;
end;

destructor THLabel.Destroy;
begin
  if Assigned(Controller) then
    Controller.RemoveControl(Self);
  inherited Destroy;
end;

{ THDBText }

function THDBText.HintProc(CurPos : TPoint) : Boolean;
var
  Field : TField;
  Text : string;
  Rect : TRect;
begin
  Result:= False;
  CurPos:= ScreenToClient(CurPos);
  try
    Field:= DataSource.DataSet.FieldByName(DataField);
    Text:= Field.AsString;
  except Exit
  end;
  Result:= Clipped(Text);
  if Assigned(FOnShowHint) then FOnShowHint(Result, Text);
  if Result then
  begin
    Controller.Canvas.Font.Assign(Font);
    CurPos:= Point(-2, -2);
    with Rect do
    begin
      TopLeft:= ClientToScreen(CurPos);
      Right:= CurPos.X + Controller.Canvas.TextWidth(Text) + 5;
      Bottom:= CurPos.Y + Controller.Canvas.TextHeight(Text);
      BottomRight:= ClientToScreen(BottomRight);
    end;
    Controller.ActivateHint(Rect, Text);
  end;
end;

function THDBText.Clipped(Text : string) : Boolean;
begin
  Controller.Canvas.Font.Assign(Font);
  Result:= Controller.Canvas.TextWidth(Text) > Width;
end;

procedure THDBText.DoMouseLeave(var Msg : TMessage);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited;
end;

procedure THDBText.MouseDown(Button : TMouseButton; Shift : TShiftState;
  X, Y : Integer);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THDBText.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  if SpHint and (Shift = []) then
    Controller.CheckMouseMove(Self, Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

constructor THDBText.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Controller.AddControl(Self, HintProc);
  FSpHint:= True;
end;

destructor THDBText.Destroy;
begin
  if Assigned(Controller) then
    Controller.RemoveControl(Self);
  inherited Destroy;
end;

{ THDBGrid }

{FG 99-12-01}

procedure THDBGrid.Loaded;
begin
  inherited;
  if AutoSizeLastColumn then
    DoAutoSize;
end;

{FG 99-11-25}

procedure THDBGrid.SetAutoSizeLastColumn(Value : Boolean);
begin
  if Value <> FAutoSizeLastColumn then
  begin
    FAutoSizeLastColumn:= Value;
    if Value then
      DoAutoSize;
  end;
end;

{FG 99-11-25}

procedure THDBGrid.ColWidthsChanged;
begin
  inherited;
  DoAutoSize;
end;

procedure THDBGrid.WMSize(var Msg : TWMSize);
begin
  inherited;
  DoAutoSize;
end;

procedure THDBGrid.DoAutoSize;
var
  GoodWidth : Integer;
  i, k : Integer;
begin
  if FAutoResizing then
    Exit;
  if (csLoading in ComponentState) or (csCreating in ControlState) then
    Exit;
  if AutoSizeLastColumn then
  begin
    FAutoResizing:= True;
    try
      if dgColLines in Options then
        GoodWidth:= ColCount
      else
        GoodWidth:= 0;

      if Columns.Count > 0 then
      begin
        k := Columns.Count - 1;
        try
          while (k >= 0) and (k < Columns.Count) and  (not Columns[k].Visible) do
            Dec(k);
        except
          Exit;
          //ShowMessage('K est égal à ' + IntToStr(k) + ' et il y a ' + IntToStr(Columns.Count) + ' colonnes.');
        end;
      end
      else
        Exit;

      if ColCount > 1 then
      begin
        for i:= 0 to ColCount - 1 - 1 - (Columns.Count - 1 - k) do
          GoodWidth:= GoodWidth + ColWidths[i];
      end;

      GoodWidth:= (ClientRect.Right - ClientRect.Left) - GoodWidth;

      if GoodWidth <= 0 then
        Exit;

      if (Columns.Count > 0) then //and (Columns[Columns.Count - 1].Width <> GoodWidth) then
      begin
        if (k >= 0) and (k < Columns.Count) then
          try
            Columns[k].Width:= GoodWidth;
          except
          end;
      end;
    finally
      FAutoResizing:= False;
    end;
  end;
end;

{$IFDEF VER120}

function THDBGrid.CalcD4GridTitleOffset : Integer;
var
  i, j : Integer;
begin
  Result:= 0;
  if dgTitles in Options then
  begin
    Result:= 1;
    if (DataSource.Dataset <> nil) and DataSource.Dataset.ObjectView then
      for i:= 0 to Columns.Count - 1 do
        if Columns[i].Showing then
        begin
          j:= Columns[i].Depth;
          if j >= Result then Result:= j + 1;
        end;
  end;
end;
{$ENDIF}

{$IFDEF WIN32}

function THDBGrid.ValueAtPos(Pos : TPoint) : string;
var
  CellPos : TGridCoord;
  Col : TColumn;
  OldActive : Integer;
begin
  Result:= '';
  CellPos:= MouseCoord(Pos.X, Pos.Y);
  if dgIndicator in Options then Dec(CellPos.X);
{$IFDEF VER120}
  if dgTitles in Options then Dec(CellPos.Y, CalcD4GridTitleOffset);
{$ELSE}
  if dgTitles in Options then Dec(CellPos.Y);
{$ENDIF}
  if (CellPos.X < 0) or (CellPos.Y < 0) then Exit;
  Col:= Columns[CellPos.X];
  OldActive:= DataLink.ActiveRecord;
  try
    DataLink.ActiveRecord:= CellPos.Y;
    if Assigned(Col.Field) then
      Result:= Col.Field.DisplayText;
  finally
    DataLink.ActiveRecord:= OldActive;
  end;
end;

function THDBGrid.FontAtPos(Pos : TPoint) : TFont;
var
  CellPos : TGridCoord;
  OldActive: Integer;
begin
  Result:= nil;
  CellPos:= MouseCoord(Pos.X, Pos.Y);
  if dgIndicator in Options then
    Dec(CellPos.X);
{$IFDEF VER120}
  if dgTitles in Options then Dec(CellPos.Y, CalcD4GridTitleOffset);
{$ELSE}
  if dgTitles in Options then Dec(CellPos.Y);
{$ENDIF}
  if (CellPos.X < 0) or (CellPos.Y < 0) then
    Exit;
  Result:= Columns[CellPos.X].Font;
  if Assigned(OnGetFont) then
  begin
    OldActive:= DataLink.ActiveRecord;
    try
      DataLink.ActiveRecord:= CellPos.Y;
      Canvas.Font.Assign(Result);
      Result:= Canvas.Font;
      OnGetFont(Self, Result);
    finally
      DataLink.ActiveRecord:= OldActive;
    end;
  end;
end;
{$ELSE}

function THDBGrid.ValueAtPos(Pos : TPoint) : string;
var
  CellPos : TGridCoord;
  OldActive : Integer;
  i : Integer;
begin
  Result:= '';
  CellPos:= MouseCoord(Pos.X, Pos.Y);
  if ((CellPos.X <= 0) and (dgIndicator in Options)) or
    ((CellPos.Y <= 0) and (dgTitles in Options)) then Exit;
  OldActive:= DataLink.ActiveRecord;
  if not (dgIndicator in Options) then Inc(CellPos.X);
  if not (dgTitles in Options) then Inc(CellPos.Y);
  try
    DataLink.ActiveRecord:= CellPos.Y - 1;
    Result:= Fields[CellPos.X - 1].DisplayText;
  finally
    DataLink.ActiveRecord:= OldActive;
  end;
end;

function THDBGrid.FontAtPos(Pos : TPoint) : TFont;
begin
  Result:= Font;
end;
{$ENDIF}

function THDBGrid.RectAtPos(Pos : TPoint) : TRect;
var
  Cell : TGridCoord;
begin
  Cell:= MouseCoord(Pos.X, Pos.Y);
  Result:= CellRect(Cell.X, Cell.Y);
  if (Result.Bottom < Pos.Y) or (Result.Right < Pos.X) then
    Result:= Rect(-1, -1, -1, -1);
end;

function THDBGrid.HintProc(CurPos : TPoint) : Boolean;
var
  Rect : TRect;
  Text : string;
  Font : TFont;
  Cell : TGridCoord;
begin
  Result:= False;
  CurPos:= ScreenToClient(CurPos);
  Text:= ValueAtPos(CurPos);
  Result:= (Text <> '') and Clipped(Text, CurPos);
  if Assigned(FHintEvent) then
  begin
    Cell:= MouseCoord(CurPos.X, CurPos.Y);
    OnShowHint(Cell.X, Cell.Y, Result, Text);
  end;
  if Result then
  begin
    Font:= FontAtPos(CurPos);
    if Font = nil then
      Font:= Self.Font;
    Controller.Canvas.Font.Assign(Font);
    Rect:= RectAtPos(CurPos);
    with Rect do
    begin
      Dec(Bottom, 4);
      Right:= Left + Controller.Canvas.TextWidth(Text) + 5;
      TopLeft:= ClientToScreen(TopLeft);
      BottomRight:= ClientToScreen(BottomRight);
    end;
    Controller.ActivateHint(Rect, Text);
    FHintedItem:= MouseCoord(CurPos.X, CurPos.Y);
  end;
end;

function THDBGrid.Clipped(Text : string; CurPos : TPoint) : Boolean;
var
  Rect : TRect;
  Width, Modifier : Integer;
  Font : TFont;
begin
  Rect:= RectAtPos(CurPos);
  Width:= Abs(Rect.Right - Rect.Left);
  Modifier:= 0;
  Font:= FontAtPos(CurPos);
  if Font = nil then
    Font:= Self.Font;
  Controller.Canvas.Font.Assign(Font);
  Result:= Controller.Canvas.TextWidth(Text) > (Width - 2 - Modifier);
end;

procedure THDBGrid.DoMouseLeave(var Msg : TMessage);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited;
end;

procedure THDBGrid.DoExit;
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited DoExit;
end;

procedure THDBGrid.Click;
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited Click;
end;

procedure THDBGrid.KeyDown(var Key : Word; Shift : TShiftState);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited KeyDown(Key, Shift);
end;

procedure THDBGrid.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if (Controller.Captive = Self) then
    Controller.ResetCaptive;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure THDBGrid.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  CurCell : TGridCoord;
begin
  if DataLink.Active and SpHint and (Shift = []) then
  begin
    if (Controller.Captive = Self) and (Controller.CurStatus = 2) then
    begin
      CurCell:= MouseCoord(X, Y);
      if ((CurCell.X <> FHintedItem.X) or (CurCell.Y <> FHintedItem.Y)) and
        (((CurCell.X = 0) and (dgIndicator in Options)) or
        ((CurCell.Y = 0) and (dgTitles in Options)) or
        (RectAtPos(Point(X, Y)).Left = -1)) then
        Controller.ResetCaptive
      else
        if (CurCell.X <> FHintedItem.X) or (CurCell.Y <> FHintedItem.Y) then
        begin
          Controller.ReleaseHandle;
          if not HintProc(ClientToScreen(Point(X, Y))) then
            Controller.ResetCaptive;
        end;
    end;
    Controller.CheckMouseMove(Self, Shift, X, Y);
  end;
  inherited MouseMove(Shift, X, Y);
end;

function THDBGrid.SelectCell(ACol, ARow : LongInt) : Boolean;
begin
  Result:= inherited SelectCell(ACol, ARow);
  if Assigned(FOnSelectCell) then FOnSelectCell(Self, ACol, ARow, Result);
end;

constructor THDBGrid.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Controller.AddControl(Self, HintProc);
  FHintedItem.X:= -1;
  FHintedItem.Y:= -1;
  FSpHint:= True;
  FAutoSizeLastColumn:= False;
end;

destructor THDBGrid.Destroy;
begin
  if Assigned(Controller) then
    Controller.RemoveControl(Self);
  inherited Destroy;
end;

{ Général }

{$IFNDEF WIN32}

procedure FreeAll;
begin
  Controller.Free;
end;
{$ENDIF}

{$IFNDEF WIN32}
{ TzHintWindow }

constructor TzHintWindow.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Color:= $80FFFF;

  Canvas.Font.Size:= 8;
  Canvas.Brush.Style:= bsClear;
end;

procedure TzHintWindow.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style:= WS_POPUP or WS_BORDER;
    WindowClass.Style:= WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TzHintWindow.WMNCHitTest(var Message : TWMNCHitTest);
begin
  Message.Result:= HTTRANSPARENT;
end;

procedure TzHintWindow.WMNCPaint(var Message : TMessage);
var
  R : TRect;
begin
  Canvas.Handle:= GetWindowDC(Handle);
  with Canvas do
  try
    R:= Rect(0, 0, Width, Height);
    WinProcs.Rectangle(Canvas.Handle, 0, 0, Width, Height);
  finally
    Canvas.Handle:= 0;
  end;
end;

procedure TzHintWindow.Paint;
var
  R : TRect;
  C : PChar;
begin
  R:= ClientRect;
  Inc(R.Left, 1);
  Inc(R.Top, 1);
  Dec(R.Right, 1);
  Dec(R.Bottom, 1);
  Canvas.Font.Color:= clBlack;
  GetMem(C, Length(Caption) + 1);
  DrawText(Canvas.Handle, StrPCopy(C, Caption), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK);
  FreeMem(C, Length(Caption) + 1);
end;

function TzHintWindow.IsHintMsg(var Msg : TMsg) : Boolean;
begin
  with Msg do
    Result:= ((Message >= WM_KEYFIRST) and (Message <= WM_KEYLAST)) or
      ((Message = CM_ACTIVATE) or (Message = CM_DEACTIVATE)) or
      (Message = CM_APPKEYDOWN) or (Message = CM_APPSYSCOMMAND) or
      (Message = WM_COMMAND) or ((Message > WM_MOUSEMOVE) and
      (Message <= WM_MOUSELAST)) or (Message = WM_NCMOUSEMOVE);
end;

procedure TzHintWindow.ReleaseHandle;
begin
  DestroyHandle;
end;

procedure TzHintWindow.CMTextChanged(var Message : TMessage);
begin
  inherited;
  Width:= Canvas.TextWidth(Caption) + 6;
  Height:= Canvas.TextHeight(Caption) + 4;
end;

procedure TzHintWindow.ActivateHint(Rect : TRect; const AHint : string);
begin
  Caption:= AHint;
  Inc(Rect.Bottom, 4);
  BoundsRect:= Rect;

  if Rect.Top + Height > Screen.Height then
    Rect.Top:= Screen.Height - Height;
  if Rect.Left + Width > Screen.Width then
    Rect.Left:= Screen.Width - Width;
  if Rect.Left < 0 then Rect.Left:= 0;
  if Rect.Bottom < 0 then Rect.Bottom:= 0;

  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
end;

procedure TzHintWindow.ActivateHintData(Rect : TRect; const AHint : string; AData : Pointer);
begin
  ActivateHint(Rect, AHint);
end;

function TzHintWindow.CalcHintRect(MaxWidth : Integer; const AHint : string; AData : Pointer) : TRect;
var
  C : PChar;
begin
  GetMem(C, Length(AHint) + 1);
  Result:= Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, StrPCopy(C, AHint), -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX);
  FreeMem(C, Length(AHint) + 1);
  Inc(Result.Right, 6);
  Inc(Result.Bottom, 2);
end;
{$ENDIF}

initialization
  begin
    BaseShowHintDelay:= 400;
    Controller:= THintController.Create(nil);
{$IFNDEF WIN32}AddExitProc(FreeAll);{$ENDIF}
  end;
{$IFDEF WIN32}
finalization
  begin
    if Assigned(Controller) then
    begin
      while Controller.Controls.Count > 0 do
        Controller.RemoveControl(Controller.Controls[0]);
    end;
    Controller.Free;
    Controller:= nil;
  end;
{$ENDIF}

end.

