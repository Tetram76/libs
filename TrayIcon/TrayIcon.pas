unit TrayIcon;
{.$D-}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI;

type
  TTrayIconEvent = procedure(Sender: TObject; Shift: TShiftState) of object;

  TTrayIcon = class(TComponent)
  private
    { Déclarations privées }
    NotifyData : TNotifyIconData; // "structure" de l'icône
    FTrayIconMessage: Cardinal;
    FIcon: TIcon;
    FActive: Boolean;
    FMessageWindow: HWND;
    FHint: string;
    FIconID: Integer;
    FOnMouseDown: TTrayIconEvent;
    FOnMouseMove: TTrayIconEvent;
    FOnMouseUp: TTrayIconEvent;
    FOnDblClick: TTrayIconEvent;
    FUpdateCount: Integer;
    FShown: Boolean;
    procedure OnChangeTIcon(Sender: TObject);
    procedure SetIcon(Value: TIcon);
    procedure SetActive(Value: Boolean);
    procedure SetHint(Value: string);
    procedure SetIconID(Value: Integer);
    procedure ChangeIcon(Action: Integer);
    procedure ClientWndProc(var Message: TMessage);
    procedure Refresh;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property TrayIconMessage: Cardinal read FTrayIconMessage;
    property Icon: TIcon read FIcon write SetIcon;
    property Tip: string read FHint write SetHint;
    property IconID: Integer read FIconID write SetIconID;
    property OnDblClick: TTrayIconEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TTrayIconEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TTrayIconEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TTrayIconEvent read FOnMouseUp write FOnMouseUp;

    property Active: Boolean read FActive write SetActive; // Important qu'il soit à la fin
  end;

procedure Register;

implementation

uses Divers;

procedure Register;
begin
  RegisterComponents('Tetram', [TTrayIcon]);
end;

constructor TTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.FMessageWindow := Classes.AllocateHWnd(Self.ClientWndProc);
  FTrayIconMessage := RegisterWindowMessage(PChar('TetramTrayIconMessage'));
  NotifyData.cbSize := SizeOf(NotifyData);
  NotifyData.Wnd := Self.FMessageWindow;
  NotifyData.uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE;
  NotifyData.uCallbackMessage := TrayIconMessage;
  FIconID := 1;
  FIcon := TIcon.Create;
  FIcon.OnChange := OnChangeTIcon;
  FShown := False;
  FUpdateCount := 0;
end;

destructor TTrayIcon.Destroy;
begin
  Active := False;
  FIcon.OnChange := nil;
  FreeAndNil(FIcon);
  Classes.DeallocateHWnd(Self.FMessageWindow);
  inherited Destroy;
end;

procedure TTrayIcon.ClientWndProc(var Message: TMessage);
var
  Shift: TShiftState;
begin
  with Message do begin
    if (Msg = TrayIconMessage) and (wParam = FIconID) then begin
      Shift := [];
      if IsDownKey(VK_SHIFT) then Include(Shift, ssShift);
      if IsDownKey(VK_CONTROL) then Include(Shift, ssCtrl);
      if IsDownKey(VK_MENU) then Include(Shift, ssAlt);
      case lParam of
        WM_MOUSEMOVE: if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift);
        WM_LBUTTONDOWN: if Assigned(FOnMouseDown) then FOnMouseDown(Self, [ssLeft] + Shift);
        WM_RBUTTONDOWN: if Assigned(FOnMouseDown) then FOnMouseDown(Self, [ssRight] + Shift);
        WM_MBUTTONDOWN: if Assigned(FOnMouseDown) then FOnMouseDown(Self, [ssMiddle] + Shift);
        WM_LBUTTONDBLCLK: if Assigned(FOnDblClick) then FOnDblClick(Self, [ssLeft, ssDouble] + Shift);
        WM_RBUTTONDBLCLK: if Assigned(FOnDblClick) then FOnDblClick(Self, [ssRight, ssDouble] + Shift);
        WM_MBUTTONDBLCLK: if Assigned(FOnDblClick) then FOnDblClick(Self, [ssMiddle, ssDouble] + Shift);
        WM_LBUTTONUP: if Assigned(FOnMouseUp) then FOnMouseUp(Self, [ssLeft] + Shift);
        WM_RBUTTONUP: if Assigned(FOnMouseUp) then FOnMouseUp(Self, [ssRight] + Shift);
        WM_MBUTTONUP: if Assigned(FOnMouseUp) then FOnMouseUp(Self, [ssMiddle] + Shift);
      end;
    end else
      Result := DefWindowProc(Self.FMessageWindow, Msg, wParam, lParam);
  end;
end;

procedure TTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  Refresh;
end;

procedure TTrayIcon.SetActive(Value: Boolean);
begin
  if FActive = Value then Exit;
  FActive := Value;
  if FActive then ChangeIcon(NIM_ADD)
             else ChangeIcon(NIM_DELETE)
end;

procedure TTrayIcon.SetHint(Value: string);
begin
  if FHint = Value then Exit;
  FHint := Value;
  Refresh;
end;

procedure TTrayIcon.SetIconID(Value: Integer);
begin
  if FIconID = Value then Exit;
  FIconID := Value;
  Refresh;
end;

procedure TTrayIcon.Refresh;
begin
  if FActive then if FShown then ChangeIcon(NIM_MODIFY)
                            else ChangeIcon(NIM_ADD)
             else Active := True;
end;

procedure TTrayIcon.ChangeIcon(Action: Integer);
var
  i: Integer;
begin
  if Bool(FUpdateCount) or (csDesigning in ComponentState) then Exit;
  NotifyData.uID := FIconID;
  if Icon.Empty then NotifyData.hIcon := Application.Icon.Handle
                else NotifyData.hIcon := Icon.Handle;
  for i := 0 to Length(FHint) - 1 do
    NotifyData.szTip[i] := FHint[i + 1];
  NotifyData.szTip[Length(FHint)] := #0;
  Shell_NotifyIcon(Action, @NotifyData);
  FShown := Action <> NIM_DELETE;
end;

procedure TTrayIcon.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TTrayIcon.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  Refresh;
end;

procedure TTrayIcon.OnChangeTIcon(Sender: TObject);
begin
  Refresh;
end;

end.
