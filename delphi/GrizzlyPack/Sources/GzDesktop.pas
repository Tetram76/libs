unit GzDesktop;

interface

uses
  Windows, Types, Classes, GzProcessUtils, Forms;

procedure SetDesktopWorkingArea(ARect : TRect);
procedure ResetDesktopWorkingArea;

implementation

function RectIsEmpty(ARect : TRect) : Boolean;
begin
  with ARect do
    Result := (Left - Right = 0) and (Top - Bottom = 0);
end;

procedure SetDesktopWorkingArea(ARect : TRect);
var
  ALst : TStringList;
  i : Integer;
  AWP : TWindowPlacement;
begin
  ALst := TStringList.Create;
  try
    if RectIsEmpty(ARect) then
      ARect := Screen.DesktopRect;
    SystemParametersInfo(SPI_SETWORKAREA, 0, @ARect, 0);
    GetVisibleWindowList(ALst);
    for i := 0 to ALst.Count - 1 do
    begin
      GetWindowPlacement(HWND(ALst.Objects[i]), @AWP);
      if AWP.showCmd = SW_SHOWMAXIMIZED then
      begin
        ShowWindow(HWND(ALst.Objects[i]), SW_NORMAL);
        ShowWindow(HWND(ALst.Objects[i]), SW_MAXIMIZE);
      end;
    end;
  finally
    ALst.Free;
  end;
end;

procedure ResetDesktopWorkingArea;
begin
  SetDesktopWorkingArea(Rect(0, 0, 0, 0));
end;

end.
