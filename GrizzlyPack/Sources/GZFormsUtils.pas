unit GzFormsUtils;

interface

Uses Classes, SysUtils, Forms;

procedure DisableAndShow(AForm : TCustomForm);

procedure GlobalDisable;
procedure GlobalEnable;

implementation

var
  LstGlobalDisable : TList;

procedure GlobalDisable;
var
  i : Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i].Enabled then
    begin
      LstGlobalDisable.Add(Screen.Forms[i]);
      Screen.Forms[i].Enabled := False;
    end;
end;

procedure GlobalEnable;
var
  i : Integer;
begin
  for i := 0 to LstGlobalDisable.Count - 1 do
    TCustomForm(LstGlobalDisable[i]).Enabled := True;
end;

procedure DisableAndShow(AForm : TCustomForm);
var
  i : Integer;
  ALst : TList;
begin
  ALst := TList.Create;
  try
    for i := 0 to Screen.FormCount - 1 do
      if Screen.Forms[i].Enabled then
      begin
        ALst.Add(Screen.Forms[i]);
        Screen.Forms[i].Enabled := False;
      end;
    AForm.Show;
    try
      while AForm.Visible do
        Application.ProcessMessages;
    except
    end;
    for i := 0 to ALst.Count - 1 do
      TForm(ALst[i]).Enabled := True;
  finally
    ALst.Free;
  end;
end;

initialization
  LstGlobalDisable := TList.Create;
finalization
  FreeAndNil(LstGlobalDisable);
end.
