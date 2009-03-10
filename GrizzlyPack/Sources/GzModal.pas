unit GzModal;

interface

uses Classes, SysUtils, Forms;

procedure StartShowModal(AForm : TForm);
procedure EndShowModal;

implementation

var
  LstModal : TList;

procedure StartShowModal(AForm : TForm);
var
  i : Integer;
begin
  if not Assigned(LstModal) then
    LstModal := TList.Create;
  LstModal.Clear;
  for i := 0 to Screen.FormCount - 1 do
    if (Screen.Forms[i].Enabled) and (Screen.Forms[i] <> AForm) then
    begin
      LstModal.Add(Screen.Forms[i]);
      Screen.Forms[i].Enabled := False;
    end;
  AForm.Show;
end;

procedure EndShowModal;
var
  i : Integer;
begin
  if not Assigned(LstModal) then
    Exit;
  for i := 0 to LstModal.Count - 1 do
    TForm(LstModal[i]).Enabled := True;
  LstModal.Clear;
end;

initialization
  LstModal := nil;
finalization
  if Assigned(LstModal) then
    LstModal.Free;
end.
