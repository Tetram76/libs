unit QRUtils;

{$I GrizzlyDefine.inc}

interface

uses QuickRpt;

procedure QRPreviewModal(AReport : TQuickRep);

implementation

uses Windows, Classes, Forms;

procedure QRPreviewModal(AReport : TQuickRep);
var
  i : Integer;
  ALst : TList;
begin
  ALst := TList.Create;
  try
    for i := 0 to Screen.FormCount - 1 do
      if IsWindowEnabled(Screen.Forms[i].Handle) then
      begin
        ALst.Add(Screen.Forms[i]);
        Windows.EnableWindow(Screen.Forms[i].Handle, False);
      end;
    AReport.Preview;
    for i := 0 to ALst.Count - 1 do
      Windows.EnableWindow(TForm(ALst[i]).Handle, True);
  finally
    ALst.Free;
  end;
end;

end.
 