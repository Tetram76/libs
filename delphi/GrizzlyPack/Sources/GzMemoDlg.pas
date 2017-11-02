unit GzMemoDlg;

interface

uses
  SysUtils, Classes, Controls;

type
  TMemoEditDialog = class(TComponent)
  private
    FText: string;
    FTitle: string;
    { Déclarations privées }
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    function Execute : Boolean;
  published
    { Déclarations publiées }
    property Title : string read FTitle write FTitle;
    property Text : string read FText write FText;
  end;


implementation

uses FMemoEditDlg;

{ TMemoEditDialog }

function TMemoEditDialog.Execute: Boolean;
var
  AFen : TDlgMemoEdit;
begin
  Result := False;
  AFen := TDlgMemoEdit.Create(Self);
  try
    AFen.Caption := Title;
    AFen.Memo1.Text := Text;
    if AFen.ShowModal = mrOk then
    begin
      Result := True;
      Text := AFen.Memo1.Text;
    end;
  finally
    AFen.Free;
  end;
end;

end.
 