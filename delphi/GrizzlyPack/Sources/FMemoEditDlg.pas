unit FMemoEditDlg;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF GZ_D6}Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExitPnl;

type
  TDlgMemoEdit = class(TForm)
    ExitPanel1: TExitPanel;
    Memo1: TMemo;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DlgMemoEdit: TDlgMemoEdit;

implementation

{$R *.dfm}

end.
