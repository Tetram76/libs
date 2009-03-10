unit FGLChTb;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TGLDlgChoixTables = class(TForm)
    LBGroups: TListBox;
    LBItems: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BtnOk: TButton;
    BtnCancel: TButton;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  GLDlgChoixTables: TGLDlgChoixTables;

implementation

{$R *.DFM}

end.
