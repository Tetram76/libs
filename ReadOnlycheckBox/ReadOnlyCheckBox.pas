unit ReadOnlyCheckBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TReadOnlyCheckBox = class(TCheckBox)
  private
    { Déclarations privées }
    OldValue: TCheckBoxState;
  protected
    { Déclarations protégées }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    { Déclarations publiques }
  published
    { Déclarations publiées }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TReadOnlyCheckBox]);
end;

procedure TReadOnlyCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then Key := 0;
  inherited;
end;

procedure TReadOnlyCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OldValue := State;
  inherited;
end;

procedure TReadOnlyCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  State := OldValue;
end;

end.
