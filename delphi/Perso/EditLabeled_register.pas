unit EditLabeled_register;

interface

uses
  Classes, EditLabeled, DBEditLabeled;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TDateTimePickerLabeled, TDBCheckBoxLabeled, TCheckBoxLabeled, TScanEditLVLabeled, TVDTListViewLabeled, TDBEditLabeled, TDBMemoLabeled, TMemoLabeled, TEditLabeled, TSpinEditLabeled, TMaskEditLabeled, TDBComboBoxLabeled, TListBoxLabeled, TRadioGroupLabeled, TRadioButtonLabeled, TCheckListBoxLabeled]);
end;

end.
