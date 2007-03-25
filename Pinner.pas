unit Pinner;

interface

uses
  SysUtils, Classes, Controls, Buttons;

type
  TPinner = class(TSpeedButton)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

implementation

{$R Pinner.res}

uses
  Windows, Forms;

{ TPinner }

procedure TPinner.Click;
begin
  inherited;
  // if FormStyle is used instead of SetWindowPos the window is recreated wich
  // produces heavy flickering
  if TForm(Owner).FormStyle = fsStayOnTop then
    raise Exception.Create('FormStyle of Owner must not be fsStayOnTop');
  if Down then
    SetWindowPos(TForm(Owner).Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE)
  else
    SetWindowPos(TForm(Owner).Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
end;

constructor TPinner.Create(AOwner: TComponent);
begin
  inherited;
  AllowAllUp := True;
  Flat := True;
  Glyph.LoadFromResourceName(HInstance, 'Pinner');
  GroupIndex := 1;
  Height := 22;
  NumGlyphs := 4;
  Width := 22;
end;

end.
