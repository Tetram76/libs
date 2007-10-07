unit VDTButton;
{$D-}
interface

uses
  Windows, Forms, Controls, Classes, Buttons, ComCtrls, Graphics, RoundButton;

type
  TVDTButton = class(TSpeedButton)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
  published
    { Déclarations publiées }
  end;

  TVDTListView = class(TListView)
  private
    { Déclarations privées }
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
  published
    { Déclarations publiées }
    property SortType default stText;
    property Cursor default crHandPoint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Medi@ Kit', [TVDTButton, TVDTListView]);
end;

constructor TVDTButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Flat := True;
  Cursor := crHandPoint;
end;

constructor TVDTListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Anchors := [akLeft, akTop, akRight, akBottom];
  BorderStyle := bsNone;
  ColumnClick := False;
  HideSelection := False;
  ReadOnly := True;
  RowSelect := False;
  ShowColumnHeaders := False;
  SortType := stText;
  ViewStyle := vsReport;
  Cursor := crHandPoint;
end;

end.
