unit VDTButton;
{$D-}
interface

uses
  Windows, Forms, Controls, Classes, Buttons, ComCtrls, Graphics, RoundButton;

type
  TVDTButton = class(TSpeedButton)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Flat default True;
    property Cursor default crHandpoint;
  end;

  TVDTListView = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SortType default stText;
    property Cursor default crHandPoint;
    property Anchors stored True nodefault;
    property BorderStyle default bsNone;
    property ColumnClick default False;
    property HideSelection default False;
    property ReadOnly default True;
    property RowSelect default False;
    property ShowColumnHeaders default False;
    property ViewStyle default vsReport;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tetram', [TVDTButton, TVDTListView]);
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
