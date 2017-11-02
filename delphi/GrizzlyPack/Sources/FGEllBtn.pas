unit FGEllBtn;

interface

uses
  Classes, Controls, Buttons, Graphics;

type
  TEllipsisButton = class(TBitBtn)
  private
    { Déclarations privées }
    FEllipsisGlyph: Boolean;
    procedure SetEllipsisGlyph(const Value: Boolean);
  protected
    { Déclarations protégées }
    procedure Loaded; override;
  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;
  published
    { Déclarations publiées }
    property EllipsisGlyph : Boolean read FEllipsisGlyph write SetEllipsisGlyph;
    property NumGlyphs default 2;
  end;

implementation

{$R FGELLBTN.RES}

constructor TEllipsisButton.Create(AOwner : TComponent);
begin
  inherited;
  FEllipsisGlyph := True;
  Glyph.LoadFromResourceName(HInstance, 'FGELLIPSISIMAGE');
  NumGlyphs:= 2;
  Caption:= '';
  ControlStyle := ControlStyle - [csSetCaption];
  Width:= 25;
  Height:= 21;
end;

procedure TEllipsisButton.Loaded;
begin
  inherited;
  if EllipsisGlyph then
  begin
    Glyph.LoadFromResourceName(HInstance, 'FGELLIPSISIMAGE');
    NumGlyphs:= 2;
  end;
end;

procedure TEllipsisButton.SetEllipsisGlyph(const Value: Boolean);
begin
  FEllipsisGlyph := Value;
  if Value then
  begin
    Glyph.LoadFromResourceName(HInstance, 'FGELLIPSISIMAGE');
    NumGlyphs:= 2;
  end;
end;

end.
