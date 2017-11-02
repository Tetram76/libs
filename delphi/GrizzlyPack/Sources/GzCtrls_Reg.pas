unit GzCtrls_Reg;

{$I GrizzlyDefine.inc}

interface

procedure Register;

implementation

uses
  Classes, Graphics, GzConsts,
  FGCPGlyf, HintCtrl, ExitPnl, ProgressGauge, FGEllBtn, GzFileGrid
  {$IFDEF GZ_D6}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF};


procedure Register;
begin
  RegisterComponents(SGrizzlyHint, [THListBox, THEdit, THDBEdit, THLabel, THDBText, THDBGrid]);

  RegisterComponents(SGrizzlyCtrls, [TExitPanel]);
  RegisterPropertyEditor(TypeInfo(TBitmap), TBtnExitPanel, 'Glyph', TGlyphProperty);

  RegisterComponents(SGrizzlyCtrls, [TProgressGauge]);

  RegisterComponents(SGrizzlyCtrls, [TEllipsisButton]);

  RegisterComponents(SGrizzlyCtrls, [TFileGrid]);

end;

end.
