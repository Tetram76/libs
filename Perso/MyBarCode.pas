unit MyBarCode;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics;

type
  TBarLineType = (white, black, black_half);  {for internal use only}
  TBarcodeType = (bcEAN8, bcEAN13);

  TMyBarCode = class(TComponent)
  private
    { Déclarations privées }
    FHeight: Integer;
    FText: string;
    FTop: Integer;
    FLeft: Integer;
    FModul: Integer;
    FTyp: TBarcodeType;
    FRatio: Double;
    FCheckSum: Boolean;
    FShowText: Boolean;
    FAngle: Double;
    FColor: TColor;
    FColorBar: TColor;
    modules: array[0..3] of Shortint;
    procedure OneBarProps(code: Char; var Width: Integer; var lt: TBarLineType);
    procedure DoLines(data: string; Canvas: TCanvas);
    function Code_EAN8: string;
    function Code_EAN13: string;
    procedure MakeModules;
    procedure SetModul(v: Integer);
    function GetWidth: Integer;
  protected
    { Déclarations protégées }
    function MakeData: string;
  public
    { Déclarations publiques }
    constructor Create(Owner: TComponent); override;
    procedure DrawBarcode(Canvas: TCanvas);
    procedure DrawText(Canvas: TCanvas);
  published
    { Déclarations publiées }
    property Height: Integer read FHeight write FHeight;
    property Text: string  read FText write FText;
    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property Modul: Integer read FModul write SetModul;
    property Ratio: Double  read FRatio write FRatio;
    property Typ: TBarcodeType read FTyp write FTyp default bcEAN13;
    property Checksum: Boolean read FCheckSum write FCheckSum default False;
    property Angle: Double read FAngle write FAngle;
    property ShowText: Boolean read FShowText write FShowText default False;
    property Width: Integer read GetWidth;
    property Color: TColor read FColor write FColor default clWhite;
    property ColorBar: TColor read FColorBar write FColorBar default clBlack;
  end;

procedure Register;

implementation

function Rotate2D(p: TPoint; alpha: Double): TPoint;
var
  sinus, cosinus: Extended;
begin
  sinus := sin(alpha);
  cosinus := cos(alpha);
  Result.x := Round(p.x * cosinus + p.y * sinus);
  Result.y := Round(-p.x * sinus + p.y * cosinus);
end;

function Translate2D(a, b: TPoint): TPoint;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

constructor TMyBarcode.Create(Owner:TComponent);
begin
  inherited Create(owner);
  FAngle := 0.0;
  FRatio := 2.0;
  FModul := 1;
  FTyp := bcEAN13;
  FCheckSum := False;
  FShowText := False;
  FColor := clWhite;
  FColorBar := clBlack;
end;

procedure TMyBarcode.SetModul(v: Integer);
begin
  if (v >= 1) and (v < 50) then FModul := v;
end;

procedure TMyBarcode.OneBarProps(code: Char; var Width: Integer; var lt: TBarLineType);
begin
  case code of
    '0': begin width := modules[0]; lt := white; end;
    '1': begin width := modules[1]; lt := white; end;
    '2': begin width := modules[2]; lt := white; end;
    '3': begin width := modules[3]; lt := white; end;

    '5': begin width := modules[0]; lt := black; end;
    '6': begin width := modules[1]; lt := black; end;
    '7': begin width := modules[2]; lt := black; end;
    '8': begin width := modules[3]; lt := black; end;

    'A': begin width := modules[0]; lt := black_half; end;
    'B': begin width := modules[1]; lt := black_half; end;
    'C': begin width := modules[2]; lt := black_half; end;
    'D': begin width := modules[3]; lt := black_half; end;

    else raise Exception.CreateFmt('%s: internal Error', [Self.ClassName]);
  end;
end;

function TMyBarcode.MakeData: string;
var
  i: Integer;
begin
  MakeModules;
  FText := Trim(FText); {remove blanks}
  for i := 1 to Length(Ftext) do
    if (FText[i] > '9') or (FText[i] < '0') then
      raise Exception.Create('Barcode must be numeric');
  if FTyp = bcEAN13 then Result := Code_EAN13
                    else Result := Code_EAN8;
end;

function TMyBarcode.GetWidth: Integer;
var
  data: string;
  i: Integer;
  w: Integer;
  lt: TBarLineType;
begin
  Result := 0;
  {get barcode pattern}
  data := MakeData;
  for i := 1 to Length(data) do begin {examine the pattern string}
    OneBarProps(data[i], w, lt);
    Inc(Result, w);
  end;
end;

{
////////////////////////////// EAN /////////////////////////////////////////
}

function getEAN(Nr: String): String;
var
  i, fak, sum: Integer;
  tmp: String;
begin
  sum := 0;
  tmp := Copy(nr, 1, Length(Nr) - 1);
  fak := Length(tmp);
  for i := 1 to Length(tmp) do begin
    if (fak mod 2) = 0 then sum := sum + (StrToInt(tmp[i]) * 1)
                       else sum := sum + (StrToInt(tmp[i]) * 3);
    Dec(fak);
  end;
  if (sum mod 10) = 0 then Result := tmp + '0'
                      else Result := tmp + IntToStr(10 - (sum mod 10));
end;

{
////////////////////////////// EAN13 /////////////////////////////////////////
}

{General Pattern for BarCode EAN}
const table_EAN: array['A'..'C', '0'..'9', 1..4] of Char =
 ((('2', '6', '0', '5'),    { 0 }
   ('1', '6', '1', '5'),    { 1 }
   ('1', '5', '1', '6'),    { 2 }
   ('0', '8', '0', '5'),    { 3 }
   ('0', '5', '2', '6'),    { 4 }
   ('0', '6', '2', '5'),    { 5 }
   ('0', '5', '0', '8'),    { 6 }
   ('0', '7', '0', '6'),    { 7 }
   ('0', '6', '0', '7'),    { 8 }
   ('2', '5', '0', '6')),   { 9 }
  (('0', '5', '1', '7'),    { 0 }
   ('0', '6', '1', '6'),    { 1 }
   ('1', '6', '0', '6'),    { 2 }
   ('0', '5', '3', '5'),    { 3 }
   ('1', '7', '0', '5'),    { 4 }
   ('0', '7', '1', '5'),    { 5 }
   ('3', '5', '0', '5'),    { 6 }
   ('1', '5', '2', '5'),    { 7 }
   ('2', '5', '1', '5'),    { 8 }
   ('1', '5', '0', '7')),   { 9 }
  (('7', '1', '5', '0'),    { 0 }
   ('6', '1', '6', '0'),    { 1 }
   ('6', '0', '6', '1'),    { 2 }
   ('5', '3', '5', '0'),    { 3 }
   ('5', '0', '7', '1'),    { 4 }
   ('5', '1', '7', '0'),    { 5 }
   ('5', '0', '5', '3'),    { 6 }
   ('5', '2', '5', '1'),    { 7 }
   ('5', '1', '5', '2'),    { 8 }
   ('7', '0', '5', '1'))    { 9 }
  );

const ParityEAN13: array[0..9, 1..6] of Char =
  (('A', 'A', 'A', 'A', 'A', 'A'),    { 0 }
   ('A', 'A', 'B', 'A', 'B', 'B'),    { 1 }
   ('A', 'A', 'B', 'B', 'A', 'B'),    { 2 }
   ('A', 'A', 'B', 'B', 'B', 'A'),    { 3 }
   ('A', 'B', 'A', 'A', 'B', 'B'),    { 4 }
   ('A', 'B', 'B', 'A', 'A', 'B'),    { 5 }
   ('A', 'B', 'B', 'B', 'A', 'A'),    { 6 }
   ('A', 'B', 'A', 'B', 'A', 'B'),    { 7 }
   ('A', 'B', 'A', 'B', 'B', 'A'),    { 8 }
   ('A', 'B', 'B', 'A', 'B', 'A')     { 9 }
   );

function TMyBarcode.Code_EAN8: string;
var
  i, j: Integer;
  tmp: String;
begin
  tmp := '00000000' + FText;
  if (Length(FText) <> 8) and FCheckSum then tmp := getEAN(Copy(tmp, Length(tmp) - 6, 7) + '0')
                                        else tmp := Copy(tmp, Length(tmp) - 7, 8);

  if Length(tmp) <> 8 then raise Exception.Create('Invalid Text len (EAN8)');

  Result := 'A0A';   {Startcode}
  for i := 1 to 4 do
    for j := 1 to 4 do
      Result := Result + table_EAN['A', tmp[i], j] ;
  Result := Result + '0A0A0';   {Transition}
  for i := 5 to 8 do
    for j := 1 to 4 do
      Result := Result + table_EAN['C', tmp[i], j] ;
  Result := Result + 'A0A';   {Stopcode}
end;

function TMyBarcode.Code_EAN13: string;
var
  i, j, LK: Integer;
  tmp: String;
begin
  tmp := '0000000000000' + FText;
  if (Length(FText) <> 13) and FCheckSum then tmp := getEAN(Copy(tmp, Length(tmp) - 11, 12) + '0')
                                         else tmp := Copy(tmp, Length(tmp) - 12, 13);
  if Length(tmp) <> 13 then raise Exception.Create('Invalid Text len (EAN13)');

  LK := StrToInt(tmp[1]);
  tmp := Copy(tmp,2,12);

  Result := 'A0A';   {Startcode}
  for i := 1 to 6 do
    for j := 1 to 4 do
      Result := Result + table_EAN[ParityEAN13[LK, i], tmp[i], j];
  Result := Result + '0A0A0';   {Transition}
  for i := 7 to 12 do
    for j := 1 to 4 do
      Result := Result + table_EAN['C', tmp[i], j];
  Result := Result + 'A0A';   {Stopcode}
end;

procedure TMyBarcode.MakeModules;
begin
  if Ratio < 2.0 then Ratio := 2.0;
  if Ratio > 3.0 then Ratio := 3.0;

  modules[0] := FModul;
  modules[1] := Round(FModul*FRatio);
  modules[2] := modules[1] * 3 div 2;
  modules[3] := modules[1] * 2;
end;

{
Draw the Barcode

Parameter :
'data' holds the pattern for a Barcode.
A barcode begins always with a black line and
ends with a black line.

The white Lines builds the space between the black Lines.

A black line must always followed by a white Line and vica versa.

Examples:
	'50505'   // 3 thin black Lines with 2 thin white Lines
	'606'     // 2 fat black Lines with 1 thin white Line

	'5605015' // Error


data[] : see procedure OneBarProps

}
procedure TMyBarcode.DoLines(data: string; Canvas: TCanvas);
var
  i: Integer;
  lt: TBarLineType;
  xadd: Integer;
  width, height: Integer;
  a,b,c,d,     {Edges of a line (we need 4 Point because the line}
					{is a recangle}
  orgin: TPoint;
  alpha: Double;

begin
  xadd := 0;
  orgin.x := FLeft;
  orgin.y := FTop;
  alpha := FAngle * pi / 180.0;

  with Canvas do begin
    Pen.Width := 1;
    for i := 1 to Length(data) do begin {examine the pattern string}
      OneBarProps(data[i], width, lt);
      if (lt = black) or (lt = black_half) then Pen.Color := FColorBar
                                           else Pen.Color := FColor;
      Brush.Color := Pen.Color;
      if lt = black_half then height := Variant(FHeight * 1.1)
                         else height := FHeight;
      a.x := xadd;
      a.y := 0;
      b.x := xadd;
      b.y := height;
      {c.x := xadd+width;}
      c.x := xadd + Width - 1;  {23.04.1999 Line was 1 Pixel too wide}
      c.y := Height;
      {d.x := xadd+width;}
      d.x := xadd + Width - 1;  {23.04.1999 Line was 1 Pixel too wide}
      d.y := 0;

      {a,b,c,d builds the rectangle we want to draw}
      {rotate the rectangle}
      a := Translate2D(Rotate2D(a, alpha), orgin);
      b := Translate2D(Rotate2D(b, alpha), orgin);
      c := Translate2D(Rotate2D(c, alpha), orgin);
      d := Translate2D(Rotate2D(d, alpha), orgin);
      {draw the rectangle}
      Polygon([a,b,c,d]);
      xadd := xadd + width;
    end;
  end;
end;

procedure TMyBarcode.DrawBarcode(Canvas: TCanvas);
var
  data: string;
  SaveFont: TFont;
  SavePen: TPen;
  SaveBrush: TBrush;
begin
  Savefont := TFont.Create;
  SavePen := TPen.Create;
  SaveBrush := TBrush.Create;
  data := MakeData;
  try
    Savefont.Assign(Canvas.Font);
    SavePen.Assign(Canvas.Pen);
    SaveBrush.Assign(Canvas.Brush);
    DoLines(data, Canvas);
    if FShowText then DrawText(Canvas);
    Canvas.Font.Assign(savefont);
    Canvas.Pen.Assign(SavePen);
    Canvas.Brush.Assign(SaveBrush);
  finally
    Savefont.Free;
    SavePen.Free;
    SaveBrush.Free;
  end;
end;

{
	draw contents and type/name of barcode
	as human readable text at the left
	upper edge of the barcode.

	main use for this procedure is testing.

	note: this procedure changes Pen and Brush
	of the current canvas.
}
procedure TMyBarcode.DrawText(Canvas:TCanvas);
begin
  with Canvas do begin
    Font.Size := 4;
    {the fixed font size is a problem, if you
     use very large or small barcodes}

    Pen.Color := clBlack;
    Brush.Color := clWhite;
    TextOut(FLeft, FTop, FText);
  end;
end;

procedure Register;
begin
  RegisterComponents('Tetram', [TMyBarCode]);
end;

end.
