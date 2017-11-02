unit EvalUser;

interface

uses EvalIntf;

type
  { Type conversion functions }

  TStringFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TIntegerFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TFloatFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TBoolFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  { Custom functions/variables interface }

  TFloatMethod = function: Extended of object;
  TBoolMethod = function: Boolean of object;
  TIntegerMethod = function: Integer of object;
  TStringMethod = function: string of object;
  TVariantMethod = function: Variant of object;

  TFloatMethodSymbol = class(TExpression)
  private
    FMethod: TFloatMethod;
  protected
    function GetAsFloat: Extended; override;
  public
    constructor Create(Method: TFloatMethod);
  end;

  TBoolMethodSymbol = class(TExpression)
  private
    FMethod: TBoolMethod;
  protected
    function GetAsBoolean: Boolean; override;
  public
    constructor Create(Method: TBoolMethod);
  end;

  TIntegerMethodSymbol = class(TExpression)
  private
    FMethod: TIntegerMethod;
  protected
    function GetAsInteger: Integer; override;
  public
    constructor Create(Method: TIntegerMethod);
  end;

  TStringMethodSymbol = class(TExpression)
  private
    FMethod: TStringMethod;
  protected
    function GetAsString: string; override;
  public
    constructor Create(Method: TStringMethod);
  end;

  TVariantMethodSymbol = class(TExpression)
  private
    FMethod: TVariantMethod;
  protected
    function GetAsVariant: Variant; override;
  public
    constructor Create(Method: TVariantMethod);
  end;

  { Date functions }

  TDateFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TTimeFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TNowFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  { Date functions }

  TYearFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TMonthFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TDayFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TDayOfWeekFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  { string functions }

  TLengthFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TCopyFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TChrFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TDateToStrFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TTimeToStrFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TFormatFloatFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TIntToHexFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  THexToIntFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  procedure RegisterSymbols;
  procedure UnRegisterAll;

implementation

uses SysUtils;

  { Custom method procs }

constructor TFloatMethodSymbol.Create(Method: TFloatMethod);
begin
  FMethod:= Method;
  SetExpressionType(etFloat);
end;

function TFloatMethodSymbol.GetAsFloat: Extended;
begin
  Result:= FMethod;
end;

constructor TBoolMethodSymbol.Create(Method: TBoolMethod);
begin
  FMethod:= Method;
  SetExpressionType(etBoolean);
end;

function TBoolMethodSymbol.GetAsBoolean: Boolean;
begin
  Result:= FMethod;
end;

constructor TIntegerMethodSymbol.Create(Method: TIntegerMethod);
begin
  FMethod:= Method;
  SetExpressionType(etInteger);
end;

function TIntegerMethodSymbol.GetAsInteger: Integer;
begin
  Result:= FMethod;
end;

constructor TStringMethodSymbol.Create(Method: TStringMethod);
begin
  FMethod:= Method;
  SetExpressionType(etString);
end;

function TStringMethodSymbol.GetAsString: string;
begin
  Result:= FMethod;
end;

constructor TVariantMethodSymbol.Create(Method: TVariantMethod);
begin
  FMethod:= Method;
  SetExpressionType(etVariant);
end;

function TVariantMethodSymbol.GetAsVariant: Variant;
begin
  Result:= FMethod;
end;

  { Date functions }

{ TDateFunction }

function TDateFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 0;
  Result.Result:= etFloat;
end;

procedure TDateFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Date;
end;

{ TTimeFunction }

function TTimeFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 0;
  Result.Result:= etFloat;
end;

procedure TTimeFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Time;
end;

{ TNowFunction }

function TNowFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 0;
  Result.Result:= etFloat;
end;

procedure TNowFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Now;
end;

{ TYearFunction }

function TYearFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etInteger;
end;

procedure TYearFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var Y, M, D: Word;
begin
  DecodeDate(Parameters^[1].AsFloat, Y, M, D);
  Result.AsInteger:= Y;
end;

{ TMonthFunction }

function TMonthFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etInteger;
end;

procedure TMonthFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var Y, M, D: Word;
begin
  DecodeDate(Parameters^[1].AsFloat, Y, M, D);
  Result.AsInteger:= M;
end;

{ TDayFunction }

function TDayFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etInteger;
end;

procedure TDayFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var Y, M, D: Word;
begin
  DecodeDate(Parameters^[1].AsFloat, Y, M, D);
  Result.AsInteger:= D;
end;

{ TDayOfWeekFunction }

function TDayOfWeekFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etInteger;
end;

procedure TDayOfWeekFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsInteger:= DayOfWeek(Parameters^[1].AsFloat);
end;

{ TStringFunction }

function TStringFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etVariant;
  Result.Result:= etString;
end;

procedure TStringFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= Parameters^[1].AsString;
end;

{ TIntegerFunction }

function TIntegerFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etVariant;
  Result.Result:= etInteger;
end;

procedure TIntegerFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsInteger:= Parameters^[1].AsInteger;
end;

{ TFloatFunction }

function TFloatFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etVariant;
  Result.Result:= etFloat;
end;

procedure TFloatFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Parameters^[1].AsFloat;
end;

{ TBoolFunction }

function TBoolFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etVariant;
  Result.Result:= etBoolean;
end;

procedure TBoolFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsBoolean:= Parameters^[1].AsBoolean;
end;

{ TLengthFunction }

function TLengthFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etString;
  Result.Result:= etInteger;
end;

procedure TLengthFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsInteger:= Length(Parameters^[1].AsString);
end;

{ TCopyFunction }

function TCopyFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 3;
  Result.Parameters[1]:= etString;
  Result.Parameters[2]:= etInteger;
  Result.Parameters[3]:= etInteger;
  Result.Result:= etString;
end;

procedure TCopyFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= Copy(Parameters^[1].AsString,
                          Parameters^[2].AsInteger,
                          Parameters^[3].AsInteger);
end;

{ TChrFunction }

function TChrFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etInteger;
  Result.Result:= etString;
end;

procedure TChrFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= Char(Parameters^[1].AsInteger);
end;

{ TDateToStrFunction }

function TDateToStrFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etString;
end;

procedure TDateToStrFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= FormatDateTime('dd/mm/yyyy', Parameters^[1].AsFloat);
end;

{ TTimeToStrFunction }

function TTimeToStrFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etString;
end;

procedure TTimeToStrFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= TimeToStr(Parameters^[1].AsFloat);
end;

{ TFormatFloatFunction }

function TFormatFloatFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 2;
  Result.Parameters[1]:= etString;
  Result.Parameters[2]:= etFloat;
  Result.Result:= etString;
end;

procedure TFormatFloatFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= FormatFloat(Parameters^[1].AsString, Parameters^[2].AsFloat);
end;

{ TIntToHexFunction }

function TIntToHexFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etInteger;
  Result.Result:= etString;
end;

procedure TIntToHexFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsString:= IntToHex(Parameters^[1].AsInteger, 8);
end;

{ THexToIntFunction }

function THexToIntFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etString;
  Result.Result:= etInteger;
end;

procedure THexToIntFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsInteger:= StrToInt('$' + Parameters^[1].AsString);
end;

procedure RegisterSymbols;
begin
  RegisterGlobalFunction('DATE', TDateFunction);
  RegisterGlobalFunction('TIME', TTimeFunction);
  RegisterGlobalFunction('NOW', TNowFunction);
  RegisterGlobalFunction('YEAR', TYearFunction);
  RegisterGlobalFunction('MONTH', TMonthFunction);
  RegisterGlobalFunction('DAY', TDayFunction);
  RegisterGlobalFunction('DAYOFWEEK', TDayOfWeekFunction);
  RegisterGlobalFunction('STRING', TStringFunction);
  RegisterGlobalFunction('INTEGER', TIntegerFunction);
  RegisterGlobalFunction('FLOAT', TFloatFunction);
  RegisterGlobalFunction('BOOL', TBoolFunction);
  RegisterGlobalFunction('COPY', TCopyFunction);
  RegisterGlobalFunction('CHR', TChrFunction);
  RegisterGlobalFunction('DATETOSTR', TDateToStrFunction);
  RegisterGlobalFunction('TIMETOSTR', TTimeToStrFunction);
  RegisterGlobalFunction('FORMATFLOAT', TFormatFloatFunction);
  RegisterGlobalFunction('INTTOHEX', TIntToHexFunction);
  RegisterGlobalFunction('HEXTOINT', THexToIntFunction);
  RegisterGlobalFunction('LENGTH', TLengthFunction);
end;

procedure UnRegisterAll;
begin
  GlobalUnregister('DATE');
  GlobalUnregister('TIME');
  GlobalUnregister('NOW');
  GlobalUnregister('YEAR');
  GlobalUnregister('MONTH');
  GlobalUnregister('DAY');
  GlobalUnregister('STRING');
  GlobalUnregister('INTEGER');
  GlobalUnregister('FLOAT');
  GlobalUnregister('BOOL');
  GlobalUnregister('COPY');
  GlobalUnregister('CHR');
  GlobalUnregister('DATETOSTR');
  GlobalUnregister('TIMETOSTR');
  GlobalUnregister('FORMATFLOAT');
  GlobalUnregister('INTTOHEX');
  GlobalUnregister('HEXTOINT');
  GlobalUnregister('LENGTH');
end;

initialization
  RegisterSymbols;

end.
