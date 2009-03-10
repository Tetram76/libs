unit EvalMath;

interface

uses EvalIntf;

type
  TSqrOperator = class(TOperator)
  public
    function OperatorKind: TOperatorKind; override;
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TFactOperator = class(TOperator)
  public
    function OperatorKind: TOperatorKind; override;
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TPowerOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  {}

  TTruncFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TRoundFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TFloatFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
  end;

  TAbsFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TFracFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TSqrtFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TCosFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TSinFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TTanFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TArcCosFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TArcSinFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TArcTanFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TCosHFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TSinHFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TTanHFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TArcCosHFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TArcSinHFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TArcTanHFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TRadToDegFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TDegToRadFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TExpFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TLnFunction = class(TFloatFunction)
  public
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TMinFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TMaxFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  procedure RegisterSymbols;
  procedure UnRegisterAll;

implementation

uses Math;

{ TSqrOperator }

function TSqrOperator.OperatorKind: TOperatorKind;
begin
  Result:= okUnaryLeft;
end;

function TSqrOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if Type1 in [etInteger, etFloat] then
    Result:= Type1
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TSqrOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1: TExpressionType;
begin
  T1:= Expr1.ExprType;
  if T1 = etInteger then
    Result.AsInteger:= Expr1.AsInteger * Expr1.AsInteger
  else if T1 = etFloat then
    Result.AsFloat:= Expr1.AsFloat * Expr1.AsFloat
  else if T1 = etVariant then
    Result.AsVariant:= Expr1.AsVariant * Expr1.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TFactOperator }

function TFactOperator.OperatorKind: TOperatorKind;
begin
  Result:= okUnaryLeft;
end;

function TFactOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if Type1 = etInteger then
    Result:= etFloat
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TFactOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1: TExpressionType;
    i: Integer;
    R: Extended;
begin
  T1:= Expr1.ExprType;
  if T1 in [etInteger, etVariant] then
  begin
    R:= 1;
    for i:= Expr1.AsInteger downto 1 do
      R:= R * i;
    Result.AsFloat:= R;
  end else
    Error(ErrorIncorrectOperation);
end;

{ TPowerOperator }

function TPowerOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etFloat
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TPowerOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr1.ExprType;
  if (T1 in [etInteger, etFloat, etVariant]) and (T2 in [etInteger, etFloat, etVariant]) then
    Result.AsFloat:= Power(Expr1.AsFloat, Expr2.AsFloat)
  else
    Error(ErrorIncorrectOperation);
end;

{ TTruncFunction }

function TTruncFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etInteger;
end;

procedure TTruncFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsInteger:= Trunc(Parameters^[1].AsFloat);
end;

{ TRoundFunction }

function TRoundFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etInteger;
end;

procedure TRoundFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsInteger:= Round(Parameters^[1].AsFloat);
end;

{ TFloatFunction }

function TFloatFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etFloat;
  Result.Result:= etFloat;
end;

{ TAbsFunction }

procedure TAbsFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Abs(Parameters^[1].AsFloat);
end;

{ TFracFunction }

procedure TFracFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Frac(Parameters^[1].AsFloat);
end;

{ TSqrtFunction }

procedure TSqrtFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Sqrt(Parameters^[1].AsFloat);
end;

{ TCosFunction }

procedure TCosFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Cos(Parameters^[1].AsFloat);
end;

{ TSinFunction }

procedure TSinFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Sin(Parameters^[1].AsFloat);
end;

{ TTanFunction }

procedure TTanFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Tan(Parameters^[1].AsFloat);
end;

{ TArcCosFunction }

procedure TArcCosFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= ArcCos(Parameters^[1].AsFloat);
end;

{ TArcSinFunction }

procedure TArcSinFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= ArcSin(Parameters^[1].AsFloat);
end;

{ TArcTanFunction }

procedure TArcTanFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= ArcTan(Parameters^[1].AsFloat);
end;

{ TCosHFunction }

procedure TCosHFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= CosH(Parameters^[1].AsFloat);
end;

{ TSinHFunction }

procedure TSinHFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= SinH(Parameters^[1].AsFloat);
end;

{ TTanHFunction }

procedure TTanHFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= TanH(Parameters^[1].AsFloat);
end;

{ TArcCosHFunction }

procedure TArcCosHFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= ArcCosH(Parameters^[1].AsFloat);
end;

{ TArcSinHFunction }

procedure TArcSinHFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= ArcSinH(Parameters^[1].AsFloat);
end;

{ TArcTanHFunction }

procedure TArcTanHFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= ArcTanH(Parameters^[1].AsFloat);
end;

{ TExpFunction }

procedure TExpFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Exp(Parameters^[1].AsFloat);
end;

{ TLnFunction }

procedure TLnFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  Result.AsFloat:= Ln(Parameters^[1].AsFloat);
end;

{ TDegToRadFunction }

procedure TDegToRadFunction.SolveFunction(Parameters: PExpressionArray;
  Result: TExpression);
begin
  Result.AsFloat:= DegToRad(Parameters^[1].AsFloat);
end;

{ TRadToDegFunction }

procedure TRadToDegFunction.SolveFunction(Parameters: PExpressionArray;
  Result: TExpression);
begin
  Result.AsFloat:= RadToDeg(Parameters^[1].AsFloat);
end;


{ TMinFunction }

function TMinFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 2;
  Result.Parameters[1]:= etFloat;
  Result.Parameters[2]:= etFloat;
  Result.Result:= etFloat;
end;

procedure TMinFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var V1, V2: Extended;
begin
  V1:= Parameters^[1].AsFloat;
  V2:= Parameters^[2].AsFloat;
  if V2 < V1 then
    V1:= V2;
  Result.AsFloat:= V1;
end;

{ TMaxFunction }

function TMaxFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 2;
  Result.Parameters[1]:= etFloat;
  Result.Parameters[2]:= etFloat;
  Result.Result:= etFloat;
end;

procedure TMaxFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var V1, V2: Extended;
begin
  V1:= Parameters^[1].AsFloat;
  V2:= Parameters^[2].AsFloat;
  if V2 > V1 then
    V1:= V2;
  Result.AsFloat:= V1;
end;

procedure RegisterSymbols;
begin
  RegisterGlobalOperator('²', opHighest, TSqrOperator);
  RegisterGlobalOperator('!', opHighest, TFactOperator);
  RegisterGlobalOperator('^', opHighest, TPowerOperator);

  RegisterGlobalVariable('PI', TExpression.CreateFloat(PI));

  RegisterGlobalFunction('TRUNC', TTruncFunction);
  RegisterGlobalFunction('ROUND', TRoundFunction);
  RegisterGlobalFunction('ABS', TAbsFunction);
  RegisterGlobalFunction('FRAC', TFracFunction);
  RegisterGlobalFunction('SQRT', TSqrtFunction);
  RegisterGlobalFunction('COS', TCosFunction);
  RegisterGlobalFunction('SIN', TSinFunction);
  RegisterGlobalFunction('TAN', TTanFunction);
  RegisterGlobalFunction('ARCCOS', TArcCosFunction);
  RegisterGlobalFunction('ARCSIN', TArcSinFunction);
  RegisterGlobalFunction('ARCTAN', TArcTanFunction);
  RegisterGlobalFunction('COSH', TCosHFunction);
  RegisterGlobalFunction('SINH', TSinHFunction);
  RegisterGlobalFunction('TANH', TTanHFunction);
  RegisterGlobalFunction('ARCCOSH', TArcCosHFunction);
  RegisterGlobalFunction('ARCSINH', TArcSinHFunction);
  RegisterGlobalFunction('ARCTANH', TArcTanHFunction);
  RegisterGlobalFunction('EXP', TExpFunction);
  RegisterGlobalFunction('LN', TLnFunction);
  RegisterGlobalFunction('MIN', TMinFunction);
  RegisterGlobalFunction('MAX', TMaxFunction);
  RegisterGlobalFunction('DEGTORAD', TDegToRadFunction);
  RegisterGlobalFunction('RADTODEG', TRadToDegFunction);
end;

procedure UnRegisterAll;
begin
  GlobalUnRegister('²');
  GlobalUnRegister('!');
  GlobalUnRegister('^');
  GlobalUnRegister('PI');
  GlobalUnRegister('TRUNC');
  GlobalUnRegister('ROUND');
  GlobalUnRegister('ABS');
  GlobalUnRegister('FRAC');
  GlobalUnRegister('SQRT');
  GlobalUnRegister('COS');
  GlobalUnRegister('SIN');
  GlobalUnRegister('TAN');
  GlobalUnRegister('ARCCOS');
  GlobalUnRegister('ARCSIN');
  GlobalUnRegister('ARCTAN');
  GlobalUnRegister('COSH');
  GlobalUnRegister('SINH');
  GlobalUnRegister('TANH');
  GlobalUnRegister('ARCCOSH');
  GlobalUnRegister('ARCSINH');
  GlobalUnRegister('ARCTANH');
  GlobalUnRegister('EXP');
  GlobalUnRegister('LN');
  GlobalUnRegister('MIN');
  GlobalUnRegister('MAX');
end;

initialization
  RegisterSymbols;

end.
