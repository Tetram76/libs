unit EvalClasses;

interface

{$I GrizzlyDefine.INC}

uses EvalIntf
     {$IFDEF GZ_D6}, Variants{$ENDIF};

type
  TAddOperator = class(TOperator)
  public
    function DualOperator: Boolean; override;
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TSubOperator = class(TOperator)
  public
    function DualOperator: Boolean; override;
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TMulOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TDivOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TEqualOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TDifferentOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TInferiorOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TSuperiorOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TInferiorEqualOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TSuperiorEqualOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TAndOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TOrOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TXorOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TIntDivOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TModOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TShlOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TShrOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TNotOperator = class(TOperator)
  public
    function OperatorKind: TOperatorKind; override;
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TAssignOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TIfFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TEvalFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TUserSymbol = class(TExpression)
  private
    FExpression: TExpression;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsFloat: Extended; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
  public
    constructor CreateUser(Expression: TExpression);
    destructor Destroy; override;
  end;

  TDeclareFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  TUnDeclareFunction = class(TFunctionExpression)
  public
    function GetDeclaration: TFunctionDeclaration; override;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); override;
  end;

  procedure RegisterSymbols;
  procedure UnRegisterAll;

implementation

uses SysUtils;

const
  ErrorMustAssignToVar = 'L''opérateur de gauche doit être une variable pour assignation';

  { Operators }

{ TAddOperator }

function TAddOperator.DualOperator: Boolean;
begin
  Result:= True;
end;

function TAddOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type2 = etError) and (Type1 in [etInteger, etFloat, etString, etVariant]) then
    Result:= Type1
  else if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etFloat
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etString
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TAddOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  if Expr2 = nil then
  begin
    case T1 of
      etInteger: Result.AsInteger:= Expr1.AsInteger;
      etFloat: Result.AsFloat:= Expr1.AsFloat;
      etString: Result.AsString:= Expr1.AsString;
      etVariant: Result.AsVariant:= Expr1.AsVariant;
      else Error(ErrorIncorrectOperation);
    end;
  end else begin
    T2:= Expr2.ExprType;
    if (T1 = etInteger) and (T2 = etInteger) then
      Result.AsInteger:= Expr1.AsInteger + Expr2.AsInteger
    else if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
      Result.AsFloat:= Expr1.AsFloat + Expr2.AsFloat
    else if (T1 = etString) and (T2 = etString) then
      Result.AsString:= Expr1.AsString + Expr2.AsString
    else if (T1 = etVariant) or (T2 = etVariant) then
      Result.AsVariant:= Expr1.AsVariant + Expr2.AsVariant
    else
      Error(ErrorIncorrectOperation);
  end;
end;

{ TSubOperator }

function TSubOperator.DualOperator: Boolean;
begin
  Result:= True;
end;

function TSubOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type2 = etError) and (Type1 in [etInteger, etFloat, etVariant]) then
    Result:= Type1
  else if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etFloat
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TSubOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  if Expr2 = nil then
  begin
    case T1 of
      etInteger: Result.AsInteger:= - Expr1.AsInteger;
      etFloat: Result.AsFloat:= - Expr1.AsFloat;
      etVariant: Result.AsVariant:= - Expr1.AsVariant;
      else Error(ErrorIncorrectOperation);
    end;
  end else begin
    T2:= Expr2.ExprType;
    if (T1 = etInteger) and (T2 = etInteger) then
      Result.AsInteger:= Expr1.AsInteger - Expr2.AsInteger
    else if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
      Result.AsFloat:= Expr1.AsFloat - Expr2.AsFloat
    else if (T1 = etVariant) or (T2 = etVariant) then
      Result.AsVariant:= Expr1.AsVariant - Expr2.AsVariant
    else
      Error(ErrorIncorrectOperation);
  end;
end;

{ TMulOperator }

function TMulOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etFloat
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TMulOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger * Expr2.AsInteger
  else if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsFloat:= Expr1.AsFloat * Expr2.AsFloat
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant * Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TDivOperator }

function TDivOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etFloat
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TDivOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsFloat:= Expr1.AsFloat / Expr2.AsFloat
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant / Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TEqual }

function TEqualOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etBoolean
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TEqualOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsBoolean:= Expr1.AsFloat = Expr2.AsFloat
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean = Expr2.AsBoolean
  else if (T1 = etString) and (T2 = etString) then
    Result.AsBoolean:= Expr1.AsString = Expr2.AsString
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant = Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TDifferentOperator }

function TDifferentOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etBoolean
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TDifferentOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsBoolean:= Expr1.AsFloat <> Expr2.AsFloat
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean <> Expr2.AsBoolean
  else if (T1 = etString) and (T2 = etString) then
    Result.AsBoolean:= Expr1.AsString <> Expr2.AsString
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant <> Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TInferiorOperator }

function TInferiorOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etBoolean
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TInferiorOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsBoolean:= Expr1.AsFloat < Expr2.AsFloat
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean < Expr2.AsBoolean
  else if (T1 = etString) and (T2 = etString) then
    Result.AsBoolean:= Expr1.AsString < Expr2.AsString
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant < Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TSuperiorOperator }

function TSuperiorOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etBoolean
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TSuperiorOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsBoolean:= Expr1.AsFloat > Expr2.AsFloat
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean > Expr2.AsBoolean
  else if (T1 = etString) and (T2 = etString) then
    Result.AsBoolean:= Expr1.AsString > Expr2.AsString
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant > Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TInferiorEqualOperator }

function TInferiorEqualOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etBoolean
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TInferiorEqualOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsBoolean:= Expr1.AsFloat <= Expr2.AsFloat
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean <= Expr2.AsBoolean
  else if (T1 = etString) and (T2 = etString) then
    Result.AsBoolean:= Expr1.AsString <= Expr2.AsString
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant <= Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TSuperiorEqualOperator }

function TSuperiorEqualOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 in [etInteger, etFloat]) and (Type2 in [etInteger, etFloat]) then
    Result:= etBoolean
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etString) and (Type2 = etString) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TSuperiorEqualOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 in [etInteger, etFloat]) and (T2 in [etInteger, etFloat]) then
    Result.AsBoolean:= Expr1.AsFloat >= Expr2.AsFloat
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean >= Expr2.AsBoolean
  else if (T1 = etString) and (T2 = etString) then
    Result.AsBoolean:= Expr1.AsString >= Expr2.AsString
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant >= Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TAndOperator }

function TAndOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TAndOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger and Expr2.AsInteger
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean and Expr2.AsBoolean
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant and Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TOrOperator }

function TOrOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TOrOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger or Expr2.AsInteger
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean or Expr2.AsBoolean
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant or Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TXorOperator }

function TXorOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TXorOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger xor Expr2.AsInteger
  else if (T1 = etBoolean) and (T2 = etBoolean) then
    Result.AsBoolean:= Expr1.AsBoolean xor Expr2.AsBoolean
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant xor Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TIntDivOperator }

function TIntDivOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TIntDivOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger div Expr2.AsInteger
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant div Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TModOperator }

function TModOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TModOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger mod Expr2.AsInteger
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant mod Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TShlOperator }

function TShlOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TShlOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger shl Expr2.AsInteger
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant shl Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TShrOperator }

function TShrOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TShrOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1, T2: TExpressionType;
begin
  T1:= Expr1.ExprType;
  T2:= Expr2.ExprType;
  if (T1 = etInteger) and (T2 = etInteger) then
    Result.AsInteger:= Expr1.AsInteger shr Expr2.AsInteger
  else if (T1 = etVariant) or (T2 = etVariant) then
    Result.AsVariant:= Expr1.AsVariant shr Expr2.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TNotOperator }

function TNotOperator.OperatorKind: TOperatorKind;
begin
  Result:= okUnaryRight;
end;

function TNotOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if Type1 in [etInteger, etBoolean] then
    Result:= Type1
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TNotOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
var T1: TExpressionType;
begin
  T1:= Expr1.ExprType;
  if T1 = etInteger then
    Result.AsInteger:= not Expr1.AsInteger
  else if T1 = etBoolean then
    Result.AsBoolean:= not Expr1.AsBoolean
  else if T1 = etVariant then
    Result.AsVariant:= not Expr1.AsVariant
  else
    Error(ErrorIncorrectOperation);
end;

{ TAssignOperator }

function TAssignOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  Result:= Type1;
end;

procedure TAssignOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if Expr1.ClassType <> TVarExpression then
    Error(ErrorMustAssignToVar);
  Expr1.AsVariant:= Expr2.AsVariant;
  Result.AsVariant:= Expr1.AsVariant;
end;

{ Functions }

function TIfFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 3;
  Result.Parameters[1]:= etBoolean;
  Result.Parameters[2]:= etVariant;
  Result.Parameters[3]:= etVariant;
  Result.Result:= etVariant;
end;

procedure TIfFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  if Parameters^[1].AsBoolean then
    Result.AsVariant:= Parameters^[2].AsVariant
  else
    Result.AsVariant:= Parameters^[3].AsVariant;
end;

{ TEvalFunction }

function TEvalFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etString;
  Result.Result:= etVariant;
end;

procedure TEvalFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var Ev: TExpressionEvaluator;
begin
  Ev:= TExpressionEvaluator.Create(GetOwner, TEvaluator(GetOwner).LocalSymbols);
  try
    Ev.Evaluate(Parameters^[1].AsString);
    Result.AsVariant:= Ev.Result.AsVariant;
  finally
    Ev.Free;
  end;
end;

{ TUserSymbol }

constructor TUserSymbol.CreateUser(Expression: TExpression);
begin
  inherited Create(Expression.ExprType);
  FExpression:= Expression;
end;

destructor TUserSymbol.Destroy;
begin
  FreeExpression(FExpression); { If the expression is a symbol, we must keep it } 
  inherited Destroy;
end;

function TUserSymbol.GetAsBoolean: Boolean;
begin
  Result:= FExpression.AsBoolean;
end;

function TUserSymbol.GetAsInteger: Integer;
begin
  Result:= FExpression.AsInteger;
end;

function TUserSymbol.GetAsFloat: Extended;
begin
  Result:= FExpression.AsFloat;
end;

function TUserSymbol.GetAsString: string;
begin
  Result:= FExpression.AsString;
end;

function TUserSymbol.GetAsVariant: Variant;
begin
  Result:= FExpression.AsVariant;
end;

{ TDeclareFunction }

function TDeclareFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 2;
  Result.Parameters[1]:= etString; { Symbol name }
  Result.Parameters[2]:= etString; { Expression that will be sub-evaluated }
  Result.Result:= etString; { Result will be an error message if it could not be registered }
end;

procedure TDeclareFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
var Ev: TExpressionEvaluator;
    Expr: TUserSymbol;
begin
  Ev:= TExpressionEvaluator.Create(GetOwner, TEvaluator(GetOwner).LocalSymbols);
  try
    Ev.Evaluate(Parameters^[2].AsString);
    Expr:= TUserSymbol.CreateUser(Ev.ExtractResult);
    try
      TEvaluator(GetOwner).RegisterVariable(Parameters^[1].AsString, Expr);
      Result.AsString:= '';
    except
      on E: Exception do
        Result.AsString:= E.Message;
    end;
  finally
    Ev.Free;
  end;
end;

{ TUnDeclareFunction }

function TUnDeclareFunction.GetDeclaration: TFunctionDeclaration;
begin
  Result.ParamCount:= 1;
  Result.Parameters[1]:= etString; { Symbol name to undeclare }
  Result.Result:= etString; { Result will be an error message if it could not be registered }
end;

procedure TUnDeclareFunction.SolveFunction(Parameters: PExpressionArray; Result: TExpression);
begin
  try
    TEvaluator(GetOwner).UnRegisterSymbol(Parameters^[1].AsString);
    Result.AsString:= '';
  except
    on E: Exception do
      Result.AsString:= E.Message;
  end;
end;

procedure RegisterSymbols;
begin
  RegisterGlobalOperator('+', opMedium, TAddOperator);
  RegisterGlobalOperator('-', opMedium, TSubOperator);
  RegisterGlobalOperator('*', opHigh, TMulOperator);
  RegisterGlobalOperator('/', opHigh, TDivOperator);
  RegisterGlobalOperator('=', opLow, TEqualOperator);
  RegisterGlobalOperator('<>', opLow, TDifferentOperator);
  RegisterGlobalOperator('<', opLow, TInferiorOperator);
  RegisterGlobalOperator('>', opLow, TSuperiorOperator);
  RegisterGlobalOperator('<=', opLow, TInferiorEqualOperator);
  RegisterGlobalOperator('>=', opLow, TSuperiorEqualOperator);
  RegisterGlobalOperator('AND', opMedium, TAndOperator);
  RegisterGlobalOperator('OR', opLow, TOrOperator);
  RegisterGlobalOperator('XOR', opLow, TXorOperator);
  RegisterGlobalOperator('DIV', opHigh, TIntDivOperator);
  RegisterGlobalOperator('MOD', opHigh, TModOperator);
  RegisterGlobalOperator('SHL', opHigh, TShlOperator);
  RegisterGlobalOperator('SHR', opHigh, TShrOperator);

  RegisterGlobalOperator('NOT', opHighest, TNotOperator);

  RegisterGlobalOperator(':=', opLowest, TAssignOperator);

  {}

  RegisterGlobalVariable(STrue, TExpression.CreateBoolean(True));
  RegisterGlobalVariable(SFalse, TExpression.CreateBoolean(False));
  RegisterGlobalVariable(SNull, TExpression.CreateVariant(Null));

  {}

  RegisterGlobalFunction('IF', TIfFunction);
  RegisterGlobalFunction('EVAL', TEvalFunction);
  RegisterGlobalFunction('DECLARE', TDeclareFunction);
  RegisterGlobalFunction('UNDECLARE', TUnDeclareFunction);
end;

procedure UnregisterAll;
begin
  GlobalUnregister('+');
  GlobalUnregister('-');
  GlobalUnregister('*');
  GlobalUnregister('/');
  GlobalUnregister('=');
  GlobalUnregister('<>');
  GlobalUnregister('<');
  GlobalUnregister('>');
  GlobalUnregister('<=');
  GlobalUnregister('>=');
  GlobalUnregister('AND');
  GlobalUnregister('OR');
  GlobalUnregister('XOR');
  GlobalUnregister('DIV');
  GlobalUnregister('MOD');
  GlobalUnregister('SHL');
  GlobalUnregister('SHR');
  GlobalUnregister('NOT');
  GlobalUnregister(':=');
  GlobalUnregister(STrue);
  GlobalUnregister(SFalse);
  GlobalUnregister('IF');
  GlobalUnregister('EVAL');
  GlobalUnregister('DECLARE');
  GlobalUnregister('UNDECLARE');
end;

initialization
  RegisterSymbols;

end.
