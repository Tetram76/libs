{***************************************************************
 *
 * Unit Name: EvalIntf
 * Purpose  : Full expression evaluator. Entirely parametrable => You can freely
              declare your own variables, functions or operators.
              A library is already given through the following units :
                EvalClasses, EvalMath, EvalUser and EvalDB. 
 * Author   : Alexandre GUILLIEN
 * History  :
 *
 *  01/07/2000 : V 1.2  : First release.
 *
 ****************************************************************}

{$I GrizzlyDefine.INC}

unit EvalIntf;

interface

uses Classes, SysUtils, Parser {$IFDEF GZ_D6}, Variants{$ENDIF};

type
  TExpressionType = (etBoolean, etInteger, etFloat, etString, etVariant, etSymbol, etError);
  TSymbolKind = (skOperator, skFunction, skVariable, skError);

{ Evaluation Automaton :
    4 states :
      0 = (None)
      1 = (Expression)
      2 = (OpRight)
      3 = (OpBinary)
      4 = (OpLeft)
      5 = (Finished)
    0 => 1, 2, 5
    1 => 3, 4, 5
    2 => 1, 2
    3 => 1, 2
    4 => 3, 4, 5
    5 => End of evaluation
}
  TEvaluationState = (esNone, esExpression, esOpRight, esOpBinary, esOpLeft, esFinished, esError);
  TEvaluationResult = (erOk, erError);

const
  AllowedStates: array[esNone..esOpLeft] of set of TEvaluationState =
    ([esExpression, esOpRight], [esOpBinary, esOpLeft, esFinished], [esExpression, esOpRight],
     [esExpression, esOpRight], [esOpBinary, esOpLeft, esFinished]);

const
  DecimalChar = '.';
  IgnoredChars = [' ', #13, #10];
  SymbolChars = ['0'..'9', 'A'..'Z', 'a'..'z', '_'];
  OperatorChars = ['²', '&', '~', '#', '|', '-', '\', '^', '=', '+', '*', '%',
    ':', '/', '!', '@', '<', '>'];
  FreeSymbolChars = [#32..#255];
  NumericChars = ['0'..'9', DecimalChar];
  StringChar = '''';
  BeginParenthesis = '(';
  EndParenthesis = ')';
  ParenthesisChars = [BeginParenthesis, EndParenthesis];
  ParamSeparator = ',';
  BeginFreeSymbol = '[';
  EndFreeSymbol = ']';
  FreeSymbolDelimiters = [BeginFreeSymbol, EndFreeSymbol];

  SubEvaluationEndChars = [EndParenthesis, ParamSeparator];

var
  STrue: string = 'True';
  SFalse: string = 'False';
  SNull: string = 'Null';

type
  EExpressionError = class(Exception);

  TExpression = class(TPersistent)
  private
    FExprType: TExpressionType;
    FValue: Pointer;
  protected
    procedure FreeValue;
    procedure SetExpressionType(ExprType: TExpressionType);
    {}
    function GetExprType: TExpressionType; virtual;
    {}
    function GetAsBoolean: Boolean; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsFloat: Extended; virtual;
    function GetAsString: string; virtual;
    function GetAsVariant: Variant; virtual;
    {}
    procedure SetAsBoolean(Value: Boolean); virtual;
    procedure SetAsInteger(Value: Integer); virtual;
    procedure SetAsFloat(Value: Extended); virtual;
    procedure SetAsString(const Value: string); virtual;
    procedure SetAsVariant(Value: Variant); virtual;
    {}
    procedure Error(const ErrorMessage: string);
  public
    constructor Create(ExprType: TExpressionType);
    {}
    constructor CreateBoolean(Value: Boolean);
    constructor CreateInteger(Value: Integer);
    constructor CreateFloat(Value: Extended);
    constructor CreateString(Value: string);
    constructor CreateVariant(Value: Variant);
    constructor CreateSymbol(Value: string; Dummy: Boolean = false);
    {}
    destructor Destroy; override;
    {}
    procedure Assign(Source: TPersistent); override;    
    {}
    property ExprType: TExpressionType read GetExprType;
    {}
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsString: string read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  TOperatorPriority = (opLowest, opLow, opMedium, opHigh, opHighest);
  TOperatorKind = (okBinary, okUnaryLeft, okUnaryRight);

  TOperator = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent); virtual;
    {}
    procedure Error(const ErrorMessage: string);
    {}
      // Special for binary operators => allow them to be right operators
    function DualOperator: Boolean; virtual;
    function OperatorKind: TOperatorKind; virtual;
    function OperationResult(Expr1, Expr2: TExpressionType): TExpressionType; virtual;
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); virtual; abstract;
  end;

  TOperatorClass = class of TOperator;

  EOperationError = class(Exception);

  TOperatedExpression = class(TExpression)
  private
    FOperator: TOperator;
    FLeftExpr, FRightExpr: TExpression;
    FSolved: Boolean;
  protected
    procedure SolveOperation;
    property Solved: Boolean read FSolved write FSolved;
    {}
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsFloat: Extended; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    {}
    property LeftExpr: TExpression read FLeftExpr;
    property RightExpr: TExpression read FRightExpr;
    property Operator: TOperator read FOperator;
  public
    destructor Destroy; override;
  end;

  TVarExpression = class(TExpression)
  private
    FVariable: TExpression;
  protected
    function GetExprType: TExpressionType; override;
    {}
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsFloat: Extended; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    {}
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsFloat(Value: Extended); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(Value: Variant); override;
  public
    constructor CreateVariable(Variable: TExpression);
    {}
    property Variable: TExpression read FVariable;
  end;

  EFunctionError = class(Exception);

  TExpressionArray = array[1..255] of TExpression;
  PExpressionArray = ^TExpressionArray;
  TExprTypeArray = array[1..255] of TExpressionType;
  PExprTypeArray = ^TExprTypeArray;

const MaxParam = 64;

type
  TFunctionDeclaration = record
    ParamCount: Integer;
    Parameters: array[1..MaxParam] of TExpressionType;
    Result: TExpressionType;
  end;

  TSymbolList = class;

  TFunctionExpression = class(TExpression)
  private
    FOwner: TPersistent;
    FParameters: PExpressionArray;
    FSolved: Boolean;
    {}
    function GetResultType: TExpressionType;
    function GetParamCount: Integer;
    function GetParamTypes(Idx: Integer): TExpressionType;
    function GetParameters(Idx: Integer): TExpression;
  protected
    procedure CheckBounds(Idx: Integer);
    procedure CheckParam(Idx: Integer);
    procedure Solve;
    property Solved: Boolean read FSolved write FSolved;
    {}
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsFloat: Extended; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    {}
    procedure SetParameters(Idx: Integer; Param: TExpression);
    property Parameters[Idx: Integer]: TExpression read GetParameters write SetParameters;
    {}
    procedure FreeParameters;
    {}
    function GetOwner: TPersistent; override;
    {}
    property ResultType: TExpressionType read GetResultType;
    property ParamCount: Integer read GetParamCount;
    property ParamTypes[Idx: Integer]: TExpressionType read GetParamTypes;
  public
    constructor Create(Owner: TPersistent); virtual;
    destructor Destroy; override;
    {}
    function GetDeclaration: TFunctionDeclaration; virtual; abstract;
    procedure SolveFunction(Parameters: PExpressionArray; Result: TExpression); virtual; abstract;
  end;

  TFunctionClass = class of TFunctionExpression;

  EEvaluatorError = class(Exception)
  private
    FIndex: Integer;
  public
    constructor Create(const Msg: string; ErrorIndex: Integer);
    {}
    property ErrorIndex: Integer read FIndex;
  end;

  TSubEvaluationStack = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    {}
    procedure Push(EndChar: Char);
    procedure Pop;
    function CurrentEndChar: Char;
    {}
    function IsEmpty: Boolean;
  end;

  TSubOperation = record
    Expr: TExpression;
    Operator: TOperator;
    Priority: TOperatorPriority;
    OperatorKind: TOperatorKind;
  end;
  PSubOperation = ^TSubOperation;

  TOperationStack = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    {}
    procedure Push(const SubOp: TSubOperation);
    procedure Pop;
    function CurrentOperation: TSubOperation;
    {}
    function IsEmpty: Boolean;
  end;

  TExpressionEvaluator = class(TCustomEvalParser)
  private
    FOwner: TPersistent;
    FSubEvaluationList: TSubEvaluationStack;
    FSubEvaluation: Boolean;
    FSubEndChar: Char;
    FResult: TExpression;
    {}
    FLocalSymbols: TSymbolList;
    {}
    function ReadNumericExpr: TExpression;
    function ReadStringExpr: TExpression;
    function ReadOperatorExpr: TExpression;
    function ReadSymbolExpr: TExpression;
    function ReadFreeSymbolExpr: TExpression;
  protected
    procedure GotoNextExpression;
    function NextExpression: TExpression;
    {}
    property SubEvaluation: Boolean read FSubEvaluation;
    property SubEvaluationList: TSubEvaluationStack read FSubEvaluationList;
    property SubEvaluationEndChar: Char read FSubEndChar;
    {}
    procedure Error(const ErrorMessage: string);
    procedure RaiseEvaluatorError(E: Exception);
    {}
    function GetOwner: TPersistent; override;
    {}
    property LocalSymbols: TSymbolList read FLocalSymbols;
  public
    constructor Create(Owner: TPersistent; LocalSymbols: TSymbolList);
    destructor Destroy; override;
    {}
    function Evaluate(const Expression: string): TExpression;
    {}
    function ExtractResult: TExpression;
    property Result: TExpression read FResult;
  end;

  TEvaluator = class(TComponent)
  private
    FSymbols: TSymbolList;
    FResult: TExpression;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure RegisterOperator(const Name: string; Priority: TOperatorPriority;
      Operator: TOperatorClass);
    procedure RegisterVariable(const Name: string; Variable: TExpression);
    procedure RegisterFunction(const Name: string; Fn: TFunctionClass);
    {}
    function SymbolExists(const Name: string): Boolean;
    procedure UnRegisterSymbol(const Name: string);
    {}
    property LocalSymbols: TSymbolList read FSymbols;
    {}
    function Evaluate(const Expression: string): TExpression;
    {}
    function ExtractResult: TExpression;
    property Result: TExpression read FResult;
  end;

  TSymbol = record
    Kind: TSymbolKind;
    case TSymbolKind of
      skOperator: (Priority: TOperatorPriority; Operator: TOperatorClass);
      skFunction: (Fn: TFunctionClass);
      skVariable: (Variable: TExpression);
  end;
  PSymbol = ^TSymbol;

  ESymbolListError = class(Exception);

  TSymbolList = class
  private
    FList: TStringList;
    {}
    procedure TestValidity(Kind: TSymbolKind; const Name: string);
    function GetSymbolCount: Integer;
  protected
    function GetSymbol(const Name: string): TSymbol;
    {}
    procedure FreeItem(Item: PSymbol);
    property List: TStringList read FList;
    {}
    procedure Error(const Msg: string);
  public
    constructor Create;
    destructor Destroy; override;
    {}
    procedure RegisterVariable(const Name: string; Variable: TExpression);
    procedure RegisterFunction(const Name: string; Fn: TFunctionClass);
    procedure RegisterOperator(const Name: string; Priority: TOperatorPriority;
      Operator: TOperatorClass);
    {}
    function SymbolExists(const Name: string): Boolean;
    {}
    procedure RemoveSymbol(const Name: string); // auto Free
    {}
    procedure ClearSymbols;
    {}
    property SymbolCount: Integer read GetSymbolCount;
  end;

const ErrorNumeric = 'Expression numérique incorrecte';
      ErrorStringTooLong = 'Expression chaîne trop longue';
      ErrorStringIncomplete = 'Expression chaîne non terminée';
      ErrorIncorrectExpression = 'Expression incorrecte';
      ErrorInvalidChar = 'Caractère invalide : "%s", ASCII %d';
      ErrorFreeSymbol = 'Symbole libre non terminé';

      ErrorOperatorExpected = 'Opérateur attendu';
      ErrorExpressionExpected = 'Expression attendue';

      ErrorUnknownSymbol = 'Symbole inconnu : "%s"';

      ErrorIncorrectOperationTypes = 'Types pour opération incorrects : ' +
        'Opération "%s" avec %s et %s';

      ErrorIncorrectOperation = 'Opération incorrecte';
      ErrorIncorrectConversion = 'Conversion incorrecte';

      ErrorSymbolNameInvalid = 'Nom de symbole invalide : "%s"';
      ErrorDuplicateSymbol = 'Symbole dupliqué : "%s"';

      ErrorDuplicateFunction = 'Identificateur de fonction dupliquée : "%s"';
      ErrorParamIndexInvalid = 'Index de Paramètre de fonction invalide';
      ErrorParenthesisExpected = 'Fonction incomplète : Parenthèse attendue';
      ErrorWrongTypeParameter = 'Paramètre d''un type non autorisé';

      ErrorFunctionDeclaration = 'Erreur de déclaration de fonction';
      ErrorParamTypeUnknown = 'Type de paramètre inconnu : "%s"';
      ErrorWrongOperationParameters = 'Paramètres de fonction incorrects';

  procedure FreeExpression(var Expression: TExpression);

  procedure RegisterGlobalOperator(const Name: string; Priority: TOperatorPriority;
    Operator: TOperatorClass);
  procedure RegisterGlobalVariable(const Name: string; Variable: TExpression);
  procedure RegisterGlobalFunction(const Name: string; Fn: TFunctionClass);
  function GlobalSymbolExists(const Name: string): Boolean;
  procedure GlobalUnregister(const Name: string);

implementation

uses SharedUtils;

var GlobalSymbols: TSymbolList;

function IsParamCompatible(Master, Slave: TExpressionType): Boolean;
begin
  if Slave = etVariant then
    Result:= True
  else
    case Master of
      etFloat: Result:= Slave in [etInteger, etFloat, etVariant];
      etVariant: Result:= Slave in [etBoolean, etString, etInteger, etFloat, etVariant];
      etError: Result:= False;
      else Result:= Master = Slave;
    end;
end;

function IsSymbolValid(const S: string): Boolean;
var i: Integer;
begin
  for i:= 1 to Length(S) do
    if not (S[i] in SymbolChars) then
    begin
      Result:= False;
      Exit;
    end;
  Result:= True;
end;

function IsOperatorValid(const S: string): Boolean;
var i: Integer;
    TestSet: set of Char;
begin
  if Length(S) > 0 then
  begin
    if S[1] in OperatorChars then
      TestSet:= OperatorChars
    else
      TestSet:= SymbolChars;
    for i:= 1 to Length(S) do
      if not (S[i] in TestSet) then
      begin
        Result:= False;
        Exit;
      end;
    Result:= True;
  end else
    Result:= False;
end;

procedure FreeExpression(var Expression: TExpression);
begin
  if Assigned(Expression) then
  begin
    Expression.Free;
    Expression:= nil;
  end;
end;

constructor EEvaluatorError.Create(const Msg: string; ErrorIndex: Integer);
begin
  FIndex:= ErrorIndex;
  inherited Create(Msg);
end;

{ TExpressionEvaluator }

constructor TExpressionEvaluator.Create(Owner: TPersistent; LocalSymbols: TSymbolList);
begin
  FOwner:= Owner;
  FLocalSymbols:= LocalSymbols;
  FSubEvaluationList:= TSubEvaluationStack.Create;
  StrQuote:= StringChar;
  Self.DecimalChar:= EvalIntf.DecimalChar;
  Self.SymbolChars:= EvalIntf.SymbolChars;
  Self.IgnoredChars:= EvalIntf.IgnoredChars;
end;

destructor TExpressionEvaluator.Destroy;
begin
  FreeExpression(FResult);
  {}
  FSubEvaluationList.Free;
  FSubEvaluationList:= nil;
  {}
  inherited Destroy;
end;

function TExpressionEvaluator.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure TExpressionEvaluator.Error(const ErrorMessage: string);
begin
  raise EEvaluatorError.Create(ErrorMessage, ParseIndex);
end;

procedure TExpressionEvaluator.RaiseEvaluatorError(E: Exception);
begin
  Error(E.Message);
end;

function TExpressionEvaluator.ExtractResult: TExpression;
begin
  Result:= FResult;
  FResult:= nil;
end;

function TExpressionEvaluator.Evaluate(const Expression: string): TExpression;
var Operator1, Operator2: TSubOperation;
    CurrentState, NewState: TEvaluationState;
    SubOperations: TOperationStack;
    SubFinished: Boolean;
    function GetOperatorState(OperatorKind: TOperatorKind): TEvaluationState; forward;
    procedure SetCurrentExpression(Expression: TExpression); forward;
    procedure SetCurrentOperator(Operator: TOperator; Priority: TOperatorPriority;
      OperatorKind: TOperatorKind); forward;
    function SubEvaluate(EvaluationEndChar: Char): TExpression; forward;
    function SubEvaluationTermination: Boolean; forward;
    procedure GetNextState; forward;
    function EvaluateFunction(FnClass: TFunctionClass): TFunctionExpression; forward;
    function AnalyseSymbol(var Expression: TExpression): TExpression; forward;
    procedure TerminateEvaluation; forward;
    procedure ProcessState; forward;
    {}
    procedure OperatorExpected;
    begin
      Error(ErrorOperatorExpected);
    end;
    {}
    procedure ExpressionExpected;
    begin
      Error(ErrorExpressionExpected);
    end;
    {}
    procedure SetCurrentExpression(Expression: TExpression);
    begin
      if Expression <> nil then
      begin
        if Operator1.Expr = nil then
          Operator1.Expr:= Expression
        else
          Operator2.Expr:= Expression;
      end;
    end;
    {}
    procedure SetCurrentOperator(Operator: TOperator; Priority: TOperatorPriority;
      OperatorKind: TOperatorKind);
    var Operation: PSubOperation;
    begin
      if Operator1.Operator = nil then
        Operation:= @Operator1
      else
        Operation:= @Operator2;
      Operation^.Operator:= Operator;
      Operation^.Priority:= Priority;
      Operation^.OperatorKind:= OperatorKind;
    end;
    {}
    function SubEvaluate(EvaluationEndChar: Char): TExpression;
    begin
      SubEvaluationList.Push(EvaluationEndChar);
      FSubEvaluation:= True;
      FSubEndChar:= EvaluationEndChar;
      Evaluate(Expression);
      Result:= Self.Result;
      SubEvaluationList.Pop;
      FSubEvaluation:= not SubEvaluationList.IsEmpty;
      if FSubEvaluation then
        FSubEndChar:= SubEvaluationList.CurrentEndChar;
    end;
    {}
    function SubEvaluationTermination: Boolean;
    begin
      if SubEvaluation then
      begin
        Result:= CurrentChar = SubEvaluationEndChar;
        if Result then
        begin // If we are a SubEvaluator, evaluation termination
          SubFinished:= True;
          TerminateEvaluation;
          SkipChar;
        end;
      end else
        Result:= False;
    end;
    {}
    procedure GetNextState; // Get next expression/operator
    var Result: TExpression;
    begin
      if not SubFinished then
        GotoNextExpression; // Goto next valuable token
      Result:= nil;
      if SubFinished or (not CanContinue and not SubEvaluation) or SubEvaluationTermination then
      begin
        NewState:= esFinished; // Check if new state = esFinished
        Exit;
      end else if CurrentChar = BeginParenthesis then
      begin // if BeginParenthesis => SubEvaluation
        SkipChar;
        Result:= SubEvaluate(EndParenthesis);
      end else // else, get next expression, whatever it is
        Result:= NextExpression;
      if Result = nil then // if chars unknown or unexpected, then error
        NewState:= esError
      else if Result.ExprType = etSymbol then // Analyse symbol if it is a symbol
        Result:= AnalyseSymbol(Result) // => This proc sets new state
      else
        NewState:= esExpression; // if not a symbol, it is a standard expression

      SetCurrentExpression(Result); // Set current expression as Result
    end;
    {}
    function AnalyseSymbol(var Expression: TExpression): TExpression;
    var Symbol: TSymbol;
        S: string;
        OpKind: TOperatorKind;
        Operator: TOperator;
    begin
      Result:= nil;
      S:= Expression.AsString;
      FreeExpression(Expression);
      Symbol:= LocalSymbols.GetSymbol(S);
      if Symbol.Kind = skError then
        Symbol:= GlobalSymbols.GetSymbol(S);
      if Symbol.Kind = skError then
        Error(Format(ErrorUnknownSymbol, [S]));
      case Symbol.Kind of
        skOperator:
          begin
            Operator:= Symbol.Operator.Create(GetOwner);
            if (CurrentState in [esNone, esOpRight, esOpBinary]) and
               (Operator.OperatorKind = okBinary) and Operator.DualOperator then
              OpKind:= okUnaryRight
            else
              OpKind:= Operator.OperatorKind;
            NewState:= GetOperatorState(OpKind);
            SetCurrentOperator(Operator, Symbol.Priority, OpKind);
          end;
        skFunction:
          begin
            Result:= EvaluateFunction(Symbol.Fn);
            NewState:= esExpression;
          end;
        skVariable:
          begin
            Result:= TVarExpression.CreateVariable(Symbol.Variable);
            NewState:= esExpression;
          end;
        skError: Error(Format(ErrorUnknownSymbol, [S]));
      end;
    end;
    {}
    function EvaluateFunction(FnClass: TFunctionClass): TFunctionExpression;
    var i: Integer;
    begin
      Result:= FnClass.Create(GetOwner);
      if Result.ParamCount > 0 then
      try
        GotoNextExpression; // Go & check if there is a parenthesis
        if CanContinue and (CurrentChar = BeginParenthesis) then
        begin
          SkipChar;
          if Result.ParamCount >= 1 then
          begin
            for i:= 1 to Result.ParamCount - 1 do
            begin
              Result.Parameters[i]:= SubEvaluate(ParamSeparator);
              Result.CheckParam(i);
            end;
            Result.Parameters[Result.ParamCount]:= SubEvaluate(EndParenthesis);
            Result.CheckParam(Result.ParamCount);
          end;
        end else
          Error(ErrorParenthesisExpected);
      except
        on E: Exception do
        begin
          Result.Free;
          RaiseEvaluatorError(E);
        end;
      end;
    end;
    {}
    procedure ClearOperation(var Operation: TSubOperation);
    begin
      Operation.Expr:= nil;
      Operation.Operator:= nil;
    end;
    {}
    procedure PushOperator; // Save informations of Operator1
    begin
      if not Assigned(SubOperations) then
        SubOperations:= TOperationStack.Create;
      if Operator1.OperatorKind in [okUnaryRight, okUnaryLeft] then
      begin
        Operator2.Expr:= Operator1.Expr;
        Operator1.Expr:= nil;
      end;
      SubOperations.Push(Operator1);
      Operator1:= Operator2;
      ClearOperation(Operator2);
      CurrentState:= NewState;
    end;
    {}
    function GetOperatorState(OperatorKind: TOperatorKind): TEvaluationState;
    begin {$WARNINGS OFF}
      case OperatorKind of
        okBinary: Result:= esOpBinary;
        okUnaryLeft: Result:= esOpLeft;
        okUnaryRight: Result:= esOpRight;
      end;
    end;  {$WARNINGS ON}
    {}
    procedure PopOperator;
    begin
      Operator1:= SubOperations.CurrentOperation;
      SubOperations.Pop;
      CurrentState:= GetOperatorState(Operator1.OperatorKind);
      if CurrentState = esOpRight then
      begin
        Operator1.Expr:= Operator2.Expr;
        Operator2.Expr:= nil;
      end;
    end;
    {}
    procedure EvaluateCurrentOperation;
    var Operation: TOperatedExpression;
        T1, T2: TExpressionType;
        OpRes: TExpressionType;
    begin // Creates the required TOperatedExpression
      if Assigned(Operator1.Operator) then
      begin
        T2:= etError;
        T1:= Operator1.Expr.ExprType;
        if Assigned(Operator2.Expr) then
          T2:= Operator2.Expr.ExprType;
        OpRes:= Operator1.Operator.OperationResult(T1, T2);
        if OpRes = etError then
          Error(ErrorWrongOperationParameters);
        Operation:= TOperatedExpression.Create(OpRes);
        Operation.FLeftExpr:= Operator1.Expr;
        Operation.FRightExpr:= Operator2.Expr;
        Operation.FOperator:= Operator1.Operator;
        if (not Assigned(SubOperations)) or SubOperations.IsEmpty then
        begin
          ClearOperation(Operator1);
          Operator1.Expr:= Operation;
          Operator1.Operator:= Operator2.Operator;
          Operator1.Priority:= Operator2.Priority;
          Operator1.OperatorKind:= Operator2.OperatorKind;
          ClearOperation(Operator2);
          if Operator1.Operator = nil then
            NewState:= esExpression
          else
            NewState:= GetOperatorState(Operator1.OperatorKind);
        end else
        begin
          Operator2.Expr:= Operation;
          PopOperator;
          if Operator2.Operator = nil then
            NewState:= esExpression
          else
            NewState:= GetOperatorState(Operator2.OperatorKind);
          if NewState in [esOpBinary, esOpLeft] then
          begin
            if Operator1.Priority >= Operator2.Priority then
              EvaluateCurrentOperation
            else
              PushOperator;
          end else
            ProcessState;
        end;
      end;
    end;
    {}
    procedure TerminateEvaluation;
    begin
      if Assigned(Operator1.Operator) or Assigned(Operator2.Operator) then
        EvaluateCurrentOperation;
      FResult:= Operator1.Expr;
    end;
    {}
    procedure ProcessSubOperation;
    begin
      CurrentState:= NewState;
      GetNextState;
      if NewState = esFinished then
        TerminateEvaluation
      else if NewState in [esOpBinary, esOpLeft] then
      begin
        if Operator1.Priority >= Operator2.Priority then
          EvaluateCurrentOperation
        else
          PushOperator;
      end else
        OperatorExpected;
    end;
    {}
    procedure ProcessState;
    begin
      case CurrentState of
        esNone: if not (NewState in [esExpression, esOpRight]) then
                  ExpressionExpected;
        esExpression:
          if NewState = esFinished then
            TerminateEvaluation
          else if NewState = esOpLeft then
            EvaluateCurrentOperation
          else if NewState <> esOpBinary then
            OperatorExpected;
        esOpBinary:
          if NewState = esExpression then
            ProcessSubOperation
          else if NewState = esOpRight then
            PushOperator
          else ExpressionExpected;
        esOpRight:
          if NewState = esOpRight then
            PushOperator
          else if NewState = esExpression then
            ProcessSubOperation
          else ExpressionExpected;
       esOpLeft:
          if NewState = esFinished then
            TerminateEvaluation
          else if NewState in [esOpBinary, esOpLeft] then
            EvaluateCurrentOperation
          else
            OperatorExpected;
          esError: Error(ErrorIncorrectExpression);
      end;
      CurrentState:= NewState;
    end;
begin
  SubFinished:= False;
  if not SubEvaluation then
  begin
    Self.Expression:= Expression;
    FreeExpression(FResult);
    ParseIndex:= 1; // Initialization
  end;
  CurrentState:= esNone;
  FResult:= nil;
  SubOperations:= nil;
  ClearOperation(Operator1);
  ClearOperation(Operator2);
  try
    repeat
      GetNextState;
      ProcessState;
    until CurrentState = esFinished;
    TerminateEvaluation;
    SubOperations.Free;
    SubOperations:= nil;
  except
    on E: Exception do
    begin
      FSubEvaluation:= False;
      FResult:= nil; // On ne libère pas FResult car il est dans Operator1 ou 2
      SubOperations.Free;
      while not SubEvaluationList.IsEmpty do
        SubEvaluationList.Pop;
      FreeExpression(Operator1.Expr);
      Operator1.Operator.Free;
      FreeExpression(Operator2.Expr);
      Operator2.Operator.Free;
      RaiseEvaluatorError(E);
    end;
  end;
  Result:= FResult;
end;

function TExpressionEvaluator.NextExpression: TExpression;
begin {$WARNINGS OFF}
  if CanContinue then
    case CurrentChar of
      '0'..'9', EvalIntf.DecimalChar: Result:= ReadNumericExpr;
      StringChar: Result:= ReadStringExpr;
      BeginFreeSymbol: Result:= ReadFreeSymbolExpr;
      else if CurrentChar in OperatorChars then
        Result:= ReadOperatorExpr
      else if CurrentChar in SymbolChars then
        Result:= ReadSymbolExpr
      else
        Error(Format(ErrorInvalidChar, [CurrentChar, Ord(CurrentChar)]));
    end
  else
    Result:= nil;
end; {$WARNINGS ON}

procedure TExpressionEvaluator.GotoNextExpression;
begin
  GotoNextToken(IgnoredChars);
end;

function TExpressionEvaluator.ReadNumericExpr: TExpression;
var Value: TNumericValue;
begin {$WARNINGS OFF}
  Value:= ReadNumeric;
  case Value.Kind of
    ntInteger: Result:= TExpression.CreateInteger(Value.AsInteger);
    ntFloat: Result:= TExpression.CreateFloat(Value.AsFloat);
  end;
end; {$WARNINGS ON}

function TExpressionEvaluator.ReadStringExpr: TExpression;
begin
  Result:= TExpression.CreateString(ReadString);
end;

function TExpressionEvaluator.ReadOperatorExpr: TExpression;
var Beginning: Integer;
begin
  Beginning:= ParseIndex;
  while CanContinue and (CurrentChar in OperatorChars) do
    SkipChar;
  // Note : whatever the next char, it is not an operator char. Any error will occur on next evaluation
  Result:= TExpression.CreateSymbol(Copy(Expression, Beginning, ParseIndex - Beginning));
end;

function TExpressionEvaluator.ReadSymbolExpr: TExpression;
var Beginning: Integer;
begin
  Beginning:= ParseIndex;
  while CanContinue and (CurrentChar in SymbolChars) do
    SkipChar;
  // Note : whatever the next char, it is not a symbol char. Any error will occur on next evaluation
  Result:= TExpression.CreateSymbol(Copy(Expression, Beginning, ParseIndex - Beginning));
end;

function TExpressionEvaluator.ReadFreeSymbolExpr: TExpression;
var Beginning: Integer;
begin
  SkipChar;
  Beginning:= ParseIndex;
  while CanContinue and (CurrentChar in FreeSymbolChars) and
    (CurrentChar <> EndFreeSymbol) do
      SkipChar;
  if not (CurrentChar in FreeSymbolChars) then
    Error(Format(ErrorInvalidChar, [CurrentChar, Ord(CurrentChar)]));
  if not CanContinue then
    Error(ErrorFreeSymbol);
  Result:= TExpression.CreateSymbol(Copy(Expression, Beginning, ParseIndex - Beginning));
  SkipChar;
end;

{}

constructor TEvaluator.Create(AOwner: TComponent);
begin
  FSymbols:= TSymbolList.Create;
  {}
  inherited Create(AOwner);
end;

destructor TEvaluator.Destroy;
begin
  FreeExpression(FResult);
  {}
  FSymbols.Free;
  FSymbols:= nil;
  {}
  inherited Destroy;
end;

procedure TEvaluator.RegisterOperator(const Name: string; Priority: TOperatorPriority;
  Operator: TOperatorClass);
begin
  LocalSymbols.RegisterOperator(Name, Priority, Operator);
end;

procedure TEvaluator.RegisterVariable(const Name: string; Variable: TExpression);
begin
  LocalSymbols.RegisterVariable(Name, Variable);
end;

procedure TEvaluator.RegisterFunction(const Name: string; Fn: TFunctionClass);
begin
  LocalSymbols.RegisterFunction(Name, Fn);
end;

function TEvaluator.SymbolExists(const Name: string): Boolean;
begin
  Result:= LocalSymbols.SymbolExists(Name) or GlobalSymbolExists(Name);
end;

procedure TEvaluator.UnRegisterSymbol(const Name: string);
begin
  LocalSymbols.RemoveSymbol(Name);
end;

function TEvaluator.ExtractResult: TExpression;
begin
  Result:= FResult;
  FResult:= nil;
end;

function TEvaluator.Evaluate(const Expression: string): TExpression;
var E: TExpressionEvaluator;
begin
  E:= TExpressionEvaluator.Create(Self, LocalSymbols);
  try
    FreeExpression(FResult);
    E.Evaluate(Expression);
    FResult:= E.ExtractResult;
    Result:= FResult;
  finally
    E.Free;
  end;
end;

{}

constructor TSubEvaluationStack.Create;
begin
  FList:= TList.Create;
end;

destructor TSubEvaluationStack.Destroy;
begin
  FList.Free;
  FList:= nil;
  inherited Destroy;
end;

procedure TSubEvaluationStack.Push(EndChar: Char);
begin
  FList.Add(Pointer(EndChar));
end;

procedure TSubEvaluationStack.Pop;
begin
  FList.Delete(FList.Count - 1);
end;

function TSubEvaluationStack.CurrentEndChar: Char;
begin
  Result:= Char(FList.Last);
end;

function TSubEvaluationStack.IsEmpty: Boolean;
begin
  Result:= FList.Count = 0;
end;

{}

constructor TOperationStack.Create;
begin
  FList:= TList.Create;
end;

destructor TOperationStack.Destroy;
var i: Integer;
begin
  for i:= 0 to FList.Count - 1 do
    Dispose(PSubOperation(FList[i]));
  FList.Free;
  FList:= nil;
  inherited Destroy;
end;

procedure TOperationStack.Push(const SubOp: TSubOperation);
var P: PSubOperation;
begin
  New(P);
  P^:= SubOp;
  FList.Add(P);
end;

procedure TOperationStack.Pop;
var Idx: Integer;
begin
  Idx:= FList.Count - 1;
  Dispose(PSubOperation(FList.Items[Idx]));
  FList.Delete(Idx);
end;

function TOperationStack.CurrentOperation: TSubOperation;
begin
  Result:= PSubOperation(FList.Items[FList.Count - 1])^;
end;

function TOperationStack.IsEmpty: Boolean;
begin
  Result:= FList.Count = 0;
end;

{ TOperator }

constructor TOperator.Create(Owner: TPersistent);
begin
  FOwner:= Owner;
end;

function TOperator.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure TOperator.Error(const ErrorMessage: string);
begin
  raise EOperationError.Create(ErrorMessage);
end;

function TOperator.DualOperator: Boolean;
begin
  Result:= False;
end;

function TOperator.OperatorKind: TOperatorKind;
begin
  Result:= okBinary;
end;

function TOperator.OperationResult(Expr1, Expr2: TExpressionType): TExpressionType;
begin
  if (Expr1 = etVariant) or (Expr2 = etVariant) then
    Result:= etVariant
  else
    Result:= etError;
end;

  { TExpression }

procedure TExpression.Error(const ErrorMessage: string);
begin
  raise EExpressionError.Create(ErrorMessage);
end;

constructor TExpression.Create(ExprType: TExpressionType);
begin
  FExprType:= etError;
  SetExpressionType(ExprType);
end;

constructor TExpression.CreateBoolean(Value: Boolean);
begin
  FExprType:= etError;
  SetExpressionType(etBoolean);
  SetAsBoolean(Value);
end;

constructor TExpression.CreateInteger(Value: Integer);
begin
  FExprType:= etError;
  SetExpressionType(etInteger);
  SetAsInteger(Value);
end;

constructor TExpression.CreateFloat(Value: Extended);
begin
  FExprType:= etError;
  SetExpressionType(etFloat);
  SetAsFloat(Value);
end;

constructor TExpression.CreateString(Value: string);
begin
  FExprType:= etError;
  SetExpressionType(etString);
  SetAsString(Value);
end;

constructor TExpression.CreateSymbol(Value: string; Dummy: Boolean);
begin
  FExprType:= etError;
  SetExpressionType(etSymbol);
  SetAsString(Value);
end;

constructor TExpression.CreateVariant(Value: Variant);
begin
  FExprType:= etError;
  SetExpressionType(etVariant);
  SetAsVariant(Value);
end;

destructor TExpression.Destroy;
begin
  FreeValue;
  inherited Destroy;
end;

procedure TExpression.Assign(Source: TPersistent);
begin
  if Source is TExpression then
  begin
    SetExpressionType(TExpression(Source).ExprType);
    AsVariant:= TExpression(Source).AsVariant;
  end else
    inherited Assign(Self);
end;

{$WARNINGS OFF}

function TExpression.GetExprType: TExpressionType;
begin
  Result:= FExprType;
end;

function TExpression.GetAsBoolean: Boolean;
begin
  case ExprType of
    etBoolean: Result:= PBool(FValue)^;
    etInteger:
      if AsInteger = 0 then
        Result:= False
      else
        Result:= True;
    etString:
      if CompareText(AsString, STrue) = 0 then
        Result:= True
      else if CompareText(AsString, SFalse) = 0 then
        Result:= False
      else
        Error(ErrorIncorrectConversion);
    etVariant: Result:= AsVariant;
    else
      Error(ErrorIncorrectConversion);
  end;
end;

function TExpression.GetAsInteger: Integer;
begin
  case ExprType of
    etBoolean: Result:= Integer(AsBoolean);
    etInteger: Result:= PInteger(FValue)^;
    etString: Result:= StrToInt(AsString);
    etFloat: Result:= Trunc(AsFloat);
    etVariant: if VarIsNull(AsVariant) then Result:= 0 else Result:= AsVariant;
    else
      Error(ErrorIncorrectConversion);
  end;
end;

function TExpression.GetAsFloat: Extended;
begin
  case ExprType of
    etInteger: Result:= AsInteger;
    etString: Result:= StrToFloat(AsString);
    etFloat: Result:= PExtended(FValue)^;
    etVariant: if VarIsNull(AsVariant) then Result:= 0 else Result:= AsVariant;
    else
      Error(ErrorIncorrectConversion);
  end;
end;

function TExpression.GetAsString: string;
begin
  case ExprType of
    etBoolean:
      if AsBoolean then
        Result:= STrue
      else
        Result:= SFalse;
    etInteger: Result:= IntToStr(AsInteger);
    etString, etSymbol: Result:= PString(FValue)^;
    etFloat: Result:= FloatToStr(AsFloat);
    etVariant: if VarIsNull(AsVariant) then Result:= '' else Result:= AsVariant;
    else
      Error(ErrorIncorrectConversion);
  end;
end;

function TExpression.GetAsVariant: Variant;
begin
  case ExprType of
    etBoolean: Result:= AsBoolean;
    etInteger: Result:= AsInteger;
    etString, etSymbol: Result:= AsString;
    etFloat: Result:= AsFloat;
    etVariant: Result:= PVariant(FValue)^;
    else
      Error(ErrorIncorrectConversion);
  end;
end;

{$WARNINGS ON}

{}

procedure TExpression.SetAsBoolean(Value: Boolean);
begin
  case ExprType of
    etBoolean: PBool(FValue)^:= Value;
    etInteger: SetAsInteger(Integer(Value));
    etString:
      if Value then
        SetAsString(STrue)
      else
        SetAsString(SFalse);
    etVariant: SetAsVariant(Value);
    else
      Error(ErrorIncorrectConversion);
  end;
end;

procedure TExpression.SetAsInteger(Value: Integer);
begin
  case ExprType of
    etBoolean:
      if Value = 0 then
        SetAsBoolean(False)
      else if Value = 1 then
        SetAsBoolean(True)
      else
        Error(ErrorIncorrectConversion);
    etInteger: PInteger(FValue)^:= Value;
    etString: SetAsString(IntToStr(Value));
    etFloat: SetAsFloat(Value);
    etVariant: SetAsVariant(Value);
    else
      Error(ErrorIncorrectConversion);
  end;
end;

procedure TExpression.SetAsFloat(Value: Extended);
begin
  case ExprType of
    etInteger: SetAsInteger(Trunc(Value));
    etString: SetAsString(FloatToStr(Value));
    etFloat: PExtended(FValue)^:= Value;
    etVariant: SetAsVariant(Value);
    else
      Error(ErrorIncorrectConversion);
  end;
end;

procedure TExpression.SetAsString(const Value: string);
begin
  case ExprType of
    etBoolean:
      if CompareText(Value, STrue) = 0 then
        SetAsBoolean(True)
      else if CompareText(Value, SFalse) = 0 then
        SetAsBoolean(False)
      else
        Error(ErrorIncorrectConversion);
    etInteger: SetAsInteger(StrToInt(Value));
    etString, etSymbol: PString(FValue)^:= Value;
    etFloat: SetAsFloat(StrToFloat(Value));
    etVariant: SetAsVariant(Value);
    else
      Error(ErrorIncorrectConversion);
  end;
end;

procedure TExpression.SetAsVariant(Value: Variant);
begin
  case ExprType of
    etBoolean: SetAsBoolean(Value);
    etInteger: SetAsInteger(Value);
    etString, etSymbol: SetAsString(Value);
    etFloat: SetAsFloat(Value);
    etVariant: PVariant(FValue)^:= Value;
    else
      Error(ErrorIncorrectConversion);
  end;
end;

{}

procedure TExpression.FreeValue;
begin
  if FExprType = etError then
    Exit; // This condition is NECESSARY because there may have problems with TVarExpr
  case ExprType of
    etBoolean: Dispose(PBool(FValue));
    etInteger: Dispose(PInteger(FValue));
    etFloat: Dispose(PExtended(FValue));
    etString, etSymbol: Dispose(PString(FValue));
    etVariant: Dispose(PVariant(FValue));
  end;
  FExprType:= etError;
end;

procedure TExpression.SetExpressionType(ExprType: TExpressionType);
begin
  if ExprType <> FExprType then
  begin
    FreeValue;
    case ExprType of
      etBoolean: New(PBool(FValue));
      etInteger: New(PInteger(FValue));
      etFloat: New(PExtended(FValue));
      etString, etSymbol: New(PString(FValue));
      etVariant: New(PVariant(FValue));
      else
        Error(ErrorIncorrectOperation);
    end;
    FExprType:= ExprType;
  end;
end;

{ TOperatedExpression }

destructor TOperatedExpression.Destroy;
begin
  FOperator.Free;
  FOperator:= nil;
  FreeExpression(FLeftExpr);
  FreeExpression(FRightExpr);
  inherited Destroy;
end;

procedure TOperatedExpression.SolveOperation;
begin
  if not Solved then
    Operator.Operate(LeftExpr, RightExpr, TExpression(Self));
  Solved:= True;
end;

function TOperatedExpression.GetAsBoolean: Boolean;
begin
  SolveOperation;
  Result:= inherited GetAsBoolean;
  Solved:= False;
end;

function TOperatedExpression.GetAsInteger: Integer;
begin
  SolveOperation;
  Result:= inherited GetAsInteger;
  Solved:= False;
end;

function TOperatedExpression.GetAsFloat: Extended;
begin
  SolveOperation;
  Result:= inherited GetAsFloat;
  Solved:= False;
end;

function TOperatedExpression.GetAsString: string;
begin
  SolveOperation;
  Result:= inherited GetAsString;
  Solved:= False;
end;

function TOperatedExpression.GetAsVariant: Variant;
begin
  SolveOperation;
  Result:= inherited GetAsVariant;
  Solved:= False;
end;

{ TFunctionExpression }

constructor TFunctionExpression.Create(Owner: TPersistent);
begin
  FOwner:= Owner;
  GetMem(FParameters, ParamCount * SizeOf(Pointer));
  FillChar(FParameters^, ParamCount * SizeOf(Pointer), Integer(nil));
  Self.SetExpressionType(ResultType);
end;

procedure TFunctionExpression.FreeParameters;
var i: Integer;
begin
  for i:= 1 to ParamCount do
    FreeExpression(FParameters^[i]);
end;

destructor TFunctionExpression.Destroy;
begin
  FreeParameters;
  FreeMem(FParameters, ParamCount * SizeOf(Pointer));
  inherited Destroy;
end;

function TFunctionExpression.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure TFunctionExpression.CheckBounds(Idx: Integer);
begin
  if (Idx > ParamCount) or (Idx < 1) then
    raise EFunctionError.Create(ErrorParamIndexInvalid);
end;

procedure TFunctionExpression.CheckParam(Idx: Integer);
begin
  if not IsParamCompatible(ParamTypes[Idx], Parameters[Idx].ExprType) then
    raise EFunctionError.Create(ErrorWrongTypeParameter);
end;

function TFunctionExpression.GetParameters(Idx: Integer): TExpression;
begin
  CheckBounds(Idx);
  Result:= FParameters^[Idx];
end;

function TFunctionExpression.GetResultType: TExpressionType;
begin
  Result:= GetDeclaration.Result;
end;

function TFunctionExpression.GetParamCount: Integer;
begin
  Result:= GetDeclaration.ParamCount;
end;

function TFunctionExpression.GetParamTypes(Idx: Integer): TExpressionType;
begin
  CheckBounds(Idx);
  Result:= GetDeclaration.Parameters[Idx];
end;

procedure TFunctionExpression.SetParameters(Idx: Integer; Param: TExpression);
begin
  CheckBounds(Idx);
  FParameters^[Idx]:= Param;
end;

procedure TFunctionExpression.Solve;
begin
  if not Solved then
    SolveFunction(FParameters, Self);
  Solved:= True;
end;

function TFunctionExpression.GetAsBoolean: Boolean;
begin
  Solve;
  Result:= inherited GetAsBoolean;
  Solved:= False;
end;

function TFunctionExpression.GetAsInteger: Integer;
begin
  Solve;
  Result:= inherited GetAsInteger;
  Solved:= False;
end;

function TFunctionExpression.GetAsFloat: Extended;
begin
  Solve;
  Result:= inherited GetAsFloat;
  Solved:= False;
end;

function TFunctionExpression.GetAsString: string;
begin
  Solve;
  Result:= inherited GetAsString;
  Solved:= False;
end;

function TFunctionExpression.GetAsVariant: Variant;
begin
  Solve;
  Result:= inherited GetAsVariant;
  Solved:= False;
end;

{ TVarExpression }

constructor TVarExpression.CreateVariable(Variable: TExpression);
begin
  FVariable:= Variable;
  inherited Create(etError);
end;

function TVarExpression.GetExprType: TExpressionType;
begin
  Result:= FVariable.ExprType;
end;

function TVarExpression.GetAsBoolean: Boolean;
begin
  Result:= FVariable.AsBoolean;
end;

function TVarExpression.GetAsInteger: Integer;
begin
  Result:= FVariable.AsInteger;
end;

function TVarExpression.GetAsFloat: Extended;
begin
  Result:= FVariable.AsFloat;
end;

function TVarExpression.GetAsString: string;
begin
  Result:= FVariable.AsString;
end;

function TVarExpression.GetAsVariant: Variant;
begin
  Result:= FVariable.AsVariant;
end;

procedure TVarExpression.SetAsBoolean(Value: Boolean);
begin
  FVariable.AsBoolean:= Value;
end;

procedure TVarExpression.SetAsInteger(Value: Integer);
begin
  FVariable.AsInteger:= Value;
end;

procedure TVarExpression.SetAsFloat(Value: Extended);
begin
  FVariable.AsFloat:= Value;
end;

procedure TVarExpression.SetAsString(const Value: string);
begin
  FVariable.AsString:= Value;
end;

procedure TVarExpression.SetAsVariant(Value: Variant);
begin
  FVariable.AsVariant:= Value;
end;

{ TSymbolList }

constructor TSymbolList.Create;
begin
  FList:= TStringList.Create;
  FList.Sorted:= True;
end;

destructor TSymbolList.Destroy;
begin
  ClearSymbols;
  FList.Free;
  FList:= nil;
  inherited Destroy;
end;

procedure TSymbolList.RegisterVariable(const Name: string; Variable: TExpression);
var P: PSymbol;
begin
  try
    TestValidity(skVariable, Name);
  except
    Variable.Free;
    raise;
  end;
  New(P);
  P^.Kind:= skVariable;
  P^.Variable:= Variable;
  FList.AddObject(Name, Pointer(P));
end;

procedure TSymbolList.RegisterFunction(const Name: string; Fn: TFunctionClass);
var P: PSymbol;
begin
  TestValidity(skFunction, Name);
  New(P);
  P^.Kind:= skFunction;
  P^.Fn:= Fn;
  FList.AddObject(Name, Pointer(P));
end;

procedure TSymbolList.RegisterOperator(const Name: string; Priority: TOperatorPriority;
  Operator: TOperatorClass);
var P: PSymbol;
begin
  TestValidity(skOperator, Name);
  New(P);
  P^.Kind:= skOperator;
  P^.Operator:= Operator;
  P^.Priority:= Priority;
  FList.AddObject(Name, Pointer(P));
end;

procedure TSymbolList.RemoveSymbol(const Name: string);
var i: Integer;
    P: PSymbol;
begin
  i:= FList.IndexOf(Name);
  if i = -1 then
    Error(Format(ErrorUnknownSymbol, [Name]))
  else
  begin
    P:= Pointer(FList.Objects[i]);
    FreeItem(P);
    FList.Delete(i);
  end;
end;

procedure TSymbolList.ClearSymbols;
var i: Integer;
begin
  for i:= FList.Count - 1 downto 0 do
    FreeItem(PSymbol(FList.Objects[i]));
  FList.Clear;
end;

function TSymbolList.GetSymbolCount: Integer;
begin
  Result:= FList.Count;
end;

function TSymbolList.SymbolExists(const Name: string): Boolean;
begin
  Result:= FList.IndexOf(Name) <> -1;
end;

function TSymbolList.GetSymbol(const Name: string): TSymbol;
var Idx: Integer;
begin
  Idx:= FList.IndexOf(Name);
  if Idx <> -1 then
    Result:= PSymbol(FList.Objects[Idx])^
  else
    Result.Kind:= skError;
end;

procedure TSymbolList.Error(const Msg: string);
begin
  raise ESymbolListError.Create(Msg);
end;

procedure TSymbolList.FreeItem(Item: PSymbol);
begin
  if Item^.Kind = skVariable then // if it is a variable, free it
    Item^.Variable.Free;
  Dispose(Item);
end;

procedure TSymbolList.TestValidity(Kind: TSymbolKind; const Name: string);
var B: Boolean;
begin
  case Kind of
    skVariable: B:= Name <> '';
    skFunction: B:= IsSymbolValid(Name);
    else
      B:= IsOperatorValid(Name);
  end;
  if not B then
    Error(Format(ErrorSymbolNameInvalid, [Name]))
  else if FList.IndexOf(Name) <> -1 then
    Error(Format(ErrorDuplicateSymbol, [Name]));
end;

{}

procedure RegisterGlobalVariable(const Name: string; Variable: TExpression);
begin
  GlobalSymbols.RegisterVariable(Name, Variable);
end;

procedure RegisterGlobalFunction(const Name: string; Fn: TFunctionClass);
begin
  GlobalSymbols.RegisterFunction(Name, Fn);
end;

procedure RegisterGlobalOperator(const Name: string; Priority: TOperatorPriority;
  Operator: TOperatorClass);
begin
  GlobalSymbols.RegisterOperator(Name, Priority, Operator);
end;

function GlobalSymbolExists(const Name: string): Boolean;
begin
  Result:= GlobalSymbols.SymbolExists(Name);
end;

procedure GlobalUnregister(const Name: string);
begin
  GlobalSymbols.RemoveSymbol(Name);
end;

initialization
begin
  GlobalSymbols:= TSymbolList.Create;
end;

finalization
begin
  GlobalSymbols.Free;
  GlobalSymbols:= nil;
end;

end.
