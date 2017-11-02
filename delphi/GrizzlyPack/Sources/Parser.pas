unit Parser;

interface

uses Classes, SysUtils;

type
  EParserError = class(Exception);

  TCharSet = set of Char;

  TCustomParser = class(TPersistent)
  private
    FExpression: string;
  protected
    ParseIndex: Integer;
    {}
    function CanContinue: Boolean;
    {}
    property Expression: string read FExpression write FExpression;
  public
    function GotoNextToken(IgnoredChars: TCharSet): Boolean;
    {}
    function CurrentChar: Char;
    function SkipChar: Char;
    function IsCharValid(AllowedChars: TCharSet): Boolean;
    function ReadValue(AllowedChars: TCharSet): string;
    {}
    procedure Error(const ErrorMessage: string);
    procedure ErrorFmt(const ErrorMessage: string; Params: array of const);
  end;

  TNumericType = (ntInteger, ntFloat);

  TNumericValue = record
    Kind: TNumericType;
    case TNumericType of
      ntInteger: (AsInteger: Integer);
      ntFloat: (AsFloat: Double);
  end; 

  TCustomEvalParser = class(TCustomParser)
  private
    FStrQuote: Char;
    FDecimalChar: Char;
    FIgnoredChars: TCharSet;
    FSymbolChars: TCharSet;
  public
    function ReadString: string;
    function ReadNumeric: TNumericValue;
    function ReadSymbol: string;
    {}
    property StrQuote: Char read FStrQuote write FStrQuote;
    property DecimalChar: Char read FDecimalChar write FDecimalChar;
    property SymbolChars: TCharSet read FSymbolChars write FSymbolChars;
    property IgnoredChars: TCharSet read FIgnoredChars write FIgnoredChars;
  end;

resourcestring
  ErrorNumeric = 'Expression numérique incorrecte : "%s"';
  ErrorStringIncomplete = 'Expression chaîne non terminée';
//  ErrorNumeric = 'Invalid numeric expression : "%s"';
//  ErrorStringIncomplete = 'String expression not finished';

implementation

{ TCustomParser }

function TCustomParser.CurrentChar: Char;
begin
  Result:= Expression[ParseIndex];
end;

function TCustomParser.SkipChar: Char;
begin
  Result:= Expression[ParseIndex];
  Inc(ParseIndex);
end;

function TCustomParser.IsCharValid(AllowedChars: TCharSet): Boolean;
begin
  Result:= CanContinue and (CurrentChar in AllowedChars); 
end;

function TCustomParser.GotoNextToken(IgnoredChars: TCharSet): Boolean;
begin
  while IsCharValid(IgnoredChars) do
    Inc(ParseIndex);
  Result:= CanContinue;
end;

function TCustomParser.ReadValue(AllowedChars: TCharSet): string;
var Beginning: Integer;
begin
  Beginning:= ParseIndex;
  while IsCharValid(AllowedChars) do
    Inc(ParseIndex);
  Result:= Copy(Expression, Beginning, ParseIndex - Beginning);
end;

function TCustomParser.CanContinue: Boolean;
begin
  Result:= ParseIndex <= Length(Expression);
end;

procedure TCustomParser.Error(const ErrorMessage: string);
begin
  raise EParserError.Create(ErrorMessage);
end;

procedure TCustomParser.ErrorFmt(const ErrorMessage: string; Params: array of const);
begin
  raise EParserError.CreateFmt(ErrorMessage, Params);
end;

{ TCustomEvalParser }

function TCustomEvalParser.ReadNumeric: TNumericValue;
var S: string;
  procedure RaiseNumericError;
  begin
    ErrorFmt(ErrorNumeric, [S]);
  end;
  procedure ReadExponent;
  begin
    SkipChar;
    if IsCharValid(['+', '-', '0'..'9']) then
    begin
      S:= S + 'E' + SkipChar;
      if IsCharValid(['0'..'9']) then
        S:= S + ReadValue(['0'..'9'])
      else
        RaiseNumericError;
    end else
      RaiseNumericError;
  end;
begin
  Result.Kind:= ntInteger;
  S:= ReadValue(['0'..'9']);
  if CanContinue then
  begin
    if CurrentChar in ['E', 'e'] then
    begin
      Result.Kind:= ntFloat;
      ReadExponent;
    end else if CurrentChar = DecimalChar then
    begin
      Result.Kind:= ntFloat;
      S:= S + DecimalSeparator;
      SkipChar;
      if IsCharValid(['0'..'9']) then
      begin
        S:= S + ReadValue(['0'..'9']);
        if IsCharValid(['E', 'e']) then
          ReadExponent;
      end else
        RaiseNumericError;
    end;
  end;
  if IsCharValid(SymbolChars) then
    RaiseNumericError;
  Result.AsFloat:= StrToFloat(S);
  if Result.Kind = ntInteger then
  begin
    if Result.AsFloat > MaxInt then
      Result.Kind:= ntFloat
    else
      Result.AsInteger:= StrToInt(S);
  end;
end;

function TCustomEvalParser.ReadString: string;
var Counter: Integer;
    Complete: Boolean;
    Buffer: array[0..255] of Char;
  procedure FlushBuffer;
  var Dest: Integer;
  begin
    Dest:= Length(Result) + 1;
    SetLength(Result, Length(Result) + Counter + 1);
    Move(Buffer[0], Result[Dest], Counter + 1);
    Counter:= 0;
  end;
begin
  Complete:= False;
  Counter:= -1;
  Result:= '';
  SkipChar; // ignores the first quote
  while CanContinue do
  begin
    if CurrentChar = StrQuote then
    begin
      SkipChar;
      if not IsCharValid([StrQuote]) then
      begin
        Complete:= True;
        Break;
      end;
    end;
    Inc(Counter);
    if Counter > 255 then
      FlushBuffer;
    Buffer[Counter]:= CurrentChar;
    SkipChar;
  end;
  if not Complete then
    Error(ErrorStringIncomplete);
  FlushBuffer;
end;

function TCustomEvalParser.ReadSymbol: string;
begin
  Result:= ReadValue(SymbolChars);
end;

end.
