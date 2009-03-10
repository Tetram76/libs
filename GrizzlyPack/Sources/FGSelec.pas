unit FGSelec;

{$I GrizzlyDefine.inc}

interface

uses SysUtils, Classes, DB, UFGFilt {$IFDEF GZ_D6}, Variants{$ENDIF};

const
  Links: array[0..1, 0..1] of string = (('AND', msgAND), ('OR', msgOR));

type
  TOperatorMode = (omSQL, omFilter);

  TOperator = (oEqual, oNotEqual, oSuperior, oInferior, oSuperiorEqual,
    oInferiorEqual, oBeginningBy, oEndingBy, oContaining, oIsNull, oIsNotNull);
  TOperatorParam = (opName, opSymbol, opBefore, opAfter, opWithValue);

  POperatorTab = ^TOperatorTab;
  TOperatorTab = array[Low(TOperator)..High(TOperator),
                       Low(TOperatorParam)..High(TOperatorParam)] of string;

  TOperators = class(TObject)
  private
    FCount: Integer;
    FTableau: POperatorTab;
    function GetStrings(Idx: TOperator): string;
    function GetOperators(Idx: TOperator): string;
    function GetStringBefore(Idx: TOperator): string;
    function GetStringAfter(Idx: TOperator): string;
    function GetWithValue(Idx: TOperator): Boolean;
  public
    constructor Create(AMode: TOperatorMode);
    property Count: Integer read FCount;
    procedure AssignTo(AStrings: TStrings);
    property Strings[Idx: TOperator]: string read GetStrings;
    property Operators[Idx: TOperator]: string read GetOperators;
    property StringBefore[Idx: TOperator]: string read GetStringBefore;
    property StringAfter[Idx: TOperator]: string read GetStringAfter;
    property WithValue[Idx: TOperator]: Boolean read GetWithValue;
  end;

  TSelectionList = class;

  TBrackets = class(TPersistent)
  private
    FOpenCount, FCloseCount: Byte;
  public
    procedure Open;
    procedure RemoveOpen;
    procedure Close;
    procedure RemoveClose;
  published
    property OpenCount: Byte read FOpenCount write FOpenCount;
    property CloseCount: Byte read FCloseCount write FCloseCount;
  end;

  TSelectionLine = class(TCollectionItem)
  private
    FField: Integer;
    FOperator: TOperator;
    FValue: string;
    FDisplayValue: string;
    FLink: Integer;
    FBrackets: TBrackets;
    {}
    procedure SetBrackets(const Value: TBrackets);
    function GetParent: TSelectionList;
  protected
    function GetFilterLine: string;
    function GetSQLLine: string;
    function GetSQLDisplayLine: string;
    function GetFilterDisplayLine: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    {}
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    {}
    property Parent: TSelectionList read GetParent;
    {}
    property FilterLine: string read GetFilterLine;
    property SQLLine: string read GetSQLLine;
    property FilterDisplayLine: string read GetFilterDisplayLine;
    property SQLDisplayLine: string read GetSQLDisplayLine;
  published
    property Field: Integer read FField write FField;
    property Operator: TOperator read FOperator write FOperator;
    property Value: string read FValue write FValue;
    property DisplayValue: string read FDisplayValue write FDisplayValue;
    property Link: Integer read FLink write FLink;
    property Brackets: TBrackets read FBrackets write SetBrackets;
  end;

  TSelectionList = class(TCollection)
  private
    FFieldNames: TStringList;
    FFieldLabels: TStringList;
    FMode: TOperatorMode;
  protected
    function GetFieldNames(Index: Integer): string;
    function GetFieldLabels(Index: Integer): string;
    function GetFields(Index: Integer): TField;
    function GetSelectionAsSQL: string;
    function GetSelectionAsFilter: string;
    function GetDisplayLines(AIndex: Integer): string;
    function GetFilterLines(AIndex: Integer): string;
    function GetLineCount: Integer;
    function GetLines(AIndex: Integer): TSelectionLine;
  public
    constructor Create;
    destructor Destroy; override;
    {}
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    {}
    procedure ClearAll;
    {}
    function AddField(ALabel, AName: string; AField: TField): Integer;
    procedure DeleteField(AIndex: Integer);
    function IndexOfFieldByName(AName: string): Integer;
    function IndexOfFieldByLabel(ALabel: string): Integer;
    procedure AutoAddFields(ADataSet: TDataSet; ATagValue: Integer);
    function AddLine(ALinkIndex, AFieldIndex: Integer; AOperator: TOperator;
      AValue, ADisplayValue: string): Integer;
    procedure DeleteLine(AIndex: Integer);
    {}
    property Mode: TOperatorMode read FMode write FMode;
    {}
    property SelectionAsSQL: string read GetSelectionAsSQL;
    property SelectionAsFilter: string read GetSelectionAsFilter;
    {}
    property FieldNames[Index: Integer]: string read GetFieldNames;
    property FieldLabels[Index: Integer]: string read GetFieldLabels;
    property Fields[Index: Integer]: TField read GetFields;
    property FilterLines[AIndex: Integer]: string read GetFilterLines;
    {}
    property Lines[AIndex: Integer]: TSelectionLine read GetLines;
    property LineCount: Integer read GetLineCount;
    property DisplayLines[AIndex: Integer]: string read GetDisplayLines;
  end;

var
  SQLOperators: TOperators;
  FilterOperators: TOperators;
  SQLDateFormat : string;
  SQLTimeFormat : string;
  SQLDateTimeFormat : string;
  SQLDateQuote : Char;
  SQLBoolTrue : string;
  SQLBoolFalse : string;
  SQLBoolQuote : Char;

implementation

{ TBrackets }

procedure TBrackets.Open;
begin
  if OpenCount < 255 then
    Inc(FOpenCount);
end;

procedure TBrackets.RemoveOpen;
begin
  if OpenCount > 0 then
    Dec(FOpenCount);
end;

procedure TBrackets.Close;
begin
  if CloseCount < 255 then
    Inc(FCloseCount);
end;

procedure TBrackets.RemoveClose;
begin
  if CloseCount > 0 then
    Dec(FCloseCount);
end;

{ TSelectionLine }

constructor TSelectionLine.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  {}
  FBrackets:= TBrackets.Create;
end;

destructor TSelectionLine.Destroy;
begin
  FBrackets.Free;
  {}
  inherited Destroy;
end;

Const
  SeparatorValueDisplayValue = #13#1#2#10;
  
procedure TSelectionLine.SaveToStream(AStream: TStream);
var
  i: Integer;
  Valeur : string;
begin
  AStream.Write(Brackets.OpenCount, SizeOf(Brackets.OpenCount));
  AStream.Write(Brackets.CloseCount, SizeOf(Brackets.CloseCount));
  AStream.Write(Field, SizeOf(Field));
  i:= Integer(Operator);
  AStream.Write(i, SizeOf(i));
  //Stockage nouvelle formule en stockant aussi DisplayValue... séparé par #13#1#2#10
  Valeur := Value + SeparatorValueDisplayValue + DisplayValue;
  i:= Length(Valeur);
  AStream.Write(i, SizeOf(i));
  AStream.Write(Pointer(Valeur)^, i);
  AStream.Write(Link, SizeOf(Link));
end;

procedure TSelectionLine.LoadFromStream(AStream: TStream);
var
  i: Integer;
begin
  AStream.ReadBuffer(Brackets.FOpenCount, SizeOf(Brackets.FOpenCount));
  AStream.ReadBuffer(Brackets.FCloseCount, SizeOf(Brackets.FCloseCount));
  AStream.ReadBuffer(FField, SizeOf(FField));
  AStream.ReadBuffer(i, SizeOf(i));
  FOperator:= TOperator(i);
  AStream.ReadBuffer(i, SizeOf(i));
  SetLength(FValue, i);
  AStream.ReadBuffer(Pointer(FValue)^, i);
  i := Pos(SeparatorValueDisplayValue, FValue);
  if i <= 0 then
    FDisplayValue := FValue
  else
  begin
    FDisplayValue := Copy(FValue, i + Length(SeparatorValueDisplayValue), Length(FValue) - i - Length(SeparatorValueDisplayValue) + 1);
    FValue := Copy(FValue, 1, i - 1);
  end;
  AStream.ReadBuffer(FLink, SizeOf(FLink));
end;

function TSelectionLine.GetFilterDisplayLine: string;
begin
  if Brackets.OpenCount > 0 then
    Result:= StringOfChar('(', Brackets.OpenCount)
  else
    Result:= '';
  Result:= Result + Parent.FieldLabels[Field]
    + ' ' + FilterOperators.Strings[Operator];
  if FilterOperators.WithValue[Operator] then
    Result:= Result + ' ' + DisplayValue;
  if Brackets.CloseCount > 0 then
    Result:= Result + StringOfChar(')', Brackets.CloseCount)
end;

function TSelectionLine.GetSQLDisplayLine: string;
begin
  if Brackets.OpenCount > 0 then
    Result:= StringOfChar('(', Brackets.OpenCount)
  else
    Result:= '';
  Result:= Result + Parent.FieldLabels[Field]
    + ' ' + SQLOperators.Strings[Operator];
  if SQLOperators.WithValue[Operator] then
    Result:= Result + ' ' + DisplayValue;
  if Brackets.CloseCount > 0 then
    Result:= Result + StringOfChar(')', Brackets.CloseCount)
end;

function TSelectionLine.GetFilterLine: string;
var
  AField: TField;
  ADataType: TFieldType;
  AFieldName: string;
  ALUKF: string;
  ARes: variant;
  AValue: string;
  function GetTheString(ADT: TFieldType; AFN, AFV, AOP: string): string;
  begin
  end;
begin
  if Brackets.OpenCount > 0 then
    Result:= StringOfChar('(', Brackets.OpenCount)
  else
    Result:= '';
  AField:= TField(Parent.FFieldNames.Objects[Field]);
  AFieldName:= Parent.FieldNames[Field];
  AValue:= Value;
  if AField = nil then
    ADataType:= ftString
  else
  begin
    if AField.FieldKind = fkLookup then
    begin
      { Le lookup est pseudo-géré seulement en cas de demande d'égalité }
      if Operator = oEqual then
      begin
        {Et on ne traite pour le moment que les clefs mono-champs...}

        {Donc dans ce cas :
          - Retrouver les champs à chercher... LookupKeyFields
          - Voir si ça existe... Lookup sur LookupDataset
          - Créer la ou les lignes de filtre sur les champs clefs.
          - Si ça n'existe pas... Il faudra ne rien retourner... Comment faire ?
        }
        ALUKF:= AField.LookupKeyFields;
        if pos(';', ALUKF) <= 0 then
        begin
          ARes:= AField.LookupDataSet.Lookup(AField.LookupResultField, Value, ALUKF);
          if not VarIsEmpty(ARes) then
          begin
            AField:= AField.Dataset.FindField(AField.KeyFields);
            AFieldName:= AField.FieldName;
            AValue:= ARes;
          end
          else
          begin
            {... Il faudrait faire en sorte que la sélection soit nulle...}
          end;
        end;
      end;
    end;

    ADataType:= AField.DataType;
  end;
  case ADataType of
    ftString, ftWideString, ftDate, ftTime, ftDateTime, ftBoolean:
      begin
        Result:= Result + AFieldName
          + ' ' + FilterOperators.Operators[Operator];
        if FilterOperators.WithValue[Operator] then
          Result:= Result + ' ' +
            AnsiQuotedStr(Format('%s%s%s',
              [FilterOperators.StringBefore[Operator],
               AValue,
               FilterOperators.StringAfter[Operator]]), '''');
      end;
    ftSmallInt, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD,
    ftAutoInc, ftLargeInt:
      begin
        Result:= Result + AFieldName
          + ' ' + FilterOperators.Operators[Operator];
        if FilterOperators.WithValue[Operator] then
          Result:= Result + ' '
            + FilterOperators.StringBefore[Operator]
            + AValue
            + FilterOperators.StringAfter[Operator] + '';
      end
  else
    Result:= '';
  end;
  if (Result <> '') and (Brackets.CloseCount > 0) then
    Result:= Result + StringOfChar(')', Brackets.CloseCount)
end;

function TSelectionLine.GetSQLLine: string;
var
  AField: TField;
  ADataType: TFieldType;
  FTemp : string;
  FDate : TDateTime;
begin
  if Brackets.OpenCount > 0 then
    Result:= StringOfChar('(', Brackets.OpenCount)
  else
    Result:= '';
  AField:= TField(Parent.FFieldNames.Objects[Field]);
  if AField = nil then
    ADataType:= ftString
  else
    ADataType:= AField.DataType;

  Result:= Result + Parent.FieldNames[Field]
    + ' ' + SQLOperators.Operators[Operator];

  if SQLOperators.WithValue[Operator] then
  begin
    case ADataType of // Ajout ftMemo 29/01/2001 pour gérer VARCHAR > 255 = Memo BDE
      ftString, ftWideString, ftMemo:
        begin
          Result:= Result + ' ' +
            AnsiQuotedStr(Format('%s%s%s',
            [SQLOperators.StringBefore[Operator],
             Value,
             SQLOperators.StringAfter[Operator]]), '''');
        end;
      ftBoolean :
        begin
          if Length(Value) > 0 then
          begin
            if Value[1] in ['F', 'f', '0'] then
              FTemp := SQLBoolFalse
            else
              FTemp := SQLBoolTrue;
          end
          else
            FTemp := SQLBoolFalse;
          FTemp := Format('%s%s%s',
              [SQLOperators.StringBefore[Operator],
               FTemp,
               SQLOperators.StringAfter[Operator]]);

          if SQLBoolQuote <> #0 then
            Result:= Result + ' ' + AnsiQuotedStr(FTemp, SQLBoolQuote)
          else
            Result:= Result + ' ' + FTemp;
        end;
      ftDate, ftTime, ftDateTime:
        begin
          case ADataType of
          ftDate : FTemp := FormatDateTime(SQLDateFormat, StrToDate(Value));
          ftTime : FTemp := FormatDateTime(SQLTimeFormat, StrToTime(Value));
          ftDateTime :
            begin
              FDate := StrToDateTime(Value);
              if Frac(FDate) = 0 then
                FTemp := FormatDateTime(SQLDateFormat, StrToDate(Value))
              else
                FTemp := FormatDateTime(SQLDateFormat + ' ' + SQLTimeFormat, StrToDateTime(Value));
            end;
          end;
          FTemp := Format('%s%s%s',
              [SQLOperators.StringBefore[Operator],
               FTemp,
               SQLOperators.StringAfter[Operator]]);

          if SQLDateQuote <> #0 then
            Result:= Result + ' ' + AnsiQuotedStr(FTemp, SQLDateQuote)
          else
            Result:= Result + ' ' + FTemp;
        end;
      ftSmallInt, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD,
      ftAutoInc, ftLargeInt:
        begin
          if SQLOperators.WithValue[Operator] then
            Result:= Result + ' '
              + SQLOperators.StringBefore[Operator]
              + Value
              + SQLOperators.StringAfter[Operator] + '';
        end
    else
      Result:= '';
    end;
  end;
  if Brackets.CloseCount > 0 then
    Result:= Result + StringOfChar(')', Brackets.CloseCount)
end;

procedure TSelectionLine.SetBrackets(const Value: TBrackets);
begin
  FBrackets.FOpenCount:= Value.OpenCount;
  FBrackets.FCloseCount:= Value.CloseCount;
end;

function TSelectionLine.GetParent: TSelectionList;
begin
  Result:= TSelectionList(Collection);
end;

{ TSelectionList }

constructor TSelectionList.Create;
begin
  inherited Create(TSelectionLine);
  FFieldNames:= TStringList.Create;
  FFieldLabels:= TStringList.Create;
  FMode:= omFilter;
end;

destructor TSelectionList.Destroy;
begin
  FreeAndNil(FFieldNames);
  FreeAndNil(FFieldLabels);
  inherited Destroy;
end;

procedure TSelectionList.SaveToStream(AStream: TStream);
var
  i: Integer;
begin
  i:= Count;
  AStream.Write(i, SizeOf(i));
  if Count > 0 then
    for i:= 0 to Count - 1 do
      TSelectionLine(Lines[i]).SaveToStream(AStream);
  {On ne sauvegarde pas les titres et noms de champs qui sont en
   théorie prévus à la conception}
  {C'est une limite de conception qui ne doit pas être oubliée}
end;

procedure TSelectionList.LoadFromStream(AStream: TStream);
var
  i, k: Integer;
  ASelL: TSelectionLine;
begin
  Clear;
  AStream.Read(k, SizeOf(k));
  if k > 0 then
    for i:= 1 to k do
    begin
      ASelL:= TSelectionLine(Add);
      ASelL.LoadFromStream(AStream);
    end;
end;

procedure TSelectionList.ClearAll;
begin
  Clear;
  FFieldNames.Clear;
  FFieldLabels.Clear;
end;

function TSelectionList.GetLines(AIndex: Integer): TSelectionLine;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result:= TSelectionLine(inherited Items[AIndex])
  else
    Result:= nil;
end;

function TSelectionList.GetLineCount: Integer;
begin
  Result:= Count;
end;

function TSelectionList.GetSelectionAsSQL: string;
var
  i, OC, CC: Integer;
  TheLine: TSelectionLine;
begin
  Result:= '';
  if Count > 0 then
  begin
    OC:= 0;
    CC:= 0;
    for i:= 0 to Count - 1 do
    begin
      TheLine:= Lines[i];
      Inc(OC, TheLine.Brackets.OpenCount);
      Inc(CC, TheLine.Brackets.CloseCount);
      if i > 0 then
        Result:= Result + Links[TheLine.Link, 0] + ' ';
      Result:= Result + '(' + TheLine.SQLLine + ') ';
    end;
    if OC > CC then
      Result:= Result + StringOfChar(')', OC - CC)
    else if OC < CC then
      Result:= StringOfChar('(', CC - OC);
    Result:= Format('(%s)', [Result]);
  end;
end;

function TSelectionList.GetSelectionAsFilter: string;
var
  i, OC, CC: Integer;
  TheLine: TSelectionLine;
begin
  Result:= '';
  if Count > 0 then
  begin
    OC:= 0;
    CC:= 0;
    for i:= 0 to Count - 1 do
    begin
      TheLine:= Lines[i];
      Inc(OC, TheLine.Brackets.OpenCount);
      Inc(CC, TheLine.Brackets.CloseCount);
      if i > 0 then
        Result:= Result + Links[TheLine.Link, 0] + ' ';
      Result:= Result + '(' + TheLine.FilterLine + ') ';
    end;
    if OC > CC then
      Result:= Result + StringOfChar(')', OC - CC)
    else if OC < CC then
      Result:= StringOfChar('(', CC - OC);
    Result:= Format('(%s)', [Result]);
  end;
end;

function TSelectionList.GetDisplayLines(AIndex: Integer): string;
var
  ALine: TSelectionLine;
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    ALine:= TSelectionLine(Lines[AIndex]);
    if AIndex > 0 then
    begin
      Result:= Links[ALine.Link, 1] + ' ';
    end
    else
      Result:= '';
    if Mode = omSQL then
      Result:= Result + ALine.SQLDisplayLine
    else
      Result:= Result + ALine.FilterDisplayLine;
  end
  else
    Result:= '';
end;

function TSelectionList.GetFilterLines(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    if Mode = omSQL then
      Result:= TSelectionLine(Lines[AIndex]).SQLLine
    else
      Result:= TSelectionLine(Lines[AIndex]).FilterLine;
  end
  else
    Result:= '';
end;

function TSelectionList.AddLine(ALinkIndex, AFieldIndex: Integer; AOperator: TOperator;
  AValue, ADisplayValue: string): Integer;
var TheLine: TSelectionLine;
begin
  TheLine:= TSelectionLine(Add);
  TheLine.Field:= AFieldIndex;
  TheLine.Operator:= AOperator;
  TheLine.Value:= AValue;
  TheLine.DisplayValue:= ADisplayValue;
  TheLine.Link:= ALinkIndex;
  Result:= Count - 1;
end;

procedure TSelectionList.DeleteLine(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  {$IFDEF VER130}
    Delete(AIndex);
  {$ELSE}
    Items[AIndex].Free;
  {$ENDIF}
end;

procedure TSelectionList.AutoAddFields(ADataSet: TDataSet; ATagValue: Integer);
var
  i: Integer;
  AField: TField;
begin
  if ADataset = nil then
    Exit;
  if ADataSet.FieldCount = 0 then
    Exit;
  for i:= 0 to ADataset.FieldCount - 1 do
    if (ATagValue <= 0) or (ATagValue = ADataSet.Fields[i].Tag) then
    begin
      AField:= ADataset.Fields[i];
      if AField.Origin <> '' then
        AddField(AField.DisplayLabel, AField.Origin, AField)
      else
        AddField(AField.DisplayLabel, AField.FieldName, AField);
    end;
end;

function TSelectionList.GetFieldNames(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FFieldNames.Count) then
    Result:= FFieldNames[Index]
  else
    Result:= '';
end;

function TSelectionList.GetFieldLabels(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FFieldLabels.Count) then
    Result:= FFieldLabels[Index]
  else
    Result:= '';
end;

function TSelectionList.GetFields(Index: Integer): TField;
begin
  if (Index >= 0) and (Index < FFieldNames.Count) then
    Result:= TField(FFieldNames.Objects[Index])
  else
    Result:= nil;
end;

function TSelectionList.AddField(ALabel, AName: string; AField: TField): Integer;
begin
  if AField <> nil then
  begin
    if AName <> '' then
      Result:= FFieldNames.AddObject(AName, AField)
    else
    begin
      if AField.Origin <> '' then
        Result:= FFieldNames.AddObject(AField.Origin, AField)
      else
        Result:= FFieldNames.AddObject(AField.FieldName, AField);
    end;
    FFieldLabels.Add(ALabel);
  end
  else
  begin
    Result:= FFieldNames.AddObject(AName, nil);
    FFieldLabels.Add(ALabel);
  end;
end;

procedure TSelectionList.DeleteField(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FFieldNames.Count) then
    FFieldNames.Delete(AIndex);
  if (AIndex >= 0) and (AIndex < FFieldLabels.Count) then
    FFieldLabels.Delete(AIndex);
end;

function TSelectionList.IndexOfFieldByName(AName: string): Integer;
begin
  Result:= FFieldNames.IndexOf(AName);
end;

function TSelectionList.IndexOfFieldByLabel(ALabel: string): Integer;
begin
  Result:= FFieldNames.IndexOf(ALabel);
end;

{ TOperators }

const
  OperateursFilter: array[Low(TOperator)..High(TOperator), 0..4] of string =
  ((msgEqual, '=', '', '', ''),
    (msgNotEqual, '<>', '', '', ''),
    (msgGreater, '>', '', '', ''),
    (msgSmaller, '<', '', '', ''),
    (msgGreaterOrEqual, '>=', '', '', ''),
    (msgSmallerOrEqual, '<=', '', '', ''),
    (msgBeginningWith, '=', '', '*', ''),
    (msgFinishingWith, '=', '*', '', ''),
    (msgContaining, '=', '*', '*', ''),
    (msgIsNull, '=null', '', '', '1'),
    (msgIsNotNull, '<>null', '', '', '1')
    );

  OperateursSQL: array[Low(TOperator)..High(TOperator), 0..4] of string =
  ((msgEqual, '=', '', '', ''),
    (msgNotEqual, '<>', '', '', ''),
    (msgGreater, '>', '', '', ''),
    (msgSmaller, '<', '', '', ''),
    (msgGreaterOrEqual, '>=', '', '', ''),
    (msgSmallerOrEqual, '<=', '', '', ''),
    (msgBeginningWith, 'like', '', '%', ''),
    (msgFinishingWith, 'like', '%', '', ''),
    (msgContaining, 'like', '%', '%', ''),
    (msgIsNull, 'is null', '', '', '1'),
    (msgIsNotNull, 'is not null', '', '', '1')
    );

constructor TOperators.Create(AMode: TOperatorMode);
begin
  inherited Create;
  if AMode = omSQL then
  begin
    FCount:= Integer(High(OperateursSQL)) - Integer(Low(OperateursSQL)) + 1;
    FTableau:= @OperateursSQL;
  end
  else
  begin
    FCount:= Integer(High(OperateursFilter)) - Integer(Low(OperateursFilter)) + 1;
    FTableau:= @OperateursFilter;
  end;
end;

function TOperators.GetStrings(Idx: TOperator): string;
begin
  if (Idx >= Low(TOperator)) and (Idx <= High(TOperator)) then
    Result:= FTableau^[Idx, opName]
  else
    Result:= '';
end;

function TOperators.GetOperators(Idx: TOperator): string;
begin
  if (Idx >= Low(TOperator)) and (Idx <= High(TOperator)) then
    Result:= FTableau^[Idx, opSymbol]
  else
    Result:= '';
end;

function TOperators.GetStringBefore(Idx: TOperator): string;
begin
  if (Idx >= Low(TOperator)) and (Idx <= High(TOperator)) then
    Result:= FTableau^[Idx, opBefore]
  else
    Result:= '';
end;

function TOperators.GetStringAfter(Idx: TOperator): string;
begin
  if (Idx >= Low(TOperator)) and (Idx <= High(TOperator)) then
    Result:= FTableau^[Idx, opAfter]
  else
    Result:= '';
end;

function TOperators.GetWithValue(Idx: TOperator): Boolean;
begin
  if (Idx >= Low(TOperator)) and (Idx <= High(TOperator)) then
    Result:= (FTableau^[Idx, opWithValue] = '')
  else
    Result:= True;
end;

procedure TOperators.AssignTo(AStrings: TStrings);
var i: TOperator;
begin
  if AStrings.Count > 0 then
    AStrings.Clear;
  for i:= Low(TOperator) to High(TOperator) do
    AStrings.Add(Strings[i]);
end;

initialization
  SQLOperators:= TOperators.Create(omSQL);
  FilterOperators:= TOperators.Create(omFilter);
  SQLDateFormat:= ShortDateFormat;
  SQLTimeFormat:= ShortTimeFormat;
  SQLDateQuote:= '''';
  SQLBoolTrue:= '1';
  SQLBoolFalse:= '0';
  SQLBoolQuote:= #0;
finalization
  SQLOperators.Free;
  FilterOperators.Free;
end.

