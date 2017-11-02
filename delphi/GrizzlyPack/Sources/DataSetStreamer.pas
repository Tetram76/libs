{***************************************************************
 *
 * Unit Name: DataSetStreamer
 * Purpose  : Component that allows to streams the datas of any TDataSet in various
              format.
 * Author   : Alexandre GUILLIEN
 * History  :
 *
 *  14/09/2000 : V 2.0  : Complete rebuild. Formats are now defined by classes.
                          It remains however backward compatible.
 *  08/08/2000 : V 1.2  : New export format added : ConstText Format.
 *  25/07/2000 : V 1.1  : Lone procs are replaced by a class : TDataSetStreamer.
                          New export format added : CSV Format.
 *  01/07/1999 : V 1.0  : First release
 *
 ****************************************************************}

{ Important : when re-loading informations from a file, whatever the format,
    you must be sure that the field structure of the re-loading DataSet has
    EXACTLY the same structure as the DataSet that saved the records. There
    is not any control upon it.
  However, there may be exceptions :
    In DataSet Format, the following datatypes are compatible :
      ftString, ftBytes and ftVarBytes
      ftInteger and ftAutoInc
      ftSmallint and ftWord
      ftFloat, ftDate, ftTime and ftDateTime
      ftCurrency and ftBCD
      All blob fields
    In CSV Format, all datatypes are compatible ... provided the AsString
    value is assignable with the given value.
    In ConstText Format, the structure must be EXACTLY the same.
}

unit DataSetStreamer;

{$I GrizzlyDefine.INC}

interface

uses DB, Classes, SysUtils;

type
  TDataSetFormat = class
  private
    FDataSet: TDataSet;
    FExportCalcFields: Boolean;
  protected
    procedure ErrorFieldType(const ErrorMsg: string; FieldType: TFieldType);
    {}
    property DataSet: TDataSet read FDataSet;
    property ExportCalcFields: Boolean read FExportCalcFields;
  public
    procedure SaveRecordToStream(Stream: TStream); virtual; abstract;
    function LoadRecordFromStream(Stream: TStream): Boolean; virtual; abstract;
    {}
    procedure WriteHeader(Stream: TStream); virtual;
    procedure WriteTrailer(Stream: TStream); virtual;
    { PrY }
    function ReadHeader(Stream: TStream): Boolean; virtual;
    { /PrY}
    constructor Create(DataSet: TDataSet; ExportCalcFields: Boolean);
  end;

  TDataSetFormatClass = class of TDataSetFormat;

  TExportFormat = (efDataSet, efTextCSV, efTextConst);

  TGenericFormat = class(TDataSetFormat)
  public
    procedure SaveRecordToStream(Stream: TStream); override;
    function LoadRecordFromStream(Stream: TStream): Boolean; override;
    procedure WriteTrailer(Stream: TStream); override;
  end;

  TCSVFormat = class(TDataSetFormat)
  public
    function ReadHeader(Stream: TStream): Boolean; override;
    procedure WriteHeader(Stream: TStream); override;
    procedure SaveRecordToStream(Stream: TStream); override;
    function LoadRecordFromStream(Stream: TStream): Boolean; override;
  end;

  TTextConstFormat = class(TDataSetFormat)
  public
    procedure SaveRecordToStream(Stream: TStream); override;
    function LoadRecordFromStream(Stream: TStream): Boolean; override;
  end;

  TDataSetStreamer = class
  private
    FDataSet: TDataSet;
    FFormat: TDataSetFormatClass;
    FExportCalc: Boolean;
  protected
  public
    constructor Create(DataSet: TDataSet; Format: TExportFormat;
      ExportCalc: Boolean);
    {}
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    {}
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    {}
    property DataSet: TDataSet read FDataSet write FDataSet;
    property Format: TDataSetFormatClass read FFormat write FFormat;
    property ExportCalcFields: Boolean read FExportCalc write FExportCalc;
  end;

  procedure CSVReadValue(const Rec: string; List: TStrings);
  procedure CSVReadValueFromStream(Stream: TStream; List: TStrings);

  {}

  procedure ReadCSVFromFile(FileName: string; DataSet: TDataSet);
  procedure ReadCSVFromStream(Stream: TStream; DataSet: TDataSet);
  procedure ReadCSVFromString(S: string; DataSet: TDataSet);


const
  DataSetFormats: array[Low(TExportFormat)..High(TExportFormat)] of TDataSetFormatClass =
    (TGenericFormat, TCSVFormat, TTextConstFormat);

  efGeneric = efDataSet;

implementation

uses FGUtils, TypInfo, TetraClasses;

resourcestring
  {$IFDEF ENGLISH}
  msgCantProcessFieldType = 'Unable to save this field type : %s';
  {$ENDIF}
  {$IFDEF FRENCH}
  msgCantProcessFieldType = 'Type de champs %s impossible à sauvegarder';
  {$ENDIF}

{ TDataSetStreamer }

constructor TDataSetStreamer.Create(DataSet: TDataSet; Format: TExportFormat;
  ExportCalc: Boolean);
begin
  FDataSet:= DataSet;
  Self.Format:= DataSetFormats[Format];
  Self.ExportCalcFields:= ExportCalc;
  if Self.Format = nil then
    Self.Format:= TGenericFormat;
end;

procedure TDataSetStreamer.SaveToFile(FileName: string);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TDataSetStreamer.LoadFromFile(FileName: string);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TDataSetStreamer.SaveToStream(Stream: TStream);
var Bookmark: TBookmarkStr;
    F: TDataSetFormat;
begin
  F:= Format.Create(DataSet, ExportCalcFields);
  try
    Bookmark:= DataSet.Bookmark;
    DataSet.DisableControls;
    try
      DataSet.First;
      F.WriteHeader(Stream);
      while not DataSet.EOF do
      begin
        F.SaveRecordToStream(Stream);
        DataSet.Next;
      end;
      F.WriteTrailer(Stream);
      DataSet.Bookmark:= Bookmark;
    finally
      DataSet.EnableControls;
    end;
  finally
    F.Free;
  end;
end;

procedure TDataSetStreamer.LoadFromStream(Stream: TStream);
var
  StreamSize: Integer;
  F: TDataSetFormat;
  P: Int64;
begin
  F:= Format.Create(DataSet, ExportCalcFields);
  try
    while not DataSet.IsEmpty do
      DataSet.Delete;
    StreamSize:= Stream.Size;
    { PrY }
    P := Stream.Position;
    if not F.ReadHeader(Stream) then
      Stream.Position := P;
    { /PrY }
    while Stream.Position <> StreamSize do
    begin
      DataSet.Append;
      if F.LoadRecordFromStream(Stream) then
        DataSet.Post
      else
      begin
        DataSet.Cancel;
        Break;
      end;
    end;
    DataSet.First;
  finally
    F.Free;
  end;
end;

{ TDataSetFormat }

constructor TDataSetFormat.Create(DataSet: TDataSet; ExportCalcFields: Boolean);
begin
  FDataSet:= DataSet;
  FExportCalcFields:= ExportCalcFields;
end;

procedure TDataSetFormat.ErrorFieldType(const ErrorMsg: string; FieldType: TFieldType);
begin
  raise EDatabaseError.Create(Format(ErrorMsg,
    [GetEnumName(TypeInfo(TFieldType), Integer(FieldType))]));
end;

function TDataSetFormat.ReadHeader(Stream: TStream): Boolean;
var
  X: TUsefulStream;
  i: Integer;
  DatasetName: String;
  FieldName: String;
  FieldDataType: TFieldType;
  FieldSize: Integer;
  FieldIsRequired: Boolean;
begin

  X := TUsefulStream(Stream);

  DatasetName := X.ReadString;
  i := X.ReadLongInt;

  //Puisqu'il faut prévoir la compatibilité ascendante, il faut donc prévoir
  //des règles pour reconnaître les anciens fichiers.......... (Hein Pierre ?)
  if (i <= 0) or (i > 2000) or (WashedString(DatasetName) <> DatasetName) or (Length(DatasetName) > 64) then
  begin
    Result := False;
    Exit;
  end;

  Dataset.Close;
  Dataset.FieldDefs.Clear;

  for i := 0 to i - 1 do
  begin
    FieldName := X.ReadString;
    FieldDataType := TFieldType(X.ReadLongInt);
    FieldSize := X.ReadLongInt;
    FieldIsRequired := Boolean(X.ReadLongInt);
    Dataset.FieldDefs.Add(FieldName,FieldDataType,FieldSize,FieldIsRequired);
  end;

  if DatasetName <> '' then
    try
      Dataset.Name := DatasetName;
    except
    end;
  Dataset.Open;

  Result := true;
end;

procedure TDataSetFormat.WriteHeader(Stream: TStream);
var
  X: TUsefulStream;
  i: Integer;
  D: TFieldDef;
begin
  { Do nothing }
  X := TUsefulStream(Stream);
  X.WriteString(Dataset.Name);
  X.WriteLongInt(DataSet.FieldDefs.Count);
  for i := 0 to Dataset.FieldDefs.Count - 1 do
  begin
    D := Dataset.FieldDefs.Items[i];
    X.WriteString(D.Name);
    X.WriteLongInt(Ord(D.DataType));
    X.WriteLongInt(D.Size);
    X.WriteLongInt(Integer(D.Required));
  end;
end;

procedure TDataSetFormat.WriteTrailer(Stream: TStream);
begin
  { Do nothing }
end;

{ TGenericFormat }

procedure TGenericFormat.SaveRecordToStream(Stream: TStream);
var i: Integer;
    L: LongInt;
    LI: Int64;
    D: Double;
    C: Currency;
    HasData: Byte;
    F: TField;
    M: TMemoryStream;
begin
  M:= nil;
  try
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if ExportCalcFields or (F.FieldKind = fkData) then
      begin
        if F is TBlobField then
        begin
          if not Assigned(M) then
            M:= TMemoryStream.Create;
          M.Clear;
          TBlobField(F).SaveToStream(M);
          L:= M.Size;
          Stream.Write(L, SizeOf(LongInt));
          Stream.CopyFrom(M, 0);
        end else
        begin
          if F.DataType in [ftUnknown, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
            ftParadoxOle, ftDBaseOle, ftTypedBinary] then
              ErrorFieldType(msgCantProcessFieldType, F.DataType);
          if F.IsNull then
          begin
            HasData:= 0;
            Stream.Write(HasData, SizeOf(Byte));
          end else
          begin
            HasData:= 1;
            Stream.Write(HasData, SizeOf(Byte));
            case F.DataType of
              ftString:
              begin
                L:= Length(F.AsString);
                Stream.Write(L, SizeOf(LongInt));
                Stream.Write(F.AsString[1], L);
              end;
              ftInteger, ftAutoInc:
              begin
                L:= F.AsInteger;
                Stream.Write(L, SizeOf(LongInt));
              end;
              ftLargeint:
              begin
                LI:= F.AsVariant;
                Stream.Write(LI, SizeOf(Int64));
              end;
              ftSmallint, ftWord:
              begin
                L:= F.AsInteger;
                Stream.Write(L, SizeOf(SmallInt));
              end;
              ftBoolean:
              begin
                L:= Integer(F.AsBoolean);
                Stream.Write(L, SizeOf(Boolean));
              end;
              ftFloat, ftDate, ftTime, ftDateTime:
              begin
                D:= F.AsFloat;
                Stream.Write(D, SizeOf(Double));
              end;
              ftCurrency, ftBCD:
              begin
                C:= F.AsCurrency;
                Stream.Write(C, SizeOf(Currency));
              end;
              ftBytes, ftVarBytes:
              begin
                L:= F.DataSize;
                Stream.Write(L, SizeOf(LongInt));
                Stream.Write(F.AsString[1], L);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    M.Free;
  end;
end;

procedure TGenericFormat.WriteTrailer(Stream: TStream);
var L: Integer;
begin
  L:= -1; // $FFFFFFFF
  Stream.Write(L, SizeOf(Integer));
end;

function TGenericFormat.LoadRecordFromStream(Stream: TStream): Boolean;
var i: Integer;
    L: LongInt;
    LI: Int64;
    D: Double;
    C: Currency;
    HasData: Byte;
    F: TField;
    A: array[0..255] of Char;
    M: TMemoryStream;
    function ReadString: string;
    var R: Integer;
      function Min(I1, I2: Integer): Integer;
      begin
        if I1 > I2 then
          Result:= I2
        else
          Result:= I1;
      end;
    begin { Fonction pour lire les chaines si ces dernières sont > à 255 }
      Result:= '';
      Stream.Read(L, SizeOf(LongInt));
      while L > 0 do
      begin
        R:= Stream.Read(A, Min(L, 255));
        L:= L - R;
        A[R]:= #0; { Termine la chaîne A tel un PChar }
        Result:= Result + A;
      end;
    end;
begin
  Result:= False;
  M:= nil;
  try
    if DataSet.FieldCount = 0 then
    begin
      Stream.Read(HasData, 1);
      if HasData = $FF then
        Stream.Read(L, SizeOf(LongInt) - SizeOf(Byte))
      else
        Stream.Seek(-1, soFromCurrent);
      Exit;
    end;
    for i:= 0 to DataSet.FieldCount - 1 do
    begin
      F:= DataSet.Fields[i];
      if ExportCalcFields or (F.FieldKind = fkData) then
      begin
        if F is TBlobField then
        begin
          if not Assigned(M) then
            M:= TMemoryStream.Create;
          M.Clear;
          Stream.Read(L, SizeOf(LongInt));
          if L = 0 then
            TBlobField(F).Clear
          else if L <> -1 then
          begin
            M.CopyFrom(Stream, L);
            M.Position:= 0;
            if F.FieldKind = fkData then
              TBlobField(F).LoadFromStream(M);
          end else
          begin
            Result:= False;
            Exit;
          end;
        end else
        begin
          if F.DataType in [ftUnknown, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
            ftParadoxOle, ftDBaseOle, ftTypedBinary] then
              ErrorFieldType(msgCantProcessFieldType, F.DataType);
          {}
          Stream.Read(HasData, SizeOf(Byte));
          if HasData = 1 then
          begin
            case F.DataType of
              ftString, ftBytes, ftVarBytes:
                if F.FieldKind = fkData then
                  F.AsString:= ReadString;
              ftInteger, ftAutoInc:
              begin
                Stream.Read(L, SizeOf(LongInt));
                if F.FieldKind = fkData then
                  F.AsInteger:= L;
              end;
              ftLargeInt:
              begin
                Stream.Read(LI, SizeOf(Int64));
                if F.FieldKind = fkData then
                  F.AsVariant:= LI;
              end;
              ftSmallint, ftWord:
              begin
                L:= 0;
                Stream.Read(L, SizeOf(SmallInt));
                if F.FieldKind = fkData then
                  F.AsInteger:= L;
              end;
              ftBoolean:
              begin
                Stream.Read(L, SizeOf(Boolean));
                if F.FieldKind = fkData then
                  F.AsBoolean:= Boolean(L);
              end;
              ftFloat, ftDate, ftTime, ftDateTime:
              begin
                Stream.Read(D, SizeOf(Double));
                if F.FieldKind = fkData then
                  F.AsFloat:= D;
              end;
              ftCurrency, ftBCD:
              begin
                Stream.Read(C, SizeOf(Currency));
                if F.FieldKind = fkData then
                  F.AsCurrency:= C;
              end;
            end;
          end else if HasData = $FF then
          begin
            Stream.Read(L, SizeOf(Integer) - SizeOf(Byte));
            Result:= False;
            Exit;
          end;
        end;
      end;
    end;
  finally
    M.Free;
  end;
  Result:= True;
end;

{ TCSVFormat }

procedure TCSVFormat.SaveRecordToStream(Stream: TStream);
var i: Integer;
    F: TField;
    S, Temp: string;
    procedure RemoveControlChars(var S: string);
    var i: Integer;
    begin // Replace all control chars by spaces
      i:= 1;
      while i <= Length(S) do
      begin
        if S[i] = #13 then
        begin
          Inc(i);
          if (i <= Length(S)) and (S[i] = #10) then
            Delete(S, i, 1);
          S[i - 1]:= #32;
          Continue; // Skip the next Inc(i)
        end else if S[i] < #32 then
          S[i]:= #32;
        Inc(i);
      end;
    end;
begin
  S:= '';
  for i:= 0 to DataSet.FieldCount - 1 do
  begin
    F:= DataSet.Fields[i];
    if (ExportCalcFields or (F.FieldKind = fkData)) and
      ((not F.IsBlob) or (F.DataType = ftMemo)) then
    begin
      if S <> '' then
        S:= S + ';';
      Temp:= F.AsString;
      if F.DataType = ftMemo then
        RemoveControlChars(Temp);
      S:= S + AnsiQuotedStr(Trim(Temp), '"');
    end;
  end;
  S:= S + #13#10;
  Stream.Write(S[1], Length(S));
end;

procedure CSVReadValue(const Rec: string; List: TStrings);
var i, Index: Integer;
    procedure AddString(const S: string);
    begin
      if List.Count <= Index then
        List.Add('');
      List[Index]:= S;
      Inc(Index);
    end;
    function ReadQuotedString: string;
    begin
      Result:= '';
      Inc(i);
      while i <= Length(Rec) do
      begin
        if Rec[i] = '"' then
        begin
          Inc(i);
          if (i > Length(Rec)) or (Rec[i] <> '"') then
            Break;
        end;
        Result:= Result + Rec[i];
        Inc(i);
      end;
    end;
    function ReadNonQuotedString: string;
    var Start: Integer;
    begin { Note : c'est la boucle principale qui "sautera" le ; final }
      Start:= i;
      Inc(i);
      while (i <= Length(Rec)) and (Rec[i] <> ';') do
        Inc(i);
      Result:= Copy(Rec, Start, i - Start);
    end;
begin
  i:= 1;
  Index:= 0;
  while i <= Length(Rec) do
  begin
    if Rec[i] = ';' then
      AddString('')
    else if Rec[i] = '"' then
      AddString(ReadQuotedString)
    else
      AddString(ReadNonQuotedString);
    Inc(i);
  end;
  i:= Length(Rec);
  if (i > 0) and (Rec[i] = ';') then
    AddString('');
end;

procedure CSVReadValueFromStream(Stream: TStream; List: TStrings);
var i, Index: Integer;
    S: TUsefulStream;
    C, TC: Char;
    procedure AddString(const S: string);
    begin
      if List.Count <= Index then
        List.Add('');
      List[Index]:= S;
      Inc(Index);
    end;
    function ReadQuotedString: string;
    begin
      Result:= '';
      while True do
      begin
        C:= S.ReadChar;
        if S.EOF then
          Break;
        if C = '"' then
        begin
          TC:= S.ReadChar;
          if S.EOF then
            Break;
          if TC <> '"' then
          begin
            C:= TC;
            if C <> ';' then
              S.Seek(-1, soFromCurrent);
            Break;
          end;
        end;
        Result:= Result + C;
      end;
    end;
    function ReadNonQuotedString: string;
    begin { Note : c'est la boucle principale qui "sautera" le ; final }
      Result:= C;
      while True do
      begin
        C:= S.ReadChar;
        if S.EOF or (C = ';') then
          Break;
        if C = #13 then
        begin
          S.Seek(-1, soFromCurrent);
          Break;
        end;
        Result:= Result + C;
      end;
    end;
begin
  S:= TUsefulStream(Stream);
  i:= 0;
  Index:= 0;
  while True do
  begin
    TC:= S.ReadChar;
    if S.EOF then
      Break;
    if TC = #13 then
    begin
      TC:= S.ReadChar;
      if TC <> #10 then
        S.Seek(-1, soFromCurrent);
      Break;
    end;
    C:= TC;
    if C = ';' then
      AddString('')
    else if C = '"' then
      AddString(ReadQuotedString)
    else
      AddString(ReadNonQuotedString);
    Inc(i);
  end;
  if (i > 0) and (C = ';') then
    AddString('');
end;

function TCSVFormat.LoadRecordFromStream(Stream: TStream): Boolean;
var i, j: Integer;
    F: TField;
    L: TStringList;
begin
  L:= TStringList.Create;
  try
    with TUsefulStream(Stream) do
    begin
      CSVReadValueFromStream(Stream, L);
      j:= 0;
      for i:= 0 to DataSet.FieldCount - 1 do
      begin
        F:= DataSet.Fields[i];
        if ExportCalcFields or (F.FieldKind = fkData) then
        begin
          if F.FieldKind = fkData then
            if j < L.Count then
              F.AsString:= L[j]
            else
              F.AsString:= '';
          Inc(j);
        end;
      end;
    end;
    Result:= True;
  finally
    L.Free;
  end;
end;

procedure TCSVFormat.WriteHeader(Stream: TStream);
var
  F: TField;
  S, Temp: String;
  i: Integer;
begin
  S:= '';
  for i:= 0 to DataSet.FieldCount - 1 do
  begin
    F:= DataSet.Fields[i];
    if (ExportCalcFields or (F.FieldKind = fkData)) and
      ((not F.IsBlob) or (F.DataType = ftMemo)) then
    begin
      if S <> '' then
        S:= S + ';';
      Temp:= F.FieldName;
      S:= S + StringReplace(Temp,' ','_',[rfReplaceAll]);
    end;
  end;
  S:= S + #13#10;
  Stream.Write(S[1], Length(S));
end;

function TCSVFormat.ReadHeader(Stream: TStream): Boolean;
var
  i, j: Integer;
  F: TField;
  L: TStringList;
begin
  Result := true;

  L:= TStringList.Create;
  try
    with TUsefulStream(Stream) do
    begin
      CSVReadValueFromStream(Stream, L);
      j:= 0;
      for i:= 0 to DataSet.FieldCount - 1 do
      begin
        F:= DataSet.Fields[i];

        if ExportCalcFields or (F.FieldKind = fkData) then
        begin
          if F.FieldKind = fkData then
            if j < L.Count then
            begin
              if F.FieldName <> L[j] then
              begin
                Result := false;
                Break;
              end;
            end;
          Inc(j);
        end;
      end;
    end;
  finally
    L.Free;
  end;
end;

{ TTextConstFormat }

procedure TTextConstFormat.SaveRecordToStream(Stream: TStream);
var i, j, L: Integer;
    F: TField;
    S, Temp: string;
begin
  S:= '';
  Temp:= '';
  L:= 0;
  for i:= 0 to DataSet.FieldCount - 1 do
  begin
    F:= DataSet.Fields[i];
    if ExportCalcFields or (F.FieldKind = fkData) then
    begin
      case F.DataType of
        ftString: L:= F.Size;
        ftInteger, ftAutoInc: L:= 10; // Integer = Signed 32 bits => MaxVal = +/-2.10^9
        ftSmallint: L:= 6; // SmallInt = signed 16 bits   => MaxVal = +/- 32767
        ftWord: L:= 5;     // Word     = unsigned 16 bits => MaxVal = +   65535
        ftBoolean: L:= 1;  // Bool     = 0 or 1
        ftFloat, ftCurrency, ftBCD: L:= 21; // Arbitrary
        ftDate: L:= 10;    // Standard dd/mm/yyyy
        ftTime: L:= 8;     // Standard hh:nn:ss
        ftDateTime: L:= 19; // Standard dd/mm/yyyy hh:nn:ss
        ftLargeInt: L:= 20 // Int64...
        else
          Continue;
      end;
      case F.DataType of
        ftBoolean:  Temp:= IntToStr(Abs(Integer(F.AsBoolean)));
        ftDate:     Temp:= FormatDateTime('dd/mm/yyyy', F.AsDateTime);
        ftTime:     Temp:= FormatDateTime('hh:nn:ss', F.AsDateTime);
        ftDateTime: Temp:= FormatDateTime('dd/mm/yyyy hh:nn:ss', F.AsDateTime);
        else
          Temp:= F.AsString;
      end;
      j:= Length(Temp);
      if j < L then
      begin // Fill with empty spaces
        SetLength(Temp, L);
        FillChar(Temp[j + 1], L - j, $20);
      end;
      S:= S + Temp;
    end;
  end;
  S:= S + #13#10;
  Stream.Write(S[1], Length(S));
end;

function TTextConstFormat.LoadRecordFromStream(Stream: TStream): Boolean;
  function GetDataSize(Field: TField): Integer;
  begin
    case Field.DataType of
      ftString: Result:= Field.Size;
      ftInteger, ftAutoInc: Result:= 10; // Integer = Signed 32 bits => MaxVal = +/-2.10^9
      ftSmallint: Result:= 6; // SmallInt = signed 16 bits   => MaxVal = +/- 32767
      ftWord: Result:= 5;     // Word     = unsigned 16 bits => MaxVal = +   65535
      ftBoolean: Result:= 1;  // Bool     = 0 or 1
      ftFloat, ftCurrency, ftBCD: Result:= 21; // Arbitrary
      ftDate: Result:= 10;    // Standard dd/mm/yyyy
      ftTime: Result:= 8;     // Standard hh:nn:ss
      ftDateTime: Result:= 19; // Standard dd/mm/yyyy hh:nn:ss
      ftLargeInt: Result:= 20 // Int64... +/-XXX
      else
        Result:= 0;
    end;
  end;
var i, L, P: Integer;
    F: TField;
    S, V: string;
begin
  S:= '';
  L:= 0;
  for i:= 0 to DataSet.FieldCount - 1 do
  begin
    F:= DataSet.Fields[i];
    if ExportCalcFields or (F.FieldKind = fkData) then
      L:= L + GetDataSize(F);
  end;
  Inc(L, Length(#13#10));
  SetLength(S, L);
  Stream.Read(S[1], L);
  P:= 1;
  for i:= 0 to DataSet.FieldCount - 1 do
  begin
    F:= DataSet.Fields[i];
    if ExportCalcFields or (F.FieldKind = fkData) then
    begin
      L:= GetDataSize(F);
      if F.FieldKind = fkData then
      begin
        V:= Trim(Copy(S, P, L));
        case F.DataType of
          ftBoolean: F.AsInteger:= StrToInt(V);
          else
            F.AsString:= V;
        end;
      end;
      Inc(P, L);
    end;
  end;
  Result:= True;
end;

{ --------------------------------------------------------------------------- }

procedure ReadCSVFromFile(FileName: string; DataSet: TDataSet);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmOpenRead);
  try
    ReadCSVFromStream(F, DataSet);
  finally
    F.Free;
  end;
end;

procedure ReadCSVFromStream(Stream: TStream; DataSet: TDataSet);
var S: string;
begin
  SetLength(S, Stream.Size - Stream.Position);
  Stream.Read(S[1], Length(S));
  ReadCSVFromString(S, DataSet);
end;

procedure ReadCSVFromString(S: string; DataSet: TDataSet);
var FieldIndex: Integer;
    Modified: Boolean;
    procedure NewRecord;
    begin
      if (DataSet.State = dsInsert) and Modified then
        DataSet.Post;
      if DataSet.State = dsBrowse then
        DataSet.Append;
      Modified:= False;
    end;
    procedure AddEmptyString(var P: PChar);
    begin
      DataSet.Fields[FieldIndex].AsString:= '';
      Inc(FieldIndex);
      Inc(P);
      Modified:= True;
    end;
    procedure AddQuotedString(var P: PChar);
    var PD, PF, PW: PChar;
        S: string;
    begin
      Inc(P);
      PD:= P;
      while P^ <> #0 do
      begin
        if P^ = '"' then
        begin
          Inc(P);
          if P^ <> '"' then
            Break
          else
            Inc(P);
        end else
          Inc(P);
      end;
      PF:= P - 1;
      SetLength(S, Integer(PF) - Integer(PD));
      PW:= PChar(S);
      if PF <> PD then
      begin
        while PD <> PF do
        begin
          if PD^ = '"' then
            Inc(PD);
          PW^:= PD^;
          Inc(PD);
          Inc(PW);
        end;
        PW^:= #0;
      end;
      DataSet.Fields[FieldIndex].AsString:= PChar(S);
      Inc(FieldIndex);
      if P^ = ';' then
        Inc(P);
      Modified:= True;
    end;
    procedure AddNonQuotedString(var P: PChar);
    var PD, PF, PW: PChar;
        S: string;
    begin
      PD:= P;
      while not (P^ in [#0, ';', #13, #10]) do
        Inc(P);
      PF:= P;
      SetLength(S, Integer(PF) - Integer(PD));
      PW:= PChar(S);
      Move(PD^, PW^, Integer(PF) - Integer(PD));
      DataSet.Fields[FieldIndex].AsString:= PChar(S);
      Inc(FieldIndex);
      if P^ = ';' then
        Inc(P);
      Modified:= True;
    end;
var P: PChar;
begin
  P:= PChar(S);
  FieldIndex:= 0;
  NewRecord;
  while P^ <> #0 do
  begin
    if P^ in [#13, #10] then
    begin
      NewRecord;
      FieldIndex:= 0;
      Inc(P);
    end else if P^ = ';' then
      AddEmptyString(P)
    else if P^ = '"' then
      AddQuotedString(P)
    else
      AddNonQuotedString(P);
  end;
  NewRecord;
  DataSet.Cancel;
end;

end.
