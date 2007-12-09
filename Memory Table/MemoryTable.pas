unit MemoryTable;

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }
{$N+,P+,S-}

interface

uses SysUtils, Classes, Controls, Bde, DB, DBTables, Math;

type

{ TMemoryTable }

  TMemoryTable = class(TDBDataSet)
  private
    FTableName: TFileName;
    FMoveHandle: HDBICur;
    FEnableDelete: Boolean;
    FDisableEvents: Boolean;
    procedure EncodeFieldDesc(var FieldDesc: FLDDesc; const Name: string; DataType: TFieldType; Size, Precision: Word);
    procedure SetTableName(const Value: TFileName);
    function SupportedFieldType(AType: TFieldType): Boolean;
    procedure DeleteCurrentRecord;
  protected
    function CreateHandle: HDBICur; override;
    procedure DoBeforeClose; override;
    procedure DoAfterClose; override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
    procedure DoBeforeScroll; override;
    procedure DoAfterScroll; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure InternalDelete; override;
  public
    constructor Create(AOwner: TComponent); override;
    function BatchMove(ASource: TDataSet; AMode: TBatchMode; ARecordCount: Longint): Longint;
    procedure CopyStructure(ASource: TDataSet);
    procedure CreateTable;
    procedure DeleteTable;
    procedure EmptyTable;
    procedure GotoRecord(RecordNo: Longint);
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    procedure SetFieldValues(const FieldNames: array of string; const Values: array of const);
  published
    property EnableDelete: Boolean read FEnableDelete write FEnableDelete default True;
    property TableName: TFileName read FTableName write SetTableName;
  end;

procedure _DBError(const Msg: string);
procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
function FieldLogicMap(FldType: TFieldType): Integer;
function FieldSubtypeMap(FldType: TFieldType): Integer;

procedure Register;

implementation

uses DBConsts, Forms;

{ Memory tables are created in RAM and deleted when you close them. They
  are much faster and are very useful when you need fast operations on
  small tables. Memory tables do not support certain features (like
  deleting records, referntial integrity, indexes, autoincrement fields
  and BLOBs) }

{ TMemoryTable }

const
  SInvalidBatchMove = 'Paramètres de déplacement batch incorrects';

constructor TMemoryTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnableDelete := True;
end;

function TMemoryTable.BatchMove(ASource: TDataSet; AMode: TBatchMode;
  ARecordCount: Longint): Longint;
var
  SourceActive: Boolean;
  MovedCount: Longint;
begin
  if (ASource = nil) or (Self = ASource) or
    not (AMode in [batCopy, batAppend]) then _DBError(SInvalidBatchMove);
  SourceActive := ASource.Active;
  try
    ASource.DisableControls;
    DisableControls;
    ASource.Open;
    ASource.CheckBrowseMode;
    ASource.UpdateCursorPos;
    if AMode = batCopy then begin
      Close;
      CopyStructure(ASource);
    end;
    if not Active then Open;
    CheckBrowseMode;
    if ARecordCount > 0 then begin
      ASource.UpdateCursorPos;
      MovedCount := ARecordCount;
    end
    else begin
      ASource.First;
      MovedCount := MaxLongint;
    end;
    try
      Result := 0;
      while not ASource.EOF do begin
        Append;
        AssignRecord(ASource, Self, True);
        Post;
        Inc(Result);
        if Result >= MovedCount then Break;
        ASource.Next;
      end;
    finally
      Self.First;
    end;
  finally
    if not SourceActive then ASource.Close;
    Self.EnableControls;
    ASource.EnableControls;
  end;
end;

procedure TMemoryTable.CopyStructure(ASource: TDataSet);
var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do Fields[I].Free;
  if (ASource = nil) then Exit;
  ASource.FieldDefs.Update;
  FieldDefs := ASource.FieldDefs;
  for I := 0 to FieldDefs.Count - 1 do begin
    if SupportedFieldType(FieldDefs.Items[I].DataType) then begin
      if (csDesigning in ComponentState) and (Owner <> nil) then
        FieldDefs.Items[I].CreateField(Owner)
      else
        FieldDefs.Items[I].CreateField(Self);
    end;
  end;
end;

procedure TMemoryTable.DeleteCurrentRecord;
var
  CurRecNo, CurRec: Longint;
  Buffer: Pointer;
  iFldCount: Word;
  FieldDescs: PFLDDesc;
begin
  CurRecNo := RecNo;
  iFldCount := FieldDefs.Count;
  FieldDescs := AllocMem(iFldCount * SizeOf(FLDDesc));
  try
    Check(DbiGetFieldDescs(Handle, FieldDescs));
    Check(DbiCreateInMemTable(DBHandle, '$InMem$', iFldCount, FieldDescs,
      FMoveHandle));
    try
      DisableControls;
      Buffer := AllocMem(RecordSize);
      try
        First;
        CurRec := 0;
        while not Self.EOF do begin
          Inc(CurRec);
          if CurRec <> CurRecNo then begin
            DbiInitRecord(FMoveHandle, Buffer);
            Self.GetCurrentRecord(Buffer);
            Check(DbiAppendRecord(FMoveHandle, Buffer));
          end;
          Self.Next;
        end;
        FDisableEvents := True;
        try
          Close;
          Open;
          FMoveHandle := nil;
        finally
          FDisableEvents := False;
        end;
      finally
        FreeMem(Buffer, RecordSize);
      end;
    except
      DbiCloseCursor(FMoveHandle);
      FMoveHandle := nil;
      raise;
    end;
    GotoRecord(CurRecNo - 1);
  finally
    if FieldDescs <> nil then
      FreeMem(FieldDescs, iFldCount * SizeOf(FLDDesc));
    FMoveHandle := nil;
    EnableControls;
  end;
end;

function TMemoryTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: PChar;
begin
  Result := inherited GetFieldData(Field, Buffer);
  if not Result then begin
    RecBuf := nil;
    case State of
      dsBrowse: if not IsEmpty then RecBuf := ActiveBuffer;
      dsEdit, dsInsert: RecBuf := ActiveBuffer;
      dsCalcFields: RecBuf := CalcBuffer;
    end;
    if RecBuf = nil then Exit;
    with Field do
      if (FieldNo > 0) then begin
        Check(DbiGetField(Handle, FieldNo, RecBuf, nil, IsBlank));
        Result := not IsBlank;
      end;
  end;
end;

procedure TMemoryTable.InternalDelete;
begin
  if EnableDelete then DeleteCurrentRecord
  else inherited;
end;

function TMemoryTable.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TMemoryTable.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := False;
end;

procedure TMemoryTable.DoAfterClose;
begin
  if not FDisableEvents then inherited DoAfterClose;
end;

procedure TMemoryTable.DoAfterOpen;
begin
  if not FDisableEvents then inherited DoAfterOpen;
end;

procedure TMemoryTable.DoBeforeClose;
begin
  if not FDisableEvents then inherited DoBeforeClose;
end;

procedure TMemoryTable.DoBeforeOpen;
begin
  if not FDisableEvents then inherited DoBeforeOpen;
end;

procedure TMemoryTable.DoBeforeScroll;
begin
  if not FDisableEvents then inherited DoBeforeScroll;
end;

procedure TMemoryTable.DoAfterScroll;
begin
  if not FDisableEvents then inherited DoAfterScroll;
end;

function TMemoryTable.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result := not (AType in [ftUnknown, ftWideString] +  ftNonTextTypes);
end;

function TMemoryTable.CreateHandle: HDBICur;
var
  I: Integer;
  FldDescList: TFieldDescList;
  FieldDescs: PFLDDesc;
  iFldCount: Cardinal;
  szTblName: DBITBLNAME;
begin
  if (FMoveHandle <> nil) then begin
    Result := FMoveHandle;
    Exit;
  end;
  if FieldCount > 0 then FieldDefs.Clear;
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do begin
      if not SupportedFieldType(Fields[I].DataType) then
        DatabaseErrorFmt(SUnknownFieldType, [Fields[I].FieldName]);
      with Fields[I] do
        if not (Calculated {$IFDEF WIN32} or Lookup {$ENDIF}) then
          FieldDefs.Add(FieldName, DataType, Size, Required);
    end;
  iFldCount := FieldDefs.Count;
  SetDBFlag(dbfTable, True);
  try
    if TableName = '' then
      AnsiToNative(Locale, '$RxInMem$', szTblName, SizeOf(szTblName) - 1)
    else
      AnsiToNative(Locale, TableName, szTblName, SizeOf(szTblName) - 1);
    SetLength(FldDescList, iFldCount);
    FieldDescs := BDE.PFLDDesc(FldDescList);
    for I := 0 to FieldDefs.Count - 1 do begin
      with FieldDefs[I] do
        EncodeFieldDesc(FldDescList[I], Name, DataType, Size, Precision);
    end;
    Check(DbiTranslateRecordStructure(nil, iFldCount, FieldDescs, nil, nil,
      FieldDescs, False));
    Check(DbiCreateInMemTable(DBHandle, szTblName, iFldCount, FieldDescs,
      Result));
  finally
    SetDBFlag(dbfTable, False);
  end;
end;

procedure TMemoryTable.CreateTable;
begin
  CheckInactive;
  Open;
end;

procedure TMemoryTable.DeleteTable;
begin
  CheckBrowseMode;
  Close;
end;

procedure TMemoryTable.EmptyTable;
begin
  if Active then begin
    CheckBrowseMode;
    DisableControls;
    FDisableEvents := True;
    try
      Close;
      Open;
    finally
      FDisableEvents := False;
      EnableControls;
    end;
  end;
end;

procedure TMemoryTable.EncodeFieldDesc(var FieldDesc: FLDDesc; const Name: string; DataType: TFieldType; Size, Precision: Word);
begin
  with FieldDesc do begin
    FillChar(szName, SizeOf(szName), 0);
    AnsiToNative(Locale, Name, szName, SizeOf(szName) - 1);
    iFldType := FieldLogicMap(DataType);
    iSubType := FieldSubtypeMap(DataType);
    if iSubType = fldstAUTOINC then iSubType := 0;
    case DataType of
      ftString, ftFixedChar, ftBytes, ftVarBytes, ftBlob..ftTypedBinary:
        iUnits1 := Size;
      ftBCD:
        begin
          { Default precision is 32, Size = Scale }
          if (Precision > 0) and (Precision <= 32) then iUnits1 := Precision
          else iUnits1 := 32;
          iUnits2 := Size;  {Scale}
        end;
    end;
  end;
end;

function TMemoryTable.GetRecordCount: Integer;
begin
  if State = dsInactive then _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(Handle, Result));
end;

procedure TMemoryTable.SetRecNo(Value: Integer);
var
  Rslt: DBIResult;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  Rslt := DbiSetToSeqNo(Handle, Value);
  if Rslt = DBIERR_EOF then Last
  else if Rslt = DBIERR_BOF then First
  else begin
    Check(Rslt);
    Resync([rmExact, rmCenter]);
  end;
end;

function TMemoryTable.GetRecNo: Integer;
var
  Rslt: DBIResult;
begin
  Result := -1;
  if State in [dsBrowse, dsEdit] then begin
    UpdateCursorPos;
    Rslt := DbiGetSeqNo(Handle, Result);
    if (Rslt = DBIERR_EOF) or (Rslt = DBIERR_BOF) then Exit
    else Check(Rslt);
  end;
end;

procedure TMemoryTable.GotoRecord(RecordNo: Longint);
begin
  RecNo := RecordNo;
end;

function TMemoryTable.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

procedure TMemoryTable.SetFieldValues(const FieldNames: array of string;
  const Values: array of const);
var
  I: Integer;
  Pos: Longint;
begin
  Pos := RecNo;
  DisableControls;
  try
    First;
    while not EOF do begin
      Edit;
      for I := 0 to Max(High(FieldNames), High(Values)) do
        FieldByName(FieldNames[I]).AssignValue(Values[I]);
      Post;
      Next;
    end;
    GotoRecord(Pos);
  finally
    EnableControls;
  end;
end;

procedure TMemoryTable.SetTableName(const Value: TFileName);
begin
  CheckInactive;
  FTableName := Value;
  DataEvent(dePropertyChange, 0);
end;

// **********************************************************************

procedure _DBError(const Msg: string);
begin
  DatabaseError(Msg);
end;

procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
var
  I: Integer;
  F, FSrc: TField;
begin
  if not (Dest.State in dsEditModes) then _DBError(SNotEditing);
  if ByName then begin
    for I := 0 to Source.FieldCount - 1 do begin
      F := Dest.FindField(Source.Fields[I].FieldName);
      if F <> nil then begin
        F.Value := Source.Fields[I].Value;
      end;
    end;
  end
  else begin
    for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do
    begin
      F := Dest.FindField(Dest.FieldDefs[I].Name);
      FSrc := Source.FindField(Source.FieldDefs[I].Name);
      if (F <> nil) and (FSrc <> nil) then begin
        F.Value := FSrc.Value;
      end;
    end;
  end;
end;

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TList;
  Bookmark: TBookmarkStr;

  function CompareField(Field: TField; Value: Variant): Boolean;
  var
    S: string;
  begin
    if Field.DataType = ftString then begin
      S := Field.AsString;
      if (loPartialKey in Options) then
        Delete(S, Length(Value) + 1, MaxInt);
      if (loCaseInsensitive in Options) then
        Result := AnsiCompareText(S, Value) = 0
      else
        Result := AnsiCompareStr(S, Value) = 0;
    end
    else Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
      Result := CompareField(TField(Fields.First), KeyValues)
    else begin
      Result := True;
      for I := 0 to FieldCount - 1 do
        Result := Result and CompareField(TField(Fields[I]), KeyValues[I]);
    end;
  end;

begin
  Result := False;
  with DataSet do begin
    CheckBrowseMode;
    if BOF and EOF then Exit;
  end;
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.Bookmark;
      try
        with DataSet do begin
          First;
          while not EOF do begin
            Result := CompareRecord;
            if Result then Break;
            Next;
          end;
        end;
      finally
        if not Result {$IFDEF RX_D3} and
          DataSet.BookmarkValid(PChar(Bookmark)) {$ENDIF} then
          DataSet.Bookmark := Bookmark;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

function FieldLogicMap(FldType: TFieldType): Integer;
begin
  Result := FldTypeMap[FldType];
end;

function FieldSubtypeMap(FldType: TFieldType): Integer;
begin
  Result := FldSubtypeMap[FldType];
end;

procedure Register;
begin
  RegisterComponents('Tetram', [TMemoryTable]);
end;

end.
