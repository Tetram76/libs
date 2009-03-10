unit DBTransfer;

{$I GrizzlyDefine.inc}

interface

uses SysUtils, Classes, DB, Dialogs;

type
  TTransferMode = (tmCopy, tmAppend, tmUpdate, tmAppendUpdate, tmDelete);
  TAfterEachTransferEvent = procedure (Sender : TObject; Posted : Boolean) of object;
  TTransferEvent = procedure(Sender: TObject; var DoTransfer: Boolean) of object;
  TUpdateMode = (umLeftJoin, umRightJoin);

  EDBTransferError = class(Exception);

  TDBTransfer = class(TComponent)
  private
    FAbortOnError: Boolean;
    FSource, FDest: TDataSet;
    FTransferMode: TTransferMode;

    FTransferFields, FLinkFields: TStrings;
    FCachedSourceFields: TStringList;
    FCachedDestFields: TList;

    FBeforeTrans, FOnTransfer: TTransferEvent;
    FAfterTrans: TNotifyEvent;
    FDoTrans: Boolean;
    FErrorFile: string;
    FLogErrors: Boolean;
    FUpdateMode: TUpdateMode;
    FAfterEachTrans: TAfterEachTransferEvent;
    procedure SetSource(Source: TDataSet);
    procedure SetDest(Dest: TDataSet);
    procedure SetDoTrans(DoTrans: Boolean);
    {}
    procedure Copy(SourceFields: TStrings; DestFields: TList; ErrorLog: TStream);
    procedure Update(SourceFields: TStrings; DestFields: TList; ErrorLog: TStream);
    procedure Delete;
    {}
    procedure SetLinkFields(const Value: TStrings);
    procedure SetTransferFields(const Value: TStrings);
    procedure ExtractItems(const S: string; var Name, Value: string);
  protected
    procedure PostRecord(ErrorLog: TStream);
    procedure DeleteRecord;
    procedure LogError(E: Exception; ErrorLog: TStream);
    procedure AssignLocateValues(var Values: Variant; LocateDestFields: TStrings);
    function BuildLocateFields(LocateDestFields: TStrings): string;
    procedure AssignFieldValues(SourceFields: TStrings; DestFields: TList);
    procedure ExtractFieldRef(DataSet: TDataSet; FieldRef: string; List: TStrings);
    procedure GetUpdateDataSets(var LeftDataSet, RightDataSet: TDataSet);
    {}
    procedure InternalTransfer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Transfer;
    procedure AssignCurrentRecord;
    procedure AutoLinkTransferFields;
    procedure ResetCachedFields;
    procedure AssociateFields(Source, Destination: string);
    {}
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode default umRightJoin;
  published
    property Source: TDataSet read FSource write SetSource;
    property Destination: TDataSet read FDest write SetDest;
    property TransferMode: TTransferMode read FTransferMode write FTransferMode default tmCopy;
    property DoTransfer: Boolean read FDoTrans write SetDoTrans;
    {}
    property TransferFields: TStrings read FTransferFields write SetTransferFields;
    property LinkFields: TStrings read FLinkFields write SetLinkFields;
    {}
    property BeforeTransfer: TTransferEvent read FBeforeTrans write FBeforeTrans;
    property OnTransferRecord: TTransferEvent read FOnTransfer write FOnTransfer;
    property AfterTransfer: TNotifyEvent read FAfterTrans write FAfterTrans;
    property AfterEachTransfer: TAfterEachTransferEvent read FAfterEachTrans write FAfterEachTrans;
    {}
    property AbortOnError: Boolean read FAbortOnError write FAbortOnError;
    property FileErrorLog: string read FErrorFile write FErrorFile;
    property LogErrors: Boolean read FLogErrors write FLogErrors;
  end;


const
  msgSourceDataSetMissing = 'Propriété Source manquante !';
  msgDestDataSetMissing = 'Propriété Destination manquante !';
  msgSourceFieldValues = 'Valeur des champs source = ';
  msgDestFieldValues = 'Valeur des champs destination = ';
  msgCantLinkBlobFields = 'Impossible de lier des champs blobs !';
  msgLinkFieldsMissing = 'Champs de lien manquants';
  msgSourceDestEqual = 'La source et la destination sont les mêmes !';

implementation

uses Controls, GzConsts, AUtils {$IFDEF GZ_D6}, Variants{$ENDIF};

{ ********************************* TDBTransfer ******************************** }

procedure TDBTransfer.ExtractItems(const S: string; var Name, Value: string);
var P: Integer;
begin
  P:= AnsiPos('=', S);
  Name:= System.Copy(S, 1, P - 1);
  Value:= System.Copy(S, P + 1, Length(S));
end;

procedure TDBTransfer.AutoLinkTransferFields;
var i: Integer;
    FS, FD: TField;
begin
  Source.Open;
  Destination.Open;
  TransferFields.Clear;
  for i:= 0 to Destination.FieldCount - 1 do
  begin
    FD:= Destination.Fields[i];
    FS:= Source.FindField(FD.FieldName);
    if Assigned(FS) then
      TransferFields.Add(Format('%s=%s', [FS.FieldName, FD.FieldName]));
  end;
end;

procedure TDBTransfer.ResetCachedFields;
begin
  FCachedSourceFields.Clear;
  FCachedDestFields.Clear;
end;

procedure TDBTransfer.AssociateFields(Source, Destination: string);
var
  X: Integer;
begin
  // Ajout de cette méthode pour associer des champs dynamiquement
  for X := 0 to TransferFields.Count - 1 do
    // Si la destination existe, on la supprime
    if Pos('=' + Destination, TransferFields[X]) > 0 then
      TransferFields.Delete(X);
  // Ajout de la nouvelle association
  TransferFields.Add(Source + '=' + Destination);
  // Mise à jour interne des liaisons
  ResetCachedFields;
end;

procedure TDBTransfer.AssignCurrentRecord;
var i: Integer;
    Source, Dest: string;
begin
  if FCachedSourceFields.Count = 0 then
    for i:= 0 to TransferFields.Count - 1 do
    begin
      ExtractItems(TransferFields[i], Source, Dest);
      ExtractFieldRef(FSource, Source, FCachedSourceFields);
      FCachedDestFields.Add(Destination.FieldByName(Dest));
    end;
  AssignFieldValues(FCachedSourceFields, FCachedDestFields);
end;

constructor TDBTransfer.Create(AOwner: TComponent);
begin
  FTransferFields:= TStringList.Create;
  FLinkFields:= TStringList.Create;
  {}
  FCachedSourceFields:= TStringList.Create;
  FCachedDestFields:= TList.Create;
  {}
  inherited Create(AOwner);
end;

destructor TDBTransfer.Destroy;
begin
  FLinkFields.Free; FLinkFields:= nil;
  FTransferFields.Free; FTransferFields:= nil;
  FCachedSourceFields.Free; FCachedSourceFields:= nil;
  FCachedDestFields.Free; FCachedDestFields:= nil;
  {}
  inherited Destroy;
end;

procedure TDBTransfer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> nil) then
  begin
    if AComponent = Source then Source:= nil;
    if AComponent = Destination then Destination:= nil;
  end;
end;

procedure TDBTransfer.Transfer;
begin
  if not Assigned(Source) then raise EDBTransferError.Create(msgSourceDataSetMissing);
  if not Assigned(Destination) then raise EDBTransferError.Create(msgDestDataSetMissing);
  if Source = Destination then
    raise EDBTransferError.Create(msgSourceDestEqual);
  Source.DisableControls;
  try
    Destination.DisableControls;
    try
      try
        InternalTransfer;
      finally
        if ((not Source.Active) and (Source.FieldCount = 0)) or
           ((not Destination.Active) and (Destination.FieldCount = 0)) then
          ResetCachedFields;
      end;
    finally
      Destination.EnableControls;
    end;
  finally
    Source.EnableControls;
  end;
end;

procedure TDBTransfer.InternalTransfer;
var i: Integer;
    OldSActive, OldDActive: Boolean;
    SourceBookmark, DestBookmark: TBookmarkStr;
    DoTransfer: Boolean;
    {}
    ErrorLog: TFileStream;
    ErrorOpenMode: Integer;
    SourceField, DestField: string;
begin
  DoTransfer:= True;
  if Assigned(FBeforeTrans) then
    FBeforeTrans(Self, DoTransfer);
  if DoTransfer then
  begin
    OldSActive:= Source.Active;
    OldDActive:= Destination.Active;
    { Open Source }
    Source.Active:= True;
    try
      { Open Destination }
      Destination.Active:= True;
      try
        { Store Bookmarks }
        SourceBookmark:= Source.Bookmark;
        DestBookmark:= Destination.Bookmark;
        { Extract Fields }
        if FCachedSourceFields.Count = 0 then
          for i:= 0 to TransferFields.Count - 1 do
          begin
            ExtractItems(TransferFields[i], SourceField, DestField);
            ExtractFieldRef(FSource, SourceField, FCachedSourceFields);
            FCachedDestFields.Add(Destination.FieldByName(DestField));
          end;
        { Empty Dest table if in Copy Mode }
        if TransferMode = tmCopy then
          while not Destination.IsEmpty do Destination.Delete;
        { Prepare ErrorLog }
        ErrorLog:= nil;
        if LogErrors then
        begin
          if FileExists(FileErrorLog) then
            ErrorOpenMode:= fmOpenReadWrite or fmShareDenyNone
          else
            ErrorOpenMode:= fmCreate or fmShareDenyNone;
          ErrorLog:= TFileStream.Create(FileErrorLog, ErrorOpenMode);
          ErrorLog.Seek(0, 2);
        end;
        try
          { Do the transfer }
          case TransferMode of
            tmAppend, tmCopy: Copy(FCachedSourceFields, FCachedDestFields, ErrorLog);
            tmUpdate, tmAppendUpdate: Update(FCachedSourceFields, FCachedDestFields, ErrorLog);
            tmDelete: Delete;
          end;
        finally
          ErrorLog.Free;
        end;
      finally
        if OldDActive and Assigned(PChar(DestBookmark)) then //and Destination.BookmarkValid(PChar(DestBookmark)) then
          Destination.Bookmark:= DestBookmark
        else
          Destination.Active:= OldDActive;
      end;
    finally
      if OldSActive and Source.BookmarkValid(PChar(SourceBookmark)) then
        Source.Bookmark:= SourceBookmark
      else
        Source.Active:= OldSActive;
    end;
  end;
  if Assigned(FAfterTrans) then
    FAfterTrans(Self);
end;

procedure TDBTransfer.ExtractFieldRef(DataSet: TDataSet; FieldRef: string; List: TStrings);
var Field: TField;
begin
  Field:= DataSet.FindField(FieldRef);
  if Field = nil then
  begin
    if (FieldRef[1] = '"') and (FieldRef[Length(FieldRef)] = '"') then
      FieldRef:= System.Copy(FieldRef, 2, Length(FieldRef) - 2)
    else
      Field:= DataSet.FieldByName(FieldRef); { Force exception }
  end else
    FieldRef:= '';
  List.AddObject(FieldRef, Field);
end;

procedure TDBTransfer.PostRecord(ErrorLog: TStream);
var
  DoTransfer: Boolean;
  Posted : Boolean;
begin
  Posted := False;
  DoTransfer:= True;
  if Assigned(FOnTransfer) then
    FOnTransfer(Self, DoTransfer);
  if DoTransfer then
  try
    Destination.Post;
    Posted := True;
  except
    on E: Exception do
    begin
      LogError(E, ErrorLog);
      Destination.Cancel;
      if AbortOnError then
        raise;
    end;
  end else
    Destination.Cancel;
  if Assigned(FAfterEachTrans) then
    FAfterEachTrans(Self, Posted);
end;

procedure TDBTransfer.DeleteRecord;
var DoTransfer: Boolean;
begin
  DoTransfer:= True;
  if Assigned(FOnTransfer) then
    FOnTransfer(Self, DoTransfer);
  if DoTransfer then
    Destination.Delete;
end;

procedure TDBTransfer.LogError(E: Exception; ErrorLog: TStream);
var i: Integer;
    SError: string;
begin
  if Assigned(ErrorLog) then
  begin
    { ComponentName + ErrorMessage }
    SError:= Self.Name + #9 + E.Message + #13#10;
    { Field Values at the time of Error (Source) }
    SError:= SError + msgSourceFieldValues;
    for i:= 0 to Source.FieldCount - 1 do
      SError:= SError + #9 + Source.Fields[i].FieldName + ' : ' + Source.Fields[i].AsString;
    SError:= SError + #13#10;
    { Field Values at the time of Error (Destination) }
    SError:= SError + msgDestFieldValues;
    for i:= 0 to Destination.FieldCount - 1 do
      SError:= SError + #9 + Destination.Fields[i].FieldName + ' : ' + Destination.Fields[i].AsString;
    SError:= SError + #13#10;
    ErrorLog.Write(PChar(SError)^, Length(SError));
  end;
end;

procedure TDBTransfer.Copy(SourceFields: TStrings; DestFields: TList; ErrorLog: TStream);
begin
  Source.First;
  Destination.First;
  while not Source.EOF do
  begin
    Destination.Append;
    AssignFieldValues(SourceFields, DestFields);
    PostRecord(ErrorLog);
    Source.Next;
  end;
end;

function TDBTransfer.BuildLocateFields(LocateDestFields: TStrings): string;
var i: Integer;
    SourceField, DestField: string;
    SF, DF: TField;
begin
  Result:= '';
  for i:= 0 to FLinkFields.Count - 1 do
  begin
    if i <> 0 then
      Result:= Result + ';';
    ExtractItems(FLinkFields[i], SourceField, DestField);
    SF:= Source.FindField(SourceField);
    DF:= Destination.FieldByName(DestField);
    if (Assigned(SF) and SF.IsBlob) or DF.IsBlob then
      raise EDBTransferError.Create(msgCantLinkBlobFields);
    if (UpdateMode = umRightJoin) and (TransferMode <> tmAppendUpdate) then
    begin { Force Exception if necessary with FieldByName }
      Result:= Result + Source.FieldByName(SourceField).FieldName;
      ExtractFieldRef(Destination, DestField, LocateDestFields);
    end else
    begin
      Result:= Result + Destination.FieldByName(DestField).FieldName;
      ExtractFieldRef(Source, SourceField, LocateDestFields);
    end;
  end;
  if LocateDestFields.Count = 0 then
    raise EDBTransferError.Create(msgLinkFieldsMissing);
end;

procedure TDBTransfer.AssignLocateValues(var Values: Variant; LocateDestFields: TStrings);
var i: Integer;
begin
  if LocateDestFields.Count > 1 then
  begin
    for i:= 0 to LocateDestFields.Count - 1 do
      if LocateDestFields.Objects[i] = nil then
        Values[i]:= LocateDestFields[i]
      else
        Values[i]:= TField(LocateDestFields.Objects[i]).AsVariant;
  end else
    Values:= TField(LocateDestFields.Objects[0]).AsVariant;
end;

procedure TDBTransfer.AssignFieldValues(SourceFields: TStrings; DestFields: TList);
var i: Integer;
begin
  for i:= 0 to DestFields.Count - 1 do
  begin { Assign FieldValue or ConstValue }
    if SourceFields.Objects[i] = nil then
      TField(DestFields[i]).AsString:= SourceFields[i]
    else
      TField(DestFields[i]).Assign(TField(SourceFields.Objects[i]));
  end;
end;

procedure TDBTransfer.GetUpdateDataSets(var LeftDataSet, RightDataSet: TDataSet);
var Temp: TDataSet;
begin
  LeftDataSet:= Source;
  RightDataSet:= Destination;
  if (UpdateMode = umRightJoin) and (TransferMode <> tmAppendUpdate) then
  begin // GenericSwap(LeftDataSet, RightDataSet, SizeOf(TDataSet));
    Temp:= LeftDataSet;
    LeftDataSet:= RightDataSet;
    RightDataSet:= Temp;
  end;
end;

procedure TDBTransfer.Update(SourceFields: TStrings; DestFields: TList; ErrorLog: TStream);
var LocateFields: string;
    LocateDestFields: TStrings;
    V: Variant;
    DataSet1, DataSet2: TDataSet;
    Append: Boolean;
begin
  LocateDestFields:= TStringList.Create;
  try
    GetUpdateDataSets(DataSet1, DataSet2);

    Append:= TransferMode = tmAppendUpdate;

    LocateFields:= BuildLocateFields(LocateDestFields);
    if LocateDestFields.Count > 1 then
      V:= VarArrayCreate([0, LocateDestFields.Count - 1], varVariant);
    DataSet1.First;
    while not DataSet1.EOF do
    begin
      AssignLocateValues(V, LocateDestFields);
      if DataSet2.Locate(LocateFields, V, []) then
      begin
        Destination.Edit;
        AssignFieldValues(SourceFields, DestFields);
        PostRecord(ErrorLog);
      end else if Append then
      begin
        Destination.Append;
        AssignFieldValues(SourceFields, DestFields);
        PostRecord(ErrorLog);
      end;
      DataSet1.Next;
    end;
  finally
    LocateDestFields.Free;
  end;
end;

procedure TDBTransfer.Delete;
var LocateFields: string;
    LocateDestFields: TStrings;
    V: Variant;
    DataSet1, DataSet2: TDataSet;
begin
  LocateDestFields:= TStringList.Create;
  try
    GetUpdateDataSets(DataSet1, DataSet2);

    LocateFields:= BuildLocateFields(LocateDestFields);
    V:= VarArrayCreate([0, LocateDestFields.Count - 1], varVariant);
    DataSet1.First;
    while not DataSet1.EOF do
    begin
      AssignLocateValues(V, LocateDestFields);
      if DataSet2.Locate(LocateFields, V, []) then
      begin
        repeat
          DeleteRecord;
        until not DataSet2.Locate(LocateFields, V, []);
        if DataSet1 = Source then
          DataSet1.Next;
      end else
        DataSet1.Next;
    end;
  finally
    LocateDestFields.Free;
  end;
end;

{ Get/Set procs }

procedure TDBTransfer.SetTransferFields(const Value: TStrings);
begin
  ResetCachedFields;
  {}
  FTransferFields.Assign(Value);
end;

procedure TDBTransfer.SetLinkFields(const Value: TStrings);
begin
  FLinkFields.Assign(Value);
end;

procedure TDBTransfer.SetDoTrans(DoTrans: Boolean);
begin
  if DoTrans then
    Transfer;
end;

procedure TDBTransfer.SetSource(Source: TDataSet);
begin
  if FSource <> Source then
  begin
    FSource:= Source;
    if Source <> nil then
      Source.FreeNotification(Self);
  end;
  {}
  ResetCachedFields;
end;

procedure TDBTransfer.SetDest(Dest: TDataSet);
begin
  if FDest <> Dest then
  begin
    FDest:= Dest;
    if Dest <> nil then
      Dest.FreeNotification(Self);
  end;
  {}
  ResetCachedFields;
end;

end.

