{***************************************************************
 *
 * Unit Name: VDataSet
 * Purpose  : Component that implements a memory dataset with index and
              filter features
 * Author   : Alexandre GUILLIEN
 * History  :
 *
 *  14/08/2003 : V 1.46 : Removed a memory leak in ApplyRange
 *  09/07/2003 : V 1.45 : Corrected a bug with coBeginningBy and coEndingBy with strings
                          (invalid result if "beginning by" or "ending by" an empty string)
 *  20/05/2003 : V 1.44 : Corrected a bug in cached updates (when dataset had required fields)
 *  04/03/2003 : V 1.43 : Corrected a major bug in TDataList.Delete
 *  25/04/2002 : V 1.42 : Corrected a bug with Empty Blobs
 *  22/06/2001 : V 1.41 : Corrected a bug with ranges.
 *  09/06/2001 : V 1.40 : Added new TDataList class. It uses the TVirtualDataSet
                          as a generic data container.
                          Added Cached updates support.
 *  24/05/2001 : V 1.31 : Strings and Blobs are now stored in the same way,
                          that is, as Delphi long strings.
 *  21/09/2000 : V 1.30 : New functionnalities :
                            - Save datas at design time
                            - ImportDataSet function added
                            - SetRange, ApplyRange and CancelRange functions added
                            - MasterSource & MasterFields properties added
                            - Can filter & index Memo fields
                          New design time editor functions :
                            - Import field definitions
                            - Import data
                            - Edit
                            - Load from file
                            - Save to file
                          Compliant with D5 philosophy => Property editors are all
                          in an independent unit (VDataSetEd.pas).
                          Removed dependency to SharedUtils. Faster sort and
                          indexed search.
                          Fixed a rare bug with OnFilterRecord.
                          Fixed a bug with blob fields.
 *  25/07/2000 : V 1.21 : Bugs corrected : Delete with filters could not work.
                                           Changing of active index could not work.
 *  01/07/2000 : V 1.2  : Complete rebuild of Filters. Unique index available.
 *  24/08/1999 : V 0.9  : first beta release
 *
 *
 *  TO DO List : - Property editor for MasterFields
 *               - D5-Like FieldDefs & IndexDefs published properties, but D3 Compatible
 *                 and the associated property editors
 *
 ****************************************************************}

{$I GrizzlyDefine.INC}

unit VDataSet;

interface

uses
  SysUtils, Classes, DB, EvalIntf, DataSetStreamer
  {$IFDEF GZ_D6}, Variants{$ENDIF};

type
  EKeyViol = class(EDatabaseError);

  TUpdateRecordEvent = procedure(Sender: TObject; UpdateKind: TUpdateKind;
    OldValues, NewValues: TDataSet; ModifiedFields: TList) of object;

  TCompareOption = (coCaseInsensitive, coBeginningBy, coEndingBy, coContaining, coDescending);
  TCompareOptions = set of TCompareOption;
  TTextCompareProc = function(const S1, S2: string): Integer;

  TCachedUpdate = class;

  TIndexDef = record
    IndexFieldNames: string;
    IndexOptions: TIndexOptions;
  end;

  TMaintainedIndex = class
  public
    IndexFieldNames: string;
    IndexFields: TList;
    DescFieldNames: string;
    DescFields: TList;
    Records: TList;
    Options: TIndexOptions;
    {}
    constructor Create;
    destructor Destroy; override;
  end;

  TIndexList = class(TList)
  public
    destructor Destroy; override;
    {}
    procedure FreeItem(Idx: Integer);
    {}
    function IndexOf(IndexFieldNames: string; IndexOptions: TIndexOptions; DescFieldNames: string): Integer;
    function IndexOfCompatibleIndex(IndexFieldNames: string; IndexOptions: TIndexOptions; DescFieldNames: string): Integer;
  end;

  TDataList = class;

  TCustomVirtualDataSet = class(TDataSet)
  private
    { Data handling }
    FRecords: TList;      // Each Item points on a memory single zone
    FFieldPos: TList;     // Position (Offset) of each field in each record
    FFieldIndex: array of Integer;   // Index in FFieldPos of each FieldNo
    FBlobsPos: TList;     // Position (Offset) of each BlobField in each record/buffer
    FBuffBlobInit: TList; // Temporary buffer to speed up Blob handling
    FRecordSize: Integer; // Effective size of a record
    FBufferSize: Integer; // Effective size of a buffer
    FRecInfoPos: Integer; // Position of optional infos in buffers (TRecordInfo)
    FCurRec: Integer;     // Current record number
    { Index Handling }
    FIndexFields: TList;  // List of key fields in the currently active index
    FIndexFieldNames: string;
    FIndexOptions: TIndexOptions;
    FDescFieldNames: string;
    FDescFields: TList;
    FMaintained: Boolean; // Specifies if the current index is maintained or not TIndexOptions
    FMaintainedIndexs: TIndexList; // List of other maintained Indexs
    { Ranges }
    FRangeActive: Boolean;  // Specifies if Range is currently active
    FRangedRecords: TList;  // Current range record list
    FStartValues, FEndValues: Variant; // Current range values
    { Filter Handling }
    FFilterEvaluator: TEvaluator; // Filter evaluator
    FFilteredRecords: TList;      // Current filter record list
    { General Compare Options (Indexs, Filters) }
    FCompareOptions: TCompareOptions; // Compare options used in CompareFields fn
    FTextCompareProc: TTextCompareProc;
    FStrCompareProc: TTextCompareProc;
    { Buffer used for dsSetKey, dsFilter States }
    FKeyRec: PChar;
    { CachedUpdates }
    FCachedUpdates: Boolean;    // Specifies if CachedUpdates are activated
    FUpdateList: TList;         // List of all cached updates
    FOnUpdateRecord: TUpdateRecordEvent;
    { Export }
    FExportFormat: TExportFormat; // Export format used with Load / Save procs
    FExportCalc: Boolean;
    { Design datas }
    FDesignData: TMemoryStream;
    FApplyingUpdates: Boolean;
    FImporting: Boolean;
    FReadOnly: Boolean;   // Temporary variable to handle design data loading
    { Compare procs }
    function GetAnsiCompare: Boolean;
    procedure SetAnsiCompare(Value: Boolean);
    { Index Procs }
    procedure SortList(List: TList);
    function SearchInList(List: TList; Value: Pointer; var Index: Integer;
      Duplicates, Direction: Boolean): Boolean;
    function IndexedRecordCompare(Rec1, Rec2: Pointer): Integer;
    function CompareStrings(P1, P2: PChar; Options: TCompareOptions): Integer;
    function CompareFields(Rec1, Rec2: Pointer; FieldList: TList; Options: TCompareOptions; DescFields: TList): Integer;
    procedure SortCurrentRecords;
    procedure CheckIndexFields(const IndexFieldNames: string);
    {}
    procedure SetIndexFieldNames(IndexFieldNames: string);
    procedure SetIndexOptions(Options: TIndexOptions);
    procedure SetDescFieldNames(DescFieldNames: string);
    procedure ActivateIndex(Idx: TMaintainedIndex);
    procedure SetIndexCompareOptions;
    procedure SetLocateCompareOptions(Options: TLocateOptions);
    {}
    procedure AssignRangeActiveRec(Rec: Variant);
    function AcceptRangeActiveRecord(Rec: PChar): Boolean;
    {}
    function IsIndexed: Boolean;
    {}
    function GetIndexCount: Integer;
    function GetIndexDef(Idx: Integer): TIndexDef;
    { Filter Procs }
    procedure CreateFilter;
    procedure DecodeFilter;
    function AcceptFilterRecord(Rec: PChar): Boolean;
    function GetFilterCompareOptions(var Value: string): TCompareOptions;
    { Cached updates }
    function GetUpdatePendings: Boolean;
    procedure SetCachedUpdates(const Value: Boolean);
    function GetChangeCount: Integer;
    {}
    procedure AddCachedUpdate(UpdateKind: TUpdateKind; OldRec, NewRec: PChar);
    procedure CancelRecordUpdate(Update: Pointer);
    function DoAddRecord(Rec: PChar; Pos: Integer): Integer;
    function DoModifyRecord(CurRec: PChar; NewRec: PChar; RecIdx: Integer): Integer;
    procedure DoDeleteRecord(Rec: PChar; RecIdx: Integer);
    function InternalDoAddRecord(Rec: PChar; Records: TList; Idx: Integer): Integer;
    procedure InternalUpdateIndexs(CurRec, NewRec: PChar; State: TDataSetState);
    function InternalMoveIndexedRecord(CurRec, NewRec: PChar; Records: TList;
      Idx: Integer): Integer;
    {}
    function GetActiveBuffer: PChar;
    function GetMemoryUsed: Integer;
    procedure ImportDesignData;
    procedure SetReadOnly(const Value: Boolean);
  protected
    function GetCanModify: Boolean; override;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly default False;

    property AllRecords : TList read FRecords;
    property RangedRecords : TList read FRangedRecords;
    property FilteredRecords : TList read FFilteredRecords;

    property UpdatesEnabled: Boolean read FCachedUpdates write FCachedUpdates;
    property ApplyingUpdates: Boolean read FApplyingUpdates;
    { For descendents }
    property KeyRec: PChar read FKeyRec write FKeyRec;
    property CurRec: Integer read FCurRec write FCurRec;
    { Design time streaming }
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure ReadDesignData(Stream: TStream);
    procedure WriteDesignData(Stream: TStream);
    { Field Definition }
    procedure InternalInitFieldDefs; override;
    procedure InitStructure; virtual; // Initialize our internal vision of fields
    { Data handling }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    {}
    // Creates a new record in memory (used internally and with dataset buffers)
    function CreateRecord(RecordSize: Integer): PChar;
    procedure InitializeRecord(P: PChar); // Empties a record
    procedure FreeRecord(var P: PChar);
    procedure CopyRecords(SourceRec, DestRec: PChar);
    { Blobs }
    function GetBlobString(Field: TField; Buffer: PChar): PString;
    { Bookmarks }
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    { Navigation/Edition }
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalUpdateIndex;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function IsCursorOpen: Boolean; override;
    { Filters }
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    { Miscellaneous }
    procedure InternalHandleException; override;
    { Indexs }
    function IndexOf(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Integer;
    function CurrentRecords: TList; // Gives a direct access to the current set of records
   { Cached Updates }
    procedure UpdateRecord(Update: TCachedUpdate; OldValues, NewValues: TDataSet;
      ModifiedFields: TList); virtual;
    procedure DoUpdateRecord(UpdateKind: TUpdateKind; OldValues, NewValues: TDataSet;
      ModifiedFields: TList); 
    {>>Moved from public to protected}
    { Indexs }
    procedure UseIndex(const IndexFieldNames: string; Options: TIndexOptions; const DescFieldNames: string; CanCreate: Boolean); virtual;
    procedure AddIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string = ''); virtual;
    procedure DropIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string); virtual;
    property IndexCount: Integer read GetIndexCount;
    property IndexDefs[Idx: Integer]: TIndexDef read GetIndexDef;
    { Ranges }
    procedure SetRange(const StartValues, EndValues: array of const); virtual;
    procedure ApplyRange; virtual;
    procedure CancelRange; virtual;
    property RangeActive: Boolean read FRangeActive;
    {Import/Export functions}
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure ImportFieldDefs(DataSet: TDataSet); virtual;
    procedure ImportDataSet(DataSet: TDataSet); virtual;
    procedure ImportCurrentRecord(DataSet: TDataSet); virtual;
    function CanReadDesignData: Boolean; virtual;
    property Importing : Boolean read FImporting write FImporting;
    {Memory info}
    property RecordSize: Integer read FRecordSize;
    property MemoryUsed: Integer read GetMemoryUsed;
    { Cached Updates }
    procedure ApplyUpdates; virtual;
    procedure CommitUpdates; virtual;
    procedure CancelUpdates; virtual;
    property UpdatePendings: Boolean read GetUpdatePendings;
    property ChangeCount: Integer read GetChangeCount;
    {>>Moved from published to protected}
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;
    property IndexOptions: TIndexOptions read FIndexOptions write SetIndexOptions;
    property DescFieldNames: string read FDescFieldNames write SetDescFieldNames;
    property AnsiCompare: Boolean read GetAnsiCompare write SetAnsiCompare;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EmptyTable;

    { Data Reading }
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    { Bookmarks }
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    {Basic Record search}
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    function FindKey(const KeyValues: array of const): Boolean;

    {Import/Export functions}
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string); virtual;

  published
    property ExportFormat: TExportFormat read FExportFormat write FExportFormat;
    property ExportCalcFields: Boolean read FExportCalc write FExportCalc;
    {}
    property Active;
    property Filter;
    property Filtered;
    property FilterOptions;
    property OnCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  TVirtualDataSet = class(TCustomVirtualDataSet)
  private
    { Master Detail }
    FMasterLink: TDataLink;
    {}
    function GetMasterSource: TDataSource;
    procedure SetMasterSource(const Value: TDataSource);
    function GetMasterFields: string;
    procedure SetMasterFields(const Value: string);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    { Master Detail }
    procedure MasterDetailChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RecordSize;
    property MemoryUsed;

    { Indexs }
    procedure UseIndex(const IndexFieldNames: string; Options: TIndexOptions; const DescFieldNames: string; CanCreate: Boolean); override;
    procedure AddIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string = ''); override;
    procedure DropIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string); override;
    property IndexCount;
    property IndexDefs;

    { Ranges }
    procedure SetRange(const StartValues, EndValues: array of const); override;
    procedure ApplyRange; override;
    procedure CancelRange; override;
    property RangeActive;

    {Array Access}
    function IndexLookup(const Index: Integer; const ResultFields: string): Variant;

    {Import/Export functions}
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure ImportFieldDefs(DataSet: TDataSet); override;
    procedure ImportDataSet(DataSet: TDataSet); override;
    procedure ImportCurrentRecord(DataSet: TDataSet); override;
    { Cached Updates }
    procedure ApplyUpdates; override;
    procedure CommitUpdates; override;
    procedure CancelUpdates; override;
    property UpdatePendings;
  published
    {}
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    {}
    property CachedUpdates;
    property OnUpdateRecord;
    property IndexFieldNames;
    property IndexOptions;
    property DescFieldNames;
    property AnsiCompare;
  end;

  TDataList = class
  private
    FData: TCustomVirtualDataSet;
    {}
    function GetCount: Integer;
    function GetFilter: string;
    function GetIndexFieldNames: string;
    function GetIndexOptions: TIndexOptions;
    function GetItemIndex: Integer;
    procedure SetFilter(const Value: string);
    procedure SetIndexFieldNames(const Value: string);
    procedure SetIndexOptions(const Value: TIndexOptions);
    procedure SetItemIndex(const Value: Integer);
    function GetRangeActive: Boolean;
    function GetRawCount: Integer;
    function GetField(RecIdx, FieldIdx: Integer): TField;
    function GetValue(Idx: Integer; FieldName: string): TField;
  protected
    procedure CheckBounds(Idx: Integer);
  public
    constructor Create(Structure: array of string);
    destructor Destroy; override;
    {}
    procedure Clear;
    {}
    procedure Add(Values: array of Variant);
    procedure Delete(Idx: Integer);
    {}
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Count: Integer read GetCount;
    property RawCount: Integer read GetRawCount;
    {}
    function IndexOf(const KeyFields: string; const KeyValues: array of Variant;
      Options: TLocateOptions{$IFDEF D4ABOVE} = []{$ENDIF}): Integer;
    function FindKey(const KeyValues: array of const): Integer;
    {}
    procedure AddIndex(const IndexFieldNames: string; Options: TIndexOptions);
    {}
    procedure SetRange(const StartValues, EndValues: array of const);
    procedure CancelRange;
    property RangeActive: Boolean read GetRangeActive;
    {}
    property Filter: string read GetFilter write SetFilter;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexOptions: TIndexOptions read GetIndexOptions write SetIndexOptions;
    {}
    property Values[Idx: Integer; FieldName: string]: TField read GetValue; default;
    property Fields[RecIdx, FieldIdx: Integer]: TField read GetField;
    {}
    property RawData: TCustomVirtualDataSet read FData;
  end;

  TCachedUpdate = class
  private
    FDataSet: TCustomVirtualDataSet;
  public
    UpdateKind: TUpdateKind;
    RecId: PChar;
    OldData: PChar;
    NewData: PChar;
    OldPos: Integer;
    {}
    constructor Create(DataSet: TCustomVirtualDataSet);
    destructor Destroy; override;
  end;

var
  ExtendedFilterChar: Char = '*';

implementation

uses DBConsts, TypInfo, Math, FMTBcd;

resourcestring
  {$IFDEF FRENCH}
  msgNoRecordAvailable = 'Pas d''enregistrements disponibles';
  msgCantCompareFieldTypes = 'Impossible de comparer des champs du type %s';
  msgDuplicateKeyInIndex = 'Clé dupliquée dans l''index "%s"';
  msgPostKeyViol = 'Validation impossible : violation de clé dans l''index "%s" !';
  msgNoCompatibleFields = 'Rien à transférer entre les 2 ensembles de données';
  msgNoIndexDefinedForRange = 'Il n''y a pas d''index défini pour appliquer la portée';
  msgNoStartValuesForRange = 'Les valeurs de départ de la portée ne sont pas définies';
  msgNoEndValuesForRange = 'Les valeurs de fin de la portée ne sont pas définies';
  msgIncorrectRangeActiveDataType = 'Type de données incorrect pour la portée';
  msgIncorrectIndexField = 'Type de champ incompatible pour les indexs : champs %s, type %s';
  msgIncorrectFindKeyDataType = 'Type de données incorrect pour un FindKey';
  msgIncorrectListIndex = 'Index de liste incorrect : %d';
  {$ENDIF}
  {$IFDEF ENGLISH}
  msgNoRecordAvailable = 'No record available';
  msgCantCompareFieldTypes = 'Unable to compare fields of the type %s';
  msgDuplicateKeyInIndex = 'Duplicate key in index "%s"';
  msgPostKeyViol = 'Can''t post : key violation in index "%s" !';
  msgNoCompatibleFields = 'Nothing to transfer between the 2 datasets';
  msgNoIndexDefinedForRange = 'No index defined to apply range';
  msgNoStartValuesForRange = 'Start values of range undefined';
  msgNoEndValuesForRange = 'End values of range undefined';
  msgIncorrectIndexField = 'Incorrect index field kind : field %s, kind %s';
  msgIncorrectRangeActiveDataType = 'Incorrect datatype for range';
  msgIncorrectFindKeyDataType = 'Incorrect datatype for FindKey';
  msgIncorrectListIndex = 'Incorrect list index : %d';
  {$ENDIF}

type
  TListDataSet = class(TCustomVirtualDataSet)
  protected
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  end;

  TUpdateDataSet = class(TCustomVirtualDataSet)
  private
    procedure ImportStructure(ADataset: TCustomVirtualDataset);
  protected
    FReferenceDataset : TCustomVirtualDataset;
    procedure InitStructure; override;
  end;

type
  PPointer  = ^Pointer;
  PObject   = ^TObject;
  TCompareProc = function(P1, P2: Pointer): Integer of object;

type
  TRecordInfo = packed record
    Bookmark: Pointer;
    BookmarkFlag: TBookmarkFlag;
  end;
  PRecordInfo = ^TRecordInfo;

  TMasterDataLink = class(TDataLink)
  private
    FDataSet: TDataSet;
    FFieldNames: string;
    FFields: TList;
    {}
    procedure GetFields;
    procedure SetFieldNames(const Value: string);
    function GetLinkActive: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(DataSet: TDataSet);
    destructor Destroy; override;
    {}
    property FieldNames: string read FFieldNames write SetFieldNames;
    property Fields: TList read FFields;
    {}
    property LinkActive: Boolean read GetLinkActive;
  end;

type
  TBlobStream = class(TStream)
  private
    FDataSet: TCustomVirtualDataSet;
    FAssocBuffer: PChar;
    FModified: Boolean;
    FPosition: Integer;
    FField: TBlobField;
    FMode: TBlobStreamMode;
  protected
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function GetBlobSize: Longint;
  end;

function Min(I1, I2: Integer): Integer;
begin
  if I1 > I2 then Result:= I2 else Result:= I1;
end;

{ TCustomVirtualDataSet }

function TCustomVirtualDataSet.AllocRecordBuffer: PChar;
begin
  Result:= CreateRecord(FBufferSize);
end;

function TCustomVirtualDataSet.CanReadDesignData: Boolean;
begin
  Result := True;
end;

procedure TCustomVirtualDataSet.CopyRecords(SourceRec, DestRec: PChar);
var
  i: Integer;
  SrcString, DstString: PString;
begin
  if FBlobsPos.Count > 0 then
  begin
      { Save blob references }
    for i:= 0 to FBlobsPos.Count - 1 do
      FBuffBlobInit[i]:= PPointer(DestRec + 1 + Integer(FBlobsPos[i]))^;
      { Copy all non-blob fields values }
    Move(SourceRec^, DestRec^, FRecordSize);
      { Copy back blob references and assign blob values }
    for i:= 0 to FBlobsPos.Count - 1 do
    begin
      SrcString:= PString(FBuffBlobInit[i]);
      DstString:= PString(PPointer(SourceRec + 1 + Integer(FBlobsPos[i]))^);
      SetLength(SrcString^, Length(DstString^));
      Move(PChar(DstString^)^, PChar(SrcString^)^, Length(DstString^));
      PPointer(DestRec + 1 + Integer(FBlobsPos[i]))^:= FBuffBlobInit[i];
    end;
  end else
    Move(SourceRec^, DestRec^, FRecordSize);
end;

constructor TCustomVirtualDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BookmarkSize:= SizeOf(Pointer);
  FFieldPos:= TList.Create;
  FIndexFields:= TList.Create;
  FDescFields:= TList.Create;
  FBlobsPos:= TList.Create;
  FBuffBlobInit:= TList.Create;
  FMaintainedIndexs:= TIndexList.Create;
  FIndexOptions:= [];
  FTextCompareProc:= @CompareText;
  FStrCompareProc:= @CompareStr;
end;

function TCustomVirtualDataSet.CreateRecord(RecordSize: Integer): PChar;
var
  i: Integer;
  P: PString;
begin
  GetMem(Result, RecordSize);
  {}
  for i:= 0 to FBlobsPos.Count - 1 do
  begin
    New(P);
    PPointer(Result + 1 + Integer(FBlobsPos[i]))^:= P;
    Result^:= #0;
  end;
end;

function TCustomVirtualDataSet.CurrentRecords: TList;
begin
  if Filtered then
    Result:= FFilteredRecords
  else if FRangeActive then
    Result:= FRangedRecords
  else
    Result:= FRecords;
end;

procedure TCustomVirtualDataSet.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('RawData', ReadDesignData, WriteDesignData, Active);
end;

destructor TCustomVirtualDataSet.Destroy;
begin
  Close;

  FUpdateList.Free;
  FIndexFields.Free;
  FDescFields.Free;
  FFieldPos.Free;
  FBuffBlobInit.Free;
  FBlobsPos.Free;
  FRecords.Free;
  FRangedRecords.Free;
  FFilteredRecords.Free;
  FMaintainedIndexs.Free;
  FDesignData.Free;
  inherited Destroy;
end;

procedure TCustomVirtualDataSet.FreeRecord(var P: PChar);
var
  i: Integer;
  PStr: PString;
begin
  for i:= 0 to FBlobsPos.Count - 1 do
  begin
    PStr:= PPointer(P + 1 + Integer(FBlobsPos[i]))^;
    Dispose(PStr);
  end;
  {}
  FreeMem(P);
end;

procedure TCustomVirtualDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeRecord(Buffer);
end;

function TCustomVirtualDataSet.GetActiveBuffer: PChar;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Result:= nil
      else
        Result:= ActiveBuffer;
    dsEdit, dsInsert: Result:= ActiveBuffer;
    dsCalcFields: Result:= CalcBuffer;
    dsSetKey, dsFilter: Result:= FKeyRec;
    else
      Result:= nil;
    end;
end;

function TCustomVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean;
begin
  if Field.DataType = ftWideString then
    Result := GetFieldData(Field, Buffer)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

function TCustomVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  CurrentBuffer: PChar;
  P: PString;
  L: Integer;

  procedure AssignWS;
  var
    PW : PWideChar;
    T : Integer;
  begin
    T := L + SizeOf(WideChar);
    GetMem(PW, T);
    try
      FillChar(PW^, T, 0);
      Move(P^[1], PW^, L);
      WideString(Buffer^) := PW;
    finally
      FreeMem(PW, T);
    end;
  end;
  function IsAGoodBlob : Boolean;
  var
    AMS : TStream;
    AString : string;
    Len : Integer;
  begin
    Result := (Field is TBlobField) and (TBlobField(Field).BlobSize > 0);
    if Result and (Buffer <> nil) then
    begin
      AMS := CreateBlobStream(Field, bmRead);
      try
        Len := AMS.Size;
        SetString(AString, nil, Len);
        AMS.ReadBuffer(Pointer(AString)^, Len);
        P:= @AString;
        L:= Length(P^);
        if L <> 0 then
          Move(P^[1], Buffer^, L + 1)
        else
          Char(Buffer^):= #0;
      finally
        AMS.Free;
      end;
    end;
  end;
begin
  Result:= False;
  CurrentBuffer:= GetActiveBuffer;
  if CurrentBuffer = nil then	Exit;
  with Field do
  begin
    if State in [dsSetKey, dsFilter, dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      if IsAGoodBlob then
      begin
        Result := True;
        Exit;
      end;
      if FieldKind in [fkData, fkInternalCalc] then
        Inc(CurrentBuffer, Integer(FFieldPos[FFieldIndex[Field.FieldNo - 1]]))
      else
        Inc(CurrentBuffer, FRecordSize + Offset);
      Result:= Boolean(CurrentBuffer^);
      if Result and (Buffer <> nil) then
      begin
        if (FieldKind in [fkData, fkInternalCalc]) and (DataType = ftWideString) then
        begin
          Inc(CurrentBuffer);
          P:= PString(PPointer(CurrentBuffer)^);
          L:= Length(P^);
          AssignWS;
        end
        else if (FieldKind in [fkData, fkInternalCalc]) and (DataType = ftString) and (DataSize > 10) then
        begin
          Inc(CurrentBuffer);
          P:= PString(PPointer(CurrentBuffer)^);
          L:= Length(P^);
          if L <> 0 then
            Move(P^[1], Buffer^, L + 1)
          else
            Char(Buffer^):= #0;
        end
        else if (FieldKind in [fkData, fkInternalCalc]) and (DataType = ftBCD) then
          Move(CurrentBuffer[1], Buffer^, SizeOf(TBCD))
        else
          Move(CurrentBuffer[1], Buffer^, DataSize);
      end;
    end;
  end;
end;

function TCustomVirtualDataSet.GetAnsiCompare: Boolean;
begin
  Result:= @FTextCompareProc = @AnsiCompareText;
end;

function TCustomVirtualDataSet.GetMemoryUsed: Integer;
var
  i: Integer;
        
  function ListSize(List: TList): Integer;
  begin
    Result:= List.Capacity * SizeOf(Pointer);
  end;
        
  function DataSize: Integer;
  var
    i, j: Integer;
    P: PChar;
  begin
    Result:= RecordSize * FRecords.Count;
    if FBlobsPos.Count > 0 then
      for i:= 0 to FRecords.Count - 1 do
      begin
        P:= FRecords[i];
          // Note : + 8 represents the size of the delphi string "object" :
          // dynamic pointer + string size
        for j:= 0 to FBlobsPos.Count - 1 do
          Result:= Result + 8 + Length(PString(PPointer(P + 1 + Integer(FBlobsPos[j]))^)^);
      end;
  end;
        
begin
  Result:= DataSize;
  Result:= Result + ListSize(FRecords);
  if Assigned(FFilteredRecords) then
    Result:= Result + ListSize(FFilteredRecords);
  if Assigned(FRangedRecords) then
    Result:= Result + ListSize(FRangedRecords);
  for i:= 0 to FMaintainedIndexs.Count - 1 do
    Result:= Result + ListSize(TMaintainedIndex(FMaintainedIndexs[i]).Records);
end;

function TCustomVirtualDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Records: TList;
begin
  Records:= CurrentRecords;
  if Records.Count <= 0 then
    Result:= grEOF
  else 
  begin
    Result:= grOK;
    case GetMode of
    gmNext:
      if FCurRec >= Records.Count - 1  then
          Result:= grEOF
      else
          Inc(FCurRec);
    gmPrior:
      if FCurRec <= 0 then
          Result:= grBOF
      else
          Dec(FCurRec);
    gmCurrent:
      if (FCurRec < 0) or (FCurRec >= Records.Count) then
          Result:= grError;
    end;
    if Result = grOK then
    begin
      CopyRecords(Records[FCurRec], Buffer);
      with PRecordInfo(Buffer + FRecInfoPos)^ do
      begin
        BookmarkFlag:= bfCurrent;
        Bookmark:= Records[FCurRec];
      end;
      GetCalcFields(Buffer);
    end	
    else if (Result = grError) and DoCheck then
      DatabaseError(msgNoRecordAvailable);
  end;
end;

function TCustomVirtualDataSet.GetRecordSize: Word;
begin
  Result:= FRecordSize;
end;

procedure TCustomVirtualDataSet.ImportCurrentRecord(DataSet: TDataSet);
var
  L1, L2: TList;
  F: TField;
  i: Integer;
begin
  FImporting := True;
  try
    CheckActive;
    {}
    L1:= TList.Create;
    L2:= TList.Create;
    try
      for i:= 0 to FieldCount - 1 do
      begin
        F:= DataSet.FindField(Fields[i].FieldName);
        if Assigned(F) and (Fields[i].FieldKind = fkData) then
        begin
          L1.Add(F);
          L2.Add(Fields[i]);
        end;
      end;
      {}
      for i:= 0 to L1.Count - 1 do
        TField(L2[i]).Assign(TField(L1[i]));
    finally
      L2.Free;
      L1.Free;
    end;
  finally
    FImporting := False;
  end;
end;

procedure TCustomVirtualDataSet.ImportDataSet(DataSet: TDataSet);
var
  L1, L2: TList;
  F: TField;
  i: Integer;
begin
  FImporting := True;
  try
    L1:= TList.Create;
    L2:= TList.Create;
    try
      for i:= 0 to FieldCount - 1 do
      begin
        F:= DataSet.FindField(Fields[i].FieldName);
        if Assigned(F) and (Fields[i].FieldKind = fkData) then
        begin
          L1.Add(F);
          L2.Add(Fields[i]);
          //Fields[i].Origin := F.Origin;
        end;
      end;
      EmptyTable;
      DataSet.First;
      while not DataSet.EOF do
      begin
        Append;
        for i:= 0 to L1.Count - 1 do
          TField(L2[i]).Assign(TField(L1[i]));
        Post;
        DataSet.Next;
      end;
    finally
      L2.Free;
      L1.Free;
    end;
  finally
    FImporting := False;
  end;
end;

procedure TCustomVirtualDataSet.ImportDesignData;
var
  DF: TExportFormat;
begin
  if Assigned(FDesignData) and CanReadDesignData and Active then
  begin
    DF:= ExportFormat;
    try
      ExportFormat:= efGeneric;
      LoadFromStream(FDesignData);
    finally
      ExportFormat:= DF;
    end;
    FDesignData.Free;
    FDesignData:= nil;
  end;
end;

procedure TCustomVirtualDataSet.ImportFieldDefs(DataSet: TDataSet);
var
  i: Integer;
begin
  FImporting := True;
  try
    CheckInactive;
    FieldDefs.Clear;
    for i:= 0 to DataSet.FieldDefs.Count - 1 do
      FieldDefs.AddFieldDef.Assign(DataSet.FieldDefs[i]);
  finally
    FImporting := False;
  end;
end;

procedure TCustomVirtualDataSet.InitializeRecord(P: PChar);
var
  i: Integer;
begin
  if FBlobsPos.Count > 0 then
  begin
      { Save temporarily blob references }
    for i:= 0 to FBlobsPos.Count - 1 do
      FBuffBlobInit[i]:= PPointer(P + 1 + Integer(FBlobsPos[i]))^;
      { Initialize record }
      FillChar(P^, FRecordSize, 0);
      { Copy back blob references and empty them }
    for i:= 0 to FBlobsPos.Count - 1 do
    begin
      SetLength(PString(FBuffBlobInit[i])^, 0);
      PPointer(P + 1 + Integer(FBlobsPos[i]))^:= FBuffBlobInit[i];
    end;
  end 
  else
    FillChar(P^, FRecordSize, 0);
end;

procedure TCustomVirtualDataSet.InitStructure;
var
  i: Integer;
begin
  FFieldPos.Clear;
  FBlobsPos.Clear;
  FBuffBlobInit.Clear;
  SetLength(FFieldIndex, FieldCount);
  FRecordSize:= 0;
  for i:= 0 to FieldCount - 1 do
    FFieldIndex[i] := 0;
  for i:= 0 to FieldCount - 1 do
    with Fields[i] do
    begin
      if not (FieldKind in [fkCalculated, fkLookup]) then
      begin
        FFieldPos.Add(Pointer(FRecordSize));
        FFieldIndex[FieldNo - 1] := FFieldPos.Count - 1;
        if (Fields[i] is TBlobField) or (DataType = ftWideString) or
          ((DataType = ftString) and (DataSize > 10)) then
        begin
          FBlobsPos.Add(Pointer(FRecordSize));
          FBuffBlobInit.Add(nil);
          Inc(FRecordSize, SizeOf(Pointer) + 1);
        end
        else
        if (DataType = ftBCD) then
        begin
          Inc(FRecordSize, SizeOf(TBCD) + 1);
        end
        else
          Inc(FRecordSize, DataSize + 1);
      end;
    end;
{  for i:= 0 to FieldCount - 1 do
    with Fields[i] do
    begin
      if not (FieldKind in [fkCalculated, fkLookup]) then
      begin
        FFieldPos.Add(Pointer(FRecordSize));
        FFieldIndex[FieldNo - 1] := FFieldPos.Count - 1;
        if (Fields[i] is TBlobField) or (DataType = ftWideString) or
          ((DataType = ftString) and (DataSize > 10)) then
        begin
          FBlobsPos.Add(Pointer(FRecordSize));
          FBuffBlobInit.Add(nil);
          Inc(FRecordSize, SizeOf(Pointer) + 1);
        end
        else
        if (DataType = ftBCD) then
        begin
          Inc(FRecordSize, SizeOf(TBCD) + 1);
        end
        else
          Inc(FRecordSize, DataSize + 1);
      end;
    end;}
end;

procedure TCustomVirtualDataSet.InternalHandleException;
begin
end;

procedure TCustomVirtualDataSet.InternalInitFieldDefs;
var
  i: Integer;
  AFD : TFieldDef;
  AF : TField;
begin
  if FieldCount = 0 then
    Exit;
  FieldDefs.Clear;
  for i:= 0 to FieldCount - 1 do
  begin
    AF := Fields[i];
    if AF.FieldKind in [fkData, fkInternalCalc] then
    begin
      AFD := FieldDefs.AddFieldDef;

      AFD.FieldNo := AF.FieldNo;
      AFD.Name := AF.FieldName;
      AFD.DataType := AF.DataType;
      if AF.IsBlob or (AF is TStringField) or (AF is TWideStringField) then
        AFD.Size := AF.DataSize;
      if AF is TBCDField then
        AFD.Precision := TBCDField(AF).Precision
      else if AF is TFMTBCDField then
        AFD.Precision := TFMTBCDField(AF).Precision;

      if AF.Required then
        AFD.Attributes := AFD.Attributes + [faRequired];
      if AF.ReadOnly  then
        AFD.Attributes := AFD.Attributes + [faReadonly];
      if ((AF is TStringField) and TStringField(AF).FixedChar) or (AF.DataType = ftFixedChar) then
        AFD.Attributes := AFD.Attributes + [faFixed];

      AFD.InternalCalcField := (AF.FieldKind = fkInternalCalc);
    end;
  end;
end;

procedure TCustomVirtualDataSet.InternalInitRecord(Buffer: PChar);
begin
  InitializeRecord(Buffer);
end;

procedure TCustomVirtualDataSet.Loaded;
begin
  inherited Loaded;
  ImportDesignData;
end;

procedure TCustomVirtualDataSet.LoadFromFile(const FileName: string);
var
  F: TFileStream;
begin
  CheckBrowseMode;
  F:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TCustomVirtualDataSet.LoadFromStream(Stream: TStream);
var
  S: TDataSetStreamer;
  Temp: Boolean;
begin
  CheckBrowseMode;
  S:= TDataSetStreamer.Create(Self, ExportFormat, ExportCalcFields);
  try
    Temp:= UpdatesEnabled;
    UpdatesEnabled:= False;
    try
      EmptyTable; // Faster than the DataSet methods included in the DataSetStreamer
      S.LoadFromStream(Stream);
    finally
      UpdatesEnabled:= Temp;
    end;
  finally
    S.Free;
  end;
end;

procedure TCustomVirtualDataSet.ReadDesignData(Stream: TStream);
var
  L: Integer;
begin
  Stream.Read(L, SizeOf(Integer));
  if L <> 0 then
  begin
    FDesignData:= TMemoryStream.Create;
    FDesignData.CopyFrom(Stream, L);
    FDesignData.Position:= 0;
  end;
end;

procedure TCustomVirtualDataSet.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  CheckBrowseMode;
  F:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TCustomVirtualDataSet.SaveToStream(Stream: TStream);
var
  S: TDataSetStreamer;
begin
  CheckBrowseMode;
  S:= TDataSetStreamer.Create(Self, ExportFormat, ExportCalcFields);
  try
    S.SaveToStream(Stream);
  finally
    S.Free;
  end;
end;

procedure TCustomVirtualDataSet.SetAnsiCompare(Value: Boolean);
begin
  if Value then
  begin
    FTextCompareProc:= @AnsiCompareText;
    FStrCompareProc:= @AnsiCompareStr;
  end else
  begin
    FTextCompareProc:= @CompareText;
    FStrCompareProc:= @CompareStr;
  end;
end;

procedure TCustomVirtualDataSet.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
begin
  if Field.DataType = ftWideString then
    SetFieldData(Field, Buffer)
  else
    inherited;
end;

procedure TCustomVirtualDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var CurrentBuffer: PChar;
    P: PString;
    W: WideString;
    T: Integer;
begin
  with Field do
  begin
    if not (State in dsWriteModes) then	DatabaseError(SNotEditing);
    CurrentBuffer:= GetActiveBuffer;
    if Field.FieldKind in [fkData, fkInternalCalc] then
    begin
      if State = dsCalcFields then DatabaseError(SNotEditing);
      Validate(Buffer);
      Inc(CurrentBuffer, Integer(FFieldPos[FFieldIndex[FieldNo - 1]]))
    end
    else
      Inc(CurrentBuffer, FRecordSize + Offset);
    Boolean(CurrentBuffer[0]):= Buffer <> nil; //Octet pour dire si c'est null ou pas...
    if Boolean(CurrentBuffer[0]) then
    begin
      if (FieldKind in [fkData, fkInternalCalc]) and (DataType = ftWideString) then
      begin
        Inc(CurrentBuffer);
        P:= PString(PPointer(CurrentBuffer)^);
        W := WideString(Buffer^);
        T:= Length(W) * SizeOf(WideChar);
        SetLength(P^, T);
        Move(W[1], P^[1], T);
      end
      else if (FieldKind in [fkData, fkInternalCalc]) and (DataType = ftString) and (DataSize > 10) then
      begin
        Inc(CurrentBuffer);
        P:= PString(PPointer(CurrentBuffer)^);
        P^:= PChar(Buffer);
      end
      else if (FieldKind in [fkData, fkInternalCalc]) and (DataType = ftBCD) then
        Move(Buffer^, CurrentBuffer[1], SizeOf(TBCD))
      else
        Move(Buffer^, CurrentBuffer[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end;

procedure TCustomVirtualDataSet.WriteDesignData(Stream: TStream);
var
  DF: TExportFormat;
  M: TMemoryStream;
  L: Integer;
  OldFilter, OldRange: Boolean;
begin
  M:= TMemoryStream.Create;
  DF:= ExportFormat;
  try
    OldFilter:= Filtered;
    OldRange:= RangeActive;
    Filtered:= False;
    CancelRange;
    try
      ExportFormat:= efGeneric;
      SaveToStream(M);
      L:= M.Size;
      Stream.Write(L, SizeOf(Integer));
      if L <> 0 then
      begin
        M.Position:= 0;
        Stream.CopyFrom(M, L);
      end;
    finally
      if OldRange then
        ApplyRange;
      Filtered:= OldFilter;
    end;
  finally
    ExportFormat:= DF;
    M.Free;
  end;
end;

{ Blob handling }

function TCustomVirtualDataSet.GetBlobString(Field: TField; Buffer: PChar): PString;
begin
  Result:= PString(Pointer(Buffer + 1 + Integer(FFieldPos[FFieldIndex[Field.FieldNo - 1]]))^);
end;

function TCustomVirtualDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:= TBlobStream.Create(Field as TBlobField, Mode);
end;

{ *************************** Open/Close *************************** }

procedure TCustomVirtualDataSet.InternalOpen;
begin
  FCurRec:= -1; { Note : -1 => Empty }
  { Create the fields }
  if DefaultFields then
    CreateFields
  else { Init Field definitions }
    InternalInitFieldDefs;
  BindFields(True);
  { Calculate the record buffer size & the Field positions in the internal structure }
  InitStructure;
  { Calculate the size of buffers }
  FRecInfoPos:= FRecordSize + CalcFieldsSize;
  FBufferSize:= FRecInfoPos + SizeOf(TRecordInfo);
  { Create data list }
  FRecords:= TList.Create;
  FRecords.Capacity:= 50;
  { Prepares Indexs & Filters }
  InternalUpdateIndex;
  if RangeActive then
    ApplyRange;
  if Filtered then
  begin
    DecodeFilter;
    CreateFilter;
  end;
end;

procedure TCustomVirtualDataSet.InternalClose;
begin
  { Destroy cached updates }
  if CachedUpdates then
    CommitUpdates;
  { Destroy all datas }
  if Assigned(FRecords) then
  begin
    EmptyTable;
    FRecords.Free;
    FRecords:= nil;
    FFilteredRecords.Free;
    FFilteredRecords:= nil;
    FRangedRecords.Free;
    FRangedRecords:= nil;
    FRangeActive:= False;
  end;
  if DefaultFields then
    DestroyFields;
end;

function TCustomVirtualDataSet.IsCursorOpen: Boolean;
begin
  Result:= Assigned(FRecords);
end;

{ ***************************** Bookmark functions ***************************** }

procedure TCustomVirtualDataSet.InternalGotoBookmark(Bookmark: Pointer);
var 
  Index: Integer;
begin
  Index:= CurrentRecords.IndexOf(PPointer(Bookmark)^);
  if Index <> -1 then
    FCurRec:= Index;
end;

procedure TCustomVirtualDataSet.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@(PRecordInfo(Buffer + FRecInfoPos)^.Bookmark));
end;

function TCustomVirtualDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result:= PRecordInfo(Buffer + FRecInfoPos)^.BookmarkFlag;
end;

function TCustomVirtualDataSet.GetChangeCount: Integer;
begin
  if Assigned(FUpdateList) then
    Result:= FUpdateList.Count
  else
    Result:= 0;
end;

procedure TCustomVirtualDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer + FRecInfoPos)^.BookmarkFlag:= Value;
end;

procedure TCustomVirtualDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PPointer(Data)^:= PRecordInfo(Buffer + FRecInfoPos)^.Bookmark;
end;

procedure TCustomVirtualDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PRecordInfo(Buffer + FRecInfoPos)^.Bookmark:= PPointer(Data)^;
end;

function TCustomVirtualDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result:= CurrentRecords.IndexOf(PPointer(Bookmark)^) <> -1;
end;

function TCustomVirtualDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var I1, I2: Integer;
begin
  if Assigned(Bookmark1) and Assigned(Bookmark2) then
  begin
    I1:= CurrentRecords.IndexOf(PPointer(Bookmark1)^);
    I2:= CurrentRecords.IndexOf(PPointer(Bookmark2)^);
    Result:= I2 - I1;
  end
  else
    Result := -1;
end;

{ ***************************** Navigation/Edition ***************************** }

procedure TCustomVirtualDataSet.InternalFirst;
begin
  FCurRec:= -1;
end;

procedure TCustomVirtualDataSet.InternalLast;
begin
  FCurRec:= CurrentRecords.Count;
end;

procedure TCustomVirtualDataSet.SetIndexCompareOptions;
begin
  FCompareOptions:= [];
  if ixDescending in FIndexOptions then
    Include(FCompareOptions, coDescending);
  if ixCaseInsensitive in FIndexOptions then
    Include(FCompareOptions, coCaseInsensitive);
end;

procedure TCustomVirtualDataSet.InternalPost;
var 
  CurRec: Pointer;
  NewRec: Pointer;
  
  procedure CheckUniqueness;
  var 
    i: Integer;
    Index: TMaintainedIndex;
    
    procedure CheckIsUnique;
    var 
      Tmp: Integer;
    begin
      SetIndexCompareOptions;
      if (IndexFieldNames <> '') and ((State = dsInsert) or ((FRecords.Count > 0) and
         (CompareFields(CurRec, NewRec, FIndexFields, FCompareOptions, FDescFields) <> 0))) then
      begin
        if SearchInList(FRecords, NewRec, Tmp, False, False) then
        begin
          if SearchInList(FRecords, NewRec, Tmp, False, False) then
            raise EKeyViol.CreateFmt(msgPostKeyViol, [IndexFieldNames]);
        end;
      end;
    end;
    
  begin
    if FRecords.Count > 0 then
    begin
      if ixUnique in IndexOptions then
        CheckIsUnique;
      for i:= 0 to FMaintainedIndexs.Count - 1 do
      begin
        Index:= FMaintainedIndexs[i];
        if ixUnique in Index.Options then
        begin
          ActivateIndex(Index);
          try
            CheckIsUnique;
          finally // Ensures the restoration of current index
            ActivateIndex(Index);
          end;
        end;
      end;
    end;
  end;
          
begin
  inherited; //Appelle CheckRequiredFields
  NewRec:= ActiveBuffer;
  {}
  if State = dsEdit then
  begin
    CurRec:= CurrentRecords[FCurRec];
    CheckUniqueness;
    {}
    if CachedUpdates then
      AddCachedUpdate(ukModify, CurRec, NewRec);
    {}
    FCurRec:= DoModifyRecord(CurRec, NewRec, FCurRec);
  end 
  else
  begin
    CheckUniqueness;
    CurRec:= NewRec;
    {}
    NewRec:= CreateRecord(FRecordSize);
    InitializeRecord(NewRec);
    if CachedUpdates then
      AddCachedUpdate(ukInsert, CurRec, NewRec);
    CopyRecords(CurRec, NewRec);
    {}
    FCurRec:= DoAddRecord(NewRec, FCurRec);
  end;
end;

procedure TCustomVirtualDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then
    InternalLast;
  InternalPost;
end;

procedure TCustomVirtualDataSet.InternalDelete;
var 
  CurRec: PChar;
begin
  CurRec:= CurrentRecords[FCurRec];
  { Cache update the record }
  if CachedUpdates then
    AddCachedUpdate(ukDelete, CurRec, CurRec);
  { Delete it }
  DoDeleteRecord(CurRec, FCurRec);
end;

procedure TCustomVirtualDataSet.EmptyTable;
var 
  i: Integer;
  P: PChar;
begin
  if not Assigned(FRecords) then
    Exit;
  if State in [dsEdit, dsInsert] then
    Cancel;
  for i:= 0 to FRecords.Count - 1 do
  begin
    P:= FRecords[i];
    FreeRecord(P);
  end;
  FRecords.Clear;
  if Assigned(FFilteredRecords) then
    FFilteredRecords.Clear;
  if Assigned(FRangedRecords) then
    FRangedRecords.Clear;
  for i:= 0 to FMaintainedIndexs.Count - 1 do
    TMaintainedIndex(FMaintainedIndexs[i]).Records.Clear;
  FCurRec:= -1;
  ClearBuffers;
  DataEvent(deDataSetChange, 0);
end;

function TCustomVirtualDataSet.GetRecordCount: Longint;
begin
  Result:= CurrentRecords.Count;
end;

function TCustomVirtualDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result:= 1
  else
    Result:= FCurRec + 1;
end;

procedure TCustomVirtualDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= CurrentRecords.Count) then
  begin
    FCurRec:= Value - 1;
    Resync([]);
  end;
end;

procedure TCustomVirtualDataSet.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

{ *********************************** Indexs *********************************** }

function TCustomVirtualDataSet.IsIndexed: Boolean;
begin
  Result:= (FIndexFields.Count > 0) or (FMaintainedIndexs.Count > 0);
end;

procedure TCustomVirtualDataSet.CheckIndexFields(const IndexFieldNames: string);
var 
  L: TList;
  i: Integer;
begin
  L:= TList.Create;
  try
    GetFieldList(L, IndexFieldNames);
    for i:= 0 to L.Count - 1 do
      if TField(L[i]).FieldKind in [fkLookup, fkCalculated] then
        raise Exception.CreateFmt(msgIncorrectIndexField,
          [TField(L[i]).FieldName, GetEnumName(TypeInfo(TFieldKind),
            Integer(TField(L[i]).FieldKind))]);
  finally
    L.Free;
  end;
end;

procedure TCustomVirtualDataSet.SortCurrentRecords;
var 
  L: TList;
  i: Integer;
begin
  if IndexFieldNames = '' then
    Exit; // absolutely nothing to sort ...
  if ixUnique in IndexOptions then
  begin
    L:= TList.Create;
    L.Count:= FRecords.Count;
    Move(FRecords.List^, L.List^, L.Count * SizeOf(Pointer));
    SortList(L);
    for i:= 0 to L.Count - 2 do
      if CompareFields(L[i], L[i + 1], FIndexFields, FCompareOptions, FDescFields) = 0 then
      begin
        L.Free;
        raise Exception.CreateFmt(msgDuplicateKeyInIndex, [IndexFieldNames]);
      end;
    FRecords.Free;
    FRecords:= L;
  end 
  else
    SortList(FRecords);
end;

procedure TCustomVirtualDataSet.InternalUpdateIndex;
begin
  FIndexFields.Clear;
  FDescFields.Clear;
  if IndexFieldNames = '' then
    Exit;
  CheckIndexFields(IndexFieldNames);
  CheckIndexFields(DescFieldNames);
  FIndexFields.Clear;
  GetFieldList(FIndexFields, IndexFieldNames);
  FDescFields.Clear;
  GetFieldList(FDescFields, DescFieldNames);
  SetIndexCompareOptions;
  if FRecords.Count > 0 then
    SortCurrentRecords;
end;

procedure TCustomVirtualDataSet.SetIndexFieldNames(IndexFieldNames: string);
begin
  if CompareText(IndexFieldNames, FIndexFieldNames) <> 0 then
    UseIndex(IndexFieldNames, IndexOptions, DescFieldNames, False);
end;

procedure TCustomVirtualDataSet.SetIndexOptions(Options: TIndexOptions);
begin
  { VDataSet only supports [ixUnique, ixCaseInsensitive, ixDescending] options }
  Options:= Options * [ixUnique, ixCaseInsensitive, ixDescending];
  if Options <> FIndexOptions then
    UseIndex(IndexFieldNames, Options, DescFieldNames, False);
end;

procedure TCustomVirtualDataSet.SetDescFieldNames(DescFieldNames: string);
begin
  if CompareText(DescFieldNames, FDescFieldNames) <> 0 then
    UseIndex(IndexFieldNames, IndexOptions, DescFieldNames, False);
end;

procedure TCustomVirtualDataSet.SortList(List: TList);
  
  procedure QuickSort(PList: PPointerList; L, R: Integer);
  var 
    i, j: Integer;
    P, T: Pointer;
  begin
    repeat
      i:= L;
      j:= R;
      P:= PList^[(L + R) shr 1];
      repeat
        while IndexedRecordCompare(PList^[i], P) < 0 do Inc(i);
        while IndexedRecordCompare(PList^[j], P) > 0 do Dec(j);
        if i <= j then
        begin
          T:= PList^[i];
          PList^[i]:= PList^[j];
          PList^[j]:= T;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if L < j then
        QuickSort(PList, L, j);
      L:= i;
    until i >= R;
  end;
      
begin
  QuickSort(List.List, 0, List.Count - 1);
end;

function TCustomVirtualDataSet.SearchInList(List: TList; Value: Pointer; var Index: Integer;
  Duplicates, Direction: Boolean): Boolean;
  
  { Note : Negative result ==> not found }
  function DichoSearch(PList: PPointerList; StartIndex, EndIndex: Integer;
    Item: Pointer; var Index: Integer; Duplicates, Direction: Boolean): Boolean;
  var 
    Cmp, Med, InitialEnd: Integer;
    DirectionModifier: Integer;
  begin
    // Without the Duplicates and Direction parameters, it is only a simple
    // dichotomic search.
    // The Duplicates parameter forces the search until it is sure that the found
    // item is the FIRST responding to the search criteria.
    // The Direction parameter is only used with Duplicates. If set to true, it
    // will force the search until the LAST item responding the search criteria is found.
    InitialEnd:= EndIndex;
    DirectionModifier:= 0;
    if Direction then
      Inc(DirectionModifier);
    while (StartIndex < EndIndex) do
    begin
      Med:= (StartIndex + EndIndex) shr 1;
      if Direction and Duplicates then
        Inc(Med, (StartIndex + EndIndex) mod 2);
      Cmp:= IndexedRecordCompare(PList^[Med], Item);
      if Cmp = 0 then
      begin
        // if there are duplicates, the search must continue until the first/last
        if Duplicates then
        begin
          if Direction then
            StartIndex:= Med
          else
            EndIndex:= Med;
        end 
        else
        begin
          StartIndex:= Med;
          EndIndex:= Med;
        end;
      end 
      else if Cmp > 0 then
        EndIndex:= Med - DirectionModifier
      else
        StartIndex:= Med + 1;
    end;
    if StartIndex > InitialEnd then
    begin
      Cmp:= -1;
      EndIndex:= -1;
    end 
    else
      Cmp:= IndexedRecordCompare(PList^[StartIndex], Item);
    if Cmp = 0 then
      Index:= StartIndex
    else if (Cmp < 0) and (EndIndex = InitialEnd) then
      Index:= - StartIndex - 1
    else
      Index:= - StartIndex;
    Result:= Cmp = 0;
  end;
    
begin
  Result:= DichoSearch(List.List, 0, List.Count - 1, Value, Index, Duplicates, Direction);
end;

function TCustomVirtualDataSet.IndexedRecordCompare(Rec1, Rec2: Pointer): Integer;
begin
  Result:= CompareFields(Rec1, Rec2, FIndexFields, FCompareOptions, FDescFields);
end;

function TCustomVirtualDataSet.CompareStrings(P1, P2: PChar; Options: TCompareOptions): Integer;
  
  function InsensitiveContaining: Integer;
  var 
    S1, S2: string;
  begin
    S1:= UpperCase(P1);
    S2:= UpperCase(P2);
    Result:= Pos(S2, S1);
    if Result = 0 then
      Result:= 1
    else
      Result:= 0;
  end;
    
begin
  if coContaining in Options then
  begin
    if coCaseInsensitive in Options then
      Result:= InsensitiveContaining
    else
    begin
      Result:= Pos(P2, P1);
      if Result = 0 then
        Result:= 1
      else
        Result:= 0;
    end;
  end 
  else if coBeginningBy in Options then
  begin
    if (Length(P2) = 0) and (Length(P1) <> 0) then
    begin
      Result:= 1;
      Exit;
    end;
    if coCaseInsensitive in Options then
      Result:= FTextCompareProc(Copy(P1, 1, Length(P2)), P2)
    else
      Result:= FStrCompareProc(Copy(P1, 1, Length(P2)), P2);
  end 
  else
  begin
    if (Length(P2) = 0) and (Length(P1) <> 0) then
    begin
      Result:= 1;
      Exit;
    end;
    if coEndingBy in Options then
      P1:= P1 + Length(P1) - Min(Length(P1), Length(P2));
    if coCaseInsensitive in Options then
      Result:= FTextCompareProc(P1, P2)
    else
      Result:= FStrCompareProc(P1, P2);
  end;
end;

function TCustomVirtualDataSet.CompareFields(Rec1, Rec2: Pointer; FieldList: TList;
  Options: TCompareOptions; DescFields: TList): Integer;
type
  PSmallInt = ^SmallInt;
  PWord     = ^Word;
  PInteger  = ^Integer;
  PByte     = ^Byte;
  PDouble   = ^Double;
  PDateTime = ^TDateTime;
  PLargeInt = ^Int64;
var 
  i, Idx: Integer;
  P1, P2: PChar;
  F: TField;
  
  function FormatResult(Value: Double): Integer;
  begin
    if Value > 0 then
      Result:= 1
    else if Value < 0 then
      Result:= -1
    else
      Result:= 0;
  end;
  
  function MemoCompare: Integer;
  var 
    PS1, PS2: PString;
    
    function CompareBlob: Integer;
    var 
      L: Integer;
      P1, P2: PByte;
    begin
      Result:= 0;
      P1:= @PS1^[1];
      P2:= @PS2^[1];
      L:= Min(Length(PS1^), Length(PS2^));
      while (L > 0) and (Result = 0) do
      begin
        Result:= P1^ - P2^;
        Inc(P1);
        Inc(P2);
        Dec(L);
      end;
      if Result = 0 then
        Result:= Length(PS1^) - Length(PS2^);
    end;
  
  begin
    PS1:= GetBlobString(F, Rec1);
    PS2:= GetBlobString(F, Rec2);
    if F.DataType in [ftString, ftMemo] then
      Result:= CompareStrings(PChar(PS1^), PChar(PS2^), Options)
    else
      Result:= CompareBlob;
  end;
    
  function IsEmptyString(Rec, Pos: PChar): Boolean;
  begin
    if F.DataSize <= 10 then
      Result:= Pos^ = #0
    else
      Result:= GetBlobString(F, Rec)^ = '';
  end;

begin
  i:= 0;
  Result:= 0;
  while (i < FieldList.Count) and (Result = 0) do
  begin
    F:= TField(FieldList[i]);
    Idx:= FFieldIndex[F.FieldNo - 1];
    P1:= PChar(Rec1) + Integer(FFieldPos[Idx]) + 1;
    P2:= PChar(Rec2) + Integer(FFieldPos[Idx]) + 1;
    { Compare null values }
    Result:= Byte((P1 - 1)^) - Byte((P2 - 1)^);
    if (F.DataType = ftString) and (Result <> 0) then
    begin
      if ((Byte((P1 - 1)^) = 0) and IsEmptyString(Rec2, P2)) or
         ((Byte((P2 - 1)^) = 0) and IsEmptyString(Rec1, P1)) then
      begin
        Result:= 0;
        Inc(i);
        Continue;
      end;
    end;
    { If both fields are not null, we can compare the values }
    if F.IsBlob or ((F.DataType = ftString) and (F.DataSize > 10)) or (F.DataType = ftWideString) then
      Result:= MemoCompare
    else if (Result = 0) and Boolean((P1 - 1)^) then
      case F.DataType of
        ftString    : Result := CompareStrings(P1, P2, Options);
        ftSmallint  : Result := PSmallInt(P1)^ - PSmallInt(P2)^;
        ftAutoInc,
        ftInteger   : Result := PInteger(P1)^ - PInteger(P2)^;
        ftWord      : Result := PWord(P1)^ - PWord(P2)^;
        ftBoolean   : Result := PByte(P1)^ - PByte(P2)^;
        ftFloat,
        ftCurrency  : Result := FormatResult(PDouble(P1)^ - PDouble(P2)^);
        ftDateTime  : Result := FormatResult(PDateTime(P1)^ - PDateTime(P2)^);
        ftDate,
        ftTime      : Result := PInteger(P1)^ - PInteger(P2)^;
        ftLargeInt  : Result := CompareValue(PLargeInt(P1)^, PLargeInt(P2)^);
        ftBCD       : Result := BcdCompare(PBCD(P1)^, PBCD(P2)^);
        else
          DatabaseError(Format(msgCantCompareFieldTypes,
            [GetEnumName(TypeInfo(TFieldType), Integer(TField(FieldList[i]).DataType))]));
      end;
    if Result <> 0 then
    begin
      if DescFields.IndexOf(F) <> -1 then
        Result:= - Result;
    end;
    Inc(i);
  end;
  if coDescending in Options then
    Result:= - Result;
end;

procedure TCustomVirtualDataSet.SetLocateCompareOptions(Options: TLocateOptions);
begin
  FCompareOptions:= [];
  if loCaseInsensitive in Options then
    Include(FCompareOptions, coCaseInsensitive);
  if loPartialKey in Options then
    Include(FCompareOptions, coBeginningBy);
end;

function TCustomVirtualDataSet.IndexOf(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Integer;
var 
  FieldList: TList;
  DescList: TList;
  i, k: Integer;
  P: Pointer;
  Found: Boolean;
  Records: TList;
  Index: TMaintainedIndex;
  
  function CanUseIndex(var Index: TMaintainedIndex): Boolean;
  var 
    i: Integer;
    LocOpt: TIndexOptions;
    
    function CanUseFields(Fields: TList; IdxOptions: TIndexOptions): Boolean;
    var 
      i: Integer;
    begin
      // We can use any index having the required field names ...
      // the only other critical information is the case sensitivity of the index
      IdxOptions:= IdxOptions * [ixCaseInsensitive];
      Result:= (FieldList.Count <= Fields.Count) and (IdxOptions = LocOpt);
      {}
      if Result then
      begin
        i:= 0;
        while (i < FieldList.Count) and (FieldList[i] = Fields[i]) do
          Inc(i);
        Result:= i = FieldList.Count;
      end;
    end;
    
  begin
    if loCaseInsensitive in Options then
      LocOpt:= [ixCaseInsensitive]
    else
      LocOpt:= [];
    Index:= nil;
    Result:= (FDescFields.Count = 0) and CanUseFields(FIndexFields, FIndexOptions);
    if RangeActive or (Filtered and ((Filter <> '') or Assigned(OnFilterRecord))) then
      Exit; // if the DataSet is filtered, we can't use an inactive index
    if not Result then
    begin // Try to use an inactive index
      i:= 0;
      while (i < FMaintainedIndexs.Count) and
        ((not CanUseFields(TMaintainedIndex(FMaintainedIndexs[i]).IndexFields,
                TMaintainedIndex(FMaintainedIndexs[i]).Options)) or
         (TMaintainedIndex(FMaintainedIndexs[i]).DescFieldNames <> '')) do
        Inc(i);
      if i < FMaintainedIndexs.Count then
      begin
        Result:= True;
        Index:= FMaintainedIndexs[i];
      end;
    end;
  end;
        
begin
  CheckBrowseMode;
  Records:= CurrentRecords;
  Result:= -1;
  if Records.Count = 0 then Exit;
  Found:= False;
  if (KeyFields <> '') and (not IsEmpty) then
  begin
    FieldList:= TList.Create;
    DescList:= TList.Create;
    try
      CheckIndexFields(KeyFields);
      GetFieldList(FieldList, KeyFields);
      if FieldList.Count = 0 then
        Exit;
      FKeyRec:= AllocRecordBuffer;
      try
        SetTempState(dsSetKey);
        try
          // Write the values to be found in FKeyRec
          if FieldList.Count = 1 then
            TField(FieldList[0]).AsVariant:= KeyValues
          else for i:= 0 to FieldList.Count - 1 do
            TField(FieldList[i]).AsVariant:= KeyValues[i];
          SetLocateCompareOptions(Options);
          // Try to see if it is possible to use an index (active or not)
          if CanUseIndex(Index) then
          begin
            if Index <> nil then
            begin // use a maintained index
              ActivateIndex(Index);
              Records:= FRecords;
            end;
            try
              P:= FIndexFields;
              FIndexFields:= FieldList;
              FieldList:= P;
              P:= FDescFields;
              FDescFields:= DescList;
              DescList:= P;
              try
                Found:= SearchInList(Records, FKeyRec, i, not (ixUnique in IndexOptions), False);
              finally
                P:= FIndexFields;
                FIndexFields:= FieldList;
                FieldList:= P;
                P:= FDescFields;
                FDescFields:= DescList;
                DescList:= P;
              end;
            finally
              if Index <> nil then
              begin // we used a maintained inactive index
                ActivateIndex(Index); // restores the main index
                if Found then
                begin
                  k:= i;
                  i:= FRecords.IndexOf(Index.Records[i]);
                  // Finds the FIRST key value in the current index that matches
                  // the search criteria
                  while (k < FRecords.Count) and (CompareFields(Index.Records[k],
                    FKeyRec, FieldList, FCompareOptions, DescList) = 0) do
                  begin
                    i:= Min(i, FRecords.IndexOf(Index.Records[k]));
                    Inc(k);
                  end;
                end;
              end;
            end;
          end 
          else
          begin
            i:= 0;
            while (i < Records.Count) and (not Found) do
            begin
              Found:= CompareFields(Records[i], FKeyRec, FieldList, FCompareOptions, DescList) = 0;
              Inc(i);
            end;
            if Found then
              Dec(i);
          end;
        finally
          RestoreState(dsBrowse);
        end;
      finally
        FreeRecordBuffer(FKeyRec);
      end;
    finally
      DescList.Free;
      FieldList.Free;
    end;
    if Found then
      Result:= i;
  end;
end;

function TCustomVirtualDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
var 
  Idx: Integer;
begin
  Idx:= IndexOf(KeyFields, KeyValues, Options);
  Result:= Idx > -1;
  if Result then
    RecNo:= Idx + 1;
end;

function TCustomVirtualDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var 
  Idx: Integer;
  Fields: TList;
  L: TLocateOptions;
  FOldState : TDataSetState;
  
  function UseSensitiveIndex: Boolean;
  begin
    Result:= False;
    if IndexFieldNames <> '' then
    begin
      if CompareText(IndexFieldNames, Copy(KeyFields, 1, Length(IndexFieldNames))) = 0 then
        if not (ixCaseInsensitive in IndexOptions) then
          Result:= True;
    end 
    else if FMaintainedIndexs.IndexOfCompatibleIndex(KeyFields, [], '') <> -1 then
      Result:= True;
  end;
    
begin
  { We must check is there is a compatible case-sensitive index. If yes, we
      make a sensitive search, else we make a default insensitive search }
  if UseSensitiveIndex then
    L:= []
  else
    L:= [loCaseInsensitive];
  Idx:= IndexOf(KeyFields, KeyValues, []);
  Result:= Null;
  if Idx > -1 then
  begin
    FOldState := SetTempState(dsSetKey);
    try
      FKeyRec:= CurrentRecords[Idx];
      try
        Fields:= TList.Create;
        try
          //CheckIndexFields(ResultFields);
          GetFieldList(Fields, ResultFields);
          if Fields.Count = 1 then
            Result:= TField(Fields[0]).AsVariant
          else
          begin
            Result:= VarArrayCreate([0, Fields.Count - 1], varVariant);
            for Idx:= 0 to Fields.Count - 1 do
              Result[Idx]:= TField(Fields[Idx]).AsVariant;
          end;
        finally
          Fields.Free;
        end;
      finally
        FKeyRec:= nil;
      end;
    finally
      RestoreState(FOldState);
    end;
  end;
end;

{ ********************************* Ranges ********************************** }

procedure TCustomVirtualDataSet.ApplyRange;
var 
  Idx1, Idx2: Integer;
  Found1, Found2: Boolean;
  P: PChar;
  L: TList;
  
  function GetRange(R: Variant; var Idx: Integer; Direction: Boolean): Boolean;
  var 
    P: Pointer;
    Len: Integer;
    Dup: Boolean;
  begin
    AssignRangeActiveRec(R);
    Len:= Min(FIndexFields.Count, VarArrayHighBound(R, 1) + 1);
    // Note : in range searches, we ALWAYS consider there are duplicates
    // if the search fields are not as numerous as they should be
    Dup:= (Len < FIndexFields.Count) or not (ixUnique in FIndexOptions);
    {}
    L.Count:= Len;
    Move(FIndexFields.List^, L.List^, Len * SizeOf(Pointer));
    P:= FIndexFields;
    FIndexFields:= L;
    try
      SetIndexCompareOptions;
      Include(FCompareOptions, coBeginningBy);
      Result:= SearchInList(FRecords, FKeyRec, Idx, Dup, Direction);
      Idx:= Abs(Idx);
      if Direction and not Result then
        Dec(Idx);
    finally
      FIndexFields:= P;
    end;
  end;
      
begin
  CheckBrowseMode;
  if IndexFieldNames = '' then
    raise EDatabaseError.Create(msgNoIndexDefinedForRange);
  if VarIsNull(FStartValues) or VarIsEmpty(FStartValues) then
    raise EDatabaseError.Create(msgNoStartValuesForRange);
  if VarIsNull(FEndValues) or VarIsEmpty(FEndValues) then
    raise EDatabaseError.Create(msgNoEndValuesForRange);
  {}
  FKeyRec:= AllocRecordBuffer;
  try
    SetTempState(dsSetKey);
    try
      L:= TList.Create;
      try
{        if ixDescending in IndexOptions then
        begin
          Found1:= GetRange(FEndValues, Idx1, True);
          Found2:= GetRange(FStartValues, Idx2, False);
        end else}
        begin
          { a) Get first index of Starting value }
          Found1:= GetRange(FStartValues, Idx1, False);
          { b) Get end index of Ending value }
          Found2:= GetRange(FEndValues, Idx2, True);
        end;
        { c) Handle an exception : when Idx1 = Idx2 = 0 and nothing found }
        if (Idx1 = 0) and (Idx2 = 0) and (not Found1) and (not Found2) then
          Idx2:= -1;
      finally
        L.Free;
      end;
    finally
      RestoreState(dsBrowse);
    end;
  finally
    FreeRecordBuffer(FKeyRec);
  end;
  { Create the FRangedRecords TList }
  if not Assigned(FRangedRecords) then
    FRangedRecords:= TList.Create;
  { c) Move the records in the FRangedRecords TList }
  if (Found1 or Found2) and (Idx2 < Idx1) then
    Idx2:= Idx1; 
  if (Idx2 >= Idx1) then
  begin
    FRangedRecords.Count:= Idx2 - Idx1 + 1;
    P:= Pointer(FRecords.List);
    Move((P + Idx1 * SizeOf(Pointer))^, FRangedRecords.List^, SizeOf(Pointer) * (Idx2 - Idx1 + 1));
  end 
  else
    FRangedRecords.Clear;
  FRangeActive:= True;
  { If the dataset is filtered, we must update recreate them after applying range }
  if Filtered then
    CreateFilter;
  InternalFirst;
  Resync([]);
end;

procedure TCustomVirtualDataSet.CancelRange;
begin
  { Destroy the FRangedRecords TList }
  FRangeActive:= False;
  FRangedRecords.Free;
  FRangedRecords:= nil;
  { If the dataset is filtered, we will re-filter completly the dataset }
  if Filtered then
    CreateFilter;
  Resync([]);
end;

procedure TCustomVirtualDataSet.SetRange(const StartValues, EndValues: array of const);
var
  V1, V2: Variant;
  
  function GetValues(A: array of const): Variant;
  var 
    i: Integer;
    V: Variant;
  begin
    Result:= VarArrayCreate([Low(A) - Low(A), High(A) - Low(A)], varVariant);
    for i:= Low(A) to High(A) do
    begin
      case A[i].VType of
      vtInteger:    V:= A[i].VInteger;
      vtBoolean:    V:= A[i].VBoolean;
      vtChar:       V:= A[i].VChar;
      vtExtended:   V:= A[i].VExtended^;
      vtString:     V:= A[i].VString^;
      vtPChar:      V:= string(A[i].VPChar);
//    vtWideChar:   V:= A[i].VWideChar;
//    vtPWideChar:   V:= A[i].VPWideChar^;
      vtAnsiString: V:= string(A[i].VAnsiString);
      vtCurrency:   V:= A[i].VCurrency^;
      vtVariant:    V:= A[i].VVariant^;
      else
        raise Exception.Create(msgIncorrectRangeActiveDataType);
      end;
      Result[i - Low(A)]:= V;
    end;
  end;
    
begin
  CheckBrowseMode;
  { a) Decode Start values }
  V1:= GetValues(StartValues);
  { b) Decode End values }
  V2:= GetValues(EndValues);
  { c) Set range values }
  FStartValues:= V1;
  FEndValues:= V2;
  { d) Apply range }
  ApplyRange;
end;

procedure TCustomVirtualDataSet.AssignRangeActiveRec(Rec: Variant);
var 
  i: Integer;
begin
  for i:= 0 to FIndexFields.Count - 1 do
    if i <= VarArrayHighBound(Rec, 1) then
      TField(FIndexFields[i]).AsVariant:= Rec[i]
    else
      Exit;
end;

function TCustomVirtualDataSet.AcceptRangeActiveRecord(Rec: PChar): Boolean;
var 
  OldState: TDataSetState;
  OldOptions: TIndexOptions;
begin
  OldState:= SetTempState(dsSetKey);
  try
    OldOptions:= FIndexOptions;
    FKeyRec:= AllocRecordBuffer;
    FIndexOptions:= [];
    try
      AssignRangeActiveRec(FStartValues);
      SetIndexCompareOptions;
      Result:= IndexedRecordCompare(FKeyRec, Rec) <= 0;
      AssignRangeActiveRec(FEndValues);
      Result:= Result and (IndexedRecordCompare(FKeyRec, Rec) >= 0);
    finally
      FIndexOptions:= OldOptions;
      FreeRecordBuffer(FKeyRec);
    end;
  finally
    RestoreState(OldState);
  end;
end;

function TCustomVirtualDataSet.FindKey(const KeyValues: array of const): Boolean;
  
  function GetLocateValues(NbFields: Integer; A: array of const): Variant;
  var 
    i: Integer;
    
    function GetAsVar(Idx: Integer): Variant;
    begin
      case A[Idx].VType of
      vtInteger:    Result:= A[Idx].VInteger;
      vtBoolean:    Result:= A[Idx].VBoolean;
      vtChar:       Result:= A[Idx].VChar;
      vtExtended:   Result:= A[Idx].VExtended^;
      vtString:     Result:= A[Idx].VString^;
      vtPChar:      Result:= string(A[Idx].VPChar);
  //  vtWideChar:   V:= A[i].VWideChar;
  //  vtPWideChar:   V:= A[i].VPWideChar^;
      vtAnsiString: Result:= string(A[Idx].VAnsiString);
      vtCurrency:   Result:= A[Idx].VCurrency^;
      vtVariant:    Result:= A[Idx].VVariant^;
      else
        raise Exception.Create(msgIncorrectFindKeyDataType);
      end;
    end;
      
  begin
    if NbFields = 1 then
      Result:= GetAsVar(Low(A))
    else
    begin
      Result:= VarArrayCreate([Low(A) - Low(A), High(A) - Low(A)], varVariant);
      for i:= Low(A) to High(A) do
        Result[i - Low(A)]:= GetAsVar(i);
    end;
  end;
      
var 
  V: Variant;
  L: TList;
  NbFindFields: Integer;
  i: Integer;
  S: string;
  O: TLocateOptions;
begin
  if IndexFieldNames = '' then
  begin
    Result:= False;
    Exit;
  end;
  L:= TList.Create;
  try
    GetFieldList(L, IndexFieldNames);
    NbFindFields:= High(KeyValues) - Low(KeyValues) + 1; // Number of fields to locate
    if NbFindFields = L.Count then
      S:= IndexFieldNames
    else
    begin { Generates the locate field names }
      S:= '';
      for i:= 0 to NbFindFields - 1 do
        S:= S + TField(L[i]).FieldName + ';';
      S:= Copy(S, 1, Length(S) - 1);
    end;
    O:= [];
    if ixCaseInsensitive in IndexOptions then
      O:= [loCaseInsensitive];
    {}
    V:= GetLocateValues(NbFindFields, KeyValues);
    {}
    Result:= Locate(S, V, O);
  finally
    L.Free;
  end;
end;

{ **************************** Maintained Indexs **************************** }

procedure TCustomVirtualDataSet.ActivateIndex(Idx: TMaintainedIndex);
var 
  S: string;
  O: TIndexOptions;
  L1, L2: TList;
begin
  with Idx do
  begin
    S:= IndexFieldNames;
    O:= Options;
    L1:= IndexFields;
    L2:= Records;
    {}
    IndexFieldNames:= FIndexFieldNames;
    Options:= FIndexOptions;
    IndexFields:= FIndexFields;
    Records:= FRecords;
    {}
    FIndexFieldNames:= S;
    FIndexOptions:= O;
    FIndexFields:= L1;
    FRecords:= L2;
    {}
    FMaintained:= True;
  end;
end;

procedure TCustomVirtualDataSet.UseIndex(const IndexFieldNames: string; Options: TIndexOptions;
  const DescFieldNames: string; CanCreate: Boolean);
var 
  i: Integer;
  Idx: TMaintainedIndex;
  CurRec: Pointer;
  OldIndexFieldNames: string;
  OldDescFieldNames: string;
  OldOptions: TIndexOptions;
begin
  if IndexFieldNames = '' then
  begin
    Options:= [];
    FDescFieldNames:= '';
    FDescFields.Clear;
  end;
  if (CompareText(FIndexFieldNames, IndexFieldNames) = 0) and (FIndexOptions = Options) and
    (CompareText(FDescFieldNames, DescFieldNames) = 0) then
    Exit;
  Idx:= nil;
  i:= FMaintainedIndexs.IndexOf(IndexFieldNames, Options, DescFieldNames);
  if i = -1 then
  begin
    if CanCreate then
    begin
      i:= 0;
      AddIndex(IndexFieldNames, Options, DescFieldNames);
      Idx:= FMaintainedIndexs[i];
    end 
    else
    begin // Look for a compatible index (same case-sensivity, same order (asc, desc)
      i:= FMaintainedIndexs.IndexOfCompatibleIndex(IndexFieldNames, Options, DescFieldNames);
      if i = -1 then
      begin // Prepare the creation of the new index
        if Active then
        begin
          CheckIndexFields(IndexFieldNames);
          CheckIndexFields(DescFieldNames);
          FIndexFields.Clear;
          GetFieldList(FIndexFields, IndexFieldNames);
          FDescFields.Clear;
          GetFieldList(FDescFields, DescFieldNames);
        end;
        OldIndexFieldNames:= FIndexFieldNames;
        OldOptions:= FIndexOptions;
        OldDescFieldNames:= FDescFieldNames;
        FIndexFieldNames:= IndexFieldNames;
        FIndexOptions:= Options;
        FDescFieldNames:= DescFieldNames;
      end 
      else
        Idx:= FMaintainedIndexs[i];
    end;
  end 
  else
    Idx:= FMaintainedIndexs[i];
  if Active then
  begin
    CurRec:= nil;
    if CurrentRecords.Count <> 0 then
      CurRec:= CurrentRecords[FCurRec];
    SetIndexCompareOptions;
    if i = -1 then
    begin
      if (FRecords.Count > 0) and (FIndexFields.Count > 0) then
      try
        if FMaintained then
        begin
          Idx:= TMaintainedIndex.Create;
          Idx.IndexFieldNames:= OldIndexFieldNames;
          Idx.Options:= OldOptions;
          Idx.DescFieldNAmes:= OldDescFieldNames;
          Idx.IndexFields.Count:= FRecords.Count;
          Move(FRecords.List^, Idx.IndexFields.List^, FRecords.Count * SizeOf(Pointer));
          GetFieldList(Idx.IndexFields, OldIndexFieldNames);
          GetFieldList(Idx.DescFields, OldDescFieldNames);
        end;
        SortCurrentRecords;
        FMaintained:= False;
      except
        if FMaintained then
          Idx.Free;
        FIndexFieldNames:= OldIndexFieldNames;
        FIndexOptions:= OldOptions;
        FDescFieldNames:= OldDescFieldNames;
        FIndexFields.Clear;
        GetFieldList(FIndexFields, IndexFieldNames);
        GetFieldList(FDescFields, DescFieldNames);
        raise;
      end;
    end 
    else
      ActivateIndex(Idx);
    if RangeActive then
    begin
      if IndexFieldNames <> '' then
        ApplyRange
      else
        CancelRange;
    end 
    else if Filtered then
      CreateFilter;
    if CurrentRecords.Count <> 0 then
      FCurRec:= CurrentRecords.IndexOf(CurRec);
    Resync([]);
  end;
end;

procedure TCustomVirtualDataSet.AddIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string);
var 
  i: Integer;
  Idx: TMaintainedIndex;
begin
  if IndexFieldNames = '' then
  begin
    Options:= [];
    DescFieldNames:= '';
  end;
  i:= FMaintainedIndexs.IndexOf(IndexFieldNames, Options, DescFieldNames);
  if i = -1 then
  begin
    Idx:= TMaintainedIndex.Create;
    FMaintainedIndexs.Insert(0, Idx);
    try
      Idx:= FMaintainedIndexs[0];
      CheckIndexFields(IndexFieldNames);
      CheckIndexFields(DescFieldNames);
      Idx.IndexFields.Clear;
      GetFieldList(Idx.IndexFields, IndexFieldNames);
      Idx.DescFields.Clear;
      GetFieldList(Idx.DescFields, DescFieldNames);
      Idx.IndexFieldNames:= IndexFieldNames;
      Idx.Options:= Options;
      if Active then
      begin
        Idx.Records.Count:= FRecords.Count;
        Move(FRecords.List^, Idx.Records.List^, SizeOf(Pointer) * FRecords.Count);
        ActivateIndex(Idx);
        try
          SetIndexCompareOptions;
          if FRecords.Count > 0 then
            SortCurrentRecords;
        finally
          ActivateIndex(Idx);
        end;
      end;
    except
      FMaintainedIndexs.FreeItem(0);
      raise;
    end;
  end;
end;

procedure TCustomVirtualDataSet.DropIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string);
var 
  i: Integer;
begin
  if IndexFieldNames = '' then
  begin
    Options:= [];
    DescFieldNames:= '';
  end;
  i:= FMaintainedIndexs.IndexOf(IndexFieldNames, Options, DescFieldNames);
  if i <> -1 then
    FMaintainedIndexs.FreeItem(i);
end;

function TCustomVirtualDataSet.GetIndexCount: Integer;
begin
  Result:= FMaintainedIndexs.Count;
end;

function TCustomVirtualDataSet.GetIndexDef(Idx: Integer): TIndexDef;
begin
  with Result do
  begin
    IndexFieldNames:= TMaintainedIndex(FMaintainedIndexs[Idx]).IndexFieldNames;
    IndexOptions:= TMaintainedIndex(FMaintainedIndexs[Idx]).Options;
  end;
end;

{ ********************************* Filters ******************************** }

type
  TFilterEvaluator = class(TEvaluator)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TFieldVariable = class(TExpression)
  private
    FField: TField;
  protected
    function GetExprType: TExpressionType; override;
    {}
    function GetAsBoolean: Boolean; override;
    function GetAsInteger: Integer; override;
    function GetAsFloat: Extended; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
  public
    constructor CreateField(Field: TField);
  end;

  TFilterCompareOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
    function StrCompare(Expr1, Expr2: TExpression): Integer;
  end;

  TEqualOperator = class(TFilterCompareOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TDifferentOperator = class(TFilterCompareOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TSuperiorOperator = class(TFilterCompareOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TInferiorOperator = class(TFilterCompareOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TSuperiorEqualOperator = class(TFilterCompareOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TInferiorEqualOperator = class(TFilterCompareOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TFilterLogicOperator = class(TOperator)
  public
    function OperationResult(Type1, Type2: TExpressionType): TExpressionType; override;
  end;

  TAndOperator = class(TFilterLogicOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

  TOrOperator = class(TFilterLogicOperator)
  public
    procedure Operate(Expr1, Expr2: TExpression; var Result: TExpression); override;
  end;

function TCustomVirtualDataSet.AcceptFilterRecord(Rec: PChar): Boolean;
var 
  OldState: TDataSetState;
begin
  OldState:= SetTempState(dsFilter);
  try
    FKeyRec:= Rec;
    Result:= FFilterEvaluator.Result.AsBoolean;
    if Assigned(OnFilterRecord) then
      OnFilterRecord(Self, Result);
  finally
    RestoreState(OldState);
  end;
end;

procedure TCustomVirtualDataSet.CreateFilter;
var 
  i: Integer;
  OldState: TDataSetState;
  Accept: Boolean;
  Records: TList;
begin
  if not Assigned(FFilteredRecords) then
    FFilteredRecords:= TList.Create;
  if RangeActive then
    Records:= FRangedRecords
  else
    Records:= FRecords;
  FFilteredRecords.Clear;
  FFilteredRecords.Capacity:= Records.Count;
  if (Filter = '') and not Assigned(OnFilterRecord) then
  begin
    FFilteredRecords.Count:= Records.Count;
    Move(Records.List^, FFilteredRecords.List^, Records.Count * SizeOf(Pointer));
  end 
  else
  begin
    OldState:= SetTempState(dsFilter);
    try
      for i:= 0 to Records.Count - 1 do
      begin
        FKeyRec:= Records[i];
        Accept:= FFilterEvaluator.Result.AsBoolean;
        if Assigned(OnFilterRecord) then
          OnFilterRecord(Self, Accept);
        if Accept then
          FFilteredRecords.Add(FKeyRec);
      end;
    finally
      RestoreState(OldState);
    end;
    FFilteredRecords.Capacity:= FFilteredRecords.Count;
  end;
end;

procedure TCustomVirtualDataSet.DecodeFilter;
var 
  S: string;
begin
  FFilterEvaluator:= TFilterEvaluator.Create(Self);
  try
    S:= Trim(Filter);
    if S = '' then
      S:= EvalIntf.STrue;
    FFilterEvaluator.Evaluate(S);
  except
    FFilterEvaluator.Free;
    FFilterEvaluator:= nil;
    raise;
  end;
end;

procedure TCustomVirtualDataSet.SetFiltered(Value: Boolean);
begin
  if Value <> Filtered then
  begin
    if Active then
    begin
      CheckBrowseMode;
      if Value then
        DecodeFilter
      else
      begin
        FFilteredRecords.Free;
        FFilteredRecords:= nil;
        FFilterEvaluator.Free;
        FFilterEvaluator:= nil;
      end;
    end;
    inherited SetFiltered(Value);
    if Active then
    begin
      if Filtered then
        CreateFilter;
      First;
    end;
  end;
end;

procedure TCustomVirtualDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  if Value <> FilterOptions then
  begin
    if Active and Filtered then
      CheckBrowseMode;
    inherited SetFilterOptions(Value);
    if Active and Filtered then
    begin
      CreateFilter;
      First;
    end;
  end;
end;

procedure TCustomVirtualDataSet.SetFilterText(const Value: string);
var 
  FOldEvaluator: TEvaluator;
  FOldFilter: string;
begin
  if Value <> Filter then
  begin
    if Active and Filtered then
    begin
      CheckBrowseMode;
      FOldEvaluator:= FFilterEvaluator;
      FOldFilter:= Filter;
      inherited SetFilterText(Trim(Value));
      try
        DecodeFilter;
        FOldEvaluator.Free; // if successful, we destroy the old filter
      except
        // it there was an error, then the filter must not change
        FFilterEvaluator:= FOldEvaluator;
        inherited SetFilterText(FOldFilter);
        raise;
      end;
    end 
    else
      inherited SetFilterText(Trim(Value));
    if Active and Filtered then
    begin
      CreateFilter;
      First;
    end;
  end;
end;

procedure TCustomVirtualDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if (TMethod(Value).Code <> TMethod(OnFilterRecord).Code) or
     (TMethod(Value).Data <> TMethod(OnFilterRecord).Data) then
  begin
    if Active and Filtered then
      CheckBrowseMode;
    inherited SetOnFilterRecord(Value);
    if Active and Filtered then
    begin
      CreateFilter;
      First;
    end;
  end;
end;

function TCustomVirtualDataSet.GetFilterCompareOptions(var Value: string): TCompareOptions;
begin // Used by internally declared operators (TEqualOperator, ...)
  Result:= [];
  if Value <> '' then
  begin
    if Value[Length(Value)] = ExtendedFilterChar then
    begin
      System.Delete(Value, Length(Value), 1);
      Include(Result, coBeginningBy);
    end;
    if (Length(Value) > 0) and (Value[1] = ExtendedFilterChar) then
    begin
      System.Delete(Value, 1, 1);
      Include(Result, coEndingBy);
    end;
  end;
  if Result = [coBeginningBy, coEndingBy] then
    Result:= [coContaining];
end;

{ ****************************** Filter classes ****************************** }

constructor TFilterEvaluator.Create(AOwner: TComponent);
var 
  i: Integer;
  DataSet: TDataSet;
begin
  DataSet:= TDataSet(AOwner);
  inherited Create(AOwner);
  {}
  RegisterOperator('=', opLow, TEqualOperator);
  RegisterOperator('<>', opLow, TDifferentOperator);
  RegisterOperator('>', opLow, TSuperiorOperator);
  RegisterOperator('<', opLow, TInferiorOperator);
  RegisterOperator('>=', opLow, TSuperiorEqualOperator);
  RegisterOperator('<=', opLow, TInferiorEqualOperator);
  RegisterOperator('AND', opLowest, TAndOperator);
  RegisterOperator('OR', opLowest, TOrOperator);
  RegisterVariable('NULL', TExpression.CreateVariant(Null));
  RegisterVariable(STrue, TExpression.CreateBoolean(True));
  RegisterVariable(SFalse, TExpression.CreateBoolean(False));
  {}
  for i:= 0 to DataSet.FieldCount - 1 do
    if DataSet.Fields[i].FieldKind in [fkData, fkInternalCalc] then
    begin
      RegisterVariable(DataSet.Fields[i].FieldName,
                       TFieldVariable.CreateField(DataSet.Fields[i]));
    end;
end;

constructor TFieldVariable.CreateField(Field: TField);
begin
  inherited Create(etError);
  FField:= Field;
end;

function TFieldVariable.GetExprType: TExpressionType;
begin
  case FField.DataType of
  ftString, ftMemo: Result:= etString;
  ftWord, ftSmallint, ftAutoInc, ftInteger: Result:= etInteger;
  ftBoolean: Result:= etBoolean;
  ftDate, ftTime, ftDateTime, ftFloat, ftCurrency: Result:= etFloat;
  else
    Result:= etError;
  end;
end;

function TFieldVariable.GetAsBoolean: Boolean;
begin
  Result:= FField.AsBoolean;
end;

function TFieldVariable.GetAsInteger: Integer;
begin
  Result:= FField.AsInteger;
end;

function TFieldVariable.GetAsFloat: Extended;
begin
  Result:= FField.AsFloat;
end;

function TFieldVariable.GetAsString: string;
begin
  Result:= FField.AsString;
end;

function TFieldVariable.GetAsVariant: Variant;
begin
  Result:= FField.AsVariant;
end;

function TFilterCompareOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  Result:= etBoolean;
end;

function TFilterCompareOperator.StrCompare(Expr1, Expr2: TExpression): Integer;
var 
  DataSet: TCustomVirtualDataSet;
  FilterOptions: TFilterOptions;
  Options: TCompareOptions;
  S: string;
begin
  Options:= [];
  DataSet:= TCustomVirtualDataSet(TEvaluator(GetOwner).Owner);
  FilterOptions:= DataSet.FilterOptions;
  if foCaseInsensitive in FilterOptions then
    Include(Options, coCaseInsensitive);
  if (Expr2.ExprType = etString) and (Expr2.ClassType = TExpression) and
     (not (foNoPartialCompare in FilterOptions)) then
  begin
    { Note about Expr2.ClassType = TExpression :
        if Expr2 is not a simple TExpression (a constant string value),
        then Partial comparison should be fully ignored }
    S:= Expr2.AsString;
    Options:= Options + DataSet.GetFilterCompareOptions(S);
    Result:= DataSet.CompareStrings(PChar(Expr1.AsString), PChar(S), Options);
  end else
    Result:= DataSet.CompareStrings(PChar(Expr1.AsString), PChar(Expr2.AsString), Options);
end;

procedure TEqualOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if (Expr1.ExprType = etString) and (Expr2.ExprType = etString) then
    Result.AsBoolean:= StrCompare(Expr1, Expr2) = 0
  else
    Result.AsVariant:= Expr1.AsVariant = Expr2.AsVariant;
end;

procedure TDifferentOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if (Expr1.ExprType = etString) and (Expr2.ExprType = etString) then
    Result.AsBoolean:= StrCompare(Expr1, Expr2) <> 0
  else
    Result.AsVariant:= Expr1.AsVariant <> Expr2.AsVariant;
end;

procedure TSuperiorOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if (Expr1.ExprType = etString) and (Expr2.ExprType = etString) then
    Result.AsBoolean:= StrCompare(Expr1, Expr2) > 0
  else
    Result.AsVariant:= Expr1.AsVariant > Expr2.AsVariant;
end;

procedure TInferiorOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if (Expr1.ExprType = etString) and (Expr2.ExprType = etString) then
    Result.AsBoolean:= StrCompare(Expr1, Expr2) < 0
  else
    Result.AsVariant:= Expr1.AsVariant < Expr2.AsVariant;
end;

procedure TSuperiorEqualOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if (Expr1.ExprType = etString) and (Expr2.ExprType = etString) then
    Result.AsBoolean:= StrCompare(Expr1, Expr2) >= 0
  else
    Result.AsVariant:= Expr1.AsVariant >= Expr2.AsVariant;
end;

procedure TInferiorEqualOperator.Operate(Expr1, Expr2: TExpression; var Result: TExpression);
begin
  if (Expr1.ExprType = etString) and (Expr2.ExprType = etString) then
    Result.AsBoolean:= StrCompare(Expr1, Expr2) <= 0
  else
    Result.AsVariant:= Expr1.AsVariant <= Expr2.AsVariant;
end;

function TFilterLogicOperator.OperationResult(Type1, Type2: TExpressionType): TExpressionType;
begin
  if (Type1 = etBoolean) and (Type2 = etBoolean) then
    Result:= etBoolean
  else if (Type1 = etInteger) and (Type2 = etInteger) then
    Result:= etInteger
  else
    Result:= inherited OperationResult(Type1, Type2);
end;

procedure TAndOperator.Operate(Expr1, Expr2: TExpression;
  var Result: TExpression);
var 
  T1, T2: TExpressionType;
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

procedure TOrOperator.Operate(Expr1, Expr2: TExpression;
  var Result: TExpression);
var 
  T1, T2: TExpressionType;
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

procedure TCustomVirtualDataSet.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

function TCustomVirtualDataSet.GetCanModify: Boolean;
begin
  Result := not ReadOnly;
end;

{ TBlobStream }

constructor TBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FPosition:= 0;
  FMode:= Mode;
  FField:= Field;
  FDataSet:= Field.DataSet as TCustomVirtualDataSet;
  FAssocBuffer:= FDataSet.GetActiveBuffer;
  if Mode = bmWrite then
    SetLength(FDataSet.GetBlobString(FField, FAssocBuffer)^, 0); // Set size to 0
end;

destructor TBlobStream.Destroy;
begin
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  finally
    inherited Destroy;
  end;
end;

function TBlobStream.GetBlobSize: Longint;
begin
  if FAssocBuffer = nil then
    Result:= 0
  else
    Result := Length(FDataSet.GetBlobString(FField, FAssocBuffer)^);
end;

function TBlobStream.Read(var Buffer; Count: Longint): Longint;
var
  P: PString;
begin
  P:= FDataSet.GetBlobString(FField, FAssocBuffer);
  Result:= Min(Count, Length(P^));
  Move(P^[Position + 1], Buffer, Result);
  Inc(FPosition, Result);
end;

function TBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
  0: FPosition := Offset;
  1: Inc(FPosition, Offset);
  2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

function TBlobStream.Write(const Buffer; Count: LongInt): LongInt;
var
  P: PString;
begin
  P:= FDataSet.GetBlobString(FField, FAssocBuffer);
  SetLength(P^, Length(P^) + Count);
  Move(Buffer, P^[Position + 1], Count);
  Result:= Count;
  Inc(FPosition, Result);
  FModified:= True;
end;

{ ***************************** TMasterDataLink ****************************** }

procedure TMasterDataLink.ActiveChanged;
begin
  GetFields;
  if not (csDestroying in FDataSet.ComponentState) then
    TVirtualDataSet(FDataSet).MasterDetailChanged;
end;

procedure TMasterDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;

constructor TMasterDataLink.Create(DataSet: TDataSet);
begin
  inherited Create;
  FDataSet:= DataSet;
  FFields:= TList.Create;
end;

destructor TMasterDataLink.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TMasterDataLink.GetFields;
begin
  FFields.Clear;
  if Active then
    try
      DataSet.GetFieldList(FFields, FFieldNames);
    except
      FFields.Clear;
      raise;
    end;
end;

function TMasterDataLink.GetLinkActive: Boolean;
begin
  Result:= (FFields.Count > 0) and Active;
end;

procedure TMasterDataLink.LayoutChanged;
begin
  ActiveChanged;
end;

procedure TMasterDataLink.RecordChanged(Field: TField);
begin
  if (DataSource.State <> dsSetKey) and FDataSet.Active and
    (FFields.Count > 0) and ((Field = nil) or (FFields.IndexOf(Field) >= 0)) then
      TVirtualDataSet(FDataSet).MasterDetailChanged;
end;

procedure TMasterDataLink.SetFieldNames(const Value: string);
begin
  if FFieldNames <> Value then
  begin
    FFieldNames:= Value;
    ActiveChanged;
  end;
end;

{ ****************************** TIndexList ****************************** }

destructor TIndexList.Destroy;
begin
  while Count > 0 do
    FreeItem(Count - 1);
  inherited Destroy;
end;

procedure TIndexList.FreeItem(Idx: Integer);
begin
  TMaintainedIndex(Items[Idx]).Free;
  Delete(Idx);
end;

function TIndexList.IndexOf(IndexFieldNames: string; IndexOptions: TIndexOptions; DescFieldNames: string): Integer;
var 
  i: Integer;
  Index: TMaintainedIndex;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
  begin
    Index:= Items[i];
    if (CompareText(Index.IndexFieldNames, IndexFieldNames) = 0) and
       (Index.Options = IndexOptions) and
       (CompareText(Index.DescFieldNames, DescFieldNames) = 0) then
    begin
      Result:= i;
      Exit;
    end;
  end;
end;

function TIndexList.IndexOfCompatibleIndex(IndexFieldNames: string;
  IndexOptions: TIndexOptions; DescFieldNames: string): Integer;
const CompatibilityMask = [ixCaseInsensitive, ixDescending];
var 
  i: Integer;
  Index: TMaintainedIndex;
begin
  IndexOptions:= IndexOptions * CompatibilityMask;
  Result:= -1;
  for i:= 0 to Count - 1 do
  begin
    Index:= Items[i];
    if (CompareText(Index.IndexFieldNames, IndexFieldNames) = 0) and
       (Index.Options * CompatibilityMask = IndexOptions) and
       (CompareText(Index.DescFieldNames, DescFieldNames) = 0) then
    begin
      Result:= i;
      Exit;
    end;
  end;
end;

{ ****************************** TMaintainedIndex ****************************** }

constructor TMaintainedIndex.Create;
begin
  Records:= TList.Create;
  IndexFields:= TList.Create;
  DescFields:= TList.Create;
end;

destructor TMaintainedIndex.Destroy;
begin
  Records.Free;
  IndexFields.Free;
  DescFields.Free;
  //
  inherited Destroy;
end;

{ TCustomVirtualDataSet }

{ ******************************* CachedUpdates ****************************** }

constructor TCachedUpdate.Create(DataSet: TCustomVirtualDataSet);
begin
  OldData:= DataSet.CreateRecord(DataSet.FRecordSize);
  NewData:= DataSet.CreateRecord(DataSet.FRecordSize);
  {}
  FDataSet:= DataSet;
end;

destructor TCachedUpdate.Destroy;
begin
  FDataSet.FreeRecord(OldData);
  FDataSet.FreeRecord(NewData);
  {}
  inherited Destroy;
end;

procedure TCustomVirtualDataSet.AddCachedUpdate(UpdateKind: TUpdateKind;
  OldRec, NewRec: PChar);
var 
  Update: TCachedUpdate;
  L, L2: TList;
  i: Integer;
begin
  if FApplyingUpdates then
    Exit;
  if UpdateKind = ukModify then
  begin // Check if anything was modified
    L:= TList.Create;
    L2:= TList.Create;
    try
      for i:= 0 to FieldCount - 1 do
        if Fields[i].FieldKind = fkData then
          L.Add(Fields[i]);
      if CompareFields(OldRec, NewRec, L, [], L2) = 0 then
        Exit;
    finally
      L2.Free;
      L.Free;
    end;
  end;
  Update:= TCachedUpdate.Create(Self);
  Update.UpdateKind:= UpdateKind;
  if UpdateKind = ukModify then
    Update.RecId:= OldRec
  else
    Update.RecId:= NewRec;
  CopyRecords(OldRec, Update.OldData);
  CopyRecords(NewRec, Update.NewData);
  Update.OldPos:= FCurRec;
  {}
  FUpdateList.Add(Update);
end;

procedure TCustomVirtualDataSet.SetCachedUpdates(const Value: Boolean);
begin
  if FCachedUpdates <> Value then
  begin
    FCachedUpdates:= Value;
    {}
    if Value then
      FUpdateList:= TList.Create
    else
    begin
      CancelUpdates;
      FUpdateList.Free;
      FUpdateList:= nil;
    end;
  end;
end;

procedure TCustomVirtualDataSet.ApplyUpdates;
  
  procedure AssignDataSetValues(DataSet: TCustomVirtualDataSet; Values: PChar);
  var 
    P: PChar;
  begin
    DataSet.Edit;
    DataSet.Post;
    P:= DataSet.FRecords[0];
    CopyRecords(Values, P);
    DataSet.Refresh;
  end;
  
  procedure CheckModifiedFields(OldRec, NewRec: PChar; L: TList);
  var 
    i: Integer;
    TL, TL2: TList;
  begin
    TL:= TList.Create;
    TL2:= TList.Create;
    try
      L.Clear;
      for i:= 0 to FieldCount - 1 do
        if Fields[i].FieldKind = fkData then
        begin
          TL.Clear;
          TL.Add(Fields[i]);
          if CompareFields(OldRec, NewRec, TL, [], TL2) <> 0 then
            L.Add(Fields[i]);
        end;
    finally
      TL2.Free;
      TL.Free;
    end;
   end;
var
  Update: TCachedUpdate;
  OldV, NewV: TUpdateDataSet;
  ModifiedFieldList: TList;
  i: Integer;
  //OldRecNo : Integer;
  //S: string; // !!!
begin
  if CachedUpdates then
  begin
    FApplyingUpdates := True;
    try
      //OldRecNo := RecNo;
      DisableControls;
      try
        if FUpdateList.Count > 0 then
        begin
          OldV:= TUpdateDataSet.Create(nil);
          OldV.FReferenceDataset := Self;
          NewV:= TUpdateDataSet.Create(nil);
          NewV.FReferenceDataset := Self;
          ModifiedFieldList:= TList.Create;
          try
            OldV.ImportFieldDefs(Self);
            NewV.ImportFieldDefs(Self);
            OldV.Open;
            NewV.Open;
            for i:= 0 to OldV.FieldCount - 1 do
            begin
              OldV.Fields[i].Required:= False;
              NewV.Fields[i].Required:= False;
            end;
            { Calls the UpdateRecord function and destroy all updates }
            for i:= 0 to FUpdateList.Count - 1 do
            begin
              Update:= FUpdateList[i];
              AssignDataSetValues(OldV, Update.OldData);
              AssignDataSetValues(NewV, Update.NewData);
              CheckModifiedFields(Update.OldData, Update.NewData, ModifiedFieldList);
              if Update.UpdateKind = ukInsert then
              begin
                UpdateRecord(Update, NewV, OldV, ModifiedFieldList);
              end
              else
              begin
                UpdateRecord(Update, OldV, NewV, ModifiedFieldList);
              end;
            end;
          finally
            ModifiedFieldList.Free;
            NewV.Free;
            OldV.Free;
          end;
        end;
        //RecNo := OldRecNo;
      finally
        EnableControls;
      end;
    finally
      FApplyingUpdates := False;
    end;
  end;
end;

procedure TCustomVirtualDataSet.CommitUpdates;
begin
  if CachedUpdates then
    while FUpdateList.Count > 0 do
    begin
      TObject(FUpdateList[0]).Free;
      FUpdateList.Delete(0);
    end;
end;

procedure TCustomVirtualDataSet.CancelUpdates;
var 
  i: Integer;
begin
  if CachedUpdates then
  begin
    if State in dsWriteModes then
      Cancel;
    if FUpdateList.Count > 0 then
    begin
      for i:= FUpdateList.Count - 1 downto 0 do
      begin
        try // !!! A revoir ??????????
          CancelRecordUpdate(FUpdateList[i]);
          TObject(FUpdateList[i]).Free;
        except
        end;
        FUpdateList.Delete(i);
      end;
      {}
      Resync([]);
    end;
  end;
end;

procedure TCustomVirtualDataSet.UpdateRecord(Update: TCachedUpdate;
  OldValues, NewValues: TDataSet; ModifiedFields: TList);
begin
  DoUpdateRecord(Update.UpdateKind, OldValues, NewValues, ModifiedFields);
end;

procedure TCustomVirtualDataSet.CancelRecordUpdate(Update: Pointer);
var 
  Upd: TCachedUpdate;
  P: PChar;
  
  procedure RemoveFromList(L: TList; P: Pointer);
  var 
    i: Integer;
  begin
    i:= L.IndexOf(P);
    if i <> -1 then
      L.Delete(i);
  end;
    
begin
  Upd:= Update;
  case Upd.UpdateKind of
  ukInsert: // We delete it
    DoDeleteRecord(Upd.RecId, CurrentRecords.IndexOf(Upd.RecId));
  ukDelete: // We add it
  begin
    P:= CreateRecord(FRecordSize);
    CopyRecords(Upd.OldData, P);
    DoAddRecord(P, Upd.OldPos);
  end;
  ukModify: // We reset its value to its old value
    DoModifyRecord(Upd.RecId, Upd.OldData, CurrentRecords.IndexOf(Upd.RecId));
  end;
end;

procedure TCustomVirtualDataSet.DoUpdateRecord(UpdateKind: TUpdateKind;
  OldValues, NewValues: TDataSet; ModifiedFields: TList);
begin
  { Nothing at this level => Updates have already been applied }
  { Just call the event }
  if Assigned(FOnUpdateRecord) then
    FOnUpdateRecord(Self, UpdateKind, OldValues, NewValues, ModifiedFields);
end;

function TCustomVirtualDataSet.GetUpdatePendings: Boolean;
begin
  if Assigned(FUpdateList) then
    Result:= FUpdateList.Count > 0
  else
    Result:= False;
end;

function TCustomVirtualDataSet.InternalMoveIndexedRecord(CurRec, NewRec: PChar;
  Records: TList; Idx: Integer): Integer;
begin
  // Note : in dsEdit, with every indexs, we check if the key values have
  // changed before trying to change its position in the index
  SetIndexCompareOptions;
  if (Records.Count <> 1) and (CompareFields(CurRec, NewRec, FIndexFields,
    FCompareOptions, FDescFields) <> 0) and (IndexFieldNames <> '') then
  begin
    Records.Delete(Idx);
    SearchInList(Records, NewRec, Idx, not (ixUnique in FIndexOptions), False);
    Records.Insert(Abs(Idx), CurRec);
  end;
  Result:= Abs(Idx);
end;

procedure TCustomVirtualDataSet.InternalUpdateIndexs(CurRec, NewRec: PChar;
    State: TDataSetState);
var 
  i: Integer;
  Index: TMaintainedIndex;
begin
  for i:= 0 to FMaintainedIndexs.Count - 1 do
  begin
    Index:= FMaintainedIndexs[i];
    ActivateIndex(Index);
    if State = dsEdit then
      InternalMoveIndexedRecord(CurRec, NewRec, FRecords, FRecords.IndexOf(CurRec))
    else
      InternalDoAddRecord(NewRec, FRecords, FRecords.Count);
    ActivateIndex(Index);
  end;
end;

function TCustomVirtualDataSet.InternalDoAddRecord(Rec: PChar; Records: TList; Idx: Integer): Integer;
begin
  if IsIndexed and (Records.Count > 0) and (IndexFieldNames <> '') then
  begin
    SetIndexCompareOptions;
    SearchInList(Records, Rec, Result, not (ixUnique in FIndexOptions), False);
    Result:= Abs(Result);
  end 
  else
    Result:= Idx;
  if (Records.Count > 0) and (Result <> Records.Count) then
    Records.Insert(Result, Rec)
  else
    Records.Add(Rec);
end;

function TCustomVirtualDataSet.DoAddRecord(Rec: PChar; Pos: Integer): Integer;
begin
  if RangeActive then
  begin
    InternalDoAddRecord(Rec, FRecords, FRecords.Count);
    if AcceptRangeActiveRecord(Rec) then
    begin
      Result:= InternalDoAddRecord(Rec, FRangedRecords, FRangedRecords.Count);
      if Filtered then
      begin
        if AcceptFilterRecord(Rec) then
          Result:= InternalDoAddRecord(Rec, FFilteredRecords, FFilteredRecords.Count)
        else
          Result:= Pos;
      end;
    end else
      Result:= Pos;
  end 
  else if Filtered then
  begin
    InternalDoAddRecord(Rec, FRecords, FRecords.Count);
    if AcceptFilterRecord(Rec) then
      Result:= InternalDoAddRecord(Rec, FFilteredRecords, Pos)
    else
      Result:= Pos;
  end 
  else
    Result:= InternalDoAddRecord(Rec, CurrentRecords, Pos);
  if IsIndexed then
    InternalUpdateIndexs(Rec, Rec, dsInsert);
end;

function TCustomVirtualDataSet.DoModifyRecord(CurRec: PChar; NewRec: PChar;
  RecIdx: Integer): Integer;
begin
  if IsIndexed then
  begin
    Result:= InternalMoveIndexedRecord(CurRec, NewRec, CurrentRecords, RecIdx);
    if RangeActive then
    begin
      InternalMoveIndexedRecord(CurRec, NewRec, FRecords, FRecords.IndexOf(CurRec));
      if Filtered then // because Records = FFilteredRecords
        InternalMoveIndexedRecord(CurRec, NewRec, FRangedRecords, FRangedRecords.IndexOf(CurRec));
    end 
    else if Filtered then
      InternalMoveIndexedRecord(CurRec, NewRec, FRecords, FRecords.IndexOf(CurRec));
    InternalUpdateIndexs(CurRec, NewRec, dsEdit);
  end 
  else
    Result:= RecIdx;
  {}
  CopyRecords(NewRec, CurRec);
  {}
  if RangeActive and (not AcceptRangeActiveRecord(CurRec)) then
  begin
    FRangedRecords.Remove(CurRec);
    if Filtered then
      FFilteredRecords.Remove(CurRec);
  end 
  else if Filtered and (not AcceptFilterRecord(CurRec)) then
    FFilteredRecords.Remove(CurRec);
end;

procedure TCustomVirtualDataSet.DoDeleteRecord(Rec: PChar; RecIdx: Integer);
  
  procedure RemoveRecordFromList(Records: TList); forward;
  procedure ListDelete(List: TList; Idx: Integer); forward;
  procedure ListRemove(List: TList; Rec: Pointer); forward;
  
  procedure RemoveFromMaintainedIndexs;
  var 
    i: Integer;
    Index: TMaintainedIndex;
  begin
    for i:= 0 to FMaintainedIndexs.Count - 1 do
    begin
      Index:= FMaintainedIndexs[i];
      if Index.IndexFieldNames <> '' then
      begin
        ActivateIndex(Index);
        RemoveRecordFromList(FRecords);
        ActivateIndex(Index);
      end else
        ListRemove(Index.Records, Rec);
    end;
  end;
      
  procedure RemoveRecordFromList(Records: TList);
  var 
    Idx: Integer;
  begin
    SetIndexCompareOptions;
    SearchInList(Records, Rec, Idx, not (ixUnique in FIndexOptions), False);
    if (not (ixUnique in FIndexOptions)) and (Rec <> Records[Idx]) then
      repeat
        Inc(Idx);
      until Records[Idx] = Rec;
    ListDelete(Records, Idx);
  end;
    
  procedure ListDelete(List: TList; Idx: Integer);
  begin
    if Idx <> -1 then
      List.Delete(Idx);
  end;
    
  procedure ListRemove(List: TList; Rec: Pointer);
  begin
    ListDelete(List, List.IndexOf(Rec));
  end;
    
begin
  RemoveFromMaintainedIndexs;
  if IndexFieldNames <> '' then
  begin
    SetIndexCompareOptions;
    if Filtered then
    begin
      ListDelete(FFilteredRecords, RecIdx);
      if RangeActive then
        RemoveRecordFromList(FRangedRecords);
      RemoveRecordFromList(FRecords);
    end 
    else if RangeActive then
    begin
      ListDelete(FRangedRecords, RecIdx);
      RemoveRecordFromList(FRecords);
    end 
    else
      ListDelete(FRecords, RecIdx);
  end 
  else
  begin
    if Filtered then
    begin
      ListDelete(FFilteredRecords, RecIdx);
      if RangeActive then
        ListRemove(FRangedRecords, Rec);
      ListRemove(FRecords, Rec);
    end 
    else if RangeActive then
    begin
      ListDelete(FRangedRecords, RecIdx);
      ListRemove(FRecords, Rec);
    end 
    else
      ListDelete(FRecords, RecIdx);
  end;
  { Destroys physically the record }
  FreeRecord(Rec);
  {}
  if FCurRec >= CurrentRecords.Count then
    Dec(FCurRec);
end;

{}
{
function TCustomVirtualDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    SaveState := FState;
    FState := State;
    try
      Result := Field.AsVariant;
    finally
      FState := SaveState;
    end;
  end else
    Result := NULL;
end;

procedure TCustomVirtualDataSet.SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant);
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind <> fkData then Exit;
  SaveState := FState;
  FState := State;
  try
    Field.AsVariant := Value;
  finally
    FState := SaveState;
  end;
end;
}

{ TDataList }

constructor TDataList.Create(Structure: array of string);
var
  i: Integer;
begin
  FData:= TListDataSet.Create(nil);
  {}
  for i:= Low(Structure) to High(Structure) do
    FData.FieldDefs.Add(Structure[i], ftString, dsMaxStringSize, False);
  FData.Open;
  FData.Filtered:= True;
end;

destructor TDataList.Destroy;
begin
  FData.Free;
  {}
  inherited Destroy;
end;

procedure TDataList.Clear;
begin
  FData.EmptyTable;
end;

procedure TDataList.Add(Values: array of Variant);
var 
  i: Integer;
begin
  if (High(Values) - Low(Values)) >= FData.FieldCount then
    raise Exception.Create('Trop de données en entrée');
  FData.Append;
  FData.Post;
  for i:= Low(Values) to High(Values) do
    FData.Fields[i].AsVariant:= Values[i];
end;

procedure TDataList.Delete(Idx: Integer);
begin
  CheckBounds(Idx);
  {}
  FData.RecNo:= Idx + 1;
  FData.Delete;
end;

function TDataList.IndexOf(const KeyFields: string;
  const KeyValues: array of Variant; Options: TLocateOptions): Integer;
var 
  V: Variant;
begin
  if Low(KeyValues) = High(KeyValues) then
    V:= KeyValues[Low(KeyValues)]
  else
    V:= VarArrayOf(KeyValues);
  if FData.Locate(KeyFields, V, Options) then
    Result:= FData.RecNo - 1
  else
    Result:= -1;
end;

function TDataList.FindKey(const KeyValues: array of const): Integer;
begin
  if FData.FindKey(KeyValues) then
    Result:= FData.RecNo - 1
  else
    Result:= -1;
end;

procedure TDataList.AddIndex(const IndexFieldNames: string;
  Options: TIndexOptions);
begin
  FData.AddIndex(IndexFieldNames, Options, '');
end;

procedure TDataList.SetRange(const StartValues, EndValues: array of const);
begin
  FData.SetRange(StartValues, EndValues);
end;

procedure TDataList.CancelRange;
begin
  FData.CancelRange;
end;

function TDataList.GetCount: Integer;
begin
  Result:= FData.RecordCount;
end;

function TDataList.GetFilter: string;
begin
  Result:= FData.Filter;
end;

procedure TDataList.SetFilter(const Value: string);
begin
  FData.Filter:= Value;
end;

function TDataList.GetIndexFieldNames: string;
begin
  Result:= FData.IndexFieldNames;
end;

procedure TDataList.SetIndexFieldNames(const Value: string);
begin
  FData.IndexFieldNames:= Value;
end;

function TDataList.GetIndexOptions: TIndexOptions;
begin
  Result:= FData.IndexOptions;
end;

procedure TDataList.SetIndexOptions(const Value: TIndexOptions);
begin
  FData.IndexOptions:= Value;
end;

function TDataList.GetItemIndex: Integer;
begin
  Result:= FData.RecNo - 1;
end;

procedure TDataList.SetItemIndex(const Value: Integer);
begin
  CheckBounds(Value);
  FData.RecNo:= Value + 1;
end;

function TDataList.GetRangeActive: Boolean;
begin
  Result:= FData.RangeActive;
end;

function TDataList.GetRawCount: Integer;
begin
  Result:= FData.FRecords.Count;
end;

function TDataList.GetField(RecIdx, FieldIdx: Integer): TField;
begin
  Result:= FData.Fields[FieldIdx];
  ItemIndex:= RecIdx;
end;

function TDataList.GetValue(Idx: Integer; FieldName: string): TField;
begin
  Result:= FData.FieldByName(FieldName);
  ItemIndex:= Idx;
end;

procedure TDataList.CheckBounds(Idx: Integer);
begin
  if (Idx < 0) and (Idx >= FData.RecordCount) then
    raise Exception.CreateFmt('Index de liste (%d) incorrect !', [Idx]);
end;

{ TListDataSet }

procedure TListDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var 
  AutoEdit: Boolean;
begin
  AutoEdit:= not (State in [dsSetKey, dsFilter, dsEdit, dsInsert]);
  if AutoEdit then
    Edit;
  inherited SetFieldData(Field, Buffer);
  if AutoEdit then
    Post;
end;

{ TVirtualDataSet }

procedure TVirtualDataSet.AddIndex(const IndexFieldNames: string; Options: TIndexOptions; DescFieldNames: string);
begin
  inherited;
end;

procedure TVirtualDataSet.ApplyRange;
begin
  inherited;
end;

procedure TVirtualDataSet.ApplyUpdates;
begin
  inherited;
end;

procedure TVirtualDataSet.CancelRange;
begin
  inherited;
end;

procedure TVirtualDataSet.CancelUpdates;
begin
  inherited;
end;

procedure TVirtualDataSet.CommitUpdates;
begin
  inherited;
end;

constructor TVirtualDataSet.Create(AOwner: TComponent);
begin
  FMasterLink:= TMasterDataLink.Create(Self);
  inherited Create(AOwner);
end;

destructor TVirtualDataSet.Destroy;
begin
  inherited Destroy;
  FMasterLink.Free;
end;

procedure TVirtualDataSet.DropIndex(const IndexFieldNames: string;
  Options: TIndexOptions; DescFieldNames: string);
begin
  inherited;
end;

function TVirtualDataSet.GetMasterFields: string;
begin
  Result:= TMasterDataLink(FMasterLink).FieldNames;
end;

function TVirtualDataSet.GetMasterSource: TDataSource;
begin
  Result:= FMasterLink.DataSource;
end;

procedure TVirtualDataSet.ImportCurrentRecord(DataSet: TDataSet);
begin
  inherited;
end;

procedure TVirtualDataSet.ImportDataSet(DataSet: TDataSet);
begin
  CheckActive;
  inherited;
end;

procedure TVirtualDataSet.ImportFieldDefs(DataSet: TDataSet);
begin
  inherited;
end;

function TVirtualDataSet.IndexLookup(const Index: Integer; const ResultFields: string): Variant;
var
  i : Integer;
  Fields: TList;
  FOldState : TDataSetState;
begin
  Result:= Null;
  if (Index > -1) and (Index < CurrentRecords.Count) then
  begin
    FOldState := SetTempState(dsSetKey);
    try
      FKeyRec:= CurrentRecords[Index];
      try
        Fields:= TList.Create;
        try
          //CheckIndexFields(ResultFields);
          GetFieldList(Fields, ResultFields);
          if Fields.Count = 1 then
            Result:= TField(Fields[0]).AsVariant
          else
          begin
            Result:= VarArrayCreate([0, Fields.Count - 1], varVariant);
            for i := 0 to Fields.Count - 1 do
              Result[i]:= TField(Fields[i]).AsVariant;
          end;
        finally
          Fields.Free;
        end;
      finally
        FKeyRec:= nil;
      end;
    finally
      RestoreState(FOldState);
    end;
  end;
end;

procedure TVirtualDataSet.LoadFromFile(const FileName: string);
begin
  inherited;
end;

procedure TVirtualDataSet.LoadFromStream(Stream: TStream);
begin
  inherited;
end;

procedure TVirtualDataSet.MasterDetailChanged;
var 
  Range: Variant;
  i: Integer;
  Fields: TList;
begin
  if Active then
  begin
    if TMasterDataLink(FMasterLink).LinkActive then
    begin
      Fields:= TMasterDataLink(FMasterLink).FFields;
      Range:= VarArrayCreate([0, Fields.Count - 1], varVariant);
      for i:= 0 to Fields.Count - 1 do
        Range[i]:= TField(Fields[i]).AsVariant;
      FStartValues:= Range;
      FEndValues:= Range;
      ApplyRange;
    end 
    else
      CancelRange;
  end;
end;

procedure TVirtualDataSet.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (Operation = opRemove) and (Component = MasterSource) then
    MasterSource:= nil;
end;

procedure TVirtualDataSet.SetMasterFields(const Value: string);
begin
  TMasterDataLink(FMasterLink).FieldNames:= Value;
end;

procedure TVirtualDataSet.SetMasterSource(const Value: TDataSource);
begin
  FMasterLink.DataSource:= Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TVirtualDataSet.SetRange(const StartValues, EndValues: array of const);
begin
  inherited;
end;

procedure TVirtualDataSet.UseIndex(const IndexFieldNames: string;
  Options: TIndexOptions; const DescFieldNames: string;
  CanCreate: Boolean);
begin
  inherited;
end;

{ TUpdateDataSet }

procedure TUpdateDataSet.ImportStructure(ADataset : TCustomVirtualDataset);
var
  i: Integer;
begin
  FFieldPos.Clear;
  for i := 0 to ADataset.FFieldPos.Count - 1 do
    FFieldPos.Add(ADataset.FFieldPos[i]);
  FBlobsPos.Clear;
  for i := 0 to ADataset.FBlobsPos.Count - 1 do
    FBlobsPos.Add(ADataset.FBlobsPos[i]);
  SetLength(FFieldIndex, Length(ADataset.FFieldIndex));
  for i := Low(ADataset.FFieldIndex) to High(ADataset.FFieldIndex) do
    FFieldIndex[i] := ADataset.FFieldIndex[i];
  FRecordSize:= ADataset.FRecordSize;
  FBuffBlobInit.Clear;
  for i := 0 to ADataset.FBuffBlobInit.Count - 1 do
    FBuffBlobInit.Add(ADataset.FBuffBlobInit[i]);
end;

procedure TUpdateDataSet.InitStructure;
begin
  ImportStructure(FReferenceDataset);
end;

end.
