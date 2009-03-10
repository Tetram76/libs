unit GzSumInfo;

{$I GrizzlyDefine.INC}

interface

uses SysUtils, Classes, Windows, Messages, ActiveX, ComObj, Contnrs,
  GzClasses, Dialogs;

const
  FMTID_SummaryInformation : TGUID =    '{F29F85E0-4FF9-1068-AB91-08002B27B3D9}';
  FMTID_DocSummaryInformation : TGUID = '{D5CDD502-2E9C-101B-9397-08002B2CF9AE}';
  FMTID_UserDefinedProperties : TGUID = '{D5CDD505-2E9C-101B-9397-08002B2CF9AE}';
  FMTID_SystemInformation : TGUID =     '{b725f130-47ef-101a-a5f1-02608c9eebac}';

  StringPropertyMode = 0; {0 pour VT_LPSTR et 1 pour VT_LPWSTR}
  StreamVersion : Longint = 0;

  StreamReadError = 'ERREUR DE LECTURE DU FLUX';

type
  TCustomFileInformation = class;

  TFileClass = class of TCustomFileInformation;

  TCustomFileProperty = class;

  TPropertyKind = (pkSystem, pkSumInfo, pkStoredData, pkFreeData);

  TPropertyType = (ptString, ptDateTime, ptBoolean, ptInteger, ptLargeInt);

  TSysKindProperty = (skFileName, skFileDate, skFileType, skFileSize, skFileOnly, skDirOnly);

  TGUIDList = class(TStringList)
  private
    function GetGUID(Index: Integer): TGUID;
  protected
  public
    procedure AddGUID(AGUID : TGUID);
    property GUID[Index : Integer] : TGUID read GetGUID;
  end;

  TFileClassGUIDList = class(TStringList)
  private
    function GetGUIDList(Index: Integer): TGUIDList;
    function GetGUIDList2(AClass: TFileClass): TGUIDList;
  protected
  public
    procedure AddList(AClass : TFileClass);
    procedure AddGUID(AClass : TFileClass; AGUID : TGUID);
    property GUIDListByIndex[Index : Integer] : TGUIDList read GetGUIDList;
    property GUIDListByClass[AClass : TFileClass] : TGUIDList read GetGUIDList2;
  end;

  TFilePropertyParams = class
  private
    FIndex: Integer;
    FWidth: Integer;
    FTitle: string;
    FName: string;
    FAlignment: TAlignment;
    FPropertyIndex: Cardinal;
    FPropertyType: TPropertyType;
    FPropertyKind: TPropertyKind;
    FGUID: TGUID;
    procedure SetTitle(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
  public
    constructor Create(AFileInfo : TCustomFileInformation; AProperty : TCustomFileProperty);
    property Name : string read FName;
    property Title : string read FTitle write SetTitle;
    property ColWidth : Integer read FWidth write FWidth;
    property Alignment : TAlignment read FAlignment write SetAlignment;
    property Index : Integer read FIndex;
    property PropertyIndex : Cardinal read FPropertyIndex;
    property PropertyType : TPropertyType read FPropertyType;
    property PropertyKind : TPropertyKind read FPropertyKind;
    property GUID : TGUID read FGUID;
  end;

  TCustomFileProperty = class
  private
    FFilePropertyParams : TFilePropertyParams;
    FParent: TCustomFileInformation;
    FName: string;
    FTitle: string;
    FPropertyIndex: Cardinal;
    FPropertyKind: TPropertyKind;
    FSysType: TSysKindProperty;
    FGUID: TGUID;
    function GetFilePropertyParams: TFilePropertyParams;
    function GetTitle: string;
  protected
    FPropertyType: TPropertyType;
    procedure InternalCreate; virtual;
    function GetAsBoolean: Boolean; virtual; abstract;
    function GetAsDateTime: TDateTime; virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    function GetAsLargeInt: Int64; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetAsVariant: Variant; virtual; abstract;
    procedure SetAsBoolean(const Value: Boolean); virtual; abstract;
    procedure SetAsDateTime(const Value: TDateTime); virtual; abstract;
    procedure SetAsInteger(const Value: Integer); virtual; abstract;
    procedure SetAsLargeInt(const Value: Int64); virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    procedure SetAsVariant(const Value: Variant); virtual; abstract;
  public
    property AsString : string read GetAsString write SetAsString;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt : Int64 read GetAsLargeInt write SetAsLargeInt;
    property Value : Variant read GetAsVariant write SetAsVariant;

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    property Parent : TCustomFileInformation read FParent;
    procedure Clear; virtual; abstract;

    property Name : string read FName;
    property Title : string read GetTitle write FTitle;

    //Défini s'il s'agit d'une propriété système, suminfo, ou libre...
    property Kind : TPropertyKind read FPropertyKind;
    //Défini le type de donnée stockée
    property PropertyType : TPropertyType read FPropertyType;
    //Pour les propriétés de "kind" pkSumInfo
    property PropertyIndex : Cardinal read FPropertyIndex;
    property GUID : TGUID read FGUID;
    procedure ReadProp(const AProp : TPropVariant);
    procedure WriteProp(out AProp: TPropVariant);
    //Pour les propriétés de "kind" pkSystem
    property SysType : TSysKindProperty read FSysType;

    property PropertyParams : TFilePropertyParams read GetFilePropertyParams;

    constructor Create(AParent : TCustomFileInformation; AName : string; AGUID : TGUID; APropertyIndex : Cardinal); overload;
    constructor Create(AParent : TCustomFileInformation; AName : string; ASysType : TSysKindProperty); overload;
    constructor Create(AParent : TCustomFileInformation; AName : string; AStored : Boolean; APropertyIndex : Cardinal = 0); overload;
    constructor Create(AParent : TCustomFileInformation; ASysType : TSysKindProperty = skFileName); overload;
    constructor Create; overload;
  end;

  TFileStringProperty = class(TCustomFileProperty)
  private
    FValue: string;
  protected
    procedure InternalCreate; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsLargeInt(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure Clear; override;
  end;

  TFileBooleanProperty = class(TCustomFileProperty)
  private
    FValue: Boolean;
  protected
    procedure InternalCreate; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsLargeInt(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure Clear; override;
  end;

  TFileDateTimeProperty = class(TCustomFileProperty)
  private
    FValue: TDateTime;
  protected
    procedure InternalCreate; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsLargeInt(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure Clear; override;
  end;

  TFileIntegerProperty = class(TCustomFileProperty)
  private
    FValue: Integer;
  protected
    procedure InternalCreate; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsLargeInt(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure Clear; override;
  end;

  TFileLargeIntProperty = class(TCustomFileProperty)
  private
    FValue: Int64;
  protected
    procedure InternalCreate; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Int64; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsBoolean(const Value: Boolean); override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsLargeInt(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure Clear; override;
  end;

  TPropertyList = class(TObjectList)
  private
    function GetItems(Index: Integer): TCustomFileProperty;
    procedure SetItems(Index: Integer; const Value: TCustomFileProperty);
  public
    {Méthodes de liste}
    procedure Add(Item : TCustomFileProperty);
    procedure Insert(Index : Integer; Item : TCustomFileProperty);
    property Items[Index : Integer] : TCustomFileProperty read GetItems write SetItems; default;
    function Extract(Item: TCustomFileProperty): TCustomFileProperty;
    function Remove(Item: TCustomFileProperty): Integer;
    function IndexOf(Item: TCustomFileProperty): Integer;
    function First: TCustomFileProperty;
    function Last: TCustomFileProperty;

    function FindByName(AName : string) : TCustomFileProperty;
  end;

  TFileDates = record
    Created, Changed, Accessed  : TDateTime;
  end;

  TCustomFileInformation = class
  private
    FFileName: TFileName;
    FReadError: Boolean;

    function GetFileName: TFileName;
    function GetValues(Index: Integer): Variant;
    procedure SetFileName(const Value: TFileName);
    procedure SetValues(Index: Integer; const Value: Variant);
    function GetProperties(Index: Integer): TCustomFileProperty;
    function GetPropertyCount: Integer;
    function GetPropByName(Name: string): TCustomFileProperty;
    function GetFileDate: TDateTime;
    procedure SetFileDate(const Value: TDateTime);
    function GetFileSize_: Int64;
    procedure SetFileSize(const Value: Int64);
  protected
    FPropertyList : TPropertyList;
    FSysPropsOk : Boolean;
    FFileSize : Int64;
    FModified: Boolean;
    FLoaded: Boolean;
    FFileDates: TFileDates;
    FLastWrite: TDateTime;
    FLastErrorMessage: string;
    procedure AddProperty(AProperty : TCustomFileProperty);
    procedure CreateProperties; virtual;
    function GetStringProperty(const Index: Integer): TFileStringProperty;
    function GetBooleanProperty(const Index: Integer): TFileBooleanProperty;
    function GetDateTimeProperty(const Index: Integer): TFileDateTimeProperty;
    function GetIntegerProperty(const Index: Integer): TFileIntegerProperty;
    function GetLargeIntProperty(const Index: Integer): TFileLargeIntProperty;
    function GetStreamName : string; virtual;
  public
    constructor Create(const AFileName : TFileName; AutoLoad : Boolean = True; ASearchRec : Pointer = nil);
    destructor Destroy; override;

    procedure Read; virtual;
    procedure Write; virtual;
    procedure Clear; virtual;
    function CleanStream : Boolean;

    property Modified : Boolean read FModified;

    property FileName : TFileName read GetFileName write SetFileName;
    property FileDate : TDateTime read GetFileDate write SetFileDate;
    property FileSize : Int64 read GetFileSize_ write SetFileSize;

    { Pour accéder aux propriétés personnalisées }
    property Values[Index : Integer] : Variant read GetValues write SetValues;
    property Properties[Index : Integer] : TCustomFileProperty read GetProperties;
    property PropByName[Name : string] : TCustomFileProperty read GetPropByName;
    property PropertyCount : Integer read GetPropertyCount;

    { Pour accéder à d'autres propriétés }
    property LastWrite : TDateTime read FLastWrite;
    property FileDates : TFileDates read FFileDates;
    property ReadError : Boolean read FReadError;
    property ReadErrorMessage : string read FLastErrorMessage;

    property Loaded : Boolean read FLoaded;

    procedure CheckLoaded;

    procedure Assign(AItem : TCustomFileInformation; OnlyProperties : Boolean = False);
  end;

  TSystemFileInformation = class(TCustomFileInformation)
  protected
    procedure CreateProperties; override;
  public
    //Attention, propriété n°0 = FFileName...
    //property FileDate : TFileDateTimeProperty index 1 read GetDateTimeProperty;
    property FileType : TFileStringProperty index 2 read GetStringProperty;
    //property FileSize : TFileStringProperty index 3 read GetStringProperty;
  end;

const
  SystemPropertyIndex : array[TSysKindProperty] of Integer = (10, 14, 0, 12, 10, 11);


function GetFilePropertyParams(AFileInfo : TCustomFileInformation; const AName : string) : TFilePropertyParams; overload;
function GetFilePropertyParams(AFileClass : TFileClass; const AName : string) : TFilePropertyParams; overload;

procedure GetFilePropertyNames(AClassName : string; ALst : TStrings);

var
  WM_PROPERTYPARAMCHANGED : Cardinal;
  RestoreFileDate : Boolean;

procedure CheckPropertyParamList(AFileClass : TFileClass);

function GetWideChar(const AString : string) : PWideChar;

function StgOpenStorageEx (
    const pwcsName : POleStr;  //Pointer to the path of the
                              //file containing storage object
    grfMode : LongInt;         //Specifies the access mode for the object
    stgfmt : DWORD;            //Specifies the storage file format
    grfAttrs : DWORD;          //Reserved; must be zero
    pStgOptions : Pointer;     //Address of STGOPTIONS pointer
    reserved2 : Pointer;       //Reserved; must be zero
    riid : PGUID;              //Specifies the GUID of the interface pointer
    out stgOpen :              //Address of an interface pointer
    IStorage ) : HResult; stdcall;

const
  IID_IPropertySetStorage : TGUID =     '{0000013A-0000-0000-C000-000000000046}';

  STGFMT_FILE = 3;
  STGFMT_ANY = 4;

implementation

uses FGUtils, FGFiles, {$IFDEF GZ_D6}DateUtils,{$ENDIF} Math, {$IFDEF GZ_D7}Variants,{$ENDIF} ShellAPI, AFiles, ASysUtil;

var
  FFilePropertyParamsList : TStringList;
  FFileClassGUIDList : TFileClassGUIDList;
  FFileTypeCacheList : TDictionnary;

function GetWideChar(const AString : string) : PWideChar;
var
  T : Integer;
  AWideString : WideString;
begin
  T := SizeOf(WideChar) * (Length(AString) + 1);
  GetMem(Result, T);
  FillChar(Result^, T, 0);
  AWideString := AString;
  Move(PWideChar(AWideString)^, Result^, T - 1);
end;

function GetAnsiChar(const AString : string) : PAnsiChar;
var
  T : Integer;
begin
  T := SizeOf(AnsiChar) * (Length(AString) + 1);
  GetMem(Result, T);
  FillChar(Result^, T, 0);
  Move(PAnsiChar(AString)^, Result^, T - 1);
end;

procedure CheckPropertyParamList(AFileClass : TFileClass);
begin
  with AFileClass.Create('', False) do
    Free;
end;

function GetParamSearchString(AFileInfo : TCustomFileInformation; const AName : string) : string; overload;
begin
  Result := AFileInfo.ClassName + '|' + AnsiUpperCase(AName);
end;

function GetParamSearchString(AFileClass : TFileClass; const AName : string) : string; overload
begin
  Result := AFileClass.ClassName + '|' + AnsiUpperCase(AName);
end;

function GetFilePropertyParams(AFileInfo : TCustomFileInformation; const AName : string) : TFilePropertyParams;
var
  k : Integer;
begin
  if FFilePropertyParamsList.Find(GetParamSearchString(AFileInfo, AName), k) then
    Result := TFilePropertyParams(FFilePropertyParamsList.Objects[k])
  else
    Result := nil;
end;

function GetFilePropertyParams(AFileClass : TFileClass; const AName : string) : TFilePropertyParams;
var
  k : Integer;
begin
  if FFilePropertyParamsList.Find(GetParamSearchString(AFileClass, AName), k) then
    Result := TFilePropertyParams(FFilePropertyParamsList.Objects[k])
  else
    Result := nil;
end;

procedure GetFilePropertyNames(AClassName : string; ALst : TStrings);
var
  i : Integer;
begin
  ALst.Clear;
  for i := 0 to FFilePropertyParamsList.Count - 1 do
  begin
    if Copy(FFilePropertyParamsList[i], 1, Length(AClassName)) = AClassName then
      ALst.Add(AnsiUpperCase(TFilePropertyParams(FFilePropertyParamsList.Objects[i]).Name));
  end;
end;

function StgOpenStorageEx (
    const pwcsName : POleStr;  //Pointer to the path of the
                              //file containing storage object
    grfMode : LongInt;         //Specifies the access mode for the object
    stgfmt : DWORD;            //Specifies the storage file format
    grfAttrs : DWORD;          //Reserved; must be zero
    pStgOptions : Pointer;     //Address of STGOPTIONS pointer
    reserved2 : Pointer;       //Reserved; must be zero
    riid : PGUID;              //Specifies the GUID of the interface pointer
    out stgOpen :              //Address of an interface pointer
    IStorage ) : HResult; stdcall; external 'ole32.dll';


{ TCustomFileInformation }

procedure TCustomFileInformation.AddProperty(AProperty : TCustomFileProperty);
begin
  FPropertyList.Add(AProperty);
  GetFilePropertyParams(Self, AProperty.Name).FIndex := FPropertyList.IndexOf(AProperty);
end;

procedure TCustomFileInformation.Assign(AItem: TCustomFileInformation; OnlyProperties : Boolean = False);
var
  i : Integer;
begin
  FLoaded := AItem.Loaded;

  if not OnlyProperties then
    FFileName := AItem.FileName;

  if (Self.ClassName = AItem.ClassName) or (AItem is Self.ClassType) then
  begin
    for i := 0 to FPropertyList.Count - 1 do
      FPropertyList[i].Value := AItem.FPropertyList[i].Value;
  end
  else
  if Self is AItem.ClassType then
  begin
    for i := 0 to AItem.FPropertyList.Count - 1 do
      FPropertyList[i].Value := AItem.FPropertyList[i].Value;
  end;
end;

procedure TCustomFileInformation.CheckLoaded;
begin
  if not FLoaded then
    Read;
end;

function TCustomFileInformation.CleanStream : Boolean;
var
  AFileName : string;
begin
  AFileName := FileName + ':' + GetStreamName;
  Result := DeleteFile(PAnsiChar(AFileName));
end;

procedure TCustomFileInformation.Clear;
var
  i : Integer;
begin
  FModified := False;
  FSysPropsOk := False;
  FFileSize := 0;
  FLoaded := False;
  FillChar(FFileDates, SizeOf(FFileDates), 0);
  FLastWrite := 0;
  for i := 0 to FPropertyList.Count - 1 do
    FPropertyList[i].Clear;
end;

constructor TCustomFileInformation.Create(const AFileName: TFileName; AutoLoad : Boolean = True; ASearchRec : Pointer = nil);
var
  ASR : TSearchRec;
begin
  inherited Create;

  FLastErrorMessage := '';
  FReadError := False;

  FPropertyList := TPropertyList.Create(True);
  FLoaded := False;

  if Assigned(ASearchRec) then
  begin
    ASR := TSearchRec(ASearchRec^);
    FFileDates.Created := FileTimeToLocalDateTime(ASR.FindData.ftCreationTime);
    FFileDates.Changed := FileTimeToLocalDateTime(ASR.FindData.ftLastWriteTime);
    FFileDates.Accessed := FileTimeToLocalDateTime(ASR.FindData.ftLastAccessTime);
    FFileSize := ASR.Size;
    FSysPropsOk := True;
  end
  else
  begin
    FillChar(FFileDates, SizeOf(FFileDates), 0);
    FFileSize := 0;
    FSysPropsOk := False;
  end;

  CreateProperties;

  if AutoLoad then
    SetFileName(AFileName)
  else
    FFileName := AFileName;
end;

procedure TCustomFileInformation.CreateProperties;
begin
  //Vide par défaut...
end;

destructor TCustomFileInformation.Destroy;
begin
  FPropertyList.Free;
  inherited;
end;

function TCustomFileInformation.GetBooleanProperty(const Index: Integer): TFileBooleanProperty;
begin
  Result := TFileBooleanProperty(Properties[Index]);
end;

function TCustomFileInformation.GetDateTimeProperty(const Index: Integer): TFileDateTimeProperty;
begin
  Result := TFileDateTimeProperty(Properties[Index]);
end;

function TCustomFileInformation.GetFileDate: TDateTime;
begin
  Result := FFileDates.Changed;
end;

function TCustomFileInformation.GetFileName: TFileName;
begin
  Result := FFileName;
end;

function TCustomFileInformation.GetFileSize_: Int64;
begin
  Result := FFileSize;
end;

function TCustomFileInformation.GetIntegerProperty(const Index: Integer): TFileIntegerProperty;
begin
  Result := TFileIntegerProperty(Properties[Index]);
end;

function TCustomFileInformation.GetLargeIntProperty(const Index: Integer): TFileLargeIntProperty;
begin
  Result := TFileLargeIntProperty(Properties[Index]);
end;

function TCustomFileInformation.GetPropByName(Name: string): TCustomFileProperty;
var
  APropParam : TFilePropertyParams;
begin
  APropParam := GetFilePropertyParams(Self, Name);
  if Assigned(APropParam) then
    Result := Properties[APropParam.Index]
  else
    Result := nil;
end;

function TCustomFileInformation.GetProperties(Index: Integer): TCustomFileProperty;
begin
  if Index in [0..FPropertyList.Count - 1] then
    Result := FPropertyList[Index]
  else
    Result := nil;
end;

function TCustomFileInformation.GetPropertyCount: Integer;
begin
  Result := FPropertyList.Count;
end;

function TCustomFileInformation.GetStreamName: string;
begin
  Result := ClassName;
end;

function TCustomFileInformation.GetStringProperty(const Index: Integer): TFileStringProperty;
begin
  Result := TFileStringProperty(Properties[Index]);
end;

function TCustomFileInformation.GetValues(Index: Integer): Variant;
begin
  if Index in [0..FPropertyList.Count - 1] then
    Result := FPropertyList[Index].Value
  else
  if Index = -1 then
    Result := FFileName
  else
    Result := '';
end;

procedure TCustomFileInformation.Read;
var
  AGUIDList : TGUIDList;

  m : Integer;
  NOK : Boolean;

  Stg: IStorage;
{$IFDEF GZ_D10}
  AFileAge: TDateTime;
  AFileSize: Integer;
{$ELSE}
  AFileAge : Integer;
  AFileSize : Int64;
{$ENDIF}
  AProperty : TCustomFileProperty;
  AErrorMsg : string;

  function GetFileType : string;
  var
    AFileInfo : TSHFileInfo;
    AExtension : string;
  begin
    AExtension := AnsiLowerCase(ExtractFileExt(FFileName));
    Result := FFileTypeCacheList.Items[AExtension];
    if Result = '' then
    begin
      if SHGetFileInfo(PChar(FFileName), FILE_ATTRIBUTE_NORMAL, AFileInfo,
        SizeOf(AFileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_TYPENAME) > 0 then
      begin
        Result := AFileInfo.szTypeName;
        if Result <> '' then
          FFileTypeCacheList.Items[AExtension] := Result;
      end
      else
        Result := '';
    end;
  end;

  procedure ReadSysProps;
  var
    ASR : TSearchRec;
    ALI : LARGE_INTEGER;
  begin
    if SysUtils.FindFirst(FFileName, faAnyFile, ASR) = 0 then
    begin
      try
        ALI.LowPart := ASR.FindData.nFileSizeLow;
        ALI.HighPart := ASR.FindData.nFileSizeHigh;
        FFileSize := ALI.QuadPart;
        FFileDates.Created := FileTimeToLocalDateTime(ASR.FindData.ftCreationTime);
        FFileDates.Changed := FileTimeToLocalDateTime(ASR.FindData.ftLastWriteTime);
        FFileDates.Accessed := FileTimeToLocalDateTime(ASR.FindData.ftLastAccessTime);
      finally
        SysUtils.FindClose(ASR);
      end;
    end;
  end;

  function InitRootStorage : Boolean;
  var
    AFileName : WideString;
    Err : HResult;
  begin
    Result := False;
    AFileName := FFileName;
    Err := StgOpenStorageEx(PWideChar(AFileName), STGM_READ or STGM_SHARE_EXCLUSIVE, STGFMT_ANY,
                    0, nil,  nil, @IID_IPropertySetStorage, stg);

    if Err <> S_OK then
      Exit;

    Result := True;
  end;

  function ReadGUIDProps(AGUID : TGUID) : Boolean;
  var
    PropSetStg: IPropertySetStorage;
    PropStg: IPropertyStorage;
    PropSpec: array of TPropSpec;
    PropVariant: array of TPropVariant;
    NbSI, i, k : Integer;
    AOK : Boolean;
    function InitStorage : Boolean;
    var
      Err : HResult;
    begin
      AOK := False;
      Result := False;

      PropSetStg := Stg as IPropertySetStorage;

      Err := PropSetStg.Open(AGUID, STGM_READ or STGM_SHARE_EXCLUSIVE, PropStg);

      if (Err = STG_E_FILENOTFOUND) then
      begin
        AOK := True;
        Exit;
      end
      else
      if (Err = S_OK) then
        Result := True
      else
      begin
        AErrorMsg := SysErrorMessage(Err);
      end;
    end;
  begin
    NbSI := 0;
    for i := 0 to FPropertyList.Count - 1 do
    begin
      AProperty := FPropertyList[i];
      if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AProperty.GUID, AGUID)) then
        Inc(NbSI)
    end;

    if NbSI > 0 then
    begin
      if not InitStorage then
      begin
        Result := AOK;
        if not AOK then
          for i := 0 to FPropertyList.Count - 1 do
          begin
            AProperty := FPropertyList[i];
            if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AProperty.GUID, AGUID)) then
            begin
              AProperty.AsString := StreamReadError;
//              Inc(k);
            end;
          end;
        Exit;
      end;

      SetLength(PropSpec, NbSI);
      SetLength(PropVariant, NbSI);

      k := 0;
      for i := 0 to FPropertyList.Count - 1 do
      begin
        AProperty := FPropertyList[i];
        if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AProperty.GUID, AGUID)) then
        begin
          PropSpec[k].ulKind := PRSPEC_PROPID;
          PropSpec[k].propid := AProperty.PropertyIndex;
          Inc(k);
        end;
      end;

      OleCheck(PropStg.ReadMultiple(NbSI, @PropSpec[0], @PropVariant[0]));

      k := 0;
      for i := 0 to FPropertyList.Count - 1 do
      begin
        AProperty := FPropertyList[i];
        if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AProperty.GUID, AGUID)) then
        begin
          AProperty.ReadProp(PropVariant[k]);
          Inc(k);
        end;
      end;
    end;
    Result := True;
  end;
  function ReadStream : Boolean;
  var
    AFS : TFileStream;
    i, k, V : Integer;
    AProperty : TCustomFileProperty;
    AType : TPropertyType;
    AName : string;
  begin
    Result := True;
    if StreamExists(FileName, GetStreamName) then
    begin
      AFS := TFileStream.Create(FileName + ':' + GetStreamName, fmOpenRead or fmShareDenyWrite);
      try
        if AFS.Size = 0 then
          Exit;
        //Version du Stream
        V := TUsefulStream(AFS).ReadLongInt;
        if V > StreamVersion then
          raise EFileError.Create('Version de "stream" trop récente');
        //Date d'écriture
        FLastWrite := TUsefulStream(AFS).ReadDateTime;
        //Nombre de propriétés enregistrées... 1) On les compte 2) On les enregistre
        k := TUsefulStream(AFS).ReadLongInt;
        for i := 0 to k - 1 do
        begin
          AName := TUsefulStream(AFS).ReadString;
          AProperty := FPropertyList.FindByName(AName);
          if Assigned(AProperty) and (AProperty.Kind = pkStoredData) then
            AProperty.LoadFromStream(AFS)
          else
          begin
            //on ne fait pas d'erreur, on se contente de lire les données et
            //de les oublier...
            AFS.Read(AType, SizeOf(AType));
            case AType of
            ptString : TUsefulStream(AFS).ReadString;
            ptDateTime : TUsefulStream(AFS).ReadDateTime;
            ptBoolean, ptInteger  : TUsefulStream(AFS).ReadLongInt;
            ptLargeInt : TUsefulStream(AFS).ReadLargeInt;
            end;
          end;
        end;
      finally
        AFS.Free;
      end;
    end;
  end;
begin
  AErrorMsg := '';
  Clear;
  if not FileExists(FFileName) then
  begin
    FLoaded := True;
    Exit;
  end;
  {
  1) Compter les propriétés SumInfo
  2) Créer le tableau si >0
  3) Lire le tableau
  4) Lire les autres propriétés si nécessaire (au moment du comptage...)
  }
  try
    if not FSysPropsOk then
      ReadSysProps;
    {1 On lit les données autres que les pkSumInfo...}
    for m := 0 to FPropertyList.Count - 1 do
    begin
      AProperty := FPropertyList[m];
      if (AProperty.Kind = pkSystem) then
      begin
        {4)}
        case AProperty.SysType of
        skFileName : AProperty.AsString := FileName;
        skFileDate :
          begin
            if FileDate <> 0 then
              AProperty.AsDateTime := FileDate
            else
            begin
            {$IFDEF GZ_D10}
              if FileAge(FFileName, AFileAge) then
                AProperty.AsDateTime := AFileAge
              else
                Exit;
            {$ELSE}
              AFileAge := FileAge(FFileName);
              if AFileAge <> -1 then
                AProperty.AsDateTime := FileDateToDateTime(AFileAge)
              else
                Exit; //On sort car on a un verrouillage...
            {$ENDIF}
            end;
          end;
        skFileType : AProperty.AsString := GetFileType;
        skFileSize :
          begin
            if FileSize <> 0 then
              AProperty.AsLargeInt := FileSize
            else
            begin
              AFileSize := GetFileSize(FileName);
              if AFileSize <> -1 then
                AProperty.AsLargeInt := AFileSize
              else
                Exit; //On sort car on a un verrouillage...
            end;
          end;
        skFileOnly : AProperty.AsString := ExtractFileName(FileName);
        skDirOnly : AProperty.AsString := ExtractFilePath(FileName);
        end;
      end
      else
        AProperty.Clear;
    end;
  
    {2 On lit les propriétés pkStoredData}
    ReadStream;

    {3 On lit les propriétés pkSumInfo GUID par GUID}
    NOK := False;
    AGUIDList := FFileClassGUIDList.GUIDListByClass[TFileClass(Self.ClassType)];
    if Assigned(AGUIDList) and (AGUIDList.Count > 0) then
      if InitRootStorage then
        for m := 0 to AGUIDList.Count - 1 do
          if not ReadGUIDProps(AGUIDList.GUID[m]) then
            NOK := True;
  except
    on E : Exception do
    begin
      NOK := False;
      AErrorMsg := E.Message + ' (' + E.ClassName + ')';
    end;
  end;

  if NOK then
  begin
    FLastErrorMessage := AErrorMsg;
    FReadError := True;
    Exit;
  end;

  FLoaded := True;
end;

procedure TCustomFileInformation.SetFileDate(const Value: TDateTime);
begin
  FFileDates.Changed := Value;
end;

procedure TCustomFileInformation.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  FSysPropsOk := False;
  if FileExists(Value) then
    Read;
end;

procedure TCustomFileInformation.SetFileSize(const Value: Int64);
begin
  FFileSize := Value;
end;

procedure TCustomFileInformation.SetValues(Index: Integer; const Value: Variant);
begin
  if Index in [0..FPropertyList.Count - 1] then
    FPropertyList[Index].Value := Value
end;

procedure TCustomFileInformation.Write;
var
{$IFDEF GZ_D10}
  FOldAge: TDateTime;
  FNewAge: TDateTime;
{$ELSE}
  FOldAge : Integer;
{$ENDIF}

  procedure WriteSumInfo;
  var
    Stg: IStorage;
    i : Integer;
    AGUIDList : TGUIDList;
    function InitRootStorage : Boolean;
    var
      AFileName : WideString;
      Err : HResult;
      Nb : Integer;
    begin
      AFileName := FFileName;
      Nb := 0;
      repeat
        Err := StgOpenStorageEx(PWideChar(AFileName),
          STGM_SHARE_EXCLUSIVE or STGM_READWRITE,
          STGFMT_ANY,
          0, nil,  nil, @IID_IPropertySetStorage, stg);
        if (Nb < 5) and ((Err = STG_E_LOCKVIOLATION) or (Err = STG_E_SHAREVIOLATION)) then
        begin
          Inc(Nb);
          Sleep(100);
        end
        else
        if Err <> S_OK then
          OleCheck(Err);
      until Err = S_OK;
      Result := True;
    end;

    procedure DoWrite(AGUID : TGUID);
    var
      PropSetStg: IPropertySetStorage;
      PropStg: IPropertyStorage;
      PropSpec: array of TPropSpec;
      PropVariant: array of TPropVariant;
      PropId: array of Cardinal;
      PropNames: array of LPWSTR;
      AWideString : WideString;
      AString : AnsiString;
      AProperty : TCustomFileProperty;

      NbSI, i, k : Integer;
      function InitStorage : Boolean;
      var
        Err : HResult;
      begin
        PropSetStg := Stg as IPropertySetStorage;

        Err := PropSetStg.Create(AGUID, AGUID,
                  PROPSETFLAG_DEFAULT, STGM_FAILIFTHERE or STGM_READWRITE or
                  STGM_SHARE_EXCLUSIVE, PropStg);

        if Err = STG_E_FILEALREADYEXISTS then
        begin
          Err := PropSetStg.Open(AGUID, STGM_READWRITE or
                        STGM_SHARE_EXCLUSIVE, PropStg);
          if Err <> S_OK then
            OleCheck(Err);
        end
        else
          OleCheck(Err);
        Result := True;
      end;

    begin
      NbSI := 0;
      for i := 0 to FPropertyList.Count - 1 do
      begin
        AProperty := FPropertyList[i];
        if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AGUID, AProperty.GUID)) then
          Inc(NbSI);
      end;

      if NbSI > 0 then
      begin
        if not InitStorage then
          Exit;

        SetLength(PropSpec, NbSI);
        SetLength(PropVariant, NbSI);
        SetLength(PropId, NbSI);
        SetLength(PropNames, NbSI);

        k := 0;
        for i := 0 to FPropertyList.Count - 1 do
        begin
          AProperty := FPropertyList[i];
          if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AGUID, AProperty.GUID)) then
          begin
            PropSpec[k].ulKind := PRSPEC_PROPID;
            PropSpec[k].propid := AProperty.PropertyIndex;
            AProperty.WriteProp(PropVariant[k]);

            PropNames[k] := GetWideChar(AProperty.Name);
            PropId[k] := AProperty.PropertyIndex;

            Inc(k);
          end;
        end;

        OleCheck(PropStg.WriteMultiple(NbSI, @PropSpec[0], @PropVariant[0], 2));
        OleCheck(PropStg.WritePropertyNames(NbSI, @PropId[0], @PropNames[0]));
        PropStg.Commit(STGC_DEFAULT);
        PropStg := nil;

        //Libération de la mémoire des LPWSTR...
        for k := 0 to NbSI - 1 do
        begin
          AWideString := PropNames[k];
          FreeMem(PropNames[k], (Length(AWideString) + 1) * SizeOf(WideChar));
        end;
        k := 0;
        for i := 0 to FPropertyList.Count - 1 do
        begin
          AProperty := FPropertyList[i];
          if (AProperty.Kind = pkSumInfo) and (IsEqualGUID(AGUID, AProperty.GUID)) then
          begin
            if PropVariant[k].vt = VT_LPWSTR then
            begin
              AWideString := PropVariant[k].pwszVal;
              FreeMem(PropVariant[k].pwszVal, (Length(AWideString) + 1) * SizeOf(WideChar));
            end
            else
            if PropVariant[k].vt = VT_LPSTR then
            begin
              AString := AnsiString(PropVariant[k].pszVal);
              FreeMem(PropVariant[k].pszVal, (Length(AString) + 1) * SizeOf(AnsiChar));
            end;
            Inc(k);
          end;
        end;
      end
    end;
  begin
    AGUIDList := FFileClassGUIDList.GUIDListByClass[TFileClass(Self.ClassType)];
    if Assigned(AGUIDList) and (AGUIDList.Count > 0) then
    begin
      if InitRootStorage then
        for i := 0 to AGUIDList.Count - 1 do
          DoWrite(AGUIDList.GUID[i]); //De la sorte, on est sûr que toutes les interfaces sont libérées et
                                      // que le fichier est de nouveau accessible...
    end;
  end;
  procedure WriteStream;
  var
    AFS : TFileStream;
    i, k : Integer;
    AProperty : TCustomFileProperty;

  begin
    AFS := TFileStream.Create(FileName + ':' + GetStreamName, fmCreate or fmShareExclusive);
    try
      //Version du Stream
      TUsefulStream(AFS).WriteLongint(StreamVersion);
      //Date d'écriture
      TUsefulStream(AFS).WriteDateTime(Now);
      //Nombre de propriétés enregistrées... 1) On les compte 2) On les enregistre
      k := 0;
      for i := 0 to FPropertyList.Count - 1 do
        if (FPropertyList[i].Kind = pkStoredData) then
          Inc(k);
      TUsefulStream(AFS).WriteLongint(k);
      for i := 0 to FPropertyList.Count - 1 do
      begin
        AProperty := FPropertyList[i];
        if (AProperty.Kind = pkStoredData) then
        begin
          TUsefulStream(AFS).WriteString(AProperty.Name);
          AProperty.SaveToStream(AFS);
        end;
      end;
    finally
      AFS.Free;
    end;
  end;
begin
{$IFDEF GZ_D10}
  if FileAge(FFileName, FOldAge) then
  begin
    WriteSumInfo;
    WriteStream;
    if RestoreFileDate then
    begin
      FileAge(FFileName,FNewAge);
      if FOldAge <> FNewAge then
        FileSetDate(FFileName, DateTimeToFileDate(FOldAge));
    end;
  end;
{$ELSE}
  if not FileExists(FFileName) then
    Exit;

  FOldAge := FileAge(FFileName);
  {1 On écrit les SumInfo}
  WriteSumInfo;
  {2 On écrit les StoredData dans le stream NTFS}
  {Le stream a le nom de la classe coupé à 31 caractères}
  WriteStream;

  {$IFDEF GZ_D6}
  if RestoreFileDate and (FOldAge <> FileAge(FFileName)) then
    FileSetDate(FFileName, FOldAge);
  {$ENDIF}
{$ENDIF}
end;

{ TCustomFileProperty }

//Construction d'un objet de "kind" pkSumInfo, avec précision du n° de propriété et du GUID
constructor TCustomFileProperty.Create(AParent : TCustomFileInformation; AName: string; AGUID : TGUID; APropertyIndex : Cardinal);
begin
  inherited Create;
  FParent := AParent;
  FName := AName;
  FPropertyKind := pkSumInfo;
  FPropertyIndex := APropertyIndex;
  FGUID := AGUID;
  InternalCreate;
end;

//Construction d'un objet de "kind" pkSystem, avec précision du type précis
constructor TCustomFileProperty.Create(AParent : TCustomFileInformation; AName: string; ASysType: TSysKindProperty);
begin
  inherited Create;
  FParent := AParent;
  FName := AName;
  FPropertyKind := pkSystem;
  FSysType := ASysType;
  FPropertyIndex := SystemPropertyIndex[ASysType];
  InternalCreate;
end;

//Construction d'un objet de "kind" pkFreeData ou pkStoredData
constructor TCustomFileProperty.Create(AParent: TCustomFileInformation; AName: string; AStored : Boolean; APropertyIndex : Cardinal = 0);
begin
  inherited Create;
  FParent := AParent;
  FName := AName;
  FPropertyIndex := APropertyIndex;
  if AStored then
    FPropertyKind := pkStoredData
  else
    FPropertyKind := pkFreeData;
  InternalCreate;
end;

//Construction d'un objet de "kind" pkSystem et par défaut skFileName
constructor TCustomFileProperty.Create(AParent : TCustomFileInformation; ASysType : TSysKindProperty = skFileName);
begin
  inherited Create;
  FParent := AParent;
  FName := 'FileName';
  FPropertyKind := pkSystem;
  FSysType := ASysType;
  InternalCreate;
end;

constructor TCustomFileProperty.Create;
begin
  raise Exception.Create('Ce constructeur est inopérant');
end;

function TCustomFileProperty.GetFilePropertyParams: TFilePropertyParams;
begin
  if not Assigned(FFilePropertyParams) then
    FFilePropertyParams := GzSumInfo.GetFilePropertyParams(FParent, FName);
  Result := FFilePropertyParams;
end;

function TCustomFileProperty.GetTitle: string;
var
  APP : TFilePropertyParams;
begin
  if FTitle <> '' then
    Result := FTitle
  else
  begin
    APP := GetFilePropertyParams;
    if Assigned(APP) then
      Result := APP.Title
    else
      Result := '';
  end;
end;

procedure TCustomFileProperty.InternalCreate;
var
  APP : TFilePropertyParams;
begin
  APP := GetFilePropertyParams;
  if not Assigned(APP) then
    FFilePropertyParamsList.AddObject(GetParamSearchString(FParent, FName), TFilePropertyParams.Create(FParent, Self));
  if FPropertyKind = pkSumInfo then
    FFileClassGUIDList.AddGUID(TFileClass(Self.Parent.ClassType), FGUID);
end;

procedure TCustomFileProperty.LoadFromStream(AStream: TStream);
var
  AType : TPropertyType;
begin
  AStream.Read(AType, SizeOf(FPropertyType));
  case AType of
  ptString : AsString := TUsefulStream(AStream).ReadString;
  ptDateTime : AsDateTime := TUsefulStream(AStream).ReadDateTime;
  ptBoolean : AsInteger := TUsefulStream(AStream).ReadLongInt;
  ptInteger : AsInteger := TUsefulStream(AStream).ReadLongInt;
  ptLargeInt : AsLargeInt := TUsefulStream(AStream).ReadLargeInt;
  end;
end;

procedure TCustomFileProperty.ReadProp(const AProp: TPropVariant);
  function GetString : string;
  begin
    if AProp.vt = VT_LPSTR then
      Result := AProp.pszVal
    else
    if AProp.vt = VT_LPWSTR then
      Result := AProp.pwszVal
    else
      Result := '';
  end;
  function GetDateTime : TDateTime;
  begin
    if AProp.vt = VT_FILETIME then
      Result := FileTimeToLocalDateTime(AProp.filetime)
    else
      Result := 0;
  end;
  function GetBoolean : Boolean;
  begin
    if AProp.vt = VT_BOOL then
    begin
      if AProp.bool then
        Result := True
      else
        Result := False;
    end
    else
      Result := False;
  end;
  function GetInteger : Integer;
  begin
    if AProp.vt in [VT_I1, VT_I2, VT_I4, VT_UI1, VT_UI2, VT_UI4, VT_INT, VT_UINT]  then
      Result := AProp.lVal
    else
      Result := 0;
  end;
  function GetLargeInt : Int64;
  begin
    if AProp.vt in [VT_I8, VT_UI8]  then
      Result := AProp.hVal.QuadPart
    else
      Result := 0;
  end;
begin
  case FPropertyType of
  ptString : AsString := GetString;
  ptDateTime : AsDateTime := GetDateTime;
  ptBoolean : AsBoolean := GetBoolean;
  ptInteger : AsInteger := GetInteger;
  ptLargeInt : AsLargeInt := GetLargeInt;
  end;
end;

procedure TCustomFileProperty.SaveToStream(AStream: TStream);
begin
  AStream.Write(FPropertyType, SizeOf(FPropertyType));
  case FPropertyType of
  ptString : TUsefulStream(AStream).WriteString(AsString);
  ptDateTime : TUsefulStream(AStream).WriteDateTime(AsDateTime);
  ptBoolean : TUsefulStream(AStream).WriteLongInt(AsInteger);
  ptInteger : TUsefulStream(AStream).WriteLongInt(AsInteger);
  ptLargeInt : TUsefulStream(AStream).WriteLargeInt(AsLargeInt);
  end;
end;

procedure TCustomFileProperty.WriteProp(out AProp: TPropVariant);
  procedure SetString;
  begin
    if StringPropertyMode = 0 then
    begin
      AProp.pszVal := GetAnsiChar(AsString);
      AProp.vt := VT_LPSTR;
    end
    else
    begin
      AProp.pwszVal := GetWideChar(AsString);
      AProp.vt := VT_LPWSTR;
    end;
  end;
  procedure SetDateTime;
  begin
    AProp.vt := VT_FILETIME;
    AProp.filetime := LocalDateTimeToFileTime(AsDateTime);
  end;
  procedure SetBoolean;
  begin
    AProp.vt := VT_BOOL;
    AProp.bool := AsBoolean;
  end;
  procedure SetInteger;
  begin
    AProp.vt := VT_I4;
    AProp.lVal := AsInteger;
  end;
  procedure SetLargeInt;
  begin
    AProp.vt := VT_I8;
    AProp.hVal.QuadPart := AsLargeInt;
  end;
begin
  case FPropertyType of
  ptString : SetString;
  ptBoolean : SetBoolean;
  ptDateTime : SetDateTime;
  ptInteger : SetInteger;
  ptLargeInt : SetLargeInt;
  end;
end;

{ TFileStringProperty }

procedure TFileStringProperty.Clear;
begin
  FValue := '';
end;

function TFileStringProperty.GetAsBoolean: Boolean;
begin
  Result := not((FValue = '') or (FValue[1] in ['f', 'F', '0']));
end;

function TFileStringProperty.GetAsDateTime: TDateTime;
begin
  {$IFDEF GZ_D6}
  Result := StrToDateTimeDef(FValue, 0);
  {$ELSE}
    try
      Result := StrToDateTime(FValue);
    except
      Result := 0;
    end;
  {$ENDIF}
end;

function TFileStringProperty.GetAsInteger: Integer;
begin
  Result := StrToIntDef(FValue, 0);
end;

function TFileStringProperty.GetAsLargeInt: Int64;
begin
  Result := StrToIntDef(FValue, 0);
end;

function TFileStringProperty.GetAsString: string;
begin
  Result := FValue;
end;

function TFileStringProperty.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TFileStringProperty.InternalCreate;
begin
  FPropertyType := ptString;
  inherited;
end;

procedure TFileStringProperty.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    FValue := 'Vrai'
  else
    FValue := 'Faux';
end;

procedure TFileStringProperty.SetAsDateTime(const Value: TDateTime);
begin
  FValue := DateTimeToStr(Value)
end;

procedure TFileStringProperty.SetAsInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);
end;

procedure TFileStringProperty.SetAsLargeInt(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TFileStringProperty.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TFileStringProperty.SetAsVariant(const Value: Variant);
begin
  try
    FValue := Value;
  except
    FValue := '';
  end;
end;

{ TFileBooleanProperty }

procedure TFileBooleanProperty.Clear;
begin
  FValue := False;
end;

function TFileBooleanProperty.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TFileBooleanProperty.GetAsDateTime: TDateTime;
begin
  Result := 0;
end;

function TFileBooleanProperty.GetAsInteger: Integer;
begin
  Result := Ord(FValue);
end;

function TFileBooleanProperty.GetAsLargeInt: Int64;
begin
  Result := GetAsInteger;
end;

function TFileBooleanProperty.GetAsString: string;
begin
  if FValue then
    Result := 'Vrai'
  else
    Result := 'Faux';
end;

function TFileBooleanProperty.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TFileBooleanProperty.InternalCreate;
begin
  FPropertyType := ptBoolean;
  inherited;
end;

procedure TFileBooleanProperty.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value
end;

procedure TFileBooleanProperty.SetAsDateTime(const Value: TDateTime);
begin
  FValue := False;
end;

procedure TFileBooleanProperty.SetAsInteger(const Value: Integer);
begin
  FValue := (Value <> 0);
end;

procedure TFileBooleanProperty.SetAsLargeInt(const Value: Int64);
begin
  FValue := (Value <> 0);
end;

procedure TFileBooleanProperty.SetAsString(const Value: string);
begin
  FValue := not ((Value = '') or (Value[1] in ['f', 'F', '0']));
end;

procedure TFileBooleanProperty.SetAsVariant(const Value: Variant);
begin
  try
    FValue := Value;
  except
    FValue := False;
  end;
end;

{ TFileDateTimeProperty }

procedure TFileDateTimeProperty.Clear;
begin
  FValue := 0;
end;

function TFileDateTimeProperty.GetAsBoolean: Boolean;
begin
  Result := False;
end;

function TFileDateTimeProperty.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

function TFileDateTimeProperty.GetAsInteger: Integer;
begin
  Result := Trunc(FValue);
end;

function TFileDateTimeProperty.GetAsLargeInt: Int64;
begin
  Result := Trunc(FValue);
end;

function TFileDateTimeProperty.GetAsString: string;
begin
  if FValue = 0 then
    Result := ''
  else
    Result := DateTimeToStr(FValue);
end;

function TFileDateTimeProperty.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TFileDateTimeProperty.InternalCreate;
begin
  if (FPropertyKind = pkSystem) and (FSysType = skFileDate) then
    FValue := Parent.FileDate;
  FPropertyType := ptDateTime;
  inherited;
end;

procedure TFileDateTimeProperty.SetAsBoolean(const Value: Boolean);
begin
  FValue := 0;
end;

procedure TFileDateTimeProperty.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Value;
end;

procedure TFileDateTimeProperty.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TFileDateTimeProperty.SetAsLargeInt(const Value: Int64);
begin
  FValue := Value;
end;

procedure TFileDateTimeProperty.SetAsString(const Value: string);
begin
  {$IFDEF GZ_D6}
  FValue := StrToDateTimeDef(Value, 0);
  {$ENDIF}
end;

procedure TFileDateTimeProperty.SetAsVariant(const Value: Variant);
begin
  try
    FValue := Value;
  except
    FValue := 0;
  end;
end;

{ TPropertyList }

procedure TPropertyList.Add(Item: TCustomFileProperty);
begin
  inherited Add(Item);
end;

function TPropertyList.Extract(Item: TCustomFileProperty): TCustomFileProperty;
begin
  Result := TCustomFileProperty(inherited Extract(Item));
end;

function TPropertyList.FindByName(AName: string): TCustomFileProperty;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, AName) = 0 then
    begin
      Result := Items[i];
      Exit;
    end
end;

function TPropertyList.First: TCustomFileProperty;
begin
  Result := TCustomFileProperty(inherited First);
end;

function TPropertyList.GetItems(Index: Integer): TCustomFileProperty;
begin
  Result := TCustomFileProperty(inherited Items[Index]);
end;

function TPropertyList.IndexOf(Item: TCustomFileProperty): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TPropertyList.Insert(Index: Integer; Item: TCustomFileProperty);
begin
  inherited Insert(Index, Item);
end;

function TPropertyList.Last: TCustomFileProperty;
begin
  Result := TCustomFileProperty(inherited Last);
end;

function TPropertyList.Remove(Item: TCustomFileProperty): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TPropertyList.SetItems(Index: Integer; const Value: TCustomFileProperty);
begin
  inherited Items[Index] := Value;
end;

{ TFileIntegerProperty }

procedure TFileIntegerProperty.Clear;
begin
  FValue := 0;
end;

function TFileIntegerProperty.GetAsBoolean: Boolean;
begin
  if FValue = 0 then
    Result := False
  else
    Result := True;
end;

function TFileIntegerProperty.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

function TFileIntegerProperty.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TFileIntegerProperty.GetAsLargeInt: Int64;
begin
  Result := GetAsInteger;
end;

function TFileIntegerProperty.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TFileIntegerProperty.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TFileIntegerProperty.InternalCreate;
begin
  FPropertyType := ptInteger;
  inherited;
end;

procedure TFileIntegerProperty.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TFileIntegerProperty.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Trunc(Value);
end;

procedure TFileIntegerProperty.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TFileIntegerProperty.SetAsLargeInt(const Value: Int64);
begin
  FValue := Value;
end;

procedure TFileIntegerProperty.SetAsString(const Value: string);
begin
  FValue := StrToIntDef(Value, 0);
end;

procedure TFileIntegerProperty.SetAsVariant(const Value: Variant);
begin
  try
    FValue := Value;
  except
    FValue := 0;
  end;
end;

{ TFilePropertyParams }

constructor TFilePropertyParams.Create(AFileInfo : TCustomFileInformation; AProperty : TCustomFileProperty);
begin
  FName := AnsiUpperCase(AProperty.Name);
  FTitle := AProperty.Name;
  FWidth := 50;
  FAlignment := taLeftJustify;
  FIndex := -1;
  FPropertyIndex := AProperty.PropertyIndex;
  FPropertyType := AProperty.PropertyType;
  FPropertyKind := AProperty.Kind;
  FGUID := AProperty.GUID;
end;

procedure TFilePropertyParams.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  //PostMessage(HWND_BROADCAST, WM_PROPERTYPARAMCHANGED, 0, 0);
end;

procedure TFilePropertyParams.SetTitle(const Value: string);
begin
  FTitle := Value;
  //PostMessage(HWND_BROADCAST, WM_PROPERTYPARAMCHANGED, 0, 0);
end;

{ TSystemFileInformation }

procedure TSystemFileInformation.CreateProperties;
begin
  inherited;
  AddProperty(TFileStringProperty.Create(Self, 'FileName', skFileName));
  AddProperty(TFileDateTimeProperty.Create(Self, 'FileDate', skFileDate));
  AddProperty(TFileStringProperty.Create(Self, 'FileType', skFileType));
  AddProperty(TFileLargeIntProperty.Create(Self, 'FileSize', skFileSize));
end;

{ TGUIDList }

procedure TGUIDList.AddGUID(AGUID: TGUID);
var
  AStr : string;
  k : Integer;
begin
  AStr := GUIDToString(AGUID);
  if not Find(AStr, k) then
    Add(AStr);
end;

function TGUIDList.GetGUID(Index: Integer): TGUID;
begin
  Result := StringToGUID(Strings[Index]);
end;

{ TFileClassGUIDList }

procedure TFileClassGUIDList.AddGUID(AClass: TFileClass; AGUID: TGUID);
begin
  AddList(AClass);
  GUIDListByClass[AClass].AddGUID(AGUID);
end;

procedure TFileClassGUIDList.AddList(AClass: TFileClass);
var
  k : Integer;
  ALst : TGUIDList;
begin
  if not Find(AClass.ClassName, k) then
  begin
    ALst := TGUIDList.Create;
    ALst.Sorted := True;
    ALst.Duplicates := dupIgnore;
    AddObject(AClass.ClassName, ALst);
  end;
end;

function TFileClassGUIDList.GetGUIDList(Index: Integer): TGUIDList;
begin
  Result := TGUIDList(Objects[Index]);
end;

function TFileClassGUIDList.GetGUIDList2(AClass: TFileClass): TGUIDList;
var
  k : Integer;
begin
  Result := nil;
  if Find(AClass.ClassName, k) then
    Result := GUIDListByIndex[k];
end;

{ TFileLargeIntProperty }

procedure TFileLargeIntProperty.Clear;
begin
  FValue := 0;
end;

function TFileLargeIntProperty.GetAsBoolean: Boolean;
begin
  if FValue = 0 then
    Result := False
  else
    Result := True;
end;

function TFileLargeIntProperty.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

function TFileLargeIntProperty.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TFileLargeIntProperty.GetAsLargeInt: Int64;
begin
  Result := FValue;
end;

function TFileLargeIntProperty.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TFileLargeIntProperty.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TFileLargeIntProperty.InternalCreate;
begin
  FPropertyType := ptLargeInt;
  inherited;
end;

procedure TFileLargeIntProperty.SetAsBoolean(const Value: Boolean);
begin
  FValue := Ord(Value);
end;

procedure TFileLargeIntProperty.SetAsDateTime(const Value: TDateTime);
begin
  FValue := Trunc(Value);
end;

procedure TFileLargeIntProperty.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TFileLargeIntProperty.SetAsLargeInt(const Value: Int64);
begin
  FValue := Value;
end;

procedure TFileLargeIntProperty.SetAsString(const Value: string);
begin
  FValue := StrToIntDef(Value, 0);
end;

procedure TFileLargeIntProperty.SetAsVariant(const Value: Variant);
begin
  try
    FValue := Value;
  except
    FValue := 0;
  end;
end;

initialization
  FFileTypeCacheList := TDictionnary.Create;
  FFilePropertyParamsList := TStringList.Create;
  FFilePropertyParamsList.Sorted := True;
  FFilePropertyParamsList.Duplicates := dupIgnore;
  FFileClassGUIDList := TFileClassGUIDList.Create;
  FFileClassGUIDList.Sorted := True;
  FFileClassGUIDList.Duplicates := dupIgnore;
  WM_PROPERTYPARAMCHANGED := RegisterWindowMessage('CM_PROPERTYPARAMCHANGED');
  CheckPropertyParamList(TSystemFileInformation);
  RestoreFileDate := False;
finalization
  ClearStringList(FFilePropertyParamsList);
  FFilePropertyParamsList.Free;
  ClearStringList(FFileClassGUIDList);
  FFileClassGUIDList.Free;
  FFileTypeCacheList.Free;
end.

