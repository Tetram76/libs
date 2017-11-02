unit GZDBParams;

{$I GrizzlyDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, ExtCtrls, {$IFDEF GZ_D6}Variants,{$ENDIF} GZParams;

{ ATTENTION à AutoClose qui peut contrôler la destruction des TGZParams...}

{
Format de BDD attendu :
PathName : string(255)
ParamName : string(255)
Value : BLOB
}

type
  EGZDBParamError = class(Exception);

  TDBParamDatabase = class;

  {Règle de stockage : }
  { 1 octet : le type (F,I,S,B) }
  { 4 octets : la taille }
  { ensuite les données }

  TGZDBParam = class(TGZParam)
  private
    FBookmark: string;
    function GetParamDB: TDBParamDatabase;
  protected
    function GetAsString: string; override;
    procedure SetAsString(Value: string); override;
    function GetAsInteger: Integer; override;
    procedure SetAsInteger(Value: Integer); override;
    function GetAsFloat: Extended; override;
    procedure SetAsFloat(Value: Extended); override;
    function GetExists: Boolean; override;
    procedure CreateRecord;
    function FindRecord: Boolean;
  public
    constructor Create(AParamDB: TCustomParamDatabase; APath, AParamName: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;
    property ParamDB: TDBParamDatabase read GetParamDB;
    procedure Delete; override;
  end;

  TGZDBParamListSortCompare = function(DBP1, DBP2: TGZDBParam): Integer;

  TGZDBParamList = class(TList)
  protected
    function Get(Index: Integer): TGZDBParam;
    procedure Put(Index: Integer; Item: TGZDBParam);
  public
    function Add(Item: TGZDBParam): Integer;
    function Find(APath, AParamName: string; var k: Integer): Boolean;
    function IndexOf(Item: TGZDBParam): Integer;
    procedure Insert(Index: Integer; Item: TGZDBParam);
    function Remove(Item: TGZDBParam): Integer;
    property Items[Index: Integer]: TGZDBParam read Get write Put; default;
    procedure Sort(Compare: TGZDBParamListSortCompare);
  end;

  TGZDBParamField = string;

  TDBParamDatabase = class(TCustomParamDatabase)
  private
    { Déclarations privées }
    FDataset: TDataSet;
    FParamList: TGZDBParamList;
    FPathFieldName: TGZDBParamField;
    FParamNameFieldName: TGZDBParamField;
    FValueFieldName: TGZDBParamField;
    FPathField: TField;
    FParamNameField: TField;
    FValueField: TField;
    procedure SetDataSet(Value: TDataSet);
    procedure CheckFields;
  protected
    { Déclarations protégées }
    procedure SetInternalActive(Value: Boolean); override;
    function GetActive: Boolean; override;

    function CreateGZParam(const APath, AParamName: string): TGZParam; override;
    function GetParamByName(APath, AParamName: string): TGZParam; override;

    procedure GetPathList(BasePath: string; List: TStrings); override;

    procedure CheckDataset;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetParamList(Path: string; List: TStrings); override;
  published
    { Déclarations publiées }
    property Dataset: TDataSet read FDataSet write SetDataSet;
    property PathFieldName: TGZDBParamField read FPathFieldName write FPathFieldName;
    property ParamNameFieldName: TGZDBParamField read FParamNameFieldName write FParamNameFieldName;
    property ValueFieldName: TGZDBParamField read FValueFieldName write FValueFieldName;
  end;


implementation

uses FGUtils;

{ TGZDBParam }

constructor TGZDBParam.Create(AParamDB: TCustomParamDatabase; APath, AParamName: string);
begin
  inherited Create(AParamDB, APath, AParamName);

  ParamDB.FParamList.Add(Self);

  ParamDB.CheckActive;

  if FindRecord then
  begin
    try
      FBookmark:= ParamDB.FDataset.Bookmark;
    except
      FBookmark:= '';
    end;
  end
  else
    CreateRecord;
end;

function TGZDBParam.GetParamDB: TDBParamDatabase;
begin
  Result:= TDBParamDatabase(FParamDB);
end;

function TGZDBParam.GetExists: Boolean;
begin
  Result:= FindRecord;
end;

function TGZDBParam.FindRecord: Boolean;
begin
  with ParamDB do
  begin
    CheckActive;
    if FBookmark = '' then
      Result:= FDataset.Locate(FPathFieldName + ';' + FParamNameFieldName, VarArrayOf([FPath, FParamName]), [loCaseInsensitive])
    else
    begin
      try
        FDataset.Bookmark:= FBookmark;
      except
        Result:= FDataset.Locate(FPathFieldName + ';' + FParamNameFieldName, VarArrayOf([FPath, FParamName]), [loCaseInsensitive]);
        Exit;
      end;
      Result:= True;
    end;
  end;
end;

procedure TGZDBParam.CreateRecord;
begin
  with ParamDB do
  begin
    CheckActive;
    if ReadOnly then
      FBookmark:= ''
    else
    begin
      FDataset.Append;
      FPathField.AsString:= FPath;
      FParamNameField.AsString:= FParamName;
      FDataset.Post;
      try
        FBookmark:= FDataset.Bookmark;
      except
        FBookmark:= '';
      end;
    end;
  end;
end;

function TGZDBParam.GetAsString: string;
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  P: PChar;
  AExt: Extended;
  AInt: Longint;
  Taille: Longint;
begin
  if FindRecord then
  begin
    if ParamDB.FValueField.IsNull then
    begin
      Result:= '';
      Exit;
    end;
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      BField.SaveToStream(AMemStream);
      AMemStream.Position:= 0;
      P:= PChar(AMemStream.Memory);
      if P <> nil then
      begin
        Move(P[1], Taille, 4);
        if (P[0] = 'S') or (P[0] = 'B') then
        begin
          SetString(Result, PChar(@P[5]), Taille);
        end else if P[0] = 'F' then
        begin
          Move(P[5], AExt, SizeOf(Extended));
          Result:= FloatToStr(AExt);
        end else if P[0] = 'I' then
        begin
          Move(P[5], AInt, 4);
          Result:= IntToStr(AInt);
        end;
      end
      else
        Result:= '';
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

function TGZDBParam.GetAsInteger: Integer;
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  P: PChar;
  AExt: Extended;
  Taille: Longint;
  AStr: string;
begin
  if FindRecord then
  begin
    if ParamDB.FValueField.IsNull then
    begin
      Result:= 0;
      Exit;
    end;
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      BField.SaveToStream(AMemStream);
      P:= PChar(AMemStream.Memory);
      if P <> nil then
      begin
        Move(P[1], Taille, 4);
        if (P[0] = 'S') or (P[0] = 'B') then
        begin
          SetString(AStr, PChar(@P[5]), Taille);
          Result:= StrToInt(AStr);
        end
        else if P[0] = 'F' then
        begin
          Move(P[5], AExt, SizeOf(Extended));
          Result:= Trunc(AExt);
        end
        else if P[0] = 'I' then
        begin
          Move(P[5], Result, 4);
        end;
      end
      else
        Result:= 0;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

function TGZDBParam.GetAsFloat: Extended;
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  P: PChar;
  AInt: Longint;
  Taille: Longint;
  AStr: string;
begin
  if FindRecord then
  begin
    if ParamDB.FValueField.IsNull then
    begin
      Result:= 0;
      Exit;
    end;
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      BField.SaveToStream(AMemStream);
      P:= PChar(AMemStream.Memory);
      if P <> nil then
      begin
        Move(P[1], Taille, 4);
        if (P[0] = 'S') or (P[0] = 'B') then
        begin
          SetString(AStr, PChar(@P[5]), Taille);
          Result:= StrToFloat(AStr);
        end
        else if P[0] = 'F' then
        begin
          Move(P[5], Result, SizeOf(Extended));
        end
        else if P[0] = 'I' then
        begin
          Move(P[5], AInt, 4);
          Result:= AInt;
        end;
      end
      else
        Result:= 0;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

procedure TGZDBParam.SaveToStream(AStream: TStream);
var
  AMemStream: TMemoryStream;
  P: Pointer;
  BField: TBlobField;
begin
  if FindRecord then
  begin
    AStream.Position:= 0;
    AStream.Size:= 0;
    if ParamDB.FValueField.IsNull then
    begin
      Exit;
    end;
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      BField.SaveToStream(AMemStream);
      if AMemStream.Size > 5 then
      begin
        P:= Pointer(Longint(AMemStream.Memory) + 5);
        if P <> nil then
          AStream.Write(P^, AMemStream.Size - 5);
        AStream.Position:= 0;
      end;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

procedure TGZDBParam.LoadFromStream(AStream: TStream);
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  AType: string;
  P: Pointer;
  ATaille: Longint;
begin
  ParamDB.CheckReadOnly;
  if FindRecord then
  begin
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      ParamDB.FDataset.Edit;
      try
        AType:= 'B';
        ATaille:= AStream.Size;
        AMemStream.Write(PChar(AType)^, 1);
        AMemStream.Write(ATaille, 4);
        AStream.Position:= 0;
        AMemStream.Size:= 5 + ATaille;
        P:= Pointer(Longint(AMemStream.Memory) + 5);
        AStream.ReadBuffer(P^, AStream.Size);
        BField.LoadFromStream(AMemStream);
        ParamDB.FDataset.Post;
      except
        ParamDB.FDataset.Cancel;
        raise;
      end;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

procedure TGZDBParam.SetAsString(Value: string);
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  AType: string;
  ATaille: Longint;
begin
  ParamDB.CheckReadOnly;
  if FindRecord then
  begin
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      ParamDB.FDataset.Edit;
      try
        AType:= 'S';
        ATaille:= Length(Value);
        AMemStream.Write(PChar(AType)^, 1);
        AMemStream.Write(ATaille, 4);
        AMemStream.Write(PChar(Value)^, ATaille);
        AMemStream.Position:= 0;
        BField.LoadFromStream(AMemStream);
        ParamDB.FDataset.Post;
      except
        ParamDB.FDataset.Cancel;
        raise;
      end;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

procedure TGZDBParam.SetAsInteger(Value: Integer);
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  AType: string;
  ATaille: Longint;
begin
  ParamDB.CheckReadOnly;
  if FindRecord then
  begin
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      ParamDB.FDataset.Edit;
      try
        AType:= 'I';
        ATaille:= SizeOf(Longint);
        AMemStream.Write(PChar(AType)^, 1);
        AMemStream.Write(ATaille, 4);
        AMemStream.Write(Value, ATaille);
        BField.LoadFromStream(AMemStream);
        ParamDB.FDataset.Post;
      except
        ParamDB.FDataset.Cancel;
        raise;
      end;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

procedure TGZDBParam.SetAsFloat(Value: Extended);
var
  AMemStream: TMemoryStream;
  BField: TBlobField;
  AType: string;
  ATaille: Longint;
begin
  ParamDB.CheckReadOnly;
  if FindRecord then
  begin
    AMemStream:= TMemoryStream.Create;
    try
      BField:= TBlobField(ParamDB.FValueField);
      ParamDB.FDataset.Edit;
      try
        AType:= 'F';
        ATaille:= SizeOf(Extended);
        AMemStream.Write(PChar(AType)^, 1);
        AMemStream.Write(ATaille, 4);
        AMemStream.Write(Value, ATaille);
        BField.LoadFromStream(AMemStream);
        ParamDB.FDataset.Post;
      except
        ParamDB.FDataset.Cancel;
        raise;
      end;
    finally
      AMemStream.Free;
    end;
  end
  else
    raise EGZDBParamError.Create(Format('Paramètre %s introuvable dans %s', [FParamName, FPath]));
end;

procedure TGZDBParam.Delete;
begin
  ParamDB.CheckReadOnly;
  if FindRecord then
    ParamDB.FDataset.Delete;
end;

{ TDBParamDatabase }

constructor TDBParamDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FParamList:= TGZDBParamList.Create;
end;

destructor TDBParamDatabase.Destroy;
begin
  ClearList(FParamList);
  FParamList.Free;
  inherited Destroy;
end;

procedure TDBParamDatabase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
    (AComponent = FDataset) then
  begin
    FDataset:= nil;
  end;
end;

function TDBParamDatabase.CreateGZParam(const APath, AParamName: string): TGZParam;
begin
  Result:= TGZDBParam.Create(Self, APath, AParamName);
end;

function TDBParamDatabase.GetParamByName(APath, AParamName: string): TGZParam;
var
  i: integer;
begin
  CheckDataset;
  if FParamList.Find(APath, AParamName, i) then
    Result:= FParamList[i]
  else
  begin
    {Recherche du paramètre dans la table}
    if (not AutoCreateParam) and (not FDataset.Locate(FPathFieldName + ';' + FParamNameFieldName, VarArrayOf([APath, AParamName]), [loCaseInsensitive])) then
      raise EGZDBParamError.Create('Chemin et/ou paramètre inexistant');
    Result:= CreateGZParam(APath, AParamName);
  end;
end;

procedure TDBParamDatabase.GetPathList(BasePath: string; List: TStrings);
begin
end;

procedure TDBParamDatabase.GetParamList(Path: string; List: TStrings);
begin
  List.Clear;
  CheckActive;
  try
    FDataset.First;
    while not FDataset.EOF do
    begin
      if CompareText(FDataset.FieldByName(FPathFieldName).AsString, Path) = 0 then
        List.Add(FDataset.FieldByName(FParamNameFieldName).AsString);
      FDataset.Next;
    end;
  finally
    CheckAutoClose;
  end;
end;

procedure TDBParamDatabase.CheckFields;
begin
  CheckDataset;
  if FDataset.Active then
  begin
    try
      FPathField:= FDataset.FieldByName(FPathFieldName);
      FParamNameField:= FDataset.FieldByName(FParamNameFieldName);
      FValueField:= FDataset.FieldByName(FValueFieldName);
      if not FValueField.IsBlob then
        raise EGZDBParamError.Create('Le champ de valeur doit être un champ binaire');
    except
      FPathField:= nil;
      FParamNameField:= nil;
      FValueField:= nil;
      raise;
    end;
  end
  else
  begin
    FPathField:= nil;
    FParamNameField:= nil;
    FValueField:= nil;
  end;
end;

procedure TDBParamDatabase.SetInternalActive(Value: Boolean);
begin
  CheckDataset;
  ClearList(FParamList);
  if Value then
  begin
    try
      FDataSet.Open;
      if FDataset.Active then
        CheckFields
      else
        Exit;
    except
      FActive:= False;
      raise;
    end;
    FActive:= True;
  end
  else
  begin
    FActive:= False;
    if Assigned(FDataset) then
      FDataSet.Close;
  end;
end;

function TDBParamDatabase.GetActive: Boolean;
begin
  Result:= Assigned(FDataset) and FDataSet.Active and FActive;
end;

procedure TDBParamDatabase.CheckDataset;
begin
  if Dataset = nil then
    raise EGZDBParamError.Create('Ensemble de données non défini. Opération impossible.');
end;

procedure TDBParamDatabase.SetDataSet(Value: TDataSet);
begin
  FDataSet:= Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

{ TGZDBParamList }

function TGZDBParamList.Find(APath, AParamName: string; var k: Integer): Boolean;
var
  i: Integer;
begin
  k:= -1;
  Result:= False;
  for i:= 0 to Count - 1 do
  begin
    if (CompareText(Items[i].FPath, APath) = 0) and (CompareText(Items[i].FParamName, AParamName) = 0) then
    begin
      k:= i;
      Result:= True;
      Exit;
    end;
  end;
end;

function TGZDBParamList.Get(Index: Integer): TGZDBParam;
begin
  Result:= TGZDBParam(inherited Get(Index));
end;

procedure TGZDBParamList.Put(Index: Integer; Item: TGZDBParam);
begin
  inherited Put(Index, Pointer(Item));
end;

function TGZDBParamList.Add(Item: TGZDBParam): Integer;
begin
  Result:= inherited Add(Pointer(Item));
end;

function TGZDBParamList.IndexOf(Item: TGZDBParam): Integer;
begin
  Result:= inherited IndexOf(Pointer(Item));
end;

procedure TGZDBParamList.Insert(Index: Integer; Item: TGZDBParam);
begin
  inherited Insert(Index, Pointer(Item));
end;

function TGZDBParamList.Remove(Item: TGZDBParam): Integer;
begin
  Result:= inherited Remove(Pointer(Item));
end;

procedure TGZDBParamList.Sort(Compare: TGZDBParamListSortCompare);
begin
  inherited Sort(TListSortCompare(Compare));
end;

end.

