unit ADB;

interface

uses Classes, DB, AUtils;

  { Fonctions diverses }
  procedure ApplyToDataSet(DataSet: TDataSet; Proc: TParamProc; Params: Pointer);
  procedure ApplyToDataSetEx(DataSet: TDataSet; Proc: TParamProc; Params: Pointer);
  procedure ApplyToDataSetFilter(DataSet: TDataSet; Proc: TParamProc; Params: Pointer);

  procedure StoreDataSet(DataSet: TDataSet);
  procedure RestoreDataSet(DataSet: TDataSet);
{
  function GetAliasPath(const AliasName: string): string;
  procedure ExecSQL(DataBaseName: string; SQL: array of string);
  function DataBaseFiles(Dir: string): TStringList;
  function Editing(DataSet: TDataSet): Boolean;
}
//  procedure register;

implementation

uses SysUtils;

type
  TStoredDataSet = record
    DataSet: TDataSet;
    Bookmark: TBookmarkStr;
    Filter: string;
    Filtered: Boolean;
  end;
  PStoredDataSet = ^TStoredDataSet;

  TStoredDataSetList = class(TList)
  public
    destructor Destroy; override;
    function IndexOf(DataSet: TDataSet): Integer;
    procedure Add(DataSet: TDataSet);
    procedure Delete(DataSet: TDataSet);
  end;

destructor TStoredDataSetList.Destroy;
begin
  while Count > 0 do
  begin
    Dispose(PStoredDataSet(Items[Count - 1]));
    inherited Delete(Count - 1);
  end;
  inherited Destroy;
end;

procedure TStoredDataSetList.Add(DataSet: TDataSet);
var P: PStoredDataSet;
begin
  New(P);
  try
    P^.DataSet:= DataSet;
    P^.Bookmark:= DataSet.Bookmark;
    P^.Filter:= DataSet.Filter;
    P^.Filtered:= DataSet.Filtered;
    DataSet.DisableControls;
    inherited Add(P);
  except
    Dispose(P);
    raise;
  end;
end;

procedure TStoredDataSetList.Delete(DataSet: TDataSet);
var P: PStoredDataSet;
    Idx: Integer;
begin
  Idx:= IndexOf(DataSet);
  if Idx > -1 then
  begin
    P:= Items[Idx];
    try
      DataSet.EnableControls;
      DataSet.Filtered:= P^.Filtered;
      DataSet.Filter:= P^.Filter;
      DataSet.Bookmark:= P^.Bookmark;
    finally
      inherited Delete(Idx);
      Dispose(P);
    end;
  end else raise Exception.Create('Le DataSet ' + DataSet.Name + ' n''a pas été'+
    ' sauvegardé pour restauration !');
end;

function TStoredDataSetList.IndexOf(DataSet: TDataSet): Integer;
var i: Integer;
begin
  i:= Count - 1;
  Result:= -1;
  while (i >= 0) and (Result = -1) do
  begin
    if PStoredDataSet(Items[i])^.DataSet = DataSet then
      Result:= i;
    Dec(i);
  end;
end;

var StoredDataSets: TStoredDataSetList;

procedure StoreDataSet(DataSet: TDataSet);
begin
  StoredDataSets.Add(DataSet);
end;

procedure RestoreDataSet(DataSet: TDataSet);
begin
  StoredDataSets.Delete(DataSet);
end;

procedure ApplyToDataSet(DataSet: TDataSet; Proc: TParamProc; Params: Pointer);
var OldBookmark: TBookmarkStr;
begin
  DataSet.DisableControls;
  try
    OldBookmark:= DataSet.Bookmark;
    try
      Proc(Params);
    finally
      DataSet.Bookmark:= OldBookmark;
    end;
  finally
    DataSet.EnableControls;
  end;
end;

procedure ApplyToDataSetEx(DataSet: TDataSet; Proc: TParamProc; Params: Pointer);
var OldActive: Boolean;
begin
  OldActive:= DataSet.Active;
  DataSet.Active:= True;
  try
    ApplyToDataSet(DataSet, Proc, Params);
  finally
    DataSet.Active:= OldActive;
  end;
end;

procedure ApplyToDataSetFilter(DataSet: TDataSet; Proc: TParamProc; Params: Pointer);
var OldFilter: string;
    OldFiltered: Boolean;
    OldBookmark: TBookmarkStr;
begin
  DataSet.DisableControls;
  try
    OldBookmark:= DataSet.Bookmark;
    try
      OldFilter:= DataSet.Filter;
      OldFiltered:= DataSet.Filtered;
      try
        Proc(Params);
      finally
        DataSet.Filter:= OldFilter;
        DataSet.Filtered:= OldFiltered;
      end;
    finally
      DataSet.Bookmark:= OldBookmark;
    end;
  finally
    DataSet.EnableControls;
  end;
end;
{
function GetAliasPath(const AliasName: string): string;
var
  i: Integer;
  List: TStringList;
begin
  List:= TStringList.Create;
  try
     Session.GetAliasParams(AliasName, List);
  except
     for i:= 0 to Session.DatabaseCount - 1 do
        if AnsiCompareText(Session.DataBases[i].DataBaseName, AliasName) = 0 then
           Session.GetAliasParams(Session.DataBases[i].AliasName, List);
     if List.Count = 0 then raise;
  end;
  Result:= List[0];
  if Copy(Result, 1, 5) = 'PATH=' then
     Result:= Copy(Result, 6, Length(Result));
end;

procedure ExecSQL(DataBaseName: string; SQL: array of string);
var Creator: TQuery; i: Integer;
begin
  Creator:= TQuery.Create(nil);
  try
     Creator.DataBaseName:= DataBaseName;
     for i:= 0 to High(SQL) do
        Creator.SQL.Add(SQL[i]);
     Creator.ExecSQL;
  finally
     Creator.Free;
  end;
end;

function DataBaseFiles(Dir: string): TStringList;
var
  L: TStringList;
  i, j: Integer;
begin
  Result:= DirFiles(Dir, '*.DB');
  L:= DirFiles(Dir, '*.DBF');
  Result.AddStrings(L); L.Free;
  for i:= 0 to Result.Count - 1 do
  begin
     L:= DirFiles(Dir, ExtractFileShortName(Result[i]) + '.*');
     for j:= L.Count - 1 downto 0 do
        if L[j] = Result[i] then L.Delete(j);
     Result.AddStrings(L); L.Free;
  end;
end;

function Editing(DataSet: TDataSet): Boolean;
begin
  Result:= DataSet.State in [dsInsert, dsEdit];
end;
}

initialization
  StoredDataSets:= TStoredDataSetList.Create;

finalization
  StoredDataSets.Free;

end.
