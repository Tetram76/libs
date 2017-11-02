unit DBToTxt;

interface

uses SysUtils, Classes, DB;

type
  TExportMode = (emAsString, emDisplayText);
  TTitlesMode = (tmDisplayLabel, tmFieldName);

  TExportFieldEvent = procedure(Sender: TObject; Field: TField; var Value: string) of object;

  TDBExportToText = class(TComponent)
  private
    FDataSet: TDataSet;
    FSeparator: Char;
    FExportFile: TFileName;
    FExportMode: TExportMode;
    FOnExportField: TExportFieldEvent;
    FExportTitles: Boolean;
    FTitlesMode: TTitlesMode;
    procedure SetDataSet(DataSet: TDataSet);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Separator: Char read FSeparator write FSeparator;
    property ExportFile: TFileName read FExportFile write FExportFile;
    property ExportMode: TExportMode read FExportMode write FExportMode;
    property ExportTitles : Boolean read FExportTitles write FExportTitles;
    property TitlesMode: TTitlesMode read FTitlesMode write FTitlesMode;
    property OnExportField: TExportFieldEvent read FOnExportField write FOnExportField;
  end;

implementation

procedure TDBExportToText.Execute;
var F: TextFile;
    Bookmark: TBookmarkStr;
    i: Integer;
    AFieldList: TList;
  procedure WriteField(Field: TField);
  var S: string;
  begin
    if ExportMode = emDisplayText then
      S:= Field.DisplayText
    else
      S:= Field.AsString;
    if Assigned(FOnExportField) then FOnExportField(Self, Field, S);
    Write(F, S);
  end;
begin
  AssignFile(F, ExportFile);
  Rewrite(F);
  try
    AFieldList:= TList.Create;
    try
      DataSet.DisableControls;
      try
        Bookmark:= DataSet.Bookmark;
        try
          with DataSet do
          begin
            { Identification des champs à exporter }
            for i:= 0 to FieldCount - 1 do
              if Fields[i].Visible then AFieldList.Add(Fields[i]);
            { Ecriture des titres des champs }
            if ExportTitles then
            begin
              for i:= 0 to AFieldList.Count - 1 do
              begin
                if TitlesMode = tmDisplayLabel then
                  Write(F, TField(AFieldList[i]).DisplayLabel)
                else
                  Write(F, TField(AFieldList[i]).FieldName);
                if i = (AFieldList.Count - 1) then
                  Write(F, #13#10)
                else
                  Write(F, Separator);
              end;
            end;
            { Ecriture des champs }
            First;
            while not EOF do
            begin
              for i:= 0 to AFieldList.Count - 1 do
              begin
                WriteField(TField(AFieldList[i]));
                if i = (AFieldList.Count - 1) then
                  Write(F, #13#10)
                else
                  Write(F, Separator);
              end;
              Next;
            end;
          end;
        finally
          DataSet.Bookmark:= Bookmark;
        end;
      finally
        DataSet.EnableControls;
      end;
    finally
      AFieldList.Free;
    end;
  finally
    Close(F);
  end;
end;

procedure TDBExportToText.SetDataSet(DataSet: TDataSet);
begin
  FDataSet:= DataSet;
  if Assigned(DataSet) then
    DataSet.FreeNotification(Self);
end;

constructor TDBExportToText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeparator:= #9;
  FExportTitles:= False;
end;

end.
