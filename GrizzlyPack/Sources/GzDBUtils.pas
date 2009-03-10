unit GzDBUtils;

interface

uses SysUtils, Classes, DB;

function DatasetToString(ADataset: TDataset; DisableControls : Boolean = False; FieldSeparator: string = #9; LineSeparator: string = #13#10) : string;
procedure StringToDataset(ADataset : TDataset; AString : string; AFieldSeparator : string; AWithHeaders : Boolean);

implementation

function DatasetToString(ADataset: TDataset; DisableControls : Boolean = False; FieldSeparator: string = #9; LineSeparator: string = #13#10) : string;
  procedure SaveRecord(Stream: TStream);
  var i: Integer;
      F: TField;
      S, Temp: string;
  begin
    S:= '';
    for i:= 0 to ADataSet.FieldCount - 1 do
    begin
      F:= ADataset.Fields[i];
      if F.Visible then
      begin
        if S <> '' then
          S:= S + #9;
        Temp:= F.AsString;
        if (ADataset.Fields[i].DataType in [ftString, ftMemo, ftDate, ftDateTime, ftTime]) then
          Temp:= AnsiQuotedStr(Temp, '"');
        Temp:= StringReplace(Temp, #13#10, ' ', [rfReplaceAll]);
        Temp:= StringReplace(Temp, #13, ' ', [rfReplaceAll]);
        Temp:= StringReplace(Temp, #10, ' ', [rfReplaceAll]);
        S:= S + Temp;
      end;
    end;
    S:= S + #13#10;
    Stream.Write(S[1], Length(S));
  end;
  procedure SaveHeader(Stream: TStream);
  var i: Integer;
      F: TField;
      S: string;
  begin
    S:= '';
    for i:= 0 to ADataset.FieldCount - 1 do
    begin
      F:= ADataset.Fields[i];
      if F.Visible then
      begin
        if S <> '' then
          S:= S + #9;
        S:= S + F.DisplayLabel;
      end;
    end;
    S:= S + #13#10;
    Stream.Write(S[1], Length(S));
  end;
var
  F: TMemoryStream;
  k: Byte;
  Bmk : TBookmarkStr;
begin
  if DisableControls then
    ADataset.DisableControls;
  Bmk := ADataset.Bookmark;
  F:= TMemoryStream.Create;
  try
    SaveHeader(F);
    ADataset.First;
    while not ADataset.EOF do
    begin
      SaveRecord(F);
      ADataset.Next;
    end;
    k := 0;
    F.Write(k, 1);
    SetString(Result, PChar(F.Memory), F.Size);
    ADataset.Bookmark := Bmk;
  finally
    if DisableControls then
      ADataset.EnableControls;
    F.Free;
  end;
end;

procedure StringToDataset(ADataset : TDataset; AString : string; AFieldSeparator : string; AWithHeaders : Boolean);
var
  AStr : TStringList;
  AFields : TStringList;
  AValues : TStringList;
  AValue : string;
  i, j, Debut : Integer;
const
  msgDatasetPasOuvert = 'Le dataset est fermé, il doit être ouvert avant de commencer l''importation';
  msgFichierNExistePas = 'Le fichier "%s" n''existe pas';
  msgFieldSeparatorManquant = 'Séparateur de champ non-renseigné, importation impossible';
  msgFieldSeparatorAuMilieu = 'Erreur dans le fichier (présence du séparateur de champs en milieu d''enregistrement)';
  msgLineSeparatorAuMilieu = 'Erreur dans le fichier (présence d''un saut de ligne en milieu d''enregistrement)';
begin
  {Le texte doit contenir les noms de colonnes dans la première ligne}
  {ADataset doit posséder les mêmes noms de colonnes que le fichier}
  {Si un nom de colonne n'est pas trouvé dans le Dataset, la colonne est ignorée}

  {ADataset est supposé ouvert}
  if not ADataset.Active then
    raise Exception.Create(msgDatasetPasOuvert);
  {Le séparateur de champ doit au moins avoir un caractère}
  if Length(AFieldSeparator) = 0 then
    raise Exception.Create(msgFieldSeparatorManquant);

  ADataset.DisableControls;
  try
    AStr := TStringList.Create;
    AFields := TStringList.Create;
    AValues := TStringList.Create;
    try
      AStr.Text := AString;
      if AWithHeaders then
      begin
        Debut := 1;
        AFields.Text := StringReplace(AStr.Strings[0], AFieldSeparator, #13#10, [rfReplaceAll]);
        {Les guillemets sont ignorées et supprimées}
        AFields.Text := StringReplace(AFields.Text, '"', '', [rfReplaceAll]);
      end
      else
      begin
        Debut := 0;
        for i := 0 to ADataset.Fields.Count - 1 do
          AFields.Add(ADataset.Fields[i].FieldName);
      end;

      for i := Debut to AStr.Count - 1 do
      begin
        AValues.Text := StringReplace(AStr.Strings[i], AFieldSeparator, #13#10, [rfReplaceAll]);

        {Il ne faut pas de saut de ligne en milieu d'enregistrement}
        if AValues.Count < AFields.Count then
          raise Exception.Create(msgLineSeparatorAuMilieu);

        {Il ne faut pas de FieldSeparator sur-numéraires}
        if AValues.Count > AFields.Count then
          raise Exception.Create(msgFieldSeparatorAuMilieu);

        ADataset.Append;
        for j := 0 to AFields.Count - 1 do
        begin
          AValue := StringReplace(AValues[j], '"', '', [rfReplaceAll]);

          if ADataset.FieldByName(Trim(AFields[j])).DataType = ftBoolean then
            ADataset.FieldByName(Trim(AFields[j])).AsBoolean := (AValue <> '0')
          else
            ADataset.FieldByName(Trim(AFields[j])).AsString := AValue;
        end;
        ADataset.Post;
      end;
      ADataset.First;
    finally
      AValues.Free;
      AFields.Free;
      AStr.Free;
    end;
  finally
    ADataset.EnableControls;
  end;
end;

end.
