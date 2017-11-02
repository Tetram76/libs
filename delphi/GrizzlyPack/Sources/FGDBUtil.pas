unit FGDBUtil;

{Auteur : Frédéric GUILLIEN}
{Date : Août 1997}

interface

uses SysUtils, Classes, Controls, Forms, DB, {DBTables,} DBGrids, Graphics;

const
  msgPasFichierBDECFG = 'Fichier de configuration du BDE introuvable';
  msgMAJImpossibleLectureSeule = 'Mise à jour impossible, la table destination est en lecture seule';
  msgMAJImpossiblePasActive = 'Mise à jour impossible, la table destination n''est pas active';
  msgPasModifie = 'Vous n''avez encore renseigné aucun champ';
  msgPasDeMaitre = 'Il n''y a pas d''enregistrement maître, vous ne pouvez pas ajouter d''enregistrements';
  msgDatasetPasOuvert = 'Le dataset est fermé, il doit être ouvert avant de commencer l''importation';
  msgFichierNExistePas = 'Le fichier "%s" n''existe pas';
  msgFieldSeparatorManquant = 'Séparateur de champ non-renseigné, importation impossible';
  msgFieldSeparatorAuMilieu = 'Erreur dans le fichier (présence du séparateur de champs en milieu d''enregistrement)';
  msgLineSeparatorAuMilieu = 'Erreur dans le fichier (présence d''un saut de ligne en milieu d''enregistrement)';


{Renvoie le prochain numéro pour une clé numerique
en fonction du contenu de la table NumberTable
de structure :
NomTable  A   50  *
Numero    I
A noter que la fonction élminine l'extension de fichier si elle existe.
ATableName est aussi passer en majuscules pour éviter tte confusion}
//function GetNextTableNumber(ANumberTable: TTable; ATableName: string; AllowPost: Boolean): Longint;
//function GetNextTableNumberExclusive(ANumberTable: TTable; ATableName: string): Longint;

type
  ENextNumberError = class(Exception);
  EImportTextFileError = class(Exception);

procedure ImportTextFileToDataset(ADataset : TDataset; AFileName : string; AFieldSeparator : string);

//procedure FlushNumberTable(ANumberTable: TTable);

{Appelle l'application de configuration du BDE}
//procedure ShowBDEConfig;

//procedure VerificationAvantAjout(ATableMaitre: TTable);

procedure ActivateAllDatasets(AOwner: TComponent; ActivationValue: Boolean);

//procedure FlushAllBDEDatasets(AOwner: TComponent);

//procedure RefreshAllDatasets(AOwner: TComponent);

procedure OpenOrRefreshDataset(ADataset: TDataSet);

//procedure UpDateAuto(AQuery: TQuery; ATable: TDataSet);

procedure EnableTableControls(ADatamodule: TDatamodule; Value: Boolean);
procedure AutoDBGridColWidth(ADBGrid: TDBGrid);
procedure AutoDBGridColWidthIndex(ADBGrid: TDBGrid; Index: Integer);

implementation

uses FGUtils, IniFiles, Windows, Registry;

procedure ActivateAllDatasets(AOwner: TComponent; ActivationValue: Boolean);
var
  i: Integer;
begin
  with AOwner do
    for i:= 0 to ComponentCount - 1 do
      if Components[i] is TDataset then
        TDataset(Components[i]).Active:= ActivationValue;
end;

(*
procedure FlushAllBDEDatasets(AOwner: TComponent);
var
  i: Integer;
begin
  with AOwner do
    if ComponentCount > 0 then
      for i:= 0 to ComponentCount - 1 do
      begin
        if (Components[i] is TDataset) and ((Components[i] as TDataset).Active) then
        begin
          if (Components[i] is TQuery) then
          begin
            (Components[i] as TQuery).FlushBuffers;
          end
          else if (Components[i] is TTable) then
          begin
            (Components[i] as TTable).FlushBuffers;
          end;
        end;
      end;
end;
*)

(*
procedure RefreshAllDatasets(AOwner: TComponent);
var
  i: Integer;
begin
  with AOwner do
    if ComponentCount > 0 then
      for i:= 0 to ComponentCount - 1 do
      begin
        if (Components[i] is TDataset) and ((Components[i] as TDataset).Active) then
        begin
          if (Components[i] is TQuery) then
          begin
            (Components[i] as TQuery).Close;
            (Components[i] as TQuery).Open;
          end
          else
            (Components[i] as TDataset).Refresh;
        end;
      end;
end;
*)

(*
procedure FlushNumberTable(ANumberTable: TTable);
begin
  if ANumberTable.State in [dsEdit, dsInsert] then
  begin
    ANumberTable.Post;
    ANumberTable.FlushBuffers;
  end;
end;

function GetNextTableNumber(ANumberTable: TTable; ATableName: string; AllowPost: Boolean): Longint;
var
  AOuvert: Boolean;
begin
  ATableName:= AnsiUpperCase(ATableName);
  {Elimination du .db des tables Paradox... ou en général des extensions de fichier}
  if Pos('.', ATableName) > 0 then
    ATableName:= Copy(ATableName, 1, Pos('.', ATableName) - 1);
  if not ANumberTable.Active then
  begin
    AOuvert:= False;
    ANumberTable.Open
  end
  else
  begin
    ANumberTable.Refresh;
    AOuvert:= False;
  end;
  try
    {On utilise FindKey pour la compatibilité avec D1}
    if not ANumberTable.FindKey([ATableName]) then
    begin
      ANumberTable.Append;
      ANumberTable.FieldByName('NomTable').AsString:= ATableName;
      ANumberTable.FieldByName('Numero').AsInteger:= 1;
      Result:= 1;
    end
    else
    begin
      ANumberTable.Edit;
      ANumberTable.FieldByName('Numero').AsInteger:= ANumberTable.FieldByName('Numero').AsInteger + 1;
      Result:= ANumberTable.FieldByName('Numero').AsInteger;
    end;
    if AllowPost then
    begin
      if ANumberTable.State in [dsInsert, dsEdit] then
        ANumberTable.Post;
{$IFDEF VER100}
      if not ANumberTable.Database.IsSQLBased then
        ANumberTable.FlushBuffers;
{$ENDIF}
    end;
  finally
    if AOuvert and AllowPost then
      ANumberTable.Close;
  end;
end;

function GetNextTableNumberExclusive(ANumberTable: TTable; ATableName: string): Longint;
var
  AOldExc: Boolean;
  procedure OpenTable;
  var
    Debut: TDateTime;
  begin
    Debut:= Now;
//    ANumberTable.Open;
    {Essais pendant cinq secondes}
    while (not ANumberTable.Active) and (Now - Debut < 5 * (1 / (24 * 60 * 60))) do
    begin
      {Attente d'un tiers de seconde entre chaque essai...}
      sleep(330);
      try
        ANumberTable.Open;
      except
        {On rend complètement muet...}
      end;
    end;
    {S'il y a une erreur, Active=False !}
  end;
begin
  AOldExc:= ANumberTable.Exclusive;
  if ANumberTable.Active then
    ANumberTable.Close;
  ANumberTable.Exclusive:= True;

  ATableName:= AnsiUpperCase(ATableName);
  {Elimination du .db des tables Paradox... ou en général des extensions de fichier}
  if Pos('.', ATableName) > 0 then
    ATableName:= Copy(ATableName, 1, Pos('.', ATableName) - 1);

  OpenTable;
  if not ANumberTable.Active then
    raise ENextNumberError.Create('Affectation d''un numéro impossible, rééssayer plus tard');

  try
    {On utilise FindKey pour la compatibilité avec D1}
    if not ANumberTable.FindKey([ATableName]) then
    begin
      ANumberTable.Append;
      ANumberTable.FieldByName('NomTable').AsString:= ATableName;
      ANumberTable.FieldByName('Numero').AsInteger:= 1;
      Result:= 1;
    end
    else
    begin
      ANumberTable.Edit;
      ANumberTable.FieldByName('Numero').AsInteger:= ANumberTable.FieldByName('Numero').AsInteger + 1;
      Result:= ANumberTable.FieldByName('Numero').AsInteger;
    end;
    if ANumberTable.State in [dsInsert, dsEdit] then
      ANumberTable.Post;
{$IFDEF VER100}
    if not ANumberTable.Database.IsSQLBased then
      ANumberTable.FlushBuffers;
{$ENDIF}
  finally
    ANumberTable.Close;
    ANumberTable.Exclusive:= AOldExc;
  end;
end;
*)

(*
procedure ShowBDEConfig;
{$IFDEF VER80}
var
  LeIni: TIniFile;
  LeF: array[0..255] of Char;
  LeChemin: string;
begin
  LeIni:= TIniFile.Create('WIN.INI');
  try
    LeChemin:= LeIni.ReadString('IDAPI', 'DLLPATH', '');
    if LeChemin <> '' then
    begin
      if FileExists(AddSlash(LeChemin) + 'BDECFG.EXE') then
      begin
        StrPCopy(LeF, AddSlash(LeChemin) + 'BDECFG.EXE');
        WinExec(LeF, SW_SHOWNORMAL);
      end
      else if FileExists(AddSlash(LeChemin) + 'IDAPICFG.EXE') then
      begin
        StrPCopy(LeF, AddSlash(LeChemin) + 'IDAPICFG.EXE');
        WinExec(LeF, SW_SHOWNORMAL);
      end;
    end;
  finally
    LeIni.Free;
  end;
end;
{$ELSE}
var
  LeIni: TRegistry;
  LeChemin: string;
  LeF: array[0..255] of Char;
begin
  LeIni:= TRegistry.Create;
  try
    LeIni.RootKey:= HKEY_LOCAL_MACHINE;
    if LeIni.OpenKey('SOFTWARE\Borland\Database Engine\', False) then
    begin
      LeChemin:= AddSlash(ExtractFilePath(LeIni.ReadString('CONFIGFILE01')));
      if FileExists(LeChemin + 'BDECFG32.EXE') then
      begin
        StrPCopy(LeF, LeChemin + 'BDECFG32.EXE');
        WinExec(LeF, SW_SHOWNORMAL);
      end
      else
        raise Exception.Create(msgPasFichierBDECFG);
    end
    else
      raise Exception.Create(msgPasFichierBDECFG);
    LeIni.CloseKey;
  finally
    LeIni.Free;
  end;
end;
{$ENDIF}
*)

(*
procedure VerificationAvantAjout(ATableMaitre: TTable);
begin
  if (ATableMaitre.State = dsInsert) then
  begin
    if (ATableMaitre.Modified) then
    begin
      ATableMaitre.Post;
      ATableMaitre.Edit;
    end
    else
      raise Exception.Create(msgPasModifie);
  end
  else if ATableMaitre.RecordCount = 0 then
    raise Exception.Create(msgPasDeMaitre);
end;
*)

procedure OpenOrRefreshDataset(ADataset: TDataSet);
begin
  if ADataset.Active then
    ADataset.Refresh
  else
    ADataset.Open;
end;

procedure EnableTableControls(ADatamodule: TDatamodule; Value: Boolean);
var
  i: Integer;
begin
  with ADataModule do
  begin
    if ComponentCount > 0 then
      for i:= 0 to ComponentCount - 1 do
        if (Components[i] is TDataset) then
        begin
          if Value then
            TDataset(Components[i]).EnableControls
          else
            TDataset(Components[i]).DisableControls;
        end;
  end;
end;

(*
procedure UpDateAuto(AQuery: TQuery; ATable: TDataSet);
var
  LeF: TField;
  i: Integer;
begin
  if (AQuery.FieldCount > 0) and (ATable.FieldCount > 0) then
  begin
    if not ATable.CanModify then
      raise Exception.Create(msgMAJImpossibleLectureSeule);
    if not ATable.Active then
      raise Exception.Create(msgMAJImpossiblePasActive);
    ATable.Edit;
    try
      for i:= 0 to AQuery.FieldCount - 1 do
      begin
        LeF:= ATable.FindField(AQuery.Fields[i].FieldName);
        if (LeF <> nil) and (not LeF.ReadOnly) then
        begin
          if LeF.AsVariant <> AQuery.Fields[i].AsVariant then
            LeF.AsVariant:= AQuery.Fields[i].AsVariant;
        end;
      end;
      ATable.Post;
    except
      ATable.Cancel;
    end;
  end;
end;
*)

procedure AutoDBGridColWidth(ADBGrid: TDBGrid);
var
  AList: TList;
  ADataset: TDataset;
  ACol: TColumn;
  i, AW: Longint;
  OldFont: TFont;
  TheBM: TBookmark;
  OldCursor: TCursor;
begin
  if ADBGrid.Datasource = nil then
    Exit;
  if ADBGrid.Columns.Count = 0 then
    Exit;
  ADataset:= ADBGrid.Datasource.Dataset;
  if ADataset = nil then
    Exit;
  if not ADataset.Active then
    Exit;
  OldCursor:= Screen.Cursor;
  Screen.Cursor:= crHourGlass;
  try
    TheBM:= ADataset.GetBookmark;
    try
      OldFont:= TFont.Create;
      OldFont.Assign(ADBGrid.Canvas.Font);
      AList:= TList.Create;
      try
        for i:= 0 to ADBGrid.Columns.Count - 1 do
        begin
          AList.Add(Pointer(0));
        end;
        for i:= 0 to ADBGrid.Columns.Count - 1 do
        begin
          ACol:= ADBGrid.Columns.Items[i];
          ADBGrid.Canvas.Font.Assign(ACol.Title.Font);
          AW:= ADBGrid.Canvas.TextWidth(ACol.Title.Caption) + 6;
          if AW > Longint(AList.Items[i]) then
            AList.Items[i]:= Pointer(AW);
        end;
        ADataset.DisableControls;
        try
          ADataset.First;
          while not ADataset.EOF do
          begin
            for i:= 0 to ADBGrid.Columns.Count - 1 do
            begin
              ACol:= ADBGrid.Columns.Items[i];
              ADBGrid.Canvas.Font.Assign(ACol.Font);
              AW:= ADBGrid.Canvas.TextWidth(ACol.Field.DisplayText) + 6;
              if AW > Longint(AList.Items[i]) then
                AList.Items[i]:= Pointer(AW);
            end;
            ADataset.Next;
          end;
        finally
          ADataset.EnableControls;
        end;
        for i:= 0 to ADBGrid.Columns.Count - 1 do
        begin
          ACol:= ADBGrid.Columns.Items[i];
          if Longint(AList.Items[i]) > 0 then
            ACol.Width:= Trunc(Longint(AList.Items[i]) * 1.05);
        end;
      finally
        AList.Free;
        ADBGrid.Canvas.Font.Assign(OldFont);
        OldFont.Free;
      end;
      ADataset.GotoBookmark(TheBM);
    finally
      ADataset.FreeBookmark(TheBM);
    end;
  finally
    Screen.Cursor:= OldCursor;
  end;
end;

procedure AutoDBGridColWidthIndex(ADBGrid: TDBGrid; Index: Integer);
var
  ADataset: TDataset;
  ACol: TColumn;
  AW: Longint;
  OldFont: TFont;
  TheBM: TBookmark;
  OldCursor: TCursor;
  AMax: Integer;
begin
  if ADBGrid.Datasource = nil then
    Exit;
  if ADBGrid.Columns.Count = 0 then
    Exit;
  ADataset:= ADBGrid.Datasource.Dataset;
  if ADataset = nil then
    Exit;
  if not ADataset.Active then
    Exit;
  OldCursor:= Screen.Cursor;
  Screen.Cursor:= crHourGlass;
  try
    TheBM:= ADataset.GetBookmark;
    try
      OldFont:= TFont.Create;
      OldFont.Assign(ADBGrid.Canvas.Font);
      try
        AMax:= 0;
        ACol:= ADBGrid.Columns.Items[Index];
        ADBGrid.Canvas.Font.Assign(ACol.Title.Font);
        AW:= ADBGrid.Canvas.TextWidth(ACol.Title.Caption) + 4;
        if AW > AMax then
          AMax:= AW;
        ADataset.DisableControls;
        try
          ADataset.First;
          while not ADataset.EOF do
          begin
            ACol:= ADBGrid.Columns.Items[Index];
            ADBGrid.Canvas.Font.Assign(ACol.Font);
            AW:= ADBGrid.Canvas.TextWidth(ACol.Field.DisplayText) + 2;
            if AW > AMax then
              AMax:= AW;
            ADataset.Next;
          end;
        finally
          ADataset.EnableControls;
        end;
        ACol:= ADBGrid.Columns.Items[Index];
        if AMax > 0 then
        begin
          ACol.Width:= Trunc(Longint(AMax) * 1.05);
        end;
      finally
        ADBGrid.Canvas.Font.Assign(OldFont);
        OldFont.Free;
      end;
      ADataset.GotoBookmark(TheBM);
    finally
      ADataset.FreeBookmark(TheBM);
    end;
  finally
    Screen.Cursor:= OldCursor;
  end;
end;

procedure ImportTextFileToDataset(ADataset : TDataset; AFileName : string; AFieldSeparator : string);
var
  AStr : TStringList;
  AFields : TStringList;
  AValues : TStringList;
  AValue : string;
  i, j : Integer;
begin
  {Le fichier doit contenir les noms de colonnes dans la première ligne}
  {ADataset doit posséder les mêmes noms de colonnes que le fichier}
  {Si un nom de colonne n'est pas trouvé dans le Dataset, la colonne est ignorée}

  {ADataset est supposé ouvert}
  if not ADataset.Active then
    raise EImportTextFileError.Create(msgDatasetPasOuvert);
  {Le fichier DOIT exister évidemment}
  if not FileExists(AFileName) then
    raise EImportTextFileError.Create(Format(msgFichierNExistePas, [AFileName]));
  {Le séparateur de champ doit au moins avoir un caractère}
  if Length(AFieldSeparator) = 0 then
    raise EImportTextFileError.Create(msgFieldSeparatorManquant);

  AStr := TStringList.Create;
  AFields := TStringList.Create;
  AValues := TStringList.Create;
  try
    AStr.LoadFromFile(AFileName);
    AFields.Text := StringReplace(AStr.Strings[0], AFieldSeparator, #13#10, [rfReplaceAll]);
    {Les guillemets sont ignorées et supprimées}
    AFields.Text := StringReplace(AFields.Text, '"', '', [rfReplaceAll]);

    for i := 1 to AStr.Count - 1 do
    begin
      AValues.Text := StringReplace(AStr.Strings[i], AFieldSeparator, #13#10, [rfReplaceAll]);

      {Il ne faut pas de saut de ligne en milieu d'enregistrement}
      if AValues.Count < AFields.Count then
        raise EImportTextFileError.Create(msgLineSeparatorAuMilieu);

      {Il ne faut pas de FieldSeparator sur-numéraires}
      if AValues.Count > AFields.Count then
        raise EImportTextFileError.Create(msgFieldSeparatorAuMilieu);

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

  finally
    AValues.Free;
    AFields.Free;
    AStr.Free;
  end;
end;


end.

