unit GzBatch;

interface

Uses
  SysUtils, Classes, Windows, GZChkDir, ShellAPI,
  CreerLnk, FGFiles, AStrUtil, IniFiles, eMail;

procedure ExecuteScript(AScript : TStrings);

procedure ScriptCOPYFILE(const Params : Array of string);
procedure ScriptCOPYFILES(const Params : Array of string);
procedure ScriptMOVEFILE(const Params : Array of string);
procedure ScriptRENAMEFILE(const Params : Array of string);
procedure ScriptDELETEFILE(const Params : Array of string);
procedure ScriptDELETEFILES(const Params : Array of string);
procedure ScriptMAKEDIR(const Params : Array of string);
procedure ScriptDELETEDIR(const Params : Array of string);
procedure ScriptSHELLEXECUTE(const Params : Array of string);
procedure ScriptCREATESHORTCUT(const Params : Array of string);
procedure ScriptDELETELINES(const Params : Array of string);
procedure ScriptINSERTLINE(const Params : Array of string);
procedure ScriptDELETETEXT(const Params : Array of string);
procedure ScriptREPLACETEXT(const Params : Array of string);
procedure ScriptADDTEXT(const Params : Array of string);
procedure ScriptADDLINE(const Params : Array of string);
procedure ScriptWRITEINISTRING(const Params : Array of string);
procedure ScriptSENDMAIL(const Params : Array of string);
procedure ScriptMAILRECIPIENTS(const Params : Array of string);
procedure ScriptMAILATTACHMENTS(const Params : Array of string);
procedure ScriptMAILTEXT(const Params : Array of string);
procedure ScriptMAILSUBJECT(const Params : Array of string);
procedure ScriptMAILPROFILE(const Params : Array of string);
procedure ScriptLOGFILENAME(const Params : Array of string);
procedure ScriptSAVELOG(const Params : Array of string);
procedure ScriptMESSAGEBOX(const Params : Array of string);

type
  TScriptProcedure = procedure (const Params : Array of string);

  TScriptDescr = record
    Name: string;
    Proc : TScriptProcedure;
  end;

const
  NbProcs = 26;
  ScriptProcedures: Array[0..NbProcs-1] of TScriptDescr = (
    (Name:'COPYFILE';Proc:ScriptCopyFile),
    (Name:'COPYFILES';Proc:ScriptCopyFiles),
    (Name:'MOVEFILE';Proc:ScriptMoveFile),
    (Name:'RENAMEFILE';Proc:ScriptRenameFile),
    (Name:'DELETEFILE';Proc:ScriptDeleteFile),
    (Name:'DELETEFILES';Proc:ScriptDeleteFiles),
    (Name:'MAKEDIR';Proc:ScriptMakeDir),
    (Name:'DELETEDIR';Proc:ScriptDeleteDir),
    (Name:'SHELLEXECUTE';Proc:ScriptShellExecute),
    (Name:'CREATESHORTCUT';Proc:ScriptCreateShortCut),
    (Name:'DELETELINES';Proc:ScriptDeleteLines),
    (Name:'ADDLINE';Proc:ScriptAddLine),
    (Name:'INSERTLINE';Proc:ScriptInsertLine),
    (Name:'DELETETEXT';Proc:ScriptDeleteText),
    (Name:'ADDTEXT';Proc:ScriptAddText),
    (Name:'REPLACETEXT';Proc:ScriptReplaceText),
    (Name:'WRITEINISTRING';Proc:ScriptWriteIniString),
    (Name:'SENDMAIL';Proc:ScriptSendMail),
    (Name:'MAILRECIPIENTS';Proc:ScriptMailRecipients),
    (Name:'MAILATTACHMENTS';Proc:ScriptMailAttachments),
    (Name:'MAILTEXT';Proc:ScriptMailText),
    (Name:'MAILSUBJECT';Proc:ScriptMailSubject),
    (Name:'MAILPROFILE';Proc:ScriptMailProfile),
    (Name:'LOGFILENAME';Proc:ScriptLogFileName),
    (Name:'SAVELOG';Proc:ScriptSaveLog),
    (Name:'MESSAGEBOX';Proc:ScriptMESSAGEBOX)
    );

var
  ScriptLogger: TStringList;
  ActiveCommand: string;
  DebugScript: Boolean;

implementation

const
  idFinTableauParametres = '(FINTABLEAUPARAMETRES)';

procedure LogError(Message : string);
begin
  ScriptLogger.Add('[' + ActiveCommand + ']: ' + Message);
end;

{Pour faire en sorte que les espaces ne soient pas en double...}
{WordByPos ne fonctionne pas comme il faut pour ce cas
si le séparateur est en double}
function ArrangerCaracteres(AChaine : string; AChar: Char) : string;
var
  i : integer;
  Espace : Boolean;
begin
  Result:='';
  Espace:=False;
  AChaine:=Trim(AChaine);
  for i:=1 to length(AChaine) do
  begin
    if AChaine[i]=AChar then
    begin
      if not Espace then
      begin
        Espace:=True;
        Result:=Result+AChaine[i];
      end;
    end
    else
    begin
      Espace:=False;
      Result:=Result+AChaine[i];
    end;
  end;
end;

procedure ExecuteScript(AScript : TStrings);
var
  i: Integer;
  Command: String;
  ProcRec: TScriptDescr;
  Params: Array[0..20] of string;
  NbParams: Integer;
  function TrouverCommande: Boolean;
  var
    k : integer;
  begin
    Result:= False;
    for k:= 0 to NbProcs-1 do
    begin
      if UpperCase(ScriptProcedures[k].Name) = UpperCase(Command) then
      begin
        ProcRec:= ScriptProcedures[k];
        Result:= True;
        Exit;
      end;
    end;
  end;
  procedure ParseParams;
  var
    d,k: Integer;
    Guillemets : Boolean;
  begin
    NbParams:= 0;
    {Trouver le 1er caractère après la commande}
    //if Length(ActiveCommand) <= Length(Command) + 1 then
    //  Exit;
    k:= Length(Command) + 2;
    while (k < Length(ActiveCommand)) and (ActiveCommand[k] = #32) do
      inc(k);
    d:= k;
    Guillemets:= False;
    while k <= Length(ActiveCommand) do
    begin
      if not Guillemets then
      begin
        if (ActiveCommand[k] = #32) or (k = Length(ActiveCommand)) then {fin de paramètre}
        begin
          Inc(NbParams);
          Params[NbParams-1]:=trim(Copy(ActiveCommand,d,k-d+1));
          {Déplacement jusqu'au prochain caractère non-espace}
          inc(k);
          while (k < Length(ActiveCommand)) and (ActiveCommand[k] = #32) do
            inc(k);
          d:= k;
        end
        else
        begin
          if ActiveCommand[k] = #34 then {Début de guillemets}
            Guillemets:= True;
          inc(k);
        end;
      end
      else
      begin
        if (ActiveCommand[k] = #34) or (k = Length(ActiveCommand)) then {fin de paramètre}
        begin
          {Traitement des doubles "}
          if (k<Length(ActiveCommand)) and (ActiveCommand[k+1] = #34) then
            inc(k,2)
          else
          begin
            Guillemets:= False;
            Inc(NbParams);
            Params[NbParams-1]:=Trim(Copy(ActiveCommand, d, k-d+1));
            {Suppression guillemet gauche}
            if (Params[NbParams-1]<>'') and (Params[NbParams-1][1] = #34) then
              Params[NbParams-1]:= Copy(Params[NbParams-1],2,Length(Params[NbParams-1])-1);
            {Suppression guillemet droite}
            if (Params[NbParams-1]<>'') and (Params[NbParams-1][Length(Params[NbParams-1])] = #34) then
              Params[NbParams-1]:= Copy(Params[NbParams-1],1,Length(Params[NbParams-1])-1);
            Params[NbParams-1]:= ReplaceText('""','"',Params[NbParams-1]);
            {Déplacement jusqu'au prochain caractère non-espace}
            inc(k);
            while (k < Length(ActiveCommand)) and (ActiveCommand[k] = #32) do
              inc(k);
            d:= k;
          end;
        end
        else
          inc(k);
      end;
    end;
  end;
  function FindCommandName : string;
  var
    k: Integer;
  begin
    for k:= 2 to Length(ActiveCommand) do
    begin
      if ActiveCommand[k] < #35 then
      begin
        Result:= Trim(Copy(ActiveCommand, 1, k-1));
        Exit;
      end;
    end;
    Result:= Trim(ActiveCommand);
  end;
begin
  ScriptLogger.Clear;
  ScriptLogger.Add('Début d''exécution: '+DateTimeToStr(Now));
  for i:=0 to AScript.Count-1 do
  begin
    ActiveCommand:= Trim(AScript[i]);
    Command:= FindCommandName;
    if not ((ActiveCommand='') or (UpperCase(Command)='REM') or (UpperCase(Command[1])=';')) then
    begin
      if Not TrouverCommande then
        LogError('Commande inconnue')
      else
      begin
        ParseParams;
        Params[NbParams]:=idFinTableauParametres;
        ProcRec.Proc(Params);
      end;
    end;
  end;
  ScriptLogger.Add('Fin d''exécution: '+DateTimeToStr(Now));
  ScriptSaveLog(['']);
end;

function CheckNull(const AStr : string): string;
begin
  if UpperCase(AStr)='(NULL)' then
    Result:=''
  else
    Result:=AStr;
end;

function CompterParams(Params: array of string) : Integer;
var
  i: Integer;
begin
  Result:= High(Params);
  for i:=0 to High(Params) do
  begin
    if Params[i] = idFinTableauParametres then
    begin
      Result:= i;
      Exit;
    end;
  end;
end;

function CheckParams(Nb : Integer;const Params : array of string) : Boolean;
begin
  Result:=True;
  if CompterParams(Params) < Nb then
  begin
    LogError('Nombre de paramètres incorrect (' + IntToStr(Nb) + ' attendus)');
    Result:=False;
  end;
end;

procedure ScriptCOPYFILE(const Params : array of string);
var
  FichierSource, FichierDestination, StopSiExiste : string;
begin
  if not CheckParams(2,Params) then
    Exit;
  FichierSource:= CheckPath(CheckNull(Params[0]));
  FichierDestination:= CheckPath(CheckNull(Params[1]));
  StopSiExiste:=Copy(UpperCase(Params[2]),1,1);
  if StopSiExiste <> 'Y' then
    StopSiExiste:='N';
  if (StopSiExiste = 'Y') and (FileExists(FichierDestination)) then
  begin
    LogError('Le fichier destination existe déjà');
    Exit;
  end
  else
  if not FileExists(FichierSource) then
  begin
    LogError('Le fichier source n''existe pas');
    Exit;
  end;
  if DebugScript then
    Exit;
  try
    FGFiles.CopyFile(FichierSource, FichierDestination, False);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptCOPYFILES(const Params : array of string);
var
  FichiersSources,
  RepertoireDestination,
  AvecSousRepertoires,
  OptionRemplacement,
  Exclusif,
  AutoCreerRepertoireDestination : string;
  LstDirs : TStringList;
  LstFiles : TStringList;
  FinalName : string;
  i, k : Integer;
  FaireCopie : Boolean;
begin
  if not CheckParams(2,Params) then
    Exit;
  FichiersSources := CheckPath(CheckNull(Params[0]));
  RepertoireDestination := ExtractFilePath(AddSlash(CheckPath(CheckNull(Params[1]))));
  AvecSousRepertoires := Copy(UpperCase(Params[2]),1,1); {DEFAUT=N}
  OptionRemplacement := UpperCase(Params[3]); {Défaut=REPLACEALL}
  Exclusif := Copy(UpperCase(Params[4]),1,1); {DEFAUT=N}
  AutoCreerRepertoireDestination := Copy(UpperCase(Params[5]),1,1); {DEFAUT=Y}
  if AvecSousRepertoires <> 'Y' then
    AvecSousRepertoires := 'N';
  if Exclusif <> 'Y' then
    Exclusif := 'N';
  if AutoCreerRepertoireDestination <> 'N' then
    AutoCreerRepertoireDestination := 'Y';

  if AutoCreerRepertoireDestination = 'N' then
  begin
    if not DirectoryExists(RepertoireDestination) then
    begin
      LogError('Le répertoire de destination n''existe pas, copie impossible');
      Exit;
    end;
  end;

  if DebugScript then
    Exit;

  try
    k := 0;
    LstDirs := TStringList.Create;
    LstFiles := TStringList.Create;
    try
      if AvecSousRepertoires = 'Y' then
      begin
        {Remplissage de la liste des sous-répertoires}
        FGFiles.FindFiles(ExtractFilePath(FichiersSources), '*.*', faDirectory + faHidden + faSysFile + faArchive + faReadOnly, True, LstDirs);
      end;

      {Remplissage de la liste des fichiers à copier}
      if FGFiles.FindFiles(ExtractFilePath(FichiersSources), ExtractFileName(FichiersSources), faHidden + faSysFile + faArchive + faReadOnly, AvecSousRepertoires = 'Y', LstFiles) then
      begin
        if not DirectoryExists(RepertoireDestination) then
          ForceDirectories(RepertoireDestination);
        {Création des sous-répertoires (et donc, même les vides)}
        if LstDirs.Count > 0 then
        begin
          for i := 0 to LstDirs.Count - 1 do
          begin
            if DirectoryExists(LstDirs[i]) then
            begin
              FinalName := ReconcileDir(LstDirs[i], ExtractFilePath(FichiersSources), RepertoireDestination);
              if FinalName <> '' then
                ForceDirectories(FinalName);
            end;
          end;
        end;
        {Copie des fichiers}
        for i := 0 to LstFiles.Count - 1 do
        begin
          if FileExists(LstFiles[i]) then
          begin
            FinalName := ReconcileDir(LstFiles[i], ExtractFilePath(FichiersSources), RepertoireDestination);
            if FinalName <> '' then
            begin
              {Test en fonction de l'option de remplacement}
              FaireCopie := True;
              if (OptionRemplacement = 'REPLACENONE') and
                (FileExists(FinalName)) then
              begin
                FaireCopie := False;
              end;
              if (OptionRemplacement = 'REPLACEOLDER') and
                (FileExists(FinalName)) and
                (FileAge(LstFiles[i]) <= FileAge(FinalName)) then
              begin
                FaireCopie := False;
              end;
              if FaireCopie then
              begin
                try
                  if not FGFiles.CopyFile(LstFiles[i], FinalName, Exclusif = 'Y') then
                  begin
                    raise Exception.Create(Format('La copie du fichier "%s" a échouée',[LstFiles[i]]));
                  end
                  else
                    Inc(k);
                except
                  on E : Exception do
                    LogError(Format('Fichier "%s", %s', [LstFiles[i], E.Message]));
                end;
              end;
            end;
          end;
        end;
      end;
      LogError(IntToStr(k)+' fichier(s) copié(s)');

    finally
      LstDirs.Free;
      LstFiles.Free;
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;


procedure ScriptMOVEFILE(const Params : Array of string);
var
  FichierSource, FichierDestination: string;
begin
  if not CheckParams(2,Params) then
    Exit;
  FichierSource:= CheckPath(CheckNull(Params[0]));
  FichierDestination:= CheckPath(CheckNull(Params[1]));
  if not FileExists(FichierSource) then
  begin
    LogError('Le fichier source n''existe pas');
    Exit;
  end;
  if DebugScript then
    Exit;
  try
    MoveFile(PChar(FichierSource), PChar(FichierDestination));
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptRENAMEFILE(const Params : Array of string);
var
  FichierSource, FichierDestination: string;
begin
  if not CheckParams(2,Params) then
    Exit;
  FichierSource:= CheckPath(CheckNull(Params[0]));
  FichierDestination:= CheckPath(CheckNull(Params[1]));
  if not FileExists(FichierSource) then
  begin
    LogError('Le fichier source n''existe pas');
    Exit;
  end;
  if DebugScript then
    Exit;
  try
    RenameFile(FichierSource, FichierDestination);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptDELETEFILE(const Params : Array of string);
var
  FichierASupprimer: string;
begin
  if not CheckParams(1,Params) then
    Exit;
  FichierASupprimer:= CheckPath(Params[0]);
  if not FileExists(FichierASupprimer) then
  begin
    LogError('Le fichier à supprimer n''existe pas');
    Exit;
  end;
  if DebugScript then
    Exit;
  try
    DeleteFile(PChar(FichierASupprimer));
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptDELETEFILES(const Params : Array of string);
var
  FichiersASupprimer,
  AvecSousRepertoires : string;
  LstFiles : TStringList;
  i, k : Integer;
begin
  if not CheckParams(1,Params) then
    Exit;
  FichiersASupprimer:= CheckPath(Params[0]);
  AvecSousRepertoires := Copy(UpperCase(Params[1]),1,1); {DEFAUT=N}
  if AvecSousRepertoires <> 'Y' then
    AvecSousRepertoires := 'N';
  if DebugScript then
    Exit;
  try
    k:=0;
    LstFiles := TStringList.Create;
    try
      if FGFiles.FindFiles(ExtractFilePath(FichiersASupprimer), ExtractFileName(FichiersASupprimer), faArchive + faSysFile + faReadOnly + faHidden, AvecSousRepertoires = 'Y', LstFiles) then
      begin
        for i := 0 to LstFiles.Count - 1 do
        begin
          if DeleteFile(PChar(LstFiles[i])) then
            Inc(k);
        end;
      end;
      LogError(IntToStr(k)+' fichier(s) effacé(s)');
    finally
      LstFiles.Free;
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMAKEDIR(const Params : Array of string);
var
  Repertoire : string;
begin
  if not CheckParams(1,Params) then
    Exit;
  Repertoire:= CheckPath(Params[0]);
  if DebugScript then
    Exit;
  try
    ForceDirectories(Repertoire);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptDELETEDIR(const Params : Array of string);
var
  Repertoire, AvecSousRepertoires, AvecFichiers : string;
  Lst: TStringList;
begin
  if not CheckParams(3,Params) then
    Exit;
  Repertoire:= CheckPath(Params[0]);
  AvecSousRepertoires:= UpperCase(CheckNull(Params[1])+'N')[1];
  AvecFichiers:= UpperCase(CheckNull(Params[2])+'N')[1];
  if Not DirectoryExists(Repertoire) then
  begin
    LogError('Répertoire inexistant');
    Exit;
  end;
  if DebugScript then
    Exit;
  try
    if AvecSousRepertoires='Y' then
    begin
      {On supprime tout, équivalent de DelTree !}
      Lst:=TStringList.Create;
      try
        FindFiles(Repertoire, '*.*', faAnyFile, True, Lst);
        FGFiles.DeleteFiles(Lst, True);
        if Not RemoveDir(Repertoire) then
          LogError('Suppression du répertoire échouée');
      finally
        Lst.Free;
      end;
    end
    else
    if AvecFichiers='Y' then
    begin
      {On efface les fichiers puis on tente d'effacer le répertoire}
      Lst:=TStringList.Create;
      try
        FindFiles(Repertoire, '*.*', faAnyFile, False, Lst);
        FGFiles.DeleteFiles(Lst, False);
        if Not RemoveDir(Repertoire) then
          LogError('Suppression du répertoire échouée');
      finally
        Lst.Free;
      end;
    end
    else
    begin
      {On tente simplement d'effacer le répertoire}
      if Not RemoveDir(Repertoire) then
        LogError('Suppression du répertoire échouée');
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptSHELLEXECUTE(const Params : Array of string);
var
  openprint, FichierAOuvrir, Parametres, RepDefaut : string;
begin
  if not CheckParams(3,Params) then
    Exit;
  openprint:= CheckPath(CheckNull(Params[0]));
  FichierAOuvrir:= CheckPath(Params[1]);
  RepDefaut:= ExtractFilePath(FichierAOuvrir);
  Parametres:= CheckPath(CheckNull(Params[2]));
  if not FileExists(FichierAOuvrir) then
  begin
    LogError('Le fichier à ouvrir n''existe pas');
    Exit;
  end;
  if DebugScript then
    Exit;
  try
    if (UpperCase(openprint)<>'OPEN') and (UpperCase(openprint)<>'PRINT') then
      openprint:= 'open';
    ShellExecute(0,PChar(openprint),PChar(FichierAOuvrir),PChar(Parametres),PChar(RepDefaut),SW_SHOWNORMAL);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptCREATESHORTCUT(const Params : Array of string);
var
  FichierAExecuter, Arguments, CheminTravail, FichierRaccourci, MontrerFenetre : string;
  ShowCmd : Integer;
begin
  if not CheckParams(4,Params) then
    Exit;
  FichierAExecuter:= CheckPath(Params[0]);
  Arguments:= CheckNull(CheckPath(Params[1]));
  CheminTravail:= CheckNull(CheckPath(Params[2]));
  FichierRaccourci:= CheckPath(Params[3]);
  MontrerFenetre:= Params[4];

  if DebugScript then
    Exit;
  try
    if MontrerFenetre = 'MAXIMIZED' then
      ShowCmd := SW_SHOWMAXIMIZED
    else
    if MontrerFenetre = 'MINIMIZED' then
      ShowCmd := SW_SHOWMINNOACTIVE
    else
      ShowCmd := SW_SHOWNORMAL;
    CreerRaccourci(FichierAExecuter, Arguments,CheminTravail,FichierRaccourci, ShowCmd);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

function RemplirMots(Debut: Integer; var Mots: Array of string;const Params: Array of string) : Integer;
var
  i : Integer;
begin
  Result:= 0;
  i:= Debut;
  while (Params[i] <> idFinTableauParametres) and (i<6) do
  begin
    Mots[i-Debut]:=Params[i];
    inc(Result);
    inc(i);
  end;
end;

function FindWords(Mots: array of string;NbMots: Integer;Ligne: string): Boolean;
var
  i : Integer;
begin
  Ligne:=UpperCase(Ligne);
  Result:=False;
  for i:=0 to NbMots-1 do
    if (Mots[i] <> '') and (Pos(UpperCase(Mots[i]),Ligne)<=0) then
      Exit;
  Result:=True;
end;

type
  TModifyProc = procedure(Params : Array of string; var NoL : Integer; StrLst : TStrings);

procedure ScriptModifyTextFile(OnceOnly: Boolean;Mots: array of string;NbMots: Integer;Fichier: String;Params: array of string;ExecProc: TModifyProc);
var
  LstFiles : TStringList;
  i : Integer;
  procedure DoItWithThisFile(FichierFinal : string);
  var
    i: Integer;
    AF : TStringList;
  begin
    try
      AF:=TStringList.Create;
      try
        AF.LoadFromFile(FichierFinal);
        i:=0;
        While i < AF.Count do
        begin
          if FindWords(Mots, NbMots, AF[i]) then
          begin
            ExecProc(Params,i,AF);
            if OnceOnly then
              Break;
          end;
          inc(i);
        end;
        AF.SaveToFile(FichierFinal);
      finally
        AF.Free;
      end;
    except
      on E : Exception do
        LogError(Format('"%s", %s', [FichierFinal, E.Message]));
    end;
  end;
begin
  if NbMots=0 then
  begin
    LogError('Clef de recherche indéfinie');
    Exit;
  end;
  try
    LstFiles := TStringList.Create;
    try
      if FGFiles.FindFiles(ExtractFilePath(Fichier), ExtractFileName(Fichier), faHidden + faArchive + faSysFile + faReadOnly, False, LstFiles) then
      begin
        if DebugScript then
          Exit;
        for i := 0 to LstFiles.Count - 1 do
          if FileExists(LstFiles[i]) then
            DoItWithThisFile(LstFiles[i]);
      end
      else
      begin
        LogError('Aucun fichier concerné');
        Exit;
      end;
    finally
      LstFiles.Free;
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ExecProcDeleteLines(Params : Array of string; var NoL : Integer; StrLst : TStrings);
begin
  StrLst.Delete(NoL);
  Dec(NoL);
end;

procedure ScriptDELETELINES(const Params : Array of string);
var
  Fichier,Vide: string;
  Mots: Array[0..5] of string;
  NbMots: Integer;
begin
  if not CheckParams(2,Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  Vide:= '';
  NbMots:= RemplirMots(1,Mots,Params);
  ScriptModifyTextFile(False, Mots, NbMots, Fichier, [Vide], ExecProcDeleteLines);
end;

procedure ExecProcInsertLine(Params : Array of string; var NoL : Integer; StrLst : TStrings);
begin
  StrLst.Insert(NoL, Params[0]);
end;

procedure ScriptINSERTLINE(const Params : Array of string);
var
  Fichier, NouvelleLigne: string;
  Mots: Array[0..5] of string;
  NbMots: Integer;
begin
  if not CheckParams(3, Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  NouvelleLigne:= CheckPath(CheckNull(Params[1]));
  NbMots:= RemplirMots(2, Mots, Params);
  ScriptModifyTextFile(True, Mots, NbMots, Fichier, [NouvelleLigne], ExecProcInsertLine);
end;

procedure ExecProcDeleteText(Params : Array of string; var NoL : Integer; StrLst : TStrings);
begin
  StrLst[NoL]:= DeleteText(Params[0],StrLst[NoL]);
end;

procedure ScriptDELETETEXT(const Params : Array of string);
var
  Fichier, Texte: string;
  Mots: Array[0..5] of string;
  NbMots: Integer;
begin
  if not CheckParams(3, Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  Texte:= CheckPath(CheckNull(Params[1]));
  NbMots:= RemplirMots(2, Mots, Params);
  ScriptModifyTextFile(False, Mots, NbMots, Fichier, [Texte], ExecProcDeleteText);
end;

procedure ExecProcReplaceText(Params : Array of string; var NoL : Integer; StrLst : TStrings);
begin
  StrLst[NoL]:= ReplaceText(Params[0],Params[1],StrLst[NoL]);
end;

procedure ScriptREPLACETEXT(const Params : Array of string);
var
  Fichier, TexteAncien, TexteNouveau: string;
  Mots: Array[0..5] of string;
  NbMots: Integer;
begin
  if not CheckParams(4, Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  TexteAncien:= CheckPath(CheckNull(Params[1]));
  TexteNouveau:= CheckPath(CheckNull(Params[2]));
  NbMots:= RemplirMots(3, Mots, Params);
  ScriptModifyTextFile(False, Mots, NbMots, Fichier, [TexteAncien, TexteNouveau], ExecProcReplaceText);
end;

procedure ExecProcAddText(Params : Array of string; var NoL : Integer; StrLst : TStrings);
begin
  if Params[1]='END' then
    StrLst[NoL]:= StrLst[NoL] + Params[0]
  else
    StrLst[NoL]:= Params[0] + StrLst[NoL];
end;

procedure ScriptADDTEXT(const Params : Array of string);
var
  Fichier, Texte, Position: string;
  Mots: Array[0..5] of string;
  NbMots: Integer;
begin
  if not CheckParams(4, Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  Texte:= CheckPath(CheckNull(Params[1]));
  Position:= UpperCase(CheckPath(CheckNull(Params[2])));
  NbMots:= RemplirMots(3, Mots, Params);
  if (UpperCase(Position)<>'START') and (UpperCase(Position)<>'END') then
    Position:='END';
  ScriptModifyTextFile(False, Mots, NbMots, Fichier, [Texte,Position], ExecProcAddText);
end;

procedure ScriptADDLINE(const Params : Array of string);
var
  Fichier, Ligne, Position: string;
  i: Integer;
  LstFiles: TStringList;
  procedure DoItWithThisFile(FichierFinal: string);
  var
    AF : TStringList;
  begin
    AF:=TStringList.Create;
    try
      AF.LoadFromFile(FichierFinal);
      if Position = 'END' then
        AF.Add(Ligne)
      else
        AF.Insert(0,Ligne);
      AF.SaveToFile(FichierFinal);
    finally
      AF.Free;
    end;
  end;
begin
  if not CheckParams(3, Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  Ligne:= CheckPath(CheckNull(Params[1]));
  Position:= UpperCase(CheckNull(Params[2]));
  if (Position<>'START') and (Position<>'END') then
    Position:='END';
  try
    LstFiles := TStringList.Create;
    try
      if FGFiles.FindFiles(ExtractFilePath(Fichier), ExtractFileName(Fichier), faHidden + faArchive + faSysFile + faReadOnly, False, LstFiles) then
      begin
        if DebugScript then
          Exit;
        for i := 0 to LstFiles.Count - 1 do
          if FileExists(LstFiles[i]) then
            DoItWithThisFile(LstFiles[i]);
      end
      else
      begin
        LogError('Aucun fichier concerné');
        Exit;
      end;
    finally
      LstFiles.Free;
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptWRITEINISTRING(const Params : Array of string);
var
  Fichier, Section, Ligne, Valeur: string;
  AF : TIniFile;
begin
  if not CheckParams(4, Params) then
    Exit;
  Fichier:= CheckPath(Params[0]);
  Section:= CheckPath(CheckNull(Params[1]));
  Ligne:= CheckPath(CheckNull(Params[2]));
  Valeur:= CheckPath(CheckNull(Params[3]));
  if DebugScript then
    Exit;
  try
    AF:=TIniFile.Create(Fichier);
    try
      AF.WriteString(Section,Ligne,Valeur);
    finally
      AF.Free;
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;


{Gestion de l'envoi des mails}

var
  Recipients, Attachments, MailText: TStringList;
  Subject : string;
  Profile, Password : string;

const
  LogFileName : string = '($TEMP)\ScriptLog %s %s.txt';

function ResolveLogFileName: string;
begin
  Result := CheckPath(Format(LogFileName,[FormatDateTime('dd-mm-yyyy',Date), FormatDateTime('hh"h"nn',Time)]));
end;

procedure ScriptSENDMAIL(const Params : Array of string);
var
  AvecLog: string;
  AMail: TEmail;
  LongText: string;
  AError : Integer;
  procedure CheckAttachments;
  var
    i: Integer;
  begin
    i := 0;
    while i < Attachments.Count do
    begin
      if not FileExists(Attachments[i]) then
        Attachments.Delete(i)
      else
        Inc(i);
    end;
  end;
begin
  if not CheckParams(1, Params) then
    AvecLog:= 'N'
  else
    AvecLog:= CheckNull(UpperCase(Params[0]+'N')[1]);
  if (AvecLog<>'Y') and (AvecLog<>'N') then
    AvecLog:= 'N';
  if DebugScript then
    Exit;
  try
    //ScriptLogger.SaveToFile(CheckPath(LogFileName)); // Pas utilisable à cause du nom de fichier
    ScriptSAVELOG(['']);
    AMail:= TEmail.Create(nil);
    try
      if not AMail.MapiAvail then
      begin
        LogError('MAPI n''est pas disponible sur cette machine');
        Exit;
      end;
      if Profile = '' then
        AMail.UseDefProfile:= True
      else
      begin
        AMail.UseDefProfile := False;
        AMail.Profile := Profile;
        AMail.Password := Password;
      end;
      if AvecLog = 'Y' then
        Attachments.Add(ResolveLogFileName);
      CheckAttachments; // To only add valid filenames...
      AMail.Attachment.Assign(Attachments);
      AMail.Recipient.Assign(Recipients);
      LongText:= MailText.Text;
      AMail.SetLongText(PChar(LongText));
      AMail.Subject:= Subject;
      AMail.Logon;
      AError := AMail.SendMail;
      if AError <> 0 {SUCCESS_SUCCESS, SMAPI} then
        LogError('Erreur MAPI : ' + IntToStr(AError));
      AMail.Logoff;
    finally
      AMail.Free;
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMAILRECIPIENTS(const Params : Array of string);
var
  i: Integer;
begin
  Recipients.Clear;
  if not CheckParams(1,Params) then
    Exit;
  try
    for i:=0 to CompterParams(Params)-1 do
    begin
      Recipients.Add(Params[i]);
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMAILATTACHMENTS(const Params : Array of string);
var
  i: Integer;
begin
  Attachments.Clear;
  if not CheckParams(1,Params) then
    Exit;
  try
    for i:=0 to CompterParams(Params)-1 do
    begin
      Attachments.Add(CheckPath(Params[i]));
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMAILTEXT(const Params : Array of string);
var
  i: Integer;
begin
  MailText.Clear;
  if not CheckParams(1,Params) then
    Exit;
  try
    for i:=0 to CompterParams(Params)-1 do
    begin
      MailText.Add(CheckPath(Params[i]));
    end;
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMAILSUBJECT(const Params : Array of string);
begin
  Subject:= '';
  if not CheckParams(1,Params) then
    Exit;
  try
    Subject:= CheckPath(Params[0]);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMAILPROFILE(const Params : Array of string);
begin
  if not CheckParams(2,Params) then
    Exit;
  try
    Profile := CheckNull(Params[0]);
    Password := CheckNull(Params[1]);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptLOGFILENAME(const Params : Array of string);
begin
  if not CheckParams(1,Params) then
    Exit;
  try
    LogFileName:= CheckPath(Params[0]);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptSAVELOG(const Params : Array of string);
begin
  if DebugScript then
    Exit;
  try
    ScriptLogger.SaveToFile(ResolveLogFileName);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;

procedure ScriptMESSAGEBOX(const Params : Array of string);
var
  Texte, Titre: string;
begin
  if not CheckParams(2,Params) then
    Exit;
  Texte:= CheckPath(Params[0]);
  Titre:= CheckPath(Params[1]);
  if DebugScript then
    Exit;
  try
    MessageBox(0,PChar(Texte),PChar(Titre),MB_ICONINFORMATION	+ MB_OK);
  except
    on E : Exception do
      LogError(E.Message);
  end;
end;


initialization
  ScriptLogger:= TStringList.Create;
  Recipients:= TStringList.Create;
  Attachments:= TStringList.Create;
  MailText:= TStringList.Create;
  Subject:= '';
  Profile:= '';
  Password:= '';
  DebugScript:= False;
finalization
  ScriptLogger.Free;
  Recipients.Free;
  Attachments.Free;
  MailText.Free;
end.
