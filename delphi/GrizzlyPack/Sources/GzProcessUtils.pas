unit GzProcessUtils;

{Auteur: Frederic GUILLIEN 30/9/99 - 10/2004}

interface

uses Classes, SysUtils, Windows, {FGUtils, }TLHelp32;

type
{$IFDEF WIN32}
	THandleProcess=DWord;
{$ELSE}
	THandleProcess=Word;
{$ENDIF}

{On stocke dans des Tstrings, la chaine est un libellé ou le titre, l'objet (propriété Objects) est le handle correspondant}

{Retrouve le nom du travail en cours dans une application (ex : pour "Microsoft Word - Document1" la fonction renverra Document1}
function FindProjectName(AProcessId : THandleProcess; Title : string; Separator : string; WithFraming, EraseLastWord : Boolean; ANbreMots : SmallInt;AInversion : Boolean; var HWndFenetrePrincipale : HWnd) : string;

{Retourne les listes des applications actuellement actives et désactivées}
function GetActivityList(LstBefore, LstResult, LstNew, LstEnded, LstSelection : Tstrings) : Boolean;
{LstBefore : Liste des applications actives lors de la dernière exécution de la procédure}
{LstResult : Liste des applications actives en ce moment}
{LstNew : Liste des applications nouvelles}
{LstEnded : Liste des applications disparues}
{LstSelection : Liste des applications suivies. Si vide, on suit tout}

function GetProcessExeName(AProcessId : THandleProcess): string;
function GetWindowProcess(AWindow : HWnd) : THandleProcess;
procedure GetProcessList(AList : Tstrings);
procedure GetModuleList(AList : Tstrings);
procedure GetProcessWindowList(AProcessId : DWord; AList : Tstrings);

procedure GetVisibleWindowList(AList : Tstrings);

//Build the same list as the task manager does...
procedure GetTaskList(AList : Tstrings);
//Activate a window, and restore it if necessary...
procedure ActivateWindow(AHWnd : HWnd);
//Send WM_CLOSE to all task's main window
procedure CloseAllTasks;


implementation

function FindProjectName(AProcessId : THandleProcess; Title : string; Separator : string; WithFraming, EraseLastWord : Boolean; ANbreMots : SmallInt;AInversion : Boolean;var HWndFenetrePrincipale : HWnd) : string;
var
	Lst : TstringList;
  i,j : Integer;
  procedure TraitementFinal(var AChaine : string);
  var
    j,k : Integer;
    Fin : string;
  begin
    if (ANbreMots>0) then
    begin
      Fin:=AChaine;
      AChaine:='';
      k:=0;
      j:=Pos(' ',Fin);
      while (j>0) and (k<ANbreMots) do
      begin
        Inc(k);
        AChaine:=AChaine+Copy(Fin,1,j);
        Delete(Fin,1,j);
        j:=Pos(' ',Fin);
      end;
    end;
    AChaine:=Trim(AChaine);
    if WithFraming then
    begin
      AChaine:=Trim(Copy(AChaine,2,Length(AChaine)-2));
    end;
    if EraseLastWord and (Pos(#32,AChaine)>0) then
    begin
      while AChaine[Length(AChaine)]<>#32 do
        Delete(AChaine, Length(AChaine), 1);
      Delete(AChaine, Length(AChaine), 1);
    end;
    AChaine:=Trim(AChaine);
  end;
begin
  Separator:=Copy(Separator,2,Length(Separator)-2);
 	Result:='';
	Lst:=TstringList.Create;
  try
		GetProcessWindowList(AProcessId,Lst);
    if Lst.Count>0 then
    begin
	    for i:=0 to Lst.Count-1 do
      begin
        if Not AInversion then
        begin
          if (Copy(Lst[i],1,Length(Title+Separator))=Title+Separator) and (Length(Lst[i])>Length(Title+Separator)) and (Result='') then
          begin
          	HWndFenetrePrincipale:=HWnd(Longint(Lst.Objects[i]));
  					j:=Pos(Separator,Lst[i])+Length(Separator);
            Result:=Trim(Copy(Lst[i],j,Length(Lst[i])-j+1));
            TraitementFinal(Result);
            Exit;
          end;
        end
        else
        {Le nom de projet est avant le nom de l'application...}
        begin
          if (Pos(Separator+Title,Lst[i])>0) and (Length(Lst[i])>Length(Title+Separator)) and (Result='') then
          begin
          	HWndFenetrePrincipale:=HWnd(Longint(Lst.Objects[i]));
  					j:=Pos(Separator,Lst[i]);
            Result:=Trim(Copy(Lst[i],1,j-1));
            TraitementFinal(Result);
            Exit;
          end;
        end;
      end;
    end;
  finally
  	Lst.Free;
  end;
end;

function GetActivityList(LstBefore, LstResult, LstNew, LstEnded, LstSelection : Tstrings) : Boolean;
var
	i,j : Integer;
  LstB2 : TstringList;
begin
	Result:=False;
	if (LstBefore=Nil) or (LstResult=Nil) then
  begin
  	Raise Exception.Create('LstBefore and LstResult must be assigned !');
  end;
  LstBefore.BeginUpDate;
  LstResult.BeginUpDate;
	if LstEnded<>Nil then
	  LstEnded.BeginUpDate;
  if LstNew<>Nil then
	  LstNew.BeginUpDate;
  try
	  LstB2:=TstringList.Create;
	  try
	  	LstB2.Assign(LstBefore);
			{On crée la liste des applications actives dans LstResult}
			GetProcessList(LstResult);
		  Result:=True;
		  {Elimination de ce qui n'est pas dans LstSelection}
			if (LstSelection<>Nil) and (LstSelection.Count>0) and (LstResult.Count>0) then
		  begin
		  	i:=0;
		  	while (i<LstResult.Count) and (LstResult.Count>0)do
		    begin
		    	j:=LstSelection.IndexOf(LstResult[i]);
		      if j<0 then
		      begin
		      	LstResult.Delete(i);
		        Dec(i);
		      end;
		      Inc(i);
		    end;
		  end;
      {On crée la liste des applications nouvelles dans LstNew}
      if LstNew<>Nil then
      begin
        LstNew.Assign(LstResult);
		    if (LstResult.Count>0) and (LstB2.Count>0) then
		    begin
		    	for i:=0 to LstB2.Count-1 do
		      begin
		        j:=LstNew.IndexOf(LstB2[i]);
		        if j>=0 then
		        	LstNew.Delete(j);
		      	if LstNew.Count=0 then
		        	Break;
		      end;
		    end;
      end;
		  {On crée la liste des applications disparues dans LstEnded}
			if LstEnded<>Nil then
		  begin
				LstEnded.Assign(LstB2);
		    if (LstResult.Count>0) then
		    begin
		    	for i:=0 to LstResult.Count-1 do
		      begin
		        j:=LstEnded.IndexOf(LstResult[i]);
		        if j>=0 then
		        	LstEnded.Delete(j);
		      	if LstEnded.Count=0 then
		        	Break;
		      end;
		    end;
		  end;
	  finally
	  	LstB2.Free;
	  end;
  finally
	  LstBefore.EndUpDate;
	  LstResult.EndUpDate;
		if LstEnded<>Nil then
		  LstEnded.EndUpDate;
		if LstNew<>Nil then
		  LstNew.EndUpDate;
  end;
end;

function GetProcessExeName(AProcessId : THandleProcess): string;
var
	AProcess : TProcessEntry32;
  Retour : Boolean;
	hSnapshot : THandle;
begin
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapShot=0 then
  	Exit;
 	try
		AProcess.dwSize:=SizeOf(TProcessEntry32);
	  Retour:=Process32First(hSnapShot, AProcess);
	  while Retour do
	  begin
	  	if AProcess.th32ProcessId=AProcessId then
      begin
        Result:=StrPas(AProcess.szExeFile);
        Exit;
      end;
		  Retour:=Process32Next(hSnapShot, AProcess);
	  end;
  finally
	  CloseHandle(hSnapShot);
  end;
end;

function GetWindowProcess(AWindow : HWnd) : THandleProcess;
begin
	GetWindowThreadProcessId(AWindow,@Result);
end;

procedure GetProcessList(AList : Tstrings);
var
	AProcess : TProcessEntry32;
  Retour : Boolean;
	hSnapshot : THandle;
begin
	if AList=Nil then
  	Exit;
  AList.BeginUpDate;
  try
		AList.Clear;
	  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	  if hSnapShot=0 then
	  	Exit;
  	try
			AProcess.dwSize:=SizeOf(TProcessEntry32);
		  Retour:=Process32First(hSnapShot, AProcess);
		  while Retour do
		  begin
		  	AList.AddObject(StrPas(AProcess.szExeFile),Pointer(AProcess.th32ProcessId));
			  Retour:=Process32Next(hSnapShot, AProcess);
		  end;
    finally
		  CloseHandle(hSnapShot);
    end;
  finally
  	AList.EndUpDate;
  end;
end;

procedure GetModuleList(AList : Tstrings);
var
	AModule : TModuleEntry32;
  Retour : Boolean;
	hSnapshot : THandle;
begin
	if AList=Nil then
  	Exit;
  AList.BeginUpDate;
  try
		AList.Clear;
	  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, 0);
		if hSnapShot=0 then
		 	Exit;
    try
			AModule.dwSize:=SizeOf(TModuleEntry32);
		  Retour:=Module32First(hSnapShot, AModule);
		  while Retour do
		  begin
		  	AList.AddObject(StrPas(AModule.szExePath)+'/'+StrPas(AModule.szModule),Pointer(AModule.th32ModuleId));
			  Retour:=Module32Next(hSnapShot, AModule);
		  end;
    finally
		  CloseHandle(hSnapShot);
    end;
  finally
  	AList.EndUpDate;
  end;
end;

procedure GetProcessWindowList(AProcessId : DWord; AList : Tstrings);
var
	LeHWnd : HWnd;
  szTitre : Array[0..255] of Char;
  AP2 : DWord;
begin
	AList.Clear;
  LeHWnd:=GetWindow(GetDesktopWindow,GW_CHILD);
  LeHWnd:=GetWindow(LeHWnd,GW_HWNDFIRST);
  while LeHWnd<>0 do
  begin
  	GetWindowText(LeHWnd,szTitre,255);
    GetWindowThreadProcessId(LeHWnd, @AP2);
    if (AP2=AProcessId) and (AP2<>0) and (StrLen(szTitre)>0) then
    	AList.AddObject(StrPas(szTitre),Pointer(LeHWnd));
  	LeHWnd:=GetWindow(LeHWnd,GW_HWNDNEXT);
  end;
end;

procedure GetVisibleWindowList(AList : Tstrings);
var
  AHWnd : HWnd;
  AExe, ATitre : string;
  szTitre : array[0..255] of Char;
  AP2 : DWord;
begin
	AList.Clear;
  AHWnd:=GetWindow(GetDesktopWindow,GW_CHILD);
  AHWnd:=GetWindow(AHWnd,GW_HWNDFIRST);
  while AHWnd<>0 do
  begin

    if IsWindowVisible(AHWnd) then
    begin
      FillChar(szTitre, 255, 0);
      GetWindowText(AHWnd,szTitre,255);
      ATitre := Trim(StrPas(szTitre));
      if Length(ATitre) > 0 then
      begin
        AList.AddObject(ATitre, Pointer(AHWnd));
      end;
    end;

  	AHWnd:=GetWindow(AHWnd,GW_HWNDNEXT);
  end;
end;

//Build the same list as the task manager does...
procedure GetTaskList(AList : Tstrings);
var
  AHWnd : HWnd;
  AExe, ATitre : string;
  szTitre : array[0..255] of Char;
  AP2 : DWord;
begin
	AList.Clear;
  AHWnd:=GetWindow(GetDesktopWindow,GW_CHILD);
  AHWnd:=GetWindow(AHWnd,GW_HWNDFIRST);
  while AHWnd<>0 do
  begin

    if IsWindowVisible(AHWnd) and (GetParent(AHWnd) = 0) and (GetWindow(AHWnd, GW_OWNER) = 0) then
    begin
      FillChar(szTitre, 255, 0);
      GetWindowText(AHWnd,szTitre,255);
      ATitre := Trim(StrPas(szTitre));
      if Length(ATitre) > 0 then
      begin
        GetWindowThreadProcessId(AHWnd, @AP2);
        AExe := GetProcessExeName(AP2);
        if not ((AExe = 'explorer.exe') and (ATitre = 'Program Manager')) then
        begin
          AList.AddObject(ATitre, Pointer(AHWnd));
        end;
      end;
    end;

  	AHWnd:=GetWindow(AHWnd,GW_HWNDNEXT);
  end;
end;

procedure ActivateWindow(AHWnd : HWnd);
var
  AWP : TWindowPlacement;
begin
  GetWindowPlacement(AHWnd, @AWP);
  if AWP.showCmd = SW_SHOWMINIMIZED then
    ShowWindow(AHWnd, SW_RESTORE);

  SetForegroundWindow(AHWnd);
end;

procedure CloseAllTasks;
var
  ALst : TstringList;
  i : Integer;
  AH : Integer;
const
  WM_CLOSE = $0010;
begin
  ALst := TstringList.Create;
  try
    GetTaskList(ALst);
    for i := 0 to ALst.Count - 1 do
    begin
      AH := Longint(ALst.Objects[i]);
      if GetWindowProcess(AH) <> GetCurrentProcessId then
      begin
        PostMessage(AH, WM_CLOSE, 0, 0);
        Sleep(100);
      end;
    end;
  finally
    ALst.Free;
  end;
end;


end.
