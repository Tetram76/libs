unit CreerLnk;

interface

function CreerRaccourci(FichierAExecuter, Arguments, CheminTravail, FichierRaccourci : string; ShowCmd : Integer) : Integer;

function CreerRaccourciPChar(FichierAExecuter, Arguments, CheminTravail, FichierRaccourci : PChar) : Integer; StdCall; export;

implementation

Uses Windows, ActiveX, ComObj, ShlObj, SysUtils;

function CreerRaccourci(FichierAExecuter, Arguments, CheminTravail, FichierRaccourci :string; ShowCmd : Integer) : Integer;
var
  PersistFile : IPersistFile;
  ShellLink : IShellLink;
  RaccourciWC : array[0..MAX_PATH] of WideChar;
begin
  ChangeFileExt(FichierRaccourci,'.LNK');
  ShellLink:=CreateComObject(CLSID_ShellLink) as IShellLink;
  ShellLink.QueryInterface(IPersistFile, PersistFile);
  ShellLink.SetPath(PChar(FichierAExecuter));
  ShellLink.SetArguments(PChar(Arguments));
  ShellLink.SetWorkingDirectory(PChar(CheminTravail));
  ShellLink.SetShowCmd(ShowCmd);
  MultiByteToWideChar(CP_ACP, 0, PChar(FichierRaccourci), -1, @RaccourciWC, MAX_PATH);
  Result:=PersistFile.Save(RaccourciWC, True);
end;

function CreerRaccourciPChar(FichierAExecuter, Arguments, CheminTravail, FichierRaccourci : PChar) : Integer;
begin
  Result:=CreerRaccourci(String(FichierAExecuter), String(Arguments),
    String(CheminTravail), String(FichierRaccourci), SW_SHOWNORMAL);
end;

end.
