program ExeUpdate;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils;

var
  FichierSource, FichierDestination : string;

function ExecCommandLine(CommandLine: string; AShowWindow : Word = SW_SHOWNORMAL): TProcessInformation;
var StartUpInfos: TStartUpInfo;
begin
  StartUpInfos.cb:= SizeOf(StartUpInfos);
  StartUpInfos.lpReserved:= nil;
  StartUpInfos.lpDesktop:= nil;
  StartUpInfos.lpTitle:= nil;
  StartUpInfos.dwFlags:= STARTF_USESHOWWINDOW;
  StartUpInfos.cbReserved2:= 0;
  StartUpInfos.lpReserved2:= nil;
  StartUpInfos.wShowWindow:= AShowWindow;
  if not CreateProcess(nil, PChar(CommandLine), nil, nil,
    False, CREATE_DEFAULT_ERROR_MODE + {CREATE_NEW_CONSOLE +} NORMAL_PRIORITY_CLASS,
    nil, nil, StartUpInfos, Result) then
      FillChar(Result, SizeOf(Result), 0); // if failure, result is 0
end;

var
  OK : Boolean;
  AF : Integer;

begin
  Sleep(500);

  FichierSource := ExtractFilePath(ParamStr(0)) + ParamStr(1);
  FichierDestination := ExtractFilePath(ParamStr(0)) + ParamStr(2);

  { Boucle tant que le fichier Exe est encore en exécution... }
  repeat
    AF := FileOpen(FichierDestination, fmOpenRead or fmShareExclusive);
    OK := AF >= 0;
  until OK;
  FileClose(AF);

  CopyFile(PChar(FichierSource), PChar(FichierDestination), False);

  Sleep(1000);

  DeleteFile(PChar(FichierSource));

  Sleep(500);

  if SameText(ParamStr(3), 'restart') then
    ExecCommandLine(FichierDestination);
end.
