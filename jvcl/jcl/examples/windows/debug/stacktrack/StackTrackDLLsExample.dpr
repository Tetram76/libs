program StackTrackDLLsExample;

{$I jcl.inc}

uses
  Forms,
  StackTrackDLLsDemoMain in 'StackTrackDLLsDemoMain.pas' {MainForm},
  ExceptDlg in '..\..\..\..\experts\debug\dialog\ExceptDlg.pas' {ExceptionDialog};

{$R *.res}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
