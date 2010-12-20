program FunLabyVersionCheck;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  SysUtils,
  Forms,
  FunLabyVersionCheckMain in 'FunLabyVersionCheckMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Vérification des nouvelles versions de FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);

  if FindCmdLineSwitch('autocheck') then
  begin
    if not FormMain.DoAutoCheckIfNeeded then
      FormMain.Free;
  end;

  Application.Run;
end.

