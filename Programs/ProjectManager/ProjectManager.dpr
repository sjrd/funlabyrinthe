program ProjectManager;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  ProjectManagerMain in 'ProjectManagerMain.pas' {FormMain},
  ProjectDatabase in 'ProjectDatabase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Gestionnaire de projets FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
