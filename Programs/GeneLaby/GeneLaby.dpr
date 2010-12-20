program GeneLaby;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  ScUtils,
  LabyGene in 'LabyGene.pas' {FormPrincipale};

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Générateur de Labyrinthes';
  Application.HelpFile := Dir+'FunLabyrinthe.chm';
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.Run;
end.

