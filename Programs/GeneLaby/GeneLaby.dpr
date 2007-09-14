program GeneLaby;

uses
  Forms,
  WinHelpViewer,
  LabyGene in 'LabyGene.pas' {FormPrincipale};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Générateur de Labyrinthes';
  Application.HelpFile := 'GeneLaby.hlp';
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.Run;
end.

