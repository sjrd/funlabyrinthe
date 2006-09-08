program Labyrinthe;

uses
  Forms,
  WinHelpViewer,
  LabyrintheMain in 'LabyrintheMain.pas' {FormPrincipale},
  LiftDialog in 'LiftDialog.pas' {FormBougeAscenseur},
  PropertiesDialog in 'PropertiesDialog.pas' {FormProprietes},
  PlayerView in 'PlayerView.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Labyrinthe';
  Application.Tag := 1;
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.CreateForm(TFormBougeAscenseur, FormBougeAscenseur);
  Application.CreateForm(TFormProprietes, FormProprietes);
  Application.Run;
end.
