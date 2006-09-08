program Labyrinthe;

uses
  Forms,
  WinHelpViewer,
  ScUtils,
  LabyrintheMain in 'LabyrintheMain.pas' {FormMain},
  LiftDialog in 'LiftDialog.pas' {FormBougeAscenseur},
  PropertiesDialog in 'PropertiesDialog.pas' {FormProperties},
  PlayerView in 'PlayerView.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Labyrinthe';
  Application.HelpFile := Dir+'Labyrinthe.hlp';
  Application.Tag := 1;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormBougeAscenseur, FormBougeAscenseur);
  Application.CreateForm(TFormProperties, FormProperties);
  Application.Run;
end.

