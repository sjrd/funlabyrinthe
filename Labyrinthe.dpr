program Labyrinthe;

uses
  Forms,
  WinHelpViewer,
  ScUtils,
  FunLabyUtils,
  LabyrintheMain in 'LabyrintheMain.pas' {FormMain},
  PropertiesDialog in 'PropertiesDialog.pas' {FormProperties},
  PlayUtils in 'PlayUtils.pas';

{$R *.RES}

begin
  if not CheckValidLaunch then exit;
  Application.Initialize;
  Application.Title := 'FunLabyrinthe';
  Application.HelpFile := Dir+'Labyrinthe.hlp';
  Application.Tag := 1;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormProperties, FormProperties);
  Application.Run;
end.

