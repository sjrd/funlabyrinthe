program FunLaby;

uses
  Forms,
  WinHelpViewer,
  ScUtils,
  FunLabyUtils,
  FunLabyMain in 'FunLabyMain.pas' {FormMain},
  PlayUtils in 'PlayUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FunLabyrinthe';
  Application.HelpFile := Dir+'Labyrinthe.hlp';
  Application.Tag := 1;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

