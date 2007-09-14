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
  if not CheckValidLaunch then
    Exit;
  Application.Initialize;
  Application.Title := 'FunLabyrinthe';
  Application.HelpFile := Dir+'Labyrinthe.hlp';
  Application.Tag := 1;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

