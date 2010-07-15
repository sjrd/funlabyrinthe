program FunLaby;

uses
  Forms,
  WinHelpViewer,
  ScUtils,
  FunLabyUtils,
  FilesUtils,
  FunLabyMain in 'FunLabyMain.pas' {FormMain},
  PlayUtils in 'PlayUtils.pas';

{$R *.RES}

begin
  RunAutoVersionCheck;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'FunLabyrinthe';
  Application.HelpFile := Dir+'FunLabyrinthe.chm';
  Application.Tag := 1;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

