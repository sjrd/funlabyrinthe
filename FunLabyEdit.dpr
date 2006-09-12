program FunLabyEdit;

uses
  XPMan,
  Forms,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Éditeur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

