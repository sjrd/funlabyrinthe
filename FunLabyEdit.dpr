program FunLabyEdit;

uses
  XPMan,
  Forms,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain},
  PlayerPlugins in 'PlayerPlugins.pas' {FormPlugins},
  PlayerAttributes in 'PlayerAttributes.pas' {FormAttributes};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Éditeur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAttributes, FormAttributes);
  Application.Run;
end.

