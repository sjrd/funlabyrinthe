program FunLabyEdit;

uses
  XPMan,
  Forms,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain},
  PlayerPlugins in 'PlayerPlugins.pas' {FormPlugins},
  PlayerAttributes in 'PlayerAttributes.pas' {FormAttributes},
  PlayerObjects in 'PlayerObjects.pas' {FormObjects};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '�diteur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

