program FunLabyVersionCheck;

uses
  Forms,
  FunLabyVersionCheckMain in 'FunLabyVersionCheckMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Vérification des nouvelles versions de FunLabyrinthe';
  Application.Run;
end.

