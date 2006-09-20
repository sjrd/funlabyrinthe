program FunLabyEdit;

uses
  XPMan,
  Forms,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain},
  PlayerPlugins in 'PlayerPlugins.pas' {FormPlugins},
  PlayerAttributes in 'PlayerAttributes.pas' {FormAttributes},
  PlayerObjects in 'PlayerObjects.pas' {FormObjects},
  FileProperties in 'FileProperties.pas' {FormFileProperties},
  AddMap in 'AddMap.pas' {FormAddMap},
  MapBase in 'MapBase.pas' {FormMapBase};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Éditeur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAddMap, FormAddMap);
  Application.CreateForm(TFormMapBase, FormMapBase);
  Application.Run;
end.

