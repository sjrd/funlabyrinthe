program FunLabyEdit;

uses
  XPMan,
  Forms,
  FunLabyUtils,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain},
  PlayerPlugins in 'PlayerPlugins.pas' {FormPlugins},
  EditParameters in 'EditParameters.pas' {FormParameters},
  FileProperties in 'FileProperties.pas' {FormFileProperties},
  AddMap in 'AddMap.pas' {FormAddMap},
  MapBase in 'MapBase.pas' {FormMapBase},
  MapEditor in 'MapEditor.pas' {FrameMapEditor: TFrame},
  FunLabyEditConsts in 'FunLabyEditConsts.pas',
  EditPluginManager in 'EditPluginManager.pas',
  NewUnit in 'NewUnit.pas' {FormCreateNewUnit},
  EditUnits in 'EditUnits.pas' {FormEditUnits};

{$R *.res}

begin
  if not CheckValidLaunch then
    Exit;
  Application.Initialize;
  Application.Title := 'Éditeur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

