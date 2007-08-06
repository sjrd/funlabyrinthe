program FunLabyEdit;

uses
  XPMan,
  Forms,
  FunLabyUtils,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain},
  PlayerPlugins in 'PlayerPlugins.pas' {FormPlugins},
  PlayerAttributes in 'PlayerAttributes.pas' {FormAttributes},
  FileProperties in 'FileProperties.pas' {FormFileProperties},
  AddMap in 'AddMap.pas' {FormAddMap},
  MapBase in 'MapBase.pas' {FormMapBase},
  MapEditor in 'MapEditor.pas' {FrameMapEditor: TFrame},
  FunLabyEditConsts in 'FunLabyEditConsts.pas',
  EditPluginManager in 'EditPluginManager.pas';

{$R *.res}

begin
  if not CheckValidLaunch then exit;
  Application.Initialize;
  Application.Title := '�diteur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

