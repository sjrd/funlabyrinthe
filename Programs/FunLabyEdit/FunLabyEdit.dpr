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
  NewSourceFile in 'NewSourceFile.pas' {FormCreateNewSourceFile},
  EditUnits in 'EditUnits.pas' {FormEditUnits},
  CompilerMessages in 'CompilerMessages.pas' {FormCompilerMessages},
  MapViewer in 'MapViewer.pas' {FormMapViewer},
  BaseMapViewer in 'BaseMapViewer.pas' {FrameBaseMapViewer: TFrame},
  ObjectInspector in 'ObjectInspector.pas' {FrameInspector: TFrame},
  FunLabyEditTypes in 'FunLabyEditTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Éditeur FunLabyrinthe';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

