program FunLabyEdit;

uses
  XPMan,
  SysUtils,
  Forms,
  ScUtils,
  FunLabyUtils,
  FilesUtils,
  FunLabyEditMain in 'FunLabyEditMain.pas' {FormMain},
  PlayerPlugins in 'PlayerPlugins.pas' {FormPlugins},
  EditParameters in 'EditParameters.pas' {FormParameters},
  FileProperties in 'FileProperties.pas' {FormFileProperties},
  MapEditor in 'MapEditor.pas' {FrameMapEditor: TFrame},
  FunLabyEditConsts in 'FunLabyEditConsts.pas',
  EditPluginManager in 'EditPluginManager.pas',
  NewSourceFile in 'NewSourceFile.pas' {FormCreateNewSourceFile},
  EditUnits in 'EditUnits.pas' {FormEditUnits},
  CompilerMessages in 'CompilerMessages.pas' {FormCompilerMessages},
  MapViewer in 'MapViewer.pas' {FormMapViewer},
  BaseMapViewer in 'BaseMapViewer.pas' {FrameBaseMapViewer: TFrame},
  ObjectInspector in 'ObjectInspector.pas' {FrameInspector: TFrame},
  FunLabyEditTypes in 'FunLabyEditTypes.pas',
  EditMap in 'EditMap.pas' {FormEditMap},
  MapImage in 'MapImage.pas' {FrameMapImage: TFrame},
  EditFilers in 'EditFilers.pas';

{$R *.res}

begin
  RunAutoVersionCheck;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Éditeur FunLabyrinthe';
  Application.HelpFile := Dir+'FunLabyrinthe.chm';
  Application.CreateForm(TFormMain, FormMain);

  if FindCmdLineSwitch('autocompile') then
  begin
    // Auto-compile run
    if not FormMain.AutoCompile then
      Application.Run
    else
      Application.Free;
  end else
  begin
    // Normal run
    Application.Run;
  end;
end.

