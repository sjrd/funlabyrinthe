{*
  Gestionnaire de plug-in de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit EditPluginManager;

interface

uses
  Windows, SysUtils, FunLabyUtils, FilesUtils, ScUtils;

var
  /// Handles de modules des plug-in de l'éditeur chargés
  EditPluginModules: array of HModule;

implementation

{*
  Charge les plug-in
*}
procedure LoadPlugins;
const
  AllocBy = 16;
var
  PluginsDir, OldCurrentDir: string;
  SearchRec: TSearchRec;
  Index: Integer;
begin
  Index := 0;

  PluginsDir := JoinPath([Dir, EditPluginsDir]);

  SetLength(OldCurrentDir, GetCurrentDirectory(0, nil)-1);
  GetCurrentDirectory(Length(OldCurrentDir)+1, PChar(OldCurrentDir));

  SetCurrentDirectory(PChar(PluginsDir));
  try
    if FindFirst(JoinPath([PluginsDir, '*.bpl']), faAnyFile, SearchRec) = 0 then
    try
      repeat
        if Index >= Length(EditPluginModules) then
          SetLength(EditPluginModules, Index+AllocBy);

        EditPluginModules[Index] := LoadPackage(
          JoinPath([PluginsDir, SearchRec.Name]));
        if EditPluginModules[Index] <> 0 then
          Inc(Index);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
      SetLength(EditPluginModules, Index);
    end;
  finally
    SetCurrentDirectory(PChar(OldCurrentDir));
  end;
end;

{*
  Décharge les plug-in
*}
procedure UnloadPlugins;
var
  I: Integer;
begin
  for I := 0 to Length(EditPluginModules)-1 do
    UnloadPackage(EditPluginModules[I]);
  SetLength(EditPluginModules, 0);
end;

initialization
  LoadPlugins;
finalization
  UnloadPlugins;
end.

