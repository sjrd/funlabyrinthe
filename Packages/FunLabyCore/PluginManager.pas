{*
  Gestionnaire de plugins
  @author sjrd
  @version 5.2
*}
unit PluginManager;

interface

uses
  Windows, SysUtils;

procedure LoadPlugins(const PluginsDir: TFileName);

implementation

uses
  FunLabyUtils, FilesUtils, ScUtils;

var
  /// Handles de modules des plugins chargés
  PluginModules: array of HModule;

{*
  Charge les plugins contenu dans un dossier donné
  @param PluginsDir   Dossier dont charger les plugins
*}
procedure LoadPlugins(const PluginsDir: TFileName);
const
  AllocBy = 16;
var
  OldCurrentDir: string;
  Index: Integer;
begin
  Index := Length(PluginModules);

  SetLength(OldCurrentDir, GetCurrentDirectory(0, nil)-1);
  GetCurrentDirectory(Length(OldCurrentDir)+1, PChar(OldCurrentDir));

  SetCurrentDirectory(PChar(PluginsDir));
  try
    IterateDir(PluginsDir, '*.bpl',
      procedure(const PluginFileName: TFileName; const SearchRec: TSearchRec)
      begin
        if Index >= Length(PluginModules) then
          SetLength(PluginModules, Index+AllocBy);

        PluginModules[Index] := LoadPackage(PluginFileName);
        if PluginModules[Index] <> 0 then
          Inc(Index);
      end);
  finally
    SetCurrentDirectory(PChar(OldCurrentDir));
    SetLength(PluginModules, Index);
  end;
end;

{*
  Décharge les plug-in
*}
procedure UnloadPlugins;
var
  I: Integer;
begin
  for I := 0 to Length(PluginModules)-1 do
    UnloadPackage(PluginModules[I]);
  SetLength(PluginModules, 0);
end;

initialization
finalization
  UnloadPlugins;
end.

