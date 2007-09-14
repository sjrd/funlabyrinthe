{*
  Gestionnaire de plug-in de FunLabyEdit
  @author sjrd
  @version 5.0
*}
unit EditPluginManager;

interface

uses
  SysUtils, FunLabyUtils;

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
  SearchRec: TSearchRec;
  Index: Integer;
begin
  Index := 0;

  if FindFirst(fEditPluginDir+'*.bpl', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if Index >= Length(EditPluginModules) then
        SetLength(EditPluginModules, Index+AllocBy);

      EditPluginModules[Index] := LoadPackage(fEditPluginDir+SearchRec.Name);
      if EditPluginModules[Index] <> 0 then
        Inc(Index);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
    SetLength(EditPluginModules, Index);
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

