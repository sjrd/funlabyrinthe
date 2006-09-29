{*
  Recense les composants de compatibilit� 4.x de FunLabyrinthe
  L'unit� FunLabyCoreMain recense tous les composants du package
  Compatibility4x, ceux qui d�crivent les composants de compatibilit� 4.x de
  FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xMain;

interface

uses
  Classes, SysUtils, FunLabyUtils, C4xCommon, C4xScrews;

procedure LoadComponents(Master : TMaster; Params : TStrings); stdcall;

procedure RegisterComponents(Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;

implementation

{*
  Charge tous les composants de compatibilit� 4.x de FunLabyrinthe
  @param Master   Ma�tre FunLabyrinthe dans lequel charger les composants
  @param Params   Param�tres envoy�s au fichier unit�
*}
procedure LoadComponents(Master : TMaster; Params : TStrings);
var I : integer;
begin
  // Effets

  for I := 1 to 75 do
    TActionsEffect.Create(Master, idActionsEffect, sButton, I);
  TSunkButton.Create(Master, idSunkButton, sSunkButton);

  // Obstacles

  for I := 1 to 75 do
    TActionsObstacle.Create(Master, idActionsObstacle, sButton, I);
end;

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
  @param Master                        Ma�tre FunLabyrinthe
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure RegisterComponents(Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);

  procedure RegSingle(Component : TComponentID);
  begin
    RegisterSingleComponentProc(Master.ScrewComponent[Component]);
  end;

  procedure RegSet(Template : TComponentID;
    Components : array of TScrewComponent;
    const DialogTitle, DialogPrompt : string);
  begin
    RegisterComponentSetProc(Master.ScrewComponent[Template],
      Components, DialogTitle, DialogPrompt);
  end;

var I : integer;
    Components : array[1..75] of TScrewComponent;
begin
  // Effets

  RegSingle(idSunkButton);

  // Cases

  for I := 1 to 75 do
    Components[I] := Master.Effect[Format(idActionsScrew, [I])];
  RegSet(idActionsScrewTemplate, Components, sButtonTitle, sButtonPrompt);
end;

exports
  LoadComponents name 'LoadComponents',
  RegisterComponents name 'RegisterComponents';

end.

