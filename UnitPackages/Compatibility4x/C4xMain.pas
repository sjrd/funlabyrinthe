{*
  Recense les composants de compatibilit� 4.x de FunLabyrinthe
  L'unit� C4xMain recense tous les composants du package
  Compatibility4x, ceux qui d�crivent les composants de compatibilit� 4.x de
  FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xMain;

interface

uses
  Classes, SysUtils, FunLabyUtils, UnitFiles, C4xCommon, C4xComponents;

procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings); stdcall;

procedure RegisterComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;

implementation

{*
  Charge tous les composants de compatibilit� 4.x de FunLabyrinthe
  @param UnitFile   Fichier unit� appelant
  @param Master     Ma�tre FunLabyrinthe dans lequel charger les composants
  @param Params     Param�tres envoy�s au fichier unit�
*}
procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
var I : integer;
begin
  // Effets

  TDecorativeEffect.Create(Master, idButtonTemplate, sButtonTemplate, fButton);

  TDecorativeEffect.Create(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  // Actions

  for I := 1 to 75 do
    TActions.Create(Master, I);
end;

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
  @param UnitFile                      Fichier unit� appelant
  @param Master                        Ma�tre FunLabyrinthe
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure RegisterComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);

  procedure RegSingle(Component : TComponentID);
  begin
    RegisterSingleComponentProc(Master.ScrewComponent[Component]);
  end;

  procedure RegSet(Template : TComponentID;
    const Components : array of TScrewComponent; BaseIndex : integer;
    const DialogTitle, DialogPrompt : string);
  begin
    RegisterComponentSetProc(Master.ScrewComponent[Template],
      Components, BaseIndex, DialogTitle, DialogPrompt);
  end;

var I : integer;
    Components : array[1..75] of TScrewComponent;
begin
  // Effets

  RegSingle(idSunkenButton);

  // Cases

  for I := 1 to 75 do
    Components[I] := Master.Screw[Format(idActionsScrew, [I])];
  RegSet(idActionsScrewTemplate, Components, Low(Components),
    sButtonTitle, sButtonPrompt);
end;

exports
  LoadComponents name 'LoadComponents',
  RegisterComponents name 'RegisterComponents';

end.

