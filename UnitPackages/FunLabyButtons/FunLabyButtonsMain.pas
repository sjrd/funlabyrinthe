{*
  Recense les composants des boutons de FunLabyrinthe
  L'unit� FunLabyCoreMain recense tous les composants du package FunLabyButtons,
  ceux qui d�crivent les boutons de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyButtonsMain;

interface

uses
  Classes, FunLabyUtils;

procedure LoadComponents(Master : TMaster; Params : TStrings); stdcall;

procedure RegisterComponents(Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;

implementation

{*
  Charge tous les composants des boutons de FunLabyrinthe
  @param Master   Ma�tre FunLabyrinthe dans lequel charger les composants
  @param Params   Param�tres envoy�s au fichier unit�
*}
procedure LoadComponents(Master : TMaster; Params : TStrings);
begin
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
begin
end;

exports
  LoadComponents name 'LoadComponents',
  RegisterComponents name 'RegisterComponents';

end.

