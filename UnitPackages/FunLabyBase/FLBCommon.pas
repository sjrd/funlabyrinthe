{*
  Constantes et classes communes
  L'unité FLBCommon décrit les constantes et classes qui sont communes aux
  autres parties, ce qui inclut essentiellement les constantes d'actions et les
  plug-in « polyvalents ».
  @author sjrd
  @version 5.0
*}
unit FLBCommon;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils;

const {don't localize}
  /// Action d'aller sur l'eau
  actGoOnWater = 'GoOnWater';
  /// Action d'ouvrir une serrure en argent
  actOpenSilverLock = 'OpenSilverLock';
  /// Action d'ouvrir une serrure en or
  actOpenGoldenLock = 'OpenGoldenLock';

implementation

end.

