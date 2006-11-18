{*
  Constantes et classes communes
  L'unité C4xCommon décrit les constantes qui sont communes aux autres parties.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xCommon;

interface

const
  phPushing = 1; /// Phase "pushing"
  phExecute = 2; /// Phase "execute"

  MaxVar = 20; /// Indice maximal de variable

const {don't localize}
  idGrass = 'Grass';                             /// ID de l'herbe
  idInactiveTransporter = 'InactiveTransporter'; /// ID du téléporteur inactif

  idGrassScrew = 'Grass--';         /// ID de la case herbe
  idWaterScrew = 'Water--';         /// ID de la case eau
  idBoatScrew = 'GrassWater-Boat-'; /// ID de la case barque

  idBuoys = 'Buoys';           /// ID des bouées
  idPlanks = 'Planks';         /// ID des planches
  idSilverKeys = 'SilverKeys'; /// ID des clefs d'argent
  idGoldenKeys = 'GoldenKeys'; /// ID des clefs d'or
  idBoat = 'Boat%d';           /// ID de la barque

implementation

end.

