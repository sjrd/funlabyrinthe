{*
  Constantes et classes communes
  L'unité C4xCommon décrit les constantes qui sont communes aux autres parties.
  @author sjrd
  @version 5.0
*}
unit C4xCommon;

interface

const
  phPushing = 1; /// Phase "pushing"
  phExecute = 2; /// Phase "execute"

  MinActionsCount = 78; /// Nombre minimal d'actions
  MaxVar = 20;          /// Indice maximal de variable

const {don't localize}
  idGrassSquare = 'Grass---'; /// ID de la case herbe
  idWaterSquare = 'Water---'; /// ID de la case eau

  DefaultSquaresImgName = 'Cases'; /// Fichier Cases par défaut

resourcestring
  SCategoryButtons = 'Boutons'; /// Catégorie des boutons

const {don't localize}
  fCompatibility4xBuoyPlugin = 'Compatibility4x/BuoyPlugin';

implementation

end.

