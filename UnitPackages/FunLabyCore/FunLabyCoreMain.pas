{*
  Recense les composants au coeur de Funlabyrinthe
  L'unité FunLabyCoreMain recense tous les composants du package FunLabyCore,
  ceux qui sont au coeur de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyCoreMain;

interface

uses
  Classes, FunLabyUtils, Common, Fields, SimpleEffects, SimpleObjects, Plank,
  Boat, Lift, Obstacles;

procedure LoadComponents(Master : TMaster; Params : TStrings); stdcall;

implementation

{*
  Charge tous les composants au coeur de FunLabyrinthe
  @param Master   Maître FunLabyrinthe dans lequel charger les composants
  @param Params   Paramètres envoyés au fichier unité
*}
procedure LoadComponents(Master : TMaster; Params : TStrings);
var I : integer;
begin
  // Plug-in
  TMaskPlugin.Create(Master, idAvoidShowPlugin);
  TBuoyPlugin.Create(Master, idBuoyPlugin);
  TPlankPlugin.Create(Master, idPlankPlugin);
  TBoatPlugin.Create(Master, idBoatPlugin);

  // Défintions d'objet
  TBuoys.Create(Master, idBuoys, sBuoys);
  TPlanks.Create(Master, idPlanks, sPlanks);
  TSilverKeys.Create(Master, idSilverKeys, sSilverKeys);
  TGoldenKeys.Create(Master, idGoldenKeys, sGoldenKeys);

  // Terrain et effet spécial planche
  TPlankField.Create(Master, idPlankField, '');
  TPlankEffect.Create(Master, idPlankEffect, '');

  // Terrain et effet spécial ascenseur occupé
  TEngagedLiftField.Create(Master, idEngagedLiftField, '');
  TEngagedLiftEffect.Create(Master, idEngagedLiftEffect, '');

  // Terrains
  TGrass.Create(Master, idGrass, sGrass);
  TWall.Create(Master, idWall, sWall);
  TWater.Create(Master, idWater, sWater);
  THole.Create(Master, idHole, sHole);
  TSky.Create(Master, idSky, sSky);

  TGrass.Create(Master, idGrassWater, sWater, Master.Field[idWater]);

  // Effets de case
  TArrow.Create(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.Create(Master, idEastArrow , sEastArrow , diEast );
  TArrow.Create(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.Create(Master, idWestArrow , sWestArrow , diWest );
  TArrow.Create(Master, idCrossroads, sCrossroads, diNone );

  TTransporter.Create(Master, idInactiveTransporter, sInactiveTransporter, 0);
  for I := 1 to 15 do
    TTransporter.Create(Master, idTransporterNext, sTransporterNext,
      I, tkNext);
  for I := 1 to 15 do
    TTransporter.Create(Master, idTransporterPrev, sTransporterPrev,
      I, tkPrevious);
  for I := 1 to 15 do
    TTransporter.Create(Master, idTransporterRandom, sTransporterRandom,
      I, tkRandom);

  TStairs.Create(Master, idUpStairs, sUpStairs, True);
  TStairs.Create(Master, idDownStairs, sDownStairs, False);
  TLift.Create(Master, idLift, sLift);

  TDirectTurnstile.Create(Master, idDirectTurnstile, sDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile, sIndirectTurnstile);

  TOutside.Create(Master, idOutside, sOutside);
  TTreasure.Create(Master, idTreasure, sTreasure);

  TBuoy.Create(Master, idBuoy, sBuoy);
  TPlank.Create(Master, idPlank, sPlank);
  TSilverKey.Create(Master, idSilverKey, sSilverKey);
  TGoldenKey.Create(Master, idGoldenKey, sGoldenKey);

  for I := 1 to 10 do
    TBoat.Create(Master, idBoat, sBoat, I);

  // Obstacles
  TSilverBlock.Create(Master, idSilverBlock, sSilverBlock);
  TGoldenBlock.Create(Master, idGoldenBlock, sGoldenBlock);
  TSecretWay.Create(Master, idSecretWay, sSecretWay);
end;

exports
  LoadComponents name 'LoadComponents';

end.

