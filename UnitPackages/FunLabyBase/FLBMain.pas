{*
  Recense les composants de base de Funlabyrinthe
  L'unité FLBMain recense tous les composants du package FunLabyBase, ceux qui
  sont à la base de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBMain;

interface

uses
  SysUtils, Classes, FunLabyUtils, UnitFiles, Generics, FLBCommon, FLBFields,
  FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBBoat, FLBLift, FLBObstacles;

procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings); stdcall;

procedure RegisterComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;

implementation

{*
  Charge tous les composants au coeur de FunLabyrinthe
  @param UnitFile   Fichier unité appelant
  @param Master     Maître FunLabyrinthe dans lequel charger les composants
  @param Params     Paramètres envoyés au fichier unité
*}
procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
var Buoys, Planks, SilverKeys, GoldenKeys : TObjectDef;
    I : integer;
begin
  // Plug-in
  TBuoyPlugin.Create(Master, idBuoyPlugin);
  TPlankPlugin.Create(Master, idPlankPlugin);
  TBoatPlugin.Create(Master, idBoatPlugin);

  // Défintions d'objet
  Buoys := TBuoys.Create(Master, idBuoys, sBuoys);
  Planks := TPlanks.Create(Master, idPlanks, sPlanks);
  SilverKeys := TSilverKeys.Create(Master, idSilverKeys, sSilverKeys);
  GoldenKeys := TGoldenKeys.Create(Master, idGoldenKeys, sGoldenKeys);

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
  for I := 16 to 30 do
    TTransporter.Create(Master, idTransporterPrev, sTransporterPrev,
      I, tkPrevious);
  for I := 31 to 45 do
    TTransporter.Create(Master, idTransporterRandom, sTransporterRandom,
      I, tkRandom);
  TTransporter.Create(Master, idTransporterTemplate, sTransporterTemplate, 0);

  TStairs.Create(Master, idUpStairs, sUpStairs, True);
  TStairs.Create(Master, idDownStairs, sDownStairs, False);
  TLift.Create(Master, idLift, sLift);

  TDirectTurnstile.Create(Master, idDirectTurnstile, sDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile, sIndirectTurnstile);

  TOutside.Create(Master, idOutside, sOutside);
  TTreasure.Create(Master, idTreasure, sTreasure);

  for I := 1 to 10 do
    TBoat.Create(Master, idBoat, sBoat, I);
  TBoat.Create(Master, idBoatTemplate, sBoatTemplate, 0);

  // Outils

  TObjectTool.Create(Master, idBuoy, Buoys, sFoundBuoy, sBuoy);
  TObjectTool.Create(Master, idPlank, Planks, sFoundPlank, sPlank);
  TObjectTool.Create(Master, idSilverKey, SilverKeys,
    sFoundSilverKey, sSilverKey);
  TObjectTool.Create(Master, idGoldenKey, GoldenKeys,
    sFoundGoldenKey, sGoldenKey);

  // Obstacles

  TSilverBlock.Create(Master, idSilverBlock, sSilverBlock);
  TGoldenBlock.Create(Master, idGoldenBlock, sGoldenBlock);
  TSecretWay.Create(Master, idSecretWay, sSecretWay);
end;

{*
  Enregistre les différents composants à placer dans la palette d'édition
  @param UnitFile                      Fichier unité appelant
  @param Master                        Maître FunLabyrinthe
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
    Transporters : array[0..45] of TScrewComponent;
    Boats : array[1..10] of TScrewComponent;
begin
  // Terrains

  RegSingle(idGrass);
  RegSingle(idWall);
  RegSingle(idWater);
  RegSingle(idHole);
  RegSingle(idSky);

  // Effets

  RegSingle(idNorthArrow);
  RegSingle(idEastArrow);
  RegSingle(idSouthArrow);
  RegSingle(idWestArrow);
  RegSingle(idCrossroads);

  Transporters[0] := Master.ScrewComponent[idInactiveTransporter];
  for I := 1 to 15 do
    Transporters[I] := Master.ScrewComponent[Format(idTransporterNext, [I])];
  for I := 16 to 30 do
    Transporters[I] := Master.ScrewComponent[Format(idTransporterPrev, [I])];
  for I := 31 to 45 do
    Transporters[I] := Master.ScrewComponent[Format(idTransporterRandom, [I])];
  RegSet(idTransporterTemplate, Transporters, Low(Transporters),
    sTransporterTitle, sTransporterPrompt);

  RegSingle(idUpStairs);
  RegSingle(idDownStairs);
  RegSingle(idLift);

  RegSingle(idDirectTurnstile);
  RegSingle(idIndirectTurnstile);

  RegSingle(idTreasure);

  RegSingle(idBuoy);
  RegSingle(idPlank);
  RegSingle(idSilverKey);
  RegSingle(idGoldenKey);

  // Obstacles

  RegSingle(idSilverBlock);
  RegSingle(idGoldenBlock);
  RegSingle(idSecretWay);

  // Cases

  for I := 1 to 10 do
    Boats[I] := Master.ScrewComponent[Format(idBoatScrew, [I])];
  RegSet(idBoatScrewTemplate, Boats, Low(Boats), sBoatTitle, sBoatPrompt);

  RegSingle(idOutsideScrew);
end;

{$IFNDEF DCTD}
exports
  LoadComponents name 'LoadComponents',
  RegisterComponents name 'RegisterComponents';
{$ENDIF}

end.

