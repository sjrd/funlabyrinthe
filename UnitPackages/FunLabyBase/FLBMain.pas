{*
  Recense les composants de base de Funlabyrinthe
  L'unité FLBMain recense tous les composants du package FunLabyBase, ceux qui
  sont à la base de FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FLBMain;

interface

uses
  SysUtils, Classes, ScUtils, FunLabyUtils, Generics,
  FLBCommon, FLBFields, FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBBoat,
  FLBLift, FLBObstacles, FLBShowMessage;

implementation

uses
  SepiReflectionCore, SepiMembers;

{---------------------}
{ FunLaby unit events }
{---------------------}

{*
  Initialise l'unité FunLabyBase
  @param Master   Maître FunLabyrinthe dans lequel charger les composants
*}
procedure InitializeUnit(Master: TMaster);
var
  Buoys, Planks, SilverKeys, GoldenKeys: TObjectDef;
  UpStairs, DownStairs: TStairs;
begin
  // Plug-in
  TBuoyPlugin.Create(Master, idBuoyPlugin);
  TPlankPlugin.Create(Master, idPlankPlugin);
  TDefaultShowMessagePlugin.Create(Master, idDefaultShowMessagePlugin);

  // Définitions d'objet
  Buoys := TBuoys.Create(Master, idBuoys);
  Planks := TPlanks.Create(Master, idPlanks);
  SilverKeys := TSilverKeys.Create(Master, idSilverKeys);
  GoldenKeys := TGoldenKeys.Create(Master, idGoldenKeys);

  // Terrains

  TGround.CreateGround(Master, idGrass, sGrass, fGrass);
  TWall.Create(Master, idWall);
  TWater.Create(Master, idWater);
  THole.Create(Master, idHole);
  TSky.Create(Master, idSky);
  TOutside.Create(Master, idOutside);

  // Effets de case

  TArrow.CreateArrow(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.CreateArrow(Master, idEastArrow , sEastArrow , diEast );
  TArrow.CreateArrow(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.CreateArrow(Master, idWestArrow , sWestArrow , diWest );
  TArrow.CreateArrow(Master, idCrossroads, sCrossroads, diNone );

  TInactiveTransporter.Create(Master, idInactiveTransporter);
  TTransporterCreator.Create(Master, idTransporterCreator);

  UpStairs := TUpStairs.Create(Master, idUpStairs);
  DownStairs := TDownStairs.Create(Master, idDownStairs);
  TLift.Create(Master, idLift);

  UpStairs.PairingStairs := DownStairs;
  DownStairs.PairingStairs := UpStairs;

  TDirectTurnstile.Create(Master, idDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile);

  TTreasure.Create(Master, idTreasure);

  TDecorativeEffect.CreateDeco(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  TBoatCreator.Create(Master, idBoatCreator);

  // Outils

  TObjectTool.CreateTool(Master, idBuoy, Buoys, sFoundBuoy, sBuoy);
  TPlankTool.CreateTool(Master, idPlank, Planks, sFoundPlank, sPlank);
  TObjectTool.CreateTool(Master, idSilverKey, SilverKeys,
    sFoundSilverKey, sSilverKey);
  TObjectTool.CreateTool(Master, idGoldenKey, GoldenKeys,
    sFoundGoldenKey, sGoldenKey);

  // Obstacles

  TSilverBlock.Create(Master, idSilverBlock);
  TGoldenBlock.Create(Master, idGoldenBlock);
  TSecretWay.Create(Master, idSecretWay);
end;

{*
  [@inheritDoc]
*}
procedure GameStarted(Master: TMaster);
var
  TestMsg: TPlayerShowMsgMessage;
  DefaultShowMessagePlugin: TPlugin;
  I: Integer;
  Player: TPlayer;
begin
  { For each player, if no plug-in handles the ShowMessage player message, give
    him a default plug-in for his messages. }

  TestMsg.MsgID := msgShowMessage;
  DefaultShowMessagePlugin := Master.Plugin[idDefaultShowMessagePlugin];

  for I := 0 to Master.PlayerCount-1 do
  begin
    Player := Master.Players[I];
    Player.Dispatch(TestMsg);

    if not TestMsg.Handled then
      Player.AddPlugin(DefaultShowMessagePlugin);
  end;
end;

{---------------------}
{ ImportUnit function }
{---------------------}

{*
  Importe l'alias d'unité FunLabyBase
  @param Root   Racine Sepi
  @return Alias d'unité créé
*}
function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnitAlias.Create(Root, 'FunLabyBase',
    ['FLBBoat', 'FLBCommon', 'FLBFields', 'FLBLift', 'FLBObstacles', 'FLBPlank',
    'FLBShowMessage', 'FLBSimpleEffects', 'FLBSimpleObjects']);

  Result.CurrentVisibility := mvPrivate;

  TSepiMethod.Create(Result, 'InitializeUnit',
    'static procedure(Master: TMaster)').SetCode(@InitializeUnit);
  TSepiMethod.Create(Result, 'GameStarted',
    'static procedure(Master: TMaster)').SetCode(@GameStarted);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('FunLabyBase', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FunLabyBase');
end.

