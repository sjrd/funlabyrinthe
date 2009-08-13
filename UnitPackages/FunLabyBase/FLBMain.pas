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
  SysUtils, Classes, ScUtils, FunLabyUtils, FilesUtils, UnitFiles, Generics,
  FLBCommon, FLBFields, FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBBoat,
  FLBLift, FLBObstacles, FLBShowMessage;

type
  TFunLabyBaseUnit = class(TInterfacedUnitFile)
  protected
    procedure GameStarted; override;
  public
    constructor Create(AMasterFile: TMasterFile; Params: TStrings);
  end;

function CreateUnitFile(BPLHandler: TBPLUnitFile; Master: TMaster;
  Params: TStrings): IUnitFile50;

implementation

{*
  Crée l'unité FunLabyBase
  @param BPLHandler   Gestionnaire d'unité BPL prenant en charge ce paquet
  @param Master       Maître FunLabyrinthe
  @param Params       Paramètres passés à l'unité
  @return Interface de l'unité FunLabyBase créée
*}
function CreateUnitFile(BPLHandler: TBPLUnitFile; Master: TMaster;
  Params: TStrings): IUnitFile50;
begin
  Result := TFunLabyBaseUnit.Create(BPLHandler.MasterFile, Params);
end;

{-------------------------}
{ Classe TFunLabyBaseUnit }
{-------------------------}

{*
  Crée l'unité FunLabyBase et charge tous les composants
  @param UnitFile   Fichier unité appelant
  @param Master     Maître FunLabyrinthe dans lequel charger les composants
  @param Params     Paramètres envoyés au fichier unité
*}
constructor TFunLabyBaseUnit.Create(AMasterFile: TMasterFile;
  Params: TStrings);
var
  Buoys, Planks, SilverKeys, GoldenKeys: TObjectDef;
begin
  inherited Create(AMasterFile);

  // Plug-in
  TBuoyPlugin.Create(Master, idBuoyPlugin);
  TPlankPlugin.Create(Master, idPlankPlugin);
  TBoatPlugin.Create(Master, idBoatPlugin);
  TDefaultShowMessagePlugin.Create(Master, idDefaultShowMessagePlugin);

  // Définitions d'objet
  Buoys := TBuoys.Create(Master, idBuoys, sBuoys);
  Planks := TPlanks.Create(Master, idPlanks, sPlanks);
  SilverKeys := TSilverKeys.Create(Master, idSilverKeys, sSilverKeys);
  GoldenKeys := TGoldenKeys.Create(Master, idGoldenKeys, sGoldenKeys);

  // Terrains

  TGround.Create(Master, idGrass, sGrass, fGrass);
  TWall.Create(Master, idWall, sWall);
  TWater.Create(Master, idWater, sWater);
  THole.Create(Master, idHole, sHole);
  TSky.Create(Master, idSky, sSky);
  TOutside.Create(Master, idOutside, sOutside);

  // Effets de case

  TArrow.Create(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.Create(Master, idEastArrow , sEastArrow , diEast );
  TArrow.Create(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.Create(Master, idWestArrow , sWestArrow , diWest );
  TArrow.Create(Master, idCrossroads, sCrossroads, diNone );

  TInactiveTransporter.Create(Master, idInactiveTransporter,
    sInactiveTransporter);
  TTransporterCreator.Create(Master, idTransporterCreator);

  TStairs.Create(Master, idUpStairs, sUpStairs, True);
  TStairs.Create(Master, idDownStairs, sDownStairs, False);
  TLift.Create(Master, idLift, sLift);

  TDirectTurnstile.Create(Master, idDirectTurnstile, sDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile, sIndirectTurnstile);

  TTreasure.Create(Master, idTreasure, sTreasure);

  TDecorativeEffect.Create(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  TBoatCreator.Create(Master, idBoatCreator);
  if not Master.Editing then
    TUnderBoatCreator.Create(Master, idUnderBoatCreator);

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
  [@inheritDoc]
*}
procedure TFunLabyBaseUnit.GameStarted;
var
  TestMsg: TPlayerShowMsgMessage;
  DefaultShowMessagePlugin: TPlugin;
  I, J, X, Y, Z: Integer;
  Player: TPlayer;
  Map: TMap;
  Pos: T3DPoint;
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

  { If there is at least a TBoat component, look for every TBoat in every map
    and put there a special under-boat water. }
  for I := 0 to Master.ToolCount-1 do
  begin
    if Master.Tools[I] is TBoat then
    begin
      for J := 0 to Master.MapCount-1 do
      begin
        Map := Master.Maps[J];

        for X := 0 to Map.Dimensions.X-1 do
        begin
          for Y := 0 to Map.Dimensions.Y-1 do
          begin
            for Z := 0 to Map.Dimensions.Z-1 do
            begin
              Pos := Point3D(X, Y, Z);

              if Map[Pos].Tool is TBoat then
                Map[Pos] := TBoat.ToUnderBoatWater(Map[Pos]);
            end;
          end;
        end;
      end;

      Break;
    end;
  end;
end;

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

end.

