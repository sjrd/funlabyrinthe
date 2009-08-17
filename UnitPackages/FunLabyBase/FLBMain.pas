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

  TStairs.CreateStairs(Master, idUpStairs, True);
  TStairs.CreateStairs(Master, idDownStairs, False);
  TLift.Create(Master, idLift);

  TDirectTurnstile.Create(Master, idDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile);

  TTreasure.Create(Master, idTreasure);

  TDecorativeEffect.CreateDeco(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  TBoatCreator.Create(Master, idBoatCreator);
  if not Master.Editing then
    TUnderBoatCreator.Create(Master, idUnderBoatCreator);

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

