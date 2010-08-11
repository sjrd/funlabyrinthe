{*
  Recense les composants de base de Funlabyrinthe
  L'unit� FLBMain recense tous les composants du package FunLabyBase, ceux qui
  sont � la base de FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FLBMain;

interface

uses
  SysUtils, Classes, ScUtils, FunLabyUtils, FilesUtils, UnitFiles, Generics,
  FLBCommon, FLBFields, FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBBoat,
  FLBLift, FLBObstacles, FLBShowMessage, SepiReflectionCore;

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
  Cr�e l'unit� FunLabyBase
  @param BPLHandler   Gestionnaire d'unit� BPL prenant en charge ce paquet
  @param Master       Ma�tre FunLabyrinthe
  @param Params       Param�tres pass�s � l'unit�
  @return Interface de l'unit� FunLabyBase cr��e
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
  Cr�e l'unit� FunLabyBase et charge tous les composants
  @param UnitFile   Fichier unit� appelant
  @param Master     Ma�tre FunLabyrinthe dans lequel charger les composants
  @param Params     Param�tres envoy�s au fichier unit�
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
  TDefaultShowMessagePlugin.Create(Master, idDefaultShowMessagePlugin);

  // D�finitions d'objet
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
  Importe l'alias d'unit� FunLabyBase
  @param Root   Racine Sepi
  @return Alias d'unit� cr��
*}
function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnitAlias.Create(Root, 'FunLabyBase',
    ['FLBBoat', 'FLBCommon', 'FLBFields', 'FLBLift', 'FLBObstacles', 'FLBPlank',
    'FLBShowMessage', 'FLBSimpleEffects', 'FLBSimpleObjects']);
  Result.Complete;
end;

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

initialization
  SepiRegisterImportedUnit('FunLabyBase', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FunLabyBase');
end.

