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
  SysUtils, Classes, FunLabyUtils, FilesUtils, UnitFiles, Generics, FLBCommon,
  FLBFields, FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBBoat, FLBLift,
  FLBObstacles, FLBShowMessage;

type
  TFunLabyBaseUnit = class(TInterfacedUnitFile)
  protected
    procedure GameStarted; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc); override;
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
  I: Integer;
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

  TGround.Create(Master, idGrass, sGrass);
  TWall.Create(Master, idWall, sWall);
  TWater.Create(Master, idWater, sWater);
  THole.Create(Master, idHole, sHole);
  TSky.Create(Master, idSky, sSky);

  TGround.Create(Master, idGroundWater, sWater, '', Master.Field[idWater]);

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

{*
  [@inheritDoc]
*}
procedure TFunLabyBaseUnit.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);

  procedure RegSingle(Component: TComponentID);
  begin
    RegisterSingleComponentProc(Master.SquareComponent[Component]);
  end;

  procedure RegSet(Template: TComponentID;
    const Components: array of TSquareComponent; BaseIndex: Integer;
    const DialogTitle, DialogPrompt: string);
  begin
    RegisterComponentSetProc(Master.SquareComponent[Template],
      Components, BaseIndex, DialogTitle, DialogPrompt);
  end;

var
  I: Integer;
  Transporters: array[0..45] of TSquareComponent;
  Boats: array[1..10] of TSquareComponent;
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

  Transporters[0] := Master.SquareComponent[idInactiveTransporter];
  for I := 1 to 15 do
    Transporters[I] := Master.SquareComponent[Format(idTransporterNext, [I])];
  for I := 16 to 30 do
    Transporters[I] := Master.SquareComponent[Format(idTransporterPrev, [I])];
  for I := 31 to 45 do
    Transporters[I] := Master.SquareComponent[Format(idTransporterRandom, [I])];
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
    Boats[I] := Master.SquareComponent[Format(idBoatSquare, [I])];
  RegSet(idBoatSquareTemplate, Boats, Low(Boats), sBoatTitle, sBoatPrompt);

  RegSingle(idOutsideSquare);
end;

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

end.

