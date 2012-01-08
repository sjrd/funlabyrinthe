{*
  Recense les composants de compatibilité 4.x de FunLabyrinthe
  L'unité C4xMain recense tous les composants du package
  Compatibility4x, ceux qui décrivent les composants de compatibilité 4.x de
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit C4xMain;

interface

uses
  Classes, SysUtils, StrUtils, Math, Contnrs, TypInfo, ScUtils, ScLists,
  ScStrUtils, SdDialogs, FunLabyUtils, Generics,
  FLBFields, FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBLift,
  FLBObstacles, FLBShowMessage,
  C4xCommon, C4xComponents, C4xFields, C4xBoat, C4xSquaresTable;

implementation

uses
  SepiReflectionCore, SepiMembers, SepiSystemUnit;

{-----------------------}
{ TPainterAdapter class }
{-----------------------}

type
  TPainterAdapter = class(TPainter)
  private
    FSquaresImgName: string; /// Nom du fichier Cases

    procedure Adapt;
  public
    procedure AdaptPainter(APainter: TPainter);
    procedure AdaptPainters(Instance: TObject);

    property SquaresImgName: string read FSquaresImgName write FSquaresImgName;
  end;

procedure TPainterAdapter.Adapt;
const
  ImgNamesToAdapt: array[0..30] of string = (
    fGrass, fWater, fWall, fHole, fSilverBlock,
    fGoldenBlock, fNorthArrow, fEastArrow, fSouthArrow, fWestArrow,
    fTransporter, fUpStairs, fDownStairs, fButton, fSunkenButton,
    fOutside, fBuoy, fPlank, fSilverKey, fGoldenKey,
    fInfoStone, fLift, fOpenedLift, fSwitchOn, fSwitchOff,
    fBoat, fTreasure, fCrossroads, fDirectTurnstile, fIndirectTurnstile,
    fSky
  );
var
  ImgNameFmt, Line, DummyStr, RectStr: string;
  I, Index, XIndex, YIndex: Integer;
begin
  ImgNameFmt := fCompatibility + SquaresImgName + '@%d,%d:%d,%d';

  for I := 0 to Description.Count-1 do
  begin
    Line := Description[I];

    if AnsiStartsStr(fCompatibility + DefaultSquaresImgName + '@', Line) then
    begin
      if SquaresImgName = DefaultSquaresImgName then
        Continue;

      SplitToken(Line, '@', DummyStr, RectStr);
    end else
    begin
      Index := AnsiIndexStr(Line, ImgNamesToAdapt);
      if Index < 0 then
        Continue;

      XIndex := Index mod 5;
      YIndex := Index div 5;

      RectStr := Format('%d,%d:%d,%d',
        [XIndex*SquareSize, YIndex*SquareSize, (XIndex+1)*SquareSize,
        (YIndex+1)*SquareSize]);
    end;

    Description[I] := fCompatibility + SquaresImgName + '@' + RectStr;
  end;
end;

procedure TPainterAdapter.AdaptPainter(APainter: TPainter);
begin
  BeginUpdate;
  try
    Assign(APainter);
    Adapt;
    APainter.Assign(Self);
  finally
    Clear;
    EndUpdate;
  end;
end;

procedure TPainterAdapter.AdaptPainters(Instance: TObject);
var
  I, PropCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropClass: TClass;
begin
  PropCount := GetPropList(Instance.ClassInfo, [tkClass], nil);
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropList(Instance.ClassInfo, [tkClass], PropList, False);

    for I := 0 to PropCount-1 do
    begin
      PropInfo := PropList^[I];
      PropClass := GetTypeData(PropInfo.PropType^).ClassType;
      if PropClass.InheritsFrom(TPainter) then
        AdaptPainter(TPainter(GetOrdProp(Instance, PropInfo)));
    end;
  finally
    FreeMem(PropList);
  end;
end;

{-----------------}
{ Helper routines }
{-----------------}

{*
  Crée les composants de cette unité
*}
procedure CreateComponents(Master: TMaster);
var
  I: Integer;
  Buoys, Planks, SilverKeys, GoldenKeys: TObjectDef;
  UpStairs, DownStairs: TStairs;
begin
  // Plugins

  with TBuoyPlugin.Create(Master, idBuoyPlugin) do
  begin
    PainterBefore.Clear;
    PainterBefore.AddImage(fCompatibility4xBuoyPlugin);
  end;

  TPlankPlugin.Create(Master, idPlankPlugin);
  TOldBoatPlugin.Create(Master, idOldBoatPlugin);

  TDefaultShowMessagePlugin.Create(Master, idDefaultShowMessagePlugin);

  TCompatibilityHacksPlugin.Create(Master, idCompatibilityHacksPlugin);
  TZonesPlugin.Create(Master, idZonesPlugin);

  // Définitions d'objet

  Buoys := TBuoys.Create(Master, idBuoys);
  Planks := TPlanks.Create(Master, idPlanks);
  SilverKeys := TSilverKeys.Create(Master, idSilverKeys);
  GoldenKeys := TGoldenKeys.Create(Master, idGoldenKeys);

  // Terrains

  TGround.CreateGround(Master, idGrass, sGrass, fGrass);
  TWall.Create(Master, idWall);
  TOldWater.Create(Master, idWater);
  TOldHole.Create(Master, idHole);
  TSky.Create(Master, idSky);

  for I := 1 to 10 do
    TOldBoat.CreateNumbered(Master, Format(fmtidBoat, [I]), I);

  // Effets

  TArrow.CreateArrow(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.CreateArrow(Master, idEastArrow , sEastArrow , diEast );
  TArrow.CreateArrow(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.CreateArrow(Master, idWestArrow , sWestArrow , diWest );
  TArrow.CreateArrow(Master, idCrossroads, sCrossroads, diNone );

  TInactiveTransporter.Create(Master, idInactiveTransporter);

  UpStairs := TUpStairs.Create(Master, idUpStairs);
  DownStairs := TDownStairs.Create(Master, idDownStairs);
  TLift.Create(Master, idLift);

  UpStairs.PairingStairs := DownStairs;
  DownStairs.PairingStairs := UpStairs;

  for I := 1 to 20 do
    TOldStairs.Create(Master, Format(idOldStairs, [I]));

  TDirectTurnstile.Create(Master, idDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile);

  TDecorativeEffect.CreateDeco(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

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
  Charge les actions depuis le fichier d'actions
  @param FileName   Nom du fichier d'actions
*}
procedure LoadActions(Master: TMaster; const FileName: TFileName);
const {don't localize}
  KindStrings: array[0..14] of string = (
    'GameStarted', 'PushButton', 'Switch', 'InfoStone', 'Hidden',
    'TransporterNext', 'TransporterPrevious', 'TransporterRandom', 'Outside',
    'Treasure', 'Custom', 'Object', 'Obstacle', 'Direction', 'Zone'
  );
var
  FileContents, SubContents: TStrings;
  ActionsList: TObjectList;
  Number, FirstLine, LastLine: Integer;
  StrNumber, InfoLine, Graphics: string;
  Kind: TActionsKind;
  Zone: T3DPoint;
  ActionsID: TComponentID;
  Actions: TActions;
begin
  { Don't localize any of the strings in this procedure. }

  FileContents := nil;
  SubContents := nil;
  ActionsList := nil;
  try
    FileContents := TStringList.Create;
    SubContents := TStringList.Create;
    ActionsList := TObjectList.Create(False);

    if FileName <> '' then
      FileContents.LoadFromFile(FileName);
    FileContents.Add('[]');
    Number := 0;

    while True do
    begin
      // Ici on lit une série d'actions, dans l'ordre de Number

      StrNumber := Format('[%d;', [Number]);
      FirstLine := StringsOps.FindAtPos(FileContents, StrNumber);
      if FirstLine < 0 then
        Break;

      InfoLine := FileContents[FirstLine];
      Inc(FirstLine);
      LastLine := StringsOps.FindAtPos(FileContents, '[', 1, FirstLine);

      // Les actions sont dans les lignes [FirstLine ; LastLine[

      Delete(InfoLine, 1, Length(StrNumber));
      if InfoLine[Length(InfoLine)] <> ']' then
        Break;
      Delete(InfoLine, Length(InfoLine), 1);

      // Maintenant InfoLine est du gabarit 'Kind' ou 'Kind;Graphics'

      Kind := TActionsKind(AnsiIndexStr(
        GetFirstToken(InfoLine, ';'), KindStrings));
      if Ord(Kind) < 0 then
        Break;
      if Kind in CustomActionsKind then
        Graphics := GetLastToken(InfoLine, ';')
      else
        Graphics := '';

      // Déterminer l'ID des actions à créer

      if Kind <> akZone then
        ActionsID := ''
      else
      begin
        Zone.X := StrToIntDef(GetXToken(InfoLine, ';', 2), -1);
        Zone.Y := StrToIntDef(GetXToken(InfoLine, ';', 3), -1);
        Zone.Z := StrToIntDef(GetXToken(InfoLine, ';', 4), -1);
        if (Zone.X < 0) or (Zone.Y < 0) or (Zone.Z < 0) then
          Break;
        ActionsID := Format(idZoneActions, [Zone.X, Zone.Y, Zone.Z]);
      end;

      // Récupération des actions proprement dites

      StringsOps.CopyFrom(SubContents, FileContents,
        FirstLine, LastLine-FirstLine);

      // Création des actions en question

      Actions := TActions.Create(Master, Number, Kind,
        Graphics, SubContents, ActionsID);
      ActionsList.Add(Actions);

      // Passage à l'itération suivante

      Inc(Number);
    end;

    // On s'assure qu'il y a suffisamment d'actions
    SubContents.Clear;
    while Number < MinActionsCount do
    begin
      Actions := TActions.Create(Master, Number, akPushButton,
        '', SubContents, '');
      ActionsList.Add(Actions);
      Inc(Number);
    end;

    TC4xInfos.Create(Master, ActionsList);
  finally
    ActionsList.Free;
    SubContents.Free;
    FileContents.Free;
  end;
end;

{*
  Met à jour les peintres des composants standard
  @param SquaresImgName   Nom de l'image du fichier Cases
*}
procedure UpdatePainters(Master: TMaster; const SquaresImgName: string);
var
  PainterAdapter: TPainterAdapter;
  I: Integer;
begin
  PainterAdapter := TPainterAdapter.Create(Master.ImagesMaster);
  try
    PainterAdapter.SquaresImgName := SquaresImgName;

    for I := 0 to Master.ComponentCount-1 do
      PainterAdapter.AdaptPainters(Master.Components[I]);
  finally
    PainterAdapter.Free;
  end;
end;

{*
  Met à jour les catégories des cases
*}
procedure UpdateSquareCategories(Master: TMaster);
var
  Infos: TC4xInfos;
  I: Integer;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;

  for I := 0 to Infos.ActionsCount-1 do
    if Infos.Actions[I].Kind in RegisteredActionsKind then
      Master.Square[Format(idActionsSquare, [I])].Category := SCategoryButtons;
end;

{---------------------}
{ FunLaby unit events }
{---------------------}

{*
  Initialize unit
*}
procedure InitializeUnit(Master: TMaster);
var
  ActionsFileName: TFileName;
  SquaresImgName: string;
begin
  ActionsFileName := Master.FindResource('../../Sources/Actions.c4x', rkImage);
  SquaresImgName := DefaultSquaresImgName;

  CreateComponents(Master);
  LoadActions(Master, ActionsFileName);
  UpdatePainters(Master, SquaresImgName);
  UpdateSquareCategories(Master);
end;

{*
  Game started
*}
procedure GameStarted(Master: TMaster);
var
  Player: TPlayer;
  Infos: TC4xInfos;
  TestMsg: TPlayerShowMsgMessage;
  I: Integer;
begin
  Player := Master.Players[0];
  Infos := Master.Component[idC4xInfos] as TC4xInfos;

  { If no plug-in handles the ShowMessage player message, give the player a
    default plug-in for his messages. }

  TestMsg.MsgID := msgShowMessage;
  Player.Dispatch(TestMsg);
  if not TestMsg.Handled then
    Player.AddPlugin(Master.Plugin[idDefaultShowMessagePlugin]);

  { Compatibility hacks plug-in. }

  Player.AddPlugin(Master.Plugin[idCompatibilityHacksPlugin]);

  { If there are zone actions, give the player the zone plug-in. }

  for I := 0 to Infos.ActionsCount-1 do
  begin
    if Infos.Actions[I].Kind = akZone then
    begin
      Player.AddPlugin(Master.Plugin[idZonesPlugin]);
      Break;
    end;
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
  Result := TSepiUnit.Create(Root, 'Compatibility4x', ['FunLabyUtils']);

  Result.CurrentVisibility := mvPrivate;

  TSepiMethod.Create(Result, 'InitializeUnit',
    'static procedure(Master: TMaster)').SetCode(@InitializeUnit);
  TSepiMethod.Create(Result, 'GameStarted',
    'static procedure(Master: TMaster)').SetCode(@GameStarted);

  TSepiConstant.Create(Result, 'attrBoatNumber', attrBoatNumber);
  TSepiVariable.Create(Result, 'attrtypeBoatNumber',
    TSepiSystemUnit(Root.SystemUnit).Integer);

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('Compatibility4x', ImportUnit);
finalization
  SepiUnregisterImportedUnit('Compatibility4x');
end.

