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
  ScStrUtils, SdDialogs, FunLabyUtils, FilesUtils, UnitFiles, Generics,
  FLBFields, FLBSimpleEffects, FLBSimpleObjects, FLBPlank, FLBLift,
  FLBObstacles, FLBShowMessage,
  C4xCommon, C4xComponents, C4xFields, C4xBoat, C4xSquaresTable;

type
  TCompatibility4xUnit = class(TInterfacedUnitFile)
  private
    FInfos: TC4xInfos; /// Informations générales

    procedure CreateComponents;
    procedure LoadActions(const FileName: TFileName);
    procedure UpdatePainters(const SquaresImgName: string);
    procedure UpdateSquareCategories;
  protected
    procedure GameStarted; override;
  public
    constructor Create(AMasterFile: TMasterFile; Params: TStrings);

    property Infos: TC4xInfos read FInfos;
  end;

function CreateUnitFile(BPLHandler: TBPLUnitFile; Master: TMaster;
  Params: TStrings): IUnitFile50;

implementation

{*
  Crée l'unité Compatibility4x
  @param BPLHandler   Gestionnaire d'unité BPL prenant en charge ce paquet
  @param Master       Maître FunLabyrinthe
  @param Params       Paramètres passés à l'unité
  @return Interface de l'unité Compatibility4x créée
*}
function CreateUnitFile(BPLHandler: TBPLUnitFile; Master: TMaster;
  Params: TStrings): IUnitFile50;
begin
  Result := TCompatibility4xUnit.Create(BPLHandler.MasterFile, Params);
end;

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

{-----------------------------}
{ Classe TCompatibility4xUnit }
{-----------------------------}

const {don't localize}
  attrFileName = 'FileName';             /// Attribut pour le nom de fichier
  attrSquaresImgName = 'SquaresImgName'; /// Nom de l'image du fichier Cases

{*
  Charge tous les composants de compatibilité 4.x de FunLabyrinthe
  @param AMasterFile   Fichier maître
  @param Params        Paramètres envoyés au fichier unité
*}
constructor TCompatibility4xUnit.Create(AMasterFile: TMasterFile;
  Params: TStrings);
var
  ActionsFileName: TFileName;
  SquaresImgName: string;
begin
  inherited Create(AMasterFile);

  try
    ActionsFileName := MasterFile.ResolveHRef(
      Params.Values[attrFileName], fUnitsDir);
  except
    ActionsFileName := '';
  end;

  SquaresImgName := RemoveDiacritics(Params.Values[attrSquaresImgName]);
  if Master.ImagesMaster.ResolveImgName(fCompatibility+SquaresImgName) = '' then
    SquaresImgName := DefaultSquaresImgName;

  CreateComponents;
  LoadActions(ActionsFileName);
  UpdatePainters(SquaresImgName);
  UpdateSquareCategories;
end;

{*
  Crée les composants de cette unité
*}
procedure TCompatibility4xUnit.CreateComponents;
var
  I: Integer;
  Buoys, Planks, SilverKeys, GoldenKeys: TObjectDef;
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

  TStairs.CreateStairs(Master, idUpStairs, True);
  TStairs.CreateStairs(Master, idDownStairs, False);
  TLift.Create(Master, idLift);

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
procedure TCompatibility4xUnit.LoadActions(const FileName: TFileName);
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

    FInfos := TC4xInfos.Create(MasterFile, ActionsList);
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
procedure TCompatibility4xUnit.UpdatePainters(const SquaresImgName: string);
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
procedure TCompatibility4xUnit.UpdateSquareCategories;
var
  I: Integer;
begin
  for I := 0 to Infos.ActionsCount-1 do
    if Infos.Actions[I].Kind in RegisteredActionsKind then
      Master.Square[Format(idActionsSquare, [I])].Category := SCategoryButtons;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibility4xUnit.GameStarted;
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

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

end.

