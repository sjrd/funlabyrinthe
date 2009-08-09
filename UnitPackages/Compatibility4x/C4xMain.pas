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
  Classes, SysUtils, StrUtils, Math, Contnrs, ScUtils, ScLists, ScStrUtils,
  SdDialogs, FunLabyUtils, FilesUtils, UnitFiles, Generics,
  FLBFields, FLBSimpleEffects, FLBBoat,
  C4xCommon, C4xComponents, C4xFields, C4xSquaresTable;

type
  TCompatibility4xUnit = class(TInterfacedUnitFile)
  private
    FSourceHRef: string;
  protected
    procedure GameStarted; override;
  public
    constructor Create(AMasterFile: TMasterFile; Params: TStrings);

    property SourceHRef: string read FSourceHRef;
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

{-----------------------------}
{ Classe TCompatibility4xUnit }
{-----------------------------}

const {don't localize}
  attrFileName = 'FileName';   /// Attribut pour le nom de fichier
  attrCounters = 'Counters';   /// Enregistrement des compteurs
  attrVariables = 'Variables'; /// Enregistrement des variables
  attrShowTips = 'ShowTips';   /// Affichage des indices

{*
  Charge tous les composants de compatibilité 4.x de FunLabyrinthe
  @param AMasterFile   Fichier maître
  @param Params        Paramètres envoyés au fichier unité
*}
constructor TCompatibility4xUnit.Create(AMasterFile: TMasterFile;
  Params: TStrings);
const {don't localize}
  KindStrings: array[0..14] of string = (
    'GameStarted', 'PushButton', 'Switch', 'InfoStone', 'Hidden',
    'TransporterNext', 'TransporterPrevious', 'TransporterRandom', 'Outside',
    'Treasure', 'Custom', 'Object', 'Obstacle', 'Direction', 'Zone'
  );
var
  FileName: TFileName;
  FileContents, SubContents: TStrings;
  ActionsList: TObjectList;
  Number, FirstLine, LastLine: Integer;
  StrNumber, InfoLine, Graphics: string;
  Kind: TActionsKind;
  I: Integer;
  Zone: T3DPoint;
  ActionsID: TComponentID;
  Actions: TActions;
  Infos: TC4xInfos;
begin
  { Don't localize any of the strings in this procedure. }

  inherited Create(AMasterFile);

  try
    FileName := MasterFile.ResolveHRef(
      Params.Values[attrFileName], fUnitsDir);
    FSourceHRef := Params.Values[attrFileName];
  except
    FileName := '';
  end;

  // Supprimer certains composants de FunLabyBase

  Master.Component[idTransporterCreator].Free;
  Master.Component[idBoatCreator].Free;

  // Plug-in

  TGameStartedPlugin.Create(Master, idGameStartedPlugin);
  TZonesPlugin.Create(Master, idZonesPlugin);
  TCompatibilityHacksPlugin.Create(Master, idCompatibilityHacksPlugin);

  // Terrains

  TOldWater.Create(Master, idOldWater, sWater);
  TOldHole.Create(Master, idOldHole, sHole);

  // Effets

  for I := 1 to 20 do
    TOldStairs.Create(Master, Format(idOldStairs, [I]), sStairs);

  // Outils

  for I := 1 to 10 do
    TNumberedBoat.Create(Master, Format(idNumberedBoat, [I]),
      Format(sNumberedBoat, [I]), I);

  // Actions : elles sont stockées dans le fichier donné par FileName

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

    Infos := TC4xInfos.Create(MasterFile, ActionsList);
  finally
    ActionsList.Free;
    SubContents.Free;
    FileContents.Free;
  end;

  for I := 0 to Infos.ActionsCount-1 do
    if Infos.Actions[I].Kind in RegisteredActionsKind then
      Master.Square[Format(idActionsSquare, [I])].Category := SCategoryButtons;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibility4xUnit.GameStarted;
var
  Infos: TC4xInfos;
  Player: TPlayer;
  I, J: Integer;
  idOldOutside, idOldTreasure: TComponentID;
  OldWater, OldHole, OldOutside, OldTreasure: TSquare;
  Map: TMap;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  Player := Master.Players[0];

  idOldOutside := idOutside+'---';
  idOldTreasure := idGrass+'-'+idTreasure+'--';

  for I := 0 to Infos.ActionsCount-1 do
  begin
    case Infos.Actions[I].Kind of
      akOutside:
        idOldOutside := Format(idActionsSquare, [I]);
      akTreasure:
        idOldTreasure := Format(idActionsSquare, [I]);
      akZone:
        Player.AddPlugin(Master.Plugin[idZonesPlugin]);
    end;
  end;

  Player.AddPlugin(Master.Plugin[idCompatibilityHacksPlugin]);
  Player.AddPlugin(Master.Plugin[idGameStartedPlugin]);

  OldWater := Master.Square[idOldWater+'---'];
  OldHole  := Master.Square[idOldHole+'---'];
  OldOutside := Master.Square[idOldOutside];
  OldTreasure := Master.Square[idOldTreasure];

  for I := 0 to Master.MapCount-1 do
  begin
    Map := Master.Maps[I];

    for J := 0 to Map.LinearMapCount-1 do
    begin
      case AnsiIndexStr(Map.LinearMap[J].Field.ID,
        [idWater, idHole, idOutside]) of
        0: Map.LinearMap[J] := OldWater;
        1: Map.LinearMap[J] := OldHole;
        2: Map.LinearMap[J] := OldOutside;
      end;

      if Map.LinearMap[J].Effect.ID = idTreasure then
        Map.LinearMap[J] := OldTreasure;
    end;
  end;
end;

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

end.

