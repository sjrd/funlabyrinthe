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
  FLBFields, C4xCommon, C4xComponents, C4xFields, C4xSquaresTable;

type
  TCompatibility4xUnit = class(TInterfacedUnitFile)
  private
    FSourceHRef: string;
  protected
    procedure GameStarted; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc); override;
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

  TDecorativeEffect.Create(Master, idButtonTemplate, sButtonTemplate, fButton);

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

    TC4xInfos.Create(MasterFile, ActionsList);
  finally
    ActionsList.Free;
    SubContents.Free;
    FileContents.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibility4xUnit.GameStarted;
var
  Infos: TC4xInfos;
  Player: TPlayer;
  I, J: Integer;
  idOldOutside: TComponentID;
  OldWater, OldHole, OldOutside: TSquare;
  Map: TMap;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  Player := Master.Players[0];

  idOldOutside := idOutside+'---';

  for I := 0 to Infos.ActionsCount-1 do
  begin
    case Infos.Actions[I].Kind of
      akOutside:
        idOldOutside := Format(idActionsSquare, [I]);
      akZone:
        Player.AddPlugin(Master.Plugin[idZonesPlugin]);
    end;
  end;

  Player.AddPlugin(Master.Plugin[idCompatibilityHacksPlugin]);
  Player.AddPlugin(Master.Plugin[idGameStartedPlugin]);

  OldWater := Master.Square[idOldWater+'---'];
  OldHole  := Master.Square[idOldHole+'---'];
  OldOutside := Master.Square[idOldOutside];

  for I := 0 to Master.MapCount-1 do
  begin
    Map := Master.Maps[I];

    for J := 0 to Map.LinearMapCount-1 do
    begin
      case AnsiIndexStr(Map.LinearMap[J].Field.ID,
        ['Water', 'Hole', 'Outside']) of
        0: Map.LinearMap[J] := OldWater;
        1: Map.LinearMap[J] := OldHole;
        2: Map.LinearMap[J] := OldOutside;
      end;
    end;
  end;
end;

{*
  [inheritDoc]
*}
procedure TCompatibility4xUnit.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);
var
  Infos: TC4xInfos;
  I: Integer;
  Components: array of TSquareComponent;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  if Infos.ActionsCount > 0 then
  begin
    SetLength(Components, Infos.ActionsCount);

    for I := 0 to Infos.ActionsCount-1 do
    begin
      if Infos.Actions[I].Kind in ActionsKindsWithoutSquare then
        Components[I] := nil
      else
        Components[I] := Master.Square[Format(idActionsSquare, [I])];
    end;

    RegisterComponentSetProc(Master.SquareComponent[idActionsSquareTemplate],
      Components, 0, sButtonTitle,
      Format(sButtonPrompt, [Infos.ActionsCount-1]));
  end;
end;

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

end.

