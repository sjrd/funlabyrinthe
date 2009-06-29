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
  SdDialogs, FunLabyUtils, FilesUtils, UnitFiles, Generics, FLBFields,
  C4xCommon, C4xComponents, C4xFields, C4xSquaresTable;

resourcestring
  sAskForTipsTitle = 'Activation des indices';
  sAskForTips = 'Ce labyrinthe propose certains indices : '+
    'voulez-vous les activer ?';

type
  TCompatibility4xUnit = class(TInterfacedUnitFile)
  private
    FSourceHRef: string;
  protected
    procedure GameStarted; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc); override;

    procedure GetParams(Params: TStrings); override;
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
  FileContents, SubContents, Counters: TStrings;
  ActionsList: TObjectList;
  Number, FirstLine, LastLine: Integer;
  StrNumber, InfoLine, Graphics: string;
  Kind: TActionsKind;
  Infos: TC4xInfos;
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

  TZonesPlugin.Create(Master, idZonesPlugin);

  // Terrains

  TOldWater.Create(Master, idOldWater, sWater);
  TOldHole.Create(Master, idOldHole, sHole);

  // Effets

  for I := 1 to 20 do
    TOldStairs.Create(Master, Format(idOldStairs, [I]), sStairs);

  TDecorativeEffect.Create(Master, idButtonTemplate, sButtonTemplate, fButton);

  TDecorativeEffect.Create(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  // Actions : elles sont stockées dans le fichier donné par FileName

  Counters := TStringList.Create;
  try
    Counters.Delimiter := ' ';

    FileContents := TStringList.Create;
    try
      if FileName <> '' then
        FileContents.LoadFromFile(FileName);
      FileContents.Add('[]');
      Number := 0;
      SubContents := TStringList.Create;
      try
        ActionsList := TObjectList.Create(False);
        try
          Counters.DelimitedText := Params.Values[attrCounters];
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
            if Counters.Count > Number then
              Actions.Counter := StrToIntDef(Counters[Number], 0);

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
            if Counters.Count > Number then
              Actions.Counter := StrToIntDef(Counters[Number], 0);
            Inc(Number);
          end;

          Infos := TC4xInfos.Create(MasterFile, ActionsList);
        finally
          ActionsList.Free;
        end;
      finally
        SubContents.Free;
      end;
    finally
      FileContents.Free;
    end;

    Counters.DelimitedText := Params.Values[attrVariables];
    for I := 1 to Min(Counters.Count, MaxVar) do
      Infos.Variables[I] := StrToIntDef(Counters[I-1], 0);
  finally
    Counters.Free;
  end;

  if Params.Values[attrShowTips] <> '' then
    Infos.ShowTips := Params.Values[attrShowTips] = 'yes';
end;

{*
  [@inheritDoc]
*}
procedure TCompatibility4xUnit.GameStarted;
var
  Infos: TC4xInfos;
  I: Integer;
  DoNextPhase, HasMoved, HasShownMsg, Successful: Boolean;
  WereZones, WereTips: Boolean;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  DoNextPhase := False;
  WereZones := False;
  WereTips := False;

  for I := 0 to Infos.ActionsCount-1 do
  begin
    with Infos.Actions[I] do
    begin
      if Kind = akZone then
        WereZones := True;

      if StringsOps.FindText(Actions, 'Indice') >= 0 then
        WereTips := True;
    end;
  end;

  if WereZones then
    Master.Players[0].AddPlugin(Master.Plugin[idZonesPlugin]);

  if not Infos.KnowShowTips then
  begin
    if not WereTips then
      Infos.ShowTips := False
    else
    begin
      Infos.ShowTips := Master.Players[0].ShowDialog(
        sAskForTipsTitle, sAskForTips, dtConfirmation, dbYesNo) = drYes;
    end;
  end;

  for I := 0 to Infos.ActionsCount-1 do
  begin
    with Infos.Actions[I] do
    begin
      if Kind = akGameStarted then
      begin
        Execute(phExecute, Master.Players[0], False, Master.Players[0].Position,
          DoNextPhase, HasMoved, HasShownMsg, Successful);
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
  Infos: TC4xInfos;
  I: Integer;
  Components: array of TSquareComponent;
begin
  // Effets

  RegSingle(idSunkenButton);

  // Cases

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

    RegSet(idActionsSquareTemplate, Components, 0,
      sButtonTitle, Format(sButtonPrompt, [Infos.ActionsCount-1]));
  end;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibility4xUnit.GetParams(Params: TStrings);
var
  Infos: TC4xInfos;
  I: Integer;
  Counters: string;
begin
  Params.Values[attrFileName] := SourceHRef;

  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  if Infos.ActionsCount > 0 then
  begin
    Counters := '';
    I := Infos.ActionsCount-1;
    while (I >= 0) and (Infos.Actions[I].Counter = 0) do
      Dec(I);
    while I >= 0 do
    begin
      Counters := IntToStr(Infos.Actions[I].Counter) + ' ' + Counters;
      Dec(I);
    end;
    if Counters <> '' then
      Params.Values[attrCounters] := Copy(Counters, 1, Length(Counters)-1);

    Counters := '';
    I := MaxVar;
    while (I > 0) and (Infos.Variables[I] = 0) do
      Dec(I);
    while I > 0 do
    begin
      Counters := IntToStr(Infos.Variables[I]) + ' ' + Counters;
      Dec(I);
    end;
    if Counters <> '' then
      Params.Values[attrVariables] := Copy(Counters, 1, Length(Counters)-1);
  end;

  if Infos.KnowShowTips then
    Params.Values[attrShowTips] := IIF(Infos.ShowTips, 'yes', 'no');
end;

{$IFNDEF DCTD}
exports
  CreateUnitFile name 'CreateUnitFile';
{$ENDIF}

end.

