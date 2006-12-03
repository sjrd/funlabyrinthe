{*
  Recense les composants de compatibilité 4.x de FunLabyrinthe
  L'unité C4xMain recense tous les composants du package
  Compatibility4x, ceux qui décrivent les composants de compatibilité 4.x de
  FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xMain;

interface

uses
  Classes, SysUtils, StrUtils, Math, ScUtils, ScLists, ScStrUtils, FunLabyUtils,
  UnitFiles, C4xCommon, C4xComponents;

procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings); stdcall;

procedure GameStarted(UnitFile : TBPLUnitFile; Master : TMaster); stdcall;

procedure RegisterComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;

procedure GetParams(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings); stdcall;

implementation

const {don't localize}
  attrFileName = 'FileName';   /// Attribut pour le nom de fichier
  attrCounters = 'Counters';   /// Enregistrement des compteurs
  attrVariables = 'Variables'; /// Enregistrement des variables

{*
  Charge tous les composants de compatibilité 4.x de FunLabyrinthe
  @param UnitFile   Fichier unité appelant
  @param Master     Maître FunLabyrinthe dans lequel charger les composants
  @param Params     Paramètres envoyés au fichier unité
*}
procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
const {don't localize}
  KindStrings : array[0..13] of string = (
    'GameStarted', 'PushButton', 'Switch', 'InfoStone', 'Hidden',
    'TransporterNext', 'TransporterPrevious', 'TransporterRandom', 'Outside',
    'Treasure', 'Custom', 'Object', 'Obstacle', 'Direction'
  );
var FileName : TFileName;
    FileContents, SubContents, Counters : TStrings;
    Number, FirstLine, LastLine : integer;
    StrNumber, InfoLine, Graphics : string;
    Kind : TActionsKind;
    Infos : TC4xInfos;
    I : integer;
begin
  { Don't localize any of the strings in this procedure. }

  try
    FileName := UnitFile.MasterFile.ResolveHRef(
      Params.Values[attrFileName], fUnitsDir);
    UnitFile.Attributes.Values[attrFileName] := Params.Values[attrFileName];
  except
    FileName := '';
  end;

  // Effets

  TDecorativeEffect.Create(Master, idButtonTemplate, sButtonTemplate, fButton);

  TDecorativeEffect.Create(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  // Actions : elles sont stockées dans le fichier donné par FileName

  Counters := TStringList.Create;
  try
    Counters.Delimiter := ' ';

    if FileName = '' then exit;
    FileContents := TStringList.Create;
    try
      FileContents.LoadFromFile(FileName);
      FileContents.Add('[]');
      Number := 0;
      SubContents := TStringList.Create;
      try
        Counters.DelimitedText := UnitFile.Attributes.Values[attrCounters];
        while True do
        begin
          // Ici on lit une série d'actions, dans l'ordre de Number

          StrNumber := Format('[%d;', [Number]);
          FirstLine := StringsOps.FindAtPos(FileContents, StrNumber);
          if FirstLine < 0 then Break;

          InfoLine := FileContents[FirstLine];
          inc(FirstLine);
          LastLine := StringsOps.FindAtPos(FileContents, '[', 1, FirstLine);

          // Maintenant les actions sont dans les lignes [FirstLine ; LastLine[

          Delete(InfoLine, 1, Length(StrNumber));
          if InfoLine[Length(InfoLine)] <> ']' then Break;
          Delete(InfoLine, Length(InfoLine), 1);

          // Maintenant InfoLine est du gabarit 'Kind' ou 'Kind;Graphics'

          Kind := TActionsKind(AnsiIndexStr(
            GetFirstToken(InfoLine, ';'), KindStrings));
          if Ord(Kind) < 0 then Break;
          if Kind in CustomActionsKind then
            Graphics := GetLastToken(InfoLine, ';')
          else
            Graphics := '';

          // Récupération des actions proprement dites

          StringsOps.CopyFrom(SubContents, FileContents,
            FirstLine, LastLine-FirstLine);

          // Création des actions en question

          with TActions.Create(Master, Number, Kind, Graphics, SubContents) do
          begin
            if Counters.Count > Number then
              Counter := StrToIntDef(Counters[Number], 0);
          end;

          // Passage à l'itération suivante

          inc(Number);
        end;

        Infos := TC4xInfos.Create(UnitFile.MasterFile, Number);
      finally
        SubContents.Free;
      end;
    finally
      FileContents.Free;
    end;

    Counters.DelimitedText := UnitFile.Attributes.Values[attrVariables];
    for I := 1 to Min(Counters.Count, MaxVar) do
      Infos.Variables[I] := StrToIntDef(Counters[I-1], 0);
  finally
    Counters.Free;
  end;
end;

{*
  Exécuté lorsque la partie vient juste d'être commencée
  GameStarted est appelée lorsque la partie vient juste d'être commencée (en
  mode jeu, donc pas en mode édition).
  @param UnitFile   Fichier unité appelant
  @param Master     Maître FunLabyrinthe
*}
procedure GameStarted(UnitFile : TBPLUnitFile; Master : TMaster);
var Infos : TC4xInfos;
    I : integer;
    DoNextPhase, HasMoved, HasShownMsg : boolean;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  DoNextPhase := False;

  for I := 0 to Infos.ActionsCount-1 do
  begin
    with Infos.Actions[I] do if Kind = akGameStarted then
    begin
      Execute(phExecute, Master.Players[0], False, No3DPoint,
        DoNextPhase, HasMoved, HasShownMsg);
    end;
  end;
end;

{*
  Enregistre les différents composants à placer dans la palette d'édition
  @param UnitFile                      Fichier unité appelant
  @param Master                        Maître FunLabyrinthe
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure RegisterComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);

  procedure RegSingle(Component : TComponentID);
  begin
    RegisterSingleComponentProc(Master.ScrewComponent[Component]);
  end;

  procedure RegSet(Template : TComponentID;
    const Components : array of TScrewComponent; BaseIndex : integer;
    const DialogTitle, DialogPrompt : string);
  begin
    RegisterComponentSetProc(Master.ScrewComponent[Template],
      Components, BaseIndex, DialogTitle, DialogPrompt);
  end;

var Infos : TC4xInfos;
    I : integer;
    Components : array of TScrewComponent;
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
      case Infos.Actions[I].Kind of
        akGameStarted : Components[I] := nil;
        akObstacle :
        begin
          Components[I] :=
            Master.Screw[Format(idActionsScrewWithObstacle, [I])];
        end;
        else Components[I] := Master.Screw[Format(idActionsScrew, [I])];
      end;
    end;

    RegSet(idActionsScrewTemplate, Components, 0,
      sButtonTitle, Format(sButtonPrompt, [Infos.ActionsCount-1]));
  end;
end;

{*
  Dresse la liste des paramètres à enregistrer
  @param UnitFile   Fichier unité appelant
  @param Master     Maître FunLabyrinthe
  @param Params     Liste des paramètres
*}
procedure GetParams(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
var Infos : TC4xInfos;
    I : integer;
    Counters : TStrings;
begin
  Params.Values[attrFileName] := UnitFile.Attributes.Values[attrFileName];

  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  if Infos.ActionsCount > 0 then
  begin
    Counters := TStringList.Create;
    try
      Counters.Delimiter := ' ';

      for I := 0 to Infos.ActionsCount-1 do
        Counters.Add(IntToStr(Infos.Actions[I].Counter));
      Params.Values[attrCounters] := Counters.DelimitedText;

      Counters.Clear;
      for I := 1 to MaxVar do
        Counters.Add(IntToStr(Infos.Variables[I]));
      Params.Values[attrVariables] := Counters.DelimitedText;
    finally
      Counters.Free;
    end;
  end;
end;

{$IFNDEF DCTD}
exports
  LoadComponents name 'LoadComponents',
  GameStarted name 'GameStarted',
  RegisterComponents name 'RegisterComponents',
  GetParams name 'GetParams';
{$ENDIF}

end.

