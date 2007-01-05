{*
  Recense les composants de compatibilit� 4.x de FunLabyrinthe
  L'unit� C4xMain recense tous les composants du package
  Compatibility4x, ceux qui d�crivent les composants de compatibilit� 4.x de
  FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xMain;

interface

uses
  Classes, SysUtils, StrUtils, Math, Contnrs, ScUtils, ScLists, ScStrUtils,
  FunLabyUtils, UnitFiles, Generics, C4xCommon, C4xComponents, C4xScrewsTable;

resourcestring
  sAskForTipsTitle = 'Activation des indices';
  sAskForTips = 'Ce labyrinthe propose certains indices : '+
    'voulez-vous les activer ?';

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
  attrShowTips = 'ShowTips';   /// Affichage des indices

{*
  Charge tous les composants de compatibilit� 4.x de FunLabyrinthe
  @param UnitFile   Fichier unit� appelant
  @param Master     Ma�tre FunLabyrinthe dans lequel charger les composants
  @param Params     Param�tres envoy�s au fichier unit�
*}
procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
const {don't localize}
  KindStrings : array[0..14] of string = (
    'GameStarted', 'PushButton', 'Switch', 'InfoStone', 'Hidden',
    'TransporterNext', 'TransporterPrevious', 'TransporterRandom', 'Outside',
    'Treasure', 'Custom', 'Object', 'Obstacle', 'Direction', 'Zone'
  );
var FileName : TFileName;
    FileContents, SubContents, Counters : TStrings;
    ActionsList : TObjectList;
    Number, FirstLine, LastLine : integer;
    StrNumber, InfoLine, Graphics : string;
    Kind : TActionsKind;
    Infos : TC4xInfos;
    I : integer;
    Zone : T3DPoint;
    ActionsID : TComponentID;
    Actions : TActions;
    C : Char;
begin
  { Don't localize any of the strings in this procedure. }

  try
    FileName := UnitFile.MasterFile.ResolveHRef(
      Params.Values[attrFileName], fUnitsDir);
    UnitFile.Attributes.Values[attrFileName] := Params.Values[attrFileName];
  except
    FileName := '';
  end;

  // Plug-in

  TZonesPlugin.Create(Master, idZonesPlugin);

  // Effets

  for I := 1 to 20 do
    TOldStairs.Create(Master, Format(idOldStairs, [I]), sStairs);

  TDecorativeEffect.Create(Master, idButtonTemplate, sButtonTemplate, fButton);

  TDecorativeEffect.Create(Master, idSunkenButton,
    sSunkenButton, fSunkenButton);

  // Actions : elles sont stock�es dans le fichier donn� par FileName

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
        ActionsList := TObjectList.Create(False);
        try
          Counters.DelimitedText := UnitFile.Attributes.Values[attrCounters];
          while True do
          begin
            // Ici on lit une s�rie d'actions, dans l'ordre de Number

            StrNumber := Format('[%d;', [Number]);
            FirstLine := StringsOps.FindAtPos(FileContents, StrNumber);
            if FirstLine < 0 then Break;

            InfoLine := FileContents[FirstLine];
            inc(FirstLine);
            LastLine := StringsOps.FindAtPos(FileContents, '[', 1, FirstLine);

            // Les actions sont dans les lignes [FirstLine ; LastLine[

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

            // D�terminer l'ID des actions � cr�er

            if Kind <> akZone then ActionsID := '' else
            begin
              Zone.X := StrToIntDef(GetXToken(InfoLine, ';', 2), -1);
              Zone.Y := StrToIntDef(GetXToken(InfoLine, ';', 3), -1);
              Zone.Z := StrToIntDef(GetXToken(InfoLine, ';', 4), -1);
              if (Zone.X < 0) or (Zone.Y < 0) or (Zone.Z < 0) then Break;
              ActionsID := Format(idZoneActions, [Zone.X, Zone.Y, Zone.Z]);
            end;

            // R�cup�ration des actions proprement dites

            StringsOps.CopyFrom(SubContents, FileContents,
              FirstLine, LastLine-FirstLine);

            // Cr�ation des actions en question

            Actions := TActions.Create(Master, Number, Kind,
              Graphics, SubContents, ActionsID);
            ActionsList.Add(Actions);
            if Counters.Count > Number then
              Actions.Counter := StrToIntDef(Counters[Number], 0);

            // Passage � l'it�ration suivante

            inc(Number);
          end;

          Infos := TC4xInfos.Create(UnitFile.MasterFile, ActionsList);
        finally
          ActionsList.Free;
        end;
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

  if Params.Values[attrShowTips] <> '' then
    Infos.ShowTips := Params.Values[attrShowTips] = 'yes';

  // Les composants de compatibilit� ne g�rent pas le comptage de r�f�rences
  // On s'assure donc que les cases utilis�es sont les m�mes du d�but � la fin
  for C := #33 to #255 do if ScrewsTable[C] <> '' then
    Master.Screw[ScrewsTable[C]].AddRef;
end;

{*
  Ex�cut� lorsque la partie vient juste d'�tre commenc�e
  GameStarted est appel�e lorsque la partie vient juste d'�tre commenc�e (en
  mode jeu, donc pas en mode �dition).
  @param UnitFile   Fichier unit� appelant
  @param Master     Ma�tre FunLabyrinthe
*}
procedure GameStarted(UnitFile : TBPLUnitFile; Master : TMaster);
var Infos : TC4xInfos;
    I : integer;
    DoNextPhase, HasMoved, HasShownMsg, Successful : boolean;
    WereZones, WereTips : boolean;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  DoNextPhase := False;
  WereZones := False;
  WereTips := False;

  for I := 0 to Infos.ActionsCount-1 do with Infos.Actions[I] do
  begin
    if Kind = akZone then
      WereZones := True;

    if StringsOps.FindText(Actions, 'Indice') >= 0 then
      WereTips := True;
  end;

  if WereZones then
    Master.Players[0].AddPlugin(Master.Plugin[idZonesPlugin]);

  if not Infos.KnowShowTips then
  begin
    if not WereTips then Infos.ShowTips := False else
    begin
      Infos.ShowTips := Master.Players[0].ShowDialog(
        sAskForTipsTitle, sAskForTips, dtConfirmation, dbYesNo) = drYes;
    end;
  end;

  for I := 0 to Infos.ActionsCount-1 do with Infos.Actions[I] do
  begin
    if Kind = akGameStarted then
    begin
      Execute(phExecute, Master.Players[0], False, Master.Players[0].Position,
        DoNextPhase, HasMoved, HasShownMsg, Successful);
    end;
  end;
end;

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
  @param UnitFile                      Fichier unit� appelant
  @param Master                        Ma�tre FunLabyrinthe
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
      if Infos.Actions[I].Kind in ActionsKindsWithoutScrew then
        Components[I] := nil
      else
        Components[I] := Master.Screw[Format(idActionsScrew, [I])];
    end;

    RegSet(idActionsScrewTemplate, Components, 0,
      sButtonTitle, Format(sButtonPrompt, [Infos.ActionsCount-1]));
  end;
end;

{*
  Dresse la liste des param�tres � enregistrer
  @param UnitFile   Fichier unit� appelant
  @param Master     Ma�tre FunLabyrinthe
  @param Params     Liste des param�tres
*}
procedure GetParams(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
var Infos : TC4xInfos;
    I : integer;
    Counters : string;
begin
  Params.Values[attrFileName] := UnitFile.Attributes.Values[attrFileName];

  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  if Infos.ActionsCount > 0 then
  begin
    Counters := '';
    I := Infos.ActionsCount-1;
    while (I >= 0) and (Infos.Actions[I].Counter = 0) do dec(I);
    while I >= 0 do
    begin
      Counters := IntToStr(Infos.Actions[I].Counter) + ' ' + Counters;
      dec(I);
    end;
    if Counters <> '' then
      Params.Values[attrCounters] := Copy(Counters, 1, Length(Counters)-1);

    Counters := '';
    I := MaxVar;
    while (I > 0) and (Infos.Variables[I] = 0) do dec(I);
    while I > 0 do
    begin
      Counters := IntToStr(Infos.Variables[I]) + ' ' + Counters;
      dec(I);
    end;
    if Counters <> '' then
      Params.Values[attrVariables] := Copy(Counters, 1, Length(Counters)-1);
  end;

  if Infos.KnowShowTips then
    Params.Values[attrShowTips] := IIF(Infos.ShowTips, 'yes', 'no');
end;

{$IFNDEF DCTD}
exports
  LoadComponents name 'LoadComponents',
  GameStarted name 'GameStarted',
  RegisterComponents name 'RegisterComponents',
  GetParams name 'GetParams';
{$ENDIF}

end.

