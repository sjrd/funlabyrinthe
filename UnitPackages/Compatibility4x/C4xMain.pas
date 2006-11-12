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
  Classes, SysUtils, StrUtils, ScUtils, ScLists, ScStrUtils, FunLabyUtils,
  UnitFiles, C4xCommon, C4xComponents;

procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings); stdcall;

procedure GameLoaded(UnitFile : TBPLUnitFile; Master : TMaster;
  FirstTime : boolean); stdcall;

procedure RegisterComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;

implementation

const {don't localize}
  attrFileName = 'FileName';         /// Attribut pour le nom de fichier
  attrActionsCount = 'ActionsCount'; /// Nombre d'actions cr��es

{*
  Charge tous les composants de compatibilit� 4.x de FunLabyrinthe
  @param UnitFile   Fichier unit� appelant
  @param Master     Ma�tre FunLabyrinthe dans lequel charger les composants
  @param Params     Param�tres envoy�s au fichier unit�
*}
procedure LoadComponents(UnitFile : TBPLUnitFile; Master : TMaster;
  Params : TStrings);
const {don't localize}
  KindStrings : array[0..11] of string = (
    'PushButton', 'Switch', 'InfoStone', 'Hidden', 'TransporterNext',
    'TransporterPrevious', 'TransporterRandom', 'Outside', 'Custom', 'Object',
    'Obstacle', 'Direction'
  );
var FileName : TFileName;
    FileContents, SubContents : TStrings;
    Number, FirstLine, LastLine : integer;
    StrNumber, InfoLine, Graphics : string;
    Kind : TActionsKind;
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

  // Actions : elles sont stock�es dans le fichier donn� par FileName

  if FileName = '' then exit;
  FileContents := TStringList.Create;
  try
    FileContents.LoadFromFile(FileName);
    FileContents.Add('[]');
    Number := 0;
    SubContents := TStringList.Create;
    try
      while True do
      begin
        // Ici on lit une s�rie d'actions, dans l'ordre de Number

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

        // R�cup�ration des actions proprement dites

        StringsOps.CopyFrom(SubContents, FileContents,
          FirstLine, LastLine-FirstLine);

        // Cr�ation des actions en question

        TActions.Create(Master, Number, Kind, Graphics, SubContents);
      end;
    finally
      SubContents.Free;
      UnitFile.Attributes.Values[attrActionsCount] := IntToStr(Number);
    end;
  finally
    FileContents.Free;
  end;
end;

{*
  Ex�cut� lorsque la partie vient juste d'�tre charg�e
  GameLoaded est appel�e lorsque la partie vient juste d'�tre charg�e (en mode
  jeu, donc pas en mode �dition), que ce soit pour la premi�re fois ou � la
  suite du chargement d'une sauvegarde.
  @param UnitFile    Fichier unit� appelant
  @param FirstTime   Indique si c'est la premi�re fois que la partie est charg�e
*}
procedure GameLoaded(UnitFile : TBPLUnitFile; Master : TMaster;
  FirstTime : boolean);
var Count, I : integer;
    Actions : TActions;
    Bool : boolean;
begin
  Count := StrToIntDef(UnitFile.Attributes.Values[attrActionsCount], 0);
  Bool := False;

  for I := 0 to Count-1 do
  begin
    Actions := TActions(Master.Component[Format(idActions, [I])]);
    if Actions.Kind = akGameStarted then
      Actions.Execute(phExecute, Master.Players[0], False, No3DPoint, Bool);
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

var Count, I : integer;
    Components : array of TScrewComponent;
begin
  // Effets

  RegSingle(idSunkenButton);

  // Cases

  Count := StrToIntDef(UnitFile.Attributes.Values[attrActionsCount], 0);
  if Count > 0 then
  begin
    SetLength(Components, Count);

    for I := 0 to Count-1 do
    begin
      if TActions(Master.Component[Format(idActions, [I])]).Kind =
           akGameStarted then
        Components[I] := nil
      else
        Components[I] := Master.Screw[Format(idActionsScrew, [I])];
    end;

    RegSet(idActionsScrewTemplate, Components, 0,
      sButtonTitle, sButtonPrompt);
  end;
end;

exports
  LoadComponents name 'LoadComponents',
  GameLoaded name 'GameLoaded',
  RegisterComponents name 'RegisterComponents';

end.

