{*
  Gestion des fichiers unité de base de FunLabyrinthe
  L'unité UnitFiles décrit les classes de gestion des fichiers unité de base de
  FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit UnitFiles;

interface

uses
  Windows, SysUtils, Classes, FunLabyUtils, FilesUtils;

resourcestring
  sCantLoadPackage = 'Impossible de charger le paquet "%s"';

type
  {*
    Représente un fichier unité de type paquet Borland
    TBPLUnitFile représente un fichier unité qui charge un paquet Borland
    définissant les nouveaux éléments.
  *}
  TBPLUnitFile = class(TUnitFile)
  private
    FHandle : HMODULE;      /// Module du package chargé
    FAttributes : TStrings; /// Attributs libres pour le package
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      Params : TStrings); override;
    destructor Destroy; override;

    procedure GameLoaded(FirstTime : boolean); override;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc); override;

    procedure GetParams(Params : TStrings); override;

    property Attributes : TStrings read FAttributes;
  end;

implementation

{---------------------}
{ Classe TBPLUnitFile }
{---------------------}

const {don't localize}
  /// Type MIME des unités de type package Borland
  BPLMIMEType = 'application/bpl';

  /// Procédure de chargement des composants
  LoadComponentsProc = 'LoadComponents';

  /// Procédure de déchargement des composants
  UnloadComponentsProc = 'UnloadComponents';

  /// Procédure de chargement de la partie
  GameLoadedProc = 'GameLoaded';

  /// Procédure d'enregistrement des composants
  RegisterComponentsProc = 'RegisterComponents';

  /// Procédure de récupération des paramètres
  GetParamsProc = 'GetParams';

{*
  Crée une instance de TBPLUnitFile
  @param AMasterFile   Fichier maître
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Paramètres envoyés à l'unité
*}
constructor TBPLUnitFile.Create(AMasterFile : TMasterFile; const AHRef : string;
  const AFileName : TFileName; const AMIMEType : string; Params : TStrings);
type
  TLoadComponentsProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    Params : TStrings); stdcall;
var LoadComponents : TLoadComponentsProc;
begin
  inherited;

  FAttributes := TStringList.Create;
  FAttributes.Assign(Params);

  FHandle := LoadPackage(FileName);
  if FHandle = 0 then
    raise EFileError.CreateFmt(sCantLoadPackage, [FileName]);

  LoadComponents := TLoadComponentsProc(
    GetProcAddress(FHandle, LoadComponentsProc));

  if Assigned(LoadComponents) then
    LoadComponents(Self, Master, Params);
end;

{*
  Détruit l'instance
*}
destructor TBPLUnitFile.Destroy;
type
  TUnloadComponentsProc = procedure(UnitFile : TBPLUnitFile;
    Master : TMaster); stdcall;
var UnloadComponents : TUnloadComponentsProc;
begin
  UnloadComponents := TUnloadComponentsProc(
    GetProcAddress(FHandle, UnloadComponentsProc));

  if Assigned(UnloadComponents) then
    UnloadComponents(Self, Master);

  if FHandle <> 0 then
    UnloadPackage(FHandle);

  FAttributes.Free;

  inherited;
end;

{*
  Exécuté lorsque la partie vient juste d'être chargée
  GameLoaded est appelée lorsque la partie vient juste d'être chargée (en mode
  jeu, donc pas en mode édition), que ce soit pour la première fois ou à la
  suite du chargement d'une sauvegarde.
  @param FirstTime   Indique si c'est la première fois que la partie est chargée
*}
procedure TBPLUnitFile.GameLoaded(FirstTime : boolean);
type
  TGameLoadedProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    FirstTime : boolean); stdcall;
var GameLoaded : TGameLoadedProc;
begin
  GameLoaded := TGameLoadedProc(
    GetProcAddress(FHandle, GameLoadedProc));

  if Assigned(GameLoaded) then
    GameLoaded(Self, Master, FirstTime);
end;

{*
  Enregistre les différents composants à placer dans la palette d'édition
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure TBPLUnitFile.RegisterComponents(
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);
type
  TRegisterComponentsProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    RegisterSingleComponentProc : TRegisterSingleComponentProc;
    RegisterComponentSetProc : TRegisterComponentSetProc); stdcall;
var RegisterComponents : TRegisterComponentsProc;
begin
  RegisterComponents := TRegisterComponentsProc(
    GetProcAddress(FHandle, RegisterComponentsProc));

  if Assigned(RegisterComponents) then
    RegisterComponents(Self, Master,
      RegisterSingleComponentProc, RegisterComponentSetProc);
end;

{*
  Dresse la liste des paramètres à enregistrer
  @param Params   Liste des paramètres
*}
procedure TBPLUnitFile.GetParams(Params : TStrings);
type
  TGetParamsProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster;
    Params : TStrings); stdcall;
var GetParams : TGetParamsProc;
begin
  GetParams := TGetParamsProc(GetProcAddress(FHandle, GetParamsProc));

  if Assigned(GetParams) then
    GetParams(Self, Master, Params);
end;

initialization
  TMasterFile.RegisterUnitFileClass(BPLMIMEType, TBPLUnitFile);
finalization
  TMasterFile.UnregisterUnitFileClass(BPLMIMEType, TBPLUnitFile);
end.

