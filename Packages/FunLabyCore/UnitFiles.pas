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
  SysUtils, Classes, FunLabyUtils, FilesUtils;

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

    procedure Notify(const ProcName : string);
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      Params : TStrings); override;
    destructor Destroy; override;

    procedure Loaded; override;
    procedure Unloading; override;

    procedure GameStarted; override;
    procedure GameEnded; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc); override;

    procedure GetParams(Params : TStrings); override;

    property Attributes : TStrings read FAttributes;
  end;

implementation

uses
  Windows;

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

  /// Procédure de chargement du projet
  LoadedProc = 'Loaded';

  /// Procédure de déchargement du projet
  UnloadingProc = 'Unloading';

  /// Procédure de commencement de la partie
  GameStartedProc = 'GameStarted';

  /// Procédure de terminaison de la partie
  GameEndedProc = 'GameEnded';

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
  Appelle une routine de notification du paquet sous-jacent
  @param ProcName   Nom de la routine à appeler
*}
procedure TBPLUnitFile.Notify(const ProcName : string);
type
  TNotifyProc = procedure(UnitFile : TBPLUnitFile; Master : TMaster); stdcall;
var Notify : TNotifyProc;
begin
  Notify := TNotifyProc(
    GetProcAddress(FHandle, PChar(ProcName)));

  if Assigned(Notify) then
    Notify(Self, Master);
end;

{*
  Exécuté lorsque le projet a été complètement chargé
  Loaded est appelée une fois que le projet a été complètement chargé. À ce
  moment, toutes les unités sont chargées, les cartes également, et tous les
  joueurs de même, à leurs positions respectives, et avec leurs attributs et/ou
  plug-in.
  Loaded est appelée aussi bien en mode édition qu'en mode jeu.
*}
procedure TBPLUnitFile.Loaded;
begin
  Notify(LoadedProc);
end;

{*
  Exécuté lorsque le projet est sur le point d'être déchargé
  Unloading est appelée lorsque le projet est sur le point d'être déchargé. À ce
  moment, tous les objets sont encore accessibles, pour la dernière fois.
  Unloading est appelée aussi bien en mode édition qu'en mode jeu.
*}
procedure TBPLUnitFile.Unloading;
begin
  Notify(UnloadingProc);
end;

{*
  Exécuté lorsque la partie vient juste d'être commencée
  GameStarted est appelée lorsque la partie vient juste d'être commencée (en
  mode jeu, donc pas en mode édition).
*}
procedure TBPLUnitFile.GameStarted;
begin
  Notify(GameStartedProc);
end;

{*
  Exécuté lorsque la partie vient juste de se terminer
  GameEnded est appelée lorsque la partie vient juste d'être terminée (en mode
  jeu, donc pas en mode édition), avant que le maître FunLabyrinthe ne soit
  libéré.
  Une partie est terminée lorsque plus aucun joueur n'est dans l'état psPlaying.
*}
procedure TBPLUnitFile.GameEnded;
begin
  Notify(GameEndedProc);
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

