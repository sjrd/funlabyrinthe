{*
  Gestion des fichiers unit� de base de FunLabyrinthe
  L'unit� UnitFiles d�crit les classes de gestion des fichiers unit� de base de
  FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
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
    Repr�sente un fichier unit� de type paquet Borland
    TBPLUnitFile repr�sente un fichier unit� qui charge un paquet Borland
    d�finissant les nouveaux �l�ments.
  *}
  TBPLUnitFile = class(TUnitFile)
  private
    FHandle : HMODULE;      /// Module du package charg�
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
  /// Type MIME des unit�s de type package Borland
  BPLMIMEType = 'application/bpl';

  /// Proc�dure de chargement des composants
  LoadComponentsProc = 'LoadComponents';

  /// Proc�dure de d�chargement des composants
  UnloadComponentsProc = 'UnloadComponents';

  /// Proc�dure de chargement du projet
  LoadedProc = 'Loaded';

  /// Proc�dure de d�chargement du projet
  UnloadingProc = 'Unloading';

  /// Proc�dure de commencement de la partie
  GameStartedProc = 'GameStarted';

  /// Proc�dure de terminaison de la partie
  GameEndedProc = 'GameEnded';

  /// Proc�dure d'enregistrement des composants
  RegisterComponentsProc = 'RegisterComponents';

  /// Proc�dure de r�cup�ration des param�tres
  GetParamsProc = 'GetParams';

{*
  Cr�e une instance de TBPLUnitFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Param�tres envoy�s � l'unit�
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
  D�truit l'instance
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
  @param ProcName   Nom de la routine � appeler
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
  Ex�cut� lorsque le projet a �t� compl�tement charg�
  Loaded est appel�e une fois que le projet a �t� compl�tement charg�. � ce
  moment, toutes les unit�s sont charg�es, les cartes �galement, et tous les
  joueurs de m�me, � leurs positions respectives, et avec leurs attributs et/ou
  plug-in.
  Loaded est appel�e aussi bien en mode �dition qu'en mode jeu.
*}
procedure TBPLUnitFile.Loaded;
begin
  Notify(LoadedProc);
end;

{*
  Ex�cut� lorsque le projet est sur le point d'�tre d�charg�
  Unloading est appel�e lorsque le projet est sur le point d'�tre d�charg�. � ce
  moment, tous les objets sont encore accessibles, pour la derni�re fois.
  Unloading est appel�e aussi bien en mode �dition qu'en mode jeu.
*}
procedure TBPLUnitFile.Unloading;
begin
  Notify(UnloadingProc);
end;

{*
  Ex�cut� lorsque la partie vient juste d'�tre commenc�e
  GameStarted est appel�e lorsque la partie vient juste d'�tre commenc�e (en
  mode jeu, donc pas en mode �dition).
*}
procedure TBPLUnitFile.GameStarted;
begin
  Notify(GameStartedProc);
end;

{*
  Ex�cut� lorsque la partie vient juste de se terminer
  GameEnded est appel�e lorsque la partie vient juste d'�tre termin�e (en mode
  jeu, donc pas en mode �dition), avant que le ma�tre FunLabyrinthe ne soit
  lib�r�.
  Une partie est termin�e lorsque plus aucun joueur n'est dans l'�tat psPlaying.
*}
procedure TBPLUnitFile.GameEnded;
begin
  Notify(GameEndedProc);
end;

{*
  Enregistre les diff�rents composants � placer dans la palette d'�dition
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
  Dresse la liste des param�tres � enregistrer
  @param Params   Liste des param�tres
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

