{*
  Gestion des fichiers unit� de base de FunLabyrinthe
  L'unit� UnitFiles d�crit les classes de gestion des fichiers unit� de base de
  FunLabyrinthe.
  @author sjrd
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
    Interface d'un fichier unit�
    @author sjrd
    @version 5.0
  *}
  IUnitFile50 = interface
    ['{F9BDF4FD-9FC4-4B6B-BB12-6F587E4AB3C5}']

    {*
      Ex�cut� lorsque le projet a �t� compl�tement charg�
      Loaded est appel�e une fois que le projet a �t� compl�tement charg�. � ce
      moment, toutes les unit�s sont charg�es, les cartes �galement, et tous les
      joueurs de m�me, � leurs positions respectives, et avec leurs attributs
      et/ou plug-in.
      Loaded est appel�e aussi bien en mode �dition qu'en mode jeu.
    *}
    procedure Loaded;

    {*
      Ex�cut� lorsque le projet est sur le point d'�tre d�charg�
      Unloading est appel�e lorsque le projet est sur le point d'�tre d�charg�.
      � ce moment, tous les objets sont encore accessibles, pour la derni�re
      fois.
      Unloading est appel�e aussi bien en mode �dition qu'en mode jeu.
    *}
    procedure Unloading;

    {*
      Ex�cut� lorsque la partie vient juste d'�tre commenc�e
      GameStarted est appel�e lorsque la partie vient juste d'�tre commenc�e (en
      mode jeu, donc pas en mode �dition).
    *}
    procedure GameStarted;

    {*
      Ex�cut� lorsque la partie vient juste de se terminer
      GameEnded est appel�e lorsque la partie vient juste d'�tre termin�e (en
      mode jeu, donc pas en mode �dition), avant que le ma�tre FunLabyrinthe ne
      soit lib�r�.
      Une partie est termin�e lorsque plus aucun joueur n'est dans l'�tat
      psPlaying.
    *}
    procedure GameEnded;

    {*
      Enregistre les diff�rents composants � placer dans la palette d'�dition
      @param RegisterSingleComponentProc   Call-back pour un unique composant
      @param RegisterComponentSetProc      Call-back pour un set de composants
    *}
    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc);

    {*
      Dresse la liste des param�tres � enregistrer
      Les descendants de TUnitFile peuvent surcharger cette m�thode pour
      indiquer au fichier ma�tre les param�tres qu'il doit enregistrer.
      @param Params   Liste des param�tres
    *}
    procedure GetParams(Params: TStrings);
  end;

  {*
    Repr�sente un fichier unit� de type paquet Borland
    TBPLUnitFile repr�sente un fichier unit� qui charge un paquet Borland
    d�finissant les nouveaux �l�ments.
    @author sjrd
    @version 5.0
  *}
  TBPLUnitFile = class(TUnitFile)
  private
    FHandle: HMODULE;           /// Module du package charg�
    FUnitFileIntf: IUnitFile50; /// Interface vers le fichier unit�
  public
    constructor Create(AMasterFile: TMasterFile; const AHRef: string;
      const AFileName: TFileName; const AGUID: TGUID;
      Params: TStrings); override;
    destructor Destroy; override;

    procedure Loaded; override;
    procedure Unloading; override;

    procedure GameStarted; override;
    procedure GameEnded; override;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc); override;

    procedure GetParams(Params: TStrings); override;

    property Handle: HMODULE read FHandle;
    property UnitFileIntf: IUnitFile50 read FUnitFileIntf;
  end;

  {*
    Impl�mentation simple de l'interface IUnitFile50
    @author sjrd
    @version 5.0
  *}
  TInterfacedUnitFile = class(TInterfacedObject, IUnitFile50)
  private
    FMasterFile: TMasterFile;
    FMaster: TMaster;
  protected
    procedure Loaded; virtual;
    procedure Unloading; virtual;

    procedure GameStarted; virtual;
    procedure GameEnded; virtual;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc); virtual;

    procedure GetParams(Params: TStrings); virtual;

    property MasterFile: TMasterFile read FMasterFile;
    property Master: TMaster read FMaster;
  public
    constructor Create(AMasterFile: TMasterFile);
  end;

const
  /// GUID du gestionnaire d'unit�s de type package Borland
  BPLUnitHandlerGUID: TGUID = '{B28D4F92-6C46-4F22-87F9-432165EDA4C6}';

implementation

uses
  Windows, SepiImportsFunLaby;

{---------------------}
{ Classe TBPLUnitFile }
{---------------------}

const {don't localize}
  /// Proc�dure de cr�ation de fichier unit� interfac�
  CreateUnitFileProc = 'CreateUnitFile';

{*
  Cr�e une instance de TBPLUnitFile
  @param AMasterFile   Fichier ma�tre
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Param�tres envoy�s � l'unit�
*}
constructor TBPLUnitFile.Create(AMasterFile: TMasterFile; const AHRef: string;
  const AFileName: TFileName; const AGUID: TGUID; Params: TStrings);
type
  TCreateUnitFileProc = function(BPLHandler: TBPLUnitFile; Master: TMaster;
      Params: TStrings): IUnitFile50; stdcall;
var
  CreateUnitFile: TCreateUnitFileProc;
begin
  inherited;

  FHandle := LoadPackage(FileName);
  if FHandle = 0 then
    raise EFileError.CreateFmt(sCantLoadPackage, [FileName]);

  CreateUnitFile := TCreateUnitFileProc(
    GetProcAddress(FHandle, CreateUnitFileProc));

  if Assigned(CreateUnitFile) then
    FUnitFileIntf := CreateUnitFile(Self, Master, Params);
end;

{*
  [@inheritDoc]
*}
destructor TBPLUnitFile.Destroy;
begin
  FUnitFileIntf := nil;

  if FHandle <> 0 then
    UnloadPackage(FHandle);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.Loaded;
begin
  UnitFileIntf.Loaded;
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.Unloading;
begin
  UnitFileIntf.Unloading;
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.GameStarted;
begin
  UnitFileIntf.GameStarted;
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.GameEnded;
begin
  UnitFileIntf.GameEnded;
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);
begin
  UnitFileIntf.RegisterComponents(
    RegisterSingleComponentProc, RegisterComponentSetProc);
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.GetParams(Params: TStrings);
begin
  UnitFileIntf.GetParams(Params);
end;

{----------------------------}
{ Classe TInterfacedUnitFile }
{----------------------------}

{*
  Cr�e une instance de TInterfacedUnitFile
  @param AMasterFile   Fichier ma�tre
*}
constructor TInterfacedUnitFile.Create(AMasterFile: TMasterFile);
begin
  inherited Create;

  FMasterFile := AMasterFile;
  FMaster := FMasterFile.Master;
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedUnitFile.Loaded;
begin
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedUnitFile.Unloading;
begin
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedUnitFile.GameStarted;
begin
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedUnitFile.GameEnded;
begin
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedUnitFile.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);
begin
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedUnitFile.GetParams(Params: TStrings);
begin
end;

initialization
  UnitFileClasses.Add(BPLUnitHandlerGUID, TBPLUnitFile);
finalization
  UnitFileClasses.Remove(BPLUnitHandlerGUID);
end.

