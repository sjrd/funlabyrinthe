{*
  Gestion des fichiers unité de base de FunLabyrinthe
  L'unité UnitFiles décrit les classes de gestion des fichiers unité de base de
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit UnitFiles;

interface

uses
  SysUtils, Classes, FunLabyUtils, FilesUtils, SepiReflectionCore, SepiMembers,
  SepiRuntime;

resourcestring
  sCantLoadPackage = 'Impossible de charger le paquet "%s"';

type
  {*
    Interface d'un fichier unité
    @author sjrd
    @version 5.0
  *}
  IUnitFile50 = interface
    ['{F9BDF4FD-9FC4-4B6B-BB12-6F587E4AB3C5}']

    {*
      Exécuté lorsque le projet a été complètement chargé
      Loaded est appelée une fois que le projet a été complètement chargé. À ce
      moment, toutes les unités sont chargées, les cartes également, et tous les
      joueurs de même, à leurs positions respectives, et avec leurs attributs
      et/ou plug-in.
      Loaded est appelée aussi bien en mode édition qu'en mode jeu.
    *}
    procedure Loaded;

    {*
      Exécuté lorsque le projet est sur le point d'être déchargé
      Unloading est appelée lorsque le projet est sur le point d'être déchargé.
      À ce moment, tous les objets sont encore accessibles, pour la dernière
      fois.
      Unloading est appelée aussi bien en mode édition qu'en mode jeu.
    *}
    procedure Unloading;

    {*
      Exécuté lorsque la partie vient juste d'être commencée
      GameStarted est appelée lorsque la partie vient juste d'être commencée (en
      mode jeu, donc pas en mode édition).
    *}
    procedure GameStarted;

    {*
      Exécuté lorsque la partie vient juste de se terminer
      GameEnded est appelée lorsque la partie vient juste d'être terminée (en
      mode jeu, donc pas en mode édition), avant que le maître FunLabyrinthe ne
      soit libéré.
      Une partie est terminée lorsque plus aucun joueur n'est dans l'état
      psPlaying.
    *}
    procedure GameEnded;

    {*
      Enregistre les différents composants à placer dans la palette d'édition
      @param RegisterSingleComponentProc   Call-back pour un unique composant
      @param RegisterComponentSetProc      Call-back pour un set de composants
    *}
    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc);

    {*
      Dresse la liste des paramètres à enregistrer
      Les descendants de TUnitFile peuvent surcharger cette méthode pour
      indiquer au fichier maître les paramètres qu'il doit enregistrer.
      @param Params   Liste des paramètres
    *}
    procedure GetParams(Params: TStrings);
  end;

  {*
    Représente un fichier unité de type paquet Borland
    TBPLUnitFile représente un fichier unité qui charge un paquet Borland
    définissant les nouveaux éléments.
    @author sjrd
    @version 5.0
  *}
  TBPLUnitFile = class(TUnitFile)
  private
    FHandle: HMODULE;           /// Module du package chargé
    FUnitFileIntf: IUnitFile50; /// Interface vers le fichier unité
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
    Représente un fichier unité Sepi compilée
    TSepiUnitFile représente un fichier unité Sepi compilée.
    @author sjrd
    @version 5.0
  *}
  TSepiUnitFile = class(TUnitFile)
  private
    FRuntimeUnit: TSepiRuntimeUnit; /// Unité Sepi runtime
    FSepiUnit: TSepiUnit;           /// Unité Sepi

    procedure CallSimpleProc(const Name: string);
    procedure InitializeUnit(Params: TStrings);
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

    property RuntimeUnit: TSepiRuntimeUnit read FRuntimeUnit;
    property SepiUnit: TSepiUnit read FSepiUnit;
  end;

  {*
    Implémentation simple de l'interface IUnitFile50
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
  /// GUID du gestionnaire d'unités de type package Borland
  BPLUnitHandlerGUID: TGUID = '{B28D4F92-6C46-4F22-87F9-432165EDA4C6}';

  /// GUID du gestionnaire d'unités Sepi compilées
  SepiUnitHandlerGUID: TGUID = '{AA09143C-74BF-48E5-984A-010EE900EE06}';

implementation

uses
  Windows, SepiImportsFunLaby;

{---------------------}
{ Classe TBPLUnitFile }
{---------------------}

const {don't localize}
  /// Procédure de création de fichier unité interfacé
  CreateUnitFileProc = 'CreateUnitFile';

{*
  Crée une instance de TBPLUnitFile
  @param AMasterFile   Fichier maître
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Paramètres envoyés à l'unité
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

{----------------------}
{ Classe TSepiUnitFile }
{----------------------}

{*
  Crée une instance de TSepiUnitFile
  @param AMasterFile   Fichier maître
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Paramètres envoyés à l'unité
*}
constructor TSepiUnitFile.Create(AMasterFile: TMasterFile; const AHRef: string;
  const AFileName: TFileName; const AGUID: TGUID; Params: TStrings);
begin
  inherited;

  FRuntimeUnit := TSepiRuntimeUnit.Create(MasterFile.SepiRoot, FileName);
  FSepiUnit := FRuntimeUnit.SepiUnit;

  // Set the unit ref-count to 1
  SepiUnit.Root.LoadUnit(SepiUnit.Name);

  InitializeUnit(Params);
end;

{*
  [@inheritDoc]
*}
destructor TSepiUnitFile.Destroy;
begin
  CallSimpleProc('FinalizeUnit');
  SepiUnit.Root.UnloadUnit(SepiUnit.Name);

  inherited;
end;

procedure TSepiUnitFile.CallSimpleProc(const Name: string);
var
  Method: TSepiMethod;
  Proc: TProcedure;
begin
  // Get method
  if not (SepiUnit.GetMeta(Name) is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetMeta(Name));

  // Check signature
  with Method.Signature do
  begin
    if Kind <> mkUnitProcedure then
      Exit;
    if ParamCount <> 0 then
      Exit;
    if CallingConvention <> ccRegister then
      Exit;
  end;

  // Call the procedure
  @Proc := Method.Code;
  Proc;
end;

{*
  Initialise l'unité
*}
procedure TSepiUnitFile.InitializeUnit(Params: TStrings);
type
  TInitializeUnitProc = procedure(Master: TMaster; Params: TStrings);
var
  Method: TSepiMethod;
  FunLabyUtilsUnit, ClassesUnit: TSepiUnit;
  InitializeUnitProc: TInitializeUnitProc;
begin
  // Don't localize strings in this method

  // Get method
  if not (SepiUnit.GetMeta('InitializeUnit') is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetMeta('InitializeUnit'));

  // Check signature
  with Method.Signature do
  begin
    FunLabyUtilsUnit := SepiUnit.Root.GetMeta('FunLabyUtils') as TSepiUnit;
    ClassesUnit := SepiUnit.Root.GetMeta('Classes') as TSepiUnit;

    if Kind <> mkUnitProcedure then
      Exit;
    if ParamCount <> 2 then
      Exit;
    if not Params[0].CompatibleWith(FunLabyUtilsUnit.GetMeta(
      'TMaster') as TSepiType) then
      Exit;
    if not Params[1].CompatibleWith(ClassesUnit.GetMeta(
      'TStrings') as TSepiType) then
      Exit;
    if CallingConvention <> ccRegister then
      Exit;
  end;

  // Call the procedure
  @InitializeUnitProc := Method.Code;
  InitializeUnitProc(Master, Params);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.Loaded;
begin
  CallSimpleProc('Loaded'); // don't localize
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.Unloading;
begin
  CallSimpleProc('Unloading'); // don't localize
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.GameStarted;
begin
  CallSimpleProc('GameStarted'); // don't localize
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.GameEnded;
begin
  CallSimpleProc('GameEnded'); // don't localize
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);
type
  TRegisterComponentsProc = procedure(
    RegisterSingleComponentProc: TRegisterSingleComponentProc;
    RegisterComponentSetProc: TRegisterComponentSetProc);
var
  Method: TSepiMethod;
  FunLabyUtilsUnit: TSepiUnit;
  RegisterComponentsProc: TRegisterComponentsProc;
begin
  // Don't localize strings in this method

  // Get method
  if not (SepiUnit.GetMeta('RegisterComponents') is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetMeta('RegisterComponents'));

  // Check signature
  with Method.Signature do
  begin
    FunLabyUtilsUnit := SepiUnit.Root.GetMeta('FunLabyUtils') as TSepiUnit;

    if Kind <> mkUnitProcedure then
      Exit;
    if ParamCount <> 2 then
      Exit;
    if not Params[0].CompatibleWith(FunLabyUtilsUnit.GetMeta(
      'TRegisterSingleComponentProc') as TSepiType) then
      Exit;
    if not Params[1].CompatibleWith(FunLabyUtilsUnit.GetMeta(
      'TRegisterComponentSetProc') as TSepiType) then
      Exit;
    if CallingConvention <> ccRegister then
      Exit;
  end;

  // Call the procedure
  @RegisterComponentsProc := Method.Code;
  RegisterComponentsProc(RegisterSingleComponentProc,
    RegisterComponentSetProc);
end;

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.GetParams(Params: TStrings);
type
  TGetParamsProc = procedure(Params: TStrings);
var
  Method: TSepiMethod;
  ClassesUnit: TSepiUnit;
  GetParamsProc: TGetParamsProc;
begin
  // Don't localize strings in this method

  // Get method
  if not (SepiUnit.GetMeta('GetParams') is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetMeta('GetParams'));

  // Check signature
  with Method.Signature do
  begin
    ClassesUnit := SepiUnit.Root.GetMeta('Classes') as TSepiUnit;

    if Kind <> mkUnitProcedure then
      Exit;
    if ParamCount <> 1 then
      Exit;
    if not Params[0].CompatibleWith(ClassesUnit.GetMeta(
      'TStrings') as TSepiType) then
      Exit;
    if CallingConvention <> ccRegister then
      Exit;
  end;

  // Call the procedure
  @GetParamsProc := Method.Code;
  GetParamsProc(Params);
end;

{----------------------------}
{ Classe TInterfacedUnitFile }
{----------------------------}

{*
  Crée une instance de TInterfacedUnitFile
  @param AMasterFile   Fichier maître
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
  UnitFileClasses.Add(SepiUnitHandlerGUID, TSepiUnitFile);
finalization
  UnitFileClasses.Remove(BPLUnitHandlerGUID);
  UnitFileClasses.Remove(SepiUnitHandlerGUID);
end.

