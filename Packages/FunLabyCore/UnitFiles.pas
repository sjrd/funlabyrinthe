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
  SepiRuntime, FunLabyCoreConsts;

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

    procedure LoadUnitPackage;
    procedure LoadSepiUnits;
    procedure CreateUnitFileIntf;
  protected
    procedure Load; override;
  public
    destructor Destroy; override;

    procedure Loaded; override;
    procedure Unloading; override;

    procedure GameStarted; override;
    procedure GameEnded; override;

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
    FSepiUnit: TSepiUnit; /// Unité Sepi

    procedure CallSimpleProc(const Name: string);
    procedure InitializeUnit(Params: TStrings);
  protected
    procedure Load; override;
  public
    procedure Loaded; override;
    procedure Unloading; override;

    procedure GameStarted; override;
    procedure GameEnded; override;

    procedure GetParams(Params: TStrings); override;

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

    procedure GetParams(Params: TStrings); virtual;

    property MasterFile: TMasterFile read FMasterFile;
    property Master: TMaster read FMaster;
  public
    constructor Create(AMasterFile: TMasterFile);
  end;

const
  /// Extension des unités de type package Borland
  BPLUnitExtension: string = '.bpl';

  /// Extension des unités Sepi compilées
  SepiUnitExtension: string = '.scu';

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
  [@inheritDoc]
*}
destructor TBPLUnitFile.Destroy;
begin
  FUnitFileIntf := nil;

  inherited;
end;

{*
  Charge le package de l'unité
*}
procedure TBPLUnitFile.LoadUnitPackage;
var
  OldCurrentDir: string;
begin
  // TODO Avoid loading the same package again and again

  SetLength(OldCurrentDir, GetCurrentDirectory(0, nil)-1);
  GetCurrentDirectory(Length(OldCurrentDir)+1, PChar(OldCurrentDir));

  SetCurrentDirectory(PChar(fUnitsDir));
  try
    FHandle := LoadPackage(FileName);
    if FHandle = 0 then
      raise EInOutError.CreateFmt(SCantLoadPackage, [FileName]);
  finally
    SetCurrentDirectory(PChar(OldCurrentDir));
  end;
end;

{*
  Routine de call-back pour GetPackageInfo utilisée par LoadSepiUnits
  @param Name       Nom de l'élément de package
  @param NameType   Type d'élément
  @param Flags      Flags
  @param Param      Pointer(SepiRoot)
*}
procedure PackageInfoCallback(const Name: string; NameType: TNameType;
  Flags: Byte; Param: Pointer);
begin
  if (NameType = ntContainsUnit) and Assigned(SepiImportedUnit(Name)) then
    TSepiRoot(Param).LoadUnit(Name);
end;

{*
  Charge les unités Sepi fournies par le package
*}
procedure TBPLUnitFile.LoadSepiUnits;
var
  Flags: Integer;
begin
  GetPackageInfo(FHandle, MasterFile.SepiRoot, Flags, PackageInfoCallback);
end;

{*
  Crée l'interface de fichier unité
*}
procedure TBPLUnitFile.CreateUnitFileIntf;
type
  TCreateUnitFileProc = function(BPLHandler: TBPLUnitFile; Master: TMaster;
    Params: TStrings): IUnitFile50;
var
  CreateUnitFile: TCreateUnitFileProc;
begin
  CreateUnitFile := TCreateUnitFileProc(
    GetProcAddress(FHandle, CreateUnitFileProc));

  if Assigned(CreateUnitFile) then
    FUnitFileIntf := CreateUnitFile(Self, Master, CreationParams);
end;

{*
  [@inheritDoc]
*}
procedure TBPLUnitFile.Load;
begin
  LoadUnitPackage;
  LoadSepiUnits;
  CreateUnitFileIntf;

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
procedure TBPLUnitFile.GetParams(Params: TStrings);
begin
  inherited;
  UnitFileIntf.GetParams(Params);
end;

{----------------------}
{ Classe TSepiUnitFile }
{----------------------}

{*
  [@inheritDoc]
*}
procedure TSepiUnitFile.Load;
begin
  FSepiUnit := MasterFile.SepiRoot.GetComponent(
    ChangeFileExt(ExtractFileName(FileName), '')) as TSepiUnit;

  if FSepiUnit = nil then
    FSepiUnit := TSepiRuntimeUnit.Create(MasterFile.SepiRoot,
      FileName).SepiUnit;

  // Set the unit ref-count to 1
  SepiUnit.Root.LoadUnit(SepiUnit.Name);

  InitializeUnit(CreationParams);

  inherited;
end;

{*
  Appelle une procédure simple
  @param Name   Nom de la procédure à appeler
*}
procedure TSepiUnitFile.CallSimpleProc(const Name: string);
type
  TSimpleProc = procedure(Master: TMaster);
var
  Method: TSepiMethod;
  Proc: TSimpleProc;
begin
  // Get method
  if not (SepiUnit.GetComponent(Name) is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetComponent(Name));

  // Check signature
  with Method.Signature do
  begin
    if Kind <> skStaticProcedure then
      Exit;
    if ParamCount <> 1 then
      Exit;
    if (Params[0].Kind <> pkValue) or
      (not Params[0].CompatibleWith(MasterFile.SepiRoot.FindClass(
      TMaster))) then
      Exit;
    if CallingConvention <> ccRegister then
      Exit;
  end;

  // Call the procedure
  @Proc := Method.Code;
  Proc(Master);
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
  if not (SepiUnit.GetComponent('InitializeUnit') is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetComponent('InitializeUnit'));

  // Check signature
  with Method.Signature do
  begin
    FunLabyUtilsUnit := SepiUnit.Root.GetComponent('FunLabyUtils') as TSepiUnit;
    ClassesUnit := SepiUnit.Root.GetComponent('Classes') as TSepiUnit;

    if Kind <> skStaticProcedure then
      Exit;
    if ParamCount <> 2 then
      Exit;
    if not Params[0].CompatibleWith(FunLabyUtilsUnit.FindClass(TMaster)) then
      Exit;
    if not Params[1].CompatibleWith(ClassesUnit.FindClass(TStrings)) then
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
procedure TSepiUnitFile.GetParams(Params: TStrings);
type
  TGetParamsProc = procedure(Params: TStrings);
var
  Method: TSepiMethod;
  ClassesUnit: TSepiUnit;
  GetParamsProc: TGetParamsProc;
begin
  // Don't localize strings in this method

  inherited;

  // Get method
  if not (SepiUnit.GetComponent('GetParams') is TSepiMethod) then
    Exit;
  Method := TSepiMethod(SepiUnit.GetComponent('GetParams'));

  // Check signature
  with Method.Signature do
  begin
    ClassesUnit := SepiUnit.Root.GetComponent('Classes') as TSepiUnit;

    if Kind <> skStaticProcedure then
      Exit;
    if ParamCount <> 1 then
      Exit;
    if not Params[0].CompatibleWith(ClassesUnit.FindClass(TStrings)) then
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
procedure TInterfacedUnitFile.GetParams(Params: TStrings);
begin
end;

initialization
  FunLabyRegisterClasses([TBPLUnitFile, TSepiUnitFile]);

  UnitFileClasses.Add(BPLUnitExtension, TBPLUnitFile);
  UnitFileClasses.Add(SepiUnitExtension, TSepiUnitFile);
finalization
  UnitFileClasses.Remove(BPLUnitExtension);
  UnitFileClasses.Remove(SepiUnitExtension);
end.

