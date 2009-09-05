{*
  Gestion des fichiers FunLabyrinthe
  L'unité FilesUtils contient des routines et classes se chargeant du
  traitement (création, ouverture, enregistrement) des fichiers et
  l'interfaçage de leurs données avec les classes métier.
  @author sjrd
  @version 5.0
*}
unit FilesUtils;

interface

uses
  SysUtils, Classes, Contnrs, ScUtils, ScLists, ScXML, SepiReflectionCore,
  SepiMembers, SepiRuntime, FunLabyUtils, FunLabyFilers, FunLabyCoreConsts,
  GraphicEx;

type
  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmPlay);

  TMasterFile = class;

  {*
    Représente un fichier dépendant d'un fichier maître FunLabyrinthe
    TDependantFile est la classe de base pour les classes chargeant et
    enregistrant des fichiers dépendant d'un fichier maître FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TDependantFile = class(TFunLabyPersistent)
  private
    FMasterFile: TMasterFile; /// Fichier maître
    FMaster: TMaster;         /// Maître FunLabyrinthe
    FHRef: string;            /// HRef
    FFileName: TFileName;     /// Nom du fichier

    function GetHRef: string;
    procedure SetHRef(const Value: string);
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function ResolveHRef: TFileName; virtual;
  public
    constructor Create(AMasterFile: TMasterFile); virtual;

    property MasterFile: TMasterFile read FMasterFile;
    property Master: TMaster read FMaster;
    property HRef: string read FHRef;
    property FileName: TFileName read FFileName;
  end;

  /// Classe de TDependantFile
  TDependantFileClass = class of TDependantFile;

  {*
    Liste de fichiers dépendants
    @author sjrd
    @version 5.0
  *}
  TDependantFileList = class(TFunLabyCollection)
  private
    FMasterFile: TMasterFile;
  protected
    function CreateItem(ItemClass: TFunLabyPersistentClass):
      TFunLabyPersistent; override;
  public
    constructor Create(AMasterFile: TMasterFile);

    property MasterFile: TMasterFile read FMasterFile;
  end;

  {*
    Représente un fichier unité
    TUnitFile est la classe de base pour les fichiers d'unité FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TUnitFile = class(TDependantFile)
  private
    FCreationParams: TStrings; /// Paramètres de création
    FLoaded: Boolean;          /// Indique si l'unité a déjà été chargée
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    procedure EndState(State: TPersistentState); override;

    procedure Load; virtual;

    property CreationParams: TStrings read FCreationParams;
    property IsLoaded: Boolean read FLoaded;
  public
    constructor Create(AMasterFile: TMasterFile); override;
    destructor Destroy; override;

    procedure Loaded; virtual;
    procedure Unloading; virtual;

    procedure GameStarted; virtual;
    procedure GameEnded; virtual;

    procedure GetParams(Params: TStrings); virtual;
  end;

  /// Classe de TUnitFile
  TUnitFileClass = class of TUnitFile;

  {*
    Liste de fichiers unité
    @author sjrd
    @version 5.0
  *}
  TUnitFileList = class(TDependantFileList)
  private
    function AddUnitFile(const HRef: string): TUnitFile;

    function GetItems(Index: Integer): TUnitFile;
  public
    property Items[Index: Integer]: TUnitFile read GetItems; default;
  end;

  {*
    Représente un fichier source
    @author sjrd
    @version 5.0
  *}
  TSourceFile = class(TDependantFile)
  end;

  /// Classe de TSourceFile
  TSourceFileClass = class of TSourceFile;

  {*
    Liste de fichiers source
    @author sjrd
    @version 5.0
  *}
  TSourceFileList = class(TDependantFileList)
  private
    function AddSourceFile(const HRef: string): TSourceFile;

    function GetItems(Index: Integer): TSourceFile;
  public
    property Items[Index: Integer]: TSourceFile read GetItems; default;
  end;

  {*
    Paramètre d'un fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitFileParam = record
    Name: string;  /// Nom
    Value: string; /// Valeur
  end;

  /// Tableau des paramètres d'un fichier unité
  TUnitFileParams = array of TUnitFileParam;

  /// Pointeur vers TUnitFileDesc
  PUnitFileDesc = ^TUnitFileDesc;

  {*
    Descripteur de fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitFileDesc = record
    HRef: string;            /// Adresse HRef
    Params: TUnitFileParams; /// Paramètres
  end;

  /// Tableau de descripteurs de fichiers unité
  TUnitFileDescs = array of TUnitFileDesc;

  {*
    Représente un fichier maître FunLabyrinthe
    TMasterFile représente un fichier maître FunLabyrinthe (extension .flp).
    Elle est capable de charger tous les fichiers annexes au moyen des autres
    classes de l'unité.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TMasterFile = class(TFunLabyPersistent)
  private
    FSepiRoot: TSepiRoot; /// Racine Sepi

    FFileName: TFileName; /// Nom du fichier
    FMode: TFileMode;     /// Mode d'ouverture du fichier
    FVersion: string;     /// Version lors de l'enregistrement

    FTitle: string;       /// Titre du labyrinthe
    FDescription: string; /// Description
    FDifficulty: string;  /// Difficulté
    FAuthor: string;      /// Nom de l'auteur

    FAllowEdit: Boolean;   /// Indique si le fichier peut être édité
    FIsSaveguard: Boolean; /// Indique si le fichier était une sauvegarde

    FMaster: TMaster; /// Maître FunLabyrinthe

    FUnitFiles: TUnitFileList;     /// Liste des fichiers unité
    FSourceFiles: TSourceFileList; /// Liste des fichiers source

    FWritingUnitFiles: TUnitFileList; /// Liste des fichiers unité à écrire

    constructor BaseCreate(ABaseSepiRoot: TSepiRoot;
      const AFileName: TFileName = ''; AMode: TFileMode = fmEdit);

    function LoadSepiUnit(Sender: TSepiRoot;
      const UnitName: string): TSepiUnit;

    procedure InvalidFormat;

    procedure Load(const ADocument: IInterface);
    procedure TestOpeningValidity;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
    procedure StoreDefaults; override;
  public
    constructor Create(ABaseSepiRoot: TSepiRoot; const AFileName: TFileName;
      AMode: TFileMode);
    constructor CreateNew(ABaseSepiRoot: TSepiRoot;
      const UnitFileDescs: TUnitFileDescs); overload;
    constructor CreateNew(ABaseSepiRoot: TSepiRoot); overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function ResolveHRef(const HRef, DefaultDir: string): TFileName;
    function MakeHRef(const FileName: TFileName;
      const DefaultDir: string): string;

    function AddSourceFile(const HRef: string): TSourceFile;
    procedure RemoveSourceFile(SourceFile: TSourceFile);

    procedure GameStarted;
    procedure GameEnded;

    procedure GetUnitFileDescs(out UnitFileDescs: TUnitFileDescs);

    procedure Save(const UnitFileDescs: TUnitFileDescs;
      const AFileName: TFileName = ''); overload;
    procedure Save(const AFileName: TFileName = ''); overload;

    property SepiRoot: TSepiRoot read FSepiRoot;

    property FileName: TFileName read FFileName;
    property Mode: TFileMode read FMode;
    property Version: string read FVersion;

    property Master: TMaster read FMaster;

    property UnitFiles: TUnitFileList read FUnitFiles;
    property SourceFiles: TSourceFileList read FSourceFiles;

    property AllowEdit: Boolean read FAllowEdit;
    property IsSaveguard: Boolean read FIsSaveguard;
  published
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Difficulty: string read FDifficulty write FDifficulty;
    property Author: string read FAuthor write FAuthor;
  end;

  {*
    Collection de classes de fichiers unité, référencées par leur GUID
    @author sjrd
    @version 5.0
  *}
  TUnitFileClassList = class(TCustomValueBucketList)
  protected
    function BucketFor(const Key): Cardinal; override;
    function KeyEquals(const Key1, Key2): Boolean; override;
  public
    constructor Create;

    procedure Add(const Extension: string; UnitFileClass: TUnitFileClass);
    procedure Remove(const Extension: string);
    function Find(const Extension: string): TUnitFileClass;
  end;

function HRefToFileName(const HRef: string;
  const BaseDirs: array of TFileName): TFileName;
function FileNameToHRef(const FileName: TFileName;
  const BaseDirs: array of TFileName): string;

const {don't localize}
  HRefDelim = '/'; /// Délimiteur dans les href

  FunLabyBaseHRef = 'FunLabyBase.bpl'; /// HRef de l'unité FunLabyBase

var
  /// Gestionnaires d'unité : association extension <-> classe d'unité
  UnitFileClasses: TUnitFileClassList = nil;

implementation

uses
  StrUtils, ScStrUtils, IniFiles, Variants, MSXML, ActiveX;

{*
  Compare deux numéros de versions représentés textuellement
  @param Version1   Premier numéro de version
  @param Version2   Second numéro de version
  @return 0 si les versions sont égales, 1 si la première est supérieure à la
          seconde, et -1 dans le cas contraire
*}
function CompareVersions(const Version1, Version2: string): Integer;
var
  SubVer1, SubVer2, MajVer1, MajVer2: string;
begin
  SubVer1 := Version1;
  SubVer2 := Version2;

  repeat
    MajVer1 := GetFirstToken(Version1, '.');
    Delete(SubVer1, 1, Length(MajVer1)+1);

    MajVer2 := GetFirstToken(Version2, '.');
    Delete(SubVer2, 1, Length(MajVer2)+1);

    Result := StrToIntDef(MajVer1, 0) - StrToIntDef(MajVer2, 0);
  until (Result <> 0) or ((SubVer1 = '') and (SubVer2 = ''));
end;

{*
  Convertit un HRef en nom de fichier
  @param HRef       HRef à convertir
  @param BaseDirs   Liste de répertoires de base à tester avant l'absolu
  @return Nom du fichier sur lequel pointe HRef
  @throws EFileError Le fichier n'existe pas
*}
function HRefToFileName(const HRef: string;
  const BaseDirs: array of TFileName): TFileName;
var
  I: Integer;
  SubFile: TFileName;
begin
  {$IF HRefDelim <> PathDelim}
  SubFile := StringReplace(HRef, HRefDelim, PathDelim, [rfReplaceAll]);
  {$ELSE}
  SubFile := HRef;
  {$IFEND}

  for I := Low(BaseDirs) to High(BaseDirs) do
  begin
    if FileExists(BaseDirs[I]+SubFile) then
    begin
      Result := BaseDirs[I]+SubFile;
      Exit;
    end;
  end;

  if FileExists(SubFile) then
    Result := SubFile
  else
    raise EInOutError.CreateFmt(SFileNotFound, [HRef]);
end;

{*
  Convertit un nom de fichier en HRef
  @param FileName   Nom de fichier à convertir
  @param BaseDirs   Liste de répertoires de base à tester avant l'absolu
  @return HRef pointant sur le nom de fichier FileName
*}
function FileNameToHRef(const FileName: TFileName;
  const BaseDirs: array of TFileName): string;
var
  I: Integer;
begin
  Result := FileName;

  for I := Low(BaseDirs) to High(BaseDirs) do
  begin
    if AnsiStartsText(BaseDirs[I], FileName) then
    begin
      Result := Copy(FileName, Length(BaseDirs[I])+1, MaxInt);
      Break;
    end;
  end;

  {$IF HRefDelim <> PathDelim}
  Result := StringReplace(Result, PathDelim, HRefDelim, [rfReplaceAll]);
  {$IFEND}
end;

{-----------------------}
{ Classe TDependantFile }
{-----------------------}

{*
  Crée une instance de TDependantFile
  @param AMasterFile   Fichier maître
*}
constructor TDependantFile.Create(AMasterFile: TMasterFile);
begin
  inherited Create;

  FMasterFile := AMasterFile;
  FMaster := FMasterFile.Master;
end;

{*
  HRef pour l'enregistrement
  @return HRef pour l'enregistrement
*}
function TDependantFile.GetHRef: string;
begin
  Result := MasterFile.MakeHRef(FileName, fUnitsDir);
end;

{*
  Renseigne le href du fichier
  @param Value   Valeur de href
*}
procedure TDependantFile.SetHRef(const Value: string);
begin
  FHRef := Value;
  FFileName := ResolveHRef;
end;

{*
  [@inheritDoc]
*}
procedure TDependantFile.DefineProperties(Filer: TFunLabyFiler);
begin
  Filer.DefineProcProperty('HRef', TypeInfo(string),
    @TDependantFile.GetHRef, @TDependantFile.SetHRef);

  inherited;
end;

{*
  Résoud le href en nom de fichier
  @return Nom de fichier correspondant à HRef
  @throws EInOutError Aucun fichier correspondant n'a été trouvé
*}
function TDependantFile.ResolveHRef: TFileName;
begin
  if FileExists(HRef) then
    Result := HRef
  else
    Result := fUnitsDir + AnsiReplaceStr(HRef, HRefDelim, PathDelim);
end;

{--------------------------}
{ TDependantFileList class }
{--------------------------}

{*
  Crée une liste de fichiers dépendants
  @param AMasterFile   Fichier maître
*}
constructor TDependantFileList.Create(AMasterFile: TMasterFile);
begin
  inherited Create;

  FMasterFile := AMasterFile;
end;

{*
  [@inheritDoc]
*}
function TDependantFileList.CreateItem(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := TDependantFileClass(ItemClass).Create(MasterFile);
end;

{------------------}
{ Classe TUnitFile }
{------------------}

{*
  [@inheritDoc]
*}
constructor TUnitFile.Create(AMasterFile: TMasterFile);
begin
  inherited Create(AMasterFile);

  FCreationParams := TStringList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TUnitFile.Destroy;
begin
  FCreationParams.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TUnitFile.DefineProperties(Filer: TFunLabyFiler);
var
  Params: TStrings;
begin
  inherited;

  if (psWriting in PersistentState) and IsLoaded then
  begin
    Params := TStringList.Create;
    try
      GetParams(Params);
      Filer.DefineStrings('Params', Params);
    finally
      Params.Free;
    end;
  end else
  begin
    Filer.DefineStrings('Params', FCreationParams);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TUnitFile.EndState(State: TPersistentState);
begin
  inherited;

  if (psReading in State) and (not IsLoaded) and (HRef <> '') then
  begin
    if not FileExists(FileName) then
      FFileName := MasterFile.ResolveHRef(HRef, fUnitsDir);
    Load;
  end;
end;

{*
  Charge le fichier
*}
procedure TUnitFile.Load;
begin
  FLoaded := True;
end;

{*
  Exécuté lorsque le projet a été complètement chargé
  Loaded est appelée une fois que le projet a été complètement chargé. À ce
  moment, toutes les unités sont chargées, les cartes également, et tous les
  joueurs de même, à leurs positions respectives, et avec leurs attributs et/ou
  plug-in.
  Loaded est appelée aussi bien en mode édition qu'en mode jeu.
*}
procedure TUnitFile.Loaded;
begin
end;

{*
  Exécuté lorsque le projet est sur le point d'être déchargé
  Unloading est appelée lorsque le projet est sur le point d'être déchargé. À ce
  moment, tous les objets sont encore accessibles, pour la dernière fois.
  Unloading est appelée aussi bien en mode édition qu'en mode jeu.
*}
procedure TUnitFile.Unloading;
begin
end;

{*
  Exécuté lorsque la partie vient juste d'être commencée
  GameStarted est appelée lorsque la partie vient juste d'être commencée (en
  mode jeu, donc pas en mode édition).
*}
procedure TUnitFile.GameStarted;
begin
end;

{*
  Exécuté lorsque la partie vient juste de se terminer
  GameEnded est appelée lorsque la partie vient juste d'être terminée (en mode
  jeu, donc pas en mode édition), avant que le maître FunLabyrinthe ne soit
  libéré.
  Une partie est terminée lorsque plus aucun joueur n'est dans l'état psPlaying.
*}
procedure TUnitFile.GameEnded;
begin
end;

{*
  Dresse la liste des paramètres à enregistrer
  Les descendants de TUnitFile peuvent surcharger cette méthode pour indiquer
  au fichier maître les paramètres qu'il doit enregistrer.
  @param Params   Liste des paramètres
*}
procedure TUnitFile.GetParams(Params: TStrings);
begin
  Params.Assign(FCreationParams);
end;

{---------------------}
{ TUnitFileList class }
{---------------------}

{*
  Ajoute un fichier unité
  @param HRef   HRef du fichier unité (uniquement pour déterminer la classe)
  @return Fichier unité créé
*}
function TUnitFileList.AddUnitFile(const HRef: string): TUnitFile;
begin
  Result := TUnitFile(Add(UnitFileClasses.Find(ExtractFileExt(HRef))));
end;

{*
  [@inheritDoc]
*}
function TUnitFileList.GetItems(Index: Integer): TUnitFile;
begin
  Result := TUnitFile(inherited Items[Index]);
end;

{-----------------------}
{ TSourceFileList class }
{-----------------------}

{*
  Ajoute un nouveau fichier source
  @param HRef   HRef du fichier source
  @return Fichier source ajouté
*}
function TSourceFileList.AddSourceFile(const HRef: string): TSourceFile;
begin
  Result := TSourceFile.Create(MasterFile);

  try
    Result.SetHRef(HRef);
    AddItem(Result);
  except
    Result.Free;
    raise;
  end;
end;

{*
  [@inheritDoc]
*}
function TSourceFileList.GetItems(Index: Integer): TSourceFile;
begin
  Result := TSourceFile(inherited Items[Index]);
end;

{--------------------}
{ Classe TMasterFile }
{--------------------}

{*
  Constructeur de base (parties communes de Create et CreateNew)
  @param ABaseSepiRoot   Racine Sepi de base (peut être nil)
  @param AFileName       Nom du fichier à ouvrir
  @param AMode           Mode sous lequel ouvrir le fichier
*}
constructor TMasterFile.BaseCreate(ABaseSepiRoot: TSepiRoot;
  const AFileName: TFileName = ''; AMode: TFileMode = fmEdit);
begin
  inherited Create;

  if ABaseSepiRoot = nil then
    FSepiRoot := TSepiRoot.Create
  else
    FSepiRoot := TSepiRootFork.Create(ABaseSepiRoot);

  FFileName := AFileName;
  FMode := AMode;
  FVersion := CurrentVersion;

  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create(Mode = fmEdit);

  FUnitFiles := TUnitFileList.Create(Self);
  FSourceFiles := TSourceFileList.Create(Self);

  FSepiRoot.OnLoadUnit := LoadSepiUnit;
  FSepiRoot.LoadUnit('FunLabyUtils');
end;

{*
  Ouvre un fichier FunLabyrinthe
  @param ABaseSepiRoot   Racine Sepi de base (peut être nil)
  @param AFileName       Nom du fichier à ouvrir
  @param AMode           Mode sous lequel ouvrir le fichier
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TMasterFile.Create(ABaseSepiRoot: TSepiRoot;
  const AFileName: TFileName; AMode: TFileMode);
begin
  BaseCreate(ABaseSepiRoot, AFileName, AMode);

  Load(LoadXMLDocumentFromFile(FFileName));
  TestOpeningValidity;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param ABaseSepiRoot   Racine Sepi de base (peut être nil)
  @param UnitFileDescs   Descripteurs des fichiers unité à utiliser
*}
constructor TMasterFile.CreateNew(ABaseSepiRoot: TSepiRoot;
  const UnitFileDescs: TUnitFileDescs);
var
  I, J: Integer;
  UnitFile: TUnitFile;
begin
  BaseCreate(ABaseSepiRoot);

  // Ajouter les unités décrites par UnitFileDescs
  for I := 0 to Length(UnitFileDescs)-1 do
  begin
    with UnitFileDescs[I] do
    begin
      UnitFile := UnitFiles.AddUnitFile(HRef);

      // Simulation of reading with a filer
      UnitFile.BeginState([psReading]);
      try
        UnitFile.SetHRef(HRef);

        UnitFile.FCreationParams.Clear;
        for J := 0 to Length(Params)-1 do
          with Params[J] do
            UnitFile.FCreationParams.Values[Name] := Value;
      finally
        UnitFile.EndState([psReading]);
      end;
    end;
  end;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param ABaseSepiRoot   Racine Sepi de base (peut être nil)
*}
constructor TMasterFile.CreateNew(ABaseSepiRoot: TSepiRoot);
var
  UnitFileDescs: TUnitFileDescs;
begin
  SetLength(UnitFileDescs, 1);
  UnitFileDescs[0].HRef := FunLabyBaseHRef;

  CreateNew(ABaseSepiRoot, UnitFileDescs);
end;

{*
  Détruit l'instance
*}
destructor TMasterFile.Destroy;
begin
  FMaster.Free;
  FSourceFiles.Free;
  FUnitFiles.Free;

  FSepiRoot.Free;

  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TMasterFile.AfterConstruction;
var
  I: Integer;
begin
  inherited;

  for I := 0 to UnitFiles.Count-1 do
    UnitFiles[I].Loaded;
end;

{*
  Exécuté avant la destruction de l'objet
  BeforeDestruction est appelé avant l'exécution du premier destructeur.
  N'appelez pas directement BeforeDestruction.
*}
procedure TMasterFile.BeforeDestruction;
var
  I: Integer;
begin
  inherited;

  for I := UnitFiles.Count-1 downto 0 do
    UnitFiles[I].Unloading;
end;

{*
  Charge une unité Sepi d'après son nom
  @param Sender     Racine Sepi qui a déclenché l'événement
  @param UnitName   Nom de l'unité à charger
*}
function TMasterFile.LoadSepiUnit(Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  Dirs: array of TFileName;
  I: Integer;
  FileName: TFileName;
begin
  SetLength(Dirs, UnitFiles.Count+1);
  Dirs[0] := fUnitsDir;

  for I := 0 to UnitFiles.Count-1 do
    Dirs[I+1] := IncludeTrailingPathDelimiter(ExtractFilePath(
      UnitFiles[I].FileName));

  try
    FileName := HRefToFileName(UnitName+'.scu', Dirs);
    Result := TSepiRuntimeUnit.Create(Sender, FileName).SepiUnit;
  except
    on Error: EInOutError do
      Result := nil;
  end;
end;

{*
  Génère une erreur indiquant que le fichier ne respecte pas le format attendu
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
procedure TMasterFile.InvalidFormat;
begin
  raise EInOutError.Create(SInvalidFileFormat);
end;

{*
  Charge le contenu du document
  @param Document   Document XML DOM contenu du fichier
  @throws EInOutError : Un fichier à charger n'existe pas ou n'est pas valide
*}
procedure TMasterFile.Load(const ADocument: IInterface);

  function NullToEmptyStr(Value: Variant): Variant;
  begin
    if VarIsNull(Value) then
      Result := ''
    else
      Result := Value;
  end;

var
  Document: IXMLDOMDocument;
begin
  { Don't localize strings in this method }

  Document := ADocument as IXMLDOMDocument;

  with Document.documentElement as IXMLDOMElement do
  begin
    if nodeName <> 'funlabyrinthe' then
      InvalidFormat;

    // Test de version
    FVersion := getAttribute('version');
    if FVersion = '' then
      InvalidFormat;
    if CompareVersions(FVersion, CurrentVersion) > 0 then
      raise EInOutError.CreateFmt(SVersionTooHigh, [FVersion]);

    // Attributs du fichier
    FAllowEdit := NullToEmptyStr(getAttribute('allowedit')) <> 'no';
    FIsSaveguard := NullToEmptyStr(getAttribute('issaveguard')) = 'yes';

    // Chargement général
    TFunLabyXMLReader.ReadPersistent(Self,
      Document.documentElement as IXMLDOMElement);
  end;
end;

{*
  Teste la validité de l'ouverture d'un fichier
  TestOpeningValidity vérifie que le fichier ouvert ne l'a pas été
  « illégalement ». Deux cas d'illégalité sont à tester :
  - Le fichier est une sauvegarde et est ouvert autrement que pour y jouer ;
  - Le fichier a été interdit d'édition, et ouvert dans ce mode.
  @throws EInOutError : Le fichier a été ouvert illégalement
*}
procedure TMasterFile.TestOpeningValidity;
begin
  if IsSaveguard and (Mode <> fmPlay) then
    raise EInOutError.Create(SCantEditSaveguard);
  if (not AllowEdit) and (Mode = fmEdit) then
    raise EInOutError.Create(SEditingNotAllowed);
end;

{*
  [@inheritDoc]
*}
procedure TMasterFile.DefineProperties(Filer: TFunLabyFiler);
var
  Maps, Players, Additionnal: TStrings;
  I: Integer;
  Component: TFunLabyComponent;
  ID: TComponentID;
  CompClassName: string;
  CompClass: TSepiClass;
begin
  inherited;

  if (psWriting in PersistentState) and (FWritingUnitFiles <> nil) then
    Filer.DefinePersistent('UnitFiles', FWritingUnitFiles)
  else
    Filer.DefinePersistent('UnitFiles', UnitFiles);

  if Mode <> fmPlay then
    Filer.DefinePersistent('SourceFiles', SourceFiles);

  // Legacy read map and player list
  if psReading in PersistentState then
  begin
    Maps := nil;
    Players := nil;
    try
      Maps := TStringList.Create;
      Players := TStringList.Create;

      Filer.DefineStrings('Maps', Maps);
      Filer.DefineStrings('Players', Players);

      for I := 0 to Maps.Count-1 do
        Master.CreateAdditionnalComponent(TMap, Maps[I]);

      for I := 0 to Players.Count-1 do
        Master.CreateAdditionnalComponent(TPlayer,
          GetFirstToken(Players[I], '='));
    finally
      Maps.Free;
      Players.Free;
    end;
  end;

  // Create additionnal components
  Additionnal := TStringList.Create;
  try
    Additionnal.NameValueSeparator := ':';

    if psWriting in PersistentState then
    begin
      for I := 0 to Master.ComponentCount-1 do
      begin
        Component := Master.Components[I];
        if Component.IsAdditionnal then
          Additionnal.Values[Component.ID] := Component.ClassName;
      end;
    end;

    Filer.DefineStrings('AdditionnalComponents', Additionnal,
      nil, Additionnal.Count > 0);

    if psReading in PersistentState then
    begin
      for I := 0 to Additionnal.Count-1 do
      begin
        ID := Additionnal.Names[I];
        CompClassName := Additionnal.ValueFromIndex[I];
        CompClass := SepiRoot.FindType(CompClassName) as TSepiClass;
        Assert(CompClass.DelphiClass.InheritsFrom(TFunLabyComponent));
        Master.CreateAdditionnalComponent(
          TFunLabyComponentClass(CompClass.DelphiClass), ID);
      end;
    end;
  finally
    Additionnal.Free;
  end;

  if psReading in PersistentState then
    Master.StoreDefaults;

  Filer.DefinePersistent('Master', Master);
end;

{*
  [@inheritDoc]
*}
procedure TMasterFile.StoreDefaults;
begin
end;

{*
  Résoud l'adresse HRef en cherchant dans les dossiers correspondants
  @param HRef         Adresse HRef du fichier
  @param DefaultDir   Dossier par défaut du type de fichier attendu
  @return Nom du fichier qualifié de son chemin d'accès
  @throws EInOutError Le fichier n'existe pas
*}
function TMasterFile.ResolveHRef(const HRef, DefaultDir: string): TFileName;
begin
  Result := HRefToFileName(HRef, [DefaultDir]);
end;

{*
  Construit une adresse HRef pour un fichier
  @param FileName     Nom du fichier
  @param DefaultDir   Dossier par défaut du type du fichier
  @return Adresse HRef du fichier, relativement au contexte du fichier maître
  @throws EInOutError Le fichier n'existe pas
*}
function TMasterFile.MakeHRef(const FileName: TFileName;
  const DefaultDir: string): string;
begin
  Result := FileNameToHRef(FileName, [DefaultDir]);
end;

{*
  Ajoute un fichier source
  @param HRef   Adresse du fichier
  @return Le fichier source créé
*}
function TMasterFile.AddSourceFile(const HRef: string): TSourceFile;
begin
  if Mode = fmPlay then
    raise EInOutError.Create(SSourcesNotHandledWhilePlaying);

  Result := SourceFiles.AddSourceFile(HRef);
end;

{*
  Retire un fichier source
  @param SourceFile   Fichier source à retirer
*}
procedure TMasterFile.RemoveSourceFile(SourceFile: TSourceFile);
begin
  FSourceFiles.Remove(SourceFile);
end;

{*
  Commence la partie
*}
procedure TMasterFile.GameStarted;
var
  I: Integer;
begin
  for I := 0 to UnitFiles.Count-1 do
    UnitFiles[I].GameStarted;

  with Master do
  begin
    for I := 0 to MobileComponentCount-1 do
      Timers.ScheduleNotificationMsg(0, MobileComponents[I], msgGameStarted);
  end;
end;

{*
  Termine la partie
*}
procedure TMasterFile.GameEnded;
var
  I: Integer;
begin
  for I := 0 to UnitFiles.Count-1 do
    UnitFiles[I].GameEnded;
end;

{*
  Fournit un tableau des descripteurs des fichiers unité
  @param UnitFileDescs   Descripteurs des fichiers unité en sortie
*}
procedure TMasterFile.GetUnitFileDescs(out UnitFileDescs: TUnitFileDescs);
var
  FileIdx, ParamIdx: Integer;
  UnitFile: TUnitFile;
  ParamList: TStrings;
begin
  ParamList := TStringList.Create;
  try
    SetLength(UnitFileDescs, UnitFiles.Count);
    for FileIdx := 0 to UnitFiles.Count-1 do
    begin
      with UnitFileDescs[FileIdx] do
      begin
        UnitFile := UnitFiles[FileIdx];
        HRef := UnitFile.HRef;

        ParamList.Clear;
        UnitFile.GetParams(ParamList);

        SetLength(Params, ParamList.Count);
        for ParamIdx := 0 to ParamList.Count-1 do
        begin
          with Params[ParamIdx] do
          begin
            Name := ParamList.Names[ParamIdx];
            Value := ParamList.ValueFromIndex[ParamIdx];
          end;
        end;
      end;
    end;
  finally
    ParamList.Free;
  end;
end;

{*
  Enregistre le fichier en modifiant les fichiers unité utilisés
  Save ne vérifie aucunement des dépendances éventuelles entre unités ou des
  cartes envers les unités.
  @param UnitFileDescs   Descripteurs de fichiers unité, pour les modifier
  @param AFileName       Nom du fichier à enregistrer (vide conserve l'existant)
*}
procedure TMasterFile.Save(const UnitFileDescs: TUnitFileDescs;
  const AFileName: TFileName = '');
var
  I, J: Integer;
  UnitFile: TUnitFile;
begin
  FWritingUnitFiles := TUnitFileList.Create(Self);
  try
    // Build writing unit file list - they will never be loaded
    for I := 0 to Length(UnitFileDescs)-1 do
    begin
      with UnitFileDescs[I] do
      begin
        UnitFile := FWritingUnitFiles.AddUnitFile(HRef);
        UnitFile.SetHRef(HRef);

        for J := 0 to Length(Params)-1 do
          with Params[J] do
            UnitFile.CreationParams.Values[Name] := Value;
      end;
    end;

    // Save with this overridden unit file list
    Save(AFileName);
  finally
    FreeAndNil(FWritingUnitFiles);
  end;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier à enregistrer (si vide, conserve l'existant)
*}
procedure TMasterFile.Save(const AFileName: TFileName = '');
var
  Document: IXMLDOMDocument;
  FunLabyrinthe: IXMLDOMElement;
  FileName: TFileName;
begin
  { Don't localize strings in this method }

  if (AFileName = '') and (Mode = fmPlay) and (not IsSaveguard) then
    raise EInOutError.Create(SNoFileName);
  if AFileName = '' then
    FileName := FFileName
  else
    FileName := AFileName;

  Document := CoDOMDocument.Create;
  Document.async := False;

  with Document do
  begin
    FunLabyrinthe := Document.createElement('funlabyrinthe');

    FunLabyrinthe.setAttribute('version', CurrentVersion);
    if not AllowEdit then
      FunLabyrinthe.setAttribute('allowedit', 'no');
    if Mode = fmPlay then
      FunLabyrinthe.setAttribute('issaveguard', 'yes');

    TFunLabyXMLWriter.WritePersistent(Self, FunLabyrinthe);

    appendChild(FunLabyrinthe);
  end;

  SaveXMLDocumentToFile(Document, FileName);

  FFileName := FileName;
  if Mode = fmPlay then
    FIsSaveguard := True;
end;

{---------------------------}
{ Classe TUnitFileClassList }
{---------------------------}

{*
  Crée une nouvelle instance de TUnitFileClassList
*}
constructor TUnitFileClassList.Create;
begin
  inherited Create(TypeInfo(string), SizeOf(TUnitFileClass));
end;

{*
  [@inheritDoc]
*}
function TUnitFileClassList.BucketFor(const Key): Cardinal;
begin
  Result := HashOfStr(string(Key)) mod Cardinal(BucketCount);
end;

{*
  [@inheritDoc]
*}
function TUnitFileClassList.KeyEquals(const Key1, Key2): Boolean;
begin
  Result := string(Key1) = string(Key2);
end;

{*
  Référence un gestionnaire d'unité
  @param Extension       Extension de fichier unité à recenser
  @param UnitFileClass   Classe du gestionnaire
*}
procedure TUnitFileClassList.Add(const Extension: string;
  UnitFileClass: TUnitFileClass);
begin
  AddData(Extension, UnitFileClass);
end;

{*
  Supprime un gestionnaire d'unité
  @param Extension   Extension de fichier unité à supprimer
*}
procedure TUnitFileClassList.Remove(const Extension: string);
begin
  RemoveData(Extension);
end;

{*
  Trouve la classe d'un gestionnaire à partir de son extension
  @param Extension   Extension du fichier unité
  @return Classe du gestionnaire gérant les fichiers du type spécifié
  @throws EInOutError Type de fichier inconnu
*}
function TUnitFileClassList.Find(const Extension: string): TUnitFileClass;
begin
  if not (inherited Find(Extension, Result)) then
    raise EInOutError.CreateFmt(SUnknownUnitType, [Extension]);
end;

initialization
  FunLabyRegisterClass(TSourceFile);

  UnitFileClasses := TUnitFileClassList.Create;
finalization
  UnitFileClasses.Free;
  UnitFileClasses := nil;
end.

