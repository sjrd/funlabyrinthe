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
  /// Type de callback pour la recherche de fichiers
  TSearchFileCallback = reference to procedure(const FileName: TFileName;
    const SearchRec: TSearchRec);

  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmPlay);

  {*
    Représente un fichier maître FunLabyrinthe
    TMasterFile représente un fichier maître FunLabyrinthe (extension .flp).
    Elle est capable de charger tous les fichiers annexes au moyen des autres
    classes de l'unité.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TMasterFile = class(TFunLabyPersistent, IMasterMetaData)
  private
    FSepiRoot: TSepiRoot; /// Racine Sepi

    FFileName: TFileName;   /// Nom du fichier
    FProjectDir: TFileName; /// Dossier du projet
    FMode: TFileMode;       /// Mode d'ouverture du fichier
    FVersion: string;       /// Version lors de l'enregistrement

    FTitle: string;       /// Titre du labyrinthe
    FDescription: string; /// Description
    FKind: string;        /// Genre
    FDifficulty: string;  /// Difficulté
    FAuthor: string;      /// Nom de l'auteur

    FIsSaveguard: Boolean; /// Indique si le fichier était une sauvegarde

    FMaster: TMaster; /// Maître FunLabyrinthe

    FUsedUnits: TStrings; /// Liste des unités utilisées par ce projet

    FSourceFiles: TStrings; /// Liste des fichiers source

    constructor BaseCreate(ABaseSepiRoot: TSepiRoot;
      const AFileName: TFileName = ''; AMode: TFileMode = fmEdit);

    function LoadSepiUnit(Sender: TSepiRoot;
      const UnitName: string): TSepiUnit;

    procedure InvalidFormat;

    procedure Load(const ADocument: IInterface);
    procedure CompatibilityLoadUnits(const ADocument: IInterface);
    procedure CompatibilityLoadSources(const ADocument: IInterface);
    procedure TestOpeningValidity;
    procedure LoadUsedUnits;

    procedure CallUnitEventProcs(const Name: string);
    procedure CallUnitEventProc(SepiUnit: TSepiUnit; const Name: string);
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
    procedure StoreDefaults; override;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function IsEditing: Boolean;
  public
    constructor Create(ABaseSepiRoot: TSepiRoot; const AFileName: TFileName;
      AMode: TFileMode);
    constructor CreateNew(ABaseSepiRoot: TSepiRoot;
      AUsedUnits: TStrings); overload;
    constructor CreateNew(ABaseSepiRoot: TSepiRoot); overload;
    constructor CreateAutoCompiler(ABaseSepiRoot: TSepiRoot;
      const AProjectDir: TFileName);
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function ResolveHRef(const HRef, Dir: string): TFileName; overload;
    function ResolveHRef(const HRef, Dir: string;
      const Extensions: array of string): TFileName; overload;
    function MakeHRef(const FileName: TFileName;
      const Dir: string): string;

    function FindResource(const HRef: string; Kind: TResourceKind;
      const Extensions: array of string): TFileName;
    function MakeResourceHRef(const FileName: TFileName;
      Kind: TResourceKind): string;

    procedure GameStarted;
    procedure GameEnded;

    procedure Save(UsedUnits: TStrings;
      const AFileName: TFileName = ''); overload;
    procedure Save(const AFileName: TFileName = ''); overload;

    property SepiRoot: TSepiRoot read FSepiRoot;

    property FileName: TFileName read FFileName;
    property ProjectDir: TFileName read FProjectDir;
    property Mode: TFileMode read FMode;
    property Version: string read FVersion;

    property Master: TMaster read FMaster;

    property UsedUnits: TStrings read FUsedUnits;

    property SourceFiles: TStrings read FSourceFiles;

    property IsSaveguard: Boolean read FIsSaveguard;
  published
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Kind: string read FKind write FKind;
    property Difficulty: string read FDifficulty write FDifficulty;
    property Author: string read FAuthor write FAuthor;
  end;

function HasExtension(const FileName: TFileName;
  const Extension: string): Boolean;

function JoinPath(const Parts: array of TFileName): TFileName;
function JoinHRef(const Parts: array of string): string;

function HRefToFileName(const HRef: string; const BaseDirs: array of TFileName;
  const Extensions: array of string): TFileName;
function FileNameToHRef(const FileName: TFileName;
  const BaseDirs: array of TFileName): string;

procedure IterateDir(const Dir: TFileName; const Pattern: string;
  const Callback: TSearchFileCallback; ExcludeDotAndDoubleDot: Boolean = True);

procedure IterateProjects(const Callback: TSearchFileCallback);

procedure RunAutoVersionCheck;
procedure EditVersionCheckOptions;

const {don't localize}
  /// Nom du dossier de la bibliothèque
  LibraryDir = 'Library';
  /// Nom du dossier des projets
  ProjectsDir = 'Projects';
  /// Nom du dossier des sauvegardes
  SaveguardsDir = 'Saveguards';

  /// Nom du dossier des ressources
  ResourcesDir = 'Resources';
  /// Nom du dossier des sources
  SourcesDir = 'Sources';
  /// Nom du dossier des unités compilées
  UnitsDir = 'Units';
  /// Nom du dossier des screenshots
  ScreenshotsDir = 'Screenshots';

  /// Nom du dossier des paquets définissant des unités
  UnitPackagesDir = 'UnitPackages';
  /// Nom du dossier des plugins de l'éditeur
  EditPluginsDir = 'EditPlugins';

  /// Application des types de ressources vers leurs noms de dossiers respectifs
  ResourceKindToDir: array[TResourceKind] of string = (
    'Images', 'Sounds'
  );

var {don't localize}
  /// Chemin complet du dossier contenant tous les documents FunLabyrinthe
  FunLabyAppDataPath: string;

  /// Chemin complet de la bibliothèque
  LibraryPath: string;
  /// Chemin complet du dossier des projets
  ProjectsPath: string;
  /// Chemin complet du dossier des sauvegardes
  SaveguardsPath: string;

const {don't localize}
  HRefDelim = '/'; /// Délimiteur dans les href

  FunLabyProjectExt = 'flp'; /// Extension d'un fichier projet

  FunLabyBaseUnitName = 'FunLabyBase'; /// Nom de l'unité FunLabyBase

implementation

uses
  StrUtils, ScStrUtils, IniFiles, Variants, TypInfo, MSXML, ActiveX,
  PluginManager;

{*
  Teste si un nom de fichier donné a une extension donnée
  @param FileName   Nom de fichier
  @param Extension   Extension
  @return True si FileName a Extension comme extension
*}
function HasExtension(const FileName: TFileName;
  const Extension: string): Boolean;
begin
  Result := AnsiSameText(ExtractFileExt(FileName), '.'+Extension);
end;

{*
  Joint plusieurs parties d'un nom de fichier
  @param Parts   Parties à joindre
  @return Parts[0] + PathDelim + Parts[1] + PathDelim + ... + Parts[N-1]
*}
function JoinPath(const Parts: array of TFileName): TFileName;
var
  I: Integer;
begin
  Result := Parts[0];
  for I := 1 to High(Parts) do
  begin
    if (Result <> '') and (Result[Length(Result)] <> PathDelim) then
      Result := Result + PathDelim + Parts[I]
    else
      Result := Result + Parts[I];
  end;
end;

{*
  Joint plusieurs parties d'un href
  @param Parts   Parties à joindre
  @return Parts[0] + HRefDelim + Parts[1] + HRefDelim + ... + Parts[N-1]
*}
function JoinHRef(const Parts: array of string): string;
var
  I: Integer;
begin
  Result := Parts[0];
  for I := 1 to High(Parts) do
    Result := Result + HRefDelim + Parts[I];
end;

{*
  Convertit un HRef en nom de fichier
  @param HRef         HRef à convertir
  @param BaseDirs     Liste de répertoires de base à tester avant l'absolu
  @param Extensions   Extensions possibles
  @return Nom du fichier sur lequel pointe HRef
  @throws EFileError Le fichier n'existe pas
*}
function HRefToFileName(const HRef: string; const BaseDirs: array of TFileName;
  const Extensions: array of string): TFileName;
var
  I, J: Integer;
  SubFile: TFileName;
begin
  {$IF HRefDelim <> PathDelim}
  SubFile := StringReplace(HRef, HRefDelim, PathDelim, [rfReplaceAll]);
  {$ELSE}
  SubFile := HRef;
  {$IFEND}

  for I := Low(BaseDirs) to High(BaseDirs) do
  begin
    for J := Low(Extensions) to High(Extensions) do
    begin
      Result := JoinPath([BaseDirs[I], SubFile + Extensions[J]]);
      if FileExists(Result) then
        Exit;
    end;
  end;

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
    if AnsiStartsText(BaseDirs[I]+PathDelim, FileName) then
    begin
      Result := Copy(FileName, Length(BaseDirs[I])+(1+1), MaxInt);
      {$IF HRefDelim <> PathDelim}
      Result := StringReplace(Result, PathDelim, HRefDelim, [rfReplaceAll]);
      {$IFEND}
      Exit;
    end;
  end;

  raise EInOutError.CreateFmt(SCannotMakeHRef, [FileName]);
end;

{*
  Itère sur tous les fichiers contenu dans un dossier
  @param Dir        Dossier
  @param Pattern    Pattern du nom de fichier
  @param Callback   Callback à appeler pour chaque fichier
*}
procedure IterateDir(const Dir: TFileName; const Pattern: string;
  const Callback: TSearchFileCallback; ExcludeDotAndDoubleDot: Boolean = True);
var
  SearchRec: TSearchRec;
begin
  if FindFirst(JoinPath([Dir, Pattern]), faAnyFile, SearchRec) = 0 then
  try
    repeat
      if ExcludeDotAndDoubleDot and
        ((SearchRec.Name = '.') or (SearchRec.Name = '..')) then
        Continue;

      Callback(JoinPath([Dir, SearchRec.Name]), SearchRec);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

{*
  Itère sur tous les projets
  @param Callback   Callback à appeler pour chaque fichier
*}
procedure IterateProjects(const Callback: TSearchFileCallback);
begin
  IterateDir(ProjectsPath, '*',
    procedure(const ProjectDir: TFileName; const SearchRec: TSearchRec)
    begin
      if SearchRec.Attr and faDirectory = 0 then
        Exit;

      IterateDir(ProjectDir, '*.flp', Callback);
    end);
end;

{*
  Déclenche la vérification automatique d'une nouvelle version de FunLabyrinthe
*}
procedure RunAutoVersionCheck;
begin
  RunURL(Dir + 'FunLabyVersionCheck.exe', DefaultRunURLVerb, '-autocheck');
end;

{*
  Déclenche l'édition des options pour la vérification de nouvelles versions
*}
procedure EditVersionCheckOptions;
begin
  RunURL(Dir + 'FunLabyVersionCheck.exe');
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
  FProjectDir := ExtractFilePath(FFileName);
  FMode := AMode;
  FVersion := CurrentVersion;

  FIsSaveguard := False;

  FMaster := TMaster.Create(Self);

  FUsedUnits := TStringList.Create;

  FSourceFiles := TStringList.Create;

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
  @param AUsedUnits      Unités utilisées
*}
constructor TMasterFile.CreateNew(ABaseSepiRoot: TSepiRoot;
  AUsedUnits: TStrings);
begin
  BaseCreate(ABaseSepiRoot);

  FUsedUnits.Assign(AUsedUnits);
  LoadUsedUnits;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param ABaseSepiRoot   Racine Sepi de base (peut être nil)
*}
constructor TMasterFile.CreateNew(ABaseSepiRoot: TSepiRoot);
begin
  BaseCreate(ABaseSepiRoot);

  FUsedUnits.Add(FunLabyBaseUnitName);
  LoadUsedUnits;
end;

{*
  Crée un nouveau fichier maître d'auto-compilation
  @param ABaseSepiRoot   Racine Sepi de base (peut être nil)
  @param AProjectDir     Dossier fictif du projet
*}
constructor TMasterFile.CreateAutoCompiler(ABaseSepiRoot: TSepiRoot;
  const AProjectDir: TFileName);
begin
  BaseCreate(ABaseSepiRoot);

  FProjectDir := IncludeTrailingPathDelimiter(AProjectDir);
end;

{*
  Détruit l'instance
*}
destructor TMasterFile.Destroy;
begin
  FMaster.Free;
  FSourceFiles.Free;

  FUsedUnits.Free;

  FSepiRoot.Free;

  inherited;
end;

{*
  Charge une unité Sepi d'après son nom
  @param Sender     Racine Sepi qui a déclenché l'événement
  @param UnitName   Nom de l'unité à charger
*}
function TMasterFile.LoadSepiUnit(Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  FileName: TFileName;
begin
  try
    FileName := ResolveHRef(UnitName+'.scu', UnitsDir);
    Result := TSepiRuntimeUnit.Create(Sender, FileName).SepiUnit;
  except
    on EInOutError do
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

  with Document.documentElement do
  begin
    if nodeName <> 'funlabyrinthe' then
      InvalidFormat;

    // Test de version
    FVersion := getAttribute('version');
    if FVersion = '' then
      InvalidFormat;
    if CompareVersion(FVersion, CurrentVersion) > 0 then
      raise EInOutError.CreateFmt(SVersionTooHigh, [FVersion]);

    // Attributs du fichier
    FIsSaveguard := NullToEmptyStr(getAttribute('issaveguard')) = 'yes';

    // Compatibility with FunLabyrinthe < 5.2
    CompatibilityLoadUnits(Document);

    // Chargement général
    TFunLabyXMLReader.ReadPersistent(Self,
      Document.documentElement);

    // Compatibility with FunLabyrinthe < 5.2
    CompatibilityLoadSources(Document);
  end;
end;

{*
  Charge la liste des unités (compatibilité < 5.2)
  @param Document   Document XML DOM contenu du fichier
*}
procedure TMasterFile.CompatibilityLoadUnits(const ADocument: IInterface);
const {don't localize}
  Query = '/funlabyrinthe/collection[@name="UnitFiles"]/items/item'+
    '/property[@name="HRef"]';
var
  Document: IXMLDOMDocument;
  Nodes: IXMLDOMNodeList;
  I: Integer;
  HRef, UnitName: string;
begin
  { This method aims at loading so-called unit files from FunLabyrinthe < 5.2,
    as plain units from FunLabyrinthe >= 5.2. }

  Document := ADocument as IXMLDOMDocument;
  Nodes := Document.documentElement.selectNodes(Query);

  for I := 0 to Nodes.length-1 do
  begin
    HRef := Nodes.item[I].text;
    Delete(HRef, 1, LastDelimiter(HRefDelim, HRef));
    UnitName := ChangeFileExt(HRef, '');
    FUsedUnits.Add(UnitName);
  end;
end;

{*
  Charge la liste des sources (compatibilité < 5.2)
  @param Document   Document XML DOM contenu du fichier
*}
procedure TMasterFile.CompatibilityLoadSources(const ADocument: IInterface);
const {don't localize}
  Query = '/funlabyrinthe/collection[@name="SourceFiles"]/items/item'+
    '/property[@name="HRef"]';
var
  Document: IXMLDOMDocument;
  Nodes: IXMLDOMNodeList;
  I: Integer;
begin
  { This method aims at loading source files from FunLabyrinthe < 5.2,
    as new source files from FunLabyrinthe >= 5.2. }

  Document := ADocument as IXMLDOMDocument;
  Nodes := Document.documentElement.selectNodes(Query);

  for I := 0 to Nodes.length-1 do
    FSourceFiles.Add(Nodes.item[I].text);
end;

{*
  Teste la validité de l'ouverture d'un fichier
  TestOpeningValidity vérifie que le fichier ouvert ne l'a pas été
  « illégalement ». Un cas d'illégalité existe actuellement :
  - Le fichier est une sauvegarde et est ouvert autrement que pour y jouer.
  @throws EInOutError : Le fichier a été ouvert illégalement
*}
procedure TMasterFile.TestOpeningValidity;
begin
  if IsSaveguard and (Mode <> fmPlay) then
    raise EInOutError.Create(SCantEditSaveguard);
end;

{*
  Charge les unités utilisées
*}
procedure TMasterFile.LoadUsedUnits;
var
  I: Integer;
begin
  for I := 0 to UsedUnits.Count-1 do
    SepiRoot.LoadUnit(UsedUnits[I]);

  CallUnitEventProcs('InitializeUnit'); {don't localize}
end;

{*
  Appelle les méthodes d'événement des unités
  @param Name   Nom de la méthode d'événement
*}
procedure TMasterFile.CallUnitEventProcs(const Name: string);
var
  I: Integer;
begin
  for I := 0 to SepiRoot.UnitCount-1 do
    CallUnitEventProc(SepiRoot.Units[I], Name);
end;

{*
  Appelle une méthode d'événement d'une unité
  @param SepiUnit   Unité Sepi dont appeler une méthode événement
  @param Name       Nom de la méthode d'événement
*}
procedure TMasterFile.CallUnitEventProc(SepiUnit: TSepiUnit;
  const Name: string);
type
  TUnitEventProc = procedure(Master: TMaster);
var
  Method: TSepiMethod;
  Proc: TUnitEventProc;
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
      (not Params[0].CompatibleWith(SepiRoot.FindClass(TMaster))) then
      Exit;
    if CallingConvention <> ccRegister then
      Exit;
  end;

  // Call the procedure
  @Proc := Method.Code;
  Proc(Master);
end;

{*
  [@inheritDoc]
*}
procedure TMasterFile.DefineProperties(Filer: TFunLabyFiler);
var
  Additionnal: TStrings;
  I: Integer;
  Component: TFunLabyComponent;
  ID: TComponentID;
  CompClassName: string;
  CompClass: TSepiClass;
begin
  inherited;

  if Mode = fmPlay then
    Filer.DefineFieldProperty('ProjectDir', TypeInfo(TFileName), @FProjectDir);

  Filer.DefineStrings('UsedUnits', FUsedUnits);
  if psReading in PersistentState then
    LoadUsedUnits;

  if Mode <> fmPlay then
    Filer.DefineStrings('SourceFiles', SourceFiles);

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
  [@inheritDoc]
*}
function TMasterFile.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{*
  [@inheritDoc]
*}
function TMasterFile._AddRef: Integer;
begin
  Result := -1;
end;

{*
  [@inheritDoc]
*}
function TMasterFile._Release: Integer;
begin
  Result := -1;
end;

{*
  [@inheritDoc]
*}
function TMasterFile.IsEditing: Boolean;
begin
  Result := Mode = fmEdit;
end;

{*
  [@inheritDoc]
*}
procedure TMasterFile.AfterConstruction;
begin
  inherited;

  CallUnitEventProcs('Loaded'); {don't localize}
end;

{*
  [@inheritDoc]
*}
procedure TMasterFile.BeforeDestruction;
begin
  inherited;

  CallUnitEventProcs('Unloading'); {don't localize}
end;

{*
  Résoud l'adresse HRef en cherchant dans les dossiers correspondants
  @param HRef   Adresse HRef du fichier
  @param Dir    Nom du dossier de recherche
  @return Nom du fichier qualifié de son chemin d'accès
  @throws EInOutError Le fichier n'existe pas
*}
function TMasterFile.ResolveHRef(const HRef, Dir: string): TFileName;
begin
  Result := ResolveHRef(HRef, Dir, ['']);
end;

{*
  Résoud l'adresse HRef en cherchant dans les dossiers correspondants
  @param HRef         Adresse HRef du fichier
  @param Dir          Nom du dossier de recherche
  @param Extensions   Extensions possibles
  @return Nom du fichier qualifié de son chemin d'accès
  @throws EInOutError Le fichier n'existe pas
*}
function TMasterFile.ResolveHRef(const HRef, Dir: string;
  const Extensions: array of string): TFileName;
begin
  Result := HRefToFileName(HRef,
    [JoinPath([ProjectDir, Dir]), JoinPath([LibraryPath, Dir])], Extensions);
end;

{*
  Construit une adresse HRef pour un fichier
  @param FileName   Nom du fichier
  @param Dir        Nom du dossier de recherche
  @return Adresse HRef du fichier, relativement au contexte du fichier maître
  @throws EInOutError Le fichier n'existe pas
*}
function TMasterFile.MakeHRef(const FileName: TFileName;
  const Dir: string): string;
begin
  Result := FileNameToHRef(FileName,
    [JoinPath([ProjectDir, Dir]), JoinPath([LibraryPath, Dir])]);
end;

{*
  Cherche une ressource d'après son href et son type
  @param HRef         HRef de la ressource
  @param Kind         Type de ressource recherchée
  @param Extensions   Extensions possibles
  @return Nom complet du fichier pour cette ressource
  @throws EResourceNotFoundException La ressource n'a pas pu être trouvée
*}
function TMasterFile.FindResource(const HRef: string;
  Kind: TResourceKind; const Extensions: array of string): TFileName;
begin
  try
    Result := ResolveHRef(HRef,
      JoinPath([ResourcesDir, ResourceKindToDir[Kind]]), Extensions);
  except
    on EInOutError do
      raise EResourceNotFoundException.CreateFmt(SResourceNotFound,
        [HRef, GetEnumName(TypeInfo(TResourceKind), Ord(Kind))]);
  end;
end;

{*
  Construit une adresse HRef pour un fichier ressource
  @param FileName   Nom du fichier ressource
  @param Kind       Type de ressource
  @return HRef de la ressource, relativement au contexte du fichier maître
  @throws EInOutError Le fichier n'existe pas
*}
function TMasterFile.MakeResourceHRef(const FileName: TFileName;
  Kind: TResourceKind): string;
begin
  try
    Result := MakeHRef(FileName,
      JoinPath([ResourcesDir, ResourceKindToDir[Kind]]));
  except
    on EInOutError do
      raise EResourceNotFoundException.CreateFmt(SCannotMakeResourceHRef,
        [FileName, GetEnumName(TypeInfo(TResourceKind), Ord(Kind))]);
  end;
end;

{*
  Commence la partie
*}
procedure TMasterFile.GameStarted;
var
  I: Integer;
begin
  CallUnitEventProcs('GameStarted'); {don't localize}

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
begin
  CallUnitEventProcs('GameEnded'); {don't localize}
end;

{*
  Enregistre le fichier en modifiant les fichiers unité utilisés
  Save ne vérifie aucunement des dépendances éventuelles entre unités ou des
  cartes envers les unités.
  @param UnitFileDescs   Descripteurs de fichiers unité, pour les modifier
  @param AFileName       Nom du fichier à enregistrer (vide conserve l'existant)
*}
procedure TMasterFile.Save(UsedUnits: TStrings;
  const AFileName: TFileName = '');
var
  OldUsedUnits: TStrings;
begin
  OldUsedUnits := FUsedUnits;
  try
    FUsedUnits := UsedUnits;
    Save(AFileName);
  finally
    FUsedUnits := OldUsedUnits;
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

initialization
  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    FunLabyAppDataPath :=
      ReadString('Directories', 'AppData', Dir); {don't localize}

    LibraryPath := JoinPath([FunLabyAppDataPath, LibraryDir]);
    ProjectsPath := JoinPath([FunLabyAppDataPath, ProjectsDir]);
    SaveguardsPath := JoinPath([FunLabyAppDataPath, SaveguardsDir]);
  finally
    Free;
  end;

  LoadPlugins(JoinPath([Dir, UnitPackagesDir]));
end.

