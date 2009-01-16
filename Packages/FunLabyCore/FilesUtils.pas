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
  SysUtils, Classes, Contnrs, ScUtils, ScLists, SepiReflectionCore,
  FunLabyUtils;

resourcestring
  sInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  sVersionTooHigh = 'Le fichier a été enregistré avec une version ultérieure '+
    'de FunLabyrinthe (v%s). Il ne peut être ouvert.';
  sFileNotFound = 'Le fichier spécifié "%s" n''existe pas';
  sUnknownUnitType = 'Type d''unité ''%s'' inconnu';
  sThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  sEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';
  sSourcesNotHandledWhilePlaying =
    'Les fichiers source ne sont pas gérés en mode jeu';

  sNoFileName = 'Aucun nom de fichier spécifié';

  sTemporaryStatedMap =
    'La carte d''ID %s est dans un état temporaire qui ne peut être enregistré';

type
  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmPlay);

  /// Générée si un fichier ne respecte pas le format attendu
  EFileError = class(Exception);

  TMasterFile = class;

  {*
    Représente un fichier dépendant d'un fichier maître FunLabyrinthe
    TDependantFile est la classe de base pour les classes chargeant et
    enregistrant des fichiers dépendant d'un fichier maître FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TDependantFile = class
  private
    FMasterFile: TMasterFile; /// Fichier maître
    FHRef: string;            /// HRef
    FFileName: TFileName;     /// Nom du fichier
    FMaster: TMaster;         /// Maître FunLabyrinthe
  public
    constructor Create(AMasterFile: TMasterFile; const AHRef: string;
      const AFileName: TFileName);

    property MasterFile: TMasterFile read FMasterFile;
    property HRef: string read FHRef;
    property FileName: TFileName read FFileName;
    property Master: TMaster read FMaster;
  end;

  {*
    Représente un fichier unité
    TUnitFile est la classe de base pour les fichiers d'unité FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TUnitFile = class(TDependantFile)
  private
    FHandlerGUID: TGUID; /// GUID du gestionnaire du fichier unité
  public
    constructor Create(AMasterFile: TMasterFile; const AHRef: string;
      const AFileName: TFileName; const AHandlerGUID: TGUID;
      Params: TStrings); virtual;
    procedure AfterConstruction; override;

    procedure Loaded; virtual;
    procedure Unloading; virtual;

    procedure GameStarted; virtual;
    procedure GameEnded; virtual;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc); virtual;

    procedure GetParams(Params: TStrings); virtual;

    property HandlerGUID: TGUID read FHandlerGUID;
  end;

  TUnitFileClass = class of TUnitFile;

  {*
    Représente un fichier source
    @author sjrd
    @version 5.0
  *}
  TSourceFile = class(TDependantFile)
  public
    procedure AfterConstruction; override;
  end;

  TSourceFileClass = class of TSourceFile;

  {*
    Représente un fichier carte
    TMapFile représente un fichier carte FunLabyrinthe (extension .flm). Elle
    fournit des méthodes pour créer, charger et enregistrer des cartes.
    @author sjrd
    @version 5.0
  *}
  TMapFile = class(TDependantFile)
  private
    FMapID: TComponentID; /// ID de la carte liée
    FMap: TMap;           /// Carte liée
  public
    constructor Create(AMasterFile: TMasterFile; const AHRef: string;
      const AFileName: TFileName; const AMapID: TComponentID);
    constructor CreateNew(AMasterFile: TMasterFile;
      const AMapID: TComponentID; const ADimensions: T3DPoint;
      AZoneWidth, AZoneHeight: Integer);
    procedure AfterConstruction; override;

    procedure Save(const AHRef: string = '';
      const AFileName: TFileName = '');

    property MapID: TComponentID read FMapID;
    property Map: TMap read FMap;
  end;

  TMapFileClass = class of TMapFile;

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
    GUID: TGUID;             /// GUID du gestionnaire
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
  TMasterFile = class
  private
    FSepiRoot: TSepiRoot; /// Meta-racine Sepi

    FFileName: TFileName; /// Nom du fichier
    FMode: TFileMode;     /// Mode d'ouverture du fichier
    FVersion: string;     /// Version lors de l'enregistrement

    FTitle: string;       /// Titre du labyrinthe
    FDescription: string; /// Description
    FDifficulty: string;  /// Difficulté
    FAuthorID: Integer;   /// ID Web de l'auteur, ou 0 si non renseigné
    FAuthor: string;      /// Nom de l'auteur

    FAllowEdit: Boolean;   /// Indique si le fichier peut être édité
    FIsSaveguard: Boolean; /// Indique si le fichier était une sauvegarde

    FMaster: TMaster; /// Maître FunLabyrinthe

    FUnitFiles: TObjectList;   /// Liste des fichiers unité
    FSourceFiles: TObjectList; /// Liste des fichiers source
    FMapFiles: TObjectList;    /// Liste des fichiers carte

    procedure InvalidFormat;

    procedure Load(ADocument: IInterface);
    procedure TestOpeningValidity;

    function GetUnitFileCount: Integer;
    function GetUnitFiles(Index: Integer): TUnitFile;
    function GetSourceFileCount: Integer;
    function GetSourceFiles(Index: Integer): TSourceFile;
    function GetMapFileCount: Integer;
    function GetMapFiles(Index: Integer): TMapFile;
  public
    constructor Create(ASepiRoot: TSepiRoot; const AFileName: TFileName;
      AMode: TFileMode);
    constructor CreateNew(ASepiRoot: TSepiRoot;
      const UnitFileDescs: TUnitFileDescs; FileContents: TStrings = nil);
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function ResolveHRef(const HRef, DefaultDir: string): TFileName;
    function MakeHRef(const FileName: TFileName;
      const DefaultDir: string): string;

    function AddSourceFile(const HRef: string): TSourceFile;
    procedure RemoveSourceFile(SourceFile: TSourceFile);

    function AddMapFile(const ID: TComponentID; const HRef: string;
      MaxViewSize: Integer = 1): TMapFile;
    function AddNewMapFile(const ID: TComponentID;
      const Dimensions: T3DPoint;
      ZoneWidth, ZoneHeight: Integer;
      MaxViewSize: Integer = 1): TMapFile;

    procedure GameStarted;
    procedure GameEnded;

    procedure RegisterComponents(
      RegisterSingleComponentProc: TRegisterSingleComponentProc;
      RegisterComponentSetProc: TRegisterComponentSetProc);

    procedure GetUnitFileDescs(out UnitFileDescs: TUnitFileDescs);

    procedure Save(const UnitFileDescs: TUnitFileDescs;
      const AFileName: TFileName = ''); overload;
    procedure Save(const AFileName: TFileName = ''); overload;

    property SepiRoot: TSepiRoot read FSepiRoot;

    property FileName: TFileName read FFileName;
    property Mode: TFileMode read FMode;
    property Version: string read FVersion;

    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Difficulty: string read FDifficulty write FDifficulty;
    property AuthorID: Integer read FAuthorID write FAuthorID;
    property Author: string read FAuthor write FAuthor;

    property AllowEdit: Boolean read FAllowEdit;
    property IsSaveguard: Boolean read FIsSaveguard;

    property Master: TMaster read FMaster;

    property UnitFileCount: Integer read GetUnitFileCount;
    property UnitFiles[Index: Integer]: TUnitFile read GetUnitFiles;
    property SourceFileCount: Integer read GetSourceFileCount;
    property SourceFiles[Index: Integer]: TSourceFile read GetSourceFiles;
    property MapFileCount: Integer read GetMapFileCount;
    property MapFiles[Index: Integer]: TMapFile read GetMapFiles;
  end;

  {*
    Collection de classes de fichiers unité, référencées par leur GUID
    @author sjrd
    @version 5.0
  *}
  TUnitFileClassList = class(TCustomValueBucketList)
  public
    constructor Create;

    procedure Add(const GUID: TGUID; UnitFileClass: TUnitFileClass);
    procedure Remove(const GUID: TGUID);
    function Find(const GUID: TGUID): TUnitFileClass;
  end;

function HRefToFileName(const HRef: string;
  const BaseDirs: array of TFileName): TFileName;
function FileNameToHRef(const FileName: TFileName;
  const BaseDirs: array of TFileName): string;

const {don't localize}
  HRefDelim = '/'; /// Délimiteur dans les href

var
  /// Gestionnaires d'unité : association GUID <-> classe d'unité
  UnitFileClasses: TUnitFileClassList = nil;

implementation

uses
  StrUtils, ScStrUtils, IniFiles, Variants, MSXML;

const {don't localize}
  /// Code de format d'un fichier FLM (correspond à '.flm')
  FLMFormatCode: Longint = $6D6C662E;

  FLMVersion = 1; /// Version courante du format FLM

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
  SubFile := StringReplace(HRef, HRefDelim, PathDelim, [rfReplaceAll]);

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
    raise EFileError.CreateFmt(sFileNotFound, [HRef]);
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

  Result := StringReplace(Result, PathDelim, HRefDelim, [rfReplaceAll]);
end;

{-----------------------}
{ Classe TDependantFile }
{-----------------------}

{*
  Crée une instance de TDependantFile
  @param AMasterFile   Fichier maître
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
*}
constructor TDependantFile.Create(AMasterFile: TMasterFile;
  const AHRef: string; const AFileName: TFileName);
begin
  inherited Create;
  FMasterFile := AMasterFile;
  FHRef := AHRef;
  FFileName := AFileName;
  FMaster := FMasterFile.Master;
end;

{------------------}
{ Classe TUnitFile }
{------------------}

{*
  Crée une instance de TUnitFile
  @param AMasterFile   Fichier maître
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param Params        Paramètres envoyés à l'unité
*}
constructor TUnitFile.Create(AMasterFile: TMasterFile; const AHRef: string;
  const AFileName: TFileName; const AHandlerGUID: TGUID; Params: TStrings);
begin
  inherited Create(AMasterFile, AHRef, AFileName);
  FHandlerGUID := AHandlerGUID;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TUnitFile.AfterConstruction;
begin
  inherited;
  MasterFile.FUnitFiles.Add(Self);
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
  Enregistre les différents composants à placer dans la palette d'édition
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure TUnitFile.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);
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
end;

{--------------------}
{ Classe TSourceFile }
{--------------------}

{*
  [@inheritDoc]
*}
procedure TSourceFile.AfterConstruction;
begin
  inherited;
  MasterFile.FSourceFiles.Add(Self);
end;

{-----------------}
{ Classe TMapFile }
{-----------------}

{*
  Crée une instance de TMapFile en chargeant la carte depuis un fichier
  @param AMasterFile   Fichier maître
  @param AHRef         HRef du fichier
  @param AFileName     Nom du fichier
  @param AMapID        ID de la carte
*}
constructor TMapFile.Create(AMasterFile: TMasterFile; const AHRef: string;
  const AFileName: TFileName; const AMapID: TComponentID);
var
  Stream: TStream;
  ZoneWidth, ZoneHeight, I, Count, Value, ScrewSize: Integer;
  Dimensions: T3DPoint;
  Palette: array of TScrew;
begin
  inherited Create(AMasterFile, AHRef, AFileName);
  FMapID := AMapID;

  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    // Contrôle de format
    Stream.ReadBuffer(Value, 4);
    if Value <> FLMFormatCode then
      EFileError.Create(sInvalidFileFormat);

    // Contrôle de version de format
    Stream.ReadBuffer(Value, 4);
    if Value > FLMVersion then
      EFileError.CreateFmt(sVersionTooHigh, [IntToStr(Value)]);

    // Lecture des dimensions et de la taille d'une zone
    Stream.ReadBuffer(Dimensions, SizeOf(T3DPoint));
    Stream.ReadBuffer(ZoneWidth, 4);
    Stream.ReadBuffer(ZoneHeight, 4);

    // Création de la carte elle-même
    FMap := TMap.Create(Master, MapID, Dimensions, ZoneWidth, ZoneHeight);

    // Lecture de la palette de cases
    Stream.ReadBuffer(Count, 4);
    SetLength(Palette, Count);
    for I := 0 to Count-1 do
      Palette[I] := Master.Screw[ReadStrFromStream(Stream)];

    // Lecture de la carte
    if Count <= 256 then
      ScrewSize := 1
    else
      ScrewSize := 2;
    Value := 0;
    for I := 0 to Map.LinearMapCount-1 do
    begin
      Stream.ReadBuffer(Value, ScrewSize);
      Map.LinearMap[I] := Palette[Value];
    end;
  finally
    Stream.Free;
  end;
end;

{*
  Crée une instance de TMapFile, en créant une nouvelle carte
  La nouvelle carte ainsi créée est vide : il est impératif de la remplir avant
  toute utilisation, sous peine de violations d'accès.
  @param AMasterFile   Fichier maître
  @param AMapID        ID de la carte
  @param ADimensions   Dimensions de la carte
  @param AZoneWidth    Largeur d'une zone
  @param AZoneHeight   Hauteur d'une zone
*}
constructor TMapFile.CreateNew(AMasterFile: TMasterFile;
  const AMapID: TComponentID; const ADimensions: T3DPoint;
  AZoneWidth, AZoneHeight: Integer);
begin
  inherited Create(AMasterFile, '', '');
  FMapID := AMapID;
  FMap := TMap.Create(Master, AMapID, ADimensions, AZoneWidth, AZoneHeight);
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TMapFile.AfterConstruction;
begin
  inherited;
  MasterFile.FMapFiles.Add(Self);
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier dans lequel enregistrer
*}
procedure TMapFile.Save(const AHRef: string = '';
  const AFileName: TFileName = '');
var
  I, Value, Count, PaletteCountPos, ScrewSize: Integer;
  Stream: TStream;
  Dimensions: T3DPoint;
begin
  if AHRef <> '' then
  begin
    FHRef := AHRef;
    FFileName := AFileName;
  end;

  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    // Indication de format
    Value := FLMFormatCode;
    Stream.WriteBuffer(Value, 4);

    // Indication de version de format
    Value := FLMVersion;
    Stream.WriteBuffer(Value, 4);

    // Écriture des dimensions et de la taille d'une zone
    Dimensions := Map.Dimensions;
    Stream.WriteBuffer(Dimensions, SizeOf(T3DPoint));
    Value := Map.ZoneWidth;
    Stream.WriteBuffer(Value, 4);
    Value := Map.ZoneHeight;
    Stream.WriteBuffer(Value, 4);

    // Préparation de la palette (Screws.Tag) et écriture de celle-ci
    for I := 0 to Master.ScrewCount-1 do
      Master.Screws[I].Tag := -1;
    Count := 0;
    PaletteCountPos := Stream.Position;
    Stream.WriteBuffer(Count, 4); // On repassera changer çà plus tard
    for I := 0 to Map.LinearMapCount-1 do
    begin
      with Map.LinearMap[I] do
      begin
        if ClassType <> TScrew then
          raise EFileError.CreateFmt(sTemporaryStatedMap, [Map.ID]);
        if Tag < 0 then
        begin
          Tag := Count;
          WriteStrToStream(Stream, ID);
          Inc(Count);
        end;
      end;
    end;

    // Écriture de la carte
    if Count <= 256 then
      ScrewSize := 1
    else
      ScrewSize := 2;
    for I := 0 to Map.LinearMapCount-1 do
    begin
      Value := Map.LinearMap[I].Tag;
      Stream.WriteBuffer(Value, ScrewSize);
    end;

    // On retourne écrire la taille de la palette
    Stream.Seek(PaletteCountPos, soFromBeginning);
    Stream.WriteBuffer(Count, 4);
  finally
    Stream.Free;
  end;
end;

{--------------------}
{ Classe TMasterFile }
{--------------------}

{*
  Ouvre un fichier FunLabyrinthe
  @param AFileName   Nom du fichier à ouvrir
  @param AMode       Mode sous lequel ouvrir le fichier
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TMasterFile.Create(ASepiRoot: TSepiRoot;
  const AFileName: TFileName; AMode: TFileMode);
var
  Document: IXMLDOMDocument;
begin
  inherited Create;

  FSepiRoot := ASepiRoot;

  FFileName := AFileName;
  FMode := AMode;
  FVersion := CurrentVersion;

  FTitle := '';
  FDescription := '';
  FDifficulty := '';
  FAuthorID := 0;
  FAuthor := '';

  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create(Mode = fmEdit);

  FUnitFiles := TObjectList.Create;
  FSourceFiles := TObjectList.Create;
  FMapFiles := TObjectList.Create;

  Document := CoDOMDocument.Create;
  Document.async := False;
  if not Document.load(FFileName) then
    InvalidFormat;

  Load(Document);
  TestOpeningValidity;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param Dimensions     Dimensions de la carte
  @param FileContents   Contenu pré-créé du fichier (ou nil pour créer un vide)
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TMasterFile.CreateNew(ASepiRoot: TSepiRoot;
  const UnitFileDescs: TUnitFileDescs; FileContents: TStrings = nil);
var
  I, J: Integer;
  Parameters: TStrings;
  Document: IXMLDOMDocument;
begin
  inherited Create;

  FSepiRoot := ASepiRoot;

  FFileName := '';
  FMode := fmEdit;
  FVersion := CurrentVersion;

  FTitle := '';
  FDescription := '';
  FDifficulty := '';
  FAuthorID := 0;
  FAuthor := '';

  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create(True);

  FUnitFiles := TObjectList.Create;
  FSourceFiles := TObjectList.Create;
  FMapFiles := TObjectList.Create;

  // Ajouter les unités décrites par UnitFileDescs
  if Length(UnitFileDescs) > 0 then
  begin
    Parameters := TStringList.Create;
    try
      for I := 0 to Length(UnitFileDescs)-1 do
      begin
        with UnitFileDescs[I] do
        begin
          Parameters.Clear;
          for J := 0 to Length(Params)-1 do
            with Params[I] do
              Parameters.Values[Name] := Value;
          UnitFileClasses.Find(GUID).Create(Self, HRef,
            ResolveHRef(HRef, fUnitsDir), GUID, Parameters);
        end;
      end;
    finally
      Parameters.Free;
    end;
  end;

  // Prendre en compte un éventuel XML de base
  if Assigned(FileContents) then
  begin
    Document := CoDOMDocument.Create;
    Document.async := False;
    if not Document.loadXML(FileContents.Text) then
      InvalidFormat;

    Load(Document);
    TestOpeningValidity;
  end;
end;

{*
  Détruit l'instance
*}
destructor TMasterFile.Destroy;
begin
  { Ici on détruit le maître d'abord, afin de permettre aux unités de
    décharger leurs infos sans contraintes. Évidemment il ne faut donc pas que
    les gestionnaires d'unités et de cartes n'accèdent encore au maître dans
    le destructeur. }
  FMaster.Free;
  FMapFiles.Free;
  FSourceFiles.Free;

  { Dans la mesure où des unités pourraient être dépendantes d'autres, il faut
    absolument les libérer dans l'ordre inverse de chargement. }
  if Assigned(FUnitFiles) then
  begin
    while FUnitFiles.Count > 0 do
      FUnitFiles.Delete(FUnitFiles.Count-1);
    FUnitFiles.Free;
  end;

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

  for I := 0 to UnitFileCount-1 do
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

  for I := UnitFileCount-1 downto 0 do
    UnitFiles[I].Unloading;
end;

{*
  Génère une erreur indiquant que le fichier ne respecte pas le format attendu
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
procedure TMasterFile.InvalidFormat;
begin
  raise EFileError.Create(sInvalidFileFormat);
end;

{*
  Charge le contenu du document
  @param Document   Document XML DOM contenu du fichier
  @throws EFileError : Un fichier à charger n'existe pas ou n'est pas valide
*}
procedure TMasterFile.Load(ADocument: IInterface);

  function NullToEmptyStr(Value: Variant): Variant;
  begin
    if VarIsNull(Value) then
      Result := ''
    else
      Result := Value;
  end;

var
  Document: IXMLDOMDocument;
  Params: TStrings;
  I, J, MaxViewSize: Integer;
  ID, HRef, Name, MapID: string;
  FileType: TGUID;
  Position: T3DPoint;
  Player: TPlayer;
  Node: IXMLDOMNode;
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
      raise EFileError.CreateFmt(sVersionTooHigh, [FVersion]);

    // Attributs du fichier
    FAllowEdit := NullToEmptyStr(getAttribute('allowedit')) <> 'no';
    FIsSaveguard := NullToEmptyStr(getAttribute('issaveguard')) = 'yes';

    // Infos standart sur le labyrinthe
    FTitle := selectSingleNode('./title').text;
    FDescription := NullToEmptyStr(selectSingleNode('./description').text);
    FDifficulty := NullToEmptyStr(selectSingleNode('./difficulty').text);
    with selectSingleNode('./author') as IXMLDOMElement do
    begin
      FAuthorID := StrToIntDef(NullToEmptyStr(getAttribute('id')), 0);
      FAuthor := NullToEmptyStr(text);
    end;

    // Attributs du maître
    with selectSingleNode('./master') do
    begin
      Node := selectSingleNode('./temporization');
      if Node <> nil then
        Master.Temporization := StrToIntDef(NullToEmptyStr(Node.text), 0);
    end;

    // Unités utilisées
    Params := THashedStringList.Create;
    try
      with selectNodes('./units/unit') do
      begin
        for I := 0 to length-1 do
        begin
          with item[I] as IXMLDOMElement do
          begin
            FileType := StringToGUID(getAttribute('type'));
            HRef := getAttribute('href');

            Params.Clear;
            with selectNodes('./param') do
            begin
              for J := 0 to length-1 do
                with item[J] as IXMLDOMElement do
                  Params.Values[getAttribute('name')] := getAttribute('value');
            end;

            UnitFileClasses.Find(FileType).Create(Self, HRef,
              ResolveHRef(HRef, fUnitsDir), FileType, Params);
          end;
        end;
      end;
    finally
      Params.Free;
    end;

    // Fichiers source liés - pas en mode jeu
    if Mode <> fmPlay then
    begin
      with selectNodes('./sources/source') do
      begin
        for I := 0 to length-1 do
        begin
          with item[I] as IXMLDOMElement do
          begin
            HRef := getAttribute('href');
            TSourceFile.Create(Self, HRef, ResolveHRef(HRef, fUnitsDir));
          end;
        end;
      end;
    end;

    // Cartes
    with selectNodes('./maps/map') do
    begin
      for I := 0 to length-1 do
      begin
        with item[I] as IXMLDOMElement do
        begin
          ID := getAttribute('id');
          HRef := getAttribute('href');

          if VarIsNull(getAttribute('maxviewsize')) then
            MaxViewSize := MinViewSize
          else
            MaxViewSize := getAttribute('maxviewsize');

          AddMapFile(ID, HRef, MaxViewSize);
        end;
      end;
    end;

    // Joueurs
    with selectNodes('./players/player') do
    begin
      if length <> 1 then
        raise EFileError.Create(sThereMustBeOnePlayer);
      for I := 0 to length-1 do
      begin
        with item[I] as IXMLDOMElement do
        begin
          ID := getAttribute('id');
          if VarIsNull(getAttribute('name')) then
            Name := ID
          else
            Name := getAttribute('name');

          with selectSingleNode('./position') as IXMLDOMElement do
          begin
            MapID := getAttribute('map');
            Position.X := getAttribute('posx');
            Position.Y := getAttribute('posy');
            Position.Z := getAttribute('posz');
          end;

          Player := TPlayer.Create(Master, ID, Name,
            Master.Map[MapID], Position);

          if NullToEmptyStr(getAttribute('lost')) = 'yes' then
            Player.Lose;

          with selectNodes('./attributes/attribute') do
          begin
            for J := 0 to length-1 do
              with item[J] as IXMLDOMElement do
                Player.Attribute[getAttribute('name')] := getAttribute('value');
          end;

          with selectNodes('./plugins/plugin') do
          begin
            for J := 0 to length-1 do
              with item[J] as IXMLDOMElement do
                Player.AddPlugin(Master.Plugin[getAttribute('id')]);
          end;
        end;
      end;
    end;
  end;
end;

{*
  Teste la validité de l'ouverture d'un fichier
  TestOpeningValidity vérifie que le fichier ouvert ne l'a pas été
  « illégalement ». Deux cas d'illégalité sont à tester :
  - Le fichier est une sauvegarde et est ouvert autrement que pour y jouer ;
  - Le fichier a été interdit d'édition, et ouvert dans ce mode.
  @throws EFileError : Le fichier a été ouvert illégalement
*}
procedure TMasterFile.TestOpeningValidity;
begin
  if IsSaveguard and (Mode <> fmPlay) then
    raise EFileError.Create(sCantEditSaveguard);
  if (not AllowEdit) and (Mode = fmEdit) then
    raise EFileError.Create(sEditingNotAllowed);
end;

{*
  Nombre de fichiers unité
  @return Nombre de fichiers unité
*}
function TMasterFile.GetUnitFileCount: Integer;
begin
  Result := FUnitFiles.Count;
end;

{*
  Tableau zero-based des fichiers unité
  @param Index   Index du fichier unité
  @return Le fichier unité dont l'index a été spécifié
*}
function TMasterFile.GetUnitFiles(Index: Integer): TUnitFile;
begin
  Result := TUnitFile(FUnitFiles[Index]);
end;

{*
  Nombre de fichiers source
  @return Nombre de fichiers source
*}
function TMasterFile.GetSourceFileCount: Integer;
begin
  Result := FSourceFiles.Count;
end;

{*
  Tableau zero-based des fichiers source
  @param Index   Index du fichier source
  @return Le fichier source dont l'index a été spécifié
*}
function TMasterFile.GetSourceFiles(Index: Integer): TSourceFile;
begin
  Result := TSourceFile(FSourceFiles[Index]);
end;

{*
  Nombre de fichiers carte
  @return Nombre de fichiers carte
*}
function TMasterFile.GetMapFileCount: Integer;
begin
  Result := FMapFiles.Count;
end;

{*
  Tableau zero-based des fichiers carte
  @param Index   Index du fichier carte
  @return Le fichier carte dont l'index a été spécifié
*}
function TMasterFile.GetMapFiles(Index: Integer): TMapFile;
begin
  Result := TMapFile(FMapFiles[Index]);
end;

{*
  Résoud l'adresse HRef en cherchant dans les dossiers correspondants
  @param HRef         Adresse HRef du fichier
  @param DefaultDir   Dossier par défaut du type de fichier attendu
  @return Nom du fichier qualifié de son chemin d'accès
  @throws EFileError Le fichier n'existe pas
*}
function TMasterFile.ResolveHRef(const HRef, DefaultDir: string): TFileName;
var
  FilePath, SubDir: string;
begin
  FilePath := ExtractFilePath(FileName);
  SubDir := ChangeFileExt(ExtractFileName(FileName), '') + PathDelim;

  Result := HRefToFileName(HRef,
    [FilePath+SubDir, DefaultDir+SubDir, FilePath, DefaultDir]);
end;

{*
  Construit une adresse HRef pour un fichier
  @param FileName     Nom du fichier
  @param DefaultDir   Dossier par défaut du type du fichier
  @return Adresse HRef du fichier, relativement au contexte du fichier maître
  @throws EFileError Le fichier n'existe pas
*}
function TMasterFile.MakeHRef(const FileName: TFileName;
  const DefaultDir: string): string;
var
  FilePath, SubDir: string;
begin
  FilePath := ExtractFilePath(FileName);
  SubDir := ChangeFileExt(ExtractFileName(FileName), '') + PathDelim;

  Result := FileNameToHRef(FileName,
    [FilePath+SubDir, DefaultDir+SubDir, FilePath, DefaultDir]);
end;

{*
  Ajoute un fichier source
  @param HRef   Adresse du fichier
  @return Le fichier source créé
*}
function TMasterFile.AddSourceFile(const HRef: string): TSourceFile;
begin
  if Mode = fmPlay then
    raise EFileError.Create(sSourcesNotHandledWhilePlaying);
  Result := TSourceFile.Create(Self, HRef, ResolveHRef(HRef, fUnitsDir));
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
  Ajoute un fichier carte
  @param ID            ID de la carte
  @param HRef          Adresse du fichier
  @param MaxViewSize   Taille maximale d'une vue pour cette carte
  @return Le fichier carte créé et chargé
*}
function TMasterFile.AddMapFile(const ID: TComponentID; const HRef: string;
  MaxViewSize: Integer = 1): TMapFile;
begin
  Result := TMapFile.Create(Self, HRef, ResolveHRef(HRef, fMapsDir), ID);
  Result.Map.MaxViewSize := MaxViewSize;
end;

{*
  Ajoute un nouveau fichier carte
  @param ID            ID de la carte
  @param Dimensions    Dimensions
  @param ZoneWidth     Largeur d'une zone
  @param ZoneHeight    Hauteur d'une zone
  @param MaxViewSize   Taille maximale d'une vue pour cette carte
  @return Le fichier carte créé
*}
function TMasterFile.AddNewMapFile(const ID: TComponentID;
  const Dimensions: T3DPoint; ZoneWidth, ZoneHeight: Integer;
  MaxViewSize: Integer = 1): TMapFile;
begin
  Result := TMapFile.CreateNew(Self, ID, Dimensions, ZoneWidth, ZoneHeight);
  Result.Map.MaxViewSize := MaxViewSize;
end;

{*
  Commence la partie
*}
procedure TMasterFile.GameStarted;
var
  I: Integer;
begin
  for I := 0 to UnitFileCount-1 do
    UnitFiles[I].GameStarted;
end;

{*
  Termine la partie
*}
procedure TMasterFile.GameEnded;
var
  I: Integer;
begin
  for I := 0 to UnitFileCount-1 do
    UnitFiles[I].GameEnded;
end;

{*
  Enregistre les différents composants à placer dans la palette d'édition
  @param RegisterSingleComponentProc   Call-back pour un unique composant
  @param RegisterComponentSetProc      Call-back pour un ensemble de composants
*}
procedure TMasterFile.RegisterComponents(
  RegisterSingleComponentProc: TRegisterSingleComponentProc;
  RegisterComponentSetProc: TRegisterComponentSetProc);
var
  I: Integer;
begin
  for I := 0 to UnitFileCount-1 do
  begin
    UnitFiles[I].RegisterComponents(
      RegisterSingleComponentProc, RegisterComponentSetProc);
  end;
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
    SetLength(UnitFileDescs, UnitFileCount);
    for FileIdx := 0 to UnitFileCount-1 do
    begin
      with UnitFileDescs[FileIdx] do
      begin
        UnitFile := UnitFiles[FileIdx];
        GUID := UnitFile.HandlerGUID;
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
  Document: IXMLDOMDocument;
  FunLabyrinthe, MasterNode, Units, Sources, Maps, Players: IXMLDOMElement;
  Player: IXMLDOMElement;
  Element, Param: IXMLDOMElement;
  Params: TStrings;
  MapHRef: string;
  FileName, MapFileName: TFileName;
  I, J: Integer;
begin
  { Don't localize strings in this method }

  if (AFileName = '') and (Mode = fmPlay) and (not IsSaveguard) then
    raise EFileError.Create(sNoFileName);
  if AFileName = '' then
    FileName := FFileName
  else
    FileName := AFileName;

  if Mode = fmPlay then
  begin
    MapHRef := ExtractFileName(FileName);
    I := LastDelimiter('.', MapHRef);
    if I > 0 then
    begin
      SetLength(MapHRef, I);
      MapHRef[I] := PathDelim;
    end else
      MapHRef := MapHRef + '-files' + PathDelim;

    MapFileName := ExtractFilePath(FileName) + MapHRef;
    MapHRef := StringReplace(MapHRef, PathDelim, HRefDelim, [rfReplaceAll]);

    ForceDirectories(MapFileName);
  end else
  begin
    MapHRef := '';
    MapFileName := '';
  end;

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

    with FunLabyrinthe do
    begin
      Element := Document.createElement('title');
      Element.text := Title;
      appendChild(Element);

      Element := Document.createElement('description');
      Element.text := Description;
      appendChild(Element);

      Element := Document.createElement('difficulty');
      Element.text := Difficulty;
      appendChild(Element);

      Element := Document.createElement('author');
      Element.text := Author;
      if AuthorID > 0 then
        Element.setAttribute('id', AuthorID);
      appendChild(Element);

      // Attributs du maître
      MasterNode := Document.createElement('master');
      with MasterNode do
      begin
        if Master.Temporization <> DefaultTemporization then
        begin
          Element := Document.createElement('temporization');
          Element.text := IntToStr(Master.Temporization);
          appendChild(Element);
        end;
      end;
      appendChild(MasterNode);

      // Unités
      Units := Document.createElement('units');
      with Units do
      begin
        for I := 0 to Length(UnitFileDescs)-1 do
        begin
          with UnitFileDescs[I] do
          begin
            Element := Document.createElement('unit');
            Element.setAttribute('type', GUIDToString(GUID));
            Element.setAttribute('href', HRef);

            with Element do
              for J := 0 to Length(Params)-1 do
              begin
                Param := Document.createElement('param');
                Param.setAttribute('name', Params[J].Name);
                Param.setAttribute('value', Params[J].Value);
                appendChild(Param);
              end;

            appendChild(Element); // unit
          end;
        end;
      end;
      appendChild(Units);

      // Fichiers source - pas en mode jeu
      if Mode <> fmPlay then
      begin
        Sources := Document.createElement('sources');
        with Sources do
        begin
          for I := 0 to SourceFileCount-1 do
          begin
            Element := Document.createElement('source');
            Element.setAttribute('href', SourceFiles[I].HRef);
            appendChild(Element);
          end;
        end;
        appendChild(Sources);
      end;

      // Cartes
      Maps := Document.createElement('maps');
      with Maps do
      begin
        for I := 0 to MapFileCount-1 do
        begin
          with MapFiles[I] do
          begin
            if Mode <> fmPlay then
              Save
            else
              Save(MapHRef+MapID+'.flm', MapFileName+MapID+'.flm');

            Element := Document.createElement('map');
            Element.setAttribute('id', Map.ID);
            Element.setAttribute('href', HRef);
            if Map.MaxViewSize > 1 then
              Element.setAttribute('maxviewsize', Map.MaxViewSize);
            appendChild(Element);
          end;
        end;
      end;
      appendChild(Maps);

      // Joueurs
      Players := Document.createElement('players');
      with Players do
      begin
        Params := TStringList.Create;
        try
          for I := 0 to Master.PlayerCount-1 do
          begin
            with Master.Players[I] do
            begin
              Player := Document.createElement('player');
              Player.setAttribute('id', ID);
              Player.setAttribute('name', Name);
              if PlayState = psLost then
                Player.setAttribute('lost', 'yes');

              with Player do
              begin
                Element := Document.createElement('position');
                Element.setAttribute('map', Map.ID);
                Element.setAttribute('posx', Position.X);
                Element.setAttribute('posy', Position.Y);
                Element.setAttribute('posz', Position.Z);
                appendChild(Element);

                Element := Document.createElement('attributes');
                with Element do
                begin
                  GetAttributes(Params);
                  for J := 0 to Params.Count-1 do
                  begin
                    Param := Document.createElement('attribute');
                    Param.setAttribute('name', Params[J]);
                    Param.setAttribute('value', Integer(Params.Objects[J]));
                    appendChild(Param);
                  end;
                end;
                appendChild(Element); // attributes

                Element := Document.createElement('plugins');
                with Element do
                begin
                  GetPluginIDs(Params);
                  for J := 0 to Params.Count-1 do
                  begin
                    Param := Document.createElement('plugin');
                    Param.setAttribute('id', Params[J]);
                    appendChild(Param);
                  end;
                end;
                appendChild(Element); // plugins
              end;

              appendChild(Player);
            end;
          end;
        finally
          Params.Free;
        end;
      end;
      appendChild(Players);
    end;

    appendChild(FunLabyrinthe);
  end;

  Document.save(FileName);
  FFileName := FileName;
  if Mode = fmPlay then
    FIsSaveguard := True;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier à enregistrer (si vide, conserve l'existant)
*}
procedure TMasterFile.Save(const AFileName: TFileName = '');
var
  UnitFileDescs: TUnitFileDescs;
begin
  GetUnitFileDescs(UnitFileDescs);
  Save(UnitFileDescs, AFileName);
end;

{---------------------------}
{ Classe TUnitFileClassList }
{---------------------------}

{*
  Crée une nouvelle instance de TUnitFileClassList
*}
constructor TUnitFileClassList.Create;
begin
  inherited Create(SizeOf(TGUID), SizeOf(TUnitFileClass));
end;

{*
  Référence un gestionnaire d'unité
  @param GUID            GUID du gestionnaire
  @param UnitFileClass   Classe du gestionnaire
*}
procedure TUnitFileClassList.Add(const GUID: TGUID;
  UnitFileClass: TUnitFileClass);
begin
  AddData(GUID, UnitFileClass);
end;

{*
  Supprime un gestionnaire d'unité
  @param GUID   GUID du gestionnaire à supprimer
*}
procedure TUnitFileClassList.Remove(const GUID: TGUID);
begin
  RemoveData(GUID);
end;

{*
  Trouve la classe d'un gestionnaire à partir de son GUID
  @param GUID   GUID du gestionnaire
  @return Classe du gestionnaire gérant les fichiers de type GUID
  @throws EFileError Type de fichier inconnu
*}
function TUnitFileClassList.Find(const GUID: TGUID): TUnitFileClass;
begin
  if not (inherited Find(GUID, Result)) then
    raise EFileError.CreateFmt(sUnknownUnitType, [GUIDToString(GUID)]);
end;

initialization
  UnitFileClasses := TUnitFileClassList.Create;
finalization
  UnitFileClasses.Free;
  UnitFileClasses := nil;
end.

