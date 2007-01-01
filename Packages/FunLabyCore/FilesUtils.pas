{*
  Gestion des fichiers FunLabyrinthe
  L'unité FilesUtils contient des routines et classes se chargeant du
  traitement (création, ouverture, enregistrement) des fichiers et
  l'interfaçage de leurs données avec les classes métier.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FilesUtils;

interface

uses
  SysUtils, Classes, Contnrs, ScUtils, ScLists, ScStrUtils, MSXML, FunLabyUtils,
  Variants, IniFiles;

resourcestring
  sInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  sVersionTooHigh = 'Le fichier a été enregistré avec une version ultérieure '+
    'de FunLabyrinthe (v%s). Il ne peut être ouvert.';
  sFileNotFound = 'Le fichier spécifié "%s" n''existe pas';
  sUnknownUnitType = 'Type d''unité ''%s'' inconnu';
  sUnknownMapType = 'Type de carte ''%s'' inconnu';
  sThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  sEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';

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
  *}
  TDependantFile = class
  private
    FMasterFile : TMasterFile; /// Fichier maître
    FHRef : string;            /// HRef
    FFileName : TFileName;     /// Nom du fichier
    FMaster : TMaster;         /// Maître FunLabyrinthe
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName);

    property MasterFile : TMasterFile read FMasterFile;
    property HRef : string read FHRef;
    property FileName : TFileName read FFileName;
    property Master : TMaster read FMaster;
  end;

  {*
    Représente un fichier unité
    TUnitFile est la classe de base pour les fichiers d'unité FunLabyrinthe.
  *}
  TUnitFile = class(TDependantFile)
  private
    FMIMEType : string; /// Type MIME du fichier unité
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMIMEType : string;
      Params : TStrings); virtual;
    procedure AfterConstruction; override;

    procedure Loaded; virtual;
    procedure Unloading; virtual;

    procedure GameStarted; virtual;
    procedure GameEnded; virtual;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc); virtual;

    procedure GetParams(Params : TStrings); virtual;

    property MIMEType : string read FMIMEType;
  end;
  TUnitFileClass = class of TUnitFile;

  {*
    Représente un fichier carte
    TMapFile représente un fichier carte FunLabyrinthe (extension .flm). Elle
    fournit des méthodes pour créer, charger et enregistrer des cartes.
  *}
  TMapFile = class(TDependantFile)
  private
    FMapID : TComponentID; /// ID de la carte liée
    FMap : TMap;           /// Carte liée
  public
    constructor Create(AMasterFile : TMasterFile; const AHRef : string;
      const AFileName : TFileName; const AMapID : TComponentID);
    constructor CreateNew(AMasterFile : TMasterFile;
      const AMapID : TComponentID; const ADimensions : T3DPoint;
      AZoneWidth, AZoneHeight : integer);
    procedure AfterConstruction; override;

    procedure Save(const AHRef : string = ''; const AFileName : TFileName = '');

    property MapID : TComponentID read FMapID;
    property Map : TMap read FMap;
  end;
  TMapFileClass = class of TMapFile;

  {*
    Représente un fichier maître FunLabyrinthe
    TMasterFile représente un fichier maître FunLabyrinthe (extension .flg).
    Elle est capable de charger tous les fichiers annexes au moyen des autres
    classes de l'unité.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
  *}
  TMasterFile = class
  private
    FFileName : TFileName;    /// Nom du fichier
    FMode : TFileMode;        /// Mode d'ouverture du fichier
    FVersion : string;        /// Version lors de l'enregistrement

    FTitle : string;          /// Titre du labyrinthe
    FDescription : string;    /// Description
    FDifficulty : string;     /// Difficulté
    FAuthorID : integer;      /// ID Web de l'auteur, ou 0 si non renseigné
    FAuthor : string;         /// Nom de l'auteur

    FAllowEdit : boolean;     /// Indique si le fichier peut être édité
    FIsSaveguard : boolean;   /// Indique si le fichier était une sauvegarde

    FMaster : TMaster;        /// Maître FunLabyrinthe

    FUnitFiles : TObjectList; /// Liste des fichiers unité
    FMapFiles : TObjectList;  /// Liste des fichiers carte

    procedure InvalidFormat;

    procedure Load(Document : IXMLDOMDocument;
      CreatePlayerControllerProc : TCreatePlayerControllerProc = nil);
    procedure TestOpeningValidity;

    function GetUnitFileCount : integer;
    function GetUnitFiles(Index : integer) : TUnitFile;
    function GetMapFileCount : integer;
    function GetMapFiles(Index : integer) : TMapFile;
  public
    constructor Create(const AFileName : TFileName; AMode : TFileMode;
      CreatePlayerControllerProc : TCreatePlayerControllerProc = nil);
    constructor CreateNew(FileContents : TStrings = nil);
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class procedure RegisterUnitFileClass(const MIMEType : string;
      UnitFileClass : TUnitFileClass);
    class procedure UnregisterUnitFileClass(const MIMEType : string;
      UnitFileClass : TUnitFileClass);
    class function FindUnitFileClass(const MIMEType : string) : TUnitFileClass;

    function ResolveHRef(const HRef, DefaultDir : string) : TFileName;

    function AddUnitFile(const MIMEType, HRef : string;
      Params : TStrings = nil) : TUnitFile;
    function AddMapFile(const ID : TComponentID; const HRef : string;
      MaxViewSize : integer = 1) : TMapFile;
    function AddNewMapFile(const ID : TComponentID; const Dimensions : T3DPoint;
      ZoneWidth, ZoneHeight : integer;
      MaxViewSize : integer = 1) : TMapFile;

    procedure GameStarted;
    procedure GameEnded;

    procedure RegisterComponents(
      RegisterSingleComponentProc : TRegisterSingleComponentProc;
      RegisterComponentSetProc : TRegisterComponentSetProc);

    procedure Save(AFileName : TFileName = '');

    property FileName : TFileName read FFileName;
    property Mode : TFileMode read FMode;
    property Version : string read FVersion;

    property Title : string read FTitle write FTitle;
    property Description : string read FDescription write FDescription;
    property Difficulty : string read FDifficulty write FDifficulty;
    property AuthorID : integer read FAuthorID write FAuthorID;
    property Author : string read FAuthor write FAuthor;

    property AllowEdit : boolean read FAllowEdit;
    property IsSaveguard : boolean read FIsSaveguard;

    property Master : TMaster read FMaster;

    property UnitFileCount : integer read GetUnitFileCount;
    property UnitFiles[index : integer] : TUnitFile read GetUnitFiles;
    property MapFileCount : integer read GetMapFileCount;
    property MapFiles[index : integer] : TMapFile read GetMapFiles;
  end;

const {don't localize}
  HRefDelim = '/'; /// Délimiteur dans les href

implementation

uses
  UnitFiles;

const {don't localize}
  /// Code de format d'un fichier FLM (correspond à '.flm')
  FLMFormatCode : LongInt = $6D6C662E;

  FLMVersion = 1; /// Version courante du format FLM

var
  UnitFileClasses : TStrings = nil; /// Type MIME -> Classe unité

{*
  Compare deux numéros de versions représentés textuellement
  @param Version1   Premier numéro de version
  @param Version2   Second numéro de version
  @return 0 si les versions sont égales, 1 si la première est supérieure à la
          seconde, et -1 dans le cas contraire
*}
function CompareVersions(Version1, Version2 : string) : integer;
var SubVer1, SubVer2 : string;
begin
  repeat
    SubVer1 := GetFirstToken(Version1, '.');
    Delete(Version1, 1, Length(SubVer1)+1);
    SubVer2 := GetFirstToken(Version2, '.');
    Delete(Version2, 1, Length(SubVer2)+1);
    Result := StrToIntDef(SubVer1, 0) - StrToIntDef(SubVer2, 0);
  until (Result <> 0) or ((Version1 = '') and (Version2 = ''));
end;

{*
  S'assure que la liste des classes d'unités est créée
*}
procedure EnsureClassListCreated;
begin
  if UnitFileClasses = nil then
    UnitFileClasses := TStringList.Create;
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
constructor TDependantFile.Create(AMasterFile : TMasterFile;
  const AHRef : string; const AFileName : TFileName);
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
constructor TUnitFile.Create(AMasterFile : TMasterFile; const AHRef : string;
  const AFileName : TFileName; const AMIMEType : string; Params : TStrings);
begin
  inherited Create(AMasterFile, AHRef, AFileName);
  FMIMEType := AMIMEType;
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
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);
begin
end;

{*
  Dresse la liste des paramètres à enregistrer
  Les descendants de TUnitFile peuvent surcharger cette méthode pour indiquer
  au fichier maître les paramètres qu'il doit enregistrer.
  @param Params   Liste des paramètres
*}
procedure TUnitFile.GetParams(Params : TStrings);
begin
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
constructor TMapFile.Create(AMasterFile : TMasterFile; const AHRef : string;
  const AFileName : TFileName; const AMapID : TComponentID);
var Stream : TStream;
    ZoneWidth, ZoneHeight, I, Count, Value, ScrewSize : integer;
    Dimensions : T3DPoint;
    Palette : array of TScrew;
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
    Stream.ReadBuffer(Dimensions, sizeof(T3DPoint));
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
    if Count <= 256 then ScrewSize := 1 else ScrewSize := 2;
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
constructor TMapFile.CreateNew(AMasterFile : TMasterFile;
  const AMapID : TComponentID; const ADimensions : T3DPoint;
  AZoneWidth, AZoneHeight : integer);
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
procedure TMapFile.Save(const AHRef : string = '';
  const AFileName : TFileName = '');
var I, Value, Count, PaletteCountPos, ScrewSize : integer;
    Stream : TStream;
    Dimensions : T3DPoint;
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
    Stream.WriteBuffer(Dimensions, sizeof(T3DPoint));
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
    for I := 0 to Map.LinearMapCount-1 do with Map.LinearMap[I] do
    begin
      if ClassType <> TScrew then
        raise EFileError.CreateFmt(sTemporaryStatedMap, [Map.ID]);
      if Tag < 0 then
      begin
        Tag := Count;
        WriteStrToStream(Stream, ID);
        inc(Count);
      end;
    end;

    // Écriture de la carte
    if Count <= 256 then ScrewSize := 1 else ScrewSize := 2;
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
constructor TMasterFile.Create(const AFileName : TFileName; AMode : TFileMode;
  CreatePlayerControllerProc : TCreatePlayerControllerProc = nil);
var Document : IXMLDOMDocument;
begin
  inherited Create;
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
  FMapFiles := TObjectList.Create;

  Document := CoDOMDocument.Create;
  Document.async := False;
  if not Document.load(FFileName) then
    InvalidFormat;

  Load(Document, CreatePlayerControllerProc);
  TestOpeningValidity;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param Dimensions     Dimensions de la carte
  @param FileContents   Contenu pré-créé du fichier (ou nil pour créer un vide)
  @throws EInvalidFileFormat : Le fichier ne respecte pas le format attendu
*}
constructor TMasterFile.CreateNew(FileContents : TStrings = nil);
var Document : IXMLDOMDocument;
begin
  inherited Create;
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
  FMapFiles := TObjectList.Create;

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

  { Dans la mesure où des unités pourraient être dépendantes d'autres, il faut
    absolument les libérer dans l'ordre inverse de chargement. }
  while FUnitFiles.Count > 0 do
    FUnitFiles.Delete(FUnitFiles.Count-1);
  FUnitFiles.Free;

  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TMasterFile.AfterConstruction;
var I : integer;
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
var I : integer;
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
procedure TMasterFile.Load(Document : IXMLDOMDocument;
  CreatePlayerControllerProc : TCreatePlayerControllerProc = nil);

  function NullToEmptyStr(Value : Variant) : Variant;
  begin
    if VarIsNull(Value) then Result := '' else Result := Value;
  end;

var Params : TStrings;
    I, J, MaxViewSize : integer;
    ID, FileType, HRef, Name, MapID : string;
    Position : T3DPoint;
    PlayerController : TPlayerController;
    Player : TPlayer;
    Node : IXMLDOMNode;
begin
  { Don't localize strings in this method }

  with Document.documentElement as IXMLDOMElement do
  begin
    if nodeName <> 'funlabyrinthe' then InvalidFormat;

    // Test de version
    FVersion := getAttribute('version');
    if FVersion = '' then InvalidFormat;
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
        for I := 0 to length-1 do with item[I] as IXMLDOMElement do
        begin
          FileType := getAttribute('type');
          HRef := getAttribute('href');

          Params.Clear;
          with selectNodes('./param') do
          begin
            for J := 0 to length-1 do with item[J] as IXMLDOMElement do
              Params.Values[getAttribute('name')] := getAttribute('value');
          end;

          AddUnitFile(FileType, HRef, Params);
        end;
      end;
    finally
      Params.Free;
    end;

    // Cartes
    with selectNodes('./maps/map') do
    begin
      for I := 0 to length-1 do with item[I] as IXMLDOMElement do
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

    // Joueurs
    with selectNodes('./players/player') do
    begin
      if length <> 1 then
        raise EFileError.Create(sThereMustBeOnePlayer);
      for I := 0 to length-1 do with item[I] as IXMLDOMElement do
      begin
        ID := getAttribute('id');
        if VarIsNull(getAttribute('name')) then Name := ID else
          Name := getAttribute('name');

        with selectSingleNode('./position') as IXMLDOMElement do
        begin
          MapID := getAttribute('map');
          Position.X := getAttribute('posx');
          Position.Y := getAttribute('posy');
          Position.Z := getAttribute('posz');
        end;

        if Assigned(CreatePlayerControllerProc) then
          PlayerController := CreatePlayerControllerProc(I)
        else
          PlayerController := nil;

        Player := TPlayer.Create(Master, ID, Name, Master.Map[MapID], Position,
          PlayerController);

        if NullToEmptyStr(getAttribute('lost')) = 'yes' then
          Player.Lose;

        with selectNodes('./attributes/attribute') do
        begin
          for J := 0 to length-1 do with item[J] as IXMLDOMElement do
            Player.Attribute[getAttribute('name')] := getAttribute('value');
        end;

        with selectNodes('./plugins/plugin') do
        begin
          for J := 0 to length-1 do with item[J] as IXMLDOMElement do
            Player.AddPlugin(Master.Plugin[getAttribute('id')]);
        end;

        { TODO 1 : Ajouter le support du dessin du joueur }
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
function TMasterFile.GetUnitFileCount : integer;
begin
  Result := FUnitFiles.Count;
end;

{*
  Tableau zero-based des fichiers unité
  @param Index   Index du fichier unité
  @return Le fichier unité dont l'index a été spécifié
*}
function TMasterFile.GetUnitFiles(Index : integer) : TUnitFile;
begin
  Result := TUnitFile(FUnitFiles[Index]);
end;

{*
  Nombre de fichiers carte
  @return Nombre de fichiers carte
*}
function TMasterFile.GetMapFileCount : integer;
begin
  Result := FMapFiles.Count;
end;

{*
  Tableau zero-based des fichiers carte
  @param Index   Index du fichier carte
  @return Le fichier carte dont l'index a été spécifié
*}
function TMasterFile.GetMapFiles(Index : integer) : TMapFile;
begin
  Result := TMapFile(FMapFiles[Index]);
end;

{*
  Enregistre un gestionnaire d'unité
  Si il existait déjà un gestionnaire pour le type MIME indiqué, celui-ci est
  remplacé.
  @param MIMEType       Type MIME géré par la classe
  @param MapFileClass   Classe gestionnaire du type MIME
*}
class procedure TMasterFile.RegisterUnitFileClass(const MIMEType : string;
  UnitFileClass : TUnitFileClass);
var Index : integer;
begin
  EnsureClassListCreated;
  Index := UnitFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    UnitFileClasses.AddObject(MIMEType, TObject(UnitFileClass))
  else
    UnitFileClasses.Objects[Index] := TObject(UnitFileClass);
end;

{*
  Supprime un gestionnaire d'unité
  UnregisterUnitFileClass vérifie d'abord que le gestionnaire que l'on veut
  effacer est bien celui affecté au type MIME indiqué.
  @param MIMEType       Type MIME géré par la classe
  @param MapFileClass   Classe gestionnaire du type MIME
*}
class procedure TMasterFile.UnregisterUnitFileClass(const MIMEType : string;
  UnitFileClass : TUnitFileClass);
var Index : integer;
begin
  if not Assigned(UnitFileClasses) then exit;
  Index := UnitFileClasses.IndexOf(MIMEType);
  if (Index >= 0) and
     (UnitFileClasses.Objects[Index] = TObject(UnitFileClass)) then
    UnitFileClasses.Delete(Index);
end;

{*
  Trouve la classe de fichier unité gérant le type MIME donné
  @param MIMEType   Type MIME dont il faut trouver le gestionnaire
  @return Classe de fichier unité gérant le type MIME spécifié
  @throws EFileError : Type d'unité inconnu
*}
class function TMasterFile.FindUnitFileClass(
  const MIMEType : string) : TUnitFileClass;
var Index : integer;
begin
  EnsureClassListCreated;
  Index := UnitFileClasses.IndexOf(MIMEType);
  if Index < 0 then
    raise EFileError.CreateFmt(sUnknownUnitType, [MIMEType])
  else
    Result := TUnitFileClass(UnitFileClasses.Objects[Index]);
end;

{*
  Résoud l'adresse HRef en cherchant dans les dossiers correspondants
  @param HRef         Adresse HRef du fichier
  @param DefaultDir   Dossier par défaut du type de fichier attendu
  @return Nom du fichier qualifié de son chemin d'accès
  @throws EFileError : Le fichier n'existe pas
*}
function TMasterFile.ResolveHRef(const HRef, DefaultDir : string) : TFileName;

  function TestAndReturn(const Path : string;
    out FileName : TFileName) : boolean;
  begin
    Result := FileExists(Path);
    if Result then
      FileName := Path;
  end;

  var FilePath, SubDir, SubFile : string;
begin
  FilePath := ExtractFilePath(FileName);
  SubDir := ChangeFileExt(ExtractFileName(FileName), '') + PathDelim;
  SubFile := StringReplace(HRef, HRefDelim, PathDelim, [rfReplaceAll]);

  if (not TestAndReturn(FilePath   + SubDir + SubFile, Result)) and
     (not TestAndReturn(DefaultDir + SubDir + SubFile, Result)) and
     (not TestAndReturn(FilePath   +          SubFile, Result)) and
     (not TestAndReturn(DefaultDir +          SubFile, Result)) and
     (not TestAndReturn(                      SubFile, Result)) then
    raise EFileError.CreateFmt(sFileNotFound, [HRef]);
end;

{*
  Ajoute un fichier unité
  @param MIMEType   Type MIME
  @param HRef       Adresse du fichier
  @param Params     Paramètres passés au fichier, ou nil pour aucun
  @return Le fichier unité créé et chargé
*}
function TMasterFile.AddUnitFile(const MIMEType, HRef : string;
  Params : TStrings = nil) : TUnitFile;
var OwnParams : boolean;
begin
  if Assigned(Params) then OwnParams := False else
  begin
    OwnParams := True;
    Params := TStringList.Create;
  end;
  try
    Result := FindUnitFileClass(MIMEType).Create(Self, HRef,
      ResolveHRef(HRef, fUnitsDir), MIMEType, Params);
  finally
    if OwnParams then Params.Free;
  end;
end;

{*
  Ajoute un fichier carte
  @param ID            ID de la carte
  @param HRef          Adresse du fichier
  @param MaxViewSize   Taille maximale d'une vue pour cette carte
  @return Le fichier carte créé et chargé
*}
function TMasterFile.AddMapFile(const ID : TComponentID; const HRef : string;
  MaxViewSize : integer = 1) : TMapFile;
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
function TMasterFile.AddNewMapFile(const ID : TComponentID;
  const Dimensions : T3DPoint; ZoneWidth, ZoneHeight : integer;
  MaxViewSize : integer = 1) : TMapFile;
begin
  Result := TMapFile.CreateNew(Self, ID, Dimensions, ZoneWidth, ZoneHeight);
  Result.Map.MaxViewSize := MaxViewSize;
end;

{*
  Commence la partie
*}
procedure TMasterFile.GameStarted;
var I : integer;
begin
  for I := 0 to UnitFileCount-1 do
    UnitFiles[I].GameStarted;
end;

{*
  Termine la partie
*}
procedure TMasterFile.GameEnded;
var I : integer;
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
  RegisterSingleComponentProc : TRegisterSingleComponentProc;
  RegisterComponentSetProc : TRegisterComponentSetProc);
var I : integer;
begin
  for I := 0 to UnitFileCount-1 do
  begin
    UnitFiles[I].RegisterComponents(
      RegisterSingleComponentProc, RegisterComponentSetProc);
  end;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier à enregistrer (si vide, conserve l'existant)
*}
procedure TMasterFile.Save(AFileName : TFileName = '');
var Document : IXMLDOMDocument;
    FunLabyrinthe, Units, Maps, Players, Player : IXMLDOMElement;
    Element, Param : IXMLDOMElement;
    Params : TStrings;
    MapHRef : string;
    MapFileName : TFileName;
    I, J : integer;
begin
  { Don't localize strings in this method }

  if (AFileName = '') and (Mode = fmPlay) and (not IsSaveguard) then
    raise EFileError.Create(sNoFileName);
  if AFileName = '' then
    AFileName := FFileName;

  if Mode = fmPlay then
  begin
    MapHRef := ExtractFileName(AFileName);
    I := LastDelimiter('.', MapHRef);
    if I > 0 then
    begin
      SetLength(MapHRef, I);
      MapHRef[I] := PathDelim;
    end else MapHRef := MapHRef + '-files' + PathDelim;

    MapFileName := ExtractFilePath(AFileName) + MapHRef;
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
      Element := Document.createElement('master');
      with Element do
      begin
        if Master.Temporization <> DefaultTemporization then
        begin
          Element := Document.createElement('temporization');
          Element.text := IntToStr(Master.Temporization);
          appendChild(Element);
        end;
      end;
      appendChild(Element);

      // Unités
      Units := Document.createElement('units');
      with Units do
      begin
        Params := TStringList.Create;
        try
          for I := 0 to UnitFileCount-1 do with UnitFiles[I] do
          begin
            Params.Clear;
            GetParams(Params);

            Element := Document.createElement('unit');
            Element.setAttribute('type', MIMEType);
            Element.setAttribute('href', HRef);

            with Element do for J := 0 to Params.Count-1 do
            begin
              Param := Document.createElement('param');
              Param.setAttribute('name', Params.Names[J]);
              Param.setAttribute('value', Params.ValueFromIndex[J]);
              appendChild(Param);
            end;

            appendChild(Element); // unit
          end;
        finally
          Params.Free;
        end;
      end;
      appendChild(Units);

      // Cartes
      Maps := Document.createElement('maps');
      with Maps do
      begin
        for I := 0 to MapFileCount-1 do with MapFiles[I] do
        begin
          if Mode <> fmPlay then Save else
            Save(MapHRef+MapID+'.flm', MapFileName+MapID+'.flm');

          Element := Document.createElement('map');
          Element.setAttribute('id', Map.ID);
          Element.setAttribute('href', HRef);
          if Map.MaxViewSize > 1 then
            Element.setAttribute('maxviewsize', Map.MaxViewSize);
          appendChild(Element);
        end;
      end;
      appendChild(Maps);

      // Joueurs
      Players := Document.createElement('players');
      with Players do
      begin
        Params := TStringList.Create;
        try
          for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
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
                  Param.setAttribute('value', integer(Params.Objects[J]));
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
        finally
          Params.Free;
        end;
      end;
      appendChild(Players);
    end;

    appendChild(FunLabyrinthe);
  end;

  Document.save(AFileName);
  FFileName := AFileName;
  if Mode = fmPlay then
    FIsSaveguard := True;
end;

initialization
  EnsureClassListCreated;
finalization
  UnitFileClasses.Free;
  UnitFileClasses := nil;
end.

