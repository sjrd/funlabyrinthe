{*
  Types et classes de bases de FunLabyrinthe
  FunLabyUtils comprend les types et classes de base de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, Controls, IniFiles, ScUtils,
  Forms, Dialogs, StdCtrls, Math, ScLists, ScStrUtils;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sDefaultScrewName = '%1:s sur %0:s';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';
  sComponentNotFound = 'Le composant d''ID %s n''existe pas';
  sEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  sCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';

const
  CurrentVersion = '5.0'; /// Version courante de FunLabyrinthe
  ScrewSize = 30;         /// Taille (en largeur et hauteur) d'une case
  DefaultZoneSize = 7;    /// Taille par défaut d'une zone (en cases)
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type représentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Pointeur vers T3DPoint
  P3DPoint = ^T3DPoint;

  /// Générée si un composant recherché n'est pas trouvé
  EComponentNotFound = class(Exception);

  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmEditActions, fmPlay);

  /// Générée si l'ouverture d'un fichier est invalide
  EInvalidFileOpening = class(Exception);

  TMaster = class;
  TPlayer = class;

  {*
    Gère le chargement des images d'après leur nom
    TImagesMaster s'occupe de charger automatiquement les images qu'on lui
    demande d'afficher. Il les conserve dans une liste d'image.
  *}
  TImagesMaster = class
  private
    FImgList : TImageList; /// Liste d'images interne
    FImgNames : TStrings;  /// Liste des noms des images
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const ImgName : string) : integer;
    procedure Draw(Index : integer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); overload;
    procedure Draw(const ImgName : string; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); overload;
  end;

  {*
    Enregistre est affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une méthode
    pour les dessiner les unes sur les autres, par transparence.
  *}
  TPainter = class
  private
    FMaster : TImagesMaster; /// Maître d'images
    FImgNames : TStrings;    /// Liste des noms des images
    FCachedImg : TBitmap;    /// Copie cache de l'image résultante

    procedure ImgNamesChange(Sender : TObject);
  public
    constructor Create(AMaster : TImagesMaster);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);

    property ImgNames : TStrings read FImgNames;
  end;

  {*
    Classe de base pour les composants de FunLabyrinthe
    TFunLabyComponent est la classe de base pour tous les composants de
    FunLabyrinthe. Elle fournit des propriétés et des méthodes pour repérer le
    maître FunLabyrinthe et pour identifier le composant.
  *}
  TFunLabyComponent = class
  private
    FMaster : TMaster;  /// Maître FunLabyrinthe
    FID : TComponentID; /// ID du composant
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID);

    property Master : TMaster read FMaster;
    property ID : TComponentID read FID;
  end;

  {*
    Classe de base pour les composants devant être affichés
    TVisualComponent étend la classe TFunLabyComponent pour lui ajouter un
    traitement standart et simple de nommage et de dessin.
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName : string;      /// Nom du composant
    FPainter : TPainter; /// Peintre par défaut
  protected
    property Painter : TPainter read FPainter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0); virtual;

    property Name : string read FName;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlayerPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs façons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Empêcher le déplacement du joueur et réagir à son déplacement effectif ;
    - Indiquer au joueur qu'il a la capacité de faire certaines actions.
  *}
  TPlayerPlugin = class(TFunLabyComponent)
  private
    FPainterBefore : TPainter; /// Peintre par défaut sous le joueur
    FPainterAfter : TPainter;  /// Peintre par défaut sur le joueur
  protected
    property PainterBefore : TPainter read FPainterBefore;
    property PainterAfter : TPainter read FPainterAfter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;
    procedure DrawAfter(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Dest : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); virtual;

    function CanYou(Player : TPlayer; Action : integer) : boolean; virtual;
  end;

  {*
    Classe de base pour les objets d'un joueur
    TPlayerObject est la classe de base pour les objets que possède le joueur.
    Les objets peuvent rendre un joueur capable d'effectuer certaines actions.
  *}
  TPlayerObject = class(TVisualComponent)
  private
    FPlayer : TPlayer; /// Joueur possédant l'objet
    FCount : integer;  /// Nombre d'objets de ce type que possède le joueur
  protected
    function GetShownInfos : string; virtual;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; APlayer : TPlayer);

    function CanYou(Action : integer) : boolean; virtual;
    procedure UseFor(Action : integer); virtual;

    property Player : TPlayer read FPlayer;
    property Count : integer read FCount write FCount;
    property ShownInfos : string read GetShownInfos;
  end;

  {*
    Classe de base pour les terrains
    TField est la classe de base pour la création de terrains. Les terrains
    sont la première composante d'une case.
  *}
  TField = class(TVisualComponent)
  private
    FDelegateDrawTo : TField; /// Terrain délégué pour l'affichage

    /// Réservation d'un emplacement dans la VMT pour stocker le Draw original
    procedure OriginalDraw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); virtual; abstract;
    procedure DerivedDraw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); virtual;
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Pos, Dest : T3DPoint;
      var Cancel : boolean); virtual;
  end;

  {*
    Classe de base pour les effets de case
    TEffect est la classe de base la création d'effets de case. Les effets
    sont la seconde composante d'une case.
  *}
  TEffect = class(TVisualComponent)
  public
    procedure Pushed(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint); virtual;
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); virtual;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); virtual;
  end;

  {*
    Classe de base pour les cases
    TScrew est la classe de base pour les cases de la carte. Elle possède une
    code et quatre méthodes qui définissent son comportement.
  *}
  TScrew = class(TVisualComponent)
  private
    FField : TField;   /// Terrain
    FEffect : TEffect; /// Effet
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AField : TField; AEffect : TEffect);

    property Field : TField read FField;
    property Effect : TEffect read FEffect;
  end;

  {*
    Représente la carte du jeu
    TMap gère et représente la carte du jeu. Elle offre des propriétés et
    méthodes pour lire et modifier cette carte.
  *}
  TMap = class(TFunLabyComponent)
  private
    FDimensions : T3DPoint;     /// Dimensions de la carte (en cases)
    FMap : array of TScrew;     /// Cases sur la carte
    FOutside : array of TScrew; /// Cases en-dehors de la carte

    function GetMap(Position : T3DPoint) : TScrew;
    procedure SetMap(Position : T3DPoint; Value : TScrew);
    function GetOutside(Floor : integer) : TScrew;
    procedure SetOutside(Floor : integer; Value : TScrew);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      ADimensions : T3DPoint);

    function InMap(Position : T3DPoint) : boolean;

    property Dimensions : T3DPoint read FDimensions;
    property Map[Position : T3DPoint] : TScrew
      read GetMap write SetMap; default;
    property Outside[Floor : integer] : TScrew
      read GetOutside write SetOutside;
  end;

  {*
    Classe représentant un joueur
    TPlayer représente un joueur. Elle possède de nombreuses propriétés et
    méthodes permettant d'afficher le joueur, de le déplacer, de lui greffer
    des plug-in, etc.
  *}
  TPlayer = class(TVisualComponent)
  private
    FMap : TMap;             /// Carte
    FPosition : T3DPoint;    /// Position
    FPosAddr : P3DPoint;     /// Position
    FDirection : TDirection; /// Direction
    /// Peintres selon la direction du joueur
    FDirPainters : array[diNorth..diWest] of TPainter;
    FColor : TColor;         /// Couleur
    FPlugins : TObjectList;  /// Liste des plug-in
    FObjects : TObjectList;  /// Liste des objets

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlayerPlugin;
    function GetObjectCount : integer;
    function GetObjects(Index : integer) : TPlayerObject;

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlayerPlugin read GetPlugins;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AMap : TMap; APosition : T3DPoint);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    procedure AddPlugin(Plugin : TPlayerPlugin);
    procedure RemovePlugin(Plugin : TPlayerPlugin);

    function ShowDialog(const Title, Text : string;
      DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
      DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
    function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
      DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
      const RadioTitles : array of string; var Selected : integer;
      OverButtons : boolean = False) : Word;

    function CanYou(Action : integer) : boolean;

    function Move(Dir : TDirection; KeyPressed : boolean;
      out Redo : boolean) : boolean;

    property Map : TMap read FMap;
    property Position : P3DPoint read FPosAddr;
    property Direction : TDirection read FDirection write FDirection;
    property Color : TColor read FColor write FColor;
    property ObjectCount : integer read GetObjectCount;
    property Objects[index : integer] : TPlayerObject read GetObjects;
  end;

  {*
    Maître FunLabyrinthe
    TMaster gère les différents composants de FunLabyrinthe.
  *}
  TMaster = class
  private
    FImagesMaster : TImagesMaster; /// Maître d'images
    FComponents : TStrings;        /// Table de hashage ID -> composant
    FPlugins : TObjectList;        /// Liste des plug-in
    FFields : TObjectList;         /// Liste des terrains
    FEffects : TObjectList;        /// Liste des effets
    FScrews : TObjectList;         /// Liste des cases
    FMaps : TObjectList;           /// Liste des cartes
    FPlayers : TObjectList;        /// Liste des joueurs

    function GetComponent(const ID : TComponentID) : TFunLabyComponent;
    function GetPlugin   (const ID : TComponentID) : TPlayerPlugin;
    function GetField    (const ID : TComponentID) : TField;
    function GetEffect   (const ID : TComponentID) : TEffect;
    function GetScrew    (const ID : TComponentID) : TScrew;
    function GetMap      (const ID : TComponentID) : TMap;
    function GetPlayer   (const ID : TComponentID) : TPlayer;

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlayerPlugin;
    function GetFieldCount : integer;
    function GetFields(Index : integer) : TField;
    function GetEffectCount : integer;
    function GetEffects(Index : integer) : TEffect;
    function GetScrewCount : integer;
    function GetScrews(Index : integer) : TScrew;
    function GetMapCount : integer;
    function GetMaps(Index : integer) : TMap;
    function GetPlayerCount : integer;
    function GetPlayers(Index : integer) : TPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddComponent(Component : TFunLabyComponent);

    property ImagesMaster : TImagesMaster read FImagesMaster;

    property Component[const ID : TComponentID] : TFunLabyComponent
      read GetComponent;
    property Plugin   [const ID : TComponentID] : TPlayerPlugin read GetPlugin;
    property Field    [const ID : TComponentID] : TField        read GetField;
    property Effect   [const ID : TComponentID] : TEffect       read GetEffect;
    property Screw    [const ID : TComponentID] : TScrew        read GetScrew;
    property Map      [const ID : TComponentID] : TMap          read GetMap;
    property Player   [const ID : TComponentID] : TPlayer       read GetPlayer;

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlayerPlugin read GetPlugins;
    property FieldCount : integer read GetFieldCount;
    property Fields[index : integer] : TField read GetFields;
    property EffectCount : integer read GetEffectCount;
    property Effects[index : integer] : TEffect read GetEffects;
    property ScrewCount : integer read GetScrewCount;
    property Screws[index : integer] : TScrew read GetScrews;
    property MapCount : integer read GetMapCount;
    property Maps[index : integer] : TMap read GetMaps;
    property PlayerCount : integer read GetPlayerCount;
    property Players[index : integer] : TPlayer read GetPlayers;
  end;

  {*
    Crée, ouvre, enregistre et gère les fichiers FunLabyrinthe
    TFunLabyFile se charge de créer, ouvrir, enregistrer et gérer les fichiers
    FunLabyrinthe (extension .lab). Elle est la seule de toutes les classes
    métier de FunLabyrinthe à « savoir » qu'il existe des fichiers.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
  *}
  TFunLabyFile = class
  private
    FFileName : TFileName;  /// Nom du fichier
    FMode : TFileMode;      /// Mode d'ouverture du fichier
    FName : string;         /// Nom du labyrinthe
    FDescription : string;  /// Description
    FVersion : string;      /// Version lors de l'enregistrement
    FAllowEdit : boolean;   /// Indique si le fichier peut être édité
    FIsSaveguard : boolean; /// Indique si le fichier était une sauvegarde

    FMaster : TMaster;      /// Maître FunLabyrinthe

    procedure NameFromFileName;
    procedure Load(FileContents : TStrings = nil);
    procedure TestOpeningValidity;
  public
    constructor Create(const AFileName : TFileName; AMode : TFileMode);
    constructor CreateNew(Dimensions : T3DPoint; FileContents : TStrings = nil);
    destructor Destroy; override;

    procedure Save(const AFileName : TFileName = '');

    property FileName : TFileName read FFileName;
    property Mode : TFileMode read FMode;
    property Name : string read FName write FName;
    property Description : string read FDescription write FDescription;
    property Version : string read FVersion;
    property AllowEdit : boolean read FAllowEdit;
    property IsSaveguard : boolean read FIsSaveguard;

    property Master : TMaster read FMaster;
  end;

const {don't localize}
  /// Fichier INI de FunLabyrinthe
  fIniFileName = 'FunLabyrinthe.ini';

var {don't localize}
  /// Dossier de FunLabyrinthe dans Application Data
  fFunLabyAppData : string = '';
  /// Dossier des fichiers image
  fScrewsDir : string = 'Screws\';
  /// Dossier des fichiers son
  fSoundsDir : string = 'Sounds\';
  /// Dossier des fichiers labyrinthe
  fLabyrinthsDir : string = 'Labyrinths\';
  /// Dossier des fichiers sauvegarde
  fSaveguardsDir : string = 'Saveguards\';

  /// Chaîne de format pour les fichiers image
  fScrewFileName : string = '%s.bmp';

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;

implementation

uses
  Screws, StrUtils;

const {don't localize}
  secDimensions = '[Dimensions]';
  keyColumns = 'Colonnes';
  keyRows = 'Lignes';
  keyFloors = 'Etages';

  secLabyrinth = '[Labyrinthe]';

{*
  Crée un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord supérieur du rectangle
  @return Le rectangle de type TRect
*}
function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X+ScrewSize;
  Result.Bottom := Y+ScrewSize;
end;

procedure GenerateDefaultFileContents(Dimensions : T3DPoint;
  FileContents : TStrings);
var X, Y, Z : integer;
    Str0, Str2, StrB : string;
begin
  FileContents.Add(secLabyrinth);

  // Création de la ligne d'herbe
  SetLength(Str0, Dimensions.X);
  Str0[1] := '2';
  Str0[Dimensions.X] := '2';
  for X := 2 to Dimensions.X-1 do Str0[X] := '0';

  // Création de la ligne de murs
  SetLength(Str2, Dimensions.X);
  for X := 1 to Dimensions.X do Str2[X] := '2';

  // Création de la ligne de dehors
  SetLength(StrB, Dimensions.X);
  for X := 1 to Dimensions.X do StrB[X] := 'B';

  for Z := 0 to Dimensions.Z-1 do
  begin
    // Génération de la bande de dehors fictive héritée de la v1.0
    if Z > 0 then
    begin
      for Y := 1 to 7 do
        FileContents.Add(StrB);
    end;

    // Première ligne de l'étage
    FileContents.Add(Str2);

    // Contenu de l'étage
    for Y := 2 to Dimensions.Y do
      FileContents.Add(Str0);

    // Dernière ligne de l'étage
    FileContents.Add(Str2);
  end;
end;

procedure LoadString(FileContents : TStrings;
  SectionBegin, SectionEnd : integer; const Key : string; var Value : string);
var I : integer;
begin
  I := StringsOps.FindAtPos(FileContents, Key+':', 1, SectionBegin, SectionEnd);
  if I < 0 then exit;
  Value := FileContents[I];
  Value := Trim(Copy(Value, Length(Key)+2, Length(Value)));
end;

procedure LoadInt(FileContents : TStrings; SectionBegin, SectionEnd : integer;
  const Key : string; var Value : integer);
var StrValue : string;
    ErrorCode : integer;
begin
  StrValue := '';
  LoadString(FileContents, SectionBegin, SectionEnd, Key, StrValue);
  Val(StrValue, Value, ErrorCode);
end;

////////////////////////////
/// Classe TImagesMaster ///
////////////////////////////

{*
  Crée une instance de TImagesMaster
*}
constructor TImagesMaster.Create;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(ScrewSize, ScrewSize);
  FImgNames := THashedStringList.Create;
end;

{*
  Détruit l'instance
*}
destructor TImagesMaster.Destroy;
begin
  FImgNames.Free;
  FImgList.Free;
  inherited;
end;

{*
  Renvoie l'index de l'image dont le nom est spécifié
  IndexOf renvoie l'index de l'image dont le nom est spécifié dans la
  liste d'images interne. Si l'image n'a pas encore été chargée, IndexOf
  la charge.
  @param ImgName   Nom de l'image
  @return Index de l'image
*}
function TImagesMaster.IndexOf(const ImgName : string) : integer;
var NewImg : TBitmap;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  begin
    NewImg := TBitmap.Create;
    try
      NewImg.LoadFromFile(Format(fScrewFileName, [ImgName]));
      FImgList.AddMasked(NewImg, clTransparent);
      Result := FImgNames.Add(ImgName);
    finally
      NewImg.Free;
    end;
  end;
end;

{*
  Dessine une image à partir de son index
  Draw dessine l'image indiquée sur un canevas.
  @param Index    Index de l'image à dessiner
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonnée X du point à partir duquel dessiner l'image
  @param Y        Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index : integer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FImgList.Draw(Canvas, X, Y, Index);
end;

{*
  Dessine une image à partir de son nom
  Draw dessine l'image indiquée sur un canevas.
  @param ImgName   Nom de l'image à dessiner
  @param Canvas    Canevas sur lequel dessiner l'image
  @param X         Coordonnée X du point à partir duquel dessiner l'image
  @param Y         Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(const ImgName : string; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  Draw(IndexOf(ImgName), Canvas, X, Y);
end;

///////////////////////
/// Classe TPainter ///
///////////////////////

{*
  Crée une instance de TPainter
  @param AMaster   Maître d'images associé au peintre
*}
constructor TPainter.Create(AMaster : TImagesMaster);
begin
  inherited Create;
  FMaster := AMaster;
  FImgNames := TStringList.Create;
  TStringList(FImgNames).OnChange := ImgNamesChange;
  FCachedImg := TBitmap.Create;
  with FCachedImg do
  begin
    Width := ScrewSize;
    Height := ScrewSize;
    Canvas.Brush.Color := clTransparent;
    Canvas.Pen.Style := psClear;
  end;
  ImgNamesChange(nil);
end;

{*
  Détruit l'instance
*}
destructor TPainter.Destroy;
begin
  FCachedImg.Free;
  FImgNames.Free;
  inherited;
end;

{*
  Événement OnChange de la liste des noms des images
  ImgNamesChange est appelé lorsque la liste des noms des images change.
  Elle actualise l'image cache.
  @param Sender   Objet lançant l'événement
*}
procedure TPainter.ImgNamesChange(Sender : TObject);
var I : integer;
begin
  FCachedImg.Canvas.Rectangle(0, 0, ScrewSize, ScrewSize);
  for I := 0 to FImgNames.Count-1 do
    FMaster.Draw(FImgNames[I], FCachedImg.Canvas);
end;

{*
  Dessine les images sur un canevas
  La méthode Draw dessine les images de ImgNames sur le canevas, à la
  position indiquée. Les différentes images sont superposée, celle d'index
  0 tout au-dessous.
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPainter.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.BrushCopy(ScrewRect(X, Y), FCachedImg, ScrewRect, clTransparent);
end;

////////////////////////////////
/// Classe TFunLabyComponent ///
////////////////////////////////

{*
  Crée une instance de TFunLabyComponent
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
*}
constructor TFunLabyComponent.Create(AMaster : TMaster;
  const AID : TComponentID);
begin
  inherited Create;
  FMaster := AMaster;
  FID := AID;
end;

///////////////////////////////
/// Classe TVisualComponent ///
///////////////////////////////

{*
  Crée une instance de TVisualComponent
*}
constructor TVisualComponent.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID);
  FName := AName;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.ImgNames.BeginUpdate;
end;

{*
  Détruit l'instance
*}
destructor TVisualComponent.Destroy;
begin
  FPainter.Free;
  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TVisualComponent.AfterConstruction;
begin
  inherited;
  FPainter.ImgNames.EndUpdate;
end;

{*
  Dessine le composant sur un canevas
  Draw dessine le composant sur un canevas à la position indiquée.
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TVisualComponent.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

////////////////////////////
/// Classe TPlayerPlugin ///
////////////////////////////

{*
  Crée une instance de TPlayerPlugin
  @param AMaster   Maître FunLabyrinthe
*}
constructor TPlayerPlugin.Create(AMaster : TMaster; const AID : TComponentID);
begin
  inherited Create(AMaster, AID);
  FPainterBefore := TPainter.Create(FMaster.ImagesMaster);
  FPainterBefore.ImgNames.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.ImgNames.BeginUpdate;
end;

{*
  Détruit l'instance
*}
destructor TPlayerPlugin.Destroy;
begin
  FPainterAfter.Free;
  FPainterBefore.Free;
  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TPlayerPlugin.AfterConstruction;
begin
  inherited;
  FPainterBefore.ImgNames.EndUpdate;
  FPainterAfter.ImgNames.EndUpdate;
end;

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessiné
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlayerPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterBefore.Draw(Canvas, X, Y);
end;

{*
  Dessine sur le joueur
  DrawAfter est exécuté lors du dessin du joueur, après celui-ci. Le dessin
  effectué dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessiné
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlayerPlugin.DrawAfter(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterAfter.Draw(Canvas, X, Y);
end;

{*
  Un joueur se déplace
  Moving est exécuté lorsqu'un joueur se déplace d'une case à une autre. Pour
  annuler le déplacement, Moving peut positionner le paramètre Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de départ
  @param Dest           Case d'arrivée
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TPlayerPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de départ
  @param Dest         Case d'arrivée
*}
procedure TPlayerPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
begin
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TPlayerPlugin.CanYou(Player : TPlayer; Action : integer) : boolean;
begin
  Result := False;
end;

////////////////////////////
/// Classe TPlayerObject ///
////////////////////////////

{*
  Crée une instance de TPlayerObject
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'objet
  @param AName     Nom de l'objet
  @param APlayer   Joueur propriétaire
*}
constructor TPlayerObject.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; APlayer : TPlayer);
begin
  inherited Create(AMaster, AID, AName);
  FPlayer := APlayer;
  FCount := 0;
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @return Informations textuelles, ou une chaîne vide si rien à afficher
*}
function TPlayerObject.GetShownInfos : string;
begin
  Result := Format(sDefaultObjectInfos, [Name, Count]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TPlayerObject.CanYou(Action : integer) : boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Action   Action à effectuer
*}
procedure TPlayerObject.UseFor(Action : integer);
begin
end;

/////////////////////
/// Classe TField ///
/////////////////////

{*
  Crée une instance de TField
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TField.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName);
  FDelegateDrawTo := ADelegateDrawTo;

  { On dérive tout appel à Draw vers DerivedDraw, en modifiant directement la
    VMT de la classe. On prend soin de conserver l'ancienne valeur, pour
    pouvoir tout de même l'appeler si FDelegateDrawTo vaut nil.
    Remarque : Comme toutes les instances d'une même classe partagent la même
    VMT, il faut éviter le cas où ce remplacement a déjà eu lieu, tout
    simplement en testant si Draw vaut déjà DerivedDraw.                       }
  {$IFNDEF DCTD} // évite le bug de DelphiCodeToDoc avec l'assembleur
  asm
    mov edx, [eax] // récupération de la VMT

    // test du cas où le remplacement a déjà été fait
    mov ecx, dword ptr TField.DerivedDraw
    cmp dword ptr [edx + VMTOFFSET TVisualComponent.Draw], ecx
    je  @@AlreadyDone

    // sauvegarde du pointeur vers Draw dans OriginalDraw
    mov ecx, dword ptr [edx + VMTOFFSET TField.Draw]
    mov dword ptr [edx + VMTOFFSET TField.OriginalDraw], ecx
    // remplacement par DerivedDraw
    mov ecx, dword ptr TField.DerivedDraw
    mov dword ptr [edx], ecx

    @@AlreadyDone :
  end;
  {$ENDIF}
end;

{*
  Dessine le terrain sur le canevas indiqué, ou délègue le dessin
  Grâce à la redirection mise en place dans le constructeur, tout appel à Draw
  se résoud en l'appel de DerivedDraw. On peut alors tester s'il faut déléguer
  l'affichage ou appeler la méthode Draw originale.
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonnée X du point à partir duquel dessiner le terrain
  @param Y        Coordonnée Y du point à partir duquel dessiner le terrain
*}
procedure TField.DerivedDraw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  if FDelegateDrawTo = nil then
    OriginalDraw(Canvas, X, Y)
  else
    FDelegateDrawTo.DerivedDraw(Canvas, X, Y);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TField.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
end;

{*
  Exécuté lorsque le joueur tente de sortir de la case
  Exiting est exécuté lorsque le joueur tente de sortir de la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Pos            Position de la case
  @param Dest           Case de destination
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TField.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

//////////////////////
/// Classe TEffect ///
//////////////////////

{*
  Exécuté lorsque le joueur a poussé sur la case
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
*}
procedure TEffect.Pushed(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint);
begin
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TEffect.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TEffect.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
end;

/////////////////////
/// Classe TScrew ///
/////////////////////

{*
  Crée une instance de TScrew
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de la case
  @param AName     Nom de la case
  @param AField    Terrain
  @param AEffect   Effet
*}
constructor TScrew.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AField : TField; AEffect : TEffect);
begin
  inherited Create(AMaster, AID, AName);
  FField := AField;
  FEffect := AEffect;
end;

///////////////////
/// Classe TMap ///
///////////////////

{*
  Crée une instance de TMap
  @param AMaster       Maître FunLabyrinthe
  @param ADimensions   Dimensions de la carte (en cases)
*}
constructor TMap.Create(AMaster : TMaster; const AID : TComponentID;
  ADimensions : T3DPoint);
var X, Y, Z : integer;
begin
  inherited Create(AMaster, AID);
  FDimensions := ADimensions;

  SetLength(FMap, FDimensions.X * FDimensions.Y * FDimensions.Z);
  for X := 0 to FDimensions.X-1 do
    for Y := 0 to FDimensions.Y-1 do
      for Z := 0 to FDimensions.Z-1 do
        FMap[Z*FDimensions.X*FDimensions.Y + Y*FDimensions.X + X] := nil;

  SetLength(FOutside, FDimensions.Z);
  for Z := 0 to FDimensions.Z-1 do
    FOutside[Z] := nil;
end;

{*
  Tableau des cases indexé par leur position sur la carte
  @param Position   Position sur la carte
  @return La case à la position spécifiée
*}
function TMap.GetMap(Position : T3DPoint) : TScrew;
var Index : integer;
begin
  if InMap(Position) then
  begin
    Index := Position.Z;
    Index := Index * FDimensions.Y;
    inc(Index, Position.Y);
    Index := Index * FDimensions.X;
    inc(Index, Position.X);

    Result := FMap[Index];
  end else Result := Outside[Position.Z];
end;

{*
  Modifie le tableau des cases indexé par leur position sur la carte
  @param Position   Position sur la carte
  @param Value      Nouvelle case
*}
procedure TMap.SetMap(Position : T3DPoint; Value : TScrew);
var Index : integer;
begin
  if not InMap(Position) then exit;

  Index := Position.Z;
  Index := Index * FDimensions.Y;
  inc(Index, Position.Y);
  Index := Index * FDimensions.X;
  inc(Index, Position.X);

  FMap[Index] := Value;
end;

{*
  Tableau des cases hors de la carte indexé par étage
  @param Floor   Étage
  @return La case hors de la carte à l'étage spécifié
*}
function TMap.GetOutside(Floor : integer) : TScrew;
begin
  if Floor < 0 then Floor := 0 else
  if Floor >= FDimensions.Z then Floor := FDimensions.Z-1;
  Result := FOutside[Floor];
end;

{*
  Modifie le tableau des cases hors de la carte indexé par étage
  @param Floor   Étage
  @param Value   Nouvelle case
*}
procedure TMap.SetOutside(Floor : integer; Value : TScrew);
begin
  if (Floor >= 0) and (Floor < FDimensions.Z) then
    FOutside[Floor] := Value;
end;

{*
  Teste si une coordonnée est à l'intérieur de la carte
  @param Position   Coordonnée à tester
  @return True si la coordonnée est dans la carte, False sinon
*}
function TMap.InMap(Position : T3DPoint) : boolean;
begin
  Result :=
    (Position.X >= 0) and (Position.X < FDimensions.X) and
    (Position.Y >= 0) and (Position.Y < FDimensions.Y) and
    (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

//////////////////////
/// Classe TPlayer ///
//////////////////////

{*
  Crée une instance de TPlayer
  @param AMaster     Maître FunLabyrinthe
  @param AID         ID du joueur
  @param AName       Nom du joueur
  @param AMap        Carte de départ
  @param APosition   Position de départ
*}
constructor TPlayer.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AMap : TMap; APosition : T3DPoint);
var Dir : TDirection;
begin
  inherited Create(AMaster, AID, AName);
  FMap := AMap;
  FPosition := APosition;
  FPosAddr := @FPosition;
  FDirection := diNone;
  for Dir in [diNorth..diWest] do
    FDirPainters[Dir] := nil;
  FColor := clBlue;
  FPlugins := TObjectList.Create(False);
  FObjects := TObjectList.Create;
end;

{*
  Détruit l'instance
*}
destructor TPlayer.Destroy;
var Dir : TDirection;
begin
  FObjects.Free;
  FPlugins.Free;
  for Dir in [diNorth..diWest] do if Assigned(FDirPainters[Dir]) then
    FDirPainters[Dir].Free;
  inherited;
end;

{*
  Nombre de plug-in greffés au joueur
  @return Nombre de plug-in
*}
function TPlayer.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in greffés au joueur
  @param Index   Index du plug-in dans le tableau
  @return Le plug-in à la position indiquée
*}
function TPlayer.GetPlugins(Index : integer) : TPlayerPlugin;
begin
  Result := TPlayerPlugin(FPlugins[Index]);
end;

{*
  Nombre de types d'objets du joueur
  @return Nombre de types d'objets
*}
function TPlayer.GetObjectCount : integer;
begin
  Result := FObjects.Count;
end;

{*
  Tableau zero-based des types d'objets du joueur
  @param Index   Index de l'objet dans le tableau
  @return L'objet à la position indiquée
*}
function TPlayer.GetObjects(Index : integer) : TPlayerObject;
begin
  Result := TPlayerObject(FObjects[Index]);
end;

{*
  Dessine le joueur sur un canevas
  Draw dessine le joueur sur un canevas à la position indiquée.
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonnée X du point à partir duquel dessiner le joueur
  @param Y        Coordonnée Y du point à partir duquel dessiner le joueur
*}
procedure TPlayer.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
var I : integer;
begin
  // Dessine les plug-in en-dessous du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawBefore(Self, Canvas, X, Y);

  // Dessine le peintre correspondant à la direction...
  if FColor = clDefault then
  begin
    if (FDirection = diNone) or (not Assigned(FDirPainters[FDirection])) then
      Painter.Draw(Canvas, X, Y)
    else
      FDirPainters[FDirection].Draw(Canvas, X, Y);
  end else
  // ... ou le traditionnel disque coloré
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+24, Y+24);
    end;
  end;

  // Dessine les plug-in au-dessus du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawAfter(Self, Canvas, X, Y);
end;

{*
  Greffe un plug-in au joueur
  @param Plugin   Le plug-in à greffer
*}
procedure TPlayer.AddPlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Add(Plugin);
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in à retirer
*}
procedure TPlayer.RemovePlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Remove(Plugin);
end;

{*
  Affiche une boîte de dialogue
  @param Title        Titre de la boîte de dialogue
  @param Text         Texte de la boîte de dialogue
  @param DlgType      Type de boîte de dialogue
  @param DlgButtons   Boutons présents dans la boîte de dialogue
  @param DefButton    Bouton sélectionné par défaut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Code de résultat du bouton cliqué
*}
function TPlayer.ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
begin
  // En prévision d'une exécution en thread du jeu (et client-serveur)
  Result := ScUtils.ShowDialog(Title, Text, DlgType, DlgButtons,
    DefButton, AddFlags);
end;

{*
  Affiche une boîte de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title   Titre de la boîte de dialogue
  @param Text    Texte de la boîte de dialogue
  @param DlgType   Type de boîte de dialogue
  @param DlgButtons   Boutons présents dans la boîte de dialogue
  @param DefButton    Bouton sélectionné par défaut
  @param RadioTitles   Libellés des différents boutons radio
  @param Selected      Bouton radio sélectionné
  @param OverButtons   Boutons radio placés au-dessus des boutons si True
  @return Code de résultat du bouton cliqué
*}
function TPlayer.ShowDialogRadio(const Title, Text : string;
  DlgType : TMsgDlgType; DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Création de la boite de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la boite de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Création des boutons radio et détermination de la largeur minimale
    MaxWidth := 0;
    for I := High(RadioTitles) downto Low(RadioTitles) do
    with TRadioButton.Create(Form) do
    begin
      FreeNotification(Form);
      Parent := Form;
      Width := Canvas.TextWidth(RadioTitles[I]) + 20;
      MaxWidth := Max(MaxWidth, Width-20);
      Caption := RadioTitles[I];
      Checked := I = Selected;
      Tag := I;
      Left := 8;

      // OverButtons indique si les RadioBox sont au-dessus ou en-dessous des
      // boutons
      if OverButtons then
        Top := Form.Height - 90 - (High(RadioTitles) - I) * 25
      else
        Top := Form.Height - 50 - (High(RadioTitles) - I) * 25;
    end;

    // Il faut aussi vérifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On récupère chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par défaut et on le sélectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, décaler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, décaler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la boite de dialogue
    Position := poScreenCenter;

    // Affichage de la boîte de dialogue
    Result := ShowModal;

    // Récupération du choix de l'utilisateur
    Selected := -1;
    for I := 0 to ControlCount-1 do
      if (Controls[I] is TRadioButton) and TRadioButton(Controls[I]).Checked then
        Selected := Controls[I].Tag;
  finally
    Free;
  end;
end;

{*
  Indique si le joueur est capable d'effectuer une action donnée
  CanYou commence par tester si un plug-in permet l'action. Sinon, il
  détermine quels sont les objets permettant cette action. S'il y en a
  plusieurs, le joueur se voit demander d'en choisir un, et celui-ci est
  utilisé.
  @param Action   Action à tester
  @return True si le joueur est capabled d'effectuer l'action, False sinon
*}
function TPlayer.CanYou(Action : integer) : boolean;
var I, GoodObjectCount : integer;
    GoodObjects : array of TPlayerObject;
    RadioTitles : array of string;
    GoodObject : TPlayerObject;
begin
  Result := True;

  // Les plug-in ont la priorité, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do if Plugins[I].CanYou(Self, Action) then exit;

  // Listage des objets susceptibles d'aider le joueur
  SetLength(GoodObjects, ObjectCount);
  GoodObjectCount := 0;
  for I := 0 to ObjectCount-1 do if Objects[I].CanYou(Action) then
  begin
    GoodObjects[GoodObjectCount] := Objects[I];
    inc(GoodObjectCount);
  end;

  // Aucun objet trouvé : échec
  if GoodObjectCount = 0 then
  begin
    Result := False;
    exit;
  end;

  // Si plusieurs objets, demande au joueur lequel utiliser
  if GoodObjectCount = 1 then GoodObject := GoodObjects[0] else
  begin
    SetLength(RadioTitles, GoodObjectCount);
    for I := 0 to GoodObjectCount-1 do
      RadioTitles[I] := GoodObjects[I].Name;
    I := 0;
    ShowDialogRadio(sWhichObject, sWhichObject, mtConfirmation, [mbOK], mrOK,
      RadioTitles, I, True);
    GoodObject := GoodObjects[I];
  end;

  // Utilisation de l'objet
  GoodObject.UseFor(Action);
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir   Direction du déplacement
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Redo         Indique s'il faut réitérer le déplacement
  @return True si le déplacement a réussi, False sinon
*}
function TPlayer.Move(Dir : TDirection; KeyPressed : boolean;
  out Redo : boolean) : boolean;
var I : integer;
    Src, Dest : T3DPoint;
    OldDir : TDirection;
    Cancel, AbortEntered : boolean;
begin
  // Initialisation des variables
  Result := False;
  Redo := False;
  Src := FPosition;
  Dest := FPosition;
  case Dir of
    diNorth : dec(Dest.Y);
    diEast  : inc(Dest.X);
    diSouth : inc(Dest.Y);
    diWest  : dec(Dest.X);
    else exit;
  end;
  OldDir := FDirection;
  FDirection := Dir;
  Cancel := False;
  AbortEntered := False;

  // Premier passage : le déplacement est-il permis ?
  try
    // Case source : exiting
    Map[Src].Field.Exiting(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Plug-in : moving
    for I := 0 to PluginCount-1 do
      Plugins[I].Moving(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Case destination : entering
    Map[Dest].Field.Entering(Self, OldDir, KeyPressed, Src, Dest, Cancel,
      AbortEntered);
    if Cancel then exit;
  finally
    // Case destination : pushed (seulement si on a annulé)
    if Cancel then
      Map[Dest].Effect.Pushed(Self, KeyPressed, Src, Dest);
  end;

  // Déplacement du joueur (à moins qu'il ait été déplacé par ailleurs)
  if Same3DPoint(FPosition, Src) then
    FPosition := Dest
  else
    Dest := FPosition;

  // Second passage : le déplacement a été fait
  begin
    // Case source : exited
    Map[Src].Effect.Exited(Self, KeyPressed, Src, Dest);
    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, KeyPressed, Src, Dest);
    // Case destination : entered (sauf si AbortEntered a été positionné à True)
    if not AbortEntered then
      Map[Dest].Effect.Entered(Self, KeyPressed, Src, Dest, Redo);
  end;
end;

//////////////////////
/// Classe TMaster ///
//////////////////////

{*
  Crée une instance de TMaster
*}
constructor TMaster.Create;
begin
  inherited Create;
  FImagesMaster := TImagesMaster.Create;
  FComponents := THashedStringList.Create;
  with TStringList(FComponents) do
  begin
    CaseSensitive := True;
    Duplicates := dupError;
  end;

  FPlugins := TObjectList.Create;
  FFields  := TObjectList.Create;
  FEffects := TObjectList.Create;
  FScrews  := TObjectList.Create;
  FMaps    := TObjectList.Create;
  FPlayers := TObjectList.Create;
end;

{*
  Détruit l'instance
*}
destructor TMaster.Destroy;
begin
  FPlayers.Free;
  FMaps.Free;
  FScrews.Free;
  FEffects.Free;
  FFields.Free;
  FPlugins.Free;

  FComponents.Free;
  FImagesMaster.Free;
  inherited;
end;

{*
  Tableau des composants indexé par leur ID
  @param ID   ID du composant à trouver
  @return Le composant dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun composant ne correspond à l'ID spécifié
*}
function TMaster.GetComponent(const ID : TComponentID) : TFunLabyComponent;
var Index : integer;
begin
  Index := FComponents.IndexOf(ID);
  if Index >= 0 then
    Result := TFunLabyComponent(FComponents.Objects[Index])
  else
    raise EComponentNotFound.CreateFmt(sComponentNotFound, [ID]);
end;

{*
  Tableau des plug-in indexé par leur ID
  @param ID   ID du plug-in à trouver
  @return Le plug-in dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun plug-in ne correspond à l'ID spécifié
*}
function TMaster.GetPlugin(const ID : TComponentID) : TPlayerPlugin;
begin
  Result := Component[ID] as TPlayerPlugin;
end;

{*
  Tableau des terrains indexé par leur ID
  @param ID   ID du terrain à trouver
  @return Le terrain dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun terrain ne correspond à l'ID spécifié
*}
function TMaster.GetField(const ID : TComponentID) : TField;
begin
  Result := Component[ID] as TField;
end;

{*
  Tableau des effets de case indexé par leur ID
  @param ID   ID de l'effet à trouver
  @return L'effet dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun effet ne correspond à l'ID spécifié
*}
function TMaster.GetEffect(const ID : TComponentID) : TEffect;
begin
  Result := Component[ID] as TEffect;
end;

{*
  Tableau des cases indexé par leur ID
  Si la case n'a pas pu être trouvée, GetScrew essaye de trouver les terrain
  et effet correspondant à son ID et de créer la case automatiquement
  @param ID   ID de la case à trouver
  @return La case dont l'ID a été spécifié
  @throws EComponentNotFound : Aucune case ne correspond à l'ID spécifié
*}
function TMaster.GetScrew(const ID : TComponentID) : TScrew;
var AField : TField;
    AEffect : TEffect;
begin
  try
    Result := Component[ID] as TScrew;
  except
    on Error : EComponentNotFound do
    begin
      Result := nil;
      try
        AField := Field[GetFirstToken(ID, '-')];
        AEffect := Effect[GetLastToken(ID, '-')];
        Result := TScrew.Create(Self, ID,
          Format(sDefaultScrewName, [AField.Name, AEffect.Name]),
          AField, AEffect);
      except
      end;
      if Result = nil then raise;
    end;
  end;
end;

{*
  Tableau des cartes indexé par leur ID
  @param ID   ID de la carte à trouver
  @return La carte dont l'ID a été spécifié
  @throws EComponentNotFound : Aucune carte ne correspond à l'ID spécifié
*}
function TMaster.GetMap(const ID : TComponentID) : TMap;
begin
  Result := Component[ID] as TMap;
end;

{*
  Tableau des joueurs indexé par leur ID
  @param ID   ID du joueur à trouver
  @return Le joueur dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun joueur ne correspond à l'ID spécifié
*}
function TMaster.GetPlayer(const ID : TComponentID) : TPlayer;
begin
  Result := Component[ID] as TPlayer;
end;

{*
  Nombre de plug-in
  @return Nombre de plug-in
*}
function TMaster.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in
  @param Index   Index du plug-in
  @return Le plug-in à la position spécifiée
*}
function TMaster.GetPlugins(Index : integer) : TPlayerPlugin;
begin
  Result := TPlayerPlugin(FPlugins[Index]);
end;

{*
  Nombre de terrains
  @return Nombre de terrains
*}
function TMaster.GetFieldCount : integer;
begin
  Result := FFields.Count;
end;

{*
  Tableau zero-based des terrains
  @param Index   Index du terrain
  @return Le terrain à la position spécifiée
*}
function TMaster.GetFields(Index : integer) : TField;
begin
  Result := TField(FFields[Index]);
end;

{*
  Nombre d'effets
  @return Nombre d'effets
*}
function TMaster.GetEffectCount : integer;
begin
  Result := FEffects.Count;
end;

{*
  Tableau zero-based des effets
  @param Index   Index de l'effet
  @return L'effet à la position spécifiée
*}
function TMaster.GetEffects(Index : integer) : TEffect;
begin
  Result := TEffect(FEffects[Index]);
end;

{*
  Nombre de cases
  @return Nombre de cases
*}
function TMaster.GetScrewCount : integer;
begin
  Result := FScrews.Count;
end;

{*
  Tableau zero-based des cases
  @param Index   Index de la case
  @return La case à la position spécifiée
*}
function TMaster.GetScrews(Index : integer) : TScrew;
begin
  Result := TScrew(FScrews[Index]);
end;

{*
  Nombre de cartes
  @return Nombre de cartes
*}
function TMaster.GetMapCount : integer;
begin
  Result := FMaps.Count;
end;

{*
  Tableau zero-based des cartes
  @param Index   Index de la carte
  @return La carte à la position spécifiée
*}
function TMaster.GetMaps(Index : integer) : TMap;
begin
  Result := TMap(FMaps[Index]);
end;

{*
  Nombre de joueurs
  @return Nombre de joueurs
*}
function TMaster.GetPlayerCount : integer;
begin
  Result := FPlayers.Count;
end;

{*
  Tableau zero-based des joueurs
  @param Index   Index du joueur
  @return Le joueur à la position spécifiée
*}
function TMaster.GetPlayers(Index : integer) : TPlayer;
begin
  Result := TPlayer(FPlayers[Index]);
end;

{*
  Ajoute un composant
  @param Component   Le composant à ajouter
*}
procedure TMaster.AddComponent(Component : TFunLabyComponent);
begin
  FComponents.AddObject(Component.ID, Component);
  if Component is TPlayerPlugin then FPlugins.Add(Component) else
  if Component is TField        then FFields .Add(Component) else
  if Component is TEffect       then FEffects.Add(Component) else
  if Component is TScrew        then FScrews .Add(Component) else
  if Component is TMap          then FMaps   .Add(Component) else
  if Component is TPlayer       then FPlayers.Add(Component) else
  assert(False);
end;

///////////////////////////
/// Classe TFunLabyFile ///
///////////////////////////

{*
  Ouvre un fichier FunLabyrinthe
  @param AFileName   Nom du fichier à ouvrir
  @param AMode       Mode sous lequel ouvrir le fichier
*}
constructor TFunLabyFile.Create(const AFileName : TFileName; AMode : TFileMode);
begin
  inherited Create;
  FFileName := AFileName;
  FMode := AMode;
  NameFromFileName;
  FDescription := '';
  FVersion := CurrentVersion;
  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create;
  try
    Load;
    TestOpeningValidity;
  except
    FMaster.Free;
    raise;
  end;
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param Dimensions     Dimensions de la carte
  @param FileContents   Contenu pré-créé du fichier (ou nil si par défaut)
*}
constructor TFunLabyFile.CreateNew(Dimensions : T3DPoint;
  FileContents : TStrings = nil);
begin
  inherited Create;
  FFileName := '';
  FMode := fmEdit;
  FName := '';
  FDescription := '';
  FVersion := CurrentVersion;
  FAllowEdit := True;
  FIsSaveguard := False;

  FMaster := TMaster.Create;
  try
    Load(FileContents);
    TestOpeningValidity;
  except
    FMaster.Free;
    raise;
  end;
end;

{*
  Détruit l'instance
*}
destructor TFunLabyFile.Destroy;
begin
  FMaster.Free;
  inherited;
end;

{*
  Donne au labyrinthe un nom par défaut à partir du nom du fichier
*}
procedure TFunLabyFile.NameFromFileName;
var I : integer;
begin
  FName := ExtractFileName(FFileName);
  I := Length(FName);
  while (I > 0) and (FName[I] <> '.') do dec(I);
  if I > 0 then
    SetLength(FName, I);
end;

{*
  Charge le contenu du fichier dans les classes
  @param FileContents   Contenu du fichier
*}
procedure TFunLabyFile.Load(FileContents : TStrings = nil);
begin
  { TODO 2 : Charger le fichier }
end;

{*
  Teste la validité de l'ouverture d'un fichier
  TestOpeningValidity vérifie que le fichier ouvert ne l'a pas été
  « illégalement ». Deux cas d'illégalité sont à tester :
  - Le fichier est une sauvegarde et est ouvert autrement que pour y jouer ;
  - Le fichier a été interdit d'édition, et ouvert dans ce mode.
  @throws EInvalidFileOpening : Le fichier a été ouvert illégalement
*}
procedure TFunLabyFile.TestOpeningValidity;
begin
  if IsSaveguard and (Mode <> fmPlay) then
    raise EInvalidFileOpening.Create(sCantEditSaveguard);
  if (not AllowEdit) and (Mode = fmEdit) then
    raise EInvalidFileOpening.Create(sEditingNotAllowed);
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier à enregistrer (si vide, conserve l'existant)
*}
procedure TFunLabyFile.Save(const AFileName : TFileName = '');
begin
  { TODO 2 : Enregistrer le fichier }
end;

initialization
  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    fFunLabyAppData := ReadString('Directories', 'AppData', Dir); {don't localize}

    fScrewsDir := fFunLabyAppData + fScrewsDir;
    fSoundsDir := fFunLabyAppData + fSoundsDir;
    fLabyrinthsDir := fFunLabyAppData + fLabyrinthsDir;
    fSaveguardsDir := fFunLabyAppData + fSaveguardsDir;

    fScrewFileName := fScrewsDir+fScrewFileName;
  finally
    Free;
  end;
end.

