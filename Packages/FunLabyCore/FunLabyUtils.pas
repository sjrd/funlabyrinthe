{*
  Types et classes de bases de FunLabyrinthe
  FunLabyUtils comprend les types et classes de base de FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FunLabyUtils;

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Contnrs, Controls, Dialogs,
  TypInfo, ScUtils, ScCoroutines, SdDialogs;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sNothing = 'Rien';
  sEffectName = '%1:s sur %0:s';
  sToolName = '%s avec %s';
  sObstacleName = '%s obstrué par %s';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';
  sComponentNotFound = 'Le composant d''ID %s n''existe pas';
  sUnsupportedCommand = 'La commande %s n''est pas supportée';
  sCantLaunchThroughNetwork = 'FunLabyrinthe doit être installé sur '+
    'l''ordinateur local pour fonctionner';

  sDescription = 'Description';
  sMessage = 'Message';
  sTip = 'Indice';
  sChoice = 'Choix';
  sError = 'Erreur';
  sFailure = 'Échec';
  sBlindAlley = 'Impasse';
  sWon = 'Gagné !';
  sLost = 'Perdu !';

const {don't localize}
  CurrentVersion = '5.0'; /// Version courante de FunLabyrinthe
  FunLabyAuthorName = 'Sébastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://sjrd.developpez.com/programmes/funlaby/'; /// Site

  SquareSize = 30;        /// Taille (en largeur et hauteur) d'une case
  MinViewSize = 1;        /// Taille minimale d'une vue
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp
  NoRefCount = MaxInt;    /// Valeur sentinelle : pas de comptage des références

  attrColor = 'Color';             /// Attribut de joueur pour Color
  attrShowCounter = 'ShowCounter'; /// Attribut de joueur pour ShowCounter

  /// Attribut de joueur pour ViewBorderSize
  attrViewBorderSize = 'ViewBorderSize';

  msgShowMessage = $01; /// Message pour afficher un message au joueur

  CommandShowDialog = 'ShowDialog';           /// Commande ShowDialog
  CommandShowDialogRadio = 'ShowDialogRadio'; /// Commande ShowDialogRadio
  CommandChooseNumber = 'ChooseNumber';       /// Commande ChooseNumber

  SquareIDDelim = '-';            /// Délimiteur des parties d'un ID de case
  SquareIDFormat = '%s-%s-%s-%s'; /// Format d'un ID de case

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type représentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Type représentant une action
  TPlayerAction = type string;

  /// État de victoire/défaite d'un joueur
  TPlayState = (psPlaying, psWon, psLost);

  /// Classe de base des exceptions FunLabyrinthe
  EFunLabyException = class(Exception);

  /// Générée si un composant recherché n'est pas trouvé
  EComponentNotFound = class(EFunLabyException);

  /// Générée si une commande n'est pas supportée
  EUnsupportedCommand = class(EFunLabyException);

  /// Générée en cas de mauvaise définition d'une case
  EBadSquareDefException = class(EFunLabyException);

  TFunLabyComponent = class;
  TSquareComponent = class;
  TSquare = class;
  TMap = class;
  IPlayerMode = interface;
  TPlayerMode = class;
  TPlayer = class;
  TMaster = class;

  {*
    Position qualifiée, composée d'une carte et d'une position sur la carte
    @author sjrd
    @version 5.0
  *}
  TQualifiedPos = record
    Map: TMap;          /// Carte, ou nil pour une position nulle
    Position: T3DPoint; /// Position sur la carte, si Map <> nil
  end;

  {*
    Base pour les messages envoyés à des joueurs
    @author sjrd
    @version 5.0
  *}
  TPlayerMessage = record
    MsgID: Word;       /// ID du message
    Handled: Boolean;  /// Indique si le message a été géré
    Reserved: Byte;    /// Réservé
    Player: TPlayer;   /// Joueur concerné
  end;

  {*
    Structure du message pour afficher un message au joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerShowMsgMessage = record
    MsgID: Word;              /// ID du message
    Handled: Boolean;         /// Indique si le message a été géré
    Reserved: Byte;           /// Réservé
    Player: TPlayer;          /// Joueur concerné
    Text: string;             /// Texte à afficher
    Answers: TStringDynArray; /// Réponses possibles (peut être vide)
    Selected: Integer;        /// Index de la réponse choisie par le joueur
  end;

  {*
    Type des gestionnaires d'événements OnSendCommand de TPlayer
    @param Sender    Joueur concerné
    @param Command   Commande à effectuer
    @param Params    Paramètres de la commande
    @return Résultat de la commande
    @throws EUnsupportedCommand : La commande demandée n'est pas supportée
  *}
  TSendCommandEvent = function(Sender: TPlayer;
    const Command: string; const Params: string = ''): string of object;

  {*
    Type de méthode call-back pour l'enregistrement d'un unique composant
    @param Component   Le composant à enregistrer
  *}
  TRegisterSingleComponentProc = procedure(
    Component: TSquareComponent) of object;

  {*
    Type de méthode call-back pour l'enregistrement d'un ensemble de composants
    @param Template       Composant modèle pour l'image et le nom à afficher
    @param Components     Liste des composants faisant partie de l'ensemble
    @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
    @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
  *}
  TRegisterComponentSetProc = procedure(Template: TSquareComponent;
    const Components: array of TSquareComponent; BaseIndex: Integer;
    const DialogTitle, DialogPrompt: string) of object;

  {*
    Contexte de dessin d'une case
    @author sjrd
    @version 5.0
  *}
  TDrawSquareContext = class(TObject)
  private
    FCanvas: TCanvas;   /// Canevas cible
    FX: Integer;        /// Absisce où dessiner
    FY: Integer;        /// Ordonnée où dessiner
    FSquareRect: TRect; /// Rectangle de la case à dessiner

    FIsNowhere: Boolean;  /// Indique si le dessin est fait "nulle part"
    FQPos: TQualifiedPos; /// Position dessinée

    FTickCount: Cardinal; /// Tick count pour ce contexte

    FPlayer: TPlayer; /// Joueur à dessiner (si applicable)

    procedure SetPlayer(APlayer: TPlayer);
  public
    constructor Create(ACanvas: TCanvas); overload;
    constructor Create(ACanvas: TCanvas; X, Y: Integer); overload;
    constructor Create(ACanvas: TCanvas; X, Y: Integer;
      const AQPos: TQualifiedPos); overload;

    procedure SetTickCount(ATickCount: Cardinal);

    property Canvas: TCanvas read FCanvas;
    property X: Integer read FX;
    property Y: Integer read FY;
    property SquareRect: TRect read FSquareRect;

    property IsNowhere: Boolean read FIsNowhere;
    property QPos: TQualifiedPos read FQPos;
    property Map: TMap read FQPos.Map;
    property Pos: T3DPoint read FQPos.Position;

    property TickCount: Cardinal read FTickCount;

    property Player: TPlayer read FPlayer;
  end;

  {*
    Contexte de dessin d'une vue d'un joueur
    @author sjrd
    @version 5.0
  *}
  TDrawViewContext = class(TObject)
  private
    FPlayerMode: IPlayerMode; /// Mode principal du joueur
    FPlayer: TPlayer;         /// Joueur dont la vue est affichée

    FCanvas: TCanvas; /// Canevas cible
    FViewRect: TRect; /// Rectangle de la vue à dessiner

    FUseZone: Boolean; /// Indique si ce contexte utilise une zone de carte
    FMap: TMap;        /// Carte dont dessiner une zone
    FFloor: Integer;   /// Étage de la zone à dessiner
    FZone: TRect;      /// Zone (étendue aux bordures) à dessiner
    FZoneSize: TPoint; /// Taille de la zone

    FTickCount: Cardinal; /// Tick count pour l'affichage

    procedure ComputeZone;
  public
    constructor Create(const APlayerMode: IPlayerMode; ACanvas: TCanvas);

    function IsSquareVisible(const QPos: TQualifiedPos): Boolean; overload;
    function IsSquareVisible(Map: TMap;
      const Position: T3DPoint): Boolean; overload;
    function IsSquareVisible(const Position: T3DPoint): Boolean; overload;

    property PlayerMode: IPlayerMode read FPlayerMode;
    property Player: TPlayer read FPlayer;

    property Canvas: TCanvas read FCanvas;
    property ViewRect: TRect read FViewRect;

    property UseZone: Boolean read FUseZone;
    property Map: TMap read FMap;
    property Floor: Integer read FFloor;
    property Zone: TRect read FZone;
    property ZoneSize: TPoint read FZoneSize;
    property ZoneWidth: Integer read FZoneSize.X;
    property ZoneHeight: Integer read FZoneSize.Y;

    property TickCount: Cardinal read FTickCount;
  end;

  {*
    Contexte d'événement de touche
    @author sjrd
    @version 5.0
  *}
  TKeyEventContext = class(TObject)
  private
    FKey: Word;          /// Touche pressée
    FShift: TShiftState; /// État des touches spéciales

    FHandled: Boolean; /// Indique si l'événement a été géré
  public
    constructor Create(AKey: Word; AShift: TShiftState);

    property Key: Word read FKey;
    property Shift: TShiftState read FShift;

    property Handled: Boolean read FHandled write FHandled;
  end;

  {*
    Gère le chargement des images d'après leur nom
    TImagesMaster s'occupe de charger automatiquement les images qu'on lui
    demande d'afficher. Il les conserve dans une liste d'image.
    @author sjrd
    @version 5.0
  *}
  TImagesMaster = class
  private
    FImgList: TImageList; /// Liste d'images interne
    FImgNames: TStrings;  /// Liste des noms des images
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const ImgName: string; Bitmap: TBitmap): Integer;

    function IndexOf(const ImgName: string): Integer;

    procedure Draw(Index: Integer; Context: TDrawSquareContext); overload;
    procedure Draw(const ImgName: string;
      Context: TDrawSquareContext); overload;

    procedure Draw(Index: Integer; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); overload;
    procedure Draw(const ImgName: string; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); overload;
  end;

  {*
    Bitmap de la taille d'une case, gérant automatiquement la transparence
    @author sjrd
    @version 5.0
  *}
  TSquareBitmap = class(TBitmap)
  public
    constructor Create; override;

    procedure EmptySquare;
    procedure DrawSquare(Context: TDrawSquareContext);
  end;

  {*
    Enregistre et affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une méthode
    pour les dessiner les unes sur les autres, par transparence.
    @author sjrd
    @version 5.0
  *}
  TPainter = class
  private
    FMaster: TImagesMaster;    /// Maître d'images
    FImgNames: TStrings;       /// Liste des noms des images
    FCachedImg: TSquareBitmap; /// Copie cache de l'image résultante

    procedure ImgNamesChange(Sender: TObject);
  public
    constructor Create(AMaster: TImagesMaster);
    destructor Destroy; override;

    procedure Draw(Context: TDrawSquareContext);

    property ImgNames: TStrings read FImgNames;
  end;

  {*
    Contexte d'un mouvement du joueur
    @author sjrd
    @version 5.0
  *}
  TMoveContext = class(TObject)
  private
    FPlayer: TPlayer; /// Joueur qui se déplace

    FSrcMap: TMap;   /// Carte source
    FSrc: T3DPoint;  /// Case source
    FDestMap: TMap;  /// Carte destination
    FDest: T3DPoint; /// Case destination
    FMap: TMap;      /// Carte courante
    FPos: T3DPoint;  /// Position courante

    FOldDirection: TDirection; /// Ancienne direction du joueur
    FKeyPressed: Boolean;      /// True si une touche a été pressée

    FCancelled: Boolean;  /// True si le déplacement a été annulé
    FGoOnMoving: Boolean; /// True s'il faut réitérer le déplacement

    procedure SwitchToSrc;
    procedure SwitchToDest;

    function GetSrcSquare: TSquare;
    procedure SetSrcSquare(Value: TSquare);
    function GetDestSquare: TSquare;
    procedure SetDestSquare(Value: TSquare);
    function GetSquare: TSquare;
    procedure SetSquare(Value: TSquare);
  public
    constructor Create(APlayer: TPlayer; const ADest: TQualifiedPos;
      AKeyPressed: Boolean); overload;
    constructor Create(APlayer: TPlayer; const ADest: T3DPoint;
      AKeyPressed: Boolean); overload;

    procedure Cancel;

    property Player: TPlayer read FPlayer;

    property SrcMap: TMap read FSrcMap;
    property Src: T3DPoint read FSrc;
    property SrcSquare: TSquare read GetSrcSquare write SetSrcSquare;

    property DestMap: TMap read FDestMap;
    property Dest: T3DPoint read FDest;
    property DestSquare: TSquare read GetDestSquare write SetDestSquare;

    property Map: TMap read FMap;
    property Pos: T3DPoint read FPos;
    property Square: TSquare read GetSquare write SetSquare;

    property OldDirection: TDirection read FOldDirection;
    property KeyPressed: Boolean read FKeyPressed;

    property Cancelled: Boolean read FCancelled write FCancelled;
    property GoOnMoving: Boolean read FGoOnMoving write FGoOnMoving;
  end;

  {*
    Données d'un composant liées à un joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerData = class(TObject)
  private
    FComponent: TFunLabyComponent; /// Composant propriétaire
    FPlayer: TPlayer;              /// Joueur lié
  public
    constructor Create(AComponent: TFunLabyComponent;
      APlayer: TPlayer); virtual;

    property Component: TFunLabyComponent read FComponent;
    property Player: TPlayer read FPlayer;
  end;

  /// Classe de TPlayerData
  TPlayerDataClass = class of TPlayerData;

  /// Alias de type utilisé par le codegen FunDelphi
  TFunLabyComponentPlayerData = TPlayerData;

  {*
    Classe de base pour les composants de FunLabyrinthe
    TFunLabyComponent est la classe de base pour tous les composants de
    FunLabyrinthe. Elle fournit des propriétés et des méthodes pour repérer le
    maître FunLabyrinthe et pour identifier le composant.
    @author sjrd
    @version 5.0
  *}
  TFunLabyComponent = class
  private
    FMaster: TMaster;  /// Maître FunLabyrinthe
    FID: TComponentID; /// ID du composant
    {*
      Valeur non fonctionnelle pouvant servir au fonctionnement d'un algorithme
      Cette valeur est susceptible d'être utilisée par beaucoup d'algorithmes
      différents, et donc interférer. Il ne faut donc l'utiliser que
      ponctuellement.
    *}
    FTag: Integer;

    FPlayerData: TBucketItemArray; /// Données par joueur

    function GetSafeID: TComponentID;
  protected
    class function GetPlayerDataClass: TPlayerDataClass; virtual;

    function GetPlayerData(Player: TPlayer): TPlayerData;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
    destructor Destroy; override;

    property Master: TMaster read FMaster;
    property ID: TComponentID read FID;
    property SafeID: TComponentID read GetSafeID;
    property Tag: Integer read FTag write FTag;
  end;

  /// Classe de TFunLabyComponentClass
  TFunLabyComponentClass = class of TFunLabyComponent;

  {*
    Classe de base pour les composants devant être affichés
    TVisualComponent étend la classe TFunLabyComponent pour lui ajouter un
    traitement standart et simple de nommage et de dessin.
    @author sjrd
    @version 5.0
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName: string;             /// Nom du composant
    FPainter: TPainter;        /// Peintre par défaut
    FCachedImg: TSquareBitmap; /// Image en cache (pour les dessins invariants)

    procedure PrivDraw(Context: TDrawSquareContext); virtual;
  protected
    FStaticDraw: Boolean; /// Indique si le dessin du composant est invariant

    procedure DoDraw(Context: TDrawSquareContext); virtual;

    property Painter: TPainter read FPainter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(Context: TDrawSquareContext); overload;
    procedure Draw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); overload;

    property Name: string read FName;
    property StaticDraw: Boolean read FStaticDraw;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs façons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Empêcher le déplacement du joueur et réagir à son déplacement effectif ;
    - Indiquer au joueur qu'il a la capacité de faire certaines actions.
    @author sjrd
    @version 5.0
  *}
  TPlugin = class(TFunLabyComponent)
  private
    FPainterBefore: TPainter; /// Peintre par défaut sous le joueur
    FPainterAfter: TPainter;  /// Peintre par défaut sur le joueur
  protected
    FZIndex: Integer; /// Z-index parmi les plug-in

    property PainterBefore: TPainter read FPainterBefore;
    property PainterAfter: TPainter read FPainterAfter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Context: TDrawSquareContext); virtual;
    procedure DrawAfter(Context: TDrawSquareContext); virtual;

    procedure Moving(Context: TMoveContext); virtual;
    procedure Moved(Context: TMoveContext); virtual;

    procedure DrawView(Context: TDrawViewContext); virtual;
    procedure PressKey(Context: TKeyEventContext); virtual;

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; virtual;

    property ZIndex: Integer read FZIndex;
  end;

  /// Tableau dynamique de TPlugin
  TPluginDynArray = array of TPlugin;

  {*
    Données liées à un joueur pour une définition d'objet
    @author sjrd
    @version 5.0
  *}
  TObjectDefPlayerData = class(TPlayerData)
  private
    FCount: Integer; /// Nombre d'objets possédés par le joueur
  end;

  {*
    Classe de base pour les définitions d'objets
    TObjectDef est la classe de base pour les définitions d'objets que possède
    le joueur.
    Les objets peuvent rendre un joueur capable d'effectuer certaines actions.
    @author sjrd
    @version 5.0
  *}
  TObjectDef = class(TVisualComponent)
  protected
    class function GetPlayerDataClass: TPlayerDataClass; override;

    function GetCount(Player: TPlayer): Integer; virtual;
    procedure SetCount(Player: TPlayer; Value: Integer); virtual;

    function GetShownInfos(Player: TPlayer): string; virtual;
  public
    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; virtual;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction); virtual;

    property Count[Player: TPlayer]: Integer read GetCount write SetCount;
    property ShownInfos[Player: TPlayer]: string read GetShownInfos;
  end;

  {*
    Classe de base pour les composants de case
    @author sjrd
    @version 5.0
  *}
  TSquareComponent = class(TVisualComponent)
  end;

  {*
    Classe de base pour les terrains
    TField est la classe de base pour la création de terrains. Les terrains
    sont la première composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TField = class(TSquareComponent)
  private
    FDelegateDrawTo: TField; /// Terrain délégué pour l'affichage

    procedure PrivDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADelegateDrawTo: TField = nil);

    procedure Entering(Context: TMoveContext); virtual;
    procedure Exiting(Context: TMoveContext); virtual;

    procedure Entered(Context: TMoveContext); virtual;
    procedure Exited(Context: TMoveContext); virtual;
  end;

  {*
    Classe de base pour les effets de case
    TEffect est la classe de base pour la création d'effets de case. Les effets
    sont la deuxième composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TEffect = class(TSquareComponent)
  public
    procedure Entered(Context: TMoveContext); virtual;
    procedure Exited(Context: TMoveContext); virtual;

    procedure Execute(Context: TMoveContext); virtual;
  end;

  {*
    Classe de base pour les outils
    TTool est la classe de base pour la création d'outils. Les outils sont la
    troisième composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TTool = class(TSquareComponent)
  public
    procedure Find(Context: TMoveContext); virtual;
  end;

  {*
    Classe de base pour les obstacles
    TObstacle est la classe de base pour la création d'obstacles. Les obstacles
    sont la quatrième composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TObstacle = class(TSquareComponent)
  public
    procedure Pushing(Context: TMoveContext); virtual;
  end;

  {*
    Représente une case du jeu
    TSquare représente une case du jeu. Une case possède quatre composantes : le
    terrain, l'effet, l'outil et l'obstacle. Chacune de ces composantes est
    optionnelle.
    @author sjrd
    @version 5.0
  *}
  TSquare = class(TSquareComponent)
  private
    FField: TField;       /// Terrain
    FEffect: TEffect;     /// Effet
    FTool: TTool;         /// Outil
    FObstacle: TObstacle; /// Obstacle
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; AField: TField; AEffect: TEffect; ATool: TTool;
      AObstacle: TObstacle);

    procedure DefaultHandler(var Msg); override;

    function Contains(Component: TSquareComponent): Boolean;

    procedure Entering(Context: TMoveContext); virtual;
    procedure Exiting(Context: TMoveContext); virtual;

    procedure Entered(Context: TMoveContext); virtual;
    procedure Exited(Context: TMoveContext); virtual;

    procedure Execute(Context: TMoveContext); virtual;

    procedure Pushing(Context: TMoveContext); virtual;

    property Field: TField read FField;
    property Effect: TEffect read FEffect;
    property Tool: TTool read FTool;
    property Obstacle: TObstacle read FObstacle;
  end;

  {*
    Représente la carte du jeu
    TMap gère et représente la carte du jeu. Elle offre des propriétés et
    méthodes pour lire et modifier cette carte.
    @author sjrd
    @version 5.0
  *}
  TMap = class(TFunLabyComponent)
  private
    FDimensions: T3DPoint;   /// Dimensions de la carte (en cases)
    FZoneWidth: Integer;     /// Largeur d'une zone de la carte
    FZoneHeight: Integer;    /// Hauteur d'une zone de la carte
    FMaxViewSize: Integer;   /// Taille maximale d'une vue pour cette carte
    FMap: array of TSquare;  /// Carte stockée de façon linéaire
    FOutsideOffset: Integer; /// Offset de départ de l'extérieur

    procedure SetMaxViewSize(Value: Integer);

    function GetMap(const Position: T3DPoint): TSquare;
    procedure SetMap(const Position: T3DPoint; Value: TSquare);

    function GetOutside(Floor: Integer): TSquare;
    procedure SetOutside(Floor: Integer; Value: TSquare);

    function GetLinearMapCount: Integer;
    function GetLinearMap(Index: Integer): TSquare;
    procedure SetLinearMap(Index: Integer; Value: TSquare);
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer);

    function InMap(const Position: T3DPoint): Boolean;

    function PlayersOn(const Position: T3DPoint): Integer;

    property Dimensions: T3DPoint read FDimensions;
    property ZoneWidth: Integer read FZoneWidth;
    property ZoneHeight: Integer read FZoneHeight;
    property MaxViewSize: Integer read FMaxViewSize write SetMaxViewSize;

    property Map[const Position: T3DPoint]: TSquare
      read GetMap write SetMap; default;

    property Outside[Floor: Integer]: TSquare
      read GetOutside write SetOutside;

    property LinearMapCount: Integer read GetLinearMapCount;
    property LinearMap[Index: Integer]: TSquare
      read GetLinearMap write SetLinearMap;
  end;

  /// Classe de TPlayerMode
  TPlayerModeClass = class of TPlayerMode;

  {*
    Mode principal d'un joueur
    À tout moment, chaque joueur est dans un mode donné qui détermine comment
    est affichée sa vue, et comment il est contrôlé. Un joueur a un et un seul
    mode principal, mais ses plug-in peuvent modifier les comportements de son
    mode principal.
    Vous devez utiliser la class TPlayerMode comme classe de base pour toute
    implémentation de IPlayerMode.
    @author sjrd
    @version 5.0
  *}
  IPlayerMode = interface(IInterface)
    ['{7EDA1965-B154-4E2C-A9E4-CC48B503B85B}']

    {*
      Classe de ce mode
      @return Classe de ce mode
    *}
    function GetModeClass: TPlayerModeClass;

    {*
      Joueur lié à ce mode
      @return Joueur lié à ce mode
    *}
    function GetPlayer: TPlayer;

    {*
      Largeur de la vue
      @return Largeur de la vue, en pixels
    *}
    function GetWidth: Integer;

    {*
      Hauteur de la vue
      @return Hauteur de la vue, en pixels
    *}
    function GetHeight: Integer;

    {*
      Indique si ce mode utilise un fonctionnement par zone
      @return True si utilise un fonctionnement par zone, False sinon
    *}
    function GetUseZone: Boolean;

    {*
      Dessine la vue du joueur
      @param Context   Contexte de dessin de la vue
    *}
    procedure DrawView(Context: TDrawViewContext);

    {*
      Presse une touche pour le joueur
      @param Context   Contexte de l'appui de touche
    *}
    procedure PressKey(Context: TKeyEventContext);

    property ModeClass: TPlayerModeClass read GetModeClass;
    property Player: TPlayer read GetPlayer;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property UseZone: Boolean read GetUseZone;
  end;

  {*
    Mode principal d'un joueur
    Squelette d'implémentation de IPlayerMode.
    @author sjrd
    @version 5.0
  *}
  TPlayerMode = class(TInterfacedObject, IPlayerMode)
  private
    FMaster: TMaster; /// Maître FunLabyrinthe
    FPlayer: TPlayer; /// Joueur lié à ce mode
  protected
    function GetModeClass: TPlayerModeClass;
    function GetPlayer: TPlayer;
    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetUseZone: Boolean; virtual;
  public
    constructor Create(APlayer: TPlayer); virtual;

    procedure DrawView(Context: TDrawViewContext); virtual; abstract;
    procedure PressKey(Context: TKeyEventContext); virtual; abstract;

    property Master: TMaster read FMaster;
    property Player: TPlayer read FPlayer;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property UseZone: Boolean read GetUseZone;
  end;

  {*
    Mode labyrinthe (mode standard de FunLabyrinthe)
    @author sjrd
    @version 5.0
  *}
  TLabyrinthPlayerMode = class(TPlayerMode)
  private
    FCacheBitmap: TBitmap; /// Bitmap cache

    FOldMap: TMap;              /// Ancienne carte
    FOldFloor: Integer;         /// Ancien étage
    FOldZone: TRect;            /// Ancienne zone
    FOldView: array of TSquare; /// Ancienne vue
  protected
    procedure InvalidateCache(Context: TDrawViewContext);
    procedure InvalidateCacheIfNeeded(Context: TDrawViewContext);
    procedure UpdateCache(Context: TDrawViewContext);
    procedure DrawPlayers(Context: TDrawViewContext);

    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetUseZone: Boolean; override;

    property CacheBitmap: TBitmap read FCacheBitmap;
  public
    constructor Create(APlayer: TPlayer); override;
    destructor Destroy; override;

    procedure DrawView(Context: TDrawViewContext); override;
    procedure PressKey(Context: TKeyEventContext); override;
  end;

  {*
    Classe représentant un joueur
    TPlayer représente un joueur. Elle possède de nombreuses propriétés et
    méthodes permettant d'afficher le joueur, de le déplacer, de lui greffer
    des plug-in, etc.
    @author sjrd
    @version 5.0
  *}
  TPlayer = class(TVisualComponent)
  private
    FMap: TMap;                        /// Carte
    FPosition: T3DPoint;               /// Position
    FDirection: TDirection;            /// Direction
    FMode: IPlayerMode;                /// Mode principal
    FModeStack: IInterfaceList;        /// Pile des modes sauvegardés
    FShowCounter: Integer;             /// Compteur de visibilité
    FColor: TColor;                    /// Couleur
    FViewBorderSize: Integer;          /// Taille de la bordure de la vue
    FPlugins: TObjectList;             /// Liste des plug-in
    FAttributes: TStrings;             /// Liste des attributs
    FOnSendCommand: TSendCommandEvent; /// Événement d'exécution de commande
    FPlayState: TPlayState;            /// État de victoire/défaite

    /// Verrou global pour le joueur
    FLock: TMultiReadExclusiveWriteSynchronizer;

    FKeyPressCoroutine: TCoroutine; /// Coroutine d'appui sur touche
    FKeyPressKey: Word;             /// Touche appuyée
    FKeyPressShift: TShiftState;    /// État des touches spéciales

    procedure GetPluginList(out PluginList: TPluginDynArray);

    procedure PrivDraw(Context: TDrawSquareContext); override;

    function IsMoveAllowed(Context: TMoveContext): Boolean;
    procedure MoveTo(Context: TMoveContext; Execute: Boolean = True); overload;

    procedure KeyPressCoroutineProc(Coroutine: TCoroutine);

    function GetVisible: Boolean;
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;

    procedure PositionChanged; virtual;

    function GetAttribute(const AttrName: string): Integer; virtual;
    procedure SetAttribute(const AttrName: string; Value: Integer); virtual;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; AMap: TMap; const APosition: T3DPoint);
    destructor Destroy; override;

    procedure Dispatch(var Msg); override;
    procedure DefaultHandler(var Msg); override;

    procedure GetAttributes(Attributes: TStrings); virtual;
    procedure GetPluginIDs(PluginIDs: TStrings);

    procedure DrawInPlace(Canvas: TCanvas; X: Integer = 0;
      Y: Integer = 0);

    procedure ChangeMode(ModeClass: TPlayerModeClass);
    procedure BeginTempMode(ModeClass: TPlayerModeClass);
    procedure EndTempMode;

    procedure AddPlugin(Plugin: TPlugin);
    procedure RemovePlugin(Plugin: TPlugin);

    function AbleTo(const Action: TPlayerAction): Boolean;
    function DoAction(const Action: TPlayerAction): Boolean;

    procedure Move(Dir: TDirection; KeyPressed: Boolean;
      out Redo: Boolean);

    procedure MoveTo(const Dest: T3DPoint; Execute: Boolean;
      out Redo: Boolean); overload;
    procedure MoveTo(const Dest: T3DPoint); overload;
    procedure MoveTo(const Dest: TQualifiedPos; Execute: Boolean;
      out Redo: Boolean); overload;
    procedure MoveTo(const Dest: TQualifiedPos); overload;

    procedure Temporize;
    procedure NaturalMoving;

    procedure ChangePosition(AMap: TMap; const APosition: T3DPoint);

    procedure Show;
    procedure Hide;

    function SendCommand(const Command: string;
      const Params: string = ''): string;

    function ShowDialog(const Title, Text: string;
      DlgType: TDialogType = dtInformation; DlgButtons: TDialogButtons = dbOK;
      DefButton: Byte = 1; AddFlags: LongWord = 0): TDialogResult;
    function ShowDialogRadio(const Title, Text: string; DlgType: TMsgDlgType;
      DlgButtons: TMsgDlgButtons; DefButton: TModalResult;
      const RadioTitles: array of string; var Selected: Integer;
      OverButtons: Boolean = False): Word;
    function ChooseNumber(const Title, Prompt: string;
      Default, Min, Max: Integer): Integer;

    procedure ShowMessage(const Text: string);

    procedure Win;
    procedure Lose;

    procedure DrawView(Canvas: TCanvas); virtual;
    procedure PressKey(Key: Word; Shift: TShiftState);

    procedure WaitForKey(out Key: Word; out Shift: TShiftState);
    procedure WaitForSpecificKey(Key: Word; Shift: TShiftState = []);

    property Map: TMap read FMap;
    property Position: T3DPoint read FPosition;
    property Direction: TDirection read FDirection write FDirection;
    property Mode: IPlayerMode read FMode;
    property Visible: Boolean read GetVisible;
    property Color: TColor read FColor write FColor;
    property ViewBorderSize: Integer read FViewBorderSize write FViewBorderSize;
    property Attribute[const AttrName: string]: Integer
      read GetAttribute write SetAttribute;
    property OnSendCommand: TSendCommandEvent
      read FOnSendCommand write FOnSendCommand;
    property PlayState: TPlayState read FPlayState;
  end;

  {*
    Maître FunLabyrinthe
    TMaster gère les différents composants de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TMaster = class
  private
    FImagesMaster: TImagesMaster; /// Maître d'images
    FComponents: TStrings;        /// Table de hashage ID -> composant
    FPlugins: TObjectList;        /// Liste des plug-in
    FObjectDefs: TObjectList;     /// Liste des définitions d'objet
    FFields: TObjectList;         /// Liste des terrains
    FEffects: TObjectList;        /// Liste des effets
    FTools: TObjectList;          /// Liste des outils
    FObstacles: TObjectList;      /// Liste des obstacles
    FSquares: TObjectList;        /// Liste des cases
    FMaps: TObjectList;           /// Liste des cartes
    FPlayers: TObjectList;        /// Liste des joueurs

    FEditing: Boolean;            /// Indique si on est en mode édition
    FTemporization: Integer;      /// Temporisation en millisecondes
    FBeginTickCount: Cardinal;    /// Tick count système au lancement
    FTerminated: Boolean;         /// Indique si la partie est terminée

    function GetComponent(const ID: TComponentID): TFunLabyComponent;
    function GetSquareComponent(const ID: TComponentID): TSquareComponent;

    function GetPlugin(const ID: TComponentID): TPlugin;
    function GetObjectDef(const ID: TComponentID): TObjectDef;
    function GetField(const ID: TComponentID): TField;
    function GetEffect(const ID: TComponentID): TEffect;
    function GetTool(const ID: TComponentID): TTool;
    function GetObstacle(const ID: TComponentID): TObstacle;
    function GetSquare(const ID: TComponentID): TSquare;
    function GetMap(const ID: TComponentID): TMap;
    function GetPlayer(const ID: TComponentID): TPlayer;

    function GetPluginCount: Integer;
    function GetPlugins(Index: Integer): TPlugin;
    function GetObjectDefCount: Integer;
    function GetObjectDefs(Index: Integer): TObjectDef;
    function GetFieldCount: Integer;
    function GetFields(Index: Integer): TField;
    function GetEffectCount: Integer;
    function GetEffects(Index: Integer): TEffect;
    function GetToolCount: Integer;
    function GetTools(Index: Integer): TTool;
    function GetObstacleCount: Integer;
    function GetObstacles(Index: Integer): TObstacle;
    function GetSquareCount: Integer;
    function GetSquares(Index: Integer): TSquare;
    function GetMapCount: Integer;
    function GetMaps(Index: Integer): TMap;
    function GetPlayerCount: Integer;
    function GetPlayers(Index: Integer): TPlayer;

    procedure SetTemporization(Value: Integer);

    function GetTickCount: Cardinal;

    procedure AddComponent(Component: TFunLabyComponent);
    procedure RemoveComponent(Component: TFunLabyComponent);

    procedure Terminate;
  public
    constructor Create(AEditing: Boolean);
    destructor Destroy; override;

    procedure Temporize;

    function SquareByComps(
      const Field, Effect, Tool, Obstacle: TComponentID): TSquare;

    property ImagesMaster: TImagesMaster read FImagesMaster;

    property Component[const ID: TComponentID]: TFunLabyComponent
      read GetComponent;
    property SquareComponent[const ID: TComponentID]: TSquareComponent
      read GetSquareComponent;
    property Plugin[const ID: TComponentID]: TPlugin read GetPlugin;
    property ObjectDef[const ID: TComponentID]: TObjectDef read GetObjectDef;
    property Field[const ID: TComponentID]: TField read GetField;
    property Effect[const ID: TComponentID]: TEffect read GetEffect;
    property Tool[const ID: TComponentID]: TTool read GetTool;
    property Obstacle[const ID: TComponentID]: TObstacle read GetObstacle;
    property Square[const ID: TComponentID]: TSquare read GetSquare;
    property Map[const ID: TComponentID]: TMap read GetMap;
    property Player[const ID: TComponentID]: TPlayer read GetPlayer;

    property PluginCount: Integer read GetPluginCount;
    property Plugins[Index: Integer]: TPlugin read GetPlugins;
    property ObjectDefCount: Integer read GetObjectDefCount;
    property ObjectDefs[Index: Integer]: TObjectDef read GetObjectDefs;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TField read GetFields;
    property EffectCount: Integer read GetEffectCount;
    property Effects[Index: Integer]: TEffect read GetEffects;
    property ToolCount: Integer read GetToolCount;
    property Tools[Index: Integer]: TTool read GetTools;
    property ObstacleCount: Integer read GetObstacleCount;
    property Obstacles[Index: Integer]: TObstacle read GetObstacles;
    property SquareCount: Integer read GetSquareCount;
    property Squares[Index: Integer]: TSquare read GetSquares;
    property MapCount: Integer read GetMapCount;
    property Maps[Index: Integer]: TMap read GetMaps;
    property PlayerCount: Integer read GetPlayerCount;
    property Players[Index: Integer]: TPlayer read GetPlayers;

    property Editing: Boolean read FEditing;
    property Temporization: Integer read FTemporization write SetTemporization;
    property TickCount: Cardinal read GetTickCount;
    property Terminated: Boolean read FTerminated;
  end;

const {don't localize}
  /// Fichier INI de FunLabyrinthe
  fIniFileName = 'FunLabyrinthe.ini';

  /// Position qualifiée nulle
  NoQPos: TQualifiedPos = (Map: nil; Position: (X: 0; Y: 0; Z: 0));

  /// Rectangle d'une case à l'origine
  BaseSquareRect: TRect = (
    Left: 0; Top: 0; Right: SquareSize; Bottom: SquareSize
  );

  /// Temporisation par défaut
  DefaultTemporization = 500;

  /// Couleur par défaut d'un joueur
  DefaultPlayerColor = clBlue;

  /// Taille de bordure de vue par défaut
  DefaultViewBorderSize = 1;

  /// Application d'une direction vers la direction opposée
  NegDir: array[TDirection] of TDirection = (
    diNone, diSouth, diWest, diNorth, diEast
  );

var {don't localize}
  /// Dossier de FunLabyrinthe dans Application Data
  fFunLabyAppData: string = '';
  /// Dossier des fichiers image
  fSquaresDir: string = 'Squares\';
  /// Dossier des fichiers son
  fSoundsDir: string = 'Sounds\';
  /// Dossier des unités
  fUnitsDir: string = 'Units\';
  /// Dossier des cartes
  fMapsDir: string = 'Maps\';
  /// Dossier des fichiers labyrinthe
  fLabyrinthsDir: string = 'Labyrinths\';
  /// Dossier des fichiers sauvegarde
  fSaveguardsDir: string = 'Saveguards\';
  /// Dossier des plug-in de l'éditeur
  fEditPluginDir: string = 'EditPlugins\';

  /// Chaîne de format pour les fichiers image
  fSquareFileName: string = '%s.bmp';

function CheckValidLaunch: Boolean;
procedure ShowFunLabyAbout;

function PointBehind(const Src: T3DPoint; Dir: TDirection): T3DPoint;
function PointBefore(const Src: T3DPoint; Dir: TDirection): T3DPoint;

function SquareRect(X, Y: Integer): TRect;
procedure EmptyRect(Canvas: TCanvas; Rect: TRect);
procedure EmptySquareRect(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);

function SameRect(const Left, Right: TRect): Boolean;

function IsNoQPos(const QPos: TQualifiedPos): Boolean;

implementation

uses
  IniFiles, StrUtils, Forms, ScStrUtils, ScDelphiLanguage;

{*
  Vérifie que FunLabyrinthe a été lancé de façon valide
  FunLabyrinthe est lancé de façon valide lorsque l'exécutable se situe sur
  l'ordinateur local. Il est en effet impossible, sinon, d'accéder aux
  packages Delphi et autres ressources non partageables.
  @return True si FunLabyrinthe a été lancé de façon valide, False sinon
*}
function CheckValidLaunch: Boolean;
begin
  if (Dir[1] = PathDelim) and (Dir[2] = PathDelim) then
  begin
    ShowDialog(sError, sCantLaunchThroughNetwork, dtError);
    Result := False;
  end else
    Result := True;
end;

{*
  Affiche une boîte de dialogue À propos de FunLabyrinthe
  @param Icon   Icône du programme
  @param Name   Nom du programme
*}
procedure ShowFunLabyAbout;
begin
  with TSdAboutDialog.Create(nil) do
  try
    ProgramIcon := Application.Icon;
    ProgramName := Application.Title;
    ProgramVersion := CurrentVersion;
    AuthorName := FunLabyAuthorName;
    AuthorEMail := FunLabyAuthorEMail;
    WebSite := FunLabyWebSite;

    Execute;
  finally
    Free;
  end;
end;

{*
  Renvoie le point situé derrière un point dans la direction indiquée
  @param Src   Point origine
  @param Dir   Direction dans laquelle on va
  @return Le point situé derrière le point Src dans la direction Dir
*}
function PointBehind(const Src: T3DPoint; Dir: TDirection): T3DPoint;
begin
  Result := Src;
  case Dir of
    diNorth: Dec(Result.Y);
    diEast:  Inc(Result.X);
    diSouth: Inc(Result.Y);
    diWest:  Dec(Result.X);
  end;
end;

{*
  Renvoie le point situé devant un point depuis la direction indiquée
  @param Src   Point origine
  @param Dir   Direction depuis laquelle on vient
  @return Le point situé devant le point Src selon la direction Dir
*}
function PointBefore(const Src: T3DPoint; Dir: TDirection): T3DPoint;
begin
  Result := PointBehind(Src, NegDir[Dir]);
end;

{*
  Crée un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord supérieur du rectangle
  @return Le rectangle de type TRect
*}
function SquareRect(X, Y: Integer): TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X+SquareSize;
  Result.Bottom := Y+SquareSize;
end;

{*
  Efface un rectangle sur un canevas (avec du transparent)
  @param Canvas   Canevas à traiter
  @param Rect     Rectangle à effacer
*}
procedure EmptyRect(Canvas: TCanvas; Rect: TRect);
begin
  with Canvas do
  begin
    Brush.Color := clTransparent;
    Brush.Style := bsSolid;
    Pen.Color := clTransparent;
    Pen.Style := psSolid;
    Rectangle(Rect);
  end;
end;

{*
  Efface un rectangle de case sur un canevas (avec du transparent)
  @param Canvas   Canevas à traiter
  @param X        Bord gauche du rectangle
  @param Y        Bord supérieur du rectangle
*}
procedure EmptySquareRect(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  EmptyRect(Canvas, SquareRect(X, Y));
end;

{*
  Teste si deux rectangles sont égaux
  @param Left    Rectangle de gauche
  @param Right   Rectangle de droite
  @return True si les rectangles sont égaux, False sinon
*}
function SameRect(const Left, Right: TRect): Boolean;
begin
  Result := (Left.Left = Right.Left) and (Left.Top = Right.Top) and
    (Left.Right = Right.Right) and (Left.Bottom = Right.Bottom);
end;

{*
  Détermine si une position qualifiée est nulle
  @param QPos   Position à tester
  @return True si la position qualifiée QPos est nulle, False sinon
*}
function IsNoQPos(const QPos: TQualifiedPos): Boolean;
begin
  Result := QPos.Map = nil;
end;

{----------------------}
{ Classe TImagesMaster }
{----------------------}

{*
  Crée une instance de TImagesMaster
*}
constructor TImagesMaster.Create;
var
  EmptySquare: TBitmap;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(SquareSize, SquareSize);
  FImgNames := THashedStringList.Create;

  EmptySquare := TSquareBitmap.Create;
  try
    Add('', EmptySquare);
  finally
    EmptySquare.Free;
  end;
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
  Ajoute une image à partir d'un bitmap au maître d'images
  Si une image de même nom existe déjà, l'ajout est ignoré.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoyé.
  @param ImgName   Nom de l'image
  @param Bitmap    Bitmap contenant l'image à ajouter
  @return Index de l'image nouvellement ajoutée, ou existante
*}
function TImagesMaster.Add(const ImgName: string; Bitmap: TBitmap): Integer;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  try
    FImgList.AddMasked(Bitmap, clTransparent);
    try
      Result := FImgNames.Add(ImgName);
    except
      FImgList.Delete(FImgList.Count-1);
      raise;
    end;
  except
    Result := 0; // Image vide
  end;
end;

{*
  Renvoie l'index de l'image dont le nom est spécifié
  IndexOf renvoie l'index de l'image dont le nom est spécifié dans la
  liste d'images interne. Si l'image n'a pas encore été chargée, IndexOf
  la charge.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoyé.
  @param ImgName   Nom de l'image
  @return Index de l'image
*}
function TImagesMaster.IndexOf(const ImgName: string): Integer;
var
  NewImg: TBitmap;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  begin
    NewImg := TBitmap.Create;
    try
      try
        NewImg.LoadFromFile(Format(fSquareFileName, [ImgName]));
        Result := Add(ImgName, NewImg);
      except
        Result := 0; // Image vide
      end;
    finally
      NewImg.Free;
    end;
  end;
end;

{*
  Dessine une image à partir de son index
  Draw dessine l'image indiquée sur un canevas.
  @param Index     Index de l'image à dessiner
  @param Context   Contexte de dessin de la case
*}
procedure TImagesMaster.Draw(Index: Integer; Context: TDrawSquareContext);
begin
  with Context do
    FImgList.Draw(Canvas, X, Y, Index);
end;

{*
  Dessine une image à partir de son nom
  Draw dessine l'image indiquée sur un canevas.
  @param ImgName   Nom de l'image à dessiner
  @param Context   Contexte de dessin de la case
*}
procedure TImagesMaster.Draw(const ImgName: string;
  Context: TDrawSquareContext);
begin
  Draw(IndexOf(ImgName), Context);
end;

{*
  Dessine une image à partir de son index
  Draw dessine l'image indiquée sur un canevas.
  @param Index    Index de l'image à dessiner
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonnée X du point à partir duquel dessiner l'image
  @param Y        Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index: Integer; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
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
procedure TImagesMaster.Draw(const ImgName: string; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  Draw(IndexOf(ImgName), Canvas, X, Y);
end;

{----------------------}
{ Classe TSquareBitmap }
{----------------------}

{*
  Crée une instance de TSquareBitmap
*}
constructor TSquareBitmap.Create;
begin
  inherited;

  Width := SquareSize;
  Height := SquareSize;
  EmptySquare;
end;

{*
  Efface le contenu de la case
*}
procedure TSquareBitmap.EmptySquare;
begin
  EmptySquareRect(Canvas);
end;

{*
  Dessine l'image de case sur un canevas
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonnée X du point à partir duquel dessiner l'image
  @param Y        Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TSquareBitmap.DrawSquare(Context: TDrawSquareContext);
var
  OldBrushStyle: TBrushStyle;
begin
  with Context do
  begin
    OldBrushStyle := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
    Canvas.BrushCopy(SquareRect, Self, BaseSquareRect, clTransparent);
    Canvas.Brush.Style := OldBrushStyle;
  end;
end;

{--------------------------}
{ TDrawSquareContext class }
{--------------------------}

{*
  Crée un contexte de dessin de case
*}
constructor TDrawSquareContext.Create(ACanvas: TCanvas);
begin
  Create(ACanvas, 0, 0, NoQPos);
end;

{*
  Crée un contexte de dessin de case
  @param ACanvas   Canevas cible
  @param X         Abscisce où dessiner
  @param Y         Abscisce où dessiner
*}
constructor TDrawSquareContext.Create(ACanvas: TCanvas; X, Y: Integer);
begin
  Create(ACanvas, X, Y, NoQPos);
end;

{*
  Crée un contexte de dessin de case
  @param ACanvas   Canevas cible
  @param X         Abscisce où dessiner
  @param Y         Abscisce où dessiner
  @param AQPos     Position qualifiée à dessiner
*}
constructor TDrawSquareContext.Create(ACanvas: TCanvas; X, Y: Integer;
  const AQPos: TQualifiedPos);
begin
  inherited Create;

  FCanvas := ACanvas;
  FX := X;
  FY := Y;
  FSquareRect := FunLabyUtils.SquareRect(X, Y);

  FIsNowhere := IsNoQPos(AQPos);
  FQPos := AQPos;
end;

{*
  Spécifie le joueur à dessiner
  @param APlayer   Joueur à dessiner
*}
procedure TDrawSquareContext.SetPlayer(APlayer: TPlayer);
begin
  FPlayer := APlayer;
end;

{*
  Spécifie le tick count pour ce contexte de dessin
  @param ATickCount   Tick count pour ce contexte de dessin
*}
procedure TDrawSquareContext.SetTickCount(ATickCount: Cardinal);
begin
  FTickCount := ATickCount;
end;

{------------------------}
{ TDrawViewContext class }
{------------------------}

{*
  Crée un contexte de dessin d'une vue
  @param APlayerMode   Mode principal du joueur dont dessiner la vue
  @param ACanvas       Canevas cible
*}
constructor TDrawViewContext.Create(const APlayerMode: IPlayerMode;
  ACanvas: TCanvas);
begin
  inherited Create;

  FPlayerMode := APlayerMode;
  FPlayer := APlayerMode.Player;
  FCanvas := ACanvas;

  FViewRect := Rect(0, 0, PlayerMode.Width, PlayerMode.Height);
  FUseZone := PlayerMode.UseZone;

  if UseZone then
    ComputeZone;

  FTickCount := Player.Master.TickCount;
end;

{*
  Calcule la zone à afficher
*}
procedure TDrawViewContext.ComputeZone;
var
  OrigX, OrigY, BorderSize: Integer;
begin
  FMap := Player.Map;
  FFloor := Player.Position.Z;

  BorderSize := Player.ViewBorderSize;

  // Start at player's position
  OrigX := Player.Position.X;
  OrigY := Player.Position.Y;

  { If the player has stopped playing, and he's at the map border, let's show
    the closest in-map zone instead. }
  if Player.PlayState <> psPlaying then
  begin
    if OrigX = -1 then
      OrigX := 0
    else if OrigX = Map.Dimensions.X then
      Dec(OrigX);
    if OrigY = -1 then
      OrigY := 0
    else if OrigY = Map.Dimensions.Y then
      Dec(OrigY);
  end;

  // Find the zone upper left corner
  Dec(OrigX, IntMod(OrigX, Map.ZoneWidth));
  Dec(OrigY, IntMod(OrigY, Map.ZoneHeight));

  // Find the view upper left corner
  Dec(OrigX, BorderSize);
  Dec(OrigY, BorderSize);

  // Make FZone
  FZoneSize.X := Map.ZoneWidth + 2*BorderSize;
  FZoneSize.Y := Map.ZoneHeight + 2*BorderSize;
  FZone := Rect(OrigX, OrigY, OrigX + FZoneSize.X, OrigY + FZoneSize.Y);
end;

{*
  Teste si une case est visible dans cette zone
  @param QPos   Position qualifiée de la case à tester
  @return True si la case visible, False sinon
*}
function TDrawViewContext.IsSquareVisible(const QPos: TQualifiedPos): Boolean;
begin
  Result := IsSquareVisible(QPos.Map, QPos.Position);
end;

{*
  Teste si une case est visible dans cette zone
  @param Map        Carte de la case à tester
  @param Position   Position de la case à tester
  @return True si la case visible, False sinon
*}
function TDrawViewContext.IsSquareVisible(Map: TMap;
  const Position: T3DPoint): Boolean;
begin
  Result := (Map = Self.Map) and (Position.Z = Floor) and
    PtInRect(Zone, Point(Position.X, Position.Y));
end;

{*
  Teste si une case est visible dans cette zone
  @param Position   Position de la case à tester
  @return True si la case visible, False sinon
*}
function TDrawViewContext.IsSquareVisible(const Position: T3DPoint): Boolean;
begin
  Result := IsSquareVisible(Map, Position);
end;

{------------------------}
{ TKeyEventContext class }
{------------------------}

{*
  Crée un contexte d'événement de touche
  @param AKey     Touche pressée
  @param AShift   État des touches spéciales
*}
constructor TKeyEventContext.Create(AKey: Word; AShift: TShiftState);
begin
  inherited Create;

  FKey := AKey;
  FShift := AShift;
end;

{-----------------}
{ Classe TPainter }
{-----------------}

{*
  Crée une instance de TPainter
  @param AMaster   Maître d'images associé au peintre
*}
constructor TPainter.Create(AMaster: TImagesMaster);
begin
  inherited Create;

  FMaster := AMaster;
  FImgNames := TStringList.Create;
  TStringList(FImgNames).OnChange := ImgNamesChange;
  FCachedImg := TSquareBitmap.Create;
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
procedure TPainter.ImgNamesChange(Sender: TObject);
var
  I: Integer;
begin
  FCachedImg.EmptySquare;
  for I := 0 to FImgNames.Count-1 do
    FMaster.Draw(FImgNames[I], FCachedImg.Canvas);
end;

{*
  Dessine les images sur un canevas
  La méthode Draw dessine les images de ImgNames sur le canevas, à la
  position indiquée. Les différentes images sont superposée, celle d'index
  0 tout au-dessous.
  @param Context   Contexte de dessin de la case
*}
procedure TPainter.Draw(Context: TDrawSquareContext);
begin
  FCachedImg.DrawSquare(Context);
end;

{--------------------}
{ TMoveContext class }
{--------------------}

{*
  Crée un contexte de déplacement du joueur
  @param APlayer       Joueur qui se déplace
  @param ADest         Destination
  @param AKeyPressed   True si une touche a été pressée
*}
constructor TMoveContext.Create(APlayer: TPlayer; const ADest: TQualifiedPos;
  AKeyPressed: Boolean);
begin
  inherited Create;

  FPlayer := APlayer;

  FSrcMap := Player.Map;
  FSrc := Player.Position;
  FDestMap := ADest.Map;
  FDest := ADest.Position;

  SwitchToSrc;

  FOldDirection := Player.Direction;
  FKeyPressed := AKeyPressed;

  FCancelled := False;
  FGoOnMoving := False;
end;

{*
  Crée un contexte de déplacement du joueur restant sur la même carte
  @param APlayer       Joueur qui se déplace
  @param ADest         Destination
  @param AKeyPressed   True si une touche a été pressée
*}
constructor TMoveContext.Create(APlayer: TPlayer; const ADest: T3DPoint;
  AKeyPressed: Boolean);
var
  DestQPos: TQualifiedPos;
begin
  DestQPos.Map := APlayer.Map;
  DestQPos.Position := ADest;

  Create(APlayer, DestQPos, AKeyPressed);
end;

{*
  Passe la case courante à la source
*}
procedure TMoveContext.SwitchToSrc;
begin
  FMap := SrcMap;
  FPos := Src;
end;

{*
  Passe la case courante à la destination
*}
procedure TMoveContext.SwitchToDest;
begin
  FMap := DestMap;
  FPos := Dest;
end;

{*
  Case source
  @return Case source
*}
function TMoveContext.GetSrcSquare: TSquare;
begin
  Result := SrcMap[Src];
end;

{*
  Modifie la case source
  @return Nouvelle case source
*}
procedure TMoveContext.SetSrcSquare(Value: TSquare);
begin
  SrcMap[Src] := Value;
end;

{*
  Case destination
  @return Case destination
*}
function TMoveContext.GetDestSquare: TSquare;
begin
  Result := DestMap[Dest];
end;

{*
  Modifie la case destination
  @return Nouvelle case destination
*}
procedure TMoveContext.SetDestSquare(Value: TSquare);
begin
  DestMap[Dest] := Value;
end;

{*
  Case courante
  @return Case courante
*}
function TMoveContext.GetSquare: TSquare;
begin
  Result := Map[Pos];
end;

{*
  Modifie la case courante
  @return Nouvelle case courante
*}
procedure TMoveContext.SetSquare(Value: TSquare);
begin
  Map[Pos] := Value;
end;

{*
  Annule le déplacement
*}
procedure TMoveContext.Cancel;
begin
  FCancelled := True;
end;

{-------------------}
{ TPlayerData class }
{-------------------}

{*
  Crée les données liées à un joueur
  @param AComponent   Composant propriétaire
  @param APlayer      Joueur
*}
constructor TPlayerData.Create(AComponent: TFunLabyComponent; APlayer: TPlayer);
begin
  inherited Create;

  FComponent := AComponent;
  FPlayer := APlayer;
end;

{--------------------------}
{ Classe TFunLabyComponent }
{--------------------------}

{*
  Crée une instance de TFunLabyComponent
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
*}
constructor TFunLabyComponent.Create(AMaster: TMaster;
  const AID: TComponentID);
begin
  if ClassType <> TFunLabyComponent then
    Assert(GetPlayerDataClass.InheritsFrom(
      TFunLabyComponentClass(ClassParent).GetPlayerDataClass));

  inherited Create;

  FMaster := AMaster;
  FID := AID;
  if FID <> '' then
    Master.AddComponent(Self);
end;

{*
  [@inheritDoc]
*}
destructor TFunLabyComponent.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FPlayerData)-1 do
    TPlayerData(FPlayerData[I].Data).Free;

  if (FID <> '') and Assigned(Master) then
    Master.RemoveComponent(Self);

  inherited;
end;

{*
  ID du composant
  Accès moins rapide que ID mais qui renvoie un ID vide si le composant vaut
  nil.
  @return ID du composant s'il existe, un ID vide sinon
*}
function TFunLabyComponent.GetSafeID: TComponentID;
begin
  if Assigned(Self) then
    Result := FID
  else
    Result := '';
end;

{*
  Classe de données liées au joueur
  Toute classe A héritant d'une classe B et qui réimplémente GetPlayerClass doit
  renvoyer une sous-classe de B.GetPlayerDataClass.
  @return Classe de données liées au joueur
*}
class function TFunLabyComponent.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TPlayerData;
end;

{*
  Obtient les données liées à un joueur donné
  @param Player   Joueur dont obtenir les données
  @return Données liées au joueur Player
*}
function TFunLabyComponent.GetPlayerData(Player: TPlayer): TPlayerData;
var
  I: Integer;
begin
  for I := 0 to Length(FPlayerData)-1 do
  begin
    if FPlayerData[I].Item = Player then
    begin
      Result := TPlayerData(FPlayerData[I].Data);
      Exit;
    end;
  end;

  I := Length(FPlayerData);
  SetLength(FPlayerData, I+1);
  FPlayerData[I].Item := Player;
  FPlayerData[I].Data := GetPlayerDataClass.Create(Self, Player);
  Result := TPlayerData(FPlayerData[I].Data);
end;

{-------------------------}
{ Classe TVisualComponent }
{-------------------------}

{*
  Crée une instance de TVisualComponent
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TVisualComponent.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID);

  FName := AName;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.ImgNames.BeginUpdate;
  FCachedImg := nil;
  FStaticDraw := True;
end;

{*
  Détruit l'instance
*}
destructor TVisualComponent.Destroy;
begin
  FCachedImg.Free;
  FPainter.Free;
  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TVisualComponent.AfterConstruction;
var
  Context: TDrawSquareContext;
begin
  inherited;
  FPainter.ImgNames.EndUpdate;

  if StaticDraw then
  begin
    FCachedImg := TSquareBitmap.Create;
    Context := TDrawSquareContext.Create(FCachedImg.Canvas);
    try
      PrivDraw(Context);
    finally
      Context.Free;
    end;
  end;
end;

{*
  Dessine le composant sur un canevas
  PrivDraw dessine le composant sur un canevas à la position indiquée.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.PrivDraw(Context: TDrawSquareContext);
begin
  DoDraw(Context);
end;

{*
  Dessine le composant sur un canevas
  DoDraw dessine le composant sur un canevas à la position indiquée.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.DoDraw(Context: TDrawSquareContext);
begin
  FPainter.Draw(Context);
end;

{*
  Dessine de façon optimisée le composant sur un canevas
  Draw dessine le composant sur un canevas à la position indiquée.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.Draw(Context: TDrawSquareContext);
begin
  if StaticDraw then
    FCachedImg.DrawSquare(Context)
  else
    PrivDraw(Context);
end;

{*
  Dessine de façon optimisée le composant sur un canevas
  Draw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TVisualComponent.Draw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
var
  Context: TDrawSquareContext;
begin
  Context := TDrawSquareContext.Create(Canvas, X, Y, QPos);
  try
    Draw(Context);
  finally
    Context.Free;
  end;
end;

{----------------}
{ Classe TPlugin }
{----------------}

{*
  Crée une instance de TPlugin
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du plug-in
*}
constructor TPlugin.Create(AMaster: TMaster; const AID: TComponentID);
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
destructor TPlugin.Destroy;
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
procedure TPlugin.AfterConstruction;
begin
  inherited;
  FPainterBefore.ImgNames.EndUpdate;
  FPainterAfter.ImgNames.EndUpdate;
end;

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TPlugin.DrawBefore(Context: TDrawSquareContext);
begin
  FPainterBefore.Draw(Context);
end;

{*
  Dessine sur le joueur
  DrawAfter est exécuté lors du dessin du joueur, après celui-ci. Le dessin
  effectué dans DrawAfter se retrouve donc sur le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TPlugin.DrawAfter(Context: TDrawSquareContext);
begin
  FPainterAfter.Draw(Context);
end;

{*
  Un joueur se déplace
  Moving est exécuté lorsqu'un joueur se déplace d'une case à une autre. Pour
  annuler le déplacement, Moving peut positionner le paramètre Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TPlugin.Moving(Context: TMoveContext);
begin
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Context   Contexte du déplacement
*}
procedure TPlugin.Moved(Context: TMoveContext);
begin
end;

{*
  Dessine la vue d'un joueur
  @param Context   Contexte de dessin de la vue
*}
procedure TPlugin.DrawView(Context: TDrawViewContext);
begin
end;

{*
  Presse une touche pour un joueur
  @param Context   Contexte de l'appui sur touche
*}
procedure TPlugin.PressKey(Context: TKeyEventContext);
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
function TPlugin.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := False;
end;

{-------------------}
{ Classe TObjectDef }
{-------------------}

{*
  [@inheritDoc]
*}
class function TObjectDef.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TObjectDefPlayerData;
end;

{*
  Nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @return Nombre d'objets que ce joueur possède
*}
function TObjectDef.GetCount(Player: TPlayer): Integer;
begin
  Result := TObjectDefPlayerData(GetPlayerData(Player)).FCount;
end;

{*
  Modifie le nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @param Value    Nouveau nombre d'objets
*}
procedure TObjectDef.SetCount(Player: TPlayer; Value: Integer);
begin
  TObjectDefPlayerData(GetPlayerData(Player)).FCount := Value;
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
*}
function TObjectDef.GetShownInfos(Player: TPlayer): string;
begin
  Result := Format(sDefaultObjectInfos, [Name, Count[Player]]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TObjectDef.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
*}
procedure TObjectDef.UseFor(Player: TPlayer; const Action: TPlayerAction);
begin
end;

{---------------}
{ Classe TField }
{---------------}

{*
  Crée une instance de TField
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TField.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADelegateDrawTo: TField = nil);
begin
  inherited Create(AMaster, AID, AName);
  FDelegateDrawTo := ADelegateDrawTo;
  if Assigned(FDelegateDrawTo) then
    FStaticDraw := FDelegateDrawTo.StaticDraw;
end;

{*
  [@inheritDoc]
*}
procedure TField.PrivDraw(Context: TDrawSquareContext);
begin
  if FDelegateDrawTo = nil then
    inherited
  else
    FDelegateDrawTo.Draw(Context);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TField.Entering(Context: TMoveContext);
begin
end;

{*
  Exécuté lorsque le joueur tente de sortir de la case
  Exiting est exécuté lorsque le joueur tente de sortir de la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TField.Exiting(Context: TMoveContext);
begin
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Context   Contexte du déplacement
*}
procedure TField.Entered(Context: TMoveContext);
begin
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Context   Contexte du déplacement
*}
procedure TField.Exited(Context: TMoveContext);
begin
end;

{----------------}
{ Classe TEffect }
{----------------}

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Context   Contexte du déplacement
*}
procedure TEffect.Entered(Context: TMoveContext);
begin
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Context   Contexte du déplacement
*}
procedure TEffect.Exited(Context: TMoveContext);
begin
end;

{*
  Exécute l'effet
  @param Context   Contexte du déplacement
*}
procedure TEffect.Execute(Context: TMoveContext);
begin
end;

{--------------}
{ Classe TTool }
{--------------}

{*
  Exécuté lorsque le joueur trouve l'outil
  Find est exécuté lorsque le joueur trouve l'outil. C'est-à-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Context   Contexte du déplacement
*}
procedure TTool.Find(Context: TMoveContext);
begin
end;

{------------------}
{ Classe TObstacle }
{------------------}

{*
  Exécuté lorsque le joueur pousse sur l'obstacle
  Pushing est exécuté lorsque le joueur pousse sur l'obstacle. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Execute de la case ne soit exécutée, il faut positionner
  AbortExecute à True.
  @param Context   Contexte du déplacement
*}
procedure TObstacle.Pushing(Context: TMoveContext);
begin
  Context.Cancel;
end;

{----------------}
{ Classe TSquare }
{----------------}

{*
  Crée une instance de TSquare
  @param AMaster     Maître FunLabyrinthe
  @param AID         ID de la case
  @param AName       Nom de la case
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TSquare.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; AField: TField; AEffect: TEffect; ATool: TTool;
  AObstacle: TObstacle);
begin
  inherited Create(AMaster, AID, AName);
  FStaticDraw := False;
  FField := AField;
  FEffect := AEffect;
  FTool := ATool;
  FObstacle := AObstacle;

  FStaticDraw :=
    ((not Assigned(FField)) or FField.StaticDraw) and
    ((not Assigned(FEffect)) or FEffect.StaticDraw) and
    ((not Assigned(FTool)) or FTool.StaticDraw) and
    ((not Assigned(FObstacle)) or FObstacle.StaticDraw);
end;

{*
  [@inheritDoc]
*}
procedure TSquare.DoDraw(Context: TDrawSquareContext);
begin
  if Assigned(Field) then
    Field.Draw(Context);
  if Assigned(Effect) then
    Effect.Draw(Context);
  if Assigned(Tool) then
    Tool.Draw(Context);
  if Assigned(Obstacle) then
    Obstacle.Draw(Context);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TSquare.DefaultHandler(var Msg);
begin
  if Assigned(Field) then
    Field.Dispatch(Msg);
  if Assigned(Effect) then
    Effect.Dispatch(Msg);
  if Assigned(Tool) then
    Tool.Dispatch(Msg);
  if Assigned(Obstacle) then
    Obstacle.Dispatch(Msg);
end;

{*
  Teste si cette case contient un composant donné
  Si le composant est une case entière, renvoie True si c'est cette case. Sinon,
  renvoie True si le sous-composant correspondant à la classe de Component
  (ex. : Field pour TField) est le composant indiqué.
  @param Component   Composant à tester
  @return True si cette case contient le composant spécifié, False sinon
*}
function TSquare.Contains(Component: TSquareComponent): Boolean;
begin
  if Component is TField then
    Result := Field = Component
  else if Component is TEffect then
    Result := Effect = Component
  else if Component is TTool then
    Result := Tool = Component
  else if Component is TObstacle then
    Result := Obstacle = Component
  else if Component is TSquare then
    Result := Self = Component
  else
    Result := False;
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TSquare.Entering(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Entering(Context);
end;

{*
  Exécuté lorsque le joueur tente de sortir de la case
  Exiting est exécuté lorsque le joueur tente de sortir de la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TSquare.Exiting(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Exiting(Context);
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Context   Contexte du déplacement
*}
procedure TSquare.Entered(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Entered(Context);
  if Assigned(Effect) then
    Effect.Entered(Context);
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Context   Contexte du déplacement
*}
procedure TSquare.Exited(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Exited(Context);
  if Assigned(Effect) then
    Effect.Exited(Context);
end;

{*
  Trouve l'objet et exécute l'effet
  @param Context   Contexte du déplacement
*}
procedure TSquare.Execute(Context: TMoveContext);
begin
  if Assigned(Tool) then
    Tool.Find(Context);
  if Assigned(Effect) then
    Effect.Execute(Context);
end;

{*
  Exécuté lorsque le joueur pousse sur l'obstacle
  Pushing est exécuté lorsque le joueur pousse sur l'obstacle. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Context   Contexte du déplacement
*}
procedure TSquare.Pushing(Context: TMoveContext);
begin
  if Assigned(Obstacle) then
    Obstacle.Pushing(Context);
end;

{-------------}
{ Classe TMap }
{-------------}

{*
  Crée une instance de TMap
  @param AMaster       Maître FunLabyrinthe
  @param AID           ID de la carte
  @param ADimensions   Dimensions de la carte (en cases)
  @param AZoneSize     Taille d'une zone de la carte
*}
constructor TMap.Create(AMaster: TMaster; const AID: TComponentID;
  ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer);
begin
  inherited Create(AMaster, AID);
  FDimensions := ADimensions;
  FZoneWidth := AZoneWidth;
  FZoneHeight := AZoneHeight;
  FMaxViewSize := MinViewSize;

  FOutsideOffset := FDimensions.X * FDimensions.Y * FDimensions.Z;
  SetLength(FMap, FOutsideOffset + FDimensions.Z);
  FillChar(FMap[0], Length(FMap)*4, 0);
end;

{*
  Modifie la taille maximale d'une vue pour cette carte
  @param Value   Nouvelle taille maximale
*}
procedure TMap.SetMaxViewSize(Value: Integer);
begin
  if Value < MinViewSize then
    FMaxViewSize := MinViewSize
  else
    FMaxViewSize := Value;
end;

{*
  Tableau des cases indexé par leur position sur la carte
  @param Position   Position sur la carte
  @return La case à la position spécifiée
*}
function TMap.GetMap(const Position: T3DPoint): TSquare;
var
  Index: Integer;
begin
  if InMap(Position) then
  begin
    Index := Position.Z;
    Index := Index * FDimensions.Y;
    Inc(Index, Position.Y);
    Index := Index * FDimensions.X;
    Inc(Index, Position.X);

    Result := FMap[Index];
  end else
    Result := Outside[Position.Z];
end;

{*
  Modifie le tableau des cases indexé par leur position sur la carte
  @param Position   Position sur la carte
  @param Value      Nouvelle case
*}
procedure TMap.SetMap(const Position: T3DPoint; Value: TSquare);
var
  Index: Integer;
begin
  if not InMap(Position) then
    Exit;

  Index := Position.Z;
  Index := Index * FDimensions.Y;
  Inc(Index, Position.Y);
  Index := Index * FDimensions.X;
  Inc(Index, Position.X);

  LinearMap[Index] := Value;
end;

{*
  Tableau des cases hors de la carte indexé par étage
  @param Floor   Étage
  @return La case hors de la carte à l'étage spécifié
*}
function TMap.GetOutside(Floor: Integer): TSquare;
begin
  if Floor < 0 then
    Floor := 0
  else if Floor >= FDimensions.Z then
    Floor := FDimensions.Z-1;
  Result := FMap[Floor + FOutsideOffset];
end;

{*
  Modifie le tableau des cases hors de la carte indexé par étage
  @param Floor   Étage
  @param Value   Nouvelle case
*}
procedure TMap.SetOutside(Floor: Integer; Value: TSquare);
begin
  if (Floor >= 0) and (Floor < FDimensions.Z) then
    LinearMap[Floor + FOutsideOffset] := Value;
end;

{*
  Taille du tableau de la carte linéaire
  @return Taille du tableau de la carte linéaire
*}
function TMap.GetLinearMapCount: Integer;
begin
  Result := Length(FMap);
end;

{*
  Tableau zero-based de la carte linéaire
  @param Index   Index dans le tableau
  @return Case de la carte linéaire à l'index spécifié
*}
function TMap.GetLinearMap(Index: Integer): TSquare;
begin
  Result := FMap[Index];
end;

{*
  Modifie le tableau zero-based de la carte linéaire
  @param Index   Index dans le tableau
  @param Value   Nouvelle case
*}
procedure TMap.SetLinearMap(Index: Integer; Value: TSquare);
begin
  FMap[Index] := Value;
end;

{*
  Teste si une coordonnée est à l'intérieur de la carte
  @param Position   Coordonnée à tester
  @return True si la coordonnée est dans la carte, False sinon
*}
function TMap.InMap(const Position: T3DPoint): Boolean;
begin
  Result :=
    (Position.X >= 0) and (Position.X < FDimensions.X) and
    (Position.Y >= 0) and (Position.Y < FDimensions.Y) and
    (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

{*
  Détermine le nombre de joueurs qui se trouvent à un point de la carte
  @param Position   Position de la carte où chercher les joueurs
  @return Nombre de joueurs qui se trouvent au point Position de la carte
*}
function TMap.PlayersOn(const Position: T3DPoint): Integer;
var
  I: Integer;
begin
  Result := 0;
  if IsNo3DPoint(Position) then
    Exit;
  for I := 0 to Master.PlayerCount-1 do
  begin
    if (Master.Players[I].Map = Self) and
      Same3DPoint(Master.Players[I].Position, Position) then
      Inc(Result);
  end;
end;

{-------------------}
{ TPlayerMode class }
{-------------------}

{*
  Crée le mode
  @param APlayer   Joueur
*}
constructor TPlayerMode.Create(APlayer: TPlayer);
begin
  inherited Create;

  FMaster := APlayer.Master;
  FPlayer := APlayer;
end;

{*
  [@inheritDoc]
*}
function TPlayerMode.GetModeClass: TPlayerModeClass;
begin
  Result := TPlayerModeClass(ClassType);
end;

{*
  [@inheritDoc]
*}
function TPlayerMode.GetPlayer: TPlayer;
begin
  Result := FPlayer;
end;

{*
  [@inheritDoc]
*}
function TPlayerMode.GetUseZone: Boolean;
begin
  Result := False;
end;

{----------------------------}
{ TLabyrinthPlayerMode class }
{----------------------------}

{*
  [@inheritDoc]
*}
constructor TLabyrinthPlayerMode.Create(APlayer: TPlayer);
begin
  inherited;

  FCacheBitmap := TBitmap.Create;
end;

{*
  [@inheritDoc]
*}
destructor TLabyrinthPlayerMode.Destroy;
begin
  FCacheBitmap.Free;

  inherited;
end;

{*
  Invalide entièrement la cache
*}
procedure TLabyrinthPlayerMode.InvalidateCache(Context: TDrawViewContext);
begin
  with Context do
  begin
    FOldMap := Map;
    FOldFloor := Floor;
    FOldZone := Zone;

    SetLength(FOldView, ZoneWidth * ZoneHeight);
    FillChar(FOldView[0], SizeOf(TSquare) * Length(FOldView), 0);

    FCacheBitmap.Width := ZoneWidth * SquareSize;
    FCacheBitmap.Height := ZoneHeight * SquareSize;
  end;
end;

{*
  Invalide entièrement la cache si nécessaire
  @param Context   Contexte de dessin de la vue
*}
procedure TLabyrinthPlayerMode.InvalidateCacheIfNeeded(
  Context: TDrawViewContext);
begin
  if (FOldMap <> Context.Map) or (FOldFloor <> Context.Floor) or
    (not SameRect(FOldZone, Context.Zone)) then
  begin
    InvalidateCache(Context);
  end;
end;

{*
  Met à jour la cache
  @param Context   Contexte de dessin de la vue
*}
procedure TLabyrinthPlayerMode.UpdateCache(Context: TDrawViewContext);
var
  QPos: TQualifiedPos;
  X, Y: Integer;
  Square: TSquare;
  DrawSquareContext: TDrawSquareContext;
begin
  InvalidateCacheIfNeeded(Context);

  with Context do
  begin
    QPos.Map := Map;
    QPos.Position.Z := Floor;

    for X := 0 to ZoneWidth-1 do
    begin
      for Y := 0 to ZoneHeight-1 do
      begin
        QPos.Position.X := Zone.Left + X;
        QPos.Position.Y := Zone.Top + Y;
        Square := Map[QPos.Position];

        if Square <> FOldView[Y*ZoneWidth + X] then
        begin
          DrawSquareContext := TDrawSquareContext.Create(FCacheBitmap.Canvas,
            X*SquareSize, Y*SquareSize, QPos);
          try
            DrawSquareContext.SetTickCount(Context.TickCount);
            Square.Draw(DrawSquareContext);
          finally
            DrawSquareContext.Free;
          end;

          if Square.StaticDraw then
            FOldView[Y*Width + X] := Square
          else
            FOldView[Y*Width + X] := nil;
        end;
      end;
    end;
  end;
end;

{*
  Dessine les joueurs
  @param Context   Contexte de dessin de la vue
*}
procedure TLabyrinthPlayerMode.DrawPlayers(Context: TDrawViewContext);
var
  I: Integer;
begin
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if Context.IsSquareVisible(Map, Position) then
      begin
        DrawInPlace(Context.Canvas,
          (Position.X-Context.Zone.Left) * SquareSize,
          (Position.Y-Context.Zone.Top) * SquareSize);
      end;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
function TLabyrinthPlayerMode.GetWidth: Integer;
begin
  Result := Player.Map.ZoneWidth + 2*Player.ViewBorderSize;
  Result := Result * SquareSize;
end;

{*
  [@inheritDoc]
*}
function TLabyrinthPlayerMode.GetHeight: Integer;
begin
  Result := Player.Map.ZoneHeight + 2*Player.ViewBorderSize;
  Result := Result * SquareSize;
end;

{*
  [@inheritDoc]
*}
function TLabyrinthPlayerMode.GetUseZone: Boolean;
begin
  Result := True;
end;

{*
  [@inheritDoc]
*}
procedure TLabyrinthPlayerMode.DrawView(Context: TDrawViewContext);
begin
  UpdateCache(Context);
  Context.Canvas.Draw(0, 0, FCacheBitmap);

  DrawPlayers(Context);
end;

{*
  [@inheritDoc]
*}
procedure TLabyrinthPlayerMode.PressKey(Context: TKeyEventContext);
var
  Dir: TDirection;
  Redo: Boolean;
begin
  if Context.Shift <> [] then
    Exit;

  case Context.Key of
    VK_UP: Dir := diNorth;
    VK_RIGHT: Dir := diEast;
    VK_DOWN: Dir := diSouth;
    VK_LEFT: Dir := diWest;
  else
    Exit;
  end;

  Context.Handled := True;

  Player.Move(Dir, True, Redo);
  if Redo then
    Player.NaturalMoving;
end;

{----------------}
{ Classe TPlayer }
{----------------}

{*
  Crée une instance de TPlayer
  @param AMaster     Maître FunLabyrinthe
  @param AID         ID du joueur
  @param AName       Nom du joueur
  @param AMap        Carte de départ
  @param APosition   Position de départ
*}
constructor TPlayer.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; AMap: TMap; const APosition: T3DPoint);
begin
  inherited Create(AMaster, AID, AName);

  FStaticDraw := False;
  FMap := AMap;
  FPosition := APosition;
  FDirection := diNone;
  FMode := TLabyrinthPlayerMode.Create(Self);
  FModeStack := TInterfaceList.Create;
  FShowCounter := 0;
  FColor := DefaultPlayerColor;
  FViewBorderSize := DefaultViewBorderSize;
  FPlugins := TObjectList.Create(False);
  FAttributes := THashedStringList.Create;
  TStringList(FAttributes).CaseSensitive := True;
  FOnSendCommand := nil;
  FPlayState := psPlaying;

  FLock := TMultiReadExclusiveWriteSynchronizer.Create;

  FKeyPressCoroutine := TCoroutine.Create(KeyPressCoroutineProc, clNextInvoke);
end;

{*
  [@inheritDoc]
*}
destructor TPlayer.Destroy;
begin
  FKeyPressCoroutine.Free;

  FLock.Free;

  FAttributes.Free;
  FPlugins.Free;
  
  inherited;
end;

{*
  Obtient une une liste des plug-in du joueur
  @param PluginList   Liste où enregistrer la liste des plug-in
*}
procedure TPlayer.GetPluginList(out PluginList: TPluginDynArray);
var
  I: Integer;
begin
  FLock.BeginRead;
  try
    SetLength(PluginList, FPlugins.Count);
    for I := 0 to FPlugins.Count-1 do
      PluginList[I] := TPlugin(FPlugins[I]);
  finally
    FLock.EndRead;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.PrivDraw(Context: TDrawSquareContext);
var
  I: Integer;
  Plugins: TPluginDynArray;
begin
  if not Visible then
    Exit;

  GetPluginList(Plugins);

  Context.SetPlayer(Self);
  try
    // Dessine les plug-in en-dessous du joueur
    for I := 0 to Length(Plugins)-1 do
      Plugins[I].DrawBefore(Context);

    // Dessin du joueur lui-même
    inherited;

    // Dessine les plug-in au-dessus du joueur
    for I := 0 to Length(Plugins)-1 do
      Plugins[I].DrawAfter(Context);
  finally
    Context.SetPlayer(nil);
  end;
end;

{*
  Teste si un déplacement est permis
  @param Context   Contexte de déplacement
  @return True si le déplacement est permis, False sinon
*}
function TPlayer.IsMoveAllowed(Context: TMoveContext): Boolean;
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  Result := False;

  with Context do
  begin
    SwitchToSrc;

    // Case source : exiting
    SrcSquare.Exiting(Context);
    if Cancelled then
      Exit;

    // Plug-in : moving
    GetPluginList(Plugins);
    for I := 0 to Length(Plugins)-1 do
      Plugins[I].Moving(Context);
    if Cancelled then
      Exit;

    SwitchToDest;

    // Case destination : entering
    DestSquare.Entering(Context);
    if Cancelled then
      Exit;

    // Case destination : pushing
    DestSquare.Pushing(Context);
    if Cancelled then
      Exit;
  end;

  Result := True;
end;

{*
  Déplace le joueur
  @param Context   Contexte de déplacement
  @param Execute   Indique s'il faut exécuter la case d'arrivée (défaut = True)
*}
procedure TPlayer.MoveTo(Context: TMoveContext; Execute: Boolean = True);
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  FLock.BeginWrite;
  try
    FMap := Context.DestMap;
    FPosition := Context.Dest;
    PositionChanged;
  finally
    FLock.EndWrite;
  end;

  with Context do
  begin
    SwitchToSrc;

    // Case source : exited
    SrcSquare.Exited(Context);

    SwitchToDest;

    // Plug-in : moved
    GetPluginList(Plugins);
    for I := 0 to Length(Plugins)-1 do
      Plugins[I].Moved(Context);

    // Case destination : entered
    DestSquare.Entered(Context);
  end;

  // Case destination : execute (seulement si Execute vaut True)
  if Execute then
    Map[Position].Execute(Context);
end;

{*
  Procédure de la coroutine d'appui sur touche
  @param Coroutine   Objet coroutine gérant cette procédure
*}
procedure TPlayer.KeyPressCoroutineProc(Coroutine: TCoroutine);
var
  Context: TKeyEventContext;
  Plugins: TPluginDynArray;
  I: Integer;
begin
  // FKeyPressKey and FKeyPressShift have been set before this method is called.

  Context := TKeyEventContext.Create(FKeyPressKey, FKeyPressShift);
  try
    GetPluginList(Plugins);
    for I := 0 to Length(Plugins)-1 do
    begin
      Plugins[I].PressKey(Context);
      if Context.Handled then
        Exit;
    end;

    Mode.PressKey(Context);
  finally
    Context.Free;
  end;
end;

{*
  Indique si le joueur est visible
  @return True s'il est visible, False sinon
*}
function TPlayer.GetVisible: Boolean;
begin
  Result := FShowCounter >= 0;
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.DoDraw(Context: TDrawSquareContext);
var
  Color: TColor;
begin
  Color := FColor;

  if Color <> clTransparent then
  begin
    with Context, Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+SquareSize-6, Y+SquareSize-6);
    end;
  end;
end;

{*
  Notification que la position a changé
*}
procedure TPlayer.PositionChanged;
begin
  if (Map <> nil) and (ViewBorderSize > Map.MaxViewSize) then
    ViewBorderSize := Map.MaxViewSize;
end;

{*
  Tableau indexé par chaîne des attributs du joueur
  @param AttrName   Nom de l'attribut à récupérer
  @return Attribut dont le nom a été spécifié
*}
function TPlayer.GetAttribute(const AttrName: string): Integer;
var
  Index: Integer;
begin
  FLock.BeginRead;
  try
    case AnsiIndexStr(AttrName,
      [attrColor, attrShowCounter, attrViewBorderSize]) of
      0: Result := FColor;
      1: Result := FShowCounter;
      2: Result := FViewBorderSize;
    else
      Index := FAttributes.IndexOf(AttrName);
      if Index < 0 then
        Result := 0
      else
        Result := Integer(FAttributes.Objects[Index]);
    end;
  finally
    FLock.EndRead;
  end;
end;

{*
  Modifie le tableau indexé par chaîne des attributs du joueur
  @param AttrName   Nom de l'attribut à modifier
  @param Value      Nouvelle valeur de l'attribut
*}
procedure TPlayer.SetAttribute(const AttrName: string; Value: Integer);
var
  Index: Integer;
begin
  FLock.BeginWrite;
  try
    case AnsiIndexStr(AttrName,
      [attrColor, attrShowCounter, attrViewBorderSize]) of
      0: FColor := Value;
      1: FShowCounter := Value;
      2: FViewBorderSize := MinMax(Value, MinViewSize, Map.MaxViewSize);
    else
      Index := FAttributes.IndexOf(AttrName);
      if Index < 0 then
      begin
        if Value <> 0 then
          FAttributes.AddObject(AttrName, TObject(Value));
      end else
      begin
        if Value = 0 then
          FAttributes.Delete(Index)
        else
          FAttributes.Objects[Index] := TObject(Value);
      end;
    end;
  finally
    FLock.EndWrite;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.Dispatch(var Msg);
begin
  TPlayerMessage(Msg).Handled := False;
  TPlayerMessage(Msg).Player := Self;
  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.DefaultHandler(var Msg);
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  GetPluginList(Plugins);
  for I := 0 to Length(Plugins)-1 do
  begin
    if TPlayerMessage(Msg).Handled then
      Exit;
      
    Plugins[I].Dispatch(Msg);
  end;
end;

{*
  Dresse la liste des attributs du joueur
  @param Attributes   Liste de chaînes dans laquelle enregistrer les attributs
*}
procedure TPlayer.GetAttributes(Attributes: TStrings);
begin
  FLock.BeginRead;
  try
    with Attributes do
    begin
      Assign(FAttributes);
      if FColor <> DefaultPlayerColor then
        AddObject(attrColor, TObject(FColor));
      if FShowCounter <> 0 then
        AddObject(attrShowCounter, TObject(FShowCounter));
      if FViewBorderSize <> DefaultViewBorderSize then
        AddObject(attrViewBorderSize, TObject(FViewBorderSize));
    end;
  finally
    FLock.EndRead;
  end;
end;

{*
  Dresse la liste des ID des plug-in du joueur
  @param Plugins   Liste de chaînes dans laquelle enregistrer les ID des plug-in
*}
procedure TPlayer.GetPluginIDs(PluginIDs: TStrings);
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  GetPluginList(Plugins);
  PluginIDs.Clear;
  for I := 0 to Length(Plugins)-1 do
    PluginIDs.AddObject(Plugins[I].ID, Plugins[I]);
end;

{*
  Dessine le joueur sur un canevas
  DrawInPlace dessine le joueur sur un canevas à la position indiquée, avec pour
  position qualifiée sa position actuelle.
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonnée X du point à partir duquel dessiner le joueur
  @param Y        Coordonnée Y du point à partir duquel dessiner le joueur
*}
procedure TPlayer.DrawInPlace(Canvas: TCanvas; X: Integer = 0;
  Y: Integer = 0);
var
  QPos: TQualifiedPos;
begin
  FLock.BeginRead;
  try
    QPos.Map := Map;
    QPos.Position := Position;
  finally
    FLock.EndRead;
  end;

  Draw(QPos, Canvas, X, Y);
end;

{*
  Modifie le mode principal du joueur (l'ancien est perdu)
  @param ModeClass   Classe du nouveau mode principal du joueur
*}
procedure TPlayer.ChangeMode(ModeClass: TPlayerModeClass);
begin
  FModeStack.Lock;
  try
    Assert(ModeClass <> nil);

    FMode := ModeClass.Create(Self);
  finally
    FModeStack.Unlock;
  end;
end;

{*
  Commence l'utilisation d'un mode temporaire
  L'ancien mode sera remis en place à l'appel de EndTempMode.
  @param ModeClass   Classe du nouveau mode principal du joueur
*}
procedure TPlayer.BeginTempMode(ModeClass: TPlayerModeClass);
begin
  FModeStack.Lock;
  try
    Assert(ModeClass <> nil);

    FModeStack.Add(FMode);
    FMode := ModeClass.Create(Self);
  finally
    FModeStack.Unlock;
  end;
end;

{*
  Termine l'utilisation du mode temporaire courant
*}
procedure TPlayer.EndTempMode;
begin
  FModeStack.Lock;
  try
    Assert(FModeStack.Count > 0);

    FMode := FModeStack.Last as IPlayerMode;
    FModeStack.Delete(FModeStack.Count-1);
  finally
    FModeStack.Unlock;
  end;
end;

{*
  Greffe un plug-in au joueur
  @param Plugin   Le plug-in à greffer
*}
procedure TPlayer.AddPlugin(Plugin: TPlugin);
var
  Index: Integer;
begin
  FLock.BeginWrite;
  try
    if FPlugins.IndexOf(Plugin) >= 0 then
      Exit;

    { Keep plug-in list ordered by z-index. }
    Index := 0;
    while (Index < FPlugins.Count) and
      (Plugin.ZIndex > TPlugin(FPlugins[Index]).ZIndex) do
      Inc(Index);
    FPlugins.Insert(Index, Plugin);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in à retirer
*}
procedure TPlayer.RemovePlugin(Plugin: TPlugin);
begin
  FLock.BeginWrite;
  try
    FPlugins.Remove(Plugin);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Indique si le joueur est capable d'effectuer une action donnée
  Un joueur est capable d'effectuer une action si l'un de ses plug-in ou l'un de
  ses objets le lui permet.
  @param Action   Action à tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TPlayer.AbleTo(const Action: TPlayerAction): Boolean;
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  Result := True;

  GetPluginList(Plugins);
  for I := 0 to Length(Plugins)-1 do
    if Plugins[I].AbleTo(Self, Action) then
      Exit;

  for I := 0 to Master.ObjectDefCount-1 do
    if Master.ObjectDefs[I].AbleTo(Self, Action) then
      Exit;

  Result := False;
end;

{*
  Exécute l'action spécifiée
  DoAction vérifie d'abord que le joueur en est bien capable. Et si plusieurs
  objets permettent d'effectuer l'action, le joueur se voit demander d'en
  choisir un.
  @param Action   Action à tester
  @return True si le joueur a été capable d'effectuer l'action, False sinon
*}
function TPlayer.DoAction(const Action: TPlayerAction): Boolean;
var
  Plugins: TPluginDynArray;
  I, GoodObjectCount: Integer;
  GoodObjects: array of TObjectDef;
  RadioTitles: array of string;
  GoodObject: TObjectDef;
begin
  Result := True;

  // Les plug-in ont la priorité, puisqu'ils n'ont pas d'effet de bord
  GetPluginList(Plugins);
  for I := 0 to Length(Plugins)-1 do
    if Plugins[I].AbleTo(Self, Action) then
      Exit;

  FLock.BeginWrite;
  try
    // Listage des objets susceptibles d'aider le joueur
    SetLength(GoodObjects, Master.ObjectDefCount);
    GoodObjectCount := 0;
    for I := 0 to Master.ObjectDefCount-1 do
    begin
      if Master.ObjectDefs[I].AbleTo(Self, Action) then
      begin
        GoodObjects[GoodObjectCount] := Master.ObjectDefs[I];
        Inc(GoodObjectCount);
      end;
    end;

    // Aucun objet trouvé : échec
    if GoodObjectCount = 0 then
    begin
      Result := False;
      Exit;
    end;

    // Si plusieurs objets, demande au joueur lequel utiliser
    if GoodObjectCount = 1 then
      GoodObject := GoodObjects[0]
    else
    begin
      SetLength(RadioTitles, GoodObjectCount);
      for I := 0 to GoodObjectCount-1 do
        RadioTitles[I] := GoodObjects[I].Name;
      I := 0;
      ShowDialogRadio(sWhichObject, sWhichObject, mtConfirmation,
        [mbOK], mrOk, RadioTitles, I, True);
      GoodObject := GoodObjects[I];
    end;

    // Utilisation de l'objet
    GoodObject.UseFor(Self, Action);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir          Direction du déplacement
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Redo         Indique s'il faut réitérer le déplacement
*}
procedure TPlayer.Move(Dir: TDirection; KeyPressed: Boolean;
  out Redo: Boolean);
var
  Context: TMoveContext;
begin
  Redo := False;

  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  Context := TMoveContext.Create(Self, PointBehind(Position, Dir), KeyPressed);
  try
    FDirection := Dir;

    if not IsMoveAllowed(Context) then
      Exit;

    if Same3DPoint(Position, Context.Src) and (Map = Context.SrcMap) then
      MoveTo(Context);

    Redo := Context.GoOnMoving;
  finally
    Context.Free;
  end;
end;

{*
  Déplace le joueur
  @param Dest      Position de destination
  @param Execute   Indique s'il faut exécuter la case d'arrivée
  @param Redo      Indique s'il faut réitérer le déplacement
*}
procedure TPlayer.MoveTo(const Dest: T3DPoint; Execute: Boolean;
  out Redo: Boolean);
var
  DestQPos: TQualifiedPos;
begin
  DestQPos.Map := Map;
  DestQPos.Position := Dest;

  MoveTo(DestQPos, Execute, Redo);
end;

{*
  Déplace le joueur
  Cette variante de MoveTo n'exécute pas la case d'arrivée
  @param Dest   Position de destination
*}
procedure TPlayer.MoveTo(const Dest: T3DPoint);
var
  Redo: Boolean;
begin
  MoveTo(Dest, False, Redo);
end;

{*
  Déplace le joueur
  @param Dest      Position de destination
  @param Execute   Indique s'il faut exécuter la case d'arrivée
  @param Redo      Indique s'il faut réitérer le déplacement
*}
procedure TPlayer.MoveTo(const Dest: TQualifiedPos; Execute: Boolean;
  out Redo: Boolean);
var
  Context: TMoveContext;
begin
  Redo := False;

  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  Context := TMoveContext.Create(Self, Dest, False);
  try
    MoveTo(Context, Execute);
    Redo := Context.GoOnMoving;
  finally
    Context.Free;
  end;
end;

{*
  Déplace le joueur
  Cette variante de MoveTo n'exécute pas la case d'arrivée
  @param Dest   Position de destination
*}
procedure TPlayer.MoveTo(const Dest: TQualifiedPos);
var
  Redo: Boolean;
begin
  MoveTo(Dest, False, Redo);
end;

{*
  Temporise l'exécution
*}
procedure TPlayer.Temporize;
begin
  Master.Temporize;
end;

{*
  Déplacement naturel, selon le mouvement déjà entamé (sorte d'inertie)
  Après un mouvement donné expressément, si Redo vaut True, il suffit d'appeler
  NaturalMoving pour continuer le mouvement normalement.
*}
procedure TPlayer.NaturalMoving;
var
  Redo: Boolean;
begin
  repeat
    Temporize;
    Move(Direction, False, Redo);
  until not Redo;
end;

{*
  Modifie la position sans interragir avec les cases
  ChangePosition ne doit être utilisée qu'en mode édition, ou sous réserve
  d'être certain de ce qu'on fait.
*}
procedure TPlayer.ChangePosition(AMap: TMap; const APosition: T3DPoint);
begin
  FLock.BeginWrite;
  try
    FMap := AMap;
    FPosition := APosition;
  finally
    FLock.EndWrite;
  end;
end;

{*
  Affiche le joueur
*}
procedure TPlayer.Show;
begin
  InterlockedIncrement(FShowCounter);
end;

{*
  Cache le joueur
*}
procedure TPlayer.Hide;
begin
  InterlockedDecrement(FShowCounter);
end;

{*
  Envoie une commande au joueur
  @param Command   Commande à envoyer
  @param Params    Paramètres de la commande
  @return Résultat de la commande
  @throws EUnsupportedCommand : La commande demandée n'est pas supportée
*}
function TPlayer.SendCommand(const Command: string;
  const Params: string = ''): string;
begin
  if Assigned(FOnSendCommand) then
    Result := FOnSendCommand(Self, Command, Params)
  else
    Result := '';
end;

{*
  Affiche une boîte de dialogue
  @param Title        Titre de la boîte de dialogue
  @param Text         Texte de la boîte de dialogue
  @param DlgType      Type de boîte de dialogue
  @param DlgButtons   Boutons présents dans la boîte de dialogue
  @param DefButton    Bouton sélectionné par défaut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Bouton sur lequel l'utilisateur a cliqué
*}
function TPlayer.ShowDialog(const Title, Text: string;
  DlgType: TDialogType = dtInformation; DlgButtons: TDialogButtons = dbOK;
  DefButton: Byte = 1; AddFlags: LongWord = 0): TDialogResult;
var
  Params: string;
begin
  Params := StrToStrRepres(Title);
  Params := Params + #10 + StrToStrRepres(Text);
  Params := Params + #10 + GetEnumName(
    TypeInfo(TDialogType), Integer(DlgType));
  Params := Params + #10 + GetEnumName(
    TypeInfo(TDialogButtons), Integer(DlgButtons));
  Params := Params + #10 + IntToStr(DefButton);
  Params := Params + #10 + IntToStr(AddFlags);

  Result := TDialogResult(GetEnumValue(TypeInfo(TDialogResult),
    SendCommand(CommandShowDialog, Params)));
end;

{*
  Affiche une boîte de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title         Titre de la boîte de dialogue
  @param Text          Texte de la boîte de dialogue
  @param DlgType       Type de boîte de dialogue
  @param DlgButtons    Boutons présents dans la boîte de dialogue
  @param DefButton     Bouton sélectionné par défaut
  @param RadioTitles   Libellés des différents boutons radio
  @param Selected      Bouton radio sélectionné
  @param OverButtons   Boutons radio placés au-dessus des boutons si True
  @return Bouton sur lequel l'utilisateur a cliqué
*}
function TPlayer.ShowDialogRadio(const Title, Text: string;
  DlgType: TMsgDlgType; DlgButtons: TMsgDlgButtons; DefButton: TModalResult;
  const RadioTitles: array of string; var Selected: Integer;
  OverButtons: Boolean = False): Word;
var
  Params, CmdResult: string;
  I: Integer;
begin
  Params := StrToStrRepres(Title);
  Params := Params + #10 + StrToStrRepres(Text);
  Params := Params + #10 + GetEnumName(TypeInfo(TMsgDlgType), Integer(DlgType));
  Params := Params + #10 + EnumSetToStr(DlgButtons, TypeInfo(TMsgDlgButtons));
  Params := Params + #10 + IntToStr(DefButton);
  Params := Params + #10 + IntToStr(Length(RadioTitles));
  for I := Low(RadioTitles) to High(RadioTitles) do
    Params := Params + #10 + StrToStrRepres(RadioTitles[I]);
  Params := Params + #10 + IntToStr(Selected);
  Params := Params + #10 + GetEnumName(TypeInfo(Boolean), Integer(OverButtons));

  CmdResult := SendCommand(CommandShowDialogRadio, Params);
  Selected := StrToInt(GetFirstToken(CmdResult, ' '));
  Result := StrToInt(GetLastToken(CmdResult, ' '));
end;

{*
  Affiche une invite au joueur lui demandant de choisir un nombre
  @param Title     Titre de la boîte de dialogue
  @param Prompt    Invite
  @param Default   Valeur par défaut affichée
  @param Min       Valeur minimale que peut choisir le joueur
  @param Max       Valeur maximale que peut choisir le joueur
  @return La valeur qu'a choisie le joueur
*}
function TPlayer.ChooseNumber(const Title, Prompt: string;
  Default, Min, Max: Integer): Integer;
var
  Params: string;
begin
  Params := StrToStrRepres(Title);
  Params := Params + #10 + StrToStrRepres(Prompt);
  Params := Params + #10 + IntToStr(Default);
  Params := Params + #10 + IntToStr(Min);
  Params := Params + #10 + IntToStr(Max);

  Result := StrToInt(SendCommand(CommandChooseNumber, Params));
end;

{*
  Affiche un message au joueur
  @param Text   Texte à afficher au joueur
*}
procedure TPlayer.ShowMessage(const Text: string);
var
  Msg: TPlayerShowMsgMessage;
begin
  Msg.MsgID := msgShowMessage;
  Msg.Text := Text;
  Dispatch(Msg);
end;

{*
  Fait gagner le joueur
*}
procedure TPlayer.Win;
var
  I: Integer;
begin
  FLock.BeginWrite;
  try
    if FPlayState <> psPlaying then
      Exit;

    // Ce joueur a gagné
    FPlayState := psWon;

    // Les autres joueurs ont perdu
    for I := 0 to Master.PlayerCount-1 do
      if Master.Players[I] <> Self then
        Master.Players[I].FPlayState := psLost;

    // La partie est terminée
    Master.Terminate;
  finally
    FLock.EndWrite;
  end;
end;

{*
  Fait perdre le joueur
*}
procedure TPlayer.Lose;
var
  I: Integer;
begin
  FLock.BeginWrite;
  try
    if FPlayState <> psPlaying then
      Exit;

    // Ce joueur a perdu
    FPlayState := psLost;

    // Si plus aucun joueur ne joue, la partie est terminée
    for I := 0 to Master.PlayerCount-1 do
      if Master.Players[I].PlayState = psPlaying then
        Exit;
    Master.Terminate;
  finally
    FLock.EndWrite;
  end;
end;

{*
  Dessine la vue du joueur
  @param Canvas   Canevas cible (doit correspondre à Width/Height)
*}
procedure TPlayer.DrawView(Canvas: TCanvas);
var
  Context: TDrawViewContext;
  Plugins: TPluginDynArray;
  I: Integer;
begin
  Context := TDrawViewContext.Create(Mode, Canvas);
  try
    Mode.DrawView(Context);

    GetPluginList(Plugins);
    for I := 0 to Length(Plugins)-1 do
      Plugins[I].DrawView(Context);
  finally
    Context.Free;
  end;
end;

{*
  Presse une touche pour le joueur
  Cette méthode ne peut *pas* être appelée depuis le code d'action du joueur !
  @param Key     Touche pressée
  @param Shift   État des touches spéciales
*}
procedure TPlayer.PressKey(Key: Word; Shift: TShiftState);
begin
  Assert(not FKeyPressCoroutine.CoroutineRunning);

  FKeyPressKey := Key;
  FKeyPressShift := Shift;

  try
    FKeyPressCoroutine.Invoke;
  except
    FKeyPressCoroutine.Reset;
    raise;
  end;
end;

{*
  Attend qu'une touche soit pressée par le joueur
  Cette méthode ne peut être appelée que depuis le code d'action du joueur !
  @param Key     En sortie : Touche appuyée
  @param Shift   En sortie : État des touches spéciales lors de l'appui
*}
procedure TPlayer.WaitForKey(out Key: Word; out Shift: TShiftState);
begin
  Assert(FKeyPressCoroutine.CoroutineRunning);

  FKeyPressCoroutine.Yield;

  Key := FKeyPressKey;
  Shift := FKeyPressShift;
end;

{*
  Attend qu'une touche spécifique soit pressée par le joueur
  Cette méthode ne peut être appelée que depuis le code d'action du joueur !
  @param Key     Touche que doit appuyer le joueur
  @param Shift   État des touches spéciales requis
*}
procedure TPlayer.WaitForSpecificKey(Key: Word; Shift: TShiftState = []);
var
  AKey: Word;
  AShift: TShiftState;
begin
  repeat
    WaitForKey(AKey, AShift);
  until (AKey = Key) and (AShift = Shift);
end;

{----------------}
{ Classe TMaster }
{----------------}

{*
  Crée une instance de TMaster
*}
constructor TMaster.Create(AEditing: Boolean);
begin
  inherited Create;

  FImagesMaster := TImagesMaster.Create;
  FComponents := THashedStringList.Create;
  with TStringList(FComponents) do
  begin
    CaseSensitive := True;
    Duplicates := dupError;
  end;

  FPlugins    := TObjectList.Create(False);
  FObjectDefs := TObjectList.Create(False);
  FFields     := TObjectList.Create(False);
  FEffects    := TObjectList.Create(False);
  FTools      := TObjectList.Create(False);
  FObstacles  := TObjectList.Create(False);
  FSquares    := TObjectList.Create(False);
  FMaps       := TObjectList.Create(False);
  FPlayers    := TObjectList.Create(False);

  FEditing := AEditing;
  FTemporization := DefaultTemporization;
  FBeginTickCount := Windows.GetTickCount;
  FTerminated := False;
end;

{*
  Détruit l'instance
*}
destructor TMaster.Destroy;
var
  I: Integer;
begin
  if Assigned(FComponents) then
  begin
    for I := FComponents.Count-1 downto 0 do
      FComponents.Objects[I].Free;
  end;

  FPlayers.Free;
  FMaps.Free;
  FSquares.Free;
  FObstacles.Free;
  FTools.Free;
  FEffects.Free;
  FFields.Free;
  FObjectDefs.Free;
  FPlugins.Free;

  FComponents.Free;
  FImagesMaster.Free;

  inherited;
end;

{*
  Tableau des composants indexé par leur ID
  @param ID   ID du composant à trouver
  @return Le composant dont l'ID a été spécifié, ou nil si ID était vide
  @throws EComponentNotFound : Aucun composant ne correspond à l'ID spécifié
*}
function TMaster.GetComponent(const ID: TComponentID): TFunLabyComponent;
var
  Index: Integer;
begin
  if ID = '' then
    Result := nil
  else
  begin
    Index := FComponents.IndexOf(ID);
    if Index >= 0 then
      Result := TFunLabyComponent(FComponents.Objects[Index])
    else
      raise EComponentNotFound.CreateFmt(sComponentNotFound, [ID]);
  end;
end;

{*
  Tableau des composants de case indexé par leur ID
  @param ID   ID du composant à trouver
  @return Le composant dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun composant ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas de case
*}
function TMaster.GetSquareComponent(const ID: TComponentID): TSquareComponent;
begin
  try
    Result := Component[ID] as TSquareComponent;
  except
    on Error: EComponentNotFound do
    begin
      if NberCharInStr(SquareIDDelim, ID) = 3 then
        Result := Square[ID]
      else
        raise;
    end;
  end;
end;

{*
  Tableau des plug-in indexé par leur ID
  @param ID   ID du plug-in à trouver
  @return Le plug-in dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun plug-in ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas un plug-in
*}
function TMaster.GetPlugin(const ID: TComponentID): TPlugin;
begin
  Result := Component[ID] as TPlugin;
end;

{*
  Tableau des définitions d'objet indexé par leur ID
  @param ID   ID de la définition d'objet à trouver
  @return La définition d'objet dont l'ID a été spécifié
  @throws EComponentNotFound : Aucune définition ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas une def d'objet
*}
function TMaster.GetObjectDef(const ID: TComponentID): TObjectDef;
begin
  Result := Component[ID] as TObjectDef;
end;

{*
  Tableau des terrains indexé par leur ID
  @param ID   ID du terrain à trouver
  @return Le terrain dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun terrain ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas un terrain
*}
function TMaster.GetField(const ID: TComponentID): TField;
begin
  Result := Component[ID] as TField;
end;

{*
  Tableau des effets de case indexé par leur ID
  @param ID   ID de l'effet à trouver
  @return L'effet dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun effet ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas un effet
*}
function TMaster.GetEffect(const ID: TComponentID): TEffect;
begin
  Result := Component[ID] as TEffect;
end;

{*
  Tableau des outils indexé par leur ID
  @param ID   ID de l'outil à trouver
  @return L'outil dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun outil ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas un outil
*}
function TMaster.GetTool(const ID: TComponentID): TTool;
begin
  Result := Component[ID] as TTool;
end;

{*
  Tableau des obstacles indexé par leur ID
  @param ID   ID de l'obstacle à trouver
  @return L'obstacle dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun obstacle ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas un obstacle
*}
function TMaster.GetObstacle(const ID: TComponentID): TObstacle;
begin
  Result := Component[ID] as TObstacle;
end;

{*
  Tableau des cases indexé par leur ID
  Si la case n'a pas pu être trouvée, GetSquare essaye de trouver les terrain
  et effet correspondant à son ID et de créer la case automatiquement
  @param ID   ID de la case à trouver
  @return La case dont l'ID a été spécifié
  @throws EComponentNotFound : Aucune case ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas une case
*}
function TMaster.GetSquare(const ID: TComponentID): TSquare;
var
  AField: TField;
  AEffect: TEffect;
  ATool: TTool;
  AObstacle: TObstacle;
  AName: string;
begin
  try
    Result := Component[ID] as TSquare;
  except
    on Error: EComponentNotFound do
    begin
      Result := nil;

      if NberCharInStr(SquareIDDelim, ID) = 3 then
      try
        AField    := Field   [GetXToken(ID, SquareIDDelim, 1)];
        AEffect   := Effect  [GetXToken(ID, SquareIDDelim, 2)];
        ATool     := Tool    [GetXToken(ID, SquareIDDelim, 3)];
        AObstacle := Obstacle[GetXToken(ID, SquareIDDelim, 4)];

        if Assigned(AField) then
          AName := AField.Name
        else
          AName := sNothing;
        if Assigned(AEffect) then
          AName := Format(sEffectName, [AName, AEffect.Name]);
        if Assigned(ATool) then
          AName := Format(sToolName, [AName, ATool.Name]);
        if Assigned(AObstacle) then
          AName := Format(sObstacleName, [AName, AObstacle.Name]);

        Result := TSquare.Create(Self, ID, AName,
          AField, AEffect, ATool, AObstacle);
      except
      end;

      if Result = nil then
        raise;
    end;
  end;
end;

{*
  Tableau des cartes indexé par leur ID
  @param ID   ID de la carte à trouver
  @return La carte dont l'ID a été spécifié
  @throws EComponentNotFound : Aucune carte ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas une carte
*}
function TMaster.GetMap(const ID: TComponentID): TMap;
begin
  Result := Component[ID] as TMap;
end;

{*
  Tableau des joueurs indexé par leur ID
  @param ID   ID du joueur à trouver
  @return Le joueur dont l'ID a été spécifié
  @throws EComponentNotFound : Aucun joueur ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas un joueur
*}
function TMaster.GetPlayer(const ID: TComponentID): TPlayer;
begin
  Result := Component[ID] as TPlayer;
end;

{*
  Nombre de plug-in
  @return Nombre de plug-in
*}
function TMaster.GetPluginCount: Integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in
  @param Index   Index du plug-in
  @return Le plug-in à la position spécifiée
*}
function TMaster.GetPlugins(Index: Integer): TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Nombre de définitions d'objet
  @return Nombre de définitions d'objet
*}
function TMaster.GetObjectDefCount: Integer;
begin
  Result := FObjectDefs.Count;
end;

{*
  Tableau zero-based des définitions d'objet
  @param Index   Index de la définition d'objet
  @return La définition d'objet à la position spécifiée
*}
function TMaster.GetObjectDefs(Index: Integer): TObjectDef;
begin
  Result := TObjectDef(FObjectDefs[Index]);
end;

{*
  Nombre de terrains
  @return Nombre de terrains
*}
function TMaster.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

{*
  Tableau zero-based des terrains
  @param Index   Index du terrain
  @return Le terrain à la position spécifiée
*}
function TMaster.GetFields(Index: Integer): TField;
begin
  Result := TField(FFields[Index]);
end;

{*
  Nombre d'effets
  @return Nombre d'effets
*}
function TMaster.GetEffectCount: Integer;
begin
  Result := FEffects.Count;
end;

{*
  Tableau zero-based des effets
  @param Index   Index de l'effet
  @return L'effet à la position spécifiée
*}
function TMaster.GetEffects(Index: Integer): TEffect;
begin
  Result := TEffect(FEffects[Index]);
end;

{*
  Nombre d'outils
  @return Nombre d'outils
*}
function TMaster.GetToolCount: Integer;
begin
  Result := FTools.Count;
end;

{*
  Tableau zero-based des outils
  @param Index   Index de l'outil
  @return L'outil à la position spécifiée
*}
function TMaster.GetTools(Index: Integer): TTool;
begin
  Result := TTool(FTools[Index]);
end;

{*
  Nombre d'obstacles
  @return Nombre d'obstacles
*}
function TMaster.GetObstacleCount: Integer;
begin
  Result := FObstacles.Count;
end;

{*
  Tableau zero-based des obstacles
  @param Index   Index de l'obstacle
  @return L'obstacle à la position spécifiée
*}
function TMaster.GetObstacles(Index: Integer): TObstacle;
begin
  Result := TObstacle(FObstacles[Index]);
end;

{*
  Nombre de cases
  @return Nombre de cases
*}
function TMaster.GetSquareCount: Integer;
begin
  Result := FSquares.Count;
end;

{*
  Tableau zero-based des cases
  @param Index   Index de la case
  @return La case à la position spécifiée
*}
function TMaster.GetSquares(Index: Integer): TSquare;
begin
  Result := TSquare(FSquares[Index]);
end;

{*
  Nombre de cartes
  @return Nombre de cartes
*}
function TMaster.GetMapCount: Integer;
begin
  Result := FMaps.Count;
end;

{*
  Tableau zero-based des cartes
  @param Index   Index de la carte
  @return La carte à la position spécifiée
*}
function TMaster.GetMaps(Index: Integer): TMap;
begin
  Result := TMap(FMaps[Index]);
end;

{*
  Nombre de joueurs
  @return Nombre de joueurs
*}
function TMaster.GetPlayerCount: Integer;
begin
  Result := FPlayers.Count;
end;

{*
  Tableau zero-based des joueurs
  @param Index   Index du joueur
  @return Le joueur à la position spécifiée
*}
function TMaster.GetPlayers(Index: Integer): TPlayer;
begin
  Result := TPlayer(FPlayers[Index]);
end;

{*
  Modifie la temporisation
  @param Value   Nouvelle temporisation en millisecondes
*}
procedure TMaster.SetTemporization(Value: Integer);
begin
  if Value > 0 then
    FTemporization := Value;
end;

{*
  Tick count de la partie
  @return Tick count de la partie
*}
function TMaster.GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount - FBeginTickCount;
end;

{*
  Ajoute un composant
  @param Component   Le composant à ajouter
*}
procedure TMaster.AddComponent(Component: TFunLabyComponent);
begin
  FComponents.AddObject(Component.ID, Component);

  if Component is TPlugin then
    FPlugins.Add(Component)
  else if Component is TObjectDef then
    FObjectDefs.Add(Component)
  else if Component is TField then
    FFields.Add(Component)
  else if Component is TEffect then
    FEffects.Add(Component)
  else if Component is TTool then
    FTools.Add(Component)
  else if Component is TObstacle then
    FObstacles.Add(Component)
  else if Component is TSquare then
    FSquares.Add(Component)
  else if Component is TMap then
    FMaps.Add(Component)
  else if Component is TPlayer then
    FPlayers.Add(Component);
end;

{*
  Retire un composant
  @param Component   Le composant à retirer
*}
procedure TMaster.RemoveComponent(Component: TFunLabyComponent);
begin
  if Component is TPlugin then
    FPlugins.Remove(Component)
  else if Component is TObjectDef then
    FObjectDefs.Remove(Component)
  else if Component is TField then
    FFields.Remove(Component)
  else if Component is TEffect then
    FEffects.Remove(Component)
  else if Component is TTool then
    FTools.Remove(Component)
  else if Component is TObstacle then
    FObstacles.Remove(Component)
  else if Component is TSquare then
    FSquares.Remove(Component)
  else if Component is TMap then
    FMaps.Remove(Component)
  else if Component is TPlayer then
    FPlayers.Remove(Component);

  FComponents.Delete(FComponents.IndexOf(Component.ID));
end;

{*
  Met fin à la partie
*}
procedure TMaster.Terminate;
begin
  FTerminated := True;
end;

{*
  Temporise l'exécution
*}
procedure TMaster.Temporize;
begin
  Sleep(Temporization);
end;

{*
  Obtient une case à partir des ID de ses composantes
  @param Field      ID du terrain
  @param Effect     ID de l'effet
  @param Tool       ID de l'outil
  @param Obstacle   ID de l'obstacle
  @return Case avec avec les composantes spécifiées
*}
function TMaster.SquareByComps(
  const Field, Effect, Tool, Obstacle: TComponentID): TSquare;
begin
  Result := Square[Format(SquareIDFormat, [Field, Effect, Tool, Obstacle])];
end;

initialization
  Randomize;
  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    fFunLabyAppData :=
      ReadString('Directories', 'AppData', Dir); {don't localize}

    fSquaresDir := fFunLabyAppData + fSquaresDir;
    fSoundsDir := fFunLabyAppData + fSoundsDir;
    FUnitsDir := fFunLabyAppData + fUnitsDir;
    FMapsDir := fFunLabyAppData + fMapsDir;
    fLabyrinthsDir := fFunLabyAppData + fLabyrinthsDir;
    fSaveguardsDir := fFunLabyAppData + fSaveguardsDir;
    fEditPluginDir := fFunLabyAppData + fEditPluginDir;

    fSquareFileName := fSquaresDir+fSquareFileName;
  finally
    Free;
  end;
end.

