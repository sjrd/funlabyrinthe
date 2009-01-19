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
  TypInfo, ScUtils, SdDialogs;

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

  ScrewSize = 30;         /// Taille (en largeur et hauteur) d'une case
  MinViewSize = 1;        /// Taille minimale d'une vue
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp
  NoRefCount = MaxInt;    /// Valeur sentinelle : pas de comptage des références

  attrColor = 'Color';             /// Attribut de joueur pour Color
  attrShowCounter = 'ShowCounter'; /// Attribut de joueur pour ShowCounter

  CommandShowDialog = 'ShowDialog';           /// Commande ShowDialog
  CommandShowDialogRadio = 'ShowDialogRadio'; /// Commande ShowDialogRadio
  CommandChooseNumber = 'ChooseNumber';       /// Commande ChooseNumber

  ScrewIDDelim = '-';            /// Délimiteur des parties d'un ID de case
  ScrewIDFormat = '%s-%s-%s-%s'; /// Format d'un ID de case

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type représentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Type représentant une action
  TPlayerAction = type string;

  /// État de victoire/défaite d'un joueur
  TPlayState = (psPlaying, psWon, psLost);

  /// Générée si un composant recherché n'est pas trouvé
  EComponentNotFound = class(Exception);

  /// Générée si une commande n'est pas supportée
  EUnsupportedCommand = class(Exception);

  TScrewComponent = class;
  TPlayer = class;
  TMap = class;
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
    Component: TScrewComponent) of object; stdcall;

  {*
    Type de méthode call-back pour l'enregistrement d'un ensemble de composants
    @param Template       Composant modèle pour l'image et le nom à afficher
    @param Components     Liste des composants faisant partie de l'ensemble
    @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
    @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
  *}
  TRegisterComponentSetProc = procedure(Template: TScrewComponent;
    const Components: array of TScrewComponent; BaseIndex: Integer;
    const DialogTitle, DialogPrompt: string) of object; stdcall;

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
  TScrewBitmap = class(TBitmap)
  public
    constructor Create; override;

    procedure EmptyScrew;
    procedure DrawScrew(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
  end;

  {*
    Enregistre est affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une méthode
    pour les dessiner les unes sur les autres, par transparence.
    @author sjrd
    @version 5.0
  *}
  TPainter = class
  private
    FMaster: TImagesMaster;   /// Maître d'images
    FImgNames: TStrings;      /// Liste des noms des images
    FCachedImg: TScrewBitmap; /// Copie cache de l'image résultante

    procedure ImgNamesChange(Sender: TObject);
  public
    constructor Create(AMaster: TImagesMaster);
    destructor Destroy; override;

    procedure Draw(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);

    property ImgNames: TStrings read FImgNames;
  end;

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

    function GetSafeID: TComponentID;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
    destructor Destroy; override;

    property Master: TMaster read FMaster;
    property ID: TComponentID read FID;
    property SafeID: TComponentID read GetSafeID;
    property Tag: Integer read FTag write FTag;
  end;

  {*
    Classe de base pour les composants devant être affichés
    TVisualComponent étend la classe TFunLabyComponent pour lui ajouter un
    traitement standart et simple de nommage et de dessin.
    @author sjrd
    @version 5.0
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName: string;            /// Nom du composant
    FPainter: TPainter;       /// Peintre par défaut
    FCachedImg: TScrewBitmap; /// Image en cache (pour les dessins invariants)

    procedure PrivDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); virtual;
  protected
    FStaticDraw: Boolean; /// Indique si le dessin du composant est invariant

    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); virtual;

    property Painter: TPainter read FPainter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0);

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
    property PainterBefore: TPainter read FPainterBefore;
    property PainterAfter: TPainter read FPainterAfter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
      Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); virtual;
    procedure DrawAfter(Player: TPlayer; const QPos: TQualifiedPos;
      Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); virtual;

    procedure Moving(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Dest: T3DPoint;
      var Cancel: Boolean); virtual;
    procedure Moved(Player: TPlayer; const Src, Dest: T3DPoint); virtual;

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; virtual;
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
  TScrewComponent = class(TVisualComponent)
  end;

  {*
    Classe de base pour les terrains
    TField est la classe de base pour la création de terrains. Les terrains
    sont la première composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TField = class(TScrewComponent)
  private
    FDelegateDrawTo: TField; /// Terrain délégué pour l'affichage

    procedure PrivDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADelegateDrawTo: TField = nil);

    procedure Entering(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel: Boolean); virtual;
    procedure Exiting(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Pos, Dest: T3DPoint;
      var Cancel: Boolean); virtual;

    procedure Entered(Player: TPlayer; const Src, Pos: T3DPoint); virtual;
    procedure Exited(Player: TPlayer; const Pos, Dest: T3DPoint); virtual;
  end;

  {*
    Classe de base pour les effets de case
    TEffect est la classe de base pour la création d'effets de case. Les effets
    sont la deuxième composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TEffect = class(TScrewComponent)
  public
    procedure Entered(Player: TPlayer; const Src, Pos: T3DPoint); virtual;
    procedure Exited(Player: TPlayer; const Pos, Dest: T3DPoint); virtual;

    procedure Execute(Player: TPlayer; const Pos: T3DPoint;
      var GoOnMoving: Boolean); virtual;
  end;

  {*
    Classe de base pour les outils
    TTool est la classe de base pour la création d'outils. Les outils sont la
    troisième composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TTool = class(TScrewComponent)
  public
    procedure Find(Player: TPlayer; const Pos: T3DPoint); virtual;
  end;

  {*
    Classe de base pour les obstacles
    TObstacle est la classe de base pour la création d'obstacles. Les obstacles
    sont la quatrième composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TObstacle = class(TScrewComponent)
  public
    procedure Pushing(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel, AbortExecute: Boolean); virtual;
  end;

  {*
    Représente une case du jeu
    TScrew représente une case du jeu. Une case possède quatre composantes : le
    terrain, l'effet, l'outil et l'obstacle. Chacune de ces composantes est
    optionnelle.
    @author sjrd
    @version 5.0
  *}
  TScrew = class(TScrewComponent)
  private
    FField: TField;       /// Terrain
    FEffect: TEffect;     /// Effet
    FTool: TTool;         /// Outil
    FObstacle: TObstacle; /// Obstacle
  protected
    FRefCount: Integer; /// Compteur de références

    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; AField: TField; AEffect: TEffect; ATool: TTool;
      AObstacle: TObstacle);
    procedure BeforeDestruction; override;

    procedure DefaultHandler(var Msg); override;

    procedure Entering(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel: Boolean); virtual;
    procedure Exiting(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Pos, Dest: T3DPoint;
      var Cancel: Boolean); virtual;

    procedure Entered(Player: TPlayer; const Src, Pos: T3DPoint); virtual;
    procedure Exited(Player: TPlayer; const Pos, Dest: T3DPoint); virtual;

    procedure Execute(Player: TPlayer; const Pos: T3DPoint;
      var GoOnMoving: Boolean); virtual;

    procedure Pushing(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel, AbortExecute: Boolean); virtual;

    function AddRef: Integer; virtual;
    function Release: Integer; virtual;

    property Field: TField read FField;
    property Effect: TEffect read FEffect;
    property Tool: TTool read FTool;
    property Obstacle: TObstacle read FObstacle;

    property RefCount: Integer read FRefCount;
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
    FMap: array of TScrew;   /// Carte stockée de façon linéaire
    FOutsideOffset: Integer; /// Offset de départ de l'extérieur

    procedure SetMaxViewSize(Value: Integer);

    function GetMap(const Position: T3DPoint): TScrew;
    procedure SetMap(const Position: T3DPoint; Value: TScrew);

    function GetOutside(Floor: Integer): TScrew;
    procedure SetOutside(Floor: Integer; Value: TScrew);

    function GetLinearMapCount: Integer;
    function GetLinearMap(Index: Integer): TScrew;
    procedure SetLinearMap(Index: Integer; Value: TScrew);
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer);

    function InMap(const Position: T3DPoint): Boolean;

    function PlayersOn(const Position: T3DPoint): Integer;

    property Dimensions: T3DPoint read FDimensions;
    property ZoneWidth: Integer read FZoneWidth;
    property ZoneHeight: Integer read FZoneHeight;
    property MaxViewSize: Integer read FMaxViewSize write SetMaxViewSize;

    property Map[const Position: T3DPoint]: TScrew
      read GetMap write SetMap; default;

    property Outside[Floor: Integer]: TScrew
      read GetOutside write SetOutside;

    property LinearMapCount: Integer read GetLinearMapCount;
    property LinearMap[Index: Integer]: TScrew
      read GetLinearMap write SetLinearMap;
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
    FShowCounter: Integer;             /// Compteur de visibilité
    FColor: TColor;                    /// Couleur
    FPlugins: TObjectList;             /// Liste des plug-in
    FAttributes: TStrings;             /// Liste des attributs
    FOnSendCommand: TSendCommandEvent; /// Événement d'exécution de commande
    FPlayState: TPlayState;            /// État de victoire/défaite

    procedure PrivDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;

    function GetVisible: Boolean;

    function GetPluginCount: Integer;
    function GetPlugins(Index: Integer): TPlugin;

    property PluginCount: Integer read GetPluginCount;
    property Plugins[Index: Integer]: TPlugin read GetPlugins;
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;

    function GetAttribute(const AttrName: string): Integer; virtual;
    procedure SetAttribute(const AttrName: string; Value: Integer); virtual;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; AMap: TMap; const APosition: T3DPoint);
    destructor Destroy; override;

    procedure GetAttributes(Attributes: TStrings); virtual;
    procedure GetPluginIDs(PluginIDs: TStrings);

    procedure DrawInPlace(Canvas: TCanvas; X: Integer = 0;
      Y: Integer = 0);

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

    procedure Win;
    procedure Lose;

    property Map: TMap read FMap;
    property Position: T3DPoint read FPosition;
    property Direction: TDirection read FDirection write FDirection;
    property Visible: Boolean read GetVisible;
    property Color: TColor read FColor write FColor;
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
    FScrews: TObjectList;         /// Liste des cases
    FMaps: TObjectList;           /// Liste des cartes
    FPlayers: TObjectList;        /// Liste des joueurs

    FEditing: Boolean;            /// Indique si on est en mode édition
    FTemporization: Integer;      /// Temporisation en millisecondes
    FBeginTickCount: Cardinal;    /// Tick count système au lancement
    FTickCount: Cardinal;         /// Tick count de la partie
    FTerminated: Boolean;         /// Indique si la partie est terminée

    function GetComponent(const ID: TComponentID): TFunLabyComponent;
    function GetScrewComponent(const ID: TComponentID): TScrewComponent;

    function GetPlugin(const ID: TComponentID): TPlugin;
    function GetObjectDef(const ID: TComponentID): TObjectDef;
    function GetField(const ID: TComponentID): TField;
    function GetEffect(const ID: TComponentID): TEffect;
    function GetTool(const ID: TComponentID): TTool;
    function GetObstacle(const ID: TComponentID): TObstacle;
    function GetScrew(const ID: TComponentID): TScrew;
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
    function GetScrewCount: Integer;
    function GetScrews(Index: Integer): TScrew;
    function GetMapCount: Integer;
    function GetMaps(Index: Integer): TMap;
    function GetPlayerCount: Integer;
    function GetPlayers(Index: Integer): TPlayer;

    procedure SetTemporization(Value: Integer);

    procedure AddComponent(Component: TFunLabyComponent);
    procedure RemoveComponent(Component: TFunLabyComponent);

    procedure Terminate;
  public
    constructor Create(AEditing: Boolean);
    destructor Destroy; override;

    procedure Temporize;
    procedure UpdateTickCount;

    function ScrewByComps(
      const Field, Effect, Tool, Obstacle: TComponentID): TScrew;

    property ImagesMaster: TImagesMaster read FImagesMaster;

    property Component[const ID: TComponentID]: TFunLabyComponent
      read GetComponent;
    property ScrewComponent[const ID: TComponentID]: TScrewComponent
      read GetScrewComponent;
    property Plugin[const ID: TComponentID]: TPlugin read GetPlugin;
    property ObjectDef[const ID: TComponentID]: TObjectDef read GetObjectDef;
    property Field[const ID: TComponentID]: TField read GetField;
    property Effect[const ID: TComponentID]: TEffect read GetEffect;
    property Tool[const ID: TComponentID]: TTool read GetTool;
    property Obstacle[const ID: TComponentID]: TObstacle read GetObstacle;
    property Screw[const ID: TComponentID]: TScrew read GetScrew;
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
    property ScrewCount: Integer read GetScrewCount;
    property Screws[Index: Integer]: TScrew read GetScrews;
    property MapCount: Integer read GetMapCount;
    property Maps[Index: Integer]: TMap read GetMaps;
    property PlayerCount: Integer read GetPlayerCount;
    property Players[Index: Integer]: TPlayer read GetPlayers;

    property Editing: Boolean read FEditing;
    property Temporization: Integer read FTemporization write SetTemporization;
    property TickCount: Cardinal read FTickCount;
    property Terminated: Boolean read FTerminated;
  end;

const {don't localize}
  /// Fichier INI de FunLabyrinthe
  fIniFileName = 'FunLabyrinthe.ini';

  /// Position qualifiée nulle
  NoQPos: TQualifiedPos = (Map: nil; Position: (X: 0; Y: 0; Z: 0));

  /// Temporisation par défaut
  DefaultTemporization = 500;

  /// Couleur par défaut d'un joueur
  DefaultPlayerColor = clBlue;

  /// Application d'une direction vers la direction opposée
  NegDir: array[TDirection] of TDirection = (
    diNone, diSouth, diWest, diNorth, diEast
  );

var {don't localize}
  /// Dossier de FunLabyrinthe dans Application Data
  fFunLabyAppData: string = '';
  /// Dossier des fichiers image
  fScrewsDir: string = 'Screws\';
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
  fScrewFileName: string = '%s.bmp';

function CheckValidLaunch: Boolean;
procedure ShowFunLabyAbout;

function PointBehind(const Src: T3DPoint; Dir: TDirection): T3DPoint;
function ScrewRect(X: Integer = 0; Y: Integer = 0): TRect;
procedure EmptyRect(Canvas: TCanvas; Rect: TRect);
procedure EmptyScrewRect(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);

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
  Crée un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord supérieur du rectangle
  @return Le rectangle de type TRect
*}
function ScrewRect(X: Integer = 0; Y: Integer = 0): TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X+ScrewSize;
  Result.Bottom := Y+ScrewSize;
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
procedure EmptyScrewRect(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  EmptyRect(Canvas, ScrewRect(X, Y));
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
  EmptyScrew: TBitmap;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(ScrewSize, ScrewSize);
  FImgNames := THashedStringList.Create;

  EmptyScrew := TScrewBitmap.Create;
  try
    Add('', EmptyScrew);
  finally
    EmptyScrew.Free;
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
        NewImg.LoadFromFile(Format(fScrewFileName, [ImgName]));
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

{---------------------}
{ Classe TScrewBitmap }
{---------------------}

{*
  Crée une instance de TScrewBitmap
*}
constructor TScrewBitmap.Create;
begin
  inherited;

  Width := ScrewSize;
  Height := ScrewSize;
  EmptyScrew;
end;

{*
  Efface le contenu de la case
*}
procedure TScrewBitmap.EmptyScrew;
begin
  EmptyScrewRect(Canvas);
end;

{*
  Dessine l'image de case sur un canevas
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonnée X du point à partir duquel dessiner l'image
  @param Y        Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TScrewBitmap.DrawScrew(Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
var
  OldBrushStyle: TBrushStyle;
begin
  OldBrushStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.BrushCopy(ScrewRect(X, Y), Self, ScrewRect, clTransparent);
  Canvas.Brush.Style := OldBrushStyle;
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
  FCachedImg := TScrewBitmap.Create;
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
  FCachedImg.EmptyScrew;
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
procedure TPainter.Draw(Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  FCachedImg.DrawScrew(Canvas, X, Y);
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
  inherited Create;
  FMaster := AMaster;
  FID := AID;
  if FID <> '' then
    Master.AddComponent(Self);
end;

{*
  Détruit l'instance
*}
destructor TFunLabyComponent.Destroy;
begin
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
begin
  inherited;
  FPainter.ImgNames.EndUpdate;

  if StaticDraw then
  begin
    FCachedImg := TScrewBitmap.Create;
    PrivDraw(NoQPos, FCachedImg.Canvas);
  end;
end;

{*
  Dessine le composant sur un canevas
  PrivDraw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TVisualComponent.PrivDraw(const QPos: TQualifiedPos;
  Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  DoDraw(QPos, Canvas, X, Y);
end;

{*
  Dessine le composant sur un canevas
  DoDraw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TVisualComponent.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
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
begin
  if StaticDraw then
    FCachedImg.DrawScrew(Canvas, X, Y)
  else
    PrivDraw(QPos, Canvas, X, Y);
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
  @param Player   Joueur qui est dessiné
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlugin.DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
  Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  FPainterBefore.Draw(Canvas, X, Y);
end;

{*
  Dessine sur le joueur
  DrawAfter est exécuté lors du dessin du joueur, après celui-ci. Le dessin
  effectué dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessiné
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlugin.DrawAfter(Player: TPlayer; const QPos: TQualifiedPos;
  Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
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
procedure TPlugin.Moving(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Dest: T3DPoint; var Cancel: Boolean);
begin
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Player   Joueur qui se déplace
  @param Src      Case de départ
  @param Dest     Case d'arrivée
*}
procedure TPlugin.Moved(Player: TPlayer; const Src, Dest: T3DPoint);
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
  Nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @return Nombre d'objets que ce joueur possède
*}
function TObjectDef.GetCount(Player: TPlayer): Integer;
begin
  Result := Player.Attribute[ID];
end;

{*
  Modifie le nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @param Value    Nouveau nombre d'objets
*}
procedure TObjectDef.SetCount(Player: TPlayer; Value: Integer);
begin
  Player.Attribute[ID] := Value;
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
  Dessine le composant sur un canevas
  PrivDraw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TField.PrivDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  if FDelegateDrawTo = nil then
    inherited
  else
    FDelegateDrawTo.Draw(QPos, Canvas, X, Y);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TField.Entering(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint; var Cancel: Boolean);
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
procedure TField.Exiting(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Pos, Dest: T3DPoint; var Cancel: Boolean);
begin
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Player   Joueur qui se déplace
  @param Src      Case de provenance
  @param Pos      Position de la case
*}
procedure TField.Entered(Player: TPlayer; const Src, Pos: T3DPoint);
begin
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Player   Joueur qui se déplace
  @param Pos      Position de la case
  @param Dest     Case de destination
*}
procedure TField.Exited(Player: TPlayer; const Pos, Dest: T3DPoint);
begin
end;

{----------------}
{ Classe TEffect }
{----------------}

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Player   Joueur qui se déplace
  @param Src      Case de provenance
  @param Pos      Position de la case
*}
procedure TEffect.Entered(Player: TPlayer; const Src, Pos: T3DPoint);
begin
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Player   Joueur qui se déplace
  @param Pos      Position de la case
  @param Dest     Case de destination
*}
procedure TEffect.Exited(Player: TPlayer; const Pos, Dest: T3DPoint);
begin
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TEffect.Execute(Player: TPlayer; const Pos: T3DPoint;
  var GoOnMoving: Boolean);
begin
end;

{--------------}
{ Classe TTool }
{--------------}

{*
  Exécuté lorsque le joueur trouve l'outil
  Find est exécuté lorsque le joueur trouve l'outil. C'est-à-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Player   Joueur qui a trouvé l'outil
  @param Pos      Position de la case
*}
procedure TTool.Find(Player: TPlayer; const Pos: T3DPoint);
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
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortExecute   À positionner à True pour empêcher le Execute
*}
procedure TObstacle.Pushing(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel, AbortExecute: Boolean);
begin
  Cancel := True;
end;

{---------------}
{ Classe TScrew }
{---------------}

{*
  Crée une instance de TScrew
  @param AMaster     Maître FunLabyrinthe
  @param AID         ID de la case
  @param AName       Nom de la case
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TScrew.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; AField: TField; AEffect: TEffect; ATool: TTool;
  AObstacle: TObstacle);
begin
  inherited Create(AMaster, AID, AName);
  FStaticDraw := False;
  FField := AField;
  FEffect := AEffect;
  FTool := ATool;
  FObstacle := AObstacle;
  FRefCount := 0;

  FStaticDraw :=
    ((not Assigned(FField)) or FField.StaticDraw) and
    ((not Assigned(FEffect)) or FEffect.StaticDraw) and
    ((not Assigned(FTool)) or FTool.StaticDraw) and
    ((not Assigned(FObstacle)) or FObstacle.StaticDraw);
end;

{*
  Dessine la case sur un canevas
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TScrew.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  if Assigned(Field) then
    Field.Draw(QPos, Canvas, X, Y);
  if Assigned(Effect) then
    Effect.Draw(QPos, Canvas, X, Y);
  if Assigned(Tool) then
    Tool.Draw(QPos, Canvas, X, Y);
  if Assigned(Obstacle) then
    Obstacle.Draw(QPos, Canvas, X, Y);

  inherited;
end;

{*
  Exécuté avant la destruction de l'objet
  BeforeDestruction est appelé avant l'exécution du premier destructeur.
  N'appelez pas directement BeforeDestruction.
*}
procedure TScrew.BeforeDestruction;
begin
  inherited;
  // Il ne faut surtout pas détruire une case déjà en cours de destruction
  FRefCount := NoRefCount;
end;

{*
  [@inheritDoc]
*}
procedure TScrew.DefaultHandler(var Msg);
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
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TScrew.Entering(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint; var Cancel: Boolean);
begin
  AddRef;
  try
    if Assigned(Field) then
      Field.Entering(Player, OldDirection, KeyPressed, Src, Pos, Cancel);
  finally
    Release;
  end;
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
procedure TScrew.Exiting(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Pos, Dest: T3DPoint; var Cancel: Boolean);
begin
  AddRef;
  try
    if Assigned(Field) then
      Field.Exiting(Player, OldDirection, KeyPressed, Pos, Dest, Cancel);
  finally
    Release;
  end;
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Player   Joueur qui se déplace
  @param Src      Case de provenance
  @param Pos      Position de la case
*}
procedure TScrew.Entered(Player: TPlayer; const Src, Pos: T3DPoint);
begin
  AddRef;
  try
    if Assigned(Field) then
      Field.Entered(Player, Src, Pos);
    if Assigned(Effect) then
      Effect.Entered(Player, Src, Pos);
  finally
    Release;
  end;
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Player   Joueur qui se déplace
  @param Pos      Position de la case
  @param Dest     Case de destination
*}
procedure TScrew.Exited(Player: TPlayer; const Pos, Dest: T3DPoint);
begin
  AddRef;
  try
    if Assigned(Field) then
      Field.Exited(Player, Pos, Dest);
    if Assigned(Effect) then
      Effect.Exited(Player, Pos, Dest);
  finally
    Release;
  end;
end;

{*
  Trouve l'objet et exécute l'effet
  @param Player       Joueur concerné
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TScrew.Execute(Player: TPlayer; const Pos: T3DPoint;
  var GoOnMoving: Boolean);
begin
  AddRef;
  try
    if Assigned(Tool) then
      Tool.Find(Player, Pos);
    if Assigned(Effect) then
      Effect.Execute(Player, Pos, GoOnMoving);
  finally
    Release;
  end;
end;

{*
  Exécuté lorsque le joueur pousse sur l'obstacle
  Pushing est exécuté lorsque le joueur pousse sur l'obstacle. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortExecute   À positionner à True pour empêcher le Execute
*}
procedure TScrew.Pushing(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel, AbortExecute: Boolean);
begin
  AddRef;
  try
    if Assigned(Obstacle) then
    begin
      Obstacle.Pushing(Player, OldDirection, KeyPressed,
        Src, Pos, Cancel, AbortExecute);
    end;
  finally
    Release;
  end;
end;

{*
  Incrémente le compteur de références de la case
  @return Nouvelle valeur du compteur de références
*}
function TScrew.AddRef: Integer;
begin
  if FRefCount <> NoRefCount then
    Inc(FRefCount);
  Result := FRefCount;
end;

{*
  Décrémente le compteur de références de la case
  @return Nouvelle valeur du compteur de références
*}
function TScrew.Release: Integer;
begin
  if FRefCount <> NoRefCount then
    Dec(FRefCount);
  Result := FRefCount;

  if FRefCount = 0 then
    Free;
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
function TMap.GetMap(const Position: T3DPoint): TScrew;
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
procedure TMap.SetMap(const Position: T3DPoint; Value: TScrew);
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
function TMap.GetOutside(Floor: Integer): TScrew;
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
procedure TMap.SetOutside(Floor: Integer; Value: TScrew);
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
function TMap.GetLinearMap(Index: Integer): TScrew;
begin
  Result := FMap[Index];
end;

{*
  Modifie le tableau zero-based de la carte linéaire
  @param Index   Index dans le tableau
  @param Value   Nouvelle case
*}
procedure TMap.SetLinearMap(Index: Integer; Value: TScrew);
begin
  if FMap[Index] = Value then
    Exit;

  if Assigned(FMap[Index]) then
    FMap[Index].Release;

  FMap[Index] := Value;

  if Assigned(Value) then
    Value.AddRef;
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
  FShowCounter := 0;
  FColor := DefaultPlayerColor;
  FPlugins := TObjectList.Create(False);
  FAttributes := THashedStringList.Create;
  TStringList(FAttributes).CaseSensitive := True;
  FOnSendCommand := nil;
  FPlayState := psPlaying;
end;

{*
  Détruit l'instance
*}
destructor TPlayer.Destroy;
begin
  FAttributes.Free;
  FPlugins.Free;
  inherited;
end;

{*
  Dessine le composant sur un canevas
  PrivDraw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TPlayer.PrivDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
var
  I: Integer;
begin
  // Dessine les plug-in en-dessous du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawBefore(Self, QPos, Canvas, X, Y);

  // Dessin du joueur lui-même, à moins d'être invisible
  if Visible then
    inherited;

  // Dessine les plug-in au-dessus du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawAfter(Self, QPos, Canvas, X, Y);
end;

function TPlayer.GetVisible: Boolean;
begin
  Result := FShowCounter >= 0;
end;

{*
  Nombre de plug-in greffés au joueur
  @return Nombre de plug-in
*}
function TPlayer.GetPluginCount: Integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in greffés au joueur
  @param Index   Index du plug-in dans le tableau
  @return Le plug-in à la position indiquée
*}
function TPlayer.GetPlugins(Index: Integer): TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Dessine le joueur sur un canevas
  Draw dessine le joueur sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonnée X du point à partir duquel dessiner le joueur
  @param Y        Coordonnée Y du point à partir duquel dessiner le joueur
*}
procedure TPlayer.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  if FColor <> clTransparent then
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+ScrewSize-6, Y+ScrewSize-6);
    end;
  end;
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
  case AnsiIndexStr(AttrName, [attrColor, attrShowCounter]) of
    0: Result := FColor;
    1: Result := FShowCounter;
  else
    Index := FAttributes.IndexOf(AttrName);
    if Index < 0 then
      Result := 0
    else
      Result := Integer(FAttributes.Objects[Index]);
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
  case AnsiIndexStr(AttrName, [attrColor, attrShowCounter]) of
    0: FColor := Value;
    1: FShowCounter := Value;
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
end;

{*
  Dresse la liste des attributs du joueur
  @param Attributes   Liste de chaînes dans laquelle enregistrer les attributs
*}
procedure TPlayer.GetAttributes(Attributes: TStrings);
begin
  with Attributes do
  begin
    Assign(FAttributes);
    if FColor <> DefaultPlayerColor then
      AddObject(attrColor, TObject(FColor));
    if FShowCounter <> 0 then
      AddObject(attrShowCounter, TObject(FShowCounter));
  end;
end;

{*
  Dresse la liste des ID des plug-in du joueur
  @param Plugins   Liste de chaînes dans laquelle enregistrer les ID des plug-in
*}
procedure TPlayer.GetPluginIDs(PluginIDs: TStrings);
var
  I: Integer;
begin
  PluginIDs.Clear;
  for I := 0 to PluginCount-1 do
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
  QPos.Map := Map;
  QPos.Position := Position;
  Draw(QPos, Canvas, X, Y);
end;

{*
  Greffe un plug-in au joueur
  @param Plugin   Le plug-in à greffer
*}
procedure TPlayer.AddPlugin(Plugin: TPlugin);
begin
  FPlugins.Add(Plugin);
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in à retirer
*}
procedure TPlayer.RemovePlugin(Plugin: TPlugin);
begin
  FPlugins.Remove(Plugin);
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
  I: Integer;
begin
  Result := True;

  for I := 0 to PluginCount-1 do
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
  I, GoodObjectCount: Integer;
  GoodObjects: array of TObjectDef;
  RadioTitles: array of string;
  GoodObject: TObjectDef;
begin
  Result := True;

  // Les plug-in ont la priorité, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do
    if Plugins[I].AbleTo(Self, Action) then
      Exit;

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
  I: Integer;
  Src, Dest: T3DPoint;
  OldDir: TDirection;
  Cancel, AbortExecute: Boolean;
begin
  // Initialisation des variables
  Redo := False;
  Src := FPosition;
  Dest := PointBehind(FPosition, Dir);
  OldDir := FDirection;
  FDirection := Dir;
  Cancel := False;
  AbortExecute := False;

  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  // Le déplacement est-il permis ?
  begin
    // Case source : exiting
    Map[Src].Exiting(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then
      Exit;

    // Plug-in : moving
    for I := 0 to PluginCount-1 do
      Plugins[I].Moving(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then
      Exit;

    // Case destination : entering
    Map[Dest].Entering(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then
      Exit;

    // Case destination : pushing
    Map[Dest].Pushing(Self, OldDir, KeyPressed, Src, Dest, Cancel,
      AbortExecute);
    if Cancel then
      Exit;
  end;

  // Déplacement du joueur (à moins qu'il ait été déplacé par ailleurs)
  if Same3DPoint(FPosition, Src) then
    MoveTo(Dest, not AbortExecute, Redo);
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
  Src: T3DPoint;
  I: Integer;
begin
  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  Src := Position;
  FPosition := Dest;

  // Case source : exited
  Map[Src].Exited(Self, Src, Dest);

  // Plug-in : moved
  for I := 0 to PluginCount-1 do
    Plugins[I].Moved(Self, Src, Dest);

  // Case destination : entered
  Map[Dest].Entered(Self, Src, Dest);

  // Case destination : execute (seulement si Execute vaut True)
  if Execute then
    Map[Position].Execute(Self, Dest, Redo);
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
  Src: T3DPoint;
  I: Integer;
begin
  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  if Dest.Map = Map then
  begin
    MoveTo(Dest.Position, Execute, Redo);
    Exit;
  end;

  Src := Position;

  if Assigned(Map) then
  begin
    // Case source : exited
    Map[Src].Exited(Self, Src, No3DPoint);

    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, Src, No3DPoint);
  end;

  // Déplacement
  FMap := Dest.Map;
  FPosition := Dest.Position;

  if Assigned(Map) then
  begin
    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, No3DPoint, Dest.Position);

    // Case destination : entered
    Map[Dest.Position].Entered(Self, No3DPoint, Dest.Position);

    // Case destination : execute (seulement si Execute vaut True)
    if Execute then
      Map[Position].Execute(Self, Dest.Position, Redo);
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
  Déplacement naturel, selon le mouvement déjà entamé (sorte d'inertie)
  Après un mouvement donné expressément, si Redo vaut True, il suffit d'appeler
  NaturalMoving pour continuer le mouvement normalement.
*}
procedure TPlayer.NaturalMoving;
var
  Redo: Boolean;
begin
  repeat
    Master.Temporize;
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
  FMap := AMap;
  FPosition := APosition;
end;

{*
  Affiche le joueur
*}
procedure TPlayer.Show;
begin
  Inc(FShowCounter);
end;

{*
  Cache le joueur
*}
procedure TPlayer.Hide;
begin
  Dec(FShowCounter);
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
  Fait gagner le joueur
*}
procedure TPlayer.Win;
var
  I: Integer;
begin
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
end;

{*
  Fait perdre le joueur
*}
procedure TPlayer.Lose;
var
  I: Integer;
begin
  if FPlayState <> psPlaying then
    Exit;

  // Ce joueur a perdu
  FPlayState := psLost;

  // Si plus aucun joueur ne joue, la partie est terminée
  for I := 0 to Master.PlayerCount-1 do
    if Master.Players[I].PlayState = psPlaying then
      Exit;
  Master.Terminate;
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
  FScrews     := TObjectList.Create(False);
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
  FScrews.Free;
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
function TMaster.GetScrewComponent(const ID: TComponentID): TScrewComponent;
begin
  try
    Result := Component[ID] as TScrewComponent;
  except
    on Error: EComponentNotFound do
    begin
      if NberCharInStr(ScrewIDDelim, ID) = 3 then
        Result := Screw[ID]
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
  Si la case n'a pas pu être trouvée, GetScrew essaye de trouver les terrain
  et effet correspondant à son ID et de créer la case automatiquement
  @param ID   ID de la case à trouver
  @return La case dont l'ID a été spécifié
  @throws EComponentNotFound : Aucune case ne correspond à l'ID spécifié
  @throws EInvalidCast : Le composant de l'ID spécifié n'est pas une case
*}
function TMaster.GetScrew(const ID: TComponentID): TScrew;
var
  AField: TField;
  AEffect: TEffect;
  ATool: TTool;
  AObstacle: TObstacle;
  AName: string;
begin
  try
    Result := Component[ID] as TScrew;
  except
    on Error: EComponentNotFound do
    begin
      Result := nil;

      if NberCharInStr(ScrewIDDelim, ID) = 3 then
      try
        AField    := Field   [GetXToken(ID, ScrewIDDelim, 1)];
        AEffect   := Effect  [GetXToken(ID, ScrewIDDelim, 2)];
        ATool     := Tool    [GetXToken(ID, ScrewIDDelim, 3)];
        AObstacle := Obstacle[GetXToken(ID, ScrewIDDelim, 4)];

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

        Result := TScrew.Create(Self, ID, AName,
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
function TMaster.GetScrewCount: Integer;
begin
  Result := FScrews.Count;
end;

{*
  Tableau zero-based des cases
  @param Index   Index de la case
  @return La case à la position spécifiée
*}
function TMaster.GetScrews(Index: Integer): TScrew;
begin
  Result := TScrew(FScrews[Index]);
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
  else if Component is TScrew then
    FScrews.Add(Component)
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
  else if Component is TScrew then
    FScrews.Remove(Component)
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
  Met à jour le tick count de la partie
*}
procedure TMaster.UpdateTickCount;
begin
  FTickCount := GetTickCount - FBeginTickCount;
end;

{*
  Obtient une case à partir des ID de ses composantes
  @param Field      ID du terrain
  @param Effect     ID de l'effet
  @param Tool       ID de l'outil
  @param Obstacle   ID de l'obstacle
  @return Case avec avec les composantes spécifiées
*}
function TMaster.ScrewByComps(
  const Field, Effect, Tool, Obstacle: TComponentID): TScrew;
begin
  Result := Screw[Format(ScrewIDFormat, [Field, Effect, Tool, Obstacle])];
end;

initialization
  Randomize;
  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    fFunLabyAppData :=
      ReadString('Directories', 'AppData', Dir); {don't localize}

    fScrewsDir := fFunLabyAppData + fScrewsDir;
    fSoundsDir := fFunLabyAppData + fSoundsDir;
    FUnitsDir := fFunLabyAppData + fUnitsDir;
    FMapsDir := fFunLabyAppData + fMapsDir;
    fLabyrinthsDir := fFunLabyAppData + fLabyrinthsDir;
    fSaveguardsDir := fFunLabyAppData + fSaveguardsDir;
    fEditPluginDir := fFunLabyAppData + fEditPluginDir;

    fScrewFileName := fScrewsDir+fScrewFileName;
  finally
    Free;
  end;
end.

