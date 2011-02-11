{*
  Types et classes de bases de FunLabyrinthe
  FunLabyUtils comprend les types et classes de base de FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FunLabyUtils;

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Contnrs, RTLConsts, Controls,
  Dialogs, TypInfo, SyncObjs, ScUtils, ScClasses, ScCoroutines, SdDialogs, GR32,
  FunLabyCoreConsts, FunLabyGraphics, ScIntegerSets;

const {don't localize}
  SquareSize = 30;     /// Taille (en largeur et hauteur) d'une case
  HalfSquareSize = 15; /// Moitié de la taille d'une case
  MinViewSize = 1;     /// Taille minimale d'une vue

  /// Couleur de transparence pour les fichiers .bmp
  clBmpTransparent32 = FunLabyGraphics.clBmpTransparent32;

  /// Couleur transparente
  clTransparent32 = FunLabyGraphics.clTransparent32;

  msgShowMessage = $01; /// Message pour afficher un message au joueur
  msgGameStarted = $02; /// Message envoyé lorsque le jeu commence
  msgPressKey = $03;    /// Message envoyé au joueur à l'appui sur une touche
  msgSquareEvent = $04; /// Message envoyé lors d'un événement de case

  /// Message envoyé aux composants d'une case lorsque celle-ci est éditée
  msgEditMapSquare = $05;

  /// Message demandant l'exécution d'une méthode au sein des actions
  msgRunMethod = $06;

  SquareIDDelim = '-';            /// Délimiteur des parties d'un ID de case
  SquareIDFormat = '%s-%s-%s-%s'; /// Format d'un ID de case

  /// Temporisation par défaut
  DefaultTemporization = 500;

  /// Couleur par défaut d'un joueur
  DefaultPlayerColor = clBlue32;

  /// Taille de bordure de vue par défaut
  DefaultViewBorderSize = 1;

  /// Extension d'image préférée
  PreferredImageExtension = 'png';

  /// Extension d'un peintre
  PainterExtension = 'pnt';

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type représentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Ensemble de directions
  TDirections = set of TDirection;

  /// Type représentant une action
  TPlayerAction = type string;

  /// État de victoire/défaite d'un joueur
  TPlayState = (psPlaying, psWon, psLost);

  /// Mode de dessin d'un joueur
  TPlayerDrawMode = (dmColoredPainter, dmPainter, dmDirPainter);

  /// Type de ressource
  TResourceKind = (rkImage, rkSound);

  /// Type de callback pour trouver une ressource
  TFindResourceCallback = reference to function(const HRef: string;
    Kind: TResourceKind): TFileName;

  /// Classe de base des exceptions FunLabyrinthe
  EFunLabyException = class(Exception);

  /// Générée si un composant recherché n'est pas trouvé
  EComponentNotFound = class(EFunLabyException);

  /// Déclenchée lorsqu'un ID est invalide
  EInvalidID = class(EFunLabyException);

  /// Générée si une commande invalide est exécutée
  EInvalidCommand = class(EFunLabyException);

  /// Générée si une commande n'est pas supportée
  EUnsupportedCommand = class(EFunLabyException);

  /// Générée en cas de mauvaise définition d'une case
  EBadSquareDefException = class(EFunLabyException);

  /// Générée si une ressource n'a pas pu être trouvée
  EResourceNotFoundException = class(EInOutError);

  TDrawViewContext = class;
  TMoveContext = class;
  TFunLabyFiler = class;
  TPlayerData = class;
  TFunLabyComponent = class;
  TSquareComponent = class;
  TField = class;
  TEffect = class;
  TTool = class;
  TObstacle = class;
  TSquare = class;
  TMap = class;
  TPosComponent = class;
  TVehicle = class;
  TMobileComponent = class;
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
  public
    Map: TMap;          /// Carte, ou nil pour une position nulle
    Position: T3DPoint; /// Position sur la carte, si Map <> nil
  private
    function GetIsNoQPos: Boolean; inline;
    function GetIsInside: Boolean; inline;
    function GetIsOutside: Boolean; inline;

    function GetSquare: TSquare; inline;
    function GetField: TField; inline;
    function GetEffect: TEffect; inline;
    function GetTool: TTool; inline;
    function GetObstacle: TObstacle; inline;

    procedure SetSquare(Value: TSquare); inline;
    procedure SetField(Value: TField);
    procedure SetEffect(Value: TEffect);
    procedure SetTool(Value: TTool);
    procedure SetObstacle(Value: TObstacle);

    function GetComponentCount: Integer; inline;
    function GetComponents(Index: Integer): TSquareComponent; inline;
    procedure SetComponents(Index: Integer; Value: TSquareComponent);
  public
    property X: Integer read Position.X write Position.X;
    property Y: Integer read Position.Y write Position.Y;
    property Z: Integer read Position.Z write Position.Z;

    property IsNoQPos: Boolean read GetIsNoQPos;
    property IsInside: Boolean read GetIsInside;
    property IsOutside: Boolean read GetIsOutside;

    property Square: TSquare read GetSquare write SetSquare;
    property Field: TField read GetField write SetField;
    property Effect: TEffect read GetEffect write SetEffect;
    property Tool: TTool read GetTool write SetTool;
    property Obstacle: TObstacle read GetObstacle write SetObstacle;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: TSquareComponent
      read GetComponents write SetComponents;
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
    MsgID: Word;               /// ID du message
    Handled: Boolean;          /// Indique si le message a été géré
    Reserved: Byte;            /// Réservé
    Player: TPlayer;           /// Joueur concerné
    Text: string;              /// Texte à afficher
    Answers: TStringDynArray;  /// Réponses possibles (peut être vide)
    Selected: Integer;         /// Index de la réponse choisie par le joueur
    ShowOnlySelected: Boolean; /// Si True n'affiche que l'élément sélectionné
  end;

  /// Alias de TPlayerShowMsgMessage (FunDelphi codegen)
  TShowMessageMessage = TPlayerShowMsgMessage;

  /// Structure du message envoyé au démarrage du jeu
  TGameStartedMessage = TPlayerMessage;

  {*
    Structure du message d'appui sur une touche pour le joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerPressKeyMessage = record
    MsgID: Word;        /// ID du message
    Handled: Boolean;   /// Indique si le message a été géré
    Reserved: Byte;     /// Réservé
    Player: TPlayer;    /// Joueur concerné
    Key: Word;          /// Touche appuyée
    Shift: TShiftState; /// État des touches spéciales
  end;

  {*
    Type d'événement de case
  *}
  TSquareEventKind = (
    sekEntering, sekExiting, sekEntered, sekExited, sekExecute, sekPushing
  );

  /// Ensemble de TSquareEventKind
  TSquareEventKinds = set of TSquareEventKind;

  {*
    Message envoyé aux composants avec position lors d'un événement de case
    @author sjrd
    @version 5.0
  *}
  TSquareEventMessage = record
    MsgID: Word;            /// ID du message
    Kind: TSquareEventKind; /// Type d'événement
    Reserved: Byte;         /// Réservé
    Context: TMoveContext;  /// Contexte du mouvement
  end;

  {*
    Phase d'édition d'une case de la carte
    - espAdding : Un composant va être ajouté
    - espAdd : Ce composant-ci doit être ajouté
    - espAdded : Un composant a été ajouté
    - espRemoving : Un composant va être retiré
    - espRemove : Ce composant-ci doit être retiré
    - espRemoved : Un composant a été retiré
  *}
  TEditMapSquarePhase = (
    espAdding, espAdd, espAdded, espRemoving, espRemove, espRemoved
  );

  {*
    Flag de traitement d'un message TEditMapSquareMessage
    - esfCancel : Si présent en sortie, l'éditeur ne fait plus son action par
      défaut et ne considère pas que la carte a été modifiée (n'est pas pris
      en compte en phase Added ou Removed)
    - esfHandled : Si présent en sortie, l'éditeur ne fait plus son action
      par défaut (pris en compte uniquement pendant la phase Add)
  *}
  TEditMapSquareFlag = (
    esfCancel, esfHandled
  );

  /// Flags de traitement d'un message TEditMapSquareMessage
  TEditMapSquareFlags = set of TEditMapSquareFlag;

  {*
    Structure du message d'édition d'une case dans une carte
    @author sjrd
    @version 5.0
  *}
  TEditMapSquareMessage = record
    MsgID: Word;                  /// ID du message
    Phase: TEditMapSquarePhase;   /// Phase de modification d'une case
    Flags: TEditMapSquareFlags;   /// Flags du message
    Component: TFunLabyComponent; /// Composant qui va être placé/retiré
    QPos: TQualifiedPos;          /// Position qualifiée de la case éditée
  end;

  {*
    Structure du message d'appel de méthode
    @author sjrd
    @version 5.0
  *}
  TRunMethodMessage = record
    MsgID: Word;                 /// ID du message
    Handled: Boolean;            /// Indique si le message a été géré
    Reserved: Byte;              /// Réservé
    Component: TMobileComponent; /// Composant concerné
    Method: TThreadMethod;       /// Méthode à appeler
  end;

  {*
    Type de méthode call-back pour l'enregistrement d'un composant
    @param Component   Composant à enregistrer
  *}
  TRegisterComponentProc = procedure(Component: TFunLabyComponent) of object;

  {*
    Contexte de dessin d'une case
    @author sjrd
    @version 5.0
  *}
  TDrawSquareContext = class(TObject)
  private
    FBitmap: TBitmap32; /// Bitmap cible
    FX: Integer;        /// Absisce où dessiner
    FY: Integer;        /// Ordonnée où dessiner
    FSquareRect: TRect; /// Rectangle de la case à dessiner

    FIsNowhere: Boolean;  /// Indique si le dessin est fait "nulle part"
    FQPos: TQualifiedPos; /// Position dessinée

    /// Contexte de dessin de la vue (si applicable)
    FDrawViewContext: TDrawViewContext;
    FTickCount: Cardinal; /// Tick count pour ce contexte

    FPlayer: TPlayer; /// Joueur à dessiner (si applicable)

    procedure SetPlayer(APlayer: TPlayer);
  public
    constructor Create(ABitmap: TBitmap32); overload;
    constructor Create(ABitmap: TBitmap32; X, Y: Integer); overload;
    constructor Create(ABitmap: TBitmap32; X, Y: Integer;
      const AQPos: TQualifiedPos); overload;
    constructor Create(Source: TDrawSquareContext;
      const AQPos: TQualifiedPos); overload;

    procedure SetDrawViewContext(ADrawViewContext: TDrawViewContext);
    procedure SetTickCount(ATickCount: Cardinal);

    procedure Assign(Source: TDrawSquareContext);

    procedure DrawSquareBitmap(SquareBitmap: TBitmap32);

    property Bitmap: TBitmap32 read FBitmap;
    property X: Integer read FX;
    property Y: Integer read FY;
    property SquareRect: TRect read FSquareRect;

    property IsNowhere: Boolean read FIsNowhere;
    property QPos: TQualifiedPos read FQPos;
    property Map: TMap read FQPos.Map;
    property Pos: T3DPoint read FQPos.Position;

    property DrawViewContext: TDrawViewContext read FDrawViewContext;
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

    FBitmap: TBitmap32; /// Bitmap cible
    FViewRect: TRect;   /// Rectangle de la vue à dessiner

    FUseZone: Boolean; /// Indique si ce contexte utilise une zone de carte
    FMap: TMap;        /// Carte dont dessiner une zone
    FFloor: Integer;   /// Étage de la zone à dessiner
    FZone: TRect;      /// Zone (étendue aux bordures) à dessiner
    FZoneSize: TPoint; /// Taille de la zone

    FTickCount: Cardinal; /// Tick count pour l'affichage

    procedure ComputeZone;
  public
    constructor Create(const APlayerMode: IPlayerMode; ABitmap: TBitmap32);

    function IsSquareVisible(const QPos: TQualifiedPos): Boolean; overload;
    function IsSquareVisible(Map: TMap;
      const Position: T3DPoint): Boolean; overload;
    function IsSquareVisible(const Position: T3DPoint): Boolean; overload;

    property PlayerMode: IPlayerMode read FPlayerMode;
    property Player: TPlayer read FPlayer;

    property Bitmap: TBitmap32 read FBitmap;
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
    FPlayer: TPlayer; /// Joueur qui a pressé une touche

    FKey: Word;          /// Touche pressée
    FShift: TShiftState; /// État des touches spéciales

    FHandled: Boolean; /// Indique si l'événement a été géré
  public
    constructor Create(APlayer: TPlayer; AKey: Word; AShift: TShiftState);

    property Player: TPlayer read FPlayer;

    property Key: Word read FKey;
    property Shift: TShiftState read FShift;

    property Handled: Boolean read FHandled write FHandled;
  end;

  {*
    Contexte d'un mouvement du joueur
    @author sjrd
    @version 5.0
  *}
  TMoveContext = class(TObject)
  private
    FPlayer: TPlayer; /// Joueur qui se déplace

    FSrcQPos: TQualifiedPos;  /// Source qualifiée
    FDestQPos: TQualifiedPos; /// Destination qualifiée
    FQPos: TQualifiedPos;     /// Position qualifiée courante

    FOldDirection: TDirection; /// Ancienne direction du joueur
    FKeyPressed: Boolean;      /// True si une touche a été pressée
    FKey: Word;                /// Touche pressée (si KeyPressed)
    FShift: TShiftState;       /// État des touches spéciales (si KeyPressed)

    FCancelled: Boolean;  /// True si le déplacement a été annulé
    FGoOnMoving: Boolean; /// True s'il faut réitérer le déplacement
    FHooked: Boolean;     /// True si intercepté par un TPosComponent

    FTemporization: Cardinal; /// Temporisation de ce contexte

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
      AKey: Word = 0; AShift: TShiftState = []); overload;
    constructor Create(APlayer: TPlayer; const ADest: T3DPoint;
      AKey: Word = 0; AShift: TShiftState = []); overload;

    procedure Cancel;

    procedure Temporize;

    property Player: TPlayer read FPlayer;

    property SrcQPos: TQualifiedPos read FSrcQPos;
    property SrcMap: TMap read FSrcQPos.Map;
    property Src: T3DPoint read FSrcQPos.Position;
    property SrcSquare: TSquare read GetSrcSquare write SetSrcSquare;

    property DestQPos: TQualifiedPos read FDestQPos;
    property DestMap: TMap read FDestQPos.Map;
    property Dest: T3DPoint read FDestQPos.Position;
    property DestSquare: TSquare read GetDestSquare write SetDestSquare;

    property QPos: TQualifiedPos read FQPos;
    property Map: TMap read FQPos.Map;
    property Pos: T3DPoint read FQPos.Position;
    property Square: TSquare read GetSquare write SetSquare;

    property OldDirection: TDirection read FOldDirection;
    property KeyPressed: Boolean read FKeyPressed;
    property Key: Word read FKey;
    property Shift: TShiftState read FShift;

    property Cancelled: Boolean read FCancelled write FCancelled;
    property GoOnMoving: Boolean read FGoOnMoving write FGoOnMoving;
    property Hooked: Boolean read FHooked write FHooked;

    property Temporization: Cardinal read FTemporization write FTemporization;
  end;

  {*
    Méthode avec contexte de déplacement
    @param Context   Contexte du déplacement
  *}
  TMoveContextMethod = procedure(Context: TMoveContext) of object;

  {*
    Élément de l'état d'un objet persistant
    - psCreating : l'objet est en train d'être construit
    - psDestroying : l'objet est en train d'être détruit
    - psReading : l'objet est en train d'être lu depuis un filer
    - psWriting : l'objet est en train d'être écrit dans un filer
  *}
  TPersistentStateItem = (psCreating, psDestroying, psReading, psWriting);

  /// État d'un objet persistant
  TPersistentState = set of TPersistentStateItem;

  {$M+}

  {*
    Objet persistent FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyPersistent = class(TObject)
  private
    FPersistentState: TPersistentState; /// État
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); virtual;
    procedure StoreDefaults; virtual;

    procedure BeginState(State: TPersistentState); virtual;
    procedure EndState(State: TPersistentState); virtual;

    property PersistentState: TPersistentState
      read FPersistentState write FPersistentState;
  public
    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  {$M-}

  /// Classe de TFunLabyPersistent
  TFunLabyPersistentClass = class of TFunLabyPersistent;

  {*
    Classe de base pour les objets persistents devant implémenter un interface
    @author sjrd
    @version 5.0
  *}
  TInterfacedFunLabyPersistent = class(TFunLabyPersistent, IInterface)
  protected
    FRefCount: Integer; /// Compteur de références

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property RefCount: Integer read FRefCount;
  end;

  {*
    Classe de base pour les collections persistentes de FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyCollection = class(TFunLabyPersistent)
  private
    FItems: TObjectList; /// Éléments de la collection

    function GetCount: Integer;
    function GetItems(Index: Integer): TFunLabyPersistent;
  protected
    function CreateItem(ItemClass: TFunLabyPersistentClass):
      TFunLabyPersistent; virtual; abstract;

    procedure Notify(Item: TFunLabyPersistent;
      Action: TListNotification); virtual;

    function GetDefaultItemClass: TFunLabyPersistentClass; virtual;

    function AddItem(Item: TFunLabyPersistent): Integer;
    function InsertItem(Index: Integer; Item: TFunLabyPersistent): Integer;

    function ExtractItem(Index: Integer): TFunLabyPersistent; overload;
    function ExtractItem(
      Item: TFunLabyPersistent): TFunLabyPersistent; overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Add(ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
    function AddDefault: TFunLabyPersistent;
    function Insert(Index: Integer;
      ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
    procedure Delete(Index: Integer);
    function Remove(Item: TFunLabyPersistent): Integer;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function IndexOf(Item: TFunLabyPersistent): Integer;

    function HasDefault: Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFunLabyPersistent read GetItems; default;
  end;

  {*
    Classe de base pour les objets lecteurs et écrivains FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyFiler = class(TObject)
  private
    FOwner: TFunLabyFiler;         /// Filer propriétaire (peut être nil)
    FMaster: TMaster;              /// Maître FunLabyrinthe
    FInstance: TFunLabyPersistent; /// Instance traitée par ce filer
  protected
    procedure HandleProperty(PropInfo: PPropInfo;
      HasData: Boolean); virtual; abstract;

    procedure HandlePersistent(const Name: string;
      SubInstance: TFunLabyPersistent); virtual; abstract;

    procedure HandleCollection(const Name: string;
      Collection: TFunLabyCollection); virtual; abstract;

    procedure HandleComponent(const Name: string;
      Component: TFunLabyComponent); virtual; abstract;

    procedure HandleStrings(const Name: string; Strings: TStrings;
      ObjectType: PTypeInfo; HasData: Boolean); virtual; abstract;

    procedure HandleBinaryProperty(const Name: string;
      ReadProc, WriteProc: TStreamProc; HasData: Boolean); virtual; abstract;

    procedure EnumProperties;
    procedure InstanceBeginState(State: TPersistentState);
    procedure InstanceEndState(State: TPersistentState);

    function HasPlayerData(Component: TFunLabyComponent;
      Player: TPlayer): Boolean;
    function GetPlayerData(Component: TFunLabyComponent;
      Player: TPlayer): TPlayerData;
  public
    constructor Create(AInstance: TFunLabyPersistent;
      AOwner: TFunLabyFiler = nil);

    procedure DefinePublishedProperty(PropInfo: PPropInfo);

    procedure DefineProcProperty(const Name: string; PropType: PTypeInfo;
      GetProc, SetProc: Pointer; HasData: Boolean = True);

    procedure DefineFieldProcProperty(const Name: string; PropType: PTypeInfo;
      GetField, SetProc: Pointer; HasData: Boolean = True);

    procedure DefineFieldProperty(const Name: string; PropType: PTypeInfo;
      GetSetField: Pointer; HasData: Boolean = True);

    procedure DefinePersistent(const Name: string;
      SubInstance: TFunLabyPersistent);

    procedure DefineStrings(const Name: string; Strings: TStrings;
      ObjectType: PTypeInfo = nil); overload;
    procedure DefineStrings(const Name: string; Strings: TStrings;
      ObjectType: PTypeInfo; HasData: Boolean); overload;

    procedure DefineBinaryProperty(const Name: string;
      ReadProc, WriteProc: TStreamProc; HasData: Boolean = True);

    property Owner: TFunLabyFiler read FOwner;
    property Master: TMaster read FMaster;
    property Instance: TFunLabyPersistent read FInstance;
  end;

  {*
    Classe de base pour les lecteurs FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyReader = class(TFunLabyFiler)
  public
    constructor Create(AInstance: TFunLabyPersistent;
      AOwner: TFunLabyReader = nil);
    destructor Destroy; override;
  end;

  {*
    Classe de base pour les écrivains FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyWriter = class(TFunLabyFiler)
  public
    constructor Create(AInstance: TFunLabyPersistent;
      AOwner: TFunLabyWriter = nil);
    destructor Destroy; override;
  end;

  {*
    Pseudo-filer servant à enregistrer récursivement les propriétés par défaut
    @author sjrd
    @version 5.0
  *}
  TFunLabyStoredDefaultsFiler = class(TFunLabyFiler)
  protected
    procedure HandleProperty(PropInfo: PPropInfo; HasData: Boolean); override;

    procedure HandlePersistent(const Name: string;
      SubInstance: TFunLabyPersistent); override;

    procedure HandleCollection(const Name: string;
      Collection: TFunLabyCollection); override;

    procedure HandleComponent(const Name: string;
      Component: TFunLabyComponent); override;

    procedure HandleStrings(const Name: string; Strings: TStrings;
      ObjectType: PTypeInfo; HasData: Boolean); override;

    procedure HandleBinaryProperty(const Name: string;
      ReadProc, WriteProc: TStreamProc; HasData: Boolean); override;
  end;

  {*
    Gère le chargement des images d'après leur nom
    TImagesMaster s'occupe de charger automatiquement les images qu'on lui
    demande d'afficher. Il les conserve dans une liste d'image.
    @author sjrd
    @version 5.0
  *}
  TImagesMaster = class(TObject)
  private
    FExtensions: TStrings; /// Extensions d'images reconnues
    FImgList: TObjectList; /// Liste d'images interne
    FImgNames: TStrings;   /// Liste des noms des images

    function LoadBitmapFromPainterFile(const FileName: TFileName): TBitmap32;

    function LoadImage(const ImgName: string): TBitmap32;
    function Add(const ImgName: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function ResolveImgName(const ImgName: string): TFileName;

    function IndexOf(const ImgName: string): Integer;

    procedure Draw(Index: Integer; Context: TDrawSquareContext); overload;
    procedure Draw(const ImgName: string;
      Context: TDrawSquareContext); overload;

    procedure Draw(Index: Integer; Bitmap: TBitmap32;
      X: Integer = 0; Y: Integer = 0); overload;
    procedure Draw(const ImgName: string; Bitmap: TBitmap32;
      X: Integer = 0; Y: Integer = 0); overload;

    function GetInternalBitmap(Index: Integer): TBitmap32; overload;
    function GetInternalBitmap(const ImgName: string): TBitmap32; overload;
  end;

  {*
    Enregistre et affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une méthode
    pour les dessiner les unes sur les autres, par transparence.
    @author sjrd
    @version 5.0
  *}
  TPainter = class(TFunLabyPersistent)
  private
    FMaster: TImagesMaster; /// Maître d'images

    FDescription: TStrings;        /// Description de l'image peinte
    FDefaultDescription: TStrings; /// Description par défaut
    FCachedImgIndex: Integer;      /// Index de l'image dans le maître d'images
    FCachedImg: TBitmap32;         /// Copie cache de l'image résultante

    FStaticDraw: Boolean; /// Indique si ce peintre dessine une image statique
    FNeedCache: Boolean;  /// Indique si une image cache est nécessaire
    FSize: TPoint;        /// Taille de ce peintre

    procedure Measure;
    procedure NeedCachedImg;
    procedure DrawStatic;
    procedure DrawNotStatic;

    procedure DescriptionChange(Sender: TObject);

    function GetIsEmpty: Boolean;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
    procedure StoreDefaults; override;

    procedure ParseDescriptionLine(const Description: string;
      out Bitmap: TBitmap32; out SubRect: TRect);

    property Description: TStrings read FDescription;
  public
    constructor Create(AMaster: TImagesMaster);
    destructor Destroy; override;

    procedure Clear;
    procedure AddImage(const ImgName: string);
    procedure AddImageRect(const ImgName: string; const SubRect: TRect);
    procedure Assign(Source: TPainter);

    procedure BeginUpdate;
    procedure EndUpdate;

    function GetBitmap: TBitmap32;

    procedure Draw(Context: TDrawSquareContext);

    procedure DrawTo(Dest: TBitmap32; X: Integer = 0; Y: Integer = 0);

    procedure DrawAtTimeTo(TickCount: Cardinal; Dest: TBitmap32; X: Integer = 0;
      Y: Integer = 0);

    property Master: TImagesMaster read FMaster;
    property StaticDraw: Boolean read FStaticDraw;
    property Size: TPoint read FSize;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  {*
    Données d'un composant liées à un joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerData = class(TFunLabyPersistent)
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
  TFunLabyComponent = class(TFunLabyPersistent)
  private
    FMaster: TMaster;        /// Maître FunLabyrinthe
    FID: TComponentID;       /// ID du composant
    FIsAdditionnal: Boolean; /// Indique si ce composant est additionnel
    FIconPainter: TPainter;  /// Peintre de l'icône

    {*
      Stocke une valeur entière dans un composant
      Tag n'a pas de signification prédéfinie. Elle est fournie pour les besoins
      du développeur.
    *}
    FTag: Integer;

    FPlayerData: TBucketItemArray; /// Données par joueur

    procedure SetID(const Value: TComponentID);

    function GetSafeID: TComponentID;
  protected
    FTransient: Boolean; /// Indique si ce composant est transitoire
  protected
    class function GetPlayerDataClass: TPlayerDataClass; virtual;

    procedure ChangeID(const NewID: TComponentID); virtual;

    function HasPlayerData(Player: TPlayer): Boolean;
    function GetPlayerData(Player: TPlayer): TPlayerData;

    function GetCategory: string; virtual;
    function GetHint: string; virtual;
    function GetIsDesignable: Boolean; virtual;

    property IconPainter: TPainter read FIconPainter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    class function UsePlayerData: Boolean;

    procedure DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
      Y: Integer = 0); virtual;
    procedure DrawIconToCanvas(Canvas: TCanvas; const DestRect: TRect;
      BackgroundColor: TColor);

    property Master: TMaster read FMaster;
    property SafeID: TComponentID read GetSafeID;
    property IsAdditionnal: Boolean read FIsAdditionnal;
    property Transient: Boolean read FTransient;

    property Category: string read GetCategory;
    property Hint: string read GetHint;
    property IsDesignable: Boolean read GetIsDesignable;
  published
    property ID: TComponentID read FID write SetID stored False;
    property Tag: Integer read FTag write FTag default 0;
  end;

  /// Classe de TFunLabyComponentClass
  TFunLabyComponentClass = class of TFunLabyComponent;

  {*
    Classe de base pour les composants devant être affichés
    TVisualComponent étend la classe TFunLabyComponent pour lui ajouter un
    traitement standard et simple de nommage et de dessin.
    @author sjrd
    @version 5.0
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName: string;                 /// Nom du composant
    FDefaultName: string;          /// Name par défaut
    FPainter: TPainter;            /// Peintre
    FEditVisualTag: string;        /// Tag visuel visible uniquement à l'édition
    FDefaultEditVisualTag: string; /// EditVisualTag par défaut

    function IsNameStored: Boolean;
    function IsEditVisualTagStored: Boolean;

    procedure PrivDraw(Context: TDrawSquareContext); virtual;
  protected
    procedure StoreDefaults; override;

    function GetHint: string; override;

    procedure AutoEditVisualTag;

    procedure DoDraw(Context: TDrawSquareContext); virtual;
    procedure DoDrawEditVisualTag(Context: TDrawSquareContext); virtual;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(Context: TDrawSquareContext); overload;
    procedure Draw(const QPos: TQualifiedPos; Bitmap: TBitmap32;
      X: Integer = 0; Y: Integer = 0); overload;

    procedure DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
      Y: Integer = 0); override;
  published
    property Name: string read FName write FName stored IsNameStored;
    property Painter: TPainter read FPainter;
    property EditVisualTag: string read FEditVisualTag write FEditVisualTag
      stored IsEditVisualTagStored;
  end;

  {*
    Pseudo-composant qui permet de créer de nouveaux composants
    @author sjrd
    @version 5.0
  *}
  TComponentCreator = class(TFunLabyComponent)
  protected
    function GetCategory: string; override;
    function GetHint: string; override;

    function GetComponentClass: TFunLabyComponentClass; virtual; abstract;
  public
    function CreateComponent(const AID: TComponentID): TFunLabyComponent;

    property ComponentClass: TFunLabyComponentClass read GetComponentClass;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs façons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Empêcher le déplacement du joueur et réagir à son déplacement effectif ;
    - Modifier la vue complète du joueur ;
    - Réagir à l'appui sur un touche ;
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

    function GetCategory: string; override;
    function GetIsDesignable: Boolean; override;

    property PainterBefore: TPainter read FPainterBefore;
    property PainterAfter: TPainter read FPainterAfter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Context: TDrawSquareContext); virtual;
    procedure DrawAfter(Context: TDrawSquareContext); virtual;

    procedure Moving(Context: TMoveContext); virtual;
    procedure Moved(Context: TMoveContext); virtual;

    procedure DrawView(Context: TDrawViewContext); virtual;
    procedure PressKey(Context: TKeyEventContext); virtual;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; virtual;

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
  published
    property Count: Integer read FCount write FCount default 0;
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
  private
    FDisplayInObjectList: Boolean; /// Afficher dans la liste des objets
    FDisplayInStatusBar: Boolean;  /// Afficher dans la barre de statut
  protected
    class function GetPlayerDataClass: TPlayerDataClass; override;

    function GetCategory: string; override;

    function GetCount(Player: TPlayer): Integer; virtual;
    procedure SetCount(Player: TPlayer; Value: Integer); virtual;

    function GetShownInfos(Player: TPlayer): string; virtual;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; virtual;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer); virtual;

    property Count[Player: TPlayer]: Integer read GetCount write SetCount;
    property ShownInfos[Player: TPlayer]: string read GetShownInfos;
  published
    property DisplayInObjectList: Boolean
      read FDisplayInObjectList write FDisplayInObjectList default True;
    property DisplayInStatusBar: Boolean
      read FDisplayInStatusBar write FDisplayInStatusBar default True;
  end;

  {*
    Classe de base pour les composants de case
    @author sjrd
    @version 5.0
  *}
  TSquareComponent = class(TVisualComponent)
  protected
    procedure ChangeID(const NewID: TComponentID); override;
  end;

  /// Classe de TSquareComponent
  TSquareComponentClass = class of TSquareComponent;

  {*
    Classe de base pour les terrains
    TField est la classe de base pour la création de terrains. Les terrains
    sont la première composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TField = class(TSquareComponent)
  protected
    function GetCategory: string; override;

    procedure DoDrawCeiling(Context: TDrawSquareContext); virtual;
  public
    procedure DrawCeiling(Context: TDrawSquareContext);

    procedure DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
      Y: Integer = 0); override;

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
  private
    FEnabled: Boolean; /// Indique si cet effet est activé
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetCategory: string; override;

    property Enabled: Boolean read FEnabled write FEnabled default True;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

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
  protected
    function GetCategory: string; override;
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
  protected
    function GetCategory: string; override;
  public
    procedure Pushing(Context: TMoveContext); virtual;
  end;

  {*
    Représente une case du jeu
    TSquare représente une case du jeu. Une case possède quatre composantes : le
    terrain, l'effet, l'outil et l'obstacle. Le terrain est obligatoire, tandis
    que les trois autres composantes sont optionnelles.
    @author sjrd
    @version 5.0
  *}
  TSquare = class(TSquareComponent)
  private
    FCategory: string; /// Catégorie

    FField: TField;       /// Terrain
    FEffect: TEffect;     /// Effet
    FTool: TTool;         /// Outil
    FObstacle: TObstacle; /// Obstacle

    procedure HookEventTo(Context: TMoveContext; EventKind: TSquareEventKind;
      HookComponent: TPosComponent);
    function HookEvent(Context: TMoveContext;
      EventKind: TSquareEventKind): Boolean;

    procedure UpdateID;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TSquareComponent;
    function GetComponentClasses(Index: Integer): TSquareComponentClass;
  protected
    function GetCategory: string; override;
    procedure SetCategory(const Value: string); virtual;

    procedure DoDraw(Context: TDrawSquareContext); override;
    procedure DoDrawCeiling(Context: TDrawSquareContext); virtual;

    procedure DoEntering(Context: TMoveContext); virtual;
    procedure DoExiting(Context: TMoveContext); virtual;
    procedure DoEntered(Context: TMoveContext); virtual;
    procedure DoExited(Context: TMoveContext); virtual;
    procedure DoExecute(Context: TMoveContext); virtual;
    procedure DoPushing(Context: TMoveContext); virtual;

    procedure Configure(AField: TField; AEffect: TEffect = nil;
      ATool: TTool = nil; AObstacle: TObstacle = nil);
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    constructor CreateConfig(AMaster: TMaster; const AID: TComponentID;
      AField: TField; AEffect: TEffect = nil; ATool: TTool = nil;
      AObstacle: TObstacle = nil);

    procedure DefaultHandler(var Msg); override;

    procedure DispatchAt(var Msg; const QPos: TQualifiedPos);

    function Contains(Component: TSquareComponent): Boolean;

    procedure DrawCeiling(Context: TDrawSquareContext);

    procedure DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
      Y: Integer = 0); override;

    procedure Entering(Context: TMoveContext);
    procedure Exiting(Context: TMoveContext);
    procedure Entered(Context: TMoveContext);
    procedure Exited(Context: TMoveContext);
    procedure Execute(Context: TMoveContext);
    procedure Pushing(Context: TMoveContext); 

    property Category: string read GetCategory write SetCategory;

    property Field: TField read FField;
    property Effect: TEffect read FEffect;
    property Tool: TTool read FTool;
    property Obstacle: TObstacle read FObstacle;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: TSquareComponent read GetComponents;
    property ComponentClasses[Index: Integer]: TSquareComponentClass
      read GetComponentClasses;
  end;

  /// Tableau dynamique de TSquare
  TSquareDynArray = array of TSquare;

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
    FMap: TSquareDynArray;   /// Carte stockée de façon linéaire
    FOutsideOffset: Integer; /// Offset de départ de l'extérieur

    procedure CreateMap;

    procedure LoadMapFromStream(Stream: TStream);
    procedure SaveMapToStream(Stream: TStream);

    function GetMap(const Position: T3DPoint): TSquare;
    procedure SetMap(const Position: T3DPoint; Value: TSquare);

    function GetOutside(Floor: Integer): TSquare;
    procedure SetOutside(Floor: Integer; Value: TSquare);

    function GetLinearMapCount: Integer;
    function GetLinearMap(Index: Integer): TSquare;
    procedure SetLinearMap(Index: Integer; Value: TSquare);
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    procedure ReplaceMap(const ADimensions: T3DPoint;
      const AMap: TSquareDynArray);
  public
    constructor CreateSized(AMaster: TMaster; const AID: TComponentID;
      ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer);

    procedure Assign(Source: TMap);

    function InMap(const Position: T3DPoint): Boolean;
    function InFloors(const Position: T3DPoint): Boolean; overload;
    function InFloors(Floor: Integer): Boolean; overload;

    function PlayersOn(const Position: T3DPoint): Integer;

    property Dimensions: T3DPoint read FDimensions;
    property ZoneWidth: Integer read FZoneWidth write FZoneWidth;
    property ZoneHeight: Integer read FZoneHeight write FZoneHeight;

    property Map[const Position: T3DPoint]: TSquare
      read GetMap write SetMap; default;

    property Outside[Floor: Integer]: TSquare
      read GetOutside write SetOutside;

    property LinearMapCount: Integer read GetLinearMapCount;
    property LinearMap[Index: Integer]: TSquare
      read GetLinearMap write SetLinearMap;
  end;

  {*
    Classe représentant un composant qui a une position sur une carte
    TPosComponent est la classe de base pour les composants qui doivent avoir
    une position sur la carte. Au contraire des composants de case, qui peuvent
    être placés à plusieurs endroits, les instances de TPosComponent sont
    "uniques" en ce qu'elles ne sont positionnées qu'à un seul endroit à la
    fois.
    Les descendants de TPosComponent peuvent aussi intercepter les événements
    de la case où il se trouve. Ils doivent pour ce faire sélectionner les
    événements à intercepter via la méthode SetWantedSquareEvents dans leur
    constructeur. Ils sont alors envoyé au composant par un message
    msgSquareEvent de type TSquareEventMessage.
    @author sjrd
    @version 5.0
  *}
  TPosComponent = class(TVisualComponent)
  private
    FQPos: TQualifiedPos;   /// Position qualifiée
    FDirection: TDirection; /// Direction

    /// Types d'événements de case que ce composant désire attraper
    FWantedSquareEvents: TSquareEventKinds;

    /// Indique si le composant désire attraper les messages de la case
    FWantMessages: Boolean;

    FZIndex: Integer;        /// Z-index
    FDefaultZIndex: Integer; /// Z-index par défaut

    function IsZIndexStored: Boolean;

    procedure SetZIndex(Value: Integer);
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
    procedure StoreDefaults; override;

    procedure SetWantedSquareEvents(Value: TSquareEventKinds);
    procedure SetWantMessages(Value: Boolean);

    procedure PositionChanged; virtual;
  public
    procedure ChangePosition(const AQPos: TQualifiedPos); overload; virtual;
    procedure ChangePosition(AMap: TMap; const APosition: T3DPoint); overload;
    procedure ChangePosition(const APosition: T3DPoint); overload;

    property QPos: TQualifiedPos read FQPos;
    property Map: TMap read FQPos.Map;
    property Position: T3DPoint read FQPos.Position;

    property WantedSquareEvents: TSquareEventKinds read FWantedSquareEvents;
    property WantMessages: Boolean read FWantMessages;
  published
    property Direction: TDirection read FDirection write FDirection
      default diNone;
    property ZIndex: Integer read FZIndex write SetZIndex stored IsZIndexStored;
  end;

  /// Classe de TPosComponent
  TPosComponentClass = class of TPosComponent;

  {*
    Modificateur de case
    TSquareModifier est une classe de base abstraite facilitant la création
    d'un composant qui modifie le comportement de la case où il se trouve.
    TSquareModifier se charge d'appeler SetWantedSquareEvents, et de dispatcher
    les événements de message
    @author sjrd
    @version 5.0
  *}
  TSquareModifier = class(TPosComponent)
  private
    /// Gestionnaires des événements
    FEventHandlers: array[TSquareEventKind] of TMoveContextMethod;

    procedure MessageSquareEvent(var Msg: TSquareEventMessage);
      message msgSquareEvent;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entering(Context: TMoveContext); virtual;
    procedure Exiting(Context: TMoveContext); virtual;
    procedure Entered(Context: TMoveContext); virtual;
    procedure Exited(Context: TMoveContext); virtual;
    procedure Execute(Context: TMoveContext); virtual;
    procedure Pushing(Context: TMoveContext); virtual;
  end;

  {*
    Plugin lié à un véhicule
    @author sjrd
    @version 5.0
  *}
  TVehiclePlugin = class(TPlugin)
  private
    FVehicle: TVehicle; /// Véhicule lié à ce plugin
  protected
    function GetIsDesignable: Boolean; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure DrawBefore(Context: TDrawSquareContext); override;
    procedure DrawAfter(Context: TDrawSquareContext); override;

    procedure Moving(Context: TMoveContext); override;
    procedure Moved(Context: TMoveContext); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;

    property Vehicle: TVehicle read FVehicle;
  end;

  {*
    Véhicule
    @author sjrd
    @version 5.0
  *}
  TVehicle = class(TSquareModifier)
  private
    FPlugin: TVehiclePlugin; /// Plugin lié

    FDirPainters: array[TDirection] of TPainter; /// Peintres par direction

    FController: TPlayer; /// Joueur qui contrôle actuellement ce véhicule

    function GetPainters(Dir: TDirection): TPainter;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetCategory: string; override;

    property Plugin: TVehiclePlugin read FPlugin;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure AttachController(AController: TPlayer); virtual;
    procedure DetachController(const AQPos: TQualifiedPos); overload; virtual;
    procedure DetachController; overload;

    procedure DrawBefore(Context: TDrawSquareContext); virtual;
    procedure DrawAfter(Context: TDrawSquareContext); virtual;

    procedure Moving(Context: TMoveContext); virtual;
    procedure Moved(Context: TMoveContext); virtual;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; virtual;

    property DirPainters[Dir: TDirection]: TPainter read GetPainters;

    property Controller: TPlayer read FController;
  published
    property NorthPainter: TPainter index diNorth read GetPainters;
    property EastPainter: TPainter index diEast read GetPainters;
    property SouthPainter: TPainter index diSouth read GetPainters;
    property WestPainter: TPainter index diWest read GetPainters;
  end;

  {*
    Composant mobile, qui a des actions propres et autonomes
    TMobileComponent est la classe de base pour les composants qui doivent
    se déplacer de façon autonome, ou avoir un quelconque comportement
    autonome.
    Chaque instance de TMobileComponent a son propre thread d'exécution de ses
    actions.
    @author sjrd
    @version 5.0
  *}
  TMobileComponent = class(TPosComponent)
  private
    FActionLock: TCriticalSection;   /// Verrou pour les actions
    FInActionLock: TCriticalSection; /// Verrou pour l'intérieur des actions
    FActionCoroutine: TCoroutine;    /// Coroutine d'exécution des actions
    FActionMessagePtr: Pointer;      /// Pointeur sur le message pour l'action

    procedure MessageRunMethod(var Msg: TRunMethodMessage);
      message msgRunMethod;

    procedure ActionProc(Coroutine: TCoroutine);

    function TryPause: Boolean;
    procedure Resume;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure SendMessage(var Msg);
    procedure RunMethod(const Method: TThreadMethod);
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
  TPlayerMode = class(TInterfacedFunLabyPersistent, IPlayerMode)
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
  protected
    procedure DrawSquares(Context: TDrawViewContext; Ceiling: Boolean);
    procedure DrawPosComponents(Context: TDrawViewContext);
    procedure DrawPosComponent(Context: TDrawViewContext;
      PosComponent: TPosComponent);

    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetUseZone: Boolean; override;
  public
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
  TPlayer = class(TMobileComponent)
  private
    FMode: IPlayerMode;         /// Mode principal
    FModeStack: IInterfaceList; /// Pile des modes sauvegardés
    FShowCounter: Integer;      /// Compteur de visibilité
    FColor: TColor32;           /// Couleur
    FViewBorderSize: Integer;   /// Taille de la bordure de la vue
    FPlugins: TObjectList;      /// Liste des plug-in
    FAttributes: TStrings;      /// Liste des attributs
    FPlayState: TPlayState;     /// État de victoire/défaite
    FFoundObjects: TObjectList; /// Objets trouvés (dans l'ordre)

    FDrawMode: TPlayerDrawMode;                  /// Mode de dessin
    FColoredPainterCache: TBitmap32;             /// Cache du peintre coloré
    FDirPainters: array[TDirection] of TPainter; /// Peintres par direction

    FDefaultTemporization: Cardinal; /// Temporisation par défaut

    /// Verrou global pour le joueur
    FLock: TMultiReadExclusiveWriteSynchronizer;

    function GetPluginListStr: string;
    procedure SetPluginListStr(const Value: string);
    function GetModeListStr: string;
    procedure SetModeListStr(const Value: string);
    function GetFoundObjectsStr: string;
    procedure SetFoundObjectsStr(const Value: string);

    procedure GetPluginList(out PluginList: TPluginDynArray);

    procedure UpdateColoredPainterCache;

    procedure PrivDraw(Context: TDrawSquareContext); override;

    procedure MessagePressKey(var Msg: TPlayerPressKeyMessage);
      message msgPressKey;

    procedure FoundObject(ObjectDef: TObjectDef);

    function ResolveSoundHRef(const HRef: string): TFileName;
    procedure InternalPlaySound(const Sound: string; Flags: LongWord;
      Module: HModule = 0);

    function GetVisible: Boolean;

    procedure SetColor(Value: TColor32);

    function GetAttribute(const AttrName: string): Integer;
    procedure SetAttribute(const AttrName: string; Value: Integer);

    function GetPainters(Dir: TDirection): TPainter;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetCategory: string; override;

    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure Dispatch(var Msg); override;
    procedure DefaultHandler(var Msg); override;

    procedure GetAttributes(Attributes: TStrings);
    procedure GetPluginIDs(PluginIDs: TStrings);

    procedure GetFoundObjects(ObjectDefs: TObjectList);

    procedure ChangeMode(ModeClass: TPlayerModeClass);
    procedure BeginTempMode(ModeClass: TPlayerModeClass);
    procedure EndTempMode;

    procedure AddPlugin(Plugin: TPlugin);
    procedure RemovePlugin(Plugin: TPlugin);
    function HasPlugin(Plugin: TPlugin): Boolean;

    function AbleTo(const Action: TPlayerAction; Param: Integer = 0): Boolean;
    function DoAction(const Action: TPlayerAction; Param: Integer = 0): Boolean;

    procedure Move(Dir: TDirection; Key: Word; Shift: TShiftState;
      out Redo: Boolean; out RedoDelay: Cardinal); overload;
    procedure Move(Dir: TDirection; out Redo: Boolean;
      out RedoDelay: Cardinal); overload;
    procedure Move(Dir: TDirection; Key: Word; Shift: TShiftState); overload;
    procedure Move(Dir: TDirection); overload;

    function IsMoveAllowed(Context: TMoveContext): Boolean; overload;
    function IsMoveAllowed(const Dest: T3DPoint; Key: Word;
      Shift: TShiftState): Boolean; overload;
    function IsMoveAllowed(const Dest: T3DPoint): Boolean; overload;

    procedure MoveTo(Context: TMoveContext; Execute: Boolean = True); overload;
    procedure MoveTo(const Dest: T3DPoint; Execute: Boolean;
      out Redo: Boolean; out RedoDelay: Cardinal); overload;
    procedure MoveTo(const Dest: T3DPoint; Execute: Boolean = False); overload;
    procedure MoveTo(const Dest: TQualifiedPos; Execute: Boolean;
      out Redo: Boolean; out RedoDelay: Cardinal); overload;
    procedure MoveTo(const Dest: TQualifiedPos;
      Execute: Boolean = False); overload;

    procedure NaturalMoving(Redo: Boolean; RedoDelay: Cardinal);

    procedure ChangePosition(const AQPos: TQualifiedPos); override;

    procedure Show;
    procedure Hide;

    procedure ShowMessage(const Text: string);

    function ShowSelectionMsg(const Prompt: string;
      const Answers: array of string; Default: Integer = 0;
      ShowOnlySelected: Boolean = False): Integer; overload;
    function ShowSelectionMsg(const Prompt: string; Answers: TStrings;
      Default: Integer = 0;
      ShowOnlySelected: Boolean = False): Integer; overload;

    function ShowSelectNumberMsg(const Prompt: string;
      Default, Min, Max: Integer): Integer;

    procedure PlaySound(const HRef: string);

    procedure Win;
    procedure Lose;

    procedure DrawView(Bitmap: TBitmap32); virtual;
    procedure PressKey(Key: Word; Shift: TShiftState);

    procedure WaitForKey(out Key: Word; out Shift: TShiftState);
    procedure WaitForSpecificKey(Key: Word; Shift: TShiftState = []);

    property Mode: IPlayerMode read FMode;
    property Visible: Boolean read GetVisible;
    property Attribute[const AttrName: string]: Integer
      read GetAttribute write SetAttribute;
    property PlayState: TPlayState read FPlayState;
  published
    property Color: TColor32 read FColor write SetColor
      default DefaultPlayerColor;
    property ViewBorderSize: Integer read FViewBorderSize write FViewBorderSize
      default DefaultViewBorderSize;

    property DrawMode: TPlayerDrawMode read FDrawMode write FDrawMode
      default dmColoredPainter;
    property NorthPainter: TPainter index diNorth read GetPainters;
    property EastPainter: TPainter index diEast read GetPainters;
    property SouthPainter: TPainter index diSouth read GetPainters;
    property WestPainter: TPainter index diWest read GetPainters;

    property DefaultTemporization: Cardinal
      read FDefaultTemporization write FDefaultTemporization
      default FunLabyUtils.DefaultTemporization;
  end;

  {*
    Classe de base pour les timers (événements à déclencher à un moment donné)
    N'utilisez pas directement TTimerEntry, mais utilisez les méthodes de
    TTimerList pour programmer des événements à déclencher.
    @author sjrd
    @version 5.0
  *}
  TTimerEntry = class(TFunLabyPersistent)
  private
    FTickCount: Cardinal; /// Tick-count auquel déclencher cet événement

    procedure InternalExecuteAndFree;
  protected
    function GetHostComponent: TMobileComponent; virtual;

    procedure Execute; virtual; abstract;
  public
    constructor Create(ATickCount: Cardinal);
    constructor ReCreate; virtual;

    procedure ExecuteAndFree;
  published
    property TickCount: Cardinal read FTickCount write FTickCount;
  end;

  /// Classe de TTimerEntry
  TTimerEntryClass = class of TTimerEntry;

  {*
    Timer qui envoie un message de notification à un objet
    Un message de notification est un message sans aucun paramètre en dehors de
    l'ID du message. Si le destinataire est un TPlayer, le message envoyé est
    quand même garantit de type TPlayerMessage.
    @author sjrd
    @version 5.0
  *}
  TNotificationMsgTimerEntry = class(TTimerEntry)
  private
    FDestObject: TFunLabyComponent; /// Objet auquel envoyer le message
    FMsgID: Word;                   /// ID du message
  protected
    function GetHostComponent: TMobileComponent; override;

    procedure Execute; override;
  public
    constructor Create(ATickCount: Cardinal;
      ADestObject: TFunLabyComponent; AMsgID: Word);
  published
    property DestObject: TFunLabyComponent read FDestObject write FDestObject;
    property MsgID: Word read FMsgID write FMsgID;
  end;

  {*
    Collection de timers (événements déclenchés après un temps donné)
    @author sjrd
    @version 5.0
  *}
  TTimerCollection = class(TFunLabyCollection)
  private
    FMaster: TMaster;              /// Maître FunLabyrinthe
    FThread: TMethodThread;        /// Thread de déclenchement des timers
    FAddedEvent: TEvent;           /// Événement déclenché à l'ajout d'un timer
    FLock: TCriticalSection;       /// Verrou d'accès à la liste des timers
    FActionLock: TCriticalSection; /// Verrou d'exécution des actions

    procedure ThreadProc(Thread: TMethodThread);

    procedure Start;

    function TryPause: Boolean;
    procedure Resume;
  protected
    procedure BeginState(State: TPersistentState); override;
    procedure EndState(State: TPersistentState); override;

    procedure Notify(Item: TFunLabyPersistent;
      Action: TListNotification); override;

    function CreateItem(ItemClass: TFunLabyPersistentClass):
      TFunLabyPersistent; override;
  public
    constructor Create(AMaster: TMaster);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    procedure ScheduleNotificationMsg(Delay: Cardinal;
      DestObject: TFunLabyComponent; MsgID: Word);
    procedure ScheduleCustom(TimerEntry: TTimerEntry);

    property Master: TMaster read FMaster;
  end;

  {*
    Maître FunLabyrinthe
    TMaster gère les différents composants de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TMaster = class(TFunLabyPersistent)
  private
    /// Callback pour trouver une ressource
    FFindResourceCallback: TFindResourceCallback;

    FImagesMaster: TImagesMaster;   /// Maître d'images
    FComponents: TObjectList;       /// Liste de tous les composants
    FPlugins: TObjectList;          /// Liste des plug-in
    FObjectDefs: TObjectList;       /// Liste des définitions d'objet
    FFields: TObjectList;           /// Liste des terrains
    FEffects: TObjectList;          /// Liste des effets
    FTools: TObjectList;            /// Liste des outils
    FObstacles: TObjectList;        /// Liste des obstacles
    FSquares: TObjectList;          /// Liste des cases
    FMaps: TObjectList;             /// Liste des cartes
    FPosComponents: TObjectList;    /// Liste des objets avec position
    FMobileComponents: TObjectList; /// Liste des objets mobiles (autonomes)
    FPlayers: TObjectList;          /// Liste des joueurs
    FComponentsByID: TStrings;      /// Table de hashage ID -> composant

    FEditing: Boolean;            /// Indique si on est en mode édition
    FBeginTickCount: Cardinal;    /// Tick count système au lancement
    FPaused: Boolean;             /// Indique si le jeu est en pause
    FPauseTickCount: Cardinal;    /// Tick count au moment de la pause
    FTerminated: Boolean;         /// Indique si la partie est terminée

    FOrderedPosComponents: TObjectList;  /// PosComponents ordonnés par Z-index
    FOrderedPosComponentsValid: Boolean; /// FOrderedPosComponents est valide

    FTimers: TTimerCollection;    /// Collection des timers

    procedure InvalidateOrderedPosComponents;
    procedure EnsureOrderedPosComponentsValid;

    function GetComponentAs(const ID: TComponentID;
      RequiredClass: TFunLabyComponentClass = nil): TFunLabyComponent;
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

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TFunLabyComponent;
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
    function GetPosComponentCount: Integer;
    function GetPosComponents(Index: Integer): TPosComponent;
    function GetOrderedPosComponents(Index: Integer): TPosComponent;
    function GetMobileComponentCount: Integer;
    function GetMobileComponents(Index: Integer): TMobileComponent;
    function GetPlayerCount: Integer;
    function GetPlayers(Index: Integer): TPlayer;

    function GetTickCount: Cardinal;
    procedure SetTickCount(Value: Cardinal);

    procedure AddComponent(Component: TFunLabyComponent);
    procedure RemoveComponent(Component: TFunLabyComponent);
    procedure ComponentIDChanged(Component: TFunLabyComponent);

    procedure Terminate;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
    procedure EndState(State: TPersistentState); override;
  public
    constructor Create(AEditing: Boolean;
      const AFindResourceCallback: TFindResourceCallback);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    procedure StoreDefaults; override;

    function ComponentExists(const ID: TComponentID): Boolean;
    procedure CheckComponentID(const ID: TComponentID);

    function FindResource(const HRef: string; Kind: TResourceKind): TFileName;

    function SquareByComps(
      const Field, Effect, Tool, Obstacle: TComponentID): TSquare; overload;
    function SquareByComps(Field: TField; Effect: TEffect = nil;
      Tool: TTool = nil; Obstacle: TObstacle = nil): TSquare; overload;

    procedure RegisterComponents(RegisterComponent: TRegisterComponentProc);

    function CreateAdditionnalComponent(ComponentClass: TFunLabyComponentClass;
      const ID: TComponentID): TFunLabyComponent;

    function TryPause: Boolean;
    procedure Resume;

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

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: TFunLabyComponent read GetComponents;
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
    property PosComponentCount: Integer read GetPosComponentCount;
    property PosComponents[Index: Integer]: TPosComponent read GetPosComponents;
    property OrderedPosComponents[Index: Integer]: TPosComponent
      read GetOrderedPosComponents;
    property MobileComponentCount: Integer read GetMobileComponentCount;
    property MobileComponents[Index: Integer]: TMobileComponent
      read GetMobileComponents;
    property PlayerCount: Integer read GetPlayerCount;
    property Players[Index: Integer]: TPlayer read GetPlayers;

    property Editing: Boolean read FEditing;
    property TickCount: Cardinal read GetTickCount;
    property Paused: Boolean read FPaused;
    property Terminated: Boolean read FTerminated;

    property Timers: TTimerCollection read FTimers;
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

  /// Application d'une direction vers la direction opposée
  NegDir: array[TDirection] of TDirection = (
    diNone, diSouth, diWest, diNorth, diEast
  );

  /// Application d'une direction vers la direction à sa droite
  RightDir: array[TDirection] of TDirection = (
    diNone, diEast, diSouth, diWest, diNorth
  );

  /// Application d'une direction vers la direction à sa gauche
  LeftDir: array[TDirection] of TDirection = (
    diNone, diWest, diNorth, diEast, diSouth
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
  /// Dossier des fichiers labyrinthe
  fLabyrinthsDir: string = 'Labyrinths\';
  /// Dossier des fichiers sauvegarde
  fSaveguardsDir: string = 'Saveguards\';
  /// Dossier des screenshots
  fScreenshotsDir: string = 'Screenshots\';
  /// Dossier des plug-in de l'éditeur
  fEditPluginDir: string = 'EditPlugins\';

procedure ShowFunLabyAbout;

function FunLabyEncoding: TEncoding;

function PointBehind(const Src: T3DPoint; Dir: TDirection): T3DPoint;
function PointBefore(const Src: T3DPoint; Dir: TDirection): T3DPoint;

function CreateEmptySquareBitmap: TBitmap32;
function SquareRect(X, Y: Integer): TRect;
procedure EmptyRect(Bitmap: TBitmap32; Rect: TRect);
procedure EmptySquareRect(Bitmap: TBitmap32; X: Integer = 0; Y: Integer = 0);
procedure DrawBitmap32ToCanvas(Canvas: TCanvas; const DestRect: TRect;
  Bitmap: TBitmap32; BackgroundColor: TColor);

function SameRect(const Left, Right: TRect): Boolean;

function SameQPos(const Left, Right: TQualifiedPos): Boolean;
function IsNoQPos(const QPos: TQualifiedPos): Boolean;

procedure FunLabyRegisterClass(PersistentClass: TFunLabyPersistentClass);
procedure FunLabyUnregisterClass(PersistentClass: TFunLabyPersistentClass);

procedure FunLabyRegisterClasses(Classes: array of TFunLabyPersistentClass);
procedure FunLabyUnregisterClasses(Classes: array of TFunLabyPersistentClass);

function FunLabyGetClass(const ClassName: string): TFunLabyPersistentClass;
function FunLabyFindClass(const ClassName: string): TFunLabyPersistentClass;

implementation

uses
  IniFiles, StrUtils, Forms, Math, MMSystem, msxml, ScStrUtils, ScXML,
  ScCompilerMagic, ScTypInfo, GraphicEx, FunLabyFilers;

const
  /// Z-index par défaut pour les joueurs
  DefaultPlayerZIndex = 1024;

type
  TFunLabyEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

  {*
    Peintre d'un TPlayer
    TPlayer utilise un peintre spécialisé pour recalculer son image
    FColoredPainterCache à chaque fois que l'image du peintre change.
    @author sjrd
    @version 5.0
  *}
  TPlayerPainter = class(TPainter)
  private
    FPlayer: TPlayer; /// Joueur propriétaire

    procedure DescriptionChange(Sender: TObject);
  public
    constructor Create(AMaster: TImagesMaster; APlayer: TPlayer);
  end;

const
  /// Code de format d'un flux carte (TMap) (correspond à '.flm')
  MapStreamFormatCode: Longint = $6D6C662E;

  /// Version courante du format d'un flux carte (TMap)
  MapStreamVersion = 1;

  /// Fichier de l'image du joueur
  fPlayer = 'Pawns/Player';

var
  FFunLabyEncoding: TEncoding = nil;
  FunLabyRegisteredClasses: TStrings = nil;

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
    ProgramVersion := Format(SFullVersionNumber,
      [CurrentVersion, CurrentMinorVersion]);
    AuthorName := FunLabyAuthorName;
    AuthorEMail := FunLabyAuthorEMail;
    WebSite := FunLabyWebSite;

    Execute;
  finally
    Free;
  end;
end;

{*
  Crée la variable contenant l'encodage de FunLabyrinthe
*}
procedure CreateFunLabyEncoding;
begin
  FFunLabyEncoding := TFunLabyEncoding.Create;
end;

{*
  Encodage des fichiers FunLabyrinthe
  Il s'agit d'un encodage UTF-8 sans préambule. Cet encodage devrait être
  utilisé par la plupart des opérations sur les fichiers textes.
  @return Encodage des fichiers FunLabyrinthe
*}
function FunLabyEncoding: TEncoding;
begin
  Result := FFunLabyEncoding;
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
  Crée un bitmap de case vide (entièrement transparent)
  @return Bitmap créé
*}
function CreateEmptySquareBitmap: TBitmap32;
begin
  Result := TBitmap32.Create;
  try
    Result.SetSize(SquareSize, SquareSize);
    Result.Clear(clTransparent32);
    Result.DrawMode := dmBlend;
    Result.CombineMode := cmMerge;
  except
    Result.Free;
    raise;
  end;
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
  @param Bitmap   Bitmap à traiter
  @param Rect     Rectangle à effacer
*}
procedure EmptyRect(Bitmap: TBitmap32; Rect: TRect);
begin
  Bitmap.FillRectS(Rect, clTransparent32);
end;

{*
  Efface un rectangle de case sur un canevas (avec du transparent)
  @param Bitmap   Bitmap à traiter
  @param X        Bord gauche du rectangle
  @param Y        Bord supérieur du rectangle
*}
procedure EmptySquareRect(Bitmap: TBitmap32; X: Integer = 0; Y: Integer = 0);
begin
  EmptyRect(Bitmap, SquareRect(X, Y));
end;

{*
  Dessine un bitmap 32 sur un canevas VCL
  @param Canvas            Canevas cible
  @param DestRect          Rectangle dans lequel dessiner le bitmap
  @param Bitmap            Bitmap à dessiner
  @param BackgroundColor   Couleur de fond sur le canevas
*}
procedure DrawBitmap32ToCanvas(Canvas: TCanvas; const DestRect: TRect;
  Bitmap: TBitmap32; BackgroundColor: TColor);
var
  TempBitmap: TBitmap32;
begin
  TempBitmap := TBitmap32.Create;
  try
    TempBitmap.SetSize(Bitmap.Width, Bitmap.Height);
    TempBitmap.Clear(Color32(BackgroundColor));
    TempBitmap.Draw(0, 0, Bitmap);
    Canvas.CopyRect(DestRect, TempBitmap.Canvas, TempBitmap.BoundsRect);
  finally
    TempBitmap.Free;
  end;
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
  Teste si deux positions qualifiées sont égales
  @param Left    Position de gauche
  @param Right   Position de droite
  @return True si les positions sont égales, False sinon
*}
function SameQPos(const Left, Right: TQualifiedPos): Boolean;
begin
  Result := (Left.Map = Right.Map) and
    Same3DPoint(Left.Position, Right.Position);
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

{*
  Teste si une propriété a sa valeur par défaut
  @param Instance   Instance
  @param PropInfo   PropInfo de la propriété à tester
*}
function IsPropDefaultValue(Instance: TFunLabyPersistent;
  PropInfo: PPropInfo): Boolean;
begin
  case PropInfo.PropType^.Kind of
    tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
      Result := (PropInfo.Default <> Integer($80000000)) and
        (GetOrdProp(Instance, PropInfo) = PropInfo.Default);
    tkFloat:
      Result := GetFloatProp(Instance, PropInfo) = 0.0;
    tkString, tkLString, tkWString, tkUString:
      Result := GetStrProp(Instance, PropInfo) = '';
    tkClass:
      Result := GetOrdProp(Instance, PropInfo) = 0;
    tkInt64:
      Result := GetInt64Prop(Instance, PropInfo) = 0;
  else
    Result := False;
  end;
end;

{*
  Recense une classe persistente FunLabyrinthe
  @param PersistentClass   Classe à recenser
*}
procedure FunLabyRegisterClass(PersistentClass: TFunLabyPersistentClass);
begin
  if FunLabyRegisteredClasses = nil then
    FunLabyRegisteredClasses := THashedStringList.Create;

  if FunLabyRegisteredClasses.IndexOfObject(TObject(PersistentClass)) < 0 then
    FunLabyRegisteredClasses.AddObject(PersistentClass.ClassName,
      TObject(PersistentClass));
end;

{*
  Dérecense une classe persistente FunLabyrinthe
  @param PersistentClass   Classe à dérecenser
*}
procedure FunLabyUnregisterClass(PersistentClass: TFunLabyPersistentClass);
var
  Index: Integer;
begin
  if FunLabyRegisteredClasses = nil then
    Exit;

  Index := FunLabyRegisteredClasses.IndexOf(PersistentClass.ClassName);
  if (Index >= 0) and
    (FunLabyRegisteredClasses.Objects[Index] = TObject(PersistentClass)) then
    FunLabyRegisteredClasses.Delete(Index);
end;

{*
  Recense une liste de classes persistentes FunLabyrinthe
  @param Classes   Classes à recenser
*}
procedure FunLabyRegisterClasses(Classes: array of TFunLabyPersistentClass);
var
  I: Integer;
begin
  for I := Low(Classes) to High(Classes) do
    FunLabyRegisterClass(Classes[I]);
end;

{*
  Dérecense une liste de classes persistentes FunLabyrinthe
  @param Classes   Classes à dérecenser
*}
procedure FunLabyUnregisterClasses(Classes: array of TFunLabyPersistentClass);
var
  I: Integer;
begin
  for I := Low(Classes) to High(Classes) do
    FunLabyUnregisterClass(Classes[I]);
end;

{*
  Obtient une classe persistente par son nom
  @param ClassName   Nom de la classe recherchée
  @return Classe persistente dont le nom a été spécifié, ou nil si non trouvé
*}
function FunLabyGetClass(const ClassName: string): TFunLabyPersistentClass;
var
  Index: Integer;
begin
  if FunLabyRegisteredClasses = nil then
    Result := nil
  else
  begin
    Index := FunLabyRegisteredClasses.IndexOf(ClassName);
    if Index < 0 then
      Result := nil
    else
      Result := TFunLabyPersistentClass(
        FunLabyRegisteredClasses.Objects[Index]);
  end;
end;

{*
  Obtient une classe persistente par son nom
  @param ClassName   Nom de la classe recherchée
  @return Classe persistente dont le nom a été spécifié
  @throws
*}
function FunLabyFindClass(const ClassName: string): TFunLabyPersistentClass;
begin
  Result := FunLabyGetClass(ClassName);
  if Result = nil then
    raise EClassNotFound.CreateFmt(SClassNotFound, [ClassName]);
end;

{------------------------}
{ TFunLabyEncoding class }
{------------------------}

{*
  [@inheritDoc]
*}
function TFunLabyEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{----------------------}
{ TPlayerPainter class }
{----------------------}

{*
  Crée une instance de TPlayerPainter
  @param AMaster   Maître d'images
  @param APlayer   Joueur propriétaire
*}
constructor TPlayerPainter.Create(AMaster: TImagesMaster; APlayer: TPlayer);
begin
  inherited Create(AMaster);

  TStringList(FDescription).OnChange := DescriptionChange;
  FPlayer := APlayer;
end;

{*
  [@inheritDoc]
*}
procedure TPlayerPainter.DescriptionChange(Sender: TObject);
begin
  inherited DescriptionChange(Sender);

  FPlayer.UpdateColoredPainterCache;
end;

{----------------------}
{ TQualifiedPos record }
{----------------------}

{*
  Indique si cette position qualifiée est la position nulle
  @return True si c'est la position nulle, False sinon
*}
function TQualifiedPos.GetIsNoQPos: Boolean;
begin
  Result := Map = nil;
end;

{*
  Indique si cette position qualifiée est dans la carte
  @return True si elle est dans la carte, False sinon
*}
function TQualifiedPos.GetIsInside: Boolean;
begin
  Result := Map.InMap(Position);
end;

{*
  Indique si cette position qualifiée est hors de la carte
  @return True si elle est hors de la carte, False sinon
*}
function TQualifiedPos.GetIsOutside: Boolean;
begin
  Result := not Map.InMap(Position);
end;

{*
  Case présente à cette position qualifiée
  @return Case présente à cette position qualifiée
*}
function TQualifiedPos.GetSquare: TSquare;
begin
  Result := Map[Position];
end;

{*
  Terrain présent à cette position qualifiée
  @return Terrain présent à cette position qualifiée
*}
function TQualifiedPos.GetField: TField;
begin
  Result := Map[Position].Field;
end;

{*
  Effet présent à cette position qualifiée
  @return Effet présent à cette position qualifiée
*}
function TQualifiedPos.GetEffect: TEffect;
begin
  Result := Map[Position].Effect;
end;

{*
  Outil présent à cette position qualifiée
  @return Outil présent à cette position qualifiée
*}
function TQualifiedPos.GetTool: TTool;
begin
  Result := Map[Position].Tool;
end;

{*
  Obstacle présent à cette position qualifiée
  @return Obstacle présent à cette position qualifiée
*}
function TQualifiedPos.GetObstacle: TObstacle;
begin
  Result := Map[Position].Obstacle;
end;

{*
  Modifie la case présente à cette position qualifiée
  @param Value   Nouvelle case à placer
*}
procedure TQualifiedPos.SetSquare(Value: TSquare);
begin
  Map[Position] := Value;
end;

{*
  Modifie le terrain présent à cette position qualifiée
  @param Value   Nouveau terrain à placer
*}
procedure TQualifiedPos.SetField(Value: TField);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Value, Effect, Tool, Obstacle);
end;

{*
  Modifie l'effet présent à cette position qualifiée
  @param Value   Nouvel effet à placer
*}
procedure TQualifiedPos.SetEffect(Value: TEffect);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Field, Value, Tool, Obstacle);
end;

{*
  Modifie l'outil présent à cette position qualifiée
  @param Value   Nouvel outil à placer
*}
procedure TQualifiedPos.SetTool(Value: TTool);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Field, Effect, Value, Obstacle);
end;

{*
  Modifie l'obstacle présent à cette position qualifiée
  @param Value   Nouvel obstacle à placer
*}
procedure TQualifiedPos.SetObstacle(Value: TObstacle);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Field, Effect, Tool, Value);
end;

{*
  Nombre de composants
  @return Nombre de composants
*}
function TQualifiedPos.GetComponentCount: Integer;
begin
  Result := Map[Position].ComponentCount;
end;

{*
  Tableau zero-based des composants
  @param Index   Index compris entre 0 inclus et ComponentCount exclus
  @return Composant à l'index spécifié
*}
function TQualifiedPos.GetComponents(Index: Integer): TSquareComponent;
begin
  Result := Map[Position].Components[Index];
end;

{*
  Modifie un composant
  @param Index   Index compris entre 0 inclus et ComponentCount exclus
  @param Value   Nouveau composant à placer à l'index spécifié
*}
procedure TQualifiedPos.SetComponents(Index: Integer; Value: TSquareComponent);
begin
  case Index of
    0: Field := Value as TField;
    1: Effect := Value as TEffect;
    2: Tool := Value as TTool;
    3: Obstacle := Value as TObstacle;
  else
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  end;
end;

{--------------------------}
{ TDrawSquareContext class }
{--------------------------}

{*
  Crée un contexte de dessin de case
  @param ABitmap   Bitmap cible
*}
constructor TDrawSquareContext.Create(ABitmap: TBitmap32);
begin
  Create(ABitmap, 0, 0, NoQPos);
end;

{*
  Crée un contexte de dessin de case
  @param ABitmap   Bitmap cible
  @param X         Abscisce où dessiner
  @param Y         Abscisce où dessiner
*}
constructor TDrawSquareContext.Create(ABitmap: TBitmap32; X, Y: Integer);
begin
  Create(ABitmap, X, Y, NoQPos);
end;

{*
  Crée un contexte de dessin de case
  @param ABitmap   Bitmap cible
  @param X         Abscisce où dessiner
  @param Y         Abscisce où dessiner
  @param AQPos     Position qualifiée à dessiner
*}
constructor TDrawSquareContext.Create(ABitmap: TBitmap32; X, Y: Integer;
  const AQPos: TQualifiedPos);
begin
  inherited Create;

  FBitmap := ABitmap;
  FX := X;
  FY := Y;
  FSquareRect := FunLabyUtils.SquareRect(X, Y);

  FIsNowhere := IsNoQPos(AQPos);
  FQPos := AQPos;
end;

{*
  Crée un contexte de dessin de case
  @param Source   Contexte à imiter
  @param AQPos    Position qualifiée à dessiner
*}
constructor TDrawSquareContext.Create(Source: TDrawSquareContext;
  const AQPos: TQualifiedPos);
begin
  Create(Source.Bitmap, Source.X, Source.Y, AQPos);

  Assign(Source);
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
  Spécifie le contexte de dessin de la vue
  @param ADrawViewContext   Contexte de dessin de la vue
*}
procedure TDrawSquareContext.SetDrawViewContext(
  ADrawViewContext: TDrawViewContext);
begin
  FDrawViewContext := ADrawViewContext;
  FTickCount := ADrawViewContext.TickCount;
end;

{*
  Spécifie le tick count pour ce contexte de dessin
  @param ATickCount   Tick count pour ce contexte de dessin
*}
procedure TDrawSquareContext.SetTickCount(ATickCount: Cardinal);
begin
  FTickCount := ATickCount;
end;

{*
  Copie les informations de contexte depuis un autre contexte
  Sont copiés : DrawViewContext, TickCount et Player.
  @param Source   Contexte source
*}
procedure TDrawSquareContext.Assign(Source: TDrawSquareContext);
begin
  FDrawViewContext := Source.DrawViewContext;
  FTickCount := Source.TickCount;
  FPlayer := Source.Player;
end;

{*
  Dessine un bitmap de case dans ce contexte
*}
procedure TDrawSquareContext.DrawSquareBitmap(SquareBitmap: TBitmap32);
var
  SrcRect: TRect;
begin
  SrcRect := SquareBitmap.BoundsRect;
  if (SrcRect.Right > SquareSize) or (SrcRect.Bottom > SquareSize) then
  begin
    if IsNowhere then
      SrcRect := BaseSquareRect
    else
      SrcRect := FunLabyUtils.SquareRect(
        Pos.X mod (SrcRect.Right div SquareSize) * SquareSize,
        Pos.Y mod (SrcRect.Bottom div SquareSize) * SquareSize);
  end;

  if SquareBitmap is TAnimatedBitmap32 then
    TAnimatedBitmap32(SquareBitmap).DrawAtTimeTo(
      TickCount, Bitmap, X, Y, SrcRect)
  else
    SquareBitmap.DrawTo(Bitmap, X, Y, SrcRect);
end;

{------------------------}
{ TDrawViewContext class }
{------------------------}

{*
  Crée un contexte de dessin d'une vue
  @param APlayerMode   Mode principal du joueur dont dessiner la vue
  @param ABitmap       Bitmap cible
*}
constructor TDrawViewContext.Create(const APlayerMode: IPlayerMode;
  ABitmap: TBitmap32);
begin
  inherited Create;

  FPlayerMode := APlayerMode;
  FPlayer := APlayerMode.Player;
  FBitmap := ABitmap;

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
constructor TKeyEventContext.Create(APlayer: TPlayer; AKey: Word;
  AShift: TShiftState);
begin
  inherited Create;

  FPlayer := APlayer;

  FKey := AKey;
  FShift := AShift;
end;

{----------------------}
{ Classe TImagesMaster }
{----------------------}

{*
  Crée une instance de TImagesMaster
*}
constructor TImagesMaster.Create;
var
  EmptySquare: TBitmap32;
begin
  inherited Create;

  FExtensions := TStringList.Create;
  FileFormatList.GetExtensionList(FExtensions);
  FExtensions.Move(FExtensions.IndexOf(PreferredImageExtension), 0);
  FExtensions.Add(PainterExtension);

  FImgList := TObjectList.Create;
  FImgNames := THashedStringList.Create;

  FImgNames.Add('');

  EmptySquare := CreateEmptySquareBitmap;
  try
    FImgList.Add(EmptySquare);
  except
    EmptySquare.Free;
    raise;
  end;
end;

{*
  Détruit l'instance
*}
destructor TImagesMaster.Destroy;
begin
  FImgNames.Free;
  FImgList.Free;

  FExtensions.Free;

  inherited;
end;

{*
  Charge un bitmap depuis un fichier peintre
  @param FileName   Nom du fichier peintre à charger
  @return Le bitmap chargé
*}
function TImagesMaster.LoadBitmapFromPainterFile(
  const FileName: TFileName): TBitmap32;
var
  Painter: TPainter;
  Document: IXMLDOMDocument;
  PainterBitmap: TBitmap32;
begin
  Result := nil;

  Painter := TPainter.Create(Self);
  try
    try
      Document := LoadXMLDocumentFromFile(FileName);
      TFunLabyXMLReader.ReadPersistent(Painter, Document.documentElement);

      PainterBitmap := Painter.GetBitmap;

      if PainterBitmap <> nil then
      begin
        if PainterBitmap is TAnimatedBitmap32 then
          Result := TAnimatedBitmap32.Create
        else
          Result := TBitmap32.Create;

        Result.Assign(PainterBitmap);
      end;
    except
      on Error: EInOutError do
      begin
        Result.Free;
        Result := nil;
      end;
    end;
  finally
    Painter.Free;
  end;
end;

{*
  Charge une image d'après son nom dans un bitmap
  @param ImgName   Nom de l'image
  @param Bitmap    Bitmap destination
  @return Le bitmap chargé, ou nil si non trouvé
*}
function TImagesMaster.LoadImage(const ImgName: string): TBitmap32;
var
  FileName: TFileName;
begin
  FileName := ResolveImgName(ImgName);

  if FileName = '' then
  begin
    // Not found
    Result := nil;
  end else if AnsiSameText(ExtractFileExt(FileName), '.'+PainterExtension) then
  begin
    // Painter file
    Result := LoadBitmapFromPainterFile(FileName);
  end else
  begin
    // Regular image file
    Result := LoadBitmapFromFile(FileName);
  end;
end;

{*
  Ajoute une image à partir de son nom au maître d'images
  Aucune image de même nom ne doit déjà exister dans la liste interne.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoyé.
  @param ImgName   Nom de l'image
  @return Index de l'image nouvellement ajoutée, ou existante
*}
function TImagesMaster.Add(const ImgName: string): Integer;
var
  NewBitmap: TBitmap32;
begin
  Result := 0;

  NewBitmap := LoadImage(ImgName);
  if NewBitmap = nil then
    Exit;

  try
    FImgList.Add(NewBitmap);
  except
    NewBitmap.Free;
    Exit;
  end;

  try
    Result := FImgNames.Add(ImgName);
  except
    FImgList.Delete(FImgList.Count-1);
  end;
end;

{*
  Résoud un nom d'image (sans indicateur de sous-image) en nom de fichier
  @param BaseImgName   Nom d'image (sans indicateur de sous-image @x,y)
  @return Nom de fichier pour cette image, ou '' si non trouvé
*}
function TImagesMaster.ResolveImgName(const ImgName: string): TFileName;
var
  I: Integer;
begin
  Result := fSquaresDir + AnsiReplaceStr(ImgName, '/', '\');
  if FileExists(Result) then
    Exit;

  for I := 0 to FExtensions.Count-1 do
  begin
    if FileExists(Result+'.'+FExtensions[I]) then
    begin
      Result := Result+'.'+FExtensions[I];
      Exit;
    end;
  end;

  Result := '';
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
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
    Result := Add(ImgName);
end;

{*
  Dessine une image à partir de son index
  Draw dessine l'image indiquée sur un canevas.
  @param Index     Index de l'image à dessiner
  @param Context   Contexte de dessin de la case
*}
procedure TImagesMaster.Draw(Index: Integer; Context: TDrawSquareContext);
begin
  Context.DrawSquareBitmap(TBitmap32(FImgList[Index]));
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
  @param Bitmap   Bitmap sur lequel dessiner l'image
  @param X        Coordonnée X du point à partir duquel dessiner l'image
  @param Y        Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index: Integer; Bitmap: TBitmap32;
  X: Integer = 0; Y: Integer = 0);
begin
  Bitmap.Draw(X, Y, TBitmap32(FImgList[Index]));
end;

{*
  Dessine une image à partir de son nom
  Draw dessine l'image indiquée sur un canevas.
  @param ImgName   Nom de l'image à dessiner
  @param Bitmap    Bitmap sur lequel dessiner l'image
  @param X         Coordonnée X du point à partir duquel dessiner l'image
  @param Y         Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(const ImgName: string; Bitmap: TBitmap32;
  X: Integer = 0; Y: Integer = 0);
begin
  Draw(IndexOf(ImgName), Bitmap, X, Y);
end;

{*
  Obtient le bitmap interne d'une image d'après son index
  @param Index   Index d'une image
  @return Bitmap interne pour cette image
*}
function TImagesMaster.GetInternalBitmap(Index: Integer): TBitmap32;
begin
  Result := TBitmap32(FImgList[Index]);
end;

{*
  Obtient le bitmap interne d'une image d'après son nom
  @param ImgName   Nom d'une image
  @return Bitmap interne pour cette image
*}
function TImagesMaster.GetInternalBitmap(const ImgName: string): TBitmap32;
begin
  Result := GetInternalBitmap(IndexOf(ImgName));
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
  FDescription := TStringList.Create;
  FDefaultDescription := TStringList.Create;
  TStringList(FDescription).OnChange := DescriptionChange;

  FStaticDraw := True;
end;

{*
  [@inheritDoc]
*}
destructor TPainter.Destroy;
begin
  FCachedImg.Free;
  FDefaultDescription.Free;
  FDescription.Free;

  inherited;
end;

{*
  Effectue les mesures de ce peintre
  Après l'appel à Measure, les propriétés FStaticDraw, FNeedCache et FSize sont
  mises à jour.
*}
procedure TPainter.Measure;
var
  I: Integer;
  Bitmap: TBitmap32;
  SubRect: TRect;
begin
  FStaticDraw := True;
  FNeedCache := Description.Count > 1;
  FSize := Point(1, 1);

  for I := 0 to Description.Count-1 do
  begin
    // Parse description line
    ParseDescriptionLine(Description[I], Bitmap, SubRect);

    // Update FStaticDraw
    if Bitmap is TAnimatedBitmap32 then
      FStaticDraw := False;

    // Update FNeedCache
    if not SameRect(SubRect, Bitmap.BoundsRect) then
      FNeedCache := True;

    // Update FSize
    if not IsRectEmpty(SubRect) then
    begin
      FSize.X := LCM(FSize.X, SubRect.Right - SubRect.Left);
      FSize.Y := LCM(FSize.Y, SubRect.Bottom - SubRect.Top);
    end;
  end;
end;

{*
  Crée l'image cache si elle n'est pas encore créée
*}
procedure TPainter.NeedCachedImg;
begin
  if FCachedImg <> nil then
    Exit;

  if StaticDraw then
    FCachedImg := TBitmap32.Create
  else
    FCachedImg := TAnimatedBitmap32.Create;

  FCachedImg.DrawMode := dmBlend;
  FCachedImg.CombineMode := cmMerge;
end;

{*
  Dessine un bitmap cache statique
*}
procedure TPainter.DrawStatic;
var
  I: Integer;
  Bitmap: TBitmap32;
  SubRect: TRect;
begin
  FCachedImg.SetSize(Size.X, Size.Y);
  FCachedImg.Clear(clTransparent32);

  for I := 0 to FDescription.Count-1 do
  begin
    ParseDescriptionLine(Description[I], Bitmap, SubRect);
    DrawRepeat(FCachedImg, Bitmap, FCachedImg.BoundsRect, SubRect);
  end;
end;

{*
  Dessine un bitmap cache non statique
*}
procedure TPainter.DrawNotStatic;
var
  CachedImg: TAnimatedBitmap32;
  Count: Integer;
  Images: array of TBitmap32;
  SubRects: array of TRect;
  I, J, FrameCount, Transition, PreviousTrans: Integer;
  TotalTime, CummulatedDelay: Cardinal;
  Transitions: TScIntegerSet;
  Animated: TAnimatedBitmap32;
  Frame: TBitmap32Frame;
begin
  CachedImg := FCachedImg as TAnimatedBitmap32;

  // Fetch images
  Count := Description.Count;
  SetLength(Images, Count);
  SetLength(SubRects, Count);
  for I := 0 to Count-1 do
    ParseDescriptionLine(Description[I], Images[I], SubRects[I]);

  // Compute total time - least common multiple of all total times
  TotalTime := 1;
  for I := 0 to Count-1 do
    if Images[I] is TAnimatedBitmap32 then
      TotalTime := LCM(TotalTime, TAnimatedBitmap32(Images[I]).TotalTime);

  // Build set of transition points - use it to create frames
  Transitions := TScIntegerSet.Create;
  try
    // Build transition set

    for I := 0 to Count-1 do
    begin
      if not (Images[I] is TAnimatedBitmap32) then
        Continue;
      Animated := TAnimatedBitmap32(Images[I]);

      CummulatedDelay := 0;
      J := 0;
      while CummulatedDelay < TotalTime do
      begin
        Transitions.Include(CummulatedDelay);
        Inc(CummulatedDelay, Animated.Frames[J].Delay);
        J := (J+1) mod Animated.FrameCount;
      end;
    end;

    Transitions.Exclude(0);
    Transitions.Include(TotalTime);

    // Create frames and set delays

    FrameCount := 0;
    for Transition in Transitions do
    begin
      if Transition <> 0 then // avoid warning
        Inc(FrameCount);
    end;

    CachedImg.FrameCount := FrameCount;
    CachedImg.SetSize(Size.X, Size.Y);

    I := 0;
    PreviousTrans := 0;
    for Transition in Transitions do
    begin
      CachedImg.Frames[I].Delay := Transition - PreviousTrans;
      PreviousTrans := Transition;
      Inc(I);
    end;
  finally
    Transitions.Free;
  end;

  // Draw each frame
  CummulatedDelay := 0;
  for I := 0 to FrameCount-1 do
  begin
    Frame := CachedImg.Frames[I];
    Frame.Clear(clTransparent32);

    for J := 0 to Count-1 do
      DrawRepeat(Frame, Images[J], Frame.BoundsRect, SubRects[J],
        CummulatedDelay);

    Inc(CummulatedDelay, Frame.Delay);
  end;
end;

{*
  Événement OnChange de la description
  DescriptionChange est appelé lorsque la description du peintre change.
  Elle actualise l'image cache.
  @param Sender   Objet lançant l'événement
*}
procedure TPainter.DescriptionChange(Sender: TObject);
begin
  Measure;
  FreeAndNil(FCachedImg);

  if not FNeedCache then
  begin
    if Description.Count > 0 then
      FCachedImgIndex := Master.IndexOf(Description[0])
    else
      FCachedImgIndex := -1;
  end else
  begin
    NeedCachedImg;

    if StaticDraw then
      DrawStatic
    else
      DrawNotStatic;
  end;
end;

{*
  Indique si ce peintre est vide
  @return True si ce peintre est vide, False sinon
*}
function TPainter.GetIsEmpty: Boolean;
begin
  Result := Description.Count = 0;
end;

{*
  [@inheritDoc]
*}
procedure TPainter.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineStrings('Description', Description, nil,
    not FDescription.Equals(FDefaultDescription));
end;

{*
  [@inheritDoc]
*}
procedure TPainter.StoreDefaults;
begin
  inherited;

  FDefaultDescription.Assign(FDescription);
end;

{*
  Analyse une ligne de description et en récupère ses différentes composantes
  @param Description   Ligne de description à analyser
  @param Bitmap        En sortie : Image de base
  @param SubRect       En sortie : Rectangle de l'image à conserver
*}
procedure TPainter.ParseDescriptionLine(const Description: string;
  out Bitmap: TBitmap32; out SubRect: TRect);
var
  ImgName: string;
  HasSubRect: Boolean;
  RectStr, TopLeftStr, BottomRightStr: string;
  LeftStr, TopStr, RightStr, BottomStr: string;
begin
  HasSubRect := SplitToken(Description, '@', ImgName, RectStr);
  Bitmap := Master.GetInternalBitmap(ImgName);

  if HasSubRect then
  begin
    SplitToken(RectStr, ':', TopLeftStr, BottomRightStr);
    SplitToken(TopLeftStr, ',', LeftStr, TopStr);
    SplitToken(BottomRightStr, ',', RightStr, BottomStr);

    SubRect.Left := StrToIntDef(LeftStr, 0);
    SubRect.Top := StrToIntDef(TopStr, 0);
    SubRect.Right := StrToIntDef(RightStr, SubRect.Left + SquareSize);
    SubRect.Bottom := StrToIntDef(BottomStr, SubRect.Top + SquareSize);

    IntersectRect(SubRect, SubRect, Bitmap.BoundsRect);
  end else
  begin
    SubRect := Bitmap.BoundsRect;
  end;
end;

{*
  Efface le peintre
*}
procedure TPainter.Clear;
begin
  Description.Clear;
end;

{*
  Ajoute une image au peintre
  @param ImgName   Nom de l'image à ajouter
*}
procedure TPainter.AddImage(const ImgName: string);
begin
  Description.Add(ImgName);
end;

{*
  Ajoute une partie d'une image au peintre
  @param ImgName   Nom de l'image à ajouter
  @param SubRect   Rectangle de l'image à conserver
*}
procedure TPainter.AddImageRect(const ImgName: string; const SubRect: TRect);
begin
  if IsRectEmpty(SubRect) then
    Exit;

  Description.Add(Format('%s@%d,%d:%d,%d',
    [ImgName, SubRect.Left, SubRect.Top, SubRect.Right, SubRect.Bottom]));
end;

{*
  Assigne un peintre à celui-ci
  @param Source   Peintre source
*}
procedure TPainter.Assign(Source: TPainter);
begin
  Description.Assign(Source.Description);
end;

{*
  Démarre une modification du peintre
  Chaque appel à BeginUpdate doit être clôturé par un appel à EndUpdate.
*}
procedure TPainter.BeginUpdate;
begin
  Description.BeginUpdate;
end;

{*
  Termine une modification du peintre
  Appelez EndUpdate pour balancer chaque appel à BeginUpdate.
*}
procedure TPainter.EndUpdate;
begin
  Description.EndUpdate;
end;

{*
  Récupère le bitmap complet pour ce peintre
  La valeur renvoyée peut être nil, si le peintre est vide. Elle est valide
  uniquement tant que le peintre n'est pas modifié.
  Il est préférable d'utiliser les méthodes de dessin du peintre plutôt que de
  récupérer le bitmap complet lorsque c'est possible.
  @return Bitmap complet pour ce peintre (peut être nil)
*}
function TPainter.GetBitmap: TBitmap32;
begin
  if FNeedCache then
    Result := FCachedImg
  else if FCachedImgIndex >= 0 then
    Result := FMaster.GetInternalBitmap(FCachedImgIndex)
  else
    Result := nil;
end;

{*
  Dessine le peintre dans un contexte de case
  La méthode Draw dessine le peintre dans le contexte de dessin spécifié. Les
  différentes images sont superposées, celle d'index 0 tout au-dessous.
  @param Context   Contexte de dessin de la case
*}
procedure TPainter.Draw(Context: TDrawSquareContext);
begin
  if FNeedCache then
    Context.DrawSquareBitmap(FCachedImg)
  else if FCachedImgIndex >= 0 then
    Master.Draw(FCachedImgIndex, Context);
end;

{*
  Dessine le peintre sur un bitmap quelconque
  @param Dest   Bitmap destination
  @param X      Abscisse de destination
  @param Y      Ordonnée de destination
*}
procedure TPainter.DrawTo(Dest: TBitmap32; X: Integer = 0; Y: Integer = 0);
var
  Source: TBitmap32;
begin
  Source := GetBitmap;
  if Source <> nil then
    Source.DrawTo(Dest, X, Y);
end;

{*
  Dessine le peintre sur un bitmap quelconque à un temps donné
  @param TickCount   Tick-count
  @param Dest        Bitmap destination
  @param X           Abscisse de destination
  @param Y           Ordonnée de destination
*}
procedure TPainter.DrawAtTimeTo(TickCount: Cardinal; Dest: TBitmap32;
  X: Integer = 0; Y: Integer = 0);
var
  Source: TBitmap32;
begin
  Source := GetBitmap;
  if Source <> nil then
    DrawBitmapAtTimeTo(Source, TickCount, Dest, X, Y);
end;

{--------------------}
{ TMoveContext class }
{--------------------}

{*
  Crée un contexte de déplacement du joueur
  @param APlayer   Joueur qui se déplace
  @param ADest     Destination
  @param AKey      Touche pressée (ou 0 si pas de touche pressée)
  @param AShift    État des touches spéciales (si une touche a été pressée)
*}
constructor TMoveContext.Create(APlayer: TPlayer; const ADest: TQualifiedPos;
  AKey: Word = 0; AShift: TShiftState = []);
begin
  inherited Create;

  FPlayer := APlayer;

  FSrcQPos := Player.QPos;
  FDestQPos := ADest;

  SwitchToSrc;

  FOldDirection := Player.Direction;
  FKeyPressed := AKey <> 0;
  FKey := AKey;
  FShift := AShift;

  FCancelled := False;
  FGoOnMoving := False;

  FTemporization := Player.DefaultTemporization;
end;

{*
  Crée un contexte de déplacement du joueur restant sur la même carte
  @param APlayer   Joueur qui se déplace
  @param ADest     Destination
  @param AKey      Touche pressée (ou 0 si pas de touche pressée)
  @param AShift    État des touches spéciales (si une touche a été pressée)
*}
constructor TMoveContext.Create(APlayer: TPlayer; const ADest: T3DPoint;
  AKey: Word = 0; AShift: TShiftState = []);
var
  DestQPos: TQualifiedPos;
begin
  DestQPos.Map := APlayer.Map;
  DestQPos.Position := ADest;

  Create(APlayer, DestQPos, AKey, AShift);
end;

{*
  Passe la case courante à la source
*}
procedure TMoveContext.SwitchToSrc;
begin
  FQPos := FSrcQPos;
end;

{*
  Passe la case courante à la destination
*}
procedure TMoveContext.SwitchToDest;
begin
  FQPos := FDestQPos;
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

{*
  Temporise (endort le thread pendant Temporization millisecondes
*}
procedure TMoveContext.Temporize;
begin
  Sleep(Temporization);
end;

{--------------------------}
{ TFunLabyPersistent class }
{--------------------------}

{*
  Définit les propriétés à charger depuis ou enregistrer dans un filer
  @param Filer   Filer utilisé
*}
procedure TFunLabyPersistent.DefineProperties(Filer: TFunLabyFiler);
var
  PropList: PPropList;
  Count, I: Integer;
begin
  Count := GetPropList(Self, PropList);
  if Count = 0 then
    Exit;

  try
    for I := 0 to Count-1 do
      Filer.DefinePublishedProperty(PropList[I]);
  finally
    FreeMem(PropList);
  end;
end;

{*
  Enregistre l'état actuel des propriétés comme étant par défaut
*}
procedure TFunLabyPersistent.StoreDefaults;
var
  StoreDefaultsFiler: TFunLabyStoredDefaultsFiler;
begin
  StoreDefaultsFiler := TFunLabyStoredDefaultsFiler.Create(Self);
  try
    DefineProperties(StoreDefaultsFiler);
  finally
    StoreDefaultsFiler.Free;
  end;
end;

{*
  Début d'un état
  @param State   État qui commence
*}
procedure TFunLabyPersistent.BeginState(State: TPersistentState);
begin
  FPersistentState := FPersistentState + State;
end;

{*
  Fin d'un état
  @param State   État qui se termine
*}
procedure TFunLabyPersistent.EndState(State: TPersistentState);
begin
  FPersistentState := FPersistentState - State;
end;

{*
  [@inheritDoc]
*}
class function TFunLabyPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TFunLabyPersistent(Result).FPersistentState := [psCreating];
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPersistent.AfterConstruction;
begin
  inherited;
  EndState([psCreating]);
  StoreDefaults;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyPersistent.BeforeDestruction;
begin
  inherited;
  BeginState([psDestroying]);
end;

{------------------------------------}
{ TInterfacedFunLabyPersistent class }
{------------------------------------}

{ This implementation comes from System.TInterfacedObject. }

{*
  [@inheritDoc]
*}
function TInterfacedFunLabyPersistent.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{*
  [@inheritDoc]
*}
function TInterfacedFunLabyPersistent._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

{*
  [@inheritDoc]
*}
function TInterfacedFunLabyPersistent._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedFunLabyPersistent.AfterConstruction;
begin
  // Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

{*
  [@inheritDoc]
*}
procedure TInterfacedFunLabyPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

{*
  [@inheritDoc]
*}
class function TInterfacedFunLabyPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedFunLabyPersistent(Result).FRefCount := 1;
end;

{--------------------------}
{ TFunLabyCollection class }
{--------------------------}

{*
  Crée une collection
*}
constructor TFunLabyCollection.Create;
begin
  inherited Create;

  FItems := TObjectList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TFunLabyCollection.Destroy;
begin
  FItems.Free;

  inherited;
end;

{*
  Nombre d'éléments de la collection
  @return Nombre d'éléments de la collection
*}
function TFunLabyCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{*
  Tableau zero-based des éléments de la collection
  @param Index   Index d'un élément
  @return Élément à l'index indiqué
*}
function TFunLabyCollection.GetItems(Index: Integer): TFunLabyPersistent;
begin
  Result := TFunLabyPersistent(FItems[Index]);
end;

{*
  Notification qu'un élément a été ajouté ou supprimé
  @param Item     Élément concerné
  @param Action   Action faite sur l'élément
*}
procedure TFunLabyCollection.Notify(Item: TFunLabyPersistent;
  Action: TListNotification);
begin
end;

{*
  Obtient la classe d'élément par défaut
  @return Classe d'élément par défaut
*}
function TFunLabyCollection.GetDefaultItemClass: TFunLabyPersistentClass;
begin
  Result := nil;
end;

{*
  Ajoute un élément déjà créé à la collection
  @param Item   Élément à ajouter
  @return Index du nouvel élément
*}
function TFunLabyCollection.AddItem(Item: TFunLabyPersistent): Integer;
begin
  Result := InsertItem(FItems.Count, Item);
end;

{*
  Insère un élément déjà créé à la collection
  @param Index   Index où insérer l'élémnet
  @param Item    Élément à insérer
  @return Index du nouvel élément
*}
function TFunLabyCollection.InsertItem(Index: Integer;
  Item: TFunLabyPersistent): Integer;
begin
  FItems.Insert(Index, Item);
  Notify(Item, lnAdded);
  Result := Index;
end;

{*
  Extrait un élément sans le libérer
  @param Index   Index de l'élément à extraire
  @return Élément extrait
*}
function TFunLabyCollection.ExtractItem(Index: Integer): TFunLabyPersistent;
begin
  Notify(Items[Index], lnExtracted);

  Result := TFunLabyPersistent(FItems.Extract(Items[Index]));
end;

{*
  Extrait un élément sans le libérer
  @param Item   Élément à extraire
  @return Élément extrait
*}
function TFunLabyCollection.ExtractItem(
  Item: TFunLabyPersistent): TFunLabyPersistent;
begin
  Notify(Item, lnExtracted);

  Result := TFunLabyPersistent(FItems.Extract(Item));
end;

{*
  Efface tous les éléments de la collection
*}
procedure TFunLabyCollection.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

{*
  Ajoute un élément à la collection
  @param ItemClass   Classe d'élément à ajouter
  @return Élément ajouté
*}
function TFunLabyCollection.Add(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := Insert(Count, ItemClass);
end;

{*
  Ajoute un élément de la classe par défaut à la collection
  @return Élément ajouté
  @throws EAbstractError Cette collection n'a pas de classe par défaut
*}
function TFunLabyCollection.AddDefault: TFunLabyPersistent;
var
  ItemClass: TFunLabyPersistentClass;
begin
  ItemClass := GetDefaultItemClass;

  if ItemClass = nil then
    AbstractError;

  Result := Add(ItemClass);
end;

{*
  Insert un élément dans la collection
  @param Index       Index où insérer l'élément
  @param ItemClass   Classe d'élément à ajouter
  @return Élément ajouté
*}
function TFunLabyCollection.Insert(Index: Integer;
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := CreateItem(ItemClass);
  try
    InsertItem(Index, Result);
  except
    Result.Free;
    raise;
  end;
end;

{*
  Supprime un élément de la collection
  @param Index   Index de l'élément à supprimer
*}
procedure TFunLabyCollection.Delete(Index: Integer);
begin
  Notify(Items[Index], lnDeleted);

  FItems.Delete(Index);
end;

{*
  Supprime un élément de la collection
  @param Item   Élément à supprimer
  @return Index de l'élément supprimé
*}
function TFunLabyCollection.Remove(Item: TFunLabyPersistent): Integer;
begin
  if IndexOf(Item) < 0 then
    Result := -1
  else
  begin
    Notify(Item, lnDeleted);

    Result := FItems.Remove(Item);
  end;
end;

{*
  Échange deux éléments de la collection
  @param Index1   Premier index
  @param Index2   Second index
*}
procedure TFunLabyCollection.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

{*
  Déplace un élément de la collection
  @param CurIndex   Index d'un élément
  @param NewIndex   Nouvel index de cet élément
*}
procedure TFunLabyCollection.Move(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

{*
  Cherche un élément dans la collection
  @param Item   Élément recherché
  @return Index de cet élément dans la collection, ou -1 si non trouvé
*}
function TFunLabyCollection.IndexOf(Item: TFunLabyPersistent): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

{*
  Teste si cette collection a une classe d'éléments par défaut
  @return True si elle a une classe d'éléments par défaut, False sinon
*}
function TFunLabyCollection.HasDefault: Boolean;
begin
  Result := GetDefaultItemClass <> nil;
end;

{---------------------}
{ TFunLabyFiler class }
{---------------------}

{*
  Crée un filer
  @param AInstance   Instance à traiter
  @param AOwner      Filer propriétaire
*}
constructor TFunLabyFiler.Create(AInstance: TFunLabyPersistent;
  AOwner: TFunLabyFiler = nil);
begin
  inherited Create;

  FOwner := AOwner;
  FInstance := AInstance;

  if Instance is TMaster then
    FMaster := TMaster(Instance)
  else if Instance is TFunLabyComponent then
    FMaster := TFunLabyComponent(Instance).Master
  else if Owner <> nil then
    FMaster := Owner.Master;
end;

{*
  Énumère les propriétés
*}
procedure TFunLabyFiler.EnumProperties;
begin
  Instance.DefineProperties(Self);
end;

{*
  Débute un état de l'instance
  @param State   État à commencer
*}
procedure TFunLabyFiler.InstanceBeginState(State: TPersistentState);
begin
  Instance.BeginState(State);
end;

{*
  Termine un état de l'instance
  @param State   État à terminer
*}
procedure TFunLabyFiler.InstanceEndState(State: TPersistentState);
begin
  Instance.EndState(State);
end;

{*
  Teste si un composant a des données liées à un joueur
  @param Component   Composant
  @param Player      Joueur
  @return True si le composant a des données liées au joueur spécifié
*}
function TFunLabyFiler.HasPlayerData(Component: TFunLabyComponent;
  Player: TPlayer): Boolean;
begin
  Result := Component.HasPlayerData(Player);
end;

{*
  Obtient les données liées à un joueur pour un composant
  @param Component   Composant
  @param Player      Joueur
  @return Données liées au joueur spécifié pour le composant spécifié
*}
function TFunLabyFiler.GetPlayerData(Component: TFunLabyComponent;
  Player: TPlayer): TPlayerData;
begin
  Result := Component.GetPlayerData(Player);
end;

{*
  Définit une propriété publiée
  @param PropInfo   PropInfo de la propriété
*}
procedure TFunLabyFiler.DefinePublishedProperty(PropInfo: PPropInfo);
var
  HasData: Boolean;
  PropClass: TClass;
  SubInstance: TObject;
begin
  if not Assigned(PropInfo.GetProc) then
    Exit;

  if PropInfo.PropType^.Kind = tkClass then
  begin
    PropClass := GetTypeData(PropInfo.PropType^).ClassType;
    SubInstance := TObject(GetOrdProp(Instance, PropInfo));

    if not PropClass.InheritsFrom(TFunLabyComponent) then
    begin
      if SubInstance is TFunLabyPersistent then
      begin
        DefinePersistent(TypeInfoDecode(PropInfo.Name),
          TFunLabyPersistent(SubInstance));
      end else if SubInstance is TStrings then
      begin
        DefineStrings(TypeInfoDecode(PropInfo.Name), TStrings(SubInstance), nil,
          IsStoredProp(Instance, PropInfo));
      end;

      Exit;
    end;
  end;

  if not Assigned(PropInfo.SetProc) then
    Exit;

  HasData := IsStoredProp(Instance, PropInfo) and
    (not IsPropDefaultValue(Instance, PropInfo));

  HandleProperty(PropInfo, HasData);
end;

{*
  Définit une propriété lue et écrite avec des méthodes
  @param Name       Nom de la propriété
  @param PropType   Type de la propriété
  @param GetProc    Méthode de lecture
  @param SetProc    Méthode d'écriture
  @param HasData    Indique s'il y a des données à écrire
*}
procedure TFunLabyFiler.DefineProcProperty(const Name: string;
  PropType: PTypeInfo; GetProc, SetProc: Pointer; HasData: Boolean = True);
var
  PropInfo: TPropInfo;
begin
  PropInfo.PropType := @PropType;
  PropInfo.GetProc := GetProc;
  PropInfo.SetProc := SetProc;
  PropInfo.StoredProc := Pointer($FFFFFF01);
  PropInfo.Index := Integer($80000000);
  PropInfo.Default := Integer($80000000);
  PropInfo.NameIndex := 0;
  PropInfo.Name := TypeInfoEncode(Name);

  HandleProperty(@PropInfo, HasData);
end;

{*
  Définit une propriété lue avec un champ et écrite avec une méthode
  @param Name       Nom de la propriété
  @param PropType   Type de la propriété
  @param GetField   Champ de lecture
  @param SetProc    Méthode d'écriture
  @param HasData    Indique s'il y a des données à écrire
*}
procedure TFunLabyFiler.DefineFieldProcProperty(const Name: string;
  PropType: PTypeInfo; GetField, SetProc: Pointer; HasData: Boolean = True);
var
  FieldOffset: Integer;
  FieldProc: Pointer;
begin
  FieldOffset := Integer(GetField) - Integer(Instance);
  FieldProc := Pointer($FF000000 or FieldOffset);
  DefineProcProperty(Name, PropType, FieldProc, SetProc, HasData);
end;

{*
  Définit une propriété lue et écrite avec un champ
  @param Name          Nom de la propriété
  @param PropType      Type de la propriété
  @param GetSetField   Champ de lecture et écriture
  @param HasData       Indique s'il y a des données à écrire
*}
procedure TFunLabyFiler.DefineFieldProperty(const Name: string;
  PropType: PTypeInfo; GetSetField: Pointer; HasData: Boolean = True);
var
  FieldOffset: Integer;
  FieldProc: Pointer;
begin
  FieldOffset := Integer(GetSetField) - Integer(Instance);
  FieldProc := Pointer($FF000000 or FieldOffset);
  DefineProcProperty(Name, PropType, FieldProc, FieldProc, HasData);
end;

{*
  Définit un sous-objet persistent
  @param Name          Nom de l'objet
  @param SubInstance   Sous-objet à lire/écrire
*}
procedure TFunLabyFiler.DefinePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
begin
  if SubInstance is TFunLabyCollection then
    HandleCollection(Name, TFunLabyCollection(SubInstance))
  else if SubInstance is TFunLabyComponent then
    HandleComponent(Name, TFunLabyComponent(SubInstance))
  else
    HandlePersistent(Name, SubInstance);
end;

{*
  Définit une propriété de type TStrings
  @param Name         Nom de la propriété
  @param Strings      Liste de chaînes
  @param ObjectType   Type d'objet stocké (peut être nil)
  @param HasData      Indique si la liste de chaîne contient des données
*}
procedure TFunLabyFiler.DefineStrings(const Name: string; Strings: TStrings;
  ObjectType: PTypeInfo; HasData: Boolean);
begin
  Assert((ObjectType = nil) or
    (ObjectType.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkClass]));

  HandleStrings(Name, Strings, ObjectType, HasData);
end;

{*
  Définit une propriété de type TStrings qui a des données si non vide
  @param Name         Nom de la propriété
  @param Strings      Liste de chaînes
  @param ObjectType   Type d'objet stocké (peut être nil)
*}
procedure TFunLabyFiler.DefineStrings(const Name: string; Strings: TStrings;
  ObjectType: PTypeInfo = nil);
begin
  DefineStrings(Name, Strings, ObjectType, Strings.Count > 0);
end;

{*
  Définit une propriété binaire
  @param Name        Nom de la propriété
  @param ReadProc    Méthode de call-back à utiliser pour lire les données
  @param WriteProc   Méthode de call-back à utiliser pour écrire les données
  @param HasData     Indique s'il y a des données à écrire
*}
procedure TFunLabyFiler.DefineBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean = True);
begin
  HandleBinaryProperty(Name, ReadProc, WriteProc, HasData);
end;

{----------------------}
{ TFunLabyReader class }
{----------------------}

{*
  Crée un objet lecteur FunLabyrinthe
  @param Instance   Instance à lire
  @param AOwner     Lecteur propriétaire (peut être nil)
*}
constructor TFunLabyReader.Create(AInstance: TFunLabyPersistent;
  AOwner: TFunLabyReader);
begin
  inherited Create(AInstance, AOwner);

  Instance.BeginState([psReading]);
end;

{*
  [@inheritDoc]
*}
destructor TFunLabyReader.Destroy;
begin
  Instance.EndState([psReading]);

  inherited;
end;

{----------------------}
{ TFunLabyWriter class }
{----------------------}

{*
  Crée un objet écrivain FunLabyrinthe
  @param Instance   Instance à écrire
  @param AOwner     Écrivain propriétaire (peut être nil)
*}
constructor TFunLabyWriter.Create(AInstance: TFunLabyPersistent;
  AOwner: TFunLabyWriter);
begin
  inherited Create(AInstance, AOwner);

  Instance.BeginState([psWriting]);
end;

{*
  [@inheritDoc]
*}
destructor TFunLabyWriter.Destroy;
begin
  Instance.EndState([psWriting]);

  inherited;
end;

{-----------------------------------}
{ TFunLabyStoredDefaultsFiler class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
procedure TFunLabyStoredDefaultsFiler.HandleProperty(PropInfo: PPropInfo;
  HasData: Boolean);
begin
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyStoredDefaultsFiler.HandlePersistent(const Name: string;
  SubInstance: TFunLabyPersistent);
begin
  SubInstance.StoreDefaults;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyStoredDefaultsFiler.HandleCollection(const Name: string;
  Collection: TFunLabyCollection);
begin
  Collection.StoreDefaults;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyStoredDefaultsFiler.HandleComponent(const Name: string;
  Component: TFunLabyComponent);
begin
  Component.StoreDefaults;
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyStoredDefaultsFiler.HandleStrings(const Name: string;
  Strings: TStrings; ObjectType: PTypeInfo; HasData: Boolean);
begin
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyStoredDefaultsFiler.HandleBinaryProperty(const Name: string;
  ReadProc, WriteProc: TStreamProc; HasData: Boolean);
begin
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

  FIconPainter := TPainter.Create(Master.ImagesMaster);
  FIconPainter.BeginUpdate;
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
  Modifie l'ID du composant
  @param Value   Nouvel ID du composant
*}
procedure TFunLabyComponent.SetID(const Value: TComponentID);
begin
  if Value = FID then
    Exit;

  if not Master.Editing then
    raise EInvalidCommand.Create(SEditingRequiredForSetID);

  if not IsAdditionnal then
    raise EInvalidCommand.Create(SAdditionnalRequiredForSetID);

  Master.CheckComponentID(Value);

  ChangeID(Value);
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
  Change l'ID de ce composant
  N'appelez pas directement ChangeID, mais modifiez plutôt la propriété ID du
  composant.
  Surchargez cette méthode si vous devez effectuer un traitement spécial lorsque
  l'ID du composant change.
  @param NewID   Nouvel ID
*}
procedure TFunLabyComponent.ChangeID(const NewID: TComponentID);
begin
  Assert(Master.Editing);
  FID := NewID;
  Master.ComponentIDChanged(Self);
end;

{*
  Teste si ce composant a des données pour un joueur donné
  @param Player   Joueur à tester
  @return True si ce composant à des données pour le joueur Player
*}
function TFunLabyComponent.HasPlayerData(Player: TPlayer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FPlayerData)-1 do
  begin
    if FPlayerData[I].Item = Player then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
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

{*
  Catégorie de ce composant
  La catégorie est utilisée en mode édition pour savoir comment organiser les
  différent composant dans la palette des composants. Si la catégorie est une
  chaîne vide, ce composant n'est pas disponible dans la palette.
  @return Catégorie de ce composant
*}
function TFunLabyComponent.GetCategory: string;
begin
  Result := '';
end;

{*
  Description de ce composant pour une bulle d'aide (hint)
  @return Description de ce composant pour une bulle d'aide
*}
function TFunLabyComponent.GetHint: string;
begin
  Result := ID;
end;

{*
  Indique si ce composant peut être placé dans une palette des composants
  @return True s'il peut être placé dans une palette des composants, False sinon
*}
function TFunLabyComponent.GetIsDesignable: Boolean;
begin
  Result := (ID <> '') and (Category <> '');
end;

{*
  [@inheritDoc]
*}
procedure TFunLabyComponent.AfterConstruction;
begin
  inherited;

  FIconPainter.EndUpdate;
end;

{*
  Indique si cette classe utilise des données liées au joueur
  @return True si cette classe utilise des données liées au joueur, False sinon
*}
class function TFunLabyComponent.UsePlayerData: Boolean;
begin
  Result := GetPlayerDataClass <> TPlayerData;
end;

{*
  Dessine l'icône de ce composant
  @param Bitmap   Bitmap destination
  @param X        Abscisse
  @param Y        Ordonnée
*}
procedure TFunLabyComponent.DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
  Y: Integer = 0);
var
  Context: TDrawSquareContext;
begin
  if IconPainter.IsEmpty then
    Exit;

  Context := TDrawSquareContext.Create(Bitmap, X, Y, NoQPos);
  try
    IconPainter.Draw(Context);
  finally
    Context.Free;
  end;
end;

{*
  Dessine l'icône de ce composant sur un canevas VCL
  @param Canvas            Canevas cible
  @param DestRect          Rectangle dans lequel dessiner le bitmap
  @param BackgroundColor   Couleur de fond sur le canevas
*}
procedure TFunLabyComponent.DrawIconToCanvas(Canvas: TCanvas;
  const DestRect: TRect; BackgroundColor: TColor);
var
  TempBitmap: TBitmap32;
begin
  TempBitmap := TBitmap32.Create;
  try
    TempBitmap.SetSize(SquareSize, SquareSize);
    TempBitmap.Clear(Color32(BackgroundColor));
    DrawIcon(TempBitmap);
    Canvas.CopyRect(DestRect, TempBitmap.Canvas, TempBitmap.BoundsRect);
  finally
    TempBitmap.Free;
  end;
end;

{-------------------------}
{ Classe TVisualComponent }
{-------------------------}

{*
  [@inheritDoc]
*}
constructor TVisualComponent.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FName := ID;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.BeginUpdate;
end;

{*
  [@inheritDoc]
*}
destructor TVisualComponent.Destroy;
begin
  FPainter.Free;

  inherited;
end;

{*
  Teste si la propriété Name doit être enregistrée
  @return True si la propriété Name doit être enregistrée, False sinon
*}
function TVisualComponent.IsNameStored: Boolean;
begin
  Result := FName <> FDefaultName;
end;

{*
  Teste si la propriété EditVisualTag doit être enregistrée
  @return True si la propriété EditVisualTag doit être enregistrée, False sinon
*}
function TVisualComponent.IsEditVisualTagStored: Boolean;
begin
  Result := FEditVisualTag <> FDefaultEditVisualTag;
end;

{*
  Dessine le composant dans le contexte de dessin spécifié
  PrivDraw dessine le composant dans le contexte de dessin spécifié.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.PrivDraw(Context: TDrawSquareContext);
begin
  DoDraw(Context);

  if Master.Editing and (EditVisualTag <> '') then
    DoDrawEditVisualTag(Context);
end;

{*
  [@inheritDoc]
*}
procedure TVisualComponent.StoreDefaults;
begin
  inherited;

  FDefaultName := FName;
  FDefaultEditVisualTag := FEditVisualTag;
end;

{*
  [@inheritDoc]
*}
function TVisualComponent.GetHint: string;
begin
  Result := Name;
end;

{*
  Donne une valuer à EditVisualTag basée sur l'ID de ce composant
*}
procedure TVisualComponent.AutoEditVisualTag;
var
  I: Integer;
begin
  I := Length(ID);
  while (I > 0) and CharInSet(ID[I], ['0'..'9']) do
    Dec(I);

  EditVisualTag := Copy(ID, I+1, MaxInt);
end;

{*
  Dessine le composant
  DoDraw dessine le composant dans le contexte de dessin spécifié.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.DoDraw(Context: TDrawSquareContext);
begin
  FPainter.Draw(Context);
end;

{*
  Dessine le tag visuel d'édition
  DoDraw dessine le tag visuel d'édition dans le contexte de dessin spécifié.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.DoDrawEditVisualTag(Context: TDrawSquareContext);
const {don't localize}
  FontName = 'Arial';
  FontSize = 9;
  FontColor = clBlack32;
  BackColor = clWhite32;
var
  Extent: TSize;
  TextX, TextY: Integer;
begin
  with Context do
  begin
    Bitmap.Font.Name := FontName;
    Bitmap.Font.Size := FontSize;

    Extent := Bitmap.TextExtent(EditVisualTag);
    TextX := X + (SquareSize - Extent.cx) div 2;
    TextY := Y + (SquareSize - Extent.cy) div 2;

    Bitmap.FillRectTS(TextX-1, TextY+1, TextX+Extent.cx+1, TextY+Extent.cy-1,
      BackColor);

    Bitmap.RenderText(TextX, TextY, EditVisualTag, 0, FontColor);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TVisualComponent.AfterConstruction;
begin
  inherited;

  FPainter.EndUpdate;
end;

{*
  Dessine le composant
  Draw dessine le composant dans le contexte de dessin spécifié.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.Draw(Context: TDrawSquareContext);
begin
  PrivDraw(Context);
end;

{*
  Dessine le composant
  Draw dessine le composant dans le contexte de dessin spécifié.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Bitmap   Bitmap sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TVisualComponent.Draw(const QPos: TQualifiedPos; Bitmap: TBitmap32;
  X: Integer = 0; Y: Integer = 0);
var
  Context: TDrawSquareContext;
begin
  Context := TDrawSquareContext.Create(Bitmap, X, Y, QPos);
  try
    Draw(Context);
  finally
    Context.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TVisualComponent.DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
  Y: Integer = 0);
begin
  Draw(NoQPos, Bitmap, X, Y);
end;

{-------------------------}
{ TComponentCreator class }
{-------------------------}

{*
  [@inheritDoc]
*}
function TComponentCreator.GetCategory: string;
begin
  Result := SCategoryComponentCreators;
end;

{*
  [@inheritDoc]
*}
function TComponentCreator.GetHint: string;
begin
  Result := Format(SComponentCreatorHint, [ComponentClass.ClassName]);
end;

{*
  Crée un nouveau composant
  @param AID   ID du composant à créer
  @return Composant créé
*}
function TComponentCreator.CreateComponent(
  const AID: TComponentID): TFunLabyComponent;
begin
  Result := Master.CreateAdditionnalComponent(ComponentClass, AID);
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
const
  DefaultPluginIcon = 'Miscellaneous/Plugin'; {don't localize}
begin
  inherited;

  FPainterBefore := TPainter.Create(FMaster.ImagesMaster);
  FPainterBefore.Description.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.Description.BeginUpdate;

  IconPainter.AddImage(DefaultPluginIcon);
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
  [@inheritDoc]
*}
function TPlugin.GetCategory: string;
begin
  Result := SCategoryPlugins;
end;

{*
  [@inheritDoc]
*}
function TPlugin.GetIsDesignable: Boolean;
begin
  Result := (inherited GetIsDesignable) and
    (GetPropList(ClassInfo, tkProperties, nil, False) >
    GetPropList(TypeInfo(TPlugin), tkProperties, nil, False));
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TPlugin.AfterConstruction;
begin
  inherited;

  FPainterBefore.Description.EndUpdate;
  FPainterAfter.Description.EndUpdate;
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
  @param Param    Paramètre de l'action
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TPlugin.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := False;
end;

{-------------------}
{ Classe TObjectDef }
{-------------------}

{*
  [@inheritDoc]
*}
constructor TObjectDef.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FDisplayInObjectList := True;
  FDisplayInStatusBar := True;
end;

{*
  [@inheritDoc]
*}
class function TObjectDef.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TObjectDefPlayerData;
end;

{*
  [@inheritDoc]
*}
function TObjectDef.GetCategory: string;
begin
  Result := SCategoryObjects;
end;

{*
  Nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @return Nombre d'objets que ce joueur possède
*}
function TObjectDef.GetCount(Player: TPlayer): Integer;
begin
  Result := TObjectDefPlayerData(GetPlayerData(Player)).Count;
end;

{*
  Modifie le nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @param Value    Nouveau nombre d'objets
*}
procedure TObjectDef.SetCount(Player: TPlayer; Value: Integer);
var
  Found: Boolean;
begin
  with TObjectDefPlayerData(GetPlayerData(Player)) do
  begin
    Found := Value > Count;
    Count := Value;
  end;

  if Found then
    Player.FoundObject(Self);
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
  @param Param    Paramètre de l'action
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TObjectDef.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
  @param Param    Paramètre de l'action
*}
procedure TObjectDef.UseFor(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer);
begin
end;

{-------------------------}
{ Classe TSquareComponent }
{-------------------------}

{*
  [@inheritDoc]
*}
procedure TSquareComponent.ChangeID(const NewID: TComponentID);
var
  I: Integer;
begin
  inherited;

  if not (Self is TSquare) then
    for I := 0 to Master.SquareCount-1 do
      Master.Squares[I].UpdateID;
end;

{---------------}
{ Classe TField }
{---------------}

{*
  [@inheritDoc]
*}
function TField.GetCategory: string;
begin
  Result := SCategoryFields;
end;

{*
  Dessine le plafond de ce terrain
  @param Context   Contexte de dessin d'une case
*}
procedure TField.DoDrawCeiling(Context: TDrawSquareContext);
begin
end;

{*
  Dessine le plafond de ce terrain
  @param Context   Contexte de dessin d'une case
*}
procedure TField.DrawCeiling(Context: TDrawSquareContext);
begin
  DoDrawCeiling(Context);
end;

{*
  [@inheritDoc]
*}
procedure TField.DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
  Y: Integer = 0);
var
  Context: TDrawSquareContext;
begin
  Context := TDrawSquareContext.Create(Bitmap, X, Y);
  try
    Draw(Context);
    DrawCeiling(Context);
  finally
    Context.Free;
  end;
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
  [@inheritDoc]
*}
constructor TEffect.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FEnabled := True;
end;

{*
  [@inheritDoc]
*}
procedure TEffect.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  if not IsPublishedProp(Self, 'Enabled') then
    Filer.DefineFieldProperty('Enabled', TypeInfo(Boolean),
      @FEnabled, not Enabled);
end;

{*
  [@inheritDoc]
*}
function TEffect.GetCategory: string;
begin
  Result := SCategoryEffects;
end;

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
  Execute ne devrait être appelé que lorsque Enabled vaut True.
  @param Context   Contexte du déplacement
*}
procedure TEffect.Execute(Context: TMoveContext);
begin
end;

{--------------}
{ Classe TTool }
{--------------}

{*
  [@inheritDoc]
*}
function TTool.GetCategory: string;
begin
  Result := SCategoryTools;
end;

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
  [@inheritDoc]
*}
function TObstacle.GetCategory: string;
begin
  Result := SCategoryObstacles;
end;

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
  [@inheritDoc]
*}
constructor TSquare.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FTransient := True;
end;

{*
  Crée une instance de TSquare et la configure
  @param AMaster     Maître FunLabyrinthe
  @param AID         ID de la case
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TSquare.CreateConfig(AMaster: TMaster; const AID: TComponentID;
  AField: TField; AEffect: TEffect = nil; ATool: TTool = nil;
  AObstacle: TObstacle = nil);
begin
  Create(AMaster, AID);

  Configure(AField, AEffect, ATool, AObstacle);
end;

{*
  Détourne un événement vers un composant à position
  @param Context         Contexte du mouvement
  @param EventKind       Type d'événement
  @param HookComponent   Composant qui prend en charge le détournement
*}
procedure TSquare.HookEventTo(Context: TMoveContext;
  EventKind: TSquareEventKind; HookComponent: TPosComponent);
var
  Msg: TSquareEventMessage;
begin
  Msg.MsgID := msgSquareEvent;
  Msg.Kind := EventKind;
  Msg.Context := Context;

  HookComponent.Dispatch(Msg);
end;

{*
  Détourne un événement vers un composant à position qui le demanderait
  @param Context     Contexte du mouvement
  @param EventKind   Type d'événement
  @return True si l'événement a été détourné, False sinon
*}
function TSquare.HookEvent(Context: TMoveContext;
  EventKind: TSquareEventKind): Boolean;
var
  I: Integer;
  PosComponent: TPosComponent;
begin
  for I := Master.PosComponentCount-1 downto 0 do
  begin
    PosComponent := Master.OrderedPosComponents[I];

    if (EventKind in PosComponent.WantedSquareEvents) and
      (PosComponent.Map = Context.Map) and
      Same3DPoint(PosComponent.Position, Context.Pos) then
    begin
      Context.Hooked := True;
      HookEventTo(Context, EventKind, PosComponent);

      if Context.Hooked then
      begin
        Context.Hooked := False;
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
end;

{*
  Met à jour l'ID de cette case
*}
procedure TSquare.UpdateID;
var
  NewID: TComponentID;
begin
  Assert(Master.Editing);

  if ID = '' then
    Exit;

  NewID := Format(SquareIDFormat,
    [Field.SafeID, Effect.SafeID, Tool.SafeID, Obstacle.SafeID]);

  if NewID <> ID then
    ChangeID(NewID);
end;

{*
  Nombre de composants
  @return Nombre de composants
*}
function TSquare.GetComponentCount: Integer;
begin
  Result := 4;
end;

{*
  Tableau zero-based des composants
  @param Index   Index compris entre 0 inclus et ComponentCount exclus
  @return Composant à l'index spécifié
*}
function TSquare.GetComponents(Index: Integer): TSquareComponent;
begin
  case Index of
    0: Result := FField;
    1: Result := FEffect;
    2: Result := FTool;
    3: Result := FObstacle;
  else
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  end;
end;

{*
  Tableau zero-based des classes requises des composants
  @param Index   Index compris entre 0 inclus et ComponentCount exclus
  @return Classe requise du composant à l'index spécifié
*}
function TSquare.GetComponentClasses(Index: Integer): TSquareComponentClass;
begin
  case Index of
    0: Result := TField;
    1: Result := TEffect;
    2: Result := TTool;
    3: Result := TObstacle;
  else
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  end;
end;

{*
  [@inheritDoc]
*}
function TSquare.GetCategory: string;
begin
  Result := FCategory;
end;

{*
  Modifie la propriété Category
  @param Value   Nouvelle valeur de Category
*}
procedure TSquare.SetCategory(const Value: string);
begin
  if ClassType = TSquare then
    FCategory := Value;
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
  Dessine le plafond de la case
  @param Context   Contexte de dessin d'une case
*}
procedure TSquare.DoDrawCeiling(Context: TDrawSquareContext);
begin
  if Assigned(Field) then
    Field.DrawCeiling(Context);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TSquare.DoEntering(Context: TMoveContext);
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
procedure TSquare.DoExiting(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Exiting(Context);
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Context   Contexte du déplacement
*}
procedure TSquare.DoEntered(Context: TMoveContext);
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
procedure TSquare.DoExited(Context: TMoveContext);
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
procedure TSquare.DoExecute(Context: TMoveContext);
begin
  if Assigned(Tool) then
    Tool.Find(Context);
  if Assigned(Effect) and Effect.Enabled then
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
procedure TSquare.DoPushing(Context: TMoveContext);
begin
  if Assigned(Obstacle) then
    Obstacle.Pushing(Context);
end;

{*
  Configure la case
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
procedure TSquare.Configure(AField: TField; AEffect: TEffect = nil;
  ATool: TTool = nil; AObstacle: TObstacle = nil);
begin
  FField := AField;
  FEffect := AEffect;
  FTool := ATool;
  FObstacle := AObstacle;
end;

{*
  Dispatche un message à cette case à un endroit particulier
  @param Msg    Message à dispatcher
  @param QPos   Position qualifiée où dispatcher le message
*}
procedure TSquare.DispatchAt(var Msg; const QPos: TQualifiedPos);
var
  SavedMsgID: Word;
  I: Integer;
  PosComponent: TPosComponent;
begin
  SavedMsgID := TDispatchMessage(Msg).MsgID;

  for I := Master.PosComponentCount-1 downto 0 do
  begin
    PosComponent := Master.PosComponents[I];

    if PosComponent.WantMessages and SameQPos(QPos, PosComponent.QPos) then
    begin
      PosComponent.Dispatch(Msg);

      if TDispatchMessage(Msg).MsgID = 0 then
      begin
        TDispatchMessage(Msg).MsgID := SavedMsgID;
        Exit;
      end;
    end;
  end;

  Dispatch(Msg);
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
  Dessine le plafond de la case
  @param Context   Contexte de dessin d'une case
*}
procedure TSquare.DrawCeiling(Context: TDrawSquareContext);
begin
  DoDrawCeiling(Context);
end;

{*
  [@inheritDoc]
*}
procedure TSquare.DrawIcon(Bitmap: TBitmap32; X: Integer = 0;
  Y: Integer = 0);
var
  Context: TDrawSquareContext;
begin
  Context := TDrawSquareContext.Create(Bitmap, X, Y);
  try
    Draw(Context);
    DrawCeiling(Context);
  finally
    Context.Free;
  end;
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TSquare.Entering(Context: TMoveContext);
begin
  if not HookEvent(Context, sekEntering) then
    DoEntering(Context);
end;

{*
  Exécuté lorsque le joueur tente de sortir de la case
  Exiting est exécuté lorsque le joueur tente de sortir de la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Context   Contexte du déplacement
*}
procedure TSquare.Exiting(Context: TMoveContext);
begin
  if not HookEvent(Context, sekExiting) then
    DoExiting(Context);
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  @param Context   Contexte du déplacement
*}
procedure TSquare.Entered(Context: TMoveContext);
begin
  if not HookEvent(Context, sekEntered) then
    DoEntered(Context);
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  @param Context   Contexte du déplacement
*}
procedure TSquare.Exited(Context: TMoveContext);
begin
  if not HookEvent(Context, sekExited) then
    DoExited(Context);
end;

{*
  Trouve l'objet et exécute l'effet
  @param Context   Contexte du déplacement
*}
procedure TSquare.Execute(Context: TMoveContext);
begin
  if not HookEvent(Context, sekExecute) then
    DoExecute(Context);
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
  if not HookEvent(Context, sekPushing) then
    DoPushing(Context);
end;

{-------------}
{ Classe TMap }
{-------------}

{*
  Crée une instance de TMap et lui donne des dimensions
  @param AMaster       Maître FunLabyrinthe
  @param AID           ID de la carte
  @param ADimensions   Dimensions de la carte (en cases)
  @param AZoneWidth    Largeur d'une zone de la carte
  @param AZoneHeight   Hauteur d'une zone de la carte
*}
constructor TMap.CreateSized(AMaster: TMaster; const AID: TComponentID;
  ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer);
begin
  Create(AMaster, AID);

  FIsAdditionnal := True;

  FDimensions := ADimensions;
  FZoneWidth := AZoneWidth;
  FZoneHeight := AZoneHeight;

  CreateMap;
end;

{*
  Crée la carte
*}
procedure TMap.CreateMap;
begin
  FOutsideOffset := FDimensions.X * FDimensions.Y * FDimensions.Z;
  SetLength(FMap, FOutsideOffset + FDimensions.Z);
  FillChar(FMap[0], Length(FMap)*SizeOf(TSquare), 0);
end;

{*
  Charge la carte depuis un flux
  @param Stream   Flux source
*}
procedure TMap.LoadMapFromStream(Stream: TStream);
var
  I, Count, Value, SquareSize: Integer;
  Palette: array of TSquare;
begin
  // Contrôle de format
  Stream.ReadBuffer(Value, 4);
  if Value <> MapStreamFormatCode then
    EInOutError.Create(SInvalidFileFormat);

  // Contrôle de version de format
  Stream.ReadBuffer(Value, 4);
  if Value > MapStreamVersion then
    EInOutError.CreateFmt(SVersionTooHigh, [IntToStr(Value)]);

  // Lecture des dimensions et de la taille d'une zone
  Stream.ReadBuffer(FDimensions, SizeOf(T3DPoint));
  Stream.ReadBuffer(FZoneWidth, 4);
  Stream.ReadBuffer(FZoneHeight, 4);

  // Création de la carte elle-même
  CreateMap;

  // Lecture de la palette de cases
  Stream.ReadBuffer(Count, 4);
  SetLength(Palette, Count);
  for I := 0 to Count-1 do
    Palette[I] := Master.Square[ReadStrFromStream(Stream, TEncoding.UTF8)];

  // Lecture de la carte
  if Count <= 256 then
    SquareSize := 1
  else
    SquareSize := 2;
  Value := 0;
  for I := 0 to LinearMapCount-1 do
  begin
    Stream.ReadBuffer(Value, SquareSize);
    LinearMap[I] := Palette[Value];
  end;
end;

{*
  Enregistre la carte dans un flux
  @param Stream   Flux destination
*}
procedure TMap.SaveMapToStream(Stream: TStream);
var
  I, Value, Count, PaletteCountPos, SquareSize: Integer;
begin
  // Indication de format
  Stream.WriteBuffer(MapStreamFormatCode, 4);

  // Indication de version de format
  Value := MapStreamVersion;
  Stream.WriteBuffer(Value, 4);

  // Écriture des dimensions et de la taille d'une zone
  Stream.WriteBuffer(FDimensions, SizeOf(T3DPoint));
  Stream.WriteBuffer(FZoneWidth, 4);
  Stream.WriteBuffer(FZoneHeight, 4);

  // Préparation de la palette (Squares.Tag) et écriture de celle-ci
  for I := 0 to Master.SquareCount-1 do
    Master.Squares[I].Tag := -1;
  Count := 0;
  PaletteCountPos := Stream.Position;
  Stream.WriteBuffer(Count, 4); // On repassera changer çà plus tard
  for I := 0 to LinearMapCount-1 do
  begin
    with LinearMap[I] do
    begin
      if ClassType <> TSquare then
        raise EInOutError.CreateFmt(STemporaryStatedMap, [ID]);
      if Tag < 0 then
      begin
        Tag := Count;
        WriteStrToStream(Stream, ID, TEncoding.UTF8);
        Inc(Count);
      end;
    end;
  end;

  // Écriture de la carte
  if Count <= 256 then
    SquareSize := 1
  else
    SquareSize := 2;
  for I := 0 to LinearMapCount-1 do
  begin
    Value := LinearMap[I].Tag;
    Stream.WriteBuffer(Value, SquareSize);
  end;

  // On retourne écrire la taille de la palette
  Stream.Seek(PaletteCountPos, soFromBeginning);
  Stream.WriteBuffer(Count, 4);
  Stream.Seek(0, soFromEnd);
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
  [@inheritDoc]
*}
procedure TMap.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Map', LoadMapFromStream, SaveMapToStream);
end;

{*
  Remplace la carte interne
  @param ADimensions   Dimensions de la nouvelle carte
  @param AMap          Nouvelle carte interne
*}
procedure TMap.ReplaceMap(const ADimensions: T3DPoint;
  const AMap: TSquareDynArray);
var
  AOutsideOffset: Integer;
begin
  AOutsideOffset := ADimensions.X * ADimensions.Y * ADimensions.Z;

  Assert(Length(AMap) = AOutsideOffset + ADimensions.Z);

  FMap := AMap;
  FDimensions := ADimensions;
  FOutsideOffset := AOutsideOffset;
end;

{*
  Assigne une carte à celle-ci
  @param Source   Carte source
*}
procedure TMap.Assign(Source: TMap);
begin
  FDimensions := Source.FDimensions;
  FZoneWidth := Source.FZoneWidth;
  FZoneHeight := Source.FZoneHeight;
  FMap := Copy(Source.FMap);
  FOutsideOffset := Source.FOutsideOffset;
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
  Teste si une position est dans les bornes d'étages de la carte
  @param Position   Position à tester
  @return True si la position est dans les bornes d'étages, False sinon
*}
function TMap.InFloors(const Position: T3DPoint): Boolean;
begin
  Result := (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

{*
  Teste si un numéro d'étage est dans les bornes d'étages de la carte
  @param Floor   Numéro d'étage à tester
  @return True si le numéro est dans les bornes d'étages, False sinon
*}
function TMap.InFloors(Floor: Integer): Boolean;
begin
  Result := (Floor >= 0) and (Floor < FDimensions.Z);
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
  Dessine les cases
  @param Context   Contexte de dessin de la vue
*}
procedure TLabyrinthPlayerMode.DrawSquares(Context: TDrawViewContext;
  Ceiling: Boolean);
var
  QPos: TQualifiedPos;
  X, Y: Integer;
  Square: TSquare;
  DrawSquareContext: TDrawSquareContext;
begin
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

        DrawSquareContext := TDrawSquareContext.Create(Bitmap,
          X*SquareSize, Y*SquareSize, QPos);
        try
          DrawSquareContext.SetDrawViewContext(Context);

          if Ceiling then
            Square.DrawCeiling(DrawSquareContext)
          else
            Square.Draw(DrawSquareContext);
        finally
          DrawSquareContext.Free;
        end;
      end;
    end;
  end;
end;

{*
  Dessine les joueurs et autres composants à position
  @param Context   Contexte de dessin de la vue
*}
procedure TLabyrinthPlayerMode.DrawPosComponents(Context: TDrawViewContext);
var
  I: Integer;
  PosComponent: TPosComponent;
begin
  for I := 0 to Master.PosComponentCount-1 do
  begin
    PosComponent := Master.OrderedPosComponents[I];

    // Read-lock players while drawing
    if PosComponent is TPlayer then
      TPlayer(PosComponent).FLock.BeginRead;
    try
      DrawPosComponent(Context, PosComponent);
    finally
      if PosComponent is TPlayer then
        TPlayer(PosComponent).FLock.EndRead;
    end;
  end;
end;

{*
  Dessine un composant à position
  @param Context        Contexte de dessin de la vue
  @param PosComponent   Composant à dessiner
*}
procedure TLabyrinthPlayerMode.DrawPosComponent(Context: TDrawViewContext;
  PosComponent: TPosComponent);
var
  PosComponentContext: TDrawSquareContext;
begin
  if not Context.IsSquareVisible(PosComponent.QPos) then
    Exit;

  PosComponentContext := TDrawSquareContext.Create(Context.Bitmap,
    (PosComponent.Position.X-Context.Zone.Left) * SquareSize,
    (PosComponent.Position.Y-Context.Zone.Top) * SquareSize);
  try
    PosComponentContext.SetDrawViewContext(Context);
    PosComponent.Draw(PosComponentContext);
  finally
    PosComponentContext.Free;
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
  DrawSquares(Context, False);
  DrawPosComponents(Context);
  DrawSquares(Context, True);
end;

{*
  [@inheritDoc]
*}
procedure TLabyrinthPlayerMode.PressKey(Context: TKeyEventContext);
var
  Dir: TDirection;
begin
  case Context.Key of
    VK_UP: Dir := diNorth;
    VK_RIGHT: Dir := diEast;
    VK_DOWN: Dir := diSouth;
    VK_LEFT: Dir := diWest;
  else
    Exit;
  end;

  Context.Handled := True;

  Player.Move(Dir, Context.Key, Context.Shift);
end;

{---------------------}
{ TPosComponent class }
{---------------------}

{*
  Teste si la propriété ZIndex doit être enregistrée
  @return True ssi la propriété ZIndex doit être enregistrée
*}
function TPosComponent.IsZIndexStored: Boolean;
begin
  Result := FZIndex <> FDefaultZIndex;
end;

{*
  Modifie le Z-index
  @param Value   Nouveau Z-index
*}
procedure TPosComponent.SetZIndex(Value: Integer);
begin
  FZIndex := Value;
  Master.InvalidateOrderedPosComponents;
end;

{*
  [@inheritDoc]
*}
procedure TPosComponent.DefineProperties(Filer: TFunLabyFiler);
var
  HasData: Boolean;
begin
  inherited;

  HasData := not IsNoQPos(QPos);

  Filer.DefineFieldProperty('Map', TypeInfo(TMap), @FQPos.Map, HasData);

  Filer.DefineFieldProperty('Position.X', TypeInfo(Integer),
    @FQPos.Position.X, HasData);
  Filer.DefineFieldProperty('Position.Y', TypeInfo(Integer),
    @FQPos.Position.Y, HasData);
  Filer.DefineFieldProperty('Position.Z', TypeInfo(Integer),
    @FQPos.Position.Z, HasData);
end;

{*
  [@inheritDoc]
*}
procedure TPosComponent.StoreDefaults;
begin
  inherited;

  FDefaultZIndex := FZIndex;
end;

{*
  Spécifie les types d'événements de case à intercepter
  Cette méthode ne peut être appelée que dans le constructeur.
  @param Value   Types d'événements de case à intercepter
*}
procedure TPosComponent.SetWantedSquareEvents(Value: TSquareEventKinds);
begin
  Assert(psCreating in PersistentState);

  FWantedSquareEvents := Value;
end;

{*
  Spécifie si le composant veut attraper les messages de la case
  Cette méthode ne peut être appelée que dans le constructeur.
  @param Value   True pour intercepter les messages, False sinon
*}
procedure TPosComponent.SetWantMessages(Value: Boolean);
begin
  Assert(psCreating in PersistentState);

  FWantMessages := Value;
end;

{*
  Méthode de notification que la position de ce composant à changé
*}
procedure TPosComponent.PositionChanged;
begin
end;

{*
  Change la position de ce composant
  @param AQPos   Nouvelle position qualifiée
*}
procedure TPosComponent.ChangePosition(const AQPos: TQualifiedPos);
begin
  FQPos := AQPos;
  PositionChanged;
end;

{*
  Change la position de ce composant
  @param AMap        Nouvelle carte
  @param APosition   Nouvelle position
*}
procedure TPosComponent.ChangePosition(AMap: TMap; const APosition: T3DPoint);
var
  AQPos: TQualifiedPos;
begin
  AQPos.Map := AMap;
  AQPos.Position := APosition;

  ChangePosition(AQPos);
end;

{*
  Change la position de ce composant
  @param APosition   Nouvelle position
*}
procedure TPosComponent.ChangePosition(const APosition: T3DPoint);
begin
  ChangePosition(Map, APosition);
end;

{-----------------------}
{ TSquareModifier class }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TSquareModifier.Create(AMaster: TMaster; const AID: TComponentID);
var
  AWantedEventKinds: TSquareEventKinds;
begin
  inherited;

  AWantedEventKinds := [];

  FEventHandlers[sekEntering] := Entering;
  if @FEventHandlers[sekEntering] <> @TSquareModifier.Entering then
    Include(AWantedEventKinds, sekEntering);

  FEventHandlers[sekExiting] := Exiting;
  if @FEventHandlers[sekExiting] <> @TSquareModifier.Exiting then
    Include(AWantedEventKinds, sekExiting);

  FEventHandlers[sekEntered] := Entered;
  if @FEventHandlers[sekEntered] <> @TSquareModifier.Entered then
    Include(AWantedEventKinds, sekEntered);

  FEventHandlers[sekExited] := Exited;
  if @FEventHandlers[sekExited] <> @TSquareModifier.Exited then
    Include(AWantedEventKinds, sekExited);

  FEventHandlers[sekExecute] := Execute;
  if @FEventHandlers[sekExecute] <> @TSquareModifier.Execute then
    Include(AWantedEventKinds, sekExecute);

  FEventHandlers[sekPushing] := Pushing;
  if @FEventHandlers[sekPushing] <> @TSquareModifier.Pushing then
    Include(AWantedEventKinds, sekPushing);

  SetWantedSquareEvents(AWantedEventKinds);
end;

{*
  Gestionnaire d'événement SquareEvent
  @param Msg   Message
*}
procedure TSquareModifier.MessageSquareEvent(var Msg: TSquareEventMessage);
begin
  FEventHandlers[Msg.Kind](Msg.Context);
end;

{*
  Gestionnaire de l'événement de case Entering
  @param Context   Contexte du déplacement
*}
procedure TSquareModifier.Entering(Context: TMoveContext);
begin
  Context.Square.DoEntering(Context);
end;

{*
  Gestionnaire de l'événement de case Exiting
  @param Context   Contexte du déplacement
*}
procedure TSquareModifier.Exiting(Context: TMoveContext);
begin
  Context.Square.DoExiting(Context);
end;

{*
  Gestionnaire de l'événement de case Entered
  @param Context   Contexte du déplacement
*}
procedure TSquareModifier.Entered(Context: TMoveContext);
begin
  Context.Square.DoEntered(Context);
end;

{*
  Gestionnaire de l'événement de case Exited
  @param Context   Contexte du déplacement
*}
procedure TSquareModifier.Exited(Context: TMoveContext);
begin
  Context.Square.DoExited(Context);
end;

{*
  Gestionnaire de l'événement de case Execute
  @param Context   Contexte du déplacement
*}
procedure TSquareModifier.Execute(Context: TMoveContext);
begin
  Context.Square.DoExecute(Context);
end;

{*
  Gestionnaire de l'événement de case Pushing
  @param Context   Contexte du déplacement
*}
procedure TSquareModifier.Pushing(Context: TMoveContext);
begin
  Context.Square.DoPushing(Context);
end;

{----------------------}
{ TVehiclePlugin class }
{----------------------}

{*
  [@inheritDoc]
*}
constructor TVehiclePlugin.Create(AMaster: TMaster; const AID: TComponentID);
begin
  Assert(FVehicle <> nil);

  inherited;

  FTransient := True;
end;

{*
  [@inheritDoc]
*}
function TVehiclePlugin.GetIsDesignable: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TVehiclePlugin.DrawBefore(Context: TDrawSquareContext);
begin
  Vehicle.DrawBefore(Context);
end;

{*
  [@inheritDoc]
*}
procedure TVehiclePlugin.DrawAfter(Context: TDrawSquareContext);
begin
  Vehicle.DrawAfter(Context);
end;

{*
  [@inheritDoc]
*}
procedure TVehiclePlugin.Moving(Context: TMoveContext);
begin
  Vehicle.Moving(Context);
end;

{*
  [@inheritDoc]
*}
procedure TVehiclePlugin.Moved(Context: TMoveContext);
begin
  Vehicle.Moved(Context);
end;

{*
  [@inheritDoc]
*}
function TVehiclePlugin.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := Vehicle.AbleTo(Player, Action, Param);
end;

{----------------}
{ TVehicle class }
{----------------}

{*
  [@inheritDoc]
*}
constructor TVehicle.Create(AMaster: TMaster; const AID: TComponentID);
var
  Dir: TDirection;
begin
  inherited;

  FPlugin := TVehiclePlugin(TVehiclePlugin.NewInstance);
  FPlugin.FVehicle := Self;
  FPlugin.Create(Master, ID+'Plugin'); {don't localize}

  FDirPainters[diNone] := Painter;
  for Dir := diNorth to diWest do
    FDirPainters[Dir] := TPainter.Create(Master.ImagesMaster);
end;

{*
  [@inheritDoc]
*}
destructor TVehicle.Destroy;
var
  Dir: TDirection;
begin
  for Dir := diNorth to diWest do
    FDirPainters[Dir].Free;

  inherited;
end;

{*
  Peintres de ce véhicule par direction
  @param Dir   Direction
  @return Peintre de ce véhicule pour la direction Dir
*}
function TVehicle.GetPainters(Dir: TDirection): TPainter;
begin
  Result := FDirPainters[Dir];
end;

{*
  [@inheritDoc]
*}
procedure TVehicle.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineFieldProperty('Controller', TypeInfo(TPlayer),
    @FController, FController <> nil);
end;

{*
  [@inheritDoc]
*}
function TVehicle.GetCategory: string;
begin
  Result := SCategoryVehicles;
end;

{*
  Attache à un contrôleur et retire le véhicule de la carte
  @param AController   Contrôleur
*}
procedure TVehicle.AttachController(AController: TPlayer);
begin
  ChangePosition(NoQPos);
  FController := AController;
  FController.AddPlugin(Plugin);
end;

{*
  Détache le véhicule de son contrôleur et repositionne le véhicule
  @param AQPos   Position à laquelle repositionner le véhicule
*}
procedure TVehicle.DetachController(const AQPos: TQualifiedPos);
begin
  FController.RemovePlugin(Plugin);
  FController := nil;
  ChangePosition(AQPos);
end;

{*
  Détache le véhicule de son contrôleur et repositionne le véhicule
  Le véhicule est repositionné à la position actuelle de son contrôleur
*}
procedure TVehicle.DetachController;
begin
  DetachController(Controller.QPos);
end;

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TVehicle.DrawBefore(Context: TDrawSquareContext);
begin
  DirPainters[Context.Player.Direction].Draw(Context);
end;

{*
  Dessine sur le joueur
  DrawAfter est exécuté lors du dessin du joueur, après celui-ci. Le dessin
  effectué dans DrawAfter se retrouve donc sur le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TVehicle.DrawAfter(Context: TDrawSquareContext);
begin
end;

{*
  Un joueur se déplace
  Moving est exécuté lorsqu'un joueur se déplace d'une case à une autre.
  @param Context   Contexte du déplacement
*}
procedure TVehicle.Moving(Context: TMoveContext);
begin
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Context   Contexte du déplacement
*}
procedure TVehicle.Moved(Context: TMoveContext);
begin
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @param Param    Paramètre de l'action
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TVehicle.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := False;
end;

{-------------------------}
{ Classe TMobileComponent }
{-------------------------}

{*
  [@inheritDoc]
*}
constructor TMobileComponent.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FActionLock := TCriticalSection.Create;
  FInActionLock := TCriticalSection.Create;
  FActionCoroutine := TCoroutine.Create(ActionProc, clNextInvoke);
end;

{*
  [@inheritDoc]
*}
destructor TMobileComponent.Destroy;
begin
  FActionCoroutine.Free;
  FInActionLock.Free;
  FActionLock.Free;

  inherited;
end;

{*
  Gestionnaire du message msgRunMethod
  @param Msg   Message à traiter
*}
procedure TMobileComponent.MessageRunMethod(var Msg: TRunMethodMessage);
begin
  Msg.Method;
end;

{*
  Procédure de la coroutine d'exécution des actions
  @param Coroutine   Objet coroutine gérant cette procédure
*}
procedure TMobileComponent.ActionProc(Coroutine: TCoroutine);
begin
  FInActionLock.Acquire;
  try
    // FActionMessagePtr has been set before this method is called.
    Dispatch(FActionMessagePtr^);
  finally
    FInActionLock.Release;
  end;
end;

{*
  Essaye de mettre en pause les actions de ce composant
  Si TryPause renvoie True, vous devez appelez Resume pour redémarrer les
  actions.
  @return True si les actions ont été mises en pause, False sinon
*}
function TMobileComponent.TryPause: Boolean;
begin
  Result := FInActionLock.TryEnter;
end;

{*
  Redémarre les actions de ce composant
  Resume doit être appelé pour redémarrer les actions après un TryPause réussi.
*}
procedure TMobileComponent.Resume;
begin
  FInActionLock.Release;
end;

{*
  Envoie un message au joueur dans le contexte de ses actions
  Cette méthode ne peut *pas* être appelée depuis le code d'action du
  composant ! Utilisez directement Dispatch à la place.
  @param Msg   Message à envoyer
*}
procedure TMobileComponent.SendMessage(var Msg);
var
  Acquired: Boolean;
begin
  repeat
    FInActionLock.Acquire;
    try
      Acquired := FActionLock.TryEnter;
    finally
      FInActionLock.Release;
    end;
  until Acquired;

  try
    Assert(not FActionCoroutine.CoroutineRunning);

    FActionMessagePtr := @Msg;
    FActionCoroutine.Invoke;
  finally
    FActionLock.Release;
  end;
end;

{*
  Exécute une méthode dans le contexte des actions de ce composant
  Cette méthode ne peut *pas* être appelée depuis le code d'action du
  composant ! Appelez directement la méthode en question à la place.
  @param Method   Méthode à exécuter
*}
procedure TMobileComponent.RunMethod(const Method: TThreadMethod);
var
  Msg: TRunMethodMessage;
begin
  Msg.MsgID := msgRunMethod;
  Msg.Handled := False;
  Msg.Reserved := 0;
  Msg.Component := Self;
  Msg.Method := Method;

  SendMessage(Msg);
end;

{----------------}
{ Classe TPlayer }
{----------------}

{*
  [@inheritDoc]
*}
constructor TPlayer.Create(AMaster: TMaster; const AID: TComponentID);
var
  Dir: TDirection;
begin
  inherited;

  FreeAndNil(FPainter);
  FPainter := TPlayerPainter.Create(Master.ImagesMaster, Self);
  FPainter.BeginUpdate;

  Name := SDefaultPlayerName;
  FMode := TLabyrinthPlayerMode.Create(Self);
  FModeStack := TInterfaceList.Create;
  FColor := DefaultPlayerColor;
  FViewBorderSize := DefaultViewBorderSize;
  FPlugins := TObjectList.Create(False);
  FAttributes := THashedStringList.Create;
  TStringList(FAttributes).CaseSensitive := True;
  FFoundObjects := TObjectList.Create(False);

  FColoredPainterCache := CreateEmptySquareBitmap;
  FDirPainters[diNone] := Painter;
  for Dir := diNorth to diWest do
    FDirPainters[Dir] := TPainter.Create(Master.ImagesMaster);

  FDefaultTemporization := FunLabyUtils.DefaultTemporization;

  FLock := TMultiReadExclusiveWriteSynchronizer.Create;

  ZIndex := DefaultPlayerZIndex;

  Painter.AddImage(fPlayer);
end;

{*
  [@inheritDoc]
*}
destructor TPlayer.Destroy;
var
  Dir: TDirection;
begin
  FLock.Free;

  for Dir := diNorth to diWest do
    FDirPainters[Dir].Free;
  FColoredPainterCache.Free;

  FFoundObjects.Free;
  FAttributes.Free;
  FPlugins.Free;
  
  inherited;
end;

{*
  Liste des plug-in attachés au joueur sous forme de chaîne
  @return Liste des plug-in attachés au joueur sous forme de chaîne
*}
function TPlayer.GetPluginListStr: string;
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  GetPluginList(Plugins);

  if Length(Plugins) > 0 then
  begin
    for I := 0 to Length(Plugins)-1 do
      Result := Result + Plugins[I].ID + ' ';
    SetLength(Result, Length(Result)-1);
  end;
end;

{*
  Modifie la liste des plug-in attachés au joueur à partir d'une chaîne
  @param Value   Liste des plug-in à attacher au joueur sous forme de chaîne
*}
procedure TPlayer.SetPluginListStr(const Value: string);
var
  PluginID, Remaining, Temp: string;
  Plugin: TPlugin;
begin
  FLock.BeginWrite;
  try
    FPlugins.Clear;

    Remaining := Value;
    while Remaining <> '' do
    begin
      SplitToken(Remaining, ' ', PluginID, Temp);
      Remaining := Temp;
      Plugin := Master.Plugin[PluginID];
      if Plugin <> nil then
        FPlugins.Add(Plugin);
    end;
  finally
    FLock.EndWrite;
  end;
end;

{*
  Liste des modes attachés au joueur sous forme de chaîne
  @return Liste des modes attachés au joueur sous forme de chaîne
*}
function TPlayer.GetModeListStr: string;
var
  I: Integer;
begin
  FModeStack.Lock;
  try
    for I := 0 to FModeStack.Count-1 do
      Result := Result +
        (FModeStack[I] as IPlayerMode).ModeClass.ClassName + ' ';

    Result := Result + FMode.ModeClass.ClassName;
  finally
    FModeStack.Unlock;
  end;
end;

{*
  Modifie la liste des modes attachés au joueur à partir d'une chaîne
  @param Value   Liste des modes à attacher au joueur sous forme de chaîne
*}
procedure TPlayer.SetModeListStr(const Value: string);
var
  ModeClassName, Remaining, Temp: string;
  ModeClass: TPlayerModeClass;
begin
  FModeStack.Lock;
  try
    FModeStack.Clear;

    Remaining := Value;
    while Remaining <> '' do
    begin
      SplitToken(Remaining, ' ', ModeClassName, Temp);
      Remaining := Temp;

      ModeClass := TPlayerModeClass(FunLabyFindClass(ModeClassName));

      FModeStack.Add(ModeClass.Create(Self));
    end;

    if FModeStack.Count > 0 then
    begin
      FMode := FModeStack.Last as IPlayerMode;
      FModeStack.Delete(FModeStack.Count-1);
    end;
  finally
    FModeStack.Unlock;
  end;
end;

{*
  Liste des plug-in attachés au joueur sous forme de chaîne
  @return Liste des plug-in attachés au joueur sous forme de chaîne
*}
function TPlayer.GetFoundObjectsStr: string;
var
  I: Integer;
begin
  FLock.BeginRead;
  try
    if FFoundObjects.Count > 0 then
    begin
      for I := 0 to FFoundObjects.Count-1 do
        Result := Result + TObjectDef(FFoundObjects[I]).ID + ' ';
      SetLength(Result, Length(Result)-1);
    end;
  finally
    FLock.EndRead;
  end;
end;

{*
  Modifie la liste des plug-in attachés au joueur à partir d'une chaîne
  @param Value   Liste des plug-in à attacher au joueur sous forme de chaîne
*}
procedure TPlayer.SetFoundObjectsStr(const Value: string);
var
  ObjectID, Remaining, Temp: string;
begin
  FLock.BeginWrite;
  try
    FFoundObjects.Clear;

    Remaining := Value;
    while Remaining <> '' do
    begin
      SplitToken(Remaining, ' ', ObjectID, Temp);
      Remaining := Temp;
      FFoundObjects.Add(Master.ObjectDef[ObjectID]);
    end;
  finally
    FLock.EndWrite;
  end;
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
  Met à jour le bitmap FColoredPainterCache
*}
procedure TPlayer.UpdateColoredPainterCache;
var
  X, Y: Integer;
  Line: PColor32Array;
begin
  FColoredPainterCache.Clear(clTransparent32);

  if (AlphaComponent(FColor) <> 0) and (not Painter.IsEmpty) then
  begin
    Painter.DrawTo(FColoredPainterCache);

    for Y := 0 to FColoredPainterCache.Height-1 do
    begin
      Line := FColoredPainterCache.ScanLine[Y];

      for X := 0 to FColoredPainterCache.Width-1 do
        Line[X] := MultiplyComponents(Line[X], FColor);
    end;
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
  Gestionnaire du message msgPressKey
  @param Msg   Message
*}
procedure TPlayer.MessagePressKey(var Msg: TPlayerPressKeyMessage);
var
  Context: TKeyEventContext;
  Plugins: TPluginDynArray;
  I: Integer;
begin
  Context := TKeyEventContext.Create(Self, Msg.Key, Msg.Shift);
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
  Indique que ce joueur a trouvé un objet
  @param ObjectDef   Définition de l'objet trouvé
*}
procedure TPlayer.FoundObject(ObjectDef: TObjectDef);
begin
  if Master.Editing or (not ObjectDef.DisplayInStatusBar) then
    Exit;

  FLock.BeginWrite;
  try
    if FFoundObjects.IndexOf(ObjectDef) < 0 then
      FFoundObjects.Add(ObjectDef);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Résoud un nom de fichier son
  @param HRef   HRef du fichier son
  @return Nom du fichier son, ou '' si non trouvé
*}
function TPlayer.ResolveSoundHRef(const HRef: string): TFileName;
begin
  Result := fSoundsDir + AnsiReplaceStr(HRef, '/', '\');

  if not FileExists(Result) then
    Result := '';
end;

{*
  Joue un son dans le contexte de ce joueur
  @param Sound    Identifiant du son
  @param Flags    Flags
  @param Module   Handle du module contenant la ressource (optionnel)
*}
procedure TPlayer.InternalPlaySound(const Sound: string; Flags: LongWord;
  Module: HModule = 0);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      MMSystem.PlaySound(PChar(Sound), Module, Flags);
    end);
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
  Modifie la couleur du joueur
  @param Value   Nouvelle couleur
*}
procedure TPlayer.SetColor(Value: TColor32);
begin
  FLock.BeginWrite;
  try
    FColor := Value;
    UpdateColoredPainterCache;
  finally
    FLock.EndWrite;
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
  FLock.BeginRead;
  try
    Index := FAttributes.IndexOf(AttrName);
    if Index < 0 then
      Result := 0
    else
      Result := Integer(FAttributes.Objects[Index]);
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
  finally
    FLock.EndWrite;
  end;
end;

{*
  Peintres de ce joueur par direction
  @param Dir   Direction
  @return Peintre de ce joueur pour la direction Dir
*}
function TPlayer.GetPainters(Dir: TDirection): TPainter;
begin
  Result := FDirPainters[Dir];
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineFieldProperty('ShowCounter', TypeInfo(Integer),
    @FShowCounter, FShowCounter <> 0);

  Filer.DefineFieldProperty('PlayState', TypeInfo(TPlayState),
    @FPlayState, PlayState <> psPlaying);

  Filer.DefineStrings('Attributes', FAttributes, TypeInfo(Integer));

  Filer.DefineProcProperty('Plugins', TypeInfo(string),
    @TPlayer.GetPluginListStr, @TPlayer.SetPluginListStr);

  Filer.DefineProcProperty('Modes', TypeInfo(string),
    @TPlayer.GetModeListStr, @TPlayer.SetModeListStr);

  Filer.DefineProcProperty('FoundObjects', TypeInfo(string),
    @TPlayer.GetFoundObjectsStr, @TPlayer.SetFoundObjectsStr,
    not Master.Editing);
end;

{*
  [@inheritDoc]
*}
function TPlayer.GetCategory: string;
begin
  Result := SCategoryPlayers;
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.DoDraw(Context: TDrawSquareContext);
begin
  case DrawMode of
    dmColoredPainter:
      FColoredPainterCache.DrawTo(Context.Bitmap, Context.X, Context.Y);

    dmPainter:
      Painter.Draw(Context);

    dmDirPainter:
      FDirPainters[Direction].Draw(Context);
  end;
end;

{*
  Dispatche un message au joueur
  Cette méthode ne peut *pas* être appelée hors code d'action du joueur !
  Utilisez SendMessage à la place dans ce cas.
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
    Attributes.Assign(FAttributes);
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
  Obtient la liste des objets trouvés, dans l'odre où il les a trouvés
  @param ObjectDefs   En sortie, remplie avec la liste des objets trouvés
*}
procedure TPlayer.GetFoundObjects(ObjectDefs: TObjectList);
begin
  FLock.BeginRead;
  try
    ObjectDefs.Assign(FFoundObjects);
  finally
    FLock.EndRead;
  end;
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
  Assert(Plugin <> nil);

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
  Assert(Plugin <> nil);

  FLock.BeginWrite;
  try
    FPlugins.Remove(Plugin);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Teste si un plug-in donné est attaché au joueur
  @param Plugin   Plug-in à tester
  @return True si le plug-in est attaché au joueur, False sinon
*}
function TPlayer.HasPlugin(Plugin: TPlugin): Boolean;
begin
  FLock.BeginRead;
  try
    Result := FPlugins.IndexOf(Plugin) >= 0;
  finally
    FLock.EndRead;
  end;
end;

{*
  Indique si le joueur est capable d'effectuer une action donnée
  Un joueur est capable d'effectuer une action si l'un de ses plug-in ou l'un de
  ses objets le lui permet.
  @param Action   Action à tester
  @param Param    Paramètre de l'action
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TPlayer.AbleTo(const Action: TPlayerAction;
  Param: Integer = 0): Boolean;
var
  Plugins: TPluginDynArray;
  I: Integer;
begin
  Result := True;

  GetPluginList(Plugins);
  for I := 0 to Length(Plugins)-1 do
    if Plugins[I].AbleTo(Self, Action, Param) then
      Exit;

  for I := 0 to Master.ObjectDefCount-1 do
    if Master.ObjectDefs[I].AbleTo(Self, Action, Param) then
      Exit;

  Result := False;
end;

{*
  Exécute l'action spécifiée
  DoAction vérifie d'abord que le joueur en est bien capable. Et si plusieurs
  objets permettent d'effectuer l'action, le joueur se voit demander d'en
  choisir un.
  @param Action   Action à tester
  @param Param    Paramètre de l'action
  @return True si le joueur a été capable d'effectuer l'action, False sinon
*}
function TPlayer.DoAction(const Action: TPlayerAction;
  Param: Integer = 0): Boolean;
var
  Plugins: TPluginDynArray;
  I, GoodObjectCount: Integer;
  GoodObjects: array of TObjectDef;
  ObjectNames: TStringDynArray;
  GoodObject: TObjectDef;
begin
  Result := True;

  // Les plug-in ont la priorité, puisqu'ils n'ont pas d'effet de bord
  GetPluginList(Plugins);
  for I := 0 to Length(Plugins)-1 do
    if Plugins[I].AbleTo(Self, Action, Param) then
      Exit;

  FLock.BeginWrite;
  try
    // Listage des objets susceptibles d'aider le joueur
    SetLength(GoodObjects, Master.ObjectDefCount);
    GoodObjectCount := 0;
    for I := 0 to Master.ObjectDefCount-1 do
    begin
      if Master.ObjectDefs[I].AbleTo(Self, Action, Param) then
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
      SetLength(ObjectNames, GoodObjectCount);
      for I := 0 to GoodObjectCount-1 do
        ObjectNames[I] := GoodObjects[I].Name;
      I := ShowSelectionMsg(sWhichObject, ObjectNames);
      GoodObject := GoodObjects[I];
    end;

    // Utilisation de l'objet
    GoodObject.UseFor(Self, Action, Param);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir         Direction du déplacement
  @param Key         Touche qui a été pressée pour le déplacement
  @param Shift       État des touches spéciales
  @param Redo        Indique s'il faut réitérer le déplacement
  @param RedoDelay   Délai en millisecondes avant de réitérer le déplacement
*}
procedure TPlayer.Move(Dir: TDirection; Key: Word; Shift: TShiftState;
  out Redo: Boolean; out RedoDelay: Cardinal);
var
  Context: TMoveContext;
begin
  Redo := False;

  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  Context := TMoveContext.Create(Self, PointBehind(Position, Dir),
    Key, Shift);
  try
    FDirection := Dir;

    if not IsMoveAllowed(Context) then
      Exit;

    if Same3DPoint(Position, Context.Src) and (Map = Context.SrcMap) then
      MoveTo(Context);

    Redo := Context.GoOnMoving;
    RedoDelay := Context.Temporization;
  finally
    Context.Free;
  end;
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir         Direction du déplacement
  @param Redo        Indique s'il faut réitérer le déplacement
  @param RedoDelay   Délai en millisecondes avant de réitérer le déplacement
*}
procedure TPlayer.Move(Dir: TDirection; out Redo: Boolean;
  out RedoDelay: Cardinal);
begin
  Move(Dir, 0, [], Redo, RedoDelay);
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir     Direction du déplacement
  @param Key     Touche qui a été pressée pour le déplacement
  @param Shift   État des touches spéciales
*}
procedure TPlayer.Move(Dir: TDirection; Key: Word; Shift: TShiftState);
var
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  Move(Dir, Key, Shift, Redo, RedoDelay);
  NaturalMoving(Redo, RedoDelay);
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir   Direction du déplacement
*}
procedure TPlayer.Move(Dir: TDirection);
var
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  Move(Dir, 0, [], Redo, RedoDelay);
  NaturalMoving(Redo, RedoDelay);
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
  Teste si un déplacement est permis
  @param Dest    Case destination
  @param Key     Touche pressée pour le déplacement
  @param Shift   État des touches spéciales
  @return True si le déplacement est permis, False sinon
*}
function TPlayer.IsMoveAllowed(const Dest: T3DPoint; Key: Word;
  Shift: TShiftState): Boolean;
var
  Context: TMoveContext;
begin
  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
  begin
    Result := False;
    Exit;
  end;

  Context := TMoveContext.Create(Self, Dest, Key, Shift);
  try
    Result := IsMoveAllowed(Context);
  finally
    Context.Free;
  end;
end;

{*
  Teste si un déplacement est permis
  @param Dest   Case destination
  @return True si le déplacement est permis, False sinon
*}
function TPlayer.IsMoveAllowed(const Dest: T3DPoint): Boolean;
begin
  Result := IsMoveAllowed(Dest, 0, []);
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
  ChangePosition(Context.DestMap, Context.Dest);

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
  if Execute and (Map = Context.Map) and Same3DPoint(Position, Context.Pos) then
    Map[Position].Execute(Context);
end;

{*
  Déplace le joueur
  @param Dest        Position de destination
  @param Execute     Indique s'il faut exécuter la case d'arrivée
  @param Redo        Indique s'il faut réitérer le déplacement
  @param RedoDelay   Délai en millisecondes avant de réitérer le déplacement
*}
procedure TPlayer.MoveTo(const Dest: T3DPoint; Execute: Boolean;
  out Redo: Boolean; out RedoDelay: Cardinal);
var
  DestQPos: TQualifiedPos;
begin
  DestQPos.Map := Map;
  DestQPos.Position := Dest;

  MoveTo(DestQPos, Execute, Redo, RedoDelay);
end;

{*
  Déplace le joueur, et poursuit le déplacement si nécessaire
  @param Dest      Position de destination
  @param Execute   Indique s'il faut exécuter la case d'arrivée
*}
procedure TPlayer.MoveTo(const Dest: T3DPoint; Execute: Boolean = False);
var
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  MoveTo(Dest, Execute, Redo, RedoDelay);
  NaturalMoving(Redo, RedoDelay);
end;

{*
  Déplace le joueur
  @param Dest        Position de destination
  @param Execute     Indique s'il faut exécuter la case d'arrivée
  @param Redo        Indique s'il faut réitérer le déplacement
  @param RedoDelay   Délai en millisecondes avant de réitérer le déplacement
*}
procedure TPlayer.MoveTo(const Dest: TQualifiedPos; Execute: Boolean;
  out Redo: Boolean; out RedoDelay: Cardinal);
var
  Context: TMoveContext;
begin
  Redo := False;

  // Le joueur est-il toujours en train de jouer ?
  if PlayState <> psPlaying then
    Exit;

  Context := TMoveContext.Create(Self, Dest);
  try
    MoveTo(Context, Execute);
    Redo := Context.GoOnMoving;
    RedoDelay := Context.Temporization;
  finally
    Context.Free;
  end;
end;

{*
  Déplace le joueur, et poursuit le déplacement si nécessaire
  @param Dest      Position de destination
  @param Execute   Indique s'il faut exécuter la case d'arrivée
*}
procedure TPlayer.MoveTo(const Dest: TQualifiedPos; Execute: Boolean = False);
var
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  MoveTo(Dest, Execute, Redo, RedoDelay);
  NaturalMoving(Redo, RedoDelay);
end;

{*
  Déplacement naturel, selon le mouvement déjà entamé (sorte d'inertie)
  Après un mouvement donné expressément, il suffit d'appeler NaturalMoving pour
  continuer le mouvement normalement.
*}
procedure TPlayer.NaturalMoving(Redo: Boolean; RedoDelay: Cardinal);
begin
  while Redo do
  begin
    Sleep(RedoDelay);
    Move(Direction, Redo, RedoDelay);
  end;
end;

{*
  Modifie la position sans interragir avec les cases
  ChangePosition ne doit être utilisée qu'en mode édition, ou sous réserve
  d'être certain de ce qu'on fait.
*}
procedure TPlayer.ChangePosition(const AQPos: TQualifiedPos);
begin
  FLock.BeginWrite;
  try
    inherited;
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
  Affiche un message demandant une sélection au joueur
  @param Prompt             Message d'invite
  @param Answers            Réponses possibles
  @param Default            Index de la réponse sélectionnée par défaut
  @param ShowOnlySelected   Si True, affiche uniquement l'élément sélectionné
  @return Index de la réponse sélectionnée
*}
function TPlayer.ShowSelectionMsg(const Prompt: string;
  const Answers: array of string; Default: Integer = 0;
  ShowOnlySelected: Boolean = False): Integer;
var
  Msg: TPlayerShowMsgMessage;
  I: Integer;
begin
  Msg.MsgID := msgShowMessage;
  Msg.Text := Prompt;
  Msg.Selected := MinMax(Default, 0, Length(Answers)-1);
  Msg.ShowOnlySelected := ShowOnlySelected;

  SetLength(Msg.Answers, Length(Answers));
  for I := 0 to Length(Answers)-1 do
    Msg.Answers[I] := Answers[I];

  Dispatch(Msg);
  Result := Msg.Selected;
end;

{*
  Affiche un message demandant une sélection au joueur
  @param Prompt             Message d'invite
  @param Answers            Réponses possibles
  @param Default            Index de la réponse sélectionnée par défaut
  @param ShowOnlySelected   Si True, affiche uniquement l'élément sélectionné
  @return Index de la réponse sélectionnée
*}
function TPlayer.ShowSelectionMsg(const Prompt: string; Answers: TStrings;
  Default: Integer = 0; ShowOnlySelected: Boolean = False): Integer;
var
  AnswersArray: TStringDynArray;
  I: Integer;
begin
  SetLength(AnswersArray, Answers.Count);

  for I := 0 to Answers.Count-1 do
    AnswersArray[I] := Answers[I];

  Result := ShowSelectionMsg(Prompt, AnswersArray, Default, ShowOnlySelected);
end;

{*
  Affiche un message demandant au joueur de sélectionner un nombre
  @param Prompt    Message d'invite
  @param Default   Nombre sélectionné par défaut
  @param Min       Valeur minimum
  @param Max       Valeur maximum
  @return Index de la réponse sélectionnée
*}
function TPlayer.ShowSelectNumberMsg(const Prompt: string; Default, Min,
  Max: Integer): Integer;
var
  Answers: TStringDynArray;
  I: Integer;
begin
  SetLength(Answers, Max-Min+1);

  for I := Min to Max do
    Answers[Max-I] := IntToStr(I);

  Result := Max - ShowSelectionMsg(Prompt, Answers, Max-Default, True);
end;

{*
  Joue un son pour le joueur
  @param HRef   HRef du fichier son à jouer
*}
procedure TPlayer.PlaySound(const HRef: string);
begin
  InternalPlaySound(ResolveSoundHRef(HRef), SND_FILENAME or SND_ASYNC);
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
  @param Bitmap   Bitmap cible (doit correspondre à Width/Height)
*}
procedure TPlayer.DrawView(Bitmap: TBitmap32);
var
  Context: TDrawViewContext;
  Plugins: TPluginDynArray;
  I: Integer;
begin
  Context := TDrawViewContext.Create(Mode, Bitmap);
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
var
  Msg: TPlayerPressKeyMessage;
begin
  Msg.MsgID := msgPressKey;
  Msg.Key := Key;
  Msg.Shift := Shift;

  FActionLock.Acquire;
  try
    Assert(not FActionCoroutine.CoroutineRunning);

    FActionMessagePtr := @Msg;
    FActionCoroutine.Invoke;
  finally
    FActionLock.Release;
  end;
end;

{*
  Attend qu'une touche soit pressée par le joueur
  Cette méthode ne peut être appelée que depuis le code d'action du joueur !
  @param Key     En sortie : Touche appuyée
  @param Shift   En sortie : État des touches spéciales lors de l'appui
*}
procedure TPlayer.WaitForKey(out Key: Word; out Shift: TShiftState);
var
  PressKeyMsg: ^TPlayerPressKeyMessage;
begin
  Assert(FActionCoroutine.CoroutineRunning);

  FActionCoroutine.Yield;

  PressKeyMsg := FActionMessagePtr;
  Assert(PressKeyMsg.MsgID = msgPressKey);

  Key := PressKeyMsg.Key;
  Shift := PressKeyMsg.Shift;
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

{-------------------}
{ TTimerEntry class }
{-------------------}

{*
  Crée une entrée de timer
  @param ATickCount   Tick-count auquel déclencher ce timer
*}
constructor TTimerEntry.Create(ATickCount: Cardinal);
begin
  inherited Create;

  FTickCount := ATickCount;
end;

{*
  Recrée une entrée de timer lors du chargement
*}
constructor TTimerEntry.ReCreate;
begin
  inherited Create;
end;

{*
  Exécute cette entrée de timer puis la libère
*}
procedure TTimerEntry.InternalExecuteAndFree;
begin
  try
    Execute;
  finally
    Free;
  end;
end;

{*
  Retourne le composant mobile qui doit être l'hôte de cette entrée de timer
  Si GetHostComponent renvoie une valeur non nil, la méthode Execute sera
  appelée au sein des actions de ce composant.
  Sinon, la méthode Execute est appelée au sein du thread général de gestion des
  timers.
  @return Composant hôte
*}
function TTimerEntry.GetHostComponent: TMobileComponent;
begin
  Result := nil;
end;

{*
  Exécute cette entrée de timer puis la libère
*}
procedure TTimerEntry.ExecuteAndFree;
var
  HostComponent: TMobileComponent;
begin
  HostComponent := GetHostComponent;

  if HostComponent = nil then
    InternalExecuteAndFree
  else
    HostComponent.RunMethod(InternalExecuteAndFree);
end;

{----------------------------}
{ TNotificationMsgTimerEntry }
{----------------------------}

{*
  Crée une entrée de timer de message de notification
  @param ATickCount    Tick-count auquel déclencher ce timer
  @param ADestObject   Objet destination
  @param AMsgID        ID du message
*}
constructor TNotificationMsgTimerEntry.Create(ATickCount: Cardinal;
  ADestObject: TFunLabyComponent; AMsgID: Word);
begin
  Assert(ADestObject <> nil);

  inherited Create(ATickCount);

  FDestObject := ADestObject;
  FMsgID := AMsgID;
end;

{*
  [@inheritDoc]
*}
function TNotificationMsgTimerEntry.GetHostComponent: TMobileComponent;
begin
  if DestObject is TMobileComponent then
    Result := TMobileComponent(DestObject)
  else
    Result := nil;
end;

{*
  [@inheritDoc]
*}
procedure TNotificationMsgTimerEntry.Execute;
var
  Msg: TPlayerMessage;
begin
  Assert(DestObject <> nil);

  Msg.MsgID := MsgID;

  DestObject.Dispatch(Msg);
end;

{------------------------}
{ TTimerCollection class }
{------------------------}

{*
  Crée une collection de timers
*}
constructor TTimerCollection.Create(AMaster: TMaster);
begin
  inherited Create;

  FMaster := AMaster;
  FThread := TMethodThread.Create(ThreadProc, True);
  FAddedEvent := TEvent.Create(nil, False, False, '');
  FLock := TCriticalSection.Create;
  FActionLock := TCriticalSection.Create;
end;

{*
  [@inheritDoc]
*}
destructor TTimerCollection.Destroy;
begin
  FThread.Free;
  FActionLock.Free;
  FLock.Free;
  FAddedEvent.Free;

  inherited;
end;

{*
  Méthode d'exécution du thread
*}
procedure TTimerCollection.ThreadProc(Thread: TMethodThread);
var
  Entry: TTimerEntry;
  Timeout: Cardinal;
begin
  Timeout := 0;

  while not Thread.Terminated do
  begin
    FActionLock.Acquire;
    try
      FLock.Acquire;
      try
        // Get entry
        if Count = 0 then
        begin
          Entry := nil;
          Timeout := INFINITE;
        end else
        begin
          Entry := TTimerEntry(Items[0]);

          if Entry.TickCount > Master.TickCount then
          begin
            Timeout := Entry.TickCount - Master.TickCount;
            Entry := nil;
          end else
            ExtractItem(Entry);
        end;
      finally
        FLock.Release;
      end;

      // If an entry was found, execute it and free it
      if Entry <> nil then
        Entry.ExecuteAndFree;
    finally
      FActionLock.Release;
    end;

    // If no entry was found: wait for a new one
    if Entry = nil then
      FAddedEvent.WaitFor(Timeout);
  end;
end;

{*
  Lance le thread d'exécution des timers
*}
procedure TTimerCollection.Start;
begin
  FThread.Resume;
end;

{*
  Essaye de mettre le temps en pause
  Si TryPause renvoie True, vous devez appelez Resume pour redémarrer le
  temps.
  @return True si le temps a été mis en pause, False sinon
*}
function TTimerCollection.TryPause: Boolean;
begin
  Result := FActionLock.TryEnter;
end;

{*
  Redémarre le temps
  Resume doit être appelé pour redémarrer le temps après un TryPause réussi.
*}
procedure TTimerCollection.Resume;
begin
  FActionLock.Release;
end;

{*
  [@inheritDoc]
*}
procedure TTimerCollection.BeginState(State: TPersistentState);
begin
  inherited;

  if State * [psReading, psWriting] <> [] then
    FLock.Acquire;
end;

{*
  [@inheritDoc]
*}
procedure TTimerCollection.EndState(State: TPersistentState);
begin
  if State * [psReading, psWriting] <> [] then
    FLock.Release;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TTimerCollection.Notify(Item: TFunLabyPersistent;
  Action: TListNotification);
begin
  inherited;

  if Action = lnAdded then
    FAddedEvent.SetEvent;
end;

{*
  [@inheritDoc]
*}
function TTimerCollection.CreateItem(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := TTimerEntryClass(ItemClass).ReCreate;
end;

{*
  [@inheritDoc]
*}
procedure TTimerCollection.BeforeDestruction;
begin
  inherited;

  FThread.Terminate;
  FAddedEvent.SetEvent;
  if FThread.Suspended then
    FThread.Resume;
  FThread.WaitFor;
end;

{*
  Programme un événement message de notification
  @param Delay        Délai en ms avant de déclencher l'événement
  @param DestObject   Objet destination
  @param MsgID        ID du message
*}
procedure TTimerCollection.ScheduleNotificationMsg(Delay: Cardinal;
  DestObject: TFunLabyComponent; MsgID: Word);
begin
  ScheduleCustom(TNotificationMsgTimerEntry.Create(
    Master.TickCount + Delay, DestObject, MsgID));
end;

{*
  Programme un événement personnalisé
  @param TimerEntry   Entrée de timer à programmer
*}
procedure TTimerCollection.ScheduleCustom(TimerEntry: TTimerEntry);
var
  Index: Integer;
begin
  try
    FLock.Acquire;
    try
      Index := 0;
      while (Index < Count) and
        (TTimerEntry(Items[Index]).TickCount < TimerEntry.TickCount) do
        Inc(Index);

      InsertItem(Index, TimerEntry);
    finally
      FLock.Release;
    end;
  except
    TimerEntry.Free;
    raise;
  end;
end;

{----------------}
{ Classe TMaster }
{----------------}

{*
  Crée une instance de TMaster
*}
constructor TMaster.Create(AEditing: Boolean;
  const AFindResourceCallback: TFindResourceCallback);
begin
  inherited Create;

  FFindResourceCallback := AFindResourceCallback;

  FImagesMaster := TImagesMaster.Create;

  FComponents       := TObjectList.Create(True);
  FPlugins          := TObjectList.Create(False);
  FObjectDefs       := TObjectList.Create(False);
  FFields           := TObjectList.Create(False);
  FEffects          := TObjectList.Create(False);
  FTools            := TObjectList.Create(False);
  FObstacles        := TObjectList.Create(False);
  FSquares          := TObjectList.Create(False);
  FMaps             := TObjectList.Create(False);
  FPosComponents    := TObjectList.Create(False);
  FMobileComponents := TObjectList.Create(False);
  FPlayers          := TObjectList.Create(False);

  FComponentsByID := THashedStringList.Create;
  with TStringList(FComponentsByID) do
  begin
    CaseSensitive := True;
    Sorted := True;
    Duplicates := dupError;
  end;

  FEditing := AEditing;
  FBeginTickCount := Windows.GetTickCount;
  FTerminated := False;

  FOrderedPosComponents := TObjectList.Create(False);
  FOrderedPosComponentsValid := True;

  FTimers := TTimerCollection.Create(Self);
end;

{*
  Détruit l'instance
*}
destructor TMaster.Destroy;
begin
  FTimers.Free;

  FOrderedPosComponents.Free;

  FComponentsByID.Free;

  FPlayers.Free;
  FMobileComponents.Free;
  FPosComponents.Free;
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
  Invalide la liste des PosComponents ordonnés par Z-index
*}
procedure TMaster.InvalidateOrderedPosComponents;
begin
  FOrderedPosComponentsValid := False;
end;

{*
  Compare deux PosComponents par leurs ZIndex
  @param Item1   Premier PosComponent
  @param Item2   Second PosComponent
  @return Relation d'ordre entre Item1 et Item2
*}
function ComparePosComponentsByZIndex(Item1, Item2: Pointer): Integer;
begin
  Result := TPosComponent(Item1).ZIndex - TPosComponent(Item2).ZIndex;
end;

{*
  Assure que la liste des PosComponents ordonnés par Z-index est valide
*}
procedure TMaster.EnsureOrderedPosComponentsValid;
begin
  if FOrderedPosComponentsValid then
    Exit;

  FOrderedPosComponents.Assign(FPosComponents);
  FOrderedPosComponents.Sort(ComparePosComponentsByZIndex);

  FOrderedPosComponentsValid := True;
end;

{*
  Trouve un composant d'une classe particulière par son ID
  @param ID              ID du composant à trouver
  @param RequiredClass   Classe requise pour ce composant
  @return Le composant dont l'ID a été spécifié, ou nil si ID était vide
  @throws EComponentNotFound : Aucun composant ne correspond à l'ID spécifié
  @throws EClassCastException : Le composant n'est pas du type requis
*}
function TMaster.GetComponentAs(const ID: TComponentID;
  RequiredClass: TFunLabyComponentClass = nil): TFunLabyComponent;
var
  Index: Integer;
  ErrorMsg: string;
begin
  if ID = '' then
    Result := nil
  else
  begin
    Index := FComponentsByID.IndexOf(ID);

    if Index >= 0 then
    begin
      Result := TFunLabyComponent(FComponentsByID.Objects[Index]);

      if (RequiredClass <> nil) and (Result <> nil) and
        (not Result.InheritsFrom(RequiredClass)) then
      begin
        if Editing and (ShowDialog(SInvalidComponentTitle,
          Format(SInvalidComponent, [ID])+#10+SRepareComponentError,
          dtError, dbOKCancel) = drOK) then
        begin
          Result := nil;
          if psReading in PersistentState then
            FComponentsByID.Objects[Index] := nil;
        end else
          Result := Result as RequiredClass;
      end;
    end else
    begin
      ErrorMsg := Format(SComponentNotFound, [ID]);

      if Editing and (ShowDialog(SComponentNotFoundTitle,
        ErrorMsg+#10+SRepareComponentError, dtError, dbOKCancel) = drOK) then
      begin
        Result := nil;
        if psReading in PersistentState then
          FComponentsByID.AddObject(ID, nil);
      end else
        raise EComponentNotFound.Create(ErrorMsg);
    end;
  end;
end;

{*
  Tableau des composants indexé par leur ID
  @param ID   ID du composant à trouver
  @return Le composant dont l'ID a été spécifié, ou nil si ID était vide
  @throws EComponentNotFound : Aucun composant ne correspond à l'ID spécifié
*}
function TMaster.GetComponent(const ID: TComponentID): TFunLabyComponent;
begin
  Result := GetComponentAs(ID);
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
    Result := TSquareComponent(GetComponentAs(ID, TSquareComponent));
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
  Result := TPlugin(GetComponentAs(ID, TPlugin));
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
  Result := TObjectDef(GetComponentAs(ID, TObjectDef));
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
  Result := TField(GetComponentAs(ID, TField));

  if (Result = nil) and (ID <> '') and (FieldCount > 0) then
    Result := Fields[0];
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
  Result := TEffect(GetComponentAs(ID, TEffect));
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
  Result := TTool(GetComponentAs(ID, TTool));
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
  Result := TObstacle(GetComponentAs(ID, TObstacle));
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
  AID: TComponentID;
  AName: string;
begin
  if ComponentExists(ID) then
    Result := TSquare(GetComponentAs(ID, TSquare))
  else
  begin
    Result := nil;

    if NberCharInStr(SquareIDDelim, ID) = 3 then
    try
      AField    := Field   [GetXToken(ID, SquareIDDelim, 1)];
      AEffect   := Effect  [GetXToken(ID, SquareIDDelim, 2)];
      ATool     := Tool    [GetXToken(ID, SquareIDDelim, 3)];
      AObstacle := Obstacle[GetXToken(ID, SquareIDDelim, 4)];

      AID := Format(SquareIDFormat,
        [AField.SafeID, AEffect.SafeID, ATool.SafeID, AObstacle.SafeID]);
      if (AID <> ID) and ComponentExists(AID) then
      begin
        Result := TSquare(GetComponentAs(AID, TSquare));
        Exit;
      end;

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

      Result := TSquare.CreateConfig(Self, AID,
        AField, AEffect, ATool, AObstacle);
      Result.Name := AName;
    except
    end;

    if Result = nil then
      raise EComponentNotFound.CreateFmt(SComponentNotFound, [ID]);
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
  Result := TMap(GetComponentAs(ID, TMap));
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
  Result := TPlayer(GetComponentAs(ID, TPlayer));
end;

{*
  Nombre de composants
  @return Nombre de composants
*}
function TMaster.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

{*
  Tableau zero-based des composants
  @param Index   Index du composant
  @return Le composant à la position spécifiée
*}
function TMaster.GetComponents(Index: Integer): TFunLabyComponent;
begin
  Result := TFunLabyComponent(FComponents[Index]);
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
  Nombre de composants avec position
  @return Nombre de composants avec position
*}
function TMaster.GetPosComponentCount: Integer;
begin
  Result := FPosComponents.Count;
end;

{*
  Tableau zero-based des composants avec position
  @param Index   Index du composant
  @return Le composant à la position spécifiée
*}
function TMaster.GetPosComponents(Index: Integer): TPosComponent;
begin
  Result := TPosComponent(FPosComponents[Index]);
end;

{*
  Tableau zero-based des composants avec position ordonnés par ZIndex
  @param Index   Index du composant
  @return Le composant à la position spécifiée
*}
function TMaster.GetOrderedPosComponents(Index: Integer): TPosComponent;
begin
  EnsureOrderedPosComponentsValid;
  Result := TPosComponent(FOrderedPosComponents[Index]);
end;

{*
  Nombre de composants mobiles (autonomes)
  @return Nombre de composants mobiles (autonomes)
*}
function TMaster.GetMobileComponentCount: Integer;
begin
  Result := FMobileComponents.Count;
end;

{*
  Tableau zero-based des composants mobiles (autonomes)
  @param Index   Index du composant
  @return Le composant à la position spécifiée
*}
function TMaster.GetMobileComponents(Index: Integer): TMobileComponent;
begin
  Result := TMobileComponent(FMobileComponents[Index]);
end;

{*
  Tick count de la partie
  @return Tick count de la partie
*}
function TMaster.GetTickCount: Cardinal;
begin
  if Paused then
    Result := FPauseTickCount
  else
    Result := Windows.GetTickCount - FBeginTickCount;
end;

{*
  Modifie le tick count de la partie
  @param Value   Nouveau tick count
*}
procedure TMaster.SetTickCount(Value: Cardinal);
begin
  FBeginTickCount := Windows.GetTickCount - Value;
end;

{*
  Ajoute un composant
  @param Component   Le composant à ajouter
*}
procedure TMaster.AddComponent(Component: TFunLabyComponent);
begin
  FComponents.Add(Component);

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
  else if Component is TPosComponent then
  begin
    FPosComponents.Add(Component);
    if Component is TMobileComponent then
      FMobileComponents.Add(Component);
    if Component is TPlayer then
      FPlayers.Add(Component);
    InvalidateOrderedPosComponents;
  end;

  if Component.ID <> '' then
    FComponentsByID.AddObject(Component.ID, Component);
end;

{*
  Retire un composant
  @param Component   Le composant à retirer
*}
procedure TMaster.RemoveComponent(Component: TFunLabyComponent);
var
  Index: Integer;
begin
  if psDestroying in PersistentState then
    Exit;

  Index := FComponentsByID.IndexOfObject(Component);
  if Index >= 0 then
    FComponentsByID.Delete(Index);

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
  else if Component is TPosComponent then
  begin
    FPosComponents.Remove(Component);
    if Component is TMobileComponent then
      FMobileComponents.Remove(Component);
    if Component is TPlayer then
      FPlayers.Remove(Component);
    InvalidateOrderedPosComponents;
  end;

  FComponents.Extract(Component);
end;

{*
  Notification que l'ID d'un composant a été modifié
  @param Component   Le composant dont l'ID a été modifié
*}
procedure TMaster.ComponentIDChanged(Component: TFunLabyComponent);
var
  Index: Integer;
begin
  Index := FComponentsByID.IndexOfObject(Component);
  if Index >= 0 then
  begin
    FComponentsByID.Delete(Index);
    FComponentsByID.AddObject(Component.ID, Component);
  end;
end;

{*
  Met fin à la partie
*}
procedure TMaster.Terminate;
begin
  FTerminated := True;
end;

{*
  [@inheritDoc]
*}
procedure TMaster.DefineProperties(Filer: TFunLabyFiler);
var
  I: Integer;
  Component: TFunLabyComponent;
begin
  inherited;

  Filer.DefineProcProperty('TickCount', TypeInfo(Cardinal),
    @TMaster.GetTickCount, @TMaster.SetTickCount, not Editing);

  Filer.DefineFieldProperty('Terminated', TypeInfo(Boolean),
    @FTerminated, Terminated);

  for I := 0 to ComponentCount-1 do
  begin
    Component := Components[I];

    if not Component.Transient then
      Filer.DefinePersistent(Component.ID, Component);
  end;

  Filer.DefinePersistent('Timers', Timers);
end;

{*
  [@inheritDoc]
*}
procedure TMaster.EndState(State: TPersistentState);
var
  I, J: Integer;
begin
  inherited;

  if psReading in State then
  begin
    for I := FComponentsByID.Count-1 downto 0 do
    begin
      if FComponentsByID.Objects[I] <> nil then
        Continue;

      for J := 0 to ComponentCount-1 do
      begin
        if Components[J].ID = FComponentsByID[I] then
        begin
          FComponentsByID.Objects[I] := Components[J];
          Break;
        end;
      end;

      if FComponentsByID.Objects[I] = nil then
        FComponentsByID.Delete(I);
    end;
  end;

  if (psReading in State) and (not Editing) then
    Timers.Start;
end;

{*
  [@inheritDoc]
*}
procedure TMaster.BeforeDestruction;
begin
  inherited;

  if Paused then
    Resume;
end;

{*
  [@inheritDoc]
*}
procedure TMaster.StoreDefaults;
begin
  inherited;
end;

{*
  Teste si un composant existe
  @param ID   ID de composant à tester
  @return True si un composant avec cet ID existe déjà, False sinon
*}
function TMaster.ComponentExists(const ID: TComponentID): Boolean;
begin
  Result := FComponentsByID.IndexOf(ID) >= 0;
end;

{*
  Vérifie qu'un ID de composant est valide
  @param ID   ID de composant à tester
*}
procedure TMaster.CheckComponentID(const ID: TComponentID);
begin
  if not IsValidIdent(ID) then
    raise EInvalidID.CreateFmt(SInvalidID, [ID]);

  if ComponentExists(ID) then
    raise EInvalidID.CreateFmt(SDuplicateID, [ID]);
end;

{*
  Cherche une ressource d'après son href et son type
  @param HRef   HRef de la ressource
  @param Kind   Type de ressource recherchée
  @return Nom complet du fichier pour cette ressource
  @throws EResourceNotFoundException La ressource n'a pas pu être trouvée
*}
function TMaster.FindResource(const HRef: string;
  Kind: TResourceKind): TFileName;
begin
  Result := FFindResourceCallback(HRef, Kind);
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

{*
  Obtient une case à partir de ses composantes
  @param Field      Terrain
  @param Effect     Effet
  @param Tool       Outil
  @param Obstacle   Obstacle
  @return Case avec avec les composantes spécifiées
*}
function TMaster.SquareByComps(Field: TField; Effect: TEffect = nil;
  Tool: TTool = nil; Obstacle: TObstacle = nil): TSquare;
begin
  Result := Square[Format(SquareIDFormat,
    [Field.SafeID, Effect.SafeID, Tool.SafeID, Obstacle.SafeID])];
end;

{*
  Enregistre tous les composants qui doivent être mis dans la palette
  @param RegisterComponent   Méthode call-back pour l'enregistrement
*}
procedure TMaster.RegisterComponents(RegisterComponent: TRegisterComponentProc);
var
  I: Integer;
begin
  for I := 0 to ComponentCount-1 do
    if Components[I].IsDesignable then
      RegisterComponent(Components[I]);
end;

{*
  Crée un composant additionnel (non créé de base par une unité utilisée)
  @param ComponentClass   Class du composant à créer
  @param ID               ID du composant à créer
  @return Composant créé
*}
function TMaster.CreateAdditionnalComponent(
  ComponentClass: TFunLabyComponentClass;
  const ID: TComponentID): TFunLabyComponent;
begin
  Result := ComponentClass.Create(Self, ID);
  Result.FIsAdditionnal := True;
end;

{*
  Essaye de mettre le jeu en pause
  Si TryPause renvoie True, vous devez appelez Resume pour redémarrer le jeu.
  @return True si le jeu a été mis en pause, False sinon
*}
function TMaster.TryPause: Boolean;
var
  DoneMobCount: Integer;
  I: Integer;
begin
  DoneMobCount := 0;

  Result := Timers.TryPause;
  if not Result then
    Exit;

  try
    for I := 0 to MobileComponentCount-1 do
    begin
      Result := MobileComponents[I].TryPause;
      if not Result then
        Break;

      Inc(DoneMobCount);
    end;

    if Result then
    begin
      FPauseTickCount := TickCount;
      FPaused := True;
    end;
  finally
    if not Result then
    begin
      for I := 0 to DoneMobCount-1 do
        MobileComponents[I].Resume;
      Timers.Resume;
    end;
  end;
end;

{*
  Redémarre le jeu
  Resume doit être appelé pour redémarrer le jeu après un TryPause réussi.
*}
procedure TMaster.Resume;
var
  I: Integer;
begin
  for I := 0 to MobileComponentCount-1 do
    MobileComponents[I].Resume;
  Timers.Resume;

  SetTickCount(FPauseTickCount);
  FPaused := False;
end;

initialization
  Randomize;

  CreateFunLabyEncoding;

  FunLabyRegisterClass(TLabyrinthPlayerMode);
  FunLabyRegisterClasses([TTimerEntry, TNotificationMsgTimerEntry]);

  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    fFunLabyAppData :=
      ReadString('Directories', 'AppData', Dir); {don't localize}

    fSquaresDir := fFunLabyAppData + fSquaresDir;
    fSoundsDir := fFunLabyAppData + fSoundsDir;
    fUnitsDir := fFunLabyAppData + fUnitsDir;
    fLabyrinthsDir := fFunLabyAppData + fLabyrinthsDir;
    fSaveguardsDir := fFunLabyAppData + fSaveguardsDir;
    fScreenshotsDir := fFunLabyAppData + fScreenshotsDir;

    fEditPluginDir := Dir + fEditPluginDir;
  finally
    Free;
  end;
finalization
  FreeAndNil(FunLabyRegisteredClasses);
end.

