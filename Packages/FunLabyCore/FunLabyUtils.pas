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
  HalfSquareSize = 15; /// Moiti� de la taille d'une case
  MinViewSize = 1;     /// Taille minimale d'une vue

  /// Couleur de transparence pour les fichiers .bmp
  clBmpTransparent32 = FunLabyGraphics.clBmpTransparent32;

  /// Couleur transparente
  clTransparent32 = FunLabyGraphics.clTransparent32;

  msgShowMessage = $01; /// Message pour afficher un message au joueur
  msgGameStarted = $02; /// Message envoy� lorsque le jeu commence
  msgPressKey = $03;    /// Message envoy� au joueur � l'appui sur une touche
  msgSquareEvent = $04; /// Message envoy� lors d'un �v�nement de case

  /// Message envoy� aux composants d'une case lorsque celle-ci est �dit�e
  msgEditMapSquare = $05;

  /// Message demandant l'ex�cution d'une m�thode au sein des actions
  msgRunMethod = $06;

  SquareIDDelim = '-';            /// D�limiteur des parties d'un ID de case
  SquareIDFormat = '%s-%s-%s-%s'; /// Format d'un ID de case

  /// Temporisation par d�faut
  DefaultTemporization = 500;

  /// Couleur par d�faut d'un joueur
  DefaultPlayerColor = clBlue32;

  /// Taille de bordure de vue par d�faut
  DefaultViewBorderSize = 1;

  /// Extension d'image pr�f�r�e
  PreferredImageExtension = 'png';

  /// Extension d'un peintre
  PainterExtension = 'pnt';

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type repr�sentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Ensemble de directions
  TDirections = set of TDirection;

  /// Type repr�sentant une action
  TPlayerAction = type string;

  /// �tat de victoire/d�faite d'un joueur
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

  /// G�n�r�e si un composant recherch� n'est pas trouv�
  EComponentNotFound = class(EFunLabyException);

  /// D�clench�e lorsqu'un ID est invalide
  EInvalidID = class(EFunLabyException);

  /// G�n�r�e si une commande invalide est ex�cut�e
  EInvalidCommand = class(EFunLabyException);

  /// G�n�r�e si une commande n'est pas support�e
  EUnsupportedCommand = class(EFunLabyException);

  /// G�n�r�e en cas de mauvaise d�finition d'une case
  EBadSquareDefException = class(EFunLabyException);

  /// G�n�r�e si une ressource n'a pas pu �tre trouv�e
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
    Position qualifi�e, compos�e d'une carte et d'une position sur la carte
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
    Base pour les messages envoy�s � des joueurs
    @author sjrd
    @version 5.0
  *}
  TPlayerMessage = record
    MsgID: Word;       /// ID du message
    Handled: Boolean;  /// Indique si le message a �t� g�r�
    Reserved: Byte;    /// R�serv�
    Player: TPlayer;   /// Joueur concern�
  end;

  {*
    Structure du message pour afficher un message au joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerShowMsgMessage = record
    MsgID: Word;               /// ID du message
    Handled: Boolean;          /// Indique si le message a �t� g�r�
    Reserved: Byte;            /// R�serv�
    Player: TPlayer;           /// Joueur concern�
    Text: string;              /// Texte � afficher
    Answers: TStringDynArray;  /// R�ponses possibles (peut �tre vide)
    Selected: Integer;         /// Index de la r�ponse choisie par le joueur
    ShowOnlySelected: Boolean; /// Si True n'affiche que l'�l�ment s�lectionn�
  end;

  /// Alias de TPlayerShowMsgMessage (FunDelphi codegen)
  TShowMessageMessage = TPlayerShowMsgMessage;

  /// Structure du message envoy� au d�marrage du jeu
  TGameStartedMessage = TPlayerMessage;

  {*
    Structure du message d'appui sur une touche pour le joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerPressKeyMessage = record
    MsgID: Word;        /// ID du message
    Handled: Boolean;   /// Indique si le message a �t� g�r�
    Reserved: Byte;     /// R�serv�
    Player: TPlayer;    /// Joueur concern�
    Key: Word;          /// Touche appuy�e
    Shift: TShiftState; /// �tat des touches sp�ciales
  end;

  {*
    Type d'�v�nement de case
  *}
  TSquareEventKind = (
    sekEntering, sekExiting, sekEntered, sekExited, sekExecute, sekPushing
  );

  /// Ensemble de TSquareEventKind
  TSquareEventKinds = set of TSquareEventKind;

  {*
    Message envoy� aux composants avec position lors d'un �v�nement de case
    @author sjrd
    @version 5.0
  *}
  TSquareEventMessage = record
    MsgID: Word;            /// ID du message
    Kind: TSquareEventKind; /// Type d'�v�nement
    Reserved: Byte;         /// R�serv�
    Context: TMoveContext;  /// Contexte du mouvement
  end;

  {*
    Phase d'�dition d'une case de la carte
    - espAdding : Un composant va �tre ajout�
    - espAdd : Ce composant-ci doit �tre ajout�
    - espAdded : Un composant a �t� ajout�
    - espRemoving : Un composant va �tre retir�
    - espRemove : Ce composant-ci doit �tre retir�
    - espRemoved : Un composant a �t� retir�
  *}
  TEditMapSquarePhase = (
    espAdding, espAdd, espAdded, espRemoving, espRemove, espRemoved
  );

  {*
    Flag de traitement d'un message TEditMapSquareMessage
    - esfCancel : Si pr�sent en sortie, l'�diteur ne fait plus son action par
      d�faut et ne consid�re pas que la carte a �t� modifi�e (n'est pas pris
      en compte en phase Added ou Removed)
    - esfHandled : Si pr�sent en sortie, l'�diteur ne fait plus son action
      par d�faut (pris en compte uniquement pendant la phase Add)
  *}
  TEditMapSquareFlag = (
    esfCancel, esfHandled
  );

  /// Flags de traitement d'un message TEditMapSquareMessage
  TEditMapSquareFlags = set of TEditMapSquareFlag;

  {*
    Structure du message d'�dition d'une case dans une carte
    @author sjrd
    @version 5.0
  *}
  TEditMapSquareMessage = record
    MsgID: Word;                  /// ID du message
    Phase: TEditMapSquarePhase;   /// Phase de modification d'une case
    Flags: TEditMapSquareFlags;   /// Flags du message
    Component: TFunLabyComponent; /// Composant qui va �tre plac�/retir�
    QPos: TQualifiedPos;          /// Position qualifi�e de la case �dit�e
  end;

  {*
    Structure du message d'appel de m�thode
    @author sjrd
    @version 5.0
  *}
  TRunMethodMessage = record
    MsgID: Word;                 /// ID du message
    Handled: Boolean;            /// Indique si le message a �t� g�r�
    Reserved: Byte;              /// R�serv�
    Component: TMobileComponent; /// Composant concern�
    Method: TThreadMethod;       /// M�thode � appeler
  end;

  {*
    Type de m�thode call-back pour l'enregistrement d'un composant
    @param Component   Composant � enregistrer
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
    FX: Integer;        /// Absisce o� dessiner
    FY: Integer;        /// Ordonn�e o� dessiner
    FSquareRect: TRect; /// Rectangle de la case � dessiner

    FIsNowhere: Boolean;  /// Indique si le dessin est fait "nulle part"
    FQPos: TQualifiedPos; /// Position dessin�e

    /// Contexte de dessin de la vue (si applicable)
    FDrawViewContext: TDrawViewContext;
    FTickCount: Cardinal; /// Tick count pour ce contexte

    FPlayer: TPlayer; /// Joueur � dessiner (si applicable)

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
    FPlayer: TPlayer;         /// Joueur dont la vue est affich�e

    FBitmap: TBitmap32; /// Bitmap cible
    FViewRect: TRect;   /// Rectangle de la vue � dessiner

    FUseZone: Boolean; /// Indique si ce contexte utilise une zone de carte
    FMap: TMap;        /// Carte dont dessiner une zone
    FFloor: Integer;   /// �tage de la zone � dessiner
    FZone: TRect;      /// Zone (�tendue aux bordures) � dessiner
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
    Contexte d'�v�nement de touche
    @author sjrd
    @version 5.0
  *}
  TKeyEventContext = class(TObject)
  private
    FPlayer: TPlayer; /// Joueur qui a press� une touche

    FKey: Word;          /// Touche press�e
    FShift: TShiftState; /// �tat des touches sp�ciales

    FHandled: Boolean; /// Indique si l'�v�nement a �t� g�r�
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
    FPlayer: TPlayer; /// Joueur qui se d�place

    FSrcQPos: TQualifiedPos;  /// Source qualifi�e
    FDestQPos: TQualifiedPos; /// Destination qualifi�e
    FQPos: TQualifiedPos;     /// Position qualifi�e courante

    FOldDirection: TDirection; /// Ancienne direction du joueur
    FKeyPressed: Boolean;      /// True si une touche a �t� press�e
    FKey: Word;                /// Touche press�e (si KeyPressed)
    FShift: TShiftState;       /// �tat des touches sp�ciales (si KeyPressed)

    FCancelled: Boolean;  /// True si le d�placement a �t� annul�
    FGoOnMoving: Boolean; /// True s'il faut r�it�rer le d�placement
    FHooked: Boolean;     /// True si intercept� par un TPosComponent

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
    M�thode avec contexte de d�placement
    @param Context   Contexte du d�placement
  *}
  TMoveContextMethod = procedure(Context: TMoveContext) of object;

  {*
    �l�ment de l'�tat d'un objet persistant
    - psCreating : l'objet est en train d'�tre construit
    - psDestroying : l'objet est en train d'�tre d�truit
    - psReading : l'objet est en train d'�tre lu depuis un filer
    - psWriting : l'objet est en train d'�tre �crit dans un filer
  *}
  TPersistentStateItem = (psCreating, psDestroying, psReading, psWriting);

  /// �tat d'un objet persistant
  TPersistentState = set of TPersistentStateItem;

  {$M+}

  {*
    Objet persistent FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyPersistent = class(TObject)
  private
    FPersistentState: TPersistentState; /// �tat
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
    Classe de base pour les objets persistents devant impl�menter un interface
    @author sjrd
    @version 5.0
  *}
  TInterfacedFunLabyPersistent = class(TFunLabyPersistent, IInterface)
  protected
    FRefCount: Integer; /// Compteur de r�f�rences

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
    FItems: TObjectList; /// �l�ments de la collection

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
    Classe de base pour les objets lecteurs et �crivains FunLabyrinthe
    @author sjrd
    @version 5.0
  *}
  TFunLabyFiler = class(TObject)
  private
    FOwner: TFunLabyFiler;         /// Filer propri�taire (peut �tre nil)
    FMaster: TMaster;              /// Ma�tre FunLabyrinthe
    FInstance: TFunLabyPersistent; /// Instance trait�e par ce filer
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
    Classe de base pour les �crivains FunLabyrinthe
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
    Pseudo-filer servant � enregistrer r�cursivement les propri�t�s par d�faut
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
    G�re le chargement des images d'apr�s leur nom
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
    TPainter enregistre une liste d'images par leur noms et propose une m�thode
    pour les dessiner les unes sur les autres, par transparence.
    @author sjrd
    @version 5.0
  *}
  TPainter = class(TFunLabyPersistent)
  private
    FMaster: TImagesMaster; /// Ma�tre d'images

    FDescription: TStrings;        /// Description de l'image peinte
    FDefaultDescription: TStrings; /// Description par d�faut
    FCachedImgIndex: Integer;      /// Index de l'image dans le ma�tre d'images
    FCachedImg: TBitmap32;         /// Copie cache de l'image r�sultante

    FStaticDraw: Boolean; /// Indique si ce peintre dessine une image statique
    FNeedCache: Boolean;  /// Indique si une image cache est n�cessaire
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
    Donn�es d'un composant li�es � un joueur
    @author sjrd
    @version 5.0
  *}
  TPlayerData = class(TFunLabyPersistent)
  private
    FComponent: TFunLabyComponent; /// Composant propri�taire
    FPlayer: TPlayer;              /// Joueur li�
  public
    constructor Create(AComponent: TFunLabyComponent;
      APlayer: TPlayer); virtual;

    property Component: TFunLabyComponent read FComponent;
    property Player: TPlayer read FPlayer;
  end;

  /// Classe de TPlayerData
  TPlayerDataClass = class of TPlayerData;

  /// Alias de type utilis� par le codegen FunDelphi
  TFunLabyComponentPlayerData = TPlayerData;

  {*
    Classe de base pour les composants de FunLabyrinthe
    TFunLabyComponent est la classe de base pour tous les composants de
    FunLabyrinthe. Elle fournit des propri�t�s et des m�thodes pour rep�rer le
    ma�tre FunLabyrinthe et pour identifier le composant.
    @author sjrd
    @version 5.0
  *}
  TFunLabyComponent = class(TFunLabyPersistent)
  private
    FMaster: TMaster;        /// Ma�tre FunLabyrinthe
    FID: TComponentID;       /// ID du composant
    FIsAdditionnal: Boolean; /// Indique si ce composant est additionnel
    FIconPainter: TPainter;  /// Peintre de l'ic�ne

    {*
      Stocke une valeur enti�re dans un composant
      Tag n'a pas de signification pr�d�finie. Elle est fournie pour les besoins
      du d�veloppeur.
    *}
    FTag: Integer;

    FPlayerData: TBucketItemArray; /// Donn�es par joueur

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
    Classe de base pour les composants devant �tre affich�s
    TVisualComponent �tend la classe TFunLabyComponent pour lui ajouter un
    traitement standard et simple de nommage et de dessin.
    @author sjrd
    @version 5.0
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName: string;                 /// Nom du composant
    FDefaultName: string;          /// Name par d�faut
    FPainter: TPainter;            /// Peintre
    FEditVisualTag: string;        /// Tag visuel visible uniquement � l'�dition
    FDefaultEditVisualTag: string; /// EditVisualTag par d�faut

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
    Pseudo-composant qui permet de cr�er de nouveaux composants
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
    Un plug-in peut agir de plusieurs fa�ons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Emp�cher le d�placement du joueur et r�agir � son d�placement effectif ;
    - Modifier la vue compl�te du joueur ;
    - R�agir � l'appui sur un touche ;
    - Indiquer au joueur qu'il a la capacit� de faire certaines actions.
    @author sjrd
    @version 5.0
  *}
  TPlugin = class(TFunLabyComponent)
  private
    FPainterBefore: TPainter; /// Peintre par d�faut sous le joueur
    FPainterAfter: TPainter;  /// Peintre par d�faut sur le joueur
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
    Donn�es li�es � un joueur pour une d�finition d'objet
    @author sjrd
    @version 5.0
  *}
  TObjectDefPlayerData = class(TPlayerData)
  private
    FCount: Integer; /// Nombre d'objets poss�d�s par le joueur
  published
    property Count: Integer read FCount write FCount default 0;
  end;

  {*
    Classe de base pour les d�finitions d'objets
    TObjectDef est la classe de base pour les d�finitions d'objets que poss�de
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
    TField est la classe de base pour la cr�ation de terrains. Les terrains
    sont la premi�re composante d'une case.
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
    TEffect est la classe de base pour la cr�ation d'effets de case. Les effets
    sont la deuxi�me composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TEffect = class(TSquareComponent)
  private
    FEnabled: Boolean; /// Indique si cet effet est activ�
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
    TTool est la classe de base pour la cr�ation d'outils. Les outils sont la
    troisi�me composante d'une case.
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
    TObstacle est la classe de base pour la cr�ation d'obstacles. Les obstacles
    sont la quatri�me composante d'une case.
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
    Repr�sente une case du jeu
    TSquare repr�sente une case du jeu. Une case poss�de quatre composantes : le
    terrain, l'effet, l'outil et l'obstacle. Le terrain est obligatoire, tandis
    que les trois autres composantes sont optionnelles.
    @author sjrd
    @version 5.0
  *}
  TSquare = class(TSquareComponent)
  private
    FCategory: string; /// Cat�gorie

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
    Repr�sente la carte du jeu
    TMap g�re et repr�sente la carte du jeu. Elle offre des propri�t�s et
    m�thodes pour lire et modifier cette carte.
    @author sjrd
    @version 5.0
  *}
  TMap = class(TFunLabyComponent)
  private
    FDimensions: T3DPoint;   /// Dimensions de la carte (en cases)
    FZoneWidth: Integer;     /// Largeur d'une zone de la carte
    FZoneHeight: Integer;    /// Hauteur d'une zone de la carte
    FMap: TSquareDynArray;   /// Carte stock�e de fa�on lin�aire
    FOutsideOffset: Integer; /// Offset de d�part de l'ext�rieur

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
    Classe repr�sentant un composant qui a une position sur une carte
    TPosComponent est la classe de base pour les composants qui doivent avoir
    une position sur la carte. Au contraire des composants de case, qui peuvent
    �tre plac�s � plusieurs endroits, les instances de TPosComponent sont
    "uniques" en ce qu'elles ne sont positionn�es qu'� un seul endroit � la
    fois.
    Les descendants de TPosComponent peuvent aussi intercepter les �v�nements
    de la case o� il se trouve. Ils doivent pour ce faire s�lectionner les
    �v�nements � intercepter via la m�thode SetWantedSquareEvents dans leur
    constructeur. Ils sont alors envoy� au composant par un message
    msgSquareEvent de type TSquareEventMessage.
    @author sjrd
    @version 5.0
  *}
  TPosComponent = class(TVisualComponent)
  private
    FQPos: TQualifiedPos;   /// Position qualifi�e
    FDirection: TDirection; /// Direction

    /// Types d'�v�nements de case que ce composant d�sire attraper
    FWantedSquareEvents: TSquareEventKinds;

    /// Indique si le composant d�sire attraper les messages de la case
    FWantMessages: Boolean;

    FZIndex: Integer;        /// Z-index
    FDefaultZIndex: Integer; /// Z-index par d�faut

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
    TSquareModifier est une classe de base abstraite facilitant la cr�ation
    d'un composant qui modifie le comportement de la case o� il se trouve.
    TSquareModifier se charge d'appeler SetWantedSquareEvents, et de dispatcher
    les �v�nements de message
    @author sjrd
    @version 5.0
  *}
  TSquareModifier = class(TPosComponent)
  private
    /// Gestionnaires des �v�nements
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
    Plugin li� � un v�hicule
    @author sjrd
    @version 5.0
  *}
  TVehiclePlugin = class(TPlugin)
  private
    FVehicle: TVehicle; /// V�hicule li� � ce plugin
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
    V�hicule
    @author sjrd
    @version 5.0
  *}
  TVehicle = class(TSquareModifier)
  private
    FPlugin: TVehiclePlugin; /// Plugin li�

    FDirPainters: array[TDirection] of TPainter; /// Peintres par direction

    FController: TPlayer; /// Joueur qui contr�le actuellement ce v�hicule

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
    se d�placer de fa�on autonome, ou avoir un quelconque comportement
    autonome.
    Chaque instance de TMobileComponent a son propre thread d'ex�cution de ses
    actions.
    @author sjrd
    @version 5.0
  *}
  TMobileComponent = class(TPosComponent)
  private
    FActionLock: TCriticalSection;   /// Verrou pour les actions
    FInActionLock: TCriticalSection; /// Verrou pour l'int�rieur des actions
    FActionCoroutine: TCoroutine;    /// Coroutine d'ex�cution des actions
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
    � tout moment, chaque joueur est dans un mode donn� qui d�termine comment
    est affich�e sa vue, et comment il est contr�l�. Un joueur a un et un seul
    mode principal, mais ses plug-in peuvent modifier les comportements de son
    mode principal.
    Vous devez utiliser la class TPlayerMode comme classe de base pour toute
    impl�mentation de IPlayerMode.
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
      Joueur li� � ce mode
      @return Joueur li� � ce mode
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
    Squelette d'impl�mentation de IPlayerMode.
    @author sjrd
    @version 5.0
  *}
  TPlayerMode = class(TInterfacedFunLabyPersistent, IPlayerMode)
  private
    FMaster: TMaster; /// Ma�tre FunLabyrinthe
    FPlayer: TPlayer; /// Joueur li� � ce mode
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
    Classe repr�sentant un joueur
    TPlayer repr�sente un joueur. Elle poss�de de nombreuses propri�t�s et
    m�thodes permettant d'afficher le joueur, de le d�placer, de lui greffer
    des plug-in, etc.
    @author sjrd
    @version 5.0
  *}
  TPlayer = class(TMobileComponent)
  private
    FMode: IPlayerMode;         /// Mode principal
    FModeStack: IInterfaceList; /// Pile des modes sauvegard�s
    FShowCounter: Integer;      /// Compteur de visibilit�
    FColor: TColor32;           /// Couleur
    FViewBorderSize: Integer;   /// Taille de la bordure de la vue
    FPlugins: TObjectList;      /// Liste des plug-in
    FAttributes: TStrings;      /// Liste des attributs
    FPlayState: TPlayState;     /// �tat de victoire/d�faite
    FFoundObjects: TObjectList; /// Objets trouv�s (dans l'ordre)

    FDrawMode: TPlayerDrawMode;                  /// Mode de dessin
    FColoredPainterCache: TBitmap32;             /// Cache du peintre color�
    FDirPainters: array[TDirection] of TPainter; /// Peintres par direction

    FDefaultTemporization: Cardinal; /// Temporisation par d�faut

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
    Classe de base pour les timers (�v�nements � d�clencher � un moment donn�)
    N'utilisez pas directement TTimerEntry, mais utilisez les m�thodes de
    TTimerList pour programmer des �v�nements � d�clencher.
    @author sjrd
    @version 5.0
  *}
  TTimerEntry = class(TFunLabyPersistent)
  private
    FTickCount: Cardinal; /// Tick-count auquel d�clencher cet �v�nement

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
    Timer qui envoie un message de notification � un objet
    Un message de notification est un message sans aucun param�tre en dehors de
    l'ID du message. Si le destinataire est un TPlayer, le message envoy� est
    quand m�me garantit de type TPlayerMessage.
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
    Collection de timers (�v�nements d�clench�s apr�s un temps donn�)
    @author sjrd
    @version 5.0
  *}
  TTimerCollection = class(TFunLabyCollection)
  private
    FMaster: TMaster;              /// Ma�tre FunLabyrinthe
    FThread: TMethodThread;        /// Thread de d�clenchement des timers
    FAddedEvent: TEvent;           /// �v�nement d�clench� � l'ajout d'un timer
    FLock: TCriticalSection;       /// Verrou d'acc�s � la liste des timers
    FActionLock: TCriticalSection; /// Verrou d'ex�cution des actions

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
    Ma�tre FunLabyrinthe
    TMaster g�re les diff�rents composants de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TMaster = class(TFunLabyPersistent)
  private
    /// Callback pour trouver une ressource
    FFindResourceCallback: TFindResourceCallback;

    FImagesMaster: TImagesMaster;   /// Ma�tre d'images
    FComponents: TObjectList;       /// Liste de tous les composants
    FPlugins: TObjectList;          /// Liste des plug-in
    FObjectDefs: TObjectList;       /// Liste des d�finitions d'objet
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

    FEditing: Boolean;            /// Indique si on est en mode �dition
    FBeginTickCount: Cardinal;    /// Tick count syst�me au lancement
    FPaused: Boolean;             /// Indique si le jeu est en pause
    FPauseTickCount: Cardinal;    /// Tick count au moment de la pause
    FTerminated: Boolean;         /// Indique si la partie est termin�e

    FOrderedPosComponents: TObjectList;  /// PosComponents ordonn�s par Z-index
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

  /// Position qualifi�e nulle
  NoQPos: TQualifiedPos = (Map: nil; Position: (X: 0; Y: 0; Z: 0));

  /// Rectangle d'une case � l'origine
  BaseSquareRect: TRect = (
    Left: 0; Top: 0; Right: SquareSize; Bottom: SquareSize
  );

  /// Application d'une direction vers la direction oppos�e
  NegDir: array[TDirection] of TDirection = (
    diNone, diSouth, diWest, diNorth, diEast
  );

  /// Application d'une direction vers la direction � sa droite
  RightDir: array[TDirection] of TDirection = (
    diNone, diEast, diSouth, diWest, diNorth
  );

  /// Application d'une direction vers la direction � sa gauche
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
  /// Dossier des unit�s
  fUnitsDir: string = 'Units\';
  /// Dossier des fichiers labyrinthe
  fLabyrinthsDir: string = 'Labyrinths\';
  /// Dossier des fichiers sauvegarde
  fSaveguardsDir: string = 'Saveguards\';
  /// Dossier des screenshots
  fScreenshotsDir: string = 'Screenshots\';
  /// Dossier des plug-in de l'�diteur
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
  /// Z-index par d�faut pour les joueurs
  DefaultPlayerZIndex = 1024;

type
  TFunLabyEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

  {*
    Peintre d'un TPlayer
    TPlayer utilise un peintre sp�cialis� pour recalculer son image
    FColoredPainterCache � chaque fois que l'image du peintre change.
    @author sjrd
    @version 5.0
  *}
  TPlayerPainter = class(TPainter)
  private
    FPlayer: TPlayer; /// Joueur propri�taire

    procedure DescriptionChange(Sender: TObject);
  public
    constructor Create(AMaster: TImagesMaster; APlayer: TPlayer);
  end;

const
  /// Code de format d'un flux carte (TMap) (correspond � '.flm')
  MapStreamFormatCode: Longint = $6D6C662E;

  /// Version courante du format d'un flux carte (TMap)
  MapStreamVersion = 1;

  /// Fichier de l'image du joueur
  fPlayer = 'Pawns/Player';

var
  FFunLabyEncoding: TEncoding = nil;
  FunLabyRegisteredClasses: TStrings = nil;

{*
  Affiche une bo�te de dialogue � propos de FunLabyrinthe
  @param Icon   Ic�ne du programme
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
  Cr�e la variable contenant l'encodage de FunLabyrinthe
*}
procedure CreateFunLabyEncoding;
begin
  FFunLabyEncoding := TFunLabyEncoding.Create;
end;

{*
  Encodage des fichiers FunLabyrinthe
  Il s'agit d'un encodage UTF-8 sans pr�ambule. Cet encodage devrait �tre
  utilis� par la plupart des op�rations sur les fichiers textes.
  @return Encodage des fichiers FunLabyrinthe
*}
function FunLabyEncoding: TEncoding;
begin
  Result := FFunLabyEncoding;
end;

{*
  Renvoie le point situ� derri�re un point dans la direction indiqu�e
  @param Src   Point origine
  @param Dir   Direction dans laquelle on va
  @return Le point situ� derri�re le point Src dans la direction Dir
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
  Renvoie le point situ� devant un point depuis la direction indiqu�e
  @param Src   Point origine
  @param Dir   Direction depuis laquelle on vient
  @return Le point situ� devant le point Src selon la direction Dir
*}
function PointBefore(const Src: T3DPoint; Dir: TDirection): T3DPoint;
begin
  Result := PointBehind(Src, NegDir[Dir]);
end;

{*
  Cr�e un bitmap de case vide (enti�rement transparent)
  @return Bitmap cr��
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
  Cr�e un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord sup�rieur du rectangle
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
  @param Bitmap   Bitmap � traiter
  @param Rect     Rectangle � effacer
*}
procedure EmptyRect(Bitmap: TBitmap32; Rect: TRect);
begin
  Bitmap.FillRectS(Rect, clTransparent32);
end;

{*
  Efface un rectangle de case sur un canevas (avec du transparent)
  @param Bitmap   Bitmap � traiter
  @param X        Bord gauche du rectangle
  @param Y        Bord sup�rieur du rectangle
*}
procedure EmptySquareRect(Bitmap: TBitmap32; X: Integer = 0; Y: Integer = 0);
begin
  EmptyRect(Bitmap, SquareRect(X, Y));
end;

{*
  Dessine un bitmap 32 sur un canevas VCL
  @param Canvas            Canevas cible
  @param DestRect          Rectangle dans lequel dessiner le bitmap
  @param Bitmap            Bitmap � dessiner
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
  Teste si deux rectangles sont �gaux
  @param Left    Rectangle de gauche
  @param Right   Rectangle de droite
  @return True si les rectangles sont �gaux, False sinon
*}
function SameRect(const Left, Right: TRect): Boolean;
begin
  Result := (Left.Left = Right.Left) and (Left.Top = Right.Top) and
    (Left.Right = Right.Right) and (Left.Bottom = Right.Bottom);
end;

{*
  Teste si deux positions qualifi�es sont �gales
  @param Left    Position de gauche
  @param Right   Position de droite
  @return True si les positions sont �gales, False sinon
*}
function SameQPos(const Left, Right: TQualifiedPos): Boolean;
begin
  Result := (Left.Map = Right.Map) and
    Same3DPoint(Left.Position, Right.Position);
end;

{*
  D�termine si une position qualifi�e est nulle
  @param QPos   Position � tester
  @return True si la position qualifi�e QPos est nulle, False sinon
*}
function IsNoQPos(const QPos: TQualifiedPos): Boolean;
begin
  Result := QPos.Map = nil;
end;

{*
  Teste si une propri�t� a sa valeur par d�faut
  @param Instance   Instance
  @param PropInfo   PropInfo de la propri�t� � tester
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
  @param PersistentClass   Classe � recenser
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
  D�recense une classe persistente FunLabyrinthe
  @param PersistentClass   Classe � d�recenser
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
  @param Classes   Classes � recenser
*}
procedure FunLabyRegisterClasses(Classes: array of TFunLabyPersistentClass);
var
  I: Integer;
begin
  for I := Low(Classes) to High(Classes) do
    FunLabyRegisterClass(Classes[I]);
end;

{*
  D�recense une liste de classes persistentes FunLabyrinthe
  @param Classes   Classes � d�recenser
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
  @param ClassName   Nom de la classe recherch�e
  @return Classe persistente dont le nom a �t� sp�cifi�, ou nil si non trouv�
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
  @param ClassName   Nom de la classe recherch�e
  @return Classe persistente dont le nom a �t� sp�cifi�
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
  Cr�e une instance de TPlayerPainter
  @param AMaster   Ma�tre d'images
  @param APlayer   Joueur propri�taire
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
  Indique si cette position qualifi�e est la position nulle
  @return True si c'est la position nulle, False sinon
*}
function TQualifiedPos.GetIsNoQPos: Boolean;
begin
  Result := Map = nil;
end;

{*
  Indique si cette position qualifi�e est dans la carte
  @return True si elle est dans la carte, False sinon
*}
function TQualifiedPos.GetIsInside: Boolean;
begin
  Result := Map.InMap(Position);
end;

{*
  Indique si cette position qualifi�e est hors de la carte
  @return True si elle est hors de la carte, False sinon
*}
function TQualifiedPos.GetIsOutside: Boolean;
begin
  Result := not Map.InMap(Position);
end;

{*
  Case pr�sente � cette position qualifi�e
  @return Case pr�sente � cette position qualifi�e
*}
function TQualifiedPos.GetSquare: TSquare;
begin
  Result := Map[Position];
end;

{*
  Terrain pr�sent � cette position qualifi�e
  @return Terrain pr�sent � cette position qualifi�e
*}
function TQualifiedPos.GetField: TField;
begin
  Result := Map[Position].Field;
end;

{*
  Effet pr�sent � cette position qualifi�e
  @return Effet pr�sent � cette position qualifi�e
*}
function TQualifiedPos.GetEffect: TEffect;
begin
  Result := Map[Position].Effect;
end;

{*
  Outil pr�sent � cette position qualifi�e
  @return Outil pr�sent � cette position qualifi�e
*}
function TQualifiedPos.GetTool: TTool;
begin
  Result := Map[Position].Tool;
end;

{*
  Obstacle pr�sent � cette position qualifi�e
  @return Obstacle pr�sent � cette position qualifi�e
*}
function TQualifiedPos.GetObstacle: TObstacle;
begin
  Result := Map[Position].Obstacle;
end;

{*
  Modifie la case pr�sente � cette position qualifi�e
  @param Value   Nouvelle case � placer
*}
procedure TQualifiedPos.SetSquare(Value: TSquare);
begin
  Map[Position] := Value;
end;

{*
  Modifie le terrain pr�sent � cette position qualifi�e
  @param Value   Nouveau terrain � placer
*}
procedure TQualifiedPos.SetField(Value: TField);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Value, Effect, Tool, Obstacle);
end;

{*
  Modifie l'effet pr�sent � cette position qualifi�e
  @param Value   Nouvel effet � placer
*}
procedure TQualifiedPos.SetEffect(Value: TEffect);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Field, Value, Tool, Obstacle);
end;

{*
  Modifie l'outil pr�sent � cette position qualifi�e
  @param Value   Nouvel outil � placer
*}
procedure TQualifiedPos.SetTool(Value: TTool);
begin
  with Map[Position] do
    Map[Position] := Master.SquareByComps(Field, Effect, Value, Obstacle);
end;

{*
  Modifie l'obstacle pr�sent � cette position qualifi�e
  @param Value   Nouvel obstacle � placer
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
  @return Composant � l'index sp�cifi�
*}
function TQualifiedPos.GetComponents(Index: Integer): TSquareComponent;
begin
  Result := Map[Position].Components[Index];
end;

{*
  Modifie un composant
  @param Index   Index compris entre 0 inclus et ComponentCount exclus
  @param Value   Nouveau composant � placer � l'index sp�cifi�
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
  Cr�e un contexte de dessin de case
  @param ABitmap   Bitmap cible
*}
constructor TDrawSquareContext.Create(ABitmap: TBitmap32);
begin
  Create(ABitmap, 0, 0, NoQPos);
end;

{*
  Cr�e un contexte de dessin de case
  @param ABitmap   Bitmap cible
  @param X         Abscisce o� dessiner
  @param Y         Abscisce o� dessiner
*}
constructor TDrawSquareContext.Create(ABitmap: TBitmap32; X, Y: Integer);
begin
  Create(ABitmap, X, Y, NoQPos);
end;

{*
  Cr�e un contexte de dessin de case
  @param ABitmap   Bitmap cible
  @param X         Abscisce o� dessiner
  @param Y         Abscisce o� dessiner
  @param AQPos     Position qualifi�e � dessiner
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
  Cr�e un contexte de dessin de case
  @param Source   Contexte � imiter
  @param AQPos    Position qualifi�e � dessiner
*}
constructor TDrawSquareContext.Create(Source: TDrawSquareContext;
  const AQPos: TQualifiedPos);
begin
  Create(Source.Bitmap, Source.X, Source.Y, AQPos);

  Assign(Source);
end;

{*
  Sp�cifie le joueur � dessiner
  @param APlayer   Joueur � dessiner
*}
procedure TDrawSquareContext.SetPlayer(APlayer: TPlayer);
begin
  FPlayer := APlayer;
end;

{*
  Sp�cifie le contexte de dessin de la vue
  @param ADrawViewContext   Contexte de dessin de la vue
*}
procedure TDrawSquareContext.SetDrawViewContext(
  ADrawViewContext: TDrawViewContext);
begin
  FDrawViewContext := ADrawViewContext;
  FTickCount := ADrawViewContext.TickCount;
end;

{*
  Sp�cifie le tick count pour ce contexte de dessin
  @param ATickCount   Tick count pour ce contexte de dessin
*}
procedure TDrawSquareContext.SetTickCount(ATickCount: Cardinal);
begin
  FTickCount := ATickCount;
end;

{*
  Copie les informations de contexte depuis un autre contexte
  Sont copi�s : DrawViewContext, TickCount et Player.
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
  Cr�e un contexte de dessin d'une vue
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
  Calcule la zone � afficher
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
  @param QPos   Position qualifi�e de la case � tester
  @return True si la case visible, False sinon
*}
function TDrawViewContext.IsSquareVisible(const QPos: TQualifiedPos): Boolean;
begin
  Result := IsSquareVisible(QPos.Map, QPos.Position);
end;

{*
  Teste si une case est visible dans cette zone
  @param Map        Carte de la case � tester
  @param Position   Position de la case � tester
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
  @param Position   Position de la case � tester
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
  Cr�e un contexte d'�v�nement de touche
  @param AKey     Touche press�e
  @param AShift   �tat des touches sp�ciales
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
  Cr�e une instance de TImagesMaster
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
  D�truit l'instance
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
  @param FileName   Nom du fichier peintre � charger
  @return Le bitmap charg�
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
  Charge une image d'apr�s son nom dans un bitmap
  @param ImgName   Nom de l'image
  @param Bitmap    Bitmap destination
  @return Le bitmap charg�, ou nil si non trouv�
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
  Ajoute une image � partir de son nom au ma�tre d'images
  Aucune image de m�me nom ne doit d�j� exister dans la liste interne.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoy�.
  @param ImgName   Nom de l'image
  @return Index de l'image nouvellement ajout�e, ou existante
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
  R�soud un nom d'image (sans indicateur de sous-image) en nom de fichier
  @param BaseImgName   Nom d'image (sans indicateur de sous-image @x,y)
  @return Nom de fichier pour cette image, ou '' si non trouv�
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
  Renvoie l'index de l'image dont le nom est sp�cifi�
  IndexOf renvoie l'index de l'image dont le nom est sp�cifi� dans la
  liste d'images interne. Si l'image n'a pas encore �t� charg�e, IndexOf
  la charge.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoy�.
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
  Dessine une image � partir de son index
  Draw dessine l'image indiqu�e sur un canevas.
  @param Index     Index de l'image � dessiner
  @param Context   Contexte de dessin de la case
*}
procedure TImagesMaster.Draw(Index: Integer; Context: TDrawSquareContext);
begin
  Context.DrawSquareBitmap(TBitmap32(FImgList[Index]));
end;

{*
  Dessine une image � partir de son nom
  Draw dessine l'image indiqu�e sur un canevas.
  @param ImgName   Nom de l'image � dessiner
  @param Context   Contexte de dessin de la case
*}
procedure TImagesMaster.Draw(const ImgName: string;
  Context: TDrawSquareContext);
begin
  Draw(IndexOf(ImgName), Context);
end;

{*
  Dessine une image � partir de son index
  Draw dessine l'image indiqu�e sur un canevas.
  @param Index    Index de l'image � dessiner
  @param Bitmap   Bitmap sur lequel dessiner l'image
  @param X        Coordonn�e X du point � partir duquel dessiner l'image
  @param Y        Coordonn�e Y du point � partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index: Integer; Bitmap: TBitmap32;
  X: Integer = 0; Y: Integer = 0);
begin
  Bitmap.Draw(X, Y, TBitmap32(FImgList[Index]));
end;

{*
  Dessine une image � partir de son nom
  Draw dessine l'image indiqu�e sur un canevas.
  @param ImgName   Nom de l'image � dessiner
  @param Bitmap    Bitmap sur lequel dessiner l'image
  @param X         Coordonn�e X du point � partir duquel dessiner l'image
  @param Y         Coordonn�e Y du point � partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(const ImgName: string; Bitmap: TBitmap32;
  X: Integer = 0; Y: Integer = 0);
begin
  Draw(IndexOf(ImgName), Bitmap, X, Y);
end;

{*
  Obtient le bitmap interne d'une image d'apr�s son index
  @param Index   Index d'une image
  @return Bitmap interne pour cette image
*}
function TImagesMaster.GetInternalBitmap(Index: Integer): TBitmap32;
begin
  Result := TBitmap32(FImgList[Index]);
end;

{*
  Obtient le bitmap interne d'une image d'apr�s son nom
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
  Cr�e une instance de TPainter
  @param AMaster   Ma�tre d'images associ� au peintre
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
  Apr�s l'appel � Measure, les propri�t�s FStaticDraw, FNeedCache et FSize sont
  mises � jour.
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
  Cr�e l'image cache si elle n'est pas encore cr��e
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
  �v�nement OnChange de la description
  DescriptionChange est appel� lorsque la description du peintre change.
  Elle actualise l'image cache.
  @param Sender   Objet lan�ant l'�v�nement
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
  Analyse une ligne de description et en r�cup�re ses diff�rentes composantes
  @param Description   Ligne de description � analyser
  @param Bitmap        En sortie : Image de base
  @param SubRect       En sortie : Rectangle de l'image � conserver
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
  @param ImgName   Nom de l'image � ajouter
*}
procedure TPainter.AddImage(const ImgName: string);
begin
  Description.Add(ImgName);
end;

{*
  Ajoute une partie d'une image au peintre
  @param ImgName   Nom de l'image � ajouter
  @param SubRect   Rectangle de l'image � conserver
*}
procedure TPainter.AddImageRect(const ImgName: string; const SubRect: TRect);
begin
  if IsRectEmpty(SubRect) then
    Exit;

  Description.Add(Format('%s@%d,%d:%d,%d',
    [ImgName, SubRect.Left, SubRect.Top, SubRect.Right, SubRect.Bottom]));
end;

{*
  Assigne un peintre � celui-ci
  @param Source   Peintre source
*}
procedure TPainter.Assign(Source: TPainter);
begin
  Description.Assign(Source.Description);
end;

{*
  D�marre une modification du peintre
  Chaque appel � BeginUpdate doit �tre cl�tur� par un appel � EndUpdate.
*}
procedure TPainter.BeginUpdate;
begin
  Description.BeginUpdate;
end;

{*
  Termine une modification du peintre
  Appelez EndUpdate pour balancer chaque appel � BeginUpdate.
*}
procedure TPainter.EndUpdate;
begin
  Description.EndUpdate;
end;

{*
  R�cup�re le bitmap complet pour ce peintre
  La valeur renvoy�e peut �tre nil, si le peintre est vide. Elle est valide
  uniquement tant que le peintre n'est pas modifi�.
  Il est pr�f�rable d'utiliser les m�thodes de dessin du peintre plut�t que de
  r�cup�rer le bitmap complet lorsque c'est possible.
  @return Bitmap complet pour ce peintre (peut �tre nil)
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
  La m�thode Draw dessine le peintre dans le contexte de dessin sp�cifi�. Les
  diff�rentes images sont superpos�es, celle d'index 0 tout au-dessous.
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
  @param Y      Ordonn�e de destination
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
  Dessine le peintre sur un bitmap quelconque � un temps donn�
  @param TickCount   Tick-count
  @param Dest        Bitmap destination
  @param X           Abscisse de destination
  @param Y           Ordonn�e de destination
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
  Cr�e un contexte de d�placement du joueur
  @param APlayer   Joueur qui se d�place
  @param ADest     Destination
  @param AKey      Touche press�e (ou 0 si pas de touche press�e)
  @param AShift    �tat des touches sp�ciales (si une touche a �t� press�e)
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
  Cr�e un contexte de d�placement du joueur restant sur la m�me carte
  @param APlayer   Joueur qui se d�place
  @param ADest     Destination
  @param AKey      Touche press�e (ou 0 si pas de touche press�e)
  @param AShift    �tat des touches sp�ciales (si une touche a �t� press�e)
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
  Passe la case courante � la source
*}
procedure TMoveContext.SwitchToSrc;
begin
  FQPos := FSrcQPos;
end;

{*
  Passe la case courante � la destination
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
  Annule le d�placement
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
  D�finit les propri�t�s � charger depuis ou enregistrer dans un filer
  @param Filer   Filer utilis�
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
  Enregistre l'�tat actuel des propri�t�s comme �tant par d�faut
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
  D�but d'un �tat
  @param State   �tat qui commence
*}
procedure TFunLabyPersistent.BeginState(State: TPersistentState);
begin
  FPersistentState := FPersistentState + State;
end;

{*
  Fin d'un �tat
  @param State   �tat qui se termine
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
  Cr�e une collection
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
  Nombre d'�l�ments de la collection
  @return Nombre d'�l�ments de la collection
*}
function TFunLabyCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{*
  Tableau zero-based des �l�ments de la collection
  @param Index   Index d'un �l�ment
  @return �l�ment � l'index indiqu�
*}
function TFunLabyCollection.GetItems(Index: Integer): TFunLabyPersistent;
begin
  Result := TFunLabyPersistent(FItems[Index]);
end;

{*
  Notification qu'un �l�ment a �t� ajout� ou supprim�
  @param Item     �l�ment concern�
  @param Action   Action faite sur l'�l�ment
*}
procedure TFunLabyCollection.Notify(Item: TFunLabyPersistent;
  Action: TListNotification);
begin
end;

{*
  Obtient la classe d'�l�ment par d�faut
  @return Classe d'�l�ment par d�faut
*}
function TFunLabyCollection.GetDefaultItemClass: TFunLabyPersistentClass;
begin
  Result := nil;
end;

{*
  Ajoute un �l�ment d�j� cr�� � la collection
  @param Item   �l�ment � ajouter
  @return Index du nouvel �l�ment
*}
function TFunLabyCollection.AddItem(Item: TFunLabyPersistent): Integer;
begin
  Result := InsertItem(FItems.Count, Item);
end;

{*
  Ins�re un �l�ment d�j� cr�� � la collection
  @param Index   Index o� ins�rer l'�l�mnet
  @param Item    �l�ment � ins�rer
  @return Index du nouvel �l�ment
*}
function TFunLabyCollection.InsertItem(Index: Integer;
  Item: TFunLabyPersistent): Integer;
begin
  FItems.Insert(Index, Item);
  Notify(Item, lnAdded);
  Result := Index;
end;

{*
  Extrait un �l�ment sans le lib�rer
  @param Index   Index de l'�l�ment � extraire
  @return �l�ment extrait
*}
function TFunLabyCollection.ExtractItem(Index: Integer): TFunLabyPersistent;
begin
  Notify(Items[Index], lnExtracted);

  Result := TFunLabyPersistent(FItems.Extract(Items[Index]));
end;

{*
  Extrait un �l�ment sans le lib�rer
  @param Item   �l�ment � extraire
  @return �l�ment extrait
*}
function TFunLabyCollection.ExtractItem(
  Item: TFunLabyPersistent): TFunLabyPersistent;
begin
  Notify(Item, lnExtracted);

  Result := TFunLabyPersistent(FItems.Extract(Item));
end;

{*
  Efface tous les �l�ments de la collection
*}
procedure TFunLabyCollection.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

{*
  Ajoute un �l�ment � la collection
  @param ItemClass   Classe d'�l�ment � ajouter
  @return �l�ment ajout�
*}
function TFunLabyCollection.Add(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := Insert(Count, ItemClass);
end;

{*
  Ajoute un �l�ment de la classe par d�faut � la collection
  @return �l�ment ajout�
  @throws EAbstractError Cette collection n'a pas de classe par d�faut
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
  Insert un �l�ment dans la collection
  @param Index       Index o� ins�rer l'�l�ment
  @param ItemClass   Classe d'�l�ment � ajouter
  @return �l�ment ajout�
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
  Supprime un �l�ment de la collection
  @param Index   Index de l'�l�ment � supprimer
*}
procedure TFunLabyCollection.Delete(Index: Integer);
begin
  Notify(Items[Index], lnDeleted);

  FItems.Delete(Index);
end;

{*
  Supprime un �l�ment de la collection
  @param Item   �l�ment � supprimer
  @return Index de l'�l�ment supprim�
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
  �change deux �l�ments de la collection
  @param Index1   Premier index
  @param Index2   Second index
*}
procedure TFunLabyCollection.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

{*
  D�place un �l�ment de la collection
  @param CurIndex   Index d'un �l�ment
  @param NewIndex   Nouvel index de cet �l�ment
*}
procedure TFunLabyCollection.Move(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

{*
  Cherche un �l�ment dans la collection
  @param Item   �l�ment recherch�
  @return Index de cet �l�ment dans la collection, ou -1 si non trouv�
*}
function TFunLabyCollection.IndexOf(Item: TFunLabyPersistent): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

{*
  Teste si cette collection a une classe d'�l�ments par d�faut
  @return True si elle a une classe d'�l�ments par d�faut, False sinon
*}
function TFunLabyCollection.HasDefault: Boolean;
begin
  Result := GetDefaultItemClass <> nil;
end;

{---------------------}
{ TFunLabyFiler class }
{---------------------}

{*
  Cr�e un filer
  @param AInstance   Instance � traiter
  @param AOwner      Filer propri�taire
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
  �num�re les propri�t�s
*}
procedure TFunLabyFiler.EnumProperties;
begin
  Instance.DefineProperties(Self);
end;

{*
  D�bute un �tat de l'instance
  @param State   �tat � commencer
*}
procedure TFunLabyFiler.InstanceBeginState(State: TPersistentState);
begin
  Instance.BeginState(State);
end;

{*
  Termine un �tat de l'instance
  @param State   �tat � terminer
*}
procedure TFunLabyFiler.InstanceEndState(State: TPersistentState);
begin
  Instance.EndState(State);
end;

{*
  Teste si un composant a des donn�es li�es � un joueur
  @param Component   Composant
  @param Player      Joueur
  @return True si le composant a des donn�es li�es au joueur sp�cifi�
*}
function TFunLabyFiler.HasPlayerData(Component: TFunLabyComponent;
  Player: TPlayer): Boolean;
begin
  Result := Component.HasPlayerData(Player);
end;

{*
  Obtient les donn�es li�es � un joueur pour un composant
  @param Component   Composant
  @param Player      Joueur
  @return Donn�es li�es au joueur sp�cifi� pour le composant sp�cifi�
*}
function TFunLabyFiler.GetPlayerData(Component: TFunLabyComponent;
  Player: TPlayer): TPlayerData;
begin
  Result := Component.GetPlayerData(Player);
end;

{*
  D�finit une propri�t� publi�e
  @param PropInfo   PropInfo de la propri�t�
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
  D�finit une propri�t� lue et �crite avec des m�thodes
  @param Name       Nom de la propri�t�
  @param PropType   Type de la propri�t�
  @param GetProc    M�thode de lecture
  @param SetProc    M�thode d'�criture
  @param HasData    Indique s'il y a des donn�es � �crire
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
  D�finit une propri�t� lue avec un champ et �crite avec une m�thode
  @param Name       Nom de la propri�t�
  @param PropType   Type de la propri�t�
  @param GetField   Champ de lecture
  @param SetProc    M�thode d'�criture
  @param HasData    Indique s'il y a des donn�es � �crire
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
  D�finit une propri�t� lue et �crite avec un champ
  @param Name          Nom de la propri�t�
  @param PropType      Type de la propri�t�
  @param GetSetField   Champ de lecture et �criture
  @param HasData       Indique s'il y a des donn�es � �crire
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
  D�finit un sous-objet persistent
  @param Name          Nom de l'objet
  @param SubInstance   Sous-objet � lire/�crire
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
  D�finit une propri�t� de type TStrings
  @param Name         Nom de la propri�t�
  @param Strings      Liste de cha�nes
  @param ObjectType   Type d'objet stock� (peut �tre nil)
  @param HasData      Indique si la liste de cha�ne contient des donn�es
*}
procedure TFunLabyFiler.DefineStrings(const Name: string; Strings: TStrings;
  ObjectType: PTypeInfo; HasData: Boolean);
begin
  Assert((ObjectType = nil) or
    (ObjectType.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkClass]));

  HandleStrings(Name, Strings, ObjectType, HasData);
end;

{*
  D�finit une propri�t� de type TStrings qui a des donn�es si non vide
  @param Name         Nom de la propri�t�
  @param Strings      Liste de cha�nes
  @param ObjectType   Type d'objet stock� (peut �tre nil)
*}
procedure TFunLabyFiler.DefineStrings(const Name: string; Strings: TStrings;
  ObjectType: PTypeInfo = nil);
begin
  DefineStrings(Name, Strings, ObjectType, Strings.Count > 0);
end;

{*
  D�finit une propri�t� binaire
  @param Name        Nom de la propri�t�
  @param ReadProc    M�thode de call-back � utiliser pour lire les donn�es
  @param WriteProc   M�thode de call-back � utiliser pour �crire les donn�es
  @param HasData     Indique s'il y a des donn�es � �crire
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
  Cr�e un objet lecteur FunLabyrinthe
  @param Instance   Instance � lire
  @param AOwner     Lecteur propri�taire (peut �tre nil)
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
  Cr�e un objet �crivain FunLabyrinthe
  @param Instance   Instance � �crire
  @param AOwner     �crivain propri�taire (peut �tre nil)
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
  Cr�e les donn�es li�es � un joueur
  @param AComponent   Composant propri�taire
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
  Cr�e une instance de TFunLabyComponent
  @param AMaster   Ma�tre FunLabyrinthe
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
  Acc�s moins rapide que ID mais qui renvoie un ID vide si le composant vaut
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
  Classe de donn�es li�es au joueur
  Toute classe A h�ritant d'une classe B et qui r�impl�mente GetPlayerClass doit
  renvoyer une sous-classe de B.GetPlayerDataClass.
  @return Classe de donn�es li�es au joueur
*}
class function TFunLabyComponent.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TPlayerData;
end;

{*
  Change l'ID de ce composant
  N'appelez pas directement ChangeID, mais modifiez plut�t la propri�t� ID du
  composant.
  Surchargez cette m�thode si vous devez effectuer un traitement sp�cial lorsque
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
  Teste si ce composant a des donn�es pour un joueur donn�
  @param Player   Joueur � tester
  @return True si ce composant � des donn�es pour le joueur Player
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
  Obtient les donn�es li�es � un joueur donn�
  @param Player   Joueur dont obtenir les donn�es
  @return Donn�es li�es au joueur Player
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
  Cat�gorie de ce composant
  La cat�gorie est utilis�e en mode �dition pour savoir comment organiser les
  diff�rent composant dans la palette des composants. Si la cat�gorie est une
  cha�ne vide, ce composant n'est pas disponible dans la palette.
  @return Cat�gorie de ce composant
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
  Indique si ce composant peut �tre plac� dans une palette des composants
  @return True s'il peut �tre plac� dans une palette des composants, False sinon
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
  Indique si cette classe utilise des donn�es li�es au joueur
  @return True si cette classe utilise des donn�es li�es au joueur, False sinon
*}
class function TFunLabyComponent.UsePlayerData: Boolean;
begin
  Result := GetPlayerDataClass <> TPlayerData;
end;

{*
  Dessine l'ic�ne de ce composant
  @param Bitmap   Bitmap destination
  @param X        Abscisse
  @param Y        Ordonn�e
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
  Dessine l'ic�ne de ce composant sur un canevas VCL
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
  Teste si la propri�t� Name doit �tre enregistr�e
  @return True si la propri�t� Name doit �tre enregistr�e, False sinon
*}
function TVisualComponent.IsNameStored: Boolean;
begin
  Result := FName <> FDefaultName;
end;

{*
  Teste si la propri�t� EditVisualTag doit �tre enregistr�e
  @return True si la propri�t� EditVisualTag doit �tre enregistr�e, False sinon
*}
function TVisualComponent.IsEditVisualTagStored: Boolean;
begin
  Result := FEditVisualTag <> FDefaultEditVisualTag;
end;

{*
  Dessine le composant dans le contexte de dessin sp�cifi�
  PrivDraw dessine le composant dans le contexte de dessin sp�cifi�.
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
  Donne une valuer � EditVisualTag bas�e sur l'ID de ce composant
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
  DoDraw dessine le composant dans le contexte de dessin sp�cifi�.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.DoDraw(Context: TDrawSquareContext);
begin
  FPainter.Draw(Context);
end;

{*
  Dessine le tag visuel d'�dition
  DoDraw dessine le tag visuel d'�dition dans le contexte de dessin sp�cifi�.
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
  Draw dessine le composant dans le contexte de dessin sp�cifi�.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.Draw(Context: TDrawSquareContext);
begin
  PrivDraw(Context);
end;

{*
  Dessine le composant
  Draw dessine le composant dans le contexte de dessin sp�cifi�.
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Bitmap   Bitmap sur lequel dessiner le composant
  @param X        Coordonn�e X du point � partir duquel dessiner le composant
  @param Y        Coordonn�e Y du point � partir duquel dessiner le composant
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
  Cr�e un nouveau composant
  @param AID   ID du composant � cr�er
  @return Composant cr��
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
  Cr�e une instance de TPlugin
  @param AMaster   Ma�tre FunLabyrinthe
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
  D�truit l'instance
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
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
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
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TPlugin.DrawBefore(Context: TDrawSquareContext);
begin
  FPainterBefore.Draw(Context);
end;

{*
  Dessine sur le joueur
  DrawAfter est ex�cut� lors du dessin du joueur, apr�s celui-ci. Le dessin
  effectu� dans DrawAfter se retrouve donc sur le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TPlugin.DrawAfter(Context: TDrawSquareContext);
begin
  FPainterAfter.Draw(Context);
end;

{*
  Un joueur se d�place
  Moving est ex�cut� lorsqu'un joueur se d�place d'une case � une autre. Pour
  annuler le d�placement, Moving peut positionner le param�tre Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TPlugin.Moving(Context: TMoveContext);
begin
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Context   Contexte du d�placement
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
  Indique si le plug-in permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @param Param    Param�tre de l'action
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
  Nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
  @return Nombre d'objets que ce joueur poss�de
*}
function TObjectDef.GetCount(Player: TPlayer): Integer;
begin
  Result := TObjectDefPlayerData(GetPlayerData(Player)).Count;
end;

{*
  Modifie le nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
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
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TObjectDef.GetShownInfos(Player: TPlayer): string;
begin
  Result := Format(sDefaultObjectInfos, [Name, Count[Player]]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @param Param    Param�tre de l'action
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TObjectDef.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
  @param Param    Param�tre de l'action
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
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TField.Entering(Context: TMoveContext);
begin
end;

{*
  Ex�cut� lorsque le joueur tente de sortir de la case
  Exiting est ex�cut� lorsque le joueur tente de sortir de la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TField.Exiting(Context: TMoveContext);
begin
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Context   Contexte du d�placement
*}
procedure TField.Entered(Context: TMoveContext);
begin
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Context   Contexte du d�placement
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Context   Contexte du d�placement
*}
procedure TEffect.Entered(Context: TMoveContext);
begin
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Context   Contexte du d�placement
*}
procedure TEffect.Exited(Context: TMoveContext);
begin
end;

{*
  Ex�cute l'effet
  Execute ne devrait �tre appel� que lorsque Enabled vaut True.
  @param Context   Contexte du d�placement
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
  Ex�cut� lorsque le joueur trouve l'outil
  Find est ex�cut� lorsque le joueur trouve l'outil. C'est-�-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Context   Contexte du d�placement
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
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Execute de la case ne soit ex�cut�e, il faut positionner
  AbortExecute � True.
  @param Context   Contexte du d�placement
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
  Cr�e une instance de TSquare et la configure
  @param AMaster     Ma�tre FunLabyrinthe
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
  D�tourne un �v�nement vers un composant � position
  @param Context         Contexte du mouvement
  @param EventKind       Type d'�v�nement
  @param HookComponent   Composant qui prend en charge le d�tournement
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
  D�tourne un �v�nement vers un composant � position qui le demanderait
  @param Context     Contexte du mouvement
  @param EventKind   Type d'�v�nement
  @return True si l'�v�nement a �t� d�tourn�, False sinon
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
  Met � jour l'ID de cette case
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
  @return Composant � l'index sp�cifi�
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
  @return Classe requise du composant � l'index sp�cifi�
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
  Modifie la propri�t� Category
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
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TSquare.DoEntering(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Entering(Context);
end;

{*
  Ex�cut� lorsque le joueur tente de sortir de la case
  Exiting est ex�cut� lorsque le joueur tente de sortir de la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TSquare.DoExiting(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Exiting(Context);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Context   Contexte du d�placement
*}
procedure TSquare.DoEntered(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Entered(Context);
  if Assigned(Effect) then
    Effect.Entered(Context);
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Context   Contexte du d�placement
*}
procedure TSquare.DoExited(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Exited(Context);
  if Assigned(Effect) then
    Effect.Exited(Context);
end;

{*
  Trouve l'objet et ex�cute l'effet
  @param Context   Contexte du d�placement
*}
procedure TSquare.DoExecute(Context: TMoveContext);
begin
  if Assigned(Tool) then
    Tool.Find(Context);
  if Assigned(Effect) and Effect.Enabled then
    Effect.Execute(Context);
end;

{*
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Entered de la case ne soit ex�cut�e, il faut positionner
  AbortEntered � True.
  @param Context   Contexte du d�placement
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
  Dispatche un message � cette case � un endroit particulier
  @param Msg    Message � dispatcher
  @param QPos   Position qualifi�e o� dispatcher le message
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
  Teste si cette case contient un composant donn�
  Si le composant est une case enti�re, renvoie True si c'est cette case. Sinon,
  renvoie True si le sous-composant correspondant � la classe de Component
  (ex. : Field pour TField) est le composant indiqu�.
  @param Component   Composant � tester
  @return True si cette case contient le composant sp�cifi�, False sinon
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
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TSquare.Entering(Context: TMoveContext);
begin
  if not HookEvent(Context, sekEntering) then
    DoEntering(Context);
end;

{*
  Ex�cut� lorsque le joueur tente de sortir de la case
  Exiting est ex�cut� lorsque le joueur tente de sortir de la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TSquare.Exiting(Context: TMoveContext);
begin
  if not HookEvent(Context, sekExiting) then
    DoExiting(Context);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Context   Contexte du d�placement
*}
procedure TSquare.Entered(Context: TMoveContext);
begin
  if not HookEvent(Context, sekEntered) then
    DoEntered(Context);
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Context   Contexte du d�placement
*}
procedure TSquare.Exited(Context: TMoveContext);
begin
  if not HookEvent(Context, sekExited) then
    DoExited(Context);
end;

{*
  Trouve l'objet et ex�cute l'effet
  @param Context   Contexte du d�placement
*}
procedure TSquare.Execute(Context: TMoveContext);
begin
  if not HookEvent(Context, sekExecute) then
    DoExecute(Context);
end;

{*
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Entered de la case ne soit ex�cut�e, il faut positionner
  AbortEntered � True.
  @param Context   Contexte du d�placement
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
  Cr�e une instance de TMap et lui donne des dimensions
  @param AMaster       Ma�tre FunLabyrinthe
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
  Cr�e la carte
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
  // Contr�le de format
  Stream.ReadBuffer(Value, 4);
  if Value <> MapStreamFormatCode then
    EInOutError.Create(SInvalidFileFormat);

  // Contr�le de version de format
  Stream.ReadBuffer(Value, 4);
  if Value > MapStreamVersion then
    EInOutError.CreateFmt(SVersionTooHigh, [IntToStr(Value)]);

  // Lecture des dimensions et de la taille d'une zone
  Stream.ReadBuffer(FDimensions, SizeOf(T3DPoint));
  Stream.ReadBuffer(FZoneWidth, 4);
  Stream.ReadBuffer(FZoneHeight, 4);

  // Cr�ation de la carte elle-m�me
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

  // �criture des dimensions et de la taille d'une zone
  Stream.WriteBuffer(FDimensions, SizeOf(T3DPoint));
  Stream.WriteBuffer(FZoneWidth, 4);
  Stream.WriteBuffer(FZoneHeight, 4);

  // Pr�paration de la palette (Squares.Tag) et �criture de celle-ci
  for I := 0 to Master.SquareCount-1 do
    Master.Squares[I].Tag := -1;
  Count := 0;
  PaletteCountPos := Stream.Position;
  Stream.WriteBuffer(Count, 4); // On repassera changer �� plus tard
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

  // �criture de la carte
  if Count <= 256 then
    SquareSize := 1
  else
    SquareSize := 2;
  for I := 0 to LinearMapCount-1 do
  begin
    Value := LinearMap[I].Tag;
    Stream.WriteBuffer(Value, SquareSize);
  end;

  // On retourne �crire la taille de la palette
  Stream.Seek(PaletteCountPos, soFromBeginning);
  Stream.WriteBuffer(Count, 4);
  Stream.Seek(0, soFromEnd);
end;

{*
  Tableau des cases index� par leur position sur la carte
  @param Position   Position sur la carte
  @return La case � la position sp�cifi�e
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
  Modifie le tableau des cases index� par leur position sur la carte
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
  Tableau des cases hors de la carte index� par �tage
  @param Floor   �tage
  @return La case hors de la carte � l'�tage sp�cifi�
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
  Modifie le tableau des cases hors de la carte index� par �tage
  @param Floor   �tage
  @param Value   Nouvelle case
*}
procedure TMap.SetOutside(Floor: Integer; Value: TSquare);
begin
  if (Floor >= 0) and (Floor < FDimensions.Z) then
    LinearMap[Floor + FOutsideOffset] := Value;
end;

{*
  Taille du tableau de la carte lin�aire
  @return Taille du tableau de la carte lin�aire
*}
function TMap.GetLinearMapCount: Integer;
begin
  Result := Length(FMap);
end;

{*
  Tableau zero-based de la carte lin�aire
  @param Index   Index dans le tableau
  @return Case de la carte lin�aire � l'index sp�cifi�
*}
function TMap.GetLinearMap(Index: Integer): TSquare;
begin
  Result := FMap[Index];
end;

{*
  Modifie le tableau zero-based de la carte lin�aire
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
  Assigne une carte � celle-ci
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
  Teste si une coordonn�e est � l'int�rieur de la carte
  @param Position   Coordonn�e � tester
  @return True si la coordonn�e est dans la carte, False sinon
*}
function TMap.InMap(const Position: T3DPoint): Boolean;
begin
  Result :=
    (Position.X >= 0) and (Position.X < FDimensions.X) and
    (Position.Y >= 0) and (Position.Y < FDimensions.Y) and
    (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

{*
  Teste si une position est dans les bornes d'�tages de la carte
  @param Position   Position � tester
  @return True si la position est dans les bornes d'�tages, False sinon
*}
function TMap.InFloors(const Position: T3DPoint): Boolean;
begin
  Result := (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

{*
  Teste si un num�ro d'�tage est dans les bornes d'�tages de la carte
  @param Floor   Num�ro d'�tage � tester
  @return True si le num�ro est dans les bornes d'�tages, False sinon
*}
function TMap.InFloors(Floor: Integer): Boolean;
begin
  Result := (Floor >= 0) and (Floor < FDimensions.Z);
end;

{*
  D�termine le nombre de joueurs qui se trouvent � un point de la carte
  @param Position   Position de la carte o� chercher les joueurs
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
  Cr�e le mode
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
  Dessine les joueurs et autres composants � position
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
  Dessine un composant � position
  @param Context        Contexte de dessin de la vue
  @param PosComponent   Composant � dessiner
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
  Teste si la propri�t� ZIndex doit �tre enregistr�e
  @return True ssi la propri�t� ZIndex doit �tre enregistr�e
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
  Sp�cifie les types d'�v�nements de case � intercepter
  Cette m�thode ne peut �tre appel�e que dans le constructeur.
  @param Value   Types d'�v�nements de case � intercepter
*}
procedure TPosComponent.SetWantedSquareEvents(Value: TSquareEventKinds);
begin
  Assert(psCreating in PersistentState);

  FWantedSquareEvents := Value;
end;

{*
  Sp�cifie si le composant veut attraper les messages de la case
  Cette m�thode ne peut �tre appel�e que dans le constructeur.
  @param Value   True pour intercepter les messages, False sinon
*}
procedure TPosComponent.SetWantMessages(Value: Boolean);
begin
  Assert(psCreating in PersistentState);

  FWantMessages := Value;
end;

{*
  M�thode de notification que la position de ce composant � chang�
*}
procedure TPosComponent.PositionChanged;
begin
end;

{*
  Change la position de ce composant
  @param AQPos   Nouvelle position qualifi�e
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
  Gestionnaire d'�v�nement SquareEvent
  @param Msg   Message
*}
procedure TSquareModifier.MessageSquareEvent(var Msg: TSquareEventMessage);
begin
  FEventHandlers[Msg.Kind](Msg.Context);
end;

{*
  Gestionnaire de l'�v�nement de case Entering
  @param Context   Contexte du d�placement
*}
procedure TSquareModifier.Entering(Context: TMoveContext);
begin
  Context.Square.DoEntering(Context);
end;

{*
  Gestionnaire de l'�v�nement de case Exiting
  @param Context   Contexte du d�placement
*}
procedure TSquareModifier.Exiting(Context: TMoveContext);
begin
  Context.Square.DoExiting(Context);
end;

{*
  Gestionnaire de l'�v�nement de case Entered
  @param Context   Contexte du d�placement
*}
procedure TSquareModifier.Entered(Context: TMoveContext);
begin
  Context.Square.DoEntered(Context);
end;

{*
  Gestionnaire de l'�v�nement de case Exited
  @param Context   Contexte du d�placement
*}
procedure TSquareModifier.Exited(Context: TMoveContext);
begin
  Context.Square.DoExited(Context);
end;

{*
  Gestionnaire de l'�v�nement de case Execute
  @param Context   Contexte du d�placement
*}
procedure TSquareModifier.Execute(Context: TMoveContext);
begin
  Context.Square.DoExecute(Context);
end;

{*
  Gestionnaire de l'�v�nement de case Pushing
  @param Context   Contexte du d�placement
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
  Peintres de ce v�hicule par direction
  @param Dir   Direction
  @return Peintre de ce v�hicule pour la direction Dir
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
  Attache � un contr�leur et retire le v�hicule de la carte
  @param AController   Contr�leur
*}
procedure TVehicle.AttachController(AController: TPlayer);
begin
  ChangePosition(NoQPos);
  FController := AController;
  FController.AddPlugin(Plugin);
end;

{*
  D�tache le v�hicule de son contr�leur et repositionne le v�hicule
  @param AQPos   Position � laquelle repositionner le v�hicule
*}
procedure TVehicle.DetachController(const AQPos: TQualifiedPos);
begin
  FController.RemovePlugin(Plugin);
  FController := nil;
  ChangePosition(AQPos);
end;

{*
  D�tache le v�hicule de son contr�leur et repositionne le v�hicule
  Le v�hicule est repositionn� � la position actuelle de son contr�leur
*}
procedure TVehicle.DetachController;
begin
  DetachController(Controller.QPos);
end;

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TVehicle.DrawBefore(Context: TDrawSquareContext);
begin
  DirPainters[Context.Player.Direction].Draw(Context);
end;

{*
  Dessine sur le joueur
  DrawAfter est ex�cut� lors du dessin du joueur, apr�s celui-ci. Le dessin
  effectu� dans DrawAfter se retrouve donc sur le joueur.
  @param Context   Contexte de dessin de la case
*}
procedure TVehicle.DrawAfter(Context: TDrawSquareContext);
begin
end;

{*
  Un joueur se d�place
  Moving est ex�cut� lorsqu'un joueur se d�place d'une case � une autre.
  @param Context   Contexte du d�placement
*}
procedure TVehicle.Moving(Context: TMoveContext);
begin
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Context   Contexte du d�placement
*}
procedure TVehicle.Moved(Context: TMoveContext);
begin
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @param Param    Param�tre de l'action
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
  @param Msg   Message � traiter
*}
procedure TMobileComponent.MessageRunMethod(var Msg: TRunMethodMessage);
begin
  Msg.Method;
end;

{*
  Proc�dure de la coroutine d'ex�cution des actions
  @param Coroutine   Objet coroutine g�rant cette proc�dure
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
  Si TryPause renvoie True, vous devez appelez Resume pour red�marrer les
  actions.
  @return True si les actions ont �t� mises en pause, False sinon
*}
function TMobileComponent.TryPause: Boolean;
begin
  Result := FInActionLock.TryEnter;
end;

{*
  Red�marre les actions de ce composant
  Resume doit �tre appel� pour red�marrer les actions apr�s un TryPause r�ussi.
*}
procedure TMobileComponent.Resume;
begin
  FInActionLock.Release;
end;

{*
  Envoie un message au joueur dans le contexte de ses actions
  Cette m�thode ne peut *pas* �tre appel�e depuis le code d'action du
  composant ! Utilisez directement Dispatch � la place.
  @param Msg   Message � envoyer
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
  Ex�cute une m�thode dans le contexte des actions de ce composant
  Cette m�thode ne peut *pas* �tre appel�e depuis le code d'action du
  composant ! Appelez directement la m�thode en question � la place.
  @param Method   M�thode � ex�cuter
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
  Liste des plug-in attach�s au joueur sous forme de cha�ne
  @return Liste des plug-in attach�s au joueur sous forme de cha�ne
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
  Modifie la liste des plug-in attach�s au joueur � partir d'une cha�ne
  @param Value   Liste des plug-in � attacher au joueur sous forme de cha�ne
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
  Liste des modes attach�s au joueur sous forme de cha�ne
  @return Liste des modes attach�s au joueur sous forme de cha�ne
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
  Modifie la liste des modes attach�s au joueur � partir d'une cha�ne
  @param Value   Liste des modes � attacher au joueur sous forme de cha�ne
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
  Liste des plug-in attach�s au joueur sous forme de cha�ne
  @return Liste des plug-in attach�s au joueur sous forme de cha�ne
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
  Modifie la liste des plug-in attach�s au joueur � partir d'une cha�ne
  @param Value   Liste des plug-in � attacher au joueur sous forme de cha�ne
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
  @param PluginList   Liste o� enregistrer la liste des plug-in
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
  Met � jour le bitmap FColoredPainterCache
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

    // Dessin du joueur lui-m�me
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
  Indique que ce joueur a trouv� un objet
  @param ObjectDef   D�finition de l'objet trouv�
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
  R�soud un nom de fichier son
  @param HRef   HRef du fichier son
  @return Nom du fichier son, ou '' si non trouv�
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
  Tableau index� par cha�ne des attributs du joueur
  @param AttrName   Nom de l'attribut � r�cup�rer
  @return Attribut dont le nom a �t� sp�cifi�
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
  Modifie le tableau index� par cha�ne des attributs du joueur
  @param AttrName   Nom de l'attribut � modifier
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
  Cette m�thode ne peut *pas* �tre appel�e hors code d'action du joueur !
  Utilisez SendMessage � la place dans ce cas.
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
  @param Attributes   Liste de cha�nes dans laquelle enregistrer les attributs
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
  @param Plugins   Liste de cha�nes dans laquelle enregistrer les ID des plug-in
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
  Obtient la liste des objets trouv�s, dans l'odre o� il les a trouv�s
  @param ObjectDefs   En sortie, remplie avec la liste des objets trouv�s
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
  L'ancien mode sera remis en place � l'appel de EndTempMode.
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
  @param Plugin   Le plug-in � greffer
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
  @param Plugin   Le plug-in � retirer
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
  Teste si un plug-in donn� est attach� au joueur
  @param Plugin   Plug-in � tester
  @return True si le plug-in est attach� au joueur, False sinon
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
  Indique si le joueur est capable d'effectuer une action donn�e
  Un joueur est capable d'effectuer une action si l'un de ses plug-in ou l'un de
  ses objets le lui permet.
  @param Action   Action � tester
  @param Param    Param�tre de l'action
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
  Ex�cute l'action sp�cifi�e
  DoAction v�rifie d'abord que le joueur en est bien capable. Et si plusieurs
  objets permettent d'effectuer l'action, le joueur se voit demander d'en
  choisir un.
  @param Action   Action � tester
  @param Param    Param�tre de l'action
  @return True si le joueur a �t� capable d'effectuer l'action, False sinon
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

  // Les plug-in ont la priorit�, puisqu'ils n'ont pas d'effet de bord
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

    // Aucun objet trouv� : �chec
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
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir         Direction du d�placement
  @param Key         Touche qui a �t� press�e pour le d�placement
  @param Shift       �tat des touches sp�ciales
  @param Redo        Indique s'il faut r�it�rer le d�placement
  @param RedoDelay   D�lai en millisecondes avant de r�it�rer le d�placement
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
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir         Direction du d�placement
  @param Redo        Indique s'il faut r�it�rer le d�placement
  @param RedoDelay   D�lai en millisecondes avant de r�it�rer le d�placement
*}
procedure TPlayer.Move(Dir: TDirection; out Redo: Boolean;
  out RedoDelay: Cardinal);
begin
  Move(Dir, 0, [], Redo, RedoDelay);
end;

{*
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir     Direction du d�placement
  @param Key     Touche qui a �t� press�e pour le d�placement
  @param Shift   �tat des touches sp�ciales
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
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir   Direction du d�placement
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
  Teste si un d�placement est permis
  @param Context   Contexte de d�placement
  @return True si le d�placement est permis, False sinon
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
  Teste si un d�placement est permis
  @param Dest    Case destination
  @param Key     Touche press�e pour le d�placement
  @param Shift   �tat des touches sp�ciales
  @return True si le d�placement est permis, False sinon
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
  Teste si un d�placement est permis
  @param Dest   Case destination
  @return True si le d�placement est permis, False sinon
*}
function TPlayer.IsMoveAllowed(const Dest: T3DPoint): Boolean;
begin
  Result := IsMoveAllowed(Dest, 0, []);
end;

{*
  D�place le joueur
  @param Context   Contexte de d�placement
  @param Execute   Indique s'il faut ex�cuter la case d'arriv�e (d�faut = True)
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
  D�place le joueur
  @param Dest        Position de destination
  @param Execute     Indique s'il faut ex�cuter la case d'arriv�e
  @param Redo        Indique s'il faut r�it�rer le d�placement
  @param RedoDelay   D�lai en millisecondes avant de r�it�rer le d�placement
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
  D�place le joueur, et poursuit le d�placement si n�cessaire
  @param Dest      Position de destination
  @param Execute   Indique s'il faut ex�cuter la case d'arriv�e
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
  D�place le joueur
  @param Dest        Position de destination
  @param Execute     Indique s'il faut ex�cuter la case d'arriv�e
  @param Redo        Indique s'il faut r�it�rer le d�placement
  @param RedoDelay   D�lai en millisecondes avant de r�it�rer le d�placement
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
  D�place le joueur, et poursuit le d�placement si n�cessaire
  @param Dest      Position de destination
  @param Execute   Indique s'il faut ex�cuter la case d'arriv�e
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
  D�placement naturel, selon le mouvement d�j� entam� (sorte d'inertie)
  Apr�s un mouvement donn� express�ment, il suffit d'appeler NaturalMoving pour
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
  ChangePosition ne doit �tre utilis�e qu'en mode �dition, ou sous r�serve
  d'�tre certain de ce qu'on fait.
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
  @param Text   Texte � afficher au joueur
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
  Affiche un message demandant une s�lection au joueur
  @param Prompt             Message d'invite
  @param Answers            R�ponses possibles
  @param Default            Index de la r�ponse s�lectionn�e par d�faut
  @param ShowOnlySelected   Si True, affiche uniquement l'�l�ment s�lectionn�
  @return Index de la r�ponse s�lectionn�e
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
  Affiche un message demandant une s�lection au joueur
  @param Prompt             Message d'invite
  @param Answers            R�ponses possibles
  @param Default            Index de la r�ponse s�lectionn�e par d�faut
  @param ShowOnlySelected   Si True, affiche uniquement l'�l�ment s�lectionn�
  @return Index de la r�ponse s�lectionn�e
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
  Affiche un message demandant au joueur de s�lectionner un nombre
  @param Prompt    Message d'invite
  @param Default   Nombre s�lectionn� par d�faut
  @param Min       Valeur minimum
  @param Max       Valeur maximum
  @return Index de la r�ponse s�lectionn�e
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
  @param HRef   HRef du fichier son � jouer
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

    // Ce joueur a gagn�
    FPlayState := psWon;

    // Les autres joueurs ont perdu
    for I := 0 to Master.PlayerCount-1 do
      if Master.Players[I] <> Self then
        Master.Players[I].FPlayState := psLost;

    // La partie est termin�e
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

    // Si plus aucun joueur ne joue, la partie est termin�e
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
  @param Bitmap   Bitmap cible (doit correspondre � Width/Height)
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
  Cette m�thode ne peut *pas* �tre appel�e depuis le code d'action du joueur !
  @param Key     Touche press�e
  @param Shift   �tat des touches sp�ciales
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
  Attend qu'une touche soit press�e par le joueur
  Cette m�thode ne peut �tre appel�e que depuis le code d'action du joueur !
  @param Key     En sortie : Touche appuy�e
  @param Shift   En sortie : �tat des touches sp�ciales lors de l'appui
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
  Attend qu'une touche sp�cifique soit press�e par le joueur
  Cette m�thode ne peut �tre appel�e que depuis le code d'action du joueur !
  @param Key     Touche que doit appuyer le joueur
  @param Shift   �tat des touches sp�ciales requis
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
  Cr�e une entr�e de timer
  @param ATickCount   Tick-count auquel d�clencher ce timer
*}
constructor TTimerEntry.Create(ATickCount: Cardinal);
begin
  inherited Create;

  FTickCount := ATickCount;
end;

{*
  Recr�e une entr�e de timer lors du chargement
*}
constructor TTimerEntry.ReCreate;
begin
  inherited Create;
end;

{*
  Ex�cute cette entr�e de timer puis la lib�re
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
  Retourne le composant mobile qui doit �tre l'h�te de cette entr�e de timer
  Si GetHostComponent renvoie une valeur non nil, la m�thode Execute sera
  appel�e au sein des actions de ce composant.
  Sinon, la m�thode Execute est appel�e au sein du thread g�n�ral de gestion des
  timers.
  @return Composant h�te
*}
function TTimerEntry.GetHostComponent: TMobileComponent;
begin
  Result := nil;
end;

{*
  Ex�cute cette entr�e de timer puis la lib�re
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
  Cr�e une entr�e de timer de message de notification
  @param ATickCount    Tick-count auquel d�clencher ce timer
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
  Cr�e une collection de timers
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
  M�thode d'ex�cution du thread
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
  Lance le thread d'ex�cution des timers
*}
procedure TTimerCollection.Start;
begin
  FThread.Resume;
end;

{*
  Essaye de mettre le temps en pause
  Si TryPause renvoie True, vous devez appelez Resume pour red�marrer le
  temps.
  @return True si le temps a �t� mis en pause, False sinon
*}
function TTimerCollection.TryPause: Boolean;
begin
  Result := FActionLock.TryEnter;
end;

{*
  Red�marre le temps
  Resume doit �tre appel� pour red�marrer le temps apr�s un TryPause r�ussi.
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
  Programme un �v�nement message de notification
  @param Delay        D�lai en ms avant de d�clencher l'�v�nement
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
  Programme un �v�nement personnalis�
  @param TimerEntry   Entr�e de timer � programmer
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
  Cr�e une instance de TMaster
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
  D�truit l'instance
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
  Invalide la liste des PosComponents ordonn�s par Z-index
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
  Assure que la liste des PosComponents ordonn�s par Z-index est valide
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
  Trouve un composant d'une classe particuli�re par son ID
  @param ID              ID du composant � trouver
  @param RequiredClass   Classe requise pour ce composant
  @return Le composant dont l'ID a �t� sp�cifi�, ou nil si ID �tait vide
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
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
  Tableau des composants index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�, ou nil si ID �tait vide
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
*}
function TMaster.GetComponent(const ID: TComponentID): TFunLabyComponent;
begin
  Result := GetComponentAs(ID);
end;

{*
  Tableau des composants de case index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas de case
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
  Tableau des plug-in index� par leur ID
  @param ID   ID du plug-in � trouver
  @return Le plug-in dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun plug-in ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un plug-in
*}
function TMaster.GetPlugin(const ID: TComponentID): TPlugin;
begin
  Result := TPlugin(GetComponentAs(ID, TPlugin));
end;

{*
  Tableau des d�finitions d'objet index� par leur ID
  @param ID   ID de la d�finition d'objet � trouver
  @return La d�finition d'objet dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune d�finition ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas une def d'objet
*}
function TMaster.GetObjectDef(const ID: TComponentID): TObjectDef;
begin
  Result := TObjectDef(GetComponentAs(ID, TObjectDef));
end;

{*
  Tableau des terrains index� par leur ID
  @param ID   ID du terrain � trouver
  @return Le terrain dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun terrain ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un terrain
*}
function TMaster.GetField(const ID: TComponentID): TField;
begin
  Result := TField(GetComponentAs(ID, TField));

  if (Result = nil) and (ID <> '') and (FieldCount > 0) then
    Result := Fields[0];
end;

{*
  Tableau des effets de case index� par leur ID
  @param ID   ID de l'effet � trouver
  @return L'effet dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun effet ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un effet
*}
function TMaster.GetEffect(const ID: TComponentID): TEffect;
begin
  Result := TEffect(GetComponentAs(ID, TEffect));
end;

{*
  Tableau des outils index� par leur ID
  @param ID   ID de l'outil � trouver
  @return L'outil dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun outil ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un outil
*}
function TMaster.GetTool(const ID: TComponentID): TTool;
begin
  Result := TTool(GetComponentAs(ID, TTool));
end;

{*
  Tableau des obstacles index� par leur ID
  @param ID   ID de l'obstacle � trouver
  @return L'obstacle dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun obstacle ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un obstacle
*}
function TMaster.GetObstacle(const ID: TComponentID): TObstacle;
begin
  Result := TObstacle(GetComponentAs(ID, TObstacle));
end;

{*
  Tableau des cases index� par leur ID
  Si la case n'a pas pu �tre trouv�e, GetSquare essaye de trouver les terrain
  et effet correspondant � son ID et de cr�er la case automatiquement
  @param ID   ID de la case � trouver
  @return La case dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune case ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas une case
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
  Tableau des cartes index� par leur ID
  @param ID   ID de la carte � trouver
  @return La carte dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune carte ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas une carte
*}
function TMaster.GetMap(const ID: TComponentID): TMap;
begin
  Result := TMap(GetComponentAs(ID, TMap));
end;

{*
  Tableau des joueurs index� par leur ID
  @param ID   ID du joueur � trouver
  @return Le joueur dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun joueur ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un joueur
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
  @return Le composant � la position sp�cifi�e
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
  @return Le plug-in � la position sp�cifi�e
*}
function TMaster.GetPlugins(Index: Integer): TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Nombre de d�finitions d'objet
  @return Nombre de d�finitions d'objet
*}
function TMaster.GetObjectDefCount: Integer;
begin
  Result := FObjectDefs.Count;
end;

{*
  Tableau zero-based des d�finitions d'objet
  @param Index   Index de la d�finition d'objet
  @return La d�finition d'objet � la position sp�cifi�e
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
  @return Le terrain � la position sp�cifi�e
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
  @return L'effet � la position sp�cifi�e
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
  @return L'outil � la position sp�cifi�e
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
  @return L'obstacle � la position sp�cifi�e
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
  @return La case � la position sp�cifi�e
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
  @return La carte � la position sp�cifi�e
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
  @return Le joueur � la position sp�cifi�e
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
  @return Le composant � la position sp�cifi�e
*}
function TMaster.GetPosComponents(Index: Integer): TPosComponent;
begin
  Result := TPosComponent(FPosComponents[Index]);
end;

{*
  Tableau zero-based des composants avec position ordonn�s par ZIndex
  @param Index   Index du composant
  @return Le composant � la position sp�cifi�e
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
  @return Le composant � la position sp�cifi�e
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
  @param Component   Le composant � ajouter
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
  @param Component   Le composant � retirer
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
  Notification que l'ID d'un composant a �t� modifi�
  @param Component   Le composant dont l'ID a �t� modifi�
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
  Met fin � la partie
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
  @param ID   ID de composant � tester
  @return True si un composant avec cet ID existe d�j�, False sinon
*}
function TMaster.ComponentExists(const ID: TComponentID): Boolean;
begin
  Result := FComponentsByID.IndexOf(ID) >= 0;
end;

{*
  V�rifie qu'un ID de composant est valide
  @param ID   ID de composant � tester
*}
procedure TMaster.CheckComponentID(const ID: TComponentID);
begin
  if not IsValidIdent(ID) then
    raise EInvalidID.CreateFmt(SInvalidID, [ID]);

  if ComponentExists(ID) then
    raise EInvalidID.CreateFmt(SDuplicateID, [ID]);
end;

{*
  Cherche une ressource d'apr�s son href et son type
  @param HRef   HRef de la ressource
  @param Kind   Type de ressource recherch�e
  @return Nom complet du fichier pour cette ressource
  @throws EResourceNotFoundException La ressource n'a pas pu �tre trouv�e
*}
function TMaster.FindResource(const HRef: string;
  Kind: TResourceKind): TFileName;
begin
  Result := FFindResourceCallback(HRef, Kind);
end;

{*
  Obtient une case � partir des ID de ses composantes
  @param Field      ID du terrain
  @param Effect     ID de l'effet
  @param Tool       ID de l'outil
  @param Obstacle   ID de l'obstacle
  @return Case avec avec les composantes sp�cifi�es
*}
function TMaster.SquareByComps(
  const Field, Effect, Tool, Obstacle: TComponentID): TSquare;
begin
  Result := Square[Format(SquareIDFormat, [Field, Effect, Tool, Obstacle])];
end;

{*
  Obtient une case � partir de ses composantes
  @param Field      Terrain
  @param Effect     Effet
  @param Tool       Outil
  @param Obstacle   Obstacle
  @return Case avec avec les composantes sp�cifi�es
*}
function TMaster.SquareByComps(Field: TField; Effect: TEffect = nil;
  Tool: TTool = nil; Obstacle: TObstacle = nil): TSquare;
begin
  Result := Square[Format(SquareIDFormat,
    [Field.SafeID, Effect.SafeID, Tool.SafeID, Obstacle.SafeID])];
end;

{*
  Enregistre tous les composants qui doivent �tre mis dans la palette
  @param RegisterComponent   M�thode call-back pour l'enregistrement
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
  Cr�e un composant additionnel (non cr�� de base par une unit� utilis�e)
  @param ComponentClass   Class du composant � cr�er
  @param ID               ID du composant � cr�er
  @return Composant cr��
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
  Si TryPause renvoie True, vous devez appelez Resume pour red�marrer le jeu.
  @return True si le jeu a �t� mis en pause, False sinon
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
  Red�marre le jeu
  Resume doit �tre appel� pour red�marrer le jeu apr�s un TryPause r�ussi.
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

