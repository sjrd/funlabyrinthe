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
  Dialogs, TypInfo, ScUtils, ScCoroutines, SdDialogs, GR32, G32_Interface,
  FunLabyCoreConsts;

const {don't localize}
  SquareSize = 30;     /// Taille (en largeur et hauteur) d'une case
  HalfSquareSize = 15; /// Moiti� de la taille d'une case
  MinViewSize = 1;     /// Taille minimale d'une vue

  /// Couleur de transparence pour les fichiers .bmp
  clBmpTransparent32 = TColor32($FF008080);

  /// Couleur transparente
  clTransparent32 = TColor32($00000000);

  attrColor = 'Color';             /// Attribut de joueur pour Color
  attrShowCounter = 'ShowCounter'; /// Attribut de joueur pour ShowCounter

  /// Attribut de joueur pour ViewBorderSize
  attrViewBorderSize = 'ViewBorderSize';

  msgShowMessage = $01; /// Message pour afficher un message au joueur
  msgGameStarted = $02; /// Message envoy� lorsque le jeu commence

  /// Message envoy� aux composants d'une case lorsque celle-ci est �dit�e
  msgEditMapSquare = $03;

  CommandShowDialog = 'ShowDialog';           /// Commande ShowDialog
  CommandShowDialogRadio = 'ShowDialogRadio'; /// Commande ShowDialogRadio
  CommandChooseNumber = 'ChooseNumber';       /// Commande ChooseNumber

  SquareIDDelim = '-';            /// D�limiteur des parties d'un ID de case
  SquareIDFormat = '%s-%s-%s-%s'; /// Format d'un ID de case

  /// Temporisation par d�faut
  DefaultTemporization = 500;

  /// Couleur par d�faut d'un joueur
  DefaultPlayerColor = clBlue32;

  /// Taille de bordure de vue par d�faut
  DefaultViewBorderSize = 1;

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type repr�sentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Type repr�sentant une action
  TPlayerAction = type string;

  /// �tat de victoire/d�faite d'un joueur
  TPlayState = (psPlaying, psWon, psLost);

  /// Classe de base des exceptions FunLabyrinthe
  EFunLabyException = class(Exception);

  /// G�n�r�e si un composant recherch� n'est pas trouv�
  EComponentNotFound = class(EFunLabyException);

  /// G�n�r�e si une commande n'est pas support�e
  EUnsupportedCommand = class(EFunLabyException);

  /// G�n�r�e en cas de mauvaise d�finition d'une case
  EBadSquareDefException = class(EFunLabyException);

  TFunLabyFiler = class;
  TPlayerData = class;
  TFunLabyComponent = class;
  TSquareComponent = class;
  TSquare = class;
  TMap = class;
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
    Map: TMap;          /// Carte, ou nil pour une position nulle
    Position: T3DPoint; /// Position sur la carte, si Map <> nil
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

  {*
    Flag de traitement d'un message TEditMapSquareMessage
    - esfHandled : Si pr�sent en sortie, l'�diteur ne fait plus son action
      par d�faut (pris en compte uniquement avec esfAdding)
    - esfCancel : Si pr�sent en sortie, l'�diteur ne fait plus son action par
      d�faut et ne consid�re pas que la carte a �t� modifi�e
    - esfAdding : Le composant va �tre ajout�
    - esfRemoving : Le composant va �tre retir�
    - esfOutside : La position est en dehors de la carte (terrain seulement)
  *}
  TEditMapSquareFlag = (
    esfHandled, esfCancel, esfAdding, esfRemoving, esfOutside
  );

  /// Flags de traitement d'un message TEditMapSquareMessage
  TEditMapSquareFlags = set of TEditMapSquareFlag;

  {*
    Structure du message d'�dition d'une case dans une carte
    @author sjrd
    @version 5.0
  *}
  TEditMapSquareMessage = record
    MsgID: Word;                /// ID du message
    Flags: TEditMapSquareFlags; /// Flags du message
    QPos: TQualifiedPos;        /// Position qualifi�e de la case �dit�e
  end;

  {*
    Type des gestionnaires d'�v�nements OnSendCommand de TPlayer
    @param Sender    Joueur concern�
    @param Command   Commande � effectuer
    @param Params    Param�tres de la commande
    @return R�sultat de la commande
    @throws EUnsupportedCommand : La commande demand�e n'est pas support�e
  *}
  TSendCommandEvent = function(Sender: TPlayer;
    const Command: string; const Params: string = ''): string of object;

  {*
    Type de m�thode call-back pour l'enregistrement d'un unique composant
    @param Component   Le composant � enregistrer
  *}
  TRegisterSingleComponentProc = procedure(
    Component: TSquareComponent) of object;

  {*
    Type de m�thode call-back pour l'enregistrement d'un ensemble de composants
    @param Template       Composant mod�le pour l'image et le nom � afficher
    @param Components     Liste des composants faisant partie de l'ensemble
    @param DialogTitle    Titre de la bo�te de dialogue du choix du num�ro
    @param DialogPrompt   Invite de la bo�te de dialogue du choix du num�ro
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
    FBitmap: TBitmap32; /// Bitmap cible
    FX: Integer;        /// Absisce o� dessiner
    FY: Integer;        /// Ordonn�e o� dessiner
    FSquareRect: TRect; /// Rectangle de la case � dessiner

    FIsNowhere: Boolean;  /// Indique si le dessin est fait "nulle part"
    FQPos: TQualifiedPos; /// Position dessin�e

    FTickCount: Cardinal; /// Tick count pour ce contexte

    FPlayer: TPlayer; /// Joueur � dessiner (si applicable)

    procedure SetPlayer(APlayer: TPlayer);
  public
    constructor Create(ABitmap: TBitmap32); overload;
    constructor Create(ABitmap: TBitmap32; X, Y: Integer); overload;
    constructor Create(ABitmap: TBitmap32; X, Y: Integer;
      const AQPos: TQualifiedPos); overload;

    procedure SetTickCount(ATickCount: Cardinal);

    procedure DrawSquareBitmap(SquareBitmap: TBitmap32);

    property Bitmap: TBitmap32 read FBitmap;
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
    G�re le chargement des images d'apr�s leur nom
    TImagesMaster s'occupe de charger automatiquement les images qu'on lui
    demande d'afficher. Il les conserve dans une liste d'image.
    @author sjrd
    @version 5.0
  *}
  TImagesMaster = class(TObject)
  private
    FImgList: TObjectList; /// Liste d'images interne
    FImgNames: TStrings;   /// Liste des noms des images
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const ImgName: string; Bitmap: TBitmap32): Integer;

    function IndexOf(const ImgName: string): Integer;

    procedure Draw(Index: Integer; Context: TDrawSquareContext); overload;
    procedure Draw(const ImgName: string;
      Context: TDrawSquareContext); overload;

    procedure Draw(Index: Integer; Bitmap: TBitmap32;
      X: Integer = 0; Y: Integer = 0); overload;
    procedure Draw(const ImgName: string; Bitmap: TBitmap32;
      X: Integer = 0; Y: Integer = 0); overload;
  end;

  {*
    Enregistre et affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une m�thode
    pour les dessiner les unes sur les autres, par transparence.
    @author sjrd
    @version 5.0
  *}
  TPainter = class(TObject)
  private
    FMaster: TImagesMaster; /// Ma�tre d'images
    FImgNames: TStrings;    /// Liste des noms des images
    FCachedImg: TBitmap32;  /// Copie cache de l'image r�sultante

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
    FPlayer: TPlayer; /// Joueur qui se d�place

    FSrcMap: TMap;   /// Carte source
    FSrc: T3DPoint;  /// Case source
    FDestMap: TMap;  /// Carte destination
    FDest: T3DPoint; /// Case destination
    FMap: TMap;      /// Carte courante
    FPos: T3DPoint;  /// Position courante

    FOldDirection: TDirection; /// Ancienne direction du joueur
    FKeyPressed: Boolean;      /// True si une touche a �t� press�e

    FCancelled: Boolean;  /// True si le d�placement a �t� annul�
    FGoOnMoving: Boolean; /// True s'il faut r�it�rer le d�placement

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

    function AddItem(Item: TFunLabyPersistent): Integer;
    function InsertItem(Index: Integer; Item: TFunLabyPersistent): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Add(ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
    function Insert(Index: Integer;
      ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
    procedure Delete(Index: Integer);
    function Remove(Item: TFunLabyPersistent): Integer;
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function IndexOf(Item: TFunLabyPersistent): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFunLabyPersistent read GetItems;
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
      ObjectType: PTypeInfo = nil; HasData: Boolean = True);

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
    FMaster: TMaster;  /// Ma�tre FunLabyrinthe
    FID: TComponentID; /// ID du composant
    {*
      Valeur non fonctionnelle pouvant servir au fonctionnement d'un algorithme
      Cette valeur est susceptible d'�tre utilis�e par beaucoup d'algorithmes
      diff�rents, et donc interf�rer. Il ne faut donc l'utiliser que
      ponctuellement.
    *}
    FTag: Integer;

    FPlayerData: TBucketItemArray; /// Donn�es par joueur

    function GetSafeID: TComponentID;
  protected
    FTransient: Boolean; /// Indique si ce composant est transitoire

    class function GetPlayerDataClass: TPlayerDataClass; virtual;

    function HasPlayerData(Player: TPlayer): Boolean;
    function GetPlayerData(Player: TPlayer): TPlayerData;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
    destructor Destroy; override;

    property Master: TMaster read FMaster;
    property SafeID: TComponentID read GetSafeID;
    property Transient: Boolean read FTransient;
  published
    property ID: TComponentID read FID;
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
    FName: string;         /// Nom du composant
    FPainter: TPainter;    /// Peintre par d�faut
    FCachedImg: TBitmap32; /// Image en cache (pour les dessins invariants)

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
    procedure Draw(const QPos: TQualifiedPos; Bitmap: TBitmap32;
      X: Integer = 0; Y: Integer = 0); overload;

    procedure DrawToCanvas(Canvas: TCanvas; const DestRect: TRect;
      BackgroundColor: TColor);

    property StaticDraw: Boolean read FStaticDraw;
  published
    property Name: string read FName;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs fa�ons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Emp�cher le d�placement du joueur et r�agir � son d�placement effectif ;
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
    TField est la classe de base pour la cr�ation de terrains. Les terrains
    sont la premi�re composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TField = class(TSquareComponent)
  private
    FDelegateDrawTo: TField; /// Terrain d�l�gu� pour l'affichage

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
    TEffect est la classe de base pour la cr�ation d'effets de case. Les effets
    sont la deuxi�me composante d'une case.
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
    TTool est la classe de base pour la cr�ation d'outils. Les outils sont la
    troisi�me composante d'une case.
    @author sjrd
    @version 5.0
  *}
  TTool = class(TSquareComponent)
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
  public
    procedure Pushing(Context: TMoveContext); virtual;
  end;

  {*
    Repr�sente une case du jeu
    TSquare repr�sente une case du jeu. Une case poss�de quatre composantes : le
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
    FMaxViewSize: Integer;   /// Taille maximale d'une vue pour cette carte
    FMap: array of TSquare;  /// Carte stock�e de fa�on lin�aire
    FOutsideOffset: Integer; /// Offset de d�part de l'ext�rieur

    procedure CreateMap;

    procedure LoadMapFromStream(Stream: TStream);
    procedure SaveMapToStream(Stream: TStream);

    procedure SetMaxViewSize(Value: Integer);

    function GetMap(const Position: T3DPoint): TSquare;
    procedure SetMap(const Position: T3DPoint; Value: TSquare);

    function GetOutside(Floor: Integer): TSquare;
    procedure SetOutside(Floor: Integer; Value: TSquare);

    function GetLinearMapCount: Integer;
    function GetLinearMap(Index: Integer): TSquare;
    procedure SetLinearMap(Index: Integer; Value: TSquare);
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); overload;
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer); overload;

    function InMap(const Position: T3DPoint): Boolean;

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
  published
    property MaxViewSize: Integer read FMaxViewSize write SetMaxViewSize
      default MinViewSize;
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
  private
    FCacheBitmap: TBitmap32; /// Bitmap cache

    FOldMap: TMap;              /// Ancienne carte
    FOldFloor: Integer;         /// Ancien �tage
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

    property CacheBitmap: TBitmap32 read FCacheBitmap;
  public
    constructor Create(APlayer: TPlayer); override;
    destructor Destroy; override;

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
  TPlayer = class(TVisualComponent)
  private
    FMap: TMap;                        /// Carte
    FPosition: T3DPoint;               /// Position
    FDirection: TDirection;            /// Direction
    FMode: IPlayerMode;                /// Mode principal
    FModeStack: IInterfaceList;        /// Pile des modes sauvegard�s
    FShowCounter: Integer;             /// Compteur de visibilit�
    FColor: TColor32;                  /// Couleur
    FViewBorderSize: Integer;          /// Taille de la bordure de la vue
    FPlugins: TObjectList;             /// Liste des plug-in
    FAttributes: TStrings;             /// Liste des attributs
    FOnSendCommand: TSendCommandEvent; /// �v�nement d'ex�cution de commande
    FPlayState: TPlayState;            /// �tat de victoire/d�faite

    /// Verrou global pour le joueur
    FLock: TMultiReadExclusiveWriteSynchronizer;

    FActionCoroutine: TCoroutine;  /// Coroutine d'appui sur touche
    FActionIsMessage: Boolean;     /// L'action est un message
    FActionKey: Word;              /// Touche appuy�e
    FActionKeyShift: TShiftState;  /// �tat des touches sp�ciales
    FActionMessagePtr: Pointer;    /// Pointeur sur le message pour l'action

    function GetPluginListStr: string;
    procedure SetPluginListStr(const Value: string);
    function GetModeListStr: string;
    procedure SetModeListStr(const Value: string);

    procedure GetPluginList(out PluginList: TPluginDynArray);

    procedure PrivDraw(Context: TDrawSquareContext); override;

    function IsMoveAllowed(Context: TMoveContext): Boolean;
    procedure MoveTo(Context: TMoveContext; Execute: Boolean = True); overload;

    procedure ActionProc(Coroutine: TCoroutine);

    function GetVisible: Boolean;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    procedure DoDraw(Context: TDrawSquareContext); override;

    procedure PositionChanged; virtual;

    function GetAttribute(const AttrName: string): Integer; virtual;
    procedure SetAttribute(const AttrName: string; Value: Integer); virtual;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
    destructor Destroy; override;

    procedure Dispatch(var Msg); override;
    procedure DefaultHandler(var Msg); override;

    procedure GetAttributes(Attributes: TStrings); virtual;
    procedure GetPluginIDs(PluginIDs: TStrings);

    procedure DrawInPlace(Bitmap: TBitmap32; X: Integer = 0;
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

    function ShowSelectionMsg(const Prompt: string;
      const Answers: array of string; Default: Integer = 0;
      ShowOnlySelected: Boolean = False): Integer; overload;
    function ShowSelectionMsg(const Prompt: string; Answers: TStrings;
      Default: Integer = 0;
      ShowOnlySelected: Boolean = False): Integer; overload;

    function ShowSelectNumberMsg(const Prompt: string;
      Default, Min, Max: Integer): Integer;

    procedure Win;
    procedure Lose;

    procedure DrawView(Bitmap: TBitmap32); virtual;
    procedure PressKey(Key: Word; Shift: TShiftState);
    procedure SendMessage(var Msg);

    procedure WaitForKey(out Key: Word; out Shift: TShiftState);
    procedure WaitForSpecificKey(Key: Word; Shift: TShiftState = []);

    property Map: TMap read FMap;
    property Position: T3DPoint read FPosition;
    property Mode: IPlayerMode read FMode;
    property Visible: Boolean read GetVisible;
    property Attribute[const AttrName: string]: Integer
      read GetAttribute write SetAttribute;
    property OnSendCommand: TSendCommandEvent
      read FOnSendCommand write FOnSendCommand;
    property PlayState: TPlayState read FPlayState;
  published
    property Direction: TDirection read FDirection write FDirection
      default diNone;
    property Color: TColor32 read FColor write FColor
      default DefaultPlayerColor;
    property ViewBorderSize: Integer read FViewBorderSize write FViewBorderSize
      default DefaultViewBorderSize;
  end;

  {*
    Ma�tre FunLabyrinthe
    TMaster g�re les diff�rents composants de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TMaster = class(TFunLabyPersistent)
  private
    FImagesMaster: TImagesMaster; /// Ma�tre d'images
    FComponents: TStrings;        /// Table de hashage ID -> composant
    FPlugins: TObjectList;        /// Liste des plug-in
    FObjectDefs: TObjectList;     /// Liste des d�finitions d'objet
    FFields: TObjectList;         /// Liste des terrains
    FEffects: TObjectList;        /// Liste des effets
    FTools: TObjectList;          /// Liste des outils
    FObstacles: TObjectList;      /// Liste des obstacles
    FSquares: TObjectList;        /// Liste des cases
    FMaps: TObjectList;           /// Liste des cartes
    FPlayers: TObjectList;        /// Liste des joueurs

    FEditing: Boolean;            /// Indique si on est en mode �dition
    FTemporization: Integer;      /// Temporisation en millisecondes
    FBeginTickCount: Cardinal;    /// Tick count syst�me au lancement
    FTerminated: Boolean;         /// Indique si la partie est termin�e

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
    function GetPlayerCount: Integer;
    function GetPlayers(Index: Integer): TPlayer;

    procedure SetTemporization(Value: Integer);

    function GetTickCount: Cardinal;
    procedure SetTickCount(Value: Cardinal);

    procedure AddComponent(Component: TFunLabyComponent);
    procedure RemoveComponent(Component: TFunLabyComponent);

    procedure Terminate;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
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
    property PlayerCount: Integer read GetPlayerCount;
    property Players[Index: Integer]: TPlayer read GetPlayers;

    property Editing: Boolean read FEditing;
    property TickCount: Cardinal read GetTickCount;
    property Terminated: Boolean read FTerminated;
  published
    property Temporization: Integer read FTemporization write SetTemporization
      default DefaultTemporization;
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
  /// Dossier des plug-in de l'�diteur
  fEditPluginDir: string = 'EditPlugins\';

  /// Cha�ne de format pour les fichiers image
  fSquareFileName: string = '%s.bmp';

procedure ShowFunLabyAbout;

function PointBehind(const Src: T3DPoint; Dir: TDirection): T3DPoint;
function PointBefore(const Src: T3DPoint; Dir: TDirection): T3DPoint;

function CreateEmptySquareBitmap: TBitmap32;
function SquareRect(X, Y: Integer): TRect;
procedure EmptyRect(Bitmap: TBitmap32; Rect: TRect);
procedure EmptySquareRect(Bitmap: TBitmap32; X: Integer = 0; Y: Integer = 0);
procedure HandleBmpTransparent(Bitmap: TBitmap32);
procedure DrawBitmap32ToCanvas(Canvas: TCanvas; const DestRect: TRect;
  Bitmap: TBitmap32; BackgroundColor: TColor);

function SameRect(const Left, Right: TRect): Boolean;

function IsNoQPos(const QPos: TQualifiedPos): Boolean;

procedure FunLabyRegisterClass(PersistentClass: TFunLabyPersistentClass);
procedure FunLabyUnregisterClass(PersistentClass: TFunLabyPersistentClass);

procedure FunLabyRegisterClasses(Classes: array of TFunLabyPersistentClass);
procedure FunLabyUnregisterClasses(Classes: array of TFunLabyPersistentClass);

function FunLabyGetClass(const ClassName: string): TFunLabyPersistentClass;
function FunLabyFindClass(const ClassName: string): TFunLabyPersistentClass;

implementation

uses
  IniFiles, StrUtils, Forms, ScStrUtils, ScDelphiLanguage;

const
  /// Code de format d'un flux carte (TMap) (correspond � '.flm')
  MapStreamFormatCode: Longint = $6D6C662E;

  /// Version courante du format d'un flux carte (TMap)
  MapStreamVersion = 1;

var
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
  Convertit la couleur de transparence des .bmp en transparent r�el
  @param Bitmap   Bitmap � traiter
*}
procedure HandleBmpTransparent(Bitmap: TBitmap32);
var
  X, Y: Integer;
  Line: PColor32Array;
begin
  for Y := 0 to Bitmap.Height-1 do
  begin
    Line := Bitmap.ScanLine[Y];

    for X := 0 to Bitmap.Width-1 do
      if Line[X] = clBmpTransparent32 then
        Line[X] := clTransparent32;
  end;
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
    tkString, tkLString, tkWString:
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

  FImgList := TObjectList.Create;
  FImgNames := THashedStringList.Create;

  EmptySquare := CreateEmptySquareBitmap;
  try
    Add('', EmptySquare);
  finally
    EmptySquare.Free;
  end;
end;

{*
  D�truit l'instance
*}
destructor TImagesMaster.Destroy;
begin
  FImgNames.Free;
  FImgList.Free;

  inherited;
end;

{*
  Ajoute une image � partir d'un bitmap au ma�tre d'images
  Si une image de m�me nom existe d�j�, l'ajout est ignor�.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoy�.
  @param ImgName   Nom de l'image
  @param Bitmap    Bitmap contenant l'image � ajouter
  @return Index de l'image nouvellement ajout�e, ou existante
*}
function TImagesMaster.Add(const ImgName: string; Bitmap: TBitmap32): Integer;
var
  NewBitmap: TBitmap32;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  try
    NewBitmap := TBitmap32.Create;
    try
      NewBitmap.DrawMode := dmBlend;
      NewBitmap.Assign(Bitmap);
      FImgList.Add(NewBitmap);
    except
      NewBitmap.Free;
      raise;
    end;

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
  Renvoie l'index de l'image dont le nom est sp�cifi�
  IndexOf renvoie l'index de l'image dont le nom est sp�cifi� dans la
  liste d'images interne. Si l'image n'a pas encore �t� charg�e, IndexOf
  la charge.
  En cas d'erreur, l'index 0 - de l'image vide - est renvoy�.
  @param ImgName   Nom de l'image
  @return Index de l'image
*}
function TImagesMaster.IndexOf(const ImgName: string): Integer;
var
  NewImg: TBitmap32;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  begin
    NewImg := TBitmap32.Create;
    try
      try
        NewImg.DrawMode := dmBlend;
        NewImg.LoadFromFile(Format(fSquareFileName, [ImgName]));
        HandleBmpTransparent(NewImg);
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
  Sp�cifie le joueur � dessiner
  @param APlayer   Joueur � dessiner
*}
procedure TDrawSquareContext.SetPlayer(APlayer: TPlayer);
begin
  FPlayer := APlayer;
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
  Dessine un bitmap de case dans ce contexte
*}
procedure TDrawSquareContext.DrawSquareBitmap(SquareBitmap: TBitmap32);
begin
  Bitmap.Draw(X, Y, SquareBitmap);
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
  FImgNames := TStringList.Create;
  TStringList(FImgNames).OnChange := ImgNamesChange;
  FCachedImg := CreateEmptySquareBitmap;
  ImgNamesChange(nil);
end;

{*
  D�truit l'instance
*}
destructor TPainter.Destroy;
begin
  FCachedImg.Free;
  FImgNames.Free;
  inherited;
end;

{*
  �v�nement OnChange de la liste des noms des images
  ImgNamesChange est appel� lorsque la liste des noms des images change.
  Elle actualise l'image cache.
  @param Sender   Objet lan�ant l'�v�nement
*}
procedure TPainter.ImgNamesChange(Sender: TObject);
var
  I: Integer;
begin
  FCachedImg.FillRect(0, 0, SquareSize, SquareSize, clTransparent32);
  for I := 0 to FImgNames.Count-1 do
    FMaster.Draw(FImgNames[I], FCachedImg);
end;

{*
  Dessine les images sur un canevas
  La m�thode Draw dessine les images de ImgNames sur le canevas, � la
  position indiqu�e. Les diff�rentes images sont superpos�es, celle d'index
  0 tout au-dessous.
  @param Context   Contexte de dessin de la case
*}
procedure TPainter.Draw(Context: TDrawSquareContext);
begin
  Context.DrawSquareBitmap(FCachedImg);
end;

{--------------------}
{ TMoveContext class }
{--------------------}

{*
  Cr�e un contexte de d�placement du joueur
  @param APlayer       Joueur qui se d�place
  @param ADest         Destination
  @param AKeyPressed   True si une touche a �t� press�e
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
  Cr�e un contexte de d�placement du joueur restant sur la m�me carte
  @param APlayer       Joueur qui se d�place
  @param ADest         Destination
  @param AKeyPressed   True si une touche a �t� press�e
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
  Passe la case courante � la source
*}
procedure TMoveContext.SwitchToSrc;
begin
  FMap := SrcMap;
  FPos := Src;
end;

{*
  Passe la case courante � la destination
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
  Annule le d�placement
*}
procedure TMoveContext.Cancel;
begin
  FCancelled := True;
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
  Notification qu'un �l�ment a �t� ajout� ou supprim�
  @param Item     �l�ment concern�
  @param Action   Action faite sur l'�l�ment
*}
procedure TFunLabyCollection.Notify(Item: TFunLabyPersistent;
  Action: TListNotification);
begin
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
  SubInstance: TObject;
begin
  if not Assigned(PropInfo.GetProc) then
    Exit;

  if PropInfo.PropType^.Kind = tkClass then
  begin
    SubInstance := TObject(GetOrdProp(Instance, PropInfo));

    if not (SubInstance is TFunLabyComponent) then
    begin
      if SubInstance is TFunLabyPersistent then
        DefinePersistent(PropInfo.Name, TFunLabyPersistent(SubInstance))
      else if SubInstance is TStrings then
        DefineStrings(PropInfo.Name, TStrings(SubInstance), nil,
          IsStoredProp(Instance, PropInfo));

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
  PropInfo.Name := Name;

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
  ObjectType: PTypeInfo = nil; HasData: Boolean = True);
begin
  Assert((ObjectType = nil) or
    (ObjectType.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkClass]));

  HandleStrings(Name, Strings, ObjectType, HasData);
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

{-------------------------}
{ Classe TVisualComponent }
{-------------------------}

{*
  Cr�e une instance de TVisualComponent
  @param AMaster   Ma�tre FunLabyrinthe
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
  FStaticDraw := True;
end;

{*
  D�truit l'instance
*}
destructor TVisualComponent.Destroy;
begin
  FCachedImg.Free;
  FPainter.Free;

  inherited;
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
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
    FCachedImg := CreateEmptySquareBitmap;
    Context := TDrawSquareContext.Create(FCachedImg);
    try
      PrivDraw(Context);
    finally
      Context.Free;
    end;
  end;
end;

{*
  Dessine le composant sur un canevas
  PrivDraw dessine le composant sur un canevas � la position indiqu�e.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.PrivDraw(Context: TDrawSquareContext);
begin
  DoDraw(Context);
end;

{*
  Dessine le composant sur un canevas
  DoDraw dessine le composant sur un canevas � la position indiqu�e.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.DoDraw(Context: TDrawSquareContext);
begin
  FPainter.Draw(Context);
end;

{*
  Dessine de fa�on optimis�e le composant sur un canevas
  Draw dessine le composant sur un canevas � la position indiqu�e.
  @param Context   Contexte de dessin de la case
*}
procedure TVisualComponent.Draw(Context: TDrawSquareContext);
begin
  if StaticDraw then
    Context.DrawSquareBitmap(FCachedImg)
  else
    PrivDraw(Context);
end;

{*
  Dessine de fa�on optimis�e le composant sur un canevas
  Draw dessine le composant sur un canevas � la position indiqu�e.
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
  Dessine le composant sur un canevas VCL
  @param Canvas            Canevas cible
  @param DestRect          Rectangle dans lequel dessiner le bitmap
  @param BackgroundColor   Couleur de fond sur le canevas
*}
procedure TVisualComponent.DrawToCanvas(Canvas: TCanvas; const DestRect: TRect;
  BackgroundColor: TColor);
var
  TempBitmap: TBitmap32;
begin
  TempBitmap := TBitmap32.Create;
  try
    TempBitmap.SetSize(SquareSize, SquareSize);
    TempBitmap.Clear(Color32(BackgroundColor));
    Draw(NoQPos, TempBitmap);
    Canvas.CopyRect(DestRect, TempBitmap.Canvas, TempBitmap.BoundsRect);
  finally
    TempBitmap.Free;
  end;
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
begin
  inherited Create(AMaster, AID);
  FPainterBefore := TPainter.Create(FMaster.ImagesMaster);
  FPainterBefore.ImgNames.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.ImgNames.BeginUpdate;
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
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
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
begin
  TObjectDefPlayerData(GetPlayerData(Player)).Count := Value;
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
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TObjectDef.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TObjectDef.UseFor(Player: TPlayer; const Action: TPlayerAction);
begin
end;

{---------------}
{ Classe TField }
{---------------}

{*
  Cr�e une instance de TField
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
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
  @param Context   Contexte du d�placement
*}
procedure TEffect.Execute(Context: TMoveContext);
begin
end;

{--------------}
{ Classe TTool }
{--------------}

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
  Cr�e une instance de TSquare
  @param AMaster     Ma�tre FunLabyrinthe
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

  FTransient := True;

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
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Context   Contexte du d�placement
*}
procedure TSquare.Entering(Context: TMoveContext);
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
procedure TSquare.Exiting(Context: TMoveContext);
begin
  if Assigned(Field) then
    Field.Exiting(Context);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Context   Contexte du d�placement
*}
procedure TSquare.Entered(Context: TMoveContext);
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
procedure TSquare.Exited(Context: TMoveContext);
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
procedure TSquare.Execute(Context: TMoveContext);
begin
  if Assigned(Tool) then
    Tool.Find(Context);
  if Assigned(Effect) then
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
procedure TSquare.Pushing(Context: TMoveContext);
begin
  if Assigned(Obstacle) then
    Obstacle.Pushing(Context);
end;

{-------------}
{ Classe TMap }
{-------------}

{*
  Cr�e une instance de TMap vide
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de la carte
*}
constructor TMap.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited Create(AMaster, AID);

  FMaxViewSize := MinViewSize;
end;

{*
  Cr�e une instance de TMap
  @param AMaster       Ma�tre FunLabyrinthe
  @param AID           ID de la carte
  @param ADimensions   Dimensions de la carte (en cases)
  @param AZoneSize     Taille d'une zone de la carte
*}
constructor TMap.Create(AMaster: TMaster; const AID: TComponentID;
  ADimensions: T3DPoint; AZoneWidth, AZoneHeight: Integer);
begin
  Create(AMaster, AID);

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
    Palette[I] := Master.Square[ReadStrFromStream(Stream)];

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
        WriteStrToStream(Stream, ID);
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
  [@inheritDoc]
*}
constructor TLabyrinthPlayerMode.Create(APlayer: TPlayer);
begin
  inherited;

  FCacheBitmap := TBitmap32.Create;
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
  Invalide enti�rement la cache
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

    FCacheBitmap.SetSize(ZoneWidth * SquareSize, ZoneHeight * SquareSize);
  end;
end;

{*
  Invalide enti�rement la cache si n�cessaire
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
  Met � jour la cache
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
          DrawSquareContext := TDrawSquareContext.Create(FCacheBitmap,
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
        DrawInPlace(Context.Bitmap,
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
  Context.Bitmap.Draw(0, 0, FCacheBitmap);

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
  Cr�e une instance de TPlayer
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du joueur
  @param AName     Nom du joueur
*}
constructor TPlayer.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  FStaticDraw := False;
  FMode := TLabyrinthPlayerMode.Create(Self);
  FModeStack := TInterfaceList.Create;
  FColor := DefaultPlayerColor;
  FViewBorderSize := DefaultViewBorderSize;
  FPlugins := TObjectList.Create(False);
  FAttributes := THashedStringList.Create;
  TStringList(FAttributes).CaseSensitive := True;

  FLock := TMultiReadExclusiveWriteSynchronizer.Create;

  FActionCoroutine := TCoroutine.Create(ActionProc, clNextInvoke);
end;

{*
  [@inheritDoc]
*}
destructor TPlayer.Destroy;
begin
  FActionCoroutine.Free;

  FLock.Free;

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
begin
  FLock.BeginWrite;
  try
    FPlugins.Clear;

    Remaining := Value;
    while Remaining <> '' do
    begin
      SplitToken(Remaining, ' ', PluginID, Temp);
      Remaining := Temp;
      FPlugins.Add(Master.Plugin[PluginID]);
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
  D�place le joueur
  @param Context   Contexte de d�placement
  @param Execute   Indique s'il faut ex�cuter la case d'arriv�e (d�faut = True)
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
  Proc�dure de la coroutine d'appui sur touche
  @param Coroutine   Objet coroutine g�rant cette proc�dure
*}
procedure TPlayer.ActionProc(Coroutine: TCoroutine);
var
  Context: TKeyEventContext;
  Plugins: TPluginDynArray;
  I: Integer;
begin
  if FActionIsMessage then
  begin
    // FActionMessagePtr has been set before this method is called.
    Dispatch(FActionMessagePtr^);
  end else
  begin
    { FKeyPressKey and FKeyPressShift have been set before this method is
      called. }

    Context := TKeyEventContext.Create(Self, FActionKey, FActionKeyShift);
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
procedure TPlayer.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineFieldProperty('Map', TypeInfo(TMap), @FMap, FMap <> nil);

  Filer.DefineFieldProperty('Position.X', TypeInfo(Integer),
    @FPosition.X, FMap <> nil);
  Filer.DefineFieldProperty('Position.Y', TypeInfo(Integer),
    @FPosition.Y, FMap <> nil);
  Filer.DefineFieldProperty('Position.Z', TypeInfo(Integer),
    @FPosition.Z, FMap <> nil);

  Filer.DefineFieldProperty('ShowCounter', TypeInfo(Integer),
    @FShowCounter, FShowCounter <> 0);

  Filer.DefineFieldProperty('PlayState', TypeInfo(TPlayState),
    @FPlayState, PlayState <> psPlaying);

  Filer.DefineStrings('Attributes', FAttributes, TypeInfo(Integer));

  Filer.DefineProcProperty('Plugins', TypeInfo(string),
    @TPlayer.GetPluginListStr, @TPlayer.SetPluginListStr);

  Filer.DefineProcProperty('Modes', TypeInfo(string),
    @TPlayer.GetModeListStr, @TPlayer.SetModeListStr);
end;

{*
  [@inheritDoc]
*}
procedure TPlayer.DoDraw(Context: TDrawSquareContext);
var
  Color: TColor32;
begin
  Color := FColor;

  if Color <> clTransparent32 then
  begin
    with Context, Bitmap.Canvas do
    begin
      Brush.Color := WinColor(Color);
      Brush.Style := bsSolid;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+SquareSize-6, Y+SquareSize-6);
    end;
  end;
end;

{*
  Notification que la position a chang�
*}
procedure TPlayer.PositionChanged;
begin
  if (Map <> nil) and (ViewBorderSize > Map.MaxViewSize) then
    ViewBorderSize := Map.MaxViewSize;
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
  @param Attributes   Liste de cha�nes dans laquelle enregistrer les attributs
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
  Dessine le joueur sur un canevas
  DrawInPlace dessine le joueur sur un canevas � la position indiqu�e, avec pour
  position qualifi�e sa position actuelle.
  @param Bitmap   Bitmap sur lequel dessiner le joueur
  @param X        Coordonn�e X du point � partir duquel dessiner le joueur
  @param Y        Coordonn�e Y du point � partir duquel dessiner le joueur
*}
procedure TPlayer.DrawInPlace(Bitmap: TBitmap32; X: Integer = 0;
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

  Draw(QPos, Bitmap, X, Y);
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
  FLock.BeginWrite;
  try
    FPlugins.Remove(Plugin);
  finally
    FLock.EndWrite;
  end;
end;

{*
  Indique si le joueur est capable d'effectuer une action donn�e
  Un joueur est capable d'effectuer une action si l'un de ses plug-in ou l'un de
  ses objets le lui permet.
  @param Action   Action � tester
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
  Ex�cute l'action sp�cifi�e
  DoAction v�rifie d'abord que le joueur en est bien capable. Et si plusieurs
  objets permettent d'effectuer l'action, le joueur se voit demander d'en
  choisir un.
  @param Action   Action � tester
  @return True si le joueur a �t� capable d'effectuer l'action, False sinon
*}
function TPlayer.DoAction(const Action: TPlayerAction): Boolean;
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
    GoodObject.UseFor(Self, Action);
  finally
    FLock.EndWrite;
  end;
end;

{*
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir          Direction du d�placement
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Redo         Indique s'il faut r�it�rer le d�placement
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
  D�place le joueur
  @param Dest      Position de destination
  @param Execute   Indique s'il faut ex�cuter la case d'arriv�e
  @param Redo      Indique s'il faut r�it�rer le d�placement
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
  D�place le joueur
  Cette variante de MoveTo n'ex�cute pas la case d'arriv�e
  @param Dest   Position de destination
*}
procedure TPlayer.MoveTo(const Dest: T3DPoint);
var
  Redo: Boolean;
begin
  MoveTo(Dest, False, Redo);
end;

{*
  D�place le joueur
  @param Dest      Position de destination
  @param Execute   Indique s'il faut ex�cuter la case d'arriv�e
  @param Redo      Indique s'il faut r�it�rer le d�placement
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
  D�place le joueur
  Cette variante de MoveTo n'ex�cute pas la case d'arriv�e
  @param Dest   Position de destination
*}
procedure TPlayer.MoveTo(const Dest: TQualifiedPos);
var
  Redo: Boolean;
begin
  MoveTo(Dest, False, Redo);
end;

{*
  Temporise l'ex�cution
*}
procedure TPlayer.Temporize;
begin
  Master.Temporize;
end;

{*
  D�placement naturel, selon le mouvement d�j� entam� (sorte d'inertie)
  Apr�s un mouvement donn� express�ment, si Redo vaut True, il suffit d'appeler
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
  ChangePosition ne doit �tre utilis�e qu'en mode �dition, ou sous r�serve
  d'�tre certain de ce qu'on fait.
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
  @param Command   Commande � envoyer
  @param Params    Param�tres de la commande
  @return R�sultat de la commande
  @throws EUnsupportedCommand : La commande demand�e n'est pas support�e
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
  Affiche une bo�te de dialogue
  @param Title        Titre de la bo�te de dialogue
  @param Text         Texte de la bo�te de dialogue
  @param DlgType      Type de bo�te de dialogue
  @param DlgButtons   Boutons pr�sents dans la bo�te de dialogue
  @param DefButton    Bouton s�lectionn� par d�faut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Bouton sur lequel l'utilisateur a cliqu�
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
  Affiche une bo�te de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title         Titre de la bo�te de dialogue
  @param Text          Texte de la bo�te de dialogue
  @param DlgType       Type de bo�te de dialogue
  @param DlgButtons    Boutons pr�sents dans la bo�te de dialogue
  @param DefButton     Bouton s�lectionn� par d�faut
  @param RadioTitles   Libell�s des diff�rents boutons radio
  @param Selected      Bouton radio s�lectionn�
  @param OverButtons   Boutons radio plac�s au-dessus des boutons si True
  @return Bouton sur lequel l'utilisateur a cliqu�
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
  @param Title     Titre de la bo�te de dialogue
  @param Prompt    Invite
  @param Default   Valeur par d�faut affich�e
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
begin
  Assert(not FActionCoroutine.CoroutineRunning);

  FActionIsMessage := False;
  FActionKey := Key;
  FActionKeyShift := Shift;

  try
    FActionCoroutine.Invoke;
  except
    FActionCoroutine.Reset;
    raise;
  end;
end;

{*
  Envoie un message au joueur dans le contexte de ses actions
  @param Msg   Message � envoyer
*}
procedure TPlayer.SendMessage(var Msg);
begin
  Assert(not FActionCoroutine.CoroutineRunning);

  FActionIsMessage := True;
  FActionMessagePtr := @Msg;

  try
    FActionCoroutine.Invoke;
  except
    FActionCoroutine.Reset;
    raise;
  end;
end;

{*
  Attend qu'une touche soit press�e par le joueur
  Cette m�thode ne peut �tre appel�e que depuis le code d'action du joueur !
  @param Key     En sortie : Touche appuy�e
  @param Shift   En sortie : �tat des touches sp�ciales lors de l'appui
*}
procedure TPlayer.WaitForKey(out Key: Word; out Shift: TShiftState);
begin
  Assert(FActionCoroutine.CoroutineRunning);

  FActionCoroutine.Yield;

  Key := FActionKey;
  Shift := FActionKeyShift;
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

{----------------}
{ Classe TMaster }
{----------------}

{*
  Cr�e une instance de TMaster
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
  D�truit l'instance
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
  Tableau des composants index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�, ou nil si ID �tait vide
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
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
  Tableau des composants de case index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas de case
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
  Tableau des plug-in index� par leur ID
  @param ID   ID du plug-in � trouver
  @return Le plug-in dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun plug-in ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un plug-in
*}
function TMaster.GetPlugin(const ID: TComponentID): TPlugin;
begin
  Result := Component[ID] as TPlugin;
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
  Result := Component[ID] as TObjectDef;
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
  Result := Component[ID] as TField;
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
  Result := Component[ID] as TEffect;
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
  Result := Component[ID] as TTool;
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
  Result := Component[ID] as TObstacle;
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
  Tableau des cartes index� par leur ID
  @param ID   ID de la carte � trouver
  @return La carte dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune carte ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas une carte
*}
function TMaster.GetMap(const ID: TComponentID): TMap;
begin
  Result := Component[ID] as TMap;
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
  Result := Component[ID] as TPlayer;
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
  Result := TFunLabyComponent(FComponents.Objects[Index]);
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
  @param Component   Le composant � retirer
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
end;

{*
  Temporise l'ex�cution
*}
procedure TMaster.Temporize;
begin
  Sleep(Temporization);
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

initialization
  Randomize;

  FunLabyRegisterClass(TLabyrinthPlayerMode);

  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    fFunLabyAppData :=
      ReadString('Directories', 'AppData', Dir); {don't localize}

    fSquaresDir := fFunLabyAppData + fSquaresDir;
    fSoundsDir := fFunLabyAppData + fSoundsDir;
    fUnitsDir := fFunLabyAppData + fUnitsDir;
    fLabyrinthsDir := fFunLabyAppData + fLabyrinthsDir;
    fSaveguardsDir := fFunLabyAppData + fSaveguardsDir;

    fEditPluginDir := Dir + fEditPluginDir;

    fSquareFileName := fSquaresDir+fSquareFileName;
  finally
    Free;
  end;
finalization
  FreeAndNil(FunLabyRegisteredClasses);
end.

