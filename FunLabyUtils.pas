{*
  Types et classes de bases de FunLabyrinthe
  FunLabyUtils comprend les types et classes de base de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, Controls, IniFiles, ScUtils,
  Forms, Dialogs, StdCtrls, Math, ScStrUtils, SdDialogs;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sEffectName = '%1:s sur %0:s';
  sToolName = '%s avec %s';
  sObstacleName = '%s obstru� par %s';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';
  sComponentNotFound = 'Le composant d''ID %s n''existe pas';
  sCantLaunchThroughNetwork = 'FunLabyrinthe doit �tre install� sur '+
    'l''ordinateur local pour fonctionner';

  sDescription = 'Description';
  sMessage = 'Message';
  sTip = 'Indice';
  sChoice = 'Choix';
  sError = 'Erreur';
  sFailure = '�chec';
  sBlindAlley = 'Impasse';
  sWon = 'Gagn� !';
  sLost = 'Perdu !';

const {don't localize}
  CurrentVersion = '5.0'; /// Version courante de FunLabyrinthe
  FunLabyAuthorName = 'S�bastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://sjrd.developpez.com/programmes/funlaby/'; /// Site

  ScrewSize = 30;         /// Taille (en largeur et hauteur) d'une case
  MinViewSize = 1;        /// Taille minimale d'une vue
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type repr�sentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Type repr�sentant une action
  TPlayerAction = type string;

  /// �tat de victoire/d�faite d'un joueur
  TPlayState = (psPlaying, psWon, psLost);

  /// G�n�r�e si un composant recherch� n'est pas trouv�
  EComponentNotFound = class(Exception);

  TScrewComponent = class;
  TPlayer = class;
  TMap = class;
  TMaster = class;

  {*
    Position qualifi�e, compos�e d'une carte et d'une position sur la carte
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TQualifiedPos = record
    Map : TMap;          /// Carte, ou nil pour une position nulle
    Position : T3DPoint; /// Position sur la carte, si Map <> nil
  end;

  {*
    Type de m�thode call-back pour l'enregistrement d'un unique composant
    @param Component   Le composant � enregistrer
  *}
  TRegisterSingleComponentProc = procedure(
    Component : TScrewComponent) of object; stdcall;

  {*
    Type de m�thode call-back pour l'enregistrement d'un ensemble de composants
    @param Template       Composant mod�le pour l'image et le nom � afficher
    @param Components     Liste des composants faisant partie de l'ensemble
    @param DialogTitle    Titre de la bo�te de dialogue du choix du num�ro
    @param DialogPrompt   Invite de la bo�te de dialogue du choix du num�ro
  *}
  TRegisterComponentSetProc = procedure(Template : TScrewComponent;
    const Components : array of TScrewComponent; BaseIndex : integer;
    const DialogTitle, DialogPrompt : string) of object; stdcall;

  {*
    G�re le chargement des images d'apr�s leur nom
    TImagesMaster s'occupe de charger automatiquement les images qu'on lui
    demande d'afficher. Il les conserve dans une liste d'image.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
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
    Bitmap de la taille d'une case, g�rant automatiquement la transparence
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TScrewBitmap = class(TBitmap)
  public
    constructor Create; override;

    procedure EmptyScrew;
    procedure DrawScrew(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
  end;

  {*
    Enregistre est affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une m�thode
    pour les dessiner les unes sur les autres, par transparence.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TPainter = class
  private
    FMaster : TImagesMaster;   /// Ma�tre d'images
    FImgNames : TStrings;      /// Liste des noms des images
    FCachedImg : TScrewBitmap; /// Copie cache de l'image r�sultante

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
    FunLabyrinthe. Elle fournit des propri�t�s et des m�thodes pour rep�rer le
    ma�tre FunLabyrinthe et pour identifier le composant.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TFunLabyComponent = class
  private
    FMaster : TMaster;  /// Ma�tre FunLabyrinthe
    FID : TComponentID; /// ID du composant
    {*
      Valeur non fonctionnelle pouvant servir au fonctionnement d'un algorithme
      Cette valeur est susceptible d'�tre utilis�e par beaucoup d'algorithmes
      diff�rents, et donc interf�rer. Il ne faut donc l'utiliser que
      ponctuellement.
    *}
    FTag : integer;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID);
    destructor Destroy; override;

    property Master : TMaster read FMaster;
    property ID : TComponentID read FID;
    property Tag : integer read FTag write FTag;
  end;

  {*
    Classe de base pour les composants devant �tre affich�s
    TVisualComponent �tend la classe TFunLabyComponent pour lui ajouter un
    traitement standart et simple de nommage et de dessin.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName : string;            /// Nom du composant
    FPainter : TPainter;       /// Peintre par d�faut
    FCachedImg : TScrewBitmap; /// Image en cache (pour les dessins invariants)
  protected
    FStaticDraw : boolean; /// Indique si le dessin du composant est invariant

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;

    property Painter : TPainter read FPainter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0);

    property Name : string read FName;
    property StaticDraw : boolean read FStaticDraw;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs fa�ons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Emp�cher le d�placement du joueur et r�agir � son d�placement effectif ;
    - Indiquer au joueur qu'il a la capacit� de faire certaines actions.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlugin = class(TFunLabyComponent)
  private
    FPainterBefore : TPainter; /// Peintre par d�faut sous le joueur
    FPainterAfter : TPainter;  /// Peintre par d�faut sur le joueur
  protected
    property PainterBefore : TPainter read FPainterBefore;
    property PainterAfter : TPainter read FPainterAfter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); virtual;
    procedure DrawAfter(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); virtual;

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Dest : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); virtual;

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; virtual;
  end;

  {*
    Classe de base pour les d�finitions d'objets
    TObjectDef est la classe de base pour les d�finitions d'objets que poss�de
    le joueur.
    Les objets peuvent rendre un joueur capable d'effectuer certaines actions.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TObjectDef = class(TVisualComponent)
  protected
    function GetCount(Player : TPlayer) : integer; virtual;
    procedure SetCount(Player : TPlayer; Value : integer); virtual;

    function GetShownInfos(Player : TPlayer) : string; virtual;
  public
    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; virtual;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); virtual;

    property Count[Player : TPlayer] : integer read GetCount write SetCount;
    property ShownInfos[Player : TPlayer] : string read GetShownInfos;
  end;

  {*
    Classe de base pour les composants de case
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TScrewComponent = class(TVisualComponent)
  end;

  {*
    Classe de base pour les terrains
    TField est la classe de base pour la cr�ation de terrains. Les terrains
    sont la premi�re composante d'une case.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TField = class(TScrewComponent)
  private
    FDelegateDrawTo : TField; /// Terrain d�l�gu� pour l'affichage
  protected
    procedure DrawField(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Pos, Dest : T3DPoint;
      var Cancel : boolean); virtual;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      const Src, Pos : T3DPoint); virtual;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      const Pos, Dest : T3DPoint); virtual;
  end;

  {*
    Classe de base pour les effets de case
    TEffect est la classe de base pour la cr�ation d'effets de case. Les effets
    sont la deuxi�me composante d'une case.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TEffect = class(TScrewComponent)
  public
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      const Src, Pos : T3DPoint); virtual;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      const Pos, Dest : T3DPoint); virtual;

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); virtual;
  end;

  {*
    Effet d�coratif
    La classe TDecorativeEffect permet de cr�er facilement un effet qui ne fait
    rien, qui ajoute juste une touche d�corative.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TDecorativeEffect = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName, AImgName : string);
  end;

  {*
    Classe de base pour les outils
    TTool est la classe de base pour la cr�ation d'outils. Les outils sont la
    troisi�me composante d'une case.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TTool = class(TScrewComponent)
  public
    procedure Find(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint); virtual;
  end;

  {*
    Outil li� � une d�finition d'objet
    La classe TObjectTool permet de cr�er facilement un outil qui est li� � une
    d�finition d'objet. C'est-�-dire que trouver cet outil incr�mente le nombre
    de tels objets que poss�de le joueur
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TObjectTool = class(TTool)
  private
    FObjectDef : TObjectDef; /// D�finition d'objet li�e
    FFindMessage : string;   /// Message apparaissant lorsqu'on trouve l'outil
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      AObjectDef : TObjectDef; const AFindMessage : string;
      const AName : string = ''; const AImgName : string = '');

    procedure Find(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint); override;

    property ObjectDef : TObjectDef read FObjectDef;
    property FindMessage : string read FFindMessage;
  end;

  {*
    Classe de base pour les obstacles
    TObstacle est la classe de base pour la cr�ation d'obstacles. Les obstacles
    sont la quatri�me composante d'une case.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TObstacle = class(TScrewComponent)
  public
    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); virtual;
  end;

  {*
    Repr�sente une case du jeu
    TScrew repr�sente une case du jeu. Une case poss�de deux parties : le
    terrain et l'effet.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TScrew = class(TScrewComponent)
  private
    FField : TField;       /// Terrain
    FEffect : TEffect;     /// Effet
    FTool : TTool;         /// Outil
    FObstacle : TObstacle; /// Obstacle
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AField : TField; AEffect : TEffect; ATool : TTool;
      AObstacle : TObstacle);

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean);
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Pos, Dest : T3DPoint;
      var Cancel : boolean);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      const Src, Pos : T3DPoint);
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      const Pos, Dest : T3DPoint);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean);

    function ChangeField(NewField : TComponentID) : TScrew;
    function ChangeEffect(NewEffect : TComponentID = '') : TScrew;
    function ChangeTool(NewTool : TComponentID = '') : TScrew;
    function ChangeObstacle(NewObstacle : TComponentID = '') : TScrew;

    property Field : TField read FField;
    property Effect : TEffect read FEffect;
    property Tool : TTool read FTool;
    property Obstacle : TObstacle read FObstacle;
  end;

  {*
    Classe de base pour les cases en � surchargeant � d'autres
    TOverriddenScrew est la classe de base pour les cases sp�ciales qui
    surchargent momentan�ment une autre case. Elle fournit des propri�t�s et
    m�thodes pour identifier la case en question et la dessiner.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TOverriddenScrew = class(TScrew)
  private
    FMap : TMap;             /// Carte
    FPosition : T3DPoint;    /// Position
    FOriginalScrew : TScrew; /// Case originale
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      AMap : TMap; APosition : T3DPoint; AField : TField; AEffect : TEffect;
      ATool : TTool; AObstacle : TObstacle);
    destructor Destroy; override;

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    property Map : TMap read FMap;
    property Position : T3DPoint read FPosition;
    property OriginalScrew : TScrew read FOriginalScrew;
  end;

  {*
    Repr�sente la carte du jeu
    TMap g�re et repr�sente la carte du jeu. Elle offre des propri�t�s et
    m�thodes pour lire et modifier cette carte.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TMap = class(TFunLabyComponent)
  private
    FDimensions : T3DPoint;   /// Dimensions de la carte (en cases)
    FZoneWidth : integer;     /// Largeur d'une zone de la carte
    FZoneHeight : integer;    /// Hauteur d'une zone de la carte
    FMaxViewSize : integer;   /// Taille maximale d'une vue pour cette carte
    FMap : array of TScrew;   /// Carte stock�e de fa�on lin�aire
    FOutsideOffset : integer; /// Offset de d�part de l'ext�rieur

    procedure SetMaxViewSize(Value : integer);

    function GetMap(const Position : T3DPoint) : TScrew;
    procedure SetMap(const Position : T3DPoint; Value : TScrew);

    function GetOutside(Floor : integer) : TScrew;
    procedure SetOutside(Floor : integer; Value : TScrew);

    function GetLinearMapCount : integer;
    function GetLinearMap(Index : integer) : TScrew;
    procedure SetLinearMap(Index : integer; Value : TScrew);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      ADimensions : T3DPoint; AZoneWidth, AZoneHeight : integer);

    function InMap(const Position : T3DPoint) : boolean;

    function PlayersOn(const Position : T3DPoint) : integer;

    property Dimensions : T3DPoint read FDimensions;
    property ZoneWidth : integer read FZoneWidth;
    property ZoneHeight : integer read FZoneHeight;
    property MaxViewSize : integer read FMaxViewSize write SetMaxViewSize;

    property Map[const Position : T3DPoint] : TScrew
      read GetMap write SetMap; default;

    property Outside[Floor : integer] : TScrew
      read GetOutside write SetOutside;

    property LinearMapCount : integer read GetLinearMapCount;
    property LinearMap[index : integer] : TScrew
      read GetLinearMap write SetLinearMap;
  end;

  {*
    Interface de contr�le thread-safe d'un joueur
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  IPlayerController = interface
    {*
      Affiche une bo�te de dialogue
      @param Title        Titre de la bo�te de dialogue
      @param Text         Texte de la bo�te de dialogue
      @param DlgType      Type de bo�te de dialogue
      @param DlgButtons   Boutons pr�sents dans la bo�te de dialogue
      @param DefButton    Bouton s�lectionn� par d�faut
      @param AddFlags     Flags additionnels pour MessageBox
      @return Code de r�sultat du bouton cliqu�
    *}
    function ShowDialog(const Title, Text : string;
      DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
      DefButton : Byte = 1;
      AddFlags : LongWord = 0) : TDialogResult;

    {*
      Affiche une bo�te de dialogue avec des boutons radio
      ShowDialogRadio est une variante de ShowDialog qui affiche des boutons
      radio pour chaque choix possible.
      @param Title         Titre de la bo�te de dialogue
      @param Text          Texte de la bo�te de dialogue
      @param DlgType       Type de bo�te de dialogue
      @param DlgButtons    Boutons pr�sents dans la bo�te de dialogue
      @param DefButton     Bouton s�lectionn� par d�faut
      @param RadioTitles   Libell�s des diff�rents boutons radio
      @param Selected      Bouton radio s�lectionn�
      @param OverButtons   Boutons radio plac�s au-dessus des boutons si True
      @return Code de r�sultat du bouton cliqu�
    *}
    function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
      DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
      const RadioTitles : array of string; var Selected : integer;
      OverButtons : boolean = False) : Word;

    {*
      Affiche une invite au joueur lui demandant de choisir un nombre
      @param Title     Titre de la bo�te de dialogue
      @param Prompt    Invite
      @param Default   Valeur par d�faut affich�e
      @param Min       Valeur minimale que peut choisir le joueur
      @param Max       Valeur maximale que peut choisir le joueur
      @return La valeur qu'a choisie le joueur
    *}
    function ChooseNumber(const Title, Prompt : string;
      Default, Min, Max : integer) : integer;

    {*
      Le joueur a chang� de carte
    *}
    procedure MapChanged;
  end;

  {*
    Classe repr�sentant un joueur
    TPlayer repr�sente un joueur. Elle poss�de de nombreuses propri�t�s et
    m�thodes permettant d'afficher le joueur, de le d�placer, de lui greffer
    des plug-in, etc.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlayer = class(TVisualComponent)
  private
    FMap : TMap;                     /// Carte
    FPosition : T3DPoint;            /// Position
    FDirection : TDirection;         /// Direction
    /// Peintres selon la direction du joueur
    FDirPainters : array[diNorth..diWest] of TPainter;
    FColor : TColor;                 /// Couleur
    FPlugins : TObjectList;          /// Liste des plug-in
    FAttributes : TStrings;          /// Liste des attributs
    FController : IPlayerController; /// Contr�leur
    FPlayState : TPlayState;         /// �tat de victoire/d�faite

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlugin;

    function GetAttribute(const AttrName : string) : integer;
    procedure SetAttribute(const AttrName : string; Value : integer);

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlugin read GetPlugins;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AMap : TMap; APosition : T3DPoint);
    destructor Destroy; override;

    procedure GetAttributes(Attributes : TStrings);
    procedure GetPluginIDs(PluginIDs : TStrings);

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;
    procedure DrawInPlace(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0);

    procedure AddPlugin(Plugin : TPlugin);
    procedure RemovePlugin(Plugin : TPlugin);

    function CanYou(const Action : TPlayerAction) : boolean;

    function Move(Dir : TDirection; KeyPressed : boolean;
      out Redo : boolean) : boolean;
    procedure ChangeMap(AMap : TMap; APosition : T3DPoint);

    procedure Win;
    procedure Lose;

    property Map : TMap read FMap;
    property Position : T3DPoint read FPosition write FPosition;
    property Direction : TDirection read FDirection write FDirection;
    property Color : TColor read FColor write FColor;
    property Attribute[const AttrName : string] : integer
      read GetAttribute write SetAttribute;
    property Controller : IPlayerController read FController write FController;
    property PlayState : TPlayState read FPlayState;
  end;

  {*
    Ma�tre FunLabyrinthe
    TMaster g�re les diff�rents composants de FunLabyrinthe.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TMaster = class
  private
    FImagesMaster : TImagesMaster; /// Ma�tre d'images
    FComponents : TStrings;        /// Table de hashage ID -> composant
    FPlugins : TObjectList;        /// Liste des plug-in
    FObjectDefs : TObjectList;     /// Liste des d�finitions d'objet
    FFields : TObjectList;         /// Liste des terrains
    FEffects : TObjectList;        /// Liste des effets
    FTools : TObjectList;          /// Liste des outils
    FObstacles : TObjectList;      /// Liste des obstacles
    FScrews : TObjectList;         /// Liste des cases
    FMaps : TObjectList;           /// Liste des cartes
    FPlayers : TObjectList;        /// Liste des joueurs

    FEditing : boolean;            /// Indique si on est en mode �dition
    FTemporization : integer;      /// Temporisation en millisecondes
    FBeginTickCount : Cardinal;    /// Tick count syst�me au lancement
    FTickCount : Cardinal;         /// Tick count de la partie
    FTerminated : boolean;         /// Indique si la partie est termin�e

    function GetComponent(const ID : TComponentID) : TFunLabyComponent;
    function GetScrewComponent(const ID : TComponentID) : TScrewComponent;

    function GetPlugin   (const ID : TComponentID) : TPlugin;
    function GetObjectDef(const ID : TComponentID) : TObjectDef;
    function GetField    (const ID : TComponentID) : TField;
    function GetEffect   (const ID : TComponentID) : TEffect;
    function GetTool     (const ID : TComponentID) : TTool;
    function GetObstacle (const ID : TComponentID) : TObstacle;
    function GetScrew    (const ID : TComponentID) : TScrew;
    function GetMap      (const ID : TComponentID) : TMap;
    function GetPlayer   (const ID : TComponentID) : TPlayer;

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlugin;
    function GetObjectDefCount : integer;
    function GetObjectDefs(Index : integer) : TObjectDef;
    function GetFieldCount : integer;
    function GetFields(Index : integer) : TField;
    function GetEffectCount : integer;
    function GetEffects(Index : integer) : TEffect;
    function GetToolCount : integer;
    function GetTools(Index : integer) : TTool;
    function GetObstacleCount : integer;
    function GetObstacles(Index : integer) : TObstacle;
    function GetScrewCount : integer;
    function GetScrews(Index : integer) : TScrew;
    function GetMapCount : integer;
    function GetMaps(Index : integer) : TMap;
    function GetPlayerCount : integer;
    function GetPlayers(Index : integer) : TPlayer;

    procedure SetTemporization(Value : integer);

    procedure AddComponent(Component : TFunLabyComponent);
    procedure RemoveComponent(Component : TFunLabyComponent);

    procedure Terminate;
  public
    constructor Create(AEditing : boolean);
    destructor Destroy; override;

    procedure Temporize;
    procedure UpdateTickCount;

    property ImagesMaster : TImagesMaster read FImagesMaster;

    property Component[const ID : TComponentID] : TFunLabyComponent
      read GetComponent;
    property ScrewComponent[const ID : TComponentID] : TScrewComponent
      read GetScrewComponent;
    property Plugin   [const ID : TComponentID] : TPlugin    read GetPlugin;
    property ObjectDef[const ID : TComponentID] : TObjectDef read GetObjectDef;
    property Field    [const ID : TComponentID] : TField     read GetField;
    property Effect   [const ID : TComponentID] : TEffect    read GetEffect;
    property Tool     [const ID : TComponentID] : TTool      read GetTool;
    property Obstacle [const ID : TComponentID] : TObstacle  read GetObstacle;
    property Screw    [const ID : TComponentID] : TScrew     read GetScrew;
    property Map      [const ID : TComponentID] : TMap       read GetMap;
    property Player   [const ID : TComponentID] : TPlayer    read GetPlayer;

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlugin read GetPlugins;
    property ObjectDefCount : integer read GetObjectDefCount;
    property ObjectDefs[index : integer] : TObjectDef read GetObjectDefs;
    property FieldCount : integer read GetFieldCount;
    property Fields[index : integer] : TField read GetFields;
    property EffectCount : integer read GetEffectCount;
    property Effects[index : integer] : TEffect read GetEffects;
    property ToolCount : integer read GetToolCount;
    property Tools[index : integer] : TTool read GetTools;
    property ObstacleCount : integer read GetObstacleCount;
    property Obstacles[index : integer] : TObstacle read GetObstacles;
    property ScrewCount : integer read GetScrewCount;
    property Screws[index : integer] : TScrew read GetScrews;
    property MapCount : integer read GetMapCount;
    property Maps[index : integer] : TMap read GetMaps;
    property PlayerCount : integer read GetPlayerCount;
    property Players[index : integer] : TPlayer read GetPlayers;

    property Editing : boolean read FEditing;
    property Temporization : integer read FTemporization write SetTemporization;
    property TickCount : Cardinal read FTickCount;
    property Terminated : boolean read FTerminated;
  end;

const {don't localize}
  /// Fichier INI de FunLabyrinthe
  fIniFileName = 'FunLabyrinthe.ini';

  /// Position qualifi�e nulle
  NoQPos : TQualifiedPos = (Map : nil; Position : (X : 0; Y : 0; Z : 0));

  /// Temporisation par d�faut
  DefaultTemporization = 500;

  /// Application d'une direction vers la direction oppos�e
  NegDir : array[TDirection] of TDirection = (
    diNone, diSouth, diWest, diNorth, diEast
  );

var {don't localize}
  /// Dossier de FunLabyrinthe dans Application Data
  fFunLabyAppData : string = '';
  /// Dossier des fichiers image
  fScrewsDir : string = 'Screws\';
  /// Dossier des fichiers son
  fSoundsDir : string = 'Sounds\';
  /// Dossier des unit�s
  fUnitsDir : string = 'Units\';
  /// Dossier des cartes
  fMapsDir : string = 'Maps\';
  /// Dossier des fichiers labyrinthe
  fLabyrinthsDir : string = 'Labyrinths\';
  /// Dossier des fichiers sauvegarde
  fSaveguardsDir : string = 'Saveguards\';

  /// Cha�ne de format pour les fichiers image
  fScrewFileName : string = '%s.bmp';

function CheckValidLaunch : boolean;
procedure ShowFunLabyAbout;

function PointBehind(const Src : T3DPoint; Dir : TDirection) : T3DPoint;
function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;
procedure EmptyRect(Canvas : TCanvas; Rect : TRect);
procedure EmptyScrewRect(Canvas : TCanvas; X : integer = 0; Y : integer = 0);

function IsNoQPos(const QPos : TQualifiedPos) : boolean;

implementation

{*
  V�rifie que FunLabyrinthe a �t� lanc� de fa�on valide
  FunLabyrinthe est lanc� de fa�on valide lorsque l'ex�cutable se situe sur
  l'ordinateur local. Il est en effet impossible, sinon, d'acc�der aux
  packages Delphi et autres ressources non partageables.
  @return True si FunLabyrinthe a �t� lanc� de fa�on valide, False sinon
*}
function CheckValidLaunch : boolean;
begin
  if (Dir[1] = PathDelim) and (Dir[2] = PathDelim) then
  begin
    ShowDialog(sError, sCantLaunchThroughNetwork, dtError);
    Result := False;
  end else
  Result := True;
end;

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
function PointBehind(const Src : T3DPoint; Dir : TDirection) : T3DPoint;
begin
  Result := Src;
  case Dir of
    diNorth : dec(Result.Y);
    diEast  : inc(Result.X);
    diSouth : inc(Result.Y);
    diWest  : dec(Result.X);
  end;
end;

{*
  Cr�e un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord sup�rieur du rectangle
  @return Le rectangle de type TRect
*}
function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X+ScrewSize;
  Result.Bottom := Y+ScrewSize;
end;

{*
  Efface un rectangle sur un canevas (avec du transparent)
  @param Canvas   Canevas � traiter
  @param Rect     Rectangle � effacer
*}
procedure EmptyRect(Canvas : TCanvas; Rect : TRect);
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
  @param Canvas   Canevas � traiter
  @param X        Bord gauche du rectangle
  @param Y        Bord sup�rieur du rectangle
*}
procedure EmptyScrewRect(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  EmptyRect(Canvas, ScrewRect(X, Y));
end;

{*
  D�termine si une position qualifi�e est nulle
  @param QPos   Position � tester
  @return True si la position qualifi�e QPos est nulle, False sinon
*}
function IsNoQPos(const QPos : TQualifiedPos) : boolean;
begin
  Result := QPos.Map = nil;
end;

{----------------------}
{ Classe TImagesMaster }
{----------------------}

{*
  Cr�e une instance de TImagesMaster
*}
constructor TImagesMaster.Create;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(ScrewSize, ScrewSize);
  FImgNames := THashedStringList.Create;
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
  Renvoie l'index de l'image dont le nom est sp�cifi�
  IndexOf renvoie l'index de l'image dont le nom est sp�cifi� dans la
  liste d'images interne. Si l'image n'a pas encore �t� charg�e, IndexOf
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
  Dessine une image � partir de son index
  Draw dessine l'image indiqu�e sur un canevas.
  @param Index    Index de l'image � dessiner
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonn�e X du point � partir duquel dessiner l'image
  @param Y        Coordonn�e Y du point � partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index : integer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FImgList.Draw(Canvas, X, Y, Index);
end;

{*
  Dessine une image � partir de son nom
  Draw dessine l'image indiqu�e sur un canevas.
  @param ImgName   Nom de l'image � dessiner
  @param Canvas    Canevas sur lequel dessiner l'image
  @param X         Coordonn�e X du point � partir duquel dessiner l'image
  @param Y         Coordonn�e Y du point � partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(const ImgName : string; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  Draw(IndexOf(ImgName), Canvas, X, Y);
end;

{---------------------}
{ Classe TScrewBitmap }
{---------------------}

{*
  Cr�e une instance de TScrewBitmap
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
  @param X        Coordonn�e X du point � partir duquel dessiner l'image
  @param Y        Coordonn�e Y du point � partir duquel dessiner l'image
*}
procedure TScrewBitmap.DrawScrew(Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
var OldBrushStyle : TBrushStyle;
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
  Cr�e une instance de TPainter
  @param AMaster   Ma�tre d'images associ� au peintre
*}
constructor TPainter.Create(AMaster : TImagesMaster);
begin
  inherited Create;

  FMaster := AMaster;
  FImgNames := TStringList.Create;
  TStringList(FImgNames).OnChange := ImgNamesChange;
  FCachedImg := TScrewBitmap.Create;
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
procedure TPainter.ImgNamesChange(Sender : TObject);
var I : integer;
begin
  FCachedImg.EmptyScrew;
  for I := 0 to FImgNames.Count-1 do
    FMaster.Draw(FImgNames[I], FCachedImg.Canvas);
end;

{*
  Dessine les images sur un canevas
  La m�thode Draw dessine les images de ImgNames sur le canevas, � la
  position indiqu�e. Les diff�rentes images sont superpos�e, celle d'index
  0 tout au-dessous.
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPainter.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  FCachedImg.DrawScrew(Canvas, X, Y);
end;

{--------------------------}
{ Classe TFunLabyComponent }
{--------------------------}

{*
  Cr�e une instance de TFunLabyComponent
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
*}
constructor TFunLabyComponent.Create(AMaster : TMaster;
  const AID : TComponentID);
begin
  inherited Create;
  FMaster := AMaster;
  FID := AID;
  Master.AddComponent(Self);
end;

{*
  D�truit l'instance
*}
destructor TFunLabyComponent.Destroy;
begin
  Master.RemoveComponent(Self);
  inherited;
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
constructor TVisualComponent.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID);

  FName := AName;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.ImgNames.BeginUpdate;
  FCachedImg := nil;
  FStaticDraw := True;
end;

{*
  D�truit l'instance
*}
destructor TVisualComponent.Destroy;
begin
  if Assigned(FCachedImg) then
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
begin
  inherited;
  FPainter.ImgNames.EndUpdate;

  if StaticDraw then
  begin
    FCachedImg := TScrewBitmap.Create;
    DoDraw(NoQPos, FCachedImg.Canvas);
  end;
end;

{*
  Dessine le composant sur un canevas
  DoDraw dessine le composant sur un canevas � la position indiqu�e.
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonn�e X du point � partir duquel dessiner le composant
  @param Y        Coordonn�e Y du point � partir duquel dessiner le composant
*}
procedure TVisualComponent.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

{*
  Dessine de fa�on optimis�e le composant sur un canevas
  Draw dessine le composant sur un canevas � la position indiqu�e.
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonn�e X du point � partir duquel dessiner le composant
  @param Y        Coordonn�e Y du point � partir duquel dessiner le composant
*}
procedure TVisualComponent.Draw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  if StaticDraw then
    FCachedImg.DrawScrew(Canvas, X, Y)
  else
    DoDraw(QPos, Canvas, X, Y);
end;

{----------------}
{ Classe TPlugin }
{----------------}

{*
  Cr�e une instance de TPlugin
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du plug-in
*}
constructor TPlugin.Create(AMaster : TMaster; const AID : TComponentID);
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
  @param Player   Joueur qui est dessin�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlugin.DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  FPainterBefore.Draw(Canvas, X, Y);
end;

{*
  Dessine sur le joueur
  DrawAfter est ex�cut� lors du dessin du joueur, apr�s celui-ci. Le dessin
  effectu� dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessin�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlugin.DrawAfter(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  FPainterAfter.Draw(Canvas, X, Y);
end;

{*
  Un joueur se d�place
  Moving est ex�cut� lorsqu'un joueur se d�place d'une case � une autre. Pour
  annuler le d�placement, Moving peut positionner le param�tre Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de d�part
  @param Dest           Case d'arriv�e
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de d�part
  @param Dest         Case d'arriv�e
*}
procedure TPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
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
function TPlugin.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := False;
end;

{-------------------}
{ Classe TObjectDef }
{-------------------}

{*
  Nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
  @return Nombre d'objets que ce joueur poss�de
*}
function TObjectDef.GetCount(Player : TPlayer) : integer;
begin
  Result := Player.Attribute[ID];
end;

{*
  Modifie le nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
  @param Value    Nouveau nombre d'objets
*}
procedure TObjectDef.SetCount(Player : TPlayer; Value : integer);
begin
  Player.Attribute[ID] := Value;
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TObjectDef.GetShownInfos(Player : TPlayer) : string;
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
function TObjectDef.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
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
procedure TObjectDef.UseFor(Player : TPlayer; const Action : TPlayerAction);
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
constructor TField.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName);
  FDelegateDrawTo := ADelegateDrawTo;
  if Assigned(FDelegateDrawTo) then
    FStaticDraw := FDelegateDrawTo.StaticDraw;
end;

{*
  Dessine le terrain sur le canevas indiqu�
  Les descendants de TField doivent r�impl�menter DrawField plut�t que DoDraw.
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TField.DrawField(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited DoDraw(QPos, Canvas, X, Y);
end;

{*
  Dessine le terrain sur le canevas indiqu�, ou d�l�gue le dessin
  Les descendants de TField ne doivent pas r�impl�menter DoDraw, mais DrawField
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TField.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  if FDelegateDrawTo = nil then
    DrawField(QPos, Canvas, X, Y)
  else
    FDelegateDrawTo.Draw(QPos, Canvas, X, Y);
end;

{*
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TField.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Ex�cut� lorsque le joueur tente de sortir de la case
  Exiting est ex�cut� lorsque le joueur tente de sortir de la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Pos            Position de la case
  @param Dest           Case de destination
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TField.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
*}
procedure TField.Entered(Player : TPlayer; KeyPressed : boolean;
  const Src, Pos : T3DPoint);
begin
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TField.Exited(Player : TPlayer; KeyPressed : boolean;
  const Pos, Dest : T3DPoint);
begin
end;

{----------------}
{ Classe TEffect }
{----------------}

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
*}
procedure TEffect.Entered(Player : TPlayer; KeyPressed : boolean;
  const Src, Pos : T3DPoint);
begin
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TEffect.Exited(Player : TPlayer; KeyPressed : boolean;
  const Pos, Dest : T3DPoint);
begin
end;

{*
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TEffect.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
end;

{--------------------------}
{ Classe TDecorativeEffect }
{--------------------------}

{*
  Cr�e une instance de TDecorativeEffect
  @param AMaster    Ma�tre FunLabyrinthe
  @param AID        ID de l'effet de case
  @param AName      Nom de l'effet de case
  @param AImgName   Nom du fichier image
*}
constructor TDecorativeEffect.Create(AMaster : TMaster;
  const AID : TComponentID; const AName, AImgName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(AImgName);
end;

{--------------}
{ Classe TTool }
{--------------}

{*
  Ex�cut� lorsque le joueur trouve l'outil
  Find est ex�cut� lorsque le joueur trouve l'outil. C'est-�-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Player       Joueur qui a trouv� l'outil
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
*}
procedure TTool.Find(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint);
begin
end;

{--------------------}
{ Classe TObjectTool }
{--------------------}

{*
  Cr�e une instance de TObjectTool
  Les nom d'outil et du fichier image peuvent �tre vide. Dans ce cas, l'outil
  prend les m�mes que la d�finition d'objet � laquelle il est li�.
  @param AMaster        Ma�tre FunLabyrinthe
  @param AID            ID de l'outil
  @param AObjectDef     D�finition d'objet li�e
  @param AFindMessage   Message apparaissant lorsqu'on trouve l'outil
  @param AName          Nom de l'outil
  @param AImgName       Nom du fichier image
*}
constructor TObjectTool.Create(AMaster : TMaster; const AID : TComponentID;
  AObjectDef : TObjectDef; const AFindMessage : string;
  const AName : string = ''; const AImgName : string = '');
begin
  inherited Create(AMaster, AID, IIF(AName = '', AObjectDef.Name, AName));
  FObjectDef := AObjectDef;
  FFindMessage := AFindMessage;

  if AImgName = '' then
    FPainter.ImgNames.Assign(FObjectDef.Painter.ImgNames)
  else
    FPainter.ImgNames.Add(AImgName);
end;

{*
  Ex�cut� lorsque le joueur trouve l'outil
  Find est ex�cut� lorsque le joueur trouve l'outil. C'est-�-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Player       Joueur qui a trouv� l'outil
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
*}
procedure TObjectTool.Find(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint);
begin
  Player.Map[Pos] := Player.Map[Pos].ChangeTool;
  ObjectDef.Count[Player] := ObjectDef.Count[Player] + 1;
  if FindMessage <> '' then
    Player.Controller.ShowDialog(sMessage, FindMessage);
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
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortExecute   � positionner � True pour emp�cher le Execute
*}
procedure TObstacle.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  Cancel := True;
end;

{---------------}
{ Classe TScrew }
{---------------}

{*
  Cr�e une instance de TScrew
  @param AMaster     Ma�tre FunLabyrinthe
  @param AID         ID de la case
  @param AName       Nom de la case
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TScrew.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AField : TField; AEffect : TEffect; ATool : TTool;
  AObstacle : TObstacle);
begin
  inherited Create(AMaster, AID, AName);
  FStaticDraw := False;
  FField := AField;
  FEffect := AEffect;
  FTool := ATool;
  FObstacle := AObstacle;

  FStaticDraw := FField.StaticDraw and
    ((not Assigned(FEffect)) or FEffect.StaticDraw) and
    ((not Assigned(FTool)) or FTool.StaticDraw) and
    ((not Assigned(FObstacle)) or FObstacle.StaticDraw);
end;

{*
  Dessine la case sur un canevas
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TScrew.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
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
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint; var Cancel : boolean);
begin
  Field.Entering(Player, OldDirection, KeyPressed, Src, Pos, Cancel);
end;

{*
  Ex�cut� lorsque le joueur tente de sortir de la case
  Exiting est ex�cut� lorsque le joueur tente de sortir de la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Pos            Position de la case
  @param Dest           Case de destination
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TScrew.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Pos, Dest : T3DPoint; var Cancel : boolean);
begin
  Field.Exiting(Player, OldDirection, KeyPressed, Pos, Dest, Cancel);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
*}
procedure TScrew.Entered(Player : TPlayer; KeyPressed : boolean;
  const Src, Pos : T3DPoint);
begin
  Field.Entered(Player, KeyPressed, Src, Pos);
  if Assigned(Effect) then
    Effect.Entered(Player, KeyPressed, Src, Pos);
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TScrew.Exited(Player : TPlayer; KeyPressed : boolean;
  const Pos, Dest : T3DPoint);
begin
  Field.Exited(Player, KeyPressed, Pos, Dest);
  if Assigned(Effect) then
    Effect.Exited(Player, KeyPressed, Pos, Dest);
end;

{*
  Trouve l'objet et ex�cute l'effet
  @param Player       Joueur concern�
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TScrew.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  if Assigned(Tool) then
    Tool.Find(Player, KeyPressed, Pos);
  if Assigned(Effect) then
    Effect.Execute(Player, KeyPressed, Pos, GoOnMoving);
end;

{*
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Entered de la case ne soit ex�cut�e, il faut positionner
  AbortEntered � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortExecute   � positionner � True pour emp�cher le Execute
*}
procedure TScrew.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  if Assigned(Obstacle) then
  begin
    Obstacle.Pushing(Player, OldDirection, KeyPressed,
      Src, Pos, Cancel, AbortExecute);
  end;
end;

{*
  Change le terrain d'une case et renvoie la case modifi�e
  @param NewField   ID du nouveau terrain
  @return Une case identique � celle-ci mais avec le terrain indiqu�
*}
function TScrew.ChangeField(NewField : TComponentID) : TScrew;
var EffectID, ToolID, ObstacleID : TComponentID;
begin
  if Effect = nil then EffectID := '' else
    EffectID := Effect.ID;
  if Tool = nil then ToolID := '' else
    ToolID := Tool.ID;
  if Obstacle = nil then ObstacleID := '' else
    ObstacleID := Obstacle.ID;
  Result := Master.Screw[NewField+'-'+EffectID+'-'+ToolID+'-'+ObstacleID];
end;

{*
  Change l'effet d'une case et renvoie la case modifi�e
  @param NewEffect   ID du nouvel effet
  @return Une case identique � celle-ci mais avec l'effet indiqu�
*}
function TScrew.ChangeEffect(NewEffect : TComponentID = '') : TScrew;
var ToolID, ObstacleID : TComponentID;
begin
  if Tool = nil then ToolID := '' else
    ToolID := Tool.ID;
  if Obstacle = nil then ObstacleID := '' else
    ObstacleID := Obstacle.ID;
  Result := Master.Screw[Field.ID+'-'+NewEffect+'-'+ToolID+'-'+ObstacleID];
end;

{*
  Change l'outil d'une case et renvoie la case modifi�e
  @param NewTool   ID du nouvel outil
  @return Une case identique � celle-ci mais avec l'outil indiqu�
*}
function TScrew.ChangeTool(NewTool : TComponentID = '') : TScrew;
var EffectID, ObstacleID : TComponentID;
begin
  if Effect = nil then EffectID := '' else
    EffectID := Effect.ID;
  if Obstacle = nil then ObstacleID := '' else
    ObstacleID := Obstacle.ID;
  Result := Master.Screw[Field.ID+'-'+EffectID+'-'+NewTool+'-'+ObstacleID];
end;

{*
  Change l'obstacle d'une case et renvoie la case modifi�e
  @param NewObstacle   ID du nouvel obstacle
  @return Une case identique � celle-ci mais avec l'obstacle indiqu�
*}
function TScrew.ChangeObstacle(NewObstacle : TComponentID = '') : TScrew;
var EffectID, ToolID : TComponentID;
begin
  if Effect = nil then EffectID := '' else
    EffectID := Effect.ID;
  if Tool = nil then ToolID := '' else
    ToolID := Tool.ID;
  Result := Master.Screw[Field.ID+'-'+EffectID+'-'+ToolID+'-'+NewObstacle];
end;

{-------------------------}
{ Classe TOverriddenScrew }
{-------------------------}

{*
  Cr�e une instance de TOverriddenScrew
  @param AMaster     Ma�tre FunLabyrinthe
  @param AID         ID de la case
  @param AMap        Carte
  @param APosition   Position
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TOverriddenScrew.Create(AMaster : TMaster; const AID : TComponentID;
  AMap : TMap; APosition : T3DPoint; AField : TField; AEffect : TEffect;
  ATool : TTool; AObstacle : TObstacle);
var AOriginalScrew : TScrew;
begin
  AOriginalScrew := AMap[APosition];
  inherited Create(AMaster, AID, AOriginalScrew.Name,
    AField, AEffect, ATool, AObstacle);

  if not AOriginalScrew.StaticDraw then
    FStaticDraw := False;
  FMap := AMap;
  FPosition := APosition;
  FOriginalScrew := AOriginalScrew;

  Map[Position] := Self;
end;

{*
  D�truit l'instance
*}
destructor TOverriddenScrew.Destroy;
begin
  Map[Position] := OriginalScrew;
  inherited;
end;

{*
  Dessine la case sur un canevas
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TOverriddenScrew.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  OriginalScrew.Draw(QPos, Canvas, X, Y);
  inherited;
end;

{-------------}
{ Classe TMap }
{-------------}

{*
  Cr�e une instance de TMap
  @param AMaster       Ma�tre FunLabyrinthe
  @param AID           ID de la carte
  @param ADimensions   Dimensions de la carte (en cases)
  @param AZoneSize     Taille d'une zone de la carte
*}
constructor TMap.Create(AMaster : TMaster; const AID : TComponentID;
  ADimensions : T3DPoint; AZoneWidth, AZoneHeight : integer);
var I : integer;
begin
  inherited Create(AMaster, AID);
  FDimensions := ADimensions;
  FZoneWidth := AZoneWidth;
  FZoneHeight := AZoneHeight;
  FMaxViewSize := MinViewSize;

  FOutsideOffset := FDimensions.X * FDimensions.Y * FDimensions.Z;
  SetLength(FMap, FOutsideOffset + FDimensions.Z);
  for I := Low(FMap) to High(FMap) do
    FMap[I] := nil;
end;

{*
  Modifie la taille maximale d'une vue pour cette carte
  @param Value   Nouvelle taille maximale
*}
procedure TMap.SetMaxViewSize(Value : integer);
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
function TMap.GetMap(const Position : T3DPoint) : TScrew;
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
  Modifie le tableau des cases index� par leur position sur la carte
  @param Position   Position sur la carte
  @param Value      Nouvelle case
*}
procedure TMap.SetMap(const Position : T3DPoint; Value : TScrew);
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
  Tableau des cases hors de la carte index� par �tage
  @param Floor   �tage
  @return La case hors de la carte � l'�tage sp�cifi�
*}
function TMap.GetOutside(Floor : integer) : TScrew;
begin
  if Floor < 0 then Floor := 0 else
  if Floor >= FDimensions.Z then Floor := FDimensions.Z-1;
  Result := FMap[Floor + FOutsideOffset];
end;

{*
  Modifie le tableau des cases hors de la carte index� par �tage
  @param Floor   �tage
  @param Value   Nouvelle case
*}
procedure TMap.SetOutside(Floor : integer; Value : TScrew);
begin
  if (Floor >= 0) and (Floor < FDimensions.Z) then
    FMap[Floor + FOutsideOffset] := Value;
end;

{*
  Taille du tableau de la carte lin�aire
  @return Taille du tableau de la carte lin�aire
*}
function TMap.GetLinearMapCount : integer;
begin
  Result := Length(FMap);
end;

{*
  Tableau zero-based de la carte lin�aire
  @param Index   Index dans le tableau
  @return Case de la carte lin�aire � l'index sp�cifi�
*}
function TMap.GetLinearMap(Index : integer) : TScrew;
begin
  Result := FMap[Index];
end;

{*
  Modifie le tableau zero-based de la carte lin�aire
  @param Index   Index dans le tableau
  @param Value   Nouvelle case
*}
procedure TMap.SetLinearMap(Index : integer; Value : TScrew);
begin
  FMap[Index] := Value;
end;

{*
  Teste si une coordonn�e est � l'int�rieur de la carte
  @param Position   Coordonn�e � tester
  @return True si la coordonn�e est dans la carte, False sinon
*}
function TMap.InMap(const Position : T3DPoint) : boolean;
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
function TMap.PlayersOn(const Position : T3DPoint) : integer;
var I : integer;
begin
  Result := 0;
  if IsNo3DPoint(Position) then exit;
  for I := 0 to Master.PlayerCount-1 do
  begin
    if (Master.Players[I].Map = Self) and
       Same3DPoint(Master.Players[I].Position, Position) then
      inc(Result);
  end;
end;

{----------------}
{ Classe TPlayer }
{----------------}

{*
  Cr�e une instance de TPlayer
  @param AMaster     Ma�tre FunLabyrinthe
  @param AID         ID du joueur
  @param AName       Nom du joueur
  @param AMap        Carte de d�part
  @param APosition   Position de d�part
*}
constructor TPlayer.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AMap : TMap; APosition : T3DPoint);
var Dir : TDirection;
begin
  inherited Create(AMaster, AID, AName);

  FStaticDraw := False;
  FMap := AMap;
  FPosition := APosition;
  FDirection := diNone;
  for Dir in [diNorth..diWest] do
    FDirPainters[Dir] := nil;
  FColor := clBlue;
  FPlugins := TObjectList.Create(False);
  FAttributes := THashedStringList.Create;
  TStringList(FAttributes).CaseSensitive := True;
  FPlayState := psPlaying;
end;

{*
  D�truit l'instance
*}
destructor TPlayer.Destroy;
var Dir : TDirection;
begin
  FAttributes.Free;
  FPlugins.Free;
  for Dir in [diNorth..diWest] do if Assigned(FDirPainters[Dir]) then
    FDirPainters[Dir].Free;
  inherited;
end;

{*
  Nombre de plug-in greff�s au joueur
  @return Nombre de plug-in
*}
function TPlayer.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in greff�s au joueur
  @param Index   Index du plug-in dans le tableau
  @return Le plug-in � la position indiqu�e
*}
function TPlayer.GetPlugins(Index : integer) : TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Tableau index� par cha�ne des attributs du joueur
  @param AttrName   Nom de l'attribut � r�cup�rer
  @return Attribut dont le nom a �t� sp�cifi�
*}
function TPlayer.GetAttribute(const AttrName : string) : integer;
var Index : integer;
begin
  Index := FAttributes.IndexOf(AttrName);
  if Index < 0 then Result := 0 else
    Result := integer(FAttributes.Objects[Index]);
end;

{*
  Modifie le tableau index� par cha�ne des attributs du joueur
  @param AttrName   Nom de l'attribut � modifier
  @param Value      Nouvelle valeur de l'attribut
*}
procedure TPlayer.SetAttribute(const AttrName : string; Value : integer);
var Index : integer;
begin
  Index := FAttributes.IndexOf(AttrName);
  if Index < 0 then
  begin
    if Value <> 0 then
      FAttributes.AddObject(AttrName, TObject(Value));
  end else
  begin
    if Value = 0 then FAttributes.Delete(Index) else
      FAttributes.Objects[Index] := TObject(Value);
  end;
end;

{*
  Dresse la liste des attributs du joueur
  @param Attributes   Liste de cha�nes dans laquelle enregistrer les attributs
*}
procedure TPlayer.GetAttributes(Attributes : TStrings);
begin
  Attributes.Assign(FAttributes);
end;

{*
  Dresse la liste des ID des plug-in du joueur
  @param Plugins   Liste de cha�nes dans laquelle enregistrer les ID des plug-in
*}
procedure TPlayer.GetPluginIDs(PluginIDs : TStrings);
var I : integer;
begin
  PluginIDs.Clear;
  for I := 0 to PluginCount-1 do
    PluginIDs.AddObject(Plugins[I].ID, Plugins[I]);
end;

{*
  Dessine le joueur sur un canevas
  Draw dessine le joueur sur un canevas � la position indiqu�e.
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonn�e X du point � partir duquel dessiner le joueur
  @param Y        Coordonn�e Y du point � partir duquel dessiner le joueur
*}
procedure TPlayer.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
var I : integer;
begin
  // Dessine les plug-in en-dessous du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawBefore(Self, QPos, Canvas, X, Y);

  // Dessine le peintre correspondant � la direction...
  if FColor = clDefault then
  begin
    if IsNoQPos(QPos) or (FDirection = diNone) or
       (not Assigned(FDirPainters[FDirection])) then
      Painter.Draw(Canvas, X, Y)
    else
      FDirPainters[FDirection].Draw(Canvas, X, Y);
  end else
  // ... ou le traditionnel disque color�
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+ScrewSize-6, Y+ScrewSize-6);
    end;
  end;

  // Dessine les plug-in au-dessus du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawAfter(Self, QPos, Canvas, X, Y);
end;

{*
  Dessine le joueur sur un canevas
  DrawInPlace dessine le joueur sur un canevas � la position indiqu�e, avec pour
  position qualifi�e sa position actuelle.
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonn�e X du point � partir duquel dessiner le joueur
  @param Y        Coordonn�e Y du point � partir duquel dessiner le joueur
*}
procedure TPlayer.DrawInPlace(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
var QPos : TQualifiedPos;
begin
  QPos.Map := Map;
  QPos.Position := Position;
  Draw(QPos, Canvas, X, Y);
end;

{*
  Greffe un plug-in au joueur
  @param Plugin   Le plug-in � greffer
*}
procedure TPlayer.AddPlugin(Plugin : TPlugin);
begin
  FPlugins.Add(Plugin);
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in � retirer
*}
procedure TPlayer.RemovePlugin(Plugin : TPlugin);
begin
  FPlugins.Remove(Plugin);
end;

{*
  Indique si le joueur est capable d'effectuer une action donn�e
  CanYou commence par tester si un plug-in permet l'action. Sinon, il
  d�termine quels sont les objets permettant cette action. S'il y en a
  plusieurs, le joueur se voit demander d'en choisir un, et celui-ci est
  utilis�.
  @param Action   Action � tester
  @return True si le joueur est capabled d'effectuer l'action, False sinon
*}
function TPlayer.CanYou(const Action : TPlayerAction) : boolean;
var I, GoodObjectCount : integer;
    GoodObjects : array of TObjectDef;
    RadioTitles : array of string;
    GoodObject : TObjectDef;
begin
  Result := True;

  // Les plug-in ont la priorit�, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do if Plugins[I].CanYou(Self, Action) then exit;

  // Listage des objets susceptibles d'aider le joueur
  SetLength(GoodObjects, Master.ObjectDefCount);
  GoodObjectCount := 0;
  for I := 0 to Master.ObjectDefCount-1 do
  begin
    if Master.ObjectDefs[I].CanYou(Self, Action) then
    begin
      GoodObjects[GoodObjectCount] := Master.ObjectDefs[I];
      inc(GoodObjectCount);
    end;
  end;

  // Aucun objet trouv� : �chec
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
    Controller.ShowDialogRadio(sWhichObject, sWhichObject, mtConfirmation,
      [mbOK], mrOK, RadioTitles, I, True);
    GoodObject := GoodObjects[I];
  end;

  // Utilisation de l'objet
  GoodObject.UseFor(Self, Action);
end;

{*
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir          Direction du d�placement
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Redo         Indique s'il faut r�it�rer le d�placement
  @return True si le d�placement a r�ussi, False sinon
*}
function TPlayer.Move(Dir : TDirection; KeyPressed : boolean;
  out Redo : boolean) : boolean;
var I : integer;
    Src, Dest : T3DPoint;
    OldDir : TDirection;
    Cancel, AbortExecute : boolean;
begin
  // Initialisation des variables
  Result := False;
  Redo := False;
  Src := FPosition;
  Dest := PointBehind(FPosition, Dir);
  OldDir := FDirection;
  FDirection := Dir;
  Cancel := False;
  AbortExecute := False;

  // Le joueur est-il toujours en train de jouer
  if PlayState <> psPlaying then exit;

  // Premier passage : le d�placement est-il permis ?
  begin
    // Case source : exiting
    Map[Src].Exiting(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;

    // Plug-in : moving
    for I := 0 to PluginCount-1 do
      Plugins[I].Moving(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;

    // Case destination : entering
    Map[Dest].Entering(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;

    // Case destination : pushing
    Map[Dest].Pushing(Self, OldDir, KeyPressed, Src, Dest, Cancel,
      AbortExecute);
    if Cancel then exit;
  end;

  // D�placement du joueur (� moins qu'il ait �t� d�plac� par ailleurs)
  if Same3DPoint(FPosition, Src) then
    FPosition := Dest
  else
    Dest := FPosition;
  Result := True;

  // Second passage : le d�placement a �t� fait
  begin
    // Case source : exited
    Map[Src].Exited(Self, KeyPressed, Src, Dest);

    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, KeyPressed, Src, Dest);

    // Case destination : entered
    Map[Dest].Entered(Self, KeyPressed, Src, Dest);

    // Case destination : execute (sauf si AbortExecute a �t� positionn� � True)
    if not AbortExecute then
      Map[Dest].Execute(Self, KeyPressed, Dest, Redo);
  end;
end;

{*
  Fait changer le joueur de carte
  @param AMap        Nouvelle carte
  @param APosition   Nouvelle position
*}
procedure TPlayer.ChangeMap(AMap : TMap; APosition : T3DPoint);
begin
  FMap := AMap;
  FPosition := APosition;

  if Assigned(Controller) then
    Controller.MapChanged;
end;

{*
  Fait gagner le joueur
*}
procedure TPlayer.Win;
var I : integer;
begin
  if FPlayState <> psPlaying then exit;

  // Ce joueur a gagn�
  FPlayState := psWon;

  // Les autres joueurs ont perdu
  for I := 0 to Master.PlayerCount-1 do if Master.Players[I] <> Self then
    Master.Players[I].FPlayState := psLost;

  // La partie est termin�e
  Master.Terminate;
end;

{*
  Fait perdre le joueur
*}
procedure TPlayer.Lose;
var I : integer;
begin
  if FPlayState <> psPlaying then exit;

  // Ce joueur a perdu
  FPlayState := psLost;

  // Si plus aucun joueur ne joue, la partie est termin�e
  for I := 0 to Master.PlayerCount-1 do
    if Master.Players[I].PlayState = psPlaying then exit;
  Master.Terminate;
end;

{----------------}
{ Classe TMaster }
{----------------}

{*
  Cr�e une instance de TMaster
*}
constructor TMaster.Create(AEditing : boolean);
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
  D�truit l'instance
*}
destructor TMaster.Destroy;
var I : integer;
begin
  for I := FComponents.Count-1 downto 0 do
    FComponents.Objects[I].Free;

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
  Tableau des composants index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
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
  Tableau des composants de case index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas de case
*}
function TMaster.GetScrewComponent(const ID : TComponentID) : TScrewComponent;
begin
  try
    Result := Component[ID] as TScrewComponent;
  except
    on Error : EComponentNotFound do
    begin
      if NberCharInStr('-', ID) = 3 then
        Result := Screw[ID]
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
function TMaster.GetPlugin(const ID : TComponentID) : TPlugin;
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
function TMaster.GetObjectDef(const ID : TComponentID) : TObjectDef;
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
function TMaster.GetField(const ID : TComponentID) : TField;
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
function TMaster.GetEffect(const ID : TComponentID) : TEffect;
begin
  if ID = '' then Result := nil else
    Result := Component[ID] as TEffect;
end;

{*
  Tableau des outils index� par leur ID
  @param ID   ID de l'outil � trouver
  @return L'outil dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun outil ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un outil
*}
function TMaster.GetTool(const ID : TComponentID) : TTool;
begin
  if ID = '' then Result := nil else
    Result := Component[ID] as TTool;
end;

{*
  Tableau des obstacles index� par leur ID
  @param ID   ID de l'obstacle � trouver
  @return L'obstacle dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun obstacle ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas un obstacle
*}
function TMaster.GetObstacle(const ID : TComponentID) : TObstacle;
begin
  if ID = '' then Result := nil else
    Result := Component[ID] as TObstacle;
end;

{*
  Tableau des cases index� par leur ID
  Si la case n'a pas pu �tre trouv�e, GetScrew essaye de trouver les terrain
  et effet correspondant � son ID et de cr�er la case automatiquement
  @param ID   ID de la case � trouver
  @return La case dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune case ne correspond � l'ID sp�cifi�
  @throws EInvalidCast : Le composant de l'ID sp�cifi� n'est pas une case
*}
function TMaster.GetScrew(const ID : TComponentID) : TScrew;
var AField : TField;
    AEffect : TEffect;
    ATool : TTool;
    AObstacle : TObstacle;
    AName : string;
begin
  try
    Result := Component[ID] as TScrew;
  except
    on Error : EComponentNotFound do
    begin
      Result := nil;

      if NberCharInStr('-', ID) = 3 then
      try
        AField := Field[GetXToken(ID, '-', 1)];
        AEffect := Effect[GetXToken(ID, '-', 2)];
        ATool := Tool[GetXToken(ID, '-', 3)];
        AObstacle := Obstacle[GetXToken(ID, '-', 4)];

        AName := AField.Name;
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

      if Result = nil then raise;
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
function TMaster.GetMap(const ID : TComponentID) : TMap;
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
  @return Le plug-in � la position sp�cifi�e
*}
function TMaster.GetPlugins(Index : integer) : TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Nombre de d�finitions d'objet
  @return Nombre de d�finitions d'objet
*}
function TMaster.GetObjectDefCount : integer;
begin
  Result := FObjectDefs.Count;
end;

{*
  Tableau zero-based des d�finitions d'objet
  @param Index   Index de la d�finition d'objet
  @return La d�finition d'objet � la position sp�cifi�e
*}
function TMaster.GetObjectDefs(Index : integer) : TObjectDef;
begin
  Result := TObjectDef(FObjectDefs[Index]);
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
  @return Le terrain � la position sp�cifi�e
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
  @return L'effet � la position sp�cifi�e
*}
function TMaster.GetEffects(Index : integer) : TEffect;
begin
  Result := TEffect(FEffects[Index]);
end;

{*
  Nombre d'outils
  @return Nombre d'outils
*}
function TMaster.GetToolCount : integer;
begin
  Result := FTools.Count;
end;

{*
  Tableau zero-based des outils
  @param Index   Index de l'outil
  @return L'outil � la position sp�cifi�e
*}
function TMaster.GetTools(Index : integer) : TTool;
begin
  Result := TTool(FTools[Index]);
end;

{*
  Nombre d'obstacles
  @return Nombre d'obstacles
*}
function TMaster.GetObstacleCount : integer;
begin
  Result := FObstacles.Count;
end;

{*
  Tableau zero-based des obstacles
  @param Index   Index de l'obstacle
  @return L'obstacle � la position sp�cifi�e
*}
function TMaster.GetObstacles(Index : integer) : TObstacle;
begin
  Result := TObstacle(FObstacles[Index]);
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
  @return La case � la position sp�cifi�e
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
  @return La carte � la position sp�cifi�e
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
  @return Le joueur � la position sp�cifi�e
*}
function TMaster.GetPlayers(Index : integer) : TPlayer;
begin
  Result := TPlayer(FPlayers[Index]);
end;

{*
  Modifie la temporisation
  @param Value   Nouvelle temporisation en millisecondes
*}
procedure TMaster.SetTemporization(Value : integer);
begin
  if Value > 0 then
    FTemporization := Value;
end;

{*
  Ajoute un composant
  @param Component   Le composant � ajouter
*}
procedure TMaster.AddComponent(Component : TFunLabyComponent);
begin
  FComponents.AddObject(Component.ID, Component);

  if Component is TPlugin    then FPlugins   .Add(Component) else
  if Component is TObjectDef then FObjectDefs.Add(Component) else
  if Component is TField     then FFields    .Add(Component) else
  if Component is TEffect    then FEffects   .Add(Component) else
  if Component is TTool      then FTools     .Add(Component) else
  if Component is TObstacle  then FObstacles .Add(Component) else
  if Component is TScrew     then FScrews    .Add(Component) else
  if Component is TMap       then FMaps      .Add(Component) else
  if Component is TPlayer    then FPlayers   .Add(Component);
end;

{*
  Retire un composant
  @param Component   Le composant � retirer
*}
procedure TMaster.RemoveComponent(Component : TFunLabyComponent);
begin
  if Component is TPlugin    then FPlugins   .Remove(Component) else
  if Component is TObjectDef then FObjectDefs.Remove(Component) else
  if Component is TField     then FFields    .Remove(Component) else
  if Component is TEffect    then FEffects   .Remove(Component) else
  if Component is TTool      then FTools     .Remove(Component) else
  if Component is TObstacle  then FObstacles .Remove(Component) else
  if Component is TScrew     then FScrews    .Remove(Component) else
  if Component is TMap       then FMaps      .Remove(Component) else
  if Component is TPlayer    then FPlayers   .Remove(Component);

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
  Temporise l'ex�cution
*}
procedure TMaster.Temporize;
begin
  Sleep(Temporization);
end;

{*
  Met � jour le tick count de la partie
*}
procedure TMaster.UpdateTickCount;
begin
  FTickCount := GetTickCount - FBeginTickCount;
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

    fScrewFileName := fScrewsDir+fScrewFileName;
  finally
    Free;
  end;
end.

