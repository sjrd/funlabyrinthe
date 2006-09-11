{*
  D�crit les composants au coeur de Funlabyrinthe
  L'unit� FunLabyCore d�crit les plug-in, objets, terrains, et effets de case
  qui sont au coeur de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyCore;

interface

uses
  Windows, SysUtils, Classes, Graphics, ScUtils, FunLabyUtils;

const {don't localize}
  actGoOnWater = 'GoOnWater';         /// Action d'aller sur l'eau
  actPassOverScrew = 'PassOverScrew'; /// Action de passer au-dessus d'une case
  /// Action d'ouvrir une serrure en argent
  actOpenSilverLock = 'OpenSilverLock';
  /// Action d'ouvrir une serrure en or
  actOpenGoldenLock = 'OpenGoldenLock';

const {don't localize}
  idBuoyPlugin = 'BuoyPlugin';   /// ID du plug-in bou�e
  idPlankPlugin = 'PlankPlugin'; /// ID du plug-in planche
  idBoatPlugin = 'BoatPlugin';   /// ID du plug-in barque

resourcestring
  sBuoys           = 'Bou�e';              /// Nom de l'objet bou�e
  sBuoyInfos       = '%d bou�e';           /// Infos bou�es (singulier)
  sBuoysInfos      = '%d bou�es';          /// Infos bou�es (pluriel)
  sPlanks          = 'Planche';            /// Nom de l'objet planche
  sPlankInfos      = '%d planche';         /// Infos planches (singulier)
  sPlanksInfos     = '%d planches';        /// Infos planches (pluriel)
  sSilverKeys      = 'Clef d''argent';     /// Nom de l'objet clef d'argent
  sSilverKeyInfos  = '%d clef d''argent';  /// Infos clefs d'argent (singulier)
  sSilverKeysInfos = '%d clefs d''argent'; /// Infos clefs d'argent (pluriel)
  sGoldenKeys      = 'Clef d''or';         /// Nom de l'objet clef d'or
  sGoldenKeyInfos  = '%d clef d''or';      /// Infos clefs d'or (singulier)
  sGoldenKeysInfos = '%d clefs d''or';     /// Infos clefs d'or (pluriel)

const {don't localize}
  idBuoys = 'Buoys';           /// ID des bou�es
  idPlanks = 'Planks';         /// ID des planches
  idSilverKeys = 'SilverKeys'; /// ID des clefs d'argent
  idGoldenKeys = 'GoldenKeys'; /// ID des clefs d'or

resourcestring
  sGrass = 'Herbe';                           /// Nom de l'herbe
  sWall = 'Mur';                              /// Nom du mur
  sWater = 'Eau';                             /// Nom de l'eau
  sHole = 'Trou';                             /// Nom du trou
  sSky = 'Ciel';                              /// Nom du ciel

  sNorthArrow = 'Fl�che nord';                /// Nom de la fl�che nord
  sEastArrow = 'Fl�che est';                  /// Nom de la fl�che est
  sSouthArrow = 'Fl�che sud';                 /// Nom de la fl�che sud
  sWestArrow = 'Fl�che ouest';                /// Nom de la fl�che ouest
  sCrossroads = 'Carrefour';                  /// Nom du carrefour

  sInactiveTransporter = 'T�l�porteur inactif';      /// T�l�porteur inactif
  sTransporterNext = 'T�l�porteur suivant n�%d';     /// T�l�porteur suivant
  sTransporterPrev = 'T�l�porteur pr�c�dent n�%d';   /// T�l�porteur pr�c�dent
  sTransporterRandom = 'T�l�porteur al�atoire n�%d'; /// T�l�porteur al�atoire

  sUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  sDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant
  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect
  sOutside = 'Dehors';                        /// Nom du dehors
  sTreasure = 'Tr�sor';                       /// Nom du tr�sor

  sBuoy = 'Bou�e';                            /// Nom de la bou�e
  sPlank = 'Planche';                         /// Nom de la planche
  sSilverKey = 'Clef d''argent';              /// Nom de la clef d'argent
  sGoldenKey = 'Clef d''or';                  /// Nom de la clef d'or
  sBoat = 'Barque';                           /// Nom de la barque

  sSilverBlock = 'Bloc en argent';            /// Nom du bloc en argent
  sGoldenBlock = 'Bloc en or';                /// Nom du bloc en or
  sSecretWay = 'Passage secret';              /// Nom du passage secret

const {don't localize}
  idPlankField = 'PlankField';                   /// ID du terrain planche
  idPlankEffect = 'PlankEffect';                 /// ID de l'effet planche
  idPlankScrew = 'PlankScrew-%s';                /// ID de la case planche

  idGrass = 'Grass';                             /// ID de l'herbe
  idWall = 'Wall';                               /// ID du mur
  idWater = 'Water';                             /// ID de l'eau
  idHole = 'Hole';                               /// ID du trou
  idSky = 'Sky';                                 /// ID du ciel

  idGrassWater = 'GrassWater';                   /// ID de l'eau effet herbe

  idNorthArrow = 'NorthArrow';                   /// ID de la fl�che nord
  idEastArrow = 'EastArrow';                     /// ID de la fl�che est
  idSouthArrow = 'SouthArrow';                   /// ID de la fl�che sud
  idWestArrow = 'WestArrow';                     /// ID de la fl�che ouest
  idCrossroads = 'Crossroads';                   /// ID du carrefour

  idInactiveTransporter = 'InactiveTransporter'; /// ID du t�l�porteur inactif
  idTransporterNext = 'TransporterNext%d';       /// ID du t�l�porteur suivant
  idTransporterPrev = 'TransporterPrev%d';       /// ID du t�l�porteur pr�c�dent
  idTransporterRandom = 'TransporterRandom%d';   /// ID du t�l�porteur al�atoire

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant
  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect
  idOutside = 'Outside';                         /// ID du dehors
  idTreasure = 'Treasure';                       /// ID du tr�sor

  idBuoy = 'Buoy';                               /// ID de la bou�e
  idPlank = 'Plank';                             /// ID de la planche
  idSilverKey = 'SilverKey';                     /// ID de la clef d'argent
  idGoldenKey = 'GoldenKey';                     /// ID de la clef d'or
  idBoat = 'Boat%d';                             /// ID de la barque

  idSilverBlock = 'SilverBlock';                 /// ID du bloc en argent
  idGoldenBlock = 'GoldenBlock';                 /// ID du bloc en or
  idSecretWay = 'SecretWay';                     /// ID du passage secret

const {don't localize}
  fGrass = 'Grass';                         /// Fichier de l'herbe
  fWall = 'Wall';                           /// Fichier du mur
  fWater = 'Water';                         /// Fichier de l'eau
  fAlternateWater = 'AlternateWater';       /// Fichier alternatif de l'eau
  fHole = 'Hole';                           /// Fichier du trou
  fSky = 'Sky';                             /// Fichier du ciel

  fNorthArrow = 'NorthArrow';               /// Fichier de la fl�che nord
  fEastArrow = 'EastArrow';                 /// Fichier de la fl�che est
  fSouthArrow = 'SouthArrow';               /// Fichier de la fl�che sud
  fWestArrow = 'WestArrow';                 /// Fichier de la fl�che ouest
  fCrossroads = 'Crossroads';               /// Fichier du carrefour

  fTransporter = 'Transporter';             /// Fichier du t�l�porteur inactif
  fUpStairs = 'UpStairs';                   /// Fichier de l'escalier montant
  fDownStairs = 'DownStairs';               /// Fichier de l'escalier descendant
  fDirectTurnstile = 'DirectTurnstile';     /// Fichier du tourniquet direct
  fIndirectTurnstile = 'IndirectTurnstile'; /// Fichier du tourniquet indirect
  fOutside = 'Outside';                     /// Fichier du dehors
  fTreasure = 'Treasure';                   /// Fichier du tr�sor

  fBuoy = 'Buoy';                           /// Fichier de la bou�e
  fPlank = 'Plank';                         /// Fichier de la planche
  fSilverKey = 'SilverKey';                 /// Fichier de la clef d'argent
  fGoldenKey = 'GoldenKey';                 /// Fichier de la clef d'or
  fBoat = 'Boat';                           /// Fichier de la barque

  fSilverBlock = 'SilverBlock';             /// Fichier du bloc en argent
  fGoldenBlock = 'GoldenBlock';             /// Fichier du bloc en or

resourcestring
  sCantGoOnWater = 'Sans bou�e, on coule dans l''eau.';
  sCantGoOnHole = 'T''es pas bien de vouloir sauter dans ce trou !?';
  sCantOpenSilverBlock = 'Ce bloc ne dispara�tra qu''avec une clef d''argent.';
  sCantOpenGoldenBlock = 'Ce bloc ne dispara�tra qu''avec une clef d''or.';
  sCantGoOnSky = 'Tu ne peux pas voler !';

  sGotOutsideMaze = 'BRAVO ! Tu as r�ussi � sortir du labyrinthe !';
  sFoundTreasure  = 'BRAVO ! Tu as trouv� le tr�sor !';

  sFoundBuoy      = 'Tu as trouv� une bou�e.'+#10+
                    'Tu peux aller dans l''eau.';
  sFoundPlank     = 'Tu as trouv� une planche.'+#10+
                    'Tu peux franchir certains obstacles.';
  sFoundSilverKey = 'Tu as trouv� une clef d''argent.'+#10+
                    'Tu peux faire dispara�tre un bloc en argent.';
  sFoundGoldenKey = 'Tu as trouv� une clef d''or.'+#10+
                    'Tu peux faire dispara�tre un bloc en or.';

type
  {*
    Type de t�l�porteur
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Plug-in bou�e
    Affiche une bou�e sous le joueur, et permet d'aller dans l'eau.
  *}
  TBuoyPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); override;

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
  end;

  {*
    Plug-in planche
    Affiche une planche � c�t� du joueur ou sous celui-ci.
  *}
  TPlankPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;
  end;

  {*
    Plug-in barque
    Affiche une barque sous le joueur, et permet d'aller dans l'eau. De plus,
    ce plug-in bloque un mouvement si la direction a chang�.
  *}
  TBoatPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Dest : T3DPoint;
      var Cancel : boolean); override;
    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); override;

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
  end;

  {*
    D�finition de l'objet bou�e
    La bou�e permet d'aller dans l'eau.
  *}
  TBuoys = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    D�finition de l'objet planche
    La planche permet de passer au-dessus des cases
  *}
  TPlanks = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    D�finition de l'objet clef d'argent
    La clef d'argent permet d'ouvrir une serrure en argent
  *}
  TSilverKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    D�finition de l'objet clef d'or
    La clef d'or permet d'ouvrir une serrure en or
  *}
  TGoldenKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    Terrain sp�cial planche
    Ce terrain ne doit pas �tre utilis� normalement. Il n'est utilis� que par la
    case sp�ciale planche.
  *}
  TPlankField = class(TField)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Effet sp�cial planche
    Cet effet ne doit pas �tre utilis� normalement. Il n'est utilis� que par la
    case sp�ciale planche.
  *}
  TPlankEffect = class(TEffect)
  public
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Case sp�ciale planche
    Cette case est utilis�e pour le d�placement particulier de la planche.
  *}
  TPlankScrew = class(TOverriddenScrew)
  private
    FPlayer : TPlayer; /// Joueur qui passe sur la case
  public
    constructor Create(AMaster : TMaster; AMap : TMap; APosition : T3DPoint;
      APlayer : TPlayer);

    property Player : TPlayer read FPlayer;
  end;

  {*
    Herbe
    L'herbe est le terrain de base de FunLabyrinthe. Il n'a pas de condition.
  *}
  TGrass = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);
  end;

  {*
    Mur
    Le mur est un terrain qui bloque syst�matiquement le joueur.
  *}
  TWall = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Eau
    L'eau est un terrain sur lequel on peut aller avec une bou�e ou une barque,
    et au-dessus duquel on peut passer avec une planche.
  *}
  TWater = class(TField)
  private
    FAlternatePainter : TPainter;
  protected
    procedure DrawField(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    property AlternatePainter : TPainter read FAlternatePainter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Trou
    Le trou est un terrain au-dessus duquel on peut passer avec une planche, et
    sinon inaccessible.
  *}
  THole = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Ciel
    Le ciel est toujours inaccessible.
  *}
  TSky = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Fl�che (de toutes directions)
    Les fl�ches repoussent le joueur dans la direction qui leur est propre.
  *}
  TArrow = class(TEffect)
  private
    FDirection : TDirection; /// Direction de la fl�che
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADirection : TDirection);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Direction : TDirection read FDirection;
  end;

  {*
    T�l�porteur
    Le t�l�porteur emm�ne le joueur � un autre t�l�porteur
  *}
  TTransporter = class(TEffect)
  private
    FNumber : integer;        /// Num�ro du t�l�porteur
    FKind : TTransporterKind; /// Type de t�l�porteur

    procedure FindNext(Map : TMap; var Pos : T3DPoint);
    procedure FindPrevious(Map : TMap; var Pos : T3DPoint);
    procedure FindRandom(Map : TMap; var Pos : T3DPoint);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer;
      AKind : TTransporterKind = tkInactive);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Number : integer read FNumber;
    property Kind : TTransporterKind read FKind;
  end;

  {*
    Escaliers
    Les escaliers permettent de monter ou descendre d'un �tage
  *}
  TStairs = class(TEffect)
  private
    FUp : boolean; /// Indique si l'escalier est montant
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AUp : boolean);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Up : boolean read FUp;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'�
    parvenir � en sortir.
  *}
  TDirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Tourniquet Indirect
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'�
    parvenir � en sortir.
  *}
  TIndirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Dehors
    Le dehors repr�sente l'ext�rieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
  *}
  TOutside = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Tr�sor
    Le tr�sor fait remporter la victoire au joueur qui le trouve.
  *}
  TTreasure = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Bou�e
    La bou�e donne au joueur un objet bou�e
  *}
  TBuoy = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Planche
    La planche donne au joueur un objet planche
  *}
  TPlank = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Clef d'argent
    La clef d'argent donne au joueur un objet clef d'argent
  *}
  TSilverKey = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Clef d'or
    La clef d'or donne au joueur un objet clef d'or
  *}
  TGoldenKey = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Barque
    La barque est un moyen de transport permettant d'aller sur l'eau.
  *}
  TBoat = class(TEffect)
  private
    FNumber : integer; /// Num�ro de la barque
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Number : integer read FNumber;
  end;

  {*
    Bloc en argent
    Le bloc en argent peut �tre d�truit au moyen d'une clef en argent.
  *}
  TSilverBlock = class(TObstacle)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Bloc en or
    Le bloc en or peut �tre d�truit au moyen d'une clef en or.
  *}
  TGoldenBlock = class(TObstacle)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Passage secret
    Le passage secret a l'apparence d'un mur mais peut �tre ouvert sans rien.
  *}
  TSecretWay = class(TObstacle)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

const
  clPlank   = $00004080; /// Couleur de la planche
  clOutBoat = $00004080; /// Couleur de l'ext�rieur d'une barque
  clInBoat  = $00006699; /// Couleur de l'int�rieur d'une barque

procedure LoadCoreComponents(Master : TMaster; Params : TStrings);

implementation

{*
  Charge tous les composants au coeur de FunLabyrinthe
  @param Master   Ma�tre FunLabyrinthe dans lequel charger les composants
*}
procedure LoadCoreComponents(Master : TMaster; Params : TStrings);
var I : integer;
begin
  // Plug-in
  TBuoyPlugin.Create(Master, idBuoyPlugin);
  TPlankPlugin.Create(Master, idPlankPlugin);
  TBoatPlugin.Create(Master, idBoatPlugin);

  // D�fintions d'objet
  TBuoys.Create(Master, idBuoys, sBuoys);
  TPlanks.Create(Master, idPlanks, sPlanks);
  TSilverKeys.Create(Master, idSilverKeys, sSilverKeys);
  TGoldenKeys.Create(Master, idGoldenKeys, sGoldenKeys);

  // Terrain et effet sp�cial planche
  TPlankField.Create(Master, idPlankField, '');
  TPlankEffect.Create(Master, idPlankEffect, '');

  // Terrains
  TGrass.Create(Master, idGrass, sGrass);
  TWall.Create(Master, idWall, sWall);
  TWater.Create(Master, idWater, sWater);
  THole.Create(Master, idHole, sHole);
  TSky.Create(Master, idSky, sSky);

  TGrass.Create(Master, idGrassWater, sWater, Master.Field[idWater]);

  // Effets de case
  TArrow.Create(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.Create(Master, idEastArrow , sEastArrow , diEast );
  TArrow.Create(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.Create(Master, idWestArrow , sWestArrow , diWest );
  TArrow.Create(Master, idCrossroads, sCrossroads, diNone );

  TTransporter.Create(Master, idInactiveTransporter, sInactiveTransporter, 0);
  for I := 1 to 15 do
    TTransporter.Create(Master, idTransporterNext, sTransporterNext,
      I, tkNext);
  for I := 1 to 15 do
    TTransporter.Create(Master, idTransporterPrev, sTransporterPrev,
      I, tkPrevious);
  for I := 1 to 15 do
    TTransporter.Create(Master, idTransporterRandom, sTransporterRandom,
      I, tkRandom);

  TStairs.Create(Master, idUpStairs, sUpStairs, True);
  TStairs.Create(Master, idDownStairs, sDownStairs, False);

  TDirectTurnstile.Create(Master, idDirectTurnstile, sDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile, sIndirectTurnstile);

  TOutside.Create(Master, idOutside, sOutside);
  TTreasure.Create(Master, idTreasure, sTreasure);

  TBuoy.Create(Master, idBuoy, sBuoy);
  TPlank.Create(Master, idPlank, sPlank);
  TSilverKey.Create(Master, idSilverKey, sSilverKey);
  TGoldenKey.Create(Master, idGoldenKey, sGoldenKey);

  for I := 1 to 10 do
    TBoat.Create(Master, idBoat, sBoat, I);

  // Obstacles
  TSilverBlock.Create(Master, idSilverBlock, sSilverBlock);
  TGoldenBlock.Create(Master, idGoldenBlock, sGoldenBlock);
  TSecretWay.Create(Master, idSecretWay, sSecretWay);
end;

//////////////////////////
/// Classe TBuoyPlugin ///
//////////////////////////

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TBuoyPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited;
  with Canvas do
  begin
    Brush.Color := clYellow;
    Brush.Style := bsSolid;
    Pen.Color := clYellow;
    Pen.Style := psSolid;
    Ellipse(X+1, Y+1, X+ScrewSize-1, Y+ScrewSize-1);
  end;
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de d�part
  @param Dest         Case d'arriv�e
*}
procedure TBuoyPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
begin
  if not (Player.Map[Dest].Field is TWater) then
    Player.RemovePlugin(Self);
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TBuoyPlugin.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := Action = actGoOnWater;
end;

///////////////////////////
/// Classe TPlankPlugin ///
///////////////////////////

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlankPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited;

  // D�termination de l'endroit o� dessiner r�ellement la planche
  if not (Player.Map[Player.Position] is TPlankScrew) then
  begin
    case Player.Direction of
      diNorth : dec(Y, ScrewSize);
      diEast  : inc(X, ScrewSize);
      diSouth : inc(Y, ScrewSize);
      diWest  : dec(X, ScrewSize);
    end;
  end;

  // Dessin de la planche
  with Canvas do
  begin
    Brush.Color := clPlank;
    Brush.Style := bsSolid;
    Pen.Color := clPlank;
    Pen.Style := psSolid;

    if Player.Direction in [diNorth, diSouth] then
      Rectangle(X+6, Y-5, X+ScrewSize-6, Y+ScrewSize+5)
    else
      Rectangle(X-5, Y+6, X+ScrewSize+5, Y+ScrewSize-6);
  end;
end;

//////////////////////////
/// Classe TBoatPlugin ///
//////////////////////////

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TBoatPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited;
  with Canvas do
  begin
    Brush.Color := clInBoat;
    Brush.Style := bsSolid;
    Pen.Color := clOutBoat;
    Pen.Style := psSolid;
    Pen.Width := 2;

    case Player.Direction of
      diNorth :
      begin
        // Proue
        Arc(X-5, Y-2, X+25, Y+24, X+25, Y+12, X+15, Y+ 2);
        Arc(X+5, Y-2, X+35, Y+24, X+15, Y+ 2, X+ 5, Y+12);
        // Bastingage
        MoveTo(X+ 5, Y+12);
        LineTo(X+ 5, Y+29);
        LineTo(X+25, Y+29);
        LineTo(X+25, Y+12);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
      diEast :
      begin
        // Proue
        Arc(X+4, Y-5, X+32, Y+25, X+18, Y+25, X+30, Y+15);
        Arc(X+4, Y+5, X+32, Y+35, X+30, Y+15, X+18, Y+ 5);
        // Bastingage
        MoveTo(X+18, Y+ 5);
        LineTo(X+ 1, Y+ 5);
        LineTo(X+ 1, Y+25);
        LineTo(X+18, Y+25);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
      diSouth :
      begin
        // Proue
        Arc(X+5, Y+4, X+35, Y+32, X+ 5, Y+18, X+15, Y+30);
        Arc(X-5, Y+4, X+25, Y+32, X+15, Y+30, X+25, Y+18);
        // Bastingage
        MoveTo(X+ 5, Y+18);
        LineTo(X+ 5, Y+ 1);
        LineTo(X+25, Y+ 1);
        LineTo(X+25, Y+18);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
      diWest :
      begin
        // Proue
        Arc(X-2, Y+5, X+26, Y+35, X+12, Y+ 5, X   , Y+15);
        Arc(X-2, Y-5, X+26, Y+25, X   , Y+15, X+12, Y+25);
        // Bastingage
        MoveTo(X+10, Y+ 5);
        LineTo(X+29, Y+ 5);
        LineTo(X+29, Y+25);
        LineTo(X+12, Y+25);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
    end;

    Pen.Width := 1;
  end;
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
procedure TBoatPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Dest : T3DPoint; var Cancel : boolean);
begin
  if Player.Direction <> OldDirection then
    Cancel := True;
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de d�part
  @param Dest         Case d'arriv�e
*}
procedure TBoatPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
var ScrewID : TComponentID;
begin
  with Player do if not (Map[Dest].Field is TWater) then
  begin
    // Retirer le plug-in barque
    RemovePlugin(Self);

    // Placer un effet barque sur la case source
    if not Assigned(Map[Src].Obstacle) then ScrewID := '' else
      ScrewID := Map[Src].Obstacle.ID;
    ScrewID := Format(idGrassWater+'-'+idBoat+'-'+ScrewID,
      [Attribute[idBoatPlugin]]);
    Map[Src] := Master.Screw[ScrewID];

    // Remettre � 0 l'attribut du joueur concernant la barque
    Attribute[idBoatPlugin] := 0;
  end;
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TBoatPlugin.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := Action = actGoOnWater;
end;

/////////////////////
/// Classe TBuoys ///
/////////////////////

{*
  Cr�e une instance de TBuoys
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TBuoys.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fBuoy);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TBuoys.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sBuoyInfos, [ACount])
  else
    Result := Format(sBuoysInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TBuoys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actGoOnWater) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TBuoys.UseFor(Player : TPlayer; const Action : TPlayerAction);
begin
  if Action = actGoOnWater then
    Player.AddPlugin(Master.Plugin[idBuoyPlugin])
  else
    inherited;
end;

//////////////////////
/// Classe TPlanks ///
//////////////////////

{*
  Cr�e une instance de TPlanks
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TPlanks.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fPlank);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TPlanks.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sPlankInfos, [ACount])
  else
    Result := Format(sPlanksInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TPlanks.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actPassOverScrew) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TPlanks.UseFor(Player : TPlayer; const Action : TPlayerAction);
begin
  if Action = actPassOverScrew then with Player do
  begin
    TPlankScrew.Create(Master, Map, PointBehind(Position, Direction), Player);
    AddPlugin(Master.Plugin[idPlankPlugin]);
    Sleep(500);
  end else inherited;
end;

//////////////////////////
/// Classe TSilverKeys ///
//////////////////////////

{*
  Cr�e une instance de TSilverKeys
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TSilverKeys.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverKey);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TSilverKeys.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sSilverKeyInfos, [ACount])
  else
    Result := Format(sSilverKeysInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TSilverKeys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actOpenSilverLock) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TSilverKeys.UseFor(Player : TPlayer; const Action : TPlayerAction);
begin
  if Action = actOpenSilverLock then
    Count[Player] := Count[Player]-1
  else
    inherited;
end;

//////////////////////////
/// Classe TGoldenKeys ///
//////////////////////////

{*
  Cr�e une instance de TGoldenKeys
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TGoldenKeys.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenKey);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TGoldenKeys.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sGoldenKeyInfos, [ACount])
  else
    Result := Format(sGoldenKeysInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TGoldenKeys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actOpenGoldenLock) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TGoldenKeys.UseFor(Player : TPlayer; const Action : TPlayerAction);
begin
  if Action = actOpenGoldenLock then
    Count[Player] := Count[Player]-1
  else
    inherited;
end;

//////////////////////////
/// Classe TPlankField ///
//////////////////////////

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
procedure TPlankField.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  if Player <> (Player.Map[Src] as TPlankScrew).Player then
    Cancel := True;
end;

///////////////////////////
/// Classe TPlankEffect ///
///////////////////////////

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TPlankEffect.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  GoOnMoving := True;
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TPlankEffect.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
  inherited;
  Player.RemovePlugin(Master.Plugin[idPlankPlugin]);
  Player.Map[Pos].Free;
end;

//////////////////////////
/// Classe TPlankScrew ///
//////////////////////////

{*
  Cr�e une instance de TPlankScrew
  @param AMaster     Ma�tre FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param APlayer     Joueur qui passe sur la case
*}
constructor TPlankScrew.Create(AMaster : TMaster; AMap : TMap;
  APosition : T3DPoint; APlayer : TPlayer);
begin
  inherited Create(AMaster, Format(idPlankScrew, [APlayer.ID]), AMap, APosition,
    AMaster.Field[idPlankField], AMaster.Effect[idPlankEffect], nil);
  FPlayer := APlayer;
end;

/////////////////////
/// Classe TGrass ///
/////////////////////

{*
  Cr�e une instance de TGrass
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TGrass.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fGrass);
end;

////////////////////
/// Classe TWall ///
////////////////////

{*
  Cr�e une instance de TWall
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TWall.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fWall);
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
procedure TWall.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  Cancel := True;
end;

/////////////////////
/// Classe TWater ///
/////////////////////

{*
  Cr�e une instance de TWater
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TWater.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  FAlternatePainter := TPainter.Create(Master.ImagesMaster);
  FAlternatePainter.ImgNames.BeginUpdate;

  Painter.ImgNames.Add(fWater);
  AlternatePainter.ImgNames.Add(fAlternateWater);
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TWater.AfterConstruction;
begin
  inherited;
  FAlternatePainter.ImgNames.EndUpdate;
end;

{*
  D�truit l'instance
*}
destructor TWater.Destroy;
begin
  FAlternatePainter.Free;
  inherited;
end;

{*
  Dessine le terrain sur le canevas indiqu�
  Les descendants de TField doivent r�impl�menter DrawField plut�t que Draw.
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TWater.DrawField(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  if (Master.TickCount mod 2000) < 1000 then
    Painter.Draw(Canvas, X, Y)
  else
    AlternatePainter.Draw(Canvas, X, Y);
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
procedure TWater.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  with Player do
  begin
    if CanYou(actGoOnWater) then exit;

    if (Map[PointBehind(Pos, Direction)].Field = Map[Src].Field) and
      CanYou(actPassOverScrew) then exit;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnWater, dtError);
    Cancel := True;
  end;
end;

////////////////////
/// Classe THole ///
////////////////////

{*
  Cr�e une instance de THole
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor THole.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fHole);
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
procedure THole.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  with Player do
  begin
    if (Map[PointBehind(Pos, Direction)].Field = Map[Src].Field) and
      CanYou(actPassOverScrew) then exit;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnHole, dtError);
    Cancel := True;
  end;
end;

///////////////////
/// Classe TSky ///
///////////////////

{*
  Cr�e une instance de TSky
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TSky.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fSky);
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
procedure TSky.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  if KeyPressed then
    Player.ShowDialog(sBlindAlley, sCantGoOnSky, dtError);
  Cancel := True;
end;

/////////////////////
/// Classe TArrow ///
/////////////////////

{*
  Cr�e une instance de TArrow
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param ADirection   Direction de la fl�che
*}
constructor TArrow.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADirection : TDirection);
begin
  inherited Create(AMaster, AID, AName);
  FDirection := ADirection;
  case FDirection of
    diNone  : Painter.ImgNames.Add(fCrossroads);
    diNorth : Painter.ImgNames.Add(fNorthArrow);
    diEast  : Painter.ImgNames.Add(fEastArrow);
    diSouth : Painter.ImgNames.Add(fSouthArrow);
    diWest  : Painter.ImgNames.Add(fWestArrow);
  end;
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TArrow.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  if FDirection <> diNone then
    Player.Direction := FDirection;
  GoOnMoving := True;
end;

///////////////////////////
/// Classe TTransporter ///
///////////////////////////

{*
  Cr�e une instance de TTransporter
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AKind        Type de t�l�porteur
*}
constructor TTransporter.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ANumber : integer;
  AKind : TTransporterKind = tkInactive);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
  FKind := AKind;
  Painter.ImgNames.Add(fTransporter);
end;

{*
  Trouve le t�l�porteur suivant sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du t�l�porteur initiale en entr�e et finale en sortie
*}
procedure TTransporter.FindNext(Map : TMap; var Pos : T3DPoint);
var DimX, DimY, DimZ : integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    inc(Pos.X);
    if Pos.X >= DimX then
    begin
      Pos.X := 0;
      inc(Pos.Y);
      if Pos.Y >= DimY then
      begin
        Pos.Y := 0;
        inc(Pos.Z);
        if Pos.Z >= DimZ then
          Pos.Z := 0;
      end;
    end;
  until Map[Pos].Effect = Self;
end;

{*
  Trouve le t�l�porteur pr�c�dent sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du t�l�porteur initiale en entr�e et finale en sortie
*}
procedure TTransporter.FindPrevious(Map : TMap; var Pos : T3DPoint);
var DimX, DimY, DimZ : integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    dec(Pos.X);
    if Pos.X < 0 then
    begin
      Pos.X := DimX-1;
      dec(Pos.Y);
      if Pos.Y < 0 then
      begin
        Pos.Y := DimY-1;
        dec(Pos.Z);
        if Pos.Z < 0 then
          Pos.Z := DimZ-1;
      end;
    end;
  until Map[Pos].Effect = Self;
end;

{*
  Trouve un autre t�l�porteur al�atoirement sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du t�l�porteur initiale en entr�e et finale en sortie
*}
procedure TTransporter.FindRandom(Map : TMap; var Pos : T3DPoint);
const
  AllocBy = 10;
var DimX, DimY, DimZ : integer;
    Others : array of T3DPoint;
    Count, X, Y, Z : integer;
    Other : T3DPoint;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  // Recensement de toutes les cases identiques, � l'exception de l'originale
  Count := 0;
  SetLength(Others, AllocBy);
  for X := 0 to DimX-1 do for Y := 0 to DimY-1 do for Z := 0 to DimZ-1 do
  begin
    Other := Point3D(X, Y, Z);
    if (Map[Other].Effect = Self) and (not Same3DPoint(Other, Pos)) then
    begin
      if Count >= Length(Others) then
        SetLength(Others, Count+AllocBy);
      Others[Count] := Other;
      inc(Count);
    end;
  end;
  SetLength(Others, Count);

  // � moins que la liste soit vide, on en p�che un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TTransporter.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;

  // Recherche de la case de destination
  case FKind of
    tkNext     : FindNext    (Player.Map, Other);
    tkPrevious : FindPrevious(Player.Map, Other);
    tkRandom   : FindRandom  (Player.Map, Other);
    else exit; // on �vite des tests inutiles pour un inactif
  end;

  // Si l'on a trouv� une autre case, on d�place le joueur
  if Same3DPoint(Other, Pos) then exit;
  Sleep(500);
  Player.Position := Other;
end;

//////////////////////
/// Classe TStairs ///
//////////////////////

{*
  Cr�e une instance de TStairs
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AUp          Indique si l'escalier est montant
*}
constructor TStairs.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AUp : boolean);
begin
  inherited Create(AMaster, AID, AName);
  FUp := AUp;
  if Up then
    Painter.ImgNames.Add(fUpStairs)
  else
    Painter.ImgNames.Add(fDownStairs);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TStairs.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;
  if Up then
    inc(Other.Z)
  else
    dec(Other.Z);

  Sleep(500);
  Player.Position := Other;
end;

///////////////////////////////
/// Classe TDirectTurnstile ///
///////////////////////////////

{*
  Cr�e une instance de TDirectTurnstile
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TDirectTurnstile.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fDirectTurnstile);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TDirectTurnstile.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;

  Sleep(500);

  if Player.Direction = diWest then
    Dir := diNorth
  else
    Dir := Succ(Player.Direction);

  while not Player.Move(Dir, False, GoOnMoving) do
  begin
    if Player.Direction = diNorth then
      Dir := diWest
    else
      Dir := Pred(Player.Direction);
  end;
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TDirectTurnstile.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
  inherited;
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect(idIndirectTurnstile);
end;

/////////////////////////////////
/// Classe TIndirectTurnstile ///
/////////////////////////////////

{*
  Cr�e une instance de TIndirectTurnstile
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TIndirectTurnstile.Create(AMaster : TMaster;
  const AID : TComponentID; const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fIndirectTurnstile);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TIndirectTurnstile.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;

  Sleep(500);

  if Player.Direction = diNorth then
    Dir := diWest
  else
    Dir := Pred(Player.Direction);

  while not Player.Move(Dir, False, GoOnMoving) do
  begin
    if Player.Direction = diWest then
      Dir := diNorth
    else
      Dir := Succ(Player.Direction);
  end;
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TIndirectTurnstile.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
  inherited;
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect(idDirectTurnstile);
end;

///////////////////////
/// Classe TOutside ///
///////////////////////

{*
  Cr�e une instance de TOutside
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TOutside.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fOutside);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TOutside.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  Player.Win;
  Player.ShowDialog(sWon, sGotOutsideMaze);
end;

////////////////////////
/// Classe TTreasure ///
////////////////////////

{*
  Cr�e une instance de TTreasure
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TTreasure.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fTreasure);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TTreasure.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  Player.Win;
  Player.ShowDialog(sWon, sFoundTreasure);
end;

////////////////////
/// Classe TBuoy ///
////////////////////

{*
  Cr�e une instance de TBuoy
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TBuoy.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fBuoy);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TBuoy.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idBuoys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundBuoy);
end;

/////////////////////
/// Classe TPlank ///
/////////////////////

{*
  Cr�e une instance de TPlank
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TPlank.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fPlank);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TPlank.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idPlanks] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundPlank);
end;

/////////////////////////
/// Classe TSilverKey ///
/////////////////////////

{*
  Cr�e une instance de TSilverKey
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TSilverKey.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverKey);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TSilverKey.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idSilverKeys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundSilverKey);
end;

/////////////////////////
/// Classe TGoldenKey ///
/////////////////////////

{*
  Cr�e une instance de TGoldenKey
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TGoldenKey.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenKey);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TGoldenKey.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idGoldenKeys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundGoldenKey);
end;

////////////////////
/// Classe TBoat ///
////////////////////

{*
  Cr�e une instance de TBoat
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TBoat.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ANumber : integer);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
  Painter.ImgNames.Add(fBoat);
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TBoat.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var ObstacleID : TComponentID;
begin
  inherited;

  with Player do
  begin
    // Remplacement de la case par de l'eau simple
    if not Assigned(Map[Pos].Obstacle) then ObstacleID := '' else
      ObstacleID := Map[Pos].Obstacle.ID;
    Map[Pos] := Master.Screw[idWater+'--'+ObstacleID];

    // Indication du num�ro de la barque dans l'attribut correspondant
    Attribute[idBoatPlugin] := Number;

    // Ajout du plug-in barque
    AddPlugin(Master.Plugin[idBoatPlugin]);
  end;
end;

///////////////////////////
/// Classe TSilverBlock ///
///////////////////////////

{*
  Cr�e une instance de TSilverBlock
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TSilverBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverBlock);
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
  @param AbortEntered   � positionner � True pour emp�cher le Entered
*}
procedure TSilverBlock.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  if KeyPressed then with Player do
  begin
    if CanYou(actOpenSilverLock) then
      Map[Pos] := Map[Pos].ChangeObstacle
    else
      ShowDialog(sBlindAlley, sCantOpenSilverBlock, dtError);
  end;

  Cancel := True;
end;

///////////////////////////
/// Classe TGoldenBlock ///
///////////////////////////

{*
  Cr�e une instance de TGoldenBlock
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TGoldenBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenBlock);
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
  @param AbortEntered   � positionner � True pour emp�cher le Entered
*}
procedure TGoldenBlock.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  if KeyPressed then with Player do
  begin
    if CanYou(actOpenGoldenLock) then
      Map[Pos] := Map[Pos].ChangeObstacle
    else
      ShowDialog(sBlindAlley, sCantOpenGoldenBlock, dtError);
  end;

  Cancel := True;
end;

/////////////////////////
/// Classe TSecretWay ///
/////////////////////////

{*
  Cr�e une instance de TSecretWay
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TSecretWay.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fWall);
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
  @param AbortEntered   � positionner � True pour emp�cher le Entered
*}
procedure TSecretWay.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  if KeyPressed then
    Player.Map[Pos] := Player.Map[Pos].ChangeObstacle;

  Cancel := True;
end;

end.

