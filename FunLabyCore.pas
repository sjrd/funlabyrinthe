{*
  Décrit les composants au coeur de Funlabyrinthe
  L'unité FunLabyCore décrit les plug-in, objets, terrains, et effets de case
  qui sont au coeur de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
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
  idBuoyPlugin = 'BuoyPlugin'; /// ID du plug-in bouée

resourcestring
  sBuoys           = 'Bouée';              /// Nom de l'objet bouée
  sBuoyInfos       = '%d bouée';           /// Infos bouées (singulier)
  sBuoysInfos      = '%d bouées';          /// Infos bouées (pluriel)
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
  idBuoys = 'Buoys';           /// ID des bouées
  idPlanks = 'Planks';         /// ID des planches
  idSilverKeys = 'SilverKeys'; /// ID des clefs d'argent
  idGoldenKeys = 'GoldenKeys'; /// ID des clefs d'or

resourcestring
  sGrass = 'Herbe';                           /// Nom de l'herbe
  sWall = 'Mur';                              /// Nom du mur
  sWater = 'Eau';                             /// Nom de l'eau
  sHole = 'Trou';                             /// Nom du trou
  sSky = 'Ciel';                              /// Nom du ciel

  sNorthArrow = 'Flèche nord';                /// Nom de la flèche nord
  sEastArrow = 'Flèche est';                  /// Nom de la flèche est
  sSouthArrow = 'Flèche sud';                 /// Nom de la flèche sud
  sWestArrow = 'Flèche ouest';                /// Nom de la flèche ouest
  sCrossroads = 'Carrefour';                  /// Nom du carrefour

  sInactiveTransporter = 'Téléporteur inactif';      /// Téléporteur inactif
  sTransporterNext = 'Téléporteur suivant n°%d';     /// Téléporteur suivant
  sTransporterPrev = 'Téléporteur précédent n°%d';   /// Téléporteur précédent
  sTransporterRandom = 'Téléporteur aléatoire n°%d'; /// Téléporteur aléatoire

  sUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  sDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant
  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect
  sOutside = 'Dehors';                        /// Nom du dehors

  sBuoy = 'Bouée';                            /// Nom de la bouée
  sPlank = 'Planche';                         /// Nom de la planche
  sSilverKey = 'Clef d''argent';              /// Nom de la clef d'argent
  sGoldenKey = 'Clef d''or';                  /// Nom de la clef d'or

  sSilverBlock = 'Bloc en argent';            /// Nom du bloc en argent
  sGoldenBlock = 'Bloc en or';                /// Nom du bloc en or

const {don't localize}
  idPlankField = 'PlankField';                   /// ID du terrain planche
  idPlankEffect = 'PlankEffect';                 /// ID de l'effet planche
  idPlankScrew = '%s-Plank-%s';                  /// ID de la case planche

  idGrass = 'Grass';                             /// ID de l'herbe
  idWall = 'Wall';                               /// ID du mur
  idWater = 'Water';                             /// ID de l'eau
  idHole = 'Hole';                               /// ID du trou
  idSky = 'Sky';                                 /// ID du ciel

  idNorthArrow = 'NorthArrow';                   /// ID de la flèche nord
  idEastArrow = 'EastArrow';                     /// ID de la flèche est
  idSouthArrow = 'SouthArrow';                   /// ID de la flèche sud
  idWestArrow = 'WestArrow';                     /// ID de la flèche ouest
  idCrossroads = 'Crossroads';                   /// ID du carrefour

  idInactiveTransporter = 'InactiveTransporter'; /// ID du téléporteur inactif
  idTransporterNext = 'TransporterNext%d';       /// ID du téléporteur suivant
  idTransporterPrev = 'TransporterPrev%d';       /// ID du téléporteur précédent
  idTransporterRandom = 'TransporterRandom%d';   /// ID du téléporteur aléatoire

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant
  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect
  idOutside = 'Outside';                         /// ID du dehors

  idBuoy = 'Buoy';                               /// ID de la bouée
  idPlank = 'Plank';                             /// ID de la planche
  idSilverKey = 'SilverKey';                     /// ID de la clef d'argent
  idGoldenKey = 'GoldenKey';                     /// ID de la clef d'or

  idSilverBlock = 'SilverBlock';                 /// ID du bloc en argent
  idGoldenBlock = 'GoldenBlock';                 /// ID du bloc en or

const {don't localize}
  fGrass = 'Grass';                         /// Fichier de l'herbe
  fWall = 'Wall';                           /// Fichier du mur
  fWater = 'Water';                         /// Fichier de l'eau
  fAlternateWater = 'AlternateWater';       /// Fichier alternatif de l'eau
  fHole = 'Hole';                           /// Fichier du trou
  fSky = 'Sky';                             /// Fichier du ciel

  fNorthArrow = 'NorthArrow';               /// Fichier de la flèche nord
  fEastArrow = 'EastArrow';                 /// Fichier de la flèche est
  fSouthArrow = 'SouthArrow';               /// Fichier de la flèche sud
  fWestArrow = 'WestArrow';                 /// Fichier de la flèche ouest
  fCrossroads = 'Crossroads';               /// Fichier du carrefour

  fTransporter = 'Transporter';             /// Fichier du téléporteur inactif
  fUpStairs = 'UpStairs';                   /// Fichier de l'escalier montant
  fDownStairs = 'DownStairs';               /// Fichier de l'escalier descendant
  fDirectTurnstile = 'DirectTurnstile';     /// Fichier du tourniquet direct
  fIndirectTurnstile = 'IndirectTurnstile'; /// Fichier du tourniquet indirect
  fOutside = 'Outside';                     /// Fichier du dehors

  fBuoy = 'Buoy';                           /// Fichier de la bouée
  fPlank = 'Plank';                         /// Fichier de la planche
  fSilverKey = 'SilverKey';                 /// Fichier de la clef d'argent
  fGoldenKey = 'GoldenKey';                 /// Fichier de la clef d'or

  fSilverBlock = 'SilverBlock';             /// Fichier du bloc en argent
  fGoldenBlock = 'GoldenBlock';             /// Fichier du bloc en or

resourcestring
  sCantGoOnWater = 'Sans bouée, on coule dans l''eau.';
  sCantGoOnHole = 'T''es pas bien de vouloir sauter dans ce trou !?';
  sCantOpenSilverBlock = 'Ce bloc ne disparaîtra qu''avec une clef d''argent.';
  sCantOpenGoldenBlock = 'Ce bloc ne disparaîtra qu''avec une clef d''or.';
  sCantGoOnSky = 'Tu ne peux pas voler !';

  sFoundBuoy      = 'Tu as trouvé une bouée.'+#10+
                    'Tu peux aller dans l''eau.';
  sFoundPlank     = 'Tu as trouvé une planche.'+#10+
                    'Tu peux franchir certains obstacles.';
  sFoundSilverKey = 'Tu as trouvé une clef d''argent.'+#10+
                    'Tu peux faire disparaître un bloc en argent.';
  sFoundGoldenKey = 'Tu as trouvé une clef d''or.'+#10+
                    'Tu peux faire disparaître un bloc en or.';

type
  {*
    Type de téléporteur
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Plug-in bouée
    Affiche une bouée sous le joueur, et permet d'aller dans l'eau
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
    Définition de l'objet bouée
    La bouée permet d'aller dans l'eau.
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
    Définition de l'objet planche
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
    Définition de l'objet clef d'argent
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
    Définition de l'objet clef d'or
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
    Terrain spécial planche
    Ce terrain ne doit pas être utilisé normalement. Il n'est utilisé que par la
    case spéciale planche.
  *}
  TPlankField = class(TField)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Effet spécial planche
    Cet effet ne doit pas être utilisé normalement. Il n'est utilisé que par la
    case spéciale planche.
  *}
  TPlankEffect = class(TEffect)
  public
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Case spéciale planche
    Cette case est utilisée pour le déplacement particulier de la planche.
  *}
  TPlankScrew = class(TScrew)
  private
    FOriginalScrew : TScrew; /// Case originale
    FPlayer : TPlayer;       /// Joueur qui passe sur la case
  public
    constructor Create(AMaster : TMaster; AOriginalScrew : TScrew;
      APlayer : TPlayer);

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    property OriginalScrew : TScrew read FOriginalScrew;
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
    Le mur est un terrain qui bloque systématiquement le joueur.
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
    L'eau est un terrain sur lequel on peut aller avec une bouée ou une barque,
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
    Flèche (de toutes directions)
    Les flèches repoussent le joueur dans la direction qui leur est propre.
  *}
  TArrow = class(TEffect)
  private
    FDirection : TDirection; /// Direction de la flèche
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADirection : TDirection);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Direction : TDirection read FDirection;
  end;

  {*
    Téléporteur
    Le téléporteur emmène le joueur à un autre téléporteur
  *}
  TTransporter = class(TEffect)
  private
    FKind : TTransporterKind; /// Type de téléporteur

    procedure FindNext(Map : TMap; var Pos : T3DPoint);
    procedure FindPrevious(Map : TMap; var Pos : T3DPoint);
    procedure FindRandom(Map : TMap; var Pos : T3DPoint);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AKind : TTransporterKind = tkInactive);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Kind : TTransporterKind read FKind;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'à
    parvenir à en sortir.
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
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'à
    parvenir à en sortir.
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
    Le dehors représente l'extérieur du labyrinthe et fait remporter la victoire
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
    Bouée
    La bouée donne au joueur un objet bouée
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
    Bloc en argent
    Le bloc en argent peut être détruit au moyen d'une clef en argent.
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
    Le bloc en or peut être détruit au moyen d'une clef en or.
  *}
  TGoldenBlock = class(TObstacle)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

const
  clPlankColor = $00004080;

procedure LoadCoreComponents(Master : TMaster; Params : TStrings);

implementation

{*
  Charge tous les composants au coeur de FunLabyrinthe
  @param Master   Maître FunLabyrinthe dans lequel charger les composants
*}
procedure LoadCoreComponents(Master : TMaster; Params : TStrings);
var I : integer;
begin
  // Plug-in
  TBuoyPlugin.Create(Master, idBuoyPlugin);

  // Défintions d'objet
  TBuoys.Create(Master, idBuoys, sBuoys);
  TPlanks.Create(Master, idPlanks, sPlanks);
  TSilverKeys.Create(Master, idSilverKeys, sSilverKeys);
  TGoldenKeys.Create(Master, idGoldenKeys, sGoldenKeys);

  // Terrain et effet spécial planche
  TPlankField.Create(Master, idPlankField, '');
  TPlankEffect.Create(Master, idPlankEffect, '');

  // Terrains
  TGrass.Create(Master, idGrass, sGrass);
  TWall.Create(Master, idWall, sWall);
  TWater.Create(Master, idWater, sWater);
  THole.Create(Master, idHole, sHole);
  TSky.Create(Master, idSky, sSky);

  // Effets de case
  TArrow.Create(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.Create(Master, idEastArrow , sEastArrow , diEast );
  TArrow.Create(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.Create(Master, idWestArrow , sWestArrow , diWest );
  TArrow.Create(Master, idCrossroads, sCrossroads, diNone );

  TTransporter.Create(Master, idInactiveTransporter, sInactiveTransporter);
  for I := 1 to 15 do
    TTransporter.Create(Master, Format(idTransporterNext, [I]),
      Format(sTransporterNext, [I]), tkNext);
  for I := 1 to 15 do
    TTransporter.Create(Master, Format(idTransporterPrev, [I]),
      Format(sTransporterPrev, [I]), tkPrevious);
  for I := 1 to 15 do
    TTransporter.Create(Master, Format(idTransporterRandom, [I]),
      Format(sTransporterRandom, [I]), tkRandom);

  TDirectTurnstile.Create(Master, idDirectTurnstile, sDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile, sIndirectTurnstile);

  TOutside.Create(Master, idOutside, sOutside);

  TBuoy.Create(Master, idBuoy, sBuoy);
  TPlank.Create(Master, idPlank, sPlank);
  TSilverKey.Create(Master, idSilverKey, sSilverKey);
  TGoldenKey.Create(Master, idGoldenKey, sGoldenKey);

  // Obstacles
  TSilverBlock.Create(Master, idSilverBlock, sSilverBlock);
  TGoldenBlock.Create(Master, idGoldenBlock, sGoldenBlock);
end;

//////////////////////////
/// Classe TBuoyPlugin ///
//////////////////////////

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessiné
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
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
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de départ
  @param Dest         Case d'arrivée
*}
procedure TBuoyPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
begin
  if not (Player.Map[Dest].Field is TWater) then
    Player.RemovePlugin(Self);
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TBuoyPlugin.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := Action = actGoOnWater;
end;

/////////////////////
/// Classe TBuoys ///
/////////////////////

{*
  Crée une instance de TBuoys
  @param AMaster   Maître FunLabyrinthe
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
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TBuoys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actGoOnWater) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
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
  Crée une instance de TPlanks
  @param AMaster   Maître FunLabyrinthe
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
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TPlanks.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actPassOverScrew) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
*}
procedure TPlanks.UseFor(Player : TPlayer; const Action : TPlayerAction);
var Pos : T3DPoint;
begin
  if Action = actPassOverScrew then with Player do
  begin
    Pos := PointBehind(Position, Direction);
    Map[Pos] := TPlankScrew.Create(Master, Map[Pos], Player);
    Sleep(500);
  end else inherited;
end;

//////////////////////////
/// Classe TSilverKeys ///
//////////////////////////

{*
  Crée une instance de TSilverKeys
  @param AMaster   Maître FunLabyrinthe
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
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TSilverKeys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actOpenSilverLock) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
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
  Crée une instance de TGoldenKeys
  @param AMaster   Maître FunLabyrinthe
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
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TGoldenKeys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actOpenGoldenLock) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TPlankEffect.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  GoOnMoving := True;
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TPlankEffect.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
var PlankScrew : TPlankScrew;
begin
  inherited;
  PlankScrew := Player.Map[Pos] as TPlankScrew;
  Player.Map[Pos] := PlankScrew.OriginalScrew;
  PlankScrew.Free;
end;

//////////////////////////
/// Classe TPlankScrew ///
//////////////////////////

{*
  Crée une instance de TPlankScrew
  @param AMaster          Maître FunLabyrinthe
  @param AOriginalScrew   Case originale
  @param APlayer          Joueur qui passe sur la case
*}
constructor TPlankScrew.Create(AMaster : TMaster; AOriginalScrew : TScrew;
  APlayer : TPlayer);
begin
  inherited Create(AMaster,
    Format(idPlankScrew, [AOriginalScrew.ID, APlayer.ID]), AOriginalScrew.Name,
    AMaster.Field[idPlankField], AMaster.Effect[idPlankEffect], nil);
  FOriginalScrew := AOriginalScrew;
  FPlayer := APlayer;
end;

{*
  Dessine la case sur un canevas
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlankScrew.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  OriginalScrew.Draw(Canvas, X, Y);

  with Canvas do
  begin
    Brush.Color := clPlankColor;
    Brush.Style := bsSolid;
    Pen.Color := clPlankColor;
    Pen.Style := psSolid;

    if Player.Direction in [diNorth, diSouth] then
      Rectangle(X+6, Y-5, X+ScrewSize-6, Y+ScrewSize+5)
    else
      Rectangle(X-5, Y+6, X+ScrewSize+5, Y+ScrewSize-6);
  end;
end;

/////////////////////
/// Classe TGrass ///
/////////////////////

{*
  Crée une instance de TGrass
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
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
  Crée une instance de TWall
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TWall.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fWall);
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
  Crée une instance de TWater
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
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
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TWater.AfterConstruction;
begin
  inherited;
  FAlternatePainter.ImgNames.EndUpdate;
end;

{*
  Détruit l'instance
*}
destructor TWater.Destroy;
begin
  FAlternatePainter.Free;
  inherited;
end;

{*
  Dessine le terrain sur le canevas indiqué
  Les descendants de TField doivent réimplémenter DrawField plutôt que Draw.
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonnée X du point à partir duquel dessiner le terrain
  @param Y        Coordonnée Y du point à partir duquel dessiner le terrain
*}
procedure TWater.DrawField(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  if (Master.TickCount mod 2000) < 1000 then
    Painter.Draw(Canvas, X, Y)
  else
    AlternatePainter.Draw(Canvas, X, Y);
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
  Crée une instance de THole
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor THole.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fHole);
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
  Crée une instance de TSky
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TSky.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fSky);
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
  Crée une instance de TArrow
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param ADirection   Direction de la flèche
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
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
  Crée une instance de TTransporter
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AKind        Type de téléporteur
*}
constructor TTransporter.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AKind : TTransporterKind = tkInactive);
begin
  inherited Create(AMaster, AID, AName);
  FKind := AKind;
  Painter.ImgNames.Add(fTransporter);
end;

{*
  Trouve le téléporteur suivant sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du téléporteur initiale en entrée et finale en sortie
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
  Trouve le téléporteur précédent sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du téléporteur initiale en entrée et finale en sortie
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
  Trouve un autre téléporteur aléatoirement sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du téléporteur initiale en entrée et finale en sortie
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

  // Recensement de toutes les cases identiques, à l'exception de l'originale
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

  // À moins que la liste soit vide, on en pêche un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
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
    else exit; // on évite des tests inutiles pour un inactif
  end;

  // Si l'on a trouvé une autre case, on déplace le joueur
  if Same3DPoint(Other, Pos) then exit;
  Sleep(500);
  Player.Position := Other;
end;

///////////////////////////////
/// Classe TDirectTurnstile ///
///////////////////////////////

{*
  Crée une instance de TDirectTurnstile
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
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
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
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
  Crée une instance de TIndirectTurnstile
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
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
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
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
  Crée une instance de TOutside
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TOutside.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  // Faire gagner le joueur
end;

////////////////////
/// Classe TBuoy ///
////////////////////

{*
  Crée une instance de TBuoy
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TBuoy.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // Désactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incrémentation du nombre de bouées du joueur
  with Master.ObjectDef[idBuoys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundBuoy);
end;

/////////////////////
/// Classe TPlank ///
/////////////////////

{*
  Crée une instance de TPlank
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TPlank.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // Désactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incrémentation du nombre de bouées du joueur
  with Master.ObjectDef[idPlanks] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundPlank);
end;

/////////////////////////
/// Classe TSilverKey ///
/////////////////////////

{*
  Crée une instance de TSilverKey
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TSilverKey.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // Désactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incrémentation du nombre de bouées du joueur
  with Master.ObjectDef[idSilverKeys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundSilverKey);
end;

/////////////////////////
/// Classe TGoldenKey ///
/////////////////////////

{*
  Crée une instance de TGoldenKey
  @param AMaster   Maître FunLabyrinthe
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
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TGoldenKey.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // Désactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incrémentation du nombre de bouées du joueur
  with Master.ObjectDef[idGoldenKeys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.ShowDialog(sMessage, sFoundGoldenKey);
end;

///////////////////////////
/// Classe TSilverBlock ///
///////////////////////////

{*
  Crée une instance de TSilverBlock
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
*}
constructor TSilverBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverBlock);
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
  @param AbortEntered   À positionner à True pour empêcher le Entered
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
  Crée une instance de TGoldenBlock
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
*}
constructor TGoldenBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenBlock);
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
  @param AbortEntered   À positionner à True pour empêcher le Entered
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

end.

