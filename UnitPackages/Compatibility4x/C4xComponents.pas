{*
  Décrit les classes de composants de compatibilité 4.x
  L'unité C4xScrews regroupe les différentes classes de composants de
  compatibilité 4.x de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xComponents;

interface

uses
  SysUtils, Classes, Graphics, ScUtils, FunLabyUtils, C4xCommon, C4xInterpreter;

resourcestring
  sSunkenButton = 'Bouton enfoncé'; /// Nom du bouton enfoncé
  sButton = 'Bouton n°%d';          /// Nom du bouton
  sButtonTemplate = 'Bouton';       /// Nom du bouton modèle

const {don't localize}
  idActionsObject = 'ActionsObject%d';     /// ID de l'objet lié à des actions
  idActionsEffect = 'ActionsEffect%d';     /// ID de l'effet à actions
  idActionsObstacle = 'ActionsObstacle%d'; /// ID de l'obstacle à actions

  idSunkenButton = 'SunkenButton';         /// ID du bouton enfoncé
  idButtonTemplate = 'ButtonTemplate';     /// ID du bouton modèle

  /// ID de la case à actions
  idActionsScrew = idGrass+'-ActionsEffect%d-ActionsObstacle%0:d';
  /// ID de la case à actions modèle
  idActionsScrewTemplate = idGrass+'-'+idButtonTemplate+'-';

  idActions = 'Actions%d'; /// ID d'un ensemble d'actions

const {don't localize}
  fSunkenButton = 'SunkenButton'; /// Fichier du bouton enfoncé
  fButton = 'Button';             /// Fichier du bouton
  fSwitchOff = 'SwitchOff';       /// Fichier de l'interrupteur éteint
  fSwitchOn = 'SwitchOn';         /// Fichier de l'interrupteur allumé
  fInfoStone = 'InfoStone';       /// Fichier de la borne info
  fTransporter = 'Transporter';   /// Fichier du téléporteur
  fOutside = 'Outside';           /// Fichier du dehors

resourcestring
  sButtonTitle = 'Numéro du bouton';
  sButtonPrompt = 'Numéro du bouton (1 à 75) :';

type
  {*
    Représente le type d'un ensemble d'actions
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsKind = (akPushButton, akSwitch, akInfoStone, akHidden,
    akTransporterNext, akTransporterPrevious, akTransporterRandom, akOutside,
    akCustom, akObject, akObstacle, akDirection);

  TActions = class;

  {*
    Définition d'objet lié à des actions
    Les nombre des objets liés à des actions est le compteur de ces actions.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsObject = class(TObjectDef)
  private
    FActions : TActions; /// Actions propriétaires
  protected
    function GetCount(Player : TPlayer) : integer; override;
    procedure SetCount(Player : TPlayer; Value : integer); override;
  public
    constructor Create(AActions : TActions);

    property Actions : TActions read FActions;
  end;

  {*
    Effet à actions
    Un effet à actions exécute une série d'actions lorsqu'on arrive dessus.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsEffect = class(TEffect)
  private
    FActions : TActions;          /// Actions propriétaires
    FAlternatePainter : TPainter; /// Peintre alternatif
  protected
    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    property AlternatePainter : TPainter read FAlternatePainter;
  public
    constructor Create(AActions : TActions);
    procedure AfterConstruction; override;

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Actions : TActions read FActions;
  end;

  {*
    Obstacle à actions
    Un obstacle à actions exécute une série d'actions lorsqu'on pousse dessus.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsObstacle = class(TObstacle)
  private
    FActions : TActions; /// Actions propriétaires
  public
    constructor Create(AActions : TActions);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;

    property Actions : TActions read FActions;
  end;

  {*
    Représente un ensemble d'actions pour une case active particulière
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActions = class(TFunLabyComponent)
  private
    FNumber : integer;            /// Numéro d'actions
    FKind : TActionsKind;         /// Type d'actions
    FFileName : string;           /// Nom du fichier de graphismes
    FActions : TStrings;          /// Actions à exécuter
    FCounter : integer;           /// Compteur d'exécution
    FObjectDef : TActionsObject;  /// Objet correspondant
    FEffect : TActionsEffect;     /// Effet correspondant
    FObstacle : TActionsObstacle; /// Obstacle correspondant
  public
    constructor Create(AMaster : TMaster; ANumber : integer;
      AKind : TActionsKind; const AFileName : string);
    destructor Destroy; override;

    procedure Execute(Phase : integer; Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var DoNextPhase : boolean);

    property Number : integer read FNumber;
    property Kind : TActionsKind read FKind;
    property FileName : string read FFileName;
    property Actions : TStrings read FActions;
    property Counter : integer read FCounter write FCounter;
    property ObjectDef : TActionsObject read FObjectDef;
    property Effect : TActionsEffect read FEffect;
    property Obstacle : TActionsObstacle read FObstacle;
  end;

const {don't localize}
  /// Ensemble des types d'actions personnalisés
  CustomActionsKind : set of TActionsKind =
    [akCustom, akObject, akObstacle, akDirection];

  /// Sous-répertoire des images gardées pour compatibilité
  fCompatibility = 'Compatibility4x\';

implementation

{-----------------------}
{ Classe TActionsObject }
{-----------------------}

{*
  Crée une instance de TActionsObject
  @param AActions   Actions propriétaires
*}
constructor TActionsObject.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObject, [AActions.Number]),
    AActions.FileName);
  FActions := AActions;

  Painter.ImgNames.Add(fCompatibility + Name);
end;

{*
  Nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @return Nombre d'objets que ce joueur possède
*}
function TActionsObject.GetCount(Player : TPlayer) : integer;
begin
  Result := Actions.Counter;
end;

{*
  Modifie le nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @param Value    Nouveau nombre d'objets
*}
procedure TActionsObject.SetCount(Player : TPlayer; Value : integer);
begin
  Actions.Counter := Value;
end;

{-----------------------}
{ Classe TActionsEffect }
{-----------------------}

{*
  Crée une instance de TActionsEffect
  @param AActions   Actions propriétaires
*}
constructor TActionsEffect.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsEffect, [AActions.Number]),
    Format(sButton, [AActions.Number]));
  FActions := AActions;

  FAlternatePainter := TPainter.Create(Master.ImagesMaster);
  FAlternatePainter.ImgNames.BeginUpdate;

  case Actions.Kind of
    akPushButton :
    begin
      Painter.ImgNames.Add(fButton);
      AlternatePainter.ImgNames.Add(fSunkenButton);
      FStaticDraw := False;
    end;
    akSwitch :
    begin
      Painter.ImgNames.Add(fSwitchOff);
      AlternatePainter.ImgNames.Add(fSwitchOn);
      FStaticDraw := False;
    end;
    akInfoStone :
      Painter.ImgNames.Add(fInfoStone);
    akTransporterNext..akTransporterRandom :
      Painter.ImgNames.Add(fTransporter);
    akOutside :
      Painter.ImgNames.Add(fOutside);
    akCustom..akDirection :
      Painter.ImgNames.Add(fCompatibility + Actions.FileName);
  end;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TActionsEffect.AfterConstruction;
begin
  inherited;
  FAlternatePainter.ImgNames.EndUpdate;
end;

{*
  Dessine le composant sur un canevas
  DoDraw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TActionsEffect.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  if Actions.Kind = akPushButton then
  begin
    if QPos.Map.PlayersOn(QPos.Position) = 0 then
      Painter.Draw(Canvas, X, Y)
    else
      AlternatePainter.Draw(Canvas, X, Y);
  end else
  if Actions.Kind = akSwitch then
  begin
    if Odd(Actions.Counter) then
      AlternatePainter.Draw(Canvas, X, Y)
    else
      Painter.Draw(Canvas, X, Y);
  end else
  inherited;
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TActionsEffect.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  FActions.Execute(phExecute, Player, KeyPressed, Pos, GoOnMoving);
end;

{-------------------------}
{ Classe TActionsObstacle }
{-------------------------}

{*
  Crée une instance de TActionsObstacle
  @param AActions   Actions propriétaires
*}
constructor TActionsObstacle.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObstacle,
    [AActions.Number]), Format(sButton, [AActions.Number]));
  FActions := AActions;
end;

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
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TActionsObstacle.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  AbortExecute := not AbortExecute;
  FActions.Execute(phPushing, Player, KeyPressed, Pos, AbortExecute);
  AbortExecute := not AbortExecute;
end;

{----------------------------}
{ Classe TActionsInterpreter }
{----------------------------}

{*
  Crée une instance de TActionsInterpreter
  @param AMaster     Maître FunLabyrinthe
  @param ANumber     Numéro d'actions
  @param AKind       Type d'actions
  @param AFileName   Nom du fichier de graphismes
*}
constructor TActions.Create(AMaster : TMaster; ANumber : integer;
  AKind : TActionsKind; const AFileName : string);
begin
  inherited Create(AMaster, Format(idActions, [ANumber]));

  FNumber := ANumber;
  FKind := AKind;
  FFileName := AFileName;
  FActions := TStringList.Create;
  FCounter := 0;

  if Kind <> akObject then FObjectDef := nil else
    FObjectDef := TActionsObject.Create(Self);
  FEffect := TActionsEffect.Create(Self);
  FObstacle := TActionsObstacle.Create(Self);
end;

{*
  Détruit l'instance
*}
destructor TActions.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{*
  Exécute les actions
  @param Phase         Phase courante (phPushing ou phExecute)
  @param Player        Joueur concerné
  @param KeyPressed    True si une touche a été pressée pour le déplacement
  @param Pos           Position de la case
  @param DoNextPhase   Indique s'il faut exécuter la phase suivante
*}
procedure TActions.Execute(Phase : integer; Player : TPlayer;
  KeyPressed : boolean; const Pos : T3DPoint; var DoNextPhase : boolean);
begin
  TActionsInterpreter.Execute(FActions, Master, Phase, Player,
    KeyPressed, Pos, DoNextPhase);
end;

end.

