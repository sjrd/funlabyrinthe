{*
  Décrit les composants de cases
  L'unité Screws regroupe les différents composants de cases de compatibilité
  4.x de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xScrews;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, C4xCommon;

resourcestring
  sSunkenButton = 'Bouton enfoncé'; /// Nom du bouton enfoncé
  sButton = 'Bouton n°%d';          /// Nom du bouton
  sButtonTemplate = 'Bouton';       /// Nom du bouton modèle

const {don't localize}
  idActionsEffect = 'ActionsEffect%d';     /// ID de l'effet à actions
  idActionsObstacle = 'ActionsObstacle%d'; /// ID de l'obstacle à actions

  idSunkenButton = 'SunkenButton';         /// ID du bouton enfoncé
  idButtonTemplate = 'ButtonTemplate';     /// ID du bouton modèle

  /// ID de la case à actions
  idActionsScrew = idGrass+'-ActionsEffect%d-ActionsObstacle%0:d';
  /// ID de la case à actions modèle
  idActionsScrewTemplate = idGrass+'-'+idButtonTemplate+'-';

const {don't localize}
  fSunkenButton = 'SunkenButton'; /// Fichier du bouton enfoncé
  fButton = 'Button';             /// Fichier du bouton

resourcestring
  sButtonTitle = 'Numéro du bouton';
  sButtonPrompt = 'Numéro du bouton (1 à 75) :';

type
  {*
    Effet à actions
    Un effet à actions exécute une série d'actions lorsqu'on arrive dessus.
  *}
  TActionsEffect = class(TEffect)
  private
    FNumber : integer;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Obstacle à actions
    Un obstacle à actions exécute une série d'actions lorsqu'on pousse dessus.
  *}
  TActionsObstacle = class(TObstacle)
  private
    FNumber : integer;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

implementation

{-----------------------}
{ Classe TActionsEffect }
{-----------------------}

{*
  Crée une instance de TActionsEffect
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TActionsEffect.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ANumber : integer);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
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
procedure TActionsEffect.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
end;

{-------------------------}
{ Classe TActionsObstacle }
{-------------------------}

{*
  Crée une instance de TActionsObstacle
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TActionsObstacle.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ANumber : integer);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
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
procedure TActionsObstacle.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  inherited;
end;

end.

