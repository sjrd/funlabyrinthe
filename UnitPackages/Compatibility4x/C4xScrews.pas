{*
  D�crit les composants de cases
  L'unit� Screws regroupe les diff�rents composants de cases de compatibilit�
  4.x de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xScrews;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, C4xCommon;

resourcestring
  sSunkenButton = 'Bouton enfonc�'; /// Nom du bouton enfonc�
  sButton = 'Bouton n�%d';          /// Nom du bouton
  sButtonTemplate = 'Bouton';       /// Nom du bouton mod�le

const {don't localize}
  idActionsEffect = 'ActionsEffect%d';     /// ID de l'effet � actions
  idActionsObstacle = 'ActionsObstacle%d'; /// ID de l'obstacle � actions

  idSunkenButton = 'SunkenButton';         /// ID du bouton enfonc�
  idButtonTemplate = 'ButtonTemplate';     /// ID du bouton mod�le

  /// ID de la case � actions
  idActionsScrew = idGrass+'-ActionsEffect%d-ActionsObstacle%0:d';
  /// ID de la case � actions mod�le
  idActionsScrewTemplate = idGrass+'-'+idButtonTemplate+'-';

const {don't localize}
  fSunkenButton = 'SunkenButton'; /// Fichier du bouton enfonc�
  fButton = 'Button';             /// Fichier du bouton

resourcestring
  sButtonTitle = 'Num�ro du bouton';
  sButtonPrompt = 'Num�ro du bouton (1 � 75) :';

type
  {*
    Effet � actions
    Un effet � actions ex�cute une s�rie d'actions lorsqu'on arrive dessus.
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
    Obstacle � actions
    Un obstacle � actions ex�cute une s�rie d'actions lorsqu'on pousse dessus.
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
  Cr�e une instance de TActionsEffect
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
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
  Cr�e une instance de TActionsObstacle
  @param AMaster   Ma�tre FunLabyrinthe
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
procedure TActionsObstacle.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  inherited;
end;

end.

