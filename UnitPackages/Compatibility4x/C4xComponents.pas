{*
  D�crit les classes de composants de compatibilit� 4.x
  L'unit� C4xScrews regroupe les diff�rentes classes de composants de
  compatibilit� 4.x de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xComponents;

interface

uses
  SysUtils, Classes, Graphics, ScUtils, FunLabyUtils, C4xCommon, C4xInterpreter;

resourcestring
  sSunkenButton = 'Bouton enfonc�'; /// Nom du bouton enfonc�
  sButton = 'Bouton n�%d';          /// Nom du bouton
  sButtonTemplate = 'Bouton';       /// Nom du bouton mod�le

const {don't localize}
  idActionsObject = 'ActionsObject%d';     /// ID de l'objet li� � des actions
  idActionsEffect = 'ActionsEffect%d';     /// ID de l'effet � actions
  idActionsObstacle = 'ActionsObstacle%d'; /// ID de l'obstacle � actions

  idSunkenButton = 'SunkenButton';         /// ID du bouton enfonc�
  idButtonTemplate = 'ButtonTemplate';     /// ID du bouton mod�le

  /// ID de la case � actions
  idActionsScrew = idGrass+'-ActionsEffect%d-ActionsObstacle%0:d';
  /// ID de la case � actions mod�le
  idActionsScrewTemplate = idGrass+'-'+idButtonTemplate+'-';

  idActions = 'Actions%d'; /// ID d'un ensemble d'actions

const {don't localize}
  fSunkenButton = 'SunkenButton'; /// Fichier du bouton enfonc�
  fButton = 'Button';             /// Fichier du bouton

resourcestring
  sButtonTitle = 'Num�ro du bouton';
  sButtonPrompt = 'Num�ro du bouton (1 � 75) :';

type
  TActions = class;

  {*
    D�finition d'objet li� � des actions
    Les nombre des objets li�s � des actions est le compteur de ces actions.
  *}
  TActionsObject = class(TObjectDef)
  private
    FActions : TActions; /// Actions propri�taires
  protected
    function GetCount(Player : TPlayer) : integer; override;
    procedure SetCount(Player : TPlayer; Value : integer); override;
  public
    constructor Create(AActions : TActions);

    property Actions : TActions read FActions;
  end;

  {*
    Effet � actions
    Un effet � actions ex�cute une s�rie d'actions lorsqu'on arrive dessus.
  *}
  TActionsEffect = class(TEffect)
  private
    FActions : TActions; /// Actions propri�taires
  public
    constructor Create(AActions : TActions);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Actions : TActions read FActions;
  end;

  {*
    Obstacle � actions
    Un obstacle � actions ex�cute une s�rie d'actions lorsqu'on pousse dessus.
  *}
  TActionsObstacle = class(TObstacle)
  private
    FActions : TActions; /// Actions propri�taires
  public
    constructor Create(AActions : TActions);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;

    property Actions : TActions read FActions;
  end;

  {*
    Repr�sente un ensemble d'actions pour une case active particuli�re
  *}
  TActions = class(TFunLabyComponent)
  private
    FNumber : integer;            /// Num�ro d'actions
    FActions : TStrings;          /// Actions � ex�cuter
    FCounter : integer;           /// Compteur d'ex�cution
    FObjectDef : TActionsObject;  /// Objet correspondant
    FEffect : TActionsEffect;     /// Effet correspondant
    FObstacle : TActionsObstacle; /// Obstacle correspondant

    procedure SetActions(Value : TStrings);
  public
    constructor Create(AMaster : TMaster; ANumber : integer);
    destructor Destroy; override;

    procedure Execute(Phase : integer; Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var DoNextPhase : boolean);

    property Number : integer read FNumber;
    property Actions : TStrings read FActions write SetActions;
    property Counter : integer read FCounter write FCounter;
    property ObjectDef : TActionsObject read FObjectDef;
    property Effect : TActionsEffect read FEffect;
    property Obstacle : TActionsObstacle read FObstacle;
  end;

implementation

{-----------------------}
{ Classe TActionsObject }
{-----------------------}

{*
  Cr�e une instance de TActionsObject
  @param AActions   Actions propri�taires
*}
constructor TActionsObject.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObject, [AActions.Number]),
    Format(sButton, [AActions.Number]));
  FActions := AActions;
end;

{*
  Nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
  @return Nombre d'objets que ce joueur poss�de
*}
function TActionsObject.GetCount(Player : TPlayer) : integer;
begin
  Result := Actions.Counter;
end;

{*
  Modifie le nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
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
  Cr�e une instance de TActionsEffect
  @param AActions   Actions propri�taires
*}
constructor TActionsEffect.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsEffect, [AActions.Number]),
    Format(sButton, [AActions.Number]));
  FActions := AActions;
end;

{*
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
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
  Cr�e une instance de TActionsObstacle
  @param AActions   Actions propri�taires
*}
constructor TActionsObstacle.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObstacle,
    [AActions.Number]), Format(sButton, [AActions.Number]));
  FActions := AActions;
end;

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
  @param AbortEntered   � positionner � True pour emp�cher le Entered
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
  Cr�e une instance de TActionsInterpreter
  @param AMaster   Ma�tre FunLabyrinthe
  @param ANumber   Num�ro d'actions
*}
constructor TActions.Create(AMaster : TMaster; ANumber : integer);
begin
  inherited Create(AMaster, Format(idActions, [ANumber]));

  FNumber := ANumber;
  FActions := TStringList.Create;
  FCounter := 0;
  FObjectDef := TActionsObject.Create(Self);
  FEffect := TActionsEffect.Create(Self);
  FObstacle := TActionsObstacle.Create(Self);
end;

{*
  D�truit l'instance
*}
destructor TActions.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{*
  Modifie les actions � ex�cuter
  @param Value   Nouvelles actions
*}
procedure TActions.SetActions(Value : TStrings);
begin
  FActions.Assign(Value);
end;

{*
  Ex�cute les actions
  @param Phase         Phase courante (phPushing ou phExecute)
  @param Player        Joueur concern�
  @param KeyPressed    True si une touche a �t� press�e pour le d�placement
  @param Pos           Position de la case
  @param DoNextPhase   Indique s'il faut ex�cuter la phase suivante
*}
procedure TActions.Execute(Phase : integer; Player : TPlayer;
  KeyPressed : boolean; const Pos : T3DPoint; var DoNextPhase : boolean);
begin
  TActionsInterpreter.Execute(FActions, Master, Phase, Player,
    KeyPressed, Pos, DoNextPhase);
end;

end.

