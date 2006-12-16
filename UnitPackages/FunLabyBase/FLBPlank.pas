{*
  Décrit le comportement complet de la planche
  L'unité FLBPlank regroupe tous les composants intervenant dans le
  fonctionnement de la planche.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBPlank;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, FLBCommon;

const {don't localize}
  idPlankPlugin = 'PlankPlugin'; /// ID du plug-in planche

resourcestring
  sPlanks      = 'Planche';     /// Nom de l'objet planche
  sPlankInfos  = '%d planche';  /// Infos planches (singulier)
  sPlanksInfos = '%d planches'; /// Infos planches (pluriel)

  sPlank = 'Planche';           /// Nom de la planche

const {don't localize}
  idPlanks = 'Planks';            /// ID des planches

  idPlankField = 'PlankField';    /// ID du terrain planche
  idPlankEffect = 'PlankEffect';  /// ID de l'effet planche
  idPlankScrew = 'PlankScrew-%s'; /// ID de la case planche

  idPlank = 'Plank';              /// ID de la planche

const {don't localize}
  fPlank = 'Plank'; /// Fichier de la planche

resourcestring
  sFoundPlank = 'Tu as trouvé une planche.'+#10+
                'Tu peux franchir certains obstacles.';

type
  {*
    Plug-in planche
    Affiche une planche à côté du joueur ou sous celui-ci.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); override;
  end;

  {*
    Définition de l'objet planche
    La planche permet de passer au-dessus des cases
    @author Sébastien Jean Robert Doeraene
    @version 5.0
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
    Terrain spécial planche
    Ce terrain ne doit pas être utilisé normalement. Il n'est utilisé que par la
    case spéciale planche.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankField = class(TField)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Effet spécial planche
    Cet effet ne doit pas être utilisé normalement. Il n'est utilisé que par la
    case spéciale planche.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankEffect = class(TEffect)
  public
    procedure Exited(Player : TPlayer; const Pos, Dest : T3DPoint); override;

    procedure Execute(Player : TPlayer; const Pos : T3DPoint;
      var GoOnMoving : boolean); override;
  end;

  {*
    Case spéciale planche
    Cette case est utilisée pour le déplacement particulier de la planche.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankScrew = class(TOverriddenScrew)
  private
    FPlayer : TPlayer; /// Joueur qui passe sur la case
  public
    constructor Create(AMaster : TMaster; AMap : TMap; APosition : T3DPoint;
      APlayer : TPlayer);

    property Player : TPlayer read FPlayer;
  end;

const
  clPlank = $00004080; /// Couleur de la planche

implementation

{---------------------}
{ Classe TPlankPlugin }
{---------------------}

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessiné
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlankPlugin.DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  inherited;

  // Détermination de l'endroit où dessiner réellement la planche
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

{----------------}
{ Classe TPlanks }
{----------------}

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
begin
  if Action = actPassOverScrew then with Player do
  begin
    TPlankScrew.Create(Master, Map, PointBehind(Position, Direction), Player);
    AddPlugin(Master.Plugin[idPlankPlugin]);
    Master.Temporize;
  end else inherited;
end;

{--------------------}
{ Classe TPlankField }
{--------------------}

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
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  if Player <> (Player.Map[Src] as TPlankScrew).Player then
    Cancel := True;
end;

{---------------------}
{ Classe TPlankEffect }
{---------------------}

{*
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player   Joueur qui se déplace
  @param Pos      Position de la case
  @param Dest     Case de destination
*}
procedure TPlankEffect.Exited(Player : TPlayer; const Pos, Dest : T3DPoint);
begin
  inherited;
  Player.RemovePlugin(Master.Plugin[idPlankPlugin]);
  Player.Map[Pos].Free;
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TPlankEffect.Execute(Player : TPlayer; const Pos : T3DPoint;
  var GoOnMoving : boolean);
begin
  inherited;
  GoOnMoving := True;
end;

{--------------------}
{ Classe TPlankScrew }
{--------------------}

{*
  Crée une instance de TPlankScrew
  @param AMaster     Maître FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param APlayer     Joueur qui passe sur la case
*}
constructor TPlankScrew.Create(AMaster : TMaster; AMap : TMap;
  APosition : T3DPoint; APlayer : TPlayer);
begin
  inherited Create(AMaster, Format(idPlankScrew, [APlayer.ID]), AMap, APosition,
    AMaster.Field[idPlankField], AMaster.Effect[idPlankEffect], nil, nil);
  FPlayer := APlayer;
end;

end.

