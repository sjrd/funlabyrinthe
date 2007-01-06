{*
  D�crit le comportement complet de la planche
  L'unit� FLBPlank regroupe tous les composants intervenant dans le
  fonctionnement de la planche.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBPlank;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, Generics, FLBCommon;

const {don't localize}
  idPlankPlugin = 'PlankPlugin'; /// ID du plug-in planche

resourcestring
  sPlanks      = 'Planche';     /// Nom de l'objet planche
  sPlankInfos  = '%d planche';  /// Infos planches (singulier)
  sPlanksInfos = '%d planches'; /// Infos planches (pluriel)

  sPlank = 'Planche';           /// Nom de la planche

const {don't localize}
  idPlanks = 'Planks'; /// ID des planches

  idPlank = 'Plank';   /// ID de la planche

const {don't localize}
  fPlank = 'Plank'; /// Fichier de la planche

resourcestring
  sFoundPlank = 'Tu as trouv� une planche.'+#10+
                'Tu peux franchir certains obstacles.';

type
  {*
    Plug-in planche
    Affiche une planche � c�t� du joueur ou sous celui-ci.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); override;
  end;

  {*
    D�finition de l'objet planche
    La planche permet de passer au-dessus des cases
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlanks = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function AbleTo(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    Case sp�ciale planche
    Cette case est utilis�e pour le d�placement particulier de la planche.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlankScrew = class(TOverriddenScrew)
  private
    FPlayer : TPlayer; /// Joueur qui passe sur la case
  public
    constructor Create(AMaster : TMaster; AMap : TMap;
      const APosition : T3DPoint; APlayer : TPlayer);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;

    procedure Exited(Player : TPlayer; const Pos, Dest : T3DPoint); override;

    procedure Execute(Player : TPlayer; const Pos : T3DPoint;
      var GoOnMoving : boolean); override;

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
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlankPlugin.DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
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

{----------------}
{ Classe TPlanks }
{----------------}

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
function TPlanks.AbleTo(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actPassOverScrew) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action));
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
    Master.Temporize;
  end else inherited;
end;

{--------------------}
{ Classe TPlankScrew }
{--------------------}

{*
  Cr�e une instance de TPlankScrew
  @param AMaster     Ma�tre FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param APlayer     Joueur qui passe sur la case
*}
constructor TPlankScrew.Create(AMaster : TMaster; AMap : TMap;
  const APosition : T3DPoint; APlayer : TPlayer);
begin
  inherited Create(AMaster, '', AMap, APosition);
  FPlayer := APlayer;
end;

{*
  [@inheritDoc]
*}
procedure TPlankScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  if Player <> FPlayer then
    Cancel := True;
end;

{*
  [@inheritDoc]
*}
procedure TPlankScrew.Exited(Player : TPlayer; const Pos, Dest : T3DPoint);
begin
  inherited;
  Player.RemovePlugin(Master.Plugin[idPlankPlugin]);
  Free;
end;

{*
  [@inheritDoc]
*}
procedure TPlankScrew.Execute(Player : TPlayer; const Pos : T3DPoint;
  var GoOnMoving : boolean);
begin
  inherited;
  Master.Temporize;
  Player.MoveTo(PointBehind(Pos, Player.Direction), True, GoOnMoving);
end;

end.

