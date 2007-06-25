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

const {don't localize}
  attrUsePlank = 'UsePlank'; /// Attribut indiquant l'usage de la planche

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

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Dest : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Définition de l'objet planche
    La planche permet de passer au-dessus des cases
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TPlanks = class(TObjectDef)
  protected
    procedure SetCount(Player : TPlayer; Value : integer); override;

    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);
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

  if Player.Attribute[attrUsePlank] = 0 then exit;

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

{*
  [@inheritDoc]
*}
procedure TPlankPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Dest : T3DPoint; var Cancel : boolean);
var Behind : T3DPoint;
    Msg : TPlankMessage;
    Field : TField;
begin
  Behind := PointBehind(Dest, Player.Direction);

  Msg.MsgID := msgPlank;
  Msg.Kind := pmkPassOver;
  Msg.Result := False;
  Msg.Player := Player;
  Msg.Pos := Dest;
  Msg.Src := Src;
  Msg.Dest := Behind;

  with Player do
  begin
    // On vérifie que la case du milieu peut être survolée
    Field := Map[Msg.Pos].Field;
    if Assigned(Field) then
      Field.Dispatch(Msg);
    if not Msg.Result then exit;

    // On vérifie que la case de départ ou d'arrivée autorise le déplacement
    Msg.Kind := pmkLeaveFrom;
    Msg.Result := False;
    Field := Map[Msg.Src].Field;
    if Assigned(Field) then
      Field.Dispatch(Msg);
    if not Msg.Result then
    begin
      Msg.Kind := pmkArriveAt;
      Field := Map[Msg.Dest].Field;
      if Assigned(Field) then
        Field.Dispatch(Msg);
      if not Msg.Result then exit;
    end;

    TPlankScrew.Create(Master, Map, Msg.Pos, Player);
    Master.Temporize;
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
  [@inheritDoc]
*}
procedure TPlanks.SetCount(Player : TPlayer; Value : integer);
begin
  inherited;

  if Value > 0 then
    Player.AddPlugin(Master.Plugin[idPlankPlugin])
  else
    Player.RemovePlugin(Master.Plugin[idPlankPlugin]);
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
  const APosition : T3DPoint; APlayer : TPlayer);
begin
  inherited Create(AMaster, '', AMap, APosition);
  FPlayer := APlayer;
  FPlayer.Attribute[attrUsePlank] := 1;
end;

{*
  [@inheritDoc]
*}
procedure TPlankScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  inherited;
  if Player <> FPlayer then
    Cancel := True;
end;

{*
  [@inheritDoc]
*}
procedure TPlankScrew.Exited(Player : TPlayer; const Pos, Dest : T3DPoint);
begin
  inherited;
  FPlayer.Attribute[attrUsePlank] := 0;
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

