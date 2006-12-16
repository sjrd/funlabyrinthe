{*
  Décrit le comportement complet de la barque
  L'unité FLBBoat regroupe tous les composants intervenant dans le
  fonctionnement de la barque.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBBoat;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, FLBCommon, FLBFields;

const {don't localize}
  idBoatPlugin = 'BoatPlugin'; /// ID du plug-in barque

resourcestring
  sBoat = 'Barque n°%d';    /// Nom de la barque
  sBoatTemplate = 'Barque'; /// Nom de la barque modèle

const {don't localize}
  idBoat = 'Boat%d';               /// ID de la barque
  idBoatTemplate = 'BoatTemplate'; /// ID de la barque modèle

  idBoatScrew = idGrassWater+'--'+idBoat+'-'; /// ID de la case barque
  idBoatScrewTemplate = idGrassWater+'--'+idBoatTemplate+'-'; /// Barque modèle

const {don't localize}
  fBoat = 'Boat'; /// Fichier de la barque

resourcestring
  sBoatTitle = 'Numéro de la barque';
  sBoatPrompt = 'Numéro de la barque (1 à 10) :';

type
  {*
    Plug-in barque
    Affiche une barque sous le joueur, et permet d'aller dans l'eau. De plus,
    ce plug-in bloque un mouvement si la direction a changé.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TBoatPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); override;

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Dest : T3DPoint;
      var Cancel : boolean); override;
    procedure Moved(Player : TPlayer; const Src, Dest : T3DPoint); override;

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
  end;

  {*
    Barque
    La barque est un moyen de transport permettant d'aller sur l'eau.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TBoat = class(TTool)
  private
    FNumber : integer; /// Numéro de la barque
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer);

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    procedure Find(Player : TPlayer; const Pos : T3DPoint); override;

    property Number : integer read FNumber;
  end;

const
  clOutBoat = $00004080; /// Couleur de l'extérieur d'une barque
  clInBoat  = $00006699; /// Couleur de l'intérieur d'une barque

implementation

{--------------------}
{ Classe TBoatPlugin }
{--------------------}

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
procedure TBoatPlugin.DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
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
      diNone, diNorth :
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
  Un joueur se déplace
  Moving est exécuté lorsqu'un joueur se déplace d'une case à une autre. Pour
  annuler le déplacement, Moving peut positionner le paramètre Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de départ
  @param Dest           Case d'arrivée
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TBoatPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Dest : T3DPoint; var Cancel : boolean);
begin
  if Player.Direction <> OldDirection then
    Cancel := True;
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Player   Joueur qui se déplace
  @param Src      Case de départ
  @param Dest     Case d'arrivée
*}
procedure TBoatPlugin.Moved(Player : TPlayer; const Src, Dest : T3DPoint);
begin
  with Player do if not (Map[Dest].Field is TWater) then
  begin
    // Retirer le plug-in barque
    RemovePlugin(Self);

    // Placer un outil barque sur la case source
    Map[Src] := Map[Src].ChangeField(idGrassWater).ChangeTool(
      Format(idBoat, [Attribute[idBoatPlugin]]));

    // Remettre à 0 l'attribut du joueur concernant la barque
    Attribute[idBoatPlugin] := 0;
  end;
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TBoatPlugin.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := Action = actGoOnWater;
end;

{--------------}
{ Classe TBoat }
{--------------}

{*
  Crée une instance de TBoat
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'outil
  @param AName     Nom de l'outil
  @param ANumber   Numéro de la barque
*}
constructor TBoat.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ANumber : integer);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
  Painter.ImgNames.Add(fBoat);
end;

{*
  Dessine la barque sur le canevas indiqué
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonnée X du point à partir duquel dessiner le terrain
  @param Y        Coordonnée Y du point à partir duquel dessiner le terrain
*}
procedure TBoat.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited;

  if Master.Editing and (Number <> 0) then
    DrawScrewNumber(Canvas, X, Y, Number);
end;

{*
  Exécuté lorsque le joueur trouve l'outil
  Find est exécuté lorsque le joueur trouve l'outil. C'est-à-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Player   Joueur qui a trouvé l'outil
  @param Pos      Position de la case
*}
procedure TBoat.Find(Player : TPlayer; const Pos : T3DPoint);
begin
  inherited;

  with Player do
  begin
    // Remplacement de la case par de l'eau simple
    Map[Pos] := Map[Pos].ChangeField(idWater);

    // Indication du numéro de la barque dans l'attribut correspondant
    Attribute[idBoatPlugin] := Number;

    // Ajout du plug-in barque
    AddPlugin(Master.Plugin[idBoatPlugin]);
  end;
end;

end.

