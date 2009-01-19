{*
  D�crit le comportement complet de la barque
  L'unit� FLBBoat regroupe tous les composants intervenant dans le
  fonctionnement de la barque.
  @author sjrd
  @version 5.0
*}
unit FLBBoat;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, GraphicsTools, MapTools, FLBCommon,
  FLBFields;

const {don't localize}
  idBoatPlugin = 'BoatPlugin'; /// ID du plug-in barque

resourcestring
  sBoat = 'Barque n�%d';    /// Nom de la barque
  sBoatTemplate = 'Barque'; /// Nom de la barque mod�le

const {don't localize}
  idBoat = 'Boat%d';               /// ID de la barque
  idBoatTemplate = 'BoatTemplate'; /// ID de la barque mod�le

  idBoatSquare = idGroundWater+'--'+idBoat+'-'; /// ID de la case barque
  idBoatSquareTemplate = idGroundWater+'--'+idBoatTemplate+'-'; /// Barque mod�le

const {don't localize}
  fBoat = 'Boat'; /// Fichier de la barque

resourcestring
  sBoatTitle = 'Num�ro de la barque';
  sBoatPrompt = 'Num�ro de la barque (1 � 10) :';

type
  {*
    Plug-in barque
    Affiche une barque sous le joueur, et permet d'aller dans l'eau. De plus,
    ce plug-in bloque un mouvement si la direction a chang�.
    @author sjrd
    @version 5.0
  *}
  TBoatPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
      Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); override;

    procedure Moving(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Dest: T3DPoint;
      var Cancel: Boolean); override;
    procedure Moved(Player: TPlayer; const Src, Dest: T3DPoint); override;

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; override;
  end;

  {*
    Barque
    La barque est un moyen de transport permettant d'aller sur l'eau.
    @author sjrd
    @version 5.0
  *}
  TBoat = class(TTool)
  private
    FNumber: Integer; /// Num�ro de la barque
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ANumber: Integer);

    procedure Find(Player: TPlayer; const Pos: T3DPoint); override;

    property Number: Integer read FNumber;
  end;

const
  clOutBoat = $00004080; /// Couleur de l'ext�rieur d'une barque
  clInBoat  = $00006699; /// Couleur de l'int�rieur d'une barque

implementation

{--------------------}
{ Classe TBoatPlugin }
{--------------------}

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
procedure TBoatPlugin.DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
  Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
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
      diNone, diNorth:
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
      diEast:
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
      diSouth:
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
      diWest:
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
procedure TBoatPlugin.Moving(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Dest: T3DPoint; var Cancel: Boolean);
begin
  if Player.Direction <> OldDirection then
    Cancel := True;
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Player   Joueur qui se d�place
  @param Src      Case de d�part
  @param Dest     Case d'arriv�e
*}
procedure TBoatPlugin.Moved(Player: TPlayer; const Src, Dest: T3DPoint);
begin
  with Player do
  begin
    if not (Map[Dest].Field is TWater) then
    begin
      // Retirer le plug-in barque
      RemovePlugin(Self);

      // Placer un outil barque sur la case source
      with Map[Src] do
        Map[Src] := Master.SquareByComps(idGroundWater, Effect.SafeID,
          Format(idBoat, [Attribute[idBoatPlugin]]), Obstacle.SafeID);

      // Remettre � 0 l'attribut du joueur concernant la barque
      Attribute[idBoatPlugin] := 0;
    end;
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
function TBoatPlugin.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := Action = actGoOnWater;
end;

{--------------}
{ Classe TBoat }
{--------------}

{*
  Cr�e une instance de TBoat
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'outil
  @param AName     Nom de l'outil
  @param ANumber   Num�ro de la barque
*}
constructor TBoat.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ANumber: Integer);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
  Painter.ImgNames.Add(fBoat);
end;

{*
  Dessine la barque sur le canevas indiqu�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TBoat.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  inherited;

  if Master.Editing and (Number <> 0) then
    DrawSquareNumber(Canvas, X, Y, Number);
end;

{*
  Ex�cut� lorsque le joueur trouve l'outil
  Find est ex�cut� lorsque le joueur trouve l'outil. C'est-�-dire lorsqu'il
  arrive sur une case sur laquelle se trouve l'outil.
  @param Player   Joueur qui a trouv� l'outil
  @param Pos      Position de la case
*}
procedure TBoat.Find(Player: TPlayer; const Pos: T3DPoint);
begin
  with Player do
  begin
    // Remplacement de la case par de l'eau simple
    with Map[Pos] do
      Map[Pos] := Master.SquareByComps(idWater, Effect.SafeID,
        '', Obstacle.SafeID);

    // Indication du num�ro de la barque dans l'attribut correspondant
    Attribute[idBoatPlugin] := Number;

    // Ajout du plug-in barque
    AddPlugin(Master.Plugin[idBoatPlugin]);
  end;
end;

end.

