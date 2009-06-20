{*
  Décrit le comportement complet de la barque
  L'unité FLBBoat regroupe tous les composants intervenant dans le
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
  sBoat = 'Barque n°%d';    /// Nom de la barque
  sBoatTemplate = 'Barque'; /// Nom de la barque modèle

const {don't localize}
  idBoat = 'Boat%d';               /// ID de la barque
  idBoatTemplate = 'BoatTemplate'; /// ID de la barque modèle

  idBoatSquare = idGroundWater+'--'+idBoat+'-'; /// ID de la case barque
  idBoatSquareTemplate = idGroundWater+'--'+idBoatTemplate+'-'; /// Barque modèle

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
    @author sjrd
    @version 5.0
  *}
  TBoatPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
      Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); override;

    procedure Moving(Context: TMoveContext); override;
    procedure Moved(Context: TMoveContext); override;

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
    FNumber: Integer; /// Numéro de la barque
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ANumber: Integer);

    procedure Find(Context: TMoveContext); override;

    property Number: Integer read FNumber;
  end;

const
  clOutBoat = $00004080; /// Couleur de l'extérieur d'une barque
  clInBoat  = $00006699; /// Couleur de l'intérieur d'une barque

implementation

{--------------------}
{ Classe TBoatPlugin }
{--------------------}

{*
  [@inheritDoc]
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
  [@inheritDoc]
*}
procedure TBoatPlugin.Moving(Context: TMoveContext);
begin
  with Context do
  begin
    if Player.Direction <> OldDirection then
      Cancel;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TBoatPlugin.Moved(Context: TMoveContext);
begin
  with Context do
  begin
    if not (DestSquare.Field is TWater) then
    begin
      // Retirer le plug-in barque
      Player.RemovePlugin(Self);

      // Placer un outil barque sur la case source
      with SrcSquare do
        SrcSquare := Master.SquareByComps(idGroundWater, Effect.SafeID,
          Format(idBoat, [Player.Attribute[idBoatPlugin]]), Obstacle.SafeID);

      // Remettre à 0 l'attribut du joueur concernant la barque
      Player.Attribute[idBoatPlugin] := 0;
    end;
  end;
end;

{*
  [@inheritDoc]
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
  Crée une instance de TBoat
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'outil
  @param AName     Nom de l'outil
  @param ANumber   Numéro de la barque
*}
constructor TBoat.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ANumber: Integer);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
  Painter.ImgNames.Add(fBoat);
end;

{*
  [@inheritDoc]
*}
procedure TBoat.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  inherited;

  if Master.Editing and (Number <> 0) then
    DrawSquareNumber(Canvas, X, Y, Number);
end;

{*
  [@inheritDoc]
*}
procedure TBoat.Find(Context: TMoveContext);
begin
  with Context do
  begin
    // Remplacement de la case par de l'eau simple
    with Square do
      Square := Master.SquareByComps(idWater, Effect.SafeID,
        '', Obstacle.SafeID);

    // Indication du numéro de la barque dans l'attribut correspondant
    Player.Attribute[idBoatPlugin] := Number;

    // Ajout du plug-in barque
    Player.AddPlugin(Master.Plugin[idBoatPlugin]);
  end;
end;

end.

