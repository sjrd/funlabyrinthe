{*
  Barque de compatibilité 4.x
  @author sjrd
  @version 5.0
*}
unit C4xBoat;

interface

uses
  SysUtils, Classes, Graphics, ScUtils, SdDialogs, FunLabyUtils, MapTools,
  Generics, FLBCommon, FLBFields, FLBBoat;

resourcestring
  sNumberedBoat = 'Barque n°%d'; /// Nom d'une barque numérotée

const
  idOldBoatPlugin = 'OldBoatPlugin'; /// ID du plug-in barque de compatibilité
  fmtidBoat = 'Boat%d';              /// Format d'ID d'une barque

  fBoat = FLBBoat.fBoat; /// Nom du fichier image de la barque

  /// Attribut numéro de la barque
  attrBoatNumber = 'BoatNumber';

var
  attrtypeBoatNumber: Integer; /// Type de l'attribut numéro de la barque

type
  {*
    Plug-in barque
    Affiche une barque sous le joueur, et permet d'aller dans l'eau. De plus,
    ce plug-in bloque un mouvement si la direction a changé.
    @author sjrd
    @version 5.0
  *}
  TOldBoatPlugin = class(TPlugin)
  public
    procedure DrawBefore(Context: TDrawSquareContext); override;

    procedure Moving(Context: TMoveContext); override;
    procedure Moved(Context: TMoveContext); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;
  end;

  {*
    Barque de compatibilité
    @author sjrd
    @version 5.0
  *}
  TOldBoat = class(TGround)
  private
    FNumber: Integer; /// Numéro de la barque
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    constructor CreateNumbered(AMaster: TMaster; const AID: TComponentID;
      ANumber: Integer);

    procedure Entered(Context: TMoveContext); override;

    property Number: Integer read FNumber;
  end;

const
  clOutBoat = $00004080; /// Couleur de l'extérieur d'une barque
  clInBoat  = $00006699; /// Couleur de l'intérieur d'une barque

implementation

{----------------------}
{ TOldBoatPlugin class }
{----------------------}

{*
  [@inheritDoc]
*}
procedure TOldBoatPlugin.DrawBefore(Context: TDrawSquareContext);
begin
  inherited;

  with Context, Bitmap.Canvas do
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

    Bitmap.DeleteCanvas;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TOldBoatPlugin.Moving(Context: TMoveContext);
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
procedure TOldBoatPlugin.Moved(Context: TMoveContext);
begin
  with Context do
  begin
    if not (DestSquare.Field is TWater) then
    begin
      SrcSquare := ChangeField(SrcSquare, Format(fmtidBoat,
        [Integer(Player.Attributes[attrBoatNumber]^)]));

      Integer(Player.Attributes[attrBoatNumber]^) := 0;
      Player.RemovePlugin(Self);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
function TOldBoatPlugin.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := Action = actGoOnWater;
end;

{----------------}
{ TOldBoat class }
{----------------}

{*
  [@inheritDoc]
*}
constructor TOldBoat.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := sBoat;
  Painter.AddImage(fBoat);
end;

{*
  Crée une barque numérotée
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de la barque
  @param ANumber   Numéro de la barque
*}
constructor TOldBoat.CreateNumbered(AMaster: TMaster; const AID: TComponentID;
  ANumber: Integer);
begin
  Create(AMaster, AID);

  FNumber := ANumber;

  Name := Format(sNumberedBoat, [ANumber]);
  EditVisualTag := IntToStr(Number);
end;

{*
  [@inheritDoc]
*}
procedure TOldBoat.Entered(Context: TMoveContext);
begin
  with Context do
  begin
    Integer(Player.Attributes[attrBoatNumber]^) := Number;
    Player.AddPlugin(Master.Plugin[idOldBoatPlugin]);

    Square := ChangeField(Square, idWater);
  end;
end;

end.

