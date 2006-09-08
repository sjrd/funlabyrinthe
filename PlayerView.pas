unit PlayerView;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

const {don't localize}
  DefaultViewSize = 9;       /// Taille par défaut d'une vue
  attrViewSize = 'ViewSize'; /// Nom d'attribut pour la taille de la vue

type
  TPlayerView = class
  private
    FMaster : TMaster; /// Maître FunLabyrinthe
    FPlayer : TPlayer; /// Joueur lié

    function GetSize : integer;
    procedure SetSize(Value : integer);
  public
    constructor Create(APlayer : TPlayer);

    procedure Draw(Canvas : TCanvas);

    property Master : TMaster read FMaster;
    property Player : TPlayer read FPlayer;
    property Size : integer read GetSize write SetSize;
  end;

implementation

//////////////////////////
/// Classe TPlayerView ///
//////////////////////////

{*
  Crée une instance de TPlayerView
  @param APlayer   Joueur lié
*}
constructor TPlayerView.Create(APlayer : TPlayer);
begin
  inherited Create;
  FMaster := APlayer.Master;
  FPlayer := APlayer;

  if Size = 0 then
    Size := DefaultViewSize;
end;

{*
  Taille de la vue
  @return Taille de la vue
*}
function TPlayerView.GetSize : integer;
begin
  Result := Player.Attribute[attrViewSize];
end;

{*
  Modifie la taille de la vue
  @param Nouvelle taille de la vue
*}
procedure TPlayerView.SetSize(Value : integer);
begin
  if (Value > 0) and Odd(Value) then
    Player.Attribute[attrViewSize] := Value;
end;

{*
  Dessine la vue sur un canevas
  @param Canvas   Canevas sur lequel dessiner la vue
*}
procedure TPlayerView.Draw(Canvas : TCanvas);
var Map : TMap;
    Size : integer;
    OrigX, OrigY : integer;
    X, Y, Z, I : integer;
begin
  Map := Player.Map;
  Size := GetSize; // éviter les recalculs intempestifs

  // Origine à la position du joueur
  OrigX := Player.Position.X;
  OrigY := Player.Position.Y;
  // Origine au niveau de la zone
  dec(OrigX, IntMod(OrigX, Map.ZoneSize));
  dec(OrigY, IntMod(OrigY, Map.ZoneSize));
  // Origine au niveau de la vue
  dec(OrigX, (Size-Map.ZoneSize) div 2);
  dec(OrigY, (Size-Map.ZoneSize) div 2);

  // Dessin des cases
  Z := Player.Position.Z;
  for X := 0 to Size-1 do for Y := 0 to Size-1 do
    Map[Point3D(OrigX+X, OrigY+Y, Z)].Draw(Canvas, X*ScrewSize, Y*ScrewSize);

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
    if Position.Z = Z then
      Draw(Canvas, (Position.X-OrigX)*ScrewSize, (Position.Y-OrigY)*ScrewSize);
end;

end.

