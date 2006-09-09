unit PlayerView;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

const {don't localize}
  attrViewSize = 'ViewSize'; /// Nom d'attribut pour la taille de la vue

type
  TPlayerView = class
  private
    FMaster : TMaster; /// Maître FunLabyrinthe
    FPlayer : TPlayer; /// Joueur lié

    function GetMinSize : integer;
    function GetMaxSize : integer;

    function GetSize : integer;
    procedure SetSize(Value : integer);
    function GetTotalSize : integer;
  public
    constructor Create(APlayer : TPlayer);

    procedure Draw(Canvas : TCanvas);

    property Master : TMaster read FMaster;
    property Player : TPlayer read FPlayer;
    property MinSize : integer read GetMinSize;
    property MaxSize : integer read GetMaxSize;
    property Size : integer read GetSize write SetSize;
    property TotalSize : integer read GetTotalSize;
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

  Size := Size; // Mettre Size dans les bornes [MinSize ; MaxSize]
end;

{*
  Taille minimale de la vue
  @return Taille minimale de la vue
*}
function TPlayerView.GetMinSize : integer;
begin
  Result := MinViewSize;
end;

{*
  Taille maximale de la vue
  @return Taille maximale de la vue
*}
function TPlayerView.GetMaxSize : integer;
begin
  Result := Player.Map.MaxViewSize;
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
  Player.Attribute[attrViewSize] := MinMax(Value, MinSize, MaxSize);
end;

{*
  Taille totale affichée par la vue
  @return Taille totale affichée par la vue
*}
function TPlayerView.GetTotalSize : integer;
begin
  Result := Player.Map.ZoneSize + 2*Size;
end;

{*
  Dessine la vue sur un canevas
  @param Canvas   Canevas sur lequel dessiner la vue
*}
procedure TPlayerView.Draw(Canvas : TCanvas);
var Map : TMap;
    Size, TotalSize : integer;
    OrigX, OrigY : integer;
    X, Y, Z, I : integer;
begin
  // Simplifier et accélérer les accès aux informations
  Map := Player.Map;
  Size := GetSize;
  TotalSize := GetTotalSize;

  // Origine à la position du joueur
  OrigX := Player.Position.X;
  OrigY := Player.Position.Y;
  // Origine au niveau de la zone
  dec(OrigX, IntMod(OrigX, Map.ZoneSize));
  dec(OrigY, IntMod(OrigY, Map.ZoneSize));
  // Origine au niveau de la vue
  dec(OrigX, Size);
  dec(OrigY, Size);

  // Dessin des cases
  Z := Player.Position.Z;
  for X := 0 to TotalSize-1 do for Y := 0 to TotalSize-1 do
    Map[Point3D(OrigX+X, OrigY+Y, Z)].Draw(Canvas, X*ScrewSize, Y*ScrewSize);

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
    if Position.Z = Z then
      Draw(Canvas, (Position.X-OrigX)*ScrewSize, (Position.Y-OrigY)*ScrewSize);
end;

end.

