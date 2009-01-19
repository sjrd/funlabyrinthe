{*
  Donne quelques outils de traitements des cartes de FunLabyrinthe
  L'unité MapTools donne quelques routines utilitaires permettant
  d'effectuer facilement des traitements récurrents sur les cartes de
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit MapTools;

interface

uses
  ScUtils, FunLabyUtils;

type
  {*
    Procédure recherchant un composant de case sur une carte
    @param Map         Carte sur laquelle chercher
    @param Pos         Début de la recherche en entrée et résultat en sortie
    @param Component   Composant à rechercher
  *}
  TFindSquareProc = procedure(Map: TMap; var Pos: T3DPoint;
    Component: TSquareComponent);

function ChangeField(Square: TSquare;
  const NewField: TComponentID = ''): TSquare;
function ChangeEffect(Square: TSquare;
  const NewEffect: TComponentID = ''): TSquare;
function ChangeTool(Square: TSquare;
  const NewTool: TComponentID = ''): TSquare;
function ChangeObstacle(Square: TSquare;
  const NewObstacle: TComponentID = ''): TSquare;

function ChangeComp(Square: TSquare;
  NewComp: TSquareComponent): TSquare; overload;
function ChangeComp(Square: TSquare;
  const NewComp: TComponentID): TSquare; overload;

procedure FindNextSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
procedure FindPreviousSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
procedure FindSquareAtRandom(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);

implementation

{*
  Change le terrain d'une case et renvoie la case modifiée
  @param Square      Case originale
  @param NewField   ID du nouveau terrain
  @return Une case identique à Square mais avec le terrain indiqué
*}
function ChangeField(Square: TSquare;
  const NewField: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      NewField, Effect.SafeID, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'effet d'une case et renvoie la case modifiée
  @param Square       Case originale
  @param NewEffect   ID du nouvel effet
  @return Une case identique à Square mais avec l'effet indiqué
*}
function ChangeEffect(Square: TSquare;
  const NewEffect: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, NewEffect, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'outil d'une case et renvoie la case modifiée
  @param Square     Case originale
  @param NewTool   ID du nouvel outil
  @return Une case identique à Square mais avec l'outil indiqué
*}
function ChangeTool(Square: TSquare;
  const NewTool: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, Effect.SafeID, NewTool, Obstacle.SafeID);
end;

{*
  Change l'obstacle d'une case et renvoie la case modifiée
  @param Square         Case originale
  @param NewObstacle   ID du nouvel obstacle
  @return Une case identique à Square mais avec l'obstacle indiqué
*}
function ChangeObstacle(Square: TSquare;
  const NewObstacle: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, Effect.SafeID, Tool.SafeID, NewObstacle);
end;

{*
  Change un des composants d'une case et renvoie la case modifiée
  @param Square     Case originale
  @param NewComp   Composant à modifier
  @return Une case identique à Square mais avec le composant indiqué
*}
function ChangeComp(Square: TSquare; NewComp: TSquareComponent): TSquare;
begin
  if NewComp is TField then
    Result := ChangeField(Square, NewComp.ID)
  else if NewComp is TEffect then
    Result := ChangeEffect(Square, NewComp.ID)
  else if NewComp is TTool then
    Result := ChangeTool(Square, NewComp.ID)
  else if NewComp is TObstacle then
    Result := ChangeObstacle(Square, NewComp.ID)
  else if NewComp is TSquare then
    Result := TSquare(NewComp)
  else
    Result := Square;
end;

{*
  Change un des composants d'une case et renvoie la case modifiée
  @param Square     Case originale
  @param NewComp   ID du composant à modifier
  @return Une case identique à Square mais avec le composant indiqué
*}
function ChangeComp(Square: TSquare; const NewComp: TComponentID): TSquare;
begin
  Result := ChangeComp(Square, Square.Master.SquareComponent[NewComp]);
end;

{*
  Détermine si la bonne case a été trouvée
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
function SquareCompFound(Map: TMap; const Pos: T3DPoint;
  Component: TSquareComponent): Boolean;
var
  Square: TSquare;
begin
  Square := Map[Pos];
  if Component is TField then
    Result := Square.Field = Component
  else if Component is TEffect then
    Result := Square.Effect = Component
  else if Component is TTool then
    Result := Square.Tool = Component
  else if Component is TObstacle then
    Result := Square.Obstacle = Component
  else
    Result := Square = Component;
end;

{*
  Trouve l'élément de case suivant sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
procedure FindNextSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
var
  DimX, DimY, DimZ: Integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    Inc(Pos.X);
    if Pos.X >= DimX then
    begin
      Pos.X := 0;
      Inc(Pos.Y);
      if Pos.Y >= DimY then
      begin
        Pos.Y := 0;
        Inc(Pos.Z);
        if Pos.Z >= DimZ then
          Pos.Z := 0;
      end;
    end;
  until SquareCompFound(Map, Pos, Component);
end;

{*
  Trouve l'élément de case précédent sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
procedure FindPreviousSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
var
  DimX, DimY, DimZ: Integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    Dec(Pos.X);
    if Pos.X < 0 then
    begin
      Pos.X := DimX-1;
      Dec(Pos.Y);
      if Pos.Y < 0 then
      begin
        Pos.Y := DimY-1;
        Dec(Pos.Z);
        if Pos.Z < 0 then
          Pos.Z := DimZ-1;
      end;
    end;
  until SquareCompFound(Map, Pos, Component);
end;

{*
  Trouve un autre élément de case aléatoirement sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
procedure FindSquareAtRandom(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
const
  AllocBy = 10;
var
  DimX, DimY, DimZ: Integer;
  Others: array of T3DPoint;
  Count, X, Y, Z: Integer;
  Other: T3DPoint;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  // Recensement de toutes les cases identiques, à l'exception de l'originale
  Count := 0;
  SetLength(Others, AllocBy);
  for X := 0 to DimX-1 do
  begin
    for Y := 0 to DimY-1 do
    begin
      for Z := 0 to DimZ-1 do
      begin
        Other := Point3D(X, Y, Z);
        if (SquareCompFound(Map, Other, Component)) and
          (not Same3DPoint(Other, Pos)) then
        begin
          if Count >= Length(Others) then
            SetLength(Others, Count+AllocBy);
          Others[Count] := Other;
          Inc(Count);
        end;
      end;
    end;
  end;
  SetLength(Others, Count);

  // À moins que la liste soit vide, on en pêche un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

end.

