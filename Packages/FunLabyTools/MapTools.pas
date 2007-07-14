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
  TFindScrewProc = procedure(Map : TMap; var Pos : T3DPoint;
    Component : TScrewComponent);

function ChangeField(Screw : TScrew;
  const NewField : TComponentID = '') : TScrew;
function ChangeEffect(Screw : TScrew;
  const NewEffect : TComponentID = '') : TScrew;
function ChangeTool(Screw : TScrew;
  const NewTool : TComponentID = '') : TScrew;
function ChangeObstacle(Screw : TScrew;
  const NewObstacle : TComponentID = '') : TScrew;

function ChangeComp(Screw : TScrew;
  NewComp : TScrewComponent) : TScrew; overload;
function ChangeComp(Screw : TScrew;
  const NewComp : TComponentID) : TScrew; overload;

procedure FindNextScrew(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
procedure FindPreviousScrew(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
procedure FindScrewAtRandom(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);

implementation

{*
  Change le terrain d'une case et renvoie la case modifiée
  @param Screw      Case originale
  @param NewField   ID du nouveau terrain
  @return Une case identique à Screw mais avec le terrain indiqué
*}
function ChangeField(Screw : TScrew;
  const NewField : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      NewField, Effect.SafeID, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'effet d'une case et renvoie la case modifiée
  @param Screw       Case originale
  @param NewEffect   ID du nouvel effet
  @return Une case identique à Screw mais avec l'effet indiqué
*}
function ChangeEffect(Screw : TScrew;
  const NewEffect : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      Field.SafeID, NewEffect, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'outil d'une case et renvoie la case modifiée
  @param Screw     Case originale
  @param NewTool   ID du nouvel outil
  @return Une case identique à Screw mais avec l'outil indiqué
*}
function ChangeTool(Screw : TScrew;
  const NewTool : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      Field.SafeID, Effect.SafeID, NewTool, Obstacle.SafeID);
end;

{*
  Change l'obstacle d'une case et renvoie la case modifiée
  @param Screw         Case originale
  @param NewObstacle   ID du nouvel obstacle
  @return Une case identique à Screw mais avec l'obstacle indiqué
*}
function ChangeObstacle(Screw : TScrew;
  const NewObstacle : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      Field.SafeID, Effect.SafeID, Tool.SafeID, NewObstacle);
end;

{*
  Change un des composants d'une case et renvoie la case modifiée
  @param Screw     Case originale
  @param NewComp   Composant à modifier
  @return Une case identique à Screw mais avec le composant indiqué
*}
function ChangeComp(Screw : TScrew; NewComp : TScrewComponent) : TScrew;
begin
  if NewComp is TField    then Result := ChangeField   (Screw, NewComp.ID) else
  if NewComp is TEffect   then Result := ChangeEffect  (Screw, NewComp.ID) else
  if NewComp is TTool     then Result := ChangeTool    (Screw, NewComp.ID) else
  if NewComp is TObstacle then Result := ChangeObstacle(Screw, NewComp.ID) else

  if NewComp is TScrew then Result := TScrew(NewComp) else
  Result := Screw;
end;

{*
  Change un des composants d'une case et renvoie la case modifiée
  @param Screw     Case originale
  @param NewComp   ID du composant à modifier
  @return Une case identique à Screw mais avec le composant indiqué
*}
function ChangeComp(Screw : TScrew; const NewComp : TComponentID) : TScrew;
begin
  Result := ChangeComp(Screw, Screw.Master.ScrewComponent[NewComp]);
end;

{*
  Détermine si la bonne case a été trouvée
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
function ScrewCompFound(Map : TMap; const Pos : T3DPoint;
  Component : TScrewComponent) : boolean;
var Screw : TScrew;
begin
  Screw := Map[Pos];
  if Component is TField    then Result := Screw.Field    = Component else
  if Component is TEffect   then Result := Screw.Effect   = Component else
  if Component is TTool     then Result := Screw.Tool     = Component else
  if Component is TObstacle then Result := Screw.Obstacle = Component else
  Result := Screw = Component;
end;

{*
  Trouve l'élément de case suivant sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
procedure FindNextScrew(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
var DimX, DimY, DimZ : integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    inc(Pos.X);
    if Pos.X >= DimX then
    begin
      Pos.X := 0;
      inc(Pos.Y);
      if Pos.Y >= DimY then
      begin
        Pos.Y := 0;
        inc(Pos.Z);
        if Pos.Z >= DimZ then
          Pos.Z := 0;
      end;
    end;
  until ScrewCompFound(Map, Pos, Component);
end;

{*
  Trouve l'élément de case précédent sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
procedure FindPreviousScrew(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
var DimX, DimY, DimZ : integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    dec(Pos.X);
    if Pos.X < 0 then
    begin
      Pos.X := DimX-1;
      dec(Pos.Y);
      if Pos.Y < 0 then
      begin
        Pos.Y := DimY-1;
        dec(Pos.Z);
        if Pos.Z < 0 then
          Pos.Z := DimZ-1;
      end;
    end;
  until ScrewCompFound(Map, Pos, Component);
end;

{*
  Trouve un autre élément de case aléatoirement sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         Début de la recherche en entrée et résultat en sortie
  @param Component   Composant à rechercher
*}
procedure FindScrewAtRandom(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
const
  AllocBy = 10;
var DimX, DimY, DimZ : integer;
    Others : array of T3DPoint;
    Count, X, Y, Z : integer;
    Other : T3DPoint;
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
  for X := 0 to DimX-1 do for Y := 0 to DimY-1 do for Z := 0 to DimZ-1 do
  begin
    Other := Point3D(X, Y, Z);
    if (ScrewCompFound(Map, Other, Component)) and
       (not Same3DPoint(Other, Pos)) then
    begin
      if Count >= Length(Others) then
        SetLength(Others, Count+AllocBy);
      Others[Count] := Other;
      inc(Count);
    end;
  end;
  SetLength(Others, Count);

  // À moins que la liste soit vide, on en pêche un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

end.
