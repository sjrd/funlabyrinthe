{*
  Donne quelques outils de traitements des cartes de FunLabyrinthe
  L'unit� MapTools donne quelques routines utilitaires permettant
  d'effectuer facilement des traitements r�currents sur les cartes de
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
    Proc�dure recherchant un composant de case sur une carte
    @param Map         Carte sur laquelle chercher
    @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
    @param Component   Composant � rechercher
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
  Change le terrain d'une case et renvoie la case modifi�e
  @param Screw      Case originale
  @param NewField   ID du nouveau terrain
  @return Une case identique � Screw mais avec le terrain indiqu�
*}
function ChangeField(Screw : TScrew;
  const NewField : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      NewField, Effect.SafeID, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'effet d'une case et renvoie la case modifi�e
  @param Screw       Case originale
  @param NewEffect   ID du nouvel effet
  @return Une case identique � Screw mais avec l'effet indiqu�
*}
function ChangeEffect(Screw : TScrew;
  const NewEffect : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      Field.SafeID, NewEffect, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'outil d'une case et renvoie la case modifi�e
  @param Screw     Case originale
  @param NewTool   ID du nouvel outil
  @return Une case identique � Screw mais avec l'outil indiqu�
*}
function ChangeTool(Screw : TScrew;
  const NewTool : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      Field.SafeID, Effect.SafeID, NewTool, Obstacle.SafeID);
end;

{*
  Change l'obstacle d'une case et renvoie la case modifi�e
  @param Screw         Case originale
  @param NewObstacle   ID du nouvel obstacle
  @return Une case identique � Screw mais avec l'obstacle indiqu�
*}
function ChangeObstacle(Screw : TScrew;
  const NewObstacle : TComponentID = '') : TScrew;
begin
  with Screw do
    Result := Master.ScrewByComps(
      Field.SafeID, Effect.SafeID, Tool.SafeID, NewObstacle);
end;

{*
  Change un des composants d'une case et renvoie la case modifi�e
  @param Screw     Case originale
  @param NewComp   Composant � modifier
  @return Une case identique � Screw mais avec le composant indiqu�
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
  Change un des composants d'une case et renvoie la case modifi�e
  @param Screw     Case originale
  @param NewComp   ID du composant � modifier
  @return Une case identique � Screw mais avec le composant indiqu�
*}
function ChangeComp(Screw : TScrew; const NewComp : TComponentID) : TScrew;
begin
  Result := ChangeComp(Screw, Screw.Master.ScrewComponent[NewComp]);
end;

{*
  D�termine si la bonne case a �t� trouv�e
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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
  Trouve l'�l�ment de case suivant sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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
  Trouve l'�l�ment de case pr�c�dent sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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
  Trouve un autre �l�ment de case al�atoirement sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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

  // Recensement de toutes les cases identiques, � l'exception de l'originale
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

  // � moins que la liste soit vide, on en p�che un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

end.
