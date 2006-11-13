{*
  Donne quelques outils de traitements sur FunLabyrinthe
  L'unit� FunLabyTools donne quelques routines utilitaires permettant
  d'effectuer facilement des traitements r�currents dans FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyTools;

interface

uses
  ScUtils, FunLabyUtils;

procedure FindNextScrew(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
procedure FindPreviousScrew(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);
procedure FindScrewAtRandom(Map : TMap; var Pos : T3DPoint;
  Component : TScrewComponent);

implementation

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
