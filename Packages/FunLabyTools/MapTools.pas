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

{$ASSERTIONS ON}

uses
  SysUtils, ScUtils, FunLabyUtils, FunLabyToolsConsts;

type
  {*
    Proc�dure recherchant un composant de case sur une carte
    @param Map         Carte sur laquelle chercher
    @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
    @param Component   Composant � rechercher
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

function MakeSquare(const Components: array of TSquareComponent): TSquare;

function MakeSquareNoOpenArray(Component1: TSquareComponent;
  Component2: TSquareComponent = nil; Component3: TSquareComponent = nil;
  Component4: TSquareComponent = nil): TSquare;

procedure FindNextSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
procedure FindPreviousSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
procedure FindSquareAtRandom(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);

implementation

{*
  Change le terrain d'une case et renvoie la case modifi�e
  @param Square     Case originale
  @param NewField   ID du nouveau terrain
  @return Une case identique � Square mais avec le terrain indiqu�
*}
function ChangeField(Square: TSquare;
  const NewField: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      NewField, Effect.SafeID, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'effet d'une case et renvoie la case modifi�e
  @param Square      Case originale
  @param NewEffect   ID du nouvel effet
  @return Une case identique � Square mais avec l'effet indiqu�
*}
function ChangeEffect(Square: TSquare;
  const NewEffect: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, NewEffect, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'outil d'une case et renvoie la case modifi�e
  @param Square    Case originale
  @param NewTool   ID du nouvel outil
  @return Une case identique � Square mais avec l'outil indiqu�
*}
function ChangeTool(Square: TSquare;
  const NewTool: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, Effect.SafeID, NewTool, Obstacle.SafeID);
end;

{*
  Change l'obstacle d'une case et renvoie la case modifi�e
  @param Square        Case originale
  @param NewObstacle   ID du nouvel obstacle
  @return Une case identique � Square mais avec l'obstacle indiqu�
*}
function ChangeObstacle(Square: TSquare;
  const NewObstacle: TComponentID = ''): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, Effect.SafeID, Tool.SafeID, NewObstacle);
end;

{*
  Change un des composants d'une case et renvoie la case modifi�e
  @param Square    Case originale
  @param NewComp   Composant � modifier
  @return Une case identique � Square mais avec le composant indiqu�
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
  Change un des composants d'une case et renvoie la case modifi�e
  @param Square    Case originale
  @param NewComp   ID du composant � modifier
  @return Une case identique � Square mais avec le composant indiqu�
*}
function ChangeComp(Square: TSquare; const NewComp: TComponentID): TSquare;
begin
  Result := ChangeComp(Square, Square.Master.SquareComponent[NewComp]);
end;

{*
  Renseigne un ID partiel de case
  @param DestID     ID destination
  @param SourceID   ID source
  @param ErrorMsg   Message d'erreur si DestID �tait d�j� renseign�
  @throw EBadSquareDefException L'ID destination �tait d�j� renseign�
*}
procedure SetSquarePartialID(var DestID: TComponentID;
  const SourceID: TComponentID; const ErrorMsg: string);
begin
  if SourceID = '' then
    Exit;

  if DestID <> '' then
    raise EBadSquareDefException.Create(ErrorMsg);

  DestID := SourceID;
end;

{*
  Construit une case � partir de un � plusieurs composants
  @param Components   Composants de la case
  @return Case construite
*}
function MakeSquare(const Components: array of TSquareComponent): TSquare;
var
  FieldID, EffectID, ToolID, ObstacleID: TComponentID;
  I: Integer;
  Component: TSquareComponent;
  Square: TSquare;
begin
  Assert(Length(Components) > 0);

  for I := Low(Components) to High(Components) do
  begin
    Component := Components[I];

    if Component is TField then
      SetSquarePartialID(FieldID, Component.ID, SDuplicateField)
    else if Component is TEffect then
      SetSquarePartialID(EffectID, Component.ID, SDuplicateEffect)
    else if Component is TTool then
      SetSquarePartialID(ToolID, Component.ID, SDuplicateTool)
    else if Component is TObstacle then
      SetSquarePartialID(ObstacleID, Component.ID, SDuplicateObstacle)
    else if Component is TSquare then
    begin
      Square := TSquare(Component);
      SetSquarePartialID(FieldID, Square.Field.SafeID, SDuplicateField);
      SetSquarePartialID(EffectID, Square.Effect.SafeID, SDuplicateEffect);
      SetSquarePartialID(ToolID, Square.Tool.SafeID, SDuplicateTool);
      SetSquarePartialID(ObstacleID, Square.Obstacle.SafeID,
        SDuplicateObstacle);
    end else
      Assert(False);
  end;

  if FieldID = '' then
    raise EBadSquareDefException.Create(SMissingField);

  Result := Components[0].Master.Square[Format(SquareIDFormat,
    [FieldID, EffectID, ToolID, ObstacleID])];
end;

{*
  Construit une case � partir de un � plusieurs composants
  Cette variante de MakeSquare n'utilise pas de tableau ouvert, ce qui permet
  de s'en servir dans la version actuelle de Sepi.
  @param Component1   Premier composant de la case
  @param Component2   Deuxi�me composant de la case
  @param Component3   Troisi�me composant de la case
  @param Component4   Quatri�me composant de la case
  @return Case construite
*}
function MakeSquareNoOpenArray(Component1: TSquareComponent;
  Component2: TSquareComponent = nil; Component3: TSquareComponent = nil;
  Component4: TSquareComponent = nil): TSquare;
var
  Components: array[0..3] of TSquareComponent;
  Count: Integer;
begin
  Components[0] := Component1;
  Components[1] := Component2;
  Components[2] := Component3;
  Components[3] := Component4;

  Count := 1;
  while (Count < 4) and (Components[Count] <> nil) do
    Inc(Count);

  Result := MakeSquare(Slice(Components, Count));
end;

{*
  D�termine si la bonne case a �t� trouv�e
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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
  Trouve l'�l�ment de case suivant sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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
  Trouve l'�l�ment de case pr�c�dent sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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
  Trouve un autre �l�ment de case al�atoirement sur la carte
  @param Map         Carte sur laquelle chercher
  @param Pos         D�but de la recherche en entr�e et r�sultat en sortie
  @param Component   Composant � rechercher
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

  // Recensement de toutes les cases identiques, � l'exception de l'originale
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

  // � moins que la liste soit vide, on en p�che un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

end.

