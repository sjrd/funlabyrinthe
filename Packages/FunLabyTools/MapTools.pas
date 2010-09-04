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

{$ASSERTIONS ON}

uses
  SysUtils, ScUtils, FunLabyUtils, FunLabyToolsConsts;

type
  {*
    Procédure recherchant un composant de case sur une carte
    @param Map         Carte sur laquelle chercher
    @param Pos         Début de la recherche en entrée et résultat en sortie
    @param Component   Composant à rechercher
  *}
  TFindSquareProc = procedure(Map: TMap; var Pos: T3DPoint;
    Component: TSquareComponent);

  TPosComponentPredicate = function(PosComponent: TPosComponent): Boolean;

function ChangeField(Square: TSquare; NewField: TField): TSquare; overload;
function ChangeField(Square: TSquare;
  const NewField: TComponentID): TSquare; overload;

function ChangeEffect(Square: TSquare; NewEffect: TEffect): TSquare; overload;
function ChangeEffect(Square: TSquare;
  const NewEffect: TComponentID): TSquare; overload;
function RemoveEffect(Square: TSquare): TSquare;

function ChangeTool(Square: TSquare; NewTool: TTool): TSquare; overload;
function ChangeTool(Square: TSquare;
  const NewTool: TComponentID): TSquare; overload;
function RemoveTool(Square: TSquare): TSquare;

function ChangeObstacle(Square: TSquare;
  NewObstacle: TObstacle): TSquare; overload;
function ChangeObstacle(Square: TSquare;
  const NewObstacle: TComponentID): TSquare; overload;
function RemoveObstacle(Square: TSquare): TSquare;

function ChangeComp(Square: TSquare;
  NewComp: TSquareComponent): TSquare; overload;
function ChangeComp(Square: TSquare;
  const NewComp: TComponentID): TSquare; overload;

function MakeSquare(const Components: array of TSquareComponent): TSquare;

procedure FindNextSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
procedure FindPreviousSquare(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);
procedure FindSquareAtRandom(Map: TMap; var Pos: T3DPoint;
  Component: TSquareComponent);

function IsAnyPosComponent(const QPos: TQualifiedPos;
  Predicate: TPosComponentPredicate): Boolean; overload;
function IsAnyPosComponent(const QPos: TQualifiedPos): Boolean; overload;
function IsAnySquareModifier(const QPos: TQualifiedPos): Boolean;

implementation

{*
  Change le terrain d'une case et renvoie la case modifiée
  @param Square     Case originale
  @param NewField   Nouveau terrain
  @return Une case identique à Square mais avec le terrain indiqué
*}
function ChangeField(Square: TSquare; NewField: TField): TSquare;
begin
  Result := ChangeField(Square, NewField.SafeID);
end;

{*
  Change le terrain d'une case et renvoie la case modifiée
  @param Square     Case originale
  @param NewField   ID du nouveau terrain
  @return Une case identique à Square mais avec le terrain indiqué
*}
function ChangeField(Square: TSquare; const NewField: TComponentID): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      NewField, Effect.SafeID, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Change l'effet d'une case et renvoie la case modifiée
  @param Square      Case originale
  @param NewEffect   Nouvel effet
  @return Une case identique à Square mais avec l'effet indiqué
*}
function ChangeEffect(Square: TSquare; NewEffect: TEffect): TSquare;
begin
  Result := ChangeEffect(Square, NewEffect.SafeID);
end;

{*
  Change l'effet d'une case et renvoie la case modifiée
  @param Square      Case originale
  @param NewEffect   ID du nouvel effet
  @return Une case identique à Square mais avec l'effet indiqué
*}
function ChangeEffect(Square: TSquare; const NewEffect: TComponentID): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, NewEffect, Tool.SafeID, Obstacle.SafeID);
end;

{*
  Retire l'effet d'une case et renvoie la case modifiée
  @param Square   Case originale
  @return Une case identique à Square mais sans effect
*}
function RemoveEffect(Square: TSquare): TSquare;
begin
  Result := ChangeEffect(Square, '');
end;

{*
  Change l'outil d'une case et renvoie la case modifiée
  @param Square    Case originale
  @param NewTool   Nouvel outil
  @return Une case identique à Square mais avec l'outil indiqué
*}
function ChangeTool(Square: TSquare; NewTool: TTool): TSquare;
begin
  Result := ChangeTool(Square, NewTool.SafeID);
end;

{*
  Change l'outil d'une case et renvoie la case modifiée
  @param Square    Case originale
  @param NewTool   ID du nouvel outil
  @return Une case identique à Square mais avec l'outil indiqué
*}
function ChangeTool(Square: TSquare; const NewTool: TComponentID): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, Effect.SafeID, NewTool, Obstacle.SafeID);
end;

{*
  Retire l'outil d'une case et renvoie la case modifiée
  @param Square   Case originale
  @return Une case identique à Square mais sans outil
*}
function RemoveTool(Square: TSquare): TSquare;
begin
  Result := ChangeTool(Square, '');
end;

{*
  Change l'obstacle d'une case et renvoie la case modifiée
  @param Square        Case originale
  @param NewObstacle   Nouvel obstacle
  @return Une case identique à Square mais avec l'obstacle indiqué
*}
function ChangeObstacle(Square: TSquare; NewObstacle: TObstacle): TSquare;
begin
  Result := ChangeObstacle(Square, NewObstacle.SafeID);
end;

{*
  Change l'obstacle d'une case et renvoie la case modifiée
  @param Square        Case originale
  @param NewObstacle   ID du nouvel obstacle
  @return Une case identique à Square mais avec l'obstacle indiqué
*}
function ChangeObstacle(Square: TSquare;
  const NewObstacle: TComponentID): TSquare;
begin
  with Square do
    Result := Master.SquareByComps(
      Field.SafeID, Effect.SafeID, Tool.SafeID, NewObstacle);
end;

{*
  Retire l'obstacle d'une case et renvoie la case modifiée
  @param Square   Case originale
  @return Une case identique à Square mais sans obstacle
*}
function RemoveObstacle(Square: TSquare): TSquare;
begin
  Result := ChangeObstacle(Square, '');
end;

{*
  Change un des composants d'une case et renvoie la case modifiée
  @param Square    Case originale
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
  @param Square    Case originale
  @param NewComp   ID du composant à modifier
  @return Une case identique à Square mais avec le composant indiqué
*}
function ChangeComp(Square: TSquare; const NewComp: TComponentID): TSquare;
begin
  Result := ChangeComp(Square, Square.Master.SquareComponent[NewComp]);
end;

{*
  Renseigne un ID partiel de case
  @param DestID     ID destination
  @param SourceID   ID source
  @param ErrorMsg   Message d'erreur si DestID était déjà renseigné
  @throw EBadSquareDefException L'ID destination était déjà renseigné
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
  Construit une case à partir de un à plusieurs composants
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
    end else if Component <> nil then
      Assert(False);
  end;

  if FieldID = '' then
    raise EBadSquareDefException.Create(SMissingField);

  Result := Components[0].Master.Square[Format(SquareIDFormat,
    [FieldID, EffectID, ToolID, ObstacleID])];
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

{*
  Teste s'il y a un composant à position à un endroit donné de la carte
  @param QPos        Position où tester
  @param Predicate   Prédicat qui filtre les composants pris en compte
  @return True si au moins un composant a été trouvé, False sinon
*}
function IsAnyPosComponent(const QPos: TQualifiedPos;
  Predicate: TPosComponentPredicate): Boolean;
var
  Master: TMaster;
  I: Integer;
  PosComponent: TPosComponent;
begin
  if QPos.IsNoQPos then
  begin
    Result := False;
    Exit;
  end;

  Master := QPos.Map.Master;

  for I := 0 to Master.PosComponentCount-1 do
  begin
    PosComponent := Master.PosComponents[I];

    if SameQPos(PosComponent.QPos, QPos) and Predicate(PosComponent) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{*
  Prédicat qui vaut toujours True
*}
function PosComponentAlwaysTrue(PosComponent: TPosComponent): Boolean;
begin
  Result := True;
end;

{*
  Teste s'il y a un composant à position à un endroit donné de la carte
  @param QPos   Position où tester
  @return True si au moins un composant a été trouvé, False sinon
*}
function IsAnyPosComponent(const QPos: TQualifiedPos): Boolean;
begin
  Result := IsAnyPosComponent(QPos, PosComponentAlwaysTrue);
end;

{*
  Prédicat qui vaut True pour les modificateurs de case
*}
function IsSquareModifier(PosComponent: TPosComponent): Boolean;
begin
  Result := PosComponent is TSquareModifier;
end;

{*
  Teste s'il y a un modificateur de case à un endroit donné de la carte
  @param QPos   Position où tester
  @return True si au moins un modificateur a été trouvé, False sinon
*}
function IsAnySquareModifier(const QPos: TQualifiedPos): Boolean;
begin
  Result := IsAnyPosComponent(QPos, IsSquareModifier);
end;

end.

