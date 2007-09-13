unit Generics;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

type
  {*
    Effet décoratif
    La classe TDecorativeEffect permet de créer facilement un effet qui ne fait
    rien, qui ajoute juste une touche décorative.
    @author sjrd
    @version 5.0
  *}
  TDecorativeEffect = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName, AImgName: string);
  end;

  {*
    Outil lié à une définition d'objet
    La classe TObjectTool permet de créer facilement un outil qui est lié à une
    définition d'objet. C'est-à-dire que trouver cet outil incrémente le nombre
    de tels objets que possède le joueur
    @author sjrd
    @version 5.0
  *}
  TObjectTool = class(TTool)
  private
    FObjectDef: TObjectDef; /// Définition d'objet liée
    FFindMessage: string;   /// Message apparaissant lorsqu'on trouve l'outil
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      AObjectDef: TObjectDef; const AFindMessage: string;
      const AName: string = ''; const AImgName: string = '');

    procedure Find(Player: TPlayer; const Pos: T3DPoint); override;

    property ObjectDef: TObjectDef read FObjectDef;
    property FindMessage: string read FFindMessage;
  end;

  {*
    Classe de base pour les cases en « surchargeant » d'autres
    TOverriddenScrew est la classe de base pour les cases spéciales qui
    surchargent momentanément une autre case. Elle fournit des propriétés et
    méthodes pour identifier la case en question et la dessiner.
    @author sjrd
    @version 5.0
  *}
  TOverriddenScrew = class(TScrew)
  private
    FMap: TMap;             /// Carte
    FPosition: T3DPoint;    /// Position
    FOriginalScrew: TScrew; /// Case originale
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      AMap: TMap; const APosition: T3DPoint; AField: TField = nil;
      AEffect: TEffect = nil; ATool: TTool = nil;
      AObstacle: TObstacle = nil);
    destructor Destroy; override;

    property Map: TMap read FMap;
    property Position: T3DPoint read FPosition;
    property OriginalScrew: TScrew read FOriginalScrew;
  end;

implementation

uses
  MapTools;

{--------------------------}
{ Classe TDecorativeEffect }
{--------------------------}

{*
  Crée une instance de TDecorativeEffect
  @param AMaster    Maître FunLabyrinthe
  @param AID        ID de l'effet de case
  @param AName      Nom de l'effet de case
  @param AImgName   Nom du fichier image
*}
constructor TDecorativeEffect.Create(AMaster: TMaster;
  const AID: TComponentID; const AName, AImgName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(AImgName);
end;

{--------------------}
{ Classe TObjectTool }
{--------------------}

{*
  Crée une instance de TObjectTool
  Les nom d'outil et du fichier image peuvent être vide. Dans ce cas, l'outil
  prend les mêmes que la définition d'objet à laquelle il est lié.
  @param AMaster        Maître FunLabyrinthe
  @param AID            ID de l'outil
  @param AObjectDef     Définition d'objet liée
  @param AFindMessage   Message apparaissant lorsqu'on trouve l'outil
  @param AName          Nom de l'outil
  @param AImgName       Nom du fichier image
*}
constructor TObjectTool.Create(AMaster: TMaster; const AID: TComponentID;
  AObjectDef: TObjectDef; const AFindMessage: string;
  const AName: string = ''; const AImgName: string = '');
begin
  inherited Create(AMaster, AID, IIF(AName = '', AObjectDef.Name, AName));
  FObjectDef := AObjectDef;
  FFindMessage := AFindMessage;

  if AImgName = '' then
    FStaticDraw := FObjectDef.StaticDraw
  else
    Painter.ImgNames.Add(AImgName);
end;

{*
  [@inheritDoc]
*}
procedure TObjectTool.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  inherited;

  if Painter.ImgNames.Count = 0 then
    FObjectDef.Draw(QPos, Canvas, X, Y);
end;

{*
  [@inheritDoc]
*}
procedure TObjectTool.Find(Player: TPlayer; const Pos: T3DPoint);
begin
  Player.Map[Pos] := ChangeTool(Player.Map[Pos]);
  ObjectDef.Count[Player] := ObjectDef.Count[Player] + 1;
  if FindMessage <> '' then
    Player.ShowDialog(sMessage, FindMessage);
end;

{-------------------------}
{ Classe TOverriddenScrew }
{-------------------------}

{*
  Crée une instance de TOverriddenScrew
  @param AMaster     Maître FunLabyrinthe
  @param AID         ID de la case
  @param AMap        Carte
  @param APosition   Position
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TOverriddenScrew.Create(AMaster: TMaster; const AID: TComponentID;
  AMap: TMap; const APosition: T3DPoint; AField: TField = nil;
  AEffect: TEffect = nil; ATool: TTool = nil; AObstacle: TObstacle = nil);
var
  AOriginalScrew: TScrew;
begin
  AOriginalScrew := AMap[APosition];
  inherited Create(AMaster, AID, AOriginalScrew.Name,
    AField, AEffect, ATool, AObstacle);
  FRefCount := NoRefCount;

  if not AOriginalScrew.StaticDraw then
    FStaticDraw := False;
  FMap := AMap;
  FPosition := APosition;
  FOriginalScrew := AOriginalScrew;

  OriginalScrew.AddRef;
  Map[Position] := Self;
end;

{*
  Détruit l'instance
*}
destructor TOverriddenScrew.Destroy;
begin
  Map[Position] := OriginalScrew;
  OriginalScrew.Release;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TOverriddenScrew.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  OriginalScrew.Draw(QPos, Canvas, X, Y);
  inherited;
end;

end.

