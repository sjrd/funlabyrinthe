unit Generics;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

type
  {*
    Effet d�coratif
    La classe TDecorativeEffect permet de cr�er facilement un effet qui ne fait
    rien, qui ajoute juste une touche d�corative.
    @author sjrd
    @version 5.0
  *}
  TDecorativeEffect = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName, AImgName: string);
  end;

  {*
    Obstacle d�coratif
    La classe TDecorativeObstacle permet de cr�er facilement un obstacle qui ne
    fait rien, qui ajoute juste une touche d�corative.
    @author sjrd
    @version 5.0
  *}
  TDecorativeObstacle = class(TObstacle)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName, AImgName: string);
  end;

  {*
    Outil li� � une d�finition d'objet
    La classe TObjectTool permet de cr�er facilement un outil qui est li� � une
    d�finition d'objet. C'est-�-dire que trouver cet outil incr�mente le nombre
    de tels objets que poss�de le joueur
    @author sjrd
    @version 5.0
  *}
  TObjectTool = class(TTool)
  private
    FObjectDef: TObjectDef; /// D�finition d'objet li�e
    FFindMessage: string;   /// Message apparaissant lorsqu'on trouve l'outil
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      AObjectDef: TObjectDef; const AFindMessage: string;
      const AName: string = ''; const AImgName: string = '');

    procedure Find(Context: TMoveContext); override;

    property ObjectDef: TObjectDef read FObjectDef;
    property FindMessage: string read FFindMessage;
  end;

  {*
    Classe de base pour les cases en � surchargeant � d'autres
    TOverriddenSquare est la classe de base pour les cases sp�ciales qui
    surchargent momentan�ment une autre case. Elle fournit des propri�t�s et
    m�thodes pour identifier la case en question et la dessiner.
    @author sjrd
    @version 5.0
  *}
  TOverriddenSquare = class(TSquare)
  private
    FMap: TMap;             /// Carte
    FPosition: T3DPoint;    /// Position
    FOriginalSquare: TSquare; /// Case originale
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
    property OriginalSquare: TSquare read FOriginalSquare;
  end;

implementation

uses
  MapTools;

{--------------------------}
{ Classe TDecorativeEffect }
{--------------------------}

{*
  Cr�e une instance de TDecorativeEffect
  @param AMaster    Ma�tre FunLabyrinthe
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

{----------------------------}
{ Classe TDecorativeObstacle }
{----------------------------}

{*
  Cr�e une instance de TDecorativeObstacle
  @param AMaster    Ma�tre FunLabyrinthe
  @param AID        ID de l'obstacle de case
  @param AName      Nom de l'obstacle de case
  @param AImgName   Nom du fichier image
*}
constructor TDecorativeObstacle.Create(AMaster: TMaster;
  const AID: TComponentID; const AName, AImgName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(AImgName);
end;

{--------------------}
{ Classe TObjectTool }
{--------------------}

{*
  Cr�e une instance de TObjectTool
  Les nom d'outil et du fichier image peuvent �tre vide. Dans ce cas, l'outil
  prend les m�mes que la d�finition d'objet � laquelle il est li�.
  @param AMaster        Ma�tre FunLabyrinthe
  @param AID            ID de l'outil
  @param AObjectDef     D�finition d'objet li�e
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
procedure TObjectTool.Find(Context: TMoveContext);
begin
  with Context do
  begin
    Square := RemoveTool(Square);
    ObjectDef.Count[Player] := ObjectDef.Count[Player] + 1;
    if FindMessage <> '' then
      Player.ShowDialog(sMessage, FindMessage);
  end;
end;

{-------------------------}
{ Classe TOverriddenSquare }
{-------------------------}

{*
  Cr�e une instance de TOverriddenSquare
  @param AMaster     Ma�tre FunLabyrinthe
  @param AID         ID de la case
  @param AMap        Carte
  @param APosition   Position
  @param AField      Terrain
  @param AEffect     Effet
  @param ATool       Outil
  @param AObstacle   Obstacle
*}
constructor TOverriddenSquare.Create(AMaster: TMaster; const AID: TComponentID;
  AMap: TMap; const APosition: T3DPoint; AField: TField = nil;
  AEffect: TEffect = nil; ATool: TTool = nil; AObstacle: TObstacle = nil);
var
  AOriginalSquare: TSquare;
begin
  AOriginalSquare := AMap[APosition];
  inherited Create(AMaster, AID, AOriginalSquare.Name,
    AField, AEffect, ATool, AObstacle);
  FRefCount := NoRefCount;

  if not AOriginalSquare.StaticDraw then
    FStaticDraw := False;
  FMap := AMap;
  FPosition := APosition;
  FOriginalSquare := AOriginalSquare;

  OriginalSquare.AddRef;
  Map[Position] := Self;
end;

{*
  D�truit l'instance
*}
destructor TOverriddenSquare.Destroy;
begin
  Map[Position] := OriginalSquare;
  OriginalSquare.Release;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TOverriddenSquare.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  OriginalSquare.Draw(QPos, Canvas, X, Y);
  inherited;
end;

end.

