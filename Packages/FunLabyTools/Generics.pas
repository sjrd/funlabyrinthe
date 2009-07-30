unit Generics;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

const
  fButton = 'Buttons/Button';             /// Image du bouton
  fSunkenButton = 'Buttons/SunkenButton'; /// Image du bouton enfoncé
  fSwitchOff = 'Buttons/SwitchOff';       /// Image de l'interrupteur off
  fSwitchOn = 'Buttons/SwitchOn';         /// Image de l'interrupteur on

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
    Données d'un joueur pour TCounterEffect
    @author sjrd
    @version 5.0
  *}
  TCounterEffectPlayerData = class(TPlayerData)
  private
    FCounter: Integer; /// Compteur
  published
    property Counter: Integer read FCounter write FCounter default 0;
  end;

  {*
    Effet qui maintient un compteur par joueur
    @author sjrd
    @version 5.0
  *}
  TCounterEffect = class(TEffect)
  private
    FGlobalCounter: Integer; /// Compteur global (cumul de tous les joueurs)

    function GetCounter(Player: TPlayer): Integer;
    procedure SetCounter(Player: TPlayer; Value: Integer);
  protected
    class function GetPlayerDataClass: TPlayerDataClass; override;

    procedure IncCounter(Player: TPlayer);

    function IsFirstTime(Player: TPlayer): Boolean;

    property Counter[Player: TPlayer]: Integer read GetCounter write SetCounter;
  public
    procedure Execute(Context: TMoveContext); override;
  published
    property GlobalCounter: Integer read FGlobalCounter write FGlobalCounter;
  end;

  {*
    Bouton poussoir
    @author sjrd
    @version 5.0
  *}
  TPushButton = class(TCounterEffect)
  private
    FDownPainter: TPainter; /// Peintre pour le bouton enfoncé
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;

    property DownPainter: TPainter read FDownPainter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
    destructor Destroy; override;
    
    procedure AfterConstruction; override;
  end;

  {*
    Interrupteur
    @author sjrd
    @version 5.0
  *}
  TSwitch = class(TCounterEffect)
  private
    FIsOn: Boolean; /// Indique si la position est On

    FOffPainter: TPainter; /// Peintre pour la position Off
    FOnPainter: TPainter;  /// Peintre pour la position On
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;

    property OffPainter: TPainter read FOffPainter;
    property OnPainter: TPainter read FOnPainter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Execute(Context: TMoveContext); override;
  published
    property IsOn: Boolean read FIsOn write FIsOn default False;
  end;

  {*
    Obstacle décoratif
    La classe TDecorativeObstacle permet de créer facilement un obstacle qui ne
    fait rien, qui ajoute juste une touche décorative.
    @author sjrd
    @version 5.0
  *}
  TDecorativeObstacle = class(TObstacle)
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
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      AObjectDef: TObjectDef; const AFindMessage: string;
      const AName: string = ''; const AImgName: string = '');

    procedure Find(Context: TMoveContext); override;

    property ObjectDef: TObjectDef read FObjectDef;
    property FindMessage: string read FFindMessage;
  end;

  {*
    Classe de base pour les cases en « surchargeant » d'autres
    TOverriddenSquare est la classe de base pour les cases spéciales qui
    surchargent momentanément une autre case. Elle fournit des propriétés et
    méthodes pour identifier la case en question et la dessiner.
    @author sjrd
    @version 5.0
  *}
  TOverriddenSquare = class(TSquare)
  private
    FMap: TMap;               /// Carte
    FPosition: T3DPoint;      /// Position
    FOriginalSquare: TSquare; /// Case originale
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
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

{-----------------------}
{ Classe TCounterEffect }
{-----------------------}

{*
  Valeur du compteur pour un joueur donné
  @param Player   Joueur concerné
  @return Valeur du compteur pour le joueur spécifié
*}
function TCounterEffect.GetCounter(Player: TPlayer): Integer;
begin
  Result := TCounterEffectPlayerData(GetPlayerData(Player)).Counter;
end;

{*
  Modifie la valeur du compteur pour un joueur donné
  @param Player   Joueur concerné
  @param Value    Nouvelle valeur du compteur
*}
procedure TCounterEffect.SetCounter(Player: TPlayer; Value: Integer);
begin
  TCounterEffectPlayerData(GetPlayerData(Player)).Counter := Value;
end;

{*
  [@inheritDoc]
*}
class function TCounterEffect.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TCounterEffectPlayerData;
end;

{*
  Incrémente le compteur pour un joueur donné
  @param Player   Joueur concerné
*}
procedure TCounterEffect.IncCounter(Player: TPlayer);
begin
  with TCounterEffectPlayerData(GetPlayerData(Player)) do
    Counter := Counter + 1;
end;

{*
  Teste si c'est la première fois pour un joueur qu'active cet effet
  @param Player   Joueur concerné
  @return True si c'est la première fois, False sinon
*}
function TCounterEffect.IsFirstTime(Player: TPlayer): Boolean;
begin
  Result := Counter[Player] = 1;
end;

{*
  [@inheritDoc]
*}
procedure TCounterEffect.Execute(Context: TMoveContext);
begin
  inherited;

  Inc(FGlobalCounter);
  IncCounter(Context.Player);
end;

{--------------------}
{ Classe TPushButton }
{--------------------}

{*
  Crée une instance de TButton
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom du terrain
*}
constructor TPushButton.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  FStaticDraw := Master.Editing;

  FDownPainter := TPainter.Create(Master.ImagesMaster);
  FDownPainter.ImgNames.BeginUpdate;

  Painter.ImgNames.Add(fButton);
  DownPainter.ImgNames.Add(fSunkenButton);
end;

{*
  [@inheritDoc]
*}
destructor TPushButton.Destroy;
begin
  FDownPainter.Free;
  
  inherited Destroy;
end;

{*
  [@inheritDoc]
*}
procedure TPushButton.DoDraw(Context: TDrawSquareContext);
begin
  if Context.IsNowhere or (Context.Map.PlayersOn(Context.Pos) = 0) then
    Painter.Draw(Context)
  else
    DownPainter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TPushButton.AfterConstruction;
begin
  inherited;
  
  FDownPainter.ImgNames.EndUpdate;
end;

{----------------}
{ Classe TSwitch }
{----------------}

{*
  Crée une instance de TSwitch
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom du terrain
*}
constructor TSwitch.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  FStaticDraw := Master.Editing;
  FOffPainter := Painter;

  FOnPainter := TPainter.Create(Master.ImagesMaster);
  FOnPainter.ImgNames.BeginUpdate;

  OffPainter.ImgNames.Add(fSwitchOff);
  OnPainter.ImgNames.Add(fSwitchOn);
end;

{*
  [@inheritDoc]
*}
destructor TSwitch.Destroy;
begin
  FOnPainter.Free;
  
  inherited Destroy;
end;

{*
  [@inheritDoc]
*}
procedure TSwitch.DoDraw(Context: TDrawSquareContext);
begin
  if IsOn then
    OnPainter.Draw(Context)
  else
    OffPainter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TSwitch.AfterConstruction;
begin
  inherited;
  
  FOnPainter.ImgNames.EndUpdate;
end;

{*
  [@inheritDoc]
*}
procedure TSwitch.Execute(Context: TMoveContext);
begin
  inherited;

  IsOn := not IsOn;
end;

{----------------------------}
{ Classe TDecorativeObstacle }
{----------------------------}

{*
  Crée une instance de TDecorativeObstacle
  @param AMaster    Maître FunLabyrinthe
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
procedure TObjectTool.DoDraw(Context: TDrawSquareContext);
begin
  inherited;

  if Painter.ImgNames.Count = 0 then
    FObjectDef.Draw(Context);
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
      Player.ShowMessage(FindMessage);
  end;
end;

{--------------------------}
{ Classe TOverriddenSquare }
{--------------------------}

{*
  Crée une instance de TOverriddenSquare
  @param AMaster     Maître FunLabyrinthe
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

  if not AOriginalSquare.StaticDraw then
    FStaticDraw := False;
  FMap := AMap;
  FPosition := APosition;
  FOriginalSquare := AOriginalSquare;

  Map[Position] := Self;
end;

{*
  [@inheritDoc]
*}
destructor TOverriddenSquare.Destroy;
begin
  if Assigned(OriginalSquare) then
    Map[Position] := OriginalSquare;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TOverriddenSquare.DoDraw(Context: TDrawSquareContext);
begin
  OriginalSquare.Draw(Context);
  inherited;
end;

end.

