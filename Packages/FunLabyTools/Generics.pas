unit Generics;

interface

uses
  Graphics, ScUtils, FunLabyUtils, FunLabyCoreConsts, FunLabyToolsConsts;

const
  fButton = 'Buttons/Button';             /// Image du bouton
  fSunkenButton = 'Buttons/SunkenButton'; /// Image du bouton enfonc�
  fSwitchOff = 'Buttons/SwitchOff';       /// Image de l'interrupteur off
  fSwitchOn = 'Buttons/SwitchOn';         /// Image de l'interrupteur on

const
  /// Message li� � la planche
  msgPlank = $10;

type
  {*
    Type de message li� � la planche
    - plkPassOver : Test sur la case au-dessus de laquelle on passe
    - plkArriveAt : Test sur la case sur laquelle on arriverait
    - plkLeaveFrom : Test sur la case de laquelle on vient
  *}
  TPlankMessageKind = (pmkPassOver, pmkLeaveFrom, pmkArriveAt);

  {*
    Message li� � la planche
    @author sjrd
    @version 5.0
  *}
  TPlankMessage = record
    MsgID: Word;             /// ID du message
    Kind: TPlankMessageKind; /// Type de message
    Result: Boolean;         /// True pour autoriser, False sinon
    Player: TPlayer;         /// Joueur concern�
    Pos: T3DPoint;           /// Case au-dessus de laquelle on passe
    Src: T3DPoint;           /// Case dont on vient
    Dest: T3DPoint;          /// Case vers laquelle on va
  end;

type
  {*
    Sol
    Le sol est le terrain de base de FunLabyrinthe. Il n'a pas de condition. On
    peut en cr�er plusieurs versions, avec des graphismes diff�rents.
    @author sjrd
    @version 5.0
  *}
  TGround = class(TField)
  private
    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  public
    constructor CreateGround(AMaster: TMaster; const AID: TComponentID;
      const AName, AImgName: string);
  end;

  {*
    Effet d�coratif
    La classe TDecorativeEffect permet de cr�er facilement un effet qui ne fait
    rien, qui ajoute juste une touche d�corative.
    @author sjrd
    @version 5.0
  *}
  TDecorativeEffect = class(TEffect)
  protected
    function GetCategory: string; override;
  public
    constructor CreateDeco(AMaster: TMaster; const AID: TComponentID;
      const AName, AImgName: string);
  end;

  {*
    Donn�es d'un joueur pour TCounterEffect
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
    property GlobalCounter: Integer read FGlobalCounter write FGlobalCounter
      default 0;
  end;

  {*
    Bouton poussoir
    @author sjrd
    @version 5.0
  *}
  TPushButton = class(TCounterEffect)
  private
    FDownPainter: TPainter; /// Peintre pour le bouton enfonc�
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Entered(Context: TMoveContext); override;
    procedure Execute(Context: TMoveContext); override;
    procedure Exited(Context: TMoveContext); override;

    procedure ButtonDown(Context: TMoveContext); virtual;
    procedure ButtonUp(Context: TMoveContext); virtual;
  published
    property Enabled;

    property DownPainter: TPainter read FDownPainter;
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
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Execute(Context: TMoveContext); override;

    procedure SwitchOn(Context: TMoveContext); virtual;
    procedure SwitchOff(Context: TMoveContext); virtual;

    property OffPainter: TPainter read FOffPainter;
  published
    property IsOn: Boolean read FIsOn write FIsOn default False;

    property OnPainter: TPainter read FOnPainter;
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
    constructor CreateDeco(AMaster: TMaster; const AID: TComponentID;
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

    FDefaultObjectDef: TObjectDef; /// ObjectDef par d�faut
    FDefaultFindMessage: string;   /// FindMessage par d�faut

    function IsObjectDefStored: Boolean;
    function IsFindMessageStored: Boolean;
  protected
    procedure StoreDefaults; override;

    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor CreateTool(AMaster: TMaster; const AID: TComponentID;
      AObjectDef: TObjectDef; const AFindMessage: string;
      const AName: string = ''; const AImgName: string = '');

    procedure Find(Context: TMoveContext); override;
  published
    property ObjectDef: TObjectDef read FObjectDef write FObjectDef
      stored IsObjectDefStored;
    property FindMessage: string read FFindMessage write FFindMessage
      stored IsFindMessageStored;
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
    FMap: TMap;               /// Carte
    FPosition: T3DPoint;      /// Position
    FOriginalSquare: TSquare; /// Case originale
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      AMap: TMap; const APosition: T3DPoint; AField: TField = nil;
      AEffect: TEffect = nil; ATool: TTool = nil;
      AObstacle: TObstacle = nil); reintroduce;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Map: TMap read FMap;
    property Position: T3DPoint read FPosition;
    property OriginalSquare: TSquare read FOriginalSquare;
  end;

implementation

uses
  MapTools;

{----------------}
{ Classe TGround }
{----------------}

{*
  Cr�e une instance de TGround
  @param AMaster    Ma�tre FunLabyrinthe
  @param AID        ID du terrain
  @param AName      Nom du terrain
  @param AImgName   Nom de l'image des graphismes
*}
constructor TGround.CreateGround(AMaster: TMaster; const AID: TComponentID;
  const AName, AImgName: string);
begin
  Create(AMaster, AID);

  Name := AName;
  if AImgName <> '' then
    Painter.AddImage(AImgName);
end;

{*
  Gestionnaire de message msgPlank
  TGround permet d'y poser la planche pour autant que de l'autre c�t�, il y ait
  �galement un TGround, et qu'il n'y ait d'obstacle d'aucun c�t�.
  @param Msg   Message
*}
procedure TGround.PlankMessage(var Msg: TPlankMessage);
begin
  with Msg, Player do
  begin
    if Kind = pmkLeaveFrom then
    begin
      Result := (Map[Dest].Field is TGround) and
        (Map[Src].Obstacle = nil) and (Map[Dest].Obstacle = nil);
    end;
  end;
end;

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
constructor TDecorativeEffect.CreateDeco(AMaster: TMaster;
  const AID: TComponentID; const AName, AImgName: string);
begin
  Create(AMaster, AID);

  Name := AName;
  Painter.AddImage(AImgName);
end;

{*
  [@inheritDoc]
*}
function TDecorativeEffect.GetCategory: string;
begin
  Result := SCategoryNeutrals;
end;

{-----------------------}
{ Classe TCounterEffect }
{-----------------------}

{*
  Valeur du compteur pour un joueur donn�
  @param Player   Joueur concern�
  @return Valeur du compteur pour le joueur sp�cifi�
*}
function TCounterEffect.GetCounter(Player: TPlayer): Integer;
begin
  Result := TCounterEffectPlayerData(GetPlayerData(Player)).Counter;
end;

{*
  Modifie la valeur du compteur pour un joueur donn�
  @param Player   Joueur concern�
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
  Incr�mente le compteur pour un joueur donn�
  @param Player   Joueur concern�
*}
procedure TCounterEffect.IncCounter(Player: TPlayer);
begin
  with TCounterEffectPlayerData(GetPlayerData(Player)) do
    Counter := Counter + 1;
end;

{*
  Teste si c'est la premi�re fois pour un joueur qu'active cet effet
  @param Player   Joueur concern�
  @return True si c'est la premi�re fois, False sinon
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
  [@inheritDoc]
*}
constructor TPushButton.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FDownPainter := TPainter.Create(Master.ImagesMaster);
  FDownPainter.BeginUpdate;

  Painter.AddImage(fButton);
  DownPainter.AddImage(fSunkenButton);
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
  if not Enabled then
    DownPainter.Draw(Context)
  else if Context.IsNowhere or (Context.Map.PlayersOn(Context.Pos) = 0) then
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
  
  FDownPainter.EndUpdate;
end;

{*
  [@inheritDoc]
*}
procedure TPushButton.Entered(Context: TMoveContext);
begin
  inherited;

  if Enabled then
  begin
    inherited Execute(Context);

    if Context.Map.PlayersOn(Context.Pos) = 1 then
      ButtonDown(Context);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TPushButton.Execute(Context: TMoveContext);
begin
end;

{*
  [@inheritDoc]
*}
procedure TPushButton.Exited(Context: TMoveContext);
begin
  if Enabled and (Context.Map.PlayersOn(Context.Pos) = 0) then
    ButtonUp(Context);

  inherited;
end;

{*
  D�clench� lorsque le bouton est enfonc�
  @param Context   Contexte du mouvement
*}
procedure TPushButton.ButtonDown(Context: TMoveContext);
begin
end;

{*
  D�clench� lorsque le bouton est rel�ch�
  @param Context   Contexte du mouvement
*}
procedure TPushButton.ButtonUp(Context: TMoveContext);
begin
end;

{----------------}
{ Classe TSwitch }
{----------------}

{*
  [@inheritDoc]
*}
constructor TSwitch.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FOffPainter := Painter;

  FOnPainter := TPainter.Create(Master.ImagesMaster);
  FOnPainter.BeginUpdate;

  OffPainter.AddImage(fSwitchOff);
  OnPainter.AddImage(fSwitchOn);
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
  
  FOnPainter.EndUpdate;
end;

{*
  [@inheritDoc]
*}
procedure TSwitch.Execute(Context: TMoveContext);
begin
  inherited;

  IsOn := not IsOn;

  if IsOn then
    SwitchOn(Context)
  else
    SwitchOff(Context);
end;

{*
  D�clench� lorsque l'interrupteur passe en position ON
  @param Context   Contexte du mouvement
*}
procedure TSwitch.SwitchOn(Context: TMoveContext);
begin
end;

{*
  D�clench� lorsque l'interrupteur passe en position OFF
  @param Context   Contexte du mouvement
*}
procedure TSwitch.SwitchOff(Context: TMoveContext);
begin
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
constructor TDecorativeObstacle.CreateDeco(AMaster: TMaster;
  const AID: TComponentID; const AName, AImgName: string);
begin
  inherited;

  Name := AName;
  Painter.AddImage(AImgName);
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
constructor TObjectTool.CreateTool(AMaster: TMaster; const AID: TComponentID;
  AObjectDef: TObjectDef; const AFindMessage: string;
  const AName: string = ''; const AImgName: string = '');
begin
  Create(AMaster, AID);

  if AName = '' then
    Name := AObjectDef.Name
  else
    Name := AName;

  FObjectDef := AObjectDef;
  FFindMessage := AFindMessage;

  if AImgName <> '' then
    Painter.AddImage(AImgName);
end;

{*
  Teste si la propri�t� ObjectDef doit �tre sauvegard�e
  @return True si la propri�t� ObjectDef doit �tre sauvegard�e, False sinon
*}
function TObjectTool.IsObjectDefStored: Boolean;
begin
  Result := FObjectDef <> FDefaultObjectDef;
end;

{*
  Teste si la propri�t� FindMessage doit �tre sauvegard�e
  @return True si la propri�t� FindMessage doit �tre sauvegard�e, False sinon
*}
function TObjectTool.IsFindMessageStored: Boolean;
begin
  Result := FFindMessage <> FDefaultFindMessage;
end;

{*
  [@inheritDoc]
*}
procedure TObjectTool.StoreDefaults;
begin
  inherited;

  FDefaultObjectDef := FObjectDef;
  FDefaultFindMessage := FFindMessage;
end;

{*
  [@inheritDoc]
*}
procedure TObjectTool.DoDraw(Context: TDrawSquareContext);
begin
  if Painter.IsEmpty and (FObjectDef <> nil) then
    FObjectDef.Draw(Context)
  else
    inherited;
end;

{*
  [@inheritDoc]
*}
procedure TObjectTool.Find(Context: TMoveContext);
begin
  with Context do
  begin
    Square := RemoveTool(Square);

    if ObjectDef <> nil then
      ObjectDef.Count[Player] := ObjectDef.Count[Player] + 1
    else
      Player.ShowMessage(SObjectDefIsNil);

    if FindMessage <> '' then
      Player.ShowMessage(FindMessage);
  end;
end;

{--------------------------}
{ Classe TOverriddenSquare }
{--------------------------}

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
  if AField = nil then
    AField := AOriginalSquare.Field;

  CreateConfig(AMaster, AID, AField, AEffect, ATool, AObstacle);

  Name := AOriginalSquare.Name;

  FMap := AMap;
  FPosition := APosition;
  FOriginalSquare := AOriginalSquare;
end;

{*
  [@inheritDoc]
*}
procedure TOverriddenSquare.DoDraw(Context: TDrawSquareContext);
begin
  OriginalSquare.Draw(Context);
  Painter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TOverriddenSquare.AfterConstruction;
begin
  inherited;

  Map[Position] := Self;
end;

{*
  [@inheritDoc]
*}
procedure TOverriddenSquare.BeforeDestruction;
begin
  inherited;

  if Map[Position] = Self then
    Map[Position] := OriginalSquare;
end;

end.

