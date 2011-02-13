{*
  D�crit les classes de composants de compatibilit� 4.x
  L'unit� C4xSquares regroupe les diff�rentes classes de composants de
  compatibilit� 4.x de FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit C4xComponents;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, StrUtils, Consts, ScUtils,
  ScLists, GR32, FunLabyUtils, MapTools,
  FLBFields, FLBSimpleEffects, FLBBoat, C4xCommon;

resourcestring
  sStairs = 'Escalier';             /// Nom de l'escalier
  sNumberedBoat = 'Barque n�%d';    /// Nom d'une barque num�rot�e

  sButton = 'Bouton n�%d';          /// Nom du bouton
  sButtonTemplate = 'Bouton';       /// Nom du bouton mod�le

const
  /// ID du plug-in de hacks de compatibilit�
  idCompatibilityHacksPlugin = 'CompatibilityHacksPlugin';
  /// ID du plug-in des zones
  idZonesPlugin = 'ZonesPlugin';

  idUpStairs = 'UpStairs';                   /// ID de l'escalier montant
  idDownStairs = 'DownStairs';               /// ID de l'escalier descendant
  idOldStairs = 'OldStairs%d';               /// ID des escaliers de la v1.0

  idActionsObject = 'ActionsObject%d';       /// ID de l'objet li� � des actions
  idActionsEffect = 'ActionsEffect%d';       /// ID de l'effet � actions
  idActionsObstacle = 'ActionsObstacle%d';   /// ID de l'obstacle � actions

  idButtonTemplate = 'ButtonTemplate';       /// ID du bouton mod�le

  /// ID de la case � actions
  idActionsSquare = idGrass+'-ActionsEffect%0:d--ActionsObstacle%0:d';
  /// ID de la case � actions mod�le
  idActionsSquareTemplate = idGrass+'-'+idButtonTemplate+'--';

  idActions = 'Actions%d';                /// ID d'un ensemble d'actions
  idZoneActions = 'ZoneActions:%d:%d:%d'; /// ID d'un ensemble d'actions de zone

  idC4xInfos = 'C4xInfos'; /// ID des infos sur C4x

const {don't localize}
  fSunkenButton = 'Buttons/SunkenButton'; /// Fichier du bouton enfonc�
  fButton = 'Buttons/Button';             /// Fichier du bouton
  fSwitchOff = 'Buttons/SwitchOff';       /// Fichier de l'interrupteur �teint
  fSwitchOn = 'Buttons/SwitchOn';         /// Fichier de l'interrupteur allum�
  fInfoStone = 'Icons/Information';       /// Fichier de la borne info
  fTransporter = 'Miscellaneous/Transporter'; /// Fichier du t�l�porteur
  fOutside = 'Fields/Outside';            /// Fichier du dehors
  fTreasure = 'Chests/Treasure';          /// Fichier du tr�sor

resourcestring
  sAskForTips = 'Ce labyrinthe propose certains indices : '+
    'voulez-vous les activer ?';

  sGotOutsideMaze = 'BRAVO ! Tu as r�ussi � sortir du labyrinthe !';
  sFoundTreasure = 'BRAVO ! Tu as trouv� le tr�sor !';

type
  {*
    Repr�sente le type d'un ensemble d'actions
    @author sjrd
    @version 5.0
  *}
  TActionsKind = (akGameStarted, akPushButton, akSwitch, akInfoStone, akHidden,
    akTransporterNext, akTransporterPrevious, akTransporterRandom, akOutside,
    akTreasure, akCustom, akObject, akObstacle, akDirection, akZone);

  TActions = class;

  {*
    Plug-in effectuant quelques "hacks" pour rester compatible avec la 4.x
    @author sjrd
    @version 5.0
  *}
  TCompatibilityHacksPlugin = class(TPlugin)
  private
    FFreezedView: TBitmap32; /// Vue gel�e
    FUpdating: Boolean;      /// Indique si est en train d'�tre mise � jour

    procedure GameStartedMsg(var Msg: TPlayerMessage); message msgGameStarted;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure Moved(Context: TMoveContext); override;

    procedure DrawView(Context: TDrawViewContext); override;
    procedure PressKey(Context: TKeyEventContext); override;

    procedure UpdateView(Player: TPlayer);
  end;

  {*
    Plug-in de joueur de gestion des zones
    Ce plug-in assure la compatibilit� des actions de zones de
    FunLabyrinthe 4.x.
    @author sjrd
    @version 5.0
  *}
  TZonesPlugin = class(TPlugin)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Moved(Context: TMoveContext); override;
  end;

  {*
    Escaliers de la v1.0
    Les escaliers permettent de monter ou descendre d'un �tage
    @author sjrd
    @version 5.0
  *}
  TOldStairs = class(TEffect)
  protected
    function GetIsDesignable: Boolean; override;

    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Execute(Context: TMoveContext); override;
  end;

  {*
    D�finition d'objet li� � des actions
    Les nombre des objets li�s � des actions est le compteur de ces actions.
    @author sjrd
    @version 5.0
  *}
  TActionsObject = class(TObjectDef)
  private
    FActions: TActions; /// Actions propri�taires
  protected
    function GetCount(Player: TPlayer): Integer; override;
    procedure SetCount(Player: TPlayer; Value: Integer); override;
  public
    constructor Create(AActions: TActions); reintroduce;

    property Actions: TActions read FActions;
  end;

  {*
    Effet � actions
    Un effet � actions ex�cute une s�rie d'actions lorsqu'on arrive dessus.
    @author sjrd
    @version 5.0
  *}
  TActionsEffect = class(TEffect)
  private
    FActions: TActions;          /// Actions propri�taires
    FAlternatePainter: TPainter; /// Peintre alternatif
  protected
    function GetIsDesignable: Boolean; override;

    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AActions: TActions); reintroduce;
    procedure AfterConstruction; override;

    procedure Execute(Context: TMoveContext); override;

    property Actions: TActions read FActions;
  published
    property AlternatePainter: TPainter read FAlternatePainter;
  end;

  {*
    Obstacle � actions
    Un obstacle � actions ex�cute une s�rie d'actions lorsqu'on pousse dessus.
    @author sjrd
    @version 5.0
  *}
  TActionsObstacle = class(TObstacle)
  private
    FActions: TActions; /// Actions propri�taires
  protected
    function GetIsDesignable: Boolean; override;
  public
    constructor Create(AActions: TActions); reintroduce;

    procedure Pushing(Context: TMoveContext); override;

    property Actions: TActions read FActions;
  end;

  {*
    Repr�sente un ensemble d'actions pour une case active particuli�re
    @author sjrd
    @version 5.0
  *}
  TActions = class(TFunLabyComponent)
  private
    FNumber: Integer;            /// Num�ro d'actions
    FKind: TActionsKind;         /// Type d'actions
    FFileName: string;           /// Nom du fichier de graphismes
    FActions: TStrings;          /// Actions � ex�cuter
    FCounter: Integer;           /// Compteur d'ex�cution
    FInactive: TComponentID;     /// Effet inactif correspondant
    FObjectDef: TActionsObject;  /// Objet correspondant
    FEffect: TActionsEffect;     /// Effet correspondant
    FObstacle: TActionsObstacle; /// Obstacle correspondant
  public
    constructor Create(AMaster: TMaster; ANumber: Integer;
      AKind: TActionsKind; const AFileName: string; AActions: TStrings;
      const AID: TComponentID = ''); reintroduce;
    destructor Destroy; override;

    procedure Execute(Phase: Integer; Player: TPlayer; KeyPressed: Boolean;
      const Pos: T3DPoint; var DoNextPhase: Boolean;
      out HasMoved, HasShownMsg, Successful: Boolean);

    property Number: Integer read FNumber;
    property Kind: TActionsKind read FKind;
    property FileName: string read FFileName;
    property Actions: TStrings read FActions;
    property Inactive: TComponentID read FInactive;
    property ObjectDef: TActionsObject read FObjectDef;
    property Effect: TActionsEffect read FEffect;
    property Obstacle: TActionsObstacle read FObstacle;
  published
    property Counter: Integer read FCounter write FCounter default 0;
  end;

  {*
    Composant unique par parties conservant les infos de ce package
    @author sjrd
    @version 5.0
  *}
  TC4xInfos = class(TFunLabyComponent)
  private
    FKnowShowTips: Boolean;                  /// Affichage des indices fix�
    FShowTips: Boolean;                      /// Affichage les indices
    FActionsCount: Integer;                  /// Nombres d'actions
    FActions: array of TActions;             /// Liste des actions
    FVariables: array[1..MaxVar] of Integer; /// Variables

    procedure SetShowTips(Value: Boolean);

    function GetActions(Index: Integer): TActions;

    function GetVariables(Index: Integer): Integer;
    procedure SetVariables(Index, Value: Integer);
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
    procedure StoreDefaults; override;
  public
    constructor Create(AMaster: TMaster;
      AActions: TObjectList); reintroduce;

    property KnowShowTips: Boolean read FKnowShowTips;

    property ActionsCount: Integer read FActionsCount;
    property Actions[Index: Integer]: TActions read GetActions;

    property Variables[Index: Integer]: Integer
      read GetVariables write SetVariables;
  published
    property ShowTips: Boolean read FShowTips write SetShowTips default False;
  end;

const {don't localize}
  /// Ensemble des types d'actions qui ne sont pas associ�es � une case
  ActionsKindsWithoutSquare: set of TActionsKind =
    [akGameStarted, akZone];

  /// Ensemble des types d'actions personnalis�s
  CustomActionsKind: set of TActionsKind =
    [akCustom, akObject, akObstacle, akDirection];

  /// Ensemble des types d'actions � placer dans la palette
  RegisteredActionsKind: set of TActionsKind =
    [akPushButton..akTransporterRandom, akCustom..akDirection];

  /// Sous-r�pertoire des images gard�es pour compatibilit�
  fCompatibility = 'Compatibility4x/';

procedure UpdateView(Player: TPlayer);

implementation

uses
  C4xInterpreter;

{*
  Met � jour la vue du joueur
  @param Player   Joueur dont mettre � jour la vue
*}
procedure UpdateView(Player: TPlayer);
var
  Plugin: TPlugin;
begin
  Plugin := Player.Master.Plugin[idCompatibilityHacksPlugin];
  (Plugin as TCompatibilityHacksPlugin).UpdateView(Player);
end;

{---------------------------------}
{ TCompatibilityHacksPlugin class }
{---------------------------------}

{*
  [@inheritDoc]
*}
constructor TCompatibilityHacksPlugin.Create(AMaster: TMaster;
  const AID: TComponentID);
begin
  inherited;

  FFreezedView := TBitmap32.Create;
  FZIndex := 512;
end;

{*
  [@inheritDoc]
*}
destructor TCompatibilityHacksPlugin.Destroy;
begin
  FFreezedView.Free;

  inherited;
end;

{*
  Gestionnaire du message msgGameStarted
  @param Msg   Message
*}
procedure TCompatibilityHacksPlugin.GameStartedMsg(var Msg: TPlayerMessage);
var
  Infos: TC4xInfos;
  Player: TPlayer;
  DoNextPhase, HasMoved, HasShownMsg, Successful: Boolean;
  WereTips: Boolean;
  I: Integer;
begin
  Infos := Master.Component[idC4xInfos] as TC4xInfos;
  Player := Msg.Player;

  try
    DoNextPhase := False;
    WereTips := False;

    if not Infos.KnowShowTips then
    begin
      for I := 0 to Infos.ActionsCount-1 do
      begin
        if StringsOps.FindText(Infos.Actions[I].Actions, 'Indice') >= 0 then
        begin
          WereTips := True;
          Break;
        end;
      end;

      Infos.ShowTips := WereTips and
        (Player.ShowSelectionMsg(sAskForTips,
          [AnsiReplaceStr(SMsgDlgYes, '&', ''),
          AnsiReplaceStr(SMsgDlgNo, '&', '')]) = 0);
    end;

    for I := 0 to Infos.ActionsCount-1 do
    begin
      with Infos.Actions[I] do
      begin
        if Kind = akGameStarted then
        begin
          Execute(phExecute, Player, False, Player.Position,
            DoNextPhase, HasMoved, HasShownMsg, Successful);
        end;
      end;
    end;
  finally
    UpdateView(Player);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibilityHacksPlugin.Moved(Context: TMoveContext);
begin
  UpdateView(Context.Player);
end;

{*
  [@inheritDoc]
*}
procedure TCompatibilityHacksPlugin.DrawView(Context: TDrawViewContext);
begin
  FFreezedView.Lock;
  try
    if not FUpdating then
    begin
      with Context do
      begin
        if (FFreezedView.Width <> Player.Mode.Width) or
          (FFreezedView.Height <> Player.Mode.Height) then
        begin
          FFreezedView.SetSize(Player.Mode.Width, Player.Mode.Height);
          FFreezedView.Clear(clBlack32);
        end;

        Bitmap.Draw(0, 0, FFreezedView);
      end;
    end;
  finally
    FFreezedView.Unlock;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibilityHacksPlugin.PressKey(Context: TKeyEventContext);
begin
  with Context do
  begin
    if Key in [VK_UP, VK_RIGHT, VK_DOWN, VK_LEFT] then
    begin
      Player.Mode.PressKey(Context);
      Handled := True;

      UpdateView(Player);
    end;
  end;
end;

{*
  Met � jour la vue du joueur
  @param Player   Joueur dont mettre la vue � jour
*}
procedure TCompatibilityHacksPlugin.UpdateView(Player: TPlayer);
begin
  FFreezedView.Lock;
  try
    FUpdating := True;

    FFreezedView.SetSize(Player.Mode.Width, Player.Mode.Height);
    Player.DrawView(FFreezedView);
  finally
    FUpdating := False;
    FFreezedView.Unlock;
  end;
end;

{---------------------}
{ Classe TZonesPlugin }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TZonesPlugin.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  FZIndex := -512;
end;

{*
  [@inheritDoc]
*}
procedure TZonesPlugin.Moved(Context: TMoveContext);
var
  Zone: T3DPoint;
  ActionsID: TComponentID;
  Actions: TActions;
  DoNextPhase, HasMoved, HasShownMsg, Successful: Boolean;
begin
  with Context do
  begin
    Zone.X := Dest.X div 7;
    Zone.Y := Dest.Y div 7;
    Zone.Z := Dest.Z;

    if (Src.X div 7 = Zone.X) and (Src.Y div 7 = Zone.Y) and
      (Src.Z = Zone.Z) then
      Exit;

    ActionsID := Format(idZoneActions, [Zone.X, Zone.Y, Zone.Z]);
    Actions := nil;
    try
      Actions := Master.Component[ActionsID] as TActions;
    except
      on Error: EComponentNotFound do;
      on Error: EInvalidCast do;
    end;

    if Actions <> nil then
    begin
      DoNextPhase := False;
      Actions.Execute(phExecute, Player, True, Dest,
        DoNextPhase, HasMoved, HasShownMsg, Successful);
      if DoNextPhase then
        Player.MoveTo(Player.Position, True);
    end;
  end;
end;

{----------------}
{ Classe TStairs }
{----------------}

{*
  [@inheritDoc]
*}
constructor TOldStairs.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SStairs;
end;

{*
  [@inheritDoc]
*}
function TOldStairs.GetIsDesignable: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TOldStairs.DoDraw(Context: TDrawSquareContext);
var
  StairsID: TComponentID;
  Pos, Other: T3DPoint;
begin
  inherited;

  if Context.IsNowhere then
    StairsID := idUpStairs
  else
  begin
    Pos := Context.Pos;
    Other := Pos;
    FindNextSquare(Context.Map, Other, Self);

    if (Other.Z < Pos.Z) or ((Other.Z = Pos.Z) and
      (Other.Y < Pos.Y) or ((Other.Y = Pos.Y) and (Other.X < Pos.X))) then
      StairsID := idDownStairs
    else
      StairsID := idUpStairs;
  end;

  Master.Effect[StairsID].Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TOldStairs.Execute(Context: TMoveContext);
var
  Other: T3DPoint;
begin
  with Context do
  begin
    Other := Pos;
    FindNextSquare(Map, Other, Self);

    if not Same3DPoint(Pos, Other) then
    begin
      Temporize;
      Player.MoveTo(Other);
    end;
  end;
end;

{-----------------------}
{ Classe TActionsObject }
{-----------------------}

{*
  Cr�e une instance de TActionsObject
  @param AActions   Actions propri�taires
*}
constructor TActionsObject.Create(AActions: TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObject, [AActions.Number]));

  FActions := AActions;

  Name := Actions.FileName;
  Painter.AddImage(fCompatibility + RemoveDiacritics(Actions.FileName));
end;

{*
  [@inheritDoc]
*}
function TActionsObject.GetCount(Player: TPlayer): Integer;
begin
  Result := Actions.Counter;
end;

{*
  [@inheritDoc]
*}
procedure TActionsObject.SetCount(Player: TPlayer; Value: Integer);
begin
  Actions.Counter := Value;
end;

{-----------------------}
{ Classe TActionsEffect }
{-----------------------}

{*
  Cr�e une instance de TActionsEffect
  @param AActions   Actions propri�taires
*}
constructor TActionsEffect.Create(AActions: TActions);
begin
  inherited Create(AActions.Master, Format(idActionsEffect, [AActions.Number]));

  FActions := AActions;

  Name := Format(sButton, [Actions.Number]);
  if Actions.Kind in RegisteredActionsKind then
    EditVisualTag := IntToStr(Actions.Number);

  FAlternatePainter := TPainter.Create(Master.ImagesMaster);
  FAlternatePainter.BeginUpdate;

  case Actions.Kind of
    akPushButton:
    begin
      Painter.AddImage(fButton);
      AlternatePainter.AddImage(fSunkenButton);
    end;
    akSwitch:
    begin
      Painter.AddImage(fSwitchOff);
      AlternatePainter.AddImage(fSwitchOn);
    end;
    akInfoStone:
      Painter.AddImage(fInfoStone);
    akTransporterNext..akTransporterRandom:
      Painter.AddImage(fTransporter);
    akOutside:
      Painter.AddImage(fOutside);
    akTreasure:
      Painter.AddImage(fTreasure);
    akCustom..akDirection:
      Painter.AddImage(fCompatibility + RemoveDiacritics(Actions.FileName));
  end;
end;

{*
  [@inheritDoc]
*}
function TActionsEffect.GetIsDesignable: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TActionsEffect.AfterConstruction;
begin
  inherited;
  FAlternatePainter.EndUpdate;
end;

{*
  [@inheritDoc]
*}
procedure TActionsEffect.DoDraw(Context: TDrawSquareContext);
begin
  with Context do
  begin
    case Actions.Kind of
      akPushButton:
      begin
        if IsNowhere or (QPos.Map.PlayersOn(QPos.Position) = 0) then
          Painter.Draw(Context)
        else
          AlternatePainter.Draw(Context);
      end;

      akSwitch:
      begin
        if Odd(Actions.Counter) then
          AlternatePainter.Draw(Context)
        else
          Painter.Draw(Context);
      end;
    else
      inherited;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TActionsEffect.Execute(Context: TMoveContext);
var
  VarGoOnMoving, HasMoved, HasShownMsg, Successful: Boolean;
  Other: T3DPoint;
begin
  with Context do
  begin
    VarGoOnMoving := Actions.Kind = akDirection;

    FActions.Execute(phExecute, Player, True, Pos, VarGoOnMoving,
      HasMoved, HasShownMsg, Successful);

    GoOnMoving := VarGoOnMoving;
    Temporization := Player.DefaultTemporization;

    if Actions.Kind in [akTransporterNext..akTransporterRandom] then
    begin
      if (not HasMoved) and (Square.Effect = Self) then
      begin
        // Action par d�faut des actions de type t�l�porteur

        Other := Pos;

        // Recherche de la case de destination
        case Actions.Kind of
          akTransporterNext     : FindNextSquare    (Map, Other, Self);
          akTransporterPrevious : FindPreviousSquare(Map, Other, Self);
          akTransporterRandom   : FindSquareAtRandom(Map, Other, Self);
        end;

        // Si l'on a trouv� une autre case, on d�place le joueur
        if Same3DPoint(Other, Pos) then
          Exit;
        Temporize;
        Player.MoveTo(Other);
      end;
    end else if Actions.Kind in [akOutside..akTreasure] then
    begin
      if Player.Map[Player.Position].Effect = Self then
      begin
        // Action par d�faut des actions de type case de fin
        Player.Win;
        if not HasShownMsg then
        begin
          if Actions.Kind = akOutside then
            Player.ShowMessage(sGotOutsideMaze)
          else
            Player.ShowMessage(sFoundTreasure);
        end;
      end;
    end;
  end;
end;

{-------------------------}
{ Classe TActionsObstacle }
{-------------------------}

{*
  Cr�e une instance de TActionsObstacle
  @param AActions   Actions propri�taires
*}
constructor TActionsObstacle.Create(AActions: TActions);
begin
  inherited Create(AActions.Master,
    Format(idActionsObstacle, [AActions.Number]));

  FActions := AActions;

  Name := Format(sButton, [AActions.Number]);
end;

{*
  [@inheritDoc]
*}
function TActionsObstacle.GetIsDesignable: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TActionsObstacle.Pushing(Context: TMoveContext);
var
  DoNextPhase, HasMoved, HasShownMsg, Successful: Boolean;
begin
  if FActions.Kind <> akObstacle then
    Exit;

  with Context do
  begin
    DoNextPhase := False;
    FActions.Execute(phPushing, Player, KeyPressed, Pos, DoNextPhase,
      HasMoved, HasShownMsg, Successful);
    Cancelled := Same3DPoint(Src, Player.Position) and (not Successful);
    Temporization := Player.DefaultTemporization;
  end;
end;

{-----------------}
{ Classe TActions }
{-----------------}

{*
  Cr�e une instance de TActions
  @param AMaster     Ma�tre FunLabyrinthe
  @param ANumber     Num�ro d'actions
  @param AKind       Type d'actions
  @param AFileName   Nom du fichier de graphismes
  @param AID         ID des actions, ou vide pour utiliser un ID par d�faut
*}
constructor TActions.Create(AMaster: TMaster; ANumber: Integer;
  AKind: TActionsKind; const AFileName: string; AActions: TStrings;
  const AID: TComponentID = '');
var
  IntFileName, XIndex, YIndex: Integer;
begin
  inherited Create(AMaster, IIF(AID = '', Format(idActions, [ANumber]), AID));

  FNumber := ANumber;
  FKind := AKind;
  FFileName := AFileName;
  FActions := TStringList.Create;
  FActions.Assign(AActions);
  FCounter := 0;

  if TryStrToInt(FFileName, IntFileName) then
  begin
    XIndex := IntFileName mod 10 - 1;
    YIndex := IntFileName div 10 - 1;
    FFileName := Format(DefaultSquaresImgName + '@%d,%d:%d,%d',
      [XIndex*SquareSize, YIndex*SquareSize, (XIndex+1)*SquareSize,
      (YIndex+1)*SquareSize]);
  end;

  case Kind of
    akPushButton: FInactive := idSunkenButton;
    akTransporterNext..akTransporterRandom: FInactive := idInactiveTransporter;
  else
    FInactive := '';
  end;

  if Kind in ActionsKindsWithoutSquare then
  begin
    FObjectDef := nil;
    FEffect := nil;
    FObstacle := nil;
  end else
  begin
    if Kind <> akObject then
      FObjectDef := nil
    else
      FObjectDef := TActionsObject.Create(Self);
    FEffect := TActionsEffect.Create(Self);
    FObstacle := TActionsObstacle.Create(Self);
  end;
end;

{*
  [@inheritDoc]
*}
destructor TActions.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{*
  Ex�cute les actions
  @param Phase         Phase courante (phPushing ou phExecute)
  @param Player        Joueur concern�
  @param KeyPressed    True si une touche a �t� press�e pour le d�placement
  @param Pos           Position de la case
  @param DoNextPhase   Indique s'il faut ex�cuter la phase suivante
  @param HasMoved      Indique si un AllerA a �t� fait
  @param HasShownMsg   Indique si un message a �t� affich�
  @param Successful    Indique si la manoeuvre est r�ussie
*}
procedure TActions.Execute(Phase: Integer; Player: TPlayer;
  KeyPressed: Boolean; const Pos: T3DPoint; var DoNextPhase: Boolean;
  out HasMoved, HasShownMsg, Successful: Boolean);
begin
  Inc(FCounter);
  if Kind <> akGameStarted then
    UpdateView(Player);

  TActionsInterpreter.Execute(@FCounter, FActions, Master, Phase, Player,
    KeyPressed, Pos, DoNextPhase, HasMoved, HasShownMsg, Successful,
    Inactive);
end;

{------------------}
{ Classe TC4xInfos }
{------------------}

{*
  Cr�e une instance de TC4xInfos
  @param AMaster    Ma�tre FunLabyrinthe
  @param AActions   Liste des actions
*}
constructor TC4xInfos.Create(AMaster: TMaster;
  AActions: TObjectList);
begin
  inherited Create(AMaster, idC4xInfos);

  FKnowShowTips := False;
  FShowTips := False;
  FActionsCount := AActions.Count;

  SetLength(FActions, FActionsCount);
  Move(AActions.List^, FActions[0], FActionsCount*SizeOf(TObject));
  FillChar(FVariables[1], MaxVar*SizeOf(Integer), 0);
end;

{*
  D�finit l'affichage des indices
  @param Value   Nouvelle valeur de l'affichage des indices
*}
procedure TC4xInfos.SetShowTips(Value: Boolean);
begin
  FKnowShowTips := True;
  FShowTips := Value;
end;

{*
  Tableau zero-based des actions
  @param Index   Index des actions
  @return Les actions � l'index sp�cifi�
*}
function TC4xInfos.GetActions(Index: Integer): TActions;
begin
  Result := FActions[Index];
end;

{*
  Tableau index� par des entiers de 1 � MaxVar des variables
  @param Index   Index de la variable � r�cup�rer
  @return Valeur de la variable d'index Index
  @see MaxVar
*}
function TC4xInfos.GetVariables(Index: Integer): Integer;
begin
  Result := FVariables[Index];
end;

{*
  Tableau index� par des entiers de 1 � MaxVar des variables
  @param Index   Index de la variable � r�cup�rer
  @param Value   Nouvelle valeur de la variable d'index Index
  @see MaxVar
*}
procedure TC4xInfos.SetVariables(Index, Value: Integer);
begin
  FVariables[Index] := Value;
end;

{*
  [@inheritDoc]
*}
procedure TC4xInfos.DefineProperties(Filer: TFunLabyFiler);
var
  I: Integer;
begin
  inherited;

  for I := 1 to MaxVar do
    Filer.DefineFieldProperty(Format('Variable%d', [I]), TypeInfo(Integer),
      @FVariables[I], FVariables[I] <> 0);
end;

{*
  [@inheritDoc]
*}
procedure TC4xInfos.StoreDefaults;
var
  I: Integer;
begin
  inherited;

  { Big fat hack to give the player the compatibility painter, after it is
    created, but before its own StoreDefaults method is called. }
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I].Painter do
    begin
      BeginUpdate;
      try
        Clear;
        AddImage(fCompatibility4xPlayer);
      finally
        EndUpdate;
      end;
    end;
  end;
end;

end.

