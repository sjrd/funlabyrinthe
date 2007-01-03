{*
  Décrit les classes de composants de compatibilité 4.x
  L'unité C4xScrews regroupe les différentes classes de composants de
  compatibilité 4.x de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xComponents;

interface

uses
  SysUtils, Classes, Graphics, Contnrs, ScUtils, FunLabyUtils, FilesUtils,
  MapTools, C4xCommon;

resourcestring
  sStairs = 'Escalier';             /// Nom de l'escalier

  sSunkenButton = 'Bouton enfoncé'; /// Nom du bouton enfoncé
  sButton = 'Bouton n°%d';          /// Nom du bouton
  sButtonTemplate = 'Bouton';       /// Nom du bouton modèle

const {don't localize}
  idZonesPlugin = 'ZonesPlugin';           /// ID du plug-in des zones

  idUpStairs = 'UpStairs';                 /// ID de l'escalier montant
  idDownStairs = 'DownStairs';             /// ID de l'escalier descendant
  idOldStairs = 'OldStairs%d';             /// ID des escaliers de la v1.0

  idActionsObject = 'ActionsObject%d';     /// ID de l'objet lié à des actions
  idActionsEffect = 'ActionsEffect%d';     /// ID de l'effet à actions
  idActionsObstacle = 'ActionsObstacle%d'; /// ID de l'obstacle à actions

  idSunkenButton = 'SunkenButton';         /// ID du bouton enfoncé
  idButtonTemplate = 'ButtonTemplate';     /// ID du bouton modèle

  /// ID de la case à actions
  idActionsScrew = idGrass+'-ActionsEffect%0:d--ActionsObstacle%0:d';
  /// ID de la case à actions modèle
  idActionsScrewTemplate = idGrass+'-'+idButtonTemplate+'--';

  idActions = 'Actions%d';                /// ID d'un ensemble d'actions
  idZoneActions = 'ZoneActions:%d:%d:%d'; /// ID d'un ensemble d'actions de zone

  idC4xInfos = 'C4xInfos'; /// ID des infos sur C4x

const {don't localize}
  fSunkenButton = 'SunkenButton'; /// Fichier du bouton enfoncé
  fButton = 'Button';             /// Fichier du bouton
  fSwitchOff = 'SwitchOff';       /// Fichier de l'interrupteur éteint
  fSwitchOn = 'SwitchOn';         /// Fichier de l'interrupteur allumé
  fInfoStone = 'InfoStone';       /// Fichier de la borne info
  fTransporter = 'Transporter';   /// Fichier du téléporteur
  fOutside = 'Outside';           /// Fichier du dehors
  fTreasure = 'Treasure';         /// Fichier du trésor

resourcestring
  sGotOutsideMaze = 'BRAVO ! Tu as réussi à sortir du labyrinthe !';
  sFoundTreasure  = 'BRAVO ! Tu as trouvé le trésor !';

  sButtonTitle = 'Numéro du bouton';
  sButtonPrompt = 'Numéro du bouton (0 à %d) :';

type
  {*
    Représente le type d'un ensemble d'actions
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsKind = (akGameStarted, akPushButton, akSwitch, akInfoStone, akHidden,
    akTransporterNext, akTransporterPrevious, akTransporterRandom, akOutside,
    akTreasure, akCustom, akObject, akObstacle, akDirection, akZone);

  TActions = class;

  {*
    Plug-in de joueur de gestion des zones
    Ce plug-in assure la compatibilité des actions de zones de
    FunLabyrinthe 4.x.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TZonesPlugin = class(TPlugin)
  public
    procedure Moved(Player : TPlayer; const Src, Dest : T3DPoint); override;
  end;

  {*
    Escaliers de la v1.0
    Les escaliers permettent de monter ou descendre d'un étage
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TOldStairs = class(TEffect)
  protected
    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; const Pos : T3DPoint;
      var GoOnMoving : boolean); override;
  end;

  {*
    Définition d'objet lié à des actions
    Les nombre des objets liés à des actions est le compteur de ces actions.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsObject = class(TObjectDef)
  private
    FActions : TActions; /// Actions propriétaires
  protected
    function GetCount(Player : TPlayer) : integer; override;
    procedure SetCount(Player : TPlayer; Value : integer); override;
  public
    constructor Create(AActions : TActions);

    property Actions : TActions read FActions;
  end;

  {*
    Effet à actions
    Un effet à actions exécute une série d'actions lorsqu'on arrive dessus.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsEffect = class(TEffect)
  private
    FActions : TActions;          /// Actions propriétaires
    FAlternatePainter : TPainter; /// Peintre alternatif
  protected
    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    property AlternatePainter : TPainter read FAlternatePainter;
  public
    constructor Create(AActions : TActions);
    procedure AfterConstruction; override;

    procedure Execute(Player : TPlayer; const Pos : T3DPoint;
      var GoOnMoving : boolean); override;

    property Actions : TActions read FActions;
  end;

  {*
    Obstacle à actions
    Un obstacle à actions exécute une série d'actions lorsqu'on pousse dessus.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsObstacle = class(TObstacle)
  private
    FActions : TActions; /// Actions propriétaires
  public
    constructor Create(AActions : TActions);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;

    property Actions : TActions read FActions;
  end;

  {*
    Représente un ensemble d'actions pour une case active particulière
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActions = class(TFunLabyComponent)
  private
    FNumber : integer;            /// Numéro d'actions
    FKind : TActionsKind;         /// Type d'actions
    FFileName : string;           /// Nom du fichier de graphismes
    FActions : TStrings;          /// Actions à exécuter
    FCounter : integer;           /// Compteur d'exécution
    FInactive : TComponentID;     /// Effet inactif correspondant
    FObjectDef : TActionsObject;  /// Objet correspondant
    FEffect : TActionsEffect;     /// Effet correspondant
    FObstacle : TActionsObstacle; /// Obstacle correspondant
  public
    constructor Create(AMaster : TMaster; ANumber : integer;
      AKind : TActionsKind; const AFileName : string; AActions : TStrings;
      const AID : TComponentID = '');
    destructor Destroy; override;

    procedure Execute(Phase : integer; Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var DoNextPhase : boolean;
      out HasMoved, HasShownMsg, Successful : boolean);

    property Number : integer read FNumber;
    property Kind : TActionsKind read FKind;
    property FileName : string read FFileName;
    property Actions : TStrings read FActions;
    property Counter : integer read FCounter write FCounter;
    property Inactive : TComponentID read FInactive;
    property ObjectDef : TActionsObject read FObjectDef;
    property Effect : TActionsEffect read FEffect;
    property Obstacle : TActionsObstacle read FObstacle;
  end;

  {*
    Composant unique par parties conservant les infos de ce package
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TC4xInfos = class(TFunLabyComponent)
  private
    FMasterFile : TMasterFile;                /// Fichier maître
    FKnowShowTips : boolean;                  /// Affichage des indices fixé
    FShowTips : boolean;                      /// Affichage les indices
    FActionsCount : integer;                  /// Nombres d'actions
    FActions : array of TActions;             /// Liste des actions
    FVariables : array[1..MaxVar] of integer; /// Variables

    procedure SetShowTips(Value : boolean);

    function GetActions(Index : integer) : TActions;

    function GetVariables(Index : integer) : integer;
    procedure SetVariables(Index, Value : integer);
  public
    constructor Create(AMasterFile : TMasterFile; AActions : TObjectList);

    property MasterFile : TMasterFile read FMasterFile;

    property KnowShowTips : boolean read FKnowShowTips;
    property ShowTips : boolean read FShowTips write SetShowTips;

    property ActionsCount : integer read FActionsCount;
    property Actions[index : integer] : TActions read GetActions;

    property Variables[index : integer] : integer
      read GetVariables write SetVariables;
  end;

const {don't localize}
  /// Ensemble des types d'actions qui ne sont pas associées à une case
  ActionsKindsWithoutScrew : set of TActionsKind =
    [akGameStarted, akZone];

  /// Ensemble des types d'actions personnalisés
  CustomActionsKind : set of TActionsKind =
    [akCustom, akObject, akObstacle, akDirection];

  /// Sous-répertoire des images gardées pour compatibilité
  fCompatibility = 'Compatibility4x\';

implementation

uses
  C4xInterpreter;

{---------------------}
{ Classe TZonesPlugin }
{---------------------}

procedure TZonesPlugin.Moved(Player : TPlayer; const Src, Dest : T3DPoint);
var Zone : T3DPoint;
    ActionsID : TComponentID;
    Actions : TActions;
    DoNextPhase, HasMoved, HasShownMsg, Successful, Redo : boolean;
begin
  Zone.X := Dest.X div 7;
  Zone.Y := Dest.Y div 7;
  Zone.Z := Dest.Z;

  if (Src.X div 7 = Zone.X) and (Src.Y div 7 = Zone.Y) and (Src.Z = Zone.Z) then
    exit;

  ActionsID := Format(idZoneActions, [Zone.X, Zone.Y, Zone.Z]);
  Actions := nil;
  try
    Actions := Master.Component[ActionsID] as TActions;
  except
    on Error : EComponentNotFound do;
    on Error : EInvalidCast do;
  end;

  if Actions <> nil then
  begin
    DoNextPhase := False;
    Actions.Execute(phExecute, Player, True, Dest,
      DoNextPhase, HasMoved, HasShownMsg, Successful);
    if DoNextPhase then
    begin
      Player.MoveTo(Player.Position, True, Redo);
      if Redo then
        Player.NaturalMoving;
    end;
  end;
end;

{----------------}
{ Classe TStairs }
{----------------}

{*
  Crée une instance de TOldStairs
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TOldStairs.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  FStaticDraw := False;
end;

{*
  Dessine les escaliers sur le canevas indiqué
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonnée X du point à partir duquel dessiner le terrain
  @param Y        Coordonnée Y du point à partir duquel dessiner le terrain
*}
procedure TOldStairs.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
var StairsID : TComponentID;
    Pos, Other : T3DPoint;
begin
  inherited;

  if IsNoQPos(QPos) then StairsID := idUpStairs else
  begin
    Pos := QPos.Position;
    Other := Pos;
    FindNextScrew(QPos.Map, Other, Self);

    if (Other.Z < Pos.Z) or ( (Other.Z = Pos.Z) and
       (Other.Y < Pos.Y) or ( (Other.Y = Pos.Y) and (Other.X < Pos.X) ) ) then
      StairsID := idDownStairs
    else
      StairsID := idUpStairs;
  end;

  Master.Effect[StairsID].Draw(QPos, Canvas, X, Y)
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TOldStairs.Execute(Player : TPlayer; const Pos : T3DPoint;
  var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;
  FindNextScrew(Player.Map, Other, Self);

  if not Same3DPoint(Pos, Other) then
  begin
    Master.Temporize;
    Player.MoveTo(Other);
  end;
end;

{-----------------------}
{ Classe TActionsObject }
{-----------------------}

{*
  Crée une instance de TActionsObject
  @param AActions   Actions propriétaires
*}
constructor TActionsObject.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObject, [AActions.Number]),
    AActions.FileName);
  FActions := AActions;

  Painter.ImgNames.Add(fCompatibility + Name);
end;

{*
  Nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @return Nombre d'objets que ce joueur possède
*}
function TActionsObject.GetCount(Player : TPlayer) : integer;
begin
  Result := Actions.Counter;
end;

{*
  Modifie le nombre d'objets de ce type possédés par un joueur
  @param Player   Joueur concerné
  @param Value    Nouveau nombre d'objets
*}
procedure TActionsObject.SetCount(Player : TPlayer; Value : integer);
begin
  Actions.Counter := Value;
end;

{-----------------------}
{ Classe TActionsEffect }
{-----------------------}

{*
  Crée une instance de TActionsEffect
  @param AActions   Actions propriétaires
*}
constructor TActionsEffect.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsEffect, [AActions.Number]),
    Format(sButton, [AActions.Number]));
  FActions := AActions;

  FAlternatePainter := TPainter.Create(Master.ImagesMaster);
  FAlternatePainter.ImgNames.BeginUpdate;

  case Actions.Kind of
    akPushButton :
    begin
      Painter.ImgNames.Add(fButton);
      AlternatePainter.ImgNames.Add(fSunkenButton);
      FStaticDraw := False;
    end;
    akSwitch :
    begin
      Painter.ImgNames.Add(fSwitchOff);
      AlternatePainter.ImgNames.Add(fSwitchOn);
      FStaticDraw := False;
    end;
    akInfoStone :
      Painter.ImgNames.Add(fInfoStone);
    akTransporterNext..akTransporterRandom :
      Painter.ImgNames.Add(fTransporter);
    akOutside :
      Painter.ImgNames.Add(fOutside);
    akTreasure :
      Painter.ImgNames.Add(fTreasure);
    akCustom..akDirection :
      Painter.ImgNames.Add(fCompatibility + Actions.FileName);
  end;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TActionsEffect.AfterConstruction;
begin
  inherited;
  FAlternatePainter.ImgNames.EndUpdate;
end;

{*
  Dessine le composant sur un canevas
  DoDraw dessine le composant sur un canevas à la position indiquée.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TActionsEffect.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  if Actions.Kind = akPushButton then
  begin
    if QPos.Map.PlayersOn(QPos.Position) = 0 then
      Painter.Draw(Canvas, X, Y)
    else
      AlternatePainter.Draw(Canvas, X, Y);
  end else
  if Actions.Kind = akSwitch then
  begin
    if Odd(Actions.Counter) then
      AlternatePainter.Draw(Canvas, X, Y)
    else
      Painter.Draw(Canvas, X, Y);
  end else
  inherited;
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TActionsEffect.Execute(Player : TPlayer; const Pos : T3DPoint;
  var GoOnMoving : boolean);
var HasMoved, HasShownMsg, Successful : boolean;
    Other : T3DPoint;
begin
  GoOnMoving := Actions.Kind = akDirection;
  FActions.Execute(phExecute, Player, True, Pos, GoOnMoving,
    HasMoved, HasShownMsg, Successful);

  if Actions.Kind in [akTransporterNext..akTransporterRandom] then
  begin
    if (not HasMoved) and (Player.Map[Pos].Effect = Self) then
    begin
      // Action par défaut des actions de type téléporteur

      Other := Pos;

      // Recherche de la case de destination
      case Actions.Kind of
        akTransporterNext     : FindNextScrew    (Player.Map, Other, Self);
        akTransporterPrevious : FindPreviousScrew(Player.Map, Other, Self);
        akTransporterRandom   : FindScrewAtRandom(Player.Map, Other, Self);
      end;

      // Si l'on a trouvé une autre case, on déplace le joueur
      if Same3DPoint(Other, Pos) then exit;
      Master.Temporize;
      Player.MoveTo(Other);
    end;
  end else
  if Actions.Kind in [akOutside..akTreasure] then
  begin
    if Player.Map[Player.Position].Effect = Self then
    begin
      // Action par défaut des actions de type case de fin
      Player.Win;
      if not HasShownMsg then
      begin
        if Actions.Kind = akOutside then
          Player.ShowDialog(sWon, sGotOutsideMaze)
        else
          Player.ShowDialog(sWon, sFoundTreasure);
      end;
    end;
  end;
end;

{-------------------------}
{ Classe TActionsObstacle }
{-------------------------}

{*
  Crée une instance de TActionsObstacle
  @param AActions   Actions propriétaires
*}
constructor TActionsObstacle.Create(AActions : TActions);
begin
  inherited Create(AActions.Master, Format(idActionsObstacle,
    [AActions.Number]), Format(sButton, [AActions.Number]));
  FActions := AActions;
end;

{*
  Exécuté lorsque le joueur pousse sur l'obstacle
  Pushing est exécuté lorsque le joueur pousse sur l'obstacle. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Execute de la case ne soit exécutée, il faut positionner
  AbortExecute à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortExecute   À positionner à True pour empêcher le Execute
*}
procedure TActionsObstacle.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
var DoNextPhase, HasMoved, HasShownMsg, Successful : boolean;
begin
  if FActions.Kind <> akObstacle then exit;
  DoNextPhase := False;
  FActions.Execute(phPushing, Player, KeyPressed, Pos, DoNextPhase,
    HasMoved, HasShownMsg, Successful);
  AbortExecute := not DoNextPhase;
  Cancel := Same3DPoint(Src, Player.Position) and (not Successful);
end;

{-----------------}
{ Classe TActions }
{-----------------}

{*
  Crée une instance de TActions
  @param AMaster     Maître FunLabyrinthe
  @param ANumber     Numéro d'actions
  @param AKind       Type d'actions
  @param AFileName   Nom du fichier de graphismes
  @param AID         ID des actions, ou vide pour utiliser un ID par défaut
*}
constructor TActions.Create(AMaster : TMaster; ANumber : integer;
  AKind : TActionsKind; const AFileName : string; AActions : TStrings;
  const AID : TComponentID = '');
begin
  inherited Create(AMaster, IIF(AID = '', Format(idActions, [ANumber]), AID));

  FNumber := ANumber;
  FKind := AKind;
  FFileName := AFileName;
  FActions := TStringList.Create;
  FActions.Assign(AActions);
  FCounter := 0;

  case Kind of
    akPushButton : FInactive := idSunkenButton;
    akTransporterNext..akTransporterRandom : FInactive := idInactiveTransporter;
    else FInactive := '';
  end;

  if Kind in ActionsKindsWithoutScrew then
  begin
    FObjectDef := nil;
    FEffect := nil;
    FObstacle := nil;
  end else
  begin
    if Kind <> akObject then FObjectDef := nil else
      FObjectDef := TActionsObject.Create(Self);
    FEffect := TActionsEffect.Create(Self);
    FObstacle := TActionsObstacle.Create(Self);
  end;
end;

{*
  Détruit l'instance
*}
destructor TActions.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{*
  Exécute les actions
  @param Phase         Phase courante (phPushing ou phExecute)
  @param Player        Joueur concerné
  @param KeyPressed    True si une touche a été pressée pour le déplacement
  @param Pos           Position de la case
  @param DoNextPhase   Indique s'il faut exécuter la phase suivante
  @param HasMoved      Indique si un AllerA a été fait
  @param HasShownMsg   Indique si un message a été affiché
  @param Successful    Indique si la manoeuvre est réussie
*}
procedure TActions.Execute(Phase : integer; Player : TPlayer;
  KeyPressed : boolean; const Pos : T3DPoint; var DoNextPhase : boolean;
  out HasMoved, HasShownMsg, Successful : boolean);
begin
  inc(FCounter);
  TActionsInterpreter.Execute(@FCounter, FActions, Master, Phase, Player,
    KeyPressed, Pos, DoNextPhase, HasMoved, HasShownMsg, Successful, Inactive);
end;

{------------------}
{ Classe TC4xInfos }
{------------------}

{*
  Crée une instance de TC4xInfos
  @param AMasterFile   Fichier maître
  @param AActions      Liste des actions
*}
constructor TC4xInfos.Create(AMasterFile : TMasterFile;
  AActions : TObjectList);
var I : integer;
begin
  inherited Create(AMasterFile.Master, idC4xInfos);
  FMasterFile := AMasterFile;
  FKnowShowTips := False;
  FShowTips := False;
  FActionsCount := AActions.Count;
  SetLength(FActions, FActionsCount);
  for I := 0 to FActionsCount-1 do
    FActions[I] := TActions(AActions[I]);
  for I := 1 to MaxVar do
    FVariables[I] := 0;
end;

{*
  Définit l'affichage des indices
  @param Value   Nouvelle valeur de l'affichage des indices
*}
procedure TC4xInfos.SetShowTips(Value : boolean);
begin
  FKnowShowTips := True;
  FShowTips := Value;
end;

{*
  Tableau zero-based des actions
  @param Index   Index des actions
  @return Les actions à l'index spécifié
*}
function TC4xInfos.GetActions(Index : integer) : TActions;
begin
  Result := FActions[Index];
end;

{*
  Tableau indexé par des entiers de 1 à MaxVar des variables
  @param Index   Index de la variable à récupérer
  @return Valeur de la variable d'index Index
  @see MaxVar
*}
function TC4xInfos.GetVariables(Index : integer) : integer;
begin
  Result := FVariables[Index];
end;

{*
  Tableau indexé par des entiers de 1 à MaxVar des variables
  @param Index   Index de la variable à récupérer
  @param Value   Nouvelle valeur de la variable d'index Index
  @see MaxVar
*}
procedure TC4xInfos.SetVariables(Index, Value : integer);
begin
  FVariables[Index] := Value;
end;

end.

