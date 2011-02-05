{*
  Décrit les effets simples (se suffisant à eux-mêmes)
  L'unité FLBSimpleEffects regroupe les différents effets qui se suffisent à
  eux-mêmes, c'est-à-dire qui n'ont pas besoin de composants annexes pour
  fonctionner.
  @author sjrd
  @version 5.0
*}
unit FLBSimpleEffects;

interface

uses
  SysUtils, Graphics, ScUtils, SdDialogs, FunLabyUtils, FunLabyCoreConsts,
  Generics, MapTools, GraphicsTools, FLBCommon, FLBFields, GR32;

resourcestring
  SNorthArrow = 'Flèche nord';                /// Nom de la flèche nord
  SEastArrow = 'Flèche est';                  /// Nom de la flèche est
  SSouthArrow = 'Flèche sud';                 /// Nom de la flèche sud
  SWestArrow = 'Flèche ouest';                /// Nom de la flèche ouest
  SCrossroads = 'Carrefour';                  /// Nom du carrefour

  SInactiveTransporter = 'Téléporteur inactif'; /// Téléporteur inactif
  STransporter = 'Téléporteur';                 /// Téléporteur

  SUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  SDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant

  SDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  SIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect

  STreasure = 'Trésor';                       /// Nom du trésor

  SSunkenButton = 'Bouton enfoncé';           /// Nom du bouton enfoncé

resourcestring
  SCategoryTransporters = 'Téléporteurs';
  STransporterCreatorHint = 'Créer un nouveau téléporteur';

const {don't localize}
  idNorthArrow = 'NorthArrow';                   /// ID de la flèche nord
  idEastArrow = 'EastArrow';                     /// ID de la flèche est
  idSouthArrow = 'SouthArrow';                   /// ID de la flèche sud
  idWestArrow = 'WestArrow';                     /// ID de la flèche ouest
  idCrossroads = 'Crossroads';                   /// ID du carrefour

  idInactiveTransporter = 'InactiveTransporter'; /// ID du téléporteur inactif

  /// ID du créateur de téléporteurs
  idTransporterCreator = 'TransporterCreator';

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant

  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect

  idTreasure = 'Treasure';                       /// ID du trésor

  idSunkenButton = 'SunkenButton';               /// ID du bouton enfoncé

const {don't localize}
  fNorthArrow = 'Arrows/NorthArrow'; /// Fichier de la flèche nord
  fEastArrow = 'Arrows/EastArrow';   /// Fichier de la flèche est
  fSouthArrow = 'Arrows/SouthArrow'; /// Fichier de la flèche sud
  fWestArrow = 'Arrows/WestArrow';   /// Fichier de la flèche ouest
  fCrossroads = 'Arrows/Crossroads'; /// Fichier du carrefour

  fTransporter = 'Miscellaneous/Transporter'; /// Fichier du téléporteur
  fCreator = 'Creators/Creator';              /// Fichier des créateurs
  /// Fichier du téléporteur désactivé
  fDisabledTransporter = 'Miscellaneous/DisabledTransporter';

  fUpStairs = 'Stairs/UpStairs';     /// Fichier de l'escalier montant
  fDownStairs = 'Stairs/DownStairs'; /// Fichier de l'escalier descendant

  /// Fichier du tourniquet direct
  fDirectTurnstile = 'Arrows/DirectTurnstile';
  /// Fichier du tourniquet indirect
  fIndirectTurnstile = 'Arrows/IndirectTurnstile';

  fTreasure = 'Chests/Treasure'; /// Fichier du trésor

resourcestring
  sFoundTreasure = 'BRAVO ! Tu as trouvé le trésor !';

resourcestring
  sPlaceStairsTitle = 'Placement d''un escalier';
  sCantPlaceUpStairsAtLastFloor =
    'Impossible de placer un escalier montant au dernier étage';
  sCantPlaceDownStairsAtFirstFloor =
    'Impossible de placer un escalier descendant au rez-de-chaussée';
  sDifferentStairsDestSquare =
    'La case à l''autre bout de l''escalier n''est pas la même que celle-ci. '+
    'Êtes-vous sûr de vouloir placer un escalier ici ?';

type
  {*
    Type de téléporteur
    @author sjrd
    @version 5.0
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Flèche (de toutes directions)
    Les flèches repoussent le joueur dans la direction qui leur est propre. Le
    carrefour en est un cas particulier qui laisse inchangée la direction du
    joueur.
    @author sjrd
    @version 5.0
  *}
  TArrow = class(TEffect)
  private
    FDirection: TDirection;        /// Direction de la flèche
    FDefaultDirection: TDirection; /// Direction par défaut

    function IsDirectionStored: Boolean;
  protected
    procedure StoreDefaults; override;
  public
    constructor CreateArrow(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADirection: TDirection);

    procedure Execute(Context: TMoveContext); override;
  published
    property Direction: TDirection read FDirection write FDirection
      stored IsDirectionStored;
  end;

  {*
    Téléporteur
    Le téléporteur emmène le joueur à un autre téléporteur
    @author sjrd
    @version 5.0
  *}
  TTransporter = class(TEffect)
  private
    FDisabledPainter: TPainter; /// Peintre désactivé
    FKind: TTransporterKind;    /// Type de téléporteur
  protected
    function GetCategory: string; override;

    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure Execute(Context: TMoveContext); override;
  published
    property Enabled;
    property DisabledPainter: TPainter read FDisabledPainter;

    property Kind: TTransporterKind read FKind write FKind default tkNext;
  end;

  {*
    Téléporteur inactif
    @author sjrd
    @version 1.0
  *}
  TInactiveTransporter = class(TTransporter)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  protected
    function GetCategory: string; override;
  published
    property Kind default tkInactive;
  end;

  {*
    Créateur de téléporteurs
    @author sjrd
    @version 5.0
  *}
  TTransporterCreator = class(TComponentCreator)
  protected
    function GetCategory: string; override;
    function GetHint: string; override;

    function GetComponentClass: TFunLabyComponentClass; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  end;

  {*
    Escaliers
    Les escaliers permettent de monter ou descendre d'un étage
    @author sjrd
    @version 5.1.2
  *}
  TStairs = class(TEffect)
  private
    FPairingStairs: TStairs; /// Escaliers qui vont de paire avec ceux-ci

    FDefaultPairingStairs: TStairs; /// Valeur par défaut de PairingStairs

    procedure EditSquareMap(var Msg: TEditMapSquareMessage);
      message msgEditMapSquare;

    function IsPairingStairsStored: Boolean;
  protected
    procedure StoreDefaults; override;

    function GetDestination(const Pos: T3DPoint): T3DPoint; virtual; abstract;

    procedure ErrorDestIsOutsideMap; virtual; abstract;
  public
    procedure Execute(Context: TMoveContext); override;
  published
    property PairingStairs: TStairs read FPairingStairs write FPairingStairs
      stored IsPairingStairsStored;
  end;

  {*
    Escalier montant
    @author sjrd
    @version 5.1.2
  *}
  TUpStairs = class(TStairs)
  protected
    function GetDestination(const Pos: T3DPoint): T3DPoint; override;

    procedure ErrorDestIsOutsideMap; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  end;

  {*
    Escalier descendant
    @author sjrd
    @version 5.1.2
  *}
  TDownStairs = class(TStairs)
  protected
    function GetDestination(const Pos: T3DPoint): T3DPoint; override;

    procedure ErrorDestIsOutsideMap; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'à
    parvenir à en sortir.
    @author sjrd
    @version 5.0
  *}
  TDirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Exited(Context: TMoveContext); override;

    procedure Execute(Context: TMoveContext); override;
  end;

  {*
    Tourniquet Indirect
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'à
    parvenir à en sortir.
    @author sjrd
    @version 5.0
  *}
  TIndirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Exited(Context: TMoveContext); override;

    procedure Execute(Context: TMoveContext); override;
  end;

  {*
    Trésor
    Le trésor fait remporter la victoire au joueur qui le trouve.
    @author sjrd
    @version 5.0
  *}
  TTreasure = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Execute(Context: TMoveContext); override;
  end;

var { FunDelphi codegen }
  compNorthArrow: TArrow;
  compEastArrow: TArrow;
  compSouthArrow: TArrow;
  compWestArrow: TArrow;
  compCrossroads: TArrow;

  compInactiveTransporter: TInactiveTransporter;

  compTransporterCreator: TTransporterCreator;

  compUpStairs: TStairs;
  compDownStairs: TStairs;

  compDirectTurnstile: TDirectTurnstile;
  compIndirectTurnstile: TIndirectTurnstile;

  compTreasure: TTreasure;

  compSunkenButton: TDecorativeEffect;

implementation

{---------------}
{ Classe TArrow }
{---------------}

{*
  Crée une instance de TArrow
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param ADirection   Direction de la flèche
*}
constructor TArrow.CreateArrow(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADirection: TDirection);
begin
  Create(AMaster, AID);

  Name := AName;
  Direction := ADirection;

  case Direction of
    diNone:  Painter.AddImage(fCrossroads);
    diNorth: Painter.AddImage(fNorthArrow);
    diEast:  Painter.AddImage(fEastArrow);
    diSouth: Painter.AddImage(fSouthArrow);
    diWest:  Painter.AddImage(fWestArrow);
  end;
end;

{*
  Teste si la propriété Direction doit être enregistrée
  @return True si la propriété Direction doit être enregistrée, False sinon
*}
function TArrow.IsDirectionStored: Boolean;
begin
  Result := FDirection <> FDefaultDirection;
end;

{*
  [@inheritDoc]
*}
procedure TArrow.StoreDefaults;
begin
  inherited;

  FDefaultDirection := FDirection;
end;

{*
  [@inheritDoc]
*}
procedure TArrow.Execute(Context: TMoveContext);
begin
  with Context do
  begin
    if Direction <> diNone then
      Player.Direction := FDirection;
    GoOnMoving := True;
  end;
end;

{---------------------}
{ Classe TTransporter }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TTransporter.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  AutoEditVisualTag;

  FDisabledPainter := TPainter.Create(Master.ImagesMaster);
  FDisabledPainter.BeginUpdate;

  FKind := tkNext;

  Name := STransporter;
  Painter.AddImage(fTransporter);
  DisabledPainter.AddImage(fDisabledTransporter);
end;

{*
  [@inheritDoc]
*}
destructor TTransporter.Destroy;
begin
  FDisabledPainter.Free;

  inherited;
end;

{*
  [@inheritDoc]
*}
function TTransporter.GetCategory: string;
begin
  Result := SCategoryTransporters;
end;

{*
  [@inheritDoc]
*}
procedure TTransporter.DoDraw(Context: TDrawSquareContext);
begin
  if Enabled then
    Painter.Draw(Context)
  else
    DisabledPainter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TTransporter.AfterConstruction;
begin
  inherited;

  FDisabledPainter.EndUpdate;
end;

{*
  [@inheritDoc]
*}
procedure TTransporter.Execute(Context: TMoveContext);
var
  Other: T3DPoint;
begin
  with Context do
  begin
    Other := Pos;

    // Recherche de la case de destination
    case FKind of
      tkNext:     FindNextSquare(Map, Other, Self);
      tkPrevious: FindPreviousSquare(Map, Other, Self);
      tkRandom:   FindSquareAtRandom(Map, Other, Self);
    else
      Exit; // on évite des tests inutiles pour un inactif
    end;

    // Si l'on a trouvé une autre case, on déplace le joueur
    if Same3DPoint(Other, Pos) then
      Exit;
    Temporize;
    Player.MoveTo(Other);
  end;
end;

{-----------------------------}
{ Classe TInactiveTransporter }
{-----------------------------}

{*
  [@inheritDoc]
*}
constructor TInactiveTransporter.Create(AMaster: TMaster;
  const AID: TComponentID);
begin
  inherited;

  Kind := tkInactive;
end;

{*
  [@inheritDoc]
*}
function TInactiveTransporter.GetCategory: string;
begin
  Result := SCategoryNeutrals;
end;

{---------------------------}
{ TTransporterCreator class }
{---------------------------}

{*
  [@inheritDoc]
*}
constructor TTransporterCreator.Create(AMaster: TMaster;
  const AID: TComponentID);
begin
  inherited;

  IconPainter.AddImage(fTransporter);
  IconPainter.AddImage(fCreator);
end;

{*
  [@inheritDoc]
*}
function TTransporterCreator.GetCategory: string;
begin
  Result := SCategoryTransporters;
end;

{*
  [@inheritDoc]
*}
function TTransporterCreator.GetHint: string;
begin
  Result := STransporterCreatorHint;
end;

{*
  [@inheritDoc]
*}
function TTransporterCreator.GetComponentClass: TFunLabyComponentClass;
begin
  Result := TTransporter;
end;

{----------------}
{ Classe TStairs }
{----------------}

{*
  Déclenché en édition lorsqu'une case est modifiée avec un escalier
  @param Msg   Message
*}
procedure TStairs.EditSquareMap(var Msg: TEditMapSquareMessage);
var
  Other: T3DPoint;
begin
  with Msg, QPos do
  begin
    Other := GetDestination(Position);

    if (Phase = espAdd) and (not Map.InMap(Other)) then
    begin
      ErrorDestIsOutsideMap;
      Include(Flags, esfCancel);
      Exit;
    end;

    if (not Map.InMap(Other)) or (PairingStairs = nil) then
      Exit;

    if Phase = espRemove then
    begin
      Map[Position] := RemoveEffect(Map[Position]);
      Map[Other] := RemoveEffect(Map[Other]);
      Include(Flags, esfHandled);
    end;

    if Phase = espAdd then
    begin
      if Map[Other] <> Map[Position] then
      begin
        if ShowDialog(sPlaceStairsTitle, sDifferentStairsDestSquare,
          dtConfirmation, dbOKCancel) <> drOK then
        begin
          Include(Flags, esfCancel);
          Exit;
        end;
      end;

      Map[Position] := ChangeEffect(Map[Position], Self);
      Map[Other] := ChangeEffect(Map[Other], PairingStairs);

      Include(Flags, esfHandled);
    end;
  end;
end;

{*
  Teste si la propriété PairingStairs doit être enregistrée
  @return True si la propriété PairingStairs doit être enregistrée, False sinon
*}
function TStairs.IsPairingStairsStored: Boolean;
begin
  Result := FPairingStairs <> FDefaultPairingStairs;
end;

{*
  [@inheritDoc]
*}
procedure TStairs.StoreDefaults;
begin
  inherited;

  FDefaultPairingStairs := FPairingStairs;
end;

{*
  [@inheritDoc]
*}
procedure TStairs.Execute(Context: TMoveContext);
begin
  with Context do
  begin
    Temporize;
    Player.MoveTo(GetDestination(Pos));
  end;
end;

{-----------------}
{ TUpStairs class }
{-----------------}

{*
  [@inheritDoc]
*}
constructor TUpStairs.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SUpStairs;
  Painter.AddImage(fUpStairs);
end;

{*
  [@inheritDoc]
*}
function TUpStairs.GetDestination(const Pos: T3DPoint): T3DPoint;
begin
  Result := Pos;
  Inc(Result.Z);
end;

{*
  [@inheritDoc]
*}
procedure TUpStairs.ErrorDestIsOutsideMap;
begin
  ShowDialog(sPlaceStairsTitle, sCantPlaceUpStairsAtLastFloor, dtError);
end;

{-------------------}
{ TDownStairs class }
{-------------------}

{*
  [@inheritDoc]
*}
constructor TDownStairs.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SDownStairs;
  Painter.AddImage(fDownStairs);
end;

{*
  [@inheritDoc]
*}
function TDownStairs.GetDestination(const Pos: T3DPoint): T3DPoint;
begin
  Result := Pos;
  Dec(Result.Z);
end;

{*
  [@inheritDoc]
*}
procedure TDownStairs.ErrorDestIsOutsideMap;
begin
  ShowDialog(sPlaceStairsTitle, sCantPlaceDownStairsAtFirstFloor, dtError);
end;

{-------------------------}
{ Classe TDirectTurnstile }
{-------------------------}

{*
  [@inheritDoc]
*}
constructor TDirectTurnstile.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SDirectTurnstile;
  Painter.AddImage(fDirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TDirectTurnstile.Exited(Context: TMoveContext);
begin
  with Context do
    Square := ChangeEffect(Square, idIndirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TDirectTurnstile.Execute(Context: TMoveContext);
var
  Dir: TDirection;
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  with Context do
  begin
    Temporize;

    Dir := RightDir[Player.Direction];

    repeat
      Player.Move(Dir, Redo, RedoDelay);
      GoOnMoving := Redo;
      Temporization := RedoDelay;

      Dir := LeftDir[Player.Direction];
    until not Same3DPoint(Player.Position, Pos);
  end;
end;

{---------------------------}
{ Classe TIndirectTurnstile }
{---------------------------}

{*
  [@inheritDoc]
*}
constructor TIndirectTurnstile.Create(AMaster: TMaster;
  const AID: TComponentID);
begin
  inherited;

  Name := SIndirectTurnstile;
  Painter.AddImage(fIndirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TIndirectTurnstile.Exited(Context: TMoveContext);
begin
  with Context do
    Square := ChangeEffect(Square, idDirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TIndirectTurnstile.Execute(Context: TMoveContext);
var
  Dir: TDirection;
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  with Context do
  begin
    Temporize;

    Dir := LeftDir[Player.Direction];

    repeat
      Player.Move(Dir, Redo, RedoDelay);
      GoOnMoving := Redo;
      Temporization := RedoDelay;

      Dir := RightDir[Player.Direction];
    until not Same3DPoint(Player.Position, Pos);
  end;
end;

{------------------}
{ Classe TTreasure }
{------------------}

{*
  [@inheritDoc]
*}
constructor TTreasure.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := STreasure;
  Painter.AddImage(fTreasure);
end;

{*
  [@inheritDoc]
*}
procedure TTreasure.Execute(Context: TMoveContext);
begin
  with Context.Player do
  begin
    Win;
    ShowMessage(sFoundTreasure);
  end;
end;

end.

