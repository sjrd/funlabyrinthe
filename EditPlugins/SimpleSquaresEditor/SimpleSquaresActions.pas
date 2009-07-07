unit SimpleSquaresActions;

interface

uses
  SysUtils, Classes, Graphics, TypInfo, ScUtils, ScDelphiLanguage, SdDialogs,
  FunLabyUtils, SimpleSquaresUtils;

resourcestring
  SReplaceSquareActionTitle = 'Remplacer la case (%d, %d, %d)';
  SDeactivateEffectActionTitle = 'Désactiver';
  SMessageActionTitle = 'Afficher %s';
  SPlayerColorActionTitle = 'Changer la couleur du pion en %s';
  SPlayerShowTitle = 'Montrer le joueur';
  SPlayerHideTitle = 'Cacher le joueur';
  SPlayerWinTitle = 'Le joueur gagne';
  SPlayerLoseTitle = 'Le joueur perd';
  SPlayerTemporizeTitle = 'Temporisation standard';
  SGoOnMovingTitle = 'Continuer le déplacement';

type
  {*
    Position concernée par une action
    @author sjrd
    @version 5.0
  *}
  TSquarePos = class(TFunLabyPersistent)
  private
    FMapID: TComponentID; /// ID de la carte (chaîne vide = carte courante)
    FPosition: T3DPoint;  /// Position sur la carte
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
  public
    procedure SetToPosition(const APosition: T3DPoint);
    procedure SetToQPos(const QPos: TQualifiedPos);

    function GetQPos(Master: TMaster): TQualifiedPos;

    function GetFunDelphiCode: string;

    property Position: T3DPoint read FPosition write FPosition;
  published
    property MapID: TComponentID read FMapID write FMapID;
  end;

  {*
    Définition d'une case
    @author sjrd
    @version 5.0
  *}
  TSquareDef = class(TFunLabyPersistent)
  private
    FFieldID: TComponentID;    /// ID du terrain
    FEffectID: TComponentID;   /// ID de l'effet
    FToolID: TComponentID;     /// ID de l'outil
    FObstacleID: TComponentID; /// ID de l'obstacle
  public
    procedure SetToSquare(Square: TSquare);

    function GetFunDelphiCode: string;
  published
    property FieldID: TComponentID read FFieldID write FFieldID;
    property EffectID: TComponentID read FEffectID write FEffectID;
    property ToolID: TComponentID read FToolID write FToolID;
    property ObstacleID: TComponentID read FObstacleID write FObstacleID;
  end;

  {*
    Action qui concerne une case
    @author sjrd
    @version 5.0
  *}
  TSimpleActionWithSquare = class(TSimpleAction)
  private
    FSquarePos: TSquarePos; /// Position concernée
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property SquarePos: TSquarePos read FSquarePos;
  end;

  {*
    Action qui remplace une case du labyrinthe par une autre
    @author sjrd
    @version 5.0
  *}
  TReplaceSquareAction = class(TSimpleActionWithSquare)
  private
    FReplaceBy: TSquareDef; /// Case à mettre à la place
  protected
    function GetTitle: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property ReplaceBy: TSquareDef read FReplaceBy;
  end;

  {*
    Action qui désactive l'effet courant (ou le remplace par un autre)
    @author sjrd
    @version 5.0
  *}
  TDeactivateEffectAction = class(TSimpleAction)
  private
    FEffectID: TComponentID; /// ID de l'effet à mettre à la place
  protected
    function GetTitle: string; override;
  public
    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property EffectID: TComponentID read FEffectID write FEffectID;
  end;

  {*
    Action qui affiche un message
    @author sjrd
    @version 5.0
  *}
  TMessageAction = class(TSimpleAction)
  private
    FText: string;           /// Texte du message
    FOnlyFirstTime: Boolean; /// True affiche seulement au premier passage
  protected
    function GetTitle: string; override;
  public
    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property Text: string read FText write FText;
    property OnlyFirstTime: Boolean read FOnlyFirstTime write FOnlyFirstTime
      default False;
  end;

  {*
    Action changer la couleur du pion
    @author sjrd
    @version 5.0
  *}
  TPlayerColorAction = class(TSimpleAction)
  private
    FColor: TColor; /// Couleur de remplacement
  protected
    function GetTitle: string; override;
  public
    constructor Create; override;

    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property Color: TColor read FColor write FColor default clBlue;
  end;

  {*
    Type de méthode simple
    - smPlayerShow : Player.Show;
    - smPlayerHide : Player.Hide;
    - smPlayerWin : Player.Win;
    - smPlayerLose : Player.Lose;
    - smPlayerTemporize : Player.Temporize;
    - smGoOnMoving : GoOnMoving := True;
  *}
  TSimpleMethodKind = (
    smPlayerShow, smPlayerHide, smPlayerWin, smPlayerLose,
    smPlayerTemporize, smGoOnMoving
  );

  {*
    Action méthode simple
    @author sjrd
    @version 5.0
  *}
  TSimpleMethodAction = class(TSimpleAction)
  private
    FKind: TSimpleMethodKind; /// Type de l'action
  protected
    function GetTitle: string; override;
  public
    constructor Create; override;

    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property Kind: TSimpleMethodKind read FKind write FKind;
  end;

implementation

{------------------}
{ TSquarePos class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TSquarePos.DefineProperties(Filer: TFunLabyFiler);
begin
  inherited;

  Filer.DefineFieldProperty('Position.X', TypeInfo(Integer),
    @FPosition.X, True);
  Filer.DefineFieldProperty('Position.Y', TypeInfo(Integer),
    @FPosition.Y, True);
  Filer.DefineFieldProperty('Position.Z', TypeInfo(Integer),
    @FPosition.Z, True);
end;

{*
  Positionne à une position de la carte courante
  @param APosition   Position dans la carte courante
*}
procedure TSquarePos.SetToPosition(const APosition: T3DPoint);
begin
  FMapID := '';
  FPosition := APosition;
end;

{*
  Positionne à une poistion qualifiée
  @param QPos   Position qualifiée
*}
procedure TSquarePos.SetToQPos(const QPos: TQualifiedPos);
begin
  FMapID := QPos.Map.ID;
  FPosition := QPos.Position;
end;

{*
  Obtient une position qualifiée pour un maître donné
  @param Master   Maître FunLabyrinthe
  @return Position qualifiée
*}
function TSquarePos.GetQPos(Master: TMaster): TQualifiedPos;
begin
  Result.Map := Master.Map[MapID];
  Result.Position := Position;
end;

{*
  Code Delphi représentant la case à la position indiquée
  @return Code Delphi représentant la case à la position indiquée
*}
function TSquarePos.GetFunDelphiCode: string;
begin
  // don't localize
  if MapID = '' then
    Result := 'Map'
  else
    Result := Format('Master.Map[''%s''].Map', [MapID]);

  Result := Result + Format('[%d, %d, %d]',
    [Position.X, Position.Y, Position.Z]);
end;

{------------------}
{ TSquareDef class }
{------------------}

{*
  Positionne à une case
  @param Square   Case
*}
procedure TSquareDef.SetToSquare(Square: TSquare);
begin
  FFieldID := Square.Field.SafeID;
  FEffectID := Square.Effect.SafeID;
  FToolID := Square.Tool.SafeID;
  FObstacleID := Square.Obstacle.SafeID;
end;

{*
  Code Delphi représentant la case à la position indiquée
  @return Code Delphi représentant la case à la position indiquée
*}
function TSquareDef.GetFunDelphiCode: string;
begin
  Result := Format('Master.Square[%s]',
    [StrToStrRepres(Format(SquareIDFormat,
    [FieldID, EffectID, ToolID, ObstacleID]))]);
end;

{-------------------------------}
{ TSimpleActionWithSquare class }
{-------------------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleActionWithSquare.Create;
begin
  inherited;
  FSquarePos := TSquarePos.Create;
end;

{*
  [@inheritDoc]
*}
destructor TSimpleActionWithSquare.Destroy;
begin
  FSquarePos.Free;
  inherited;
end;

{----------------------------}
{ TReplaceSquareAction class }
{----------------------------}

{*
  [@inheritDoc]
*}
constructor TReplaceSquareAction.Create;
begin
  inherited;
  FReplaceBy := TSquareDef.Create;
end;

{*
  [@inheritDoc]
*}
destructor TReplaceSquareAction.Destroy;
begin
  FReplaceBy.Free;
  inherited;
end;

{*
  [@inheritDoc]
*}
function TReplaceSquareAction.GetTitle: string;
begin
  Result := Format(SReplaceSquareActionTitle,
    [SquarePos.Position.X, SquarePos.Position.Y, SquarePos.Position.Z]);
end;

{*
  [@inheritDoc]
*}
procedure TReplaceSquareAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
begin
  Code.Add(Indent + Format('%s := ', [SquarePos.GetFunDelphiCode]));
  Code.Add(Indent + Format('  %s;', [ReplaceBy.GetFunDelphiCode]));
end;

{-------------------------------}
{ TDeactivateEffectAction class }
{-------------------------------}

{*
  [@inheritDoc]
*}
function TDeactivateEffectAction.GetTitle: string;
begin
  Result := SDeactivateEffectActionTitle;
end;

{*
  [@inheritDoc]
*}
procedure TDeactivateEffectAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
const
  StatementFormat = 'Square.Effect := %s;';
var
  NewEffect: string;
begin
  if EffectID = '' then
    NewEffect := 'nil'
  else
    NewEffect := Format('Master.Effect[%s]', [StrToStrRepres(EffectID)]);

  Code.Add(Indent + Format(StatementFormat, [NewEffect]));
end;

{----------------------}
{ TMessageAction class }
{----------------------}

{*
  [@inheritDoc]
*}
function TMessageAction.GetTitle: string;
begin
  Result := sMessage;
end;

{*
  [@inheritDoc]
*}
procedure TMessageAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
var
  NewIndent: string;
begin
  if OnlyFirstTime then
  begin
    Code.Add(Indent + 'if IsFirstTime(Player) then');
    NewIndent := Indent + '  ';
  end else
    NewIndent := Indent;

  Code.Add(NewIndent + Format('Player.ShowMessage(%s);',
    [StrToStrRepres(Text)]));
end;

{--------------------------}
{ TPlayerColorAction class }
{--------------------------}

{*
  [@inheritDoc]
*}
constructor TPlayerColorAction.Create;
begin
  inherited;

  FColor := clBlue;
end;

{*
  [@inheritDoc]
*}
function TPlayerColorAction.GetTitle: string;
begin
  Result := Format(SPlayerColorActionTitle, [ColorToString(Color)]);
end;

{*
  [@inheritDoc]
*}
procedure TPlayerColorAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
begin
  Code.Add(Indent + Format('Player.Color := %s;', [ColorToString(Color)]));
end;

{---------------------------}
{ TSimpleMethodAction class }
{---------------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleMethodAction.Create;
begin
  inherited;

  FKind := smPlayerTemporize;
end;

{*
  [@inheritDoc]
*}
function TSimpleMethodAction.GetTitle: string;
begin
  case Kind of
    smPlayerShow: Result := SPlayerShowTitle;
    smPlayerHide: Result := SPlayerHideTitle;
    smPlayerWin:  Result := SPlayerWinTitle;
    smPlayerLose: Result := SPlayerLoseTitle;
    smPlayerTemporize: Result := SPlayerTemporizeTitle;
    smGoOnMoving: Result := SGoOnMovingTitle;
  end;

{$IF Ord(High(TSimpleMethodKind)) <> 5}
  {$MESSAGE ERROR
    'Every possible value of TSimpleMethodKind must be handled here'}
{$IFEND}
end;

{*
  [@inheritDoc]
*}
procedure TSimpleMethodAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
const {don't localize}
  Statements: array[TSimpleMethodKind] of string = (
    'Player.Show;', 'Player.Hide;', 'Player.Win;', 'Player.Lose;',
    'Player.Temporize;', 'GoOnMoving := True;'
  );
begin
  Code.Add(Indent + Statements[Kind]);
end;

initialization
  FunLabyRegisterClasses([
    TReplaceSquareAction, TDeactivateEffectAction, TMessageAction,
    TPlayerColorAction, TSimpleMethodAction
  ]);
finalization
  FunLabyUnRegisterClasses([
    TReplaceSquareAction, TDeactivateEffectAction, TMessageAction,
    TPlayerColorAction, TSimpleMethodAction
  ]);
end.

