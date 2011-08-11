unit SimpleSquaresActions;

interface

uses
  SysUtils, Classes, Graphics, TypInfo, ScUtils, ScTypInfo, SdDialogs,
  FunLabyUtils, SimpleSquaresUtils, FunLabyCoreConsts, GR32;

resourcestring
  SCurrent = 'courante';

  SReplaceSquareActionTitle = 'Modifier la case %s';
  SEnableEffectActionTitle = 'Activer l''effet %s';
  SDisableEffectActionTitle = 'Désactiver l''effet %s';
  SMessageActionTitle = 'Message';
  SPlaySoundTitle = 'Jouer le son %s';
  SPlayerColorActionTitle = 'Changer la couleur du pion en %s';
  SPlayerShowTitle = 'Montrer le joueur';
  SPlayerHideTitle = 'Cacher le joueur';
  SPlayerWinTitle = 'Le joueur gagne';
  SPlayerLoseTitle = 'Le joueur perd';
  SPlayerTemporizeTitle = 'Temporisation standard';
  SGoOnMovingTitle = 'Continuer le déplacement';

type
  {*
    Origine d'une position de case
    - poCurrent : case courante (TMoveContext.Pos)
    - poAbsolute : position absolue
  *}
  TSquarePosOrigin = (poCurrent, poAbsolute);

  {*
    Position concernée par une action
    @author sjrd
    @version 5.0
  *}
  TSquarePos = class(TFunLabyPersistent)
  private
    FOrigin: TSquarePosOrigin; /// Origine de la case
    FMapID: TComponentID;      /// ID de la carte (chaîne vide = carte courante)
    FOffset: T3DPoint;         /// Offset par rapport à l'origine
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;
  public
    procedure SetToPosition(const APosition: T3DPoint);
    procedure SetToQPos(const QPos: TQualifiedPos);

    function GetQPos(Master: TMaster): TQualifiedPos;

    function GetText: string;
    function GetFunDelphiCode: string;

    property Offset: T3DPoint read FOffset write FOffset;
  published
    property Origin: TSquarePosOrigin read FOrigin write FOrigin;
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
    Remplacement d'un composant d'une case
    @author sjrd
    @version 5.0
  *}
  TReplaceSquareComponent = class(TFunLabyPersistent)
  private
    FName: string;              /// Nom du remplacement
    FIsReplaced: Boolean;       /// Indique si ce composant est modifié
    FComponentID: TComponentID; /// ID du composant à mettre à la place

    procedure SetIsReplaced(Value: Boolean);
    procedure SetComponentID(const Value: TComponentID);
  public
    constructor Create(const AName: string);

    function GetComponentFunDelphiCode: string;

    property Name: string read FName;
  published
    property IsReplaced: Boolean read FIsReplaced write SetIsReplaced
      default False;
    property ComponentID: TComponentID read FComponentID write SetComponentID;
  end;

  {*
    Action qui remplace une case du labyrinthe par une autre
    @author sjrd
    @version 5.0
  *}
  TReplaceSquareAction = class(TSimpleActionWithSquare)
  private
    /// Changements de composants
    FComponents: array[0..3] of TReplaceSquareComponent;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TReplaceSquareComponent;
  protected
    procedure DefineProperties(Filer: TFunLabyFiler); override;

    function GetTitle: string; override;

    procedure RegisterComponentIDs(ComponentIDs: TStrings); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: TReplaceSquareComponent
      read GetComponents;
  published
    property Field: TReplaceSquareComponent index 0 read GetComponents;
    property Effect: TReplaceSquareComponent index 1 read GetComponents;
    property Tool: TReplaceSquareComponent index 2 read GetComponents;
    property Obstacle: TReplaceSquareComponent index 3 read GetComponents;
  end;

  {*
    Action qui active ou désactive un effet
    @author sjrd
    @version 5.0
  *}
  TChangeEffectEnabledAction = class(TSimpleAction)
  private
    FEffectID: TComponentID; /// ID de l'effet à activer ou désactiver
    FEnabledValue: Boolean;  /// Valeur à donner à sa propriété Enabled
  protected
    function GetTitle: string; override;

    procedure RegisterComponentIDs(ComponentIDs: TStrings); override;
  public
    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property EffectID: TComponentID read FEffectID write FEffectID;
    property EnabledValue: Boolean read FEnabledValue write FEnabledValue
      default False;
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
    Action qui joue un son
    @author sjrd
    @version 5.1
  *}
  TSoundAction = class(TSimpleAction)
  private
    FSound: string; /// Son à jouer
  protected
    function GetTitle: string; override;
  public
    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property Sound: string read FSound write FSound;
  end;

  {*
    Action changer la couleur du pion
    @author sjrd
    @version 5.0
  *}
  TPlayerColorAction = class(TSimpleAction)
  private
    FColor: TColor32; /// Couleur de remplacement
  protected
    function GetTitle: string; override;
  public
    constructor Create; override;

    procedure ProduceFunDelphiCode(Code: TStrings;
      const Indent: string); override;
  published
    property Color: TColor32 read FColor write FColor default clBlue32;
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

function Color32ToString(Color: TColor32): string;

implementation

{-----------------}
{ Global routines }
{-----------------}

{*
  Transforme une couleur en chaîne de caractère
  @param Color   Couleur
  @return Chaîne représentant la couleur
*}
function Color32ToString(Color: TColor32): string;
begin
  case Color of
    clTransparent32: Result := 'clTransparent32';
    clBlack32:       Result := 'clBlack32';
    clDimGray32:     Result := 'clDimGray32';
    clGray32:        Result := 'clGray32';
    clLightGray32:   Result := 'clLightGray32';
    clWhite32:       Result := 'clWhite32';
    clMaroon32:      Result := 'clMaroon32';
    clGreen32:       Result := 'clGreen32';
    clOlive32:       Result := 'clOlive32';
    clNavy32:        Result := 'clNavy32';
    clPurple32:      Result := 'clPurple32';
    clTeal32:        Result := 'clTeal32';
    clRed32:         Result := 'clRed32';
    clLime32:        Result := 'clLime32';
    clYellow32:      Result := 'clYellow32';
    clBlue32:        Result := 'clBlue32';
    clFuchsia32:     Result := 'clFuchsia32';
    clAqua32:        Result := 'clAqua32';
    clTrWhite32:     Result := 'clTrWhite32';
    clTrBlack32:     Result := 'clTrBlack32';
    clTrRed32:       Result := 'clTrRed32';
    clTrGreen32:     Result := 'clTrGreen32';
    clTrBlue32:      Result := 'clTrBlue32';
  else
    Result := '$'+IntToHex(Color, 8);
  end;
end;

{------------------}
{ TSquarePos class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TSquarePos.DefineProperties(Filer: TFunLabyFiler);
begin
  if psReading in PersistentState then
  begin {compatibility}
    FOrigin := poAbsolute;

    Filer.DefineFieldProperty('Position.X', TypeInfo(Integer),
      @FOffset.X, False);
    Filer.DefineFieldProperty('Position.Y', TypeInfo(Integer),
      @FOffset.Y, False);
    Filer.DefineFieldProperty('Position.Z', TypeInfo(Integer),
      @FOffset.Z, False);
  end; {compatibility}

  inherited;

  Filer.DefineFieldProperty('Offset.X', TypeInfo(Integer),
    @FOffset.X, FOffset.X <> 0);
  Filer.DefineFieldProperty('Offset.Y', TypeInfo(Integer),
    @FOffset.Y, FOffset.X <> 0);
  Filer.DefineFieldProperty('Offset.Z', TypeInfo(Integer),
    @FOffset.Z, FOffset.X <> 0);
end;

{*
  Positionne à une position de la carte courante
  @param APosition   Position dans la carte courante
*}
procedure TSquarePos.SetToPosition(const APosition: T3DPoint);
begin
  FOrigin := poAbsolute;
  FMapID := '';
  FOffset := APosition;
end;

{*
  Positionne à une poistion qualifiée
  @param QPos   Position qualifiée
*}
procedure TSquarePos.SetToQPos(const QPos: TQualifiedPos);
begin
  FOrigin := poAbsolute;
  FMapID := QPos.Map.ID;
  FOffset := QPos.Position;
end;

{*
  Obtient une position qualifiée pour un maître donné
  Cette méthode ne doit être appelée que si Origin = poAbsolute.
  @param Master   Maître FunLabyrinthe
  @return Position qualifiée
*}
function TSquarePos.GetQPos(Master: TMaster): TQualifiedPos;
begin
  Assert(Origin = poAbsolute);
  Result.Map := Master.Map[MapID];
  Result.Position := Offset;
end;

{*
  Texte représentant la case à la position indiquée
  @return Texte représentant la case à la position indiquée
*}
function TSquarePos.GetText: string;
begin
  case Origin of
    poCurrent:
    begin
      Result := SCurrent;
      if not Same3DPoint(Offset, Point3D(0, 0, 0)) then
        Result := Result + Format('+(%d, %d, %d)',
          [Offset.X, Offset.Y, Offset.Z]);
    end;

    poAbsolute:
    begin
      Result := Format('%s(%d, %d, %d)',
        [MapID, Offset.X, Offset.Y, Offset.Z]);
    end;
  else
    Assert(False);
  end;
end;

{*
  Code Delphi représentant la case à la position indiquée
  @return Code Delphi représentant la case à la position indiquée
*}
function TSquarePos.GetFunDelphiCode: string;
begin
  // don't localize
  case Origin of
    poCurrent:
    begin
      if Same3DPoint(Offset, Point3D(0, 0, 0)) then
        Result := 'Square'
      else
        Result := Format('Map[Point3DAdd(Pos, %d, %d, %d)]',
          [Offset.X, Offset.Y, Offset.Z]);
    end;

    poAbsolute:
    begin
      if MapID = '' then
        Result := 'Map'
      else
        Result := Format('Master.Map[''%s''].Map', [MapID]);

      Result := Result + Format('[%d, %d, %d]',
        [Offset.X, Offset.Y, Offset.Z]);
    end;
  else
    Assert(False);
  end;
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

{-------------------------------}
{ TReplaceSquareComponent class }
{-------------------------------}

{*
  Crée une instance de TReplaceSquareComponent
  @param AName   Nom du remplacement
*}
constructor TReplaceSquareComponent.Create(const AName: string);
begin
  inherited Create;

  FName := AName;
end;

{*
  Spécifie si ce composant doit être modifié
  @param Value   True s'il doit être modifié, False sinon
*}
procedure TReplaceSquareComponent.SetIsReplaced(Value: Boolean);
begin
  FIsReplaced := Value;
  if not Value then
    FComponentID := '';
end;

{*
  Modifie l'ID du composant à mettre à la place
  @param Value   Nouvel ID
*}
procedure TReplaceSquareComponent.SetComponentID(const Value: TComponentID);
begin
  FIsReplaced := True;
  FComponentID := Value;
end;

{*
  Obtient le code FunDelphi représentant le composant de replacement
  Cette méthode ne doit être appelée que si IsReplaced = True
  @return Code FunDelphi représentant le composant de replacement
*}
function TReplaceSquareComponent.GetComponentFunDelphiCode: string;
begin
  Assert(IsReplaced);
  if ComponentID = '' then
    Result := 'nil'
  else
    Result := ComponentID;
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

  FComponents[0] := TReplaceSquareComponent.Create('Field');
  FComponents[1] := TReplaceSquareComponent.Create('Effect');
  FComponents[2] := TReplaceSquareComponent.Create('Tool');
  FComponents[3] := TReplaceSquareComponent.Create('Obstacle');
end;

{*
  [@inheritDoc]
*}
destructor TReplaceSquareAction.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FComponents)-1 do
    FComponents[I].Free;

  inherited;
end;

{*
  Nombre de composants
  @return Nombre de composants
*}
function TReplaceSquareAction.GetComponentCount: Integer;
begin
  Result := Length(FComponents);
end;

{*
  Tableau zero-based des composants
  @param Index   Index compris entre 0 inclus et ComponentCount exclu
  @return Composant à l'index spécifié
*}
function TReplaceSquareAction.GetComponents(
  Index: Integer): TReplaceSquareComponent;
begin
  Result := FComponents[Index];
end;

{*
  [@inheritDoc]
*}
procedure TReplaceSquareAction.DefineProperties(Filer: TFunLabyFiler);
var
  SquareDef: TSquareDef;
begin
  if psReading in PersistentState then
  begin {compatibility}
    SquareDef := TSquareDef.Create;
    try
      Filer.DefinePersistent('ReplaceBy', SquareDef);

      if SquareDef.FieldID <> '' then // good indicator that ReplaceBy was there
      begin
        Field.ComponentID := SquareDef.FieldID;
        Effect.ComponentID := SquareDef.EffectID;
        Tool.ComponentID := SquareDef.ToolID;
        Obstacle.ComponentID := SquareDef.ObstacleID;
      end;
    finally
      SquareDef.Free;
    end;
  end; {compatibility}

  inherited;
end;

{*
  [@inheritDoc]
*}
function TReplaceSquareAction.GetTitle: string;
begin
  Result := Format(SReplaceSquareActionTitle, [SquarePos.GetText]);
end;

{*
  [@inheritDoc]
*}
procedure TReplaceSquareAction.RegisterComponentIDs(ComponentIDs: TStrings);
var
  I: Integer;
begin
  inherited;

  for I := 0 to ComponentCount-1 do
    ComponentIDs.Add(Components[I].ComponentID);
end;

{*
  [@inheritDoc]
*}
procedure TReplaceSquareAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
var
  I: Integer;
  StrSquarePos, Line: string;
  Modified: TIntegerSet;
begin
  Assert((not Field.IsReplaced) or (Field.ComponentID <> ''),
    'Impossible de supprimer le terrain d''une case');

  StrSquarePos := SquarePos.GetFunDelphiCode;

  Modified := [];
  for I := 0 to ComponentCount-1 do
    if Components[I].IsReplaced then
      Include(Modified, I);

  // No modification
  if Modified = [] then
    Exit;

  // One component modified
  for I := 0 to ComponentCount-1 do
  begin
    if Modified <> [I] then
      Continue;

    with Components[I] do
      Code.Add(Indent + Format('%s.%s := %s;',
        [StrSquarePos, Name, GetComponentFunDelphiCode]));
    Exit;
  end;

  // At least two components modified
  Line := StrSquarePos + ' := ';

  for I := 0 to ComponentCount-1 do
  begin
    with Components[I] do
    begin
      if not IsReplaced then
        Line := Line + 'TSquareComponent(' + StrSquarePos + '.' + Name + ')+'
      else if ComponentID <> '' then
        Line := Line + GetComponentFunDelphiCode + '+';
    end;
  end;

  Line[Length(Line)] := ';';
  Code.Add(Indent + Line);
end;

{----------------------------------}
{ TChangeEffectEnabledAction class }
{----------------------------------}

{*
  [@inheritDoc]
*}
function TChangeEffectEnabledAction.GetTitle: string;
begin
  if EnabledValue then
    Result := Format(SEnableEffectActionTitle, [EffectID])
  else
    Result := Format(SDisableEffectActionTitle, [EffectID]);
end;

{*
  [@inheritDoc]
*}
procedure TChangeEffectEnabledAction.RegisterComponentIDs(
  ComponentIDs: TStrings);
begin
  inherited;

  ComponentIDs.Add(EffectID);
end;

{*
  [@inheritDoc]
*}
procedure TChangeEffectEnabledAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
const
  SelfStatementFormat = 'Enabled := %1:s;';
  OtherStatementFormat = 'SetOrdProp(%s, ''Enabled'', Byte(%s));';
var
  StatementFormat: string;
begin
  if EffectID = '' then
    StatementFormat := SelfStatementFormat
  else
    StatementFormat := OtherStatementFormat;

  Code.Add(Indent + Format(StatementFormat,
    [EffectID, BooleanIdents[EnabledValue]]));
end;

{----------------------}
{ TMessageAction class }
{----------------------}

{*
  [@inheritDoc]
*}
function TMessageAction.GetTitle: string;
begin
  Result := SMessageActionTitle;
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

{--------------------}
{ TSoundAction class }
{--------------------}

{*
  [@inheritDoc]
*}
function TSoundAction.GetTitle: string;
begin
  Result := Format(SPlaySoundTitle, [Sound]);
end;

{*
  [@inheritDoc]
*}
procedure TSoundAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
begin
  Code.Add(Indent + Format('Player.PlaySound(%s);', [StrToStrRepres(Sound)]));
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

  FColor := clBlue32;
end;

{*
  [@inheritDoc]
*}
function TPlayerColorAction.GetTitle: string;
begin
  Result := Format(SPlayerColorActionTitle, [Color32ToString(Color)]);
end;

{*
  [@inheritDoc]
*}
procedure TPlayerColorAction.ProduceFunDelphiCode(Code: TStrings;
  const Indent: string);
begin
  Code.Add(Indent + Format('Player.Color := %s;', [Color32ToString(Color)]));
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
    'Temporize;', 'GoOnMoving := True;'
  );
begin
  Code.Add(Indent + Statements[Kind]);
end;

initialization
  FunLabyRegisterClasses([
    TReplaceSquareAction, TChangeEffectEnabledAction, TMessageAction,
    TSoundAction, TPlayerColorAction, TSimpleMethodAction
  ]);
finalization
  FunLabyUnRegisterClasses([
    TReplaceSquareAction, TChangeEffectEnabledAction, TMessageAction,
    TSoundAction, TPlayerColorAction, TSimpleMethodAction
  ]);
end.

