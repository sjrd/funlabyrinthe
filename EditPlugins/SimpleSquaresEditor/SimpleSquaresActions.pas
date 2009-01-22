unit SimpleSquaresActions;

interface

uses
  SysUtils, Classes, TypInfo, ScUtils, ScDelphiLanguage, SdDialogs,
  FunLabyUtils, SimpleSquaresUtils;

resourcestring
  SReplaceSquareActionTitle = 'Remplacer la case (%d, %d, %d)';
  SMessageActionTitle = 'Afficher %s';

type
  {*
    Position concern�e par une action
    @author sjrd
    @version 5.0
  *}
  TSquarePos = class
  private
    FMapID: TComponentID; /// ID de la carte (cha�ne vide = carte courante)
    FPosition: T3DPoint;  /// Position sur la carte
  public
    constructor Load(Stream: TStream);
    constructor Create;

    procedure Save(Stream: TStream);

    procedure SetToPosition(const APosition: T3DPoint);
    procedure SetToQPos(const QPos: TQualifiedPos);

    function GetQPos(Master: TMaster): TQualifiedPos;

    function GetDelphiCode: string;

    property MapID: TComponentID read FMapID write FMapID;
    property Position: T3DPoint read FPosition write FPosition;
  end;

  {*
    D�finition d'une case
    @author sjrd
    @version 5.0
  *}
  TSquareDef = class
  private
    FFieldID: TComponentID;    /// ID du terrain
    FEffectID: TComponentID;   /// ID de l'effet
    FToolID: TComponentID;     /// ID de l'outil
    FObstacleID: TComponentID; /// ID de l'obstacle
  public
    constructor Load(Stream: TStream);
    constructor Create;

    procedure Save(Stream: TStream);

    procedure SetToSquare(Square: TSquare);

    function GetDelphiCode: string;

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
    FSquarePos: TSquarePos; /// Position concern�e
  protected
    procedure Save(Stream: TStream); override;
  public
    constructor Load(Stream: TStream); override;
    constructor Create; override;
    destructor Destroy; override;

    property SquarePos: TSquarePos read FSquarePos;
  end;

  {*
    Action qui remplace une case du labyrinthe par une autre
    @author sjrd
    @version 5.0
  *}
  TReplaceSquareAction = class(TSimpleActionWithSquare)
  private
    FReplaceBy: TSquareDef; /// Case � mettre � la place
  protected
    procedure Save(Stream: TStream); override;

    function GetTitle: string; override;
  public
    constructor Load(Stream: TStream); override;
    constructor Create; override;
    destructor Destroy; override;

    procedure ProduceDelphiCode(Code: TStrings; const Indent: string); override;

    property ReplaceBy: TSquareDef read FReplaceBy;
  end;

  {*
    Type de message simple
    mkMessage : Message
    mkTip : Indice
    mkBlindAlley : Impasse
    mkWin : Gagn� !
    mkLoose : Perdu !
    mkCustom : personnalis�
  *}
  TSimpleMessageKind = (
    mkMessage, mkTip, mkBlindAlley, mkWon, mkLost, mkCustom
  );

  {*
    Action qui affiche un message
    @author sjrd
    @version 5.0
  *}
  TMessageAction = class(TSimpleAction)
  private
    FKind: TSimpleMessageKind; /// Type de message
    FDialogTitle: string;      /// Titre du message
    FText: string;             /// Texte du message
    FDialogType: TDialogType;  /// Type de bo�te de dialogue
    FOnlyFirstTime: Boolean;   /// True affiche seulement au premier passage

    procedure SetKind(Value: TSimpleMessageKind);
    procedure SetDialogTitle(const Value: string);
    procedure SetDialogType(Value: TDialogType);
  protected
    procedure Save(Stream: TStream); override;

    function GetTitle: string; override;
  public
    constructor Load(Stream: TStream); override;
    constructor Create; override;
    destructor Destroy; override;

    procedure ProduceDelphiCode(Code: TStrings; const Indent: string); override;

    property Kind: TSimpleMessageKind read FKind write SetKind;
    property DialogTitle: string read FDialogTitle write SetDialogTitle;
    property Text: string read FText write FText;
    property DialogType: TDialogType read FDialogType write SetDialogType;
    property OnlyFirstTime: Boolean read FOnlyFirstTime write FOnlyFirstTime;
  end;

implementation

{------------------}
{ TSquarePos class }
{------------------}

{*
  Charge depuis un flux
  @param Stream   Flux source
*}
constructor TSquarePos.Load(Stream: TStream);
begin
  inherited Create;

  FMapID := ReadStrFromStream(Stream);
  Stream.ReadBuffer(FPosition, SizeOf(T3DPoint));
end;

{*
  Cr�e une position
*}
constructor TSquarePos.Create;
begin
  inherited Create;
end;

{*
  Enregistre dans un flux
  @param Stream   Flux destination
*}
procedure TSquarePos.Save(Stream: TStream);
begin
  WriteStrToStream(Stream, FMapID);
  Stream.WriteBuffer(FPosition, SizeOf(T3DPoint));
end;

{*
  Positionne � une position de la carte courante
  @param APosition   Position dans la carte courante
*}
procedure TSquarePos.SetToPosition(const APosition: T3DPoint);
begin
  FMapID := '';
  FPosition := APosition;
end;

{*
  Positionne � une poistion qualifi�e
  @param QPos   Position qualifi�e
*}
procedure TSquarePos.SetToQPos(const QPos: TQualifiedPos);
begin
  FMapID := QPos.Map.ID;
  FPosition := QPos.Position;
end;

{*
  Obtient une position qualifi�e pour un ma�tre donn�
  @param Master   Ma�tre FunLabyrinthe
  @return Position qualifi�e
*}
function TSquarePos.GetQPos(Master: TMaster): TQualifiedPos;
begin
  Result.Map := Master.Map[MapID];
  Result.Position := Position;
end;

{*
  Code Delphi repr�sentant la case � la position indiqu�e
  @return Code Delphi repr�sentant la case � la position indiqu�e
*}
function TSquarePos.GetDelphiCode: string;
begin
  // don't localize
  if MapID = '' then
    Result := 'Player.Map'
  else
    Result := Format('Master.Map[''%s'']', [MapID]);

  Result := Result + Format('.Map[Point3D(%d, %d, %d)]',
    [Position.X, Position.Y, Position.Z]);
end;

{------------------}
{ TSquareDef class }
{------------------}

{*
  Charge depuis un flux
  @param Stream   Flux source
*}
constructor TSquareDef.Load(Stream: TStream);
begin
  inherited Create;

  FFieldID := ReadStrFromStream(Stream);
  FEffectID := ReadStrFromStream(Stream);
  FToolID := ReadStrFromStream(Stream);
  FObstacleID := ReadStrFromStream(Stream);
end;

{*
  Cr�e une d�finition de case
*}
constructor TSquareDef.Create;
begin
  inherited Create;
end;

{*
  Enregistre dans un flux
  @param Stream   Flux destination
*}
procedure TSquareDef.Save(Stream: TStream);
begin
  WriteStrToStream(Stream, FFieldID);
  WriteStrToStream(Stream, FEffectID);
  WriteStrToStream(Stream, FToolID);
  WriteStrToStream(Stream, FObstacleID);
end;

{*
  Positionne � une case
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
  Code Delphi repr�sentant la case � la position indiqu�e
  @return Code Delphi repr�sentant la case � la position indiqu�e
*}
function TSquareDef.GetDelphiCode: string;
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
constructor TSimpleActionWithSquare.Load(Stream: TStream);
begin
  inherited;
  FSquarePos := TSquarePos.Load(Stream);
end;

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

{*
  [@inheritDoc]
*}
procedure TSimpleActionWithSquare.Save(Stream: TStream);
begin
  inherited;

  FSquarePos.Save(Stream);
end;

{----------------------------}
{ TReplaceSquareAction class }
{----------------------------}

{*
  [@inheritDoc]
*}
constructor TReplaceSquareAction.Load(Stream: TStream);
begin
  inherited;
  FReplaceBy := TSquareDef.Load(Stream);
end;

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
procedure TReplaceSquareAction.Save(Stream: TStream);
begin
  inherited;

  FReplaceBy.Save(Stream);
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
procedure TReplaceSquareAction.ProduceDelphiCode(Code: TStrings;
  const Indent: string);
begin
  Code.Add(Indent + Format('%s := ', [SquarePos.GetDelphiCode]));
  Code.Add(Indent + Format('  %s;', [ReplaceBy.GetDelphiCode]));
end;

{----------------------}
{ TMessageAction class }
{----------------------}

{*
  [@inheritDoc]
*}
constructor TMessageAction.Load(Stream: TStream);
begin
  inherited;

  Stream.ReadBuffer(FKind, SizeOf(TSimpleMessageKind));

  if Kind = mkCustom then
  begin
    FDialogTitle := ReadStrFromStream(Stream);
    Stream.ReadBuffer(FDialogType, SizeOf(TDialogType));
  end else
    SetKind(FKind);

  FText := ReadStrFromStream(Stream);

  Stream.ReadBuffer(FOnlyFirstTime, SizeOf(Boolean));
end;

{*
  [@inheritDoc]
*}
constructor TMessageAction.Create;
begin
  inherited;

  SetKind(mkMessage);
end;

{*
  [@inheritDoc]
*}
destructor TMessageAction.Destroy;
begin
  inherited;
end;

{*
  Modifie le type de message
  @param Value   Nouveau type de message
*}
procedure TMessageAction.SetKind(Value: TSimpleMessageKind);
const
  DefaultType: array[TSimpleMessageKind] of TDialogType = (
    dtInformation, dtWarning, dtError, dtInformation, dtError, dtInformation
  );
begin
  FKind := Value;

  if Kind <> mkCustom then
  begin
    case Kind of
      mkMessage:    FDialogTitle := sMessage;
      mkTip:        FDialogTitle := sTip;
      mkBlindAlley: FDialogTitle := sBlindAlley;
      mkWon:        FDialogTitle := sWon;
      mkLost:       FDialogTitle := sLost;
    end;

    FDialogType := DefaultType[Kind];
  end;
end;

{*
  Modifie le titre du message
  @param Value   Nouveau titre du message
*}
procedure TMessageAction.SetDialogTitle(const Value: string);
begin
  FKind := mkCustom;
  FDialogTitle := Value;
end;

{*
  Modifie le type de bo�te de dialogue
  @param Value   Nouveau type de bo�te de dialogue
*}
procedure TMessageAction.SetDialogType(Value: TDialogType);
begin
  FKind := mkCustom;
  FDialogType := Value;
end;

{*
  [@inheritDoc]
*}
procedure TMessageAction.Save(Stream: TStream);
begin
  inherited;

  Stream.WriteBuffer(FKind, SizeOf(TSimpleMessageKind));

  if Kind = mkCustom then
  begin
    WriteStrToStream(Stream, FDialogTitle);
    Stream.WriteBuffer(FDialogType, SizeOf(TDialogType));
  end;

  WriteStrToStream(Stream, FText);

  Stream.WriteBuffer(FOnlyFirstTime, SizeOf(Boolean));
end;

{*
  [@inheritDoc]
*}
function TMessageAction.GetTitle: string;
begin
  Result := Format(SMessageActionTitle, [DialogTitle]);
end;

{*
  [@inheritDoc]
*}
procedure TMessageAction.ProduceDelphiCode(Code: TStrings;
  const Indent: string);
var
  NewIndent: string;
begin
  if OnlyFirstTime then
  begin
    Code.Add(Indent + 'if IsFirstTime(Player) then');
    NewIndent := Indent + DefaultIndent;
  end else
    NewIndent := Indent;

  Code.Add(NewIndent + Format('Player.ShowDialog(%s,',
    [StrToStrRepres(DialogTitle)]));
  Code.Add(NewIndent + Format('  %s,', [StrToStrRepres(Text)]));
  Code.Add(NewIndent + Format('  %s, dbOK, 1, 0);',
    [GetEnumName(TypeInfo(TDialogType), Ord(DialogType))]));
end;

initialization
  RegisterClasses([
    TReplaceSquareAction, TMessageAction
  ]);
finalization
  UnRegisterClasses([
    TReplaceSquareAction, TMessageAction
  ]);
end.

