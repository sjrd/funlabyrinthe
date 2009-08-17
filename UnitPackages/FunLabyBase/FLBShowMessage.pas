{*
  Plug-in de prise en charge d'affichage des messages
  L'unité FLBShowMessage propose des classes de plug-in prenant en charge
  l'affichage de messages au joueur (via la méthode TPlayer.ShowMessage).
  @author sjrd
  @version 5.0
*}
unit FLBShowMessage;

interface

uses
  Types, Windows, SysUtils, Classes, Graphics, StrUtils, SyncObjs, ScUtils,
  ScStrUtils, FunLabyUtils, GR32, GR32_Polygons;

const {don't localize}
  /// ID du plug-in d'affichage de message par défaut
  idDefaultShowMessagePlugin = 'DefaultShowMessagePlugin';

type
  {*
    Classe de base pour les plug-in prenant en charge l'affichage de messages
    @author sjrd
    @version 5.0
  *}
  TCustomShowMessagePlugin = class(TPlugin)
  private
    procedure MsgShowMessageHandler(
      var Msg: TPlayerShowMsgMessage); message msgShowMessage;
  protected
    procedure ShowMessage(
      var Context: TPlayerShowMsgMessage); virtual; abstract;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  end;

  {*
    Données liées au joueur pour un TDefaultShowMessagePlugin
    @author sjrd
    @version 1.0
  *}
  TDefaultShowMessagePluginPlayerData = class(TPlayerData)
  private
    FActivated: Boolean; /// Indique si la boîte de message est activée

    FPadding: TPoint;       /// Marges de la boîte de message (bordure comprise)
    FMinLineCount: Integer; /// Nombre minimum de lignes à afficher
    FMaxLineCount: Integer; /// Nombre maximum de lignes à afficher

    FSelBulletWidth: Integer; /// Largeur du bullet indiquant la sélection
    FColSepWidth: Integer;    /// Largeur de la séparation entre colonnes

    FWorkBitmap: TBitmap; /// Bitmap de travail
    FWorkCanvas: TCanvas; /// Canevas de travail

    FMessageRect: TRect;    /// Rectangle où afficher le message
    FLines: TStrings;       /// Lignes à afficher
    FCurrentIndex: Integer; /// Index courant dans les lignes à afficher

    FShowAnswers: Boolean;      /// Indique si il faut afficher les réponses
    FAnswers: TStrings;         /// Réponses possibles
    FAnswerColCount: Integer;   /// Nombre de colonnes de réponses
    FAnswerRowCount: Integer;   /// Nombre de lignes de réponses
    FSelected: Integer;         /// Index de la réponse sélectionnée
    FShowOnlySelected: Boolean; /// Si True, affiche uniquement la sélection
  public
    constructor Create(AComponent: TFunLabyComponent;
      APlayer: TPlayer); override;
    destructor Destroy; override;

    procedure Activate;
    procedure NextLines;
    procedure Deactivate;

    property Activated: Boolean read FActivated;

    property Padding: TPoint read FPadding write FPadding;
    property MinLineCount: Integer read FMinLineCount write FMinLineCount;
    property MaxLineCount: Integer read FMaxLineCount write FMaxLineCount;

    property SelBulletWidth: Integer read FSelBulletWidth write FSelBulletWidth;
    property ColSepWidth: Integer read FColSepWidth write FColSepWidth;

    property WorkCanvas: TCanvas read FWorkCanvas;

    property MessageRect: TRect read FMessageRect write FMessageRect;
    property Lines: TStrings read FLines;
    property CurrentIndex: Integer read FCurrentIndex;

    property ShowAnswers: Boolean read FShowAnswers write FShowAnswers;
    property Answers: TStrings read FAnswers;
    property AnswerColCount: Integer read FAnswerColCount write FAnswerColCount;
    property AnswerRowCount: Integer read FAnswerRowCount write FAnswerRowCount;
    property Selected: Integer read FSelected write FSelected;
    property ShowOnlySelected: Boolean
      read FShowOnlySelected write FShowOnlySelected;
  end;

  {*
    Plug-in par défaut pour la prise en charge de l'affichage des messages
    @author sjrd
    @version 5.0
  *}
  TDefaultShowMessagePlugin = class(TCustomShowMessagePlugin)
  protected
    class function GetPlayerDataClass: TPlayerDataClass; override;

    procedure UpdateMeasures(
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure SetupFont(PlayerData: TDefaultShowMessagePluginPlayerData;
      Font: TFont); virtual;

    procedure PrepareLines(
      PlayerData: TDefaultShowMessagePluginPlayerData;
      const Text: string); virtual;
    procedure PrepareAnswers(
      PlayerData: TDefaultShowMessagePluginPlayerData;
      const Answers: TStringDynArray); virtual;

    procedure DrawBorder(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure DrawText(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure DrawAnswers(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure DrawContinueSymbol(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure DrawSelectionBullet(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData;
      BulletPos: TPoint); virtual;

    procedure WaitForContinueKey(Player: TPlayer;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    function WaitForSelectionKey(Player: TPlayer;
      PlayerData: TDefaultShowMessagePluginPlayerData): TDirection; virtual;

    procedure ApplySelectionDirection(
      PlayerData: TDefaultShowMessagePluginPlayerData;
      Direction: TDirection); virtual;

    procedure ShowMessage(var Context: TPlayerShowMsgMessage); override;
  public
    procedure DrawView(Context: TDrawViewContext); override;
  end;

implementation

{*
  Division entière arrondie à l'unité supérieure
  @param Dividand   Dividande
  @param Divisor    Diviseur
  @return Dividand div Divisor arrondi à l'unité supérieure
*}
function DivRoundUp(Dividand, Divisor: Integer): Integer;
begin
  Result := Dividand div Divisor;
  if Result * Divisor < Dividand then
    Inc(Result);
end;

{--------------------------------}
{ TCustomShowMessagePlugin class }
{--------------------------------}

{*
  Crée le plug-in
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du plug-in
*}
constructor TCustomShowMessagePlugin.Create(AMaster: TMaster;
  const AID: TComponentID);
begin
  inherited Create(AMaster, AID);

  FZIndex := 1024;
end;

{*
  Gestionnaire du message Afficher un message
  @param Msg   Message
*}
procedure TCustomShowMessagePlugin.MsgShowMessageHandler(
  var Msg: TPlayerShowMsgMessage);
begin
  Msg.Handled := True;

  if (Msg.Text <> '') or (Length(Msg.Answers) > 0) then
    ShowMessage(Msg);
end;

{-------------------------------------------}
{ TDefaultShowMessagePluginPlayerData class }
{-------------------------------------------}

{*
  Crée les données du joueur
  @param AComponent   Composant propriétaire
  @param APlayer      Joueur
*}
constructor TDefaultShowMessagePluginPlayerData.Create(
  AComponent: TFunLabyComponent; APlayer: TPlayer);
begin
  inherited;

  FWorkBitmap := TBitmap.Create;
  FWorkCanvas := FWorkBitmap.Canvas;

  FLines := TStringList.Create;
  FAnswers := TStringList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TDefaultShowMessagePluginPlayerData.Destroy;
begin
  FAnswers.Free;
  FLines.Free;

  FWorkBitmap.Free;

  inherited;
end;

{*
  Active l'affichage du message préparé
*}
procedure TDefaultShowMessagePluginPlayerData.Activate;
begin
  FCurrentIndex := 0;
  FShowAnswers := False;
  FActivated := True;
end;

{*
  Passe aux lignes suivantes
*}
procedure TDefaultShowMessagePluginPlayerData.NextLines;
begin
  Inc(FCurrentIndex, MaxLineCount);
end;

{*
  Désactive l'affichage du message
*}
procedure TDefaultShowMessagePluginPlayerData.Deactivate;
begin
  FActivated := False;
end;

{---------------------------------}
{ TDefaultShowMessagePlugin class }
{---------------------------------}

{*
  [@inheritDoc]
*}
class function TDefaultShowMessagePlugin.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TDefaultShowMessagePluginPlayerData;
end;

{*
  Met à jour les mesures d'affichage du texte
  @param PlayerData   Données du joueur
*}
procedure TDefaultShowMessagePlugin.UpdateMeasures(
  PlayerData: TDefaultShowMessagePluginPlayerData);
begin
  with PlayerData do
  begin
    Padding := Point(10, 4);
    MinLineCount := 2;
    MaxLineCount := 3;

    SelBulletWidth := 15;
    ColSepWidth := 15;
  end;
end;

{*
  Configure la police du texte
  @param PlayerData   Données du joueur
  @param Font         Police à configurer
*}
procedure TDefaultShowMessagePlugin.SetupFont(
  PlayerData: TDefaultShowMessagePluginPlayerData; Font: TFont);
begin
  Font.Name := 'Courier New';
  Font.Size := 10;
  Font.Color := clBlack;
end;

{*
  Prépare les lignes de textes à afficher
  @param PlayerData   Données du joueur
  @param Text         Texte linéaire
*}
procedure TDefaultShowMessagePlugin.PrepareLines(
  PlayerData: TDefaultShowMessagePluginPlayerData; const Text: string);
const
  BreakChars: TSysCharSet = [#9..#13, ' '];
var
  MaxLineWidth, LineBeginIndex, LastGoodIndex, Index, CurWidth: Integer;
begin
  with PlayerData do
  begin
    MaxLineWidth := MessageRect.Right - MessageRect.Left - 2*Padding.X;

    LineBeginIndex := 1;
    LastGoodIndex := 0;
    Index := 1;
    while Index <= Length(Text) do
    begin
      while (Index <= Length(Text)) and (not (Text[Index] in BreakChars)) do
        Inc(Index);

      CurWidth := WorkCanvas.TextWidth(Copy(
        Text, LineBeginIndex, Index-LineBeginIndex));

      if (CurWidth <= MaxLineWidth) or (LastGoodIndex = 0) then
        LastGoodIndex := Index;

      if (Index >= Length(Text)) or (Text[Index] in [#10..#13]) or
        (CurWidth > MaxLineWidth) then
      begin
        Index := LastGoodIndex;
        Lines.Add(Copy(Text, LineBeginIndex, Index-LineBeginIndex));

        LineBeginIndex := Index+1;
        LastGoodIndex := 0;
      end;

      if Text[Index] in [#11, #12] then
        while Lines.Count mod MaxLineCount <> 0 do
          Lines.Add('');

      Inc(Index);
    end;
  end;
end;

{*
  Prépare les réponses possibles à afficher
  @param PlayerData   Données du joueur
  @param Text         Texte linéaire
*}
procedure TDefaultShowMessagePlugin.PrepareAnswers(
  PlayerData: TDefaultShowMessagePluginPlayerData;
  const Answers: TStringDynArray);
var
  I, MaxAnswerWidth, AnswerWidth, MaxLineWidth: Integer;
begin
  for I := 0 to Length(Answers)-1 do
    PlayerData.Answers.Add(Answers[I]);

  with PlayerData do
  begin
    if ShowOnlySelected then
    begin
      AnswerColCount := 1;
      AnswerRowCount := Answers.Count;
      Exit;
    end;

    MaxAnswerWidth := 0;
    for I := 0 to Answers.Count-1 do
    begin
      AnswerWidth := WorkCanvas.TextWidth(Answers[I]);
      if AnswerWidth > MaxAnswerWidth then
        MaxAnswerWidth := AnswerWidth;
    end;

    MaxLineWidth := MessageRect.Right - MessageRect.Left - 2*Padding.X;
    AnswerColCount := (MaxLineWidth + ColSepWidth) div
      (SelBulletWidth + MaxAnswerWidth + ColSepWidth);
    if AnswerColCount = 0 then
      AnswerColCount := 1;

    AnswerRowCount := DivRoundUp(Answers.Count, AnswerColCount);

    while DivRoundUp(Answers.Count, AnswerColCount-1) = AnswerRowCount do
      AnswerColCount := AnswerColCount-1;
  end;
end;

{*
  Dessine la bordure (et le fond du texte)
  @param Context      Contexte d'affiche de la vue
  @param PlayerData   Données du joueur
*}
procedure TDefaultShowMessagePlugin.DrawBorder(Context: TDrawViewContext;
  PlayerData: TDefaultShowMessagePluginPlayerData);
var
  BorderRect: TRect;
begin
  with Context, Bitmap.Canvas do
  begin
    BorderRect := PlayerData.MessageRect;

    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    Pen.Width := 3;
    with BorderRect do
      RoundRect(Left, Top, Right, Bottom, 7, 4);
    Pen.Width := 1;
  end;
end;

{*
  Dessine le texte
  @param Context      Contexte d'affiche de la vue
  @param PlayerData   Données du joueur
*}
procedure TDefaultShowMessagePlugin.DrawText(Context: TDrawViewContext;
  PlayerData: TDefaultShowMessagePluginPlayerData);
var
  TextPos: TPoint;
  I: Integer;
begin
  with Context, PlayerData do
  begin
    // First text pos
    TextPos.X := MessageRect.Left + Padding.X;
    TextPos.Y := MessageRect.Top + Padding.Y;

    // Setup font
    SetupFont(PlayerData, Bitmap.Font);

    // Draw lines
    for I := CurrentIndex to CurrentIndex+MaxLineCount-1 do
    begin
      if I >= Lines.Count then
        Break;
      Bitmap.Textout(TextPos.X, TextPos.Y, Lines[I]);
      Inc(TextPos.Y, Bitmap.TextHeight('A'));
    end;
  end;
end;

{*
  Dessine les réponses possibles
  @param Context      Contexte d'affiche de la vue
  @param PlayerData   Données du joueur
*}
procedure TDefaultShowMessagePlugin.DrawAnswers(Context: TDrawViewContext;
  PlayerData: TDefaultShowMessagePluginPlayerData);
var
  TextPos, SelPoint: TPoint;
  MaxLineWidth, ColWidth, LineHeight: Integer;
  MinAnswerRow, MaxAnswerRow, Row, Col, Index: Integer;
begin
  with Context, PlayerData do
  begin
    // First text pos
    TextPos.Y := MessageRect.Top + Padding.Y;

    // Setup font
    SetupFont(PlayerData, Bitmap.Font);

    // Some measures
    MaxLineWidth := MessageRect.Right - MessageRect.Left - 2*Padding.X;
    ColWidth := (MaxLineWidth + ColSepWidth) div AnswerColCount;
    LineHeight := Bitmap.TextHeight('A');

    // Skip text lines
    if CurrentIndex < Lines.Count then
      Inc(TextPos.Y, (Lines.Count-CurrentIndex) * LineHeight);

    // Compute SelPoint
    SelPoint.X := Selected mod AnswerColCount;
    SelPoint.Y := Selected div AnswerColCount;

    // Compute MinAnswerRow and MaxAnswerRow
    if ShowOnlySelected then
    begin
      MinAnswerRow := SelPoint.Y;
      MaxAnswerRow := SelPoint.Y+1;
    end else
    begin
      MinAnswerRow := SelPoint.Y div MaxLineCount;
      MaxAnswerRow := MinAnswerRow + MaxLineCount;
      if MaxAnswerRow > AnswerRowCount then
        MaxAnswerRow := AnswerRowCount;
    end;

    // Draw answers
    Index := MinAnswerRow * AnswerColCount;
    for Row := MinAnswerRow to MaxAnswerRow-1 do
    begin
      TextPos.X := MessageRect.Left + Padding.X;

      for Col := 0 to AnswerColCount-1 do
      begin
        if Index >= Answers.Count then
          Break;

        if Index = Selected then
          DrawSelectionBullet(Context, PlayerData, TextPos);

        Bitmap.Textout(TextPos.X + SelBulletWidth, TextPos.Y, Answers[Index]);

        Inc(TextPos.X, ColWidth);
        Inc(Index);
      end;

      Inc(TextPos.Y, LineHeight);
    end;
  end;
end;

{*
  Dessine le symbole de continuation
  @param Context      Contexte du dessin de la vue
  @param PlayerData   Données du joueur
*}
procedure TDefaultShowMessagePlugin.DrawContinueSymbol(
  Context: TDrawViewContext; PlayerData: TDefaultShowMessagePluginPlayerData);
var
  SymbolPos: TPoint;
  Points: TArrayOfFixedPoint;
begin
  with Context do
  begin
    if (TickCount mod 1200) < 600 then
      Exit;

    SymbolPos := ViewRect.BottomRight;
    Dec(SymbolPos.X, 9);
    Dec(SymbolPos.Y, 9);

    with SymbolPos do
    begin
      SetLength(Points, 3);
      Points[0] := FixedPoint(X-3, Y-3);
      Points[1] := FixedPoint(X+3, Y-3);
      Points[2] := FixedPoint(X, Y+4);

      PolygonTS(Bitmap, Points, clBlack32);
    end;
  end;
end;

{*
  Dessine le bullet de sélection
  @param Context      Contexte du dessin de la vue
  @param PlayerData   Données du joueur
  @param BulletPos    Position du bullet
*}
procedure TDefaultShowMessagePlugin.DrawSelectionBullet(
  Context: TDrawViewContext; PlayerData: TDefaultShowMessagePluginPlayerData;
  BulletPos: TPoint);
begin
  with Context.Bitmap.Canvas, BulletPos do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    Pen.Style := psClear;

    Ellipse(X+2, Y+5, X+10, Y+13);
  end;
end;

{*
  Bloque jusqu'à l'appui d'une touche de continuation
  @param Player       Joueur qui doit appuyer sur une touche de continuation
  @param PlayerData   Données du joueur
*}
procedure TDefaultShowMessagePlugin.WaitForContinueKey(Player: TPlayer;
  PlayerData: TDefaultShowMessagePluginPlayerData);
var
  Key: Word;
  Shift: TShiftState;
begin
  repeat
    Player.WaitForKey(Key, Shift);
  until (Shift = []) and ((Key = VK_RETURN) or (Key = VK_DOWN));
end;

{*
  Bloque jusqu'à l'appui d'une touche de modification de la sélection
  @param Player       Joueur qui doit appuyer sur une touche de sélection
  @param PlayerData   Données du joueur
*}
function TDefaultShowMessagePlugin.WaitForSelectionKey(Player: TPlayer;
  PlayerData: TDefaultShowMessagePluginPlayerData): TDirection;
const
  SelectionKeys: TSysByteSet = [
    VK_RETURN, VK_UP, VK_RIGHT, VK_DOWN, VK_LEFT
  ];
var
  Key: Word;
  Shift: TShiftState;
begin
  repeat
    Player.WaitForKey(Key, Shift);
  until (Shift = []) and (Key < $100) and (Byte(Key) in SelectionKeys);

  case Key of
    VK_UP:
      Result := diNorth;
    VK_RIGHT:
      Result := diEast;
    VK_DOWN:
      Result := diSouth;
    VK_LEFT:
      Result := diWest;
  else
    Result := diNone;
  end;
end;

{*
  Applique une direction à la sélection
  @param PlayerData   Données du joueur
  @param Direction    Direction à appliquer
*}
procedure TDefaultShowMessagePlugin.ApplySelectionDirection(
  PlayerData: TDefaultShowMessagePluginPlayerData; Direction: TDirection);
var
  SelPoint: TPoint;
  NewSelected: Integer;
begin
  with PlayerData do
  begin
    SelPoint.X := Selected mod AnswerColCount;
    SelPoint.Y := Selected div AnswerColCount;

    case Direction of
      diNorth:
        Dec(SelPoint.Y);
      diEast:
        Inc(SelPoint.X);
      diSouth:
        Inc(SelPoint.Y);
      diWest:
        Dec(SelPoint.X);
    end;

    if (SelPoint.X < 0) or (SelPoint.X >= AnswerColCount) then
      Exit;

    NewSelected := SelPoint.Y*AnswerColCount + SelPoint.X;

    if (NewSelected >= 0) and (NewSelected < Answers.Count) then
      Selected := NewSelected;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TDefaultShowMessagePlugin.ShowMessage(
  var Context: TPlayerShowMsgMessage);
var
  PlayerData: TDefaultShowMessagePluginPlayerData;
  ViewSize: TPoint;
  NeededLineCount: Integer;
  Direction: TDirection;
begin
  PlayerData := TDefaultShowMessagePluginPlayerData(
    GetPlayerData(Context.Player));

  with PlayerData do
  begin
    // Setup
    UpdateMeasures(PlayerData);
    SetupFont(PlayerData, WorkCanvas.Font);
    ShowOnlySelected := Context.ShowOnlySelected;

    // Fetch view size
    with Player.Mode do
      ViewSize := Point(Width, Height);

    // Build message rect
    MessageRect := Rect(
      0, ViewSize.Y - 2*Padding.Y - MaxLineCount*WorkCanvas.TextHeight('A'),
      ViewSize.X, ViewSize.Y);

    // Prepare lines
    Lines.Clear;
    PrepareLines(PlayerData, AnsiReplaceStr(Context.Text, #13#10, #10));
    if (Lines.Count = 0) and (Length(Context.Answers) = 0) then
      Exit;

    // Prepare answers
    Selected := Context.Selected;
    Answers.Clear;
    if Length(Context.Answers) > 0 then
      PrepareAnswers(PlayerData, Context.Answers)
    else
      AnswerRowCount := 0;

    // Don't show a big box for a small message
    NeededLineCount := Lines.Count + AnswerRowCount;
    if NeededLineCount < MaxLineCount then
    begin
      if NeededLineCount > MinLineCount then
        MaxLineCount := NeededLineCount
      else
        MaxLineCount := MinLineCount;

      MessageRect := Rect(MessageRect.Left,
        ViewSize.Y - 2*Padding.Y - MaxLineCount*WorkCanvas.TextHeight('A'),
        MessageRect.Right, MessageRect.Bottom);
    end;

    // Show message
    Activate;
    repeat
      if AnswerRowCount <> 0 then
      begin
        if ShowOnlySelected and (Lines.Count-CurrentIndex < MaxLineCount) then
          Break;
        if Lines.Count-CurrentIndex + AnswerRowCount <= MaxLineCount then
          Break;
      end;

      WaitForContinueKey(Player, PlayerData);
      NextLines;
    until CurrentIndex >= Lines.Count;

    // Show answers
    if AnswerRowCount <> 0 then
    begin
      ShowAnswers := True;
      repeat
        Direction := WaitForSelectionKey(Player, PlayerData);
        if Direction <> diNone then
          ApplySelectionDirection(PlayerData, Direction);
      until Direction = diNone;
    end;

    // Finalization
    Deactivate;
    Context.Selected := Selected;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TDefaultShowMessagePlugin.DrawView(Context: TDrawViewContext);
var
  PlayerData: TDefaultShowMessagePluginPlayerData;
begin
  PlayerData := TDefaultShowMessagePluginPlayerData(
    GetPlayerData(Context.Player));
  if not PlayerData.Activated then
    Exit;

  DrawBorder(Context, PlayerData);
  DrawText(Context, PlayerData);

  if PlayerData.ShowAnswers then
    DrawAnswers(Context, PlayerData)
  else
    DrawContinueSymbol(Context, PlayerData);
end;

end.

