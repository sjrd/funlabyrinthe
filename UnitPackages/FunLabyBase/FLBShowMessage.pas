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
  Windows, SysUtils, Classes, Graphics, StrUtils, SyncObjs, ScUtils, ScStrUtils,
  FunLabyUtils;

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
    procedure ShowMessage(Player: TPlayer;
      const Text: string); virtual; abstract;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
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
    FMaxLineCount: Integer; /// Nombre de lignes à afficher maximum

    FWorkBitmap: TBitmap; /// Bitmap de travail
    FWorkCanvas: TCanvas; /// Canevas de travail

    FMessageRect: TRect;    /// Rectangle où afficher le message
    FLines: TStrings;       /// Lignes à afficher
    FCurrentIndex: Integer; /// Index courant dans les lignes à afficher
  public
    constructor Create(AComponent: TFunLabyComponent;
      APlayer: TPlayer); override;
    destructor Destroy; override;

    procedure Activate;
    procedure NextLines;
    procedure Deactivate;

    property Activated: Boolean read FActivated;

    property Padding: TPoint read FPadding write FPadding;
    property MaxLineCount: Integer read FMaxLineCount write FMaxLineCount;

    property WorkCanvas: TCanvas read FWorkCanvas;

    property MessageRect: TRect read FMessageRect write FMessageRect;
    property Lines: TStrings read FLines;
    property CurrentIndex: Integer read FCurrentIndex;
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

    procedure DrawBorder(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure DrawText(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;
    procedure DrawContinueSymbol(Context: TDrawViewContext;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;

    procedure WaitForContinueKey(Player: TPlayer;
      PlayerData: TDefaultShowMessagePluginPlayerData); virtual;

    procedure ShowMessage(Player: TPlayer; const Text: string); override;
  public
    procedure DrawView(Context: TDrawViewContext); override;
  end;

implementation

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

  if Msg.Text <> '' then
    ShowMessage(Msg.Player, Msg.Text);
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
end;

{*
  [@inheritDoc]
*}
destructor TDefaultShowMessagePluginPlayerData.Destroy;
begin
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
  FActivated := True;
end;

{*
  Passe aux lignes suivantes
*}
procedure TDefaultShowMessagePluginPlayerData.NextLines;
begin
  Inc(FCurrentIndex, MaxLineCount);
  if FCurrentIndex >= Lines.Count then
    Deactivate;
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
  PlayerData.Padding := Point(10, 4);
  PlayerData.MaxLineCount := 2;
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
  BreakChars: TSysCharSet = [#9, #10, #13, ' '];
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

      if (Index >= Length(Text)) or (Text[Index] in [#10, #13]) or
        (CurWidth > MaxLineWidth) then
      begin
        Index := LastGoodIndex;
        Lines.Add(Copy(Text, LineBeginIndex, Index-LineBeginIndex));

        LineBeginIndex := Index+1;
        LastGoodIndex := 0;
      end;

      Inc(Index);
    end;
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
  with Context.Canvas do
  begin
    BorderRect := Context.ViewRect;
    BorderRect.Top := BorderRect.Bottom - 40;

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
    Canvas.Brush.Style := bsClear;
    SetupFont(PlayerData, Canvas.Font);

    // Draw lines
    for I := CurrentIndex to CurrentIndex+MaxLineCount-1 do
    begin
      if I >= Lines.Count then
        Break;
      Canvas.TextOut(TextPos.X, TextPos.Y, Lines[I]);
      Inc(TextPos.Y, Canvas.TextHeight('A'));
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
begin
  with Context do
  begin
    if (TickCount mod 1200) < 600 then
      Exit;

    SymbolPos := ViewRect.BottomRight;
    Dec(SymbolPos.X, 9);
    Dec(SymbolPos.Y, 9);

    with Canvas, SymbolPos do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      Pen.Style := psClear;

      Polygon([Point(X-3, Y-3), Point(X+3, Y-3), Point(X, Y+4)]);
    end;
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
  [@inheritDoc]
*}
procedure TDefaultShowMessagePlugin.ShowMessage(Player: TPlayer;
  const Text: string);
var
  PlayerData: TDefaultShowMessagePluginPlayerData;
  ViewSize: TPoint;
begin
  PlayerData := TDefaultShowMessagePluginPlayerData(GetPlayerData(Player));

  with PlayerData do
  begin
    // Setup
    UpdateMeasures(PlayerData);
    SetupFont(PlayerData, WorkCanvas.Font);

    // Fetch view size
    with Player.Mode do
      ViewSize := Point(Width, Height);

    // Build message rect
    MessageRect := Rect(
      0, ViewSize.Y - 2*Padding.Y - MaxLineCount*WorkCanvas.TextHeight('A'),
      ViewSize.X, ViewSize.Y);

    // Prepare lines
    Lines.Clear;
    PrepareLines(PlayerData, AnsiReplaceStr(Text, #13#10, #10));
    if Lines.Count = 0 then
      Exit;

    // Show message box
    Activate;
    repeat
      WaitForContinueKey(Player, PlayerData);
      NextLines;
    until not Activated;
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
  DrawContinueSymbol(Context, PlayerData);
end;

end.

