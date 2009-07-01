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
    FPlayerTexts: TStrings;     /// Textes courants pour chaque joueur
    FTextsCS: TCriticalSection; /// Section critique pour l'accès à FPlayerTexts

    procedure MsgShowMessageHandler(
      var Msg: TPlayerShowMsgMessage); message msgShowMessage;
  protected
    function GetCurrentText(Player: TPlayer): string;
    procedure SetCurrentText(Player: TPlayer; const Value: string);

    procedure ShowMessage(Player: TPlayer;
      const Text: string); virtual; abstract;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID);
    destructor Destroy; override;
  end;

  {*
    Plug-in par défaut pour la prise en charge de l'affichage des messages
    @author sjrd
    @version 5.0
  *}
  TDefaultShowMessagePlugin = class(TCustomShowMessagePlugin)
  protected
    procedure SetupFont(Player: TPlayer; Font: TFont); virtual;
    procedure Measure(Player: TPlayer; out Padding: TPoint;
      out CharWidth, LineCount: Integer); virtual;
    procedure DrawBorder(Context: TDrawViewContext); virtual;
    procedure DrawText(Context: TDrawViewContext; const Text: string); virtual;
    procedure DrawContinueSymbol(Context: TDrawViewContext); virtual;
    procedure WaitForContinueKey(Player: TPlayer); virtual;

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

  FPlayerTexts := TStringList.Create;
  FTextsCS := TCriticalSection.Create;
end;

{*
  [@inheritDoc]
*}
destructor TCustomShowMessagePlugin.Destroy;
begin
  FTextsCS.Free;
  FPlayerTexts.Free;

  inherited;
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

{*
  Obtient le texte actuellement affiché pour un joueur donné
  @param Player   Joueur concerné
  @return Texte à afficher pour ce joueur
*}
function TCustomShowMessagePlugin.GetCurrentText(Player: TPlayer): string;
var
  Index: Integer;
begin
  FTextsCS.Acquire;
  try
    Index := FPlayerTexts.IndexOfObject(Player);
    if Index >= 0 then
      Result := FPlayerTexts[Index]
    else
      Result := '';
  finally
    FTextsCS.Release;
  end;
end;

{*
  Modifie le texte à afficher pour un joueur donné
  @param Player   Joueur concerné
  @param Value    Texte à afficher pour ce joueur
*}
procedure TCustomShowMessagePlugin.SetCurrentText(Player: TPlayer;
  const Value: string);
var
  Index: Integer;
begin
  FTextsCS.Acquire;
  try
    Index := FPlayerTexts.IndexOfObject(Player);

    if Index >= 0 then
    begin
      if Value = '' then
        FPlayerTexts.Delete(Index)
      else
        FPlayerTexts[Index] := Value;
    end else
    begin
      if Value <> '' then
        FPlayerTexts.AddObject(Value, Player);
    end;
  finally
    FTextsCS.Release;
  end;
end;

{---------------------------------}
{ TDefaultShowMessagePlugin class }
{---------------------------------}

{*
  Configure la police du texte
  @param Player    Joueur pour qui afficher un message
  @param Font      Police à configurer
*}
procedure TDefaultShowMessagePlugin.SetupFont(Player: TPlayer; Font: TFont);
begin
  Font.Name := 'Courier New';
  Font.Size := 10;
  Font.Color := clBlack;
end;

{*
  Mesure les paramètres d'affichage du texte
  @param Player      Joueur pour qui afficher un message
  @param Padding     En sortie : padding requis par la bordure
  @param CharWidth   En sortie : largeur d'un caractère
  @param LineCount   En sortie : nombre de lignes affichées
*}
procedure TDefaultShowMessagePlugin.Measure(Player: TPlayer;
  out Padding: TPoint; out CharWidth, LineCount: Integer);
begin
  Padding.X := 10;
  Padding.Y := 4;
  CharWidth := 8;
  LineCount := 2;
end;

{*
  Dessine la bordure (et le fond du texte)
  @param Context   Contexte d'affiche de la vue
*}
procedure TDefaultShowMessagePlugin.DrawBorder(Context: TDrawViewContext);
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
  @param Context   Contexte d'affiche de la vue
  @param Text      Texte à afficher
*}
procedure TDefaultShowMessagePlugin.DrawText(Context: TDrawViewContext;
  const Text: string);
var
  TextPos: TPoint;
  Dummy1, Dummy2: Integer;
  Line, RemainingText, Temp: string;
begin
  with Context do
  begin
    // First text pos
    Measure(Player, TextPos, Dummy1, Dummy2);
    TextPos.Y := ViewRect.Bottom - 40 + TextPos.Y;

    // Setup font
    Canvas.Brush.Style := bsClear;
    SetupFont(Player, Canvas.Font);

    // Draw lines
    RemainingText := Text;
    while RemainingText <> '' do
    begin
      SplitToken(RemainingText, #10, Line, Temp);
      RemainingText := Temp;

      Canvas.TextOut(TextPos.X, TextPos.Y, Line);
      Inc(TextPos.Y, Canvas.TextHeight('A'));
    end;
  end;
end;

{*
  Dessine le symbole de continuation
  @param Context   Contexte du dessin de la vue
*}
procedure TDefaultShowMessagePlugin.DrawContinueSymbol(
  Context: TDrawViewContext);
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
  @param Player   Joueur qui doit appuyer sur une touche de continuation
*}
procedure TDefaultShowMessagePlugin.WaitForContinueKey(Player: TPlayer);
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
  Padding: TPoint;
  CharWidth, LineCount: Integer;
  LineWidth, MaxCol, Index, I: Integer;
  WrappedText: string;
begin
  Measure(Player, Padding, CharWidth, LineCount);

  LineWidth := Player.Mode.Width - 2*Padding.X;
  MaxCol := LineWidth div CharWidth;

  WrappedText := WrapText(Text, #10, [' ', '-', #9], MaxCol);

  while WrappedText <> '' do
  begin
    Index := 0;
    for I := 1 to LineCount do
    begin
      Index := PosEx(#10, WrappedText, Index+1);
      if Index = 0 then
        Break;
    end;

    if Index = 0 then
    begin
      SetCurrentText(Player, WrappedText);
      WrappedText := '';
    end else
    begin
      SetCurrentText(Player, Copy(WrappedText, 1, Index-1));
      Delete(WrappedText, 1, Index);
    end;

    WaitForContinueKey(Player);
  end;

  SetCurrentText(Player, '');
end;

{*
  [@inheritDoc]
*}
procedure TDefaultShowMessagePlugin.DrawView(Context: TDrawViewContext);
var
  Text: string;
begin
  Text := GetCurrentText(Context.Player);
  if Text = '' then
    Exit;

  DrawBorder(Context);
  DrawText(Context, Text);
  DrawContinueSymbol(Context);
end;

end.

