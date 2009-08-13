{*
  Décrit le comportement complet de la planche
  L'unité FLBPlank regroupe tous les composants intervenant dans le
  fonctionnement de la planche.
  @author sjrd
  @version 5.0
*}
unit FLBPlank;

interface

uses
  SysUtils, Classes, Graphics, ScUtils, FunLabyUtils, Generics, FLBCommon, GR32;

const {don't localize}
  idPlankPlugin = 'PlankPlugin'; /// ID du plug-in planche

resourcestring
  sPlanks      = 'Planche';     /// Nom de l'objet planche
  sPlankInfos  = '%d planche';  /// Infos planches (singulier)
  sPlanksInfos = '%d planches'; /// Infos planches (pluriel)

  sPlank = 'Planche';           /// Nom de la planche

const {don't localize}
  idPlanks = 'Planks'; /// ID des planches

  idPlank = 'Plank';   /// ID de la planche

const {don't localize}
  fPlank = 'Objects/Plank'; /// Fichier de la planche

const {don't localize}
  attrUsePlank = 'UsePlank'; /// Attribut indiquant l'usage de la planche

resourcestring
  sFoundPlank = 'Tu as trouvé une planche.'+#10+
    'Tu peux franchir certains obstacles.';

type
  {*
    Plug-in planche
    Affiche une planche à côté du joueur ou sous celui-ci.
    @author sjrd
    @version 5.0
  *}
  TPlankPlugin = class(TPlugin)
  private
    FRequiredShift: TShiftState; /// État des touches spéciales requis
  public
    procedure DrawBefore(Context: TDrawSquareContext); override;

    procedure Moving(Context: TMoveContext); override;
  end;

  {*
    Définition de l'objet planche
    La planche permet de passer au-dessus des cases
    @author sjrd
    @version 5.0
  *}
  TPlanks = class(TObjectDef)
  private
    FRequiredShift: TShiftState; /// État des touches spéciales requis

    procedure SetRequiredShift(Value: TShiftState);
  protected
    procedure SetCount(Player: TPlayer; Value: Integer); override;

    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
  published
    property RequiredShift: TShiftState
      read FRequiredShift write SetRequiredShift default [];
  end;

  {*
    Case spéciale planche
    Cette case est utilisée pour le déplacement particulier de la planche.
    @author sjrd
    @version 5.0
  *}
  TPlankSquare = class(TOverriddenSquare)
  private
    FPlayer: TPlayer; /// Joueur qui passe sur la case
  public
    constructor Create(AMaster: TMaster; AMap: TMap;
      const APosition: T3DPoint; APlayer: TPlayer);

    procedure Entering(Context: TMoveContext); override;

    procedure Exited(Context: TMoveContext); override;

    procedure Execute(Context: TMoveContext); override;

    property Player: TPlayer read FPlayer;
  end;

const
  clPlank = $00004080; /// Couleur de la planche

implementation

{---------------------}
{ Classe TPlankPlugin }
{---------------------}

{*
  [@inheritDoc]
*}
procedure TPlankPlugin.DrawBefore(Context: TDrawSquareContext);
var
  X, Y: Integer;
  Player: TPlayer;
  PlankColor: TColor32;
begin
  inherited;

  X := Context.X;
  Y := Context.Y;
  Player := Context.Player;

  if Player.Attribute[attrUsePlank] = 0 then
    Exit;

  // Détermination de l'endroit où dessiner réellement la planche
  if not (Player.Map[Player.Position] is TPlankSquare) then
  begin
    case Player.Direction of
      diNorth: Dec(Y, SquareSize);
      diEast:  Inc(X, SquareSize);
      diSouth: Inc(Y, SquareSize);
      diWest:  Dec(X, SquareSize);
    end;
  end;

  // Dessin de la planche
  with Context.Bitmap do
  begin
    PlankColor := Color32(clPlank);

    if Player.Direction in [diNorth, diSouth] then
      FillRectS(X+6, Y-5, X+SquareSize-6, Y+SquareSize+5, PlankColor)
    else
      FillRectS(X-5, Y+6, X+SquareSize+5, Y+SquareSize-6, PlankColor);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TPlankPlugin.Moving(Context: TMoveContext);
var
  Behind: T3DPoint;
  Msg: TPlankMessage;
begin
  with Context do
  begin
    if (not Same3DPoint(PointBefore(Dest, Player.Direction), Src)) or
      (DestMap <> SrcMap) then
      Exit;

    if not (FRequiredShift <= Context.Shift) then
      Exit;

    Behind := PointBehind(Dest, Player.Direction);

    Msg.MsgID := msgPlank;
    Msg.Kind := pmkPassOver;
    Msg.Result := False;
    Msg.Player := Player;
    Msg.Pos := Dest;
    Msg.Src := Src;
    Msg.Dest := Behind;

    // On vérifie que la case du milieu peut être survolée
    DestSquare.Field.Dispatch(Msg);
    if not Msg.Result then
      Exit;

    // On vérifie que la case de départ ou d'arrivée autorise le déplacement
    Msg.Kind := pmkLeaveFrom;
    Msg.Result := False;
    SrcSquare.Field.Dispatch(Msg);
    if not Msg.Result then
    begin
      Msg.Kind := pmkArriveAt;
      Map[Msg.Dest].Field.Dispatch(Msg);
      if not Msg.Result then
        Exit;
    end;

    TPlankSquare.Create(Master, Map, Msg.Pos, Player);
    Master.Temporize;
  end;
end;

{----------------}
{ Classe TPlanks }
{----------------}

{*
  Crée une instance de TPlanks
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TPlanks.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fPlank);
end;

{*
  Modifie l'état des touches spéciales requis
*}
procedure TPlanks.SetRequiredShift(Value: TShiftState);
begin
  FRequiredShift := Value;
  (Master.Plugin[idPlankPlugin] as TPlankPlugin).FRequiredShift := Value;
end;

{*
  [@inheritDoc]
*}
procedure TPlanks.SetCount(Player: TPlayer; Value: Integer);
begin
  inherited;

  if Value > 0 then
    Player.AddPlugin(Master.Plugin[idPlankPlugin])
  else
    Player.RemovePlugin(Master.Plugin[idPlankPlugin]);
end;

{*
  [@inheritDoc]
*}
function TPlanks.GetShownInfos(Player: TPlayer): string;
var
  ACount: Integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sPlankInfos, [ACount])
  else
    Result := Format(sPlanksInfos, [ACount]);
end;

{--------------------}
{ Classe TPlankSquare }
{--------------------}

{*
  Crée une instance de TPlankSquare
  @param AMaster     Maître FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param APlayer     Joueur qui passe sur la case
*}
constructor TPlankSquare.Create(AMaster: TMaster; AMap: TMap;
  const APosition: T3DPoint; APlayer: TPlayer);
begin
  inherited Create(AMaster, '', AMap, APosition);
  FPlayer := APlayer;
  FPlayer.Attribute[attrUsePlank] := 1;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.Entering(Context: TMoveContext);
begin
  if Context.Player <> Player then
    Context.Cancel;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.Exited(Context: TMoveContext);
begin
  FPlayer.Attribute[attrUsePlank] := 0;
  Free;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.Execute(Context: TMoveContext);
var
  Redo: Boolean;
begin
  with Context do
  begin
    Master.Temporize;
    Player.MoveTo(PointBehind(Pos, Player.Direction), True, Redo);
    GoOnMoving := Redo;
  end;
end;

end.

