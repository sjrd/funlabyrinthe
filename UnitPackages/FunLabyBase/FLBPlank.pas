{*
  D�crit le comportement complet de la planche
  L'unit� FLBPlank regroupe tous les composants intervenant dans le
  fonctionnement de la planche.
  @author sjrd
  @version 5.0
*}
unit FLBPlank;

interface

uses
  SysUtils, Classes, Graphics, StrUtils, ScUtils, ScTypInfo,
  FunLabyUtils, Generics, FLBCommon, GR32;

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

resourcestring
  SFoundPlank = 'Tu as trouv� une planche.'+#10+
    'Tu peux franchir certains obstacles.';
  SPlankShiftExplanation = 'Appuie sur %s pour l''utiliser.';

type
  {*
    Plug-in planche
    Affiche une planche � c�t� du joueur ou sous celui-ci.
    @author sjrd
    @version 5.0
  *}
  TPlankPlugin = class(TPlugin)
  private
    FRequiredShift: TShiftState; /// �tat des touches sp�ciales requis
  public
    procedure DrawBefore(Context: TDrawSquareContext); override;

    procedure Moving(Context: TMoveContext); override;
  end;

  {*
    D�finition de l'objet planche
    La planche permet de passer au-dessus des cases
    @author sjrd
    @version 5.0
  *}
  TPlanks = class(TObjectDef)
  private
    FRequiredShift: TShiftState; /// �tat des touches sp�ciales requis

    procedure SetRequiredShift(Value: TShiftState);
  protected
    procedure SetCount(Player: TPlayer; Value: Integer); override;

    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  published
    property RequiredShift: TShiftState
      read FRequiredShift write SetRequiredShift default [];
  end;

  {*
    Outil planche
    @author sjrd
    @version 5.0
  *}
  TPlankTool = class(TObjectTool)
  public
    procedure Find(Context: TMoveContext); override;
  end;

  {*
    Case sp�ciale planche
    Cette case est utilis�e pour le d�placement particulier de la planche.
    @author sjrd
    @version 5.0
  *}
  TPlankSquare = class(TOverriddenSquare)
  private
    FPlayer: TPlayer; /// Joueur qui passe sur la case
  protected
    procedure DoEntering(Context: TMoveContext); override;
    procedure DoExited(Context: TMoveContext); override;
    procedure DoExecute(Context: TMoveContext); override;
  public
    constructor Create(AMaster: TMaster; AMap: TMap;
      const APosition: T3DPoint; APlayer: TPlayer);

    property Player: TPlayer read FPlayer;
  end;

var { FunDelphi codegen }
  compPlankPlugin: TPlankPlugin;
  compPlanks: TPlanks;
  compPlank: TPlankTool;

const
  clPlank = $00004080; /// Couleur de la planche

const {don't localize}
  attrUsePlank = 'UsePlank'; /// Attribut indiquant l'usage de la planche

var
  attrtypeUsePlank: Boolean; /// Type de l'attribut UsePlank

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

  if not Boolean(Player.Attributes[attrUsePlank]^) then
    Exit;

  // D�termination de l'endroit o� dessiner r�ellement la planche
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
  BehindQPos: TQualifiedPos;
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

    // On v�rifie que la case du milieu peut �tre survol�e
    DestSquare.DispatchAt(Msg, DestQPos);
    if not Msg.Result then
      Exit;

    // On v�rifie que la case de d�part ou d'arriv�e autorise le d�placement
    Msg.Kind := pmkLeaveFrom;
    Msg.Result := False;
    SrcSquare.DispatchAt(Msg, SrcQPos);
    if not Msg.Result then
    begin
      BehindQPos.Map := Map;
      BehindQPos.Position := Msg.Dest;

      Msg.Kind := pmkArriveAt;
      Map[Msg.Dest].DispatchAt(Msg, BehindQPos);
      if not Msg.Result then
        Exit;
    end;

    TPlankSquare.Create(Master, Map, Msg.Pos, Player);
    Temporize;
  end;
end;

{----------------}
{ Classe TPlanks }
{----------------}

{*
  [@inheritDoc]
*}
constructor TPlanks.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SPlanks;
  Painter.AddImage(fPlank);
end;

{*
  Modifie l'�tat des touches sp�ciales requis
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

{------------------}
{ TPlankTool class }
{------------------}

{*
  [@inheritDoc]
*}
procedure TPlankTool.Find(Context: TMoveContext);
var
  RequiredShift: TShiftState;
  ShiftStr: string;
begin
  inherited;

  RequiredShift := (ObjectDef as TPlanks).RequiredShift;
  if RequiredShift = [] then
    Exit;

  ShiftStr := EnumSetToStr(RequiredShift, TypeInfo(TShiftState));
  Delete(ShiftStr, 1, 2);
  ShiftStr := AnsiReplaceStr(ShiftStr, ', ss', '+');

  Context.Player.ShowMessage(Format(SPlankShiftExplanation, [ShiftStr]));
end;

{---------------------}
{ Classe TPlankSquare }
{---------------------}

{*
  Cr�e une instance de TPlankSquare
  @param AMaster     Ma�tre FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param APlayer     Joueur qui passe sur la case
*}
constructor TPlankSquare.Create(AMaster: TMaster; AMap: TMap;
  const APosition: T3DPoint; APlayer: TPlayer);
begin
  inherited Create(AMaster, '', AMap, APosition);
  FPlayer := APlayer;
  Boolean(FPlayer.Attributes[attrUsePlank]^) := True;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.DoEntering(Context: TMoveContext);
begin
  if Context.Player <> Player then
    Context.Cancel;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.DoExited(Context: TMoveContext);
begin
  Boolean(FPlayer.Attributes[attrUsePlank]^) := False;
  Free;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.DoExecute(Context: TMoveContext);
var
  Redo: Boolean;
  RedoDelay: Cardinal;
begin
  with Context do
  begin
    Temporize;
    Player.MoveTo(PointBehind(Pos, Player.Direction), True, Redo, RedoDelay);
    GoOnMoving := Redo;
    Temporization := RedoDelay;
  end;
end;

end.

