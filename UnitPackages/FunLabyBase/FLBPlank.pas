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
  SysUtils, Graphics, ScUtils, FunLabyUtils, Generics, FLBCommon;

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
  fPlank = 'Plank'; /// Fichier de la planche

const {don't localize}
  attrUsePlank = 'UsePlank'; /// Attribut indiquant l'usage de la planche

resourcestring
  sFoundPlank = 'Tu as trouv� une planche.'+#10+
    'Tu peux franchir certains obstacles.';

type
  {*
    Plug-in planche
    Affiche une planche � c�t� du joueur ou sous celui-ci.
    @author sjrd
    @version 5.0
  *}
  TPlankPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
      Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); override;

    procedure Moving(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Dest: T3DPoint;
      var Cancel: Boolean); override;
  end;

  {*
    D�finition de l'objet planche
    La planche permet de passer au-dessus des cases
    @author sjrd
    @version 5.0
  *}
  TPlanks = class(TObjectDef)
  protected
    procedure SetCount(Player: TPlayer; Value: Integer); override;

    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
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
  public
    constructor Create(AMaster: TMaster; AMap: TMap;
      const APosition: T3DPoint; APlayer: TPlayer);

    procedure Entering(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel: Boolean); override;

    procedure Exited(Player: TPlayer; const Pos, Dest: T3DPoint); override;

    procedure Execute(Player: TPlayer; const Pos: T3DPoint;
      var GoOnMoving: Boolean); override;

    property Player: TPlayer read FPlayer;
  end;

const
  clPlank = $00004080; /// Couleur de la planche

implementation

{---------------------}
{ Classe TPlankPlugin }
{---------------------}

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlankPlugin.DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
  Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  inherited;

  if Player.Attribute[attrUsePlank] = 0 then
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
  with Canvas do
  begin
    Brush.Color := clPlank;
    Brush.Style := bsSolid;
    Pen.Color := clPlank;
    Pen.Style := psSolid;

    if Player.Direction in [diNorth, diSouth] then
      Rectangle(X+6, Y-5, X+SquareSize-6, Y+SquareSize+5)
    else
      Rectangle(X-5, Y+6, X+SquareSize+5, Y+SquareSize-6);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TPlankPlugin.Moving(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Dest: T3DPoint; var Cancel: Boolean);
var
  Behind: T3DPoint;
  Msg: TPlankMessage;
  Field: TField;
begin
  Behind := PointBehind(Dest, Player.Direction);

  Msg.MsgID := msgPlank;
  Msg.Kind := pmkPassOver;
  Msg.Result := False;
  Msg.Player := Player;
  Msg.Pos := Dest;
  Msg.Src := Src;
  Msg.Dest := Behind;

  with Player do
  begin
    // On v�rifie que la case du milieu peut �tre survol�e
    Field := Map[Msg.Pos].Field;
    if Assigned(Field) then
      Field.Dispatch(Msg);
    if not Msg.Result then
      Exit;

    // On v�rifie que la case de d�part ou d'arriv�e autorise le d�placement
    Msg.Kind := pmkLeaveFrom;
    Msg.Result := False;
    Field := Map[Msg.Src].Field;
    if Assigned(Field) then
      Field.Dispatch(Msg);
    if not Msg.Result then
    begin
      Msg.Kind := pmkArriveAt;
      Field := Map[Msg.Dest].Field;
      if Assigned(Field) then
        Field.Dispatch(Msg);
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
  Cr�e une instance de TPlanks
  @param AMaster   Ma�tre FunLabyrinthe
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
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
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
  FPlayer.Attribute[attrUsePlank] := 1;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.Entering(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel: Boolean);
begin
  inherited;
  if Player <> FPlayer then
    Cancel := True;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.Exited(Player: TPlayer; const Pos, Dest: T3DPoint);
begin
  inherited;
  FPlayer.Attribute[attrUsePlank] := 0;
  Free;
end;

{*
  [@inheritDoc]
*}
procedure TPlankSquare.Execute(Player: TPlayer; const Pos: T3DPoint;
  var GoOnMoving: Boolean);
begin
  inherited;
  Master.Temporize;
  Player.MoveTo(PointBehind(Pos, Player.Direction), True, GoOnMoving);
end;

end.

