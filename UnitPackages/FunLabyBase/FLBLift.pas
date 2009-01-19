{*
  Décrit le comportement complet de l'ascenseur
  L'unité FLBLift regroupe tous les composants intervenant dans le
  fonctionnement de l'ascenseur.
  @author sjrd
  @version 5.0
*}
unit FLBLift;

interface

uses
  SysUtils, Graphics, ScUtils, SdDialogs, FunLabyUtils, Generics, FLBCommon;

resourcestring
  sLift = 'Ascenseur'; /// Nom de l'ascenseur

const {don't localize}
  idLift = 'Lift'; /// ID de l'ascenseur

const {don't localize}
  fLift = 'Lift';             /// Fichier de l'ascenseur
  fOpenedLift = 'OpenedLift'; /// Fichier de l'ascenseur ouvert

resourcestring
  sLiftIsEngaged = 'Cet ascenseur est occupé.';
  sChooseFloorTitle = 'Choisissez un étage';
  sChooseFloor = 'À quel étage voulez-vous aller ?';

type
  {*
    Case spéciale ascenseur occupé
    Cette case est utilisée pour l'effet particulier de l'ascenseur.
    @author sjrd
    @version 5.0
  *}
  TEngagedLiftSquare = class(TOverriddenSquare)
  private
    FIsExit: Boolean; /// Indique si c'est là que sort le joueur
  public
    constructor Create(AMaster: TMaster; AMap: TMap;
      const APosition: T3DPoint; Opened: Boolean = False;
      AIsExit: Boolean = False);

    procedure Entering(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel: Boolean); override;

    procedure Exited(Player: TPlayer; const Pos, Dest: T3DPoint); override;

    procedure Pushing(Player: TPlayer; OldDirection: TDirection;
      KeyPressed: Boolean; const Src, Pos: T3DPoint;
      var Cancel, AbortExecute: Boolean); override;

    property IsExit: Boolean read FIsExit;
  end;

  {*
    Ascenseur
    Un ascenseur permet au joueur de décider de l'étage où aller.
    @author sjrd
    @version 5.0
  *}
  TLift = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Execute(Player: TPlayer; const Pos: T3DPoint;
      var GoOnMoving: Boolean); override;
  end;

implementation

{--------------------------}
{ Classe TEngagedLiftSquare }
{--------------------------}

{*
  Crée une instance de TEngagedLiftSquare
  @param AMaster     Maître FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param Opened      Indique si l'ascenseur apparaît ouvert
  @param AIsExit     Indique si c'est là que sort le joueur
*}
constructor TEngagedLiftSquare.Create(AMaster: TMaster; AMap: TMap;
  const APosition: T3DPoint; Opened: Boolean = False;
  AIsExit: Boolean = False);
begin
  inherited Create(AMaster, '', AMap, APosition);
  FIsExit := AIsExit;

  if Opened then
    Painter.ImgNames.Add(fOpenedLift);
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftSquare.Entering(Player: TPlayer;
  OldDirection: TDirection; KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel: Boolean);
begin
  OriginalSquare.Entering(Player, OldDirection, KeyPressed,
    Src, Pos, Cancel);
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftSquare.Exited(Player: TPlayer;
  const Pos, Dest: T3DPoint);
var
  Other: T3DPoint;
begin
  if not IsExit then
    Exit;

  // Suppression des étages inférieurs
  Other := Pos;
  Dec(Other.Z);
  while Player.Map[Other] is TEngagedLiftSquare do
  begin
    Player.Map[Other].Free;
    Dec(Other.Z);
  end;

  // Suppression des étages supérieurs
  Other := Pos;
  Inc(Other.Z);
  while Player.Map[Other] is TEngagedLiftSquare do
  begin
    Player.Map[Other].Free;
    Inc(Other.Z);
  end;

  // Suppresion de cet étage-ci
  Player.Map[Pos].Free;
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftSquare.Pushing(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel, AbortExecute: Boolean);
begin
  OriginalSquare.Pushing(Player, OldDirection, KeyPressed,
    Src, Pos, Cancel, AbortExecute);
  if Cancel then
    Exit;

  if KeyPressed then
    Player.ShowDialog(sBlindAlley, sLiftIsEngaged, dtError);
  Cancel := True;
end;

{--------------}
{ Classe TLift }
{--------------}

{*
  Crée une instance de TLift
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TLift.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fLift);
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TLift.Execute(Player: TPlayer; const Pos: T3DPoint;
  var GoOnMoving: Boolean);
var
  Other: T3DPoint;
  MinFloor, MaxFloor: Integer;
begin
  // Occupation des étages inférieurs
  Other := Pos;
  Dec(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftSquare.Create(Master, Player.Map, Other);
    Dec(Other.Z);
  end;
  MinFloor := Other.Z+1;

  // Occupation des étages supérieurs
  Other := Pos;
  Inc(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftSquare.Create(Master, Player.Map, Other);
    Inc(Other.Z);
  end;
  MaxFloor := Other.Z-1;

  // Affichage de l'ascenseur ouvert pendant un temps
  with TEngagedLiftSquare.Create(Master, Player.Map, Pos, True) do
  try
    Master.Temporize;
  finally
    Free;
  end;

  // Fermer l'ascenseur et cacher le joueur complètement
  TEngagedLiftSquare.Create(Master, Player.Map, Pos);
  Player.Hide;

  // Demande au joueur de l'étage auquel il souhaite aller
  Other.Z := Player.ChooseNumber(sChooseFloorTitle, sChooseFloor,
    Pos.Z, MinFloor, MaxFloor);

  // Déplacement du joueur
  Player.MoveTo(Other);

  // Après un temps, ouvrir l'ascenseur et remontrer le joueur
  Master.Temporize;
  Player.Map[Other].Free;
  TEngagedLiftSquare.Create(Master, Player.Map, Other, True, True);
  Player.Show;
end;

end.

