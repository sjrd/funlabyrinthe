{*
  Décrit le comportement complet de l'ascenseur
  L'unité FLBLift regroupe tous les composants intervenant dans le
  fonctionnement de l'ascenseur.
  @author Sébastien Jean Robert Doeraene
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
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TEngagedLiftScrew = class(TOverriddenScrew)
  private
    FIsExit : boolean; /// Indique si c'est là que sort le joueur
  public
    constructor Create(AMaster : TMaster; AMap : TMap;
      const APosition : T3DPoint; Opened : boolean = False;
      AIsExit : boolean = False);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;

    procedure Exited(Player : TPlayer; const Pos, Dest : T3DPoint); override;

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;

    property IsExit : boolean read FIsExit;
  end;

  {*
    Ascenseur
    Un ascenseur permet au joueur de décider de l'étage où aller.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TLift = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; const Pos : T3DPoint;
      var GoOnMoving : boolean); override;
  end;

implementation

{--------------------------}
{ Classe TEngagedLiftScrew }
{--------------------------}

{*
  Crée une instance de TEngagedLiftScrew
  @param AMaster     Maître FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param Opened      Indique si l'ascenseur apparaît ouvert
  @param AIsExit     Indique si c'est là que sort le joueur
*}
constructor TEngagedLiftScrew.Create(AMaster : TMaster; AMap : TMap;
  const APosition : T3DPoint; Opened : boolean = False;
  AIsExit : boolean = False);
begin
  inherited Create(AMaster, '', AMap, APosition);
  FIsExit := AIsExit;

  if Opened then
    Painter.ImgNames.Add(fOpenedLift);
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftScrew.Entering(Player : TPlayer;
  OldDirection : TDirection; KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  OriginalScrew.Entering(Player, OldDirection, KeyPressed,
    Src, Pos, Cancel);
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftScrew.Exited(Player : TPlayer;
  const Pos, Dest : T3DPoint);
var Other : T3DPoint;
begin
  if not IsExit then exit;

  // Suppression des étages inférieurs
  Other := Pos;
  dec(Other.Z);
  while Player.Map[Other] is TEngagedLiftScrew do
  begin
    Player.Map[Other].Free;
    dec(Other.Z);
  end;

  // Suppression des étages supérieurs
  Other := Pos;
  inc(Other.Z);
  while Player.Map[Other] is TEngagedLiftScrew do
  begin
    Player.Map[Other].Free;
    inc(Other.Z);
  end;

  // Suppresion de cet étage-ci
  Player.Map[Pos].Free;
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftScrew.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  OriginalScrew.Pushing(Player, OldDirection, KeyPressed,
    Src, Pos, Cancel, AbortExecute);
  if Cancel then exit;

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
constructor TLift.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
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
procedure TLift.Execute(Player : TPlayer; const Pos : T3DPoint;
  var GoOnMoving : boolean);
var Other : T3DPoint;
    MinFloor, MaxFloor : integer;
begin
  // Occupation des étages inférieurs
  Other := Pos;
  dec(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftScrew.Create(Master, Player.Map, Other);
    dec(Other.Z);
  end;
  MinFloor := Other.Z+1;

  // Occupation des étages supérieurs
  Other := Pos;
  inc(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftScrew.Create(Master, Player.Map, Other);
    inc(Other.Z);
  end;
  MaxFloor := Other.Z-1;

  // Affichage de l'ascenseur ouvert pendant un temps
  with TEngagedLiftScrew.Create(Master, Player.Map, Pos, True) do
  try
    Master.Temporize;
  finally
    Free;
  end;

  // Fermer l'ascenseur et cacher le joueur complètement
  TEngagedLiftScrew.Create(Master, Player.Map, Pos);
  Player.Hide;

  // Demande au joueur de l'étage auquel il souhaite aller
  Other.Z := Player.ChooseNumber(sChooseFloorTitle, sChooseFloor,
    Pos.Z, MinFloor, MaxFloor);

  // Déplacement du joueur
  Player.MoveTo(Other);

  // Après un temps, ouvrir l'ascenseur et remontrer le joueur
  Master.Temporize;
  Player.Map[Other].Free;
  TEngagedLiftScrew.Create(Master, Player.Map, Other, True, True);
  Player.Show;
end;

end.

