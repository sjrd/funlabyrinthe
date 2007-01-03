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
  SysUtils, Graphics, ScUtils, FunLabyUtils, FLBCommon;

resourcestring
  sLift = 'Ascenseur'; /// Nom de l'ascenseur

const {don't localize}
  idEngagedLiftField = 'EngagedLiftField';       /// ID du terrain ascenseur
  idEngagedLiftEffect = 'EngagedLiftEffect';     /// ID de l'effet ascenseur
  idEngagedLiftScrew = 'EngageLiftScrew-%s-%d';  /// ID de la case ascenseur

  idLift = 'Lift';                               /// ID de l'ascenseur

const {don't localize}
  fLift = 'Lift';             /// Fichier de l'ascenseur
  fOpenedLift = 'OpenedLift'; /// Fichier de l'ascenseur ouvert

resourcestring
  sLiftIsEngaged = 'Cet ascenseur est occupé.';
  sChooseFloorTitle = 'Choisissez un étage';
  sChooseFloor = 'À quel étage voulez-vous aller ?';

type
  {*
    Terrain spécial ascenseur occupé
    Ce terrain ne doit pas être utilisé normalement. Il n'est utilisé que par la
    case spéciale ascenseur occupé.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TEngagedLiftField = class(TField)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Effet spécial ascenseur occupé
    Cet effet ne doit pas être utilisé normalement. Il n'est utilisé que par la
    case spécial ascenseur occupé.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TEngagedLiftEffect = class(TEffect)
  public
    procedure Exited(Player : TPlayer; const Pos, Dest : T3DPoint); override;
  end;

  {*
    Case spéciale ascenseur occupé
    Cette case est utilisée pour l'effet particulier de l'ascenseur.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TEngagedLiftScrew = class(TOverriddenScrew)
  public
    constructor Create(AMaster : TMaster; AMap : TMap; APosition : T3DPoint;
      APlayer : TPlayer; Opened : boolean = False;
      WithEffect : boolean = False);
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
{ Classe TEngagedLiftField }
{--------------------------}

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TEngagedLiftField.Entering(Player : TPlayer;
  OldDirection : TDirection; KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  if KeyPressed then
    Player.ShowDialog(sBlindAlley, sLiftIsEngaged, dtError);

  Cancel := True;
end;

{---------------------------}
{ Classe TEngagedLiftEffect }
{---------------------------}

{*
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player    Joueur qui se déplace
  @param Pos       Position de la case
  @param Dest      Case de destination
*}
procedure TEngagedLiftEffect.Exited(Player : TPlayer;
  const Pos, Dest : T3DPoint);
var Other : T3DPoint;
begin
  inherited;

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

{--------------------------}
{ Classe TEngagedLiftScrew }
{--------------------------}

{*
  Crée une instance de TEngagedLiftScrew
  @param AMaster      Maître FunLabyrinthe
  @param AMap         Carte
  @param APosition    Position
  @param Opened       Indique si l'ascenseur apparaît ouvert
  @param WithEffect   Indique s'il faut assigner l'effet spécial
*}
constructor TEngagedLiftScrew.Create(AMaster : TMaster; AMap : TMap;
  APosition : T3DPoint; APlayer : TPlayer; Opened : boolean = False;
  WithEffect : boolean = False);
var Effect : TEffect;
begin
  if not WithEffect then Effect := nil else
    Effect := AMaster.Effect[idEngagedLiftEffect];
  inherited Create(AMaster,
    Format(idEngagedLiftScrew, [APlayer.ID, APosition.Z]), AMap, APosition,
    AMaster.Field[idEngagedLiftField], Effect, nil, nil);

  if Opened then
    Painter.ImgNames.Add(fOpenedLift);
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
    TEngagedLiftScrew.Create(Master, Player.Map, Other, Player);
    dec(Other.Z);
  end;
  MinFloor := Other.Z+1;

  // Occupation des étages supérieurs
  Other := Pos;
  inc(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftScrew.Create(Master, Player.Map, Other, Player);
    inc(Other.Z);
  end;
  MaxFloor := Other.Z-1;

  // Affichage de l'ascenseur ouvert pendant un temps
  with TEngagedLiftScrew.Create(Master, Player.Map, Pos, Player, True) do
  try
    Master.Temporize;
  finally
    Free;
  end;

  // Fermer l'ascenseur et cacher le joueur complètement
  TEngagedLiftScrew.Create(Master, Player.Map, Pos, Player);
  Player.AddPlugin(Master.Plugin[idAvoidShowPlugin]);

  // Demande au joueur de l'étage auquel il souhaite aller
  Other.Z := Player.ChooseNumber(sChooseFloorTitle, sChooseFloor,
    Pos.Z, MinFloor, MaxFloor);

  // Déplacement du joueur
  Player.MoveTo(Other);

  // Après un temps, ouvrir l'ascenseur et remontrer le joueur
  Master.Temporize;
  Player.Map[Other].Free;
  TEngagedLiftScrew.Create(Master, Player.Map, Other, Player, True, True);
  Player.RemovePlugin(Master.Plugin[idAvoidShowPlugin]);
end;

end.

