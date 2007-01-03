{*
  D�crit le comportement complet de l'ascenseur
  L'unit� FLBLift regroupe tous les composants intervenant dans le
  fonctionnement de l'ascenseur.
  @author S�bastien Jean Robert Doeraene
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
  sLiftIsEngaged = 'Cet ascenseur est occup�.';
  sChooseFloorTitle = 'Choisissez un �tage';
  sChooseFloor = '� quel �tage voulez-vous aller ?';

type
  {*
    Terrain sp�cial ascenseur occup�
    Ce terrain ne doit pas �tre utilis� normalement. Il n'est utilis� que par la
    case sp�ciale ascenseur occup�.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TEngagedLiftField = class(TField)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Effet sp�cial ascenseur occup�
    Cet effet ne doit pas �tre utilis� normalement. Il n'est utilis� que par la
    case sp�cial ascenseur occup�.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TEngagedLiftEffect = class(TEffect)
  public
    procedure Exited(Player : TPlayer; const Pos, Dest : T3DPoint); override;
  end;

  {*
    Case sp�ciale ascenseur occup�
    Cette case est utilis�e pour l'effet particulier de l'ascenseur.
    @author S�bastien Jean Robert Doeraene
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
    Un ascenseur permet au joueur de d�cider de l'�tage o� aller.
    @author S�bastien Jean Robert Doeraene
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
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
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
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player    Joueur qui se d�place
  @param Pos       Position de la case
  @param Dest      Case de destination
*}
procedure TEngagedLiftEffect.Exited(Player : TPlayer;
  const Pos, Dest : T3DPoint);
var Other : T3DPoint;
begin
  inherited;

  // Suppression des �tages inf�rieurs
  Other := Pos;
  dec(Other.Z);
  while Player.Map[Other] is TEngagedLiftScrew do
  begin
    Player.Map[Other].Free;
    dec(Other.Z);
  end;

  // Suppression des �tages sup�rieurs
  Other := Pos;
  inc(Other.Z);
  while Player.Map[Other] is TEngagedLiftScrew do
  begin
    Player.Map[Other].Free;
    inc(Other.Z);
  end;

  // Suppresion de cet �tage-ci
  Player.Map[Pos].Free;
end;

{--------------------------}
{ Classe TEngagedLiftScrew }
{--------------------------}

{*
  Cr�e une instance de TEngagedLiftScrew
  @param AMaster      Ma�tre FunLabyrinthe
  @param AMap         Carte
  @param APosition    Position
  @param Opened       Indique si l'ascenseur appara�t ouvert
  @param WithEffect   Indique s'il faut assigner l'effet sp�cial
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
  Cr�e une instance de TLift
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TLift.Execute(Player : TPlayer; const Pos : T3DPoint;
  var GoOnMoving : boolean);
var Other : T3DPoint;
    MinFloor, MaxFloor : integer;
begin
  // Occupation des �tages inf�rieurs
  Other := Pos;
  dec(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftScrew.Create(Master, Player.Map, Other, Player);
    dec(Other.Z);
  end;
  MinFloor := Other.Z+1;

  // Occupation des �tages sup�rieurs
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

  // Fermer l'ascenseur et cacher le joueur compl�tement
  TEngagedLiftScrew.Create(Master, Player.Map, Pos, Player);
  Player.AddPlugin(Master.Plugin[idAvoidShowPlugin]);

  // Demande au joueur de l'�tage auquel il souhaite aller
  Other.Z := Player.ChooseNumber(sChooseFloorTitle, sChooseFloor,
    Pos.Z, MinFloor, MaxFloor);

  // D�placement du joueur
  Player.MoveTo(Other);

  // Apr�s un temps, ouvrir l'ascenseur et remontrer le joueur
  Master.Temporize;
  Player.Map[Other].Free;
  TEngagedLiftScrew.Create(Master, Player.Map, Other, Player, True, True);
  Player.RemovePlugin(Master.Plugin[idAvoidShowPlugin]);
end;

end.

