{*
  D�crit le comportement complet de l'ascenseur
  L'unit� FLBLift regroupe tous les composants intervenant dans le
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
  sLiftIsEngaged = 'Cet ascenseur est occup�.';
  sChooseFloorTitle = 'Choisissez un �tage';
  sChooseFloor = '� quel �tage voulez-vous aller ?';

type
  {*
    Case sp�ciale ascenseur occup�
    Cette case est utilis�e pour l'effet particulier de l'ascenseur.
    @author sjrd
    @version 5.0
  *}
  TEngagedLiftScrew = class(TOverriddenScrew)
  private
    FIsExit: Boolean; /// Indique si c'est l� que sort le joueur
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
    Un ascenseur permet au joueur de d�cider de l'�tage o� aller.
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
{ Classe TEngagedLiftScrew }
{--------------------------}

{*
  Cr�e une instance de TEngagedLiftScrew
  @param AMaster     Ma�tre FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param Opened      Indique si l'ascenseur appara�t ouvert
  @param AIsExit     Indique si c'est l� que sort le joueur
*}
constructor TEngagedLiftScrew.Create(AMaster: TMaster; AMap: TMap;
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
procedure TEngagedLiftScrew.Entering(Player: TPlayer;
  OldDirection: TDirection; KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel: Boolean);
begin
  OriginalScrew.Entering(Player, OldDirection, KeyPressed,
    Src, Pos, Cancel);
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftScrew.Exited(Player: TPlayer;
  const Pos, Dest: T3DPoint);
var
  Other: T3DPoint;
begin
  if not IsExit then
    Exit;

  // Suppression des �tages inf�rieurs
  Other := Pos;
  Dec(Other.Z);
  while Player.Map[Other] is TEngagedLiftScrew do
  begin
    Player.Map[Other].Free;
    Dec(Other.Z);
  end;

  // Suppression des �tages sup�rieurs
  Other := Pos;
  Inc(Other.Z);
  while Player.Map[Other] is TEngagedLiftScrew do
  begin
    Player.Map[Other].Free;
    Inc(Other.Z);
  end;

  // Suppresion de cet �tage-ci
  Player.Map[Pos].Free;
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftScrew.Pushing(Player: TPlayer; OldDirection: TDirection;
  KeyPressed: Boolean; const Src, Pos: T3DPoint;
  var Cancel, AbortExecute: Boolean);
begin
  OriginalScrew.Pushing(Player, OldDirection, KeyPressed,
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
  Cr�e une instance de TLift
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TLift.Execute(Player: TPlayer; const Pos: T3DPoint;
  var GoOnMoving: Boolean);
var
  Other: T3DPoint;
  MinFloor, MaxFloor: Integer;
begin
  // Occupation des �tages inf�rieurs
  Other := Pos;
  Dec(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftScrew.Create(Master, Player.Map, Other);
    Dec(Other.Z);
  end;
  MinFloor := Other.Z+1;

  // Occupation des �tages sup�rieurs
  Other := Pos;
  Inc(Other.Z);
  while Player.Map[Other].Effect = Self do
  begin
    TEngagedLiftScrew.Create(Master, Player.Map, Other);
    Inc(Other.Z);
  end;
  MaxFloor := Other.Z-1;

  // Affichage de l'ascenseur ouvert pendant un temps
  with TEngagedLiftScrew.Create(Master, Player.Map, Pos, True) do
  try
    Master.Temporize;
  finally
    Free;
  end;

  // Fermer l'ascenseur et cacher le joueur compl�tement
  TEngagedLiftScrew.Create(Master, Player.Map, Pos);
  Player.Hide;

  // Demande au joueur de l'�tage auquel il souhaite aller
  Other.Z := Player.ChooseNumber(sChooseFloorTitle, sChooseFloor,
    Pos.Z, MinFloor, MaxFloor);

  // D�placement du joueur
  Player.MoveTo(Other);

  // Apr�s un temps, ouvrir l'ascenseur et remontrer le joueur
  Master.Temporize;
  Player.Map[Other].Free;
  TEngagedLiftScrew.Create(Master, Player.Map, Other, True, True);
  Player.Show;
end;

end.

