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
  TEngagedLiftSquare = class(TOverriddenSquare)
  private
    FIsExit: Boolean; /// Indique si c'est l� que sort le joueur
  public
    constructor Create(AMaster: TMaster; AMap: TMap;
      const APosition: T3DPoint; Opened: Boolean = False;
      AIsExit: Boolean = False);

    procedure Entering(Context: TMoveContext); override;

    procedure Exited(Context: TMoveContext); override;

    procedure Pushing(Context: TMoveContext); override;

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

    procedure Execute(Context: TMoveContext); override;
  end;

implementation

{--------------------------}
{ Classe TEngagedLiftSquare }
{--------------------------}

{*
  Cr�e une instance de TEngagedLiftSquare
  @param AMaster     Ma�tre FunLabyrinthe
  @param AMap        Carte
  @param APosition   Position
  @param Opened      Indique si l'ascenseur appara�t ouvert
  @param AIsExit     Indique si c'est l� que sort le joueur
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
procedure TEngagedLiftSquare.Entering(Context: TMoveContext);
begin
  OriginalSquare.Entering(Context);
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftSquare.Exited(Context: TMoveContext);
var
  Other: T3DPoint;
begin
  if not IsExit then
    Exit;

  with Context do
  begin
    // Suppression des �tages inf�rieurs
    Other := Pos;
    Dec(Other.Z);
    while Map[Other] is TEngagedLiftSquare do
    begin
      Map[Other].Free;
      Dec(Other.Z);
    end;

    // Suppression des �tages sup�rieurs
    Other := Pos;
    Inc(Other.Z);
    while Player.Map[Other] is TEngagedLiftSquare do
    begin
      Map[Other].Free;
      Inc(Other.Z);
    end;

    // Suppresion de cet �tage-ci
    Map[Pos].Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TEngagedLiftSquare.Pushing(Context: TMoveContext);
begin
  with Context do
  begin
    OriginalSquare.Pushing(Context);
    if Cancelled then
      Exit;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sLiftIsEngaged, dtError);
    Cancel;
  end;
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
  [@inheritDoc]
*}
procedure TLift.Execute(Context: TMoveContext);
var
  Other: T3DPoint;
  MinFloor, MaxFloor: Integer;
begin
  with Context do
  begin
    // Occupation des �tages inf�rieurs
    Other := Pos;
    Dec(Other.Z);
    while Map[Other].Effect = Self do
    begin
      TEngagedLiftSquare.Create(Master, Map, Other);
      Dec(Other.Z);
    end;
    MinFloor := Other.Z+1;

    // Occupation des �tages sup�rieurs
    Other := Pos;
    Inc(Other.Z);
    while Map[Other].Effect = Self do
    begin
      TEngagedLiftSquare.Create(Master, Map, Other);
      Inc(Other.Z);
    end;
    MaxFloor := Other.Z-1;

    // Affichage de l'ascenseur ouvert pendant un temps
    with TEngagedLiftSquare.Create(Master, Map, Pos, True) do
    try
      Master.Temporize;
    finally
      Free;
    end;

    // Fermer l'ascenseur et cacher le joueur compl�tement
    TEngagedLiftSquare.Create(Master, Map, Pos);
    Player.Hide;

    // Demande au joueur de l'�tage auquel il souhaite aller
    Other.Z := Player.ChooseNumber(sChooseFloorTitle, sChooseFloor,
      Pos.Z, MinFloor, MaxFloor);

    // D�placement du joueur
    Player.MoveTo(Other);

    // Apr�s un temps, ouvrir l'ascenseur et remontrer le joueur
    Master.Temporize;
    Map[Other].Free;
    TEngagedLiftSquare.Create(Master, Map, Other, True, True);
    Player.Show;
  end;
end;

end.

