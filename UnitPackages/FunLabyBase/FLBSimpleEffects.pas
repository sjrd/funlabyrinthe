{*
  Décrit les effets simples (se suffisant à eux-mêmes)
  L'unité FLBSimpleEffects regroupe les différents effets qui se suffisent à
  eux-mêmes, c'est-à-dire qui n'ont pas besoin de composants annexes pour
  fonctionner.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBSimpleEffects;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, FunLabyTools, FLBCommon, FLBFields;

resourcestring
  sNorthArrow = 'Flèche nord';                /// Nom de la flèche nord
  sEastArrow = 'Flèche est';                  /// Nom de la flèche est
  sSouthArrow = 'Flèche sud';                 /// Nom de la flèche sud
  sWestArrow = 'Flèche ouest';                /// Nom de la flèche ouest
  sCrossroads = 'Carrefour';                  /// Nom du carrefour

  sInactiveTransporter = 'Téléporteur inactif';      /// Téléporteur inactif
  sTransporterNext = 'Téléporteur suivant n°%d';     /// Téléporteur suivant
  sTransporterPrev = 'Téléporteur précédent n°%d';   /// Téléporteur précédent
  sTransporterRandom = 'Téléporteur aléatoire n°%d'; /// Téléporteur aléatoire
  sTransporterTemplate = 'Téléporteur';              /// Téléporteur modèle

  sUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  sDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant

  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect

  sOutside = 'Dehors';                        /// Nom du dehors
  sTreasure = 'Trésor';                       /// Nom du trésor

const {don't localize}
  idNorthArrow = 'NorthArrow';                   /// ID de la flèche nord
  idEastArrow = 'EastArrow';                     /// ID de la flèche est
  idSouthArrow = 'SouthArrow';                   /// ID de la flèche sud
  idWestArrow = 'WestArrow';                     /// ID de la flèche ouest
  idCrossroads = 'Crossroads';                   /// ID du carrefour

  idInactiveTransporter = 'InactiveTransporter'; /// ID du téléporteur inactif
  idTransporterNext = 'TransporterNext%d';       /// ID du téléporteur suivant
  idTransporterPrev = 'TransporterPrev%d';       /// ID du téléporteur précédent
  idTransporterRandom = 'TransporterRandom%d';   /// ID du téléporteur aléatoire
  idTransporterTemplate = 'TransporterTemplate'; /// ID du téléporteur modèle

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant

  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect

  idOutside = 'Outside';                         /// ID du dehors
  idTreasure = 'Treasure';                       /// ID du trésor

  idOutsideScrew = idGrass+'-'+idOutside+'--';   /// ID de la case dehors

const {don't localize}
  fNorthArrow = 'NorthArrow';               /// Fichier de la flèche nord
  fEastArrow = 'EastArrow';                 /// Fichier de la flèche est
  fSouthArrow = 'SouthArrow';               /// Fichier de la flèche sud
  fWestArrow = 'WestArrow';                 /// Fichier de la flèche ouest
  fCrossroads = 'Crossroads';               /// Fichier du carrefour

  fTransporter = 'Transporter';             /// Fichier du téléporteur

  fUpStairs = 'UpStairs';                   /// Fichier de l'escalier montant
  fDownStairs = 'DownStairs';               /// Fichier de l'escalier descendant

  fDirectTurnstile = 'DirectTurnstile';     /// Fichier du tourniquet direct
  fIndirectTurnstile = 'IndirectTurnstile'; /// Fichier du tourniquet indirect

  fOutside = 'Outside';                     /// Fichier du dehors
  fTreasure = 'Treasure';                   /// Fichier du trésor

resourcestring
  sGotOutsideMaze = 'BRAVO ! Tu as réussi à sortir du labyrinthe !';
  sFoundTreasure  = 'BRAVO ! Tu as trouvé le trésor !';

  sTransporterTitle = 'Numéro du téléporteur';
  sTransporterPrompt = 'Numéro du téléporteur (0 à 45) :';

type
  {*
    Type de téléporteur
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Flèche (de toutes directions)
    Les flèches repoussent le joueur dans la direction qui leur est propre. Le
    carrefour en est un cas particulier qui laisse inchangée la direction du
    joueur.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TArrow = class(TEffect)
  private
    FDirection : TDirection; /// Direction de la flèche
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADirection : TDirection);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Direction : TDirection read FDirection;
  end;

  {*
    Téléporteur
    Le téléporteur emmène le joueur à un autre téléporteur
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TTransporter = class(TEffect)
  private
    FNumber : integer;        /// Numéro du téléporteur
    FKind : TTransporterKind; /// Type de téléporteur
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer;
      AKind : TTransporterKind = tkInactive);

    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Number : integer read FNumber;
    property Kind : TTransporterKind read FKind;
  end;

  {*
    Escaliers
    Les escaliers permettent de monter ou descendre d'un étage
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TStairs = class(TEffect)
  private
    FUp : boolean; /// Indique si l'escalier est montant
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AUp : boolean);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Up : boolean read FUp;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'à
    parvenir à en sortir.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TDirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      const Pos, Dest : T3DPoint); override;

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Tourniquet Indirect
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'à
    parvenir à en sortir.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TIndirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      const Pos, Dest : T3DPoint); override;

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Dehors
    Le dehors représente l'extérieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TOutside = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Trésor
    Le trésor fait remporter la victoire au joueur qui le trouve.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TTreasure = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

implementation

{---------------}
{ Classe TArrow }
{---------------}

{*
  Crée une instance de TArrow
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param ADirection   Direction de la flèche
*}
constructor TArrow.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADirection : TDirection);
begin
  inherited Create(AMaster, AID, AName);
  FDirection := ADirection;
  case FDirection of
    diNone  : Painter.ImgNames.Add(fCrossroads);
    diNorth : Painter.ImgNames.Add(fNorthArrow);
    diEast  : Painter.ImgNames.Add(fEastArrow);
    diSouth : Painter.ImgNames.Add(fSouthArrow);
    diWest  : Painter.ImgNames.Add(fWestArrow);
  end;
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TArrow.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  if FDirection <> diNone then
    Player.Direction := FDirection;
  GoOnMoving := True;
end;

{---------------------}
{ Classe TTransporter }
{---------------------}

{*
  Crée une instance de TTransporter
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AKind        Type de téléporteur
*}
constructor TTransporter.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ANumber : integer;
  AKind : TTransporterKind = tkInactive);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));
  FNumber := ANumber;
  FKind := AKind;
  Painter.ImgNames.Add(fTransporter);
end;

{*
  Dessine le téléporteur sur le canevas indiqué
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonnée X du point à partir duquel dessiner le terrain
  @param Y        Coordonnée Y du point à partir duquel dessiner le terrain
*}
procedure TTransporter.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited;

  if Master.Editing then
  begin
    case FKind of
      tkNext     : DrawScrewNumber(Canvas, X, Y, Number, clRed);
      tkPrevious : DrawScrewNumber(Canvas, X, Y, Number, clGreen);
      tkRandom   : DrawScrewNumber(Canvas, X, Y, Number, clBlue);
    end;
  end;
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TTransporter.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;

  // Recherche de la case de destination
  case FKind of
    tkNext     : FindNextScrew    (Player.Map, Other, Self);
    tkPrevious : FindPreviousScrew(Player.Map, Other, Self);
    tkRandom   : FindScrewAtRandom(Player.Map, Other, Self);
    else exit; // on évite des tests inutiles pour un inactif
  end;

  // Si l'on a trouvé une autre case, on déplace le joueur
  if Same3DPoint(Other, Pos) then exit;
  Master.Temporize;
  Player.Position := Other;
end;

{----------------}
{ Classe TStairs }
{----------------}

{*
  Crée une instance de TStairs
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AUp          Indique si l'escalier est montant
*}
constructor TStairs.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AUp : boolean);
begin
  inherited Create(AMaster, AID, AName);
  FUp := AUp;
  if Up then
    Painter.ImgNames.Add(fUpStairs)
  else
    Painter.ImgNames.Add(fDownStairs);
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TStairs.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;
  if Up then
    inc(Other.Z)
  else
    dec(Other.Z);

  Master.Temporize;
  Player.Position := Other;
end;

{-------------------------}
{ Classe TDirectTurnstile }
{-------------------------}

{*
  Crée une instance de TDirectTurnstile
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TDirectTurnstile.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fDirectTurnstile);
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TDirectTurnstile.Exited(Player : TPlayer; KeyPressed : boolean;
  const Pos, Dest : T3DPoint);
begin
  inherited;
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect(idIndirectTurnstile);
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TDirectTurnstile.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;

  Master.Temporize;

  if Player.Direction = diWest then
    Dir := diNorth
  else
    Dir := Succ(Player.Direction);

  while not Player.Move(Dir, False, GoOnMoving) do
  begin
    if Player.Direction = diNorth then
      Dir := diWest
    else
      Dir := Pred(Player.Direction);
  end;
end;

{---------------------------}
{ Classe TIndirectTurnstile }
{---------------------------}

{*
  Crée une instance de TIndirectTurnstile
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TIndirectTurnstile.Create(AMaster : TMaster;
  const AID : TComponentID; const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fIndirectTurnstile);
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TIndirectTurnstile.Exited(Player : TPlayer; KeyPressed : boolean;
  const Pos, Dest : T3DPoint);
begin
  inherited;
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect(idDirectTurnstile);
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TIndirectTurnstile.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;

  Master.Temporize;

  if Player.Direction = diNorth then
    Dir := diWest
  else
    Dir := Pred(Player.Direction);

  while not Player.Move(Dir, False, GoOnMoving) do
  begin
    if Player.Direction = diWest then
      Dir := diNorth
    else
      Dir := Succ(Player.Direction);
  end;
end;

{-----------------}
{ Classe TOutside }
{-----------------}

{*
  Crée une instance de TOutside
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TOutside.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fOutside);
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TOutside.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  Player.Win;
  Player.Controller.ShowDialog(sWon, sGotOutsideMaze);
end;

{------------------}
{ Classe TTreasure }
{------------------}

{*
  Crée une instance de TTreasure
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TTreasure.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fTreasure);
end;

{*
  Exécute l'effet
  @param Player       Joueur concerné
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TTreasure.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  Player.Win;
  Player.Controller.ShowDialog(sWon, sFoundTreasure);
end;

end.

