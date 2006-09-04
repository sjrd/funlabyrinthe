{*
  Différentes cases de FunLabyrinthe
  L'unité Screws comprend les définitions des cases de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit Screws;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

resourcestring
  sGrass = 'Herbe';                           /// Nom de l'herbe
  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect
  sOutside = 'Dehors';                        /// Nom du dehors

const {don't localize}
  fGrass = 'Grass';                           /// Fichier de l'herbe
  fDirectTurnstile = 'DirectTurnstile';       /// Fichier du tourniquet direct
  fIndirectTurnstile = 'IndirectTurnstile';   /// Fichier du tourniquet indirect
  fOutside = 'Outside';                       /// Fichier du dehors

const
  cGrass                  = 48;  /// Code de l'herbe
  cWater                  = 49;  /// Code de l'eau
  cWall                   = 50;  /// Code du mur
  cHole                   = 51;  /// Code du trou
  cSilverBlock            = 52;  /// Code du bloc en argent
  cGoldenBlock            = 53;  /// Code du bloc en or
  cNorthArrow             = 54;  /// Code de la flèche nord
  cEastArrow              = 55;  /// Code de la flèche est
  cSouthArrow             = 56;  /// Code de la flèche sud
  cWestArrow              = 57;  /// Code de la flèche ouest
  cCrossroads             = 58;  /// Code du carrefour
  cTreasure               = 59;  /// Code du trésor
  cSecretPassage          = 63;  /// Code du passage secret
  cDirectTurnstile        = 224; /// Code du tourniquet direct
  cIndirectTurnstile      = 225; /// Code du tourniquet indirect
  cButton                 = [33..47, 161..190]; /// Codes des boutons
  cSunkenButton           = 64;  /// Code du bouton enfoncé
  cInactiveTransporter    = 96;  /// Code du téléporteur inactif
  cNextOneTransporter     = [97..109];  /// Codes des téléporteurs 'suivant'
  cPreviousOneTransporter = [110..122]; /// Codes des téléporteurs 'précédent'
  cRandomTransporter      = [123..126]; /// Codes des téléporteurs aléatoires
  cStairs                 = [71..90];   /// Codes des escaliers
  cDownStairs             = 60;  /// Code de l'escalier descendant
  cLift                   = 61;  /// Code de l'ascenseur
  cOpenLift               = 5;   /// Code fictif de l'ascenseur ouvert
  cUpStairs               = 62;  /// Code de l'escalier montant
  cStart                  = 65;  /// Code du départ
  cOutside                = 66;  /// Code du dehors
  cBuoy                   = 67;  /// Code de la bouée
  cPlank                  = 68;  /// Code de la planche
  cSilverKey              = 69;  /// Code de la clef en argent
  cGoldenKey              = 70;  /// Code de la clef en or
  cSky                    = 226; /// Code du ciel
  cBoat                   = [193..202]; /// Codes des barques

type
  {*
    Herbe
    L'herbe est la case de base de FunLabyrinthe. Elle est sans actions.
  *}
  TGrass = class(TScrew)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'à
    parvenir à en sortir.
  *}
  TDirectTurnstile = class(TGrass)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Tourniquet Indirect
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'à
    parvenir à en sortir.
  *}
  TIndirectTurnstile = class(TGrass)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Dehors
    Le dehors représente l'extérieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
  *}
  TOutside = class(TGrass)
  protected
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode); overload;
  public
    constructor Create(AMaster : TMaster); overload;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

implementation

/////////////////////
/// Classe TGrass ///
/////////////////////

{*
  Crée une instance de TGrass
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom de la case
  @param ACode     Code de la case
*}
constructor TGrass.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fGrass);
end;

{*
  Crée une instance de TGrass
  @param AMaster   Maître FunLabyrinthe
*}
constructor TGrass.Create(AMaster : TMaster);
begin
  Create(AMaster, sGrass, cGrass);
end;

///////////////////////////////
/// Classe TDirectTurnstile ///
///////////////////////////////

{*
  Crée une instance de TDirectTurnstile
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom de la case
  @param ACode     Code de la case
*}
constructor TDirectTurnstile.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fDirectTurnstile);
end;

{*
  Crée une instance de TDirectTurnstile
  @param AMaster   Maître FunLabyrinthe
*}
constructor TDirectTurnstile.Create(AMaster : TMaster);
begin
  Create(AMaster, sDirectTurnstile, cDirectTurnstile);
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TDirectTurnstile.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;
  repeat
    if Player.Direction = diNorth then
      Dir := diWest
    else
      Dir := Pred(Player.Direction);
  until Player.Move(Dir, KeyPressed, GoOnMoving);
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
  Pos, Dest : T3DPoint);
begin
  inherited;
  Master.Map.CodeMap[Pos] := cIndirectTurnstile;
end;

/////////////////////////////////
/// Classe TIndirectTurnstile ///
/////////////////////////////////

{*
  Crée une instance de TIndirectTurnstile
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom de la case
  @param ACode     Code de la case
*}
constructor TIndirectTurnstile.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fIndirectTurnstile);
end;

{*
  Crée une instance de TIndirectTurnstile
  @param AMaster   Maître FunLabyrinthe
*}
constructor TIndirectTurnstile.Create(AMaster : TMaster);
begin
  Create(AMaster, sIndirectTurnstile, cIndirectTurnstile);
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TIndirectTurnstile.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Dir : TDirection;
begin
  inherited;
  repeat
    if Player.Direction = diWest then
      Dir := diNorth
    else
      Dir := Succ(Player.Direction);
  until Player.Move(Dir, KeyPressed, GoOnMoving);
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
  Pos, Dest : T3DPoint);
begin
  inherited;
  Master.Map.CodeMap[Pos] := cDirectTurnstile;
end;

///////////////////////
/// Classe TOutside ///
///////////////////////

{*
  Crée une instance de TOutside
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom de la case
  @param ACode     Code de la case
*}
constructor TOutside.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName, ACode);
  Painter.ImgNames.Add(fOutside);
end;

{*
  Crée une instance de TOutside
  @param AMaster   Maître FunLabyrinthe
*}
constructor TOutside.Create(AMaster : TMaster);
begin
  Create(AMaster, sOutside, cOutside);
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TOutside.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  // Faire gagner le joueur
end;

end.

