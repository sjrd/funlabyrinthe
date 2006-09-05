{*
  Diff�rentes cases de FunLabyrinthe
  L'unit� Screws comprend les d�finitions des cases de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit Screws;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

resourcestring
  sGrass = 'Herbe';                           /// Nom de l'herbe
  sWall = 'Mur';                              /// Nom du mur
  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect
  sOutside = 'Dehors';                        /// Nom du dehors

const {don't localize}
  fGrass = 'Grass';                           /// Fichier de l'herbe
  fWall = 'Wall';                             /// Fichier du mur
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
  cNorthArrow             = 54;  /// Code de la fl�che nord
  cEastArrow              = 55;  /// Code de la fl�che est
  cSouthArrow             = 56;  /// Code de la fl�che sud
  cWestArrow              = 57;  /// Code de la fl�che ouest
  cCrossroads             = 58;  /// Code du carrefour
  cTreasure               = 59;  /// Code du tr�sor
  cSecretPassage          = 63;  /// Code du passage secret
  cDirectTurnstile        = 224; /// Code du tourniquet direct
  cIndirectTurnstile      = 225; /// Code du tourniquet indirect
  cButton                 = [33..47, 161..190]; /// Codes des boutons
  cSunkenButton           = 64;  /// Code du bouton enfonc�
  cInactiveTransporter    = 96;  /// Code du t�l�porteur inactif
  cNextOneTransporter     = [97..109];  /// Codes des t�l�porteurs 'suivant'
  cPreviousOneTransporter = [110..122]; /// Codes des t�l�porteurs 'pr�c�dent'
  cRandomTransporter      = [123..126]; /// Codes des t�l�porteurs al�atoires
  cStairs                 = [71..90];   /// Codes des escaliers
  cDownStairs             = 60;  /// Code de l'escalier descendant
  cLift                   = 61;  /// Code de l'ascenseur
  cOpenLift               = 5;   /// Code fictif de l'ascenseur ouvert
  cUpStairs               = 62;  /// Code de l'escalier montant
  cStart                  = 65;  /// Code du d�part
  cOutside                = 66;  /// Code du dehors
  cBuoy                   = 67;  /// Code de la bou�e
  cPlank                  = 68;  /// Code de la planche
  cSilverKey              = 69;  /// Code de la clef en argent
  cGoldenKey              = 70;  /// Code de la clef en or
  cSky                    = 226; /// Code du ciel
  cBoat                   = [193..202]; /// Codes des barques

type
  {*
    Herbe
    L'herbe est le terrain de base de FunLabyrinthe. Il n'a pas de condition.
  *}
  TGrass = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);
  end;

  {*
    Mur
    Le mur est un terrain qui bloque syst�matiquement le joueur.
  *}
  TWall = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'�
    parvenir � en sortir.
  *}
  TDirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Tourniquet Indirect
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'�
    parvenir � en sortir.
  *}
  TIndirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); override;
  end;

  {*
    Dehors
    Le dehors repr�sente l'ext�rieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
  *}
  TOutside = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

implementation

/////////////////////
/// Classe TGrass ///
/////////////////////

{*
  Cr�e une instance de TGrass
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TGrass.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fGrass);
end;

////////////////////
/// Classe TWall ///
////////////////////

{*
  Cr�e une instance de TWall
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TWall.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fWall);
end;

{*
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Entered de la case ne soit ex�cut�e, il faut positionner
  AbortEntered � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortEntered   � positionner � True pour emp�cher le Entered
*}
procedure TWall.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  Cancel := True;
end;

///////////////////////////////
/// Classe TDirectTurnstile ///
///////////////////////////////

{*
  Cr�e une instance de TDirectTurnstile
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
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
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
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
  Cr�e une instance de TIndirectTurnstile
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
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
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
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
  Cr�e une instance de TOutside
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TOutside.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  // Faire gagner le joueur
end;

end.

