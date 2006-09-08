{*
  Décrit les composants au coeur de Funlabyrinthe
  L'unité FunLabyCore décrit les plug-in, objets, terrains, et effets de case
  qui sont au coeur de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyCore;

interface

uses
  Windows, SysUtils, Graphics, ScUtils, FunLabyUtils;

resourcestring
  sGrass = 'Herbe';                           /// Nom de l'herbe
  sWall = 'Mur';                              /// Nom du mur
  sWater = 'Eau';                             /// Nom de l'eau
  sHole = 'Trou';                             /// Nom du trou
  sSilverBlock = 'Bloc en argent';            /// Nom du bloc en argent
  sGoldenBlock = 'Bloc en or';                /// Nom du bloc en or
  sSky = 'Ciel';                              /// Nom du ciel

  sNorthArrow = 'Flèche nord';                /// Nom de la flèche nord
  sEastArrow = 'Flèche est';                  /// Nom de la flèche est
  sSouthArrow = 'Flèche sud';                 /// Nom de la flèche sud
  sWestArrow = 'Flèche ouest';                /// Nom de la flèche ouest

  sInactiveTransporter = 'Téléporteur inactif';      /// Téléporteur inactif
  sTransporterNext = 'Téléporteur suivant n°%d';     /// Téléporteur suivant
  sTransporterPrev = 'Téléporteur précédent n°%d';   /// Téléporteur précédent
  sTransporterRandom = 'Téléporteur aléatoire n°%d'; /// Téléporteur aléatoire

  sUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  sDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant
  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect
  sOutside = 'Dehors';                        /// Nom du dehors

const {don't localize}
  idGrass = 'Grass';                             /// ID de l'herbe
  idWall = 'Wall';                               /// ID du mur
  idWater = 'Water';                             /// ID de l'eau
  idHole = 'Hole';                               /// ID du trou
  idSilverBlock = 'SilverBlock';                 /// ID du bloc en argent
  idGoldenBlock = 'GoldenBlock';                 /// ID du bloc en or
  idSky = 'Sky';                                 /// ID du ciel

  idNoEffect = 'NoEffect';                       /// ID du sans effet
  idNorthArrow = 'NorthArrow';                   /// ID de la flèche nord
  idEastArrow = 'EastArrow';                     /// ID de la flèche est
  idSouthArrow = 'SouthArrow';                   /// ID de la flèche sud
  idWestArrow = 'WestArrow';                     /// ID de la flèche ouest

  idInactiveTransporter = 'InactiveTransporter'; /// ID du téléporteur inactif
  idTransporterNext = 'TransporterNext%d';       /// ID du téléporteur suivant
  idTransporterPrev = 'TransporterPrev%d';       /// ID du téléporteur précédent
  idTransporterRandom = 'TransporterRandom%d';   /// ID du téléporteur aléatoire

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant
  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect
  idOutside = 'Outside';                         /// ID du dehors

const {don't localize}
  fGrass = 'Grass';                         /// Fichier de l'herbe
  fWall = 'Wall';                           /// Fichier du mur
  fWater = 'Water';                         /// Fichier de l'eau
  fHole = 'Hole';                           /// Fichier du trou
  fSilverBlock = 'SilverBlock';             /// Fichier du bloc en argent
  fGoldenBlock = 'GoldenBlock';             /// Fichier du bloc en or
  fSky = 'Sky';                             /// Fichier du ciel

  fNorthArrow = 'NorthArrow';               /// Fichier de la flèche nord
  fEastArrow = 'EastArrow';                 /// Fichier de la flèche est
  fSouthArrow = 'SouthArrow';               /// Fichier de la flèche sud
  fWestArrow = 'WestArrow';                 /// Fichier de la flèche ouest

  fTransporter = 'Transporter';             /// Fichier du téléporteur inactif
  fUpStairs = 'UpStairs';                   /// Fichier de l'escalier montant
  fDownStairs = 'DownStairs';               /// Fichier de l'escalier descendant
  fDirectTurnstile = 'DirectTurnstile';     /// Fichier du tourniquet direct
  fIndirectTurnstile = 'IndirectTurnstile'; /// Fichier du tourniquet indirect
  fOutside = 'Outside';                     /// Fichier du dehors

resourcestring
  sCantUsePlank = 'Impossible de passer ici avec la planche.';
  sCantGoOnHole = 'T''es pas bien de vouloir sauter dans ce trou !?';
  sCantGoOnWater = 'Sans bouée, on coule dans l''eau.';
  sCantOpenSilverBlock = 'Ce bloc ne disparaîtra qu''avec une clef d''argent.';
  sCantOpenGoldenBlock = 'Ce bloc ne disparaîtra qu''avec une clef d''or.';
  sCantGoOnSky = 'Tu ne peux pas voler !';

  sFoundBuoy      = 'Tu as trouvé une bouée.'+#10+
                    'Tu peux aller dans l''eau.';
  sFoundPlank     = 'Tu as trouvé une planche.'+#10+
                    'Tu peux franchir certains obstacles.';
  sFoundSilverKey = 'Tu as trouvé une clef d''argent.'+#10+
                    'Tu peux faire disparaître un bloc en argent.';
  sFoundGoldenKey = 'Tu as trouvé une clef d''or.'+#10+
                    'Tu peux faire disparaître un bloc en or.';

type
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

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
    Le mur est un terrain qui bloque systématiquement le joueur.
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
    Eau
    L'eau est un terrain sur lequel on peut aller avec une bouée ou une barque,
    et au-dessus duquel on peut passer avec une planche.
  *}
  TWater = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Trou
    Le trou est un terrain au-dessus duquel on peut passer avec une planche, et
    sinon inaccessible.
  *}
  THole = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Bloc en argent
    Le bloc en argent peut être détruit au moyen d'une clef en argent.
  *}
  TSilverBlock = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Bloc en or
    Le bloc en or peut être détruit au moyen d'une clef en or.
  *}
  TGoldenBlock = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Ciel
    Le ciel est toujours inaccessible.
  *}
  TSky = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); override;
  end;

  {*
    Flèche (de toutes directions)
    Les flèches repoussent le joueur dans la direction qui leur est propre.
  *}
  TArrow = class(TEffect)
  private
    FDirection : TDirection; /// Direction de la flèche
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADirection : TDirection);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Direction : TDirection read FDirection;
  end;

  {*
    Téléporteur
    Le téléporteur emmène le joueur à un autre téléporteur
  *}
  TTransporter = class(TEffect)
  private
    FKind : TTransporterKind; /// Type de téléporteur

    procedure FindNext(Map : TMap; var Pos : T3DPoint);
    procedure FindPrevious(Map : TMap; var Pos : T3DPoint);
    procedure FindRandom(Map : TMap; var Pos : T3DPoint);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AKind : TTransporterKind = tkInactive);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Kind : TTransporterKind read FKind;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'à
    parvenir à en sortir.
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
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'à
    parvenir à en sortir.
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
    Le dehors représente l'extérieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
  *}
  TOutside = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

procedure LoadCoreComponents(Master : TMaster);

implementation

{*
  Charge tous les composants au coeur de FunLabyrinthe
  @param Master   Maître FunLabyrinthe dans lequel charger les composants
*}
procedure LoadCoreComponents(Master : TMaster);
var I : integer;
begin
  // Terrains
  TGrass.Create(Master, idGrass, sGrass);
  TWall.Create(Master, idWall, sWall);
  TWater.Create(Master, idWater, sWater);
  THole.Create(Master, idHole, sHole);
  TSilverBlock.Create(Master, idSilverBlock, sSilverBlock);
  TGoldenBlock.Create(Master, idGoldenBlock, sGoldenBlock);
  TSky.Create(Master, idSky, sSky);

  // Effets de case
  TEffect.Create(Master, idNoEffect, '');

  TArrow.Create(Master, idNorthArrow, sNorthArrow, diNorth);
  TArrow.Create(Master, idEastArrow , sEastArrow , diEast );
  TArrow.Create(Master, idSouthArrow, sSouthArrow, diSouth);
  TArrow.Create(Master, idWestArrow , sWestArrow , diWest );

  TTransporter.Create(Master, idInactiveTransporter, sInactiveTransporter);
  for I := 1 to 15 do
    TTransporter.Create(Master, Format(idTransporterNext, [I]),
      Format(sTransporterNext, [I]), tkNext);
  for I := 1 to 15 do
    TTransporter.Create(Master, Format(idTransporterPrev, [I]),
      Format(sTransporterPrev, [I]), tkPrevious);
  for I := 1 to 15 do
    TTransporter.Create(Master, Format(idTransporterRandom, [I]),
      Format(sTransporterRandom, [I]), tkRandom);

  TDirectTurnstile.Create(Master, idDirectTurnstile, sDirectTurnstile);
  TIndirectTurnstile.Create(Master, idIndirectTurnstile, sIndirectTurnstile);

  TOutside.Create(Master, idOutside, sOutside);
end;

/////////////////////
/// Classe TGrass ///
/////////////////////

{*
  Crée une instance de TGrass
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
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
  Crée une instance de TWall
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TWall.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fWall);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TWall.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  Cancel := True;
end;

/////////////////////
/// Classe TWater ///
/////////////////////

{*
  Crée une instance de TWater
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TWater.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fWater);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TWater.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  { TODO 1 : Implémenter TWater.Entering }
  Cancel := True;
end;

////////////////////
/// Classe THole ///
////////////////////

{*
  Crée une instance de THole
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor THole.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fHole);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure THole.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  { TODO 1 : Implémenter THole.Entering }
  Cancel := True;
end;

///////////////////////////
/// Classe TSilverBlock ///
///////////////////////////

{*
  Crée une instance de TSilverBlock
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TSilverBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fSilverBlock);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TSilverBlock.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  Cancel := True;
end;

///////////////////////////
/// Classe TGoldenBlock ///
///////////////////////////

{*
  Crée une instance de TGoldenBlock
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TGoldenBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fGoldenBlock);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TGoldenBlock.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  Cancel := True;
end;

///////////////////
/// Classe TSky ///
///////////////////

{*
  Crée une instance de TSky
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TSky.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fSky);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TSky.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
  if KeyPressed then
    Player.ShowDialog(sBlindAlley, sCantGoOnSky, dtError);
  Cancel := True;
end;

/////////////////////
/// Classe TArrow ///
/////////////////////

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
    diNorth : Painter.ImgNames.Add(fNorthArrow);
    diEast  : Painter.ImgNames.Add(fEastArrow);
    diSouth : Painter.ImgNames.Add(fSouthArrow);
    diWest  : Painter.ImgNames.Add(fWestArrow);
  end;
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
procedure TArrow.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  Player.Direction := FDirection;
  GoOnMoving := True;
end;

///////////////////////////
/// Classe TTransporter ///
///////////////////////////

{*
  Crée une instance de TTransporter
  @param AMaster      Maître FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AKind        Type de téléporteur
*}
constructor TTransporter.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AKind : TTransporterKind = tkInactive);
begin
  inherited Create(AMaster, AID, AName);
  FKind := AKind;
  Painter.ImgNames.Add(fTransporter);
end;

{*
  Trouve le téléporteur suivant sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du téléporteur initiale en entrée et finale en sortie
*}
procedure TTransporter.FindNext(Map : TMap; var Pos : T3DPoint);
var DimX, DimY, DimZ : integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    inc(Pos.X);
    if Pos.X >= DimX then
    begin
      Pos.X := 0;
      inc(Pos.Y);
      if Pos.Y >= DimY then
      begin
        Pos.Y := 0;
        inc(Pos.Z);
        if Pos.Z >= DimZ then
          Pos.Z := 0;
      end;
    end;
  until Map[Pos].Effect = Self;
end;

{*
  Trouve le téléporteur précédent sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du téléporteur initiale en entrée et finale en sortie
*}
procedure TTransporter.FindPrevious(Map : TMap; var Pos : T3DPoint);
var DimX, DimY, DimZ : integer;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  repeat
    dec(Pos.X);
    if Pos.X < 0 then
    begin
      Pos.X := DimX-1;
      dec(Pos.Y);
      if Pos.Y < 0 then
      begin
        Pos.Y := DimY-1;
        dec(Pos.Z);
        if Pos.Z < 0 then
          Pos.Z := DimZ-1;
      end;
    end;
  until Map[Pos].Effect = Self;
end;

{*
  Trouve un autre téléporteur aléatoirement sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du téléporteur initiale en entrée et finale en sortie
*}
procedure TTransporter.FindRandom(Map : TMap; var Pos : T3DPoint);
const
  AllocBy = 10;
var DimX, DimY, DimZ : integer;
    Others : array of T3DPoint;
    Count, X, Y, Z : integer;
    Other : T3DPoint;
begin
  with Map.Dimensions do
  begin
    DimX := X;
    DimY := Y;
    DimZ := Z;
  end;

  // Recensement de toutes les cases identiques, à l'exception de l'originale
  Count := 0;
  SetLength(Others, AllocBy);
  for X := 0 to DimX-1 do for Y := 0 to DimY-1 do for Z := 0 to DimZ-1 do
  begin
    Other := Point3D(X, Y, Z);
    if (Map[Other].Effect = Self) and (not Same3DPoint(Other, Pos)) then
    begin
      if Count >= Length(Others) then
        SetLength(Others, Count+AllocBy);
      Others[Count] := Other;
      inc(Count);
    end;
  end;
  SetLength(Others, Count);

  // À moins que la liste soit vide, on en pêche un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
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
procedure TTransporter.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;

  // Recherche de la case de destination
  case FKind of
    tkNext     : FindNext    (Player.Map, Other);
    tkPrevious : FindPrevious(Player.Map, Other);
    tkRandom   : FindRandom  (Player.Map, Other);
    else exit; // on évite des tests inutiles pour un inactif
  end;

  // Si l'on a trouvé une autre case, on déplace le joueur
  if Same3DPoint(Other, Pos) then exit;
  Sleep(500);
  Player.Position := Other;
end;

///////////////////////////////
/// Classe TDirectTurnstile ///
///////////////////////////////

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

  Sleep(500);

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
var FieldID : TComponentID;
begin
  inherited;
  FieldID := Player.Map[Pos].Field.ID;
  Player.Map[Pos] := Master.Screw[FieldID+'-'+idIndirectTurnstile];
end;

/////////////////////////////////
/// Classe TIndirectTurnstile ///
/////////////////////////////////

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

  Sleep(500);

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
var FieldID : TComponentID;
begin
  inherited;
  FieldID := Player.Map[Pos].Field.ID;
  Player.Map[Pos] := Master.Screw[FieldID+'-'+idDirectTurnstile];
end;

///////////////////////
/// Classe TOutside ///
///////////////////////

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

