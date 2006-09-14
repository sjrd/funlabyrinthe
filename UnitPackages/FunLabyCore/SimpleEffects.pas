{*
  D�crit les effets simples (se suffisant � eux-m�mes)
  L'unit� SimpleEffects regroupe les diff�rents effets qui se suffisent �
  eux-m�mes, c'est-�-dire qui n'ont pas besoin de composants annexes pour
  fonctionner.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit SimpleEffects;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, Common, Fields;

resourcestring
  sNorthArrow = 'Fl�che nord';                /// Nom de la fl�che nord
  sEastArrow = 'Fl�che est';                  /// Nom de la fl�che est
  sSouthArrow = 'Fl�che sud';                 /// Nom de la fl�che sud
  sWestArrow = 'Fl�che ouest';                /// Nom de la fl�che ouest
  sCrossroads = 'Carrefour';                  /// Nom du carrefour

  sInactiveTransporter = 'T�l�porteur inactif';      /// T�l�porteur inactif
  sTransporterNext = 'T�l�porteur suivant n�%d';     /// T�l�porteur suivant
  sTransporterPrev = 'T�l�porteur pr�c�dent n�%d';   /// T�l�porteur pr�c�dent
  sTransporterRandom = 'T�l�porteur al�atoire n�%d'; /// T�l�porteur al�atoire
  sTransporterTemplate = 'T�l�porteur';              /// T�l�porteur mod�le

  sUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  sDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant

  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect

  sOutside = 'Dehors';                        /// Nom du dehors
  sTreasure = 'Tr�sor';                       /// Nom du tr�sor

const {don't localize}
  idNorthArrow = 'NorthArrow';                   /// ID de la fl�che nord
  idEastArrow = 'EastArrow';                     /// ID de la fl�che est
  idSouthArrow = 'SouthArrow';                   /// ID de la fl�che sud
  idWestArrow = 'WestArrow';                     /// ID de la fl�che ouest
  idCrossroads = 'Crossroads';                   /// ID du carrefour

  idInactiveTransporter = 'InactiveTransporter'; /// ID du t�l�porteur inactif
  idTransporterNext = 'TransporterNext%d';       /// ID du t�l�porteur suivant
  idTransporterPrev = 'TransporterPrev%d';       /// ID du t�l�porteur pr�c�dent
  idTransporterRandom = 'TransporterRandom%d';   /// ID du t�l�porteur al�atoire
  idTransporterTemplate = 'TransporterTemplate'; /// ID du t�l�porteur mod�le

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant

  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect

  idOutside = 'Outside';                         /// ID du dehors
  idTreasure = 'Treasure';                       /// ID du tr�sor

  idOutsideScrew = idGrass+'-'+idOutside+'-';    /// ID de la case dehors

const {don't localize}
  fNorthArrow = 'NorthArrow';               /// Fichier de la fl�che nord
  fEastArrow = 'EastArrow';                 /// Fichier de la fl�che est
  fSouthArrow = 'SouthArrow';               /// Fichier de la fl�che sud
  fWestArrow = 'WestArrow';                 /// Fichier de la fl�che ouest
  fCrossroads = 'Crossroads';               /// Fichier du carrefour

  fTransporter = 'Transporter';             /// Fichier du t�l�porteur

  fUpStairs = 'UpStairs';                   /// Fichier de l'escalier montant
  fDownStairs = 'DownStairs';               /// Fichier de l'escalier descendant

  fDirectTurnstile = 'DirectTurnstile';     /// Fichier du tourniquet direct
  fIndirectTurnstile = 'IndirectTurnstile'; /// Fichier du tourniquet indirect

  fOutside = 'Outside';                     /// Fichier du dehors
  fTreasure = 'Treasure';                   /// Fichier du tr�sor

resourcestring
  sGotOutsideMaze = 'BRAVO ! Tu as r�ussi � sortir du labyrinthe !';
  sFoundTreasure  = 'BRAVO ! Tu as trouv� le tr�sor !';

  sTransporterTitle = 'Num�ro du t�l�porteur';
  sTransporterPrompt = 'Num�ro du t�l�porteur (0 � 45) :';

type
  {*
    Type de t�l�porteur
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Fl�che (de toutes directions)
    Les fl�ches repoussent le joueur dans la direction qui leur est propre. Le
    carrefour en est un cas particulier qui laisse inchang�e la direction du
    joueur.
  *}
  TArrow = class(TEffect)
  private
    FDirection : TDirection; /// Direction de la fl�che
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADirection : TDirection);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Direction : TDirection read FDirection;
  end;

  {*
    T�l�porteur
    Le t�l�porteur emm�ne le joueur � un autre t�l�porteur
  *}
  TTransporter = class(TEffect)
  private
    FNumber : integer;        /// Num�ro du t�l�porteur
    FKind : TTransporterKind; /// Type de t�l�porteur

    procedure FindNext(Map : TMap; var Pos : T3DPoint);
    procedure FindPrevious(Map : TMap; var Pos : T3DPoint);
    procedure FindRandom(Map : TMap; var Pos : T3DPoint);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ANumber : integer;
      AKind : TTransporterKind = tkInactive);

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Number : integer read FNumber;
    property Kind : TTransporterKind read FKind;
  end;

  {*
    Escaliers
    Les escaliers permettent de monter ou descendre d'un �tage
  *}
  TStairs = class(TEffect)
  private
    FUp : boolean; /// Indique si l'escalier est montant
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AUp : boolean);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;

    property Up : boolean read FUp;
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

  {*
    Tr�sor
    Le tr�sor fait remporter la victoire au joueur qui le trouve.
  *}
  TTreasure = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

implementation

/////////////////////
/// Classe TArrow ///
/////////////////////

{*
  Cr�e une instance de TArrow
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param ADirection   Direction de la fl�che
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TArrow.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  if FDirection <> diNone then
    Player.Direction := FDirection;
  GoOnMoving := True;
end;

///////////////////////////
/// Classe TTransporter ///
///////////////////////////

{*
  Cr�e une instance de TTransporter
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AKind        Type de t�l�porteur
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
  Trouve le t�l�porteur suivant sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du t�l�porteur initiale en entr�e et finale en sortie
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
  Trouve le t�l�porteur pr�c�dent sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du t�l�porteur initiale en entr�e et finale en sortie
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
  Trouve un autre t�l�porteur al�atoirement sur la carte
  @param Map   Carte sur laquelle chercher
  @param Pos   Position du t�l�porteur initiale en entr�e et finale en sortie
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

  // Recensement de toutes les cases identiques, � l'exception de l'originale
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

  // � moins que la liste soit vide, on en p�che un au hasard
  if Count > 0 then
    Pos := Others[Random(Count)];
end;

{*
  Dessine le t�l�porteur sur le canevas indiqu�
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TTransporter.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  inherited;

  if Master.Editing then
  begin
    case FKind of
      tkNext     : DrawNumber(Canvas, X, Y, Number, clRed);
      tkPrevious : DrawNumber(Canvas, X, Y, Number, clGreen);
      tkRandom   : DrawNumber(Canvas, X, Y, Number, clBlue);
    end;
  end;
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
    else exit; // on �vite des tests inutiles pour un inactif
  end;

  // Si l'on a trouv� une autre case, on d�place le joueur
  if Same3DPoint(Other, Pos) then exit;
  Sleep(500);
  Player.Position := Other;
end;

//////////////////////
/// Classe TStairs ///
//////////////////////

{*
  Cr�e une instance de TStairs
  @param AMaster      Ma�tre FunLabyrinthe
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TStairs.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
var Other : T3DPoint;
begin
  inherited;

  Other := Pos;
  if Up then
    inc(Other.Z)
  else
    dec(Other.Z);

  Sleep(500);
  Player.Position := Other;
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
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect(idIndirectTurnstile);
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
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect(idDirectTurnstile);
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
  Player.Win;
  Player.Controller.ShowDialog(sWon, sGotOutsideMaze);
end;

////////////////////////
/// Classe TTreasure ///
////////////////////////

{*
  Cr�e une instance de TTreasure
  @param AMaster   Ma�tre FunLabyrinthe
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
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TTreasure.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;
  Player.Win;
  Player.Controller.ShowDialog(sWon, sFoundTreasure);
end;

end.

