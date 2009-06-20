{*
  Décrit les effets simples (se suffisant à eux-mêmes)
  L'unité FLBSimpleEffects regroupe les différents effets qui se suffisent à
  eux-mêmes, c'est-à-dire qui n'ont pas besoin de composants annexes pour
  fonctionner.
  @author sjrd
  @version 5.0
*}
unit FLBSimpleEffects;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, MapTools, GraphicsTools, FLBCommon,
  FLBFields;

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

  idOutsideSquare = idGrass+'-'+idOutside+'--';   /// ID de la case dehors

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
  sFoundTreasure = 'BRAVO ! Tu as trouvé le trésor !';

  sTransporterTitle = 'Numéro du téléporteur';
  sTransporterPrompt = 'Numéro du téléporteur (0 à 45) :';

type
  {*
    Type de téléporteur
    @author sjrd
    @version 5.0
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Flèche (de toutes directions)
    Les flèches repoussent le joueur dans la direction qui leur est propre. Le
    carrefour en est un cas particulier qui laisse inchangée la direction du
    joueur.
    @author sjrd
    @version 5.0
  *}
  TArrow = class(TEffect)
  private
    FDirection: TDirection; /// Direction de la flèche
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADirection: TDirection);

    procedure Execute(Context: TMoveContext); override;

    property Direction: TDirection read FDirection;
  end;

  {*
    Téléporteur
    Le téléporteur emmène le joueur à un autre téléporteur
    @author sjrd
    @version 5.0
  *}
  TTransporter = class(TEffect)
  private
    FNumber: Integer;        /// Numéro du téléporteur
    FKind: TTransporterKind; /// Type de téléporteur
  protected
    procedure DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
      X: Integer = 0; Y: Integer = 0); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ANumber: Integer;
      AKind: TTransporterKind = tkInactive);

    procedure Execute(Context: TMoveContext); override;

    property Number: Integer read FNumber;
    property Kind: TTransporterKind read FKind;
  end;

  {*
    Escaliers
    Les escaliers permettent de monter ou descendre d'un étage
    @author sjrd
    @version 5.0
  *}
  TStairs = class(TEffect)
  private
    FUp: Boolean; /// Indique si l'escalier est montant
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; AUp: Boolean);

    procedure Execute(Context: TMoveContext); override;

    property Up: Boolean read FUp;
  end;

  {*
    Tourniquet Direct
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'à
    parvenir à en sortir.
    @author sjrd
    @version 5.0
  *}
  TDirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Exited(Context: TMoveContext); override;

    procedure Execute(Context: TMoveContext); override;
  end;

  {*
    Tourniquet Indirect
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'à
    parvenir à en sortir.
    @author sjrd
    @version 5.0
  *}
  TIndirectTurnstile = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Exited(Context: TMoveContext); override;

    procedure Execute(Context: TMoveContext); override;
  end;

  {*
    Dehors
    Le dehors représente l'extérieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
    @author sjrd
    @version 5.0
  *}
  TOutside = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Execute(Context: TMoveContext); override;
  end;

  {*
    Trésor
    Le trésor fait remporter la victoire au joueur qui le trouve.
    @author sjrd
    @version 5.0
  *}
  TTreasure = class(TEffect)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Execute(Context: TMoveContext); override;
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
constructor TArrow.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADirection: TDirection);
begin
  inherited Create(AMaster, AID, AName);

  FDirection := ADirection;
  case FDirection of
    diNone:  Painter.ImgNames.Add(fCrossroads);
    diNorth: Painter.ImgNames.Add(fNorthArrow);
    diEast:  Painter.ImgNames.Add(fEastArrow);
    diSouth: Painter.ImgNames.Add(fSouthArrow);
    diWest:  Painter.ImgNames.Add(fWestArrow);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TArrow.Execute(Context: TMoveContext);
begin
  with Context do
  begin
    if Direction <> diNone then
      Player.Direction := FDirection;
    GoOnMoving := True;
  end;
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
constructor TTransporter.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ANumber: Integer;
  AKind: TTransporterKind = tkInactive);
begin
  inherited Create(AMaster, Format(AID, [ANumber]), Format(AName, [ANumber]));

  FNumber := ANumber;
  FKind := AKind;
  Painter.ImgNames.Add(fTransporter);
end;

{*
  [@inheritDoc]
*}
procedure TTransporter.DoDraw(const QPos: TQualifiedPos; Canvas: TCanvas;
  X: Integer = 0; Y: Integer = 0);
begin
  inherited;

  if Master.Editing then
  begin
    case FKind of
      tkNext:     DrawSquareNumber(Canvas, X, Y, Number, clRed);
      tkPrevious: DrawSquareNumber(Canvas, X, Y, Number, clGreen);
      tkRandom:   DrawSquareNumber(Canvas, X, Y, Number, clBlue);
    end;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TTransporter.Execute(Context: TMoveContext);
var
  Other: T3DPoint;
begin
  with Context do
  begin
    Other := Pos;

    // Recherche de la case de destination
    case FKind of
      tkNext:     FindNextSquare(Map, Other, Self);
      tkPrevious: FindPreviousSquare(Map, Other, Self);
      tkRandom:   FindSquareAtRandom(Map, Other, Self);
    else
      Exit; // on évite des tests inutiles pour un inactif
    end;

    // Si l'on a trouvé une autre case, on déplace le joueur
    if Same3DPoint(Other, Pos) then
      Exit;
    Master.Temporize;
    Player.MoveTo(Other);
  end;
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
constructor TStairs.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; AUp: Boolean);
begin
  inherited Create(AMaster, AID, AName);

  FUp := AUp;
  if Up then
    Painter.ImgNames.Add(fUpStairs)
  else
    Painter.ImgNames.Add(fDownStairs);
end;

{*
  [@inheritDoc]
*}
procedure TStairs.Execute(Context: TMoveContext);
var
  Other: T3DPoint;
begin
  Other := Context.Pos;
  if Up then
    Inc(Other.Z)
  else
    Dec(Other.Z);

  Master.Temporize;
  Context.Player.MoveTo(Other);
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
constructor TDirectTurnstile.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fDirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TDirectTurnstile.Exited(Context: TMoveContext);
begin
  with Context do
    Square := ChangeEffect(Square, idIndirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TDirectTurnstile.Execute(Context: TMoveContext);
var
  Dir: TDirection;
  Redo: Boolean;
begin
  with Context do
  begin
    Master.Temporize;

    if Player.Direction = diWest then
      Dir := diNorth
    else
      Dir := Succ(Player.Direction);

    repeat
      Player.Move(Dir, False, Redo);
      GoOnMoving := Redo;

      if Player.Direction = diNorth then
        Dir := diWest
      else
        Dir := Pred(Player.Direction);
    until not Same3DPoint(Player.Position, Pos);
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
constructor TIndirectTurnstile.Create(AMaster: TMaster;
  const AID: TComponentID; const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fIndirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TIndirectTurnstile.Exited(Context: TMoveContext);
begin
  with Context do
    Square := ChangeEffect(Square, idDirectTurnstile);
end;

{*
  [@inheritDoc]
*}
procedure TIndirectTurnstile.Execute(Context: TMoveContext);
var
  Dir: TDirection;
  Redo: Boolean;
begin
  with Context do
  begin
    Master.Temporize;

    if Player.Direction = diNorth then
      Dir := diWest
    else
      Dir := Pred(Player.Direction);

    repeat
      Player.Move(Dir, False, Redo);
      GoOnMoving := Redo;

      if Player.Direction = diWest then
        Dir := diNorth
      else
        Dir := Succ(Player.Direction);
    until not Same3DPoint(Player.Position, Pos);
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
constructor TOutside.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fOutside);
end;

{*
  [@inheritDoc]
*}
procedure TOutside.Execute(Context: TMoveContext);
begin
  with Context.Player do
  begin
    Win;
    ShowDialog(sWon, sGotOutsideMaze);
  end;
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
constructor TTreasure.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fTreasure);
end;

{*
  [@inheritDoc]
*}
procedure TTreasure.Execute(Context: TMoveContext);
begin
  with Context.Player do
  begin
    Win;
    ShowDialog(sWon, sFoundTreasure);
  end;
end;

end.

