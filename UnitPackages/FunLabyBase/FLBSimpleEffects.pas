{*
  D�crit les effets simples (se suffisant � eux-m�mes)
  L'unit� FLBSimpleEffects regroupe les diff�rents effets qui se suffisent �
  eux-m�mes, c'est-�-dire qui n'ont pas besoin de composants annexes pour
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

  idOutsideSquare = idGrass+'-'+idOutside+'--';   /// ID de la case dehors

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
  sFoundTreasure = 'BRAVO ! Tu as trouv� le tr�sor !';

  sTransporterTitle = 'Num�ro du t�l�porteur';
  sTransporterPrompt = 'Num�ro du t�l�porteur (0 � 45) :';

type
  {*
    Type de t�l�porteur
    @author sjrd
    @version 5.0
  *}
  TTransporterKind = (tkInactive, tkNext, tkPrevious, tkRandom);

  {*
    Fl�che (de toutes directions)
    Les fl�ches repoussent le joueur dans la direction qui leur est propre. Le
    carrefour en est un cas particulier qui laisse inchang�e la direction du
    joueur.
    @author sjrd
    @version 5.0
  *}
  TArrow = class(TEffect)
  private
    FDirection: TDirection; /// Direction de la fl�che
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADirection: TDirection);

    procedure Execute(Context: TMoveContext); override;

    property Direction: TDirection read FDirection;
  end;

  {*
    T�l�porteur
    Le t�l�porteur emm�ne le joueur � un autre t�l�porteur
    @author sjrd
    @version 5.0
  *}
  TTransporter = class(TEffect)
  private
    FNumber: Integer;        /// Num�ro du t�l�porteur
    FKind: TTransporterKind; /// Type de t�l�porteur
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
    Les escaliers permettent de monter ou descendre d'un �tage
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
    Le tourniquet direct fait tourner le joueur dans le sens direct jusqu'�
    parvenir � en sortir.
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
    Le tourniquet indirect fait tourner le joueur dans le sens indirect jusqu'�
    parvenir � en sortir.
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
    Le dehors repr�sente l'ext�rieur du labyrinthe et fait remporter la victoire
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
    Tr�sor
    Le tr�sor fait remporter la victoire au joueur qui le trouve.
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
  Cr�e une instance de TArrow
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param ADirection   Direction de la fl�che
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
  Cr�e une instance de TTransporter
  @param AMaster      Ma�tre FunLabyrinthe
  @param AID          ID de l'effet de case
  @param AName        Nom de la case
  @param AKind        Type de t�l�porteur
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
      Exit; // on �vite des tests inutiles pour un inactif
    end;

    // Si l'on a trouv� une autre case, on d�place le joueur
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
  Cr�e une instance de TStairs
  @param AMaster      Ma�tre FunLabyrinthe
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
  Cr�e une instance de TDirectTurnstile
  @param AMaster   Ma�tre FunLabyrinthe
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
  Cr�e une instance de TIndirectTurnstile
  @param AMaster   Ma�tre FunLabyrinthe
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
  Cr�e une instance de TOutside
  @param AMaster   Ma�tre FunLabyrinthe
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
  Cr�e une instance de TTreasure
  @param AMaster   Ma�tre FunLabyrinthe
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

