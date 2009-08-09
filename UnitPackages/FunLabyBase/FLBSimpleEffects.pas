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
  SysUtils, Graphics, ScUtils, SdDialogs, FunLabyUtils, FunLabyCoreConsts,
  MapTools, GraphicsTools, FLBCommon, FLBFields, GR32;

resourcestring
  sNorthArrow = 'Fl�che nord';                /// Nom de la fl�che nord
  sEastArrow = 'Fl�che est';                  /// Nom de la fl�che est
  sSouthArrow = 'Fl�che sud';                 /// Nom de la fl�che sud
  sWestArrow = 'Fl�che ouest';                /// Nom de la fl�che ouest
  sCrossroads = 'Carrefour';                  /// Nom du carrefour

  sInactiveTransporter = 'T�l�porteur inactif'; /// T�l�porteur inactif
  sTransporter = 'T�l�porteur';                 /// T�l�porteur

  sUpStairs = 'Escalier montant';             /// Nom de l'escalier montant
  sDownStairs = 'Escalier descendant';        /// Nom de l'escalier descendant

  sDirectTurnstile = 'Tourniquet direct';     /// Nom du tourniquet direct
  sIndirectTurnstile = 'Tourniquet indirect'; /// Nom du tourniquet indirect

  sTreasure = 'Tr�sor';                       /// Nom du tr�sor

  sSunkenButton = 'Bouton enfonc�';           /// Nom du bouton enfonc�

resourcestring
  STransporterCreatorHint = 'Cr�er un nouveau t�l�porteur';

const {don't localize}
  idNorthArrow = 'NorthArrow';                   /// ID de la fl�che nord
  idEastArrow = 'EastArrow';                     /// ID de la fl�che est
  idSouthArrow = 'SouthArrow';                   /// ID de la fl�che sud
  idWestArrow = 'WestArrow';                     /// ID de la fl�che ouest
  idCrossroads = 'Crossroads';                   /// ID du carrefour

  idInactiveTransporter = 'InactiveTransporter'; /// ID du t�l�porteur inactif

  /// ID du cr�ateur de t�l�porteurs
  idTransporterCreator = 'TransporterCreator';

  idUpStairs = 'UpStairs';                       /// ID de l'escalier montant
  idDownStairs = 'DownStairs';                   /// ID de l'escalier descendant

  idDirectTurnstile = 'DirectTurnstile';         /// ID du tourniquet direct
  idIndirectTurnstile = 'IndirectTurnstile';     /// ID du tourniquet indirect

  idTreasure = 'Treasure';                       /// ID du tr�sor

  idSunkenButton = 'SunkenButton';               /// ID du bouton enfonc�

const {don't localize}
  fNorthArrow = 'Arrows/NorthArrow'; /// Fichier de la fl�che nord
  fEastArrow = 'Arrows/EastArrow';   /// Fichier de la fl�che est
  fSouthArrow = 'Arrows/SouthArrow'; /// Fichier de la fl�che sud
  fWestArrow = 'Arrows/WestArrow';   /// Fichier de la fl�che ouest
  fCrossroads = 'Arrows/Crossroads'; /// Fichier du carrefour

  fTransporter = 'Miscellaneous/Transporter'; /// Fichier du t�l�porteur

  fUpStairs = 'Stairs/UpStairs';     /// Fichier de l'escalier montant
  fDownStairs = 'Stairs/DownStairs'; /// Fichier de l'escalier descendant

  /// Fichier du tourniquet direct
  fDirectTurnstile = 'Arrows/DirectTurnstile';
  /// Fichier du tourniquet indirect
  fIndirectTurnstile = 'Arrows/IndirectTurnstile';

  fTreasure = 'Chests/Treasure'; /// Fichier du tr�sor

resourcestring
  sFoundTreasure = 'BRAVO ! Tu as trouv� le tr�sor !';

resourcestring
  sPlaceStairsTitle = 'Placement d''un escalier';
  sCantPlaceUpStairsAtLastFloor =
    'Impossible de placer un escalier montant au dernier �tage';
  sCantPlaceDownStairsAtFirstFloor =
    'Impossible de placer un escalier descendant au rez-de-chauss�e';
  sDifferentStairsDestSquare =
    'La case � l''autre bout de l''escalier n''est pas la m�me que celle-ci. '+
    '�tes-vous s�r de vouloir placer un escalier ici ?';

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
    FKind: TTransporterKind; /// Type de t�l�porteur
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Execute(Context: TMoveContext); override;
  published
    property Kind: TTransporterKind read FKind write FKind default tkNext;
  end;

  {*
    T�l�porteur inactif
    @author sjrd
    @version 1.0
  *}
  TInactiveTransporter = class(TTransporter)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);
  protected
    function GetCategory: string; override;
  published
    property Kind default tkInactive;
  end;

  {*
    Cr�ateur de t�l�porteurs
    @author sjrd
    @version 5.0
  *}
  TTransporterCreator = class(TComponentCreator)
  protected
    function GetHint: string; override;

    function DoCreateComponent(
      const AID: TComponentID): TFunLabyComponent; override;
  public
    constructor Create(AMaster: TMaster; AID: TComponentID);
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

    procedure EditSquareMap(var Msg: TEditMapSquareMessage);
      message msgEditMapSquare;
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
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du t�l�porteur
  @param AName     Nom du t�l�porteur
*}
constructor TTransporter.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  FKind := tkNext;

  Painter.ImgNames.Add(fTransporter);
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

{-----------------------------}
{ Classe TInactiveTransporter }
{-----------------------------}

{*
  Cr�e une instance de TInactiveTransporter
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du t�l�porteur
  @param AName     Nom du t�l�porteur
*}
constructor TInactiveTransporter.Create(AMaster: TMaster;
  const AID: TComponentID; const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Kind := tkInactive;
end;

{*
  [@inheritDoc]
*}
function TInactiveTransporter.GetCategory: string;
begin
  Result := SCategoryNeutrals;
end;

{---------------------------}
{ TTransporterCreator class }
{---------------------------}

{*
  Cr�e un nouveau cr�ateur de t�l�porteurs
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du cr�ateur de t�l�porteurs
*}
constructor TTransporterCreator.Create(AMaster: TMaster; AID: TComponentID);
begin
  inherited Create(AMaster, AID);

  IconPainter.ImgNames.Add(fTransporter);
end;

{*
  [@inheritDoc]
*}
function TTransporterCreator.GetHint: string;
begin
  Result := STransporterCreatorHint;
end;

{*
  [@inheritDoc]
*}
function TTransporterCreator.DoCreateComponent(
  const AID: TComponentID): TFunLabyComponent;
begin
  Result := TTransporter.Create(Master, AID, sTransporter);
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
  D�clench� en �dition lorsqu'une case est modifi�e avec un escalier
  @param Msg   Message
*}
procedure TStairs.EditSquareMap(var Msg: TEditMapSquareMessage);
var
  Other: T3DPoint;
begin
  with Msg, QPos do
  begin
    Other := Position;
    if Up then
      Inc(Other.Z)
    else
      Dec(Other.Z);

    if (Phase = espAdd) and (not Map.InMap(Other)) then
    begin
      if Up then
        ShowDialog(sPlaceStairsTitle, sCantPlaceUpStairsAtLastFloor, dtError)
      else
        ShowDialog(sPlaceStairsTitle, sCantPlaceDownStairsAtFirstFloor,
          dtError);

      Include(Flags, esfCancel);
      Exit;
    end;

    if not Map.InMap(Other) then
      Exit;

    if Phase = espRemove then
    begin
      Map[Position] := RemoveEffect(Map[Position]);
      Map[Other] := RemoveEffect(Map[Other]);
      Include(Flags, esfHandled);
    end;

    if Phase = espAdd then
    begin
      if Map[Other] <> Map[Position] then
      begin
        if ShowDialog(sPlaceStairsTitle, sDifferentStairsDestSquare,
          dtConfirmation, dbOKCancel) <> drOK then
        begin
          Include(Flags, esfCancel);
          Exit;
        end;
      end;

      Map[Position] := ChangeEffect(Map[Position], Self);
      if Up then
        Map[Other] := ChangeEffect(Map[Other], idDownStairs)
      else
        Map[Other] := ChangeEffect(Map[Other], idUpStairs);

      Include(Flags, esfHandled);
    end;
  end;
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
    ShowMessage(sFoundTreasure);
  end;
end;

end.

