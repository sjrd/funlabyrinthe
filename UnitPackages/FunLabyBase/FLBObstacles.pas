{*
  D�crit les obstacles standart de Funlabyrinthe
  L'unit� FLBObstacles regroupe les d�finitions des obstacles standart de
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FLBObstacles;

interface

uses
  SysUtils, Classes, Graphics, ScUtils, SdDialogs, FunLabyUtils, MapTools,
  FLBCommon, FLBFields;

resourcestring
  sSilverBlock = 'Bloc en argent'; /// Nom du bloc en argent
  sGoldenBlock = 'Bloc en or';     /// Nom du bloc en or
  sSecretWay = 'Passage secret';   /// Nom du passage secret

const {don't localize}
  idSilverBlock = 'SilverBlock'; /// ID du bloc en argent
  idGoldenBlock = 'GoldenBlock'; /// ID du bloc en or
  idSecretWay = 'SecretWay';     /// ID du passage secret

const {don't localize}
  fSilverBlock = 'SilverBlock'; /// Fichier du bloc en argent
  fGoldenBlock = 'GoldenBlock'; /// Fichier du bloc en or

resourcestring
  sCantOpenSilverBlock = 'Ce bloc ne dispara�tra qu''avec une clef d''argent.';
  sCantOpenGoldenBlock = 'Ce bloc ne dispara�tra qu''avec une clef d''or.';

type
  {*
    Bloc en argent
    Le bloc en argent peut �tre d�truit au moyen d'une clef en argent.
    @author sjrd
    @version 5.0
  *}
  TSilverBlock = class(TObstacle)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;
  end;

  {*
    Bloc en or
    Le bloc en or peut �tre d�truit au moyen d'une clef en or.
    @author sjrd
    @version 5.0
  *}
  TGoldenBlock = class(TObstacle)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;
  end;

  {*
    Passage secret
    Le passage secret a l'apparence d'un mur mais peut �tre ouvert sans rien.
    @author sjrd
    @version 5.0
  *}
  TSecretWay = class(TObstacle)
  protected
    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Pushing(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel, AbortExecute : boolean); override;
  end;

implementation

{---------------------}
{ Classe TSilverBlock }
{---------------------}

{*
  Cr�e une instance de TSilverBlock
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TSilverBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverBlock);
end;

{*
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Execute de la case ne soit ex�cut�e, il faut positionner
  AbortExecute � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortExecute   � positionner � True pour emp�cher le Execute
*}
procedure TSilverBlock.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  inherited;

  if KeyPressed then with Player do
  begin
    if DoAction(actOpenSilverLock) then
      Map[Pos] := ChangeObstacle(Map[Pos])
    else
      ShowDialog(sBlindAlley, sCantOpenSilverBlock, dtError);
  end;
end;

{---------------------}
{ Classe TGoldenBlock }
{---------------------}

{*
  Cr�e une instance de TGoldenBlock
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TGoldenBlock.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenBlock);
end;

{*
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Execute de la case ne soit ex�cut�e, il faut positionner
  AbortExecute � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortExecute   � positionner � True pour emp�cher le Execute
*}
procedure TGoldenBlock.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  inherited;

  if KeyPressed then with Player do
  begin
    if DoAction(actOpenGoldenLock) then
      Map[Pos] := ChangeObstacle(Map[Pos])
    else
      ShowDialog(sBlindAlley, sCantOpenGoldenBlock, dtError);
  end;
end;

{-------------------}
{ Classe TSecretWay }
{-------------------}

{*
  Cr�e une instance de TSecretWay
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TSecretWay.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fWall);
end;

{*
  Dessine le passage secret sur le canevas indiqu�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TSecretWay.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  inherited;

  if Master.Editing then with Canvas do
  begin
    Brush.Color := clWhite;
    Font.Color := clBlack;
    Font.Size := 12;
    Font.Style := [fsBold];
    Font.Name := 'Courier'; {don't localize}
    TextOut(X+10, Y+8, '!'); {don't localize}
  end;
end;

{*
  Ex�cut� lorsque le joueur pousse sur l'obstacle
  Pushing est ex�cut� lorsque le joueur pousse sur l'obstacle. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Execute de la case ne soit ex�cut�e, il faut positionner
  AbortExecute � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortExecute   � positionner � True pour emp�cher le Execute
*}
procedure TSecretWay.Pushing(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel, AbortExecute : boolean);
begin
  inherited;

  if KeyPressed then with Player do
    Map[Pos] := ChangeObstacle(Map[Pos]);
end;

end.

