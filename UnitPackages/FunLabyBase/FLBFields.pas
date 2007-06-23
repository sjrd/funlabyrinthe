{*
  Décrit les terrains de base de Funlabyrinthe
  L'unité FLBFields regroupe les définitions des terrains de base de
  FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FLBFields;

interface

uses
  Graphics, ScUtils, SdDialogs, FunLabyUtils, FLBCommon;

resourcestring
  sGrass = 'Herbe'; /// Nom de l'herbe
  sWall = 'Mur';    /// Nom du mur
  sWater = 'Eau';   /// Nom de l'eau
  sHole = 'Trou';   /// Nom du trou
  sSky = 'Ciel';    /// Nom du ciel

const {don't localize}
  idGrass = 'Grass';             /// ID de l'herbe
  idWall = 'Wall';               /// ID du mur
  idWater = 'Water';             /// ID de l'eau
  idHole = 'Hole';               /// ID du trou
  idSky = 'Sky';                 /// ID du ciel

  idGroundWater = 'GroundWater'; /// ID de l'eau effet herbe

const {don't localize}
  fGrass = 'Grass';                   /// Fichier de l'herbe
  fWall = 'Wall';                     /// Fichier du mur
  fWater = 'Water';                   /// Fichier de l'eau
  fAlternateWater = 'AlternateWater'; /// Fichier alternatif de l'eau
  fHole = 'Hole';                     /// Fichier du trou
  fSky = 'Sky';                       /// Fichier du ciel

resourcestring
  sCantGoOnWater = 'Sans bouée, on coule dans l''eau.';
  sCantGoOnHole = 'T''es pas bien de vouloir sauter dans ce trou !?';
  sCantGoOnSky = 'Tu ne peux pas voler !';

type
  {*
    Sol
    Le sol est le terrain de base de FunLabyrinthe. Il n'a pas de condition. On
    peut en créer plusieurs versions, avec des graphismes différents. Par
    défaut, il existe un type de sol : l'herbe.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TGround = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; const AImgName : string = fGrass;
      ADelegateDrawTo : TField = nil);
  end;

  {*
    Mur
    Le mur est un terrain qui bloque systématiquement le joueur.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TWall = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Eau
    L'eau est un terrain sur lequel on peut aller avec une bouée ou une barque,
    et au-dessus duquel on peut passer avec une planche.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TWater = class(TField)
  private
    FAlternatePainter : TPainter; /// Peintre alternatif
  protected
    procedure DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); override;

    property AlternatePainter : TPainter read FAlternatePainter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Trou
    Le trou est un terrain au-dessus duquel on peut passer avec une planche, et
    sinon inaccessible.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  THole = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Ciel
    Le ciel est toujours inaccessible.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TSky = class(TField)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

implementation

{---------------}
{ Classe TGrass }
{---------------}

{*
  Crée une instance de TGrass
  @param AMaster           Maître FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param AImgName          Nom de l'image des graphismes (défaut : l'herbe)
  @param ADelegateDrawTo   Un autre terrain auquel déléguer l'affichage
*}
constructor TGround.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; const AImgName : string = fGrass;
  ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  if AImgName <> '' then
    Painter.ImgNames.Add(AImgName);
end;

{--------------}
{ Classe TWall }
{--------------}

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
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TWall.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  Cancel := True;
end;

{---------------}
{ Classe TWater }
{---------------}

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

  FStaticDraw := Master.Editing;

  FAlternatePainter := TPainter.Create(Master.ImagesMaster);
  FAlternatePainter.ImgNames.BeginUpdate;

  Painter.ImgNames.Add(fWater);
  AlternatePainter.ImgNames.Add(fAlternateWater);
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TWater.AfterConstruction;
begin
  inherited;
  FAlternatePainter.ImgNames.EndUpdate;
end;

{*
  Détruit l'instance
*}
destructor TWater.Destroy;
begin
  FAlternatePainter.Free;
  inherited;
end;

{*
  Dessine le terrain sur le canevas indiqué
  Les descendants de TField doivent réimplémenter DrawField plutôt que Draw.
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonnée X du point à partir duquel dessiner le terrain
  @param Y        Coordonnée Y du point à partir duquel dessiner le terrain
*}
procedure TWater.DoDraw(const QPos : TQualifiedPos; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  if (Master.TickCount mod 2000) < 1000 then
    Painter.Draw(Canvas, X, Y)
  else
    AlternatePainter.Draw(Canvas, X, Y);
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TWater.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
var Behind : T3DPoint;
begin
  with Player do
  begin
    if DoAction(actGoOnWater) then exit;

    Behind := PointBehind(Pos, Direction);
    if (Map[Behind].Field is TGround) and
       (Map[Behind].Obstacle = Map[Src].Obstacle) and
       DoAction(actPassOverScrew) then exit;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnWater, dtError);
    Cancel := True;
  end;
end;

{--------------}
{ Classe THole }
{--------------}

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
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure THole.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
var Behind : T3DPoint;
begin
  with Player do
  begin
    Behind := PointBehind(Pos, Direction);
    if (Map[Behind].Field is TGround) and
       (Map[Behind].Obstacle = Map[Src].Obstacle) and
       DoAction(actPassOverScrew) then exit;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnHole, dtError);
    Cancel := True;
  end;
end;

{-------------}
{ Classe TSky }
{-------------}

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
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TSky.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
begin
  if KeyPressed then
    Player.ShowDialog(sBlindAlley, sCantGoOnSky, dtError);
  Cancel := True;
end;

end.

