{*
  D�crit les terrains de base de Funlabyrinthe
  L'unit� FLBFields regroupe les d�finitions des terrains de base de
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FLBFields;

interface

uses
  Types, Graphics, ScUtils, SdDialogs, GR32, FunLabyUtils, Generics,
  GraphicsTools, FLBCommon;

resourcestring
  sGrass = 'Herbe';    /// Nom de l'herbe
  sWall = 'Mur';       /// Nom du mur
  sWater = 'Eau';      /// Nom de l'eau
  sHole = 'Trou';      /// Nom du trou
  sSky = 'Ciel';       /// Nom du ciel
  sOutside = 'Dehors'; /// Nom du dehors

const {don't localize}
  idGrass = 'Grass';     /// ID de l'herbe
  idWall = 'Wall';       /// ID du mur
  idWater = 'Water';     /// ID de l'eau
  idHole = 'Hole';       /// ID du trou
  idSky = 'Sky';         /// ID du ciel
  idOutside = 'Outside'; /// ID du dehors

const {don't localize}
  fGrass = 'Fields/Grass';     /// Fichier de l'herbe
  fWall = 'Fields/Wall';       /// Fichier du mur
  fWater = 'Fields/Water';     /// Fichier de l'eau
  fHole = 'Fields/Hole';       /// Fichier du trou
  fSky = 'Fields/Sky';         /// Fichier du ciel
  fOutside = 'Fields/Outside'; /// Fichier du dehors

resourcestring
  sCantGoOnWater = 'Sans bou�e, on coule dans l''eau.';
  sCantGoOnHole = 'T''es pas bien de vouloir sauter dans ce trou !?';
  sCantGoOnSky = 'Tu ne peux pas voler !';

  sGotOutsideMaze = 'BRAVO ! Tu as r�ussi � sortir du labyrinthe !';

type
  {*
    Mur
    Le mur est un terrain qui bloque syst�matiquement le joueur.
    @author sjrd
    @version 5.0
  *}
  TWall = class(TField)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entering(Context: TMoveContext); override;
  end;

  {*
    Eau
    L'eau est un terrain sur lequel on peut aller avec une bou�e ou une barque,
    et au-dessus duquel on peut passer avec une planche.
    @author sjrd
    @version 5.0
  *}
  TWater = class(TField)
  private
    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entering(Context: TMoveContext); override;
    procedure Entered(Context: TMoveContext); override;
  end;

  {*
    Trou
    Le trou est un terrain au-dessus duquel on peut passer avec une planche, et
    sinon inaccessible.
    @author sjrd
    @version 5.0
  *}
  THole = class(TField)
  private
    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entering(Context: TMoveContext); override;
  end;

  {*
    Ciel
    Le ciel est toujours inaccessible.
    @author sjrd
    @version 5.0
  *}
  TSky = class(TField)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entering(Context: TMoveContext); override;
  end;

  {*
    Dehors
    Le dehors repr�sente l'ext�rieur du labyrinthe et fait remporter la victoire
    au joueur qui y parvient.
    @author sjrd
    @version 5.0
  *}
  TOutside = class(TGround)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entered(Context: TMoveContext); override;
  end;

implementation

{--------------}
{ Classe TWall }
{--------------}

{*
  [@inheritDoc]
*}
constructor TWall.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SWall;
  Painter.AddImage(fWall);
end;

{*
  [@inheritDoc]
*}
procedure TWall.Entering(Context: TMoveContext);
begin
  Context.Cancel;
end;

{---------------}
{ Classe TWater }
{---------------}

{*
  [@inheritDoc]
*}
constructor TWater.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SWater;
  Painter.AddImage(fWater);
end;

{*
  Gestionnaire de message msgPlank
  TWater permet de passer au-dessus d'elle avec la planche si le joueur ne sait
  pas aller dans l'eau.
  @param Msg   Message
*}
procedure TWater.PlankMessage(var Msg: TPlankMessage);
begin
  if Msg.Kind = pmkPassOver then
    Msg.Result := not Msg.Player.AbleTo(actGoOnWater);
end;

{*
  [@inheritDoc]
*}
procedure TWater.DoDraw(Context: TDrawSquareContext);
begin
  inherited;

  DissipateGroundNeighbors(Context);
end;

{*
  [@inheritDoc]
*}
procedure TWater.Entering(Context: TMoveContext);
begin
  with Context do
  begin
    if Player.DoAction(actGoOnWater) then
      Exit;

    if KeyPressed then
      Player.ShowMessage(sCantGoOnWater);
    Cancel;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TWater.Entered(Context: TMoveContext);
begin
  Context.Player.DoAction(actGoOnWater);
end;

{--------------}
{ Classe THole }
{--------------}

{*
  [@inheritDoc]
*}
constructor THole.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SHole;
  Painter.AddImage(fHole);
end;

{*
  Gestionnaire de message msgPlank
  THole permet toujours de passer au-dessus d'elle avec la planche.
  @param Msg   Message
*}
procedure THole.PlankMessage(var Msg: TPlankMessage);
begin
  if Msg.Kind = pmkPassOver then
    Msg.Result := True;
end;

{*
  [@inheritDoc]
*}
procedure THole.DoDraw(Context: TDrawSquareContext);
begin
  inherited;

  DissipateGroundNeighbors(Context);
end;

{*
  [@inheritDoc]
*}
procedure THole.Entering(Context: TMoveContext);
begin
  with Context do
  begin
    if KeyPressed then
      Player.ShowMessage(sCantGoOnHole);
    Cancel;
  end;
end;

{-------------}
{ Classe TSky }
{-------------}

{*
  [@inheritDoc]
*}
constructor TSky.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SSky;
  Painter.AddImage(fSky);
end;

{*
  [@inheritDoc]
*}
procedure TSky.Entering(Context: TMoveContext);
begin
  with Context do
  begin
    if KeyPressed then
      Player.ShowMessage(sCantGoOnSky);
    Cancel;
  end;
end;

{-----------------}
{ Classe TOutside }
{-----------------}

{*
  [@inheritDoc]
*}
constructor TOutside.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SOutside;
  Painter.AddImage(fOutside);
end;

{*
  [@inheritDoc]
*}
procedure TOutside.Entered(Context: TMoveContext);
begin
  with Context.Player do
  begin
    Win;
    ShowMessage(sGotOutsideMaze);
  end;
end;

end.

