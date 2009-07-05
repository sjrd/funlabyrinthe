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
  sCantGoOnWater = 'Sans bou�e, on coule dans l''eau.';
  sCantGoOnHole = 'T''es pas bien de vouloir sauter dans ce trou !?';
  sCantGoOnSky = 'Tu ne peux pas voler !';

type
  {*
    Sol
    Le sol est le terrain de base de FunLabyrinthe. Il n'a pas de condition. On
    peut en cr�er plusieurs versions, avec des graphismes diff�rents. Par
    d�faut, il existe un type de sol : l'herbe.
    @author sjrd
    @version 5.0
  *}
  TGround = class(TField)
  private
    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; const AImgName: string = fGrass;
      ADelegateDrawTo: TField = nil);
  end;

  {*
    Mur
    Le mur est un terrain qui bloque syst�matiquement le joueur.
    @author sjrd
    @version 5.0
  *}
  TWall = class(TField)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADelegateDrawTo: TField = nil);

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
    FAlternatePainter: TPainter; /// Peintre alternatif

    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;

    property AlternatePainter: TPainter read FAlternatePainter;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADelegateDrawTo: TField = nil);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Entering(Context: TMoveContext); override;
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
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADelegateDrawTo: TField = nil);

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
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string; ADelegateDrawTo: TField = nil);

    procedure Entering(Context: TMoveContext); override;
  end;

implementation

{---------------}
{ Classe TGrass }
{---------------}

{*
  Cr�e une instance de TGrass
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param AImgName          Nom de l'image des graphismes (d�faut : l'herbe)
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TGround.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; const AImgName: string = fGrass;
  ADelegateDrawTo: TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  if AImgName <> '' then
    Painter.ImgNames.Add(AImgName);
end;

{*
  Gestionnaire de message msgPlank
  TGround permet d'y poser la planche pour autant que de l'autre c�t�, il y ait
  �galement un TGround, et qu'il n'y ait d'obstacle d'aucun c�t�.
  @param Msg   Message
*}
procedure TGround.PlankMessage(var Msg: TPlankMessage);
begin
  with Msg, Player do
  begin
    if Kind = pmkLeaveFrom then
    begin
      Result := (Map[Dest].Field is TGround) and
        (Map[Src].Obstacle = nil) and (Map[Dest].Obstacle = nil);
    end;
  end;
end;

{--------------}
{ Classe TWall }
{--------------}

{*
  Cr�e une instance de TWall
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TWall.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADelegateDrawTo: TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fWall);
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
  Cr�e une instance de TWater
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TWater.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADelegateDrawTo: TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);

  FStaticDraw := Master.Editing;

  FAlternatePainter := TPainter.Create(Master.ImagesMaster);
  FAlternatePainter.ImgNames.BeginUpdate;

  Painter.ImgNames.Add(fWater);
  AlternatePainter.ImgNames.Add(fAlternateWater);
end;

{*
  [@inheritDoc]
*}
destructor TWater.Destroy;
begin
  FAlternatePainter.Free;
  inherited;
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
  if (Context.TickCount mod 2000) < 1000 then
    Painter.Draw(Context)
  else
    AlternatePainter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TWater.AfterConstruction;
begin
  inherited;
  FAlternatePainter.ImgNames.EndUpdate;
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

{--------------}
{ Classe THole }
{--------------}

{*
  Cr�e une instance de THole
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor THole.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADelegateDrawTo: TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fHole);
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
  Cr�e une instance de TSky
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TSky.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string; ADelegateDrawTo: TField = nil);
begin
  inherited Create(AMaster, AID, AName, ADelegateDrawTo);
  Painter.ImgNames.Add(fSky);
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

end.

