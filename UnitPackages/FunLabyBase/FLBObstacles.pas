{*
  Décrit les obstacles standart de Funlabyrinthe
  L'unité FLBObstacles regroupe les définitions des obstacles standart de
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit FLBObstacles;

interface

uses
  SysUtils, Classes, Graphics, ScUtils, SdDialogs, FunLabyUtils, MapTools,
  FLBCommon, FLBFields, GraphicsTools;

resourcestring
  sSilverBlock = 'Bloc en argent'; /// Nom du bloc en argent
  sGoldenBlock = 'Bloc en or';     /// Nom du bloc en or
  sSecretWay = 'Passage secret';   /// Nom du passage secret

const {don't localize}
  idSilverBlock = 'SilverBlock'; /// ID du bloc en argent
  idGoldenBlock = 'GoldenBlock'; /// ID du bloc en or
  idSecretWay = 'SecretWay';     /// ID du passage secret

const {don't localize}
  fSilverBlock = 'Blocks/SilverBlock'; /// Fichier du bloc en argent
  fGoldenBlock = 'Blocks/GoldenBlock'; /// Fichier du bloc en or

resourcestring
  sCantOpenSilverBlock = 'Ce bloc ne disparaîtra qu''avec une clef d''argent.';
  sCantOpenGoldenBlock = 'Ce bloc ne disparaîtra qu''avec une clef d''or.';

type
  {*
    Bloc en argent
    Le bloc en argent peut être détruit au moyen d'une clef en argent.
    @author sjrd
    @version 5.0
  *}
  TSilverBlock = class(TObstacle)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Pushing(Context: TMoveContext); override;
  end;

  {*
    Bloc en or
    Le bloc en or peut être détruit au moyen d'une clef en or.
    @author sjrd
    @version 5.0
  *}
  TGoldenBlock = class(TObstacle)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Pushing(Context: TMoveContext); override;
  end;

  {*
    Passage secret
    Le passage secret a l'apparence d'un mur mais peut être ouvert sans rien.
    @author sjrd
    @version 5.0
  *}
  TSecretWay = class(TObstacle)
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Pushing(Context: TMoveContext); override;
  end;

implementation

{---------------------}
{ Classe TSilverBlock }
{---------------------}

{*
  Crée une instance de TSilverBlock
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TSilverBlock.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fSilverBlock);
end;

{*
  [@inheritDoc]
*}
procedure TSilverBlock.Pushing(Context: TMoveContext);
begin
  inherited;

  with Context do
  begin
    if KeyPressed then
    begin
      if Player.DoAction(actOpenSilverLock) then
        Square := RemoveObstacle(Square)
      else
        Player.ShowMessage(sCantOpenSilverBlock);
    end;
  end;
end;

{---------------------}
{ Classe TGoldenBlock }
{---------------------}

{*
  Crée une instance de TGoldenBlock
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TGoldenBlock.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fGoldenBlock);
end;

{*
  [@inheritDoc]
*}
procedure TGoldenBlock.Pushing(Context: TMoveContext);
begin
  inherited;

  with Context do
  begin
    if KeyPressed then
    begin
      if Player.DoAction(actOpenGoldenLock) then
        Square := RemoveObstacle(Square)
      else
        Player.ShowMessage(sCantOpenGoldenBlock);
    end;
  end;
end;

{-------------------}
{ Classe TSecretWay }
{-------------------}

{*
  Crée une instance de TSecretWay
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du terrain
  @param AName     Nom de l'obstacle
*}
constructor TSecretWay.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fWall);
end;

{*
  [@inheritDoc]
*}
procedure TSecretWay.DoDraw(Context: TDrawSquareContext);
begin
  inherited;

  if Master.Editing then
    DrawSquareText(Context, '!');
end;

{*
  [@inheritDoc]
*}
procedure TSecretWay.Pushing(Context: TMoveContext);
begin
  inherited;

  with Context do
    if KeyPressed then
      Square := RemoveObstacle(Square);
end;

end.

