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
    Classe de base pour les blocs
    Un bloc est un obstacle qui peut avoir la faculté de cacher l'effet et
    l'outil en-dessous de lui, malgré que son dessin comporte des parties
    transparentes.
    @author sjrd
    @version 5.0
  *}
  TBlock = class(TObstacle)
  private
    FHideEffectAndTool: Boolean; /// Indique s'il faut cacher l'effet et l'outil
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  published
    property HideEffectAndTool: Boolean
      read FHideEffectAndTool write FHideEffectAndTool default False;
  end;

  {*
    Bloc en argent
    Le bloc en argent peut être détruit au moyen d'une clef en argent.
    @author sjrd
    @version 5.0
  *}
  TSilverBlock = class(TBlock)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Pushing(Context: TMoveContext); override;
  end;

  {*
    Bloc en or
    Le bloc en or peut être détruit au moyen d'une clef en or.
    @author sjrd
    @version 5.0
  *}
  TGoldenBlock = class(TBlock)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Pushing(Context: TMoveContext); override;
  end;

  {*
    Passage secret
    Le passage secret a l'apparence d'un mur mais peut être ouvert sans rien.
    @author sjrd
    @version 5.0
  *}
  TSecretWay = class(TObstacle)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Pushing(Context: TMoveContext); override;
  end;

var { FunDelphi codegen }
  compSilverBlock: TSilverBlock;
  compGoldenBlock: TGoldenBlock;
  compSecretWay: TSecretWay;

implementation

{--------------}
{ TBlock class }
{--------------}

{*
  [@inheritDoc]
*}
procedure TBlock.DoDraw(Context: TDrawSquareContext);
begin
  if HideEffectAndTool and (not Context.IsNowhere) then
    Context.QPos.Field.Draw(Context);

  inherited;
end;

{---------------------}
{ Classe TSilverBlock }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSilverBlock.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SSilverBlock;
  Painter.AddImage(fSilverBlock);
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
  [@inheritDoc]
*}
constructor TGoldenBlock.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SGoldenBlock;
  Painter.AddImage(fGoldenBlock);
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
  [@inheritDoc]
*}
constructor TSecretWay.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SSecretWay;
  Painter.AddImage(fWall);
  EditVisualTag := '!';
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

