{*
  D�crit les objets simples de FunLabyrinthe
  L'unit� FLBSimpleObjects regroupe les d�finitions des objets simples de
  FunLabyrinthe, c'est-�-dire ceux qui n'ont besoin que de la d�finition d'objet
  et de l'effet permettant de les obtenir, avec �ventuellement un plug-in
  d'affichage.
  @author sjrd
  @version 5.0
*}
unit FLBSimpleObjects;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, Generics, FLBCommon, FLBFields,
  GR32;

const {don't localize}
  idBuoyPlugin = 'BuoyPlugin'; /// ID du plug-in bou�e

resourcestring
  sBuoys           = 'Bou�e';              /// Nom de l'objet bou�e
  sBuoyInfos       = '%d bou�e';           /// Infos bou�es (singulier)
  sBuoysInfos      = '%d bou�es';          /// Infos bou�es (pluriel)
  sSilverKeys      = 'Clef d''argent';     /// Nom de l'objet clef d'argent
  sSilverKeyInfos  = '%d clef d''argent';  /// Infos clefs d'argent (singulier)
  sSilverKeysInfos = '%d clefs d''argent'; /// Infos clefs d'argent (pluriel)
  sGoldenKeys      = 'Clef d''or';         /// Nom de l'objet clef d'or
  sGoldenKeyInfos  = '%d clef d''or';      /// Infos clefs d'or (singulier)
  sGoldenKeysInfos = '%d clefs d''or';     /// Infos clefs d'or (pluriel)

  sBuoy = 'Bou�e';                         /// Nom de la bou�e
  sSilverKey = 'Clef d''argent';           /// Nom de la clef d'argent
  sGoldenKey = 'Clef d''or';               /// Nom de la clef d'or

const {don't localize}
  idBuoys = 'Buoys';           /// ID des bou�es
  idSilverKeys = 'SilverKeys'; /// ID des clefs d'argent
  idGoldenKeys = 'GoldenKeys'; /// ID des clefs d'or

  idBuoy = 'Buoy';             /// ID de la bou�e
  idSilverKey = 'SilverKey';   /// ID de la clef d'argent
  idGoldenKey = 'GoldenKey';   /// ID de la clef d'or

const {don't localize}
  fBuoyPlugin = 'Plugins/Buoy';     /// Fichier de la bou�e sous le joueur
  fBuoy = 'Objects/Buoy';           /// Fichier de la bou�e
  fSilverKey = 'Objects/SilverKey'; /// Fichier de la clef d'argent
  fGoldenKey = 'Objects/GoldenKey'; /// Fichier de la clef d'or

resourcestring
  sFoundBuoy = 'Tu as trouv� une bou�e.'+#10+
    'Tu peux aller dans l''eau.';
  sFoundSilverKey = 'Tu as trouv� une clef d''argent.'+#10+
    'Tu peux faire dispara�tre un bloc en argent.';
  sFoundGoldenKey = 'Tu as trouv� une clef d''or.'+#10+
    'Tu peux faire dispara�tre un bloc en or.';

type
  {*
    Plug-in bou�e
    Affiche une bou�e sous le joueur, et permet d'aller dans l'eau.
    @author sjrd
    @version 5.0
  *}
  TBuoyPlugin = class(TPlugin)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Moved(Context: TMoveContext); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;
  published
    property PainterBefore;
  end;

  {*
    D�finition de l'objet bou�e
    La bou�e permet d'aller dans l'eau.
    @author sjrd
    @version 5.0
  *}
  TBuoys = class(TObjectDef)
  protected
    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer); override;
  end;

  {*
    D�finition de l'objet clef d'argent
    La clef d'argent permet d'ouvrir une serrure en argent
    @author sjrd
    @version 5.0
  *}
  TSilverKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer); override;
  end;

  {*
    D�finition de l'objet clef d'or
    La clef d'or permet d'ouvrir une serrure en or
    @author sjrd
    @version 5.0
  *}
  TGoldenKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer); override;
  end;

var { FunDelphi codegen }
  compBuoyPlugin: TBuoyPlugin;
  compBuoys: TBuoys;
  compSilverKeys: TSilverKeys;
  compGoldenKeys: TGoldenKeys;

  compBuoy: TObjectTool;
  compSilverKey: TObjectTool;
  compGoldenKey: TObjectTool;

implementation

{--------------------}
{ Classe TBuoyPlugin }
{--------------------}

{*
  [@inheritDoc]
*}
constructor TBuoyPlugin.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  PainterBefore.AddImage(fBuoyPlugin);
end;

{*
  [@inheritDoc]
*}
procedure TBuoyPlugin.Moved(Context: TMoveContext);
begin
  with Context do
  begin
    if not (DestSquare.Field is TWater) then
      Player.RemovePlugin(Self);
  end;
end;

{*
  [@inheritDoc]
*}
function TBuoyPlugin.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := Action = actGoOnWater;
end;

{---------------}
{ Classe TBuoys }
{---------------}

{*
  [@inheritDoc]
*}
constructor TBuoys.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SBuoys;
  Painter.AddImage(fBuoy);
end;

{*
  [@inheritDoc]
*}
function TBuoys.GetShownInfos(Player: TPlayer): string;
var
  ACount: Integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sBuoyInfos, [ACount])
  else
    Result := Format(sBuoysInfos, [ACount]);
end;

{*
  [@inheritDoc]
*}
function TBuoys.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := ((Action = actGoOnWater) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action, Param));
end;

{*
  [@inheritDoc]
*}
procedure TBuoys.UseFor(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer);
begin
  if Action = actGoOnWater then
    Player.AddPlugin(Master.Plugin[idBuoyPlugin])
  else
    inherited;
end;

{--------------------}
{ Classe TSilverKeys }
{--------------------}

{*
  [@inheritDoc]
*}
constructor TSilverKeys.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SSilverKeys;
  Painter.AddImage(fSilverKey);
end;

{*
  [@inheritDoc]
*}
function TSilverKeys.GetShownInfos(Player: TPlayer): string;
var
  ACount: Integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sSilverKeyInfos, [ACount])
  else
    Result := Format(sSilverKeysInfos, [ACount]);
end;

{*
  [@inheritDoc]
*}
function TSilverKeys.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := ((Action = actOpenSilverLock) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action, Param));
end;

{*
  [@inheritDoc]
*}
procedure TSilverKeys.UseFor(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer);
begin
  if Action = actOpenSilverLock then
    Count[Player] := Count[Player]-1
  else
    inherited;
end;

{--------------------}
{ Classe TGoldenKeys }
{--------------------}

{*
  [@inheritDoc]
*}
constructor TGoldenKeys.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  Name := SGoldenKeys;
  Painter.AddImage(fGoldenKey);
end;

{*
  [@inheritDoc]
*}
function TGoldenKeys.GetShownInfos(Player: TPlayer): string;
var
  ACount: Integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sGoldenKeyInfos, [ACount])
  else
    Result := Format(sGoldenKeysInfos, [ACount]);
end;

{*
  [@inheritDoc]
*}
function TGoldenKeys.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := ((Action = actOpenGoldenLock) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action, Param));
end;

{*
  [@inheritDoc]
*}
procedure TGoldenKeys.UseFor(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer);
begin
  if Action = actOpenGoldenLock then
    Count[Player] := Count[Player]-1
  else
    inherited;
end;

end.

