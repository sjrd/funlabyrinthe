{*
  Décrit le comportement complet de la barque
  L'unité FLBBoat regroupe tous les composants intervenant dans le
  fonctionnement de la barque.
  @author sjrd
  @version 5.0
*}
unit FLBBoat;

interface

uses
  SysUtils, Graphics, StrUtils, ScUtils, FunLabyUtils, Generics, GraphicsTools,
  MapTools, FLBCommon, FLBFields, GR32;

const {don't localize}
  idBoatPlugin = 'BoatPlugin'; /// ID du plug-in barque

resourcestring
  sBoat = 'Barque'; /// Nom de la barque

resourcestring
  /// Hint du créateur de barques
  sBoatCreatorHint = 'Créer une nouvelle barque';

const {don't localize}
  idBoatCreator = 'BoatCreator'; /// ID du créateur de barques

  /// ID du créateur de terrains d'eau spéciale pour mettre sous les barques
  idUnderBoatCreator = 'UnderBoatCreator';

  /// Préfixe pour l'ID d'une eau en-dessous d'une barque
  PrefixUnderBoat = 'UnderBoat.';

const {don't localize}
  fBoat = 'Objects/Boat'; /// Fichier de la barque

type
  TBoat = class;

  {*
    Données du joueur pour le plug-in TBoatPlugin
    @author sjrd
    @version 5.0
  *}
  TBoatPluginPlayerData = class(TPlayerData)
  private
    FUsedBoat: TBoat;
  published
    property UsedBoat: TBoat read FUsedBoat write FUsedBoat;
  end;

  {*
    Plug-in barque
    Affiche une barque sous le joueur, et permet d'aller dans l'eau. De plus,
    ce plug-in bloque un mouvement si la direction a changé.
    @author sjrd
    @version 5.0
  *}
  TBoatPlugin = class(TPlugin)
  private
    procedure SetUsedBoat(Player: TPlayer; UsedBoat: TBoat);
  protected
    class function GetPlayerDataClass: TPlayerDataClass; override;
  public
    procedure DrawBefore(Context: TDrawSquareContext); override;

    procedure Moving(Context: TMoveContext); override;
    procedure Moved(Context: TMoveContext); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;

    function GetUsedBoat(Player: TPlayer): TBoat;
  end;

  {*
    Barque
    La barque est un moyen de transport permettant d'aller sur l'eau.
    @author sjrd
    @version 5.0
  *}
  TBoat = class(TTool)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    procedure Find(Context: TMoveContext); override;

    class function ToUnderBoatWater(Square: TSquare): TSquare;
  end;

  {*
    Créateur de barques
    @author sjrd
    @version 5.0
  *}
  TBoatCreator = class(TComponentCreator)
  protected
    function GetHint: string; override;

    function DoCreateComponent(
      const AID: TComponentID): TFunLabyComponent; override;
  public
    constructor Create(AMaster: TMaster; AID: TComponentID);
  end;

  {*
    Créateur de terrains d'eau spéciale pour mettre sous les barques
    @author sjrd
    @version 5.0
  *}
  TUnderBoatCreator = class(TComponentCreator)
  protected
    function GetIsDesignable: Boolean; override;

    function DoCreateComponent(
      const AID: TComponentID): TFunLabyComponent; override;
  end;

const
  clOutBoat = $00004080; /// Couleur de l'extérieur d'une barque
  clInBoat  = $00006699; /// Couleur de l'intérieur d'une barque

implementation

{--------------------}
{ Classe TBoatPlugin }
{--------------------}

{*
  Mémorise la barque qu'utilise un joueur
  @param Player     Joueur
  @param UsedBoat   Barque utilisée par ce joueur
*}
procedure TBoatPlugin.SetUsedBoat(Player: TPlayer; UsedBoat: TBoat);
begin
  TBoatPluginPlayerData(GetPlayerData(Player)).UsedBoat := UsedBoat;
end;

{*
  [@inheritDoc]
*}
class function TBoatPlugin.GetPlayerDataClass: TPlayerDataClass;
begin
  Result := TBoatPluginPlayerData;
end;

{*
  [@inheritDoc]
*}
procedure TBoatPlugin.DrawBefore(Context: TDrawSquareContext);
begin
  inherited;

  with Context, Bitmap.Canvas do
  begin
    Brush.Color := clInBoat;
    Brush.Style := bsSolid;
    Pen.Color := clOutBoat;
    Pen.Style := psSolid;
    Pen.Width := 2;

    case Player.Direction of
      diNone, diNorth:
      begin
        // Proue
        Arc(X-5, Y-2, X+25, Y+24, X+25, Y+12, X+15, Y+ 2);
        Arc(X+5, Y-2, X+35, Y+24, X+15, Y+ 2, X+ 5, Y+12);
        // Bastingage
        MoveTo(X+ 5, Y+12);
        LineTo(X+ 5, Y+29);
        LineTo(X+25, Y+29);
        LineTo(X+25, Y+12);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
      diEast:
      begin
        // Proue
        Arc(X+4, Y-5, X+32, Y+25, X+18, Y+25, X+30, Y+15);
        Arc(X+4, Y+5, X+32, Y+35, X+30, Y+15, X+18, Y+ 5);
        // Bastingage
        MoveTo(X+18, Y+ 5);
        LineTo(X+ 1, Y+ 5);
        LineTo(X+ 1, Y+25);
        LineTo(X+18, Y+25);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
      diSouth:
      begin
        // Proue
        Arc(X+5, Y+4, X+35, Y+32, X+ 5, Y+18, X+15, Y+30);
        Arc(X-5, Y+4, X+25, Y+32, X+15, Y+30, X+25, Y+18);
        // Bastingage
        MoveTo(X+ 5, Y+18);
        LineTo(X+ 5, Y+ 1);
        LineTo(X+25, Y+ 1);
        LineTo(X+25, Y+18);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
      diWest:
      begin
        // Proue
        Arc(X-2, Y+5, X+26, Y+35, X+12, Y+ 5, X   , Y+15);
        Arc(X-2, Y-5, X+26, Y+25, X   , Y+15, X+12, Y+25);
        // Bastingage
        MoveTo(X+10, Y+ 5);
        LineTo(X+29, Y+ 5);
        LineTo(X+29, Y+25);
        LineTo(X+12, Y+25);
        // Pont
        FloodFill(X+15, Y+15, clOutBoat, fsBorder);
      end;
    end;

    Pen.Width := 1;

    Bitmap.DeleteCanvas;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TBoatPlugin.Moving(Context: TMoveContext);
begin
  with Context do
  begin
    if Player.Direction <> OldDirection then
      Cancel;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TBoatPlugin.Moved(Context: TMoveContext);
var
  PlayerData: TBoatPluginPlayerData;
begin
  with Context do
  begin
    if not (DestSquare.Field is TWater) then
    begin
      PlayerData := TBoatPluginPlayerData(GetPlayerData(Player));

      // Retirer le plug-in barque
      Player.RemovePlugin(Self);

      // Placer un outil barque sur la case source
      SrcSquare := ChangeTool(SrcSquare, PlayerData.UsedBoat);

      // Placer de l'eau spéciale sous barque
      SrcSquare := TBoat.ToUnderBoatWater(SrcSquare);

      // Indiquer que ce joueur n'utilise plus de barqe
      PlayerData.UsedBoat := nil;
    end;
  end;
end;

{*
  [@inheritDoc]
*}
function TBoatPlugin.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := Action = actGoOnWater;
end;

{*
  Obtient la barque qu'est en train d'utiliser un joueur
  @param Player   Joueur dont obtenir la barque
  @return Barque utilisée par ce joueur, ou nil s'il n'en utilise pas
*}
function TBoatPlugin.GetUsedBoat(Player: TPlayer): TBoat;
begin
  Result := TBoatPluginPlayerData(GetPlayerData(Player)).UsedBoat;
end;

{--------------}
{ Classe TBoat }
{--------------}

{*
  Crée une instance de TBoat
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID de l'outil
  @param AName     Nom de l'outil
*}
constructor TBoat.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);

  Painter.ImgNames.Add(fBoat);
end;

{*
  [@inheritDoc]
*}
procedure TBoat.Find(Context: TMoveContext);
var
  BoatPlugin: TBoatPlugin;
  UnderBoatWaterID, WaterID: TComponentID;
begin
  BoatPlugin := Master.Plugin[idBoatPlugin] as TBoatPlugin;

  with Context do
  begin
    // Remplacer la case par de l'eau normale
    UnderBoatWaterID := Square.Field.ID;
    if AnsiStartsStr(PrefixUnderBoat, UnderBoatWaterID) then
    begin
      WaterID := Copy(UnderBoatWaterID, Length(PrefixUnderBoat)+1, MaxInt);
      Square := ChangeField(Square, WaterID);
    end;

    // Retirer la barque
    Square := RemoveTool(Square);

    // Mémoriser la barque utilisée
    BoatPlugin.SetUsedBoat(Player, Self);

    // Ajout du plug-in barque
    Player.AddPlugin(BoatPlugin);
  end;
end;

{*
  Transforme une case pour avoir de l'eau spéciale sous barque
  @param Square   Case à transformer
  @return Case transformée
*}
class function TBoat.ToUnderBoatWater(Square: TSquare): TSquare;
var
  UnderBoatWaterID: TComponentID;
begin
  with Square do
  begin
    if (Field is TWater) and (ID <> '') then
    begin
      UnderBoatWaterID := PrefixUnderBoat + Field.ID;

      if not Master.ComponentExists(UnderBoatWaterID) then
        with Master.Component[idUnderBoatCreator] as TUnderBoatCreator do
          CreateComponent(UnderBoatWaterID);

      Result := ChangeField(Square, UnderBoatWaterID);
    end else
      Result := Square;
  end;
end;

{--------------------}
{ TBoatCreator class }
{--------------------}

{*
  Crée un nouveau créateur de barques
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du créateur de barques
*}
constructor TBoatCreator.Create(AMaster: TMaster; AID: TComponentID);
begin
  inherited Create(AMaster, AID);

  IconPainter.ImgNames.Add(fBoat);
end;

{*
  [@inheritDoc]
*}
function TBoatCreator.GetHint: string;
begin
  Result := SBoatCreatorHint;
end;

{*
  [@inheritDoc]
*}
function TBoatCreator.DoCreateComponent(
  const AID: TComponentID): TFunLabyComponent;
begin
  Result := TBoat.Create(Master, AID, sBoat);
end;

{-------------------------}
{ TUnderBoatCreator class }
{-------------------------}

{*
  [@inheritDoc]
*}
function TUnderBoatCreator.GetIsDesignable: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
function TUnderBoatCreator.DoCreateComponent(
  const AID: TComponentID): TFunLabyComponent;
var
  WaterID: TComponentID;
  WaterField: TField;
begin
  Assert(AnsiStartsStr(PrefixUnderBoat, AID));

  WaterID := Copy(AID, Length(PrefixUnderBoat)+1, MaxInt);
  WaterField := Master.Field[WaterID];

  Result := TGround(TGround.NewDelegateDraw(WaterField)).Create(
    Master, AID, WaterField.Name, '')
end;

end.

