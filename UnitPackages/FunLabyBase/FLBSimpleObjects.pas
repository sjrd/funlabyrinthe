{*
  Décrit les objets simples de FunLabyrinthe
  L'unité FLBSimpleObjects regroupe les définitions des objets simples de
  FunLabyrinthe, c'est-à-dire ceux qui n'ont besoin que de la définition d'objet
  et de l'effet permettant de les obtenir, avec éventuellement un plug-in
  d'affichage.
  @author sjrd
  @version 5.0
*}
unit FLBSimpleObjects;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, FLBCommon, FLBFields;

const {don't localize}
  idBuoyPlugin = 'BuoyPlugin'; /// ID du plug-in bouée

resourcestring
  sBuoys           = 'Bouée';              /// Nom de l'objet bouée
  sBuoyInfos       = '%d bouée';           /// Infos bouées (singulier)
  sBuoysInfos      = '%d bouées';          /// Infos bouées (pluriel)
  sSilverKeys      = 'Clef d''argent';     /// Nom de l'objet clef d'argent
  sSilverKeyInfos  = '%d clef d''argent';  /// Infos clefs d'argent (singulier)
  sSilverKeysInfos = '%d clefs d''argent'; /// Infos clefs d'argent (pluriel)
  sGoldenKeys      = 'Clef d''or';         /// Nom de l'objet clef d'or
  sGoldenKeyInfos  = '%d clef d''or';      /// Infos clefs d'or (singulier)
  sGoldenKeysInfos = '%d clefs d''or';     /// Infos clefs d'or (pluriel)

  sBuoy = 'Bouée';                         /// Nom de la bouée
  sSilverKey = 'Clef d''argent';           /// Nom de la clef d'argent
  sGoldenKey = 'Clef d''or';               /// Nom de la clef d'or

const {don't localize}
  idBuoys = 'Buoys';           /// ID des bouées
  idSilverKeys = 'SilverKeys'; /// ID des clefs d'argent
  idGoldenKeys = 'GoldenKeys'; /// ID des clefs d'or

  idBuoy = 'Buoy';             /// ID de la bouée
  idSilverKey = 'SilverKey';   /// ID de la clef d'argent
  idGoldenKey = 'GoldenKey';   /// ID de la clef d'or

const {don't localize}
  fBuoy = 'Buoy';           /// Fichier de la bouée
  fSilverKey = 'SilverKey'; /// Fichier de la clef d'argent
  fGoldenKey = 'GoldenKey'; /// Fichier de la clef d'or

resourcestring
  sFoundBuoy = 'Tu as trouvé une bouée.'+#10+
    'Tu peux aller dans l''eau.';
  sFoundSilverKey = 'Tu as trouvé une clef d''argent.'+#10+
    'Tu peux faire disparaître un bloc en argent.';
  sFoundGoldenKey = 'Tu as trouvé une clef d''or.'+#10+
    'Tu peux faire disparaître un bloc en or.';

type
  {*
    Plug-in bouée
    Affiche une bouée sous le joueur, et permet d'aller dans l'eau.
    @author sjrd
    @version 5.0
  *}
  TBuoyPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
      Canvas: TCanvas; X: Integer = 0; Y: Integer = 0); override;

    procedure Moved(Player: TPlayer; const Src, Dest: T3DPoint); override;

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; override;
  end;

  {*
    Définition de l'objet bouée
    La bouée permet d'aller dans l'eau.
    @author sjrd
    @version 5.0
  *}
  TBuoys = class(TObjectDef)
  protected
    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; override;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction); override;
  end;

  {*
    Définition de l'objet clef d'argent
    La clef d'argent permet d'ouvrir une serrure en argent
    @author sjrd
    @version 5.0
  *}
  TSilverKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; override;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction); override;
  end;

  {*
    Définition de l'objet clef d'or
    La clef d'or permet d'ouvrir une serrure en or
    @author sjrd
    @version 5.0
  *}
  TGoldenKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player: TPlayer): string; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID;
      const AName: string);

    function AbleTo(Player: TPlayer;
      const Action: TPlayerAction): Boolean; override;
    procedure UseFor(Player: TPlayer; const Action: TPlayerAction); override;
  end;

implementation

{--------------------}
{ Classe TBuoyPlugin }
{--------------------}

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessiné
  @param QPos     Position qualifiée de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TBuoyPlugin.DrawBefore(Player: TPlayer; const QPos: TQualifiedPos;
  Canvas: TCanvas; X: Integer = 0; Y: Integer = 0);
begin
  inherited;
  with Canvas do
  begin
    Brush.Color := clYellow;
    Brush.Style := bsSolid;
    Pen.Color := clYellow;
    Pen.Style := psSolid;
    Ellipse(X+1, Y+1, X+SquareSize-1, Y+SquareSize-1);
  end;
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Player   Joueur qui se déplace
  @param Src      Case de départ
  @param Dest     Case d'arrivée
*}
procedure TBuoyPlugin.Moved(Player: TPlayer; const Src, Dest: T3DPoint);
begin
  if not (Player.Map[Dest].Field is TWater) then
    Player.RemovePlugin(Self);
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TBuoyPlugin.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := Action = actGoOnWater;
end;

{---------------}
{ Classe TBuoys }
{---------------}

{*
  Crée une instance de TBuoys
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TBuoys.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fBuoy);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TBuoys.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := ((Action = actGoOnWater) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
*}
procedure TBuoys.UseFor(Player: TPlayer; const Action: TPlayerAction);
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
  Crée une instance de TSilverKeys
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TSilverKeys.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverKey);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TSilverKeys.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := ((Action = actOpenSilverLock) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
*}
procedure TSilverKeys.UseFor(Player: TPlayer; const Action: TPlayerAction);
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
  Crée une instance de TGoldenKeys
  @param AMaster   Maître FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TGoldenKeys.Create(AMaster: TMaster; const AID: TComponentID;
  const AName: string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenKey);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une chaîne vide si rien à afficher
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
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TGoldenKeys.AbleTo(Player: TPlayer;
  const Action: TPlayerAction): Boolean;
begin
  Result := ((Action = actOpenGoldenLock) and (Count[Player] > 0)) or
    (inherited AbleTo(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à effectuer
*}
procedure TGoldenKeys.UseFor(Player: TPlayer; const Action: TPlayerAction);
begin
  if Action = actOpenGoldenLock then
    Count[Player] := Count[Player]-1
  else
    inherited;
end;

end.

