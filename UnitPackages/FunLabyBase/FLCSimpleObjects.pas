{*
  D�crit les objets simples de FunLabyrinthe
  L'unit� FLCSimpleObjects regroupe les d�finitions des objets simples de
  FunLabyrinthe, c'est-�-dire ceux qui n'ont besoin que de la d�finition d'objet
  et de l'effet permettant de les obtenir, avec �ventuellement un plug-in
  d'affichage.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FLCSimpleObjects;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils, FLCCommon, FLCFields;

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
  fBuoy = 'Buoy';           /// Fichier de la bou�e
  fSilverKey = 'SilverKey'; /// Fichier de la clef d'argent
  fGoldenKey = 'GoldenKey'; /// Fichier de la clef d'or

resourcestring
  sFoundBuoy      = 'Tu as trouv� une bou�e.'+#10+
                    'Tu peux aller dans l''eau.';
  sFoundSilverKey = 'Tu as trouv� une clef d''argent.'+#10+
                    'Tu peux faire dispara�tre un bloc en argent.';
  sFoundGoldenKey = 'Tu as trouv� une clef d''or.'+#10+
                    'Tu peux faire dispara�tre un bloc en or.';

type
  {*
    Plug-in bou�e
    Affiche une bou�e sous le joueur, et permet d'aller dans l'eau.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TBuoyPlugin = class(TPlugin)
  public
    procedure DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
      Canvas : TCanvas; X : integer = 0; Y : integer = 0); override;

    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); override;

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
  end;

  {*
    D�finition de l'objet bou�e
    La bou�e permet d'aller dans l'eau.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TBuoys = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    D�finition de l'objet clef d'argent
    La clef d'argent permet d'ouvrir une serrure en argent
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TSilverKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    D�finition de l'objet clef d'or
    La clef d'or permet d'ouvrir une serrure en or
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TGoldenKeys = class(TObjectDef)
  protected
    function GetShownInfos(Player : TPlayer) : string; override;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    function CanYou(Player : TPlayer;
      const Action : TPlayerAction) : boolean; override;
    procedure UseFor(Player : TPlayer; const Action : TPlayerAction); override;
  end;

  {*
    Bou�e
    La bou�e donne au joueur un objet bou�e
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TBuoy = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Clef d'argent
    La clef d'argent donne au joueur un objet clef d'argent
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TSilverKey = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

  {*
    Clef d'or
    La clef d'or donne au joueur un objet clef d'or
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TGoldenKey = class(TEffect)
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);

    procedure Execute(Player : TPlayer; KeyPressed : boolean;
      const Pos : T3DPoint; var GoOnMoving : boolean); override;
  end;

implementation

{--------------------}
{ Classe TBuoyPlugin }
{--------------------}

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param QPos     Position qualifi�e de l'emplacement de dessin
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TBuoyPlugin.DrawBefore(Player : TPlayer; const QPos : TQualifiedPos;
  Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  inherited;
  with Canvas do
  begin
    Brush.Color := clYellow;
    Brush.Style := bsSolid;
    Pen.Color := clYellow;
    Pen.Style := psSolid;
    Ellipse(X+1, Y+1, X+ScrewSize-1, Y+ScrewSize-1);
  end;
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de d�part
  @param Dest         Case d'arriv�e
*}
procedure TBuoyPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
begin
  if not (Player.Map[Dest].Field is TWater) then
    Player.RemovePlugin(Self);
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TBuoyPlugin.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := Action = actGoOnWater;
end;

{---------------}
{ Classe TBuoys }
{---------------}

{*
  Cr�e une instance de TBuoys
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TBuoys.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fBuoy);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TBuoys.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sBuoyInfos, [ACount])
  else
    Result := Format(sBuoysInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TBuoys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actGoOnWater) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TBuoys.UseFor(Player : TPlayer; const Action : TPlayerAction);
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
  Cr�e une instance de TSilverKeys
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TSilverKeys.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverKey);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TSilverKeys.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sSilverKeyInfos, [ACount])
  else
    Result := Format(sSilverKeysInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TSilverKeys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actOpenSilverLock) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TSilverKeys.UseFor(Player : TPlayer; const Action : TPlayerAction);
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
  Cr�e une instance de TGoldenKeys
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
  @param AName     Nom du composant
*}
constructor TGoldenKeys.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenKey);
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TGoldenKeys.GetShownInfos(Player : TPlayer) : string;
var ACount : integer;
begin
  ACount := Count[Player];
  if ACount < 2 then
    Result := Format(sGoldenKeyInfos, [ACount])
  else
    Result := Format(sGoldenKeysInfos, [ACount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TGoldenKeys.CanYou(Player : TPlayer;
  const Action : TPlayerAction) : boolean;
begin
  Result := ((Action = actOpenGoldenLock) and (Count[Player] > 0)) or
    (inherited CanYou(Player, Action));
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TGoldenKeys.UseFor(Player : TPlayer; const Action : TPlayerAction);
begin
  if Action = actOpenGoldenLock then
    Count[Player] := Count[Player]-1
  else
    inherited;
end;

{--------------}
{ Classe TBuoy }
{--------------}

{*
  Cr�e une instance de TBuoy
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TBuoy.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fBuoy);
end;

{*
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TBuoy.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idBuoys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.Controller.ShowDialog(sMessage, sFoundBuoy);
end;

{-------------------}
{ Classe TSilverKey }
{-------------------}

{*
  Cr�e une instance de TSilverKey
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TSilverKey.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fSilverKey);
end;

{*
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TSilverKey.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idSilverKeys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.Controller.ShowDialog(sMessage, sFoundSilverKey);
end;

{-------------------}
{ Classe TGoldenKey }
{-------------------}

{*
  Cr�e une instance de TGoldenKey
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de l'effet de case
  @param AName     Nom de la case
*}
constructor TGoldenKey.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID, AName);
  Painter.ImgNames.Add(fGoldenKey);
end;

{*
  Ex�cute l'effet
  @param Player       Joueur concern�
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TGoldenKey.Execute(Player : TPlayer; KeyPressed : boolean;
  const Pos : T3DPoint; var GoOnMoving : boolean);
begin
  inherited;

  // D�sactivation de la case
  Player.Map[Pos] := Player.Map[Pos].ChangeEffect;

  // Incr�mentation du nombre de bou�es du joueur
  with Master.ObjectDef[idGoldenKeys] do Count[Player] := Count[Player]+1;

  // Affichage d'un message de notification
  Player.Controller.ShowDialog(sMessage, sFoundGoldenKey);
end;

end.

