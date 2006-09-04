{*
  Types et classes de bases de FunLabyrinthe
  FunLabyUtils comprend les types et classes de base de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, Controls, IniFiles, ScUtils,
  Forms, Dialogs, StdCtrls, Math;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';

const
  ScrewSize = 30;         /// Taille (en largeur et hauteur) d'une case
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp

type
  /// Type représentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Pointeur vers T3DPoint
  P3DPoint = ^T3DPoint;

  /// Type représentant le code d'une case
  TScrewCode = type Byte;

  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmEditActions, fmPlay);

  TMaster = class;
  TPlayer = class;

  {*
    Gère le chargement des images d'après leur nom
    TImagesMaster s'occupe de charger automatiquement les images qu'on lui
    demande d'afficher. Il les conserve dans une liste d'image.
  *}
  TImagesMaster = class
  private
    FImgList : TImageList; /// Liste d'images interne
    FImgNames : TStrings;  /// Liste des noms des images
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const ImgName : string) : integer;
    procedure Draw(Index : integer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); overload;
    procedure Draw(const ImgName : string; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); overload;
  end;

  {*
    Enregistre est affiche par superposition une liste d'images
    TPainter enregistre une liste d'images par leur noms et propose une méthode
    pour les dessiner les unes sur les autres, par transparence.
  *}
  TPainter = class
  private
    FMaster : TImagesMaster; /// Maître d'images
    FImgNames : TStrings;    /// Liste des noms des images
    FCachedImg : TBitmap;    /// Copie cache de l'image résultante

    procedure ImgNamesChange(Sender : TObject);
  public
    constructor Create(AMaster : TImagesMaster);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);

    property ImgNames : TStrings read FImgNames;
  end;

  {*
    Classe de base pour les composants de FunLabyrinthe
    TFunLabyComponent est la classe de base pour tous les composants de
    FunLabyrinthe. Elle fournit des propriétés et des méthodes pour nommer et
    afficher le composant.
  *}
  TFunLabyComponent = class
  private
    FMaster : TMaster;   /// Maître FunLabyrinthe
    FName : string;      /// Nom du composant
    FPainter : TPainter; /// Peintre par défaut
  protected
    property Painter : TPainter read FPainter;
  public
    constructor Create(AMaster : TMaster; const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0); virtual;

    property Master : TMaster read FMaster;
    property Name : string read FName;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlayerPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs façons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Empêcher le déplacement du joueur et réagir à son déplacement effectif ;
    - Indiquer au joueur qu'il a la capacité de faire certaines actions.
  *}
  TPlayerPlugin = class
  private
    FMaster : TMaster;         /// Maître FunLabyrinthe
    FPainterBefore : TPainter; /// Peintre par défaut sous le joueur
    FPainterAfter : TPainter;  /// Peintre par défaut sur le joueur
  protected
    property PainterBefore : TPainter read FPainterBefore;
    property PainterAfter : TPainter read FPainterAfter;
  public
    constructor Create(AMaster : TMaster);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure DrawBefore(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;
    procedure DrawAfter(Player : TPlayer; Canvas : TCanvas;
      X : integer = 0; Y : integer = 0); virtual;

    procedure Moving(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Dest : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Moved(Player : TPlayer; KeyPressed : boolean;
      Src, Dest : T3DPoint); virtual;

    function CanYou(Player : TPlayer; Action : integer) : boolean; virtual;
  end;

  {*
    Classe de base pour les objets d'un joueur
    TPlayerObject est la classe de base pour les objets que possède le joueur.
    Les objets peuvent rendre un joueur capable d'effectuer certaines actions.
  *}
  TPlayerObject = class(TFunLabyComponent)
  private
    FPlayer : TPlayer; /// Joueur possédant l'objet
    FCount : integer;  /// Nombre d'objets de ce type que possède le joueur
  protected
    function GetShownInfos : string; virtual;
  public
    constructor Create(AMaster : TMaster; const AName : string;
      APlayer : TPlayer);

    function CanYou(Action : integer) : boolean; virtual;
    procedure UseFor(Action : integer); virtual;

    property Player : TPlayer read FPlayer;
    property Count : integer read FCount write FCount;
    property ShownInfos : string read GetShownInfos;
  end;

  {*
    Classe représentant un joueur
    TPlayer représente un joueur. Elle possède de nombreuses propriétés et
    méthodes permettant d'afficher le joueur, de le déplacer, de lui greffer
    des plug-in, etc.
  *}
  TPlayer = class(TFunLabyComponent)
  private
    FPosition : T3DPoint;    /// Position
    FPosAddr : P3DPoint;     /// Position
    FDirection : TDirection; /// Direction
    /// Peintres selon la direction du joueur
    FDirPainters : array[diNorth..diWest] of TPainter;
    FColor : TColor;         /// Couleur
    FPlugins : TObjectList;  /// Liste des plug-in
    FObjects : TObjectList;  /// Liste des objets

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlayerPlugin;
    function GetObjectCount : integer;
    function GetObjects(Index : integer) : TPlayerObject;

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlayerPlugin read GetPlugins;
  public
    constructor Create(AMaster : TMaster; const AName : string);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    procedure AddPlugin(Plugin : TPlayerPlugin);
    procedure RemovePlugin(Plugin : TPlayerPlugin);

    function ShowDialog(const Title, Text : string;
      DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
      DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
    function ShowDialogRadio(const Title, Text : string; DlgType : TMsgDlgType;
      DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
      const RadioTitles : array of string; var Selected : integer;
      OverButtons : boolean = False) : Word;

    function CanYou(Action : integer) : boolean;

    function Move(Dir : TDirection; KeyPressed : boolean;
      out Redo : boolean) : boolean;

    property Position : P3DPoint read FPosAddr;
    property Direction : TDirection read FDirection write FDirection;
    property Color : TColor read FColor write FColor;
    property ObjectCount : integer read GetObjectCount;
    property Objects[index : integer] : TPlayerObject read GetObjects;
  end;

  {*
    Classe de base pour les cases
    TScrew est la classe de base pour les cases de la carte. Elle possède une
    code et quatre méthodes qui définissent son comportement.
  *}
  TScrew = class(TFunLabyComponent)
  private
    FCode : TScrewCode; /// Code de case
  public
    constructor Create(AMaster : TMaster; const AName : string;
      ACode : TScrewCode);

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); virtual;
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); virtual;
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Pos, Dest : T3DPoint;
      var Cancel : boolean); virtual;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); virtual;

    property Code : TScrewCode read FCode;
  end;

  {*
    Gère les cases et met en relation celles-ci avec leurs codes respectifs
    TScrewsMaster gère les différentes cases de FunLabyrinthe, et met en
    relation les codes des cases avec celles-ci.
  *}
  TScrewsMaster = class
  private
    FMaster : TMaster;                  /// Maître FunLabyrinthe
    FScrews : array[33..255] of TScrew; /// Liste des cases par code

    function GetScrews(Code : TScrewCode) : TScrew;
  public
    constructor Create(AMaster : TMaster);
    destructor Destroy; override;

    property Master : TMaster read FMaster;
    property Screws[Code : TScrewCode] : TScrew read GetScrews; default;
  end;

  {*
    Représente la carte du jeu
    TMap gère et représente la carte du jeu. Elle offre des propriétés et
    méthodes pour lire et modifier cette carte.
  *}
  TMap = class
  private
    FMaster : TMaster;              /// Maître FunLabyrinthe
    FDimensions : T3DPoint;         /// Dimensions de la carte (en cases)
    FMap : array of TScrewCode;     /// Codes des cases sur la carte
    FOutside : array of TScrewCode; /// Codes des cases en-dehors de la carte

    function GetCodeMap(Position : T3DPoint) : TScrewCode;
    procedure SetCodeMap(Position : T3DPoint; Value : TScrewCode);
    function GetMap(Position : T3DPoint) : TScrew;
  public
    constructor Create(AMaster : TMaster; ADimensions : T3DPoint);

    function InMap(Position : T3DPoint) : boolean;

    property Master : TMaster read FMaster;
    property Dimensions : T3DPoint read FDimensions;
    property CodeMap[Position : T3DPoint] : TScrewCode
      read GetCodeMap write SetCodeMap;
    property Map[Position : T3DPoint] : TScrew read GetMap; default;
  end;

  {*
    Maître FunLabyrinthe
    TMaster crée et gère les différentes composantes de FunLabyrinthe.
  *}
  TMaster = class
  private
    FImagesMaster : TImagesMaster; /// Maître d'images
    FScrewsMaster : TScrewsMaster; /// Maître des cases
    FMap : TMap;                   /// Carte
    FPlayers : TObjectList;        /// Liste des joueurs

    function GetPlayerCount : integer;
    function GetPlayers(Index : integer) : TPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    property ImagesMaster : TImagesMaster read FImagesMaster;
    property ScrewsMaster : TScrewsMaster read FScrewsMaster;
    property Map : TMap read FMap;
    property PlayerCount : integer read GetPlayerCount;
    property Players[index : integer] : TPlayer read GetPlayers;
  end;

  {*
    Crée, ouvre, enregistre et gère les fichiers FunLabyrinthe
    TFunLabyFile se charge de créer, ouvrir, enregistrer et gérer les fichiers
    FunLabyrinthe (extension .lab). Elle est la seule de toutes les classes
    métier de FunLabyrinthe à « savoir » qu'il existe des fichiers.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
  *}
  TFunLabyFile = class
  private
    FFileName : TFileName; /// Nom du fichier
    FMode : TFileMode;     /// Mode d'ouverture du fichier
    FName : string;        /// Nom du labyrinthe
    FDescription : string; /// Description
    FVersion : string;     /// Version lors de l'enregistrement
    FAllowEdit : boolean;  /// Indique si le fichier peut être ouvert en édition

    FMaster : TMaster;     /// Maître FunLabyrinthe
  public
    constructor Create(const AFileName : TFileName; AMode : TFileMode);
    constructor CreateNew(AMap, AActions : TStrings);
    destructor Destroy; override;

    procedure Save(const AFileName : TFileName = '');

    property FileName : TFileName read FFileName;
    property Mode : TFileMode read FMode;
    property Name : string read FName write FName;
    property Description : string read FDescription write FDescription;
    property Version : string read FVersion;
    property AllowEdit : boolean read FAllowEdit;

    property Master : TMaster read FMaster;
  end;

var
  /// Chaîne de format pour les fichiers image
  sScrewFileName : string = 'Screws\%s.bmp';

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;

implementation

uses
  Screws;

{*
  Crée un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord supérieur du rectangle
  @return Le rectangle de type TRect
*}
function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X+ScrewSize;
  Result.Bottom := Y+ScrewSize;
end;

////////////////////////////
/// Classe TImagesMaster ///
////////////////////////////

{*
  Crée une instance de TImagesMaster
*}
constructor TImagesMaster.Create;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(ScrewSize, ScrewSize);
  FImgNames := THashedStringList.Create;
end;

{*
  Détruit l'instance
*}
destructor TImagesMaster.Destroy;
begin
  FImgNames.Free;
  FImgList.Free;
  inherited;
end;

{*
  Renvoie l'index de l'image dont le nom est spécifié
  IndexOf renvoie l'index de l'image dont le nom est spécifié dans la
  liste d'images interne. Si l'image n'a pas encore été chargée, IndexOf
  la charge.
  @param ImgName   Nom de l'image
  @return Index de l'image
*}
function TImagesMaster.IndexOf(const ImgName : string) : integer;
var NewImg : TBitmap;
begin
  Result := FImgNames.IndexOf(ImgName);
  if Result < 0 then
  begin
    NewImg := TBitmap.Create;
    try
      NewImg.LoadFromFile(Format(sScrewFileName, [ImgName]));
      FImgList.AddMasked(NewImg, clTransparent);
      Result := FImgNames.Add(ImgName);
    finally
      NewImg.Free;
    end;
  end;
end;

{*
  Dessine une image à partir de son index
  Draw dessine l'image indiquée sur un canevas.
  @param Index    Index de l'image à dessiner
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonnée X du point à partir duquel dessiner l'image
  @param Y        Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index : integer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FImgList.Draw(Canvas, X, Y, Index);
end;

{*
  Dessine une image à partir de son nom
  Draw dessine l'image indiquée sur un canevas.
  @param ImgName   Nom de l'image à dessiner
  @param Canvas    Canevas sur lequel dessiner l'image
  @param X         Coordonnée X du point à partir duquel dessiner l'image
  @param Y         Coordonnée Y du point à partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(const ImgName : string; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  Draw(IndexOf(ImgName), Canvas, X, Y);
end;

///////////////////////
/// Classe TPainter ///
///////////////////////

{*
  Crée une instance de TPainter
  @param AMaster   Maître d'images associé au peintre
*}
constructor TPainter.Create(AMaster : TImagesMaster);
begin
  inherited Create;
  FMaster := AMaster;
  FImgNames := TStringList.Create;
  TStringList(FImgNames).OnChange := ImgNamesChange;
  FCachedImg := TBitmap.Create;
  with FCachedImg do
  begin
    Width := ScrewSize;
    Height := ScrewSize;
    Canvas.Brush.Color := clTransparent;
    Canvas.Pen.Style := psClear;
  end;
  ImgNamesChange(nil);
end;

{*
  Détruit l'instance
*}
destructor TPainter.Destroy;
begin
  FCachedImg.Free;
  FImgNames.Free;
  inherited;
end;

{*
  Événement OnChange de la liste des noms des images
  ImgNamesChange est appelé lorsque la liste des noms des images change.
  Elle actualise l'image cache.
  @param Sender   Objet lançant l'événement
*}
procedure TPainter.ImgNamesChange(Sender : TObject);
var I : integer;
begin
  FCachedImg.Canvas.Rectangle(0, 0, ScrewSize, ScrewSize);
  for I := 0 to FImgNames.Count-1 do
    FMaster.Draw(FImgNames[I], FCachedImg.Canvas);
end;

{*
  Dessine les images sur un canevas
  La méthode Draw dessine les images de ImgNames sur le canevas, à la
  position indiquée. Les différentes images sont superposée, celle d'index
  0 tout au-dessous.
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPainter.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.BrushCopy(ScrewRect(X, Y), FCachedImg, ScrewRect, clTransparent);
end;

////////////////////////////////
/// Classe TFunLabyComponent ///
////////////////////////////////

{*
  Crée une instance de TFunLabyComponent
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom du composant
*}
constructor TFunLabyComponent.Create(AMaster : TMaster; const AName : string);
begin
  inherited Create;
  FMaster := AMaster;
  FName := AName;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.ImgNames.BeginUpdate;
end;

{*
  Détruit l'instance
*}
destructor TFunLabyComponent.Destroy;
begin
  FPainter.Free;
  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TFunLabyComponent.AfterConstruction;
begin
  inherited;
  FPainter.ImgNames.EndUpdate;
end;

{*
  Dessine le composant sur un canevas
  Draw dessine le composant sur un canevas à la position indiquée.
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonnée X du point à partir duquel dessiner le composant
  @param Y        Coordonnée Y du point à partir duquel dessiner le composant
*}
procedure TFunLabyComponent.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

////////////////////////////
/// Classe TPlayerPlugin ///
////////////////////////////

{*
  Crée une instance de TPlayerPlugin
  @param AMaster   Maître FunLabyrinthe
*}
constructor TPlayerPlugin.Create(AMaster : TMaster);
begin
  inherited Create;
  FMaster := AMaster;
  FPainterBefore := TPainter.Create(FMaster.ImagesMaster);
  FPainterBefore.ImgNames.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.ImgNames.BeginUpdate;
end;

{*
  Détruit l'instance
*}
destructor TPlayerPlugin.Destroy;
begin
  FPainterAfter.Free;
  FPainterBefore.Free;
  inherited;
end;

{*
  Exécuté après la construction de l'objet
  AfterConstruction est appelé après l'exécution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TPlayerPlugin.AfterConstruction;
begin
  inherited;
  FPainterBefore.ImgNames.EndUpdate;
  FPainterAfter.ImgNames.EndUpdate;
end;

{*
  Dessine sous le joueur
  DrawBefore est exécuté lors du dessin du joueur, avant celui-ci. Le dessin
  effectué dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessiné
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlayerPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterBefore.Draw(Canvas, X, Y);
end;

{*
  Dessine sur le joueur
  DrawAfter est exécuté lors du dessin du joueur, après celui-ci. Le dessin
  effectué dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessiné
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonnée X du point à partir duquel dessiner les images
  @param Y        Coordonnée Y du point à partir duquel dessiner les images
*}
procedure TPlayerPlugin.DrawAfter(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterAfter.Draw(Canvas, X, Y);
end;

{*
  Un joueur se déplace
  Moving est exécuté lorsqu'un joueur se déplace d'une case à une autre. Pour
  annuler le déplacement, Moving peut positionner le paramètre Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de départ
  @param Dest           Case d'arrivée
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TPlayerPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Un joueur s'est déplacé
  Moved est exécuté lorsqu'un joueur s'est déplacé d'une case à une autre.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de départ
  @param Dest         Case d'arrivée
*}
procedure TPlayerPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
begin
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donnée en paramètre.
  @param Player   Joueur concerné
  @param Action   Action à tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TPlayerPlugin.CanYou(Player : TPlayer; Action : integer) : boolean;
begin
  Result := False;
end;

////////////////////////////
/// Classe TPlayerObject ///
////////////////////////////

{*
  Crée une instance de TPlayerObject
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom de l'objet
  @param APlayer   Joueur propriétaire
*}
constructor TPlayerObject.Create(AMaster : TMaster; const AName : string;
  APlayer : TPlayer);
begin
  inherited Create(AMaster, AName);
  FPlayer := APlayer;
  FCount := 0;
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles à afficher pour l'objet.
  @return Informations textuelles, ou une chaîne vide si rien à afficher
*}
function TPlayerObject.GetShownInfos : string;
begin
  Result := Format(sDefaultObjectInfos, [FName, FCount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donnée
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donnée en paramètre.
  @param Action   Action à tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TPlayerObject.CanYou(Action : integer) : boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donnée
  UseFor est appelée lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donnée en paramètre.
  @param Action   Action à effectuer
*}
procedure TPlayerObject.UseFor(Action : integer);
begin
end;

//////////////////////
/// Classe TPlayer ///
//////////////////////

{*
  Crée une instance de TPlayer
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom du joueur
*}
constructor TPlayer.Create(AMaster : TMaster; const AName : string);
var Dir : TDirection;
begin
  inherited;
  FPosition := Point3D(0, 0, 0);
  FPosAddr := @FPosition;
  FDirection := diNone;
  for Dir in [diNorth..diWest] do
    FDirPainters[Dir] := nil;
  FColor := clBlue;
  FPlugins := TObjectList.Create(False);
  FObjects := TObjectList.Create;
end;

{*
  Détruit l'instance
*}
destructor TPlayer.Destroy;
var Dir : TDirection;
begin
  FObjects.Free;
  FPlugins.Free;
  for Dir in [diNorth..diWest] do if Assigned(FDirPainters[Dir]) then
    FDirPainters[Dir].Free;
  inherited;
end;

{*
  Nombre de plug-in greffés au joueur
  @return Nombre de plug-in
*}
function TPlayer.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in greffés au joueur
  @param Index   Index du plug-in dans le tableau
  @return Le plug-in à la position indiquée
*}
function TPlayer.GetPlugins(Index : integer) : TPlayerPlugin;
begin
  Result := TPlayerPlugin(FPlugins[Index]);
end;

{*
  Nombre de types d'objets du joueur
  @return Nombre de types d'objets
*}
function TPlayer.GetObjectCount : integer;
begin
  Result := FObjects.Count;
end;

{*
  Tableau zero-based des types d'objets du joueur
  @param Index   Index de l'objet dans le tableau
  @return L'objet à la position indiquée
*}
function TPlayer.GetObjects(Index : integer) : TPlayerObject;
begin
  Result := TPlayerObject(FObjects[Index]);
end;

{*
  Dessine le joueur sur un canevas
  Draw dessine le joueur sur un canevas à la position indiquée.
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonnée X du point à partir duquel dessiner le joueur
  @param Y        Coordonnée Y du point à partir duquel dessiner le joueur
*}
procedure TPlayer.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
var I : integer;
begin
  // Dessine les plug-in en-dessous du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawBefore(Self, Canvas, X, Y);

  // Dessine le peintre correspondant à la direction...
  if FColor = clDefault then
  begin
    if (FDirection = diNone) or (not Assigned(FDirPainters[FDirection])) then
      FPainter.Draw(Canvas, X, Y)
    else
      FDirPainters[FDirection].Draw(Canvas, X, Y);
  end else
  // ... ou le traditionnel disque coloré
  begin
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      Pen.Style := psClear;
      Ellipse(X+6, Y+6, X+24, Y+24);
    end;
  end;

  // Dessine les plug-in au-dessus du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawAfter(Self, Canvas, X, Y);
end;

{*
  Greffe un plug-in au joueur
  @param Plugin   Le plug-in à greffer
*}
procedure TPlayer.AddPlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Add(Plugin);
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in à retirer
*}
procedure TPlayer.RemovePlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Remove(Plugin);
end;

{*
  Affiche une boîte de dialogue
  @param Title        Titre de la boîte de dialogue
  @param Text         Texte de la boîte de dialogue
  @param DlgType      Type de boîte de dialogue
  @param DlgButtons   Boutons présents dans la boîte de dialogue
  @param DefButton    Bouton sélectionné par défaut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Code de résultat du bouton cliqué
*}
function TPlayer.ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
begin
  // En prévision d'une exécution en thread du jeu (et client-serveur)
  Result := ScUtils.ShowDialog(Title, Text, DlgType, DlgButtons,
    DefButton, AddFlags);
end;

{*
  Affiche une boîte de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title   Titre de la boîte de dialogue
  @param Text    Texte de la boîte de dialogue
  @param DlgType   Type de boîte de dialogue
  @param DlgButtons   Boutons présents dans la boîte de dialogue
  @param DefButton    Bouton sélectionné par défaut
  @param RadioTitles   Libellés des différents boutons radio
  @param Selected      Bouton radio sélectionné
  @param OverButtons   Boutons radio placés au-dessus des boutons si True
  @return Code de résultat du bouton cliqué
*}
function TPlayer.ShowDialogRadio(const Title, Text : string;
  DlgType : TMsgDlgType; DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Création de la boite de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la boite de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Création des boutons radio et détermination de la largeur minimale
    MaxWidth := 0;
    for I := High(RadioTitles) downto Low(RadioTitles) do
    with TRadioButton.Create(Form) do
    begin
      FreeNotification(Form);
      Parent := Form;
      Width := Canvas.TextWidth(RadioTitles[I]) + 20;
      MaxWidth := Max(MaxWidth, Width-20);
      Caption := RadioTitles[I];
      Checked := I = Selected;
      Tag := I;
      Left := 8;

      // OverButtons indique si les RadioBox sont au-dessus ou en-dessous des
      // boutons
      if OverButtons then
        Top := Form.Height - 90 - (High(RadioTitles) - I) * 25
      else
        Top := Form.Height - 50 - (High(RadioTitles) - I) * 25;
    end;

    // Il faut aussi vérifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On récupère chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par défaut et on le sélectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, décaler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, décaler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la boite de dialogue
    Position := poScreenCenter;

    // Affichage de la boîte de dialogue
    Result := ShowModal;

    // Récupération du choix de l'utilisateur
    Selected := -1;
    for I := 0 to ControlCount-1 do
      if (Controls[I] is TRadioButton) and TRadioButton(Controls[I]).Checked then
        Selected := Controls[I].Tag;
  finally
    Free;
  end;
end;

{*
  Indique si le joueur est capable d'effectuer une action donnée
  CanYou commence par tester si un plug-in permet l'action. Sinon, il
  détermine quels sont les objets permettant cette action. S'il y en a
  plusieurs, le joueur se voit demander d'en choisir un, et celui-ci est
  utilisé.
  @param Action   Action à tester
  @return True si le joueur est capabled d'effectuer l'action, False sinon
*}
function TPlayer.CanYou(Action : integer) : boolean;
var I, GoodObjectCount : integer;
    GoodObjects : array of TPlayerObject;
    RadioTitles : array of string;
    GoodObject : TPlayerObject;
begin
  Result := True;

  // Les plug-in ont la priorité, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do if Plugins[I].CanYou(Self, Action) then exit;

  // Listage des objets susceptibles d'aider le joueur
  SetLength(GoodObjects, ObjectCount);
  GoodObjectCount := 0;
  for I := 0 to ObjectCount-1 do if Objects[I].CanYou(Action) then
  begin
    GoodObjects[GoodObjectCount] := Objects[I];
    inc(GoodObjectCount);
  end;

  // Aucun objet trouvé : échec
  if GoodObjectCount = 0 then
  begin
    Result := False;
    exit;
  end;

  // Si plusieurs objets, demande au joueur lequel utiliser
  if GoodObjectCount = 1 then GoodObject := GoodObjects[0] else
  begin
    SetLength(RadioTitles, GoodObjectCount);
    for I := 0 to GoodObjectCount-1 do
      RadioTitles[I] := GoodObjects[I].Name;
    I := 0;
    ShowDialogRadio(sWhichObject, sWhichObject, mtConfirmation, [mbOK], mrOK,
      RadioTitles, I, True);
    GoodObject := GoodObjects[I];
  end;

  // Utilisation de l'objet
  GoodObject.UseFor(Action);
end;

{*
  Déplace le joueur dans la direction indiquée
  Move déplace le joueur dans la direction indiquée, en appliquant les
  comportements conjugués des cases et plug-in.
  @param Dir   Direction du déplacement
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Redo         Indique s'il faut réitérer le déplacement
  @return True si le déplacement a réussi, False sinon
*}
function TPlayer.Move(Dir : TDirection; KeyPressed : boolean;
  out Redo : boolean) : boolean;
var I : integer;
    Src, Dest : T3DPoint;
    OldDir : TDirection;
    Cancel, AbortEntered : boolean;
begin
  // Initialisation des variables
  Result := False;
  Redo := False;
  Src := FPosition;
  Dest := FPosition;
  case Dir of
    diNorth : dec(Dest.Y);
    diEast  : inc(Dest.X);
    diSouth : inc(Dest.Y);
    diWest  : dec(Dest.X);
    else exit;
  end;
  OldDir := FDirection;
  FDirection := Dir;
  Cancel := False;
  AbortEntered := False;

  // Premier passage : le déplacement est-il permis ?
  begin
    // Case source : exiting
    Master.Map[Src].Exiting(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Plug-in : moving
    for I := 0 to PluginCount-1 do
      Plugins[I].Moving(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Case destination : entering
    Master.Map[Dest].Entering(Self, OldDir, KeyPressed, Src, Dest, Cancel,
      AbortEntered);
    if Cancel then exit;
  end;

  // Déplacement du joueur (à moins qu'il ait été déplacé par ailleurs)
  if Same3DPoint(FPosition, Src) then
    FPosition := Dest
  else
    Dest := FPosition;

  // Second passage : le déplacement a été fait
  begin
    // Case source : exited
    Master.Map[Src].Exited(Self, KeyPressed, Src, Dest);
    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, KeyPressed, Src, Dest);
    // Case destination : entered (sauf si AbortEntered a été positionné à True)
    if not AbortEntered then
      Master.Map[Dest].Entered(Self, KeyPressed, Src, Dest, Redo);
  end;
end;

/////////////////////
/// Classe TScrew ///
/////////////////////

{*
  Crée une instance de TScrew
  @param AMaster   Maître FunLabyrinthe
  @param AName     Nom de la case
  @param ACode     Code de la case
*}
constructor TScrew.Create(AMaster : TMaster; const AName : string;
  ACode : TScrewCode);
begin
  inherited Create(AMaster, AName);
  FCode := ACode;
end;

{*
  Exécuté lorsque le joueur tente de venir sur la case
  Entering est exécuté lorsque le joueur tente de venir sur la case. Pour
  annuler le déplacement, il faut positionner Cancel à True. Pour éviter que
  la méthode Entered de la case ne soit exécutée, il faut positionner
  AbortEntered à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         À positionner à True pour annuler le déplacement
  @param AbortEntered   À positionner à True pour empêcher le Entered
*}
procedure TScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
end;

{*
  Exécuté lorsque le joueur est arrivé sur la case
  Entered est exécuté lorsque le joueur est arrivé sur la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   À positionner à True pour réitérer le déplacement
*}
procedure TScrew.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
end;

{*
  Exécuté lorsque le joueur tente de sortir de la case
  Exiting est exécuté lorsque le joueur tente de sortir de la case. Pour
  annuler le déplacement, il faut positionner Cancel à True.
  @param Player         Joueur qui se déplace
  @param OldDirection   Direction du joueur avant ce déplacement
  @param KeyPressed     True si une touche a été pressée pour le déplacement
  @param Pos            Position de la case
  @param Dest           Case de destination
  @param Cancel         À positionner à True pour annuler le déplacement
*}
procedure TScrew.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Exécuté lorsque le joueur est sorti de la case
  Exiting est exécuté lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se déplace
  @param KeyPressed   True si une touche a été pressée pour le déplacement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TScrew.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
end;

////////////////////////////
/// Classe TScrewsMaster ///
////////////////////////////

{*
  Crée une instance de TScrewsMaster
  @param AMaster   Maître FunLabyrinthe
*}
constructor TScrewsMaster.Create(AMaster : TMaster);
var Code : TScrewCode;
begin
  inherited Create;
  FMaster := AMaster;
  for Code := 33 to 255 do
    FScrews[Code] := nil;

  // Quelques exemples pour la route...
  FScrews[cGrass] := TGrass.Create(FMaster);
  FScrews[cDirectTurnstile] := TDirectTurnstile.Create(FMaster);
  FScrews[cIndirectTurnstile] := TIndirectTurnstile.Create(FMaster);
  FScrews[cOutside] := TOutside.Create(FMaster);
end;

{*
  Détruit l'instance
*}
destructor TScrewsMaster.Destroy;
var Code : TScrewCode;
begin
  for Code := 33 to 255 do if Assigned(FScrews[Code]) then
    FScrews[Code].Free;
  inherited;
end;

{*
  Tableau des cases indexé par leurs codes respectifs
  @param Code   Code de la case
  @return La case dont le code a été spécifié
*}
function TScrewsMaster.GetScrews(Code : TScrewCode) : TScrew;
begin
  Result := FScrews[Code];
end;

///////////////////
/// Classe TMap ///
///////////////////

{*
  Crée une instance de TMap
  @param AMaster       Maître FunLabyrinthe
  @param ADimensions   Dimensions de la carte (en cases)
*}
constructor TMap.Create(AMaster : TMaster; ADimensions : T3DPoint);
var X, Y, Z : integer;
begin
  inherited Create;
  FMaster := AMaster;
  FDimensions := ADimensions;

  SetLength(FMap, FDimensions.X * FDimensions.Y * FDimensions.Z);
  for X := 0 to FDimensions.X-1 do
    for Y := 0 to FDimensions.Y-1 do
      for Z := 0 to FDimensions.Z-1 do
        FMap[Z*FDimensions.X*FDimensions.Y + Y*FDimensions.X + X] := cGrass;

  SetLength(FOutside, FDimensions.Z);
  for Z := 0 to FDimensions.Z-1 do
    FOutside[Z] := cOutside;
end;

{*
  Tableau des codes des cases indexés par leur position sur la carte
  @param Position   Position sur la carte
  @return Le code de la case à la position spécifiée
*}
function TMap.GetCodeMap(Position : T3DPoint) : TScrewCode;
var Index : integer;
begin
  if InMap(Position) then
  begin
    Index := Position.Z;
    Index := Index * FDimensions.Y;
    inc(Index, Position.Y);
    Index := Index * FDimensions.X;
    inc(Index, Position.X);

    Result := FMap[Index];
  end else
  if (Position.Z < 0) or (Position.Z >= FDimensions.Z) then
    Result := FOutside[0]
  else
    Result := FOutside[Position.Z];
end;

{*
  Modifie le tableau des codes des cases indexées par leur position sur la carte
  @param Position   Position sur la carte
  @param Value      Nouveau code de case
*}
procedure TMap.SetCodeMap(Position : T3DPoint; Value : TScrewCode);
var Index : integer;
begin
  if (not InMap(Position)) or (Master.ScrewsMaster[Value] = nil) then exit;

  Index := Position.Z;
  Index := Index * FDimensions.Y;
  inc(Index, Position.Y);
  Index := Index * FDimensions.X;
  inc(Index, Position.X);

  FMap[Index] := Value;
end;

{*
  Tableau des cases indexé par leur position sur la carte
  @param Position   Position sur la carte
  @return La case à la position spécifiée
*}
function TMap.GetMap(Position : T3DPoint) : TScrew;
begin
  Result := Master.ScrewsMaster[CodeMap[Position]];
end;

{*
  Teste si une coordonnée est à l'intérieur de la carte
  @param Position   Coordonnée à tester
  @return True si la coordonnée est dans la carte, False sinon
*}
function TMap.InMap(Position : T3DPoint) : boolean;
begin
  Result :=
    (Position.X >= 0) and (Position.X < FDimensions.X) and
    (Position.Y >= 0) and (Position.Y < FDimensions.Y) and
    (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

//////////////////////
/// Classe TMaster ///
//////////////////////

{*
  Crée une instance de TMaster
*}
constructor TMaster.Create;
begin
  inherited Create;
  FImagesMaster := TImagesMaster.Create;
  FScrewsMaster := TScrewsMaster.Create(Self);
  FMap := TMap.Create(Self, Point3D(1, 1, 1));
  FPlayers := TObjectList.Create;
end;

{*
  Détruit l'instance
*}
destructor TMaster.Destroy;
begin
  FPlayers.Free;
  FMap.Free;
  FScrewsMaster.Free;
  FImagesMaster.Free;
  inherited;
end;

{*
  Nombre de joueurs dans la partie
  @return Nombre de joueurs
*}
function TMaster.GetPlayerCount : integer;
begin
  Result := FPlayers.Count;
end;

{*
  Tableau zero-based des joueurs dans la partie
  @param Index   Index du joueur
  @return Le joueur à la position spécifiée
*}
function TMaster.GetPlayers(Index : integer) : TPlayer;
begin
  Result := TPlayer(FPlayers[Index]);
end;

///////////////////////////
/// Classe TFunLabyFile ///
///////////////////////////

{*
  Ouvre un fichier FunLabyrinthe
  @param AFileName   Nom du fichier à ouvrir
  @param AMode       Mode sous lequel ouvrir le fichier
*}
constructor TFunLabyFile.Create(const AFileName : TFileName; AMode : TFileMode);
begin
  inherited Create;
  FFileName := AFileName;
  FMode := AMode;
  FName := '';
  FDescription := '';
  FVersion := '';
  FAllowEdit := True;
  FMaster := TMaster.Create;
  { TODO 2 : Charger un fichier }
end;

{*
  Crée un nouveau fichier FunLabyrinthe en mode édition
  @param AMap       Partie Carte du fichier
  @param AActions   Partie Actions du fichier
*}
constructor TFunLabyFile.CreateNew(AMap, AActions : TStrings);
begin
  inherited Create;
  FFileName := '';
  FMode := fmEdit;
  FName := '';
  FDescription := '';
  FVersion := '';
  FAllowEdit := True;
  FMaster := TMaster.Create;
  { TODO 2 : Créer un nouveau fichier }
end;

{*
  Détruit l'instance
*}
destructor TFunLabyFile.Destroy;
begin
  FMaster.Free;
  inherited;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier à enregistrer (si vide, conserve l'existant)
*}
procedure TFunLabyFile.Save(const AFileName : TFileName = '');
begin
  { TODO 2 : Enregistrer un fichier }
end;

initialization
  sScrewFileName := Dir+sScrewFileName;
end.

