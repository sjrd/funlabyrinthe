{*
  Types et classes de bases de FunLabyrinthe
  FunLabyUtils comprend les types et classes de base de FunLabyrinthe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, Controls, IniFiles, ScUtils,
  Forms, Dialogs, StdCtrls, Math, ScStrUtils;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sDefaultScrewName = '%1:s sur %0:s';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';
  sComponentNotFound = 'Le composant d''ID %s n''existe pas';

  sDescription = 'Description';
  sMessage = 'Message';
  sTip = 'Indice';
  sChoice = 'Choix';
  sFailure = '�chec';
  sBlindAlley = 'Impasse';
  sWon = 'Gagn� !';

const
  CurrentVersion = '5.0'; /// Version courante de FunLabyrinthe
  ScrewSize = 30;         /// Taille (en largeur et hauteur) d'une case
  DefaultZoneSize = 7;    /// Taille par d�faut d'une zone (en cases)
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp

type
  /// Identificateur de composant FunLabyrinthe
  TComponentID = type string;

  /// Type repr�sentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Pointeur vers T3DPoint
  P3DPoint = ^T3DPoint;

  /// G�n�r�e si un composant recherch� n'est pas trouv�
  EComponentNotFound = class(Exception);

  TMaster = class;
  TPlayer = class;

  {*
    G�re le chargement des images d'apr�s leur nom
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
    TPainter enregistre une liste d'images par leur noms et propose une m�thode
    pour les dessiner les unes sur les autres, par transparence.
  *}
  TPainter = class
  private
    FMaster : TImagesMaster; /// Ma�tre d'images
    FImgNames : TStrings;    /// Liste des noms des images
    FCachedImg : TBitmap;    /// Copie cache de l'image r�sultante

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
    FunLabyrinthe. Elle fournit des propri�t�s et des m�thodes pour rep�rer le
    ma�tre FunLabyrinthe et pour identifier le composant.
  *}
  TFunLabyComponent = class
  private
    FMaster : TMaster;  /// Ma�tre FunLabyrinthe
    FID : TComponentID; /// ID du composant
    {*
      Valeur non fonctionnelle pouvant servir au fonctionnement d'un algorithme
      Cette valeur est susceptible d'�tre utilis�e par beaucoup d'algorithmes
      diff�rents, et donc interf�rer. Il ne faut donc l'utiliser que
      ponctuellement.
    *}
    FTag : integer;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID);

    property Master : TMaster read FMaster;
    property ID : TComponentID read FID;
    property Tag : integer read FTag write FTag;
  end;

  {*
    Classe de base pour les composants devant �tre affich�s
    TVisualComponent �tend la classe TFunLabyComponent pour lui ajouter un
    traitement standart et simple de nommage et de dessin.
  *}
  TVisualComponent = class(TFunLabyComponent)
  private
    FName : string;      /// Nom du composant
    FPainter : TPainter; /// Peintre par d�faut
  protected
    property Painter : TPainter read FPainter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string);
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0); virtual;

    property Name : string read FName;
  end;

  {*
    Classe de base pour les plug-in de joueur
    TPlugin est la classe de base pour les plug-in de joueur.
    Un plug-in peut agir de plusieurs fa�ons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Emp�cher le d�placement du joueur et r�agir � son d�placement effectif ;
    - Indiquer au joueur qu'il a la capacit� de faire certaines actions.
  *}
  TPlugin = class(TFunLabyComponent)
  private
    FPainterBefore : TPainter; /// Peintre par d�faut sous le joueur
    FPainterAfter : TPainter;  /// Peintre par d�faut sur le joueur
  protected
    property PainterBefore : TPainter read FPainterBefore;
    property PainterAfter : TPainter read FPainterAfter;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID);
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
    Classe de base pour les d�finitions d'objets
    TObjectDef est la classe de base pour les d�finitions d'objets que poss�de
    le joueur.
    Les objets peuvent rendre un joueur capable d'effectuer certaines actions.
  *}
  TObjectDef = class(TVisualComponent)
  private
    function GetCount(Player : TPlayer) : integer;
    procedure SetCount(Player : TPlayer; Value : integer);
  protected
    function GetShownInfos(Player : TPlayer) : string; virtual;
  public
    function CanYou(Player : TPlayer; Action : integer) : boolean; virtual;
    procedure UseFor(Player : TPlayer; Action : integer); virtual;

    property Count[Player : TPlayer] : integer read GetCount write SetCount;
    property ShownInfos[Player : TPlayer] : string read GetShownInfos;
  end;

  {*
    Classe de base pour les terrains
    TField est la classe de base pour la cr�ation de terrains. Les terrains
    sont la premi�re composante d'une case.
  *}
  TField = class(TVisualComponent)
  private
    FDelegateDrawTo : TField; /// Terrain d�l�gu� pour l'affichage
  protected
    procedure DrawField(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); virtual;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; ADelegateDrawTo : TField = nil);

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Src, Pos : T3DPoint;
      var Cancel, AbortEntered : boolean); virtual;
    procedure Exiting(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; Pos, Dest : T3DPoint;
      var Cancel : boolean); virtual;
  end;

  {*
    Classe de base pour les effets de case
    TEffect est la classe de base la cr�ation d'effets de case. Les effets
    sont la seconde composante d'une case.
  *}
  TEffect = class(TVisualComponent)
  public
    procedure Pushed(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint); virtual;
    procedure Entered(Player : TPlayer; KeyPressed : boolean;
      Src, Pos : T3DPoint; var GoOnMoving : boolean); virtual;
    procedure Exited(Player : TPlayer; KeyPressed : boolean;
      Pos, Dest : T3DPoint); virtual;
  end;

  {*
    Classe de base pour les cases
    TScrew est la classe de base pour les cases de la carte. Elle poss�de une
    code et quatre m�thodes qui d�finissent son comportement.
  *}
  TScrew = class(TVisualComponent)
  private
    FField : TField;   /// Terrain
    FEffect : TEffect; /// Effet
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AField : TField; AEffect : TEffect);

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    property Field : TField read FField;
    property Effect : TEffect read FEffect;
  end;

  {*
    Repr�sente la carte du jeu
    TMap g�re et repr�sente la carte du jeu. Elle offre des propri�t�s et
    m�thodes pour lire et modifier cette carte.
  *}
  TMap = class(TFunLabyComponent)
  private
    FDimensions : T3DPoint;   /// Dimensions de la carte (en cases)
    FZoneSize : integer;      /// Taille d'une zone de la carte
    FMap : array of TScrew;   /// Carte stock�e de fa�on lin�aire
    FOutsideOffset : integer; /// Offset de d�part de l'ext�rieur

    function GetMap(Position : T3DPoint) : TScrew;
    procedure SetMap(Position : T3DPoint; Value : TScrew);

    function GetOutside(Floor : integer) : TScrew;
    procedure SetOutside(Floor : integer; Value : TScrew);

    function GetLinearMapCount : integer;
    function GetLinearMap(Index : integer) : TScrew;
    procedure SetLinearMap(Index : integer; Value : TScrew);
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      ADimensions : T3DPoint);

    function InMap(Position : T3DPoint) : boolean;

    property Dimensions : T3DPoint read FDimensions;
    property ZoneSize : integer read FZoneSize;

    property Map[Position : T3DPoint] : TScrew
      read GetMap write SetMap; default;

    property Outside[Floor : integer] : TScrew
      read GetOutside write SetOutside;

    property LinearMapCount : integer read GetLinearMapCount;
    property LinearMap[index : integer] : TScrew
      read GetLinearMap write SetLinearMap;
  end;

  {*
    Classe repr�sentant un joueur
    TPlayer repr�sente un joueur. Elle poss�de de nombreuses propri�t�s et
    m�thodes permettant d'afficher le joueur, de le d�placer, de lui greffer
    des plug-in, etc.
  *}
  TPlayer = class(TVisualComponent)
  private
    FMap : TMap;             /// Carte
    FPosition : T3DPoint;    /// Position
    FDirection : TDirection; /// Direction
    /// Peintres selon la direction du joueur
    FDirPainters : array[diNorth..diWest] of TPainter;
    FColor : TColor;         /// Couleur
    FPlugins : TObjectList;  /// Liste des plug-in
    FAttributes : TStrings;  /// Liste des attributs

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlugin;

    function GetAttribute(const AttrName : string) : integer;
    procedure SetAttribute(const AttrName : string; Value : integer);

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlugin read GetPlugins;
  public
    constructor Create(AMaster : TMaster; const AID : TComponentID;
      const AName : string; AMap : TMap; APosition : T3DPoint);
    destructor Destroy; override;

    procedure Draw(Canvas : TCanvas; X : integer = 0;
      Y : integer = 0); override;

    procedure AddPlugin(Plugin : TPlugin);
    procedure RemovePlugin(Plugin : TPlugin);

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

    property Map : TMap read FMap;
    property Position : T3DPoint read FPosition write FPosition;
    property Direction : TDirection read FDirection write FDirection;
    property Color : TColor read FColor write FColor;
    property Attribute[const AttrName : string] : integer
      read GetAttribute write SetAttribute;
  end;

  {*
    Ma�tre FunLabyrinthe
    TMaster g�re les diff�rents composants de FunLabyrinthe.
  *}
  TMaster = class
  private
    FImagesMaster : TImagesMaster; /// Ma�tre d'images
    FComponents : TStrings;        /// Table de hashage ID -> composant
    FPlugins : TObjectList;        /// Liste des plug-in
    FObjectDefs : TObjectList;     /// Liste des d�finitions d'objet
    FFields : TObjectList;         /// Liste des terrains
    FEffects : TObjectList;        /// Liste des effets
    FScrews : TObjectList;         /// Liste des cases
    FMaps : TObjectList;           /// Liste des cartes
    FPlayers : TObjectList;        /// Liste des joueurs

    function GetComponent(const ID : TComponentID) : TFunLabyComponent;
    function GetPlugin   (const ID : TComponentID) : TPlugin;
    function GetObjectDef(const ID : TComponentID) : TObjectDef;
    function GetField    (const ID : TComponentID) : TField;
    function GetEffect   (const ID : TComponentID) : TEffect;
    function GetScrew    (const ID : TComponentID) : TScrew;
    function GetMap      (const ID : TComponentID) : TMap;
    function GetPlayer   (const ID : TComponentID) : TPlayer;

    function GetPluginCount : integer;
    function GetPlugins(Index : integer) : TPlugin;
    function GetObjectDefCount : integer;
    function GetObjectDefs(Index : integer) : TObjectDef;
    function GetFieldCount : integer;
    function GetFields(Index : integer) : TField;
    function GetEffectCount : integer;
    function GetEffects(Index : integer) : TEffect;
    function GetScrewCount : integer;
    function GetScrews(Index : integer) : TScrew;
    function GetMapCount : integer;
    function GetMaps(Index : integer) : TMap;
    function GetPlayerCount : integer;
    function GetPlayers(Index : integer) : TPlayer;

    procedure AddComponent(Component : TFunLabyComponent);
  public
    constructor Create;
    destructor Destroy; override;

    property ImagesMaster : TImagesMaster read FImagesMaster;

    property Component[const ID : TComponentID] : TFunLabyComponent
      read GetComponent;
    property Plugin   [const ID : TComponentID] : TPlugin    read GetPlugin;
    property ObjectDef[const ID : TComponentID] : TObjectDef read GetObjectDef;
    property Field    [const ID : TComponentID] : TField     read GetField;
    property Effect   [const ID : TComponentID] : TEffect    read GetEffect;
    property Screw    [const ID : TComponentID] : TScrew     read GetScrew;
    property Map      [const ID : TComponentID] : TMap       read GetMap;
    property Player   [const ID : TComponentID] : TPlayer    read GetPlayer;

    property PluginCount : integer read GetPluginCount;
    property Plugins[index : integer] : TPlugin read GetPlugins;
    property ObjectDefCount : integer read GetObjectDefCount;
    property ObjectDefs[index : integer] : TObjectDef read GetObjectDefs;
    property FieldCount : integer read GetFieldCount;
    property Fields[index : integer] : TField read GetFields;
    property EffectCount : integer read GetEffectCount;
    property Effects[index : integer] : TEffect read GetEffects;
    property ScrewCount : integer read GetScrewCount;
    property Screws[index : integer] : TScrew read GetScrews;
    property MapCount : integer read GetMapCount;
    property Maps[index : integer] : TMap read GetMaps;
    property PlayerCount : integer read GetPlayerCount;
    property Players[index : integer] : TPlayer read GetPlayers;
  end;

const {don't localize}
  /// Fichier INI de FunLabyrinthe
  fIniFileName = 'FunLabyrinthe.ini';

var {don't localize}
  /// Dossier de FunLabyrinthe dans Application Data
  fFunLabyAppData : string = '';
  /// Dossier des fichiers image
  fScrewsDir : string = 'Screws\';
  /// Dossier des fichiers son
  fSoundsDir : string = 'Sounds\';
  /// Dossier des unit�s
  fUnitsDir : string = 'Units\';
  /// Dossier des cartes
  fMapsDir : string = 'Maps\';
  /// Dossier des fichiers labyrinthe
  fLabyrinthsDir : string = 'Labyrinths\';
  /// Dossier des fichiers sauvegarde
  fSaveguardsDir : string = 'Saveguards\';

  /// Cha�ne de format pour les fichiers image
  fScrewFileName : string = '%s.bmp';

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;

implementation

{*
  Cr�e un rectangle de la taille d'une case
  @param X   Bord gauche du rectangle
  @param Y   Bord sup�rieur du rectangle
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
  Cr�e une instance de TImagesMaster
*}
constructor TImagesMaster.Create;
begin
  inherited Create;
  FImgList := TImageList.CreateSize(ScrewSize, ScrewSize);
  FImgNames := THashedStringList.Create;
end;

{*
  D�truit l'instance
*}
destructor TImagesMaster.Destroy;
begin
  FImgNames.Free;
  FImgList.Free;
  inherited;
end;

{*
  Renvoie l'index de l'image dont le nom est sp�cifi�
  IndexOf renvoie l'index de l'image dont le nom est sp�cifi� dans la
  liste d'images interne. Si l'image n'a pas encore �t� charg�e, IndexOf
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
      NewImg.LoadFromFile(Format(fScrewFileName, [ImgName]));
      FImgList.AddMasked(NewImg, clTransparent);
      Result := FImgNames.Add(ImgName);
    finally
      NewImg.Free;
    end;
  end;
end;

{*
  Dessine une image � partir de son index
  Draw dessine l'image indiqu�e sur un canevas.
  @param Index    Index de l'image � dessiner
  @param Canvas   Canevas sur lequel dessiner l'image
  @param X        Coordonn�e X du point � partir duquel dessiner l'image
  @param Y        Coordonn�e Y du point � partir duquel dessiner l'image
*}
procedure TImagesMaster.Draw(Index : integer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FImgList.Draw(Canvas, X, Y, Index);
end;

{*
  Dessine une image � partir de son nom
  Draw dessine l'image indiqu�e sur un canevas.
  @param ImgName   Nom de l'image � dessiner
  @param Canvas    Canevas sur lequel dessiner l'image
  @param X         Coordonn�e X du point � partir duquel dessiner l'image
  @param Y         Coordonn�e Y du point � partir duquel dessiner l'image
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
  Cr�e une instance de TPainter
  @param AMaster   Ma�tre d'images associ� au peintre
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
    Canvas.Pen.Color := clTransparent;
  end;
  ImgNamesChange(nil);
end;

{*
  D�truit l'instance
*}
destructor TPainter.Destroy;
begin
  FCachedImg.Free;
  FImgNames.Free;
  inherited;
end;

{*
  �v�nement OnChange de la liste des noms des images
  ImgNamesChange est appel� lorsque la liste des noms des images change.
  Elle actualise l'image cache.
  @param Sender   Objet lan�ant l'�v�nement
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
  La m�thode Draw dessine les images de ImgNames sur le canevas, � la
  position indiqu�e. Les diff�rentes images sont superpos�e, celle d'index
  0 tout au-dessous.
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
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
  Cr�e une instance de TFunLabyComponent
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du composant
*}
constructor TFunLabyComponent.Create(AMaster : TMaster;
  const AID : TComponentID);
begin
  inherited Create;
  FMaster := AMaster;
  FID := AID;
  Master.AddComponent(Self);
end;

///////////////////////////////
/// Classe TVisualComponent ///
///////////////////////////////

{*
  Cr�e une instance de TVisualComponent
*}
constructor TVisualComponent.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string);
begin
  inherited Create(AMaster, AID);
  FName := AName;
  FPainter := TPainter.Create(FMaster.ImagesMaster);
  FPainter.ImgNames.BeginUpdate;
end;

{*
  D�truit l'instance
*}
destructor TVisualComponent.Destroy;
begin
  FPainter.Free;
  inherited;
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TVisualComponent.AfterConstruction;
begin
  inherited;
  FPainter.ImgNames.EndUpdate;
end;

{*
  Dessine le composant sur un canevas
  Draw dessine le composant sur un canevas � la position indiqu�e.
  @param Canvas   Canevas sur lequel dessiner le composant
  @param X        Coordonn�e X du point � partir duquel dessiner le composant
  @param Y        Coordonn�e Y du point � partir duquel dessiner le composant
*}
procedure TVisualComponent.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

//////////////////////
/// Classe TPlugin ///
//////////////////////

{*
  Cr�e une instance de TPlugin
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID du plug-in
*}
constructor TPlugin.Create(AMaster : TMaster; const AID : TComponentID);
begin
  inherited Create(AMaster, AID);
  FPainterBefore := TPainter.Create(FMaster.ImagesMaster);
  FPainterBefore.ImgNames.BeginUpdate;
  FPainterAfter := TPainter.Create(FMaster.ImagesMaster);
  FPainterAfter.ImgNames.BeginUpdate;
end;

{*
  D�truit l'instance
*}
destructor TPlugin.Destroy;
begin
  FPainterAfter.Free;
  FPainterBefore.Free;
  inherited;
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TPlugin.AfterConstruction;
begin
  inherited;
  FPainterBefore.ImgNames.EndUpdate;
  FPainterAfter.ImgNames.EndUpdate;
end;

{*
  Dessine sous le joueur
  DrawBefore est ex�cut� lors du dessin du joueur, avant celui-ci. Le dessin
  effectu� dans DrawBefore se retrouve donc sous le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterBefore.Draw(Canvas, X, Y);
end;

{*
  Dessine sur le joueur
  DrawAfter est ex�cut� lors du dessin du joueur, apr�s celui-ci. Le dessin
  effectu� dans DrawAfter se retrouve donc sur le joueur.
  @param Player   Joueur qui est dessin�
  @param Canvas   Canevas sur lequel dessiner les images
  @param X        Coordonn�e X du point � partir duquel dessiner les images
  @param Y        Coordonn�e Y du point � partir duquel dessiner les images
*}
procedure TPlugin.DrawAfter(Player : TPlayer; Canvas : TCanvas;
  X : integer = 0; Y : integer = 0);
begin
  FPainterAfter.Draw(Canvas, X, Y);
end;

{*
  Un joueur se d�place
  Moving est ex�cut� lorsqu'un joueur se d�place d'une case � une autre. Pour
  annuler le d�placement, Moving peut positionner le param�tre Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de d�part
  @param Dest           Case d'arriv�e
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Un joueur s'est d�plac�
  Moved est ex�cut� lorsqu'un joueur s'est d�plac� d'une case � une autre.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de d�part
  @param Dest         Case d'arriv�e
*}
procedure TPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
  Src, Dest : T3DPoint);
begin
end;

{*
  Indique si le plug-in permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si le plug-in permet au joueur d'effectuer
  l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si le joueur est capable d'effectuer l'action, False sinon
*}
function TPlugin.CanYou(Player : TPlayer; Action : integer) : boolean;
begin
  Result := False;
end;

/////////////////////////
/// Classe TObjectDef ///
/////////////////////////

{*
  Nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
  @return Nombre d'objets que ce joueur poss�de
*}
function TObjectDef.GetCount(Player : TPlayer) : integer;
begin
  Result := Player.Attribute[ID];
end;

{*
  Modifie le nombre d'objets de ce type poss�d�s par un joueur
  @param Player   Joueur concern�
  @param Value    Nouveau nombre d'objets
*}
procedure TObjectDef.SetCount(Player : TPlayer; Value : integer);
begin
  Player.Attribute[ID] := Value;
end;

{*
  Informations textuelles sur l'objet
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @param Player   Joueur pour lequel on veut obtenir les infos
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TObjectDef.GetShownInfos(Player : TPlayer) : string;
begin
  Result := Format(sDefaultObjectInfos, [Name, Count[Player]]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TObjectDef.CanYou(Player : TPlayer; Action : integer) : boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Player   Joueur concern�
  @param Action   Action � effectuer
*}
procedure TObjectDef.UseFor(Player : TPlayer; Action : integer);
begin
end;

/////////////////////
/// Classe TField ///
/////////////////////

{*
  Cr�e une instance de TField
  @param AMaster           Ma�tre FunLabyrinthe
  @param AID               ID du terrain
  @param AName             Nom du terrain
  @param ADelegateDrawTo   Un autre terrain auquel d�l�guer l'affichage
*}
constructor TField.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; ADelegateDrawTo : TField = nil);
begin
  inherited Create(AMaster, AID, AName);
  FDelegateDrawTo := ADelegateDrawTo;
end;

{*
  Dessine le terrain sur le canevas indiqu�
  Les descendants de TField doivent r�impl�menter DrawField plut�t que Draw.
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TField.DrawField(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  inherited Draw(Canvas, X, Y);
end;

{*
  Dessine le terrain sur le canevas indiqu�, ou d�l�gue le dessin
  Les descendants de TField ne doivent pas r�impl�menter Draw, mais DrawField
  @param Canvas   Canevas sur lequel dessiner le terrain
  @param X        Coordonn�e X du point � partir duquel dessiner le terrain
  @param Y        Coordonn�e Y du point � partir duquel dessiner le terrain
*}
procedure TField.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  if FDelegateDrawTo = nil then
    DrawField(Canvas, X, Y)
  else
    FDelegateDrawTo.Draw(Canvas, X, Y);
end;

{*
  Ex�cut� lorsque le joueur tente de venir sur la case
  Entering est ex�cut� lorsque le joueur tente de venir sur la case. Pour
  annuler le d�placement, il faut positionner Cancel � True. Pour �viter que
  la m�thode Entered de la case ne soit ex�cut�e, il faut positionner
  AbortEntered � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Src            Case de provenance
  @param Pos            Position de la case
  @param Cancel         � positionner � True pour annuler le d�placement
  @param AbortEntered   � positionner � True pour emp�cher le Entered
*}
procedure TField.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
end;

{*
  Ex�cut� lorsque le joueur tente de sortir de la case
  Exiting est ex�cut� lorsque le joueur tente de sortir de la case. Pour
  annuler le d�placement, il faut positionner Cancel � True.
  @param Player         Joueur qui se d�place
  @param OldDirection   Direction du joueur avant ce d�placement
  @param KeyPressed     True si une touche a �t� press�e pour le d�placement
  @param Pos            Position de la case
  @param Dest           Case de destination
  @param Cancel         � positionner � True pour annuler le d�placement
*}
procedure TField.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

//////////////////////
/// Classe TEffect ///
//////////////////////

{*
  Ex�cut� lorsque le joueur a pouss� sur la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
*}
procedure TEffect.Pushed(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint);
begin
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TEffect.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
begin
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Pos          Position de la case
  @param Dest         Case de destination
*}
procedure TEffect.Exited(Player : TPlayer; KeyPressed : boolean;
  Pos, Dest : T3DPoint);
begin
end;

/////////////////////
/// Classe TScrew ///
/////////////////////

{*
  Cr�e une instance de TScrew
  @param AMaster   Ma�tre FunLabyrinthe
  @param AID       ID de la case
  @param AName     Nom de la case
  @param AField    Terrain
  @param AEffect   Effet
*}
constructor TScrew.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AField : TField; AEffect : TEffect);
begin
  inherited Create(AMaster, AID, AName);
  FField := AField;
  FEffect := AEffect;
end;

procedure TScrew.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
begin
  Field.Draw(Canvas, X, Y);
  Effect.Draw(Canvas, X, Y);
end;

///////////////////
/// Classe TMap ///
///////////////////

{*
  Cr�e une instance de TMap
  @param AMaster       Ma�tre FunLabyrinthe
  @param ADimensions   Dimensions de la carte (en cases)
*}
constructor TMap.Create(AMaster : TMaster; const AID : TComponentID;
  ADimensions : T3DPoint);
var I : integer;
begin
  inherited Create(AMaster, AID);
  FDimensions := ADimensions;
  FZoneSize := DefaultZoneSize;

  FOutsideOffset := FDimensions.X * FDimensions.Y * FDimensions.Z;
  SetLength(FMap, FOutsideOffset + FDimensions.Z);
  for I := Low(FMap) to High(FMap) do
    FMap[I] := nil;
end;

{*
  Tableau des cases index� par leur position sur la carte
  @param Position   Position sur la carte
  @return La case � la position sp�cifi�e
*}
function TMap.GetMap(Position : T3DPoint) : TScrew;
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
  end else Result := Outside[Position.Z];
end;

{*
  Modifie le tableau des cases index� par leur position sur la carte
  @param Position   Position sur la carte
  @param Value      Nouvelle case
*}
procedure TMap.SetMap(Position : T3DPoint; Value : TScrew);
var Index : integer;
begin
  if not InMap(Position) then exit;

  Index := Position.Z;
  Index := Index * FDimensions.Y;
  inc(Index, Position.Y);
  Index := Index * FDimensions.X;
  inc(Index, Position.X);

  FMap[Index] := Value;
end;

{*
  Tableau des cases hors de la carte index� par �tage
  @param Floor   �tage
  @return La case hors de la carte � l'�tage sp�cifi�
*}
function TMap.GetOutside(Floor : integer) : TScrew;
begin
  if Floor < 0 then Floor := 0 else
  if Floor >= FDimensions.Z then Floor := FDimensions.Z-1;
  Result := FMap[Floor + FOutsideOffset];
end;

{*
  Modifie le tableau des cases hors de la carte index� par �tage
  @param Floor   �tage
  @param Value   Nouvelle case
*}
procedure TMap.SetOutside(Floor : integer; Value : TScrew);
begin
  if (Floor >= 0) and (Floor < FDimensions.Z) then
    FMap[Floor + FOutsideOffset] := Value;
end;

{*
  Taille du tableau de la carte lin�aire
  @return Taille du tableau de la carte lin�aire
*}
function TMap.GetLinearMapCount : integer;
begin
  Result := Length(FMap);
end;

{*
  Tableau zero-based de la carte lin�aire
  @param Index   Index dans le tableau
  @return Case de la carte lin�aire � l'index sp�cifi�
*}
function TMap.GetLinearMap(Index : integer) : TScrew;
begin
  Result := FMap[Index];
end;

{*
  Modifie le tableau zero-based de la carte lin�aire
  @param Index   Index dans le tableau
  @param Value   Nouvelle case
*}
procedure TMap.SetLinearMap(Index : integer; Value : TScrew);
begin
  FMap[Index] := Value;
end;

{*
  Teste si une coordonn�e est � l'int�rieur de la carte
  @param Position   Coordonn�e � tester
  @return True si la coordonn�e est dans la carte, False sinon
*}
function TMap.InMap(Position : T3DPoint) : boolean;
begin
  Result :=
    (Position.X >= 0) and (Position.X < FDimensions.X) and
    (Position.Y >= 0) and (Position.Y < FDimensions.Y) and
    (Position.Z >= 0) and (Position.Z < FDimensions.Z);
end;

//////////////////////
/// Classe TPlayer ///
//////////////////////

{*
  Cr�e une instance de TPlayer
  @param AMaster     Ma�tre FunLabyrinthe
  @param AID         ID du joueur
  @param AName       Nom du joueur
  @param AMap        Carte de d�part
  @param APosition   Position de d�part
*}
constructor TPlayer.Create(AMaster : TMaster; const AID : TComponentID;
  const AName : string; AMap : TMap; APosition : T3DPoint);
var Dir : TDirection;
begin
  inherited Create(AMaster, AID, AName);
  FMap := AMap;
  FPosition := APosition;
  FDirection := diNone;
  for Dir in [diNorth..diWest] do
    FDirPainters[Dir] := nil;
  FColor := clBlue;
  FPlugins := TObjectList.Create(False);
  FAttributes := THashedStringList.Create;
  TStringList(FAttributes).CaseSensitive := True;
end;

{*
  D�truit l'instance
*}
destructor TPlayer.Destroy;
var Dir : TDirection;
begin
  FAttributes.Free;
  FPlugins.Free;
  for Dir in [diNorth..diWest] do if Assigned(FDirPainters[Dir]) then
    FDirPainters[Dir].Free;
  inherited;
end;

{*
  Nombre de plug-in greff�s au joueur
  @return Nombre de plug-in
*}
function TPlayer.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in greff�s au joueur
  @param Index   Index du plug-in dans le tableau
  @return Le plug-in � la position indiqu�e
*}
function TPlayer.GetPlugins(Index : integer) : TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Tableau index� par cha�ne des attributs du joueur
  @param AttrName   Nom de l'attribut � r�cup�rer
  @return Attribut dont le nom a �t� sp�cifi�
*}
function TPlayer.GetAttribute(const AttrName : string) : integer;
var Index : integer;
begin
  Index := FAttributes.IndexOf(AttrName);
  if Index < 0 then Result := 0 else
    Result := integer(FAttributes.Objects[Index]);
end;

{*
  Modifie le tableau index� par cha�ne des attributs du joueur
  @param AttrName   Nom de l'attribut � modifier
  @param Value      Nouvelle valeur de l'attribut
*}
procedure TPlayer.SetAttribute(const AttrName : string; Value : integer);
var Index : integer;
begin
  Index := FAttributes.IndexOf(AttrName);
  if Index < 0 then
  begin
    if Value <> 0 then
      FAttributes.AddObject(AttrName, TObject(Value));
  end else
  begin
    if Value = 0 then FAttributes.Delete(Index) else
      FAttributes.Objects[Index] := TObject(Value);
  end;
end;

{*
  Dessine le joueur sur un canevas
  Draw dessine le joueur sur un canevas � la position indiqu�e.
  @param Canvas   Canevas sur lequel dessiner le joueur
  @param X        Coordonn�e X du point � partir duquel dessiner le joueur
  @param Y        Coordonn�e Y du point � partir duquel dessiner le joueur
*}
procedure TPlayer.Draw(Canvas : TCanvas; X : integer = 0; Y : integer = 0);
var I : integer;
begin
  // Dessine les plug-in en-dessous du joueur
  for I := 0 to PluginCount-1 do
    Plugins[I].DrawBefore(Self, Canvas, X, Y);

  // Dessine le peintre correspondant � la direction...
  if FColor = clDefault then
  begin
    if (FDirection = diNone) or (not Assigned(FDirPainters[FDirection])) then
      Painter.Draw(Canvas, X, Y)
    else
      FDirPainters[FDirection].Draw(Canvas, X, Y);
  end else
  // ... ou le traditionnel disque color�
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
  @param Plugin   Le plug-in � greffer
*}
procedure TPlayer.AddPlugin(Plugin : TPlugin);
begin
  FPlugins.Add(Plugin);
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in � retirer
*}
procedure TPlayer.RemovePlugin(Plugin : TPlugin);
begin
  FPlugins.Remove(Plugin);
end;

{*
  Affiche une bo�te de dialogue
  @param Title        Titre de la bo�te de dialogue
  @param Text         Texte de la bo�te de dialogue
  @param DlgType      Type de bo�te de dialogue
  @param DlgButtons   Boutons pr�sents dans la bo�te de dialogue
  @param DefButton    Bouton s�lectionn� par d�faut
  @param AddFlags     Flags additionnels pour MessageBox
  @return Code de r�sultat du bouton cliqu�
*}
function TPlayer.ShowDialog(const Title, Text : string;
  DlgType : TDialogType = dtInformation; DlgButtons : TDialogButtons = dbOK;
  DefButton : Byte = 1; AddFlags : LongWord = 0) : TDialogResult;
begin
  // En pr�vision d'une ex�cution en thread du jeu (et client-serveur)
  Result := ScUtils.ShowDialog(Title, Text, DlgType, DlgButtons,
    DefButton, AddFlags);
end;

{*
  Affiche une bo�te de dialogue avec des boutons radio
  ShowDialogRadio est une variante de ShowDialog qui affiche des boutons radio
  pour chaque choix possible.
  @param Title   Titre de la bo�te de dialogue
  @param Text    Texte de la bo�te de dialogue
  @param DlgType   Type de bo�te de dialogue
  @param DlgButtons   Boutons pr�sents dans la bo�te de dialogue
  @param DefButton    Bouton s�lectionn� par d�faut
  @param RadioTitles   Libell�s des diff�rents boutons radio
  @param Selected      Bouton radio s�lectionn�
  @param OverButtons   Boutons radio plac�s au-dessus des boutons si True
  @return Code de r�sultat du bouton cliqu�
*}
function TPlayer.ShowDialogRadio(const Title, Text : string;
  DlgType : TMsgDlgType; DlgButtons : TMsgDlgButtons; DefButton : TModalResult;
  const RadioTitles : array of string; var Selected : integer;
  OverButtons : boolean = False) : Word;
var Form : TForm;
    I, MaxWidth, OldWidth : integer;
    Button : TButton;
begin
  // Cr�ation de la boite de dialogue
  Form := CreateMessageDialog(Text, DlgType, DlgButtons);

  with Form do
  try
    Caption := Title;
    // On augmente la taille de la boite de dialogue
    Height := Height + Length(RadioTitles) * 25;

    // Cr�ation des boutons radio et d�termination de la largeur minimale
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

    // Il faut aussi v�rifier que la fiche peut afficher les textes des RadioBox
    // en entier
    OldWidth := 0;
    if (MaxWidth + 40) > Width then
    begin
      OldWidth := Width;
      Width := MaxWidth +40;
    end;

    for I := 0 to ComponentCount-1 do
    begin
      // On r�cup�re chaque bouton
      if Components[I] is TButton then
      begin
        Button := TButton(Components[I]);

        // On met le bon bouton par d�faut et on le s�lectionne
        Button.Default := Button.ModalResult = DefButton;
        if Button.Default then ActiveControl := Button;

        // S'il le faut, d�caler tous les boutons vers le bas
        if OverButtons then
          Button.Top := Button.Top + Length(RadioTitles) * 25;

        // S'il le faut, d�caler tous les boutons vers la droite
        if OldWidth > 0 then
          Button.Left := Button.Left + (Width - OldWidth) div 2;
      end;
    end;

    // On centre la boite de dialogue
    Position := poScreenCenter;

    // Affichage de la bo�te de dialogue
    Result := ShowModal;

    // R�cup�ration du choix de l'utilisateur
    Selected := -1;
    for I := 0 to ControlCount-1 do
      if (Controls[I] is TRadioButton) and TRadioButton(Controls[I]).Checked then
        Selected := Controls[I].Tag;
  finally
    Free;
  end;
end;

{*
  Indique si le joueur est capable d'effectuer une action donn�e
  CanYou commence par tester si un plug-in permet l'action. Sinon, il
  d�termine quels sont les objets permettant cette action. S'il y en a
  plusieurs, le joueur se voit demander d'en choisir un, et celui-ci est
  utilis�.
  @param Action   Action � tester
  @return True si le joueur est capabled d'effectuer l'action, False sinon
*}
function TPlayer.CanYou(Action : integer) : boolean;
var I, GoodObjectCount : integer;
    GoodObjects : array of TObjectDef;
    RadioTitles : array of string;
    GoodObject : TObjectDef;
begin
  Result := True;

  // Les plug-in ont la priorit�, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do if Plugins[I].CanYou(Self, Action) then exit;

  // Listage des objets susceptibles d'aider le joueur
  SetLength(GoodObjects, Master.ObjectDefCount);
  GoodObjectCount := 0;
  for I := 0 to Master.ObjectDefCount-1 do
  begin
    if Master.ObjectDefs[I].CanYou(Self, Action) then
    begin
      GoodObjects[GoodObjectCount] := Master.ObjectDefs[I];
      inc(GoodObjectCount);
    end;
  end;

  // Aucun objet trouv� : �chec
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
  GoodObject.UseFor(Self, Action);
end;

{*
  D�place le joueur dans la direction indiqu�e
  Move d�place le joueur dans la direction indiqu�e, en appliquant les
  comportements conjugu�s des cases et plug-in.
  @param Dir   Direction du d�placement
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Redo         Indique s'il faut r�it�rer le d�placement
  @return True si le d�placement a r�ussi, False sinon
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

  // Premier passage : le d�placement est-il permis ?
  try
    // Case source : exiting
    Map[Src].Field.Exiting(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Plug-in : moving
    for I := 0 to PluginCount-1 do
      Plugins[I].Moving(Self, OldDir, KeyPressed, Src, Dest, Cancel);
    if Cancel then exit;
    // Case destination : entering
    Map[Dest].Field.Entering(Self, OldDir, KeyPressed, Src, Dest, Cancel,
      AbortEntered);
    if Cancel then exit;
  finally
    // Case destination : pushed (seulement si on a annul�)
    if Cancel then
      Map[Dest].Effect.Pushed(Self, KeyPressed, Src, Dest);
  end;

  // D�placement du joueur (� moins qu'il ait �t� d�plac� par ailleurs)
  if Same3DPoint(FPosition, Src) then
    FPosition := Dest
  else
    Dest := FPosition;

  // Second passage : le d�placement a �t� fait
  begin
    // Case source : exited
    Map[Src].Effect.Exited(Self, KeyPressed, Src, Dest);
    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, KeyPressed, Src, Dest);
    // Case destination : entered (sauf si AbortEntered a �t� positionn� � True)
    if not AbortEntered then
      Map[Dest].Effect.Entered(Self, KeyPressed, Src, Dest, Redo);
  end;
end;

//////////////////////
/// Classe TMaster ///
//////////////////////

{*
  Cr�e une instance de TMaster
*}
constructor TMaster.Create;
begin
  inherited Create;
  FImagesMaster := TImagesMaster.Create;
  FComponents := THashedStringList.Create;
  with TStringList(FComponents) do
  begin
    CaseSensitive := True;
    Duplicates := dupError;
  end;

  FPlugins    := TObjectList.Create;
  FObjectDefs := TObjectList.Create;
  FFields     := TObjectList.Create;
  FEffects    := TObjectList.Create;
  FScrews     := TObjectList.Create;
  FMaps       := TObjectList.Create;
  FPlayers    := TObjectList.Create;
end;

{*
  D�truit l'instance
*}
destructor TMaster.Destroy;
begin
  FPlayers.Free;
  FMaps.Free;
  FScrews.Free;
  FEffects.Free;
  FFields.Free;
  FObjectDefs.Free;
  FPlugins.Free;

  FComponents.Free;
  FImagesMaster.Free;
  inherited;
end;

{*
  Tableau des composants index� par leur ID
  @param ID   ID du composant � trouver
  @return Le composant dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun composant ne correspond � l'ID sp�cifi�
*}
function TMaster.GetComponent(const ID : TComponentID) : TFunLabyComponent;
var Index : integer;
begin
  Index := FComponents.IndexOf(ID);
  if Index >= 0 then
    Result := TFunLabyComponent(FComponents.Objects[Index])
  else
    raise EComponentNotFound.CreateFmt(sComponentNotFound, [ID]);
end;

{*
  Tableau des plug-in index� par leur ID
  @param ID   ID du plug-in � trouver
  @return Le plug-in dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun plug-in ne correspond � l'ID sp�cifi�
*}
function TMaster.GetPlugin(const ID : TComponentID) : TPlugin;
begin
  Result := Component[ID] as TPlugin;
end;

{*
  Tableau des d�finitions d'objet index� par leur ID
  @param ID   ID de la d�finition d'objet � trouver
  @return La d�finition d'objet dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune d�finition ne correspond � l'ID sp�cifi�
*}
function TMaster.GetObjectDef(const ID : TComponentID) : TObjectDef;
begin
  Result := Component[ID] as TObjectDef;
end;

{*
  Tableau des terrains index� par leur ID
  @param ID   ID du terrain � trouver
  @return Le terrain dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun terrain ne correspond � l'ID sp�cifi�
*}
function TMaster.GetField(const ID : TComponentID) : TField;
begin
  Result := Component[ID] as TField;
end;

{*
  Tableau des effets de case index� par leur ID
  @param ID   ID de l'effet � trouver
  @return L'effet dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun effet ne correspond � l'ID sp�cifi�
*}
function TMaster.GetEffect(const ID : TComponentID) : TEffect;
begin
  Result := Component[ID] as TEffect;
end;

{*
  Tableau des cases index� par leur ID
  Si la case n'a pas pu �tre trouv�e, GetScrew essaye de trouver les terrain
  et effet correspondant � son ID et de cr�er la case automatiquement
  @param ID   ID de la case � trouver
  @return La case dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune case ne correspond � l'ID sp�cifi�
*}
function TMaster.GetScrew(const ID : TComponentID) : TScrew;
var AField : TField;
    AEffect : TEffect;
    AName : string;
begin
  try
    Result := Component[ID] as TScrew;
  except
    on Error : EComponentNotFound do
    begin
      Result := nil;
      try
        AField := Field[GetFirstToken(ID, '-')];
        AEffect := Effect[GetLastToken(ID, '-')];
        if AEffect.Name = '' then AName := AField.Name else
          AName := Format(sDefaultScrewName, [AField.Name, AEffect.Name]);
        Result := TScrew.Create(Self, ID, AName, AField, AEffect);
      except
      end;
      if Result = nil then raise;
    end;
  end;
end;

{*
  Tableau des cartes index� par leur ID
  @param ID   ID de la carte � trouver
  @return La carte dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucune carte ne correspond � l'ID sp�cifi�
*}
function TMaster.GetMap(const ID : TComponentID) : TMap;
begin
  Result := Component[ID] as TMap;
end;

{*
  Tableau des joueurs index� par leur ID
  @param ID   ID du joueur � trouver
  @return Le joueur dont l'ID a �t� sp�cifi�
  @throws EComponentNotFound : Aucun joueur ne correspond � l'ID sp�cifi�
*}
function TMaster.GetPlayer(const ID : TComponentID) : TPlayer;
begin
  Result := Component[ID] as TPlayer;
end;

{*
  Nombre de plug-in
  @return Nombre de plug-in
*}
function TMaster.GetPluginCount : integer;
begin
  Result := FPlugins.Count;
end;

{*
  Tableau zero-based des plug-in
  @param Index   Index du plug-in
  @return Le plug-in � la position sp�cifi�e
*}
function TMaster.GetPlugins(Index : integer) : TPlugin;
begin
  Result := TPlugin(FPlugins[Index]);
end;

{*
  Nombre de d�finitions d'objet
  @return Nombre de d�finitions d'objet
*}
function TMaster.GetObjectDefCount : integer;
begin
  Result := FObjectDefs.Count;
end;

{*
  Tableau zero-based des d�finitions d'objet
  @param Index   Index de la d�finition d'objet
  @return La d�finition d'objet � la position sp�cifi�e
*}
function TMaster.GetObjectDefs(Index : integer) : TObjectDef;
begin
  Result := TObjectDef(FObjectDefs[Index]);
end;

{*
  Nombre de terrains
  @return Nombre de terrains
*}
function TMaster.GetFieldCount : integer;
begin
  Result := FFields.Count;
end;

{*
  Tableau zero-based des terrains
  @param Index   Index du terrain
  @return Le terrain � la position sp�cifi�e
*}
function TMaster.GetFields(Index : integer) : TField;
begin
  Result := TField(FFields[Index]);
end;

{*
  Nombre d'effets
  @return Nombre d'effets
*}
function TMaster.GetEffectCount : integer;
begin
  Result := FEffects.Count;
end;

{*
  Tableau zero-based des effets
  @param Index   Index de l'effet
  @return L'effet � la position sp�cifi�e
*}
function TMaster.GetEffects(Index : integer) : TEffect;
begin
  Result := TEffect(FEffects[Index]);
end;

{*
  Nombre de cases
  @return Nombre de cases
*}
function TMaster.GetScrewCount : integer;
begin
  Result := FScrews.Count;
end;

{*
  Tableau zero-based des cases
  @param Index   Index de la case
  @return La case � la position sp�cifi�e
*}
function TMaster.GetScrews(Index : integer) : TScrew;
begin
  Result := TScrew(FScrews[Index]);
end;

{*
  Nombre de cartes
  @return Nombre de cartes
*}
function TMaster.GetMapCount : integer;
begin
  Result := FMaps.Count;
end;

{*
  Tableau zero-based des cartes
  @param Index   Index de la carte
  @return La carte � la position sp�cifi�e
*}
function TMaster.GetMaps(Index : integer) : TMap;
begin
  Result := TMap(FMaps[Index]);
end;

{*
  Nombre de joueurs
  @return Nombre de joueurs
*}
function TMaster.GetPlayerCount : integer;
begin
  Result := FPlayers.Count;
end;

{*
  Tableau zero-based des joueurs
  @param Index   Index du joueur
  @return Le joueur � la position sp�cifi�e
*}
function TMaster.GetPlayers(Index : integer) : TPlayer;
begin
  Result := TPlayer(FPlayers[Index]);
end;

{*
  Ajoute un composant
  @param Component   Le composant � ajouter
*}
procedure TMaster.AddComponent(Component : TFunLabyComponent);
var Index : integer;
begin
  Index := FComponents.AddObject(Component.ID, Component);
  try
    if Component is TPlugin    then FPlugins   .Add(Component) else
    if Component is TObjectDef then FObjectDefs.Add(Component) else
    if Component is TField     then FFields    .Add(Component) else
    if Component is TEffect    then FEffects   .Add(Component) else
    if Component is TScrew     then FScrews    .Add(Component) else
    if Component is TMap       then FMaps      .Add(Component) else
    if Component is TPlayer    then FPlayers   .Add(Component) else
    assert(False);
  except
    FComponents.Delete(Index);
    raise;
  end;
end;

initialization
  with TMemIniFile.Create(Dir+fIniFileName) do
  try
    fFunLabyAppData := ReadString('Directories', 'AppData', Dir); {don't localize}

    fScrewsDir := fFunLabyAppData + fScrewsDir;
    fSoundsDir := fFunLabyAppData + fSoundsDir;
    FUnitsDir := fFunLabyAppData + fUnitsDir;
    FMapsDir := fFunLabyAppData + fMapsDir;
    fLabyrinthsDir := fFunLabyAppData + fLabyrinthsDir;
    fSaveguardsDir := fFunLabyAppData + fSaveguardsDir;

    fScrewFileName := fScrewsDir+fScrewFileName;
  finally
    Free;
  end;
end.

