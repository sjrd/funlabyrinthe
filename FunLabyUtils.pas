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
  Forms, Dialogs, StdCtrls, Math;

resourcestring
  sDefaultObjectInfos = '%s : %d';
  sWhichObject = 'Quel objet voulez-vous utiliser ?';

const
  ScrewSize = 30;         /// Taille (en largeur et hauteur) d'une case
  clTransparent = clTeal; /// Couleur de transparence pour les fichiers .bmp

type
  /// Type repr�sentant une direction cardinale
  TDirection = (diNone, diNorth, diEast, diSouth, diWest);

  /// Pointeur vers T3DPoint
  P3DPoint = ^T3DPoint;

  /// Type repr�sentant le code d'une case
  TScrewCode = type Byte;

  /// Mode d'ouverture d'un fichier FunLabyrinthe
  TFileMode = (fmEdit, fmEditActions, fmPlay);

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
    FunLabyrinthe. Elle fournit des propri�t�s et des m�thodes pour nommer et
    afficher le composant.
  *}
  TFunLabyComponent = class
  private
    FMaster : TMaster;   /// Ma�tre FunLabyrinthe
    FName : string;      /// Nom du composant
    FPainter : TPainter; /// Peintre par d�faut
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
    Un plug-in peut agir de plusieurs fa�ons sur le joueur :
    - Dessiner sous et sur le joueur ;
    - Emp�cher le d�placement du joueur et r�agir � son d�placement effectif ;
    - Indiquer au joueur qu'il a la capacit� de faire certaines actions.
  *}
  TPlayerPlugin = class
  private
    FMaster : TMaster;         /// Ma�tre FunLabyrinthe
    FPainterBefore : TPainter; /// Peintre par d�faut sous le joueur
    FPainterAfter : TPainter;  /// Peintre par d�faut sur le joueur
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
    TPlayerObject est la classe de base pour les objets que poss�de le joueur.
    Les objets peuvent rendre un joueur capable d'effectuer certaines actions.
  *}
  TPlayerObject = class(TFunLabyComponent)
  private
    FPlayer : TPlayer; /// Joueur poss�dant l'objet
    FCount : integer;  /// Nombre d'objets de ce type que poss�de le joueur
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
    Classe repr�sentant un joueur
    TPlayer repr�sente un joueur. Elle poss�de de nombreuses propri�t�s et
    m�thodes permettant d'afficher le joueur, de le d�placer, de lui greffer
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
    TScrew est la classe de base pour les cases de la carte. Elle poss�de une
    code et quatre m�thodes qui d�finissent son comportement.
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
    G�re les cases et met en relation celles-ci avec leurs codes respectifs
    TScrewsMaster g�re les diff�rentes cases de FunLabyrinthe, et met en
    relation les codes des cases avec celles-ci.
  *}
  TScrewsMaster = class
  private
    FMaster : TMaster;                  /// Ma�tre FunLabyrinthe
    FScrews : array[33..255] of TScrew; /// Liste des cases par code

    function GetScrews(Code : TScrewCode) : TScrew;
  public
    constructor Create(AMaster : TMaster);
    destructor Destroy; override;

    property Master : TMaster read FMaster;
    property Screws[Code : TScrewCode] : TScrew read GetScrews; default;
  end;

  {*
    Repr�sente la carte du jeu
    TMap g�re et repr�sente la carte du jeu. Elle offre des propri�t�s et
    m�thodes pour lire et modifier cette carte.
  *}
  TMap = class
  private
    FMaster : TMaster;              /// Ma�tre FunLabyrinthe
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
    Ma�tre FunLabyrinthe
    TMaster cr�e et g�re les diff�rentes composantes de FunLabyrinthe.
  *}
  TMaster = class
  private
    FImagesMaster : TImagesMaster; /// Ma�tre d'images
    FScrewsMaster : TScrewsMaster; /// Ma�tre des cases
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
    Cr�e, ouvre, enregistre et g�re les fichiers FunLabyrinthe
    TFunLabyFile se charge de cr�er, ouvrir, enregistrer et g�rer les fichiers
    FunLabyrinthe (extension .lab). Elle est la seule de toutes les classes
    m�tier de FunLabyrinthe � � savoir � qu'il existe des fichiers.
    C'est la classe au plus haut niveau du fonctionnement de FunLabyrinthe.
  *}
  TFunLabyFile = class
  private
    FFileName : TFileName; /// Nom du fichier
    FMode : TFileMode;     /// Mode d'ouverture du fichier
    FName : string;        /// Nom du labyrinthe
    FDescription : string; /// Description
    FVersion : string;     /// Version lors de l'enregistrement
    FAllowEdit : boolean;  /// Indique si le fichier peut �tre ouvert en �dition

    FMaster : TMaster;     /// Ma�tre FunLabyrinthe
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
  /// Cha�ne de format pour les fichiers image
  sScrewFileName : string = 'Screws\%s.bmp';

function ScrewRect(X : integer = 0; Y : integer = 0) : TRect;

implementation

uses
  Screws;

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
      NewImg.LoadFromFile(Format(sScrewFileName, [ImgName]));
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
    Canvas.Pen.Style := psClear;
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
  D�truit l'instance
*}
destructor TFunLabyComponent.Destroy;
begin
  FPainter.Free;
  inherited;
end;

{*
  Ex�cut� apr�s la construction de l'objet
  AfterConstruction est appel� apr�s l'ex�cution du dernier constructeur.
  N'appelez pas directement AfterConstruction.
*}
procedure TFunLabyComponent.AfterConstruction;
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
procedure TFunLabyComponent.Draw(Canvas : TCanvas; X : integer = 0;
  Y : integer = 0);
begin
  FPainter.Draw(Canvas, X, Y);
end;

////////////////////////////
/// Classe TPlayerPlugin ///
////////////////////////////

{*
  Cr�e une instance de TPlayerPlugin
  @param AMaster   Ma�tre FunLabyrinthe
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
  D�truit l'instance
*}
destructor TPlayerPlugin.Destroy;
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
procedure TPlayerPlugin.AfterConstruction;
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
procedure TPlayerPlugin.DrawBefore(Player : TPlayer; Canvas : TCanvas;
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
procedure TPlayerPlugin.DrawAfter(Player : TPlayer; Canvas : TCanvas;
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
procedure TPlayerPlugin.Moving(Player : TPlayer; OldDirection : TDirection;
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
procedure TPlayerPlugin.Moved(Player : TPlayer; KeyPressed : boolean;
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
function TPlayerPlugin.CanYou(Player : TPlayer; Action : integer) : boolean;
begin
  Result := False;
end;

////////////////////////////
/// Classe TPlayerObject ///
////////////////////////////

{*
  Cr�e une instance de TPlayerObject
  @param AMaster   Ma�tre FunLabyrinthe
  @param AName     Nom de l'objet
  @param APlayer   Joueur propri�taire
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
  GetShownInfos renvoie les informations textuelles � afficher pour l'objet.
  @return Informations textuelles, ou une cha�ne vide si rien � afficher
*}
function TPlayerObject.GetShownInfos : string;
begin
  Result := Format(sDefaultObjectInfos, [FName, FCount]);
end;

{*
  Indique si l'objet permet au joueur d'effectuer une action donn�e
  CanYou doit renvoyer True si l'objet permet au joueur, en l'utilisant,
  d'effectuer l'action donn�e en param�tre.
  @param Action   Action � tester
  @return True si l'objet permet d'effectuer l'action, False sinon
*}
function TPlayerObject.CanYou(Action : integer) : boolean;
begin
  Result := False;
end;

{*
  Utiliser l'objet pour effectuer l'action donn�e
  UseFor est appel�e lorsque le joueur choisit d'utiliser cet objet pour
  effectuer l'action donn�e en param�tre.
  @param Action   Action � effectuer
*}
procedure TPlayerObject.UseFor(Action : integer);
begin
end;

//////////////////////
/// Classe TPlayer ///
//////////////////////

{*
  Cr�e une instance de TPlayer
  @param AMaster   Ma�tre FunLabyrinthe
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
  D�truit l'instance
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
  @return L'objet � la position indiqu�e
*}
function TPlayer.GetObjects(Index : integer) : TPlayerObject;
begin
  Result := TPlayerObject(FObjects[Index]);
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
      FPainter.Draw(Canvas, X, Y)
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
procedure TPlayer.AddPlugin(Plugin : TPlayerPlugin);
begin
  FPlugins.Add(Plugin);
end;

{*
  Retire un plug-in du joueur
  @param Plugin   Le plug-in � retirer
*}
procedure TPlayer.RemovePlugin(Plugin : TPlayerPlugin);
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
    GoodObjects : array of TPlayerObject;
    RadioTitles : array of string;
    GoodObject : TPlayerObject;
begin
  Result := True;

  // Les plug-in ont la priorit�, puisqu'ils n'ont pas d'effet de bord
  for I := 0 to PluginCount-1 do if Plugins[I].CanYou(Self, Action) then exit;

  // Listage des objets susceptibles d'aider le joueur
  SetLength(GoodObjects, ObjectCount);
  GoodObjectCount := 0;
  for I := 0 to ObjectCount-1 do if Objects[I].CanYou(Action) then
  begin
    GoodObjects[GoodObjectCount] := Objects[I];
    inc(GoodObjectCount);
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
  GoodObject.UseFor(Action);
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

  // D�placement du joueur (� moins qu'il ait �t� d�plac� par ailleurs)
  if Same3DPoint(FPosition, Src) then
    FPosition := Dest
  else
    Dest := FPosition;

  // Second passage : le d�placement a �t� fait
  begin
    // Case source : exited
    Master.Map[Src].Exited(Self, KeyPressed, Src, Dest);
    // Plug-in : moved
    for I := 0 to PluginCount-1 do
      Plugins[I].Moved(Self, KeyPressed, Src, Dest);
    // Case destination : entered (sauf si AbortEntered a �t� positionn� � True)
    if not AbortEntered then
      Master.Map[Dest].Entered(Self, KeyPressed, Src, Dest, Redo);
  end;
end;

/////////////////////
/// Classe TScrew ///
/////////////////////

{*
  Cr�e une instance de TScrew
  @param AMaster   Ma�tre FunLabyrinthe
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
procedure TScrew.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Src, Pos : T3DPoint;
  var Cancel, AbortEntered : boolean);
begin
end;

{*
  Ex�cut� lorsque le joueur est arriv� sur la case
  Entered est ex�cut� lorsque le joueur est arriv� sur la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
  @param Src          Case de provenance
  @param Pos          Position de la case
  @param GoOnMoving   � positionner � True pour r�it�rer le d�placement
*}
procedure TScrew.Entered(Player : TPlayer; KeyPressed : boolean;
  Src, Pos : T3DPoint; var GoOnMoving : boolean);
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
procedure TScrew.Exiting(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; Pos, Dest : T3DPoint; var Cancel : boolean);
begin
end;

{*
  Ex�cut� lorsque le joueur est sorti de la case
  Exiting est ex�cut� lorsque le joueur est sorti de la case.
  @param Player       Joueur qui se d�place
  @param KeyPressed   True si une touche a �t� press�e pour le d�placement
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
  Cr�e une instance de TScrewsMaster
  @param AMaster   Ma�tre FunLabyrinthe
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
  D�truit l'instance
*}
destructor TScrewsMaster.Destroy;
var Code : TScrewCode;
begin
  for Code := 33 to 255 do if Assigned(FScrews[Code]) then
    FScrews[Code].Free;
  inherited;
end;

{*
  Tableau des cases index� par leurs codes respectifs
  @param Code   Code de la case
  @return La case dont le code a �t� sp�cifi�
*}
function TScrewsMaster.GetScrews(Code : TScrewCode) : TScrew;
begin
  Result := FScrews[Code];
end;

///////////////////
/// Classe TMap ///
///////////////////

{*
  Cr�e une instance de TMap
  @param AMaster       Ma�tre FunLabyrinthe
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
  Tableau des codes des cases index�s par leur position sur la carte
  @param Position   Position sur la carte
  @return Le code de la case � la position sp�cifi�e
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
  Modifie le tableau des codes des cases index�es par leur position sur la carte
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
  Tableau des cases index� par leur position sur la carte
  @param Position   Position sur la carte
  @return La case � la position sp�cifi�e
*}
function TMap.GetMap(Position : T3DPoint) : TScrew;
begin
  Result := Master.ScrewsMaster[CodeMap[Position]];
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
/// Classe TMaster ///
//////////////////////

{*
  Cr�e une instance de TMaster
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
  D�truit l'instance
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
  @return Le joueur � la position sp�cifi�e
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
  @param AFileName   Nom du fichier � ouvrir
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
  Cr�e un nouveau fichier FunLabyrinthe en mode �dition
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
  { TODO 2 : Cr�er un nouveau fichier }
end;

{*
  D�truit l'instance
*}
destructor TFunLabyFile.Destroy;
begin
  FMaster.Free;
  inherited;
end;

{*
  Enregistre le fichier
  @param AFileName   Nom du fichier � enregistrer (si vide, conserve l'existant)
*}
procedure TFunLabyFile.Save(const AFileName : TFileName = '');
begin
  { TODO 2 : Enregistrer un fichier }
end;

initialization
  sScrewFileName := Dir+sScrewFileName;
end.

