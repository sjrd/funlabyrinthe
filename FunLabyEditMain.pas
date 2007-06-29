{*
  Unité principale de FunLabyEdit.exe
  Cette unité contient la fiche principale de FunLabyEdit.exe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyEditMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, Menus, ImgList, StdCtrls,
  ExtCtrls, Tabs, ComCtrls, ActnMenus, ToolWin, ActnCtrls, CategoryButtons,
  StdActns, ScUtils, FunLabyUtils, FilesUtils, Spin, ShellAPI, PlayerPlugins,
  PlayerAttributes, PlayerObjects, FileProperties, AddMap, SdDialogs,
  SepiMetaUnits, UnitFiles;

resourcestring
  sFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore implémentée';

  sDefaultPlayerName = 'Joueur';

  sPlayerPosition = 'Position : %s';
  sCenterToPosition = 'Centrer la vue sur ce joueur';
  sShowPlugins = 'Plug-in...';
  sShowAttributes = 'Attributs...';
  sShowObjects = 'Objets...';

  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a été modifié. Voulez-vous l''enregistrer ?';

  sCantSave = 'Impossible d''enregistrer';
  sCantSaveUnplacedPlayer =
    'Impossible d''enregistrer car le joueur d''ID %s n''a pas été placé';

  sCantCenterToPosition = 'Impossible de centrer le joueur';
  sCantCenterToUnplacedPlayer =
    'Impossible de centrer sur ce joueur car il n''a pas été placé';

  sReplaceOutsideTitle = 'Remplacer l''extérieur de la carte';
  sReplaceOutside = 'Voulez-vous vraiment modifier l''extérieur de la carte ?';

  sRemoveMapTitle = 'Retirer une carte';
  sRemoveMap = 'Êtes-vous certain de vouloir retirer cette carte du projet ?';

const
  opMask = $07;           /// Masque d'opération
  opShift = 3;            /// Décalage de l'information d'opération
  opCenterToPosition = 1; /// Opération centrer sur la position
  opShowPlugins = 2;      /// Opération montrer les plug-in
  opShowAttributes = 3;   /// Opération montrer les attributs
  opShowObjects = 4;      /// Opération montrer les objets

type
  {*
    Représente un ensemble de composants enregistré
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TComponentSet = class
  private
    FMinIndex : integer;                    /// Index minimal
    FMaxIndex : integer;                    /// Index maximal
    FComponents : array of TScrewComponent; /// Ensemble des composants
    FDialogTitle : string;                  /// Titre de la boîte de dialogue
    FDialogPrompt : string;                 /// Invite de la boîte de dialogue
  public
    constructor Create(const AComponents : array of TScrewComponent;
      BaseIndex : integer; const ADialogTitle, ADialogPrompt : string);

    function ChooseComponent(var LastIndex : integer) : TScrewComponent;
  end;

  {*
    Fiche principale de FunLabyEdit.exe
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TFormMain = class(TForm)
    Images: TImageList;
    ActionManager: TActionManager;
    ActionExit: TAction;
    ToolBarFile: TActionToolBar;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    PanelCenter: TPanel;
    MapTabSet: TTabSet;
    SplitterScrews: TSplitter;
    ScrewsContainer: TCategoryButtons;
    ScrewsImages: TImageList;
    PlayersContainer: TCategoryButtons;
    SplitterPlayers: TSplitter;
    ActionOpenFile: TAction;
    ActionSaveFile: TAction;
    ActionSaveFileAs: TAction;
    ActionCloseFile: TAction;
    ActionNewFile: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PanelMapInfos: TPanel;
    LabelPosition: TLabel;
    LabelField: TLabel;
    LabelEffect: TLabel;
    LabelObstacle: TLabel;
    ScrollBoxMap: TScrollBox;
    PaintBoxMap: TPaintBox;
    LabelFloor: TLabel;
    StaticPosition: TStaticText;
    StaticField: TStaticText;
    StaticEffect: TStaticText;
    StaticObstacle: TStaticText;
    ActionFileProperties: TAction;
    SaveMapDialog: TSaveDialog;
    ActionAddMap: TAction;
    ActionRemoveMap: TAction;
    StaticTool: TStaticText;
    LabelTool: TLabel;
    ActionHelpTopics: TAction;
    ActionAbout: TAction;
    ActionTest: TAction;
    procedure ActionTestExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionFilePropertiesExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure MapTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PaintBoxMapPaint(Sender: TObject);
    procedure EditFloorChange(Sender: TObject);
    procedure ScrewsContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure PlayersContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure PaintBoxMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMapMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ActionAddMapExecute(Sender: TObject);
    procedure ActionRemoveMapExecute(Sender: TObject);
    procedure ActionHelpTopicsExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
  private
    { Composants non disponibles dans Turbo Explorer }
    EditFloor : TSpinEdit;

    { Déclarations privées }
    /// Manager asynchrone de la racine Sepi
    SepiRootManager : TSepiAsynchronousRootManager;
    SepiRoot : TSepiMetaRoot;     /// Racine Sepi

    ScrewBmp : TScrewBitmap;      /// Bitmap de case à tout faire
    LastCompIndex : integer;      /// Dernier index de composant choisi

    Modified : boolean;           /// Indique si le fichier a été modifié
    MasterFile : TMasterFile;     /// Fichier maître
    Master : TMaster;             /// Maître FunLabyrinthe
    CurrentMap : TMap;            /// Carte courante

    FCurrentFloor : integer;      /// Étage courant

    Component : TVisualComponent; /// Composant à placer

    procedure SetCurrentFloor(Value : integer);

    function AddScrewButton(Template : TVisualComponent) : TButtonItem;
    procedure RegisterSingleComponent(Component : TScrewComponent); stdcall;
    procedure RegisterComponentSet(Template : TScrewComponent;
      const Components : array of TScrewComponent; BaseIndex : integer;
      const DialogTitle, DialogPrompt : string); stdcall;

    procedure LoadPlayers;

    procedure LoadFile;
    procedure UnloadFile;

    procedure NewFile;
    procedure OpenFile(const FileName : TFileName);
    function SaveFile(FileName : TFileName = '') : boolean;
    function CloseFile : boolean;

    procedure CenterToPlayerPosition(Player : TPlayer);

    property CurrentFloor : integer read FCurrentFloor write SetCurrentFloor;
  public
    { Déclarations publiques }
  end;

const {don't localize}
  FunLabyBaseHRef = 'FunLabyBase.bpl'; /// HRef de l'unité FunLabyCore
  idPlayer = 'Player';                 /// ID de l'unique joueur

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.dfm}

{----------------------}
{ Classe TComponentSet }
{----------------------}

{*
  Crée une instance de TComponentSet
  @param AComponents     Ensemble de composants
  @param ADialogTitle    Titre de la boîte de dialogue
  @param ADialogPrompt   Invite de la boîte de dialogue
*}
constructor TComponentSet.Create(const AComponents : array of TScrewComponent;
  BaseIndex : integer; const ADialogTitle, ADialogPrompt : string);
var Len, I : integer;
begin
  inherited Create;

  Len := Length(AComponents);
  FMinIndex := BaseIndex;
  FMaxIndex := BaseIndex+Len-1;

  SetLength(FComponents, Len);
  for I := 0 to Len-1 do
  begin
    FComponents[I] := AComponents[Low(AComponents) + I];
    if FComponents[I] is TScrew then
      TScrew(FComponents[I]).AddRef;
  end;

  FDialogTitle := ADialogTitle;
  FDialogPrompt := ADialogPrompt;
end;

{*
  Demande à l'utilisateur de choisir un composant dans l'ensemble de composants
  @param LastIndex   Dernier index entré, contient le nouvel index en sortie
  @return Référence au composant choisi
*}
function TComponentSet.ChooseComponent(
  var LastIndex : integer) : TScrewComponent;
begin
  LastIndex := QueryNumber(FDialogTitle, FDialogPrompt,
    MinMax(LastIndex, FMinIndex, FMaxIndex), FMinIndex, FMaxIndex);
  Result := FComponents[LastIndex - FMinIndex];
end;

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Modifie l'étage courant
  @param Value   Nouvel étage
*}
procedure TFormMain.SetCurrentFloor(Value : integer);
begin
  EditFloor.Value := Value;
  PaintBoxMap.Invalidate;
end;

{*
  Ajoute un bouton de case à partir d'un modèle de case et le renvoie
  @param Template   Modèle de case, pour l'image et le hint du bouton
  @return Le bouton nouvellement créé
*}
function TFormMain.AddScrewButton(Template : TVisualComponent) : TButtonItem;
var ImageIndex : integer;
    Category : TButtonCategory;
begin
  // Ajout de l'image du composant dans la liste d'images
  ScrewBmp.EmptyScrew;
  Template.Draw(NoQPos, ScrewBmp.Canvas);
  ImageIndex := ScrewsImages.AddMasked(ScrewBmp, clTransparent);

  // Choix de la catégorie
  if Template is TField    then Category := ScrewsContainer.Categories[0] else
  if Template is TEffect   then Category := ScrewsContainer.Categories[1] else
  if Template is TTool     then Category := ScrewsContainer.Categories[2] else
  if Template is TObstacle then Category := ScrewsContainer.Categories[3] else
  if Template is TScrew    then Category := ScrewsContainer.Categories[4] else
  Category := ScrewsContainer.Categories[5];

  // Ajout du bouton
  Result := Category.Items.Add;
  Result.ImageIndex := ImageIndex;
  Result.Hint := Template.Name;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFormMain.RegisterSingleComponent(Component : TScrewComponent);
var Button : TButtonItem;
begin
  Button := AddScrewButton(Component);
  Button.Data := Component;
  if Component is TScrew then
    TScrew(Component).AddRef;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant modèle pour l'image et le nom à afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
  @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
*}
procedure TFormMain.RegisterComponentSet(Template : TScrewComponent;
  const Components : array of TScrewComponent; BaseIndex : integer;
  const DialogTitle, DialogPrompt : string);
var Button : TButtonItem;
begin
  Button := AddScrewButton(Template);
  Button.Data := TComponentSet.Create(Components,
    BaseIndex, DialogTitle, DialogPrompt);
end;

{*
  Charge les infos sur les joueurs
*}
procedure TFormMain.LoadPlayers;
var I : integer;
    Player : TPlayer;
begin
  for I := 0 to Master.PlayerCount-1 do
  begin
    Player := Master.Players[I];

    AddScrewButton(Player).Data := Player;

    with PlayersContainer.Categories.Add do
    begin
      Caption := Player.ID;
      Color := $E8BBA2;

      Items.Add.Caption := Player.Name;

      with Items.Add do
      begin
        Caption := Format(sPlayerPosition,
          [Point3DToString(Player.Position, ', ')]);
        Hint := sCenterToPosition;
        Data := Pointer(I shl opShift + opCenterToPosition);
      end;

      with Items.Add do
      begin
        Caption := sShowPlugins;
        Data := Pointer(I shl opShift + opShowPlugins);
      end;

      with Items.Add do
      begin
        Caption := sShowAttributes;
        Data := Pointer(I shl opShift + opShowAttributes);
      end;

      with Items.Add do
      begin
        Caption := sShowObjects;
        Data := Pointer(I shl opShift + opShowObjects);
      end;
    end;
  end;
end;

{*
  Charge le MasterFile créé
  Charge le MasterFile créé dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
var I : integer;
begin
  // Un fichier nouvellement chargé n'est modifié que s'il vient d'être créé
  Modified := MasterFile.FileName = '';

  // Autres variables
  Master := MasterFile.Master;

  // Activation de l'interface utilisateur
  ActionSaveFile.Enabled := True;
  ActionSaveFileAs.Enabled := True;
  ActionCloseFile.Enabled := True;
  ActionFileProperties.Enabled := True;
  ActionTest.Enabled := True;

  MainMenuBar.ActionClient.Items[1].Visible := True; // menu des cartes
  ActionAddMap.Enabled := True;

  // Recensement des composants d'édition
  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);

  // Chargement des infos des joueurs
  LoadPlayers;

  // Recensement des cartes
  with MasterFile do for I := 0 to MapFileCount-1 do
    MapTabSet.Tabs.Add(MapFiles[I].MapID);
  CurrentFloor := 0;
  if MasterFile.MapFileCount > 0 then
    MapTabSet.TabIndex := 0;
end;

{*
  Décharge le MasterFile
  Décharge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
var I : integer;
begin
  // Vider les onglets de carte
  MapTabSet.TabIndex := -1;
  MapTabSet.Tabs.Clear;

  // Vider les onglets des joueurs
  PlayersContainer.Categories.Clear;

  // Vider les composants d'édition
  with ScrewsContainer do
  begin
    for I := 0 to Categories.Count-1 do with Categories[I] do
    begin
      while Items.Count > 0 do
      begin
        if TObject(Items[0].Data) is TComponentSet then
          TObject(Items[0].Data).Free;
        Items.Delete(0);
      end;
    end;
  end;
  ScrewsImages.Clear;

  // Désactivation de l'interface utilisateur
  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionCloseFile.Enabled := False;
  ActionFileProperties.Enabled := False;
  ActionTest.Enabled := False;

  MainMenuBar.ActionClient.Items[1].Visible := False; // menu des cartes
  ActionAddMap.Enabled := False;

  // Autres variables
  Component := nil;
  Master := nil;
end;

{*
  Crée un nouveau fichier et le charge
*}
procedure TFormMain.NewFile;
begin
  MasterFile := TMasterFile.CreateNew(SepiRoot);
  if TFormFileProperties.ManageProperties(MasterFile) then
  begin
    MasterFile.AddUnitFile(BPLUnitHandlerGUID, FunLabyBaseHRef);
    TPlayer.Create(MasterFile.Master, idPlayer, sDefaultPlayerName,
      nil, Point3D(0, 0, 0));
    LoadFile;
  end else
  begin
    MasterFile.Free;
    MasterFile := nil;
  end;
end;

{*
  Ouvre un fichier et le charge
  @param FileName   Nom du fichier maître à charger
*}
procedure TFormMain.OpenFile(const FileName : TFileName);
begin
  MasterFile := TMasterFile.Create(SepiRoot, FileName, fmEdit);
  LoadFile;
end;

{*
  Enregistre le fichier
  @param FileName   Fichier dans lequel enregistrer, ou vide pour demander
  @return True si l'enregistrement a bien été effectué, False sinon
*}
function TFormMain.SaveFile(FileName : TFileName = '') : boolean;
var DirName : TFileName;
    I : integer;
    MapFile : TMapFile;
    MapFileName : TFileName;
    MapHRef : string;
begin
  Result := False;

  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if Map = nil then
    begin
      ShowDialog(sCantSave, Format(sCantSaveUnplacedPlayer, [ID]), dtError);
      exit;
    end;
  end;

  if FileName = '' then
  begin
    if not SaveDialog.Execute then exit;
    OpenDialog.FileName := SaveDialog.FileName;
    FileName := SaveDialog.FileName;
  end;
  DirName := ExtractFilePath(FileName);

  for I := 0 to MasterFile.MapFileCount-1 do
  begin
    MapFile := MasterFile.MapFiles[I];
    MapFileName := MapFile.FileName;

    if MapFileName = '' then
    begin
      SaveMapDialog.FileName := MapFile.MapID;
      if not SaveMapDialog.Execute then exit;
      MapFileName := SaveMapDialog.FileName;
    end;

    if AnsiSameText(Copy(MapFileName, 1, Length(DirName)), DirName) then
      MapHRef := Copy(MapFileName, Length(DirName)+1, Length(MapFileName)) else
    if AnsiSameText(Copy(MapFileName, 1, Length(fMapsDir)), fMapsDir) then
      MapHRef := Copy(MapFileName, Length(fMapsDir)+1, Length(MapFileName)) else
    MapHRef := MapFileName;

    MapFile.Save(MapHRef, MapFileName);
  end;

  MasterFile.Save(FileName);
  Result := True;
  Modified := False;
end;

{*
  Ferme le fichier
  @return True si le fichier a bien été fermé, False sinon
*}
function TFormMain.CloseFile : boolean;
begin
  Result := True;

  if MasterFile = nil then exit;

  if Modified then
  begin
    case ShowDialog(sConfirmExitTitle, sConfirmExit,
                    dtConfirmation, dbYesNoCancel) of
      drYes : Result := SaveFile(MasterFile.FileName);
      drCancel : Result := False;
    end;

    if not Result then exit;
  end;

  UnloadFile;

  MasterFile.Free;
  MasterFile := nil;
end;

{*
  Centre l'affichage sur le joueur
  @param Player   Joueur  visionner
*}
procedure TFormMain.CenterToPlayerPosition(Player : TPlayer);
var TabIndex : integer;
    X, Y : integer;
begin
  if Player.Map = nil then
  begin
    ShowDialog(sCantCenterToPosition, sCantCenterToUnplacedPlayer, dtError);
    exit;
  end;

  TabIndex := Master.MapCount-1;
  while TabIndex >= 0 do
    if Master.Maps[TabIndex] = Player.Map then Break else dec(TabIndex);
  MapTabSet.TabIndex := TabIndex;

  X := Player.Position.X * ScrewSize + ScrewSize div 2;
  Y := Player.Position.Y * ScrewSize + ScrewSize div 2;
  CurrentFloor := Player.Position.Z;

  with ScrollBoxMap do
  begin
    HorzScrollBar.Position := X - ClientWidth  div 2;
    VertScrollBar.Position := Y - ClientHeight div 2;
  end;
end;

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Création dynamique des composants non disponibles dans Turbo Explorer
  EditFloor := TSpinEdit.Create(Self);
  with EditFloor do
  begin
    Name := 'EditFloor'; {don't localize}
    Parent := PanelMapInfos;
    Left := 296;
    Top := 8;
    Width := 49;
    Anchors := [akTop, akRight];
    EditorEnabled := False;
    Enabled := False;
    OnChange := EditFloorChange;
  end;

  SepiRootManager := TSepiAsynchronousRootManager.Create;
  SepiRootManager.LoadUnit('FunLabyUtils');
  SepiRoot := SepiRootManager.Root;

  ScrewBmp := TScrewBitmap.Create;

  LastCompIndex := 0;

  OpenDialog.InitialDir := fLabyrinthsDir;
  SaveDialog.InitialDir := fLabyrinthsDir;

  MasterFile := nil;
  CurrentMap := nil;

  Component := nil;

  if ParamCount > 0 then
    OpenFile(ParamStr(1));
end;

{*
  Gestionnaire d'événement OnDestroy
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ScrewBmp.Free;

  SepiRootManager.Free;
end;

{*
  Gestionnaire d'événement OnCloseQuery
  @param Sender     Objet qui a déclenché l'événement
  @param CanClose   Indique si la fenêtre peut être fermée
*}
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseFile;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Nouveau fichier
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionNewFileExecute(Sender: TObject);
begin
  if CloseFile then NewFile;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Ouvrir un fichier
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionOpenFileExecute(Sender: TObject);
begin
  if not CloseFile then exit;

  if OpenDialog.Execute then
  begin
    SaveDialog.FileName := OpenDialog.FileName;
    OpenFile(OpenDialog.FileName);
  end;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Enregistrer
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionSaveFileExecute(Sender: TObject);
begin
  SaveFile(MasterFile.FileName);
end;

{*
  Gestionnaire d'événement OnExecute de l'action Enregistrer sous
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionSaveFileAsExecute(Sender: TObject);
begin
  SaveFile;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Fermer
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionCloseFileExecute(Sender: TObject);
begin
  CloseFile;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Propriétés du fichier
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionFilePropertiesExecute(Sender: TObject);
begin
  if TFormFileProperties.ManageProperties(MasterFile) then
    Modified := True;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Tester
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionTestExecute(Sender: TObject);
begin
  {don't localize}
  if (not Modified) or SaveFile(MasterFile.FileName) then
    ShellExecute(0, 'open', PChar(Dir+'Labyrinthe.exe'),
      PChar('"'+MasterFile.FileName+'"'), nil, SW_SHOWNORMAL);
end;

{*
  Gestionnaire d'événement OnExecute de l'action Quitter
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

{*
  Gestionnaire d'événement OnChange du tab-set des cartes
  @param Sender        Objet qui a déclenché l'événement
  @param NewTab        Index de l'onglet nouvellement sélectionné
  @param AllowChange   Indique si le changement peut être effectué
*}
procedure TFormMain.MapTabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  StaticPosition.Caption := '';
  StaticField.Caption := '';
  StaticEffect.Caption := '';
  StaticTool.Caption := '';
  StaticObstacle.Caption := '';

  if NewTab < 0 then
  begin
    CurrentMap := nil;

    CurrentFloor := 0;
    EditFloor.Enabled := False;

    PaintBoxMap.Width := 100;
    PaintBoxMap.Height := 100;

    ActionRemoveMap.Enabled := False;
  end else
  begin
    CurrentMap := MasterFile.MapFiles[NewTab].Map;

    EditFloor.MaxValue := CurrentMap.Dimensions.Z-1;
    EditFloor.Enabled := CurrentMap.Dimensions.Z > 1;
    if CurrentFloor >= CurrentMap.Dimensions.Z then
      CurrentFloor := CurrentMap.Dimensions.Z-1;

    PaintBoxMap.Width  := (CurrentMap.Dimensions.X + 2*MinViewSize) * ScrewSize;
    PaintBoxMap.Height := (CurrentMap.Dimensions.Y + 2*MinViewSize) * ScrewSize;

    ActionRemoveMap.Enabled := True;
  end;
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnPaint de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.PaintBoxMapPaint(Sender: TObject);
var Left, Top, Right, Bottom : integer;
    LeftZone, TopZone, RightZone, BottomZone : integer;
    I, X, Y : integer;
    QPos : TQualifiedPos;
begin
  if CurrentMap = nil then exit;

  // Calcul des coordonnées à afficher
  Left := ScrollBoxMap.HorzScrollBar.Position div ScrewSize - MinViewSize;
  Top  := ScrollBoxMap.VertScrollBar.Position div ScrewSize - MinViewSize;
  Right  := Left + ScrollBoxMap.ClientWidth  div ScrewSize + 1;
  Bottom := Top  + ScrollBoxMap.ClientHeight div ScrewSize + 1;

  LeftZone := Left div CurrentMap.ZoneWidth;
  TopZone  := Top  div CurrentMap.ZoneHeight;
  RightZone  := LeftZone + Right  div CurrentMap.ZoneWidth ;
  BottomZone := TopZone  + Bottom div CurrentMap.ZoneHeight;

  // Dessin des cases
  QPos.Map := CurrentMap;
  for X := Left to Right do for Y := Top to Bottom do
  begin
    QPos.Position := Point3D(X, Y, CurrentFloor);
    CurrentMap[QPos.Position].Draw(QPos, PaintBoxMap.Canvas,
      (MinViewSize+X) * ScrewSize, (MinViewSize+Y) * ScrewSize);
  end;

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if (Map = CurrentMap) and (Position.Z = CurrentFloor) then
    begin
      DrawInPlace(PaintBoxMap.Canvas, (MinViewSize+Position.X) * ScrewSize,
        (MinViewSize+Position.Y) * ScrewSize);
    end;
  end;

  // Dessin des lignes de séparation des zones
  with PaintBoxMap.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psDash;
    Pen.Width := 3;

    for X := LeftZone to RightZone do
    begin
      MoveTo((CurrentMap.ZoneWidth * X + 1) * ScrewSize, Top * ScrewSize);
      LineTo((CurrentMap.ZoneWidth * X + 1) * ScrewSize,
        (Bottom+2) * ScrewSize);
    end;

    for Y := TopZone to BottomZone do
    begin
      MoveTo(Left * ScrewSize, (CurrentMap.ZoneHeight * Y + 1) * ScrewSize);
      LineTo((Right+2) * ScrewSize,
        (CurrentMap.ZoneHeight * Y + 1) * ScrewSize);
    end;

    Pen.Width := 1;
  end;
end;

{*
  Gestionnaire d'événement OnChange de la zone d'édition de l'étage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.EditFloorChange(Sender: TObject);
begin
  FCurrentFloor := EditFloor.Value;
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnButtonClicked des boutons de case
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Référence au bouton cliqué
*}
procedure TFormMain.ScrewsContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  if TObject(Button.Data) is TComponentSet then
    Component := TComponentSet(Button.Data).ChooseComponent(LastCompIndex)
  else
    Component := TScrewComponent(Button.Data);
end;

{*
  Gestionnaire d'événement OnButtonClicked des boutons de joueur
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Référence au bouton cliqué
*}
procedure TFormMain.PlayersContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
var Player : TPlayer;
begin
  Player := Master.Players[integer(Button.Data) shr opShift];
  case integer(Button.Data) and opMask of
    opCenterToPosition : CenterToPlayerPosition(Player);
    opShowPlugins :
    begin
      if TFormPlugins.ManagePlugins(Player) then
      begin
        Modified := True;
        PaintBoxMap.Invalidate;
      end;
    end;
    opShowAttributes :
    begin
      if TFormAttributes.ManageAttributes(Player) then
        Modified := True;
    end;
    opShowObjects : TFormObjects.ShowObjects(Player);
  end;
end;

{*
  Gestionnaire d'événement OnMouseDown de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Boutonde la souris qui a été enfoncé
  @param Shift    État des touches système
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
*}
procedure TFormMain.PaintBoxMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Position : T3DPoint;
    FieldID, EffectID, ToolID, NewID : TComponentID;
begin
  if (Button <> mbLeft) or (CurrentMap = nil) or (Component = nil) then exit;

  Position := Point3D(X div ScrewSize - 1, Y div ScrewSize - 1, CurrentFloor);

  if Component is TPlayer then
  begin
    TPlayer(Component).ChangePosition(CurrentMap, Position);
  end else
  begin
    NewID := Component.ID;
    with CurrentMap[Position] do
    begin
      FieldID := Field.ID;
      if Effect = nil then EffectID := '' else EffectID := Effect.ID;
      if Tool   = nil then ToolID   := '' else ToolID   := Tool.ID;

      if Component is TField then
        NewID := NewID+'---' else
      if Component is TEffect then
        NewID := FieldID+'-'+NewID+'--' else
      if Component is TTool then
        NewID := FieldID+'-'+EffectID+'-'+NewID+'-' else
      if Component is TObstacle then
        NewID := FieldID+'-'+EffectID+'-'+ToolID+'-'+NewID;
    end;

    if CurrentMap.InMap(Position) then
    begin
      CurrentMap[Position] := Master.Screw[NewID];
    end else
    begin
      if ShowDialog(sReplaceOutsideTitle, sReplaceOutside,
        dtConfirmation, dbOKCancel) <> drOK then exit;
      CurrentMap.Outside[CurrentFloor] := Master.Screw[NewID];
    end;
  end;

  Modified := True;
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnMouseMove de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Shift    État des touches système
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
*}
procedure TFormMain.PaintBoxMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var Position : T3DPoint;
begin
  if CurrentMap = nil then exit;

  Position := Point3D(X div ScrewSize - 1, Y div ScrewSize - 1, CurrentFloor);
  StaticPosition.Caption := Point3DToString(Position, ', ');

  with CurrentMap[Position] do
  begin
    StaticField.Caption := Field.Name;
    if Effect = nil then StaticEffect.Caption := '' else
      StaticEffect.Caption := Effect.Name;
    if Tool = nil then StaticTool.Caption := '' else
      StaticTool.Caption := Tool.Name;
    if Obstacle = nil then StaticObstacle.Caption := '' else
      StaticObstacle.Caption := Obstacle.Name;
  end;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Ajouter une carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAddMapExecute(Sender: TObject);
var MapID : TComponentID;
begin
  MapID := TFormAddMap.AddMap(MasterFile);
  if MapID <> '' then
  begin
    MapTabSet.TabIndex := MapTabSet.Tabs.Add(MapID);
    Modified := True;
  end;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Retirer une carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionRemoveMapExecute(Sender: TObject);
var Map : TMap;
    I, Index : integer;
begin
  Map := CurrentMap;
  if Map = nil then exit;

  if ShowDialog(sRemoveMapTitle, sRemoveMap,
    dtConfirmation, dbOKCancel, 2) <> drOK then exit;

  for I := 0 to Master.PlayerCount-1 do
  begin
    if Master.Players[I].Map = Map then
      Master.Players[I].ChangePosition(nil, No3DPoint);
  end;

  Index := MapTabSet.TabIndex;
  if (Index = 0) and (MapTabSet.Tabs.Count > 1) then
    MapTabSet.TabIndex := 1
  else
    MapTabSet.TabIndex := Index - 1;

  MapTabSet.Tabs.Delete(Index);
  Map.Free;

  Modified := True;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Rubriques d'aide
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionHelpTopicsExecute(Sender: TObject);
begin
//
end;

{*
  Gestionnaire d'événement OnExecute de l'action À propos...
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  ShowFunLabyAbout;
end;

end.

