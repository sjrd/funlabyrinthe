{*
  Unité principale de FunLabyEdit.exe
  Cette unité contient la fiche principale de FunLabyEdit.exe.
  @author sjrd
  @version 5.0
*}
unit FunLabyEditMain;

interface

uses
  Windows, SysUtils, Forms, Dialogs, Classes, ActnList, XPStyleActnCtrls,
  ActnMan, ImgList, Controls, MapEditor, ComCtrls, ActnMenus, ToolWin,
  ActnCtrls, ShellAPI, ScUtils, SdDialogs, SepiMetaUnits, FunLabyUtils,
  FilesUtils, UnitFiles, EditPluginManager, UnitEditorIntf, FileProperties,
  FunLabyEditConsts, JvTabBar;

type
  {*
    Fiche principale de FunLabyEdit.exe
    @author sjrd
    @version 5.0
  *}
  TFormMain = class(TForm)
    Images: TImageList;
    ActionManager: TActionManager;
    ActionExit: TAction;
    ToolBarFile: TActionToolBar;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    ScrewsImages: TImageList;
    ActionOpenFile: TAction;
    ActionSaveFile: TAction;
    ActionSaveFileAs: TAction;
    ActionCloseFile: TAction;
    ActionNewFile: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionFileProperties: TAction;
    SaveMapDialog: TSaveDialog;
    ActionAddMap: TAction;
    ActionRemoveMap: TAction;
    ActionHelpTopics: TAction;
    ActionAbout: TAction;
    ActionTest: TAction;
    TabBarEditors: TJvTabBar;
    ActionViewAllUnits: TAction;
    procedure TabBarEditorsTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure TabBarEditorsTabSelecting(Sender: TObject; Item: TJvTabBarItem;
      var AllowSelect: Boolean);
    procedure TabBarEditorsTabClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionTestExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionFilePropertiesExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAddMapExecute(Sender: TObject);
    procedure ActionRemoveMapExecute(Sender: TObject);
    procedure ActionHelpTopicsExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionViewAllUnitsExecute(Sender: TObject);
    procedure ActionViewUnitExecute(Sender: TObject);
    procedure TabBarEditorsTabClosing(Sender: TObject; Item: TJvTabBarItem;
      var AllowClose: Boolean);
  private
    { Déclarations privées }
    /// Manager asynchrone de la racine Sepi
    SepiRootManager : TSepiAsynchronousRootManager;
    SepiRoot : TSepiMetaRoot; /// Racine Sepi

    FrameMapEditor : TFrameMapEditor; /// Cadre d'édition des cartes

    FModified : boolean;      /// Indique si le fichier a été modifié
    MasterFile : TMasterFile; /// Fichier maître
    Master : TMaster;         /// Maître FunLabyrinthe

    procedure LoadFile;
    procedure UnloadFile;

    procedure NewFile;
    procedure OpenFile(const FileName : TFileName);
    function SaveFile(FileName : TFileName = '') : boolean;
    function CloseFile : boolean;

    procedure MarkModified;

    procedure SetModified(Value : boolean);

    property Modified : boolean read FModified write SetModified;
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

const
  MapEditorTag = 0;
  UnitEditorTag = 1;

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Charge le MasterFile créé
  Charge le MasterFile créé dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
var I : integer;
    PreviousItem : TActionClientItem;
    ViewUnitAction : TAction;
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

  // Menu des cartes
  MainMenuBar.ActionClient.Items[1].Visible := True;
  ActionAddMap.Enabled := True;

  // Menu des unités
  MainMenuBar.ActionClient.Items[2].Visible := True;
  ActionViewAllUnits.Enabled := True;
  PreviousItem := ActionManager.FindItemByAction(ActionViewAllUnits);

  // Un sous-menu pour chaque unité
  if MasterFile.UnitFileCount > 0 then
  begin
    PreviousItem := ActionManager.AddSeparator(PreviousItem);

    for I := 0 to MasterFile.UnitFileCount-1 do
    begin
      ViewUnitAction := TAction.Create(Self);
      with ViewUnitAction do
      begin
        ActionList := ActionManager;
        Caption := ExtractFileName(MasterFile.UnitFiles[I].FileName);
        Tag := Integer(MasterFile.UnitFiles[I]);
        OnExecute := ActionViewUnitExecute;
      end;

      PreviousItem := ActionManager.AddAction(ViewUnitAction, PreviousItem);
    end;
  end;

  // Chargement des cartes
  FrameMapEditor.LoadFile(SepiRoot, MasterFile);
end;

{*
  Décharge le MasterFile
  Décharge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
var I : integer;
    ViewUnitAction : TContainedAction;
begin
  // Déchargement des cartes
  FrameMapEditor.UnloadFile;

  // Désactivation de l'interface utilisateur
  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionCloseFile.Enabled := False;
  ActionFileProperties.Enabled := False;
  ActionTest.Enabled := False;

  // Menu des cartes
  MainMenuBar.ActionClient.Items[1].Visible := False;
  ActionAddMap.Enabled := False;

  // Menu des unités
  MainMenuBar.ActionClient.Items[2].Visible := False;
  ActionViewAllUnits.Enabled := False;

  with MainMenuBar.ActionClient.Items[2] do
  begin
    for I := Items.Count-1 downto 1 do
    begin
      ViewUnitAction := Items[I].Action;
      Items.Delete(I);
      ViewUnitAction.Free;
    end;
  end;

  // Autres variables
  Master := nil;

  // Ne pas laisser la croix de fermeture indiquer une modification
  Modified := False;
end;

{*
  Crée un nouveau fichier et le charge
*}
procedure TFormMain.NewFile;
begin
  while not SepiRootManager.Ready do
    Sleep(100);

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
  while not SepiRootManager.Ready do
    Sleep(100);

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

  // Vérifier que tous les joueurs sont placés
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if Map = nil then
    begin
      ShowDialog(sCantSave, Format(sCantSaveUnplacedPlayer, [ID]), dtError);
      exit;
    end;
  end;

  // Choisir un nom de fichier pour l'enregistrement
  if FileName = '' then
  begin
    if not SaveDialog.Execute then exit;
    OpenDialog.FileName := SaveDialog.FileName;
    FileName := SaveDialog.FileName;
  end;
  DirName := ExtractFilePath(FileName);

  // Enregistrer les cartes
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

  // Enregistrer le projet
  MasterFile.Save(FileName);
  Result := True;
  Modified := False;
end;

{*
  Ferme le fichier
  @return True si le fichier a bien été fermé, False sinon
*}
function TFormMain.CloseFile : boolean;
var I : integer;
    Tab : TJvTabBarItem;
    Editor : IUnitEditor50;
begin
  Result := True;

  if MasterFile = nil then exit;

  // Fermer toutes les unités ouvertes : chacune peut empêcher la fermeture
  for I := TabBarEditors.Tabs.Count-1 downto 0 do
  begin
    Tab := TabBarEditors.Tabs[I];
    if Tab.Tag <> UnitEditorTag then
      Continue;

    if Tab.Data <> nil then
    begin
      Editor := IUnitEditor50(Pointer(Tab.Data));
      if not Editor.CanClose then
        exit;
      Editor.Release;
      Tab.Data := nil;
    end;

    TabBarEditors.Tabs.Delete(I);
  end;

  // Demander de sauvegarder le projet, s'il est modifié
  if Modified then
  begin
    case ShowDialog(sConfirmExitTitle, sConfirmExit,
                    dtConfirmation, dbYesNoCancel) of
      drYes : Result := SaveFile(MasterFile.FileName);
      drCancel : Result := False;
    end;

    if not Result then exit;
  end;

  // Tout décharger
  UnloadFile;

  MasterFile.Free;
  MasterFile := nil;
end;

{*
  Marque le fichier comme modifié
*}
procedure TFormMain.MarkModified;
begin
  Modified := True;
end;

{*
  Modifie l'état Modifié du fichier
  @param Value   Nouvelle valeur
*}
procedure TFormMain.SetModified(Value : boolean);
begin
  FModified := Value;
  TabBarEditors.Tabs[0].Modified := Value;
end;

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  SepiRootManager := TSepiAsynchronousRootManager.Create;
  SepiRootManager.LoadUnit('FunLabyUtils');
  SepiRoot := SepiRootManager.Root;

  OpenDialog.InitialDir := fLabyrinthsDir;
  SaveDialog.InitialDir := fLabyrinthsDir;

  MasterFile := nil;

  FrameMapEditor := TFrameMapEditor.Create(Self);
  with FrameMapEditor do
  begin
    Parent := Self;
    Align := alClient;
    MarkModified := Self.MarkModified;
  end;
  TabBarEditors.Tabs[0].Data := FrameMapEditor;
  TabBarEditors.Tabs[0].Tag := MapEditorTag;

  if ParamCount > 0 then
    OpenFile(ParamStr(1));
end;

{*
  Gestionnaire d'événement OnDestroy
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FrameMapEditor.Free;
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
    MarkModified;
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
  Gestionnaire d'événement OnExecute de l'action Ajouter une carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAddMapExecute(Sender: TObject);
begin
  FrameMapEditor.AddMap;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Retirer une carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionRemoveMapExecute(Sender: TObject);
begin
  FrameMapEditor.RemoveCurrentMap;
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

{*
  Gestionnaire d'événement OnExecute d'une action Voir toutes les unités
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewAllUnitsExecute(Sender: TObject);
var I : integer;
begin
  with MainMenuBar.ActionClient.Items[2] do
    for I := 2 to Items.Count-1 do
      Items[I].Action.Execute;
end;

{*
  Gestionnaire d'événement OnExecute d'une action Voir une unité
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewUnitExecute(Sender: TObject);
var I : integer;
    UnitFile : TUnitFile;
    Tab : TJvTabBarItem;
    Editor : IUnitEditor50;
    EditorControl : TControl;
begin
  UnitFile := TUnitFile((Sender as TAction).Tag);

  // Si un éditeur est déjà ouvert pour ce fichier, le mettre en avant-plan
  for I := 0 to TabBarEditors.Tabs.Count-1 do
  begin
    Tab := TabBarEditors.Tabs[I];
    if (Tab.Tag = UnitEditorTag) and
       (IUnitEditor50(Pointer(Tab.Data)).UnitFile = UnitFile) then
    begin
      Tab.Selected := True;
      exit;
    end;
  end;

  // Créer l'éditeur
  try
    Editor := CreateUnitEditor(UnitFile);
  except
    on Error : EFileError do
    begin
      ShowDialog(sError, Error.Message, dtError);
      exit;
    end;
  end;

  // Placer le contrôle d'édition
  EditorControl := Editor.Control;
  EditorControl.Align := alClient;
  EditorControl.Visible := False;
  EditorControl.Parent := Self;

  // Créer un nouvel onglet pour l'éditeur et l'afficher
  Tab := TJvTabBarItem(TabBarEditors.Tabs.Add);
  Tab.Caption := ExtractFileName(UnitFile.FileName);
  Tab.Data := TObject(Editor);
  Tab.Tag := UnitEditorTag;
  Tab.Selected := True;
end;

{*
  Gestionnaire d'événement OnTabClosing de la tab bar des éditeurs
  @param Sender       Objet qui a déclenché l'événement
  @param Item         Élément qui va être fermé
  @param AllowClose   Positionner à False pour empêcher la fermeture
*}
procedure TFormMain.TabBarEditorsTabClosing(Sender: TObject;
  Item: TJvTabBarItem; var AllowClose: Boolean);
begin
  if Item.Tag = UnitEditorTag then
    AllowClose := IUnitEditor50(Pointer(Item.Data)).CanClose
  else
    AllowClose := False;
end;

{*
  Gestionnaire d'événement OnTabClosed de la tab bar des éditeurs
  @param Sender   Objet qui a déclenché l'événement
  @param Item     Élément qui est fermé
*}
procedure TFormMain.TabBarEditorsTabClosed(Sender: TObject;
  Item: TJvTabBarItem);
begin
  if Item.Tag = UnitEditorTag then
  begin
    IUnitEditor50(Pointer(Item.Data)).Release;
    Item.Data := nil;
  end;
end;

{*
  Gestionnaire d'événement OnTabSelecting de la tab bar des éditeurs
  @param Sender        Objet qui a déclenché l'événement
  @param Item          Élément qui va être affiché
  @param AllowSelect   Positionner à False pour empêcher la sélection
*}
procedure TFormMain.TabBarEditorsTabSelecting(Sender: TObject;
  Item: TJvTabBarItem; var AllowSelect: Boolean);
begin
  Item := TabBarEditors.SelectedTab;
  if (Item = nil) or (Item.Data = nil) then
    exit;

  case Item.Tag of
    MapEditorTag : TControl(Item.Data).Visible := False;
    UnitEditorTag : IUnitEditor50(Pointer(Item.Data)).Control.Visible := False;
  end;
end;

{*
  Gestionnaire d'événement OnTabSelected de la tab bar des éditeurs
  @param Sender   Objet qui a déclenché l'événement
  @param Item     Élément qui est fermé
*}
procedure TFormMain.TabBarEditorsTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
begin
  if (Item = nil) or (Item.Data = nil) then
    exit;

  case Item.Tag of
    MapEditorTag : TControl(Item.Data).Visible := True;
    UnitEditorTag : IUnitEditor50(Pointer(Item.Data)).Control.Visible := True;
  end;
end;

end.

