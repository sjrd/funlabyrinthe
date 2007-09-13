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
  FunLabyEditConsts, JvTabBar, EditUnits;

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
    OpenUnitDialog: TOpenDialog;
    ActionEditUnits: TAction;
    procedure ActionEditUnitsExecute(Sender: TObject);
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
    SepiRootManager: TSepiAsynchronousRootManager;
    SepiRoot: TSepiMetaRoot; /// Racine Sepi

    BigMenuMaps: TActionClient;  /// Menu des cartes
    BigMenuUnits: TActionClient; /// Menu des unités

    TabMapEditor: TJvTabBarItem;     /// Onglet d'édition des cartes
    FrameMapEditor: TFrameMapEditor; /// Cadre d'édition des cartes

    UnitActions: array of TAction; /// Liste des actions du menu Unités

    FModified: Boolean;      /// Indique si le fichier a été modifié
    MasterFile: TMasterFile; /// Fichier maître
    Master: TMaster;         /// Maître FunLabyrinthe

    procedure LoadFile;
    procedure UnloadFile;

    procedure NewFile;
    procedure OpenFile(const FileName: TFileName);
    function SaveFile(FileName: TFileName = ''): Boolean;
    function CloseFile: Boolean;

    procedure MarkModified;

    procedure MakeUnitActions;
    procedure DeleteUnitActions;

    procedure AddUnitEditor(Editor: IUnitEditor50);

    procedure SetModified(Value: Boolean);

    property Modified: Boolean read FModified write SetModified;
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
  BigMenuMaps.Visible := True;
  ActionAddMap.Enabled := True;

  // Menu des unités
  BigMenuUnits.Visible := True;
  ActionViewAllUnits.Enabled := True;
  ActionEditUnits.Enabled := True;
  MakeUnitActions;

  // Chargement des cartes
  FrameMapEditor.LoadFile(SepiRoot, MasterFile);
end;

{*
  Décharge le MasterFile
  Décharge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
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
  BigMenuMaps.Visible := False;
  ActionAddMap.Enabled := False;

  // Menu des unités
  BigMenuUnits.Visible := False;
  ActionViewAllUnits.Enabled := False;
  ActionEditUnits.Enabled := False;
  DeleteUnitActions;

  // Autres variables
  Master := nil;

  // Ne pas laisser la croix de fermeture indiquer une modification
  Modified := False;
end;

{*
  Crée un nouveau fichier et le charge
*}
procedure TFormMain.NewFile;
var
  UnitFileDescs: TUnitFileDescs;
begin
  SetLength(UnitFileDescs, 1);
  UnitFileDescs[0].GUID := BPLUnitHandlerGUID;
  UnitFileDescs[0].HRef := FunLabyBaseHRef;

  while not SepiRootManager.Ready do
    Sleep(100);

  MasterFile := TMasterFile.CreateNew(SepiRoot, UnitFileDescs);
  if TFormFileProperties.ManageProperties(MasterFile) then
  begin
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
procedure TFormMain.OpenFile(const FileName: TFileName);
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
function TFormMain.SaveFile(FileName: TFileName = ''): Boolean;
var
  DirName: TFileName;
  I: Integer;
  MapFile: TMapFile;
  MapFileName: TFileName;
  MapHRef: string;
begin
  Result := False;

  // Vérifier que tous les joueurs sont placés
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if Map = nil then
      begin
        SdDialogs.ShowDialog(sCantSave,
          Format(sCantSaveUnplacedPlayer, [ID]), dtError);
        Exit;
      end;
    end;
  end;

  // Choisir un nom de fichier pour l'enregistrement
  if FileName = '' then
  begin
    if not SaveDialog.Execute then
      Exit;
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
      if not SaveMapDialog.Execute then
        Exit;
      MapFileName := SaveMapDialog.FileName;
    end;

    MapHRef := FileNameToHRef(MapFileName, [DirName, fMapsDir]);

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
function TFormMain.CloseFile: Boolean;
var
  I: Integer;
  Tab: TJvTabBarItem;
  Editor: IUnitEditor50;
begin
  Result := True;

  if MasterFile = nil then
    Exit;

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
        Exit;
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
      drYes: Result := SaveFile(MasterFile.FileName);
      drCancel: Result := False;
    end;

    if not Result then
      Exit;
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
  Crée une entrée dans le menu des unités pour chaque unité
*}
procedure TFormMain.MakeUnitActions;
var
  I: Integer;
  PreviousItem: TActionClientItem;
  UnitFile: TUnitFile;
begin
  PreviousItem := BigMenuUnits.Items[BigMenuUnits.Items.Count-1];
  SetLength(UnitActions, MasterFile.UnitFileCount);

  for I := 0 to MasterFile.UnitFileCount-1 do
  begin
    UnitFile := MasterFile.UnitFiles[I];
    UnitActions[I] := TAction.Create(Self);
    with UnitActions[I] do
    begin
      ActionList := ActionManager;
      Caption := ExtractFileName(UnitFile.FileName);
      Tag := Integer(UnitFile);
      Enabled := UnitEditors.Exists(UnitFile.HandlerGUID);
      OnExecute := ActionViewUnitExecute;
    end;
    PreviousItem := ActionManager.AddAction(UnitActions[I], PreviousItem);
  end;
end;

{*
  Supprime toutes les actions Voir une unité et leurs menus associés
*}
procedure TFormMain.DeleteUnitActions;
var
  I: Integer;
begin
  for I := 0 to Length(UnitActions)-1 do
  begin
    ActionManager.DeleteActionItems([UnitActions[I]]);
    UnitActions[I].Free;
  end;
  SetLength(UnitActions, 0);
end;

{*
  Ajoute un éditeur d'unité à l'interface visuelle et l'affiche
  @param Editor   Éditeur à ajouter
*}
procedure TFormMain.AddUnitEditor(Editor: IUnitEditor50);
var
  EditorControl: TControl;
  Tab: TJvTabBarItem;
begin
  // Configurer le contrôle d'édition
  EditorControl := Editor.Control;
  EditorControl.Align := alClient;
  EditorControl.Visible := False;
  EditorControl.Parent := Self;

  // Créer un nouvel onglet pour l'éditeur et l'afficher
  Tab := TJvTabBarItem(TabBarEditors.Tabs.Add);
  Tab.Caption := ExtractFileName(Editor.UnitFile.FileName);
  Tab.Data := TObject(Pointer(Editor));
  Tab.Tag := UnitEditorTag;
  Tab.Selected := True;
end;

{*
  Modifie l'état Modifié du fichier
  @param Value   Nouvelle valeur
*}
procedure TFormMain.SetModified(Value: Boolean);
begin
  FModified := Value;
  TabMapEditor.Modified := Value;
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

  BigMenuMaps := MainMenuBar.ActionClient.Items[1];
  BigMenuUnits := MainMenuBar.ActionClient.Items[2];

  TabMapEditor := TabBarEditors.Tabs[0];
  FrameMapEditor := TFrameMapEditor.Create(Self);
  with FrameMapEditor do
  begin
    Parent := Self;
    Align := alClient;
    MarkModified := Self.MarkModified;
  end;
  TabMapEditor.Data := FrameMapEditor;
  TabMapEditor.Tag := MapEditorTag;

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
  if CloseFile then
    NewFile;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Ouvrir un fichier
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionOpenFileExecute(Sender: TObject);
begin
  if not CloseFile then
    Exit;

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
// Don't delete this comment
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
  Gestionnaire d'événement OnExecute de l'action Voir toutes les unités
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewAllUnitsExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(UnitActions)-1 do
    UnitActions[I].Execute;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Ajouter une unité
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionEditUnitsExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  if Modified and (not SaveFile(MasterFile.FileName)) then
    Exit;

  if TFormEditUnits.EditUnits(MasterFile) then
  begin
    FileName := MasterFile.FileName;
    CloseFile;
    OpenFile(FileName);
  end;
end;

{*
  Gestionnaire d'événement OnExecute d'une action Voir une unité
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewUnitExecute(Sender: TObject);
var
  I: Integer;
  UnitFile: TUnitFile;
  Tab: TJvTabBarItem;
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
      Exit;
    end;
  end;

  // Créer l'éditeur
  AddUnitEditor(UnitEditors.CreateEditor(UnitFile));
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
    Exit;

  case Item.Tag of
    MapEditorTag: TControl(Item.Data).Visible := False;
    UnitEditorTag: IUnitEditor50(Pointer(Item.Data)).Control.Visible := False;
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
    Exit;

  case Item.Tag of
    MapEditorTag: TControl(Item.Data).Visible := True;
    UnitEditorTag: IUnitEditor50(Pointer(Item.Data)).Control.Visible := True;
  end;
end;

end.

