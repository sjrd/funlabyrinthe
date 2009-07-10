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
  ActnCtrls, ShellAPI, ScUtils, SdDialogs, SepiReflectionCore, FunLabyUtils,
  FilesUtils, UnitFiles, EditPluginManager, SourceEditors, FileProperties,
  FunLabyEditConsts, JvTabBar, EditUnits, NewSourceFile, ExtCtrls, ScSyncObjs,
  CompilerMessages, MapViewer, SepiCompilerErrors, AddMap,
  SepiImportsFunLabyTools, SourceEditorEvents, FunLabyEditOTA;

type
  {*
    Fiche principale de FunLabyEdit.exe
    @author sjrd
    @version 5.0
  *}
  TFormMain = class(TForm, IOTAFunLabyEditMainForm50)
    Images: TImageList;
    ActionManager: TActionManager;
    ActionExit: TAction;
    ToolBarFile: TActionToolBar;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    SquaresImages: TImageList;
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
    ActionViewAllSources: TAction;
    OpenUnitDialog: TOpenDialog;
    ActionEditUnits: TAction;
    ActionAddExistingSource: TAction;
    ActionAddNewSource: TAction;
    ActionRemoveSource: TAction;
    OpenSourceFileDialog: TOpenDialog;
    ActionSaveAll: TAction;
    ActionCompileAll: TAction;
    PanelEditors: TPanel;
    TabBarEditors: TJvTabBar;
    ActionViewCompilerMessages: TAction;
    ActionViewMapViewer: TAction;
    ActionEditZoneSize: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationHint(Sender: TObject);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionFilePropertiesExecute(Sender: TObject);
    procedure ActionEditUnitsExecute(Sender: TObject);
    procedure ActionTestExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAddMapExecute(Sender: TObject);
    procedure ActionRemoveMapExecute(Sender: TObject);
    procedure ActionEditZoneSizeExecute(Sender: TObject);
    procedure ActionAddExistingSourceExecute(Sender: TObject);
    procedure ActionAddNewSourceExecute(Sender: TObject);
    procedure ActionRemoveSourceExecute(Sender: TObject);
    procedure ActionCompileAllExecute(Sender: TObject);
    procedure ActionViewAllSourcesExecute(Sender: TObject);
    procedure ActionViewSourceExecute(Sender: TObject);
    procedure ActionViewCompilerMessagesExecute(Sender: TObject);
    procedure ActionViewMapViewerExecute(Sender: TObject);
    procedure ActionHelpTopicsExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure TabBarEditorsTabSelecting(Sender: TObject; Item: TJvTabBarItem;
      var AllowSelect: Boolean);
    procedure TabBarEditorsTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure TabBarEditorsTabClosing(Sender: TObject; Item: TJvTabBarItem;
      var AllowClose: Boolean);
    procedure TabBarEditorsTabCloseQuery(Sender: TObject; Item: TJvTabBarItem;
      var CanClose: Boolean);
    procedure TabBarEditorsTabClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure EditorStateChange(const Sender: ISourceEditor50);
    procedure MessagesShowError(Sender: TObject; Error: TSepiCompilerError);
  private
    BackgroundTasks: TScTaskQueue; /// Tâches d'arrière-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// Tâche de chargement de la racine de base

    BigMenuMaps: TActionClient;    /// Menu des cartes
    BigMenuSources: TActionClient; /// Menu des unités

    TabMapEditor: TJvTabBarItem;     /// Onglet d'édition des cartes
    FrameMapEditor: TFrameMapEditor; /// Cadre d'édition des cartes

    SourceActions: array of TAction; /// Liste des actions du menu Sources liés

    FormCompilerMessages: TFormCompilerMessages; /// Messages du compilateur
    FormMapViewer: TFormMapViewer;               /// Visualisateur de cartes

    FModified: Boolean;      /// Indique si le fichier a été modifié
    SepiRoot: TSepiRoot;     /// Racine Sepi du fichier en cours
    MasterFile: TMasterFile; /// Fichier maître
    Master: TMaster;         /// Maître FunLabyrinthe

    procedure LoadBaseSepiRoot;
    procedure NeedBaseSepiRoot;
    procedure BackgroundDiscard(var Obj);

    procedure LoadFile;
    procedure UnloadFile;

    function IsEditor(Tab: TJvTabBarItem): Boolean; overload;
    function IsEditor(Index: Integer): Boolean; overload;
    function GetTabEditor(Tab: TJvTabBarItem): ISourceEditor50;
    function FindTab(const Editor: ISourceEditor50): TJvTabBarItem; overload;
    function FindTab(SourceFile: TSourceFile): TJvTabBarItem; overload;
    procedure OpenTab(SourceFile: TSourceFile);
    function CloseTab(Tab: TJvTabBarItem): Boolean;

    procedure NewFile;
    procedure OpenFile(const FileName: TFileName);
    function SaveFile(FileName: TFileName = ''): Boolean;
    function SaveAll: Boolean;
    function CloseFile: Boolean;

    function CompilerLoadUnit(Sender: TSepiRoot;
      const UnitName: string): TSepiUnit;
    function CompileAll: Boolean;

    procedure MarkModified;

    function MakeSourceAction(SourceFile: TSourceFile): TAction;
    procedure MakeSourceActions;
    procedure DeleteSourceActions;

    procedure AddSourceFile(const FileName: TFileName);
    procedure RemoveSourceFile(SourceFile: TSourceFile);

    procedure SetModified(Value: Boolean);

    function GetTabCount: Integer;
    function GetTabs(Index: Integer): TJvTabBarItem;
    function GetEditors(Index: Integer): ISourceEditor50;

    // IOTAFunLabyEditMainForm
    function GetMasterFile: TMasterFile;
    function GetCompilerMessages: IOTACompilerMessages50;
    function GetMapViewer: IOTAMapViewer50;

    property Modified: Boolean read FModified write SetModified;

    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TJvTabBarItem read GetTabs;
    property Editors[Index: Integer]: ISourceEditor50 read GetEditors;
  end;

const {don't localize}
  FunLabyBaseHRef = 'FunLabyBase.bpl'; /// HRef de l'unité FunLabyBase
  idPlayer = 'Player';                 /// ID de l'unique joueur

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.dfm}

const
  ClosedTabTag = -1;
  MapEditorTag = 0;
  SourceEditorTag = 1;

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Charge la racine Sepi de base
  Cette méthode est appelée dans le thread des tâches en arrière-plan
*}
procedure TFormMain.LoadBaseSepiRoot;
begin
  BaseSepiRoot.LoadUnit('FunLabyUtils');
  BaseSepiRoot.LoadUnit('Generics');
  BaseSepiRoot.LoadUnit('GraphicsTools');
  BaseSepiRoot.LoadUnit('MapTools');
  BaseSepiRoot.LoadUnit('PlayerObjects');
end;

{*
  Attend que la racine Sepi de base soit chargée, et vérifie l'état d'erreur
  En cas d'erreur pendant le chargement de la racine de base, un message
  avertit l'utilisateur et le programme est arrêté brutalement.
*}
procedure TFormMain.NeedBaseSepiRoot;
begin
  try
    BaseSepiRootLoadTask.WaitFor;
    BaseSepiRootLoadTask.RaiseException;
  except
    on Error: Exception do
    begin
      ShowDialog(SFatalErrorTitle,
        Format(SBaseSepiRootLoadError, [Error.Message]), dtError);
      System.Halt(1);
    end;
    on Error: TObject do
    begin
      ShowDialog(SFatalErrorTitle,
        Format(SBaseSepiRootLoadError, [Error.ClassName]), dtError);
      System.Halt(1);
    end;
  end;
end;

{*
  Crée une nouvelle tâche d'arrière-plan de libération d'un objet
  @param Obj   Objet à libérer en arrière-plan (mis à nil)
*}
procedure TFormMain.BackgroundDiscard(var Obj);
var
  AObj: TObject;
begin
  AObj := TObject(Obj);
  if AObj = nil then
    Exit;
  TObject(Obj) := nil;
  TScMethodTask.Create(BackgroundTasks, AObj.Free);
end;

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
  ActionSaveAll.Enabled := True;
  ActionCloseFile.Enabled := True;
  ActionFileProperties.Enabled := True;
  ActionEditUnits.Enabled := True;
  ActionTest.Enabled := True;

  // Menu des cartes
  BigMenuMaps.Visible := True;
  ActionAddMap.Enabled := True;
  ActionRemoveMap.Enabled := True;
  ActionEditZoneSize.Enabled := True;

  // Menu des sources
  BigMenuSources.Visible := True;
  ActionAddExistingSource.Enabled := SourceFileEditors.FilterCount > 0;
  ActionAddNewSource.Enabled := not SourceFileCreators.IsEmpty;
  ActionCompileAll.Enabled := True;
  ActionViewAllSources.Enabled := True;
  MakeSourceActions;

  // Chargement des cartes
  FrameMapEditor.LoadFile(MasterFile);
  FormMapViewer.Master := Master;
end;

{*
  Décharge le MasterFile
  Décharge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
begin
  // Déchargement des cartes
  FormMapViewer.Hide;
  FormMapViewer.Master := nil;
  FrameMapEditor.UnloadFile;

  // Désactivation de l'interface utilisateur
  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionSaveAll.Enabled := False;
  ActionCloseFile.Enabled := False;
  ActionFileProperties.Enabled := False;
  ActionEditUnits.Enabled := False;
  ActionTest.Enabled := False;

  // Menu des cartes
  BigMenuMaps.Visible := False;
  ActionAddMap.Enabled := False;
  ActionRemoveMap.Enabled := False;
  ActionEditZoneSize.Enabled := False;

  // Menu des sources
  BigMenuSources.Visible := False;
  ActionAddExistingSource.Enabled := False;
  ActionAddNewSource.Enabled := False;
  ActionCompileAll.Enabled := False;
  ActionViewAllSources.Enabled := False;
  DeleteSourceActions;

  // Autres variables
  Master := nil;

  // Ne pas laisser la croix de fermeture indiquer une modification
  Modified := False;
end;

{*
  Indique si un onglet est un éditeur de source
  @param Tab   Onglet
  @return True si l'onglet est un éditeur de source, False sinon
*}
function TFormMain.IsEditor(Tab: TJvTabBarItem): Boolean;
begin
  Result := Tab.Tag = SourceEditorTag;
end;

{*
  Indique si un onglet est un éditeur de source
  @param Index   Index de l'onglet
  @return True si l'onglet est un éditeur de source, False sinon
*}
function TFormMain.IsEditor(Index: Integer): Boolean;
begin
  Result := Tabs[Index].Tag = SourceEditorTag;
end;

{*
  Éditeur de source correspondant à un onglet donné
  @param Tab   Onglet
  @return Éditeur de source correspondant, ou nil si ce n'est pas un source
*}
function TFormMain.GetTabEditor(Tab: TJvTabBarItem): ISourceEditor50;
begin
  if Tab.Tag = SourceEditorTag then
    Result := ISourceEditor50(Pointer(Tab.Data))
  else
    Result := nil;
end;

{*
  Trouve l'onglet qui contient un éditeur de source donné
  @param Editor   Éditeur de source recherché
  @return Onglet correspondant
*}
function TFormMain.FindTab(const Editor: ISourceEditor50): TJvTabBarItem;
var
  I: Integer;
begin
  for I := 0 to TabCount-1 do
  begin
    if Pointer(Tabs[I].Data) = Pointer(Editor) then
    begin
      Result := Tabs[I];
      Exit;
    end;
  end;

  Assert(False);
  Result := nil;
end;

{*
  Trouve l'onglet qui contient un éditeur de source donné
  @param SourceFile   Fichier source recherché
  @return Onglet correspondant, ou nil si n'existe pas
*}
function TFormMain.FindTab(SourceFile: TSourceFile): TJvTabBarItem;
var
  I: Integer;
begin
  for I := 0 to TabCount-1 do
  begin
    if IsEditor(I) and (Editors[I].SourceFile = SourceFile) then
    begin
      Result := Tabs[I];
      Exit;
    end;
  end;

  Result := nil;
end;

{*
  Ajoute un éditeur de source à l'interface visuelle et l'affiche
  @param Editor   Éditeur à ajouter
*}
procedure TFormMain.OpenTab(SourceFile: TSourceFile);
var
  Editor: ISourceEditor50;
  EditorUsingOTA: ISourceEditorUsingOTA50;
  EditorControl: TControl;
  Tab: TJvTabBarItem;
begin
  // Créer l'éditeur
  Editor := SourceFileEditors.CreateEditor(SourceFile);

  // Set OTA main form
  if Supports(Editor, ISourceEditorUsingOTA50, EditorUsingOTA) then
    EditorUsingOTA.SetFunLabyEditMainForm(Self);

  // Configurer le contrôle d'édition
  EditorControl := Editor.Control;
  EditorControl.Align := alClient;
  EditorControl.Visible := False;
  EditorControl.Parent := PanelEditors;

  // Enregistrer les événements de l'éditeur
  Editor.OnStateChange := EditorStateChange;

  // Créer un nouvel onglet pour l'éditeur et l'afficher
  Tab := TJvTabBarItem(TabBarEditors.Tabs.Add);
  Tab.Caption := ExtractFileName(Editor.SourceFile.FileName);
  Tab.Data := TObject(Pointer(Editor));
  Tab.Tag := SourceEditorTag;
  Tab.Modified := Editor.Modified;
  Tab.Selected := True;
end;

{*
  Ferme un onglet d'édition de source
  @param Tab   Onglet à fermer
  @return True si l'onglet a bien été fermé, False sinon
*}
function TFormMain.CloseTab(Tab: TJvTabBarItem): Boolean;
var
  Editor: ISourceEditor50;
begin
  Result := False;

  if not IsEditor(Tab) then
    Exit;

  if Tab.Data <> nil then
  begin
    Editor := GetTabEditor(Tab);
    if not Editor.CanClose then
      Exit;
    Editor.Release;
    Pointer(Editor) := nil;
    Tab.Data := nil;
  end;

  Tab.Tag := ClosedTabTag;
  TabBarEditors.Tabs.Delete(Tab.Index);
  Result := True;
end;

{*
  Crée un nouveau fichier et le charge
*}
procedure TFormMain.NewFile;
var
  UnitFileDescs: TUnitFileDescs;
begin
  SetLength(UnitFileDescs, 1);
  UnitFileDescs[0].HRef := FunLabyBaseHRef;

  NeedBaseSepiRoot;

  SepiRoot := TSepiRootFork.Create(BaseSepiRoot);
  try
    MasterFile := TMasterFile.CreateNew(SepiRoot, UnitFileDescs);
    if TFormFileProperties.ManageProperties(MasterFile) then
    begin
      TPlayer.Create(MasterFile.Master, idPlayer, SDefaultPlayerName);
      LoadFile;
    end else
    begin
      BackgroundDiscard(MasterFile);
      BackgroundDiscard(SepiRoot);
    end;
  except
    BackgroundDiscard(MasterFile);
    BackgroundDiscard(SepiRoot);
    raise;
  end;
end;

{*
  Ouvre un fichier et le charge
  @param FileName   Nom du fichier maître à charger
*}
procedure TFormMain.OpenFile(const FileName: TFileName);
begin
  NeedBaseSepiRoot;

  SepiRoot := TSepiRootFork.Create(BaseSepiRoot);
  try
    MasterFile := TMasterFile.Create(SepiRoot, FileName, fmEdit);
    LoadFile;
  except
    BackgroundDiscard(MasterFile);
    BackgroundDiscard(SepiRoot);
    raise;
  end;
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
begin
  Result := False;

  // Vérifier que tous les joueurs sont placés
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if Map = nil then
      begin
        SdDialogs.ShowDialog(SCantSave,
          Format(SCantSaveUnplacedPlayer, [ID]), dtError);
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

  // Enregistrer le projet
  MasterFile.Save(FileName);
  Result := True;
  Modified := False;
end;

{*
  Enregistre tous les fichiers
  @return True si tous les fichiers ont bien été enregistrés, False sinon
*}
function TFormMain.SaveAll: Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to TabCount-1 do
  begin
    if IsEditor(I) and (not Editors[I].SaveFile) then
      Exit;
  end;

  Result := SaveFile(MasterFile.FileName);
end;

{*
  Ferme le fichier
  @return True si le fichier a bien été fermé, False sinon
*}
function TFormMain.CloseFile: Boolean;
var
  I: Integer;
  Tab: TJvTabBarItem;
begin
  Result := True;

  if MasterFile = nil then
    Exit;

  // Fermer tous les sources ouverts : chacun peut empêcher la fermeture
  for I := TabCount-1 downto 0 do
  begin
    Tab := Tabs[I];
    if IsEditor(Tab) and (not CloseTab(Tab)) then
      Exit;
  end;

  // Demander de sauvegarder le projet, s'il est modifié
  if Modified then
  begin
    case ShowDialog(SConfirmExitTitle, SConfirmExit,
        dtConfirmation, dbYesNoCancel) of
      drYes: Result := SaveFile(MasterFile.FileName);
      drCancel: Result := False;
    end;

    if not Result then
      Exit;
  end;

  // Tout décharger
  UnloadFile;

  BackgroundDiscard(MasterFile);
  BackgroundDiscard(SepiRoot);
end;

{*
  Charge une unité dans le compilateur
  @param Sender     Racine Sepi
  @param UnitName   Nom de l'unité
  @return Unité Sepi chargée, ou nil si non trouvée
*}
function TFormMain.CompilerLoadUnit(Sender: TSepiRoot;
  const UnitName: string): TSepiUnit;
var
  I: Integer;
  Editor: ISourceEditor50;
  UnitEditor: IUnitEditor50;
  SourceCompiler: ISourceCompiler50;
begin
  for I := 0 to TabCount-1 do
  begin
    Editor := Editors[I];
    if Editor = nil then
      Continue;

    if Supports(Editor, IUnitEditor50, UnitEditor) and
      (UnitEditor.UnitName = UnitName) then
    begin
      // Éditeur qui peut charger directement une unité
      Result := UnitEditor.LoadUnit(Sender);
      Exit;
    end else if Supports(Editor, ISourceCompiler50, SourceCompiler) and
      (SourceCompiler.UnitName = UnitName) then
    begin
      // Compilateur de source
      Result := SourceCompiler.CompileFile(Sender,
        FormCompilerMessages.Errors);
      Exit;
    end;
  end;

  Result := nil;
end;

{*
  Compile tous les sources ouverts
  @return True si les sources ont bien été compilés, False sinon
*}
function TFormMain.CompileAll: Boolean;
var
  CompilerRoot: TSepiRoot;
  I: Integer;
  SourceCompiler: ISourceCompiler50;
begin
  FormCompilerMessages.Clear;

  CompilerRoot := TSepiRootFork.Create(SepiRoot);
  try
    CompilerRoot.OnLoadUnit := CompilerLoadUnit;

    try
      for I := 0 to TabCount-1 do
      begin
        if IsEditor(I) and
          Supports(Editors[I], ISourceCompiler50, SourceCompiler) then
        begin
          CompilerRoot.LoadUnit(SourceCompiler.UnitName);
        end;
      end;

      Result := True;
    except
      on Error: ESepiCompilerFatalError do
        Result := False;
    end;

    FormCompilerMessages.Visible := FormCompilerMessages.Errors.Count > 0;
    if FormCompilerMessages.Visible then
      FormCompilerMessages.ShowFirst;
  finally
    CompilerRoot.Free;
  end;
end;

{*
  Marque le fichier comme modifié
*}
procedure TFormMain.MarkModified;
begin
  Modified := True;
end;

{*
  Crée une action Voir un source pour un fichier source donné
  @param SourceFile   Fichier source à voir
  @return L'action créée
*}
function TFormMain.MakeSourceAction(SourceFile: TSourceFile): TAction;
begin
  Result := TAction.Create(Self);
  with Result do
  begin
    ActionList := ActionManager;
    Caption := ExtractFileName(SourceFile.FileName);
    Tag := Integer(SourceFile);
    Enabled := SourceFileEditors.ExistsEditor(SourceFile);
    OnExecute := ActionViewSourceExecute;
  end;
end;

{*
  Crée une entrée dans le menu des sources pour chaque fichier source
*}
procedure TFormMain.MakeSourceActions;
var
  I: Integer;
  PreviousItem: TActionClientItem;
  SourceFile: TSourceFile;
begin
  PreviousItem := BigMenuSources.Items[BigMenuSources.Items.Count-1];
  SetLength(SourceActions, MasterFile.SourceFiles.Count);

  for I := 0 to MasterFile.SourceFiles.Count-1 do
  begin
    SourceFile := MasterFile.SourceFiles[I];
    SourceActions[I] := MakeSourceAction(SourceFile);
    PreviousItem := ActionManager.AddAction(SourceActions[I], PreviousItem);
  end;
end;

{*
  Supprime toutes les actions Voir un fichier source et leurs menus associés
*}
procedure TFormMain.DeleteSourceActions;
var
  I: Integer;
begin
  for I := 0 to Length(SourceActions)-1 do
  begin
    ActionManager.DeleteActionItems([SourceActions[I]]);
    SourceActions[I].Free;
  end;
  SetLength(SourceActions, 0);
end;

{*
  Ajoute un fichier source
  @param FileName   Nom du fichier source
*}
procedure TFormMain.AddSourceFile(const FileName: TFileName);
var
  SourceFile: TSourceFile;
  I: Integer;
  Action: TAction;
begin
  // Vérifier que ce fichier source n'est pas déjà attaché au projet
  for I := 0 to MasterFile.SourceFiles.Count-1 do
  begin
    if SameFileName(MasterFile.SourceFiles[I].FileName, FileName) then
    begin
      ShowDialog(SDuplicateSourceTitle, SDuplicateSource, dtError);
      Exit;
    end;
  end;

  // Créer le fichier source
  SourceFile := MasterFile.AddSourceFile(FileName);

  // Ajouter l'action Voir le source
  Action := MakeSourceAction(SourceFile);
  SetLength(SourceActions, MasterFile.SourceFiles.Count);
  SourceActions[MasterFile.SourceFiles.Count-1] := Action;
  ActionManager.AddAction(Action,
    BigMenuSources.Items[BigMenuSources.Items.Count-1]);

  // Afficher le source
  OpenTab(SourceFile);

  MarkModified;
end;

{*
  Retire un fichier source
  @param SourceFile   Fichier source à retirer
*}
procedure TFormMain.RemoveSourceFile(SourceFile: TSourceFile);
var
  Action: TAction;
  Index: Integer;
begin
  // Find the action and remove it from the list
  Action := nil;
  for Index := 0 to Length(SourceActions)-1 do
  begin
    if TSourceFile(SourceActions[Index].Tag) = SourceFile then
    begin
      Action := SourceActions[Index];
      Move(SourceActions[Index+1], SourceActions[Index],
        Length(SourceActions)-Index-1);
      Break;
    end;
  end;

  // Free the action and its items
  if Action <> nil then
  begin
    SetLength(SourceActions, Length(SourceActions)-1);
    ActionManager.DeleteActionItems([Action]);
    Action.Free;
  end;

  // Delete the source file
  MasterFile.RemoveSourceFile(SourceFile);

  MarkModified;
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
  Nombre d'onglets
  @return Nombre d'onglets
*}
function TFormMain.GetTabCount: Integer;
begin
  Result := TabBarEditors.Tabs.Count;
end;

{*
  Tableau zero-based des onglets ouverts
  @param Index   Index d'un onglet
  @return Onglet à l'index spécifié
*}
function TFormMain.GetTabs(Index: Integer): TJvTabBarItem;
begin
  Result := TabBarEditors.Tabs[Index];
end;

{*
  Tableau zero-based des éditeurs de source ouverts
  @param Index   Index d'un onglet
  @return Éditeur de source de cet onglet, ou nil si ce n'en est pas un
*}
function TFormMain.GetEditors(Index: Integer): ISourceEditor50;
begin
  Result := GetTabEditor(Tabs[Index]);
end;

{*
  [@inheritDoc]
*}
function TFormMain.GetMasterFile: TMasterFile;
begin
  Result := MasterFile;
end;

{*
  [@inheritDoc]
*}
function TFormMain.GetCompilerMessages: IOTACompilerMessages50;
begin
  Result := FormCompilerMessages;
end;

{*
  [@inheritDoc]
*}
function TFormMain.GetMapViewer: IOTAMapViewer50;
begin
  Result := FormMapViewer;
end;

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  BackgroundTasks := TScTaskQueue.Create(True, False);

  BaseSepiRoot := TSepiRoot.Create;
  BaseSepiRootLoadTask := TScMethodTask.Create(BackgroundTasks,
    LoadBaseSepiRoot, False);

  Application.OnHint := ApplicationHint;

  OpenDialog.InitialDir := fLabyrinthsDir;
  SaveDialog.InitialDir := fLabyrinthsDir;

  OpenSourceFileDialog.InitialDir := fUnitsDir;
  OpenSourceFileDialog.Filter := SourceFileEditors.FiltersAsText;

  MasterFile := nil;

  BigMenuMaps := MainMenuBar.ActionClient.Items[1];
  BigMenuSources := MainMenuBar.ActionClient.Items[2];

  TabMapEditor := TabBarEditors.Tabs[0];
  FrameMapEditor := TFrameMapEditor.Create(Self);
  with FrameMapEditor do
  begin
    Parent := PanelEditors;
    Align := alClient;
    MarkModified := Self.MarkModified;
  end;
  TabMapEditor.Data := FrameMapEditor;
  TabMapEditor.Tag := MapEditorTag;

  FormCompilerMessages := TFormCompilerMessages.Create(Self);
  FormCompilerMessages.OnShowError := MessagesShowError;

  FormMapViewer := TFormMapViewer.Create(Self);

  if ParamCount > 0 then
    OpenFile(ParamStr(1));
end;

{*
  Gestionnaire d'événement OnDestroy
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Application.OnHint := nil;

  FrameMapEditor.Free;

  BackgroundDiscard(BaseSepiRoot);
  BackgroundDiscard(BaseSepiRootLoadTask);

  while not BackgroundTasks.Ready do
    Sleep(100);
  BackgroundTasks.Free;
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
  Gestionnaire d'événement OnHint de l'application
*}
procedure TFormMain.ApplicationHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
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
var
  Tab: TJvTabBarItem;
begin
  Tab := TabBarEditors.SelectedTab;

  case Tab.Tag of
    MapEditorTag: SaveFile(MasterFile.FileName);
    SourceEditorTag: GetTabEditor(Tab).SaveFile;
  end;
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
  Gestionnaire d'événement OnExecute de l'action Enregistrer tout
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionSaveAllExecute(Sender: TObject);
begin
  SaveAll;
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
  Gestionnaire d'événement OnExecute de l'action Unités utilisées
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionEditUnitsExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  if not SaveAll then
    Exit;

  if TFormEditUnits.EditUnits(MasterFile) then
  begin
    FileName := MasterFile.FileName;
    CloseFile;
    OpenFile(FileName);
  end;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Tester
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionTestExecute(Sender: TObject);
begin
  {don't localize}
  if (not Modified) or SaveFile(MasterFile.FileName) then
    ShellExecute(0, 'open', PChar(Dir+'FunLaby.exe'),
      PChar('"'+MasterFile.FileName+'"'), nil, SW_SHOWNORMAL);
end;

{*
  Gestionnaire d'événement OnExecute de l'action Fermer
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionCloseFileExecute(Sender: TObject);
var
  Tab: TJvTabBarItem;
begin
  Tab := TabBarEditors.SelectedTab;

  case Tab.Tag of
    MapEditorTag: CloseFile;
    SourceEditorTag: CloseTab(Tab);
  end;
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
  FormMapViewer.Hide;
  FormMapViewer.Master := nil;
  try
    FrameMapEditor.AddMap;
  finally
    FormMapViewer.Master := Master;
  end;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Retirer une carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionRemoveMapExecute(Sender: TObject);
begin
  if FrameMapEditor.CurrentMap = nil then
    Exit;

  FormMapViewer.Hide;
  FormMapViewer.Master := nil;
  try
    FrameMapEditor.RemoveCurrentMap;
  finally
    FormMapViewer.Master := Master;
  end;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Modifier la taille de zone
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionEditZoneSizeExecute(Sender: TObject);
begin
  if FrameMapEditor.CurrentMap = nil then
    Exit;

  if TFormAddMap.EditMapZoneSize(FrameMapEditor.CurrentMap) then
  begin
    FrameMapEditor.InvalidateMap;
    MarkModified;
  end;
end;

{*
  Gestionnaire d'événement Ajouter un source existant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAddExistingSourceExecute(Sender: TObject);
begin
  if OpenSourceFileDialog.Execute then
    AddSourceFile(OpenSourceFileDialog.FileName);
end;

{*
  Gestionnaire d'événement Ajouter un nouveau fichier source
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAddNewSourceExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  if TFormCreateNewSourceFile.NewSourceFile(FileName) then
    AddSourceFile(FileName);
end;

{*
  Gestionnaire d'événement Retirer le fichier source courant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionRemoveSourceExecute(Sender: TObject);
var
  Tab: TJvTabBarItem;
  SourceFile: TSourceFile;
begin
  Tab := TabBarEditors.SelectedTab;
  if not IsEditor(Tab) then
    Exit;

  SourceFile := GetTabEditor(Tab).SourceFile;

  if ShowDialog(SRemoveSourceTitle, SRemoveSource, dtConfirmation,
    dbYesNo, 2) <> drYes then
    Exit;

  if CloseTab(Tab) then
    RemoveSourceFile(SourceFile);
end;

{*
  Gestionnaire d'événement OnExecute de l'action Compiler
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionCompileAllExecute(Sender: TObject);
begin
  CompileAll;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Voir toutes les unités
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewAllSourcesExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(SourceActions)-1 do
    SourceActions[I].Execute;
end;

{*
  Gestionnaire d'événement OnExecute d'une action Voir un source
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewSourceExecute(Sender: TObject);
var
  SourceFile: TSourceFile;
  Tab: TJvTabBarItem;
begin
  SourceFile := TSourceFile((Sender as TAction).Tag);

  // Si un éditeur est déjà ouvert pour ce fichier, le mettre en avant-plan
  // Sinon, ouvrir un nouvel onglet
  Tab := FindTab(SourceFile);
  if Tab <> nil then
    Tab.Selected := True
  else
    OpenTab(SourceFile);
end;

{*
  Gestionnaire d'événement OnExecute de l'action Voir les messages du compilo
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewCompilerMessagesExecute(Sender: TObject);
begin
  FormCompilerMessages.Show;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Voir le visualisateur de cartes
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionViewMapViewerExecute(Sender: TObject);
begin
  FormMapViewer.Show;
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
  Gestionnaire d'événement OnTabSelecting de la tab bar des éditeurs
  @param Sender        Objet qui a déclenché l'événement
  @param Item          Élément qui va être affiché
  @param AllowSelect   Positionner à False pour empêcher la sélection
*}
procedure TFormMain.TabBarEditorsTabSelecting(Sender: TObject;
  Item: TJvTabBarItem; var AllowSelect: Boolean);
var
  Editor: ISourceEditor50;
  ActivatingEditor: IActivatingSourceEditor;
begin
  Item := TabBarEditors.SelectedTab;
  if (Item = nil) or (Item.Data = nil) then
    Exit;

  case Item.Tag of
    MapEditorTag: TControl(Item.Data).Visible := False;
    SourceEditorTag:
    begin
      Editor := GetTabEditor(Item);
      if Supports(Editor, IActivatingSourceEditor, ActivatingEditor) then
        ActivatingEditor.Deactivate;
      Editor.Control.Visible := False;
    end;
  end;
end;

{*
  Gestionnaire d'événement OnTabSelected de la tab bar des éditeurs
  @param Sender   Objet qui a déclenché l'événement
  @param Item     Élément qui est fermé
*}
procedure TFormMain.TabBarEditorsTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
var
  Editor: ISourceEditor50;
  ActivatingEditor: IActivatingSourceEditor;
begin
  if (Item = nil) or (Item.Data = nil) then
    Exit;

  ActionSaveFileAs.Enabled := not IsEditor(Item);
  ActionRemoveSource.Enabled := IsEditor(Item);

  case Item.Tag of
    MapEditorTag: TControl(Item.Data).Visible := True;
    SourceEditorTag:
    begin
      Editor := GetTabEditor(Item);
      Editor.Control.Visible := True;
      if Supports(Editor, IActivatingSourceEditor, ActivatingEditor) then
        ActivatingEditor.Activate;
    end;
  end;
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
  if Item.Tag = MapEditorTag then
    AllowClose := False;
end;

{*
  Gestionnaire d'événement OnTabCloseQuery de la tab bar des éditeurs
  @param Sender     Objet qui a déclenché l'événement
  @param Item       Élément qui va être fermé
  @param CanClose   Positionner à False pour empêcher la fermeture
*}
procedure TFormMain.TabBarEditorsTabCloseQuery(Sender: TObject;
  Item: TJvTabBarItem; var CanClose: Boolean);
begin
  if IsEditor(Item) then
    CanClose := GetTabEditor(Item).CanClose;
end;

{*
  Gestionnaire d'événement OnTabClosed de la tab bar des éditeurs
  @param Sender   Objet qui a déclenché l'événement
  @param Item     Élément qui est fermé
*}
procedure TFormMain.TabBarEditorsTabClosed(Sender: TObject;
  Item: TJvTabBarItem);
var
  Editor: ISourceEditor50;
begin
  if IsEditor(Item) then
  begin
    Editor := GetTabEditor(Item);
    if Editor <> nil then
    begin
      Editor.Release;
      Pointer(Editor) := nil;
    end;
    Item.Data := nil;
  end;

  Item.Tag := ClosedTabTag;
end;

{*
  Gestionnaire d'événement OnChange des éditeurs de source
*}
procedure TFormMain.EditorStateChange(const Sender: ISourceEditor50);
begin
  FindTab(Sender).Modified := Sender.Modified;
end;

{*
  Gestionnaire d'événement OnShowError de la liste des erreurs
  @param Sender   Object qui a déclenché l'événement
  @param Error    Erreur à montrer
*}
procedure TFormMain.MessagesShowError(Sender: TObject;
  Error: TSepiCompilerError);
var
  I: Integer;
  Tab: TJvTabBarItem;
  SourceCompiler: ISourceCompiler50;
begin
  // Find tab
  Tab := nil;
  for I := 0 to TabCount-1 do
  begin
    if IsEditor(I) and (Editors[I].SourceFile.FileName = Error.FileName) then
    begin
      Tab := Tabs[I];
      Break;
    end;
  end;

  // Show error
  if Tab <> nil then
  begin
    BringToFront;
    Tab.Selected := True;

    if Supports(GetTabEditor(Tab), ISourceCompiler50, SourceCompiler) then
      SourceCompiler.ShowError(Error);
  end;
end;

end.

