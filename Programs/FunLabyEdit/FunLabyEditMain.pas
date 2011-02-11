{*
  Unité principale de FunLabyEdit.exe
  Cette unité contient la fiche principale de FunLabyEdit.exe.
  @author sjrd
  @version 5.0
*}
unit FunLabyEditMain;

interface

uses
  Windows, SysUtils, StrUtils, Forms, Dialogs, Classes, Contnrs, ActnList,
  XPStyleActnCtrls, ActnMan, ImgList, Controls, MapEditor, ComCtrls, ActnMenus,
  ToolWin, ActnCtrls, ShellAPI, ScUtils, SdDialogs, SepiReflectionCore,
  FunLabyUtils, FilesUtils, UnitFiles, EditPluginManager, SourceEditors,
  FileProperties, FunLabyEditConsts, JvTabBar, EditUnits, NewSourceFile,
  ExtCtrls, ScSyncObjs, CompilerMessages, MapViewer, SepiCompilerErrors,
  EditMap,
  SepiImportsFunLabyTools, SourceEditorEvents, FunLabyEditOTA, JvComponentBase,
  JvDragDrop, JvAppStorage, JvAppXMLStorage;

type
  TActionDynArray = array of TAction;
  TActionClientItemDynArray = array of TActionClientItem;

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
    ActionOpenFile: TAction;
    ActionSaveFile: TAction;
    ActionSaveFileAs: TAction;
    ActionCloseFile: TAction;
    ActionNewFile: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionFileProperties: TAction;
    ActionAddMap: TAction;
    ActionRemoveMap: TAction;
    ActionHelpTopics: TAction;
    ActionAbout: TAction;
    ActionTest: TAction;
    ActionViewAllSources: TAction;
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
    ActionCompileAndReload: TAction;
    ActionEditMap: TAction;
    ActionVersionCheck: TAction;
    DropTarget: TJvDropTarget;
    ActionOpenSourceFile: TAction;
    OptionsStorage: TJvAppXMLFileStorage;
    ActionOpenRecentNone: TAction;
    ActionAutoCompile: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationHint(Sender: TObject);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionOpenRecentFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionFilePropertiesExecute(Sender: TObject);
    procedure ActionEditUnitsExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionAutoCompileExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAddMapExecute(Sender: TObject);
    procedure ActionRemoveMapExecute(Sender: TObject);
    procedure ActionEditMapExecute(Sender: TObject);
    procedure ActionOpenSourceFileExecute(Sender: TObject);
    procedure ActionAddExistingSourceExecute(Sender: TObject);
    procedure ActionAddNewSourceExecute(Sender: TObject);
    procedure ActionRemoveSourceExecute(Sender: TObject);
    procedure ActionViewAllSourcesExecute(Sender: TObject);
    procedure ActionViewSourceExecute(Sender: TObject);
    procedure ActionViewCompilerMessagesExecute(Sender: TObject);
    procedure ActionViewMapViewerExecute(Sender: TObject);
    procedure ActionTestExecute(Sender: TObject);
    procedure ActionCompileAllExecute(Sender: TObject);
    procedure ActionCompileAndReloadExecute(Sender: TObject);
    procedure ActionHelpTopicsExecute(Sender: TObject);
    procedure ActionVersionCheckExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure TabBarEditorsTabSelecting(Sender: TObject; Item: TJvTabBarItem;
      var AllowSelect: Boolean);
    procedure TabBarEditorsTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure TabBarEditorsTabCloseQuery(Sender: TObject; Item: TJvTabBarItem;
      var CanClose: Boolean);
    procedure TabBarEditorsTabClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure EditorStateChange(const Sender: ISourceEditor50);
    procedure MessagesShowError(Sender: TObject; Error: TSepiCompilerError);
    procedure DropTargetDragAccept(Sender: TJvDropTarget; var Accept: Boolean);
    procedure DropTargetDragDrop(Sender: TJvDropTarget;
      var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
  private
    BackgroundTasks: TScTaskQueue; /// Tâches d'arrière-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// Tâche de chargement de la racine de base

    BigMenuMaps: TActionClient;    /// Menu des cartes
    BigMenuSources: TActionClient; /// Menu des unités
    BigMenuExecute: TActionClient; /// Menu d'exécution

    TabMapEditor: TJvTabBarItem;     /// Onglet d'édition des cartes
    FrameMapEditor: TFrameMapEditor; /// Cadre d'édition des cartes

    SourceActions: TActionDynArray; /// Liste des actions du menu Sources liés

    FileHistory: TStrings; /// Historique des fichiers ouverts
    FileHistoryActions: TActionDynArray; /// List des actions de l'historique

    FormCompilerMessages: TFormCompilerMessages; /// Messages du compilateur
    FormMapViewer: TFormMapViewer;               /// Visualisateur de cartes

    FModified: Boolean;      /// Indique si le fichier a été modifié
    SepiRoot: TSepiRoot;     /// Racine Sepi du fichier en cours
    MasterFile: TMasterFile; /// Fichier maître
    Master: TMaster;         /// Maître FunLabyrinthe

    FActionToFind: TAction;     /// Action à trouver
    FFoundClients: TObjectList; /// Clients trouvés

    procedure LoadBaseSepiRoot;
    procedure NeedBaseSepiRoot;
    procedure BackgroundDiscard(var Obj);

    procedure LoadConfiguration;
    procedure SaveConfiguration;
    procedure LoadFileHistory;
    procedure SaveFileHistory;
    procedure AddFileToHistory(const FileName: TFileName);
    procedure UpdateFileHistory;

    function MakeFileHistoryAction(const FileName: TFileName): TAction;
    procedure MakeFileHistoryActions;
    procedure DeleteFileHistoryActions;

    function FindItemsByAction(Action: TAction): TActionClientItemDynArray;
    procedure FindItemsByActionCallback(AClient: TActionClient);
    procedure DeleteAllActions(var Actions: TActionDynArray);

    procedure LoadFile;
    procedure UnloadFile;

    function DoAutoCompile: Boolean;
    procedure MakeBPLUnitFileDescs(out UnitFileDescs: TUnitFileDescs);
    procedure CreateAutoCompileMasterFile(const UnitFileDescs: TUnitFileDescs);
    procedure AddAllSourceFiles(const BaseDir: TFileName);
    procedure AddSourceFileFor(const BinaryFileName: TFileName);
    function FindSourceFileFor(const BinaryFileName: TFileName): TFileName;

    function IsEditor(Tab: TJvTabBarItem): Boolean; overload;
    function IsEditor(Index: Integer): Boolean; overload;
    function GetTabEditor(Tab: TJvTabBarItem): ISourceEditor50;
    function FindTab(const Editor: ISourceEditor50): TJvTabBarItem; overload;
    function FindTab(const FileName: TFileName): TJvTabBarItem; overload;
    procedure OpenTab(const FileName: TFileName);
    function CloseTab(Tab: TJvTabBarItem): Boolean;

    procedure NewFile;
    procedure OpenFile(const FileName: TFileName);
    function SaveFile(FileName: TFileName = ''): Boolean;
    function SaveAll: Boolean;
    function CloseFile: Boolean;
    function ReloadFile: Boolean;

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
  public
    function AutoCompile: Boolean;
  end;

const {don't localize}
  idPlayer = 'Player'; /// ID de l'unique joueur

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.dfm}

const
  ClosedTabTag = -1;
  MapEditorTag = 0;
  SourceEditorTag = 1;

  MaxFileHistory = 5;

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
  Charge la configuration
*}
procedure TFormMain.LoadConfiguration;
begin
  LoadFileHistory;
end;

{*
  Sauvegarde la configuration
*}
procedure TFormMain.SaveConfiguration;
begin
  SaveFileHistory;
end;

{*
  Charge l'historique des fichiers
*}
procedure TFormMain.LoadFileHistory;
const {don't localize}
  Path = 'FileHistory';
  ItemName = 'File';
begin
  OptionsStorage.ReadStringList(Path, FileHistory, True, ItemName);
  UpdateFileHistory;
end;

{*
  Sauvegarde l'historique des fichiers
*}
procedure TFormMain.SaveFileHistory;
const {don't localize}
  Path = 'FileHistory';
  ItemName = 'File';
begin
  OptionsStorage.WriteStringList(Path, FileHistory, ItemName);
end;

{*
  Met à jour l'historique des fichiers avec un nouveau fichier
  @param FileName   Nom du fichier à ajouter à l'historique
*}
procedure TFormMain.AddFileToHistory(const FileName: TFileName);
var
  Index: Integer;
begin
  Index := FileHistory.IndexOf(FileName);
  if Index >= 0 then
    FileHistory.Move(Index, 0)
  else
  begin
    while FileHistory.Count >= MaxFileHistory do
      FileHistory.Delete(FileHistory.Count-1);

    FileHistory.Insert(0, FileName);
  end;

  UpdateFileHistory;
end;

{*
  Met à jour les menus pour l'historique des fichiers
*}
procedure TFormMain.UpdateFileHistory;
begin
  DeleteFileHistoryActions;
  MakeFileHistoryActions;

  ActionOpenRecentNone.Visible := FileHistory.Count = 0;
end;

{*
  Crée une action Ouvrir un fichier récent
  @param FileName   Nom du fichier à ouvrir
  @return L'action créée
*}
function TFormMain.MakeFileHistoryAction(const FileName: TFileName): TAction;
begin
  Result := TAction.Create(Self);
  with Result do
  begin
    ActionList := ActionManager;
    Caption := ExtractFileName(FileName);
    Result.Hint := FileName;
    OnExecute := ActionOpenRecentFileExecute;
  end;
end;

{*
  Crée une entrée dans le menu des fichiers récents pour toute l'historique
*}
procedure TFormMain.MakeFileHistoryActions;
var
  I, J: Integer;
  PreviousItems: TActionClientItemDynArray;
  Action: TAction;
  FileName: TFileName;
begin
  PreviousItems := FindItemsByAction(ActionOpenRecentNone);
  SetLength(FileHistoryActions, FileHistory.Count);

  for I := 0 to FileHistory.Count-1 do
  begin
    FileName := FileHistory[I];
    Action := MakeFileHistoryAction(FileName);
    FileHistoryActions[I] := Action;

    for J := 0 to Length(PreviousItems)-1 do
      PreviousItems[J] := ActionManager.AddAction(Action, PreviousItems[J]);
  end;
end;

{*
  Supprime toutes les actions Ouvrir un fichier récent et leurs menus associés
*}
procedure TFormMain.DeleteFileHistoryActions;
begin
  DeleteAllActions(FileHistoryActions);
end;

{*
  Find all action item clients matching a given action
  @param Action   Action to find
  @return Array of all action item clients matching the given action
*}
function TFormMain.FindItemsByAction(
  Action: TAction): TActionClientItemDynArray;
var
  I: Integer;
begin
  FActionToFind := Action;
  FFoundClients.Clear;

  ActionManager.ActionBars.IterateClients(ActionManager.ActionBars,
    FindItemsByActionCallback);

  SetLength(Result, FFoundClients.Count);
  for I := 0 to FFoundClients.Count-1 do
    Result[I] := TActionClientItem(FFoundClients[I]);
end;

{*
  Callback for FindItemsByAction
  @param AClient   Client
*}
procedure TFormMain.FindItemsByActionCallback(AClient: TActionClient);
begin
  if AClient is TActionClientItem then
    with TActionClientItem(AClient) do
      if Action = FActionToFind then
        FFoundClients.Add(AClient);
end;

{*
  Delete and free all actions in a given array
  @param Actions   Actions to delete
*}
procedure TFormMain.DeleteAllActions(var Actions: TActionDynArray);
var
  I: Integer;
begin
  for I := 0 to Length(Actions)-1 do
  begin
    ActionManager.DeleteActionItems([Actions[I]]);
    Actions[I].Free;
  end;
  SetLength(Actions, 0);
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
  SepiRoot := MasterFile.SepiRoot;
  Master := MasterFile.Master;

  // Activation de l'interface utilisateur
  ActionSaveFile.Enabled := True;
  ActionSaveFileAs.Enabled := True;
  ActionSaveAll.Enabled := True;
  ActionCloseFile.Enabled := True;
  ActionFileProperties.Enabled := True;
  ActionEditUnits.Enabled := True;
  ActionAutoCompile.Enabled := False;

  // Menu des cartes
  BigMenuMaps.Visible := True;
  ActionAddMap.Enabled := True;
  ActionRemoveMap.Enabled := True;
  ActionEditMap.Enabled := True;

  // Menu des sources
  BigMenuSources.Visible := True;
  ActionOpenSourceFile.Enabled := SourceFileEditors.FilterCount > 0;
  ActionAddExistingSource.Enabled := SourceFileEditors.FilterCount > 0;
  ActionAddNewSource.Enabled := not SourceFileCreators.IsEmpty;
  ActionViewAllSources.Enabled := True;
  MakeSourceActions;

  // Menu d'exécution
  BigMenuExecute.Visible := True;
  ActionTest.Enabled := True;
  ActionCompileAll.Enabled := True;
  ActionCompileAndReload.Enabled := True;

  // Divers
  ToolBarFile.ActionClient.Items[1].Action := ActionOpenSourceFile;
  ActionOpenSourceFile.ShortCut := ActionOpenFile.ShortCut;
  ActionOpenFile.ShortCut := 0;

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
  ActionAutoCompile.Enabled := True;

  // Menu des cartes
  BigMenuMaps.Visible := False;
  ActionAddMap.Enabled := False;
  ActionRemoveMap.Enabled := False;
  ActionEditMap.Enabled := False;

  // Menu des sources
  BigMenuSources.Visible := False;
  ActionOpenSourceFile.Enabled := False;
  ActionAddExistingSource.Enabled := False;
  ActionAddNewSource.Enabled := False;
  ActionViewAllSources.Enabled := False;
  DeleteSourceActions;

  // Menu d'exécution
  BigMenuExecute.Visible := False;
  ActionTest.Enabled := False;
  ActionCompileAll.Enabled := False;
  ActionCompileAndReload.Enabled := False;

  // Divers
  ToolBarFile.ActionClient.Items[1].Action := ActionOpenFile;
  ActionOpenFile.ShortCut := ActionOpenSourceFile.ShortCut;
  ActionOpenSourceFile.ShortCut := 0;

  // Autres variables
  SepiRoot := nil;
  Master := nil;

  // Ne pas laisser la croix de fermeture indiquer une modification
  Modified := False;
end;

{*
  Lance l'auto-compilation de tous les fichiers compilés trouvés
  @return True en cas de succès, False s'il y a eu des erreurs
*}
function TFormMain.DoAutoCompile: Boolean;
var
  UnitFileDescs: TUnitFileDescs;
begin
  MakeBPLUnitFileDescs(UnitFileDescs);
  CreateAutoCompileMasterFile(UnitFileDescs);
  AddAllSourceFiles(JoinPath([FunLabyAppDataDir, SourcesDir]));
  Result := CompileAll;
end;

{*
  Crée les descripteurs d'unités pour tous les BPL trouvés
  @param UnitFileDescs   En sortie : descripteurs des unités BPL
*}
procedure TFormMain.MakeBPLUnitFileDescs(out UnitFileDescs: TUnitFileDescs);
const
  ExcludedBPLFiles: array[0..0] of string = ('Compatibility4x.bpl');
var
  FileNames: TStrings;
  SearchRec: TSearchRec;
  I: Integer;
begin
  FileNames := TStringList.Create;
  try
    // Search for *.bpl files
    if FindFirst(JoinPath([FunLabyAppDataDir, UnitsDir, '*.bpl']),
      faAnyFile, SearchRec) = 0 then
    try
      repeat
        if not AnsiMatchText(SearchRec.Name, ExcludedBPLFiles) then
          FileNames.Add(SearchRec.Name);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;

    // Make result
    SetLength(UnitFileDescs, FileNames.Count);
    for I := 0 to FileNames.Count-1 do
      UnitFileDescs[I].HRef := FileNames[I];
  finally
    FileNames.Free;
  end;
end;

{*
  Crée le fichier maître pour une auto-compilation
  @param UnitFileDescs   Descripteurs de fichiers unités
*}
procedure TFormMain.CreateAutoCompileMasterFile(
  const UnitFileDescs: TUnitFileDescs);
begin
  NeedBaseSepiRoot;

  MasterFile := TMasterFile.CreateNew(BaseSepiRoot, UnitFileDescs);
  try
    LoadFile;
  except
    BackgroundDiscard(MasterFile);
    raise;
  end;

  Modified := False;
end;

{*
  Ajoute tous les fichiers sources qu'on peut trouver au projet
  @param BaseDir   Dossier de base
*}
procedure TFormMain.AddAllSourceFiles(const BaseDir: TFileName);
const
  Extensions: array[0..3] of string = ('.scu', '.ssq', '.fnd', '.pas');
var
  SearchRec: TSearchRec;
  Ext: string;
begin
  // Add .scu files in this directory
  if FindFirst(BaseDir+'*.*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      Ext := ExtractFileExt(SearchRec.Name);
      if AnsiMatchText(Ext, Extensions) then
        AddSourceFileFor(BaseDir+ChangeFileExt(SearchRec.Name, '.scu'));
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;

  // Recurse into subdirectories
  if FindFirst(BaseDir+'*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if SearchRec.Attr and faDirectory <> 0 then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          AddAllSourceFiles(BaseDir + SearchRec.Name + '\');
      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

{*
  Ajoute le fichier source correspondant à un binaire existant
  @param BinaryFileName   Nom du fichier binaire .scu
*}
procedure TFormMain.AddSourceFileFor(const BinaryFileName: TFileName);
var
  SourceFileName: string;
begin
  SourceFileName := FindSourceFileFor(BinaryFileName);
  if SourceFileName <> '' then
    OpenFile(SourceFileName);
end;

{*
  Trouve le fichier source pour un fichier binaire
  @param BinaryFileName   Nom du fichier binaire .scu
  @return Nom du fichier source correspondant, ou '' si non trouvé
*}
function TFormMain.FindSourceFileFor(
  const BinaryFileName: TFileName): TFileName;
var
  ResultFileName: TFileName;
  ResultDateTime: TDateTime;
begin
  ResultFileName := '';

  SourceFileEditors.ForEach(
    procedure(const Key, Value; var Continue: Boolean)
    var
      FileName: TFileName;
      FileDateTime: TDateTime;
    begin
      FileName := ChangeFileExt(BinaryFileName, '.' + string(Key));

      if not FileAge(FileName, FileDateTime) then
        Exit;

      if (ResultFileName = '') or (FileDateTime > ResultDateTime) then
      begin
        ResultFileName := FileName;
        ResultDateTime := FileDateTime;
      end;
    end);

  Result := ResultFileName;
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
function TFormMain.FindTab(const FileName: TFileName): TJvTabBarItem;
var
  I: Integer;
begin
  for I := 0 to TabCount-1 do
  begin
    if IsEditor(I) and (Editors[I].FileName = FileName) then
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
procedure TFormMain.OpenTab(const FileName: TFileName);
var
  Editor: ISourceEditor50;
  SourceFile: TSourceFile;
  EditorUsingOTA: ISourceEditorUsingOTA50;
  EditorControl: TControl;
  Tab: TJvTabBarItem;
begin
  // Créer l'éditeur
  try
    Editor := SourceFileEditors.CreateEditor(FileName);
  except
    on Error: Exception do
    begin
      SourceFile := MasterFile.FindSourceFile(FileName);

      if SourceFile = nil then
      begin
        ShowDialog(SErrorTitle, Error.Message, dtError);
      end else
      begin
        if ShowDialog(SErrorTitle, Format(SErrorWhileOpeningSourceFile,
          [Error.Message]), dtError, dbYesNo, 2) = drYes then
        begin
          RemoveSourceFile(SourceFile);
        end;
      end;

      Exit;
    end;
  end;

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
  Tab.Caption := ExtractFileName(Editor.FileName);
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
begin
  NeedBaseSepiRoot;

  MasterFile := TMasterFile.CreateNew(BaseSepiRoot);
  try
    if TFormFileProperties.ManageProperties(MasterFile) then
    begin
      MasterFile.Master.CreateAdditionnalComponent(TPlayer, idPlayer);
      LoadFile;
    end else
    begin
      BackgroundDiscard(MasterFile);
    end;
  except
    BackgroundDiscard(MasterFile);
    raise;
  end;

  ActionAddMap.Execute;
end;

{*
  Ouvre un fichier et le charge
  @param FileName   Nom du fichier maître à charger
*}
procedure TFormMain.OpenFile(const FileName: TFileName);
begin
  NeedBaseSepiRoot;

  if HasExtension(FileName, FunLabyProjectExt) then
  begin
    // Load a new project

    if not CloseFile then
      Exit;

    // Try not to look for trouble more than necessary
    while not BackgroundTasks.Ready do
      Sleep(50);

    MasterFile := TMasterFile.Create(BaseSepiRoot, FileName, fmEdit);
    try
      LoadFile;
    except
      BackgroundDiscard(MasterFile);
      raise;
    end;

    AddFileToHistory(FileName);
  end else
  begin
    // Just open an editor

    OpenTab(FileName);
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
    begin
      Result := False;
      Exit;
    end;
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
end;

{*
  Recharge le fichier couramment ouvert
  @return True si le fichier a bien été fermé, False sinon
*}
function TFormMain.ReloadFile: Boolean;
var
  FileName: TFileName;
  OpenSources: TStrings;
  I: Integer;
begin
  Result := False;

  if MasterFile = nil then
    Exit;

  FileName := MasterFile.FileName;
  OpenSources := TStringList.Create;
  try
    // Save the list of open tabs
    for I := 0 to TabCount-1 do
    begin
      if IsEditor(I) then
        OpenSources.Add(Editors[I].FileName);
    end;

    // Actual reload
    OpenFile(FileName);

    // Restore open tabs
    for I := 0 to OpenSources.Count-1 do
      OpenTab(OpenSources[I]);

    { Usually we reload to have the component appear in the palette, so go to
      the tab that shows this palette. }
    TabBarEditors.SelectedTab := TabMapEditor;
  finally
    OpenSources.Free;
  end;

  Result := True;
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

  if (Sender is TSepiRootFork) and
    Assigned(TSepiRootFork(Sender).OriginalRoot.OnLoadUnit) then
    Result := TSepiRootFork(Sender).OriginalRoot.OnLoadUnit(Sender, UnitName)
  else
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
begin
  DeleteAllActions(SourceActions);
end;

{*
  Ajoute un fichier source
  @param FileName   Nom du fichier source
*}
procedure TFormMain.AddSourceFile(const FileName: TFileName);
var
  SourceFile: TSourceFile;
  Action: TAction;
begin
  // Vérifier que ce fichier source n'est pas déjà attaché au projet
  SourceFile := MasterFile.FindSourceFile(FileName);
  if SourceFile <> nil then
  begin
    OpenTab(FileName);
    Exit;
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
  OpenTab(SourceFile.FileName);

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
        (Length(SourceActions)-Index-1) * SizeOf(TAction));
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
  Lance l'auto-compilation de tous les fichiers compilés trouvés
  @return True en cas de succès, False s'il y a eu des erreurs
*}
function TFormMain.AutoCompile: Boolean;
begin
  Result := DoAutoCompile;
end;

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Create the internal objects
  BackgroundTasks := TScTaskQueue.Create(True, False);
  FileHistory := TStringList.Create;
  FFoundClients := TObjectList.Create(False);

  // Start the task of loading SepiRoot
  BaseSepiRoot := TSepiRoot.Create;
  BaseSepiRootLoadTask := TScMethodTask.Create(BackgroundTasks,
    LoadBaseSepiRoot, False);

  // Setup some dynamic properties
  Application.OnHint := ApplicationHint;

  OpenDialog.InitialDir := JoinPath([FunLabyAppDataDir, ProjectsDir]);
  SaveDialog.InitialDir := OpenDialog.InitialDir;

  OpenSourceFileDialog.InitialDir := JoinPath([FunLabyAppDataDir, SourcesDir]);
  OpenSourceFileDialog.Filter := SourceFileEditors.FiltersAsText;

  MasterFile := nil;

  // Setup references to specific menus
  BigMenuMaps := MainMenuBar.ActionClient.Items[1];
  BigMenuSources := MainMenuBar.ActionClient.Items[2];
  BigMenuExecute := MainMenuBar.ActionClient.Items[4];

  // Create the MapEditor tab
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

  // Create the compiler messages form
  FormCompilerMessages := TFormCompilerMessages.Create(Self);
  FormCompilerMessages.OnShowError := MessagesShowError;

  // Create the map viewer
  FormMapViewer := TFormMapViewer.Create(Self);

  // Load configuration
  LoadConfiguration;

  // Open the file specified on the command-line
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1));
end;

{*
  Gestionnaire d'événement OnDestroy
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
var
  TriesLeft: Integer;
begin
  SaveConfiguration;

  Application.OnHint := nil;

  FrameMapEditor.Free;

  NeedBaseSepiRoot;

  BackgroundDiscard(BaseSepiRoot);
  BackgroundDiscard(BaseSepiRootLoadTask);

  TriesLeft := 20;
  while not BackgroundTasks.Ready do
  begin
    Sleep(100);
    Dec(TriesLeft);
    if TriesLeft <= 0 then
      System.Halt(2);
  end;

  FFoundClients.Free;

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
  if OpenDialog.Execute then
  begin
    SaveDialog.FileName := OpenDialog.FileName;
    OpenFile(OpenDialog.FileName);
  end;
end;

{*
  Gestionnaire d'événement OnExecute d'une action Ouvrir un fichier récent
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionOpenRecentFileExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  FileName := (Sender as TAction).Hint;
  OpenFile(FileName);
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
begin
  if not SaveAll then
    Exit;

  if TFormEditUnits.EditUnits(MasterFile) then
    ReloadFile;
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
  Gestionnaire d'événement Tout recompiler
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAutoCompileExecute(Sender: TObject);
begin
  if AutoCompile then
    ShowDialog(SAutoCompileSuccessTitle, SAutoCompileSuccess);
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
  Gestionnaire d'événement OnExecute de l'action Édition de carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionEditMapExecute(Sender: TObject);
begin
  if FrameMapEditor.CurrentMap = nil then
    Exit;

  if TFormEditMap.EditMap(FrameMapEditor.CurrentMap) then
  begin
    FrameMapEditor.InvalidateMap;
    MarkModified;
  end;
end;

{*
  Gestionnaire d'événement Ouvrir un source existant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionOpenSourceFileExecute(Sender: TObject);
var
  I: Integer;
begin
  if OpenSourceFileDialog.Execute then
  begin
    for I := 0 to OpenSourceFileDialog.Files.Count-1 do
      OpenFile(OpenSourceFileDialog.Files[I]);
  end;
end;

{*
  Gestionnaire d'événement Ajouter un source existant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionAddExistingSourceExecute(Sender: TObject);
var
  I: Integer;
begin
  if OpenSourceFileDialog.Execute then
  begin
    for I := 0 to OpenSourceFileDialog.Files.Count-1 do
      AddSourceFile(OpenSourceFileDialog.Files[I]);
  end;
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

  SourceFile := MasterFile.FindSourceFile(GetTabEditor(Tab).FileName);

  if SourceFile = nil then
    CloseTab(Tab)
  else
  begin
    if ShowDialog(SRemoveSourceTitle, SRemoveSource, dtConfirmation,
      dbYesNo, 2) <> drYes then
      Exit;

    if CloseTab(Tab) then
      RemoveSourceFile(SourceFile);
  end;
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
  Tab := FindTab(SourceFile.FileName);
  if Tab <> nil then
    Tab.Selected := True
  else
    OpenTab(SourceFile.FileName);
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
  Gestionnaire d'événement OnExecute de l'action Tester
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionTestExecute(Sender: TObject);
begin
  if Modified and (not SaveFile(MasterFile.FileName)) then
    Exit;

  if not CompileAll then
    Exit;

  {don't localize}
  ShellExecute(0, 'open', PChar(Dir+'FunLaby.exe'),
    PChar('"'+MasterFile.FileName+'"'), nil, SW_SHOWNORMAL);
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
  Gestionnaire d'événement OnExecute de l'action Compiler et recharger
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionCompileAndReloadExecute(Sender: TObject);
begin
  if SaveAll and CompileAll then
    ReloadFile;
end;

{*
  Gestionnaire d'événement OnExecute de l'action Rubriques d'aide
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionHelpTopicsExecute(Sender: TObject);
begin
  RunURL(Application.HelpFile);
end;

{*
  Gestionnaire d'événement OnExecute de l'action Mise à jour automatique
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ActionVersionCheckExecute(Sender: TObject);
begin
  EditVersionCheckOptions;
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
      FormMapViewer.Events := nil;
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
  Events: IOTAMapViewerEvents51;
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
      if Supports(Editor, IOTAMapViewerEvents51, Events) then
        FormMapViewer.Events := Events;
    end;
  end;
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
  if Item.Tag = MapEditorTag then
  begin
    CloseFile;
    CanClose := False;
  end else if IsEditor(Item) then
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
    if IsEditor(I) and (Editors[I].FileName = Error.FileName) then
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

{*
  Gestionnaire d'événement OnDragAccept du DropTarget
  @param Sender   Objet qui a déclenché l'événeement
  @param Accept   En sortie : True pour accepter le fichier
*}
procedure TFormMain.DropTargetDragAccept(Sender: TJvDropTarget;
  var Accept: Boolean);
var
  FileNames: TStrings;
  I: Integer;
begin
  FileNames := TStringList.Create;
  try
    Sender.GetFilenames(FileNames);

    if (FileNames.Count = 1) and
      HasExtension(FileNames[0], FunLabyProjectExt) then
    begin
      // Project file, OK
    end else if MasterFile = nil then
    begin
      // No project file open, not OK
      Accept := False;
    end else
    begin
      // Editor files
      for I := 0 to FileNames.Count-1 do
      begin
        if not SourceFileEditors.ExistsEditor(FileNames[I]) then
        begin
          Accept := False;
          Exit;
        end;
      end;
    end;
  finally
    FileNames.Free;
  end;
end;

{*
  Gestionnaire d'événement OnDragDrop du DropTarget
  @param Sender   Objet qui a déclenché l'événeement
  @param Effect   Effet du drop
*}
procedure TFormMain.DropTargetDragDrop(Sender: TJvDropTarget;
  var Effect: TJvDropEffect; Shift: TShiftState; X, Y: Integer);
var
  FileNames: TStrings;
  I: Integer;
begin
  FileNames := TStringList.Create;
  try
    Sender.GetFilenames(FileNames);

    for I := 0 to FileNames.Count-1 do
      OpenFile(FileNames[I]);
  finally
    FileNames.Free;
  end;
end;

end.

