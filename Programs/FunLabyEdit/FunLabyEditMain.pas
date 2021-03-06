{*
  Unit� principale de FunLabyEdit.exe
  Cette unit� contient la fiche principale de FunLabyEdit.exe.
  @author sjrd
  @version 5.0
*}
unit FunLabyEditMain;

interface

uses
  Windows, SysUtils, StrUtils, Forms, Dialogs, Classes, Contnrs, ActnList,
  XPStyleActnCtrls, ActnMan, ImgList, Controls, MapEditor, ComCtrls, ActnMenus,
  ToolWin, ActnCtrls, ShellAPI, ScUtils, ScStrUtils, SdDialogs,
  SepiReflectionCore, FunLabyUtils, FilesUtils, PluginManager, SourceEditors,
  FileProperties, FunLabyEditConsts, JvTabBar, EditUnits, NewSourceFile,
  ExtCtrls, ScSyncObjs, CompilerMessages, MapViewer, SepiCompilerErrors,
  EditMap, SepiImportsFunLaby, NewProject,
  SepiImportsFunLabyTools, SourceEditorEvents, FunLabyEditOTA, JvComponentBase,
  JvDragDrop, JvAppStorage, JvAppXMLStorage, ExtDlgs;

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
    ActionOpenProjectManager: TAction;
    ActionComponentListing: TAction;
    SaveTextFileDialog: TSaveTextFileDialog;
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
    procedure ActionOpenProjectManagerExecute(Sender: TObject);
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
    procedure ActionComponentListingExecute(Sender: TObject);
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
    procedure SaveDialogCanClose(Sender: TObject; var CanClose: Boolean);
  private
    BackgroundTasks: TScTaskQueue; /// T�ches d'arri�re-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// T�che de chargement de la racine de base

    BigMenuMaps: TActionClient;    /// Menu des cartes
    BigMenuSources: TActionClient; /// Menu des unit�s
    BigMenuExecute: TActionClient; /// Menu d'ex�cution

    TabMapEditor: TJvTabBarItem;     /// Onglet d'�dition des cartes
    FrameMapEditor: TFrameMapEditor; /// Cadre d'�dition des cartes

    SourceActions: TActionDynArray; /// Liste des actions du menu Sources li�s

    FileHistory: TStrings; /// Historique des fichiers ouverts
    FileHistoryActions: TActionDynArray; /// List des actions de l'historique

    FormCompilerMessages: TFormCompilerMessages; /// Messages du compilateur
    FormMapViewer: TFormMapViewer;               /// Visualisateur de cartes

    FModified: Boolean;      /// Indique si le fichier a �t� modifi�
    SepiRoot: TSepiRoot;     /// Racine Sepi du fichier en cours
    MasterFile: TMasterFile; /// Fichier ma�tre
    Master: TMaster;         /// Ma�tre FunLabyrinthe

    FActionToFind: TAction;     /// Action � trouver
    FFoundClients: TObjectList; /// Clients trouv�s

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

    procedure DoAutoCompile; overload;
    procedure DoAutoCompile(const Path: TFileName); overload;
    procedure CreateAutoCompileMasterFile(const ProjectDir: TFileName = '');
    procedure AddAllSourceFiles(const BaseDir: TFileName);
    procedure AddSourceFileFor(const BinaryFileName: TFileName);
    function FindSourceFileFor(const BinaryFileName: TFileName): TFileName;

    function IsEditor(Tab: TJvTabBarItem): Boolean; overload;
    function IsEditor(Index: Integer): Boolean; overload;
    function GetTabEditor(Tab: TJvTabBarItem): ISourceEditor50;
    function FindTab(const Editor: ISourceEditor50): TJvTabBarItem; overload;
    function FindTab(const FileName: TFileName): TJvTabBarItem; overload;
    procedure OpenTab(const FileName: TFileName; SelectTab: Boolean = True);
    function CloseTab(Tab: TJvTabBarItem): Boolean;

    procedure NewFile;
    procedure OpenFile(const FileName: TFileName);
    function SaveFile(FileName: TFileName = ''): Boolean;
    function SaveAll: Boolean;
    function CloseFile: Boolean;
    function ReloadFile: Boolean;

    function CompilerLoadUnit(Sender: TSepiRoot;
      const UnitName: string): TSepiUnit;
    function CompileAll(IgnoreWarnings: Boolean = False): Boolean;

    procedure MarkModified;

    function MakeSourceAction(const SourceHRef: string): TAction;
    procedure MakeSourceActions;
    procedure DeleteSourceActions;

    function FindSourceFile(const FileName: TFileName): string;
    procedure AddSourceFile(const FileName: TFileName);
    procedure RemoveSourceFile(const SourceHRef: string);

    procedure SetModified(Value: Boolean);

    function GetTabCount: Integer;
    function GetTabs(Index: Integer): TJvTabBarItem;
    function GetEditors(Index: Integer): ISourceEditor50;

    procedure MakeComponentListing(Listing: TStrings);

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

uses
  Generics.Collections;

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
  Cette m�thode est appel�e dans le thread des t�ches en arri�re-plan
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
  Attend que la racine Sepi de base soit charg�e, et v�rifie l'�tat d'erreur
  En cas d'erreur pendant le chargement de la racine de base, un message
  avertit l'utilisateur et le programme est arr�t� brutalement.
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
  Cr�e une nouvelle t�che d'arri�re-plan de lib�ration d'un objet
  @param Obj   Objet � lib�rer en arri�re-plan (mis � nil)
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
  Met � jour l'historique des fichiers avec un nouveau fichier
  @param FileName   Nom du fichier � ajouter � l'historique
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
  Met � jour les menus pour l'historique des fichiers
*}
procedure TFormMain.UpdateFileHistory;
begin
  DeleteFileHistoryActions;
  MakeFileHistoryActions;

  ActionOpenRecentNone.Visible := FileHistory.Count = 0;
end;

{*
  Cr�e une action Ouvrir un fichier r�cent
  @param FileName   Nom du fichier � ouvrir
  @return L'action cr��e
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
  Cr�e une entr�e dans le menu des fichiers r�cents pour toute l'historique
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
  Supprime toutes les actions Ouvrir un fichier r�cent et leurs menus associ�s
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
  Charge le MasterFile cr��
  Charge le MasterFile cr�� dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
begin
  // Un fichier nouvellement charg� n'est modifi� que s'il vient d'�tre cr��
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

  // Menu d'ex�cution
  BigMenuExecute.Visible := True;
  ActionTest.Enabled := True;
  ActionCompileAll.Enabled := True;
  ActionCompileAndReload.Enabled := True;

  // Menu des outils
  ActionComponentListing.Enabled := True;

  // Divers
  ToolBarFile.ActionClient.Items[1].Action := ActionOpenSourceFile;
  ActionOpenSourceFile.ShortCut := ActionOpenFile.ShortCut;
  ActionOpenFile.ShortCut := 0;

  // Chargement des cartes
  FrameMapEditor.LoadFile(MasterFile);
  FormMapViewer.Master := Master;
end;

{*
  D�charge le MasterFile
  D�charge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
begin
  // D�chargement des cartes
  FormMapViewer.Hide;
  FormMapViewer.Master := nil;
  FrameMapEditor.UnloadFile;

  // D�sactivation de l'interface utilisateur
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

  // Menu d'ex�cution
  BigMenuExecute.Visible := False;
  ActionTest.Enabled := False;
  ActionCompileAll.Enabled := False;
  ActionCompileAndReload.Enabled := False;

  // Menu des outils
  ActionComponentListing.Enabled := False;

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
  Lance l'auto-compilation de tous les fichiers compil�s trouv�s
  @throws EAbort s'il y a eu des erreurs
*}
procedure TFormMain.DoAutoCompile;
begin
  DoAutoCompile(JoinPath([LibraryPath, SourcesDir]));

  IterateDir(ProjectsPath, '*',
    procedure(const ProjectDir: TFileName; const SearchRec: TSearchRec)
    begin
      if SearchRec.Attr and faDirectory <> 0 then
        DoAutoCompile(JoinPath([ProjectDir, SourcesDir]));
    end);
end;

{*
  Lance l'auto-compilation de tous les fichiers compil�s trouv�s dans un dossier
  @param Path   Dossier sous lequel effectuer l'auto-compilation
  @throws EAbort s'il y a eu des erreurs
*}
procedure TFormMain.DoAutoCompile(const Path: TFileName);
begin
  CreateAutoCompileMasterFile;
  AddAllSourceFiles(Path);

  if not CompileAll(True) then
    Abort;

  CloseFile;
end;

{*
  Cr�e le fichier ma�tre pour une auto-compilation
  @param UnitFileDescs   Descripteurs de fichiers unit�s
*}
procedure TFormMain.CreateAutoCompileMasterFile(
  const ProjectDir: TFileName = '');
begin
  NeedBaseSepiRoot;

  MasterFile := TMasterFile.CreateAutoCompiler(BaseSepiRoot, ProjectDir);
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
  Extensions: array[0..2] of string = ('.ssq', '.fnd', '.pas');
begin
  IterateDir(BaseDir, '*',
    procedure(const FullPath: TFileName; const SearchRec: TSearchRec)
    begin
      if SearchRec.Attr and faDirectory = 0 then
      begin
        // Add the source file
        if AnsiMatchText(ExtractFileExt(SearchRec.Name), Extensions) then
          AddSourceFileFor(FullPath);
      end else
      begin
        // Recurse into subdirectory
        AddAllSourceFiles(FullPath);
      end;
    end);
end;

{*
  Ajoute le fichier source correspondant � un binaire existant
  @param BinaryFileName   Nom du fichier binaire .scu
*}
procedure TFormMain.AddSourceFileFor(const BinaryFileName: TFileName);
var
  SourceFileName: string;
begin
  SourceFileName := FindSourceFileFor(BinaryFileName);
  Assert(SourceFileName <> '');
  OpenTab(SourceFileName, False);
end;

{*
  Trouve le fichier source pour un fichier binaire
  @param BinaryFileName   Nom du fichier binaire .scu
  @return Nom du fichier source correspondant, ou '' si non trouv�
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
  Indique si un onglet est un �diteur de source
  @param Tab   Onglet
  @return True si l'onglet est un �diteur de source, False sinon
*}
function TFormMain.IsEditor(Tab: TJvTabBarItem): Boolean;
begin
  Result := Tab.Tag = SourceEditorTag;
end;

{*
  Indique si un onglet est un �diteur de source
  @param Index   Index de l'onglet
  @return True si l'onglet est un �diteur de source, False sinon
*}
function TFormMain.IsEditor(Index: Integer): Boolean;
begin
  Result := Tabs[Index].Tag = SourceEditorTag;
end;

{*
  �diteur de source correspondant � un onglet donn�
  @param Tab   Onglet
  @return �diteur de source correspondant, ou nil si ce n'est pas un source
*}
function TFormMain.GetTabEditor(Tab: TJvTabBarItem): ISourceEditor50;
begin
  if Tab.Tag = SourceEditorTag then
    Result := ISourceEditor50(Pointer(Tab.Data))
  else
    Result := nil;
end;

{*
  Trouve l'onglet qui contient un �diteur de source donn�
  @param Editor   �diteur de source recherch�
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
  Trouve l'onglet qui contient un �diteur de source donn�
  @param SourceFile   Fichier source recherch�
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
  Ajoute un �diteur de source � l'interface visuelle et l'affiche
  @param Editor   �diteur � ajouter
*}
procedure TFormMain.OpenTab(const FileName: TFileName;
  SelectTab: Boolean = True);
var
  Editor: ISourceEditor50;
  EditorUsingOTA: ISourceEditorUsingOTA50;
  EditorControl: TControl;
  Tab: TJvTabBarItem;
begin
  // If a tab already exists for this file, simply select it
  Tab := FindTab(FileName);
  if Tab <> nil then
  begin
    Tab.Selected := True;
    Exit;
  end;

  // Create the editor
  Editor := SourceFileEditors.CreateEditor(FileName);

  // Set OTA main form
  if Supports(Editor, ISourceEditorUsingOTA50, EditorUsingOTA) then
    EditorUsingOTA.SetFunLabyEditMainForm(Self);

  // Load source file into editor
  Editor.LoadFile(FileName);

  // Configure the editor control
  EditorControl := Editor.Control;
  EditorControl.Align := alClient;
  EditorControl.Visible := False;
  EditorControl.Parent := PanelEditors;

  // Set up editor events
  Editor.OnStateChange := EditorStateChange;

  // Create the tab
  Tab := TJvTabBarItem(TabBarEditors.Tabs.Add);
  Tab.Caption := ExtractFileName(Editor.FileName);
  Tab.Data := TObject(Pointer(Editor));
  Tab.Tag := SourceEditorTag;
  Tab.Modified := Editor.Modified;

  // Select the tab if required
  if SelectTab then
    Tab.Selected := True;
end;

{*
  Ferme un onglet d'�dition de source
  @param Tab   Onglet � fermer
  @return True si l'onglet a bien �t� ferm�, False sinon
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
  Cr�e un nouveau fichier et le charge
*}
procedure TFormMain.NewFile;
var
  FileName: TFileName;
begin
  FileName := TFormNewProject.NewProject;
  if FileName = '' then
    Exit;

  OpenFile(FileName);

  if Master.MapCount = 0 then
    ActionAddMap.Execute;
end;

{*
  Ouvre un fichier et le charge
  @param FileName   Nom du fichier ma�tre � charger
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
  @return True si l'enregistrement a bien �t� effectu�, False sinon
*}
function TFormMain.SaveFile(FileName: TFileName = ''): Boolean;
begin
  Result := False;

  // Choisir un nom de fichier pour l'enregistrement
  if FileName = '' then
  begin
    if not SaveDialog.Execute then
      Exit;
    OpenDialog.FileName := SaveDialog.FileName;
    FileName := SaveDialog.FileName;
  end;

  // Enregistrer le projet
  MasterFile.Save(FileName);
  Result := True;
  Modified := False;
end;

{*
  Enregistre tous les fichiers
  @return True si tous les fichiers ont bien �t� enregistr�s, False sinon
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
  @return True si le fichier a bien �t� ferm�, False sinon
*}
function TFormMain.CloseFile: Boolean;
var
  I: Integer;
  Tab: TJvTabBarItem;
begin
  Result := True;

  if MasterFile = nil then
    Exit;

  // Fermer tous les sources ouverts : chacun peut emp�cher la fermeture
  for I := TabCount-1 downto 0 do
  begin
    Tab := Tabs[I];
    if IsEditor(Tab) and (not CloseTab(Tab)) then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Demander de sauvegarder le projet, s'il est modifi�
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

  // Tout d�charger
  UnloadFile;

  BackgroundDiscard(MasterFile);
end;

{*
  Recharge le fichier couramment ouvert
  @return True si le fichier a bien �t� ferm�, False sinon
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
  Charge une unit� dans le compilateur
  @param Sender     Racine Sepi
  @param UnitName   Nom de l'unit�
  @return Unit� Sepi charg�e, ou nil si non trouv�e
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
      // �diteur qui peut charger directement une unit�
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
  @param IgnoreWarnings   Si True, il faut une erreur pour que la fen�tre des
                          erreurs soit affich�e.
  @return True si les sources ont bien �t� compil�s, False sinon
*}
function TFormMain.CompileAll(IgnoreWarnings: Boolean = False): Boolean;
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

    with FormCompilerMessages do
    begin
      if IgnoreWarnings then
        Visible := Errors.ErrorCount > 0
      else
        Visible := Errors.Count > 0;

      if Visible then
        ShowFirst;
    end;
  finally
    CompilerRoot.Free;
  end;
end;

{*
  Marque le fichier comme modifi�
*}
procedure TFormMain.MarkModified;
begin
  Modified := True;
end;

{*
  Cr�e une action Voir un source pour un fichier source donn�
  @param SourceHRef   HRef du fichier source � voir
  @return L'action cr��e
*}
function TFormMain.MakeSourceAction(const SourceHRef: string): TAction;
begin
  Result := TAction.Create(Self);
  with Result do
  begin
    ActionList := ActionManager;
    Caption := Copy(SourceHRef, LastDelimiter(HRefDelim, SourceHRef)+1, MaxInt);
    Hint := SourceHRef;
    OnExecute := ActionViewSourceExecute;
  end;
end;

{*
  Cr�e une entr�e dans le menu des sources pour chaque fichier source
*}
procedure TFormMain.MakeSourceActions;
var
  I: Integer;
  PreviousItem: TActionClientItem;
  SourceHRef: string;
begin
  PreviousItem := BigMenuSources.Items[BigMenuSources.Items.Count-1];
  SetLength(SourceActions, MasterFile.SourceFiles.Count);

  for I := 0 to MasterFile.SourceFiles.Count-1 do
  begin
    SourceHRef := MasterFile.SourceFiles[I];
    SourceActions[I] := MakeSourceAction(SourceHRef);
    PreviousItem := ActionManager.AddAction(SourceActions[I], PreviousItem);
  end;
end;

{*
  Supprime toutes les actions Voir un fichier source et leurs menus associ�s
*}
procedure TFormMain.DeleteSourceActions;
begin
  DeleteAllActions(SourceActions);
end;

{*
  Trouve le href d'un fichier source attach� au projet
  @param FileName   Nom du fichier source
  @return HRef du fichier attach� au projet, ou '' s'il n'est pas attach�
*}
function TFormMain.FindSourceFile(const FileName: TFileName): string;
begin
  try
    Result := MasterFile.MakeHRef(FileName, SourcesDir);
    if MasterFile.SourceFiles.IndexOf(Result) < 0 then
      Result := '';
  except
    on EInOutError do
      Result := '';
  end;
end;

{*
  Ajoute un fichier source
  @param FileName   Nom du fichier source
*}
procedure TFormMain.AddSourceFile(const FileName: TFileName);
var
  SourceHRef: string;
  Action: TAction;
begin
  // Make an href for the file
  SourceHRef := MasterFile.MakeHRef(FileName, SourcesDir);

  // Test whether the file is already linked to the project
  if MasterFile.SourceFiles.IndexOf(SourceHRef) >= 0 then
  begin
    OpenTab(FileName);
    Exit;
  end;

  // Link the source file to the project
  MasterFile.SourceFiles.Add(SourceHRef);

  // Add the action that shows the source file
  Action := MakeSourceAction(SourceHRef);
  SetLength(SourceActions, MasterFile.SourceFiles.Count);
  SourceActions[MasterFile.SourceFiles.Count-1] := Action;
  ActionManager.AddAction(Action,
    BigMenuSources.Items[BigMenuSources.Items.Count-1]);

  // Show the source file
  OpenTab(FileName);

  MarkModified;
end;

{*
  Retire un fichier source
  @param SourceHRef   HRef du fichier source � retirer
*}
procedure TFormMain.RemoveSourceFile(const SourceHRef: string);
var
  Action: TAction;
  Index: Integer;
begin
  // Find the action and remove it from the list
  Action := nil;
  for Index := 0 to Length(SourceActions)-1 do
  begin
    if SourceActions[Index].Hint = SourceHRef then
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

  // Unlink the source file from the project
  MasterFile.SourceFiles.Delete(MasterFile.SourceFiles.IndexOf(SourceHRef));

  MarkModified;
end;

{*
  Modifie l'�tat Modifi� du fichier
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
  @return Onglet � l'index sp�cifi�
*}
function TFormMain.GetTabs(Index: Integer): TJvTabBarItem;
begin
  Result := TabBarEditors.Tabs[Index];
end;

{*
  Tableau zero-based des �diteurs de source ouverts
  @param Index   Index d'un onglet
  @return �diteur de source de cet onglet, ou nil si ce n'en est pas un
*}
function TFormMain.GetEditors(Index: Integer): ISourceEditor50;
begin
  Result := GetTabEditor(Tabs[Index]);
end;

{*
  Construit un listing des composants
  @param Listing   En sortie : listing des composants
*}
procedure TFormMain.MakeComponentListing(Listing: TStrings);

  function GetCountIn(ListOfPos: TStrings): Integer;
  var
    Line: string;
  begin
    Result := 0;
    for Line in ListOfPos do
      if AnsiStartsStr('    ', Line) then
        Inc(Result);
  end;

var
  CompToListOfPos: TDictionary<TSquareComponent, TStrings>;
  ListOfPos: TStrings;
  I, J, K: Integer;
  Map: TMap;
  Square: TSquare;
  Pos: T3DPoint;
  SquareComp: TSquareComponent;
  Element: TPair<TSquareComponent, TStrings>;
begin
  CompToListOfPos := TDictionary<TSquareComponent, TStrings>.Create;
  try
    for I := 0 to Master.MapCount-1 do
    begin
      Map := Master.Maps[I];
      for J := 0 to Map.LinearMapCount-1 do
      begin
        Square := Map.LinearMap[J];
        Pos := Map.LinearIndexToPos(J);

        for K := 1 to Square.ComponentCount-1 do
        begin
          SquareComp := Square.Components[K];
          if SquareComp = nil then
            Continue;
          if not CompToListOfPos.TryGetValue(SquareComp, ListOfPos) then
          begin
            ListOfPos := TStringList.Create;
            CompToListOfPos.Add(SquareComp, ListOfPos);
            ListOfPos.AddObject('  ' + Map.ID, Map);
          end else
          begin
            if ListOfPos.Objects[ListOfPos.Count-1] <> Map then
              ListOfPos.AddObject('  ' + Map.ID, Map);
          end;
          ListOfPos.AddObject('    ' + Point3DToString(Pos, ', '), Map);
        end;
      end;
    end;

    for Element in CompToListOfPos do
    begin
      Listing.Add(Format('%s -- %s -- %d exemplaire-s',
        [Element.Key.ID, Element.Key.Name, GetCountIn(Element.Value)]));
      Listing.AddStrings(Element.Value);
      Listing.Add('');
    end;
  finally
    for ListOfPos in CompToListOfPos.Values do
      ListOfPos.Free;
    CompToListOfPos.Free;
  end;
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
  Lance l'auto-compilation de tous les fichiers compil�s trouv�s
  @return True en cas de succ�s, False s'il y a eu des erreurs
*}
function TFormMain.AutoCompile: Boolean;
var
  I: Integer;
  Path: TFileName;
begin
  try
    Path := '';
    for I := 1 to ParamCount do
    begin
      if ParamStr(I)[1] <> '-' then
      begin
        Path := ParamStr(I);
        Break;
      end;
    end;

    if Path = '' then
      DoAutoCompile
    else
      DoAutoCompile(Path);

    Result := True;
  except
    on EAbort do
      Result := False;
  end;
end;

{*
  Gestionnaire d'�v�nement OnCreate
  @param Sender   Objet qui a d�clench� l'�v�nement
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

  OpenDialog.InitialDir := ProjectsPath;
  SaveDialog.InitialDir := ProjectsPath;

  OpenSourceFileDialog.InitialDir := JoinPath([LibraryPath, SourcesDir]);
  OpenSourceFileDialog.Filter := SourceFileEditors.FiltersAsText;

  SaveTextFileDialog.Encodings.Clear;
  SaveTextFileDialog.Encodings.AddObject('UTF-8 (recommand�)', FunLabyEncoding);
  SaveTextFileDialog.Encodings.AddObject('ASCII', TEncoding.ASCII);

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
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnCloseQuery
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param CanClose   Indique si la fen�tre peut �tre ferm�e
*}
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseFile;
end;

{*
  Gestionnaire d'�v�nement OnHint de l'application
*}
procedure TFormMain.ApplicationHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Nouveau fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionNewFileExecute(Sender: TObject);
begin
  if CloseFile then
    NewFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Ouvrir un fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnExecute d'une action Ouvrir un fichier r�cent
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionOpenRecentFileExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  FileName := (Sender as TAction).Hint;
  OpenFile(FileName);
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Enregistrer
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnExecute de l'action Enregistrer sous
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionSaveFileAsExecute(Sender: TObject);
begin
  SaveFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Enregistrer tout
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionSaveAllExecute(Sender: TObject);
begin
  SaveAll;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Propri�t�s du fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionFilePropertiesExecute(Sender: TObject);
begin
  if TFormFileProperties.ManageProperties(MasterFile) then
    MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Unit�s utilis�es
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionEditUnitsExecute(Sender: TObject);
begin
  if not SaveAll then
    Exit;

  if TFormEditUnits.EditUnits(MasterFile) then
    ReloadFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Fermer
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement Tout recompiler
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAutoCompileExecute(Sender: TObject);
begin
  if AutoCompile then
    ShowDialog(SAutoCompileSuccessTitle, SAutoCompileSuccess);
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Gestionnaire de projets
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionOpenProjectManagerExecute(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(Dir+'ProjectManager.exe'),
    nil, nil, SW_SHOWNORMAL);
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Quitter
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Ajouter une carte
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnExecute de l'action Retirer une carte
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnExecute de l'action �dition de carte
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement Ouvrir un source existant
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement Ajouter un source existant
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement Ajouter un nouveau fichier source
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAddNewSourceExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  if TFormCreateNewSourceFile.NewSourceFile(MasterFile, FileName) then
    AddSourceFile(FileName);
end;

{*
  Gestionnaire d'�v�nement Retirer le fichier source courant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionRemoveSourceExecute(Sender: TObject);
var
  Tab: TJvTabBarItem;
  SourceHRef: string;
begin
  Tab := TabBarEditors.SelectedTab;
  if not IsEditor(Tab) then
    Exit;

  SourceHRef := FindSourceFile(GetTabEditor(Tab).FileName);

  if SourceHRef = '' then
    CloseTab(Tab)
  else
  begin
    if ShowDialog(SRemoveSourceTitle, SRemoveSource, dtConfirmation,
      dbYesNo, 2) <> drYes then
      Exit;

    if CloseTab(Tab) then
      RemoveSourceFile(SourceHRef);
  end;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Voir toutes les unit�s
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionViewAllSourcesExecute(Sender: TObject);
var
  I: Integer;
  Action: TAction;
begin
{ We want to write:
  for I := 0 to Length(SourceActions)-1 do
    SourceActions[I].Execute;

  but this fails because Execute can result in the action being removed. }

  I := 0;
  while I < Length(SourceActions) do
  begin
    Action := SourceActions[I];
    Action.Execute;

    if (I < Length(SourceActions)) and (SourceActions[I] = Action) then
      Inc(I);
  end;
end;

{*
  Gestionnaire d'�v�nement OnExecute d'une action Voir un source
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionViewSourceExecute(Sender: TObject);
var
  SourceHRef: string;
begin
  SourceHRef := (Sender as TAction).Hint;

  try
    OpenTab(MasterFile.ResolveHRef(SourceHRef, SourcesDir));
  except
    on Error: EInOutError do
    begin
      if ShowDialog(SErrorTitle, Format(SErrorWhileOpeningSourceFile,
        [Error.Message]), dtError, dbYesNo, 2) = drYes then
      begin
        RemoveSourceFile(SourceHRef);
      end;
    end;
  end;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Voir les messages du compilo
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionViewCompilerMessagesExecute(Sender: TObject);
begin
  FormCompilerMessages.Show;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Voir le visualisateur de cartes
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionViewMapViewerExecute(Sender: TObject);
begin
  FormMapViewer.Show;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Tester
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnExecute de l'action Compiler
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionCompileAllExecute(Sender: TObject);
begin
  CompileAll;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Compiler et recharger
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionCompileAndReloadExecute(Sender: TObject);
begin
  if SaveAll and CompileAll(True) then
    ReloadFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Listings des composants
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionComponentListingExecute(Sender: TObject);
var
  Listing: TStrings;
  Encoding: TEncoding;
begin
  if SaveTextFileDialog.Execute then
  begin
    Listing := TStringList.Create;
    try
      MakeComponentListing(Listing);

      Encoding := TEncoding(
        SaveTextFileDialog.Encodings.Objects[SaveTextFileDialog.EncodingIndex]);
      Listing.SaveToFile(SaveTextFileDialog.FileName, Encoding);
    finally
      Listing.Free;
    end;
  end;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Rubriques d'aide
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionHelpTopicsExecute(Sender: TObject);
begin
  RunURL(Application.HelpFile);
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Mise � jour automatique
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionVersionCheckExecute(Sender: TObject);
begin
  EditVersionCheckOptions;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action � propos...
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  ShowFunLabyAbout;
end;

{*
  Gestionnaire d'�v�nement OnTabSelecting de la tab bar des �diteurs
  @param Sender        Objet qui a d�clench� l'�v�nement
  @param Item          �l�ment qui va �tre affich�
  @param AllowSelect   Positionner � False pour emp�cher la s�lection
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
  Gestionnaire d'�v�nement OnTabSelected de la tab bar des �diteurs
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Item     �l�ment qui est ferm�
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
  Gestionnaire d'�v�nement OnTabCloseQuery de la tab bar des �diteurs
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param Item       �l�ment qui va �tre ferm�
  @param CanClose   Positionner � False pour emp�cher la fermeture
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
  Gestionnaire d'�v�nement OnTabClosed de la tab bar des �diteurs
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Item     �l�ment qui est ferm�
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
  Gestionnaire d'�v�nement OnChange des �diteurs de source
*}
procedure TFormMain.EditorStateChange(const Sender: ISourceEditor50);
begin
  FindTab(Sender).Modified := Sender.Modified;
end;

{*
  Gestionnaire d'�v�nement OnShowError de la liste des erreurs
  @param Sender   Object qui a d�clench� l'�v�nement
  @param Error    Erreur � montrer
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
  Gestionnaire d'�v�nement OnDragAccept du DropTarget
  @param Sender   Objet qui a d�clench� l'�v�neement
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
  Gestionnaire d'�v�nement OnDragDrop du DropTarget
  @param Sender   Objet qui a d�clench� l'�v�neement
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

{*
  Gestionnaire d'�v�nement OnCanClose de la bo�te de sauvegarde
  @param Sender     Objet qui a d�clench� l'�v�neement
  @param CanClose   Indique si le nom de fichier est valid�
*}
procedure TFormMain.SaveDialogCanClose(Sender: TObject; var CanClose: Boolean);
var
  FileName, BaseDir: TFileName;
  Folder, Name: string;
begin
  try
    FileName := SaveDialog.FileName;
    BaseDir := IncludeTrailingPathDelimiter(ProjectsPath);

    // The file must be under Projects\
    if not AnsiStartsText(BaseDir, FileName) then
      Abort;

    Delete(FileName, 1, Length(BaseDir));

    // The file must be in a subfolder of Projects
    if not SplitToken(FileName, PathDelim, Folder, Name) then
      Abort;

    // The file must look like <Name>\<Name>.flp
    if not AnsiSameText(Folder+'.'+FunLabyProjectExt, Name) then
      Abort;
  except
    on EAbort do
    begin
      CanClose := False;
      ShowDialog(SCantSaveTitle, SCantSaveBadProjectFileName, dtError);
    end;
  end;
end;

initialization
  LoadPlugins(JoinPath([Dir, EditPluginsDir]));
end.

