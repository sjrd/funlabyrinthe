{*
  Unit� principale de FunLabyEdit.exe
  Cette unit� contient la fiche principale de FunLabyEdit.exe.
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
    BackgroundTasks: TScTaskQueue; /// T�ches d'arri�re-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// T�che de chargement de la racine de base

    BigMenuMaps: TActionClient;    /// Menu des cartes
    BigMenuSources: TActionClient; /// Menu des unit�s

    TabMapEditor: TJvTabBarItem;     /// Onglet d'�dition des cartes
    FrameMapEditor: TFrameMapEditor; /// Cadre d'�dition des cartes

    SourceActions: array of TAction; /// Liste des actions du menu Sources li�s

    FormCompilerMessages: TFormCompilerMessages; /// Messages du compilateur
    FormMapViewer: TFormMapViewer;               /// Visualisateur de cartes

    FModified: Boolean;      /// Indique si le fichier a �t� modifi�
    SepiRoot: TSepiRoot;     /// Racine Sepi du fichier en cours
    MasterFile: TMasterFile; /// Fichier ma�tre
    Master: TMaster;         /// Ma�tre FunLabyrinthe

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
  FunLabyBaseHRef = 'FunLabyBase.bpl'; /// HRef de l'unit� FunLabyBase
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
  Charge le MasterFile cr��
  Charge le MasterFile cr�� dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
begin
  // Un fichier nouvellement charg� n'est modifi� que s'il vient d'�tre cr��
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
  Ajoute un �diteur de source � l'interface visuelle et l'affiche
  @param Editor   �diteur � ajouter
*}
procedure TFormMain.OpenTab(SourceFile: TSourceFile);
var
  Editor: ISourceEditor50;
  EditorUsingOTA: ISourceEditorUsingOTA50;
  EditorControl: TControl;
  Tab: TJvTabBarItem;
begin
  // Cr�er l'�diteur
  Editor := SourceFileEditors.CreateEditor(SourceFile);

  // Set OTA main form
  if Supports(Editor, ISourceEditorUsingOTA50, EditorUsingOTA) then
    EditorUsingOTA.SetFunLabyEditMainForm(Self);

  // Configurer le contr�le d'�dition
  EditorControl := Editor.Control;
  EditorControl.Align := alClient;
  EditorControl.Visible := False;
  EditorControl.Parent := PanelEditors;

  // Enregistrer les �v�nements de l'�diteur
  Editor.OnStateChange := EditorStateChange;

  // Cr�er un nouvel onglet pour l'�diteur et l'afficher
  Tab := TJvTabBarItem(TabBarEditors.Tabs.Add);
  Tab.Caption := ExtractFileName(Editor.SourceFile.FileName);
  Tab.Data := TObject(Pointer(Editor));
  Tab.Tag := SourceEditorTag;
  Tab.Modified := Editor.Modified;
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
  @param FileName   Nom du fichier ma�tre � charger
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
  @return True si l'enregistrement a bien �t� effectu�, False sinon
*}
function TFormMain.SaveFile(FileName: TFileName = ''): Boolean;
var
  DirName: TFileName;
  I: Integer;
begin
  Result := False;

  // V�rifier que tous les joueurs sont plac�s
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
      Exit;
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
  BackgroundDiscard(SepiRoot);
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

  Result := nil;
end;

{*
  Compile tous les sources ouverts
  @return True si les sources ont bien �t� compil�s, False sinon
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
  Marque le fichier comme modifi�
*}
procedure TFormMain.MarkModified;
begin
  Modified := True;
end;

{*
  Cr�e une action Voir un source pour un fichier source donn�
  @param SourceFile   Fichier source � voir
  @return L'action cr��e
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
  Cr�e une entr�e dans le menu des sources pour chaque fichier source
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
  Supprime toutes les actions Voir un fichier source et leurs menus associ�s
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
  // V�rifier que ce fichier source n'est pas d�j� attach� au projet
  for I := 0 to MasterFile.SourceFiles.Count-1 do
  begin
    if SameFileName(MasterFile.SourceFiles[I].FileName, FileName) then
    begin
      ShowDialog(SDuplicateSourceTitle, SDuplicateSource, dtError);
      Exit;
    end;
  end;

  // Cr�er le fichier source
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
  @param SourceFile   Fichier source � retirer
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
  Gestionnaire d'�v�nement OnCreate
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  if not CloseFile then
    Exit;

  if OpenDialog.Execute then
  begin
    SaveDialog.FileName := OpenDialog.FileName;
    OpenFile(OpenDialog.FileName);
  end;
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
  Gestionnaire d'�v�nement OnExecute de l'action Tester
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionTestExecute(Sender: TObject);
begin
  {don't localize}
  if (not Modified) or SaveFile(MasterFile.FileName) then
    ShellExecute(0, 'open', PChar(Dir+'FunLaby.exe'),
      PChar('"'+MasterFile.FileName+'"'), nil, SW_SHOWNORMAL);
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
  Gestionnaire d'�v�nement OnExecute de l'action Modifier la taille de zone
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement Ajouter un source existant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAddExistingSourceExecute(Sender: TObject);
begin
  if OpenSourceFileDialog.Execute then
    AddSourceFile(OpenSourceFileDialog.FileName);
end;

{*
  Gestionnaire d'�v�nement Ajouter un nouveau fichier source
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAddNewSourceExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  if TFormCreateNewSourceFile.NewSourceFile(FileName) then
    AddSourceFile(FileName);
end;

{*
  Gestionnaire d'�v�nement Retirer le fichier source courant
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnExecute de l'action Compiler
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionCompileAllExecute(Sender: TObject);
begin
  CompileAll;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Voir toutes les unit�s
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionViewAllSourcesExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(SourceActions)-1 do
    SourceActions[I].Execute;
end;

{*
  Gestionnaire d'�v�nement OnExecute d'une action Voir un source
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionViewSourceExecute(Sender: TObject);
var
  SourceFile: TSourceFile;
  Tab: TJvTabBarItem;
begin
  SourceFile := TSourceFile((Sender as TAction).Tag);

  // Si un �diteur est d�j� ouvert pour ce fichier, le mettre en avant-plan
  // Sinon, ouvrir un nouvel onglet
  Tab := FindTab(SourceFile);
  if Tab <> nil then
    Tab.Selected := True
  else
    OpenTab(SourceFile);
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
  Gestionnaire d'�v�nement OnExecute de l'action Rubriques d'aide
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionHelpTopicsExecute(Sender: TObject);
begin
// Don't delete this comment
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
  Gestionnaire d'�v�nement OnTabClosing de la tab bar des �diteurs
  @param Sender       Objet qui a d�clench� l'�v�nement
  @param Item         �l�ment qui va �tre ferm�
  @param AllowClose   Positionner � False pour emp�cher la fermeture
*}
procedure TFormMain.TabBarEditorsTabClosing(Sender: TObject;
  Item: TJvTabBarItem; var AllowClose: Boolean);
begin
  if Item.Tag = MapEditorTag then
    AllowClose := False;
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
  if IsEditor(Item) then
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

