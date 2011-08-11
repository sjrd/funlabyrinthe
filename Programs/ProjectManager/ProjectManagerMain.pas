unit ProjectManagerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnPopup, ActnCtrls, ToolWin, ActnMan, ActnMenus, ActnList,
  ImgList, PlatformDefaultStyleActnCtrls, ComCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, ProjectDatabase,
  FilesUtils, ScUtils, ScXML, msxml, StrUtils, AbBase, AbBrowse, AbZBrows,
  AbZipper, SdDialogs, ShellAPI, AbUnzper, JvBaseDlg, IdException,
  JvProgressDialog, LibraryDatabase,
  FunLabyUtils, GitTools, IdURI, JvComponentBase, JvThread, JvThreadDialog,
  JvMTComponents, JvThreadProgressDialog, JvChangeNotify;

resourcestring
  SUpdateRemoteInfoCacheTitle = 'Mise à jour des informations';
  SUpdateRemoteInfoCache = 'Mise à jour des informations sur les projets et '+
    'la bibliothèque depuis Internet';

  SInstallFailedTitle = 'Échec de l''installation';
  SProjectHasNoArchive = 'Impossible d''installer ce projet car il n''y a pas '+
    'd''archive renseignée sur Internet';
  SProjectCompilationFailed =
    'Échec lors de la compilation des sources du projet';

  SConfirmInstallTitle = 'Confirmer l''installation';
  SConfirmInstallErase = 'Vous avez déjà une installation locale de ce '+
    'projet. Cette installation locale sera mise à la corbeille avant de '+
    'continuer. Voulez-vous vraiment mettre à jour ce projet ?';
  SConfirmInstallNoErase = 'Voulez-vous vraiment installer ce projet ?';

  SInstallProjectsTitle = 'Installation des projets';
  SInstallProjects = 'Installation des projets';
  SStatusDownloadingProjectArchive = 'Téléchargement du projet %s';
  SStatusExtractingProjectArchive = 'Installation du projet %s';
  SStatusCompilingProject = 'Compilation du projet %s';

  SUpdateLibraryTitle = 'Mise à jour de la bibliothèque';
  SUpdateLibrary = 'Mise à jour de la bibliothèque';
  SStatusDownloadingFile = 'Téléchargement du fichier %s';
  SStatusCompilingLibrary = 'Compilation des sources de la bibliothèque';

  SDownloadFile = 'Télécharger le fichier sans remplacer le fichier local';
  SInstallFile = 'Installer le fichier';
  SDeleteFile = 'Supprimer le fichier';
  SBackupSuffix = ' (conserver une sauvegarde)';

type
  ECompilationFailedException = class(Exception);

  TFormMain = class(TForm)
    ActionManager: TActionManager;
    Images: TImageList;
    ActionRefresh: TAction;
    ActionExit: TAction;
    ActionInstall: TAction;
    ActionExport: TAction;
    ActionOwnProject: TAction;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionToolBar: TActionToolBar;
    ListViewPopupActionBar: TPopupActionBar;
    MenuInstall: TMenuItem;
    MenuExport: TMenuItem;
    MenuOwnProject: TMenuItem;
    Grabber: TIdHTTP;
    AbZipper: TAbZipper;
    ExportDialog: TSaveDialog;
    AbUnZipper: TAbUnZipper;
    ActionRun: TAction;
    MenuSep1: TMenuItem;
    MenuRun: TMenuItem;
    PageControl: TPageControl;
    TabProjects: TTabSheet;
    ListViewProjects: TListView;
    TabLibrary: TTabSheet;
    ListViewLibrary: TListView;
    ActionApplyLibraryChanges: TAction;
    ThreadInstallProjects: TJvThread;
    ThreadUpdateLibrary: TJvThread;
    ThreadUpdateRemoteInfoCache: TJvThread;
    LocalFilesMonitor: TJvChangeNotify;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionInstallExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionOwnProjectExecute(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ListViewProjectsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewProjectsDblClick(Sender: TObject);
    procedure ActionApplyLibraryChangesExecute(Sender: TObject);
    procedure ListViewLibraryItemChecked(Sender: TObject; Item: TListItem);
    procedure GrabberWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure GrabberWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure GrabberWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure ThreadInstallProjectsExecute(Sender: TObject; Params: Pointer);
    procedure ThreadUpdateLibraryExecute(Sender: TObject; Params: Pointer);
    procedure ThreadUpdateRemoteInfoCacheExecute(Sender: TObject;
      Params: Pointer);
    procedure ThreadDialogPressCancel(CurrentThread: TJvThread);
    procedure LocalFilesMonitorChangeNotify(Sender: TObject; Dir: string;
      Actions: TJvChangeActions);
  private
    { Déclarations privées }
    FThreadDialogOptions: TJvThreadProgressDialogOptions;

    FDatabaseFileName: TFileName;
    FDatabase: TProjectDatabase;
    FProjects: TProjectList;
    FCurrentProject: TProject;

    FLibraryFiles: TLibraryFileList;

    FUseGrabberWorkForProgress: Boolean;

    procedure UpdateActionsEnabled;

    procedure Refresh;

    procedure RefreshProjects;
    procedure LoadInfoFromInternet;
    function LoadProjectInfoDocument: IXMLDOMDocument;
    procedure LoadInfoFromLocal;
    procedure LoadLocalProject(const ProjectSubFile: TFileName);
    procedure UpdateProjectList;

    procedure DoInstallProjects;
    procedure InstallProject(Project: TProject);
    procedure ExtractArchiveFromURL(const ArchiveURL: string;
      const TempArchiveBaseFileName: TFileName; const DestDir: TFileName;
      DeleteDirIfExists: Boolean = True);
    procedure CompileProject(Project: TProject);

    procedure SetCurrentProject(Value: TProject);

    procedure RefreshLibrary;
    procedure LoadLibraryInfoFromInternet;
    procedure LoadLibraryInfoFromLocal;
    procedure ScanLocalLibrary(const BaseDir: TFileName;
      ParentPatterns: TGitIgnorePatterns);
    procedure AddLocalLibraryFile(const FileName: TFileName);
    procedure UpdateLibraryFileList;

    function StatusAndCheckedToAction(Status: TLibraryFileStatus;
      Checked: Boolean): TLibraryFileActionFull;
    function ActionToStr(const Action: TLibraryFileActionFull): string;
    procedure UpdateLibraryItemAction(Item: TListItem);

    procedure DoLibraryUpdate;
    procedure ApplyLibraryFileAction(LibraryFile: TLibraryFile;
      const Action: TLibraryFileActionFull);
    procedure CompileLibrary;

    procedure DownloadFile(const URL: string; const DestFileName: TFileName;
      UseGrabberWorkForProgress: Boolean = True);
    procedure DownloadFileViaTempFile(const URL: string;
      const DestFileName: TFileName; UseGrabberWorkForProgress: Boolean = True);

    property ThreadDialogOptions: TJvThreadProgressDialogOptions
      read FThreadDialogOptions;

    property DatabaseFileName: TFileName read FDatabaseFileName;
    property Projects: TProjectList read FProjects;

    property CurrentProject: TProject
      read FCurrentProject write SetCurrentProject;

    property LibraryFiles: TLibraryFileList read FLibraryFiles;
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  RootURL = 'http://www.funlabyrinthe.com/';

  ProjectInfoURL = RootURL + 'api/projects.xml';

  GroupOwnProjects = 0;
  GroupLocalProjects = 1;
  GroupRemoteProjects = 2;

  LibraryURL = RootURL + 'media/library/';
  LibraryInfoURL = LibraryURL + 'database.txt';

  RemoteInfoCacheDir = 'TempData';

  GroupModifiedLocally = 0;
  GroupObsolete = 1;
  GroupUpToDate = 2;

var
  RemoteInfoCachePath: TFileName;
  ProjectInfoCacheFileName: TFileName;
  LibraryInfoCacheFileName: TFileName;

{*
  Create a temporary file name
  @param BaseFileName      Path and file name wanted
  @param Ext               Extension for the file
  @param AlwaysUseNumber   Set to False if not using a number at all is OK
  @author Toby Allen
*}
function CreateNewFileName(const BaseFileName, Ext: string;
  AlwaysUseNumber: Boolean = True): string;
var
  DocIndex: Integer;
begin
  DocIndex := 1;

  // if number not required and BaseFileName doesn't exist, use that
  if not AlwaysUseNumber and not FileExists(BaseFilename + Ext) then
  begin
    Result := BaseFilename + Ext;
    Exit;
  end;

  while True do
  begin
    Result := BaseFilename + IntToStr(DocIndex) + Ext;
    if not FileExists(Result) then
      Exit;

    Inc(DocIndex)
  end;
end;

{*
  Retourne le dossier temporaire
  @return Dossier temporaire
*}
function GetTempPath: TFileName;
var
  Buffer: array[0..MAX_PATH-1] of Char;
begin
  Windows.GetTempPath(MAX_PATH, Buffer);
  Result := IncludeTrailingPathDelimiter(Buffer);
end;

{*
  Recursively delete an entire directory
  @param DirName   Directory to delete
  @return True on success, False otherwise
*}
function DelTree(const DirName: string): Boolean;
var
  SHFileOpStruct: TSHFileOpStruct;
  DirBuf: array[0..MAX_PATH-1] of Char;
begin
  FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), 0);
  FillChar(DirBuf, SizeOf(DirBuf), 0);

  StrPCopy(DirBuf, DirName);

  with SHFileOpStruct do
  begin
    Wnd := 0;
    pFrom := @DirBuf;
    wFunc := FO_DELETE;
    fFlags := FOF_ALLOWUNDO;
    fFlags := fFlags or FOF_NOCONFIRMATION;
    fFlags := fFlags or FOF_SILENT;
  end;

  Result := SHFileOperation(SHFileOpStruct) = 0;
end;

{*
  Execute a program and wait for its termination
  @param ProgramName   Path to the program executable
  @param Parameters    Parameters for the program
  @return True on success, False otherwise
*}
function ExecProgram(const ProgramName, Parameters: string): Boolean;
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CommandLine: string;
begin
  FillChar(StartInfo, SizeOf(TStartupInfo), 0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), 0);
  StartInfo.cb := SizeOf(TStartupInfo);

  CommandLine := '"'+ProgramName+'" '+Parameters;

  Result := CreateProcess(PChar(ProgramName), PChar(CommandLine), nil, nil,
    False, CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS,
    nil, nil, StartInfo, ProcInfo);

  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

{-----------------}
{ TFormMain class }
{-----------------}

{*
  Met à jour la propriété Enabled des diverses actions
*}
procedure TFormMain.UpdateActionsEnabled;
var
  ActivePage: TTabSheet;
  Some: Boolean;
begin
  ActivePage := PageControl.ActivePage;

  // Actions about projects

  Some := (ActivePage = TabProjects) and (CurrentProject <> nil);

  ActionInstall.Enabled := Some and CurrentProject.IsRemoteDefined;
  ActionExport.Enabled := Some and CurrentProject.IsLocalDefined;
  ActionOwnProject.Enabled := Some and CurrentProject.IsLocalDefined;
  ActionOwnProject.Checked := Some and CurrentProject.OwnProject;
  ActionRun.Enabled := Some and CurrentProject.IsLocalDefined;

  // Actions about the library

  Self.ActionApplyLibraryChanges.Enabled := ActivePage = TabLibrary;
end;

{*
  Rafraîchit toutes les informations sur les projets
*}
procedure TFormMain.Refresh;
begin
  ThreadUpdateRemoteInfoCache.ExecuteWithDialog(nil);

  RefreshProjects;
  RefreshLibrary;
end;

{*
  Rafraîchit toutes les informations sur les projets
*}
procedure TFormMain.RefreshProjects;
begin
  Projects.BeginUpdate;
  LoadInfoFromInternet;
  LoadInfoFromLocal;
  Projects.EndUpdate;

  UpdateProjectList;
end;

{*
  Met à jour les informations locales à partir des informations sur Internet
*}
procedure TFormMain.LoadInfoFromInternet;
var
  Document: IXMLDOMDocument;
  Nodes: IXMLDOMNodeList;
  I: Integer;
  ProjectNode: IXMLDOMNode;
  Project: TProject;
  Remote: TRemoteProject;

  function GetProperty(const NodeName: string): string;
  begin
    Result := ProjectNode.selectSingleNode(NodeName).text;
  end;

begin
  Document := LoadProjectInfoDocument;
  if Document = nil then
    Exit;
  Nodes := Document.documentElement.childNodes;

  for I := 0 to Nodes.length-1 do
  begin
    ProjectNode := Nodes.item[I];

    Project := Projects.GetProject(GetProperty('path'));
    Remote := Project.Remote;
    Remote.Touch;

    Remote.ID := StrToInt(GetProperty('id'));
    Remote.AuthorID := StrToInt(GetProperty('authorid'));

    Remote.Title := GetProperty('title');
    Remote.Description := GetProperty('description');
    Remote.Kind := GetProperty('category');
    Remote.Difficulty := GetProperty('difficulty');
    Remote.Author := GetProperty('author');

    Remote.URL := GetProperty('url');
    Remote.Archive := GetProperty('archive');
    Remote.Version := GetProperty('version');

    Remote.RatingCount := StrToInt(GetProperty('ratingcount'));
    Remote.RatingAvg := StrToFloat(GetProperty('ratingavg'));
  end;
end;

{*
  Charge les informations des projets depuis Internet
  @return Un document XML contenant les informations
*}
function TFormMain.LoadProjectInfoDocument: IXMLDOMDocument;
begin
  try
    Result := LoadXMLDocumentFromFile(ProjectInfoCacheFileName);
  except
    on EInOutError do
      Result := nil;
  end;
end;

{*
  Met à jour les informations locales à partir des projets installés localement
*}
procedure TFormMain.LoadInfoFromLocal;
var
  ProjectSubFile: TFileName;
begin
  IterateProjects(
    procedure(const FileName: TFileName; const SearchRec: TSearchRec)
    begin
      ProjectSubFile := FileName;
      Delete(ProjectSubFile, 1, Length(ProjectsPath));
      if (ProjectSubFile <> '') and (ProjectSubFile[1] = PathDelim) then
        Delete(ProjectSubFile, 1, 1);

      LoadLocalProject(ProjectSubFile);
    end);
end;

{*
  Charge un projet installé localement
  @param ProjectSubFile   Nom du fichier projet sous Projects\
*}
procedure TFormMain.LoadLocalProject(const ProjectSubFile: TFileName);
var
  Document: IXMLDOMDocument;
  RootElement: IXMLDOMElement;

  function ReadProperty(const PropName: string): string;
  var
    Node: IXMLDOMNode;
  begin
    Node := RootElement.selectSingleNode(
      Format('property[@name="%s"]', [PropName]));

    if Node = nil then
      Result := ''
    else
      Result := AnsiReplaceStr(Node.text, #10, sLineBreak);
  end;

var
  Title, Description, Kind, Difficulty, Author, Version: string;
  Project: TProject;
  Local: TLocalProject;
begin
  // Extract information from project file
  try
    Document := LoadXMLDocumentFromFile(
      JoinPath([ProjectsPath, ProjectSubFile]));
    RootElement := Document.documentElement;

    Title := ReadProperty('Title');
    Description := ReadProperty('Description');
    Kind := ReadProperty('Kind');
    Difficulty := ReadProperty('Difficulty');
    Author := ReadProperty('Author');
    Version := ReadProperty('Version');
  except
    on Error: Exception do
      Exit;
  end;

  // Fill local project
  Project := Projects.GetProject(
    ExcludeTrailingPathDelimiter(ExtractFilePath(ProjectSubFile)));
  Local := Project.Local;
  Local.Touch;

  Local.FileName := ProjectSubFile;

  Local.Title := Title;
  Local.Description := Description;
  Local.Kind := Kind;
  Local.Difficulty := Difficulty;
  Local.Author := Author;
  Local.Version := Version;
end;

{*
  Met à jour la GUI à partir des informations locales
*}
procedure TFormMain.UpdateProjectList;
var
  I: Integer;
  Project: TProject;
  Item: TListItem;
begin
  ListViewProjects.Clear;

  for I := 0 to Projects.Count-1 do
  begin
    Project := Projects[I];

    Item := ListViewProjects.Items.Add;
    Item.Data := Pointer(Project);

    Item.Caption := Project.Path;
    Item.SubItems.Add(Project.Title);
    Item.SubItems.Add(Project.LocalVersion);
    Item.SubItems.Add(Project.RemoteVersion);
    Item.SubItems.Add(Project.Kind);
    Item.SubItems.Add(Project.Difficulty);
    Item.SubItems.Add(Project.Author);

    if Project.OwnProject then
      Item.GroupID := GroupOwnProjects
    else if Project.IsLocalDefined then
      Item.GroupID := GroupLocalProjects
    else
      Item.GroupID := GroupRemoteProjects;
  end;
end;

{*
  Installe les projets sélectionnés
*}
procedure TFormMain.DoInstallProjects;
var
  I: Integer;
  Project: TProject;
begin
  for I := 0 to Projects.Count-1 do
  begin
    Project := Projects[I];
    if Project.Action = paInstall then
      InstallProject(Project);
  end;

  ThreadInstallProjects.Synchronize(RefreshProjects);
end;

{*
  Installe un projet
  @param Project   Projet à installer
*}
procedure TFormMain.InstallProject(Project: TProject);
begin
  // Download and install archive
  ExtractArchiveFromURL(Project.Remote.Archive, Project.Path,
    JoinPath([ProjectsPath, Project.Path]));

  // Compile sources in this project
  CompileProject(Project);
end;

{*
  Télécharge et décompresse une archive sur Internet dans un dossier donné
  @param ArchiveURL                URL de l'archive à télécharger
  @param TempArchiveBaseFileName   Nom de base pour l'archive locale
  @param DestDir                   Dossier de destination
  @param DeleteDirIfExists         D'abord supprimer le dossier de destination
*}
procedure TFormMain.ExtractArchiveFromURL(const ArchiveURL: string;
  const TempArchiveBaseFileName: TFileName; const DestDir: TFileName;
  DeleteDirIfExists: Boolean = True);
var
  FileName: TFileName;
begin
  FileName := CreateNewFileName(GetTempPath+TempArchiveBaseFileName,
    ExtractFileExt(ArchiveURL), False);

  // Download archive
  ThreadDialogOptions.InfoText := Format(SStatusDownloadingProjectArchive,
    [TempArchiveBaseFileName]);
  DownloadFile(ArchiveURL, FileName);

  // Delete directory if required
  if DeleteDirIfExists and DirectoryExists(DestDir) then
    DelTree(DestDir);

  // Install archive
  ThreadDialogOptions.InfoText := Format(SStatusExtractingProjectArchive,
    [TempArchiveBaseFileName]);
  ForceDirectories(DestDir);
  AbUnZipper.BaseDirectory := DestDir;
  AbUnZipper.FileName := FileName;
  AbUnZipper.ExtractFiles('*');

  // Delete the archive
  DeleteFile(FileName);
end;

{*
  Compile un projet
  @param Project   Projet à compiler
*}
procedure TFormMain.CompileProject(Project: TProject);
var
  Directory: TFileName;
  ProgramName, Parameters: string;
begin
  // Check that there is something to compile
  Directory := JoinPath([ProjectsPath, Project.Path]);
  if not DirectoryExists(JoinPath([Directory, SourcesDir])) then
    Exit;

  ThreadDialogOptions.InfoText := Format(SStatusCompilingProject,
    [Project.Path]);

  // Spawn the compiler process
  ProgramName := Dir + 'FunLabyEdit.exe';
  Parameters := '-autocompile "'+Directory+'"';

  if not ExecProgram(ProgramName, Parameters) then
    raise ECompilationFailedException.CreateFmt(
      SProjectCompilationFailed, [Project.Path]);
end;

{*
  Change le projet courant
  @param Value   Nouveau projet
*}
procedure TFormMain.SetCurrentProject(Value: TProject);
begin
  if Value <> FCurrentProject then
  begin
    FCurrentProject := Value;
    UpdateActionsEnabled;
  end;
end;

{*
  Met à jour les données sur la bibliothèque
*}
procedure TFormMain.RefreshLibrary;
var
  I: Integer;
begin
  LibraryFiles.Clear;
  LoadLibraryInfoFromInternet;
  LoadLibraryInfoFromLocal;

  for I := LibraryFiles.Count-1 downto 0 do
    if (LibraryFiles[I].LocalHash = '') and LibraryFiles[I].IsDeleted then
      LibraryFiles.Delete(I);

  UpdateLibraryFileList;
end;

{*
  Met à jour les données sur la bibliothèque depuis Internet
*}
procedure TFormMain.LoadLibraryInfoFromInternet;
var
  Lines: TStrings;
  CurrentPath: TFileName;
  CurrentFile: TLibraryFile;
  I: Integer;
  Line: string;
begin
  Lines := TStringList.Create;
  try
    // Load database file
    Lines.LoadFromFile(LibraryInfoCacheFileName, FunLabyEncoding);

    // Parse the file
    CurrentPath := '';
    CurrentFile := nil;
    for I := 0 to Lines.Count-1 do
    begin
      Line := Lines[I];

      if Line[1] = '-' then
      begin
        CurrentPath := Copy(Line, 2, MaxInt);
        CurrentFile := LibraryFiles.GetFile(CurrentPath);
      end else
      begin
        if CurrentFile.RemoteHash = '' then
          CurrentFile.RemoteHash := Line
        else
          CurrentFile.OldHashes.Add(Line);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{*
  Met à jour les données sur la bibliothèque locale
*}
procedure TFormMain.LoadLibraryInfoFromLocal;
begin
  ScanLocalLibrary(LibraryPath, nil);
end;

{*
  Scanne la bibliothèque locale
  @param BaseDir          Dossier de base
  @param ParentPatterns   Motifs de fichiers à ignorer
*}
procedure TFormMain.ScanLocalLibrary(const BaseDir: TFileName;
  ParentPatterns: TGitIgnorePatterns);
var
  Patterns: TGitIgnorePatterns;
begin
  if ParentPatterns = nil then
    Patterns := TGitIgnorePatterns.Create(BaseDir)
  else
    Patterns := TGitIgnorePatterns.Create(ParentPatterns,
      ExtractFileName(BaseDir));
  try
    IterateDir(BaseDir, '*',
      procedure(const FullPath: TFileName; const SearchRec: TSearchRec)
      begin
        if Patterns.IsIgnored(SearchRec.Name) then
          Exit;

        if SearchRec.Attr and faDirectory = 0 then
        begin
          // Add the file
          AddLocalLibraryFile(FullPath);
        end else
        begin
          // Recurse into subdirectory
          if SearchRec.Name <> '.git' then
            ScanLocalLibrary(FullPath, Patterns);
        end;
      end);
  finally
    Patterns.Free;
  end;
end;

{*
  Ajoute un fichier de la bibliothèque locale
  @param FileName   Nom du fichier
*}
procedure TFormMain.AddLocalLibraryFile(const FileName: TFileName);
var
  Path: TFileName;
  LibraryFile: TLibraryFile;
begin
  Path := Copy(FileName, Length(LibraryPath)+2, MaxInt);
  Path := AnsiReplaceStr(Path, PathDelim, HRefDelim);

  LibraryFile := LibraryFiles.GetFile(Path);
  LibraryFile.LocalHash := GitHashBlob(FileName);
end;

{*
  Met à jour la GUI à partir des informations locales sur la bibliothèque
*}
procedure TFormMain.UpdateLibraryFileList;
const
  StatusStr: array[TLibraryFileStatus] of string = (
    'À jour', 'Modifié', 'Ajouté', 'Supprimé',
    'Modifié locallement', 'Ajouté locallement'
  );
  StatusToGroupID: array[TLibraryFileStatus] of Integer = (
    GroupUpToDate, GroupObsolete, GroupObsolete, GroupObsolete,
    GroupModifiedLocally, GroupModifiedLocally
  );
var
  I: Integer;
  LibraryFile: TLibraryFile;
  Item: TListItem;
begin
  ListViewLibrary.Items.BeginUpdate;
  try
    ListViewLibrary.Items.Clear;

    for I := 0 to LibraryFiles.Count-1 do
    begin
      LibraryFile := LibraryFiles[I];
      Item := ListViewLibrary.Items.Add;

      with Item do
      begin
        Caption := LibraryFile.Path;
        SubItems.Add(LibraryFile.LocalHash);
        SubItems.Add(LibraryFile.RemoteHash);
        SubItems.Add(StatusStr[LibraryFile.Status]);
        SubItems.Add('');

        Data := Pointer(LibraryFile);

        GroupID := StatusToGroupID[LibraryFile.Status];
        Checked := GroupID = GroupObsolete;
      end;

      UpdateLibraryItemAction(Item);
    end;
  finally
    ListViewLibrary.Items.EndUpdate;
  end;
end;

{*
  Détermine l'action à effectuer pour un fichier de la bibliothèque
  @param Status    Statut du fichier
  @param Checked   Indique si la case correspondant à ce fichier est cochée
  @return Action à entreprendre pour ce fichier
*}
function TFormMain.StatusAndCheckedToAction(Status: TLibraryFileStatus;
  Checked: Boolean): TLibraryFileActionFull;
begin
  Result.Action := faNone;
  Result.Backup := False;

  if not Checked then
  begin
    if Status = fsLocalUpdated then
      Result.Action := faDownload;
  end else
  begin
    case Status of
      fsRemoteUpdated, fsRemoteAdded: Result.Action := faInstall;
      fsRemoteDeleted: Result.Action := faDelete;
      fsLocalUpdated:
      begin
        Result.Action := faInstall;
        Result.Backup := True;
      end;
      fsLocalAdded:
      begin
        Result.Action := faDelete;
        Result.Backup := True;
      end;
    end;
  end;
end;

{*
  Convertit une action en chaîne de caractère
  @param Action   Action à convertir
  @return Chaîne de caractère décrivant l'action
*}
function TFormMain.ActionToStr(const Action: TLibraryFileActionFull): string;
begin
  case Action.Action of
    faDownload: Result := SDownloadFile;
    faInstall: Result := SInstallFile;
    faDelete: Result := SDeleteFile;
  end;

  if Action.Backup then
    Result := Result + SBackupSuffix;
end;

{*
  Met à jour l'action d'un élément de la bibliothèque
  @param Item   Élément à mettre à jour
*}
procedure TFormMain.UpdateLibraryItemAction(Item: TListItem);
var
  LibraryFile: TLibraryFile;
begin
  LibraryFile := TLibraryFile(Item.Data);
  LibraryFile.Action := StatusAndCheckedToAction(
    LibraryFile.Status, Item.Checked);
  Item.SubItems[3] := ActionToStr(LibraryFile.Action);
end;

{*
  Applique les mises à jour de la bibliothèque
*}
procedure TFormMain.DoLibraryUpdate;
var
  DoneSomething: Boolean;
  I, Count: Integer;
  LibraryFile: TLibraryFile;
  Action: TLibraryFileActionFull;
begin
  DoneSomething := False;

  Count := 0;
  for I := 0 to LibraryFiles.Count-1 do
    if LibraryFiles[I].Action.Action <> faNone then
      Inc(Count);

  if Count = 0 then
    Exit;

  ThreadDialogOptions.MaxProgress := Count;
  ThreadDialogOptions.ProgressPosition := 0;

  try
    for I := 0 to LibraryFiles.Count-1 do
    begin
      LibraryFile := LibraryFiles[I];
      Action := LibraryFile.Action;

      if Action.Action <> faNone then
      begin
        ApplyLibraryFileAction(LibraryFile, Action);
        ThreadDialogOptions.ProgressPosition :=
          ThreadDialogOptions.ProgressPosition+1;
        DoneSomething := True;
      end;
    end;
  finally
    ThreadDialogOptions.ProgressPosition := Count;

    ThreadUpdateLibrary.Synchronize(RefreshLibrary);

    if DoneSomething then
      CompileLibrary;
  end;
end;

{*
  Compile la bibliothèque
*}
procedure TFormMain.CompileLibrary;
begin
  ThreadDialogOptions.InfoText := SStatusCompilingLibrary;

  ExecProgram(Dir+'FunLabyEdit.exe', '-autocompile');
end;

procedure TFormMain.DownloadFile(const URL: string;
  const DestFileName: TFileName; UseGrabberWorkForProgress: Boolean = True);
var
  Stream: TStream;
  OldUseGrabberWorkForProgress: Boolean;
begin
  OldUseGrabberWorkForProgress := FUseGrabberWorkForProgress;
  Stream := TFileStream.Create(DestFileName, fmCreate);
  try
    if UseGrabberWorkForProgress then
      FUseGrabberWorkForProgress := True;
    ThreadDialogOptions.ShowCancelButton := True;

    Grabber.Get(TIdURI.URLEncode(URL), Stream);
  finally
    ThreadDialogOptions.ShowCancelButton := False;
    FUseGrabberWorkForProgress := OldUseGrabberWorkForProgress;
    Stream.Free;
  end;
end;

procedure TFormMain.DownloadFileViaTempFile(const URL: string;
  const DestFileName: TFileName; UseGrabberWorkForProgress: Boolean = True);
var
  TempFileName: TFileName;
begin
  TempFileName := DestFileName + '.part';
  try
    DownloadFile(URL, TempFileName, UseGrabberWorkForProgress);
    DeleteFile(DestFileName);
    MoveFile(PChar(TempFileName), PChar(DestFileName));
  finally
    if FileExists(TempFileName) then
      DeleteFile(TempFileName);
  end;
end;

{*
  Applique la mise à jour d'un fichier de la bibliothèque
  @param LibraryFile   Fichier de la bibliothèque à mettre à jour
  @param Action        Action à entreprendre
*}
procedure TFormMain.ApplyLibraryFileAction(LibraryFile: TLibraryFile;
  const Action: TLibraryFileActionFull);
const
  BackupExt = '~';
var
  FileName, BackupFileName: TFileName;
  URL: string;
begin
  if Action.Action = faNone then
    Exit;

  ThreadDialogOptions.InfoText := Format(
    SStatusDownloadingFile, [LibraryFile.Path]);

  FileName := JoinPath([LibraryPath,
    AnsiReplaceStr(LibraryFile.Path, HRefDelim, PathDelim)]);
  BackupFileName := FileName + BackupExt;
  URL := LibraryURL + LibraryFile.Path;

  if Action.Backup and FileExists(FileName) then
  begin
    if FileExists(BackupFileName) then
      DelTree(BackupFileName);
    MoveFile(PChar(FileName), PChar(BackupFileName));
  end;

  if (Action.Action in [faInstall, faDelete]) and FileExists(FileName) then
    DelTree(FileName);

  if Action.Action in [faDownload, faInstall] then
  begin
    ForceDirectories(ExtractFilePath(FileName));
    DownloadFileViaTempFile(URL,
      IIF(Action.Action = faDownload, BackupFileName, FileName), False);
  end;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
var
  ThreadDialog: TJvThreadProgressDialog;
begin
  ThreadDialog := TJvThreadProgressDialog.Create(Self);
  ThreadDialog.OnPressCancel := ThreadDialogPressCancel;
  FThreadDialogOptions := ThreadDialog.DialogOptions;

  ThreadUpdateRemoteInfoCache.ThreadDialog := ThreadDialog;
  ThreadInstallProjects.ThreadDialog := ThreadDialog;
  ThreadUpdateLibrary.ThreadDialog := ThreadDialog;

  ThreadDialogOptions.CancelButtonCaption := 'Annuler';
  ThreadDialogOptions.ShowDialog := True;
  ThreadDialogOptions.ShowProgressBar := True;
  ThreadDialogOptions.ShowCancelButton := False;

  FDatabaseFileName := JoinPath([ProjectsPath, 'Projects.xml']);
  FDatabase := TProjectDatabase.Create;
  FProjects := FDatabase.Projects;

  FLibraryFiles := TLibraryFileList.Create;

  if FileExists(DatabaseFileName) then
    FDatabase.LoadFromFile(DatabaseFileName);
end;

{*
  Gestionnaire d'événement OnDestroy de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FDatabase.SaveToFile(DatabaseFileName);

  FLibraryFiles.Free;
  FDatabase.Free;
end;

{*
  Gestionnaire d'événement OnActivate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormActivate(Sender: TObject);
begin
  OnActivate := nil;

  Refresh;

  LocalFilesMonitor.Notifications[0].Directory := ProjectsPath;
  LocalFilesMonitor.Notifications[1].Directory := LibraryPath;
  LocalFilesMonitor.Active := True;
end;

procedure TFormMain.ActionRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionInstallExecute(Sender: TObject);
var
  Project: TProject;
  Msg: string;
  I: Integer;
begin
  Project := CurrentProject;
  Assert(Project.IsRemoteDefined);

  if Project.Remote.Archive = '' then
  begin
    ShowDialog(SInstallFailedTitle, SProjectHasNoArchive, dtError);
    Exit;
  end;

  // Confirm install
  if DirectoryExists(JoinPath([ProjectsPath, Project.Path])) then
    Msg := SConfirmInstallErase
  else
    Msg := SConfirmInstallNoErase;

  if ShowDialog(SConfirmInstallTitle, Msg,
    dtConfirmation, dbOKCancel) <> drOK then
    Exit;

  // Prepare data for thread
  for I := 0 to Projects.Count-1 do
  begin
    if Projects[I] = Project then
      Projects[I].Action := paInstall
    else
      Projects[I].Action := paNone;
  end;

  // Install the project
  ThreadDialogOptions.Caption := SInstallProjectsTitle;
  ThreadDialogOptions.InfoText := SInstallProjects;
  ThreadInstallProjects.ExecuteWithDialog(nil);
end;

procedure TFormMain.ActionExportExecute(Sender: TObject);
var
  Project: TProject;
begin
  Project := CurrentProject;
  Assert(Project.IsLocalDefined);

  ExportDialog.FileName := Project.Path + '.zip';
  if not ExportDialog.Execute then
    Exit;
  if FileExists(ExportDialog.FileName) then
    DeleteFile(ExportDialog.FileName);

  AbZipper.BaseDirectory := JoinPath([ProjectsPath, Project.Path]);
  AbZipper.FileName := ExportDialog.FileName;

  IterateDir(AbZipper.BaseDirectory, '*',
    procedure(const FileName: TFileName; const SearchRec: TSearchRec)
    begin
      if not AnsiMatchText(SearchRec.Name, ['.git', 'Units', 'Screenshots']) then
      begin
        if SearchRec.Attr and faDirectory = 0 then
          AbZipper.AddFiles(SearchRec.Name, faAnyFile)
        else
          AbZipper.AddFiles(JoinPath([SearchRec.Name, '*']), faAnyFile);
      end;
    end);

  AbZipper.Save;
  AbZipper.CloseArchive;
end;

procedure TFormMain.ActionRunExecute(Sender: TObject);
var
  Project: TProject;
  FileName: TFileName;
begin
  Project := CurrentProject;
  Assert(Project.IsLocalDefined);

  FileName := JoinPath([ProjectsPath, Project.Local.FileName]);

  ShellExecute(0, 'open', PChar(Dir+'FunLaby.exe'),
    PChar('"'+FileName+'"'), nil, SW_SHOWNORMAL);
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin
  UpdateActionsEnabled;
end;

procedure TFormMain.ActionOwnProjectExecute(Sender: TObject);
var
  Project: TProject;
begin
  Project := CurrentProject;
  Assert(Project.IsLocalDefined);

  Project.Local.OwnProject := not Project.Local.OwnProject;

  UpdateProjectList;
end;

{*
  Gestionnaire d'événement OnSelectItem de la liste des projets
  @param Sender     Objet qui a déclenché l'événement
  @param Item       Item qui a été sélectionné
  @param Selected   False ssi l'item est désélectionné
*}
procedure TFormMain.ListViewProjectsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    CurrentProject := TProject(Item.Data)
  else
    CurrentProject := nil;
end;

procedure TFormMain.ListViewProjectsDblClick(Sender: TObject);
begin
  if CurrentProject <> nil then
    ActionInstall.Execute;
end;

procedure TFormMain.ActionApplyLibraryChangesExecute(Sender: TObject);
begin
  ThreadDialogOptions.Caption := SUpdateLibraryTitle;
  ThreadDialogOptions.InfoText := SUpdateLibrary;
  ThreadUpdateLibrary.ExecuteWithDialog(nil);
end;

procedure TFormMain.ListViewLibraryItemChecked(Sender: TObject;
  Item: TListItem);
begin
  if Item.Data <> nil then
    UpdateLibraryItemAction(Item);
end;

procedure TFormMain.GrabberWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  if FUseGrabberWorkForProgress then
  begin
    ThreadDialogOptions.MinProgress := 0;
    ThreadDialogOptions.MaxProgress := AWorkCountMax;
    ThreadDialogOptions.ProgressPosition := 0;
  end;
end;

procedure TFormMain.GrabberWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if FUseGrabberWorkForProgress then
    ThreadDialogOptions.ProgressPosition := AWorkCount;
end;

procedure TFormMain.GrabberWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if FUseGrabberWorkForProgress then
    ThreadDialogOptions.ProgressPosition := ThreadDialogOptions.MaxProgress;
end;

procedure TFormMain.ThreadInstallProjectsExecute(Sender: TObject;
  Params: Pointer);
begin
  DoInstallProjects;
end;

procedure TFormMain.ThreadUpdateLibraryExecute(Sender: TObject;
  Params: Pointer);
begin
  DoLibraryUpdate;
end;

procedure TFormMain.ThreadUpdateRemoteInfoCacheExecute(Sender: TObject;
  Params: Pointer);
begin
  ThreadDialogOptions.Caption := SUpdateRemoteInfoCacheTitle;
  ThreadDialogOptions.InfoText := SUpdateRemoteInfoCache;
  ThreadDialogOptions.ProgressPosition := 0;

  DownloadFileViaTempFile(ProjectInfoURL, ProjectInfoCacheFileName);
  DownloadFileViaTempFile(LibraryInfoURL, LibraryInfoCacheFileName);
end;

procedure TFormMain.ThreadDialogPressCancel(CurrentThread: TJvThread);
begin
  Grabber.Disconnect;
end;

procedure TFormMain.LocalFilesMonitorChangeNotify(Sender: TObject; Dir: string;
  Actions: TJvChangeActions);
begin
  if Dir = ProjectsPath then
  begin
    if not ThreadInstallProjects.OneThreadIsRunning then
      RefreshProjects;
  end else if Dir = LibraryPath then
  begin
    if not ThreadUpdateLibrary.OneThreadIsRunning then
      RefreshLibrary;
  end;
end;

initialization
  DecimalSeparator := '.';

  RemoteInfoCachePath := JoinPath([ScUtils.Dir, RemoteInfoCacheDir]);
  ProjectInfoCacheFileName := JoinPath([RemoteInfoCachePath, 'Projects.xml']);
  LibraryInfoCacheFileName := JoinPath([RemoteInfoCachePath, 'Library.txt']);
end.

