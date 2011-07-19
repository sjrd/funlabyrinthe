unit ProjectManagerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnPopup, ActnCtrls, ToolWin, ActnMan, ActnMenus, ActnList,
  ImgList, PlatformDefaultStyleActnCtrls, ComCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, ProjectDatabase,
  FilesUtils, ScUtils, ScXML, msxml, StrUtils, AbBase, AbBrowse, AbZBrows,
  AbZipper, SdDialogs, ShellAPI, AbUnzper;

resourcestring
  SInstallFailedTitle = 'Échec de l''installation';
  SProjectHasNoArchive = 'Impossible d''installer ce projet car il n''y a pas '+
    'd''archive renseignée sur Internet';
  SCompilationFailed = 'Échec lors de la compilation des sources du projet';

  SConfirmInstallTitle = 'Confirmer l''installation';
  SConfirmInstallErase = 'Vous avez déjà une installation locale de ce '+
    'projet. Cette installation locale sera mise à la corbeille avant de '+
    'continuer. Voulez-vous vraiment mettre à jour ce projet ?';
  SConfirmInstallNoErase = 'Voulez-vous vraiment installer ce projet ?';

  SInstallDoneTitle = 'Installation terminée';
  SInstallDone = 'L''installation a été faite avec succès';

type
  TFormMain = class(TForm)
    ListViewProjects: TListView;
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
    ProjectInfoGrabber: TIdHTTP;
    ProjectArchiveGrabber: TIdHTTP;
    AbZipper: TAbZipper;
    ExportDialog: TSaveDialog;
    AbUnZipper: TAbUnZipper;
    ActionRun: TAction;
    MenuSep1: TMenuItem;
    MenuRun: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionInstallExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionOwnProjectExecute(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ListViewProjectsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Déclarations privées }
    FDatabaseFileName: TFileName;
    FDatabase: TProjectDatabase;
    FProjects: TProjectList;
    FCurrentProject: TProject;

    procedure Refresh;
    procedure LoadInfoFromInternet;
    function LoadProjectInfoDocument: IXMLDOMDocument;
    procedure LoadInfoFromLocal;
    procedure LoadLocalProject(const ProjectSubFile: TFileName);
    procedure UpdateProjectList;

    procedure InstallProject(Project: TProject);
    function CompileProject(Project: TProject): Boolean;

    procedure SetCurrentProject(Value: TProject);

    property DatabaseFileName: TFileName read FDatabaseFileName;
    property Projects: TProjectList read FProjects;

    property CurrentProject: TProject
      read FCurrentProject write SetCurrentProject;
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  APIURL = 'http://www.funlabyrinthe.com/api/';
  ProjectInfoURL = APIURL + 'projects.xml';

  GroupOwnProjects = 0;
  GroupLocalProjects = 1;
  GroupRemoteProjects = 2;

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

{-----------------}
{ TFormMain class }
{-----------------}

{*
  Rafraîchit toutes les informations sur les projets
*}
procedure TFormMain.Refresh;
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
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ProjectInfoGrabber.Get(ProjectInfoURL, Stream);
    Stream.Seek(0, soFromBeginning);
    Result := LoadXMLDocumentFromStream(Stream);
  finally
    Stream.Free;
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
  Installe un projet
  @param Project   Projet à installer
*}
procedure TFormMain.InstallProject(Project: TProject);
var
  FileName: TFileName;
  Contents: TFileStream;
  Directory: TFileName;
begin
  FileName := CreateNewFileName(GetTempPath+Project.Path, '.zip', False);

  // Download archive
  Contents := TFileStream.Create(FileName, fmCreate);
  try
    ProjectArchiveGrabber.Get(Project.Remote.Archive, Contents);
  finally
    Contents.Free;
  end;

  // Remove any existing project
  Directory := JoinPath([ProjectsPath, Project.Path]);
  if DirectoryExists(Directory) then
    DelTree(Directory);

  // Install archive
  ForceDirectories(Directory);
  AbUnZipper.BaseDirectory := Directory;
  AbUnZipper.FileName := FileName;
  AbUnZipper.ExtractFiles('*');

  // Delete the archive
  DeleteFile(FileName);

  // Compile sources in this project
  if CompileProject(Project) then
    ShowDialog(SInstallDoneTitle, SInstallDone);

  Refresh;
end;

{*
  Installe un projet
  @param Project   Projet à installer
*}
function TFormMain.CompileProject(Project: TProject): Boolean;
var
  Directory: TFileName;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  ProgramName, CommandLine: string;
  CreateOK: Boolean;
begin
  Result := False;

  // Check that there is something to compile
  Directory := JoinPath([ProjectsPath, Project.Path]);
  if not DirectoryExists(JoinPath([Directory, SourcesDir])) then
  begin
    Result := True;
    Exit;
  end;

  // Spawn the compiler process
  FillChar(StartInfo, SizeOf(TStartupInfo), 0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), 0);
  StartInfo.cb := SizeOf(TStartupInfo);

  ProgramName := Dir + 'FunLabyEdit.exe';
  CommandLine := '"'+ProgramName+'" -autocompile "'+Directory+'"';

  CreateOK := CreateProcess(PChar(ProgramName), PChar(CommandLine), nil, nil,
    False, CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS,
    nil, nil, StartInfo, ProcInfo);

  if not CreateOK then
  begin
    ShowDialog(SInstallFailedTitle, SCompilationFailed, dtError);
    Exit;
  end;

  // Wait for its termination
  WaitForSingleObject(ProcInfo.hProcess, INFINITE);

  Result := True;
end;

procedure TFormMain.SetCurrentProject(Value: TProject);
var
  Some: Boolean;
begin
  if Value = FCurrentProject then
    Exit;

  FCurrentProject := Value;
  Some := Value <> nil;

  ActionInstall.Enabled := Some and Value.IsRemoteDefined;
  ActionExport.Enabled := Some and Value.IsLocalDefined;
  ActionOwnProject.Enabled := Some and Value.IsLocalDefined;
  ActionOwnProject.Checked := Some and Value.OwnProject;
  ActionRun.Enabled := Some and Value.IsLocalDefined;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  FDatabaseFileName := JoinPath([ProjectsPath, 'Projects.xml']);
  FDatabase := TProjectDatabase.Create;
  FProjects := FDatabase.Projects;

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

  FDatabase.Free;
end;

{*
  Gestionnaire d'événement OnShow de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormShow(Sender: TObject);
begin
  Refresh;
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

  // Install the project
  InstallProject(Project);
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

initialization
  DecimalSeparator := '.';
end.

