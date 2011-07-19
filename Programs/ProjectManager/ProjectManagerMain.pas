unit ProjectManagerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnPopup, ActnCtrls, ToolWin, ActnMan, ActnMenus, ActnList,
  ImgList, PlatformDefaultStyleActnCtrls, ComCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, ProjectDatabase,
  FilesUtils, ScXML, msxml, StrUtils, AbBase, AbBrowse, AbZBrows, AbZipper;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionInstallExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionOwnProjectExecute(Sender: TObject);
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

{-----------------}
{ TFormMain class }
{-----------------}

{*
  Rafraîchit toutes les informations sur les projets
*}
procedure TFormMain.Refresh;
begin
  LoadInfoFromInternet;
  LoadInfoFromLocal;
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
begin
  // TODO
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

