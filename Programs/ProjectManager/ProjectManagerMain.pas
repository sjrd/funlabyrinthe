unit ProjectManagerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnPopup, ActnCtrls, ToolWin, ActnMan, ActnMenus, ActnList,
  ImgList, PlatformDefaultStyleActnCtrls, ComCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, ProjectDatabase,
  FilesUtils, ScXML, msxml;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ListViewProjectsClick(Sender: TObject);
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
  I, ID: Integer;
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
    ID := StrToInt(GetProperty('id'));

    Project := Projects.GetProjectByRemoteID(ID);
    Remote := Project.Remote;

    Remote.AuthorID := StrToInt(GetProperty('authorid'));
    Remote.Path := GetProperty('path');

    Remote.Title := GetProperty('title');
    Remote.Description := GetProperty('description');
    Remote.Kind := GetProperty('category');
    Remote.Difficulty := GetProperty('difficulty');
    Remote.Author := GetProperty('author');

    Remote.URL := GetProperty('url');
    Remote.Archive := GetProperty('archive');

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
begin

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

    Item.Caption := Project.FileName;
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

  ActionInstall.Enabled := Some;
  ActionExport.Enabled := Some;
  ActionOwnProject.Enabled := Some;
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

{*
  Gestionnaire d'événement OnClick de la liste des projets
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ListViewProjectsClick(Sender: TObject);
var
  Selected: TListItem;
begin
  Selected := ListViewProjects.Selected;

  if Selected = nil then
    CurrentProject := nil
  else
    CurrentProject := TProject(Selected.Data);
end;

initialization
  DecimalSeparator := '.';
end.

