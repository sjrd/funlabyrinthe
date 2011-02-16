unit NewProject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvEdit, JvValidateEdit, ScUtils, ScXML,
  msxml, FilesUtils, SdDialogs, FunLabyEditConsts;

type
  TFormNewProject = class(TForm)
    Label1: TLabel;
    EditTitle: TEdit;
    LabelFileName: TLabel;
    EditFileName: TJvValidateEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EditTitleChange(Sender: TObject);
    procedure EditFileNameChange(Sender: TObject);
  private
    procedure CreateProject(const FileName: TFileName; const Title: string);
    procedure CopyProjectTemplateTo(const FileName: TFileName);
    procedure PatchProjectTitle(const FileName: TFileName; const Title: string);

    function InternalNewProject: TFileName;
  public
    class function NewProject: TFileName;
  end;

var
  FormNewProject: TFormNewProject;

implementation

{$R *.dfm}

uses
  ShellApi;

const {don't localize}
  /// Caractères valides pour un nom de fichier
  ValidChars = [
    '0'..'9', 'A'..'Z', 'a'..'z', ' ', '''', ',', '-', '_'
  ];

  /// Nom du dossier des templates
  TemplatesDir = 'Templates';

  /// Nom du template NewProject
  NewProjectTemplate = 'NewProject';

{-----------------}
{ Global routines }
{-----------------}

{*
  Construit un nom de fichier par défaut pour un titre donné
  @param Title   Titre du projet
  @return Nom de fichier suggéré pour ce projet
*}
function MakeFileNameFromTitle(const Title: string): string;
var
  FoldedTitle: string;
  I, Len: Integer;
begin
  // Fold the string to separate diacritics from their letter
  Len := FoldString(MAP_COMPOSITE, PChar(Title), -1, nil, 0) - 1;
  SetLength(FoldedTitle, Len);
  FoldString(MAP_COMPOSITE, PChar(Title), -1, PChar(FoldedTitle), Len+1);

  // Keep only valid characters
  SetLength(Result, Len);
  Len := 0;

  for I := 1 to Length(FoldedTitle) do
  begin
    if CharInSet(FoldedTitle[I], ValidChars) then
    begin
      Inc(Len);
      Result[Len] := FoldedTitle[I];
    end;
  end;

  SetLength(Result, Len);
  Result := Trim(Result);
end;

{-----------------------}
{ TFormNewProject class }
{-----------------------}

{*
  Crée un nouveau projet
  @param FileName   Nom du fichier projet à créer
  @param Title      Titre du projet
*}
procedure TFormNewProject.CreateProject(const FileName: TFileName;
  const Title: string);
begin
  CopyProjectTemplateTo(FileName);
  PatchProjectTitle(FileName, Title);
end;

{*
  Copie le template de nouveau projet vers le nouveau projet
  @param FileName   Nom du fichier projet à créer
*}
procedure TFormNewProject.CopyProjectTemplateTo(const FileName: TFileName);
var
  ProjectPath: TFileName;
  FileOp: TSHFileOpStruct;
begin
  ProjectPath := ExtractFilePath(FileName);

  // Copy the template project

  ZeroMemory(@FileOp, SizeOf(FileOp));

  with FileOp do
  begin
    wFunc := FO_COPY;
    fFlags := FOF_SILENT or FOF_NOCOPYSECURITYATTRIBS;
    pFrom := PChar(JoinPath([Dir, TemplatesDir, NewProjectTemplate]) + #0);
    pTo := PChar(ProjectPath + #0);
  end;

  if ShFileOperation(FileOp) <> 0 then
    Abort;

  // Rename the project file

  RenameFile(JoinPath([ProjectPath, NewProjectTemplate+'.flp']), FileName);
end;

{*
  Patche le titre d'un projet
  @param FileName   Nom du fichier projet à patcher
  @param Title      Titre du projet
*}
procedure TFormNewProject.PatchProjectTitle(const FileName: TFileName;
  const Title: string);
const
  TitleNodeQuery = '/funlabyrinthe/property[@name="Title"]';
var
  Document: IXMLDOMDocument;
  TitleNode: IXMLDOMNode;
begin
  // Load
  Document := LoadXMLDocumentFromFile(FileName);

  // Patch
  TitleNode := Document.selectSingleNode(TitleNodeQuery);
  TitleNode.text := Title;

  // Save back
  SaveXMLDocumentToFile(Document, FileName);
end;

{*
  Crée un nouveau projet
  @return Nom du fichier projet créé, ou '' en cas d'annulation
*}
function TFormNewProject.InternalNewProject: TFileName;
var
  OK: Boolean;
begin
  repeat
    if ShowModal <> mrOK then
    begin
      Result := '';
      Exit;
    end;

    OK := not DirectoryExists(JoinPath([ProjectsPath, EditFileName.Text]));

    if not OK then
      ShowDialog(SErrorTitle, SProjectAlreadyExists, dtError);
  until OK;

  Result := JoinPath([ProjectsPath, EditFileName.Text,
    EditFileName.Text + '.flp']);

  CreateProject(Result, EditTitle.Text);
end;

{*
  Crée un nouveau projet
  @return Nom du fichier projet créé, ou '' en cas d'annulation
*}
class function TFormNewProject.NewProject: TFileName;
begin
  with Create(nil) do
  try
    Result := InternalNewProject;
  finally
    Free;
  end;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormNewProject.FormCreate(Sender: TObject);
var
  CheckChars: string;
  C: Char;
begin
  CheckChars := '';
  for C in ValidChars do
    CheckChars := CheckChars + C;

  EditFileName.CheckChars := CheckChars;
end;

{*
  Gestionnaire d'événement OnChange de l'edit du titre
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormNewProject.EditTitleChange(Sender: TObject);
begin
  EditFileName.Text := MakeFileNameFromTitle(EditTitle.Text);
end;

{*
  Gestionnaire d'événement OnChange de l'edit du nom du fichier
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormNewProject.EditFileNameChange(Sender: TObject);
begin
  ButtonOK.Enabled := (EditTitle.Text <> '') and (EditFileName.Text <> '');
end;

end.

