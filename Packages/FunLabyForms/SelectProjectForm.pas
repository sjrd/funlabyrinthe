{*
  Boîte de dialogue de sélection d'un fichier projet
  @author sjrd
  @version 5.1
*}
unit SelectProjectForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, StrUtils, msxml,
  JvComponentBase, JvChangeNotify, ScUtils, ScXML, SdDialogs,
  FunLabyUtils, FilesUtils, FunLabyCoreConsts;

resourcestring
  SCorruptedFileDescription =
    'Il y a eu une erreur au chargement de ce fichier pour la preview. Il est '+
    'probablement inutilisable.';

type
  {*
    Boîte de dialogue de sélection d'un fichier projet
    @author sjrd
    @version 5.1
  *}
  TFormSelectProjectFile = class(TForm)
    ListViewFiles: TListView;
    PanelBottom: TPanel;
    PanelRight: TPanel;
    LabelFileName: TLabel;
    EditFileName: TComboBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelDescription: TLabel;
    EditDescription: TMemo;
    ChangeNotifier: TJvChangeNotify;
    procedure ChangeNotifierChangeNotify(Sender: TObject; Dir: string;
      Actions: TJvChangeActions);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ListViewFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FFileName: TFileName; /// Nom de fichier sélectionné

    SortColumnIndex: Integer; /// Index de la colonne de tri
    SortReverse: Boolean;     /// True pour renverser le tri

    procedure FillFileItem(Item: TListItem; const ProjectSubFile: TFileName);
    procedure FillCorruptedFileItem(Item: TListItem;
      const ProjectSubFile: TFileName);
    procedure FillFileList;
  public
    function Execute: Boolean;

    property FileName: TFileName read FFileName write FFileName;
  end;

var
  FormSelectProjectFile: TFormSelectProjectFile;

implementation

{$R *.dfm}

const
  IndexKind = 0;
  IndexDifficulty = 1;
  IndexAuthor = 2;
  IndexFileName = 3;
  IndexDescription = 4;

  GroupNewLabyrinths = 0;
  GroupOldLabyrinths = 1;
  GroupNotForPlaying = 2;
  GroupCorrupted = 3;

{------------------------------}
{ TFormSelectProjectFile class }
{------------------------------}

{*
  Remplit un élément de la liste des fichiers
  @param Item       Élément de la liste
  @param FileName   Nom du fichier
*}
procedure TFormSelectProjectFile.FillFileItem(Item: TListItem;
  const ProjectSubFile: TFileName);
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

const
  StrNotForPlaying = 'Pas pour jouer'; {don't localize}
  IsOldLabyrinthQuery1 =
    'collection[@name="UnitFiles"]//item[property="Compatibility4x.bpl"]';
  IsOldLabyrinthQuery2 =
    'collection[@name="UsedUnits"]//item[property="Compatibility4x"]';
begin
  try
    Document := LoadXMLDocumentFromFile(
      JoinPath([ProjectsPath, ProjectSubFile]));
    RootElement := Document.documentElement;

    Item.Caption := ReadProperty('Title');
    Item.SubItems.Add(ReadProperty('Kind'));
    Item.SubItems.Add(ReadProperty('Difficulty'));
    Item.SubItems.Add(ReadProperty('Author'));
    Item.SubItems.Add(ProjectSubFile);
    Item.SubItems.Add(ReadProperty('Description'));

    if Item.SubItems[IndexKind] = StrNotForPlaying then
      Item.GroupID := GroupNotForPlaying
    else if RootElement.selectSingleNode(IsOldLabyrinthQuery1) <> nil then
      Item.GroupID := GroupOldLabyrinths
    else if RootElement.selectSingleNode(IsOldLabyrinthQuery2) <> nil then
      Item.GroupID := GroupOldLabyrinths
    else
      Item.GroupID := GroupNewLabyrinths;
  except
    on Error: Exception do
    begin
      FillCorruptedFileItem(Item, FileName);
    end;
  end;
end;

{*
  Remplit un élément de la liste des fichiers pour un fichier corrompu
  @param Item       Élément de la liste
  @param FileName   Nom du fichier
*}
procedure TFormSelectProjectFile.FillCorruptedFileItem(Item: TListItem;
  const ProjectSubFile: TFileName);
var
  I: Integer;
begin
  Item.Caption := ProjectSubFile;
  for I := 0 to 2 do
    Item.SubItems.Add('');
  Item.SubItems.Add(ProjectSubFile);
  Item.SubItems.Add(SCorruptedFileDescription);

  Item.GroupID := GroupCorrupted;
end;

{*
  Remplit la liste des fichiers
*}
procedure TFormSelectProjectFile.FillFileList;
var
  ProjectSubFile: string;
begin
  ListViewFiles.Items.BeginUpdate;
  EditFileName.Items.BeginUpdate;
  try
    ListViewFiles.Clear;
    EditFileName.Items.Clear;

    IterateProjects(
      procedure(const FileName: TFileName; const SearchRec: TSearchRec)
      begin
        ProjectSubFile := FileName;
        Delete(ProjectSubFile, 1, Length(ProjectsPath));
        if (ProjectSubFile <> '') and (ProjectSubFile[1] = PathDelim) then
          Delete(ProjectSubFile, 1, 1);

        FillFileItem(ListViewFiles.Items.Add, ProjectSubFile);
        EditFileName.Items.Add(ProjectSubFile);
      end);

    ListViewFiles.AlphaSort;
  finally
    ListViewFiles.Items.EndUpdate;
    EditFileName.Items.EndUpdate;
  end;
end;

{*
  Exécute la boîte de dialogue
  @return True si l'utilisateur a sélectionné un fichier, False sinon
*}
function TFormSelectProjectFile.Execute: Boolean;
begin
  EditFileName.Text := ExtractFileName(FileName);

  FillFileList;
  Result := ShowModal = mrOK;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender    Objet qui a déclenché l'événement
*}
procedure TFormSelectProjectFile.FormCreate(Sender: TObject);
begin
  ChangeNotifier.Notifications[0].Directory := ProjectsPath;
end;

{*
  Gestionnaire d'événement OnShow de la fiche
  @param Sender    Objet qui a déclenché l'événement
*}
procedure TFormSelectProjectFile.FormShow(Sender: TObject);
begin
  ChangeNotifier.Active := True;
end;

{*
  Gestionnaire d'événement OnHide de la fiche
  @param Sender    Objet qui a déclenché l'événement
*}
procedure TFormSelectProjectFile.FormHide(Sender: TObject);
begin
  ChangeNotifier.Active := False;
end;

{*
  Gestionnaire d'événement OnChangeNotify du notificateur de changements
  @param Sender    Objet qui a déclenché l'événement
  @param Dir       Dossier
  @param Actions   Actions
*}
procedure TFormSelectProjectFile.ChangeNotifierChangeNotify(Sender: TObject;
  Dir: string; Actions: TJvChangeActions);
begin
  FillFileList;
end;

{*
  Gestionnaire d'événement OnColumnClick de la liste de fichiers
  @param Sender   Objet qui a déclenché l'événement
  @param Column   Colonne cliquée
*}
procedure TFormSelectProjectFile.ListViewFilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if SortColumnIndex = Column.Index then
    SortReverse := not SortReverse
  else
  begin
    SortColumnIndex := Column.Index;
    SortReverse := False;
  end;

  ListViewFiles.AlphaSort;
end;

{*
  Gestionnaire d'événement OnCompare de la liste de fichiers
  @param Sender    Objet qui a déclenché l'événement
  @param Item1     Premier élément
  @param Item2     Second élément
  @param Data      Donnée
  @param Compare   Résultat de la comparaison
*}
procedure TFormSelectProjectFile.ListViewFilesCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Text1, Text2: string;
begin
  if SortColumnIndex = 0 then
  begin
    Text1 := Item1.Caption;
    Text2 := Item2.Caption;
  end else
  begin
    Text1 := Item1.SubItems[SortColumnIndex-1];
    Text2 := Item2.SubItems[SortColumnIndex-1];
  end;

  Compare := AnsiCompareText(Text1, Text2);
  if SortReverse then
    Compare := -Compare;
end;

{*
  Gestionnaire d'événement OnSelectItem de la liste de fichiers
  @param Sender     Objet qui a déclenché l'événement
  @param Item       Élément modifié
  @param Selected   True si l'élément est sélectionné
*}
procedure TFormSelectProjectFile.ListViewFilesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    EditFileName.Text := Item.SubItems[IndexFileName];
    EditDescription.Text := Item.SubItems[IndexDescription];
  end;
end;

{*
  Gestionnaire d'événement OnDblClick de la liste de fichiers
  @param Sender    Objet qui a déclenché l'événement
*}
procedure TFormSelectProjectFile.ListViewFilesDblClick(Sender: TObject);
begin
  ButtonOK.Click;
end;

{*
  Gestionnaire d'événement OnClick du bouton OK
  @param Sender    Objet qui a déclenché l'événement
*}
procedure TFormSelectProjectFile.ButtonOKClick(Sender: TObject);
begin
  if Pos('.', EditFileName.Text) = 0 then
    EditFileName.Text := EditFileName.Text + '.flp';

  FileName := JoinPath([ProjectsPath, EditFileName.Text]);

  if not FileExists(FileName) then
  begin
    ShowDialog(SFileNotFoundTitle,
      Format(SFileNotFound, [EditFileName.Text]), dtError);
    Exit;
  end;

  ModalResult := mrOK;
end;

{*
  Gestionnaire d'événement OnClick du bouton Annuler
  @param Sender    Objet qui a déclenché l'événement
*}
procedure TFormSelectProjectFile.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

