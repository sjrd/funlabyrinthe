unit SimpleSquaresEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SepiReflectionCore, SepiCompiler, SepiCompilerErrors, SepiParseTrees,
  FilesUtils, UnitFiles, SourceEditors, SdDialogs, Buttons, StdCtrls, ExtCtrls,
  ImgList, CategoryButtons, ExtDlgs, Contnrs, FunLabyUtils, SimpleSquareEdit,
  SimpleSquaresUtils, SimpleSquareNew, FunLabyEditOTA, SepiFunDelphiCompiler,
  FunLabySourceEditorFrame, ScUtils, FunLabyFilers, msxml, RTLConsts, ActiveX;

resourcestring
  SimpleSquaresFilter = 'D�finitions de cases simples (*.ssq)|*.ssq';

  sConfirmDeleteTitle = 'Supprimer ce composant ?';
  sConfirmDelete = '�tes-vous certain de vouloir supprimer ce composant ? '+
    'Si un labyrinthe l''utilise d�j�, celui-ci risque de ne plus �tre '+
    'utilisable !';

const {don't localize}
  SimpleSquaresExtension = 'ssq';
  FunDelphiExtension = 'fnd';
  SepiExtension = 'scu';

type
  TFrameSimpleSquaresEditor = class;

  {*
    Liste de TSimpleSquare
    @author sjrd
    @version 1.0
  *}
  TSimpleSquareList = class(TFunLabyCollection)
  private
    FOwner: TFrameSimpleSquaresEditor; /// Cadre propri�taire

    function GetItems(Index: Integer): TSimpleSquare;
  protected
    function CreateItem(ItemClass: TFunLabyPersistentClass):
      TFunLabyPersistent; override;

    procedure Notify(Item: TFunLabyPersistent;
      Action: TListNotification); override;

    property Owner: TFrameSimpleSquaresEditor read FOwner;
  public
    constructor Create(AOwner: TFrameSimpleSquaresEditor);

    function Add(Item: TSimpleSquare): Integer;

    function FindByID(const ID: TComponentID): TSimpleSquare;
    function IDExists(const ID: TComponentID): Boolean;

    property Items[Index: Integer]: TSimpleSquare read GetItems; default;
  end;

  {*
    Contenu d'un fichier SimpleSquares (.ssq)
    @author sjrd
    @version 1.0
  *}
  TSimpleSquaresFileContents = class(TFunLabyPersistent)
  private
    FSimpleSquares: TSimpleSquareList; /// Liste des cases simples
  public
    constructor Create(AOwner: TFrameSimpleSquaresEditor);
    destructor Destroy; override;
  published
    property SimpleSquares: TSimpleSquareList read FSimpleSquares;
  end;

  {*
    �diteur d'unit� SimpleSquares (*.ssq)
    @author sjrd
    @version 5.0
  *}
  TFrameSimpleSquaresEditor = class(TFrameFunLabySourceEditor,
    ISourceCompiler50, ISourceEditorUsingOTA50, ISimpleSquaresEditor)
    SquaresContainer: TCategoryButtons;
    SplitterSquares: TSplitter;
    SquaresImages: TImageList;
    SquareEditor: TFrameEditSimpleSquare;
    SaveSourceDialog: TSaveDialog;
    procedure ButtonNewComponentClick(Sender: TObject);
    procedure ButtonDeleteComponentClick(Sender: TObject);
    procedure ButtonSaveSourceClick(Sender: TObject);
    procedure SquaresContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure SquareEditorNameImageChange(Sender: TObject);
  private
    /// Fiche principale de FunLabyEdit
    FFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    ImagesMaster: TImagesMaster;              /// Ma�tre d'images
    FileContents: TSimpleSquaresFileContents; /// Contenu du fichier
    SimpleSquares: TSimpleSquareList;         /// Liste des composants
  protected
    procedure LoadFile(ASourceFile: TSourceFile); override;
    function SaveFile: Boolean; override;

    function CompileFile(SepiRoot: TSepiRoot;
      Errors: TSepiCompilerErrorList): TSepiUnit;
    procedure ShowError(Error: TSepiCompilerError);

    procedure ISourceCompiler50.Release = Free;
    procedure ISourceEditorUsingOTA50.Release = Free;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;
    procedure SetFunLabyEditMainForm(const Value: IOTAFunLabyEditMainForm50);

    procedure UpdateButton(Square: TSimpleSquare);

    procedure ProduceFunDelphiCode(Code: TStrings);
  protected
    function FindButton(SimpleSquare: TSimpleSquare): TButtonItem;

    procedure SimpleSquareAdded(SimpleSquare: TSimpleSquare); virtual;
    procedure SimpleSquareDeleting(SimpleSquare: TSimpleSquare); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure MarkModified;
  end;

implementation

{$R *.dfm}

const
  CreateSimpleSquaresInfo: TSourceFileCreatorInfo = (
    Title: 'Cases simples';
    Description: '�diteur visuel de cases simples. Id�al pour les d�butants.';
    AskForFileName: True;
    Filter: SimpleSquaresFilter
  );

  /// Nombre magique SimpleSquares (�quivaut � .ssq)
  SimpleSquaresMagicNumber = $7173732E;

  /// Version courante de SimpleSquares
  SimpleSquaresVersion = 1;

{*
  Cr�e un �diteur de cases simples
  @param SourceFile   Fichier source � �diter
  @return Interface vers l'�diteur cr��
*}
function CreateSimpleSquaresEditor(SourceFile: TSourceFile): ISourceEditor50;
var
  Editor: TFrameSimpleSquaresEditor;
begin
  Editor := TFrameSimpleSquaresEditor.Create(nil);
  Editor.LoadFile(SourceFile);
  Result := Editor as ISourceEditor50;
end;

{*
  Cr�e un nouveau fichier source Delphi
  @param FileName   Nom du fichier source
  @return True si le fichier a �t� cr��, False sinon
*}
function CreateSimpleSquaresFile(var FileName: TFileName): Boolean;
begin
  with TStringList.Create do
  try
    Add('<?xml version="1.0" encoding="UTF-8"?>');
    Add(Format('<simplesquares version="%s">', [CurrentVersion]));
    Add('</simplesquares>');
    SaveToFile(FileName);
  finally
    Free;
  end;

  Result := True;
end;

{-------------------------}
{ TSimpleSquareList class }
{-------------------------}

{*
  Cr�e la liste
  @param AOwner   Cadre propri�taire
*}
constructor TSimpleSquareList.Create(AOwner: TFrameSimpleSquaresEditor);
begin
  inherited Create;

  FOwner := AOwner;
end;

{*
  Tableau zero-based des �l�ments
  @param Index   Index d'un �l�ment
  @return �l�ment � l'index sp�cifi�
*}
function TSimpleSquareList.GetItems(Index: Integer): TSimpleSquare;
begin
  Result := (inherited Items[Index]) as TSimpleSquare;
end;

{*
  [@inheritDoc]
*}
function TSimpleSquareList.CreateItem(
  ItemClass: TFunLabyPersistentClass): TFunLabyPersistent;
begin
  Result := TSimpleSquareClass(ItemClass).Create(Owner.ImagesMaster);
end;

{*
  [@inheritDoc]
*}
procedure TSimpleSquareList.Notify(Item: TFunLabyPersistent;
  Action: TListNotification);
begin
  case Action of
    lnAdded: Owner.SimpleSquareAdded(TSimpleSquare(Item));
    lnExtracted, lnDeleted: Owner.SimpleSquareDeleting(TSimpleSquare(Item));
  end;

  inherited;
end;

{*
  Ajoute une nouvelle case simple
  @param ItemClass   Classe de la case simple
  @return Case simple ajout�e
*}
function TSimpleSquareList.Add(Item: TSimpleSquare): Integer;
begin
  Result := inherited AddItem(Item);
end;

{*
  Trouve un composant par son ID
  @param ID   ID d'un composant
  @return Le composant correspondant, ou nil si non trouv�
*}
function TSimpleSquareList.FindByID(const ID: TComponentID): TSimpleSquare;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := Items[I];
    if AnsiSameText(Result.ID, ID) then
      Exit;
  end;

  Result := nil;
end;

{*
  Teste si un ID est d�j� utilis� ou non
  @param ID   ID � tester
  @return True si l'ID est d�j� utilis�, False sinon
*}
function TSimpleSquareList.IDExists(const ID: TComponentID): Boolean;
begin
  Result := FindByID(ID) <> nil;
end;

{----------------------------------}
{ TSimpleSquaresFileContents class }
{----------------------------------}

{*
  Cr�e un contenu de fichier SimpleSquares
  @param AOwner   �diteur propri�taire
*}
constructor TSimpleSquaresFileContents.Create(
  AOwner: TFrameSimpleSquaresEditor);
begin
  inherited Create;

  FSimpleSquares := TSimpleSquareList.Create(AOwner);
end;

{*
  [@inheritDoc]
*}
destructor TSimpleSquaresFileContents.Destroy;
begin
  FSimpleSquares.Free;

  inherited;
end;

{-------------------------------}
{ Classe TFrameDelphiUnitEditor }
{-------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameSimpleSquaresEditor.Create(AOwner: TComponent);
begin
  inherited;

  ImagesMaster := TImagesMaster.Create;
  FileContents := TSimpleSquaresFileContents.Create(Self);
  SimpleSquares := FileContents.SimpleSquares;
end;

{*
  [@inheritDoc]
*}
destructor TFrameSimpleSquaresEditor.Destroy;
begin
  FileContents.Free;
  ImagesMaster.Free;

  inherited;
end;

{*
  Charge un fichier source
  @param ASourceFile   Fichier source � charger
*}
procedure TFrameSimpleSquaresEditor.LoadFile(ASourceFile: TSourceFile);
var
  Document: IXMLDOMDocument;
  I: Integer;
begin
  inherited;

  Document := CoDOMDocument.Create;
  Document.async := False;
  if not Document.load(SourceFile.FileName) then
    raise EInOutError.CreateFmt(SFOpenError, [SourceFile.FileName]);

  SimpleSquares.Clear;
  TFunLabyXMLReader.ReadPersistent(FileContents, Document.documentElement);

  for I := 0 to SimpleSquares.Count-1 do
    UpdateButton(SimpleSquares[I]);

  Modified := False;
end;

{*
  Enregistre le fichier source
*}
function TFrameSimpleSquaresEditor.SaveFile: Boolean;
const
  XMLHeader: string = '<?xml version="1.0" encoding="UTF-8"?>'#13#10;
var
  Stream: TStream;
  StreamAdapter: IStream;
  Document: IXMLDOMDocument;
  FileContentsNode: IXMLDOMElement;
begin
  Document := CoDOMDocument.Create;
  Document.async := False;

  FileContentsNode := Document.createElement('simplesquares');
  FileContentsNode.setAttribute('version', CurrentVersion);

  TFunLabyXMLWriter.WritePersistent(FileContents, FileContentsNode);

  Document.documentElement := FileContentsNode;

  Stream := TFileStream.Create(SourceFile.FileName,
    fmCreate or fmShareExclusive);
  try
    StreamAdapter := TStreamAdapter.Create(Stream);

    Stream.WriteBuffer(XMLHeader[1], Length(XMLHeader));
    Document.save(StreamAdapter);
  finally
    StreamAdapter := nil;
    Stream.Free;
  end;

  Result := inherited SaveFile;
end;

{*
  [@inheritDoc]
*}
function TFrameSimpleSquaresEditor.CompileFile(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList): TSepiUnit;
var
  SrcFileName, DestFileName: TFileName;
  SourceFile: TStrings;
begin
  SrcFileName := Self.SourceFile.FileName;
  DestFileName := ChangeFileExt(SrcFileName, '.'+SepiExtension);

  SourceFile := TStringList.Create;
  try
    ProduceFunDelphiCode(SourceFile);

    Errors.CurrentFileName := SrcFileName;
    Result := CompileFunDelphiSource(SepiRoot, Errors, SourceFile,
      DestFileName);
  finally
    SourceFile.Free;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleSquaresEditor.ShowError(Error: TSepiCompilerError);
begin
  // Nothing to do
end;

{*
  [@inheritDoc]
*}
function TFrameSimpleSquaresEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := FFunLabyEditMainForm;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleSquaresEditor.SetFunLabyEditMainForm(
  const Value: IOTAFunLabyEditMainForm50);
begin
  FFunLabyEditMainForm := Value;
end;

{*
  Met � jour le bouton correspondant � une case simple donn�e
  @param Square   Case simple dont mettre � jour le bouton
*}
procedure TFrameSimpleSquaresEditor.UpdateButton(Square: TSimpleSquare);
var
  Button: TButtonItem;
  SquareBmp: TSquareBitmap;
begin
  Button := FindButton(Square);
  if Button = nil then
    Exit;

  // Modification des texte et hint
  Button.Caption := Square.Name;
  Button.Hint := Square.Name;

  // Modification de l'image du composant dans la liste d'images
  SquareBmp := TSquareBitmap.Create;
  try
    Square.Draw(SquareBmp.Canvas);
    SquaresImages.ReplaceMasked(Button.ImageIndex, SquareBmp, clTransparent);
  finally
    SquareBmp.Free;
  end;
end;

{*
  Produit le code Delphi de cette unit�
  @param Code   Code produit
*}
procedure TFrameSimpleSquaresEditor.ProduceFunDelphiCode(Code: TStrings);
var
  Strings: TStringList;
  I: Integer;
begin
  Code.Add(Format('unit %s;', [GetUnitName]));
  Code.Add('');
  Code.Add('uses');
  Code.Add('  ScUtils, SdDialogs, Generics, MapTools, GenericButtons;');
  Code.Add('');

  if SimpleSquares.Count = 0 then
  begin
    Code.Add('end.');
    Exit;
  end;

  Strings := TStringList.Create;
  try
    Strings.CaseSensitive := False;
    Strings.Sorted := True;
    Strings.Duplicates := dupIgnore;

    // Actions

    for I := 0 to SimpleSquares.Count-1 do
      SimpleSquares[I].RegisterActions(Strings);

    if Strings.Count > 0 then
    begin
      Code.Add('actions');
      for I := 0 to Strings.Count-1 do
        Code.Add('  '+Strings[I] + IIF(I = Strings.Count-1, ';', ','));
      Code.Add('');
    end;

    // Attributes

    Strings.Clear;
    for I := 0 to SimpleSquares.Count-1 do
      SimpleSquares[I].RegisterAttributes(Strings);

    if Strings.Count > 0 then
    begin
      Code.Add('attributes');
      for I := 0 to Strings.Count-1 do
        Code.Add('  '+Strings[I] + IIF(I = Strings.Count-1, ';', ','));
      Code.Add('');
    end;
  finally
    Strings.Free;
  end;

  // Components

  Code.Add('components');
  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceComponents(Code);
  Code.Add('');

  // Class definitions

  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceClass(Code);

  Code.Add('end.');
end;

{*
  Trouve le bouton qui repr�sente un composant donn�
  @param SimpleSquare   Composant recherch�
  @return Bouton qui repr�sente ce composant, ou nil si non trouv�
*}
function TFrameSimpleSquaresEditor.FindButton(
  SimpleSquare: TSimpleSquare): TButtonItem;
var
  I, J: Integer;
begin
  for I := 0 to SquaresContainer.Categories.Count-1 do
  begin
    for J := 0 to SquaresContainer.Categories[I].Items.Count-1 do
    begin
      Result := SquaresContainer.Categories[I].Items[J];
      if Result.Data = Pointer(SimpleSquare) then
        Exit;
    end;
  end;

  Result := nil;
end;

{*
  Notifie l'ajout d'un composant
  @param SimpleSquare   Composant ajout�
*}
procedure TFrameSimpleSquaresEditor.SimpleSquareAdded(
  SimpleSquare: TSimpleSquare);
var
  SquareBmp: TSquareBitmap;
  ImageIndex: Integer;
  Category: TButtonCategory;
  ButtonItem: TButtonItem;
begin
  if csDestroying in ComponentState then
    Exit;

  // Ajout de l'image du composant dans la liste d'images
  SquareBmp := TSquareBitmap.Create;
  try
    SimpleSquare.Draw(SquareBmp.Canvas);
    ImageIndex := SquaresImages.AddMasked(SquareBmp, clTransparent);
  finally
    SquareBmp.Free;
  end;

  // Choix de la cat�gorie
  if SimpleSquare is TSimpleField then
    Category := SquaresContainer.Categories[1]
  else if SimpleSquare is TSimpleEffect then
    Category := SquaresContainer.Categories[2]
  else if SimpleSquare is TSimpleObject then
    Category := SquaresContainer.Categories[3]
  else if SimpleSquare is TSimpleObstacle then
    Category := SquaresContainer.Categories[4]
  else
    Category := nil;

  Assert(Category <> nil);

  // Ajout du bouton
  ButtonItem := Category.Items.Add;
  ButtonItem.ImageIndex := ImageIndex;
  ButtonItem.Caption := SimpleSquare.Name;
  ButtonItem.Hint := SimpleSquare.Name;
  ButtonItem.Data := SimpleSquare;

  // Mark modified
  MarkModified;
end;

{*
  Notifie la suppression d'un composant
  @param SimpleSquare   Composant supprim�
*}
procedure TFrameSimpleSquaresEditor.SimpleSquareDeleting(
  SimpleSquare: TSimpleSquare);
var
  Button: TButtonItem;
begin
  if csDestroying in ComponentState then
    Exit;

  // Clear the component editor
  if SquareEditor.CurrentSquare = SimpleSquare then
    SquareEditor.CurrentSquare := nil;

  // Look for the button and delete it
  Button := FindButton(SimpleSquare);
  if Button <> nil then
    Button.Free;

  // Mark modified
  MarkModified;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleSquaresEditor.AfterConstruction;
begin
  inherited;

  SquareEditor.OnNameImageChange := SquareEditorNameImageChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleSquaresEditor.BeforeDestruction;
begin
  SquareEditor.CurrentSquare := nil;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleSquaresEditor.MarkModified;
begin
  Modified := True;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Nouveau composant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSimpleSquaresEditor.ButtonNewComponentClick(Sender: TObject);
var
  SimpleSquare: TSimpleSquare;
begin
  SimpleSquare := TFormNewSimpleSquare.NewSimpleSquare(ImagesMaster,
    SimpleSquares.IDExists);

  if SimpleSquare <> nil then
  begin
    SimpleSquares.Add(SimpleSquare);
    SquareEditor.CurrentSquare := SimpleSquare;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Supprimer ce composant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSimpleSquaresEditor.ButtonDeleteComponentClick(Sender: TObject);
var
  SimpleSquare: TSimpleSquare;
begin
  SimpleSquare := SquareEditor.CurrentSquare;
  if SimpleSquare = nil then
    Exit;

  if ShowDialog(sConfirmDeleteTitle, sConfirmDelete, dtWarning,
    dbOKCancel, 2) <> drOK then
    Exit;

  SquareEditor.CurrentSquare := nil;
  SimpleSquares.Remove(SimpleSquare);
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Enregistrer le source
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSimpleSquaresEditor.ButtonSaveSourceClick(Sender: TObject);
var
  Output: TStrings;
begin
  SaveSourceDialog.InitialDir := fUnitsDir;
  SaveSourceDialog.FileName := GetUnitName + '.' + FunDelphiExtension;

  if SaveSourceDialog.Execute then
  begin
    Output := TStringList.Create;
    try
      ProduceFunDelphiCode(Output);
      Output.SaveToFile(SaveSourceDialog.FileName);
    finally
      Output.Free;
    end;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick d'un bouton S�lectionner ce composant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSimpleSquaresEditor.SquaresContainerButtonClicked(
  Sender: TObject; const Button: TButtonItem);
begin
  if Button.Data <> nil then
    SquareEditor.CurrentSquare := TSimpleSquare(Button.Data);
end;

{*
  Gestionnaire d'�v�nement OnNameImageChange de l'�diteur de case simple
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSimpleSquaresEditor.SquareEditorNameImageChange(
  Sender: TObject);
begin
  UpdateButton(SquareEditor.CurrentSquare);
end;

initialization
  SourceFileEditors.Add(SimpleSquaresExtension, CreateSimpleSquaresEditor);
  SourceFileEditors.AddFilter(SimpleSquaresFilter);

  SourceFileCreators.Add(CreateSimpleSquaresFile, CreateSimpleSquaresInfo);
finalization
  SourceFileEditors.Remove(SimpleSquaresExtension);
  SourceFileEditors.RemoveFilter(SimpleSquaresFilter);

  SourceFileCreators.Remove(CreateSimpleSquaresFile);
end.

