unit SimpleSquaresEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SepiReflectionCore, SepiCompiler, SepiCompilerErrors, SepiParseTrees,
  FilesUtils, UnitFiles, SourceEditors, SdDialogs, Buttons, StdCtrls, ExtCtrls,
  ImgList, CategoryButtons, ExtDlgs, Contnrs, FunLabyUtils, SimpleSquareEdit,
  SimpleSquaresUtils, SimpleSquareNew, FunLabyEditOTA, SepiDelphiCompiler,
  FunLabySourceEditorFrame;

resourcestring
  SimpleSquaresFilter = 'Définitions de cases simples (*.ssq)|*.ssq';

  sConfirmDeleteTitle = 'Supprimer ce composant ?';
  sConfirmDelete = 'Êtes-vous certain de vouloir supprimer ce composant ? '+
    'Si un labyrinthe l''utilise déjà, celui-ci risque de ne plus être '+
    'utilisable !';

const {don't localize}
  SimpleSquaresExtension = 'ssq';
  DelphiExtension = 'pas';
  SepiExtension = 'scu';

type
  TFrameSimpleSquaresEditor = class;

  {*
    Liste de TSimpleSquare
    @author sjrd
    @version 1.0
  *}
  TSimpleSquareList = class(TObjectList)
  private
    FOwner: TFrameSimpleSquaresEditor; /// Cadre propriétaire
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    function GetItems(Index: Integer): TSimpleSquare;

    property Owner: TFrameSimpleSquaresEditor read FOwner;
  public
    constructor Create(AOwner: TFrameSimpleSquaresEditor);

    function FindByID(const ID: TComponentID): TSimpleSquare;
    function IDExists(const ID: TComponentID): Boolean;

    property Items[Index: Integer]: TSimpleSquare read GetItems; default;
  end;

  {*
    Éditeur d'unité SimpleSquares (*.ssq)
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

    ImagesMaster: TImagesMaster;      /// Maître d'images
    SimpleSquares: TSimpleSquareList; /// Liste des composants
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

    procedure ProduceDelphiCode(Code: TStrings);
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
    Description: 'Éditeur visuel de cases simples. Idéal pour les débutants.';
    AskForFileName: True;
    Filter: SimpleSquaresFilter
  );

  /// Nombre magique SimpleSquares (équivaut à .ssq)
  SimpleSquaresMagicNumber = $7173732E;

  /// Version courante de SimpleSquares
  SimpleSquaresVersion = 1;

{*
  Crée un éditeur de cases simples
  @param SourceFile   Fichier source à éditer
  @return Interface vers l'éditeur créé
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
  Crée un nouveau fichier source Delphi
  @param FileName   Nom du fichier source
  @return True si le fichier a été créé, False sinon
*}
function CreateSimpleSquaresFile(var FileName: TFileName): Boolean;
var
  MagicNumber, Version, Count: Integer;
begin
  with TFileStream.Create(FileName, fmCreate or fmShareExclusive) do
  try
    MagicNumber := SimpleSquaresMagicNumber;
    WriteBuffer(MagicNumber, 4);
    Version := SimpleSquaresVersion;
    WriteBuffer(Version, 4);
    Count := 0;
    WriteBuffer(Count, 4);
  finally
    Free;
  end;

  Result := True;
end;

{-------------------------}
{ TSimpleSquareList class }
{-------------------------}

{*
  Crée la liste
  @param AOwner   Cadre propriétaire
*}
constructor TSimpleSquareList.Create(AOwner: TFrameSimpleSquaresEditor);
begin
  inherited Create;

  FOwner := AOwner;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleSquareList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded: Owner.SimpleSquareAdded(TSimpleSquare(Ptr));
    lnExtracted, lnDeleted: Owner.SimpleSquareDeleting(TSimpleSquare(Ptr));
  end;

  inherited;
end;

{*
  Tableau zero-based des éléments
  @param Index   Index d'un élément
  @return Élément à l'index spécifié
*}
function TSimpleSquareList.GetItems(Index: Integer): TSimpleSquare;
begin
  Result := (inherited GetItem(Index)) as TSimpleSquare;
end;

{*
  Trouve un composant par son ID
  @param ID   ID d'un composant
  @return Le composant correspondant, ou nil si non trouvé
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
  Teste si un ID est déjà utilisé ou non
  @param ID   ID à tester
  @return True si l'ID est déjà utilisé, False sinon
*}
function TSimpleSquareList.IDExists(const ID: TComponentID): Boolean;
begin
  Result := FindByID(ID) <> nil;
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
  SimpleSquares := TSimpleSquareList.Create(Self);
end;

{*
  [@inheritDoc]
*}
destructor TFrameSimpleSquaresEditor.Destroy;
begin
  SimpleSquares.Free;
  ImagesMaster.Free;

  inherited;
end;

{*
  Charge un fichier source
  @param ASourceFile   Fichier source à charger
*}
procedure TFrameSimpleSquaresEditor.LoadFile(ASourceFile: TSourceFile);
var
  Stream: TStream;
  MagicNumber, Version, Count, I: Integer;
begin
  inherited;

  Stream := TFileStream.Create(SourceFile.FileName,
    fmOpenRead or fmShareDenyWrite);
  try
    Stream.ReadBuffer(MagicNumber, 4);
    Assert(MagicNumber = SimpleSquaresMagicNumber);

    Stream.ReadBuffer(Version, 4);
    Assert(Version = SimpleSquaresVersion);

    Stream.ReadBuffer(Count, 4);
    for I := 0 to Count-1 do
      SimpleSquares.Add(TSimpleSquare.LoadFromStream(ImagesMaster, Stream));
  finally
    Stream.Free;
  end;

  Modified := False;
end;

{*
  Enregistre le fichier source
*}
function TFrameSimpleSquaresEditor.SaveFile: Boolean;
var
  Stream: TStream;
  MagicNumber, Version, Count, I: Integer;
begin
  Stream := TFileStream.Create(SourceFile.FileName,
    fmCreate or fmShareExclusive);
  try
    MagicNumber := SimpleSquaresMagicNumber;
    Stream.WriteBuffer(MagicNumber, 4);

    Version := SimpleSquaresVersion;
    Stream.WriteBuffer(Version, 4);

    Count := SimpleSquares.Count;
    Stream.WriteBuffer(Count, 4);
    for I := 0 to Count-1 do
      SimpleSquares[I].SaveToStream(Stream);
  finally
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
    ProduceDelphiCode(SourceFile);

    Errors.CurrentFileName := SrcFileName;
    Result := CompileDelphiSource(SepiRoot, Errors, SourceFile, DestFileName);
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
  Produit le code Delphi de cette unité
  @param Code   Code produit
*}
procedure TFrameSimpleSquaresEditor.ProduceDelphiCode(Code: TStrings);
var
  I: Integer;
begin
  Code.Add(Format('unit %s;', [GetUnitName]));
  Code.Add('');
  Code.Add('interface');
  Code.Add('');
  Code.Add('uses');
  Code.Add('  SysUtils, Classes, Graphics, ScUtils, SdDialogs, FunLabyUtils,');
  Code.Add('  Generics, MapTools, GenericButtons;');
  Code.Add('');
  Code.Add('const');

  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceIDConst(Code);

  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceClassDef(Code);

  Code.Add('');
  Code.Add('implementation');
  Code.Add('');
  Code.Add('procedure InitializeUnit(Master: TMaster; Params: TStrings);');
  Code.Add('begin');

  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceCreateComponents(Code);

  Code.Add('end;');
  Code.Add('');
  Code.Add('procedure RegisterComponents(Master: TMaster;');
  Code.Add('  RegisterSingleComponentProc: TRegisterSingleComponentProc;');
  Code.Add('  RegisterComponentSetProc: TRegisterComponentSetProc);');
  Code.Add('begin');

  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceRegisterComponents(Code);

  Code.Add('end;');

  for I := 0 to SimpleSquares.Count-1 do
    SimpleSquares[I].ProduceImplementation(Code);

  Code.Add('');
  Code.Add('end.');
end;

{*
  Trouve le bouton qui représente un composant donné
  @param SimpleSquare   Composant recherché
  @return Bouton qui représente ce composant, ou nil si non trouvé
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
  @param SimpleSquare   Composant ajouté
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

  // Choix de la catégorie
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
  @param SimpleSquare   Composant supprimé
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
  Gestionnaire d'événement OnClick du bouton Nouveau composant
  @param Sender   Objet qui a déclenché l'événement
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
  Gestionnaire d'événement OnClick du bouton Supprimer ce composant
  @param Sender   Objet qui a déclenché l'événement
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
  Gestionnaire d'événement OnClick du bouton Enregistrer le source
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameSimpleSquaresEditor.ButtonSaveSourceClick(Sender: TObject);
var
  Output: TStrings;
begin
  SaveSourceDialog.InitialDir := fUnitsDir;
  SaveSourceDialog.FileName := GetUnitName + '.' + DelphiExtension;

  if SaveSourceDialog.Execute then
  begin
    Output := TStringList.Create;
    try
      ProduceDelphiCode(Output);
      Output.SaveToFile(SaveSourceDialog.FileName);
    finally
      Output.Free;
    end;
  end;
end;

{*
  Gestionnaire d'événement OnClick d'un bouton Sélectionner ce composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameSimpleSquaresEditor.SquaresContainerButtonClicked(
  Sender: TObject; const Button: TButtonItem);
begin
  if Button.Data <> nil then
    SquareEditor.CurrentSquare := TSimpleSquare(Button.Data);
end;

{*
  Gestionnaire d'événement OnNameImageChange de l'éditeur de case simple
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameSimpleSquaresEditor.SquareEditorNameImageChange(
  Sender: TObject);
var
  Square: TSimpleSquare;
  Button: TButtonItem;
  SquareBmp: TSquareBitmap;
begin
  Square := SquareEditor.CurrentSquare;
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

initialization
  SourceFileEditors.Add(SimpleSquaresExtension, CreateSimpleSquaresEditor);
  SourceFileEditors.AddFilter(SimpleSquaresFilter);

  SourceFileCreators.Add(CreateSimpleSquaresFile, CreateSimpleSquaresInfo);
finalization
  SourceFileEditors.Remove(SimpleSquaresExtension);
  SourceFileEditors.RemoveFilter(SimpleSquaresFilter);

  SourceFileCreators.Remove(CreateSimpleSquaresFile);
end.

