unit NewSourceFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScStrUtils, SdDialogs, FilesUtils, SourceEditors,
  FunLabyUtils, FunLabyEditConsts;

type
  TFormCreateNewSourceFile = class(TForm)
    LabelSourceFileType: TLabel;
    ListBoxSourceFileType: TListBox;
    LabelDescription: TLabel;
    MemoDescription: TMemo;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    SaveSourceFileDialog: TSaveDialog;
    procedure ListBoxSourceFileTypeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveSourceFileDialogCanClose(Sender: TObject;
      var CanClose: Boolean);
  private
    { Déclarations privées }
    procedure AddCreator(const CreateProc, Info; var Continue: Boolean);

    function ValidateFileName(const FileName: TFileName): Boolean;
  public
    { Déclarations publiques }
    class function NewSourceFile(out FileName: TFileName): Boolean;
  end;

implementation

{$R *.dfm}

type
  /// Pointeur vers TSourceFileCreator
  PSourceFileCreator = ^TSourceFileCreator;

  {*
    Créateur de fichier unité
    @author sjrd
    @version 5.0
  *}
  TSourceFileCreator = record
    CreateProc: TCreateNewSourceFileProc;
    Info: TSourceFileCreatorInfo;
  end;

{--------------------------------}
{ TFormCreateNewSourceFile class }
{--------------------------------}

{*
  Méthode de call-back pour UnitCreators.ForEach qui ajoute le créateur
  @param CreateProc   Routine de call-back de création d'unité
  @param Info         Informations sur le créateur
  @param Continue     Positionner à False pour interrompre l'énumération
*}
procedure TFormCreateNewSourceFile.AddCreator(const CreateProc, Info;
  var Continue: Boolean);
var
  Creator: PSourceFileCreator;
begin
  New(Creator);
  Creator.CreateProc := TCreateNewSourceFileProc(CreateProc);
  Creator.Info := TSourceFileCreatorInfo(Info);

  ListBoxSourceFileType.Items.AddObject(Creator.Info.Title, TObject(Creator));
end;

{*
  Vérifie qu'un nom de fichier est valide
  @param FileName   Nom du fichier à valider
  @return True si le nom de fichier est valide, False sinon
*}
function TFormCreateNewSourceFile.ValidateFileName(
  const FileName: TFileName): Boolean;
var
  UnitName, Extension, Filter, Temp, FilterMask, SubMask: string;
begin
  // Parse file name
  Extension := ExtractFileExt(FileName);
  UnitName := ExtractFileName(FileName);
  SetLength(UnitName, Length(UnitName) - Length(Extension));

  // Check unit name
  if not IsValidIdent(UnitName) then
  begin
    ShowDialog(SInvalidFileNameTitle, Format(SInvalidUnitName, [UnitName]),
      dtError);
    Result := False;
    Exit;
  end;

  // Check extension
  Filter := SaveSourceFileDialog.Filter;
  while SplitToken(Filter, '|', FilterMask, Temp) do
  begin
    SplitToken(Temp, '|', FilterMask, Filter);

    while FilterMask <> '' do
    begin
      SplitToken(FilterMask, ';', SubMask, Temp);
      FilterMask := Temp;

      if (SubMask = '*'+Extension) or (SubMask = '*.*') or (SubMask = '*') then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  ShowDialog(SInvalidFileNameTitle, Format(SInvalidExtension, [Extension]),
    dtError);
  Result := False;
end;

{*
  Affiche la boîte de dialogue de création d'une nouvelle unité
  @param FileName   En sortie : nom du fichier unité créé
  @return Éditeur de l'unité
*}
class function TFormCreateNewSourceFile.NewSourceFile(
  out FileName: TFileName): Boolean;
var
  Creator: PSourceFileCreator;
begin
  // Initialisations
  Result := False;

  // Vérifier qu'il y a au moins un créateur d'unité enregistré
  if SourceFileCreators.IsEmpty then
    Exit;

  with Create(Application) do
  try
    // Lister les créateurs
    SourceFileCreators.ForEach(AddCreator);
    ListBoxSourceFileType.Sorted := True;
    ListBoxSourceFileType.ItemIndex := 0;
    ListBoxSourceFileTypeClick(nil);

    // Afficher la boîte de dialogue
    if ShowModal <> mrOk then
      Exit;

    with ListBoxSourceFileType do
      Creator := PSourceFileCreator(Items.Objects[ItemIndex]);

    // Préparer le nom de fichier, si besoin
    if Creator.Info.AskForFileName then
    begin
      SaveSourceFileDialog.Filter := Creator.Info.Filter;
      SaveSourceFileDialog.InitialDir := fUnitsDir;
      if not SaveSourceFileDialog.Execute then
        Exit;
      FileName := SaveSourceFileDialog.FileName;
    end else
      FileName := '';

    // Créer le fichier
    Result := Creator.CreateProc(FileName);
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'événement OnDestroy de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormCreateNewSourceFile.FormDestroy(Sender: TObject);
var
  I: Integer;
  Creator: PSourceFileCreator;
begin
  for I := 0 to ListBoxSourceFileType.Items.Count-1 do
  begin
    Creator := PSourceFileCreator(ListBoxSourceFileType.Items.Objects[I]);
    Dispose(Creator);
  end;
end;

{*
  Gestionnaire d'événement OnClick de la list box des types d'unités
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormCreateNewSourceFile.ListBoxSourceFileTypeClick(Sender: TObject);
begin
  with ListBoxSourceFileType, PSourceFileCreator(Items.Objects[ItemIndex])^ do
    MemoDescription.Text := Info.Description;
end;

{*
  Gestionnaire d'événement OnCanClose de la boîte de dialogue Enregistrer
  @param Sender     Objet qui a déclenché l'événement
  @param CanClose   En sortie : indique si la boîte peut être fermée
*}
procedure TFormCreateNewSourceFile.SaveSourceFileDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
var
  I: Integer;
begin
  CanClose := False;
  for I := 0 to SaveSourceFileDialog.Files.Count-1 do
    if not ValidateFileName(SaveSourceFileDialog.Files[I]) then
      Exit;
  CanClose := True;
end;

end.

