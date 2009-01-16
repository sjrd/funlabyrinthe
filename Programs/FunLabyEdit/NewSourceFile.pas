unit NewSourceFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SdDialogs, FilesUtils, UnitEditorIntf,
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
  private
    { Déclarations privées }
    procedure AddCreator(const CreateProc, Info; var Continue: Boolean);
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
  Gestionnaire d'événement OnClick de la list box des types d'unités
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormCreateNewSourceFile.ListBoxSourceFileTypeClick(Sender: TObject);
begin
  with ListBoxSourceFileType, PSourceFileCreator(Items.Objects[ItemIndex])^ do
    MemoDescription.Text := Info.Description;
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

end.

