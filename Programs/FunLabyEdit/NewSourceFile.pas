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
    { D�clarations priv�es }
    procedure AddCreator(const CreateProc, Info; var Continue: Boolean);

    function ValidateFileName(const FileName: TFileName): Boolean;
  public
    { D�clarations publiques }
    class function NewSourceFile(out FileName: TFileName): Boolean;
  end;

implementation

{$R *.dfm}

type
  /// Pointeur vers TSourceFileCreator
  PSourceFileCreator = ^TSourceFileCreator;

  {*
    Cr�ateur de fichier unit�
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
  M�thode de call-back pour UnitCreators.ForEach qui ajoute le cr�ateur
  @param CreateProc   Routine de call-back de cr�ation d'unit�
  @param Info         Informations sur le cr�ateur
  @param Continue     Positionner � False pour interrompre l'�num�ration
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
  V�rifie qu'un nom de fichier est valide
  @param FileName   Nom du fichier � valider
  @return True si le nom de fichier est valide, False sinon
*}
function TFormCreateNewSourceFile.ValidateFileName(
  const FileName: TFileName): Boolean;
var
  UnitName, Extension: string;
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

  Result := True;
end;

{*
  Affiche la bo�te de dialogue de cr�ation d'une nouvelle unit�
  @param FileName   En sortie : nom du fichier unit� cr��
  @return �diteur de l'unit�
*}
class function TFormCreateNewSourceFile.NewSourceFile(
  out FileName: TFileName): Boolean;
const
  FilterFormat = '%s (*.%s)|*.%1:s';
var
  Creator: PSourceFileCreator;
begin
  // Initialisations
  Result := False;

  // V�rifier qu'il y a au moins un cr�ateur d'unit� enregistr�
  if SourceFileCreators.IsEmpty then
    Exit;

  with Create(Application) do
  try
    // Lister les cr�ateurs
    SourceFileCreators.ForEach(AddCreator);
    ListBoxSourceFileType.Sorted := True;
    ListBoxSourceFileType.ItemIndex := 0;
    ListBoxSourceFileTypeClick(nil);

    // Afficher la bo�te de dialogue
    if ShowModal <> mrOk then
      Exit;

    with ListBoxSourceFileType do
      Creator := PSourceFileCreator(Items.Objects[ItemIndex]);

    // Pr�parer le nom de fichier, si besoin
    if Creator.Info.AskForFileName then
    begin
      SaveSourceFileDialog.Filter := Format(FilterFormat,
        [Creator.Info.Title, Creator.Info.Extension]);
      SaveSourceFileDialog.InitialDir :=
        JoinPath([FunLabyAppDataDir, SourcesDir]);

      if not SaveSourceFileDialog.Execute then
        Exit;

      FileName := ChangeFileExt(SaveSourceFileDialog.FileName,
        '.'+Creator.Info.Extension);
    end else
      FileName := '';

    // Cr�er le fichier
    Result := Creator.CreateProc(FileName);
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnDestroy de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnClick de la list box des types d'unit�s
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormCreateNewSourceFile.ListBoxSourceFileTypeClick(Sender: TObject);
begin
  with ListBoxSourceFileType, PSourceFileCreator(Items.Objects[ItemIndex])^ do
    MemoDescription.Text := Info.Description;
end;

{*
  Gestionnaire d'�v�nement OnCanClose de la bo�te de dialogue Enregistrer
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param CanClose   En sortie : indique si la bo�te peut �tre ferm�e
*}
procedure TFormCreateNewSourceFile.SaveSourceFileDialogCanClose(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := ValidateFileName(SaveSourceFileDialog.FileName);
end;

end.

