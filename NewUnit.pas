unit NewUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SdDialogs, FilesUtils, UnitEditorIntf,
  FunLabyUtils, FunLabyEditConsts;

type
  TFormCreateNewUnit = class(TForm)
    LabelUnitType: TLabel;
    ListBoxUnitType: TListBox;
    LabelDescription: TLabel;
    MemoDescription: TMemo;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    SaveUnitDialog: TSaveDialog;
    procedure ListBoxUnitTypeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    procedure AddCreator(const CreateProc, Info; var Continue: Boolean);
  public
    { Déclarations publiques }
    class function NewUnit(out FileName: TFileName;
      out GUID: TGUID): Boolean;
  end;

implementation

{$R *.dfm}

type
  /// Pointeur vers TUnitCreator
  PUnitCreator = ^TUnitCreator;

  {*
    Créateur de fichier unité
    @author sjrd
    @version 5.0
  *}
  TUnitCreator = record
    CreateProc: TCreateNewUnitProc;
    Info: TUnitCreatorInfo;
  end;

{*
  Méthode de call-back pour UnitCreators.ForEach qui ajoute le créateur
  @param CreateProc   Routine de call-back de création d'unité
  @param Info         Informations sur le créateur
  @param Continue     Positionner à False pour interrompre l'énumération
*}
procedure TFormCreateNewUnit.AddCreator(const CreateProc, Info;
  var Continue: Boolean);
var
  Creator: PUnitCreator;
begin
  New(Creator);
  Initialize(Creator^);
  Creator.CreateProc := TCreateNewUnitProc(CreateProc);
  Creator.Info := TUnitCreatorInfo(Info);

  ListBoxUnitType.Items.AddObject(Creator.Info.Title, TObject(Creator));
end;

{*
  Affiche la boîte de dialogue de création d'une nouvelle unité
  @param FileName   En sortie : nom du fichier unité créé
  @param GUID       En sortie : GUID du type de fichier créé
  @return Éditeur de l'unité
*}
class function TFormCreateNewUnit.NewUnit(
  out FileName: TFileName; out GUID: TGUID): Boolean;
var
  Creator: PUnitCreator;
begin
  // Initialisations
  Result := False;

  // Vérifier qu'il y a au moins un créateur d'unité enregistré
  if UnitCreators.IsEmpty then
    Exit;

  with Create(Application) do
  try
    // Lister les créateurs
    UnitCreators.ForEach(AddCreator);
    ListBoxUnitType.Sorted := True;
    ListBoxUnitType.ItemIndex := 0;
    ListBoxUnitTypeClick(nil);

    // Afficher la boîte de dialogue
    if ShowModal <> mrOk then
      Exit;

    with ListBoxUnitType do
      Creator := PUnitCreator(Items.Objects[ItemIndex]);

    // Préparer le nom de fichier, si besoin
    if Creator.Info.AskForFileName then
    begin
      SaveUnitDialog.Filter := Creator.Info.Filter;
      SaveUnitDialog.InitialDir := fUnitsDir;
      if not SaveUnitDialog.Execute then
        Exit;
      FileName := SaveUnitDialog.FileName;
    end else
      FileName := '';

    // Créer le fichier
    Result := Creator.CreateProc(FileName, GUID);
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'événement OnClick de la list box des types d'unités
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormCreateNewUnit.ListBoxUnitTypeClick(Sender: TObject);
begin
  with ListBoxUnitType, PUnitCreator(Items.Objects[ItemIndex])^ do
    MemoDescription.Text := Info.Description;
end;

{*
  Gestionnaire d'événement OnDestroy de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormCreateNewUnit.FormDestroy(Sender: TObject);
var
  I: Integer;
  Creator: PUnitCreator;
begin
  for I := 0 to ListBoxUnitType.Items.Count-1 do
  begin
    Creator := PUnitCreator(ListBoxUnitType.Items.Objects[I]);
    Finalize(Creator^);
    Dispose(Creator);
  end;
end;

end.

