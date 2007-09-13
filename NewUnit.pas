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
    { D�clarations priv�es }
    procedure AddCreator(const CreateProc, Info; var Continue: Boolean);
  public
    { D�clarations publiques }
    class function NewUnit(out FileName: TFileName;
      out GUID: TGUID): Boolean;
  end;

implementation

{$R *.dfm}

type
  /// Pointeur vers TUnitCreator
  PUnitCreator = ^TUnitCreator;

  {*
    Cr�ateur de fichier unit�
    @author sjrd
    @version 5.0
  *}
  TUnitCreator = record
    CreateProc: TCreateNewUnitProc;
    Info: TUnitCreatorInfo;
  end;

{*
  M�thode de call-back pour UnitCreators.ForEach qui ajoute le cr�ateur
  @param CreateProc   Routine de call-back de cr�ation d'unit�
  @param Info         Informations sur le cr�ateur
  @param Continue     Positionner � False pour interrompre l'�num�ration
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
  Affiche la bo�te de dialogue de cr�ation d'une nouvelle unit�
  @param FileName   En sortie : nom du fichier unit� cr��
  @param GUID       En sortie : GUID du type de fichier cr��
  @return �diteur de l'unit�
*}
class function TFormCreateNewUnit.NewUnit(
  out FileName: TFileName; out GUID: TGUID): Boolean;
var
  Creator: PUnitCreator;
begin
  // Initialisations
  Result := False;

  // V�rifier qu'il y a au moins un cr�ateur d'unit� enregistr�
  if UnitCreators.IsEmpty then
    Exit;

  with Create(Application) do
  try
    // Lister les cr�ateurs
    UnitCreators.ForEach(AddCreator);
    ListBoxUnitType.Sorted := True;
    ListBoxUnitType.ItemIndex := 0;
    ListBoxUnitTypeClick(nil);

    // Afficher la bo�te de dialogue
    if ShowModal <> mrOk then
      Exit;

    with ListBoxUnitType do
      Creator := PUnitCreator(Items.Objects[ItemIndex]);

    // Pr�parer le nom de fichier, si besoin
    if Creator.Info.AskForFileName then
    begin
      SaveUnitDialog.Filter := Creator.Info.Filter;
      SaveUnitDialog.InitialDir := fUnitsDir;
      if not SaveUnitDialog.Execute then
        Exit;
      FileName := SaveUnitDialog.FileName;
    end else
      FileName := '';

    // Cr�er le fichier
    Result := Creator.CreateProc(FileName, GUID);
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick de la list box des types d'unit�s
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormCreateNewUnit.ListBoxUnitTypeClick(Sender: TObject);
begin
  with ListBoxUnitType, PUnitCreator(Items.Objects[ItemIndex])^ do
    MemoDescription.Text := Info.Description;
end;

{*
  Gestionnaire d'�v�nement OnDestroy de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
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

