unit EditUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScLists, SdDialogs, FilesUtils, FunLabyUtils,
  UnitEditorIntf, FunLabyEditConsts, NewUnit, EditParameters;

type
  TFormEditUnits = class(TForm)
    LabelUnits: TLabel;
    ListBoxUnits: TListBox;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonAddExisting: TButton;
    ButtonAddNew: TButton;
    ButtonRemove: TButton;
    ButtonEditParams: TButton;
    OpenUnitDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAddExistingClick(Sender: TObject);
    procedure ButtonAddNewClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonEditParamsClick(Sender: TObject);
  private
    { D�clarations priv�es }
    MasterFile : TMasterFile;
    Filters : TStrings;

    procedure AddUnitFilter(const Filter, GUID; var Continue : boolean);

    procedure AddUnitFileDesc(const UnitFileDesc : TUnitFileDesc);
    procedure DeleteUnitFileDesc(Index : integer);
    procedure AddUnitFile(const FileName : TFileName; const GUID : TGUID);
  public
    { D�clarations publiques }
    class function EditUnits(AMasterFile : TMasterFile) : boolean;
  end;

implementation

{$R *.dfm}

{*
  Ajoute un filtre d'unit� � la liste des filtres
  @param Filter     Filtre de fichier
  @param GUID       GUID du type de fichier
  @param Continue   Position � False pour interrompre l'�num�ration
*}
procedure TFormEditUnits.AddUnitFilter(const Filter, GUID;
  var Continue : boolean);
begin
  Filters.Add(string(Filter));
end;

{*
  Ajoute � la list box les informations d'un descripteur de fichier
  @param UnitFileDesc   Descripteur de fichier � ajouter
*}
procedure TFormEditUnits.AddUnitFileDesc(const UnitFileDesc : TUnitFileDesc);
var DescPtr : PUnitFileDesc;
begin
  if ListBoxUnits.Items.IndexOf(UnitFileDesc.HRef) >= 0 then
  begin
    ShowDialog(sDuplicateUnitTitle,
      Format(sDuplicateUnit, [UnitFileDesc.HRef]), dtError);
    exit;
  end;

  New(DescPtr);
  Initialize(DescPtr^);
  DescPtr^ := UnitFileDesc;

  ListBoxUnits.Items.AddObject(UnitFileDesc.HRef, TObject(DescPtr));
end;

{*
  Supprime un �l�ment de la list box des fichiers unit�
  @param Index   Index de l'�l�ment � supprimer
*}
procedure TFormEditUnits.DeleteUnitFileDesc(Index : integer);
var DescPtr : PUnitFileDesc;
begin
  DescPtr := PUnitFileDesc(ListBoxUnits.Items.Objects[Index]);
  Finalize(DescPtr^);

  ListBoxUnits.Items.Delete(Index);
end;

{*
  Ajoute un fichier unit�
  @param FileName   Nom du fichier
  @param GUID       GUID du type de fichier
*}
procedure TFormEditUnits.AddUnitFile(const FileName : TFileName;
  const GUID : TGUID);
var UnitFileDesc : TUnitFileDesc;
begin
  UnitFileDesc.GUID := GUID;
  UnitFileDesc.HRef := MasterFile.MakeHRef(FileName, fUnitsDir);

  AddUnitFileDesc(UnitFileDesc);
end;

{*
  Propose � l'utilisateur de modifier les unit�s d'un projet FunLabyrinthe
  Le fichier ma�tre doit avoir �t� sauvegard� avant l'appel � EditUnits, de
  sorte que les modifications �ventuelles soient assur�es, et �tre s�r qu'un
  nom de fichier est disponible.
  Lorsqu'EditUnits renvoie True, le ma�tre doit �tre d�charg� et recharg�
  ensuite.
  @param MasterFile   Fichier ma�tre dont modifier les unit�s
  @return True si les unit�s ont �t� modifi�es, False sinon
*}
class function TFormEditUnits.EditUnits(AMasterFile : TMasterFile) : boolean;
var I : integer;
    UnitFileDescs : TUnitFileDescs;
begin
  // Initialisations et contr�les de validit� d'appel
  Result := False;
  if AMasterFile.FileName = '' then
    exit;

  with Create(Application) do
  try
    // Lister les unit�s pr�sentes dans le fichier ma�tre
    MasterFile := AMasterFile;
    MasterFile.GetUnitFileDescs(UnitFileDescs);
    for I := 0 to Length(UnitFileDescs)-1 do
      AddUnitFileDesc(UnitFileDescs[I]);

    ListBoxUnits.ItemIndex := 0;

    // Affichage de la bo�te de dialogue
    if ShowModal <> mrOK then
      exit;

    // Reformer le tableau des descripteurs
    SetLength(UnitFileDescs, ListBoxUnits.Items.Count);
    for I := 0 to Length(UnitFileDescs)-1 do
    begin
      UnitFileDescs[I] := PUnitFileDesc(ListBoxUnits.Items.Objects[0])^;
      DeleteUnitFileDesc(0);
    end;

    // Enregistrer le fichier ma�tre
    MasterFile.Save(UnitFileDescs);
    Result := True;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnCreate de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.FormCreate(Sender: TObject);
var I : integer;
    Filter : string;
begin
  // Lister les filtres d'unit�
  Filters := TStringList.Create;
  UnitFilters.ForEach(AddUnitFilter);

  ButtonAddExisting.Enabled := Filters.Count > 0;
  OpenUnitDialog.InitialDir := fUnitsDir;

  if Filters.Count > 0 then
  begin
    TStringList(Filters).Sorted := True;

    Filter := Filters[0];
    for I := 1 to Filters.Count-1 do
      Filter := Filter + '|' + Filters[I];
    OpenUnitDialog.Filter := Filter;
  end;

  // V�rifier qu'il y a au moins un cr�ateur d'unit�s
  ButtonAddNew.Enabled := not UnitCreators.IsEmpty;
end;

{*
  Gestionnaire d'�v�nement OnDstroy de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.FormDestroy(Sender: TObject);
begin
  Filters.Free;
end;

{*
  Gestionnaire d'�v�nement pour ajouter une unit� existante
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.ButtonAddExistingClick(Sender: TObject);
begin
  with OpenUnitDialog do
    if Execute then
      AddUnitFile(FileName, UnitFilters.GUID[Filters[FilterIndex-1]]);
end;

{*
  Gestionnaire d'�v�nement pour ajouter une nouvelle unit�
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.ButtonAddNewClick(Sender: TObject);
var FileName : TFileName;
    GUID : TGUID;
begin
  if TFormCreateNewUnit.NewUnit(FileName, GUID) then
    AddUnitFile(FileName, GUID);
end;

{*
  Gestionnaire d'�v�nement pour retirer une unit�
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.ButtonRemoveClick(Sender: TObject);
begin
  with ListBoxUnits do
  begin
    if ItemIndex < 0 then
      exit;

    // Garder toujours au moins une unit�
    if Items.Count = 1 then
      ShowDialog(sRemoveUnitTitle, sCantRemoveLastUnit, dtError) else

    // Demander confirmation � l'utilisateur
    if ShowDialog(sRemoveUnitTitle,
        Format(sConfirmRemoveUnit, [Items[ItemIndex]]),
        dtConfirmation, dbOKCancel) = drOK then
      DeleteUnitFileDesc(ItemIndex);
  end;
end;

{*
  Gestionnaire d'�v�nement pour modifier les param�tres d'une unit�
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.ButtonEditParamsClick(Sender: TObject);
begin
  with ListBoxUnits do if ItemIndex >= 0 then
    TFormParameters.EditUnitParams(
      PUnitFileDesc(Items.Objects[ItemIndex]).Params);
end;

end.
