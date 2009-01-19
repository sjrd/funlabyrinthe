unit EditUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScLists, SdDialogs, FilesUtils, FunLabyUtils,
  SourceEditors, FunLabyEditConsts, EditParameters;

type
  TFormEditUnits = class(TForm)
    LabelUnits: TLabel;
    ListBoxUnits: TListBox;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonAdd: TButton;
    ButtonRemove: TButton;
    ButtonEditParams: TButton;
    OpenUnitDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonEditParamsClick(Sender: TObject);
  private
    { Déclarations privées }
    MasterFile: TMasterFile;
    Filters: TStrings;

    procedure AddUnitFilter(const Filter, GUID; var Continue: Boolean);

    procedure AddUnitFileDesc(const UnitFileDesc: TUnitFileDesc);
    procedure DeleteUnitFileDesc(Index: Integer);
    procedure AddUnitFile(const FileName: TFileName; const GUID: TGUID);
  public
    { Déclarations publiques }
    class function EditUnits(AMasterFile: TMasterFile): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Ajoute un filtre d'unité à la liste des filtres
  @param Filter     Filtre de fichier
  @param GUID       GUID du type de fichier
  @param Continue   Position à False pour interrompre l'énumération
*}
procedure TFormEditUnits.AddUnitFilter(const Filter, GUID;
  var Continue: Boolean);
begin
  Filters.Add(string(Filter));
end;

{*
  Ajoute à la list box les informations d'un descripteur de fichier
  @param UnitFileDesc   Descripteur de fichier à ajouter
*}
procedure TFormEditUnits.AddUnitFileDesc(const UnitFileDesc: TUnitFileDesc);
var
  DescPtr: PUnitFileDesc;
begin
  if ListBoxUnits.Items.IndexOf(UnitFileDesc.HRef) >= 0 then
  begin
    ShowDialog(sDuplicateUnitTitle,
      Format(sDuplicateUnit, [UnitFileDesc.HRef]), dtError);
    Exit;
  end;

  New(DescPtr);
  DescPtr^ := UnitFileDesc;

  ListBoxUnits.Items.AddObject(UnitFileDesc.HRef, TObject(DescPtr));
end;

{*
  Supprime un élément de la list box des fichiers unité
  @param Index   Index de l'élément à supprimer
*}
procedure TFormEditUnits.DeleteUnitFileDesc(Index: Integer);
var
  DescPtr: PUnitFileDesc;
begin
  DescPtr := PUnitFileDesc(ListBoxUnits.Items.Objects[Index]);
  Dispose(DescPtr);

  ListBoxUnits.Items.Delete(Index);
end;

{*
  Ajoute un fichier unité
  @param FileName   Nom du fichier
  @param GUID       GUID du type de fichier
*}
procedure TFormEditUnits.AddUnitFile(const FileName: TFileName;
  const GUID: TGUID);
var
  UnitFileDesc: TUnitFileDesc;
begin
  UnitFileDesc.GUID := GUID;
  UnitFileDesc.HRef := MasterFile.MakeHRef(FileName, fUnitsDir);

  AddUnitFileDesc(UnitFileDesc);
end;

{*
  Propose à l'utilisateur de modifier les unités d'un projet FunLabyrinthe
  Le fichier maître doit avoir été sauvegardé avant l'appel à EditUnits, de
  sorte que les modifications éventuelles soient assurées, et être sûr qu'un
  nom de fichier est disponible.
  Lorsqu'EditUnits renvoie True, le maître doit être déchargé et rechargé
  ensuite.
  @param MasterFile   Fichier maître dont modifier les unités
  @return True si les unités ont été modifiées, False sinon
*}
class function TFormEditUnits.EditUnits(AMasterFile: TMasterFile): Boolean;
var
  I: Integer;
  UnitFileDescs: TUnitFileDescs;
begin
  // Initialisations et contrôles de validité d'appel
  Result := False;
  if AMasterFile.FileName = '' then
    Exit;

  with Create(Application) do
  try
    // Lister les unités présentes dans le fichier maître
    MasterFile := AMasterFile;
    MasterFile.GetUnitFileDescs(UnitFileDescs);
    for I := 0 to Length(UnitFileDescs)-1 do
      AddUnitFileDesc(UnitFileDescs[I]);

    ListBoxUnits.ItemIndex := 0;

    // Affichage de la boîte de dialogue
    if ShowModal <> mrOk then
      Exit;

    // Reformer le tableau des descripteurs
    SetLength(UnitFileDescs, ListBoxUnits.Items.Count);
    for I := 0 to Length(UnitFileDescs)-1 do
    begin
      UnitFileDescs[I] := PUnitFileDesc(ListBoxUnits.Items.Objects[0])^;
      DeleteUnitFileDesc(0);
    end;

    // Enregistrer le fichier maître
    MasterFile.Save(UnitFileDescs);
    Result := True;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditUnits.FormCreate(Sender: TObject);
var
  I: Integer;
  Filter: string;
begin
  // Lister les filtres d'unité
  Filters := TStringList.Create;
  UnitFilters.ForEach(AddUnitFilter);

  //ButtonAddExisting.Enabled := Filters.Count > 0;
  OpenUnitDialog.InitialDir := fUnitsDir;

  if Filters.Count > 0 then
  begin
    TStringList(Filters).Sorted := True;

    Filter := Filters[0];
    for I := 1 to Filters.Count-1 do
      Filter := Filter + '|' + Filters[I];
    OpenUnitDialog.Filter := Filter;
  end;

  // Vérifier qu'il y a au moins un créateur d'unités
  ButtonAdd.Enabled := not SourceFileCreators.IsEmpty;
end;

{*
  Gestionnaire d'événement OnDstroy de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditUnits.FormDestroy(Sender: TObject);
begin
  Filters.Free;
end;

{*
  Gestionnaire d'événement pour ajouter une unité existante
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditUnits.ButtonAddClick(Sender: TObject);
begin
  with OpenUnitDialog do
    if Execute then
      AddUnitFile(FileName, UnitFilters.GUID[Filters[FilterIndex-1]]);
end;

{*
  Gestionnaire d'événement pour retirer une unité
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditUnits.ButtonRemoveClick(Sender: TObject);
begin
  with ListBoxUnits do
  begin
    if ItemIndex < 0 then
      Exit;

    // Garder toujours au moins une unité
    if Items.Count = 1 then
      ShowDialog(sRemoveUnitTitle, sCantRemoveLastUnit, dtError)
    else
    // Demander confirmation à l'utilisateur
    if ShowDialog(sRemoveUnitTitle,
      Format(sConfirmRemoveUnit, [Items[ItemIndex]]),
      dtConfirmation, dbOKCancel) = drOK then
      DeleteUnitFileDesc(ItemIndex);
  end;
end;

{*
  Gestionnaire d'événement pour modifier les paramètres d'une unité
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditUnits.ButtonEditParamsClick(Sender: TObject);
begin
  with ListBoxUnits do
    if ItemIndex >= 0 then
      TFormParameters.EditUnitParams(
        PUnitFileDesc(Items.Objects[ItemIndex]).Params);
end;

end.

