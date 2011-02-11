unit EditUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScLists, ScStrUtils, SdDialogs, FilesUtils,
  FunLabyUtils, SourceEditors, FunLabyEditConsts, EditParameters;

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
    { D�clarations priv�es }
    MasterFile: TMasterFile;
    Filters: TStrings;

    procedure AddUnitFilter(const Filter, Extension; var Continue: Boolean);

    procedure AddUnitFileDesc(const UnitFileDesc: TUnitFileDesc);
    procedure DeleteUnitFileDesc(Index: Integer);
    procedure AddUnitFile(const FileName: TFileName);
  public
    { D�clarations publiques }
    class function EditUnits(AMasterFile: TMasterFile): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Ajoute un filtre d'unit� � la liste des filtres
  @param Filter      Filtre de fichier
  @param Extension   Extension du type de fichier
  @param Continue    Position � False pour interrompre l'�num�ration
*}
procedure TFormEditUnits.AddUnitFilter(const Filter, Extension;
  var Continue: Boolean);
begin
  Filters.Add(string(Filter));
end;

{*
  Ajoute � la list box les informations d'un descripteur de fichier
  @param UnitFileDesc   Descripteur de fichier � ajouter
*}
procedure TFormEditUnits.AddUnitFileDesc(const UnitFileDesc: TUnitFileDesc);
var
  DescPtr: PUnitFileDesc;
begin
  if ListBoxUnits.Items.IndexOf(UnitFileDesc.HRef) >= 0 then
  begin
    ShowDialog(SDuplicateUnitTitle,
      Format(SDuplicateUnit, [UnitFileDesc.HRef]), dtError);
    Exit;
  end;

  New(DescPtr);
  DescPtr^ := UnitFileDesc;

  ListBoxUnits.Items.AddObject(UnitFileDesc.HRef, TObject(DescPtr));
end;

{*
  Supprime un �l�ment de la list box des fichiers unit�
  @param Index   Index de l'�l�ment � supprimer
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
  Ajoute un fichier unit�
  @param FileName   Nom du fichier
*}
procedure TFormEditUnits.AddUnitFile(const FileName: TFileName);
var
  UnitFileDesc: TUnitFileDesc;
begin
  UnitFileDesc.HRef := MasterFile.MakeHRef(FileName, UnitsDir);

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
class function TFormEditUnits.EditUnits(AMasterFile: TMasterFile): Boolean;
var
  I: Integer;
  UnitFileDescs: TUnitFileDescs;
begin
  // Initialisations et contr�les de validit� d'appel
  Result := False;
  if AMasterFile.FileName = '' then
    Exit;

  with Create(Application) do
  try
    // Lister les unit�s pr�sentes dans le fichier ma�tre
    MasterFile := AMasterFile;
    MasterFile.GetUnitFileDescs(UnitFileDescs);
    for I := 0 to Length(UnitFileDescs)-1 do
      AddUnitFileDesc(UnitFileDescs[I]);

    ListBoxUnits.ItemIndex := 0;

    // Affichage de la bo�te de dialogue
    if ShowModal <> mrOk then
      Exit;

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
var
  I: Integer;
  Filter, AllExtensions: string;
begin
  // Lister les filtres d'unit�
  Filters := TStringList.Create;
  UnitFilters.ForEach(AddUnitFilter);

  OpenUnitDialog.InitialDir := JoinPath([FunLabyAppDataDir, UnitsDir]);

  if Filters.Count = 0 then
    ButtonAdd.Enabled := False
  else
  begin
    TStringList(Filters).Sorted := True;

    Filter := '';
    AllExtensions := '';

    for I := 0 to Filters.Count-1 do
    begin
      Filter := Filter + '|' + Filters[I];
      AllExtensions := AllExtensions + ';' + GetLastToken(Filters[I], '|');
    end;

    AllExtensions[1] := '|';
    Filter := SAllUnitTypes + AllExtensions + Filter;

    OpenUnitDialog.Filter := Filter;
  end;
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
procedure TFormEditUnits.ButtonAddClick(Sender: TObject);
begin
  with OpenUnitDialog do
    if Execute then
      AddUnitFile(FileName);
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
      Exit;

    // Garder toujours au moins une unit�
    if Items.Count = 1 then
      ShowDialog(SRemoveUnitTitle, SCantRemoveLastUnit, dtError)
    else
    // Demander confirmation � l'utilisateur
    if ShowDialog(SRemoveUnitTitle,
      Format(SConfirmRemoveUnit, [Items[ItemIndex]]),
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
  with ListBoxUnits do
    if ItemIndex >= 0 then
      TFormParameters.EditUnitParams(
        PUnitFileDesc(Items.Objects[ItemIndex]).Params);
end;

end.

