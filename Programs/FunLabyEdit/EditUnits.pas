unit EditUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScUtils, ScLists, ScStrUtils, SdDialogs,
  FilesUtils, FunLabyUtils, SourceEditors, FunLabyEditConsts;

type
  TFormEditUnits = class(TForm)
    LabelAvailableUnits: TLabel;
    ButtonAddUnit: TSpeedButton;
    ButtonRemoveUnit: TSpeedButton;
    LabelUsedUnits: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ListBoxAvailableUnits: TListBox;
    ListBoxUsedUnits: TListBox;
    procedure ButtonAddUnitClick(Sender: TObject);
    procedure ButtonRemoveUnitClick(Sender: TObject);
  private
    MasterFile: TMasterFile;

    function InternalEditUnits(AMasterFile: TMasterFile): Boolean;

    procedure ListAvailableUnits;
    procedure ListAvailableUnitsIn(AvailableUnits: TStrings;
      const Dir: TFileName; const Pattern: string);

    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
  public
    class function EditUnits(AMasterFile: TMasterFile): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Propose � l'utilisateur de modifier les unit�s d'un projet FunLabyrinthe
  @param AMasterFile   Fichier ma�tre dont modifier les unit�s
  @return True si les unit�s ont �t� modifi�es, False sinon
*}
function TFormEditUnits.InternalEditUnits(AMasterFile: TMasterFile): Boolean;
begin
  Result := False;
  MasterFile := AMasterFile;

  // Prepare
  ListAvailableUnits;
  ListBoxUsedUnits.Items.Assign(MasterFile.UsedUnits);

  // Show dialog
  if ShowModal <> mrOk then
    Exit;

  // Save master file
  MasterFile.Save(ListBoxUsedUnits.Items);
  Result := True;
end;

{*
  Liste toutes les unit�s disponibles
  Remplit la liste ListBoxAvailableUnits avec toutes les unit�s disponibles pour
  le projet.
*}
procedure TFormEditUnits.ListAvailableUnits;
var
  AvailableUnits: TStrings;
begin
  AvailableUnits := ListBoxAvailableUnits.Items;

  AvailableUnits.BeginUpdate;
  try
    ListAvailableUnitsIn(AvailableUnits,
      JoinPath([LibraryPath, UnitsDir]), '*.scu');

    ListAvailableUnitsIn(AvailableUnits,
      JoinPath([MasterFile.ProjectDir, UnitsDir]), '*.scu');

    ListAvailableUnitsIn(AvailableUnits,
      JoinPath([Dir, UnitPackagesDir]), '*.bpl');
  finally
    AvailableUnits.EndUpdate;
  end;
end;

{*
  Liste toutes les unit�s disponibles dans un dossier donn�
  @param AvailableUnits   Liste dans laquelle lister les unit�s
  @param Dir              Dossier dans lequel chercher les unit�s
  @param Pattern          Pattern de nom de fichier
*}
procedure TFormEditUnits.ListAvailableUnitsIn(AvailableUnits: TStrings;
  const Dir: TFileName; const Pattern: string);
var
  UnitName: string;
begin
  IterateDir(Dir, Pattern,
    procedure(const FileName: TFileName; const SearchRec: TSearchRec)
    begin
      UnitName := ChangeFileExt(SearchRec.Name, '');

      if (AvailableUnits.IndexOf(UnitName) < 0) and
        (MasterFile.UsedUnits.IndexOf(UnitName) < 0) then
        AvailableUnits.Add(UnitName);
    end);
end;

{*
  D�place l'�l�ment s�lectionn� dans une autre liste
  @param List    Bo�te liste source
  @param Items   Liste d'�l�ments destination
*}
procedure TFormEditUnits.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count-1 downto 0 do
  begin
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
  end;
end;

{*
  Propose � l'utilisateur de modifier les unit�s d'un projet FunLabyrinthe
  Le fichier ma�tre doit avoir �t� sauvegard� avant l'appel � EditUnits, de
  sorte que les modifications �ventuelles soient assur�es, et �tre s�r qu'un
  nom de fichier est disponible.
  Lorsqu'EditUnits renvoie True, le ma�tre doit �tre d�charg� et recharg�
  ensuite.
  @param AMasterFile   Fichier ma�tre dont modifier les unit�s
  @return True si les unit�s ont �t� modifi�es, False sinon
*}
class function TFormEditUnits.EditUnits(AMasterFile: TMasterFile): Boolean;
begin
  Assert(AMasterFile.FileName <> '');

  with Create(Application) do
  try
    Result := InternalEditUnits(AMasterFile);
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement pour ajouter une unit� existante
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.ButtonAddUnitClick(Sender: TObject);
begin
  MoveSelected(ListBoxAvailableUnits, ListBoxUsedUnits.Items);
end;

{*
  Gestionnaire d'�v�nement pour retirer une unit�
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormEditUnits.ButtonRemoveUnitClick(Sender: TObject);
begin
  MoveSelected(ListBoxUsedUnits, ListBoxAvailableUnits.Items);
end;

end.

