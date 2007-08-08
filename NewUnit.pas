unit NewUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SdDialogs, FilesUtils, UnitEditorIntf,
  FunLabyEditConsts;

type
  TFormCreateNewUnit = class(TForm)
    LabelUnitType: TLabel;
    ListBoxUnitType: TListBox;
    LabelDescription: TLabel;
    MemoDescription: TMemo;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    procedure ListBoxUnitTypeClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class function NewUnit(MasterFile : TMasterFile) : IUnitEditor50;
  end;

var
  FormCreateNewUnit: TFormCreateNewUnit;

implementation

{$R *.dfm}

{*
  Affiche la boîte de dialogue de création d'une nouvelle unité
  @param MasterFile   Fichier maître
  @return Éditeur de l'unité
*}
class function TFormCreateNewUnit.NewUnit(
  MasterFile : TMasterFile) : IUnitEditor50;
var Creators : TUnitCreatorArray;
    I : integer;
begin
  Result := nil;
  GetUnitCreators(Creators);

  if Length(Creators) = 0 then
  begin
    ShowDialog(sNoUnitCreatorTitle, sNoUnitCreator, dtError);
    exit;
  end;

  with Create(Application) do
  try
    for I := 0 to Length(Creators)-1 do
      ListBoxUnitType.Items.AddObject(Creators[I].Title,
        TObject(@Creators[I]));

    ListBoxUnitType.ItemIndex := 0;
    ListBoxUnitTypeClick(nil);

    if ShowModal = mrOK then
      Result := Creators[ListBoxUnitType.ItemIndex].CreateProc(MasterFile);
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
  with ListBoxUnitType do
    MemoDescription.Text := PUnitCreator(Items.Objects[ItemIndex]).Description;
end;

end.

