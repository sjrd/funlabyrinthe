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
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    class function NewUnit(MasterFile : TMasterFile) : IUnitEditor50;
  end;

var
  FormCreateNewUnit: TFormCreateNewUnit;

implementation

{$R *.dfm}

{*
  Affiche la bo�te de dialogue de cr�ation d'une nouvelle unit�
  @param MasterFile   Fichier ma�tre
  @return �diteur de l'unit�
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
  Gestionnaire d'�v�nement OnClick de la list box des types d'unit�s
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormCreateNewUnit.ListBoxUnitTypeClick(Sender: TObject);
begin
  with ListBoxUnitType do
    MemoDescription.Text := PUnitCreator(Items.Objects[ItemIndex]).Description;
end;

end.

