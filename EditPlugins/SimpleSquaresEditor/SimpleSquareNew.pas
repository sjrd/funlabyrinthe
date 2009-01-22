unit SimpleSquareNew;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FunLabyUtils, SimpleSquaresUtils, SimpleSquaresEffects,
  SdDialogs;

resourcestring
  sInvalidIDTitle = 'ID incorrect';
  sIDMustNotBeEmpty = 'Vous devez sp�cifier un identifiant';
  sInvalidID = 'L''identifiant doit �tre compos� uniquement de lettres sans '+
    'accents et/ou de chiffres (et ne pas commencer par un chiffre)';
  sDuplicateID = 'L''identifiant sp�cifi� est d�j� utilis�';

type
  TComponentIDExistsCallback = function(
    const ID: TComponentID): Boolean of object;

  {*
    Bo�te de dialogue de cr�ation d'un nouveau composant de case simple
    @author sjrd
    @version 5.0
  *}
  TFormNewSimpleSquare = class(TForm)
    LabelComponentClass: TLabel;
    LabelID: TLabel;
    LabelName: TLabel;
    EditID: TEdit;
    EditName: TEdit;
    ListBoxComponentClass: TListBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ListBoxComponentClassDblClick(Sender: TObject);
  private
    ImagesMaster: TImagesMaster;          /// Ma�tre d'images
    IDExists: TComponentIDExistsCallback; /// Teste si un ID existe d�j�

    procedure AddSquareClass(SquareClass: TSimpleSquareClass);
    function InternalNewSimpleSquare: TSimpleSquare;
  public
    class function NewSimpleSquare(AImagesMaster: TImagesMaster;
      AIDExists: TComponentIDExistsCallback): TSimpleSquare;
  end;

implementation

{$R *.dfm}

{----------------------------}
{ TFormNewSimpleSquare class }
{----------------------------}

{*
  Ajoute une classe de composant de case possible
  @param SquareClass   Classe de composant � ajouter
*}
procedure TFormNewSimpleSquare.AddSquareClass(SquareClass: TSimpleSquareClass);
begin
  ListBoxComponentClass.Items.AddObject(SquareClass.ClassTitle,
    TObject(SquareClass));
end;

{*
  Affiche la bo�te de dialogue et cr�e un nouveau composant
  @return Composant cr��, ou nil si annul�
*}
function TFormNewSimpleSquare.InternalNewSimpleSquare: TSimpleSquare;
var
  SquareClass: TSimpleSquareClass;
begin
  ListBoxComponentClass.ItemIndex := 0;

  if ShowModal <> mrOK then
  begin
    Result := nil;
    Exit;
  end;

  SquareClass := TSimpleSquareClass(
    ListBoxComponentClass.Items.Objects[ListBoxComponentClass.ItemIndex]);
  Result := SquareClass.Create(ImagesMaster, EditID.Text, EditName.Text);
end;

{*
  Demande � l'utilisateur la cr�ation d'un nouveau composant
  @param AImagesMaster   Ma�tre d'images
  @return Composant cr��, ou nil si annul�
*}
class function TFormNewSimpleSquare.NewSimpleSquare(
  AImagesMaster: TImagesMaster;
  AIDExists: TComponentIDExistsCallback): TSimpleSquare;
begin
  with Create(nil) do
  try
    ImagesMaster := AImagesMaster;
    IDExists := AIDExists;
    Result := InternalNewSimpleSquare;
  finally
    Free;
  end;
end;

{*
  Cr�ation de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormNewSimpleSquare.FormCreate(Sender: TObject);
begin
  AddSquareClass(TSimpleEffect);
  AddSquareClass(TSimpleButton);
  AddSquareClass(TSimpleSwitch);

  AddSquareClass(TSimpleObject);

  AddSquareClass(TSimpleObstacle);
end;

{*
  Gestionnaire d'�v�nement OnDblClick de la liste des types de composant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormNewSimpleSquare.ListBoxComponentClassDblClick(Sender: TObject);
begin
  if ListBoxComponentClass.ItemIndex >= 0 then
    ModalResult := mrOK;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton OK
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormNewSimpleSquare.ButtonOKClick(Sender: TObject);
var
  ID: string;
begin
  ID := EditID.Text;

  if ID = '' then
  begin
    ShowDialog(sInvalidIDTitle, sIDMustNotBeEmpty, dtError);
    Exit;
  end;

  if not IsValidIdent(ID) then
  begin
    ShowDialog(sInvalidIDTitle, sInvalidID, dtError);
    Exit;
  end;

  if IDExists(ID) then
  begin
    ShowDialog(sInvalidIDTitle, sDuplicateID, dtError);
    Exit;
  end;

  ModalResult := mrOK;
end;

end.

