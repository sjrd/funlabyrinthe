unit SimpleSquareNew;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FunLabyUtils, SimpleSquaresUtils, SimpleSquaresEffects,
  SdDialogs;

resourcestring
  sInvalidIDTitle = 'ID incorrect';
  sIDMustNotBeEmpty = 'Vous devez spécifier un identifiant';
  sInvalidID = 'L''identifiant doit être composé uniquement de lettres sans '+
    'accents et/ou de chiffres (et ne pas commencer par un chiffre)';
  sDuplicateID = 'L''identifiant spécifié est déjà utilisé';

type
  TComponentIDExistsCallback = function(
    const ID: TComponentID): Boolean of object;

  {*
    Boîte de dialogue de création d'un nouveau composant de case simple
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
    ImagesMaster: TImagesMaster;          /// Maître d'images
    IDExists: TComponentIDExistsCallback; /// Teste si un ID existe déjà

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
  @param SquareClass   Classe de composant à ajouter
*}
procedure TFormNewSimpleSquare.AddSquareClass(SquareClass: TSimpleSquareClass);
begin
  ListBoxComponentClass.Items.AddObject(SquareClass.ClassTitle,
    TObject(SquareClass));
end;

{*
  Affiche la boîte de dialogue et crée un nouveau composant
  @return Composant créé, ou nil si annulé
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
  Demande à l'utilisateur la création d'un nouveau composant
  @param AImagesMaster   Maître d'images
  @return Composant créé, ou nil si annulé
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
  Création de la fiche
  @param Sender   Objet qui a déclenché l'événement
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
  Gestionnaire d'événement OnDblClick de la liste des types de composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormNewSimpleSquare.ListBoxComponentClassDblClick(Sender: TObject);
begin
  if ListBoxComponentClass.ItemIndex >= 0 then
    ModalResult := mrOK;
end;

{*
  Gestionnaire d'événement OnClick du bouton OK
  @param Sender   Objet qui a déclenché l'événement
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

