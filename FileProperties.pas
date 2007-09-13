{*
  Bo�te de dialogue des propri�t�s d'un fichier
  L'unit� FileProperties d�finit une bo�te de dialogue des propri�t�s d'un
  fichier.
  @author sjrd
  @version 5.0
*}
unit FileProperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SdDialogs, FilesUtils;

resourcestring
  sUnfilledTitleTitle = 'Titre non compl�t�';
  sUnfilledTitle = 'Vous devez indiquer le titre';

type
  {*
    Bo�te de dialogue des propri�t�s d'un fichier
    @author sjrd
    @version 5.0
  *}
  TFormFileProperties = class(TForm)
    LabelTitle: TLabel;
    EditTitle: TEdit;
    LabelDescription: TLabel;
    EditDescription: TMemo;
    LabelDifficulty: TLabel;
    EditDifficulty: TEdit;
    LabelAuthor: TLabel;
    EditAuthor: TEdit;
    LabelAuthorID: TLabel;
    EditAuthorID: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure EditAuthorIDExit(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    class function ManageProperties(MasterFile: TMasterFile): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Affiche et permet de modifier les propri�t�s du fichier
  @param MasterFile   Fichier concern�
  @return True si une modification a eu lieu, False sinon
*}
class function TFormFileProperties.ManageProperties(
  MasterFile: TMasterFile): Boolean;
begin
  with Create(Application) do
  try
    EditTitle.Text := MasterFile.Title;
    EditDescription.Text := MasterFile.Description;
    EditDifficulty.Text := MasterFile.Difficulty;
    EditAuthor.Text := MasterFile.Author;
    EditAuthorID.Text := IntToStr(MasterFile.AuthorID);

    if ShowModal <> mrOk then
      Result := False
    else
    begin
      MasterFile.Title := EditTitle.Text;
      MasterFile.Description := EditDescription.Text;
      MasterFile.Difficulty := EditDifficulty.Text;
      MasterFile.Author := EditAuthor.Text;
      MasterFile.AuthorID := StrToInt(EditAuthorID.Text);

      Result := True;
    end;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnExit de la zone d'�dition de l'ID de l'auteur
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormFileProperties.EditAuthorIDExit(Sender: TObject);
begin
  EditAuthorID.Text := IntToStr(StrToIntDef(EditAuthorID.Text, 0));
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton OK
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormFileProperties.ButtonOKClick(Sender: TObject);
begin
  EditAuthorIDExit(Sender);
  if EditTitle.Text = '' then
    ShowDialog(sUnfilledTitleTitle, sUnfilledTitle, dtError)
  else
    ModalResult := mrOk;
end;

end.

