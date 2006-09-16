unit AddMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScUtils, ScExtra, FunLabyUtils, FilesUtils,
  MapFiles;

resourcestring
  sWrongIDFormatTitle = 'Format d''ID non valide';
  sWrongIDFormat = 'L''ID ne doit �tre constitu� que de lettres non '+
    'accentu�es et de chiffres';
  sUnfilledFileNameTitle = 'Nom de fichier non rempli';
  sUnfilledFileName = 'Vous devez sp�cifier un nom de fichier';

type
  TFormAddMap = class(TForm)
    LabelID: TLabel;
    EditID: TEdit;
    RadioExistingMap: TRadioButton;
    EditFileName: TEdit;
    ButtonBrowse: TSpeedButton;
    RadioNewMap: TRadioButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    OpenDialog: TOpenDialog;
    procedure ButtonOKClick(Sender: TObject);
    procedure RadioNewMapClick(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure RadioExistingMapClick(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    class function AddMap(MasterFile : TMasterFile) : TComponentID;
  end;

const {don't localize}
  FLMMIMEType = 'application/flm';

var
  FormAddMap: TFormAddMap;

implementation

{$R *.dfm}

{*
  Affiche la bo�te de dialogue d'ajout de carte
  @param MasterFile   Fichier ma�tre
  @return ID de la nouvelle carte
*}
class function TFormAddMap.AddMap(MasterFile : TMasterFile) : TComponentID;
begin
  with Create(Application) do
  try
    OpenDialog.InitialDir := fMapsDir;
    if ShowModal <> mrOK then Result := '' else
    begin
      Result := EditID.Text;
      if RadioExistingMap.Checked then
        MasterFile.AddMapFile(Result, FLMMIMEType, EditFileName.Text)
      else
        Result := '';
    end;
  finally
    Release;
  end;
end;

procedure TFormAddMap.RadioExistingMapClick(Sender: TObject);
begin
  EditFileName.Enabled := True;
  ButtonBrowse.Enabled := True;
end;

procedure TFormAddMap.ButtonBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditFileName.Text := OpenDialog.FileName;
end;

procedure TFormAddMap.RadioNewMapClick(Sender: TObject);
begin
  EditFileName.Enabled := False;
  ButtonBrowse.Enabled := False;
end;

procedure TFormAddMap.ButtonOKClick(Sender: TObject);
begin
  if not CorrectIdentifier(EditID.Text) then
    ShowDialog(sWrongIDFormatTitle, sWrongIDFormatTitle, dtError)
  else if RadioExistingMap.Checked and (EditFileName.Text = '') then
    ShowDialog(sUnfilledFileNameTitle, sUnfilledFileName, dtError)
  else
    ModalResult := mrOK;
end;

end.

