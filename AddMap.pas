{*
  Bo�te de dialogue d'ajout d'une carte
  L'unit� AddMap d�finit une bo�te de dialogue d'ajout de carte.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit AddMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, StrUtils, ScUtils, ScExtra, ScStrUtils,
  FunLabyUtils, FilesUtils, Mask, MapBase;

resourcestring
  sWrongIDFormatTitle = 'Format d''ID non valide';
  sWrongIDFormat = 'L''ID ne doit �tre constitu� que de lettres non '+
    'accentu�es et de chiffres';
  sUnfilledFileNameTitle = 'Nom de fichier non rempli';
  sUnfilledFileName = 'Vous devez sp�cifier un nom de fichier';
  sWrongDimensionsTitle = 'Dimensions incorrectes';
  sWrongDimensions = 'Les dimensions doivent �tre strictement positives';
  sWrongZoneSizeTitle = 'Taille de zone incorrecte';
  sWrongZoneSize = 'La taille de zone doit �tre strictement positive';
  sZoneSizeMustDivideDimensions =
    'La taille de zone doit �tre un diviseur des dimensions en X et en Y';

type
  {*
    Bo�te de dialogue d'ajout d'une carte
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
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
    LabelDimensions: TLabel;
    EditDimensions: TMaskEdit;
    LabelZoneSize: TLabel;
    EditZoneSize: TMaskEdit;
    procedure ButtonOKClick(Sender: TObject);
    procedure RadioNewMapClick(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure RadioExistingMapClick(Sender: TObject);
  private
    { D�clarations priv�es }
    Dimensions : T3DPoint; /// Dimensions de la care
    ZoneSize : TPoint;     /// Dimensions d'une zone
  public
    { D�clarations publiques }
    class function AddMap(MasterFile : TMasterFile) : TComponentID;
  end;

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
      begin
        MasterFile.AddMapFile(Result, EditFileName.Text);
      end else
      begin
        TFormMapBase.GenerateBase(
          MasterFile.AddNewMapFile(
            Result, Dimensions, ZoneSize.X, ZoneSize.Y).Map);
      end;
    end;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton radio d'une carte existante
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormAddMap.RadioExistingMapClick(Sender: TObject);
begin
  EditFileName.Enabled := True;
  ButtonBrowse.Enabled := True;

  EditDimensions.Enabled := False;
  EditZoneSize.Enabled := False;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Parcourir
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormAddMap.ButtonBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditFileName.Text := OpenDialog.FileName;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton radio d'une nouvelle carte
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormAddMap.RadioNewMapClick(Sender: TObject);
begin
  EditFileName.Enabled := False;
  ButtonBrowse.Enabled := False;

  EditDimensions.Enabled := True;
  EditZoneSize.Enabled := True;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton OK
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormAddMap.ButtonOKClick(Sender: TObject);
var Str : string;
begin
  { Don't localize strings in this method }

  Str := ReplaceStr(EditDimensions.Text, ' ', '0');
  Dimensions.X := StrToIntDef(GetXToken(Str, 'x', 1), 0);
  Dimensions.Y := StrToIntDef(GetXToken(Str, 'x', 2), 0);
  Dimensions.Z := StrToIntDef(GetXToken(Str, 'x', 3), 0);

  Str := ReplaceStr(EditZoneSize.Text, ' ', '0');
  ZoneSize.X := StrToIntDef(GetXToken(Str, 'x', 1), 0);
  ZoneSize.Y := StrToIntDef(GetXToken(Str, 'x', 2), 0);

  if not CorrectIdentifier(EditID.Text) then
  begin
    ShowDialog(sWrongIDFormatTitle, sWrongIDFormatTitle, dtError);
    exit;
  end;

  if RadioExistingMap.Checked then
  begin
    if EditFileName.Text = '' then
      ShowDialog(sUnfilledFileNameTitle, sUnfilledFileName, dtError)
    else
      ModalResult := mrOK;
  end else
  begin
    if (Dimensions.X <= 0) or (Dimensions.Y <= 0) or (Dimensions.Z <= 0) then
      ShowDialog(sWrongDimensionsTitle, sWrongDimensions, dtError)
    else if (ZoneSize.X <= 0) or (ZoneSize.Y <= 0) then
      ShowDialog(sWrongZoneSizeTitle, sWrongZoneSize, dtError)
    else if (Dimensions.X mod ZoneSize.X <> 0) or
            (Dimensions.Y mod ZoneSize.Y <> 0) then
      ShowDialog(sWrongZoneSizeTitle, sZoneSizeMustDivideDimensions, dtError)
    else
      ModalResult := mrOK;
  end;
end;

end.

