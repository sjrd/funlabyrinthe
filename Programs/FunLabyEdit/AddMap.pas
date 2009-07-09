{*
  Bo�te de dialogue d'ajout d'une carte
  L'unit� AddMap d�finit une bo�te de dialogue d'ajout de carte.
  @author sjrd
  @version 5.0
*}
unit AddMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, StrUtils, ScUtils, ScDelphiLanguage, ScStrUtils,
  SdDialogs, FunLabyUtils, FilesUtils, Mask, MapBase;

resourcestring
  SWrongIDFormatTitle = 'Format d''ID non valide';
  SWrongIDFormat = 'L''ID ne doit �tre constitu� que de lettres non '+
    'accentu�es et de chiffres';
  SWrongDimensionsTitle = 'Dimensions incorrectes';
  SWrongDimensions = 'Les dimensions doivent �tre strictement positives';
  SWrongZoneSizeTitle = 'Taille de zone incorrecte';
  SWrongZoneSize = 'La taille de zone doit �tre strictement positive';
  SZoneSizeMustDivideDimensions =
    'La taille de zone doit �tre un diviseur des dimensions en X et en Y';

type
  {*
    Bo�te de dialogue d'ajout d'une carte
    @author sjrd
    @version 5.0
  *}
  TFormAddMap = class(TForm)
    LabelID: TLabel;
    EditID: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelDimensions: TLabel;
    EditDimensions: TMaskEdit;
    LabelZoneSize: TLabel;
    EditZoneSize: TMaskEdit;
    procedure ButtonOKClick(Sender: TObject);
  private
    { D�clarations priv�es }
    Dimensions: T3DPoint; /// Dimensions de la care
    ZoneSize: TPoint;     /// Dimensions d'une zone
  public
    { D�clarations publiques }
    class function AddMap(MasterFile: TMasterFile): TComponentID;
  end;

implementation

{$R *.dfm}

{*
  Affiche la bo�te de dialogue d'ajout de carte
  @param MasterFile   Fichier ma�tre
  @return ID de la nouvelle carte
*}
class function TFormAddMap.AddMap(MasterFile: TMasterFile): TComponentID;
begin
  with Create(Application) do
  try
    if ShowModal <> mrOk then
      Result := ''
    else
    begin
      Result := EditID.Text;
      TFormMapBase.GenerateBase(TMap.Create(MasterFile.Master, Result,
        Dimensions, ZoneSize.X, ZoneSize.Y));
    end;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton OK
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormAddMap.ButtonOKClick(Sender: TObject);
var
  Str: string;
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
    ShowDialog(SWrongIDFormatTitle, SWrongIDFormat, dtError);
    Exit;
  end;

  if (Dimensions.X <= 0) or (Dimensions.Y <= 0) or (Dimensions.Z <= 0) then
    ShowDialog(SWrongDimensionsTitle, SWrongDimensions, dtError)
  else if (ZoneSize.X <= 0) or (ZoneSize.Y <= 0) then
    ShowDialog(SWrongZoneSizeTitle, SWrongZoneSize, dtError)
  else if (Dimensions.X mod ZoneSize.X <> 0) or
    (Dimensions.Y mod ZoneSize.Y <> 0) then
    ShowDialog(SWrongZoneSizeTitle, SZoneSizeMustDivideDimensions, dtError)
  else
    ModalResult := mrOk;
end;

end.

