unit ReplacementScrew;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormNewCase = class(TForm)
    BoutonHerbe: TRadioButton;
    ImageHerbe: TImage;
    BoutonEau: TRadioButton;
    ImageEau: TImage;
    BoutonMur: TRadioButton;
    ImageMur: TImage;
    BoutonTrou: TRadioButton;
    ImageTrou: TImage;
    BoutonBlocArgent: TRadioButton;
    ImageBlocArgent: TImage;
    BoutonBlocOr: TRadioButton;
    ImageBlocOr: TImage;
    BoutonNord: TRadioButton;
    ImageNord: TImage;
    BoutonEst: TRadioButton;
    ImageEst: TImage;
    BoutonSud: TRadioButton;
    ImageSud: TImage;
    BoutonOuest: TRadioButton;
    ImageOuest: TImage;
    BoutonTeleporteur: TRadioButton;
    ImageTeleporteur: TImage;
    BoutonBouton: TRadioButton;
    ImageBouton: TImage;
    BoutonBoutonEnfonce: TRadioButton;
    ImageBoutonEnfonce: TImage;
    BoutonTresor: TRadioButton;
    ImageTresor: TImage;
    BoutonBouee: TRadioButton;
    ImageBouee: TImage;
    BoutonPlanche: TRadioButton;
    ImagePlanche: TImage;
    BoutonCleArgent: TRadioButton;
    ImageCleArgent: TImage;
    BoutonCleOr: TRadioButton;
    ImageCleOr: TImage;
    BoutonOK: TBitBtn;
    BoutonAnnuler: TBitBtn;
    ImageFauxMur: TImage;
    ImageBarque: TImage;
    BoutonFauxMur: TRadioButton;
    BoutonBarque: TRadioButton;
    ImageCarrefour: TImage;
    BoutonCarrefour: TRadioButton;
    ImageMoulinDirect: TImage;
    BoutonMoulinDirect: TRadioButton;
    ImageMoulinIndirect: TImage;
    BoutonMoulinIndirect: TRadioButton;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function NewCase : integer;
  end;

var
  FormNewCase: TFormNewCase;

implementation

uses EditLabyrinthe3, EditLabyrinthe6, EditLabyrintheMain, EditLabyrinthe10;

{$R *.DFM}

function TFormNewCase.NewCase : integer;
var I : integer;
begin
  Result := 0;
  if ShowModal = mrCancel then exit;
  for I := 0 to ComponentCount-1 do
    if (Components[I] is TRadioButton) and
       (Components[I] as TRadioButton).Checked then
         Result := Components[I].Tag;
  if Result = 3 then
  begin
    if FormTeleporteur.ShowModal = mrCancel then Result := 0 else
      Result := FormTeleporteur.EditNumero.Value+96;
  end else if Result = 4 then
  begin
    if FormBouton.ShowModal = mrCancel then Result := 0 else
    begin
      Result := FormBouton.EditNumero.Value;
      if (Result <= 15) then inc (Result, 32) else inc (Result, 145);
    end;
  end else if Result = 5 then
  begin
    if FormBarque.ShowModal = mrCancel then Result := 0 else
      Result := FormBarque.EditNumero.Value+192;
  end;
end;

procedure TFormNewCase.FormCreate(Sender: TObject);
var F : TFormPrincipale;
begin
  F := FormPrincipale;
  ImageHerbe         .Picture.Assign(F.ImageHerbe         .Picture);
  ImageEau           .Picture.Assign(F.ImageEau           .Picture);
  ImageMur           .Picture.Assign(F.ImageMur           .Picture);
  ImageTrou          .Picture.Assign(F.ImageTrou          .Picture);
  ImageBlocArgent    .Picture.Assign(F.ImageBlocArgent    .Picture);
  ImageBlocOr        .Picture.Assign(F.ImageBlocOr        .Picture);
  ImageNord          .Picture.Assign(F.ImageNord          .Picture);
  ImageEst           .Picture.Assign(F.ImageEst           .Picture);
  ImageSud           .Picture.Assign(F.ImageSud           .Picture);
  ImageOuest         .Picture.Assign(F.ImageOuest         .Picture);
  ImageCarrefour     .Picture.Assign(F.ImageCarrefour     .Picture);
  ImageMoulinDirect  .Picture.Assign(F.ImageMoulinDirect  .Picture);
  ImageMoulinIndirect.Picture.Assign(F.ImageMoulinIndirect.Picture);
  ImageFauxMur       .Picture.Assign(F.ImageFauxMur       .Picture);
  ImageBarque        .Picture.Assign(F.ImageBarque        .Picture);
  ImageTeleporteur   .Picture.Assign(F.ImageTeleporteur   .Picture);
  ImageBouton        .Picture.Assign(F.ImageBouton        .Picture);
  ImageBoutonEnfonce .Picture.Assign(F.ImageBoutonEnfonce .Picture);
  ImageTresor        .Picture.Assign(F.ImageTresor        .Picture);
  ImageBouee         .Picture.Assign(F.ImageBouee         .Picture);
  ImagePlanche       .Picture.Assign(F.ImagePlanche       .Picture);
  ImageCleArgent     .Picture.Assign(F.ImageCleArgent     .Picture);
  ImageCleOr         .Picture.Assign(F.ImageCleOr         .Picture);
end;

end.
