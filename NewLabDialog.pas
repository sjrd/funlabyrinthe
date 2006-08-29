unit NewLabDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin, ExtCtrls, ScUtils;

type
  TFormNouveau = class(TForm)
    LabelColonnes: TLabel;
    LabelLignes: TLabel;
    LabelEtages: TLabel;
    EditColonnes: TSpinEdit;
    EditLignes: TSpinEdit;
    EditEtages: TSpinEdit;
    BoutonOK: TBitBtn;
    BoutonAnnuler: TBitBtn;
    GroupeTypeLab: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Perso : boolean;
    function QueryInfos : boolean;
  end;

var
  FormNouveau: TFormNouveau;

implementation

{$R *.DFM}

function TFormNouveau.QueryInfos : boolean;
begin
  GroupeTypeLab.ItemIndex := 0;
  GroupeTypeLab.Enabled := FileExists(Dir+'NouvLab.dll');
  Result := (ShowModal = mrOK);
  Perso := (GroupeTypeLab.ItemIndex = 1);
end;

procedure TFormNouveau.FormShow(Sender: TObject);
begin
  EditColonnes.Value := 1;
  EditLignes.Value := 1;
  EditEtages.Value := 1;
  EditColonnes.SetFocus;
end;

end.
