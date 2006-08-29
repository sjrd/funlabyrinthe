unit NouvLabMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ScUtils;

type
  TFormParamsLab = class(TForm)
    BoutonOK: TBitBtn;
    BoutonAnnuler: TBitBtn;
    LabelBord: TLabel;
    ComboBord: TComboBox;
    BoutonAide: TBitBtn;
    GroupeType: TGroupBox;
    BoutonTypesIdentiques: TRadioButton;
    BoutonTypesDifferents: TRadioButton;
    LabelTerrain1: TLabel;
    LabelTerrain2: TLabel;
    LabelTerrain3: TLabel;
    ComboTerrain1: TComboBox;
    ComboTerrain2: TComboBox;
    ComboTerrain3: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BoutonTypesIdentiquesClick(Sender: TObject);
    procedure BoutonTypesDifferentsClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.DFM}

procedure TFormParamsLab.FormCreate(Sender: TObject);
begin
  Application.HelpFile := Dir+'NouvLab.hlp';
  ComboTerrain1.ItemIndex := 0;
  ComboTerrain2.ItemIndex := 0;
  ComboTerrain3.ItemIndex := 1;
  ComboBord    .ItemIndex := 0;
end;

procedure TFormParamsLab.BoutonTypesIdentiquesClick(Sender: TObject);
begin
  ComboTerrain1.Enabled := True;
  ComboTerrain2.Enabled := True;
  ComboTerrain3.Enabled := True;
  ComboTerrain1.ItemIndex := 0;
  ComboTerrain2.ItemIndex := 0;
  ComboTerrain3.ItemIndex := 1;
end;

procedure TFormParamsLab.BoutonTypesDifferentsClick(Sender: TObject);
begin
  ComboTerrain1.Enabled := False;
  ComboTerrain2.Enabled := False;
  ComboTerrain3.Enabled := False;
  ComboTerrain1.ItemIndex := -1;
  ComboTerrain2.ItemIndex := -1;
  ComboTerrain3.ItemIndex := -1;
end;

end.
