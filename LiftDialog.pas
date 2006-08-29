unit LiftDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TFormBougeAscenseur = class(TForm)
    LabelEtage: TLabel;
    EditEtage: TSpinEdit;
    BoutonOK: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure QueryEtage(Min, Max : integer; var Current : integer);
  end;

var
  FormBougeAscenseur: TFormBougeAscenseur;

implementation

{$R *.DFM}

procedure TFormBougeAscenseur.QueryEtage(Min, Max : integer; var Current : integer);
begin
  EditEtage.MinValue := Min;
  EditEtage.MaxValue := Max;
  EditEtage.Value := Current;
  EditEtage.Enabled := Min <> Max;
  if EditEtage.Enabled then ActiveControl := EditEtage
                       else ActiveControl := BoutonOK;
  ShowModal;
  Current := EditEtage.Value;
end;

procedure TFormBougeAscenseur.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then ModalResult := mrOK;
end;

procedure TFormBougeAscenseur.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key := #0;
end;

end.
