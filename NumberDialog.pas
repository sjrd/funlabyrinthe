unit NumberDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TFormNumber = class(TForm)
    LabelPrompt: TLabel;
    EditValue: TSpinEdit;
    ButtonOK: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class function ChooseNumber(const Title, Prompt : string;
      Default, Min, Max : integer) : integer;
  end;

var
  FormNumber: TFormNumber;

implementation

{$R *.DFM}

{*
  Affiche une invite à l'utilisateur lui demandant de choisir un nombre
  @param Title     Titre de la boîte de dialogue
  @param Prompt    Invite
  @param Default   Valeur par défaut affichée
  @param Min       Valeur minimale que peut choisir l'utilisateur
  @param Max       Valeur maximale que peut choisir l'utilisateur
  @return La valeur qu'a choisie l'utilisateur
*}
class function TFormNumber.ChooseNumber(const Title, Prompt : string;
  Default, Min, Max : integer) : integer;
begin
  with Create(Application) do
  try
    Caption := Title;
    LabelPrompt.Caption := Prompt;

    EditValue.Value := Default;
    EditValue.MinValue := Min;
    EditValue.MaxValue := Max;

    EditValue.Enabled := Min <> Max;
    if EditValue.Enabled then
      ActiveControl := EditValue
    else
      ActiveControl := ButtonOK;

    ShowModal;

    Result := EditValue.Value;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'événement OnKeyDown
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Touche enfoncée
  @param Shift    État des touches système
*}
procedure TFormNumber.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ModalResult := mrOK;
end;

{*
  Gestionnaire d'événement OnKeyPress
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Caractère frappé
*}
procedure TFormNumber.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key := #0;
end;

end.

