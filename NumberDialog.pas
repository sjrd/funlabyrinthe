unit NumberDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TFormNumber = class(TForm)
    LabelPrompt: TLabel;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Composants non disponibles dans Turbo Explorer }
    EditValue : TSpinEdit;
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    class function ChooseNumber(const Title, Prompt : string;
      Default, Min, Max : integer) : integer;
  end;

var
  FormNumber: TFormNumber;

implementation

{$R *.DFM}

{*
  Affiche une invite � l'utilisateur lui demandant de choisir un nombre
  @param Title     Titre de la bo�te de dialogue
  @param Prompt    Invite
  @param Default   Valeur par d�faut affich�e
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

procedure TFormNumber.FormCreate(Sender: TObject);
begin
  // Cr�ation dynamique des composants non disponibles dans Turbo Explorer
  EditValue := TSpinEdit.Create(Self);
  with EditValue do
  begin
    Name := 'EditValue'; {don't localize}
    Parent := Self;
    Left := 176;
    Top := 16;
    Width := 65;
    AutoSelect := False;
    EditorEnabled := False;
    TabOrder := 0;
  end;
end;

{*
  Gestionnaire d'�v�nement OnKeyDown
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Key      Touche enfonc�e
  @param Shift    �tat des touches syst�me
*}
procedure TFormNumber.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then ModalResult := mrOK;
end;

{*
  Gestionnaire d'�v�nement OnKeyPress
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Key      Caract�re frapp�
*}
procedure TFormNumber.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key := #0;
end;

end.

