unit ScrewNumberDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ScUtils;

type
  TFormNumeroCase = class(TForm)
    LabelNumero: TLabel;
    EditNumero: TSpinEdit;
    BoutonOK: TButton;
  private
    { D�clarations priv�es }
    function GetNumber(const Title : string; Min, Max : integer) : integer;
  public
    { D�clarations publiques }
    function GetButtonNumber : integer;
    function GetTeleporterNumber : integer;
    function GetBoatNumber : integer;
  end;

var
  FormNumeroCase: TFormNumeroCase;

implementation

{$R *.DFM}

resourcestring
  sButtonTitle = 'Num�ro du bouton';
  sTeleportTitle = 'Num�ro du t�l�porteur';
  sBoatTitle = 'Num�ro de la barque';
  sPrompt = '%s (%d � %d) :';

function TFormNumeroCase.GetNumber(const Title : string;
  Min, Max : integer) : integer;
begin
  Caption := Title;
  LabelNumero.Caption := Format(sPrompt, [Title, Min, Max]);
  with EditNumero do
  begin
    Value := MinMax(Value, Min, Max);
    MinValue := Min;
    MaxValue := Max;
  end;
  ActiveControl := EditNumero;
  ShowModal;
  Result := EditNumero.Value;
end;

function TFormNumeroCase.GetButtonNumber : integer;
begin
  Result := GetNumber(sButtonTitle, 1, 45);
end;

function TFormNumeroCase.GetTeleporterNumber : integer;
begin
  Result := GetNumber(sTeleportTitle, 0, 30);
end;

function TFormNumeroCase.GetBoatNumber : integer;
begin
  Result := GetNumber(sBoatTitle, 1, 10);
end;

end.
