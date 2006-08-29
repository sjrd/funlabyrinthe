unit NewLiftDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TFormAscenseur = class(TForm)
    LabelDebut: TLabel;
    LabelFin: TLabel;
    BoutonOK: TButton;
    EditDebut: TSpinEdit;
    EditFin: TSpinEdit;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure GetDebutFin(var Debut, Fin : integer; Current, MaxFin : integer);
  end;

var
  FormAscenseur: TFormAscenseur;

implementation

{$R *.DFM}

procedure TFormAscenseur.GetDebutFin(var Debut, Fin : integer; Current, MaxFin : integer);
begin
  EditDebut.MinValue := 1;
  EditDebut.MaxValue := Current;
  EditDebut.Value    := Current;
  EditDebut.Enabled  := (1 <> Current);
  EditFin.MinValue := Current;
  EditFin.MaxValue := MaxFin;
  EditFin.Value    := Current;
  EditFin.Enabled  := (Current <> MaxFin);
  if EditDebut.Enabled then ActiveControl := EditDebut else
  if EditFin  .Enabled then ActiveControl := EditFin   else
  ActiveControl := BoutonOK;
  ShowModal;
  Debut := EditDebut.Value;
  Fin   := EditFin.Value;
end;

end.
