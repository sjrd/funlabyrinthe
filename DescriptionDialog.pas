unit DescriptionDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormDescription = class(TForm)
    MemoDescription: TMemo;
    BoutonOK: TButton;
    BoutonAnnuler: TButton;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormDescription: TFormDescription;

implementation

{$R *.DFM}

end.
