unit FunLabyEditPluginUtilsReg;

interface

uses
  Classes, FunLabyControls;

procedure Register;

implementation

{*
  Enregistre les composants FunLabyrinthe dans la palette d'outils de Delphi
*}
procedure Register;
begin
  RegisterComponents('FunLabyrinthe',
    [
    // FunLabyControls
    TFLComponentComboBox
    ]);
end;

end.

