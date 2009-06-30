{*
  Donne quelques outils de traitements graphiques sur FunLabyrinthe
  L'unit� GraphicsTools donne quelques routines utilitaires permettant
  d'effectuer facilement des traitements graphiques r�currents dans
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit GraphicsTools;

interface

uses
  Graphics, FunLabyUtils;

procedure DrawSquareNumber(Context: TDrawSquareContext; Number: Integer;
  FontColor: TColor = clBlack);

implementation

uses
  SysUtils;

{*
  Dessine un num�ro sur un canevas de case
  @param Context     Contexte de dessin de la case
  @param Number      Num�ro � �crire
  @param FontColor   Couleur du texte
*}
procedure DrawSquareNumber(Context: TDrawSquareContext; Number: Integer;
  FontColor: TColor = clBlack);
begin
  with Context, Canvas do
  begin
    Brush.Color := clWhite;
    Font.Name := 'Arial'; {don't localize}
    Font.Color := FontColor;
    TextOut(X+10, Y+8, IntToStr(Number));
  end;
end;

end.

