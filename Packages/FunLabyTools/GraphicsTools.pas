{*
  Donne quelques outils de traitements graphiques sur FunLabyrinthe
  L'unité GraphicsTools donne quelques routines utilitaires permettant
  d'effectuer facilement des traitements graphiques récurrents dans
  FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit GraphicsTools;

interface

uses
  Graphics;

procedure DrawScrewNumber(Canvas : TCanvas; X, Y, Number : integer;
  FontColor : TColor = clBlack);

implementation

uses
  SysUtils;

{*
  Dessine un numéro sur un canevas de case
  @param Canvas      Canevas sur lequel dessiner le numéro
  @param X           Bord gauche de la case
  @param Y           Bord supérieur de la case
  @param Number      Numéro à écrire
  @param FontColor   Couleur du texte
*}
procedure DrawScrewNumber(Canvas : TCanvas; X, Y, Number : integer;
  FontColor : TColor = clBlack);
begin
  with Canvas do
  begin
    Brush.Color := clWhite;
    Font.Name := 'Arial'; {don't localize}
    Font.Color := FontColor;
    TextOut(X+10, Y+8, IntToStr(Number));
  end;
end;

end.

