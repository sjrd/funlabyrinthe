{*
  Donne quelques outils de traitements graphiques sur FunLabyrinthe
  L'unité GraphicsTools donne quelques routines utilitaires permettant
  d'effectuer facilement des traitements graphiques récurrents dans
  FunLabyrinthe.
  @author sjrd
  @version 5.0
*}
unit GraphicsTools;

interface

uses
  Types, SysUtils, Graphics, FunLabyUtils, GR32;

procedure DrawSquareText(Context: TDrawSquareContext; const Text: string;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);

procedure DrawSquareNumber(Context: TDrawSquareContext; Number: Integer;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);

implementation

{*
  Dessine un texte dans un contexte de dessin de case
  @param Context     Contexte de dessin de la case
  @param Text        Texte à écrire
  @param FontColor   Couleur du texte
  @param BackColor   Couleur de fond
*}
procedure DrawSquareText(Context: TDrawSquareContext; const Text: string;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);
var
  Extent: TSize;
  TextX, TextY, X, Y: Integer;
begin
  with Context do
  begin
    Bitmap.Font.Name := 'Arial'; {don't localize}
    Bitmap.Font.Color := WinColor(FontColor);

    Extent := Bitmap.TextExtent(Text);
    TextX := X + (SquareSize - Extent.cx) div 2;
    TextY := Y + (SquareSize - Extent.cy) div 2;

    Bitmap.FillRectTS(TextX-2, TextY, TextX+Extent.cx+2, TextY+Extent.cy,
      BackColor);
    Bitmap.Textout(TextX, TextY, Text);
  end;

  with Context.Bitmap do
  begin
    for X := TextX to TextX+Extent.cx-1 do
      for Y := TextY to TextY+Extent.cy-1 do
        if SetAlpha(Pixels[X, Y], 0) = SetAlpha(FontColor, 0) then
          Pixels[X, Y] := SetAlpha(Pixels[X, Y], $FF);
  end;
end;

{*
  Dessine un numéro dans un contexte de dessin de case
  @param Context     Contexte de dessin de la case
  @param Number      Numéro à écrire
  @param FontColor   Couleur du texte
  @param BackColor   Couleur de fond
*}
procedure DrawSquareNumber(Context: TDrawSquareContext; Number: Integer;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);
begin
  DrawSquareText(Context, IntToStr(Number), FontColor, BackColor);
end;

end.

