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
  TextX, TextY: Integer;
begin
  with Context do
  begin
    Bitmap.Font.Name := 'Arial'; {don't localize}
    Bitmap.Font.Color := FontColor;

    Extent := Bitmap.TextExtent(Text);
    TextX := X + (SquareSize - Extent.cx) div 2;
    TextY := Y + (SquareSize - Extent.cy) div 2;

    Bitmap.FillRectTS(TextX, TextY, TextX+Extent.cx, TextY+Extent.cy,
      BackColor);
    Bitmap.Textout(TextX, TextY, Text);
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

