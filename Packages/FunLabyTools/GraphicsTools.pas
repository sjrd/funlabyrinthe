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
  Types, SysUtils, Graphics, FunLabyUtils, GR32;

type
  TFieldPredicate = function(Field: TField): Boolean;

procedure DrawSquareText(Context: TDrawSquareContext; const Text: string;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);

procedure DrawSquareNumber(Context: TDrawSquareContext; Number: Integer;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);

procedure DissipateNeighbors(Context: TDrawSquareContext;
  const Predicate: TFieldPredicate); overload;
procedure DissipateNeighbors(Context: TDrawSquareContext); overload;
procedure DissipateGroundNeighbors(Context: TDrawSquareContext);

implementation

uses
  Math, Generics;

{*
  Dessine un texte dans un contexte de dessin de case
  @param Context     Contexte de dessin de la case
  @param Text        Texte � �crire
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

    Extent := Bitmap.TextExtent(Text);
    TextX := X + (SquareSize - Extent.cx) div 2;
    TextY := Y + (SquareSize - Extent.cy) div 2;

    Bitmap.FillRectTS(TextX-2, TextY, TextX+Extent.cx+2, TextY+Extent.cy,
      BackColor);

    Bitmap.RenderText(TextX, TextY, Text, 0, FontColor);
  end;
end;

{*
  Dessine un num�ro dans un contexte de dessin de case
  @param Context     Contexte de dessin de la case
  @param Number      Num�ro � �crire
  @param FontColor   Couleur du texte
  @param BackColor   Couleur de fond
*}
procedure DrawSquareNumber(Context: TDrawSquareContext; Number: Integer;
  FontColor: TColor32 = clBlack32; BackColor: TColor32 = clWhite32);
begin
  DrawSquareText(Context, IntToStr(Number), FontColor, BackColor);
end;

{------------------------------}
{ Dissipate neighbors routines }
{------------------------------}

const
  NeighborSize = 10;
  NeighborAlphaDiff = $FF div NeighborSize;

  NeighborClipRect: array[diNorth..diWest] of TRect = (
    (Left: 0; Top: 0; Right: SquareSize; Bottom: NeighborSize),
    (Left: SquareSize-NeighborSize; Top: 0;
      Right: SquareSize; Bottom: SquareSize),
    (Left: 0; Top: SquareSize-NeighborSize;
      Right: SquareSize; Bottom: SquareSize),
    (Left: 0; Top: 0; Right: NeighborSize; Bottom: SquareSize)
  );

  NeighborCornerClipRect: array[diNorth..diWest] of TRect = (
    (Left: SquareSize-NeighborSize; Top: 0;
      Right: SquareSize; Bottom: NeighborSize),
    (Left: SquareSize-NeighborSize; Top: SquareSize-NeighborSize;
      Right: SquareSize; Bottom: SquareSize),
    (Left: 0; Top: SquareSize-NeighborSize;
      Right: NeighborSize; Bottom: SquareSize),
    (Left: 0; Top: 0; Right: NeighborSize; Bottom: NeighborSize)
  );

{*
  Dissipe l'image des terrains voisins sur l'image d'une case
  @param Context     Contexte de dessin de la case
  @param Predicate   Pr�dicat testant si tel ou tel voisin doit �tre dissip�
*}
procedure DissipateNeighbors(Context: TDrawSquareContext;
  const Predicate: TFieldPredicate);
var
  Other: TQualifiedPos;
  Dir: TDirection;
  OtherField: TField;
  DestBmp, OtherBmp: TBitmap32;
  BaseX, BaseY, X, Y, Dist: Integer;
  OtherContext: TDrawSquareContext;
  Corner: Boolean;

  procedure CreateOtherBmpAndContext;
  begin
    OtherBmp := TBitmap32.Create;
    OtherBmp.DrawMode := dmBlend;
    OtherBmp.SetSize(SquareSize, SquareSize);

    OtherContext := TDrawSquareContext.Create(OtherBmp);
    OtherContext.Assign(Context);
  end;

begin
  if Context.IsNowhere then
    Exit;

  Other.Map := Context.Map;
  DestBmp := Context.Bitmap;
  BaseX := Context.X;
  BaseY := Context.Y;
  Dist := 0;
  Corner := False;

  OtherBmp := nil;
  OtherContext := nil;
  try
    for Dir := diNorth to diWest do
    begin
      Other.Position := PointBehind(Context.Pos, Dir);
      OtherField := Other.Field.GetEffectiveDrawingField;

      if Predicate(OtherField) then
        Corner := False
      else
      begin
        Other.Position := PointBehind(Context.Pos, RightDir[Dir]);
        if Predicate(Other.Field.GetEffectiveDrawingField) then
          Continue;

        Other.Position := PointBehind(Other.Position, Dir);
        OtherField := Other.Field.GetEffectiveDrawingField;
        if not Predicate(OtherField) then
          Continue;

        Corner := True;
      end;

      if OtherBmp = nil then
        CreateOtherBmpAndContext;

      if Corner then
        OtherBmp.ClipRect := NeighborCornerClipRect[Dir]
      else
        OtherBmp.ClipRect := NeighborClipRect[Dir];

      OtherField.Draw(OtherContext);

      for X := OtherBmp.ClipRect.Left to OtherBmp.ClipRect.Right-1 do
      begin
        for Y := OtherBmp.ClipRect.Top to OtherBmp.ClipRect.Bottom-1 do
        begin
          case Dir of
            diNorth: Dist := Y+1;
            diEast: Dist := SquareSize-X;
            diSouth: Dist := SquareSize-Y;
            diWest: Dist := X+1;
          end;

          if Corner then
          begin
            case RightDir[Dir] of
              diNorth: Dist := Max(Dist, Y+1);
              diEast: Dist := Max(Dist, SquareSize-X);
              diSouth: Dist := Max(Dist, SquareSize-Y);
              diWest: Dist := Max(Dist, X+1);
            end;
          end;

          DestBmp.SetPixelTS(BaseX+X, BaseY+Y,
            SetAlpha(OtherBmp[X, Y], $FF - Dist * NeighborAlphaDiff));
        end;
      end;
    end;
  finally
    OtherBmp.Free;
    OtherContext.Free;
  end;
end;

{*
  Pr�dicat de terrain qui renvoie toujours True
*}
function AlwaysTrue(Field: TField): Boolean;
begin
  Result := True;
end;

{*
  Dissipe l'image des terrains voisins sur l'image d'une case
  @param Context   Contexte de dessin de la case
*}
procedure DissipateNeighbors(Context: TDrawSquareContext);
begin
  DissipateNeighbors(Context, AlwaysTrue);
end;

{*
  Pr�dicat de terrain qui renvoie toujours True
*}
function FieldIsGround(Field: TField): Boolean;
begin
  Result := Field is TGround;
end;

{*
  Dissipe l'image des sols voisins sur l'image d'une case
  @param Context   Contexte de dessin de la case
*}
procedure DissipateGroundNeighbors(Context: TDrawSquareContext);
begin
  DissipateNeighbors(Context, FieldIsGround);
end;

end.

