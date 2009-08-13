unit MapViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScUtils, FunLabyUtils, FunLabyEditOTA, BaseMapViewer, GR32;

type
  {*
    Visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  TFormMapViewer = class(TForm, IOTAMapViewer50)
    MapViewer: TFrameBaseMapViewer;
    procedure MapViewerAfterPaintMap(Sender: TObject; Bitmap: TBitmap32;
      Map: TMap; Floor: Integer);
    procedure MapViewerClickSquare(Sender: TObject; const QPos: TQualifiedPos);
  private
    FSelectedSquare: TQualifiedPos; /// Case s�lectionn�e

    function GetMaster: TMaster;
    procedure SetMaster(Value: TMaster);

    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);

    function GetSelectedSquare: TQualifiedPos;
    function GetSelectedMap: TMap;
    function GetSelectedPos: T3DPoint;

    procedure SetSelectedSquare(const Value: TQualifiedPos);
    procedure SetSelectedMap(Value: TMap);
    procedure SetSelectedPos(const Value: T3DPoint);
  public
    constructor Create(AOwner: TComponent); override;

    procedure ShowPosition(const QPos: TQualifiedPos);

    property Master: TMaster read GetMaster write SetMaster;

    property SelectedSquare: TQualifiedPos
      read FSelectedSquare write SetSelectedSquare;
    property SelectedMap: TMap
      read FSelectedSquare.Map write SetSelectedMap;
    property SelectedPos: T3DPoint
      read FSelectedSquare.Position write SetSelectedPos;
  end;

implementation

{$R *.dfm}

{-----------------------}
{ Classe TFormMapViewer }
{-----------------------}

{*
  [@inheritDoc]
*}
constructor TFormMapViewer.Create(AOwner: TComponent);
begin
  inherited;

  MapViewer.OnAfterPaintMap := MapViewerAfterPaintMap;
  MapViewer.OnClickSquare := MapViewerClickSquare;
end;

{*
  Ma�tre FunLabyrinthe
  @return Ma�tre FunLabyrinthe
*}
function TFormMapViewer.GetMaster: TMaster;
begin
  Result := MapViewer.Master;
end;

{*
  Change de ma�tre
  @param Value   Nouveau ma�tre
*}
procedure TFormMapViewer.SetMaster(Value: TMaster);
begin
  FSelectedSquare := NoQPos;
  MapViewer.Master := Value;
end;

{*
  [@inheritDoc]
*}
function TFormMapViewer.GetVisible: Boolean;
begin
  Result := Visible;
end;

{*
  [@inheritDoc]
*}
procedure TFormMapViewer.SetVisible(Value: Boolean);
begin
  Visible := Value;
end;

{*
  [@inheritDoc]
*}
function TFormMapViewer.GetSelectedSquare: TQualifiedPos;
begin
  Result := FSelectedSquare;
end;

{*
  [@inheritDoc]
*}
function TFormMapViewer.GetSelectedMap: TMap;
begin
  Result := FSelectedSquare.Map;
end;

{*
  [@inheritDoc]
*}
function TFormMapViewer.GetSelectedPos: T3DPoint;
begin
  Result := FSelectedSquare.Position;
end;

{*
  Modifie la case s�lectionn�e
  @param Value   Nouvelle case s�lectionn�e
*}
procedure TFormMapViewer.SetSelectedSquare(const Value: TQualifiedPos);
begin
  FSelectedSquare := Value;
  MapViewer.InvalidateMap;
end;

{*
  Modifie la carte s�lectionn�e
  @param Value   Nouvelle carte s�lectionn�e
*}
procedure TFormMapViewer.SetSelectedMap(Value: TMap);
begin
  FSelectedSquare.Map := Value;
  MapViewer.InvalidateMap;
end;

{*
  Modifie la position s�lectionn�e
  @param Value   Nouvelle position s�lectionn�e
*}
procedure TFormMapViewer.SetSelectedPos(const Value: T3DPoint);
begin
  FSelectedSquare.Position := Value;
  MapViewer.InvalidateMap;
end;

{*
  [@inheritDoc]
*}
procedure TFormMapViewer.ShowPosition(const QPos: TQualifiedPos);
begin
  MapViewer.ShowPosition(QPos);
end;

{*
  Gestionnaire d'�v�nement OnAfterPaintMap du visualisateur de cartes
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Bitmap   Bitmap
*}
procedure TFormMapViewer.MapViewerAfterPaintMap(Sender: TObject;
  Bitmap: TBitmap32; Map: TMap; Floor: Integer);
var
  SelPoint: TPoint;
  I: Integer;
begin
  SelPoint := Point(SelectedPos.X, SelectedPos.Y);

  if (SelectedMap = Map) and (SelectedPos.Z = Floor) then
  begin
    // Draw the selection rectangle
    for I := -1 to 1 do
      with SelPoint do
        Bitmap.FrameRectS((X+1) * SquareSize - I, (Y+1) * SquareSize - I,
          (X+2) * SquareSize + I, (Y+2) * SquareSize + I, clYellow32);
  end;
end;

{*
  Gestionnaire d'�v�nement OnClickSquare de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param QPos     Position qualifi�e de la case cliqu�e
*}
procedure TFormMapViewer.MapViewerClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  SelectedSquare := QPos;
end;

end.

