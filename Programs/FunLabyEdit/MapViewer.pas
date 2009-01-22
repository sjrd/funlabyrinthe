unit MapViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScUtils, FunLabyUtils, FunLabyEditOTA, BaseMapViewer;

type
  {*
    Visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  TFormMapViewer = class(TForm, IOTAMapViewer50)
    MapViewer: TFrameBaseMapViewer;
    procedure MapViewerAfterPaint(Sender: TObject; Canvas: TCanvas;
      const SquareClipRect: TRect);
    procedure MapViewerClickSquare(Sender: TObject; const QPos: TQualifiedPos);
  private
    FSelectedSquare: TQualifiedPos; /// Case sélectionnée

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

  MapViewer.OnAfterPaint := MapViewerAfterPaint;
  MapViewer.OnClickSquare := MapViewerClickSquare;
end;

{*
  Maître FunLabyrinthe
  @return Maître FunLabyrinthe
*}
function TFormMapViewer.GetMaster: TMaster;
begin
  Result := MapViewer.Master;
end;

{*
  Change de maître
  @param Value   Nouveau maître
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
  Modifie la case sélectionnée
  @param Value   Nouvelle case sélectionnée
*}
procedure TFormMapViewer.SetSelectedSquare(const Value: TQualifiedPos);
begin
  FSelectedSquare := Value;
  MapViewer.InvalidateMap;
end;

{*
  Modifie la carte sélectionnée
  @param Value   Nouvelle carte sélectionnée
*}
procedure TFormMapViewer.SetSelectedMap(Value: TMap);
begin
  FSelectedSquare.Map := Value;
  MapViewer.InvalidateMap;
end;

{*
  Modifie la position sélectionnée
  @param Value   Nouvelle position sélectionnée
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
  Gestionnaire d'événement OnAfterPaint du visualisateur de cartes
  @param Sender           Objet qui a déclenché l'événement
  @param Canvas           Canevas
  @param SquareClipRect   Clip-rect des cases
*}
procedure TFormMapViewer.MapViewerAfterPaint(Sender: TObject; Canvas: TCanvas;
  const SquareClipRect: TRect);
var
  SelPoint: TPoint;
begin
  SelPoint := Point(SelectedPos.X, SelectedPos.Y);

  if (SelectedMap = MapViewer.CurrentMap) and
    (SelectedPos.Z = MapViewer.CurrentFloor) and
    PtInRect(SquareClipRect, SelPoint) then
  begin
    // Draw the selection rectangle
    with Canvas do
    begin
      Brush.Style := bsClear;
      Pen.Style := psSolid;
      Pen.Color := clYellow;
      Pen.Width := 3;
      try
        Rectangle((SelPoint.X+1) * SquareSize, (SelPoint.Y+1) * SquareSize,
          (SelPoint.X+2) * SquareSize, (SelPoint.Y+2) * SquareSize);
      finally
        Pen.Width := 1;
      end;
    end;
  end;
end;

{*
  Gestionnaire d'événement OnClickSquare de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param QPos     Position qualifiée de la case cliquée
*}
procedure TFormMapViewer.MapViewerClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  SelectedSquare := QPos;
end;

end.

