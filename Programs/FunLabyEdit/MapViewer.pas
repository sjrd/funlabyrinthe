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
    procedure MapViewerClickSquare(Sender: TObject; const QPos: TQualifiedPos);
    procedure MapViewerDblClickSquare(Sender: TObject;
      const QPos: TQualifiedPos);
  private
    FEvents: IOTAMapViewerEvents51; /// �v�nements OTA

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
      read GetSelectedSquare write SetSelectedSquare;
    property SelectedMap: TMap
      read GetSelectedMap write SetSelectedMap;
    property SelectedPos: T3DPoint
      read GetSelectedPos write SetSelectedPos;

    property Events: IOTAMapViewerEvents51 read FEvents write FEvents;
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

  MapViewer.OnClickSquare := MapViewerClickSquare;
  MapViewer.OnDblClickSquare := MapViewerDblClickSquare;
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
  SelectedSquare := NoQPos;
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
  Result := MapViewer.SelectedPos;
end;

{*
  [@inheritDoc]
*}
function TFormMapViewer.GetSelectedMap: TMap;
begin
  Result := SelectedSquare.Map;
end;

{*
  [@inheritDoc]
*}
function TFormMapViewer.GetSelectedPos: T3DPoint;
begin
  Result := SelectedSquare.Position;
end;

{*
  Modifie la case s�lectionn�e
  @param Value   Nouvelle case s�lectionn�e
*}
procedure TFormMapViewer.SetSelectedSquare(const Value: TQualifiedPos);
begin
  MapViewer.SelectedPos := Value;
end;

{*
  Modifie la carte s�lectionn�e
  @param Value   Nouvelle carte s�lectionn�e
*}
procedure TFormMapViewer.SetSelectedMap(Value: TMap);
var
  ASelectedPos: TQualifiedPos;
begin
  ASelectedPos := SelectedSquare;
  ASelectedPos.Map := Value;
  SelectedSquare := ASelectedPos;
end;

{*
  Modifie la position s�lectionn�e
  @param Value   Nouvelle position s�lectionn�e
*}
procedure TFormMapViewer.SetSelectedPos(const Value: T3DPoint);
var
  ASelectedPos: TQualifiedPos;
begin
  ASelectedPos := SelectedSquare;
  ASelectedPos.Position := Value;
  SelectedSquare := ASelectedPos;
end;

{*
  [@inheritDoc]
*}
procedure TFormMapViewer.ShowPosition(const QPos: TQualifiedPos);
begin
  MapViewer.ShowPosition(QPos);
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

  if Assigned(FEvents) then
    FEvents.ClickSquare(QPos);
end;

{*
  Gestionnaire d'�v�nement OnDblClickSquare de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param QPos     Position qualifi�e de la case cliqu�e
*}
procedure TFormMapViewer.MapViewerDblClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  if Assigned(FEvents) then
    FEvents.DblClickSquare(QPos);
end;

end.

