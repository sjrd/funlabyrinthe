unit BaseMapViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Tabs, ScUtils, FunLabyUtils, FilesUtils,
  GR32, GR32_Image, MapImage, ComCtrls;

type
  {*
    Cadre de base pour un visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  TFrameBaseMapViewer = class(TFrame)
    MapTabSet: TTabSet;
    PanelMapInfos: TPanel;
    LabelPosition: TLabel;
    LabelField: TLabel;
    LabelEffect: TLabel;
    LabelObstacle: TLabel;
    LabelFloor: TLabel;
    LabelTool: TLabel;
    StaticPosition: TStaticText;
    StaticField: TStaticText;
    StaticEffect: TStaticText;
    StaticTool: TStaticText;
    StaticObstacle: TStaticText;
    EditFloor: TSpinEdit;
    MapView: TFrameMapImage;
    TrackBarScale: TTrackBar;
    LabelScale: TLabel;
    procedure MapTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure MapViewClickSquare(Sender: TObject; const QPos: TQualifiedPos);
    procedure MapViewDblClickSquare(Sender: TObject; const QPos: TQualifiedPos);
    procedure MapViewHoverSquare(Sender: TObject; const QPos: TQualifiedPos);
    procedure EditFloorChange(Sender: TObject);
    procedure TrackBarScaleChange(Sender: TObject);
  private
    FMaster: TMaster; /// Ma�tre FunLabyrinthe

    FOnClickSquare: TSquarePosEvent;    /// D�clench� au clic sur une case
    FOnDblClickSquare: TSquarePosEvent; /// D�clench� au double-clic d'une case
    FOnHoverSquare: TSquarePosEvent;    /// D�clench� au passage sur une case

    procedure SetMaster(Value: TMaster);

    function GetCurrentMap: TMap;
    procedure SetCurrentMap(Value: TMap);
    function GetCurrentFloor: Integer;
    procedure SetCurrentFloor(Value: Integer);

    function GetSelectedPos: TQualifiedPos;
    procedure SetSelectedPos(const Value: TQualifiedPos);

    function GetOnAfterPaintMap: TPaintMapEvent;
    procedure SetOnAfterPaintMap(Value: TPaintMapEvent);
  protected
    procedure Loaded; override;
  public
    procedure ShowPosition(const QPos: TQualifiedPos); overload;
    procedure ShowPosition(Map: TMap; const Position: T3DPoint); overload;

    procedure UpdateMaps;
    procedure InvalidateMap;

    property Master: TMaster read FMaster write SetMaster;

    property CurrentMap: TMap read GetCurrentMap write SetCurrentMap;
    property CurrentFloor: Integer read GetCurrentFloor write SetCurrentFloor;
  published
    property SelectedPos: TQualifiedPos
      read GetSelectedPos write SetSelectedPos;

    property OnAfterPaintMap: TPaintMapEvent
      read GetOnAfterPaintMap write SetOnAfterPaintMap;

    property OnClickSquare: TSquarePosEvent
      read FOnClickSquare write FOnClickSquare;
    property OnDblClickSquare: TSquarePosEvent
      read FOnDblClickSquare write FOnDblClickSquare;
    property OnHoverSquare: TSquarePosEvent
      read FOnHoverSquare write FOnHoverSquare;
  end;

implementation

{$R *.dfm}

type
  TControlStyleAccess = class(TControl)
  public
    property ControlStyle;
  end;

{*
  Modifie le ma�tre FunLabyrinthe affich�
  @param Value   Nouveau ma�tre
*}
procedure TFrameBaseMapViewer.SetMaster(Value: TMaster);
var
  I: Integer;
begin
  if Value = FMaster then
    Exit;

  // Clear any active tabs
  if FMaster <> nil then
  begin
    CurrentMap := nil;
    MapTabSet.Tabs.Clear;
  end;

  FMaster := Value;
  if (FMaster = nil) or (FMaster.MapCount = 0) then
    Exit;

  // Create tabs
  for I := 0 to Master.MapCount-1 do
    MapTabSet.Tabs.Add(Master.Maps[I].ID);
  MapTabSet.TabIndex := 0;
end;

{*
  Carte courante
  @return Carte courante
*}
function TFrameBaseMapViewer.GetCurrentMap: TMap;
begin
  Result := MapView.Map;
end;

{*
  Modifie la carte courante
  @param Value   Nouvelle carte
*}
procedure TFrameBaseMapViewer.SetCurrentMap(Value: TMap);
begin
  // Will trigger MapTabSet.OnChange
  if Value = nil then
    MapTabSet.TabIndex := -1
  else
    MapTabSet.TabIndex := MapTabSet.Tabs.IndexOf(Value.ID);
end;

{*
  �tage courant
  @param �tage courant
*}
function TFrameBaseMapViewer.GetCurrentFloor: Integer;
begin
  Result := MapView.Floor;
end;

{*
  Modifie l'�tage courant
  @param Value   Nouvel �tage
*}
procedure TFrameBaseMapViewer.SetCurrentFloor(Value: Integer);
begin
  EditFloor.Value := Value; // Will trigger EditFloor.OnChange
end;

{*
  Position s�lectionn�e
  @return Position s�lectionn�e
*}
function TFrameBaseMapViewer.GetSelectedPos: TQualifiedPos;
begin
  Result := MapView.SelectedPos;
end;

{*
  Modifie la position s�lectionn�e
  @param Value   Nouvelle position s�lectionn�e
*}
procedure TFrameBaseMapViewer.SetSelectedPos(const Value: TQualifiedPos);
begin
  MapView.SelectedPos := Value;
end;

{*
  Dessine par-dessus la carte
  @return Gestionnaire d'�v�nement OnAfterPaintMap
*}
function TFrameBaseMapViewer.GetOnAfterPaintMap: TPaintMapEvent;
begin
  Result := MapView.OnAfterPaintMap;
end;

{*
  Modifie le gestionnaire d'�v�nement OnAfterPaintMap
  @param Value   Nouveau gestionnaire
*}
procedure TFrameBaseMapViewer.SetOnAfterPaintMap(Value: TPaintMapEvent);
begin
  MapView.OnAfterPaintMap := Value;
end;

{*
  [@inheritDoc]
*}
procedure TFrameBaseMapViewer.Loaded;
begin
  inherited;

  MapView.OnClickSquare := MapViewClickSquare;
  MapView.OnDblClickSquare := MapViewDblClickSquare;
  MapView.OnHoverSquare := MapViewHoverSquare;
end;

{*
  Centre la vue sur une position donn�e
  @param QPos   Position qualifi�e � montrer
*}
procedure TFrameBaseMapViewer.ShowPosition(const QPos: TQualifiedPos);
begin
  ShowPosition(QPos.Map, QPos.Position);
end;

{*
  Centre la vue sur une position donn�e
  @param Map        Carte � montrer
  @param Position   Position � montrer
*}
procedure TFrameBaseMapViewer.ShowPosition(Map: TMap; const Position: T3DPoint);
begin
  if Map = nil then
    Exit;

  CurrentMap := Map;
  CurrentFloor := Position.Z;
  MapView.ShowPosition(Position);
end;

{*
  Met � jour les onglets des cartes
*}
procedure TFrameBaseMapViewer.UpdateMaps;
var
  AMaster: TMaster;
begin
  AMaster := Master;
  Master := nil;
  Master := AMaster;
end;

{*
  Invalide la carte affich�e
*}
procedure TFrameBaseMapViewer.InvalidateMap;
begin
  MapView.InvalidateMap;

  if (CurrentMap <> nil) and
    (EditFloor.MaxValue <> CurrentMap.Dimensions.Z-1) then
  begin
    EditFloor.MaxValue := CurrentMap.Dimensions.Z-1;
    EditFloor.Enabled := CurrentMap.Dimensions.Z > 1;
    EditFloor.Value := EditFloor.Value;
  end;
end;

{*
  Gestionnaire d'�v�nement OnChange du tab-set des cartes
  @param Sender        Objet qui a d�clench� l'�v�nement
  @param NewTab        Index de l'onglet nouvellement s�lectionn�
  @param AllowChange   Indique si le changement peut �tre effectu�
*}
procedure TFrameBaseMapViewer.MapTabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  StaticPosition.Caption := '';
  StaticField.Caption := '';
  StaticEffect.Caption := '';
  StaticTool.Caption := '';
  StaticObstacle.Caption := '';

  if NewTab < 0 then
  begin
    MapView.Map := nil;

    EditFloor.Enabled := False;
  end else
  begin
    MapView.Map := Master.Maps[NewTab];

    EditFloor.MaxValue := CurrentMap.Dimensions.Z-1;
    EditFloor.Enabled := CurrentMap.Dimensions.Z > 1;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClickSquare de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param QPos     Position qualifi�e de la case cliqu�e
*}
procedure TFrameBaseMapViewer.MapViewClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  if Assigned(FOnClickSquare) then
    FOnClickSquare(Self, QPos);
end;

{*
  Gestionnaire d'�v�nement OnDblClickSquare de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param QPos     Position qualifi�e de la case cliqu�e
*}
procedure TFrameBaseMapViewer.MapViewDblClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  if Assigned(FOnDblClickSquare) then
    FOnDblClickSquare(Self, QPos);
end;

{*
  Gestionnaire d'�v�nement OnHoverSquare de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param QPos     Position qualifi�e de la case survol�e
*}
procedure TFrameBaseMapViewer.MapViewHoverSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  StaticPosition.Caption := Point3DToString(QPos.Position, ', ');

  with QPos.Square do
  begin
    StaticField.Caption := Field.Name;

    if Effect = nil then
      StaticEffect.Caption := ''
    else
      StaticEffect.Caption := Effect.Name;

    if Tool = nil then
      StaticTool.Caption := ''
    else
      StaticTool.Caption := Tool.Name;

    if Obstacle = nil then
      StaticObstacle.Caption := ''
    else
      StaticObstacle.Caption := Obstacle.Name;
  end;

  if Assigned(FOnHoverSquare) then
    FOnHoverSquare(Self, QPos);
end;

{*
  Gestionnaire d'�v�nement OnChange de la zone d'�dition de l'�tage
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameBaseMapViewer.EditFloorChange(Sender: TObject);
begin
  MapView.Floor := EditFloor.Value;
end;

{*
  Gestionnaire d'�v�nement OnChange de la trackbar de zoom
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameBaseMapViewer.TrackBarScaleChange(Sender: TObject);
begin
  MapView.Scale := TrackBarScale.Position * 0.1;
end;

end.

