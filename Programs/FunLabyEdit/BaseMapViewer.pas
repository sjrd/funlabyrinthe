unit BaseMapViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Spin, ExtCtrls, Tabs, ScUtils, FunLabyUtils, FilesUtils,
  GR32, GR32_Image;

type
  TPaintMapEvent = procedure(Sender: TObject; Bitmap: TBitmap32;
    const SquareClipRect: TRect) of object;

  TSquarePosEvent = procedure(Sender: TObject;
    const QPos: TQualifiedPos) of object;

  {*
    Cadre de base pour un visualisateur de cartes
    @author sjrd
    @version 5.0
  *}
  TFrameBaseMapViewer = class(TFrame)
    MapTabSet: TTabSet;
    ScrollBoxMap: TScrollBox;
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
    PaintBoxMap: TPaintBox32;
    procedure MapTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PaintBoxMapPaintBuffer(Sender: TObject);
    procedure PaintBoxMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditFloorChange(Sender: TObject);
  private
    FMaster: TMaster; /// Maître FunLabyrinthe

    FCurrentMap: TMap;      /// Carte courante
    FCurrentFloor: Integer; /// Étage courant

    FOnAfterPaint: TPaintMapEvent; /// Dessine au-dessus du reste

    FOnClickSquare: TSquarePosEvent; /// Déclenché au clic sur une case
    FOnOverSquare: TSquarePosEvent;  /// Déclenché au passage sur une case

    procedure SetMaster(Value: TMaster);

    procedure SetCurrentMap(Value: TMap);
    procedure SetCurrentFloor(Value: Integer);
  public
    procedure ShowPosition(const QPos: TQualifiedPos); overload;
    procedure ShowPosition(Map: TMap; const Position: T3DPoint); overload;

    procedure UpdateMaps;
    procedure InvalidateMap;

    property Master: TMaster read FMaster write SetMaster;

    property CurrentMap: TMap read FCurrentMap write SetCurrentMap;
    property CurrentFloor: Integer read FCurrentFloor write SetCurrentFloor;
  published
    property OnAfterPaint: TPaintMapEvent
      read FOnAfterPaint write FOnAfterPaint;

    property OnClickSquare: TSquarePosEvent
      read FOnClickSquare write FOnClickSquare;
    property OnOverSquare: TSquarePosEvent
      read FOnOverSquare write FOnOverSquare;
  end;

implementation

{$R *.dfm}

type
  TControlStyleAccess = class(TControl)
  public
    property ControlStyle;
  end;

{*
  Modifie le maître FunLabyrinthe affiché
  @param Value   Nouveau maître
*}
procedure TFrameBaseMapViewer.SetMaster(Value: TMaster);
var
  I: Integer;
begin
  if Value = FMaster then
    Exit;

  // Make the paint box opaque
  TControlStyleAccess(PaintBoxMap).ControlStyle :=
    TControlStyleAccess(PaintBoxMap).ControlStyle + [csOpaque];

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
  Modifie l'étage courant
  @param Value   Nouvel étage
*}
procedure TFrameBaseMapViewer.SetCurrentFloor(Value: Integer);
begin
  EditFloor.Value := Value; // Will trigger EditFloor.OnChange
end;

{*
  Centre la vue sur une position donnée
  @param QPos   Position qualifiée à montrer
*}
procedure TFrameBaseMapViewer.ShowPosition(const QPos: TQualifiedPos);
begin
  ShowPosition(QPos.Map, QPos.Position);
end;

{*
  Centre la vue sur une position donnée
  @param Map        Carte à montrer
  @param Position   Position à montrer
*}
procedure TFrameBaseMapViewer.ShowPosition(Map: TMap; const Position: T3DPoint);
var
  X, Y: Integer;
begin
  CurrentMap := Map;

  X := Position.X * SquareSize + SquareSize div 2;
  Y := Position.Y * SquareSize + SquareSize div 2;
  CurrentFloor := Position.Z;

  with ScrollBoxMap do
  begin
    HorzScrollBar.Position := X - ClientWidth  div 2;
    VertScrollBar.Position := Y - ClientHeight div 2;
  end;
end;

{*
  Met à jour les onglets des cartes
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
  Invalide la carte affichée
*}
procedure TFrameBaseMapViewer.InvalidateMap;
begin
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnChange du tab-set des cartes
  @param Sender        Objet qui a déclenché l'événement
  @param NewTab        Index de l'onglet nouvellement sélectionné
  @param AllowChange   Indique si le changement peut être effectué
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
    FCurrentMap := nil;

    CurrentFloor := 0;
    EditFloor.Enabled := False;

    PaintBoxMap.Width := 100;
    PaintBoxMap.Height := 100;
  end else
  begin
    FCurrentMap := Master.Maps[NewTab];

    EditFloor.MaxValue := CurrentMap.Dimensions.Z-1;
    EditFloor.Enabled := CurrentMap.Dimensions.Z > 1;
    if CurrentFloor >= CurrentMap.Dimensions.Z then
      CurrentFloor := CurrentMap.Dimensions.Z-1;

    PaintBoxMap.Width  := (CurrentMap.Dimensions.X + 2*MinViewSize)*SquareSize;
    PaintBoxMap.Height := (CurrentMap.Dimensions.Y + 2*MinViewSize)*SquareSize;
  end;

  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnPaintBuffer de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameBaseMapViewer.PaintBoxMapPaintBuffer(Sender: TObject);
var
  Left, Top, Right, Bottom: Integer;
  SquareClipRect: TRect;
  LeftZone, TopZone, RightZone, BottomZone: Integer;
  I, X, Y: Integer;
  QPos: TQualifiedPos;
  Context: TDrawSquareContext;
begin
  if CurrentMap = nil then
  begin
    PaintBoxMap.Buffer.Clear(Color32(ScrollBoxMap.Color));
    Exit;
  end;

  // Calcul des coordonnées à afficher
  Left := 0 - MinViewSize;
  Top := 0 - MinViewSize;
  Right := CurrentMap.Dimensions.X + MinViewSize;
  Bottom := CurrentMap.Dimensions.Y + MinViewSize;
  SquareClipRect := Rect(Left, Top, Right, Bottom);

  LeftZone   := Left   div CurrentMap.ZoneWidth;
  TopZone    := Top    div CurrentMap.ZoneHeight;
  RightZone  := Right  div CurrentMap.ZoneWidth;
  BottomZone := Bottom div CurrentMap.ZoneHeight;

  // Dessin des cases
  QPos.Map := CurrentMap;
  for X := Left to Right do
  begin
    for Y := Top to Bottom do
    begin
      QPos.Position := Point3D(X, Y, CurrentFloor);

      Context := TDrawSquareContext.Create(PaintBoxMap.Buffer,
        (MinViewSize+X) * SquareSize, (MinViewSize+Y) * SquareSize, QPos);
      try
        CurrentMap[QPos.Position].Draw(Context);
      finally
        Context.free;
      end;
    end;
  end;

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if (Map = CurrentMap) and (Position.Z = CurrentFloor) then
      begin
        DrawInPlace(PaintBoxMap.Buffer, (MinViewSize+Position.X) * SquareSize,
          (MinViewSize+Position.Y) * SquareSize);
      end;
    end;
  end;

  // Dessin du plafond
  QPos.Map := CurrentMap;
  for X := Left to Right do
  begin
    for Y := Top to Bottom do
    begin
      QPos.Position := Point3D(X, Y, CurrentFloor);

      Context := TDrawSquareContext.Create(PaintBoxMap.Buffer,
        (MinViewSize+X) * SquareSize, (MinViewSize+Y) * SquareSize, QPos);
      try
        CurrentMap[QPos.Position].DrawCeiling(Context);
      finally
        Context.free;
      end;
    end;
  end;

  // Dessin des lignes de séparation des zones
  with PaintBoxMap.Buffer do
  begin
    for X := LeftZone to RightZone do
    begin
      FillRectS(
        (CurrentMap.ZoneWidth * X + 1) * SquareSize - 1,
        Top * SquareSize,
        (CurrentMap.ZoneWidth * X + 1) * SquareSize + 1,
        (Bottom+2) * SquareSize,
        clBlack32);
    end;

    for Y := TopZone to BottomZone do
    begin
      FillRectS(
        Left * SquareSize,
        (CurrentMap.ZoneHeight * Y + 1) * SquareSize - 1,
        (Right+2) * SquareSize,
        (CurrentMap.ZoneHeight * Y + 1) * SquareSize + 1,
        clBlack32);
    end;
  end;

  // Dessins supplémentaires
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self, PaintBoxMap.Buffer, SquareClipRect);
end;

{*
  Gestionnaire d'événement OnMouseDown de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Boutonde la souris qui a été enfoncé
  @param Shift    État des touches système
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
*}
procedure TFrameBaseMapViewer.PaintBoxMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  QPos: TQualifiedPos;
begin
  if (Button <> mbLeft) or (CurrentMap = nil) then
    Exit;

  if Assigned(FOnClickSquare) then
  begin
    QPos.Map := CurrentMap;
    QPos.Position :=
      Point3D(X div SquareSize - 1, Y div SquareSize - 1, CurrentFloor);

    FOnClickSquare(Self, QPos);
  end;
end;

{*
  Gestionnaire d'événement OnMouseMove de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Shift    État des touches système
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
*}
procedure TFrameBaseMapViewer.PaintBoxMapMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  QPos: TQualifiedPos;
begin
  if CurrentMap = nil then
    Exit;

  QPos.Map := CurrentMap;
  QPos.Position :=
    Point3D(X div SquareSize - 1, Y div SquareSize - 1, CurrentFloor);

  StaticPosition.Caption := Point3DToString(QPos.Position, ', ');

  with CurrentMap[QPos.Position] do
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

  if Assigned(FOnOverSquare) then
    FOnOverSquare(Self, QPos);
end;

{*
  Gestionnaire d'événement OnChange de la zone d'édition de l'étage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameBaseMapViewer.EditFloorChange(Sender: TObject);
begin
  FCurrentFloor := EditFloor.Value;
  PaintBoxMap.Invalidate;
end;

end.

