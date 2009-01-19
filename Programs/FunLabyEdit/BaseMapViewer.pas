unit BaseMapViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Spin, ExtCtrls, Tabs, ScUtils, FunLabyUtils, FilesUtils;

type
  TPaintMapEvent = procedure(Sender: TObject; Canvas: TCanvas;
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
    PaintBoxMap: TPaintBox;
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
    procedure MapTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PaintBoxMapPaint(Sender: TObject);
    procedure PaintBoxMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditFloorChange(Sender: TObject);
  private
    FMaster: TMaster;         /// Ma�tre FunLabyrinthe

    FCurrentMap: TMap;      /// Carte courante
    FCurrentFloor: Integer; /// �tage courant

    FOnAfterPaint: TPaintMapEvent; /// Dessine au-dessus du reste

    FOnClickSquare: TSquarePosEvent; /// D�clench� au clic sur une case
    FOnOverSquare: TSquarePosEvent;  /// D�clench� au passage sur une case

    procedure SetMaster(Value: TMaster);

    procedure SetCurrentMap(Value: TMap);
    procedure SetCurrentFloor(Value: Integer);
  public
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
  Modifie l'�tage courant
  @param Value   Nouvel �tage
*}
procedure TFrameBaseMapViewer.SetCurrentFloor(Value: Integer);
begin
  EditFloor.Value := Value; // Will trigger EditFloor.OnChange
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
  Gestionnaire d'�v�nement OnPaint de la paint-box de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameBaseMapViewer.PaintBoxMapPaint(Sender: TObject);
var
  Left, Top, Right, Bottom: Integer;
  SquareClipRect: TRect;
  LeftZone, TopZone, RightZone, BottomZone: Integer;
  I, X, Y: Integer;
  QPos: TQualifiedPos;
begin
  if CurrentMap = nil then
    Exit;

  // Calcul des coordonn�es � afficher
  Left := ScrollBoxMap.HorzScrollBar.Position div SquareSize - MinViewSize;
  Top  := ScrollBoxMap.VertScrollBar.Position div SquareSize - MinViewSize;
  Right  := Left + ScrollBoxMap.ClientWidth  div SquareSize + 1;
  Bottom := Top  + ScrollBoxMap.ClientHeight div SquareSize + 1;
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
      CurrentMap[QPos.Position].Draw(QPos, PaintBoxMap.Canvas,
        (MinViewSize+X) * SquareSize, (MinViewSize+Y) * SquareSize);
    end;
  end;

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if (Map = CurrentMap) and (Position.Z = CurrentFloor) then
      begin
        DrawInPlace(PaintBoxMap.Canvas, (MinViewSize+Position.X) * SquareSize,
          (MinViewSize+Position.Y) * SquareSize);
      end;
    end;
  end;

  // Dessin des lignes de s�paration des zones
  with PaintBoxMap.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psDash;
    Pen.Width := 3;

    for X := LeftZone to RightZone do
    begin
      MoveTo((CurrentMap.ZoneWidth * X + 1) * SquareSize, Top * SquareSize);
      LineTo((CurrentMap.ZoneWidth * X + 1) * SquareSize,
        (Bottom+2) * SquareSize);
    end;

    for Y := TopZone to BottomZone do
    begin
      MoveTo(Left * SquareSize, (CurrentMap.ZoneHeight * Y + 1) * SquareSize);
      LineTo((Right+2) * SquareSize,
        (CurrentMap.ZoneHeight * Y + 1) * SquareSize);
    end;

    Pen.Width := 1;
  end;

  // Dessins suppl�mentaires
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self, PaintBoxMap.Canvas, SquareClipRect);
end;

{*
  Gestionnaire d'�v�nement OnMouseDown de la paint-box de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   Boutonde la souris qui a �t� enfonc�
  @param Shift    �tat des touches syst�me
  @param X        Abscisse du point cliqu�
  @param Y        Ordonn�e du point cliqu�
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
  Gestionnaire d'�v�nement OnMouseMove de la paint-box de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Shift    �tat des touches syst�me
  @param X        Abscisse du point cliqu�
  @param Y        Ordonn�e du point cliqu�
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
  Gestionnaire d'�v�nement OnChange de la zone d'�dition de l'�tage
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameBaseMapViewer.EditFloorChange(Sender: TObject);
begin
  FCurrentFloor := EditFloor.Value;
  PaintBoxMap.Invalidate;
end;

end.

