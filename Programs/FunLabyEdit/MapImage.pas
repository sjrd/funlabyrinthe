unit MapImage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32, GR32_Image, GR32_Layers, ScUtils, FunLabyUtils;

type
  TPaintMapEvent = procedure(Sender: TObject; Bitmap: TBitmap32;
    Map: TMap; Floor: Integer) of object;

  TSquarePosEvent = procedure(Sender: TObject;
    const QPos: TQualifiedPos) of object;

  TSquareMouseEvent = procedure(Sender: TObject; const QPos: TQualifiedPos;
    Button: TMouseButton; Shift: TShiftState) of object;

  {*
    Cadre dessinant l'image d'une carte
    @author sjrd
    @version 5.0
  *}
  TFrameMapImage = class(TFrame)
    MapView: TImgView32;
    procedure MapViewInitStages(Sender: TObject);
    procedure MapViewPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure MapViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    FMap: TMap;      /// Carte affich�e
    FFloor: Integer; /// �tage de la carte affich�

    FSelectedPos: TQualifiedPos; /// Position s�lectionn�e

    FOnAfterPaintMap: TPaintMapEvent; /// Dessine au-dessus de la carte

    /// D�clench� lorsque l'utilisateur a cliqu� sur une case
    FOnClickSquare: TSquarePosEvent;
    /// D�clench� lorsque l'utilisateur a double-cliqu� sur une case
    FOnDblClickSquare: TSquarePosEvent;
    /// D�clench� lorsque la souris survole une case
    FOnHoverSquare: TSquarePosEvent;
    /// D�clench� lorsqu'un bouton de la souris est press� sur une case
    FOnSquareMouseDown: TSquareMouseEvent;
    /// D�clench� lorsqu'un bouton de la souris est rel�ch� au-dessus d'une case
    FOnSquareMouseUp: TSquareMouseEvent;

    function GetPosFromPoint(X, Y: Integer): T3DPoint;
    function GetQPosFromPoint(X, Y: Integer): TQualifiedPos;

    procedure DoPaintMap(Buffer: TBitmap32);
    procedure DoDrawZoneLimits(Buffer: TBitmap32);
    procedure DoDrawSelectedPos(Buffer: TBitmap32);
    procedure DoAfterPaintMap(Buffer: TBitmap32);

    procedure SetMap(Value: TMap);
    procedure SetFloor(Value: Integer);

    procedure SetSelectedPos(const Value: TQualifiedPos);

    function GetScale: Single;
    procedure SetScale(Value: Single);
  public
    procedure InvalidateMap;

    procedure ShowPosition(const QPos: TQualifiedPos); overload;
    procedure ShowPosition(Map: TMap; const Position: T3DPoint); overload;
    procedure ShowPosition(const Position: T3DPoint); overload;

    property Map: TMap read FMap write SetMap;
    property Floor: Integer read FFloor write SetFloor;

    property SelectedPos: TQualifiedPos read FSelectedPos write SetSelectedPos;

    property Scale: Single read GetScale write SetScale;

    property OnAfterPaintMap: TPaintMapEvent
      read FOnAfterPaintMap write FOnAfterPaintMap;

    property OnClickSquare: TSquarePosEvent
      read FOnClickSquare write FOnClickSquare;
    property OnDblClickSquare: TSquarePosEvent
      read FOnDblClickSquare write FOnDblClickSquare;
    property OnHoverSquare: TSquarePosEvent
      read FOnHoverSquare write FOnHoverSquare;
    property OnSquareMouseDown: TSquareMouseEvent
      read FOnSquareMouseDown write FOnSquareMouseDown;
    property OnSquareMouseUp: TSquareMouseEvent
      read FOnSquareMouseUp write FOnSquareMouseUp;
  end;

implementation

{$R *.dfm}

const
  BorderSize = 1;

  psDrawZoneLimits = 1;
  psAfterPaintMapEvent = 2;

{----------------------}
{ TFrameMapImage class }
{----------------------}

{*
  Obtient la position d'une case � partir d'un point sur la vue de la carte
  @param X   Abscisse du point sur la vue de la carte
  @param Y   Ordonn�e du point sur la vue de la carte
  @param Position
*}
function TFrameMapImage.GetPosFromPoint(X, Y: Integer): T3DPoint;
var
  BmpPoint: TPoint;
begin
  BmpPoint := MapView.ControlToBitmap(Point(X, Y));

  if (Map = nil) or (not PtInRect(MapView.Bitmap.BoundsRect, BmpPoint)) then
    Result := No3DPoint
  else
  begin
    Result.X := BmpPoint.X div SquareSize - BorderSize;
    Result.Y := BmpPoint.Y div SquareSize - BorderSize;
    Result.Z := Floor;
  end;
end;

{*
  Obtient la position qualifi�e d'une case � partir d'un point sur la vue
  @param X   Abscisse du point sur la vue de la carte
  @param Y   Ordonn�e du point sur la vue de la carte
  @return Position qualifi�e
*}
function TFrameMapImage.GetQPosFromPoint(X, Y: Integer): TQualifiedPos;
begin
  Result.Position := GetPosFromPoint(X, Y);

  if IsNo3DPoint(Result.Position) then
    Result.Map := nil
  else
    Result.Map := Map;
end;

{*
  Dessine la carte
  @param Buffer   Buffer sur lequel dessiner la carte
*}
procedure TFrameMapImage.DoPaintMap(Buffer: TBitmap32);
var
  Left, Top, Right, Bottom: Integer;
  I, X, Y: Integer;
  QPos: TQualifiedPos;
  Context: TDrawSquareContext;
begin
  // Calcul des coordonn�es � afficher
  Left := 0 - BorderSize;
  Top := 0 - BorderSize;
  Right := Map.Dimensions.X + BorderSize;
  Bottom := Map.Dimensions.Y + BorderSize;

  // Dessin des cases
  QPos.Map := Map;
  for X := Left to Right-1 do
  begin
    for Y := Top to Bottom-1 do
    begin
      QPos.Position := Point3D(X, Y, Floor);

      Context := TDrawSquareContext.Create(Buffer,
        (BorderSize+X) * SquareSize, (BorderSize+Y) * SquareSize, QPos);
      try
        QPos.Square.Draw(Context);
      finally
        Context.free;
      end;
    end;
  end;

  // Dessin des composants avec position
  for I := 0 to Map.Master.PosComponentCount-1 do
  begin
    with Map.Master.PosComponents[I] do
    begin
      if (Map = Self.Map) and (Position.Z = Floor) then
      begin
        Context := TDrawSquareContext.Create(Buffer,
          (BorderSize+Position.X) * SquareSize,
          (BorderSize+Position.Y) * SquareSize, QPos);
        try
          Draw(Context);
        finally
          Context.free;
        end;
      end;
    end;
  end;

  // Dessin du plafond
  QPos.Map := Map;
  for X := Left to Right-1 do
  begin
    for Y := Top to Bottom-1 do
    begin
      QPos.Position := Point3D(X, Y, Floor);

      Context := TDrawSquareContext.Create(Buffer,
        (BorderSize+X) * SquareSize, (BorderSize+Y) * SquareSize, QPos);
      try
        QPos.Square.DrawCeiling(Context);
      finally
        Context.free;
      end;
    end;
  end;
end;

{*
  Dessine les limites des zones
  @param Buffer   Buffer sur lequel dessiner les limites des zones
*}
procedure TFrameMapImage.DoDrawZoneLimits(Buffer: TBitmap32);
var
  X, Y: Integer;
begin
  for X := 0 to Map.Dimensions.X div Map.ZoneWidth do
  begin
    Buffer.FillRectS(
      (Map.ZoneWidth * X + 1) * SquareSize - 1,
      0,
      (Map.ZoneWidth * X + 1) * SquareSize + 1,
      Buffer.Height,
      clBlack32);
  end;

  for Y := 0 to Map.Dimensions.Y div Map.ZoneHeight do
  begin
    Buffer.FillRectS(
      0,
      (Map.ZoneHeight * Y + 1) * SquareSize - 1,
      Buffer.Width,
      (Map.ZoneHeight * Y + 1) * SquareSize + 1,
      clBlack32);
  end;
end;

{*
  Dessine la case s�lectionn�e
  @param Buffer   Buffer sur lequel dessiner la case s�lectionn�e
*}
procedure TFrameMapImage.DoDrawSelectedPos(Buffer: TBitmap32);
var
  SelPoint: TPoint;
  I: Integer;
begin
  if (SelectedPos.Map = Map) and (SelectedPos.Z = Floor) then
  begin
    SelPoint := Point(SelectedPos.X, SelectedPos.Y);

    // Draw the selection rectangle
    for I := -1 to 1 do
      with SelPoint do
        Buffer.FrameRectS((X+1) * SquareSize - I, (Y+1) * SquareSize - I,
          (X+2) * SquareSize + I, (Y+2) * SquareSize + I, clYellow32);
  end;
end;

{*
  D�clenche l'�v�nement OnAfterPaintMap
  @param Buffer   Buffer � transmettre au gestionnaire d'�v�nement
*}
procedure TFrameMapImage.DoAfterPaintMap(Buffer: TBitmap32);
begin
  if Assigned(FOnAfterPaintMap) then
    FOnAfterPaintMap(Self, Buffer, Map, Floor);
end;

{*
  Modifie la carte � afficher
  @param Value   Nouvelle carte � afficher
*}
procedure TFrameMapImage.SetMap(Value: TMap);
begin
  if Value <> FMap then
  begin
    FMap := Value;
    FFloor := 0;
    InvalidateMap;
  end;
end;

{*
  Modifie l'�tage � afficher
  @param Value   Nouvel �tage � afficher
*}
procedure TFrameMapImage.SetFloor(Value: Integer);
begin
  if (Map <> nil) and (Value <> FFloor) then
  begin
    FFloor := MinMax(Value, 0, Map.Dimensions.Z-1);
    InvalidateMap;
  end;
end;

{*
  Modifie la position s�lectionn�e
  @param Value   Nouvelle position s�lectionn�e
*}
procedure TFrameMapImage.SetSelectedPos(const Value: TQualifiedPos);
begin
  if not SameQPos(Value, FSelectedPos) then
  begin
    FSelectedPos := Value;
    InvalidateMap;
  end;
end;

{*
  �chelle d'affichage
  @return �chelle d'affichage
*}
function TFrameMapImage.GetScale: Single;
begin
  Result := MapView.Scale;
end;

{*
  Modifie l'�chelle d'affichage
  @param Value   Nouvelle �chelle d'affichage
*}
procedure TFrameMapImage.SetScale(Value: Single);
begin
  MapView.Scale := Value;
end;

{*
  Invalide la carte
*}
procedure TFrameMapImage.InvalidateMap;
var
  Buffer: TBitmap32;
begin
  Buffer := MapView.Bitmap;
  Buffer.BeginUpdate;
  try
    if Map = nil then
      Buffer.SetSize(0, 0)
    else
    begin
      with Map.Dimensions do
        Buffer.SetSize((X+2) * SquareSize, (Y+2) * SquareSize);

      DoPaintMap(Buffer);
      DoDrawZoneLimits(Buffer);
      DoDrawSelectedPos(Buffer);
      DoAfterPaintMap(Buffer);
    end;
  finally
    Buffer.EndUpdate;
  end;

  MapView.Invalidate;
end;

{*
  Centre la vue sur une position donn�e
  @param QPos   Position qualifi�e � montrer
*}
procedure TFrameMapImage.ShowPosition(const QPos: TQualifiedPos);
begin
  Map := QPos.Map;
  ShowPosition(QPos.Position);
end;

{*
  Centre la vue sur une position donn�e
  @param Map        Carte � montrer
  @param Position   Position � montrer
*}
procedure TFrameMapImage.ShowPosition(Map: TMap; const Position: T3DPoint);
begin
  Self.Map := Map;
  ShowPosition(Position);
end;

{*
  Centre la vue sur une position donn�e
  @param Position   Position � montrer
*}
procedure TFrameMapImage.ShowPosition(const Position: T3DPoint);
begin
  Floor := Position.Z;
  MapView.ScrollToCenter((Position.X+BorderSize) * SquareSize + HalfSquareSize,
    (Position.Y+BorderSize) * SquareSize + HalfSquareSize);
end;

{*
  Gestionnaire d'�v�nement OnInitStages de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameMapImage.MapViewInitStages(Sender: TObject);

  procedure AddStage(Param: Cardinal);
  begin
    with MapView.PaintStages.Add^ do
    begin
      DsgnTime := False;
      RunTime := True;
      Stage := PST_CUSTOM;
      Parameter := Param;
    end;
  end;

begin
  AddStage(psDrawZoneLimits);
  AddStage(psAfterPaintMapEvent);
end;

{*
  Gestionnaire d'�v�nement OnPaintState de la vue de la carte
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param Buffer     Buffer � dessiner
  @param StageNum   Num�ro de l'�tape de dessin
*}
procedure TFrameMapImage.MapViewPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
begin
  if Map = nil then
    Exit;

  case MapView.PaintStages[StageNum]^.Parameter of
    psDrawZoneLimits: DoDrawZoneLimits(Buffer);
    psAfterPaintMapEvent: DoAfterPaintMap(Buffer);
  end;
end;

{*
  Gestionnaire d'�v�nement OnMouseDown de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   Bouton enfonc�
  @param Shift    �tat des touches sp�ciales
  @param X        Abscisse du point
  @param Y        Ordonn�e du point
  @param Layer    Calque
*}
procedure TFrameMapImage.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  QPos: TQualifiedPos;
begin
  QPos := GetQPosFromPoint(X, Y);
  if IsNoQPos(QPos) then
    Exit;

  if Assigned(FOnSquareMouseDown) then
    FOnSquareMouseDown(Self, QPos, Button, Shift);

  if Button = mbLeft then
  begin
    if ssDouble in Shift then
    begin
      if Assigned(FOnDblClickSquare) then
        FOnDblClickSquare(Self, QPos);
    end else
    begin
      if Assigned(FOnClickSquare) then
        FOnClickSquare(Self, QPos);
    end;
  end;
end;

{*
  Gestionnaire d'�v�nement OnMouseMove de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Shift    �tat des touches sp�ciales
  @param X        Abscisse du point
  @param Y        Ordonn�e du point
  @param Layer    Calque
*}
procedure TFrameMapImage.MapViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  QPos: TQualifiedPos;
begin
  QPos := GetQPosFromPoint(X, Y);
  if IsNoQPos(QPos) then
    Exit;

  if Assigned(FOnHoverSquare) then
    FOnHoverSquare(Self, QPos);
end;

{*
  Gestionnaire d'�v�nement OnMouseUp de la vue de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   Bouton enfonc�
  @param Shift    �tat des touches sp�ciales
  @param X        Abscisse du point
  @param Y        Ordonn�e du point
  @param Layer    Calque
*}
procedure TFrameMapImage.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  QPos: TQualifiedPos;
begin
  QPos := GetQPosFromPoint(X, Y);
  if IsNoQPos(QPos) then
    Exit;

  if Assigned(FOnSquareMouseUp) then
    FOnSquareMouseUp(Self, QPos, Button, Shift);
end;

end.

