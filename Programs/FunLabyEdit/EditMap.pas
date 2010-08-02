unit EditMap;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MapImage, ComCtrls, Spin, StdCtrls, Buttons, ScUtils, SdDialogs,
  FunLabyUtils, FunLabyEditConsts, JvValidators, JvErrorIndicator,
  JvComponentBase, ScIntegerSets, GR32;

type
  /// Direction de redimensionnement
  TResizeDirection = (rdNone, rdNorth, rdEast, rdSouth, rdWest, rdUp, rdDown);

  {*
    Carte redimensionnable
    @author sjrd
    @version 5.0
  *}
  TResizableMap = class(TMap)
  public
    procedure Resize(const NewDimensions, OrigOffset: T3DPoint);
  end;

  {*
    Boîte de dialogue d'édition d'une carte
    @author sjrd
    @version 5.0
  *}
  TFormEditMap = class(TForm)
    MapImage: TFrameMapImage;
    ButtonGrowTop: TSpeedButton;
    ButtonShrinkTop: TSpeedButton;
    ButtonGrowLeft: TSpeedButton;
    ButtonShrinkLeft: TSpeedButton;
    ButtonGrowBottom: TSpeedButton;
    ButtonShrinkBottom: TSpeedButton;
    ButtonGrowRight: TSpeedButton;
    ButtonShrinkRight: TSpeedButton;
    LabelMapID: TLabel;
    EditMapID: TEdit;
    LabelScale: TLabel;
    TrackBarScale: TTrackBar;
    GroupBoxFillZone: TGroupBox;
    GroupBoxZoneSize: TGroupBox;
    LabelZoneWidth: TLabel;
    LabelZoneHeight: TLabel;
    EditZoneWidth: TSpinEdit;
    EditZoneHeight: TSpinEdit;
    LabelZoneField: TLabel;
    ComboBoxZoneField: TComboBox;
    LabelZoneGrid: TLabel;
    ComboBoxZoneGrid: TComboBox;
    LabelZoneBorder: TLabel;
    ComboBoxZoneBorder: TComboBox;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonFillZone: TButton;
    GroupBoxFloors: TGroupBox;
    EditFloor: TSpinEdit;
    ButtonGrowUp: TSpeedButton;
    ButtonShrinkUp: TSpeedButton;
    ButtonShrinkDown: TSpeedButton;
    ButtonGrowDown: TSpeedButton;
    Validators: TJvValidators;
    MapIDFormatValidator: TJvCustomValidator;
    MapIDRequiredValidator: TJvRequiredFieldValidator;
    ZoneWidthValidator: TJvCustomValidator;
    ZoneHeightValidator: TJvCustomValidator;
    ErrorIndicator: TJvErrorIndicator;
    GroupBoxOutside: TGroupBox;
    LabelOutside: TLabel;
    ComboBoxOutside: TComboBox;
    procedure MapImageAfterPaintMap(Sender: TObject; Bitmap: TBitmap32;
      Map: TMap; Floor: Integer);
    procedure MapImageSquareMouseDown(Sender: TObject;
      const QPos: TQualifiedPos; Button: TMouseButton; Shift: TShiftState);
    procedure TrackBarScaleChange(Sender: TObject);
    procedure ButtonResizeClick(Sender: TObject);
    procedure EditZoneSizeChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure EditFloorChange(Sender: TObject);
    procedure ButtonFillZoneClick(Sender: TObject);
    procedure MapIDFormatValidatorValidate(Sender: TObject;
      ValueToValidate: Variant; var Valid: Boolean);
    procedure ZoneWidthValidatorValidate(Sender: TObject;
      ValueToValidate: Variant; var Valid: Boolean);
    procedure ZoneHeightValidatorValidate(Sender: TObject;
      ValueToValidate: Variant; var Valid: Boolean);
    procedure ComboBoxOutsideChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMaster: TMaster;    /// Maître FunLabyrinthe
    FMap: TResizableMap; /// Carte en édition
    FIsNewMap: Boolean;  /// Indique si on créée une nouvelle carte

    FTotalOrigOffset: T3DPoint; /// Offset total de l'origine

    FSelectedZones: TScIntegerSet; /// Liste des zones sélectionnées
    FLastSelectedZone: TPoint;     /// Dernière zone sélectionnée

    function CheckAtLeastOneField: Boolean;
    procedure PrepareFieldList;
    procedure Prepare(const AMapID: TComponentID);

    procedure InvalidateMap;

    procedure ResizeMap(const NewDims, OrigOffset: T3DPoint); overload;
    procedure ResizeMap(Dir: TResizeDirection; IsShrink: Boolean); overload;

    procedure UpdatePosComponents(AMap: TMap);

    function ZoneToInt(const Zone: TPoint): Integer;
    function IntToZone(ZoneInt: Integer): TPoint;
    function IsZoneSelected(const Zone: TPoint): Boolean;
    procedure ClearSelectedZones;
    procedure SelectZone(const Zone: TPoint);
    procedure UnselectZone(const Zone: TPoint);
    procedure SelectZoneRect(const ZoneRect: TRect);

    function IsBorder(const Zone: TPoint; Dir: TDirection): Boolean;

    procedure FillZoneField(const PosRect: TRect; Field: TField);
    procedure FillZoneGrid(const PosRect: TRect; Field: TField);
    procedure FillZoneBorder(const PosRect: TRect; Field: TField;
      const Zone: TPoint);

    function DoNewMap(AMaster: TMaster): TMap;
    function DoEditMap(AMap: TMap): Boolean;

    property Master: TMaster read FMaster;
    property Map: TResizableMap read FMap;
    property IsNewMap: Boolean read FIsNewMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function NewMap(AMaster: TMaster): TMap;
    class function EditMap(AMap: TMap): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Math;

{---------------------}
{ TResizableMap class }
{---------------------}

{*
  Redimensionne la carte
  @param NewDimensions   Nouvelles dimensions
  @param OrigOffset      Offset de l'ancienne origine dans la nouvelle carte
*}
procedure TResizableMap.Resize(const NewDimensions, OrigOffset: T3DPoint);

  function NewLinearIndex(X, Y, Z: Integer): Integer;
  begin
    Result := Z;
    Result := Result * NewDimensions.Y + Y;
    Result := Result * NewDimensions.X + X;
  end;

var
  NewMap: TSquareDynArray;
  OutsideOffset, X, Y, Z: Integer;
  CopyMinima, CopyMaxima, NegOrigOffset: T3DPoint;
  DefaultSquare: TSquare;
begin
  // Create NewMap and compute OutsideOffset
  OutsideOffset := NewDimensions.X * NewDimensions.Y * NewDimensions.Z;
  SetLength(NewMap, OutsideOffset + NewDimensions.Z);

  // Fill the new map with a default square
  DefaultSquare := Master.SquareByComps(Master.Fields[0]);
  for X := 0 to Length(NewMap)-1 do
    NewMap[X] := DefaultSquare;

  // Compute copy extrema and NegOrigOffset
  CopyMinima.X := Max(OrigOffset.X, 0);
  CopyMinima.Y := Max(OrigOffset.Y, 0);
  CopyMinima.Z := Max(OrigOffset.Z, 0);
  CopyMaxima.X := Min(Dimensions.X+OrigOffset.X, NewDimensions.X);
  CopyMaxima.Y := Min(Dimensions.Y+OrigOffset.Y, NewDimensions.Y);
  CopyMaxima.Z := Min(Dimensions.Z+OrigOffset.Z, NewDimensions.Z);
  NegOrigOffset.X := -OrigOffset.X;
  NegOrigOffset.Y := -OrigOffset.Y;
  NegOrigOffset.Z := -OrigOffset.Z;

  // Copy old map to new map
  for X := CopyMinima.X to CopyMaxima.X-1 do
    for Y := CopyMinima.Y to CopyMaxima.Y-1 do
      for Z := CopyMinima.Z to CopyMaxima.Z-1 do
        NewMap[NewLinearIndex(X, Y, Z)] :=
          Map[Point3DAdd(NegOrigOffset, X, Y, Z)];
  for Z := 0 to NewDimensions.Z-1 do
    NewMap[OutsideOffset+Z] := Outside[Z+NegOrigOffset.Z];

  // Replace map
  ReplaceMap(NewDimensions, NewMap);
end;

{--------------------}
{ TFormEditMap class }
{--------------------}

{*
  [@inheritDoc]
*}
constructor TFormEditMap.Create(AOwner: TComponent);
begin
  inherited;

  FSelectedZones := TScIntegerSet.Create;
end;

{*
  [@inheritDoc]
*}
destructor TFormEditMap.Destroy;
begin
  FSelectedZones.Free;

  inherited;
end;

{*
  Vérifie qu'il y a au moins un terrain disponible
  @return True s'il y a au moins un terrain disponible, False sinon
*}
function TFormEditMap.CheckAtLeastOneField: Boolean;
begin
  Result := Master.FieldCount > 0;

  if not Result then
    ShowDialog(SNoFieldTitle, SNoField, dtError);
end;

{*
  Prépare la liste des terrains disponibles
*}
procedure TFormEditMap.PrepareFieldList;
var
  I: Integer;
  Field: TField;
  FieldName: string;
begin
  for I := 0 to Master.FieldCount-1 do
  begin
    Field := Master.Fields[I];
    FieldName := Field.Name;

    if Field.IsDesignable then
    begin
      ComboBoxZoneField.Items.AddObject(FieldName, Field);
      ComboBoxZoneGrid.Items.AddObject(FieldName, Field);
      ComboBoxZoneBorder.Items.AddObject(FieldName, Field);
      ComboBoxOutside.Items.AddObject(FieldName, Field);
    end;
  end;

  ComboBoxZoneField.ItemIndex := 0;
  ComboBoxZoneGrid.ItemIndex := 0;
  ComboBoxZoneBorder.ItemIndex := 0;
end;

{*
  Prépare la fiche
*}
procedure TFormEditMap.Prepare(const AMapID: TComponentID);
begin
  // Prepare field list
  PrepareFieldList;

  // Map image
  InvalidateMap;
  TrackBarScaleChange(nil);
  MapImage.Map := Map;

  // Map ID editor
  EditMapID.Text := AMapID;
  EditMapID.Enabled := IsNewMap;

  // Zone size editors
  EditZoneWidth.Value := Map.ZoneWidth;
  EditZoneHeight.Value := Map.ZoneHeight;
  EditZoneWidth.OnChange := EditZoneSizeChange;
  EditZoneHeight.OnChange := EditZoneSizeChange;
end;

{*
  Invalide la carte
*}
procedure TFormEditMap.InvalidateMap;
begin
  MapImage.InvalidateMap;

  EditFloor.MaxValue := Map.Dimensions.Z-1;
  EditFloor.Enabled := Map.Dimensions.Z > 1;
  EditFloor.Value := EditFloor.Value;

  ComboBoxOutside.ItemIndex :=
    ComboBoxOutside.Items.IndexOfObject(Map.Outside[MapImage.Floor].Field);
end;

{*
  Erreur de redimensionnement
  @param Msg   Message d'erreur
*}
procedure ResizeError(const Msg: string);
begin
  ShowDialog(SResizeErrorTitle, Msg, dtError);
  Abort;
end;

{*
  Redimensionne la carte
  @param NewDims      Nouvelles dimensions
  @param OrigOffset   Offset de l'origine
*}
procedure TFormEditMap.ResizeMap(const NewDims, OrigOffset: T3DPoint);
begin
  if (NewDims.X <= 0) or (NewDims.Y <= 0) or (NewDims.Z <= 0) then
    ResizeError(SCantResizeToEmpty);

  // TODO Warn the user about dangerous changes

  Map.Resize(NewDims, OrigOffset);

  Inc(FTotalOrigOffset.X, OrigOffset.X);
  Inc(FTotalOrigOffset.Y, OrigOffset.Y);
  Inc(FTotalOrigOffset.Z, OrigOffset.Z);
end;

{*
  Redimensionne la carte
  @param Dir        Direction dans laquelle redimensionner
  @param IsShrink   Indique si c'est un rétrécisemment
*}
procedure TFormEditMap.ResizeMap(Dir: TResizeDirection; IsShrink: Boolean);
type
  T3DPointEntry = record
    case Integer of
      0: (Point: T3DPoint);
      1: (X, Y, Z: Integer);
      2: (Coord: array[0..2] of Integer);
  end;
const
  DirToCoordIdx: array[TResizeDirection] of Integer = (-1, 1, 0, 1, 0, 2, 2);
var
  CoordIdx: Integer;
  Dims, NewDims, OrigOffset, ZoneSize: T3DPointEntry;
  Amount, Modulo: Integer;
begin
  Assert(Dir in [rdNorth..rdDown]);

  // Initialization
  CoordIdx := DirToCoordIdx[Dir];
  Dims.Point := Map.Dimensions;
  ZoneSize.Point := Point3D(Map.ZoneWidth, Map.ZoneHeight, 1);

  // Compute Amount
  Amount := ZoneSize.Coord[CoordIdx];
  if IsShrink then
    Amount := -Amount;
  if Dir in [rdEast, rdSouth] then
  begin
    Modulo := Dims.Coord[CoordIdx] mod ZoneSize.Coord[CoordIdx];
    if Modulo <> 0 then
    begin
      if IsShrink then
        Amount := -Modulo
      else
        Amount := Amount-Modulo;
    end;
  end;

  // Compute NewDims and OrigOffset
  NewDims := Dims;
  Inc(NewDims.Coord[CoordIdx], Amount);
  OrigOffset.Point := Point3D(0, 0, 0);
  if Dir in [rdNorth, rdWest, rdDown] then
    OrigOffset.Coord[CoordIdx] := Amount;

  // Resize map to NewDims and OrigOffset
  ResizeMap(NewDims.Point, OrigOffset.Point);

  // Invalidate map and clear selected zones
  ClearSelectedZones;
  InvalidateMap;

  // Scroll view to modified area
  case Dir of
    rdNorth:
      MapImage.MapView.Scroll(0, -MaxInt);
    rdEast:
      MapImage.MapView.Scroll(MaxInt, 0);
    rdSouth:
      MapImage.MapView.Scroll(0, MaxInt);
    rdWest:
      MapImage.MapView.Scroll(-MaxInt, 0);
    rdUp:
      EditFloor.Value := Map.Dimensions.Z-1;
    rdDown:
      EditFloor.Value := 0;
  end;
end;

{*
  Déplace les composants à position (TPosComponent) selon l'offset total
  @param AMap   Carte dont il faut déplacer les composants à position
*}
procedure TFormEditMap.UpdatePosComponents(AMap: TMap);
var
  I: Integer;
  PosComponent: TPosComponent;
begin
  for I := 0 to Master.PosComponentCount-1 do
  begin
    PosComponent := Master.PosComponents[I];

    if PosComponent.Map = AMap then
    begin
      PosComponent.ChangePosition(Point3DAdd(
        PosComponent.Position, FTotalOrigOffset));
    end;
  end;
end;

{*
  Convertit une zone en nombre entier simple, pour FSelectedZones
  @return Entier représentant la zone
*}
function TFormEditMap.ZoneToInt(const Zone: TPoint): Integer;
begin
  Result := Zone.Y * (Map.Dimensions.X div Map.ZoneWidth + 1) + Zone.X;
end;

{*
  Convertit un nombre entier simple, pour FSelectedZones, en zone
  @param ZoneInt   Entier représetant la zone
  @return Zone représentée par l'entier ZoneInt
*}
function TFormEditMap.IntToZone(ZoneInt: Integer): TPoint;
var
  Divisor: Integer;
begin
  Divisor := Map.Dimensions.X div Map.ZoneWidth + 1;
  Result.X := ZoneInt mod Divisor;
  Result.Y := ZoneInt div Divisor;
end;

{*
  Teste si une zone est sélectionnée
  @param Zone   Zone à tester
  @return True si la zone est sélectionnée, False sinon
*}
function TFormEditMap.IsZoneSelected(const Zone: TPoint): Boolean;
begin
  Result := FSelectedZones.Exists(ZoneToInt(Zone));
end;

{*
  Efface l'ensemble des zones sélectionnées
*}
procedure TFormEditMap.ClearSelectedZones;
begin
  FSelectedZones.Clear;
  FLastSelectedZone := NoPoint;
end;

{*
  Sélectionne une zone
  @param Zone   Zone à sélectionner
*}
procedure TFormEditMap.SelectZone(const Zone: TPoint);
begin
  FSelectedZones.Include(ZoneToInt(Zone));
  FLastSelectedZone := Zone;
end;

{*
  Désélectionne une zone
  @param Zone   Zone à désélectionner
*}
procedure TFormEditMap.UnselectZone(const Zone: TPoint);
begin
  FSelectedZones.Exclude(ZoneToInt(Zone));
  FLastSelectedZone := NoPoint;
end;

{*
  Sélectionne un rectangle de zones
  @param ZoneRect   Rectangle de zones à sélectionner
*}
procedure TFormEditMap.SelectZoneRect(const ZoneRect: TRect);
var
  X, Y: Integer;
begin
  for X := ZoneRect.Left to ZoneRect.Right-1 do
    for Y := ZoneRect.Top to ZoneRect.Bottom-1 do
      SelectZone(Point(X, Y));
end;

{*
  Teste s'il y a une bordure sur un côté d'une zone sélectionnée
  @param Zone   Zone à tester (zone sélectionnée)
  @param Dir    Direction du côté de la zone à tester
  @return True s'il y a une bordure, False sinon
*}
function TFormEditMap.IsBorder(const Zone: TPoint; Dir: TDirection): Boolean;
var
  Neighbor: TPoint;
begin
  Neighbor := Zone;

  case Dir of
    diNorth: Dec(Neighbor.Y);
    diEast: Inc(Neighbor.X);
    diSouth: Inc(Neighbor.Y);
    diWest: Dec(Neighbor.X);
  end;

  if not Map.InMap(Point3D(Neighbor.X*Map.ZoneWidth,
    Neighbor.Y*Map.ZoneHeight, 0)) then
    Result := True
  else
    Result := not IsZoneSelected(Neighbor);
end;

{*
  Remplit le terrain d'une zone
  @param PosRect   Rectangle des positions de la zone
  @param Field     Terrain à placer
*}
procedure TFormEditMap.FillZoneField(const PosRect: TRect; Field: TField);
var
  Square: TSquare;
  X, Y: Integer;
begin
  Square := Master.SquareByComps(Field);

  for X := PosRect.Left to PosRect.Right-1 do
    for Y := PosRect.Top to PosRect.Bottom-1 do
      Map[Point3D(X, Y, MapImage.Floor)] := Square;
end;

{*
  Remplit la grille d'une zone
  @param PosRect   Rectangle des positions de la zone
  @param Field     Terrain à placer (peut être nil)
*}
procedure TFormEditMap.FillZoneGrid(const PosRect: TRect; Field: TField);
var
  Square: TSquare;
  X, Y: Integer;
begin
  if Field = nil then
    Exit;

  Square := Master.SquareByComps(Field);

  for X := PosRect.Left to PosRect.Right-1 do
    for Y := PosRect.Top to PosRect.Bottom-1 do
      if not (Odd(X-PosRect.Left) or Odd(Y-PosRect.Top)) then
        Map[Point3D(X, Y, MapImage.Floor)] := Square;
end;

{*
  Remplit le contour d'une zone
  @param PosRect   Rectangle des positions de la zone
  @param Field     Terrain à placer (peut être nil)
*}
procedure TFormEditMap.FillZoneBorder(const PosRect: TRect; Field: TField;
  const Zone: TPoint);
var
  Dir: TDirection;
  SubRect: TRect;
begin
  if Field = nil then
    Exit;

  for Dir := diNorth to diWest do
  begin
    if not IsBorder(Zone, Dir) then
      Continue;

    SubRect := PosRect;

    case Dir of
      diNorth: SubRect.Bottom := SubRect.Top+1;
      diEast: SubRect.Left := SubRect.Right-1;
      diSouth: SubRect.Top := SubRect.Bottom-1;
      diWest: SubRect.Right := SubRect.Left+1;
    end;

    FillZoneField(SubRect, Field);
  end;
end;

{*
  Crée une nouvelle carte
  @param AMaster   Maître FunLabyrinthe
  @return Carte créée, ou nil si l'utilisateur a abandonné
*}
function TFormEditMap.DoNewMap(AMaster: TMaster): TMap;
const
  MainMapID = 'MainMap'; {don't localize}
  DefaultZoneSize = 7;
  DefaultDimensions: T3DPoint = (X: DefaultZoneSize; Y: DefaultZoneSize; Z: 1);
  DefaultOutsideID = 'Outside';
var
  DefaultMapID: TComponentID;
  I: Integer;
begin
  Result := nil;

  FMaster := AMaster;
  if not CheckAtLeastOneField then
    Exit;

  if Master.MapCount = 0 then
    DefaultMapID := MainMapID
  else
    DefaultMapID := '';

  FIsNewMap := True;
  FMap := TResizableMap.CreateSized(Master, '', DefaultDimensions,
    DefaultZoneSize, DefaultZoneSize);
  try
    for I := 0 to FMap.LinearMapCount-1 do
      FMap.LinearMap[I] := Master.SquareByComps(Master.Fields[0]);
    if Master.ComponentExists(DefaultOutsideID) then
      FMap.Outside[0] := Master.SquareByComps(DefaultOutsideID, '', '', '');

    FTotalOrigOffset := Point3D(0, 0, 0);

    Prepare(DefaultMapID);

    if ShowModal = mrOK then
    begin
      Result := Master.CreateAdditionnalComponent(TMap, EditMapID.Text) as TMap;
      Result.Assign(FMap);
    end;
  finally
    FreeAndNil(FMap);
  end;
end;

{*
  Édite une carte existante
  @param AMap   Carte à éditer
  @return True si la carte a été modifiée, False sinon
*}
function TFormEditMap.DoEditMap(AMap: TMap): Boolean;
begin
  Result := False;

  FMaster := AMap.Master;
  if not CheckAtLeastOneField then
    Exit;

  FIsNewMap := False;
  FMap := TResizableMap.Create(Master, '');
  try
    FMap.Assign(AMap);
    FTotalOrigOffset := Point3D(0, 0, 0);

    Prepare(AMap.ID);

    if ShowModal = mrOK then
    begin
      AMap.Assign(FMap);
      UpdatePosComponents(AMap);

      Result := True;
    end;
  finally
    FreeAndNil(FMap);
  end;
end;

{*
  Crée une nouvelle carte
  @param AMaster   Maître FunLabyrinthe
  @return Carte créée, ou nil si l'utilisateur a abandonné
*}
class function TFormEditMap.NewMap(AMaster: TMaster): TMap;
begin
  with Create(nil) do
  try
    Result := DoNewMap(AMaster);
  finally
    Release;
  end;
end;

{*
  Édite une carte existante
  @param AMap   Carte à éditer
  @return True si la carte a été modifiée, False sinon
*}
class function TFormEditMap.EditMap(AMap: TMap): Boolean;
begin
  with Create(nil) do
  try
    Result := DoEditMap(AMap);
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  MapImage.OnAfterPaintMap := MapImageAfterPaintMap;
  MapImage.OnSquareMouseDown := MapImageSquareMouseDown;
end;

{*
  Gestionnaire d'événement OnAfterPaintMap de l'image de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Bitmap   Bitmap de la carte
  @param Map      Carte à dessiner
  @param Floor    Étage à dessiner
*}
procedure TFormEditMap.MapImageAfterPaintMap(Sender: TObject; Bitmap: TBitmap32;
  Map: TMap; Floor: Integer);
var
  ZoneInt: Integer;
  Zone: TPoint;
  Dir: TDirection;
  LineRect: TRect;
begin
  for ZoneInt in FSelectedZones do
  begin
    Zone := IntToZone(ZoneInt);

    for Dir := diNorth to diWest do
    begin
      if not IsBorder(Zone, Dir) then
        Continue;

      LineRect := Rect(-3, -3, 3, 3);

      if Dir in [diNorth, diSouth] then
        Inc(LineRect.Right, Map.ZoneWidth * SquareSize)
      else
        Inc(LineRect.Bottom, Map.ZoneHeight * SquareSize);

      OffsetRect(LineRect, Zone.X * Map.ZoneWidth * SquareSize + SquareSize,
        Zone.Y * Map.ZoneHeight * SquareSize + SquareSize);

      case Dir of
        diEast:
          OffsetRect(LineRect, Map.ZoneWidth * SquareSize, 0);
        diSouth:
          OffsetRect(LineRect, 0, Map.ZoneHeight * SquareSize);
      end;

      Bitmap.FillRectS(LineRect, clYellow32);
    end;
  end;
end;

{*
  Gestionnaire d'événement OnSquareMouseDown de l'image de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param QPos     Position qualifiée de la case cliquée
  @param Button   Bouton de la souris qui a été enfoncé
  @param Shift    État des touches spéciales
*}
procedure TFormEditMap.MapImageSquareMouseDown(Sender: TObject;
  const QPos: TQualifiedPos; Button: TMouseButton; Shift: TShiftState);
var
  Zone: TPoint;
  ZoneRect: TRect;
begin
  if (Button <> mbLeft) or (not QPos.IsInside) then
    Exit;

  Zone.X := QPos.X div Map.ZoneWidth;
  Zone.Y := QPos.Y div Map.ZoneHeight;

  Shift := Shift * [ssShift, ssAlt, ssCtrl];

  if Shift = [] then
  begin
    ClearSelectedZones;
    SelectZone(Zone);
  end else if Shift = [ssCtrl] then
  begin
    if IsZoneSelected(Zone) then
      UnselectZone(Zone)
    else
      SelectZone(Zone);
  end else if (Shift = [ssShift]) and (not IsNoPoint(FLastSelectedZone)) then
  begin
    ZoneRect.Left := Min(Zone.X, FLastSelectedZone.X);
    ZoneRect.Top := Min(Zone.Y, FLastSelectedZone.Y);
    ZoneRect.Right := Max(Zone.X, FLastSelectedZone.X)+1;
    ZoneRect.Bottom := Max(Zone.Y, FLastSelectedZone.Y)+1;

    SelectZoneRect(ZoneRect);
  end;

  MapImage.InvalidateMap;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur étage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.EditFloorChange(Sender: TObject);
begin
  MapImage.Floor := EditFloor.Value;

  ComboBoxOutside.ItemIndex :=
    ComboBoxOutside.Items.IndexOfObject(Map.Outside[MapImage.Floor].Field);
end;

{*
  Gestionnaire d'événement OnClick des boutons de redimensionnement
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.ButtonResizeClick(Sender: TObject);
var
  Dir: TResizeDirection;
  IsShrink: Boolean;
begin
  // Extract IsShrink and Dir from Sender.Tag
  with TComponent(Sender) do
  begin
    IsShrink := Tag < 0;
    if IsShrink then
      Dir := TResizeDirection(-Tag)
    else
      Dir := TResizeDirection(Tag);
  end;

  ResizeMap(Dir, IsShrink);
end;

{*
  Gestionnaire d'événement OnChange des éditeurs de taille de zone
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.EditZoneSizeChange(Sender: TObject);
begin
  Map.ZoneWidth := EditZoneWidth.Value;
  Map.ZoneHeight := EditZoneHeight.Value;
  ClearSelectedZones;
  MapImage.InvalidateMap;
end;

{*
  Gestionnaire d'événement OnClick du bouton Remplir les zones sélectionnées
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.ButtonFillZoneClick(Sender: TObject);
var
  ZoneInt: Integer;
  Zone: TPoint;
  PosRect: TRect;
begin
  for ZoneInt in FSelectedZones do
  begin
    Zone := IntToZone(ZoneInt);

    PosRect.Left := Zone.X * Map.ZoneWidth;
    PosRect.Top := Zone.Y * Map.ZoneHeight;
    PosRect.Right := PosRect.Left + Map.ZoneWidth;
    PosRect.Bottom := PosRect.Top + Map.ZoneHeight;

    FillZoneField(PosRect,
      TField(ComboBoxZoneField.Items.Objects[ComboBoxZoneField.ItemIndex]));
    FillZoneGrid(PosRect,
      TField(ComboBoxZoneGrid.Items.Objects[ComboBoxZoneGrid.ItemIndex]));
    FillZoneBorder(PosRect,
      TField(ComboBoxZoneBorder.Items.Objects[ComboBoxZoneBorder.ItemIndex]),
      Zone);
  end;

  InvalidateMap;
end;

{*
  Gestionnaire d'événement OnChange de la combo-box Extérieur du labyrinthe
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.ComboBoxOutsideChange(Sender: TObject);
var
  Field: TField;
begin
  if ComboBoxOutside.ItemIndex < 0 then
    Exit;

  Field := TField(ComboBoxOutside.Items.Objects[ComboBoxOutside.ItemIndex]);

  if Map.Outside[MapImage.Floor].Field <> Field then
  begin
    Map.Outside[MapImage.Floor] := Master.SquareByComps(Field);
    InvalidateMap;
  end;
end;

{*
  Gestionnaire d'événement OnChange de la track-bar de zoom
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.TrackBarScaleChange(Sender: TObject);
begin
  MapImage.Scale := TrackBarScale.Position * 0.1;
end;

{*
  Gestionnaire d'événement OnValidate du validateur du format de l'ID de carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.MapIDFormatValidatorValidate(Sender: TObject;
  ValueToValidate: Variant; var Valid: Boolean);
begin
  Valid := (not MapIDRequiredValidator.Valid) or IsValidIdent(ValueToValidate);
end;

{*
  Gestionnaire d'événement OnValidate du validateur de la largeur de zone
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.ZoneWidthValidatorValidate(Sender: TObject;
  ValueToValidate: Variant; var Valid: Boolean);
begin
  Valid := Map.Dimensions.X mod Integer(ValueToValidate) = 0;
end;

{*
  Gestionnaire d'événement OnValidate du validateur de la hauteur de zone
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.ZoneHeightValidatorValidate(Sender: TObject;
  ValueToValidate: Variant; var Valid: Boolean);
begin
  Valid := Map.Dimensions.Y mod Integer(ValueToValidate) = 0;
end;

{*
  Gestionnaire d'événement OnClick du bouton OK
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormEditMap.ButtonOKClick(Sender: TObject);
begin
  if Validators.Validate then
    ModalResult := mrOK;
end;

end.

