unit FunLabyControls;

interface

uses
  Types, Windows, SysUtils, Classes, Controls, StdCtrls, FunLabyUtils,
  FunLabyEditPluginConsts;

type
  TFilterComponentEvent = procedure(Sender: TObject;
    Component: TFunLabyComponent; var Accept: Boolean) of object;

  {*
    Classe de base pour les combobox contenant des composants FunLabyrinthe
    @author sjrd
    @version 5.0.1
  *}
  TCustomFLComponentComboBox = class(TCustomComboBox)
  private
    FMaster: TMaster;     /// Maître FunLabyrinthe
    FItemsValid: Boolean; /// Indique si les éléments sont valides

    FUseNil: Boolean; /// Indique si la valeur nil est disponible dans la combo

    /// Classe de composant pour un filtre simple
    FComponentClass: TFunLabyComponentClass;

    /// Filtre les composants à accepter dans la combo
    FOnFilterComponent: TFilterComponentEvent;

    procedure EnsureItemsValid;
    procedure RegisterComponent(Component: TFunLabyComponent);

    procedure SetMaster(Value: TMaster);
    procedure SetUseNil(Value: Boolean);
    procedure SetComponentClass(Value: TFunLabyComponentClass);
    procedure SetOnFilterComponent(const Value: TFilterComponentEvent);

    function GetSelected: TFunLabyComponent;
    procedure SetSelected(Value: TFunLabyComponent);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;

    procedure DropDown; override;

    procedure SetItemIndex(const Value: Integer); override;

    procedure DoFilterComponent(Component: TFunLabyComponent;
      var Accept: Boolean); virtual;

    property ItemsValid: Boolean read FItemsValid;

    property UseNil: Boolean read FUseNil write SetUseNil;
    property ComponentClass: TFunLabyComponentClass
      read FComponentClass write SetComponentClass;

    property OnFilterComponent: TFilterComponentEvent
      read FOnFilterComponent write SetOnFilterComponent;
  public
    constructor Create(AOwner: TComponent); override;

    procedure InvalidateItems; virtual;

    property Master: TMaster read FMaster write SetMaster;

    property Selected: TFunLabyComponent read GetSelected write SetSelected;
  end;

  {*
    Combobox contenant des composants FunLabyrinthe
    @author sjrd
    @version 5.0.1
  *}
  TFLComponentComboBox = class(TCustomFLComponentComboBox)
  public
    property ComponentClass;
  published
    // Copied from TComboBox
    property Align;
    property AutoComplete default True;
    property AutoCompleteDelay default 500;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    // Deleted: property Style;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    // Deleted: property ItemHeight;
    // Deleted: property ItemIndex default -1;
    // Deleted: property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    // Deleted: property Text;
    property TextHint;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    // Deleted: property Items;

    // Published from TCustomFLComponentComboBox
    property UseNil default False;

    property OnFilterComponent;
  end;

implementation

{----------------------------------}
{ TCustomFLComponentComboBox class }
{----------------------------------}

{*
  [@inheritDoc]
*}
constructor TCustomFLComponentComboBox.Create(AOwner: TComponent);
begin
  inherited;

  Style := csOwnerDrawFixed;
  ItemHeight := SquareSize + 2;

  FComponentClass := TFunLabyComponent;
end;

{*
  S'assure que la propriété Items est correctement remplie
*}
procedure TCustomFLComponentComboBox.EnsureItemsValid;
begin
  if not ItemsValid then
  begin
    Items.BeginUpdate;
    try
      Items.Clear;

      if UseNil then
        Items.AddObject(SNone, nil);

      if Master <> nil then
        Master.RegisterComponents(RegisterComponent);
    finally
      Items.EndUpdate;
    end;

    FItemsValid := True;
  end;
end;

{*
  Enregistre un composant
  @param Component   Composant à enregistrer
*}
procedure TCustomFLComponentComboBox.RegisterComponent(
  Component: TFunLabyComponent);
var
  Accept: Boolean;
begin
  Accept := True;
  DoFilterComponent(Component, Accept);

  if Accept then
    Items.AddObject(Component.Hint, Component);
end;

{*
  Spécifie le maître FunLabyrinthe
  @param Value   Nouveau maître FunLabyrinthe
*}
procedure TCustomFLComponentComboBox.SetMaster(Value: TMaster);
begin
  if Value <> FMaster then
  begin
    FMaster := Value;
    InvalidateItems;
  end;
end;

{*
  Spécifie si nil est proposé comme composant
  @param Value   True si nil est proposé, False sinon
*}
procedure TCustomFLComponentComboBox.SetUseNil(Value: Boolean);
begin
  if Value <> FUseNil then
  begin
    FUseNil := Value;
    InvalidateItems;
  end;
end;

{*
  Spécifie la classe de composants de base
  @param Value   Nouvelle classe de composants de base
*}
procedure TCustomFLComponentComboBox.SetComponentClass(
  Value: TFunLabyComponentClass);
begin
  if Value <> FComponentClass then
  begin
    FComponentClass := Value;
    InvalidateItems;
  end;
end;

{*
  Spécifie le filtre de composants personnalisé
  @param Value   Nouveau filtre
*}
procedure TCustomFLComponentComboBox.SetOnFilterComponent(
  const Value: TFilterComponentEvent);
begin
  FOnFilterComponent := Value;
  InvalidateItems;
end;

{*
  Composant sélectionné
  @return Composant sélectionné
*}
function TCustomFLComponentComboBox.GetSelected: TFunLabyComponent;
begin
  if ItemIndex < 0 then
    Result := nil
  else
    Result := Items.Objects[ItemIndex] as TFunLabyComponent;
end;

{*
  Spécifie le composant sélectionné
  @param Value   Nouveau composant sélectionné
*}
procedure TCustomFLComponentComboBox.SetSelected(Value: TFunLabyComponent);
begin
  EnsureItemsValid;
  ItemIndex := Items.IndexOfObject(Value);
end;

{*
  [@inheritDoc]
*}
procedure TCustomFLComponentComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Component: TFunLabyComponent;
  IconRect: TRect;
begin
  TControlCanvas(Canvas).UpdateTextFlags;

  // Background
  Canvas.FillRect(Rect);

  // Item
  if Index >= 0 then
  begin
    with Rect do
      IconRect := Types.Rect(Left+2, Top, Left+2+SquareSize, Bottom);

    // Icon
    Component := Items.Objects[Index] as TFunLabyComponent;
    if Component <> nil then
      Component.DrawIconToCanvas(Canvas, IconRect, Canvas.Brush.Color);

    // Text
    Canvas.TextOut(IconRect.Right + 2, Rect.Top + 8, Items[Index]);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TCustomFLComponentComboBox.DropDown;
begin
  EnsureItemsValid;

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TCustomFLComponentComboBox.SetItemIndex(const Value: Integer);
begin
  EnsureItemsValid;
  inherited;
end;

{*
  Filtre un composant en spécifiant s'il doit apparaître dans la combo
  @param Component   Composant à tester
  @param Accept      Indique si le composant est accepté (défaut = True)
*}
procedure TCustomFLComponentComboBox.DoFilterComponent(
  Component: TFunLabyComponent; var Accept: Boolean);
begin
  if not (Component is ComponentClass) then
    Accept := False
  else if Assigned(FOnFilterComponent) then
    FOnFilterComponent(Self, Component, Accept);
end;

{*
  Invalide la liste des composants dans la combo
*}
procedure TCustomFLComponentComboBox.InvalidateItems;
begin
  FItemsValid := False;
end;

end.

