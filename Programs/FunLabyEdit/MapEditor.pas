unit MapEditor;

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ImgList, ExtCtrls, StdCtrls, Tabs, CategoryButtons, Spin,
  StrUtils, ScUtils, SdDialogs, SepiReflectionCore, FunLabyUtils, FilesUtils,
  FunLabyEditConsts, PlayerObjects, PlayerPlugins, EditParameters,
  BaseMapViewer, MapTools, GR32, ObjectInspector, FunLabyEditTypes, EditMap;

type
  {*
    Cadre d'édition des cartes
    @author sjrd
    @version 5.0
  *}
  TFrameMapEditor = class(TFrame)
    SplitterSquares: TSplitter;
    SplitterPlayers: TSplitter;
    SquaresContainer: TCategoryButtons;
    PanelCenter: TPanel;
    MapViewer: TFrameBaseMapViewer;
    PanelRight: TPanel;
    FrameInspector: TFrameInspector;
    procedure SquaresContainerDrawIcon(Sender: TObject;
      const Button: TButtonItem; Canvas: TCanvas; Rect: TRect;
      State: TButtonDrawState; var TextOffset: Integer);
    procedure SquaresContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure MapViewerClickSquare(Sender: TObject; const QPos: TQualifiedPos);
  private
    MasterFile: TMasterFile; /// Fichier maître
    Master: TMaster;         /// Maître FunLabyrinthe

    LastCompIndex: Integer;        /// Dernier index de composant choisi
    FComponent: TFunLabyComponent; /// Composant à placer

    /// Call-back marquant le fichier comme modifé
    FMarkModified: TMarkModifiedProc;

    procedure InspectorMarkModified;

    function AddComponentButton(Component: TFunLabyComponent): TButtonItem;
    procedure UpdateComponentButton(Button: TButtonItem); overload;
    procedure UpdateComponentButton(Component: TFunLabyComponent); overload;
    procedure RegisterComponent(Component: TFunLabyComponent);

    function CreateNewComponent(Creator: TComponentCreator): TFunLabyComponent;

    procedure ClearSquareComponent(const QPos: TQualifiedPos;
      ComponentIndex: Integer);
    procedure AddSquareComponent(const QPos: TQualifiedPos;
      ComponentIndex: Integer; Component: TSquareComponent);
    procedure EditMapSquare(const QPos: TQualifiedPos;
      Component: TSquareComponent);

    procedure SetComponent(Value: TFunLabyComponent);

    function GetCurrentMap: TMap;
    procedure SetCurrentMap(Value: TMap);

    property Component: TFunLabyComponent read FComponent write SetComponent;
  public
    constructor Create(AOwner: TComponent); override;

    procedure CenterToPlayerPosition(Player: TPlayer);

    procedure LoadFile(AMasterFile: TMasterFile);
    procedure UnloadFile;

    procedure InvalidateMap;

    procedure AddMap;
    procedure RemoveCurrentMap;

    property CurrentMap: TMap read GetCurrentMap write SetCurrentMap;

    property MarkModified: TMarkModifiedProc
      read FMarkModified write FMarkModified;
  end;

implementation

{$R *.dfm}

{------------------------}
{ Classe TFrameMapEditor }
{------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameMapEditor.Create(AOwner: TComponent);
begin
  inherited;

  LastCompIndex := 0;
  Component := nil;

  MapViewer.OnClickSquare := MapViewerClickSquare;
  FrameInspector.MarkModified := InspectorMarkModified;
end;

{*
  Gestionnaire d'événement MarkModified de l'inspector d'objets
*}
procedure TFrameMapEditor.InspectorMarkModified;
begin
  if FrameInspector.InspectObject is TFunLabyComponent then
    UpdateComponentButton(TFunLabyComponent(FrameInspector.InspectObject));

  InvalidateMap;
  MarkModified;
end;

{*
  Ajoute un bouton de sélection d'un composant
  @param Component   Composant sélectionné par ce bouton
  @return Le bouton nouvellement créé
*}
function TFrameMapEditor.AddComponentButton(
  Component: TFunLabyComponent): TButtonItem;
var
  CategoryName: string;
  I: Integer;
  Category: TButtonCategory;
begin
  // Choix de la catégorie
  CategoryName := Component.Category;
  Category := nil;
  for I := 0 to SquaresContainer.Categories.Count-1 do
  begin
    if AnsiSameText(SquaresContainer.Categories[I].Caption, CategoryName) then
    begin
      Category := SquaresContainer.Categories[I];
      Break;
    end;
  end;

  if Category = nil then
  begin
    Category := SquaresContainer.Categories.Add;
    Category.Assign(SquaresContainer.Categories[0]);
    Category.Caption := CategoryName;
  end;

  // Ajout du bouton
  Result := Category.Items.Add;
  Result.ImageIndex := 0;
  Result.Hint := Component.Hint;
  Result.Data := Component;
end;

{*
  Met à jour le bouton représentant un composant
  @param Button   Bouton à mettre à jour
*}
procedure TFrameMapEditor.UpdateComponentButton(Button: TButtonItem);
begin
  Button.Hint := TFunLabyComponent(Button.Data).Hint;
  SquaresContainer.UpdateButton(Button);
end;

{*
  Met à jour le bouton représentant un composant
  @param Component   Composant dont mettre à jour le bouton
*}
procedure TFrameMapEditor.UpdateComponentButton(Component: TFunLabyComponent);
var
  CatIndex, ButtonIndex: Integer;
  Category: TButtonCategory;
begin
  for CatIndex := 0 to SquaresContainer.Categories.Count-1 do
  begin
    Category := SquaresContainer.Categories[CatIndex];

    for ButtonIndex := 0 to Category.Items.Count-1 do
    begin
      if Category.Items[ButtonIndex].Data = Component then
      begin
        UpdateComponentButton(Category.Items[ButtonIndex]);
        Exit;
      end;
    end;
  end;
end;

{*
  Enregistre un composant
  @param Component   Composant à enregistrer
*}
procedure TFrameMapEditor.RegisterComponent(Component: TFunLabyComponent);
begin
  AddComponentButton(Component);
end;

{*
  Crée un nouveau composant avec un créateur de composants
  @param Creator   Créateur de composants
  @return Composant créé (peut être nil)
*}
function TFrameMapEditor.CreateNewComponent(
  Creator: TComponentCreator): TFunLabyComponent;
var
  I: Integer;
  NewID: string;
begin
  Result := Creator;

  NewID := Creator.ID;
  if AnsiEndsText('Creator', NewID) then
    SetLength(NewID, Length(NewID) - 7);

  I := 1;
  while Master.ComponentExists(NewID + IntToStr(I)) do
    Inc(I);

  NewID := NewID + IntToStr(I);

  if InputQuery(SCreateCompChooseIDTitle, SCreateCompChooseID, NewID) then
  begin
    if not IsValidIdent(NewID) then
    begin
      ShowDialog(SInvalidComponentIDTitle, SBadComponentID, dtError);
    end else if Master.ComponentExists(NewID) then
    begin
      ShowDialog(SInvalidComponentIDTitle, SComponentAlreadyExists,
        dtError);
    end else
    begin
      Result := Creator.CreateComponent(NewID);

      if Result.IsDesignable then
        RegisterComponent(Result);

      MarkModified;
    end;
  end;
end;

{*
  Supprime un composant d'une case
  @param QPos             Position qualifiée de la case à modifier
  @param ComponentIndex   Index du composant à supprimer
*}
procedure TFrameMapEditor.ClearSquareComponent(const QPos: TQualifiedPos;
  ComponentIndex: Integer);
var
  Msg: TEditMapSquareMessage;
  I: Integer;
begin
  Msg.MsgID := msgEditMapSquare;
  Msg.Component := QPos.Components[ComponentIndex];
  Msg.QPos := QPos;

  // Removing phase
  Msg.Phase := espRemoving;
  for I := 0 to QPos.ComponentCount-1 do
  begin
    if I = ComponentIndex then
      Continue;

    Msg.Flags := [];
    if Msg.QPos.Components[I] <> nil then
      Msg.QPos.Components[I].Dispatch(Msg);

    if esfCancel in Msg.Flags then
      Abort;
  end;

  if Msg.QPos.Components[ComponentIndex] <> nil then
  begin
    // Remove phase
    Msg.Phase := espRemove;
    Msg.Flags := [];
    Msg.QPos.Components[ComponentIndex].Dispatch(Msg);

    if esfCancel in Msg.Flags then
      Abort;

    // Effectively remove the component - don't do this for a field
    if ComponentIndex > 0 then
      Msg.QPos.Components[ComponentIndex] := nil;
    MarkModified;
  end;

  // Removed phase
  Msg.Phase := espRemoved;
  for I := 0 to QPos.ComponentCount-1 do
  begin
    if I = ComponentIndex then
      Continue;

    Msg.Flags := [];
    if Msg.QPos.Components[I] <> nil then
      Msg.QPos.Components[I].Dispatch(Msg);
  end;
end;

{*
  Ajoute un composant sur une case
  @param QPos             Position qualifiée de la case à modifier
  @param ComponentIndex   Index du composant à ajouter
  @param Component        Composant à placer sur cette case
*}
procedure TFrameMapEditor.AddSquareComponent(const QPos: TQualifiedPos;
  ComponentIndex: Integer; Component: TSquareComponent);
var
  Msg: TEditMapSquareMessage;
  I: Integer;
begin
  Msg.MsgID := msgEditMapSquare;
  Msg.Component := Component;
  Msg.QPos := QPos;

  // Adding phase
  Msg.Phase := espAdding;
  for I := 0 to QPos.ComponentCount-1 do
  begin
    if I = ComponentIndex then
      Continue;

    Msg.Flags := [];
    if Msg.QPos.Components[I] <> nil then
      Msg.QPos.Components[I].Dispatch(Msg);

    if esfCancel in Msg.Flags then
      Abort;
  end;

  // Add phase
  Msg.Phase := espAdd;
  Msg.Flags := [];
  Component.Dispatch(Msg);

  if esfCancel in Msg.Flags then
    Abort;

  // Effectively add the component - unless it's already been handled
  if not (esfHandled in Msg.Flags) then
  begin
    if Msg.QPos.IsInside then
      Msg.QPos.Components[ComponentIndex] := Component
    else
    begin
      if not (Component is TField) then
      begin
        ShowDialog(SOnlyFieldOutsideTitle, SOnlyFieldOutside, dtError);
        Abort;
      end;

      Msg.QPos.Map.Outside[Msg.QPos.Z] := Master.SquareByComps(
        Component.ID, '', '', '');
    end;
  end;

  MarkModified;

  // Added phase
  Msg.Phase := espAdded;
  for I := 0 to QPos.ComponentCount-1 do
  begin
    if I = ComponentIndex then
      Continue;

    Msg.Flags := [];
    if Msg.QPos.Components[I] <> nil then
      Msg.QPos.Components[I].Dispatch(Msg);
  end;
end;

{*
  Modifie une case de la carte
  @param QPos        Position qualifiée de la carte
  @param Component   Composant à placer
*}
procedure TFrameMapEditor.EditMapSquare(const QPos: TQualifiedPos;
  Component: TSquareComponent);
var
  Square: TSquare;
  ComponentIndex, I: Integer;
begin
  // Only fields outside of maps
  if QPos.IsOutside and not (Component is TField) then
  begin
    ShowDialog(SOnlyFieldOutsideTitle, SOnlyFieldOutside, dtError);
    Abort;
  end;

  // Find component index
  Square := QPos.Square;
  ComponentIndex := Square.ComponentCount-1;
  while (ComponentIndex >= 0) and
    (not (Component is Square.ComponentClasses[ComponentIndex])) do
    Dec(ComponentIndex);

  // Clear in-the-way components
  for I := Square.ComponentCount-1 downto ComponentIndex do
    if ComponentIndex >= 0 then
      ClearSquareComponent(QPos, I);

  // Add the new component
  AddSquareComponent(QPos, ComponentIndex, Component);
end;

{*
  Modifie le composant à placer
  @param Value   Nouveau composant à placer
*}
procedure TFrameMapEditor.SetComponent(Value: TFunLabyComponent);
begin
  FComponent := Value;

  if (Value <> nil) and (not (Value is TSquare)) then
    FrameInspector.InspectObject := Value;
end;

{*
  Carte courante
  @return Carte actuellement visible
*}
function TFrameMapEditor.GetCurrentMap: TMap;
begin
  Result := MapViewer.CurrentMap;
end;

{*
  Change de carte
  @param Value   Nouvelle carte à afficher
*}
procedure TFrameMapEditor.SetCurrentMap(Value: TMap);
begin
  MapViewer.CurrentMap := Value;
end;

{*
  Centre l'affichage sur le joueur
  @param Player   Joueur  visionner
*}
procedure TFrameMapEditor.CenterToPlayerPosition(Player: TPlayer);
begin
  if Player.Map = nil then
  begin
    ShowDialog(sCantCenterToPosition, sCantCenterToUnplacedPlayer, dtError);
    Exit;
  end;

  MapViewer.ShowPosition(Player.Map, Player.Position);
end;

{*
  Charge un fichier
  @param AMasterFile   Fichier maître
*}
procedure TFrameMapEditor.LoadFile(AMasterFile: TMasterFile);
var
  I: Integer;
begin
  MasterFile := AMasterFile;
  Master := MasterFile.Master;

  // Recensement des composants d'édition
  Master.RegisterComponents(RegisterComponent);

  // Chargement du fichier dans l'inspecteur
  FrameInspector.LoadFile(AMasterFile);

  // Recensement des cartes
  MapViewer.Master := Master;

  // Centrer sur le premier joueur placé
  for I := 0 to Master.PlayerCount-1 do
  begin
    if Master.Players[I].Map <> nil then
    begin
      CenterToPlayerPosition(Master.Players[I]);
      Break;
    end;
  end;
end;

{*
  Décharge le fichier courant
*}
procedure TFrameMapEditor.UnloadFile;
var
  I: Integer;
begin
  // Vider les onglets de carte
  MapViewer.Master := nil;

  // Vider l'inspecteur d'objets
  FrameInspector.UnloadFile;

  // Vider les composants d'édition
  with SquaresContainer do
  begin
    for I := Categories.Count-1 downto 7 do
      Categories.Delete(I);

    for I := Categories.Count-1 downto 0 do
      Categories[I].Items.Clear;
  end;

  // Autres variables
  Component := nil;
  Master := nil;
  MasterFile := nil;
end;

{*
  Invalide la carte affichée
*}
procedure TFrameMapEditor.InvalidateMap;
begin
  MapViewer.InvalidateMap;
end;

{*
  Demande l'ajout d'une carte
*}
procedure TFrameMapEditor.AddMap;
var
  NewMap: TMap;
begin
  NewMap := TFormEditMap.NewMap(Master);

  if NewMap <> nil then
  begin
    MapViewer.UpdateMaps;
    MapViewer.CurrentMap := NewMap;

    MarkModified;
  end;
end;

{*
  Demande la suppression de la carte courante
*}
procedure TFrameMapEditor.RemoveCurrentMap;
var
  Map: TMap;
  I: Integer;
begin
  Map := MapViewer.CurrentMap;
  if Map = nil then
    Exit;

  if ShowDialog(sRemoveMapTitle, sRemoveMap,
    dtConfirmation, dbOKCancel, 2) <> drOK then
    Exit;

  for I := 0 to Master.PlayerCount-1 do
  begin
    if Master.Players[I].Map = Map then
      Master.Players[I].ChangePosition(nil, No3DPoint);
  end;

  MapViewer.Master := nil;
  try
    Map.Free;
  finally
    MapViewer.Master := Master;
  end;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnDrawIcon des boutons de case
  @param Sender       Objet qui a déclenché l'événement
  @param Button       Référence au bouton dont dessiner l'icône
  @param Canvas       Canevas cible
  @param Rect         Rectangle dans lequel dessiner l'icône
  @param State        État du bouton
  @param TextOffset   Offset du texte
*}
procedure TFrameMapEditor.SquaresContainerDrawIcon(Sender: TObject;
  const Button: TButtonItem; Canvas: TCanvas; Rect: TRect;
  State: TButtonDrawState; var TextOffset: Integer);
var
  BackgroundColor: TColor;
  Component: TFunLabyComponent;
  RectCenter: TPoint;
begin
  if bdsSelected in State then
    BackgroundColor := SquaresContainer.SelectedButtonColor
  else if bdsHot in State then
    BackgroundColor := SquaresContainer.HotButtonColor
  else
    BackgroundColor := SquaresContainer.RegularButtonColor;

  Component := TFunLabyComponent(Button.Data);

  with Rect do
    RectCenter := Point((Left+Right) div 2, (Top+Bottom) div 2);

  with RectCenter do
  begin
    if bdsDown in State then
    begin
      Inc(X);
      Inc(Y);
    end;

    Rect := Types.Rect(X - HalfSquareSize, Y - HalfSquareSize,
      X + HalfSquareSize, Y + HalfSquareSize);
  end;

  Component.DrawIconToCanvas(Canvas, Rect, BackgroundColor);
end;

{*
  Gestionnaire d'événement OnButtonClicked des boutons de case
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Référence au bouton cliqué
*}
procedure TFrameMapEditor.SquaresContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  Component := TFunLabyComponent(Button.Data);

  if Component is TComponentCreator then
    Component := CreateNewComponent(TComponentCreator(Component));
end;

{*
  Gestionnaire d'événement OnClickSquare de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param QPos     Position qualifiée de la case cliquée
*}
procedure TFrameMapEditor.MapViewerClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
begin
  if (Component = nil) or IsNoQPos(QPos) then
    Exit;

  try
    if Component is TPlayer then
    begin
      TPlayer(Component).ChangePosition(QPos.Map, QPos.Position);
      MarkModified;
    end else if Component is TSquareComponent then
    begin
      EditMapSquare(QPos, TSquareComponent(Component));
    end;
  except
    on EAbort do;
  end;

  InvalidateMap;
end;

end.

