unit MapEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, StdCtrls, Tabs, CategoryButtons, Spin,
  ScUtils, SdDialogs, SepiMetaUnits, FunLabyUtils, FilesUtils,
  FunLabyEditConsts, PlayerObjects, PlayerPlugins, EditParameters, AddMap;

type
  {*
    Méthode de call-back indiquant un fichier comme modifié
  *}
  TMarkModifiedProc = procedure of object;

  {*
    Représente un ensemble de composants enregistré
    @author sjrd
    @version 5.0
  *}
  TComponentSet = class
  private
    FMinIndex: Integer;                    /// Index minimal
    FMaxIndex: Integer;                    /// Index maximal
    FComponents: array of TScrewComponent; /// Ensemble des composants
    FDialogTitle: string;                  /// Titre de la boîte de dialogue
    FDialogPrompt: string;                 /// Invite de la boîte de dialogue
  public
    constructor Create(const AComponents: array of TScrewComponent;
      BaseIndex: Integer; const ADialogTitle, ADialogPrompt: string);

    function ChooseComponent(var LastIndex: Integer): TScrewComponent;
  end;

  {*
    Cadre d'édition des cartes
    @author sjrd
    @version 5.0
  *}
  TFrameMapEditor = class(TFrame)
    SplitterScrews: TSplitter;
    SplitterPlayers: TSplitter;
    ScrewsContainer: TCategoryButtons;
    PlayersContainer: TCategoryButtons;
    PanelCenter: TPanel;
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
    ScrollBoxMap: TScrollBox;
    PaintBoxMap: TPaintBox;
    ScrewsImages: TImageList;
    EditFloor: TSpinEdit;
    procedure MapTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PaintBoxMapPaint(Sender: TObject);
    procedure EditFloorChange(Sender: TObject);
    procedure ScrewsContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure PlayersContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure PaintBoxMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMapMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Déclarations privées }
    SepiRoot: TSepiMetaRoot;     /// Racine Sepi

    MasterFile: TMasterFile;     /// Fichier maître
    Master: TMaster;             /// Maître FunLabyrinthe

    ScrewBmp: TScrewBitmap;      /// Bitmap de case à tout faire
    LastCompIndex: Integer;      /// Dernier index de composant choisi

    CurrentMap: TMap;            /// Carte courante
    FCurrentFloor: Integer;      /// Étage courant

    Component: TVisualComponent; /// Composant à placer

    /// Call-back marquant le fichier comme modifé
    FMarkModified: TMarkModifiedProc;

    procedure SetCurrentFloor(Value: Integer);

    function AddScrewButton(Template: TVisualComponent): TButtonItem;
    procedure RegisterSingleComponent(Component: TScrewComponent); stdcall;
    procedure RegisterComponentSet(Template: TScrewComponent;
      const Components: array of TScrewComponent; BaseIndex: Integer;
      const DialogTitle, DialogPrompt: string); stdcall;

    procedure LoadPlayers;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CenterToPlayerPosition(Player: TPlayer);

    procedure LoadFile(ASepiRoot: TSepiMetaRoot; AMasterFile: TMasterFile);
    procedure UnloadFile;

    procedure AddMap;
    procedure RemoveCurrentMap;

    property CurrentFloor: Integer read FCurrentFloor write SetCurrentFloor;

    property MarkModified: TMarkModifiedProc
      read FMarkModified write FMarkModified;
  end;

implementation

{$R *.dfm}

const
  opMask = $07;           /// Masque d'opération
  opShift = 3;            /// Décalage de l'information d'opération
  opCenterToPosition = 1; /// Opération centrer sur la position
  opShowPlugins = 2;      /// Opération montrer les plug-in
  opShowAttributes = 3;   /// Opération montrer les attributs
  opShowObjects = 4;      /// Opération montrer les objets

{----------------------}
{ Classe TComponentSet }
{----------------------}

{*
  Crée une instance de TComponentSet
  @param AComponents     Ensemble de composants
  @param ADialogTitle    Titre de la boîte de dialogue
  @param ADialogPrompt   Invite de la boîte de dialogue
*}
constructor TComponentSet.Create(const AComponents: array of TScrewComponent;
  BaseIndex: Integer; const ADialogTitle, ADialogPrompt: string);
var
  Len, I: Integer;
begin
  inherited Create;

  Len := Length(AComponents);
  FMinIndex := BaseIndex;
  FMaxIndex := BaseIndex+Len-1;

  SetLength(FComponents, Len);
  for I := 0 to Len-1 do
  begin
    FComponents[I] := AComponents[Low(AComponents) + I];
    if FComponents[I] is TScrew then
      TScrew(FComponents[I]).AddRef;
  end;

  FDialogTitle := ADialogTitle;
  FDialogPrompt := ADialogPrompt;
end;

{*
  Demande à l'utilisateur de choisir un composant dans l'ensemble de composants
  @param LastIndex   Dernier index entré, contient le nouvel index en sortie
  @return Référence au composant choisi
*}
function TComponentSet.ChooseComponent(
  var LastIndex: Integer): TScrewComponent;
begin
  LastIndex := QueryNumber(FDialogTitle, FDialogPrompt,
    MinMax(LastIndex, FMinIndex, FMaxIndex), FMinIndex, FMaxIndex);
  Result := FComponents[LastIndex - FMinIndex];
end;

{------------------------}
{ Classe TFrameMapEditor }
{------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameMapEditor.Create(AOwner: TComponent);
begin
  inherited;

  ScrewBmp := TScrewBitmap.Create;

  LastCompIndex := 0;
  CurrentMap := nil;

  Component := nil;
end;

{*
  [@inheritDoc]
*}
destructor TFrameMapEditor.Destroy;
begin
  ScrewBmp.Free;

  inherited;
end;

{*
  Modifie l'étage courant
  @param Value   Nouvel étage
*}
procedure TFrameMapEditor.SetCurrentFloor(Value: Integer);
begin
  EditFloor.Value := Value;
  PaintBoxMap.Invalidate;
end;

{*
  Ajoute un bouton de case à partir d'un modèle de case et le renvoie
  @param Template   Modèle de case, pour l'image et le hint du bouton
  @return Le bouton nouvellement créé
*}
function TFrameMapEditor.AddScrewButton(
  Template: TVisualComponent): TButtonItem;
var
  ImageIndex: Integer;
  Category: TButtonCategory;
begin
  // Ajout de l'image du composant dans la liste d'images
  ScrewBmp.EmptyScrew;
  Template.Draw(NoQPos, ScrewBmp.Canvas);
  ImageIndex := ScrewsImages.AddMasked(ScrewBmp, clTransparent);

  // Choix de la catégorie
  if Template is TField then
    Category := ScrewsContainer.Categories[0]
  else if Template is TEffect then
    Category := ScrewsContainer.Categories[1]
  else if Template is TTool then
    Category := ScrewsContainer.Categories[2]
  else if Template is TObstacle then
    Category := ScrewsContainer.Categories[3]
  else if Template is TScrew then
    Category := ScrewsContainer.Categories[4]
  else
    Category := ScrewsContainer.Categories[5];

  // Ajout du bouton
  Result := Category.Items.Add;
  Result.ImageIndex := ImageIndex;
  Result.Hint := Template.Name;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFrameMapEditor.RegisterSingleComponent(Component: TScrewComponent);
var
  Button: TButtonItem;
begin
  Button := AddScrewButton(Component);
  Button.Data := Component;
  if Component is TScrew then
    TScrew(Component).AddRef;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant modèle pour l'image et le nom à afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
  @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
*}
procedure TFrameMapEditor.RegisterComponentSet(Template: TScrewComponent;
  const Components: array of TScrewComponent; BaseIndex: Integer;
  const DialogTitle, DialogPrompt: string);
var
  Button: TButtonItem;
begin
  Button := AddScrewButton(Template);
  Button.Data := TComponentSet.Create(Components,
    BaseIndex, DialogTitle, DialogPrompt);
end;

{*
  Charge les infos sur les joueurs
*}
procedure TFrameMapEditor.LoadPlayers;
var
  I: Integer;
  Player: TPlayer;
begin
  for I := 0 to Master.PlayerCount-1 do
  begin
    Player := Master.Players[I];

    AddScrewButton(Player).Data := Player;

    with PlayersContainer.Categories.Add do
    begin
      Caption := Player.ID;
      Color := $E8BBA2;

      Items.Add.Caption := Player.Name;

      with Items.Add do
      begin
        Caption := Format(sPlayerPosition,
          [Point3DToString(Player.Position, ', ')]);
        Hint := sCenterToPosition;
        Data := Pointer(I shl opShift + opCenterToPosition);
      end;

      with Items.Add do
      begin
        Caption := sShowPlugins;
        Data := Pointer(I shl opShift + opShowPlugins);
      end;

      with Items.Add do
      begin
        Caption := sShowAttributes;
        Data := Pointer(I shl opShift + opShowAttributes);
      end;

      with Items.Add do
      begin
        Caption := sShowObjects;
        Data := Pointer(I shl opShift + opShowObjects);
      end;
    end;
  end;
end;

{*
  Centre l'affichage sur le joueur
  @param Player   Joueur  visionner
*}
procedure TFrameMapEditor.CenterToPlayerPosition(Player: TPlayer);
var
  TabIndex: Integer;
  X, Y: Integer;
begin
  if Player.Map = nil then
  begin
    ShowDialog(sCantCenterToPosition, sCantCenterToUnplacedPlayer, dtError);
    Exit;
  end;

  TabIndex := Master.MapCount-1;
  while TabIndex >= 0 do
    if Master.Maps[TabIndex] = Player.Map then
      Break
    else
      Dec(TabIndex);
  MapTabSet.TabIndex := TabIndex;

  X := Player.Position.X * ScrewSize + ScrewSize div 2;
  Y := Player.Position.Y * ScrewSize + ScrewSize div 2;
  CurrentFloor := Player.Position.Z;

  with ScrollBoxMap do
  begin
    HorzScrollBar.Position := X - ClientWidth  div 2;
    VertScrollBar.Position := Y - ClientHeight div 2;
  end;
end;

{*
  Charge un fichier
  @param ASepiRoot     Racine Sepi
  @param AMasterFile   Fichier maître
*}
procedure TFrameMapEditor.LoadFile(ASepiRoot: TSepiMetaRoot;
  AMasterFile: TMasterFile);
var
  I: Integer;
begin
  SepiRoot := ASepiRoot;
  MasterFile := AMasterFile;
  Master := MasterFile.Master;

  // Recensement des composants d'édition
  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);

  // Chargement des infos des joueurs
  LoadPlayers;

  // Recensement des cartes
  with MasterFile do
    for I := 0 to MapFileCount-1 do
      MapTabSet.Tabs.Add(MapFiles[I].MapID);
  CurrentFloor := 0;
  if MasterFile.MapFileCount > 0 then
    MapTabSet.TabIndex := 0;
end;

{*
  Décharge le fichier courant
*}
procedure TFrameMapEditor.UnloadFile;
var
  I: Integer;
begin
  // Vider les onglets de carte
  MapTabSet.TabIndex := -1;
  MapTabSet.Tabs.Clear;

  // Vider les onglets des joueurs
  PlayersContainer.Categories.Clear;

  // Vider les composants d'édition
  with ScrewsContainer do
  begin
    for I := 0 to Categories.Count-1 do
    begin
      with Categories[I] do
      begin
        while Items.Count > 0 do
        begin
          if TObject(Items[0].Data) is TComponentSet then
            TObject(Items[0].Data).Free;
          Items.Delete(0);
        end;
      end;
    end;
  end;
  ScrewsImages.Clear;

  // Autres variables
  Component := nil;
  Master := nil;
  MasterFile := nil;
  SepiRoot := nil;
end;

{*
  Demande l'ajout d'une carte
*}
procedure TFrameMapEditor.AddMap;
var
  MapID: TComponentID;
begin
  MapID := TFormAddMap.AddMap(MasterFile);
  if MapID <> '' then
  begin
    MapTabSet.TabIndex := MapTabSet.Tabs.Add(MapID);
    MarkModified;
  end;
end;

{*
  Demande la suppression de la carte courante
*}
procedure TFrameMapEditor.RemoveCurrentMap;
var
  Map: TMap;
  I, Index: Integer;
begin
  Map := CurrentMap;
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

  Index := MapTabSet.TabIndex;
  if (Index = 0) and (MapTabSet.Tabs.Count > 1) then
    MapTabSet.TabIndex := 1
  else
    MapTabSet.TabIndex := Index - 1;

  MapTabSet.Tabs.Delete(Index);
  Map.Free;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange du tab-set des cartes
  @param Sender        Objet qui a déclenché l'événement
  @param NewTab        Index de l'onglet nouvellement sélectionné
  @param AllowChange   Indique si le changement peut être effectué
*}
procedure TFrameMapEditor.MapTabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  StaticPosition.Caption := '';
  StaticField.Caption := '';
  StaticEffect.Caption := '';
  StaticTool.Caption := '';
  StaticObstacle.Caption := '';

  if NewTab < 0 then
  begin
    CurrentMap := nil;

    CurrentFloor := 0;
    EditFloor.Enabled := False;

    PaintBoxMap.Width := 100;
    PaintBoxMap.Height := 100;
  end else
  begin
    CurrentMap := MasterFile.MapFiles[NewTab].Map;

    EditFloor.MaxValue := CurrentMap.Dimensions.Z-1;
    EditFloor.Enabled := CurrentMap.Dimensions.Z > 1;
    if CurrentFloor >= CurrentMap.Dimensions.Z then
      CurrentFloor := CurrentMap.Dimensions.Z-1;

    PaintBoxMap.Width  := (CurrentMap.Dimensions.X + 2*MinViewSize) * ScrewSize;
    PaintBoxMap.Height := (CurrentMap.Dimensions.Y + 2*MinViewSize) * ScrewSize;
  end;
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnPaint de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMapEditor.PaintBoxMapPaint(Sender: TObject);
var
  Left, Top, Right, Bottom: Integer;
  LeftZone, TopZone, RightZone, BottomZone: Integer;
  I, X, Y: Integer;
  QPos: TQualifiedPos;
begin
  if CurrentMap = nil then
    Exit;

  // Calcul des coordonnées à afficher
  Left := ScrollBoxMap.HorzScrollBar.Position div ScrewSize - MinViewSize;
  Top  := ScrollBoxMap.VertScrollBar.Position div ScrewSize - MinViewSize;
  Right  := Left + ScrollBoxMap.ClientWidth  div ScrewSize + 1;
  Bottom := Top  + ScrollBoxMap.ClientHeight div ScrewSize + 1;

  LeftZone := Left div CurrentMap.ZoneWidth;
  TopZone  := Top  div CurrentMap.ZoneHeight;
  RightZone  := LeftZone + Right  div CurrentMap.ZoneWidth;
  BottomZone := TopZone  + Bottom div CurrentMap.ZoneHeight;

  // Dessin des cases
  QPos.Map := CurrentMap;
  for X := Left to Right do
  begin
    for Y := Top to Bottom do
    begin
      QPos.Position := Point3D(X, Y, CurrentFloor);
      CurrentMap[QPos.Position].Draw(QPos, PaintBoxMap.Canvas,
        (MinViewSize+X) * ScrewSize, (MinViewSize+Y) * ScrewSize);
    end;
  end;

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do
  begin
    with Master.Players[I] do
    begin
      if (Map = CurrentMap) and (Position.Z = CurrentFloor) then
      begin
        DrawInPlace(PaintBoxMap.Canvas, (MinViewSize+Position.X) * ScrewSize,
          (MinViewSize+Position.Y) * ScrewSize);
      end;
    end;
  end;

  // Dessin des lignes de séparation des zones
  with PaintBoxMap.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psDash;
    Pen.Width := 3;

    for X := LeftZone to RightZone do
    begin
      MoveTo((CurrentMap.ZoneWidth * X + 1) * ScrewSize, Top * ScrewSize);
      LineTo((CurrentMap.ZoneWidth * X + 1) * ScrewSize,
        (Bottom+2) * ScrewSize);
    end;

    for Y := TopZone to BottomZone do
    begin
      MoveTo(Left * ScrewSize, (CurrentMap.ZoneHeight * Y + 1) * ScrewSize);
      LineTo((Right+2) * ScrewSize,
        (CurrentMap.ZoneHeight * Y + 1) * ScrewSize);
    end;

    Pen.Width := 1;
  end;
end;

{*
  Gestionnaire d'événement OnChange de la zone d'édition de l'étage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMapEditor.EditFloorChange(Sender: TObject);
begin
  FCurrentFloor := EditFloor.Value;
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnButtonClicked des boutons de case
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Référence au bouton cliqué
*}
procedure TFrameMapEditor.ScrewsContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  if TObject(Button.Data) is TComponentSet then
    Component := TComponentSet(Button.Data).ChooseComponent(LastCompIndex)
  else
    Component := TScrewComponent(Button.Data);
end;

{*
  Gestionnaire d'événement OnButtonClicked des boutons de joueur
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Référence au bouton cliqué
*}
procedure TFrameMapEditor.PlayersContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
var
  Player: TPlayer;
begin
  Player := Master.Players[Integer(Button.Data) shr opShift];
  case Integer(Button.Data) and opMask of
    opCenterToPosition: CenterToPlayerPosition(Player);
    opShowPlugins:
    begin
      if TFormPlugins.ManagePlugins(Player) then
      begin
        MarkModified;
        PaintBoxMap.Invalidate;
      end;
    end;
    opShowAttributes:
    begin
      if TFormParameters.EditPlayerAttributes(Player) then
        MarkModified;
    end;
    opShowObjects: TFormObjects.ShowObjects(Player);
  end;
end;

{*
  Gestionnaire d'événement OnMouseDown de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Boutonde la souris qui a été enfoncé
  @param Shift    État des touches système
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
*}
procedure TFrameMapEditor.PaintBoxMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Position: T3DPoint;
  FieldID, EffectID, ToolID, NewID: TComponentID;
begin
  if (Button <> mbLeft) or (CurrentMap = nil) or (Component = nil) then
    Exit;

  Position := Point3D(X div ScrewSize - 1, Y div ScrewSize - 1, CurrentFloor);

  if Component is TPlayer then
  begin
    TPlayer(Component).ChangePosition(CurrentMap, Position);
  end else
  begin
    NewID := Component.ID;
    with CurrentMap[Position] do
    begin
      FieldID := Field.ID;
      if Effect = nil then
        EffectID := ''
      else
        EffectID := Effect.ID;
      if Tool = nil then
        ToolID := ''
      else
        ToolID := Tool.ID;

      if Component is TField then
        NewID := NewID+'---'
      else if Component is TEffect then
        NewID := FieldID+'-'+NewID+'--'
      else if Component is TTool then
        NewID := FieldID+'-'+EffectID+'-'+NewID+'-'
      else if Component is TObstacle then
        NewID := FieldID+'-'+EffectID+'-'+ToolID+'-'+NewID;
    end;

    if CurrentMap.InMap(Position) then
    begin
      CurrentMap[Position] := Master.Screw[NewID];
    end else
    begin
      if ShowDialog(sReplaceOutsideTitle, sReplaceOutside,
        dtConfirmation, dbOKCancel) <> drOK then
        Exit;
      CurrentMap.Outside[CurrentFloor] := Master.Screw[NewID];
    end;
  end;

  MarkModified;
  PaintBoxMap.Invalidate;
end;

{*
  Gestionnaire d'événement OnMouseMove de la paint-box de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param Shift    État des touches système
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
*}
procedure TFrameMapEditor.PaintBoxMapMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Position: T3DPoint;
begin
  if CurrentMap = nil then
    Exit;

  Position := Point3D(X div ScrewSize - 1, Y div ScrewSize - 1, CurrentFloor);
  StaticPosition.Caption := Point3DToString(Position, ', ');

  with CurrentMap[Position] do
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
end;

end.

