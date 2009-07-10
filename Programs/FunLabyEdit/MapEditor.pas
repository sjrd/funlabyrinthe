unit MapEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, StdCtrls, Tabs, CategoryButtons, Spin,
  ScUtils, SdDialogs, SepiReflectionCore, FunLabyUtils, FilesUtils,
  FunLabyEditConsts, PlayerObjects, PlayerPlugins, EditParameters, AddMap,
  BaseMapViewer, MapTools;

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
    FComponents: array of TSquareComponent; /// Ensemble des composants
    FDialogTitle: string;                  /// Titre de la boîte de dialogue
    FDialogPrompt: string;                 /// Invite de la boîte de dialogue
  public
    constructor Create(const AComponents: array of TSquareComponent;
      BaseIndex: Integer; const ADialogTitle, ADialogPrompt: string);

    function ChooseComponent(var LastIndex: Integer): TSquareComponent;
  end;

  {*
    Cadre d'édition des cartes
    @author sjrd
    @version 5.0
  *}
  TFrameMapEditor = class(TFrame)
    SplitterSquares: TSplitter;
    SplitterPlayers: TSplitter;
    SquaresContainer: TCategoryButtons;
    PlayersContainer: TCategoryButtons;
    PanelCenter: TPanel;
    SquaresImages: TImageList;
    MapViewer: TFrameBaseMapViewer;
    procedure SquaresContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure PlayersContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure MapViewerClickSquare(Sender: TObject; const QPos: TQualifiedPos);
  private
    MasterFile: TMasterFile; /// Fichier maître
    Master: TMaster;         /// Maître FunLabyrinthe

    LastCompIndex: Integer;      /// Dernier index de composant choisi
    Component: TVisualComponent; /// Composant à placer

    /// Call-back marquant le fichier comme modifé
    FMarkModified: TMarkModifiedProc;

    function AddSquareButton(Template: TVisualComponent): TButtonItem;
    procedure RegisterSingleComponent(Component: TSquareComponent);
    procedure RegisterComponentSet(Template: TSquareComponent;
      const Components: array of TSquareComponent; BaseIndex: Integer;
      const DialogTitle, DialogPrompt: string);

    procedure LoadPlayers;

    function GetCurrentMap: TMap;
    procedure SetCurrentMap(Value: TMap);
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
constructor TComponentSet.Create(const AComponents: array of TSquareComponent;
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
    FComponents[I] := AComponents[Low(AComponents) + I];

  FDialogTitle := ADialogTitle;
  FDialogPrompt := ADialogPrompt;
end;

{*
  Demande à l'utilisateur de choisir un composant dans l'ensemble de composants
  @param LastIndex   Dernier index entré, contient le nouvel index en sortie
  @return Référence au composant choisi
*}
function TComponentSet.ChooseComponent(
  var LastIndex: Integer): TSquareComponent;
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

  LastCompIndex := 0;
  Component := nil;

  MapViewer.OnClickSquare := MapViewerClickSquare;
end;

{*
  Ajoute un bouton de case à partir d'un modèle de case et le renvoie
  @param Template   Modèle de case, pour l'image et le hint du bouton
  @return Le bouton nouvellement créé
*}
function TFrameMapEditor.AddSquareButton(
  Template: TVisualComponent): TButtonItem;
var
  SquareBmp: TSquareBitmap;
  ImageIndex: Integer;
  Category: TButtonCategory;
begin
  // Ajout de l'image du composant dans la liste d'images
  SquareBmp := TSquareBitmap.Create;
  try
    Template.Draw(NoQPos, SquareBmp.Canvas);
    ImageIndex := SquaresImages.AddMasked(SquareBmp, clTransparent);
  finally
    SquareBmp.Free;
  end;

  // Choix de la catégorie
  if Template is TField then
    Category := SquaresContainer.Categories[0]
  else if Template is TEffect then
    Category := SquaresContainer.Categories[1]
  else if Template is TTool then
    Category := SquaresContainer.Categories[2]
  else if Template is TObstacle then
    Category := SquaresContainer.Categories[3]
  else if Template is TSquare then
    Category := SquaresContainer.Categories[4]
  else
    Category := SquaresContainer.Categories[5];

  // Ajout du bouton
  Result := Category.Items.Add;
  Result.ImageIndex := ImageIndex;
  Result.Hint := Template.Name;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFrameMapEditor.RegisterSingleComponent(Component: TSquareComponent);
var
  Button: TButtonItem;
begin
  Button := AddSquareButton(Component);
  Button.Data := Component;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant modèle pour l'image et le nom à afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
  @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
*}
procedure TFrameMapEditor.RegisterComponentSet(Template: TSquareComponent;
  const Components: array of TSquareComponent; BaseIndex: Integer;
  const DialogTitle, DialogPrompt: string);
var
  Button: TButtonItem;
begin
  Button := AddSquareButton(Template);
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

    AddSquareButton(Player).Data := Player;

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
  @param ASepiRoot     Racine Sepi
  @param AMasterFile   Fichier maître
*}
procedure TFrameMapEditor.LoadFile(AMasterFile: TMasterFile);
begin
  MasterFile := AMasterFile;
  Master := MasterFile.Master;

  // Recensement des composants d'édition
  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);

  // Chargement des infos des joueurs
  LoadPlayers;

  // Recensement des cartes
  MapViewer.Master := Master;
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

  // Vider les onglets des joueurs
  PlayersContainer.Categories.Clear;

  // Vider les composants d'édition
  with SquaresContainer do
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
  SquaresImages.Clear;

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
  MapID: TComponentID;
begin
  MapID := TFormAddMap.AddMap(MasterFile);

  if MapID <> '' then
  begin
    MapViewer.UpdateMaps;
    MapViewer.CurrentMap := Master.Map[MapID];

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
  Gestionnaire d'événement OnButtonClicked des boutons de case
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Référence au bouton cliqué
*}
procedure TFrameMapEditor.SquaresContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  if TObject(Button.Data) is TComponentSet then
    Component := TComponentSet(Button.Data).ChooseComponent(LastCompIndex)
  else
    Component := TSquareComponent(Button.Data);
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
        MapViewer.InvalidateMap;
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
  Gestionnaire d'événement OnClickSquare de la carte
  @param Sender   Objet qui a déclenché l'événement
  @param QPos     Position qualifiée de la case cliquée
*}
procedure TFrameMapEditor.MapViewerClickSquare(Sender: TObject;
  const QPos: TQualifiedPos);
type
  TRemoveProc = function(Square: TSquare): TSquare;
var
  Modified: Boolean;
  Map: TMap;
  Position: T3DPoint;
  BaseFlags: TEditMapSquareFlags;
  EditMapSquareMsg: TEditMapSquareMessage;

  function GetSquare: TSquare;
  begin
    Result := Map[Position];
  end;

  procedure SetSquare(Value: TSquare);
  begin
    if esfOutside in BaseFlags then
      Map.Outside[Position.Z] := Value
    else
      Map[Position] := Value;
  end;

  procedure BuildMessage(AdditionalFlags: TEditMapSquareFlags);
  begin
    EditMapSquareMsg.MsgID := msgEditMapSquare;
    EditMapSquareMsg.Flags := BaseFlags + AdditionalFlags;
    EditMapSquareMsg.QPos := QPos;
  end;

  function ClearComponent(Component: TSquareComponent;
    RemoveProc: TRemoveProc): Boolean;
  begin
    if Component = nil then
      Result := True
    else
    begin
      BuildMessage([esfRemoving]);
      Component.Dispatch(EditMapSquareMsg);

      Result := not (esfCancel in EditMapSquareMsg.Flags);

      if Result then
      begin
        if (not (esfHandled in EditMapSquareMsg.Flags)) and
          Assigned(RemoveProc) then
          SetSquare(RemoveProc(GetSquare));
        Modified := True;
      end;
    end;
  end;

begin
  Modified := False;
  try
    Map := QPos.Map;
    Position := QPos.Position;

    if Component is TPlayer then
    begin
      TPlayer(Component).ChangePosition(Map, Position);
      Modified := True;
    end else
    begin
      if Map.InMap(Position) then
        BaseFlags := []
      else
      begin
        BaseFlags := [esfOutside];
        if ShowDialog(sReplaceOutsideTitle, sReplaceOutside,
          dtConfirmation, dbOKCancel) <> drOK then
          Exit;
      end;

      // Clear in-the-way components
      if not ClearComponent(GetSquare.Obstacle, RemoveObstacle) then
        Exit;
      if not (Component is TObstacle) then
      begin
        if not ClearComponent(GetSquare.Tool, RemoveTool) then
          Exit;
        if not (Component is TTool) then
        begin
          if not ClearComponent(GetSquare.Effect, RemoveEffect) then
            Exit;
          if not (Component is TEffect) then
          begin
            if not ClearComponent(GetSquare.Field, nil) then
              Exit;
          end;
        end;
      end;

      // Add the new component
      BuildMessage([esfAdding]);
      Component.Dispatch(EditMapSquareMsg);

      if esfCancel in EditMapSquareMsg.Flags then
        Exit;

      if not (esfHandled in EditMapSquareMsg.Flags) then
        SetSquare(ChangeComp(GetSquare, Component as TSquareComponent));
      Modified := True;
    end;
  finally
    if Modified then
    begin
      MarkModified;
      MapViewer.InvalidateMap;
    end;
  end;
end;

end.

