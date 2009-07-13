unit MapEditor;

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ImgList, ExtCtrls, StdCtrls, Tabs, CategoryButtons, Spin,
  ScUtils, SdDialogs, SepiReflectionCore, FunLabyUtils, FilesUtils,
  FunLabyEditConsts, PlayerObjects, PlayerPlugins, EditParameters, AddMap,
  BaseMapViewer, MapTools, GR32;

type
  {*
    M�thode de call-back indiquant un fichier comme modifi�
  *}
  TMarkModifiedProc = procedure of object;

  {*
    Repr�sente un ensemble de composants enregistr�
    @author sjrd
    @version 5.0
  *}
  TComponentSet = class
  private
    FTemplate: TVisualComponent;            /// Template
    FMinIndex: Integer;                     /// Index minimal
    FMaxIndex: Integer;                     /// Index maximal
    FComponents: array of TSquareComponent; /// Ensemble des composants
    FDialogTitle: string;                   /// Titre de la bo�te de dialogue
    FDialogPrompt: string;                  /// Invite de la bo�te de dialogue
  public
    constructor Create(ATemplate: TVisualComponent;
      const AComponents: array of TSquareComponent; BaseIndex: Integer;
      const ADialogTitle, ADialogPrompt: string);

    function ChooseComponent(var LastIndex: Integer): TSquareComponent;

    property Template: TVisualComponent read FTemplate;
  end;

  {*
    Cadre d'�dition des cartes
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
    procedure SquaresContainerDrawIcon(Sender: TObject;
      const Button: TButtonItem; Canvas: TCanvas; Rect: TRect;
      State: TButtonDrawState; var TextOffset: Integer);
    procedure SquaresContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure PlayersContainerButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure MapViewerClickSquare(Sender: TObject; const QPos: TQualifiedPos);
  private
    MasterFile: TMasterFile; /// Fichier ma�tre
    Master: TMaster;         /// Ma�tre FunLabyrinthe

    LastCompIndex: Integer;      /// Dernier index de composant choisi
    Component: TVisualComponent; /// Composant � placer

    /// Call-back marquant le fichier comme modif�
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
  opMask = $07;           /// Masque d'op�ration
  opShift = 3;            /// D�calage de l'information d'op�ration
  opCenterToPosition = 1; /// Op�ration centrer sur la position
  opShowPlugins = 2;      /// Op�ration montrer les plug-in
  opShowAttributes = 3;   /// Op�ration montrer les attributs
  opShowObjects = 4;      /// Op�ration montrer les objets

{----------------------}
{ Classe TComponentSet }
{----------------------}

{*
  Cr�e une instance de TComponentSet
  @param ATemplate       Template
  @param AComponents     Ensemble de composants
  @param ADialogTitle    Titre de la bo�te de dialogue
  @param ADialogPrompt   Invite de la bo�te de dialogue
*}
constructor TComponentSet.Create(ATemplate: TVisualComponent;
  const AComponents: array of TSquareComponent; BaseIndex: Integer;
  const ADialogTitle, ADialogPrompt: string);
var
  Len, I: Integer;
begin
  inherited Create;

  FTemplate := ATemplate;

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
  Demande � l'utilisateur de choisir un composant dans l'ensemble de composants
  @param LastIndex   Dernier index entr�, contient le nouvel index en sortie
  @return R�f�rence au composant choisi
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
  Ajoute un bouton de case � partir d'un mod�le de case et le renvoie
  @param Template   Mod�le de case, pour l'image et le hint du bouton
  @return Le bouton nouvellement cr��
*}
function TFrameMapEditor.AddSquareButton(
  Template: TVisualComponent): TButtonItem;
var
  Category: TButtonCategory;
begin
  // Choix de la cat�gorie
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
  Result.ImageIndex := 0;
  Result.Hint := Template.Name;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant � enregistrer
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
  @param Template       Composant mod�le pour l'image et le nom � afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la bo�te de dialogue du choix du num�ro
  @param DialogPrompt   Invite de la bo�te de dialogue du choix du num�ro
*}
procedure TFrameMapEditor.RegisterComponentSet(Template: TSquareComponent;
  const Components: array of TSquareComponent; BaseIndex: Integer;
  const DialogTitle, DialogPrompt: string);
var
  Button: TButtonItem;
begin
  Button := AddSquareButton(Template);
  Button.Data := TComponentSet.Create(Template, Components,
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
  @param Value   Nouvelle carte � afficher
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
  @param AMasterFile   Fichier ma�tre
*}
procedure TFrameMapEditor.LoadFile(AMasterFile: TMasterFile);
begin
  MasterFile := AMasterFile;
  Master := MasterFile.Master;

  // Recensement des composants d'�dition
  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);

  // Chargement des infos des joueurs
  LoadPlayers;

  // Recensement des cartes
  MapViewer.Master := Master;
end;

{*
  D�charge le fichier courant
*}
procedure TFrameMapEditor.UnloadFile;
var
  I: Integer;
begin
  // Vider les onglets de carte
  MapViewer.Master := nil;

  // Vider les onglets des joueurs
  PlayersContainer.Categories.Clear;

  // Vider les composants d'�dition
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
  Invalide la carte affich�e
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
  Gestionnaire d'�v�nement OnDrawIcon des boutons de case
  @param Sender       Objet qui a d�clench� l'�v�nement
  @param Button       R�f�rence au bouton dont dessiner l'ic�ne
  @param Canvas       Canevas cible
  @param Rect         Rectangle dans lequel dessiner l'ic�ne
  @param State        �tat du bouton
  @param TextOffset   Offset du texte
*}
procedure TFrameMapEditor.SquaresContainerDrawIcon(Sender: TObject;
  const Button: TButtonItem; Canvas: TCanvas; Rect: TRect;
  State: TButtonDrawState; var TextOffset: Integer);
var
  BackgroundColor: TColor;
  Component: TVisualComponent;
  RectCenter: TPoint;
begin
  if bdsSelected in State then
    BackgroundColor := SquaresContainer.SelectedButtonColor
  else if bdsHot in State then
    BackgroundColor := SquaresContainer.HotButtonColor
  else
    BackgroundColor := SquaresContainer.RegularButtonColor;

  if TObject(Button.Data) is TComponentSet then
    Component := TComponentSet(Button.Data).Template
  else
    Component := TVisualComponent(Button.Data);

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

  Component.DrawToCanvas(Canvas, Rect, BackgroundColor);
end;

{*
  Gestionnaire d'�v�nement OnButtonClicked des boutons de case
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   R�f�rence au bouton cliqu�
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
  Gestionnaire d'�v�nement OnButtonClicked des boutons de joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   R�f�rence au bouton cliqu�
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
  Gestionnaire d'�v�nement OnClickSquare de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param QPos     Position qualifi�e de la case cliqu�e
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
  if Component = nil then
    Exit;

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

        if not (Component is TField) then
        begin
          ShowDialog(sOnlyFieldOutsideTitle, sOnlyFieldOutside, dtError);
          Exit;
        end;

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

