unit FunLabyEditMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, Menus, ImgList, StdCtrls,
  ExtCtrls, Tabs, ComCtrls, ActnMenus, ToolWin, ActnCtrls, CategoryButtons,
  StdActns, ScUtils, FunLabyUtils, FilesUtils, Spin, NumberDialog;

resourcestring
  sFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore implémentée';

  sPlayerPosition = 'Position : %s';
  sCenterToPosition = 'Centrer la vue sur ce joueur';
  sShowPlugins = 'Plug-in...';
  sShowAttributes = 'Attributs...';
  sShowObjects = 'Objets...';

  sConfirmExitTitle = 'Quitter FunLabyEdit';
  sConfirmExit = 'Le fichier a été modifé. Voulez-vous l''enregistrer ?';

  sReplaceOutsideTitle = 'Remplacer l''extérieur de la carte';
  sReplaceOutside = 'Voulez-vous vraiment modifier l''extérieur de la carte ?';

const
  opMask = $07;
  opShift = 3;
  opCenterToPosition = 1;
  opShowPlugins = 2;
  opShowAttributes = 3;
  opShowObjects = 4;

type
  TComponentSet = class
  private
    FMinIndex : integer;                    /// Index minimal
    FMaxIndex : integer;                    /// Index maximal
    FComponents : array of TScrewComponent; /// Ensemble des composants
    FDialogTitle : string;                  /// Titre de la boîte de dialogue
    FDialogPrompt : string;                 /// Invite de la boîte de dialogue
  public
    constructor Create(AComponents : array of TScrewComponent;
      const ADialogTitle, ADialogPrompt : string);

    function ChooseComponent(var LastIndex : integer) : TScrewComponent;
  end;

  TFormMain = class(TForm)
    Images: TImageList;
    ActionManager: TActionManager;
    ActionExit: TAction;
    ToolBarFile: TActionToolBar;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    PanelCenter: TPanel;
    MapTabSet: TTabSet;
    SplitterScrews: TSplitter;
    ScrewsContainer: TCategoryButtons;
    ScrewsImages: TImageList;
    PlayersContainer: TCategoryButtons;
    SplitterPlayers: TSplitter;
    ActionOpenFile: TAction;
    ActionSaveFile: TAction;
    ActionSaveFileAs: TAction;
    ActionCloseFile: TAction;
    ActionNewFile: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PanelMapInfos: TPanel;
    LabelPosition: TLabel;
    LabelField: TLabel;
    LabelEffect: TLabel;
    LabelObstacle: TLabel;
    ScrollBoxMap: TScrollBox;
    PaintBoxMap: TPaintBox;
    LabelFloor: TLabel;
    EditFloor: TSpinEdit;
    StaticPosition: TStaticText;
    StaticField: TStaticText;
    StaticEffect: TStaticText;
    StaticObstacle: TStaticText;
    ActionFileProperties: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionFilePropertiesExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
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
    ScrewBmp : TBitmap;          /// Bitmap à tout faire de la taille d'une case
    LastCompIndex : integer;     /// Dernier index de composant choisi

    Modified : boolean;          /// Indique si le fichier a été modifié
    MasterFile : TMasterFile;    /// Fichier maître
    Master : TMaster;            /// Maître FunLabyrinthe
    CurrentMap : TMap;           /// Carte courante

    FCurrentFloor : integer;     /// Étage courant

    Component : TScrewComponent; /// Composant à placer

    procedure SetCurrentFloor(Value : integer);

    function AddScrewButton(Template : TScrewComponent) : TButtonItem;
    procedure RegisterSingleComponent(Component : TScrewComponent); stdcall;
    procedure RegisterComponentSet(Template : TScrewComponent;
      Components : array of TScrewComponent;
      const DialogTitle, DialogPrompt : string); stdcall;

    procedure LoadPlayers;

    procedure LoadFile;
    procedure UnloadFile;

    procedure OpenFile(FileName : TFileName);
    function SaveFile(FileName : TFileName = '') : boolean;
    function CloseFile : boolean;

    procedure CenterToPlayerPosition(Player : TPlayer);

    property CurrentFloor : integer read FCurrentFloor write SetCurrentFloor;
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

////////////////////////////
/// Classe TComponentSet ///
////////////////////////////

{*
  Crée une instance de TComponentSet
  @param AComponents     Ensemble de composants
  @param ADialogTitle    Titre de la boîte de dialogue
  @param ADialogPrompt   Invite de la boîte de dialogue
*}
constructor TComponentSet.Create(AComponents : array of TScrewComponent;
  const ADialogTitle, ADialogPrompt : string);
var Len, I : integer;
begin
  inherited Create;

  FMinIndex := Low(AComponents);
  FMaxIndex := High(AComponents);

  Len := Length(AComponents);
  SetLength(FComponents, Len);
  for I := 0 to Len-1 do
    FComponents[I] := AComponents[Low(AComponents) + I];

  FDialogTitle := ADialogTitle;
  FDialogPrompt := ADialogPrompt;
end;

function TComponentSet.ChooseComponent(
  var LastIndex : integer) : TScrewComponent;
begin
  LastIndex := TFormNumber.ChooseNumber(FDialogTitle, FDialogPrompt,
    MinMax(LastIndex, FMinIndex, FMaxIndex), FMinIndex, FMaxIndex);
  Result := FComponents[LastIndex - FMinIndex];
end;

////////////////////////
/// Classe TFormMain ///
////////////////////////

{*
  Modifie l'étage courant
  @param Value   Nouvel étage
*}
procedure TFormMain.SetCurrentFloor(Value : integer);
begin
  EditFloor.Value := Value;
  PaintBoxMap.Invalidate;
end;

{*
  Ajoute un bouton de case à partir d'un modèle de case et le renvoie
  @param Template   Modèle de case, pour l'image et le hint du bouton
  @return Le bouton nouvellement créé
*}
function TFormMain.AddScrewButton(Template : TScrewComponent) : TButtonItem;
var ImageIndex : integer;
    Category : TButtonCategory;
begin
  // Ajout de l'image du composant dans la liste d'images
  EmptyScrewRect(ScrewBmp.Canvas);
  Template.Draw(ScrewBmp.Canvas);
  ImageIndex := ScrewsImages.AddMasked(ScrewBmp, clTransparent);

  // Choix de la catégorie
  if Template is TField    then Category := ScrewsContainer.Categories[0] else
  if Template is TEffect   then Category := ScrewsContainer.Categories[1] else
  if Template is TObstacle then Category := ScrewsContainer.Categories[2] else
  Category := ScrewsContainer.Categories[3];

  // Ajout du bouton
  Result := Category.Items.Add;
  Result.ImageIndex := ImageIndex;
  Result.Hint := Template.Name;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFormMain.RegisterSingleComponent(Component : TScrewComponent);
var Button : TButtonItem;
begin
  Button := AddScrewButton(Component);
  Button.Data := Component;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant modèle pour l'image et le nom à afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
  @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
*}
procedure TFormMain.RegisterComponentSet(Template : TScrewComponent;
  Components : array of TScrewComponent;
  const DialogTitle, DialogPrompt : string);
var Button : TButtonItem;
begin
  Button := AddScrewButton(Template);
  Button.Data := TComponentSet.Create(Components, DialogTitle, DialogPrompt);
end;

{*
  Charge les infos sur les joueurs
*}
procedure TFormMain.LoadPlayers;
var I : integer;
    Player : TPlayer;
begin
  for I := 0 to Master.PlayerCount-1 do
  begin
    Player := Master.Players[I];

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
  Charge le MasterFile créé
  Charge le MasterFile créé dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
var I : integer;
begin
  // Un fichier nouvellement chargé n'est modifié que s'il vient d'être créé
  Modified := MasterFile.FileName = '';

  // Autres variables
  Master := MasterFile.Master;

  // Activation de l'interface utilisateur
  ActionSaveFile.Enabled := True;
  ActionSaveFileAs.Enabled := True;
  ActionCloseFile.Enabled := True;
  ActionFileProperties.Enabled := True;

  // Recensement des composants d'édition
  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);

  // Chargement des infos des joueurs
  LoadPlayers;

  // Recensement des cartes
  with MasterFile do for I := 0 to MapFileCount-1 do
    MapTabSet.Tabs.Add(MapFiles[I].MapID);
  CurrentFloor := 0;
  MapTabSet.TabIndex := 0;
end;

{*
  Déharge le MasterFile
  Déharge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
var I : integer;
begin
  // Vider les onglets de carte
  MapTabSet.TabIndex := -1;
  MapTabSet.Tabs.Clear;

  // Vider les onglets des joueurs
  PlayersContainer.Categories.Clear;

  // Vider les composants d'édition
  with ScrewsContainer do
  begin
    for I := 0 to Categories.Count-1 do with Categories[I] do
    begin
      while Items.Count > 0 do
      begin
        if TObject(Items[0].Data) is TComponentSet then
          TObject(Items[0].Data).Free;
        Items.Delete(0);
      end;
    end;
  end;
  ScrewsImages.Clear;

  // Désactivation de l'interface utilisateur
  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionCloseFile.Enabled := False;
  ActionFileProperties.Enabled := False;

  // Autres variables
  Component := nil;
  Master := nil;
end;

{*
  Ouvre un fichier et le charge
  @param FileName   Nom du fichier maître à charger
*}
procedure TFormMain.OpenFile(FileName : TFileName);
begin
  MasterFile := TMasterFile.Create(FileName, fmEdit);
  LoadFile;
end;

{*
  Enregistre le fichier
  @param FileName   Fichier dans lequel enregistrer, ou vide pour demander
  @return True si l'enregistrement a bien été effectué, False sinon
*}
function TFormMain.SaveFile(FileName : TFileName = '') : boolean;
begin
  if FileName = '' then
  begin
    if SaveDialog.Execute then
    begin
      OpenDialog.FileName := SaveDialog.FileName;
      FileName := SaveDialog.FileName;
    end else
    begin
      Result := False;
      exit;
    end;
  end;

  ShowDialog(sError, sFeatureIsNotImplementedYet, dtError);
  Result := False;
  { TODO 1 : Enregistrer le fichier }
end;

{*
  Ferme le fichier
  @return True si le fichier a bien été fermé, False sinon
*}
function TFormMain.CloseFile : boolean;
begin
  Result := True;

  if MasterFile = nil then exit;

  if Modified then
  begin
    case ShowDialog(sConfirmExitTitle, sConfirmExit,
                    dtConfirmation, dbYesNoCancel) of
      drYes : Result := SaveFile(MasterFile.FileName);
      drCancel : Result := False;
    end;

    if not Result then exit;
  end;

  UnloadFile;

  MasterFile.Free;
  MasterFile := nil;
end;

procedure TFormMain.CenterToPlayerPosition(Player : TPlayer);
begin
  ShowDialog(sError, sFeatureIsNotImplementedYet, dtError);
  { TODO 1 : Centrer sur la position du joueur }
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ScrewBmp := TBitmap.Create;
  ScrewBmp.Width := ScrewSize;
  ScrewBmp.Height := ScrewSize;

  LastCompIndex := 0;

  OpenDialog.InitialDir := fLabyrinthsDir;
  SaveDialog.InitialDir := fLabyrinthsDir;

  MasterFile := nil;
  CurrentMap := nil;

  Component := nil;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ScrewBmp.Free;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseFile;
end;

procedure TFormMain.ActionNewFileExecute(Sender: TObject);
begin
  ShowDialog(sError, sFeatureIsNotImplementedYet, dtError);
  { TODO 1 : Créer un nouveau fichier }
end;

procedure TFormMain.ActionOpenFileExecute(Sender: TObject);
begin
  if not CloseFile then exit;

  if OpenDialog.Execute then
  begin
    SaveDialog.FileName := OpenDialog.FileName;
    OpenFile(OpenDialog.FileName);
  end;
end;

procedure TFormMain.ActionSaveFileExecute(Sender: TObject);
begin
  SaveFile(MasterFile.FileName);
end;

procedure TFormMain.ActionSaveFileAsExecute(Sender: TObject);
begin
  SaveFile;
end;

procedure TFormMain.ActionCloseFileExecute(Sender: TObject);
begin
  CloseFile;
end;

procedure TFormMain.ActionFilePropertiesExecute(Sender: TObject);
begin
  ShowDialog(sError, sFeatureIsNotImplementedYet, dtError);
  { TODO 1 : Modifier les propriétés }
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MapTabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  StaticPosition.Caption := '';
  StaticField.Caption := '';
  StaticEffect.Caption := '';
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

procedure TFormMain.PaintBoxMapPaint(Sender: TObject);
var Left, Top, Right, Bottom : integer;
    LeftZone, TopZone, RightZone, BottomZone : integer;
    I, X, Y : integer;
begin
  if CurrentMap = nil then exit;

  // Calcul des coordonnées à afficher
  Left := ScrollBoxMap.HorzScrollBar.Position div ScrewSize - MinViewSize;
  Top  := ScrollBoxMap.VertScrollBar.Position div ScrewSize - MinViewSize;
  Right  := Left + ScrollBoxMap.ClientWidth  div ScrewSize + 1;
  Bottom := Top  + ScrollBoxMap.ClientHeight div ScrewSize + 1;

  LeftZone := Left div CurrentMap.ZoneWidth;
  TopZone  := Top  div CurrentMap.ZoneHeight;
  RightZone  := LeftZone + Right  div CurrentMap.ZoneWidth ;
  BottomZone := TopZone  + Bottom div CurrentMap.ZoneHeight;

  // Dessin des cases
  for X := Left to Right do for Y := Top to Bottom do
  begin
    CurrentMap[Point3D(X, Y, CurrentFloor)].Draw(PaintBoxMap.Canvas,
      (MinViewSize+X) * ScrewSize, (MinViewSize+Y) * ScrewSize);
  end;

  // Dessin des joueurs
  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if Position.Z = CurrentFloor then
    begin
      Draw(PaintBoxMap.Canvas, (MinViewSize+Position.X) * ScrewSize,
        (MinViewSize+Position.Y) * ScrewSize);
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

procedure TFormMain.EditFloorChange(Sender: TObject);
begin
  FCurrentFloor := EditFloor.Value;
  PaintBoxMap.Invalidate;
end;

procedure TFormMain.ScrewsContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  if TObject(Button.Data) is TComponentSet then
    Component := TComponentSet(Button.Data).ChooseComponent(LastCompIndex)
  else
    Component := TScrewComponent(Button.Data);
end;

procedure TFormMain.PlayersContainerButtonClicked(Sender: TObject;
  const Button: TButtonItem);
var Player : TPlayer;
begin
  Player := Master.Players[integer(Button.Data) shr opShift];
  case integer(Button.Data) and opMask of
    opCenterToPosition : CenterToPlayerPosition(Player);
    opShowPlugins : ;
    opShowAttributes : ;
    opShowObjects : ;
  end;
end;

procedure TFormMain.PaintBoxMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Position : T3DPoint;
    NewID : TComponentID;
begin
  if (Button <> mbLeft) or (Component = nil) then exit;

  Position := Point3D(X div ScrewSize - 1, Y div ScrewSize - 1, CurrentFloor);

  NewID := Component.ID;
  with CurrentMap[Position] do
  begin
    if Component is TField    then NewID := NewID+'--' else
    if Component is TEffect   then NewID := Field.ID+'-'+NewID+'-' else
    if Component is TObstacle then NewID := ChangeObstacle(NewID).ID;
  end;

  if CurrentMap.InMap(Position) then
  begin
    CurrentMap[Position] := Master.Screw[NewID];
  end else
  begin
    if ShowDialog(sReplaceOutsideTitle, sReplaceOutside,
      dtConfirmation, dbOKCancel) <> drOK then exit;
    CurrentMap.Outside[CurrentFloor] := Master.Screw[NewID];
  end;

  Modified := True;
  PaintBoxMap.Invalidate;
end;

procedure TFormMain.PaintBoxMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var Position : T3DPoint;
begin
  if CurrentMap = nil then exit;

  Position := Point3D(X div ScrewSize - 1, Y div ScrewSize - 1, CurrentFloor);
  StaticPosition.Caption := Point3DToString(Position, ', ');

  with CurrentMap[Position] do
  begin
    StaticField.Caption := Field.Name;
    if Effect = nil then StaticEffect.Caption := '' else
      StaticEffect.Caption := Effect.Name;
    if Obstacle = nil then StaticObstacle.Caption := '' else
      StaticObstacle.Caption := Obstacle.Name;
  end;
end;

end.

