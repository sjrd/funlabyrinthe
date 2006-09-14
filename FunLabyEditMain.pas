unit FunLabyEditMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, Menus, ImgList, StdCtrls,
  ExtCtrls, Tabs, ComCtrls, ActnMenus, ToolWin, ActnCtrls, CategoryButtons,
  StdActns, ScUtils, FunLabyUtils, FilesUtils, Spin;

resourcestring
  sFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore implémentée';

  sConfirmExitTitle = 'Quitter FunLabyEdit';
  sConfirmExit = 'Le fichier a été modifé. Voulez-vous l''enregistrer ?';

type
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure MapTabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PaintBoxMapPaint(Sender: TObject);
    procedure EditFloorChange(Sender: TObject);
  private
    { Déclarations privées }
    ScrewBmp : TBitmap;       /// Bitmap à tout faire de la taille d'une case

    Modified : boolean;
    MasterFile : TMasterFile; /// Fichier maître
    Master : TMaster;         /// Maître FunLabyrinthe
    CurrentMap : TMap;        /// Carte courante

    FCurrentFloor : integer;  /// Étage courant

    procedure SetCurrentFloor(Value : integer);

    function AddScrewButton(Template : TScrewComponent) : TButtonItem;
    procedure RegisterSingleComponent(Component : TScrewComponent); stdcall;
    procedure RegisterComponentSet(Template : TScrewComponent;
      Components : array of TScrewComponent;
      const DialogTitle, DialogPrompt : string); stdcall;

    procedure LoadFile;
    procedure UnloadFile;

    procedure OpenFile(FileName : TFileName);
    function SaveFile(FileName : TFileName = '') : boolean;
    function CloseFile : boolean;

    property CurrentFloor : integer read FCurrentFloor write SetCurrentFloor;
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

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
  Button.Data := nil;
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
  Button.Data := nil;
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

  // Recensement des composants d'édition
  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);

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

  // Vider les composants d'édition
  with ScrewsContainer do
  begin
    for I := 0 to Categories.Count-1 do with Categories[I] do
      while Items.Count > 0 do Items.Delete(0);
  end;
  ScrewsImages.Clear;

  // Désactivation de l'interface utilisateur
  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionCloseFile.Enabled := False;

  // Autres variables
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ScrewBmp := TBitmap.Create;
  ScrewBmp.Width := ScrewSize;
  ScrewBmp.Height := ScrewSize;

  OpenDialog.InitialDir := fLabyrinthsDir;
  SaveDialog.InitialDir := fLabyrinthsDir;

  MasterFile := nil;
  CurrentMap := nil;
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

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MapTabSetChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
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

end.

