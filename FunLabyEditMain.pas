unit FunLabyEditMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, Menus, ImgList, StdCtrls,
  ExtCtrls, Tabs, ComCtrls, ActnMenus, ToolWin, ActnCtrls, CategoryButtons,
  StdActns, ScUtils, FunLabyUtils, FilesUtils;

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
    ImageMap: TImage;
    SplitterScrews: TSplitter;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    LabelPosition: TLabel;
    LabelField: TLabel;
    LabelEffect: TLabel;
    LabelObstacle: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
  private
    { Déclarations privées }
    ScrewBmp : TBitmap;

    MasterFile : TMasterFile;
    Modified : boolean;

    procedure RegisterSingleComponent(Component : TScrewComponent); stdcall;
    procedure RegisterComponentSet(Template : TScrewComponent;
      Components : array of TScrewComponent;
      const DialogTitle, DialogPrompt : string); stdcall;

    procedure LoadFile;
    procedure UnloadFile;

    procedure OpenFile(FileName : TFileName);
    function SaveFile(FileName : TFileName = '') : boolean;
    function CloseFile : boolean;
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFormMain.RegisterSingleComponent(Component : TScrewComponent);
var ImageIndex : integer;
    Category : TButtonCategory;
    Button : TButtonItem;
begin
  // Ajout de l'image du composant dans la liste d'images
  EmptyScrewRect(ScrewBmp.Canvas);
  Component.Draw(ScrewBmp.Canvas);
  ImageIndex := ScrewsImages.AddMasked(ScrewBmp, clTransparent);

  // Choix de la catégorie
  if Component is TField    then Category := ScrewsContainer.Categories[0] else
  if Component is TEffect   then Category := ScrewsContainer.Categories[1] else
  if Component is TObstacle then Category := ScrewsContainer.Categories[2] else
  Category := ScrewsContainer.Categories[3];

  // Ajout du bouton
  Button := Category.Items.Add;
  Button.ImageIndex := ImageIndex;
  Button.Hint := Component.Name;
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
begin
end;

{*
  Charge le MasterFile créé
  Charge le MasterFile créé dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
begin
  Modified := MasterFile.FileName = '';

  ActionSaveFile.Enabled := True;
  ActionSaveFileAs.Enabled := True;
  ActionCloseFile.Enabled := True;

  MasterFile.RegisterComponents(RegisterSingleComponent, RegisterComponentSet);
end;

{*
  Déharge le MasterFile
  Déharge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
var I : integer;
begin
  with ScrewsContainer do
  begin
    for I := 0 to Categories.Count-1 do with Categories[I] do
      while Items.Count > 0 do Items.Delete(0);
  end;
  ScrewsImages.Clear;

  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionCloseFile.Enabled := False;
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

  Result := True;

  ShowDialog(sError, sFeatureIsNotImplementedYet, dtError);
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

end.

