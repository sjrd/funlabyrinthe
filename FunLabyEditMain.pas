unit FunLabyEditMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, Menus, ImgList, StdCtrls,
  ExtCtrls, Tabs, ComCtrls, ActnMenus, ToolWin, ActnCtrls, CategoryButtons,
  StdActns, ScUtils, FunLabyUtils, FilesUtils;

resourcestring
  sFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore implémentée';

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
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
  private
    { Déclarations privées }
    MasterFile : TMasterFile;

    procedure LoadFile;
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
  Charge le MasterFile créé
  Charge le MasterFile créé dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
begin
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
end;

{*
  Ferme le fichier
  @return True si le fichier a bien été fermé, False sinon
*}
function TFormMain.CloseFile : boolean;
begin
  Result := True;
end;

procedure TFormMain.ActionNewFileExecute(Sender: TObject);
begin
  ShowDialog(sError, sFeatureIsNotImplementedYet, dtError);
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

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

end.

