{*
  Unit� principale de Labyrinthe.exe
  Cette unit� contient la fiche principale de Labyrinthe.exe.
*}
unit LabyrintheMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ScUtils, ScStrUtils, NumberDialog,
  SdDialogs, ShellAPI, FunLabyUtils, PlayUtils, FilesUtils;

resourcestring
  sViewSize = 'Taille de la vue';
  sViewSizePrompt = 'Taille de la vue :';

  sExitConfirmTitle = 'Quitter FunLabyrinthe';
  sExitConfirm = 'Voulez-vous enregistrer la partie en cours ?';

type
  {*
    Classe de la fiche principale
  *}
  TFormMain = class(TForm)
    Image: TImage;
    StatusBar: TStatusBar;
    BigMenu: TMainMenu;
    BigMenuFile: TMenuItem;
    MenuExit: TMenuItem;
    MenuNewGame: TMenuItem;
    MenuSaveGame: TMenuItem;
    MenuLoadGame: TMenuItem;
    Sep2: TMenuItem;
    BigMenuHelp: TMenuItem;
    MenuHelpTopics: TMenuItem;
    Sep3: TMenuItem;
    MenuAbout: TMenuItem;
    NewGameDialog: TOpenDialog;
    SaveGameDialog: TSaveDialog;
    Sep1: TMenuItem;
    MenuDescription: TMenuItem;
    BigMenuOptions: TMenuItem;
    MenuTips: TMenuItem;
    MenuProperties: TMenuItem;
    MenuMapProperties: TMenuItem;
    MenuPlayerProperties: TMenuItem;
    MenuReloadGame: TMenuItem;
    AboutDialog: TSdAboutDialog;
    LoadGameDialog: TOpenDialog;
    TimerUpdateImage: TTimer;
    MenuViewSize: TMenuItem;
    procedure MenuViewSizeClick(Sender: TObject);
    procedure UpdateImage(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MovePlayer(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuNewGameClick(Sender: TObject);
    procedure MenuLoadGameClick(Sender: TObject);
    procedure MenuHelpTopicsClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuSaveGameClick(Sender: TObject);
    procedure MenuDescriptionClick(Sender: TObject);
    procedure MenuMapPropertiesClick(Sender: TObject);
    procedure MenuPlayerPropertiesClick(Sender: TObject);
    procedure MenuTipsClick(Sender: TObject);
    procedure MenuReloadGameClick(Sender: TObject);
  private
    { D�clarations priv�es }
    HiddenBitmap : TBitmap;
    LastFileName : TFileName;

    MasterFile : TMasterFile;
    Master : TMaster;
    View : TPlayerView;

    MoveThread : TMoveThread;

    procedure NewGame(FileName : TFileName);
    procedure EndGame;
    function SaveGame : boolean;

    procedure AdaptSizeToView;
    procedure ShowStatus;

    procedure MovingTerminated(Sender : TObject);
  public
    { D�clarations publiques }
  end;

var
  FormMain : TFormMain; /// Instance de la fiche principale

implementation

{$R *.DFM}

uses
  PropertiesDialog;

////////////////////////
/// Classe TFormMain ///
////////////////////////

{*
  Commence une nouvelle partie
  @param FileName   Nom du fichier ma�tre � charger
*}
procedure TFormMain.NewGame(FileName : TFileName);
begin
  if MasterFile <> nil then
    EndGame;

  LastFileName := FileName;
  MasterFile := TMasterFile.Create(FileName, fmPlay);
  Master := MasterFile.Master;
  View := TPlayerView.Create(Master.Players[0]);

  MoveThread := nil;

  Caption := MasterFile.Title;
  MenuReloadGame.Enabled := True;
  MenuSaveGame.Enabled := True;
  MenuDescription.Enabled := True;
  MenuProperties.Enabled := True;
  MenuViewSize.Enabled := True;
  MenuTips.Checked := False;
  ShowStatus;

  AdaptSizeToView;

  OnKeyDown := MovePlayer;
  TimerUpdateImage.Enabled := True;
end;

{*
  Termine la partie et lib�re les classes m�tier
*}
procedure TFormMain.EndGame;
begin
  TimerUpdateImage.Enabled := False;
  OnKeyDown := nil;

  MenuSaveGame.Enabled := False;
  MenuDescription.Enabled := False;
  MenuProperties.Enabled := False;
  MenuViewSize.Enabled := False;

  View.Free;
  MasterFile.Free;

  View := nil;
  Master := nil;
  MasterFile := nil;
end;

{*
  Enregistre la partie en cours
  @return True si la partie a effectivement �t� enregistr�e, False sinon
*}
function TFormMain.SaveGame : boolean;
begin
  if MasterFile = nil then Result := True else
  begin
    Result := SaveGameDialog.Execute;

    if Result then
      MasterFile.Save(SaveGameDialog.FileName);
  end;
end;

{*
  Adapte la taille de HiddenBitmap et de la fen�tre � la taille de la vue
*}
procedure TFormMain.AdaptSizeToView;
var ImgSize : integer;
begin
  if View = nil then ImgSize := 270 else
    ImgSize := View.TotalSize * ScrewSize;

  with HiddenBitmap do
  begin
    Width := ImgSize;
    Height := ImgSize;
  end;

  ClientWidth := ImgSize;
  ClientHeight := ImgSize+19;

  Position := poScreenCenter;
end;

{*
  Affiche les quatre objets principaux dans la barre de statut
*}
procedure TFormMain.ShowStatus;
var I : integer;
begin
  if MasterFile = nil then exit;
  for I := 0 to 3 do
  begin
    if I >= Master.ObjectDefCount then Break;
    StatusBar.Panels[I].Text :=
      Master.ObjectDefs[I].ShownInfos[View.Player];
  end;
end;

{*
  Gestionnaire d'�v�nement pour MoveThread.OnTerminated
*}
procedure TFormMain.MovingTerminated(Sender : TObject);
begin
  if MoveThread.FatalException <> nil then
    with Exception(MoveThread.FatalException) do
      ShowDialog(ClassName, Message, dtError);
  MoveThread := nil;
end;

{*
  Gestionnaire d'�v�nement OnCreate
  @param Sender   Object qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  Menu := BigMenu;
  NewGameDialog.InitialDir := fLabyrinthsDir;
  LoadGameDialog.InitialDir := fSaveguardsDir;
  SaveGameDialog.InitialDir := fSaveguardsDir;

  LastFileName := '';

  MasterFile := nil;
  Master := nil;
  View := nil;

  HiddenBitmap := TBitmap.Create;
  AdaptSizeToView;
  with HiddenBitmap.Canvas do
  begin
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Rectangle(0, 0, HiddenBitmap.Width, HiddenBitmap.Height);
  end;
  Image.Picture.Assign(HiddenBitmap);

  if ParamCount > 0 then
    NewGame(ParamStr(1));
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuNewGameClick(Sender: TObject);
begin
  if not NewGameDialog.Execute then exit;
  if ofExtensionDifferent in NewGameDialog.Options then
    RunURL(NewGameDialog.FileName)
  else
    NewGame(NewGameDialog.FileName);
end;

procedure TFormMain.MenuLoadGameClick(Sender: TObject);
begin
  if not LoadGameDialog.Execute then exit;
  if ofExtensionDifferent in LoadGameDialog.Options then
    RunURL(LoadGameDialog.FileName)
  else
    NewGame(LoadGameDialog.FileName);
end;

procedure TFormMain.MenuHelpTopicsClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormMain.MenuAboutClick(Sender: TObject);
begin
  AboutDialog.Execute;
end;

procedure TFormMain.MenuSaveGameClick(Sender: TObject);
begin
  SaveGame;
end;

procedure TFormMain.MenuDescriptionClick(Sender: TObject);
begin
  ShowDialog(sDescription, MasterFile.Description);
end;

procedure TFormMain.MenuMapPropertiesClick(Sender: TObject);
begin
  FormProperties.ShowMapProps(View.Player.Map);
end;

procedure TFormMain.MenuPlayerPropertiesClick(Sender: TObject);
begin
  FormProperties.ShowPlayerProps(View.Player);
end;

procedure TFormMain.MenuTipsClick(Sender: TObject);
begin
  MenuTips.Checked := not MenuTips.Checked;
end;

procedure TFormMain.MenuReloadGameClick(Sender: TObject);
begin
  NewGame(LastFileName);
end;

{*
  Gestionnaire d'�v�nement OnCloseQuery
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param CanClose   � position � False pour emp�cher la fermeture
*}
procedure TFormMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if MasterFile <> nil then
  case ShowDialog(sExitConfirmTitle, sExitConfirm,
                  dtConfirmation, dbYesNoCancel) of
    drYes : CanClose := SaveGame;
    drCancel : CanClose := False;
  end;
end;

{*
  Gestionnaire d'�v�nement OnKeyDown, qui d�place le joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Key      Touche enfonc�e
  @param Shift    �tat des touches syst�me
*}
procedure TFormMain.MovePlayer(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Dir : TDirection;
begin
  if (MasterFile = nil) or (MoveThread <> nil) then exit;

  case Key of
    VK_UP    : Dir := diNorth;
    VK_RIGHT : Dir := diEast;
    VK_DOWN  : Dir := diSouth;
    VK_LEFT  : Dir := diWest;
    else exit;
  end;

  MoveThread := TMoveThread.Create(View.Player, Dir, MovingTerminated);
end;

{*
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Object qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if MasterFile <> nil then
    EndGame;

  HiddenBitmap.Free;
end;

{*
  Gestionnaire d'�v�nement OnTimer
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.UpdateImage(Sender: TObject);
begin
  View.Draw(HiddenBitmap.Canvas);
  Image.Picture.Assign(HiddenBitmap);
  ShowStatus;

  // V�rification de terminaison
  if Master.Terminated and (MoveThread = nil) then
    EndGame;
end;

{*
  Gestionnaire du menu 'Taille de la vue'
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuViewSizeClick(Sender: TObject);
begin
  View.Size := TFormNumber.ChooseNumber(sViewSize, sViewSizePrompt,
    View.Size, View.MinSize, View.MaxSize);
  AdaptSizeToView;
end;

end.

