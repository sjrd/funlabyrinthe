{*
  Unit� principale de Labyrinthe.exe
  Cette unit� contient la fiche principale de Labyrinthe.exe.
*}
unit LabyrintheMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ScUtils, ScStrUtils, NumberDialog,
  SdDialogs, ShellAPI, FunLabyUtils, PlayUtils, FilesUtils, PlayerObjects;

resourcestring
  sViewSize = 'Taille de la vue';
  sViewSizePrompt = 'Taille de la vue :';

  sExitConfirmTitle = 'Enregistrer la partie';
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

    MasterFile : TMasterFile;
    Master : TMaster;
    View : TPlayerView;
    LastFileName : TFileName;

    MoveThread : TMoveThread;

    procedure NewGame(FileName : TFileName);
    function SaveGame : boolean;
    function CloseGame(DontSave : boolean = False) : boolean;

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

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Commence une nouvelle partie
  @param FileName   Nom du fichier ma�tre � charger
*}
procedure TFormMain.NewGame(FileName : TFileName);
begin
  MasterFile := TMasterFile.Create(FileName, fmPlay);
  Master := MasterFile.Master;
  View := TPlayerView.Create(Master.Players[0]);
  LastFileName := FileName;

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
  Enregistre la partie en cours
  @return True si la partie a effectivement �t� enregistr�e, False sinon
*}
function TFormMain.SaveGame : boolean;
begin
  if MasterFile = nil then Result := True else
  begin
    Result := SaveGameDialog.Execute;

    if Result then
    begin
      MasterFile.Save(SaveGameDialog.FileName);
      LastFileName := SaveGameDialog.FileName;
    end;
  end;
end;

{*
  Termine la partie et lib�re les classes m�tier
  @param DontSave   � True, ne demande pas � l'utilisateur d'enregistrer
  @return True si la partie a effectivement �t� termin�e, False sinon
*}
function TFormMain.CloseGame(DontSave : boolean = False) : boolean;
begin
  if MasterFile = nil then
  begin
    Result := True;
    exit;
  end;

  if DontSave then Result := True else
  begin
    case ShowDialog(sExitConfirmTitle, sExitConfirm,
                    dtConfirmation, dbYesNoCancel) of
      drYes : Result := SaveGame;
      drCancel : Result := False;
      else Result := True;
    end;

    if not Result then exit;
  end;

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
  Adapte la taille de HiddenBitmap et de la fen�tre � la taille de la vue
*}
procedure TFormMain.AdaptSizeToView;
var ImgWidth, ImgHeight : integer;
begin
  if View = nil then
  begin
    ImgWidth := 270;
    ImgHeight := 270;
  end else
  begin
    ImgWidth := View.Width * ScrewSize;
    ImgHeight := View.Height * ScrewSize;
  end;

  with HiddenBitmap do
  begin
    Width := ImgWidth;
    Height := ImgHeight;
  end;

  ClientWidth := ImgWidth;
  ClientHeight := ImgHeight+19;

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
  if CloseGame then
  begin
    if not NewGameDialog.Execute then exit;

    if ofExtensionDifferent in NewGameDialog.Options then
      RunURL(NewGameDialog.FileName)
    else
      NewGame(NewGameDialog.FileName);
  end;
end;

procedure TFormMain.MenuLoadGameClick(Sender: TObject);
begin
  if CloseGame then
  begin
    if not LoadGameDialog.Execute then exit;

    if ofExtensionDifferent in LoadGameDialog.Options then
      RunURL(LoadGameDialog.FileName)
    else
      NewGame(LoadGameDialog.FileName);
  end;
end;

procedure TFormMain.MenuHelpTopicsClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormMain.MenuAboutClick(Sender: TObject);
begin
  ShowFunLabyAbout;
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
  TFormObjects.ShowObjects(View.Player);
end;

procedure TFormMain.MenuTipsClick(Sender: TObject);
begin
  MenuTips.Checked := not MenuTips.Checked;
end;

procedure TFormMain.MenuReloadGameClick(Sender: TObject);
begin
  CloseGame(True);
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
  CanClose := CloseGame;
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
  CloseGame(True);

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
    CloseGame(True);
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

