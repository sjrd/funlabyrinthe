{*
  Unit� principale de FunLaby.exe
  Cette unit� contient la fiche principale de FunLaby.exe.
  @author sjrd
  @version 5.0
*}
unit FunLabyMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ScUtils, ScStrUtils, SdDialogs, ShellAPI,
  FunLabyUtils, PlayUtils, FilesUtils, PlayerObjects, SepiReflectionCore,
  UnitFiles, SepiImportsFunLaby, SepiImportsFunLabyTools;

resourcestring
  sViewSize = 'Taille de la vue';
  sViewSizePrompt = 'Taille de la vue :';

  sExitConfirmTitle = 'Enregistrer la partie';
  sExitConfirm = 'Voulez-vous enregistrer la partie en cours ?';

type
  {*
    Classe de la fiche principale
    @author sjrd
    @version 5.0
  *}
  TFormMain = class(TForm)
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
    MenuProperties: TMenuItem;
    MenuMapProperties: TMenuItem;
    MenuPlayerProperties: TMenuItem;
    MenuReloadGame: TMenuItem;
    LoadGameDialog: TOpenDialog;
    TimerUpdateImage: TTimer;
    MenuViewSize: TMenuItem;
    PaintBox: TPaintBox;
    procedure PaintBoxPaint(Sender: TObject);
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
    procedure MenuReloadGameClick(Sender: TObject);
  private
    { D�clarations priv�es }
    SepiRootManager: TSepiAsynchronousRootManager;
    SepiRoot: TSepiRoot;

    MasterFile: TMasterFile;
    Master: TMaster;
    Controller: TPlayerController;
    GameEnded: Boolean;
    LastFileName: TFileName;

    procedure NewGame(FileName: TFileName);
    function SaveGame: Boolean;
    function CloseGame(DontSave: Boolean = False): Boolean;

    procedure AdaptSizeToView;
    procedure ShowStatus;
  public
    { D�clarations publiques }
  end;

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.DFM}

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Commence une nouvelle partie
  @param FileName   Nom du fichier ma�tre � charger
*}
procedure TFormMain.NewGame(FileName: TFileName);
begin
  while not SepiRootManager.Ready do
    Sleep(100);

  MasterFile := TMasterFile.Create(SepiRoot, FileName, fmPlay);
  Master := MasterFile.Master;
  Controller := TPlayerController.Create(Master.Players[0]);
  GameEnded := False;
  LastFileName := FileName;

  Caption := MasterFile.Title;
  MenuReloadGame.Enabled := True;
  MenuSaveGame.Enabled := True;
  MenuDescription.Enabled := True;
  MenuProperties.Enabled := True;
  MenuViewSize.Enabled := True;
  ShowStatus;

  AdaptSizeToView;

  OnKeyDown := MovePlayer;
  TimerUpdateImage.Enabled := True;

  if not MasterFile.IsSaveguard then
  begin
    MasterFile.GameStarted;
    Controller.PostNotificationMessage(msgGameStarted);
  end;
end;

{*
  Enregistre la partie en cours
  @return True si la partie a effectivement �t� enregistr�e, False sinon
*}
function TFormMain.SaveGame: Boolean;
begin
  if MasterFile = nil then
    Result := True
  else
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
function TFormMain.CloseGame(DontSave: Boolean = False): Boolean;
begin
  if MasterFile = nil then
  begin
    Result := True;
    Exit;
  end;

  if DontSave or Master.Terminated then
    Result := True
  else
  begin
    case ShowDialog(sExitConfirmTitle, sExitConfirm,
        dtConfirmation, dbYesNoCancel) of
      drYes: Result := SaveGame;
      drCancel: Result := False;
    else
      Result := True;
    end;

    if not Result then
      Exit;
  end;

  Controller.Terminate;
  Controller.WaitFor;

  TimerUpdateImage.Enabled := False;
  OnKeyDown := nil;

  MenuSaveGame.Enabled := False;
  MenuDescription.Enabled := False;
  MenuProperties.Enabled := False;
  MenuViewSize.Enabled := False;

  Controller.Free;
  MasterFile.Free;

  Controller := nil;
  Master := nil;
  MasterFile := nil;

  PaintBox.Invalidate;
end;

{*
  Adapte la taille de la fen�tre � la taille de la vue
*}
procedure TFormMain.AdaptSizeToView;
var
  ImgWidth, ImgHeight: Integer;
begin
  if Controller = nil then
  begin
    ImgWidth := 270;
    ImgHeight := 270;
  end else
  begin
    ImgWidth := Controller.ViewWidth;
    ImgHeight := Controller.ViewHeight;
  end;

  ClientWidth := ImgWidth;
  ClientHeight := ImgHeight+StatusBar.Height;

  Position := poScreenCenter;
end;

{*
  Affiche les quatre objets principaux dans la barre de statut
*}
procedure TFormMain.ShowStatus;
var
  I: Integer;
begin
  if MasterFile = nil then
    Exit;
  for I := 0 to 3 do
  begin
    if I >= Master.ObjectDefCount then
      Break;
    StatusBar.Panels[I].Text :=
      Master.ObjectDefs[I].ShownInfos[Controller.Player];
  end;
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

  SepiRootManager := TSepiAsynchronousRootManager.Create;
  SepiRootManager.LoadUnit('FunLabyUtils');
  SepiRoot := SepiRootManager.Root;

  MasterFile := nil;
  Master := nil;
  Controller := nil;
  LastFileName := '';

  AdaptSizeToView;

  DoubleBuffered := True;

  if ParamCount > 0 then
    NewGame(ParamStr(1));
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Quitter
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close;
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Nouvelle partie
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuNewGameClick(Sender: TObject);
begin
  if CloseGame then
  begin
    if not NewGameDialog.Execute then
      Exit;

    if ofExtensionDifferent in NewGameDialog.Options then
      RunURL(NewGameDialog.FileName)
    else
      NewGame(NewGameDialog.FileName);
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Charger
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuLoadGameClick(Sender: TObject);
begin
  if CloseGame then
  begin
    if not LoadGameDialog.Execute then
      Exit;

    if ofExtensionDifferent in LoadGameDialog.Options then
      RunURL(LoadGameDialog.FileName)
    else
      NewGame(LoadGameDialog.FileName);
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Rubriques d'aide
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuHelpTopicsClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu � propos
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuAboutClick(Sender: TObject);
begin
  ShowFunLabyAbout;
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Sauver
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuSaveGameClick(Sender: TObject);
begin
  SaveGame;
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Description
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuDescriptionClick(Sender: TObject);
begin
  ShowDialog(sDescription, MasterFile.Description);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Propri�t�s de la carte
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuMapPropertiesClick(Sender: TObject);
begin
// Don't delete this comment
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Propri�t�s du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuPlayerPropertiesClick(Sender: TObject);
begin
  TFormObjects.ShowObjects(Controller.Player);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Recommencer
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
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
begin
  Controller.PressKey(Key, Shift);
end;

{*
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Object qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  CloseGame(True);

  SepiRootManager.Free;
end;

{*
  Gestionnaire d'�v�nement OnTimer
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.UpdateImage(Sender: TObject);
begin
  if (not GameEnded) and Master.Terminated then
  begin
    GameEnded := True;
    MasterFile.GameEnded;
  end;

  PaintBox.Invalidate;
  ShowStatus;
end;

{*
  Gestionnaire du menu 'Taille de la vue'
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuViewSizeClick(Sender: TObject);
begin
  with Controller.Player do
    ViewBorderSize := QueryNumber(sViewSize, sViewSizePrompt,
      ViewBorderSize, MinViewSize, Map.MaxViewSize);

  AdaptSizeToView;
end;

{*
  Gestionnaire d'�v�nement OnPaint de la paint box
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.PaintBoxPaint(Sender: TObject);
begin
  if Assigned(Controller) then
    Controller.DrawView(PaintBox.Canvas)
  else
  begin
    with PaintBox.Canvas do
    begin
      Brush.Color := clBlack;
      Pen.Color := clBlack;
      Rectangle(0, 0, PaintBox.Width, PaintBox.Height);
    end;
  end;
end;

end.

