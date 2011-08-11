{*
  Unit� principale de FunLaby.exe
  Cette unit� contient la fiche principale de FunLaby.exe.
  @author sjrd
  @version 5.0
*}
unit FunLabyMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ShellAPI,
  JvComponentBase, JvAppStorage, JvAppXMLStorage,
  ScUtils, ScStrUtils, ScSyncObjs, SdDialogs,
  FunLabyUtils, PlayUtils, FilesUtils, PlayerObjects,
  SepiReflectionCore, SepiImportsFunLaby, SepiImportsFunLabyTools,
  FunLabyCoreConsts, GR32, GR32_Image, SelectProjectForm;

resourcestring
  SErrorTitle = 'Erreur';
  SFatalErrorTitle = 'Erreur fatale';
  SDescriptionTitle = 'Description';

  sBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalit�s coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  SCantPlayNeedOnePlayer = 'Impossible de jouer � ce labyrinthe car il ne '+
    'contient pas exactement un joueur.';
  SCantPlayPlayerIsNowhere = 'Impossible de jouer � ce labyrinthe car le '+
    'joueur n''a pas �t� plac� sur une carte.';

  sExitConfirmTitle = 'Enregistrer la partie';
  sExitConfirm = 'Voulez-vous enregistrer la partie en cours ?';

  sCantPauseTitle = 'Pause impossible';
  sCantPause =
    'Impossible de mettre en pause maintenant, car le joueur est occup�';

  SAskForTutorialTitle = 'Didacticiel';
  SAskForTutorial =
    'Vous n''avez pas encore fait le didacticiel. Celui-ci vous apprendra � '+
    'jouer � FunLabyrinthe en vous expliquant pas-�-pas. Voulez-vous le '+
    'd�marrer maintenant ? (R�pondez Annuler si vous ne voulez pas que je '+
    'vous redemande la prochaine fois.)';

type
  TSaveScreenshotProc = reference to procedure(Screenshot: TBitmap32);

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
    MenuPlayerObjects: TMenuItem;
    MenuReloadGame: TMenuItem;
    LoadGameDialog: TOpenDialog;
    TimerUpdateImage: TTimer;
    PaintBox: TPaintBox32;
    MenuVersionCheck: TMenuItem;
    OptionsStorage: TJvAppXMLFileStorage;
    BigMenuScreenshot: TMenuItem;
    MenuScreenshotToClipboard: TMenuItem;
    MenuScreenshotToFile: TMenuItem;
    SaveScreenshotDialog: TSaveDialog;
    MenuScreenshotToFileAuto: TMenuItem;
    Sep4: TMenuItem;
    MenuOpenProjectManager: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateImage(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuNewGameClick(Sender: TObject);
    procedure MenuLoadGameClick(Sender: TObject);
    procedure MenuHelpTopicsClick(Sender: TObject);
    procedure MenuVersionCheckClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuSaveGameClick(Sender: TObject);
    procedure MenuDescriptionClick(Sender: TObject);
    procedure MenuPlayerObjectsClick(Sender: TObject);
    procedure MenuReloadGameClick(Sender: TObject);
    procedure MenuScreenshotToClipboardClick(Sender: TObject);
    procedure MenuScreenshotToFileClick(Sender: TObject);
    procedure PaintBoxPaintBuffer(Sender: TObject);
    procedure MenuScreenshotToFileAutoClick(Sender: TObject);
    procedure MenuOpenProjectManagerClick(Sender: TObject);
  private
    BackgroundTasks: TScTaskQueue; /// T�ches d'arri�re-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// T�che de chargement de la racine de base

    MasterFile: TMasterFile;       /// Fichier ma�tre
    Master: TMaster;               /// Ma�tre FunLabyrinthe
    Controller: TPlayerController; /// Contr�leur du joueur
    GameEnded: Boolean;            /// Indique si la partie est finie
    LastFileName: TFileName;       /// Dernier nom de fichier ouvert

    /// Bo�te de dialogue nouvelle partie
    NewGameDialog2: TFormSelectProjectFile;

    procedure LoadBaseSepiRoot;
    procedure NeedBaseSepiRoot;
    procedure BackgroundDiscard(var Obj);

    procedure NewGame(FileName: TFileName);
    function SaveGame: Boolean;
    function CloseGame(DontSave: Boolean = False): Boolean;

    function CheckValidMasterFile: Boolean;

    function TryPause: Boolean;
    procedure Resume;

    procedure AdaptSizeToView;
    procedure ShowStatus;

    procedure MakeScreenshot(const SaveProc: TSaveScreenshotProc);
    procedure SaveBitmap32ToPNGFile(Bitmap32: TBitmap32;
      const FileName: TFileName);

    procedure AskForTutorialIfNeeded;
  end;

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.DFM}

uses
  Clipbrd, pngimage;

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Charge la racine Sepi de base
  Cette m�thode est appel�e dans le thread des t�ches en arri�re-plan
*}
procedure TFormMain.LoadBaseSepiRoot;
begin
  BaseSepiRoot.LoadUnit('FunLabyUtils');
  BaseSepiRoot.LoadUnit('Generics');
  BaseSepiRoot.LoadUnit('GraphicsTools');
  BaseSepiRoot.LoadUnit('MapTools');
  BaseSepiRoot.LoadUnit('PlayerObjects');
end;

{*
  Attend que la racine Sepi de base soit charg�e, et v�rifie l'�tat d'erreur
  En cas d'erreur pendant le chargement de la racine de base, un message
  avertit l'utilisateur et le programme est arr�t� brutalement.
*}
procedure TFormMain.NeedBaseSepiRoot;
begin
  try
    BaseSepiRootLoadTask.WaitFor;
    BaseSepiRootLoadTask.RaiseException;
  except
    on Error: Exception do
    begin
      ShowDialog(SFatalErrorTitle,
        Format(SBaseSepiRootLoadError, [Error.Message]), dtError);
      System.Halt(1);
    end;
    on Error: TObject do
    begin
      ShowDialog(SFatalErrorTitle,
        Format(SBaseSepiRootLoadError, [Error.ClassName]), dtError);
      System.Halt(1);
    end;
  end;
end;

{*
  Cr�e une nouvelle t�che d'arri�re-plan de lib�ration d'un objet
  @param Obj   Objet � lib�rer en arri�re-plan (mis � nil)
*}
procedure TFormMain.BackgroundDiscard(var Obj);
var
  AObj: TObject;
begin
  AObj := TObject(Obj);
  if AObj = nil then
    Exit;
  TObject(Obj) := nil;
  TScMethodTask.Create(BackgroundTasks, AObj.Free);
end;

{*
  Commence une nouvelle partie
  @param FileName   Nom du fichier ma�tre � charger
*}
procedure TFormMain.NewGame(FileName: TFileName);
begin
  NeedBaseSepiRoot;

  // Try not to look for trouble more than necessary
  while not BackgroundTasks.Ready do
    Sleep(50);

  MasterFile := TMasterFile.Create(BaseSepiRoot, FileName, fmPlay);
  Master := MasterFile.Master;

  if not CheckValidMasterFile then
  begin
    Master := nil;
    BackgroundDiscard(MasterFile);
    Exit;
  end;

  Controller := TPlayerController.Create(Master.Players[0]);
  GameEnded := False;
  LastFileName := FileName;

  Caption := MasterFile.Title;
  MenuReloadGame.Enabled := True;
  MenuSaveGame.Enabled := True;
  MenuDescription.Enabled := True;
  MenuPlayerObjects.Enabled := True;
  BigMenuScreenshot.Visible := True;
  ShowStatus;

  AdaptSizeToView;

  OnKeyDown := FormKeyDown;
  TimerUpdateImage.Enabled := True;

  if not MasterFile.IsSaveguard then
    MasterFile.GameStarted;
end;

{*
  Enregistre la partie en cours
  @return True si la partie a effectivement �t� enregistr�e, False sinon
*}
function TFormMain.SaveGame: Boolean;
var
  WasPaused: Boolean;
begin
  if MasterFile = nil then
    Result := True
  else
  begin
    WasPaused := Master.Paused;
    if (not WasPaused) and (not TryPause) then
    begin
      Result := False;
      Exit;
    end;

    try
      Result := SaveGameDialog.Execute;

      if Result then
      begin
        MasterFile.Save(SaveGameDialog.FileName);
        LastFileName := SaveGameDialog.FileName;
      end;
    finally
      if not WasPaused then
        Resume;
    end;
  end;
end;

{*
  Termine la partie et lib�re les classes m�tier
  @param DontSave   � True, ne demande pas � l'utilisateur d'enregistrer
  @return True si la partie a effectivement �t� termin�e, False sinon
*}
function TFormMain.CloseGame(DontSave: Boolean = False): Boolean;
var
  WasPaused: Boolean;
begin
  if MasterFile = nil then
  begin
    Result := True;
    Exit;
  end;

  WasPaused := Master.Paused;
  if (not WasPaused) and (not TryPause) then
  begin
    Result := False;
    Exit;
  end;

  if DontSave or Master.Terminated then
    Result := True
  else
  begin
    Result := False;

    try
      case ShowDialog(sExitConfirmTitle, sExitConfirm,
          dtConfirmation, dbYesNoCancel) of
        drYes: Result := SaveGame;
        drCancel: Result := False;
      else
        Result := True;
      end;
    finally
      if (not WasPaused) or Result then
        Resume;
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
  MenuPlayerObjects.Enabled := False;
  BigMenuScreenshot.Visible := False;

  StatusBar.Panels.Clear;

  Master := nil;
  try
    FreeAndNil(Controller);
  except
  end;
  try
    FreeAndNil(MasterFile);
  except
  end;

  PaintBox.Invalidate;
end;

{*
  Teste si le fichier ma�tre est valide pour jouer
  @return True ssi le fichier est valide
*}
function TFormMain.CheckValidMasterFile: Boolean;
begin
  Result := False;

  // Need exactly one player
  if Master.PlayerCount <> 1 then
  begin
    ShowDialog(SErrorTitle, SCantPlayNeedOnePlayer, dtError);
    Exit;
  end;

  // Need the player to be placed on a valid position
  if Master.Players[0].Map = nil then
  begin
    ShowDialog(SErrorTitle, SCantPlayPlayerIsNowhere, dtError);
    Exit;
  end;

  Result := True;
end;

{*
  Essaye de mettre le jeu en pause
  Si TryPause renvoie True, vous devez appelez Resume pour red�marrer le jeu.
  @return True si le jeu a �t� mis en pause, False sinon
*}
function TFormMain.TryPause: Boolean;
begin
  Result := Master.TryPause;

  if not Result then
    ShowDialog(sCantPauseTitle, sCantPause, dtError);
end;

{*
  Red�marre le jeu
  Resume doit �tre appel� pour red�marrer le jeu apr�s un TryPause r�ussi.
*}
procedure TFormMain.Resume;
begin
  Master.Resume;
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
  FoundObjects: TObjectList;
  I: Integer;
begin
  if MasterFile = nil then
    Exit;

  FoundObjects := TObjectList.Create(False);
  try
    Controller.Player.GetFoundObjects(FoundObjects);

    for I := StatusBar.Panels.Count to FoundObjects.Count-1 do
      StatusBar.Panels.Add;

    for I := 0 to FoundObjects.Count-1 do
    begin
      with StatusBar.Panels[I] do
      begin
        Text := TObjectDef(FoundObjects[I]).ShownInfos[Controller.Player];
        Width := StatusBar.Canvas.TextWidth(Text) + 4;
      end;
    end;
  finally
    FoundObjects.Free;
  end;
end;

{*
  Fait un screenshot du jeu
  @param SaveProc   M�thode d'enregistrement du screenshot
*}
procedure TFormMain.MakeScreenshot(const SaveProc: TSaveScreenshotProc);
var
  Screenshot: TBitmap32;
begin
  if Controller = nil then
    Exit;

  Screenshot := TBitmap32.Create;
  try
    Screenshot.SetSize(Controller.ViewWidth, Controller.ViewHeight);
    Controller.DrawView(Screenshot);
    SaveProc(Screenshot);
  finally
    Screenshot.Free;
  end;
end;

{*
  Enregistre un bitmap 32 dans un fichier .png
  @param Bitmap32   Bitmap32 � enregistrer
  @param FileName   Nom du fichier .png
*}
procedure TFormMain.SaveBitmap32ToPNGFile(Bitmap32: TBitmap32;
  const FileName: TFileName);
var
  Bitmap: TBitmap;
  Image: TPngImage;
begin
  Bitmap := nil;
  Image := nil;
  try
    Bitmap := TBitmap.Create;
    Bitmap.Assign(Bitmap32);

    Image := TPngImage.Create;
    Image.Assign(Bitmap);
    Image.SaveToFile(FileName);
  finally
    Bitmap.Free;
    Image.Free;
  end;
end;

{*
  Demande � l'utilisateur s'il veut jouer au didacticiel, si n�cessaire
*}
procedure TFormMain.AskForTutorialIfNeeded;
const {don't localize}
  Path = 'game/tutorialdone';
  TutorialDir = 'Didacticiel';
  TutorialFileName = 'Didacticiel.flp';
var
  AlreadyDone: Boolean;
  Answer: TDialogResult;
begin
  if MasterFile <> nil then
    Exit;

  AlreadyDone := OptionsStorage.ReadBoolean(Path, False);
  if AlreadyDone then
    Exit;

  Answer := ShowDialog(SAskForTutorialTitle, SAskForTutorial, dtConfirmation,
    dbYesNoCancel);

  OptionsStorage.WriteBoolean(Path, Answer <> drNo);

  if Answer = drYes then
    NewGame(JoinPath([ProjectsPath, TutorialDir, TutorialFileName]));
end;

{*
  Gestionnaire d'�v�nement OnCreate
  @param Sender   Object qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  Menu := BigMenu;
  NewGameDialog.InitialDir := ProjectsPath;
  LoadGameDialog.InitialDir := SaveguardsPath;
  SaveGameDialog.InitialDir := SaveguardsPath;

  NewGameDialog2 := TFormSelectProjectFile.Create(Self);

  BackgroundTasks := TScTaskQueue.Create(True, False);

  BaseSepiRoot := TSepiRoot.Create;
  BaseSepiRootLoadTask := TScMethodTask.Create(BackgroundTasks,
    LoadBaseSepiRoot, False);

  AdaptSizeToView;

  DoubleBuffered := True;

  if ParamCount > 0 then
    NewGame(ParamStr(1));

  AskForTutorialIfNeeded;
end;

{*
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Object qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
var
  TriesLeft: Integer;
begin
  CloseGame(True);

  NeedBaseSepiRoot;

  BackgroundDiscard(BaseSepiRoot);
  BackgroundDiscard(BaseSepiRootLoadTask);

  TriesLeft := 20;
  while not BackgroundTasks.Ready do
  begin
    Sleep(100);
    Dec(TriesLeft);
    if TriesLeft <= 0 then
      System.Halt(2);
  end;

  BackgroundTasks.Free;
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
    if not NewGameDialog2.Execute then
      Exit;

    {if ofExtensionDifferent in NewGameDialog.Options then
      RunURL(NewGameDialog.FileName)
    else}
      NewGame(NewGameDialog2.FileName);
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
  RunURL(Application.HelpFile);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Mise � jour automatique
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuVersionCheckClick(Sender: TObject);
begin
  EditVersionCheckOptions;
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
  ShowDialog(SDescriptionTitle, MasterFile.Description);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Objets du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuPlayerObjectsClick(Sender: TObject);
begin
  TFormObjects.ShowObjects(Controller.Player);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Recommencer
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuReloadGameClick(Sender: TObject);
begin
  if CloseGame(True) then
    NewGame(LastFileName);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Gestionnaire de projets
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuOpenProjectManagerClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(Dir+'ProjectManager.exe'),
    nil, nil, SW_SHOWNORMAL);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Screenshot vers presse-papier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuScreenshotToClipboardClick(Sender: TObject);
begin
  MakeScreenshot(
    procedure(Screenshot: TBitmap32)
    begin
      Clipboard.Assign(Screenshot);
    end);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Screenshot vers fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuScreenshotToFileClick(Sender: TObject);
begin
  MakeScreenshot(
    procedure(Screenshot: TBitmap32)
    begin
      if SaveScreenshotDialog.Execute then
        SaveBitmap32ToPNGFile(Screenshot, SaveScreenshotDialog.FileName);
    end);
end;

{*
  Gestionnaire d'�v�nement OnClick du menu Screenshot rapide
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.MenuScreenshotToFileAutoClick(Sender: TObject);
var
  DirName, FileName: TFileName;
  I: Integer;
begin
  if MasterFile = nil then
    Exit;

  DirName := JoinPath([MasterFile.ProjectDir, ScreenshotsDir]);

  ForceDirectories(DirName);

  I := 1;
  while True do
  begin
    FileName := Format('%s'+PathDelim+'Screenshot%d.png', [DirName, I]);
    if not FileExists(FileName) then
      Break;
    Inc(I);
  end;

  MakeScreenshot(
    procedure(Screenshot: TBitmap32)
    begin
      SaveBitmap32ToPNGFile(Screenshot, FileName);
    end);
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
  Gestionnaire d'�v�nement OnKeyDown
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Key      Touche enfonc�e
  @param Shift    �tat des touches syst�me
*}
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_PAUSE then
  begin
    if Master.Paused then
      Resume
    else
      TryPause;
  end else
  begin
    Controller.PressKey(Key, Shift);
  end;
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

  if (PaintBox.Width <> Controller.ViewWidth) or
    (PaintBox.Height <> Controller.ViewHeight) then
  begin
    AdaptSizeToView;
  end;

  PaintBox.Invalidate;
  ShowStatus;
end;

{*
  Gestionnaire d'�v�nement OnPaintBuffer de la paint box
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.PaintBoxPaintBuffer(Sender: TObject);
begin
  if Assigned(Controller) then
  begin
    if (PaintBox.Width <> Controller.ViewWidth) or
      (PaintBox.Height <> Controller.ViewHeight) then
      Exit;

    Controller.DrawView(PaintBox.Buffer);
  end else
    PaintBox.Buffer.Clear;
end;

initialization
  TPicture.UnregisterGraphicClass(TPngImage);
end.

