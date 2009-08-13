{*
  Unité principale de FunLaby.exe
  Cette unité contient la fiche principale de FunLaby.exe.
  @author sjrd
  @version 5.0
*}
unit FunLabyMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ShellAPI,
  ScUtils, ScStrUtils, ScSyncObjs, SdDialogs,
  FunLabyUtils, PlayUtils, FilesUtils, PlayerObjects, SepiReflectionCore,
  UnitFiles, SepiImportsFunLaby, SepiImportsFunLabyTools, FunLabyCoreConsts,
  GR32_Image;

resourcestring
  sFatalErrorTitle = 'Erreur fatale';

  sBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalités coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

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
    MenuPlayerObjects: TMenuItem;
    MenuReloadGame: TMenuItem;
    LoadGameDialog: TOpenDialog;
    TimerUpdateImage: TTimer;
    MenuViewSize: TMenuItem;
    PaintBox: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuViewSizeClick(Sender: TObject);
    procedure UpdateImage(Sender: TObject);
    procedure MovePlayer(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuNewGameClick(Sender: TObject);
    procedure MenuLoadGameClick(Sender: TObject);
    procedure MenuHelpTopicsClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuSaveGameClick(Sender: TObject);
    procedure MenuDescriptionClick(Sender: TObject);
    procedure MenuPlayerObjectsClick(Sender: TObject);
    procedure MenuReloadGameClick(Sender: TObject);
    procedure PaintBoxPaintBuffer(Sender: TObject);
  private
    BackgroundTasks: TScTaskQueue; /// Tâches d'arrière-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// Tâche de chargement de la racine de base

    MasterFile: TMasterFile;       /// Fichier maître
    Master: TMaster;               /// Maître FunLabyrinthe
    Controller: TPlayerController; /// Contrôleur du joueur
    GameEnded: Boolean;            /// Indique si la partie est finie
    LastFileName: TFileName;       /// Dernier nom de fichier ouvert

    procedure LoadBaseSepiRoot;
    procedure NeedBaseSepiRoot;
    procedure BackgroundDiscard(var Obj);

    procedure NewGame(FileName: TFileName);
    function SaveGame: Boolean;
    function CloseGame(DontSave: Boolean = False): Boolean;

    procedure AdaptSizeToView;
    procedure ShowStatus;
  end;

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.DFM}

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Charge la racine Sepi de base
  Cette méthode est appelée dans le thread des tâches en arrière-plan
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
  Attend que la racine Sepi de base soit chargée, et vérifie l'état d'erreur
  En cas d'erreur pendant le chargement de la racine de base, un message
  avertit l'utilisateur et le programme est arrêté brutalement.
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
  Crée une nouvelle tâche d'arrière-plan de libération d'un objet
  @param Obj   Objet à libérer en arrière-plan (mis à nil)
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
  @param FileName   Nom du fichier maître à charger
*}
procedure TFormMain.NewGame(FileName: TFileName);
begin
  NeedBaseSepiRoot;

  MasterFile := TMasterFile.Create(BaseSepiRoot, FileName, fmPlay);
  Master := MasterFile.Master;
  Controller := TPlayerController.Create(Master.Players[0]);
  GameEnded := False;
  LastFileName := FileName;

  Caption := MasterFile.Title;
  MenuReloadGame.Enabled := True;
  MenuSaveGame.Enabled := True;
  MenuDescription.Enabled := True;
  MenuPlayerObjects.Enabled := True;
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
  @return True si la partie a effectivement été enregistrée, False sinon
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
  Termine la partie et libère les classes métier
  @param DontSave   À True, ne demande pas à l'utilisateur d'enregistrer
  @return True si la partie a effectivement été terminée, False sinon
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
  MenuPlayerObjects.Enabled := False;
  MenuViewSize.Enabled := False;

  StatusBar.Panels.Clear;

  Controller.Free;
  MasterFile.Free;

  Controller := nil;
  Master := nil;
  MasterFile := nil;

  PaintBox.Invalidate;
end;

{*
  Adapte la taille de la fenêtre à la taille de la vue
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
  Gestionnaire d'événement OnCreate
  @param Sender   Object qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  Menu := BigMenu;
  NewGameDialog.InitialDir := fLabyrinthsDir;
  LoadGameDialog.InitialDir := fSaveguardsDir;
  SaveGameDialog.InitialDir := fSaveguardsDir;

  BackgroundTasks := TScTaskQueue.Create(True, False);

  BaseSepiRoot := TSepiRoot.Create;
  BaseSepiRootLoadTask := TScMethodTask.Create(BackgroundTasks,
    LoadBaseSepiRoot, False);

  AdaptSizeToView;

  DoubleBuffered := True;

  if ParamCount > 0 then
    NewGame(ParamStr(1));
end;

{*
  Gestionnaire d'événement OnDestroy
  @param Sender   Object qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
var
  TriesLeft: Integer;
begin
  CloseGame(True);

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
  Gestionnaire d'événement OnClick du menu Quitter
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close;
end;

{*
  Gestionnaire d'événement OnClick du menu Nouvelle partie
  @param Sender   Objet qui a déclenché l'événement
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
  Gestionnaire d'événement OnClick du menu Charger
  @param Sender   Objet qui a déclenché l'événement
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
  Gestionnaire d'événement OnClick du menu Rubriques d'aide
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuHelpTopicsClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

{*
  Gestionnaire d'événement OnClick du menu À propos
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuAboutClick(Sender: TObject);
begin
  ShowFunLabyAbout;
end;

{*
  Gestionnaire d'événement OnClick du menu Sauver
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuSaveGameClick(Sender: TObject);
begin
  SaveGame;
end;

{*
  Gestionnaire d'événement OnClick du menu Description
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuDescriptionClick(Sender: TObject);
begin
  ShowDialog(SDescription, MasterFile.Description);
end;

{*
  Gestionnaire d'événement OnClick du menu Objets du joueur
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuPlayerObjectsClick(Sender: TObject);
begin
  TFormObjects.ShowObjects(Controller.Player);
end;

{*
  Gestionnaire d'événement OnClick du menu Recommencer
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuReloadGameClick(Sender: TObject);
begin
  CloseGame(True);
  NewGame(LastFileName);
end;

{*
  Gestionnaire d'événement OnCloseQuery
  @param Sender     Objet qui a déclenché l'événement
  @param CanClose   À position à False pour empêcher la fermeture
*}
procedure TFormMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := CloseGame;
end;

{*
  Gestionnaire d'événement OnKeyDown, qui déplace le joueur
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Touche enfoncée
  @param Shift    État des touches système
*}
procedure TFormMain.MovePlayer(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Controller.PressKey(Key, Shift);
end;

{*
  Gestionnaire d'événement OnTimer
  @param Sender   Objet qui a déclenché l'événement
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
  Gestionnaire du menu 'Taille de la vue'
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.MenuViewSizeClick(Sender: TObject);
begin
  with Controller.Player do
    ViewBorderSize := QueryNumber(sViewSize, sViewSizePrompt,
      ViewBorderSize, MinViewSize, Map.MaxViewSize);

  AdaptSizeToView;
end;

{*
  Gestionnaire d'événement OnPaintBuffer de la paint box
  @param Sender   Objet qui a déclenché l'événement
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

end.

