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
  ScUtils, ScStrUtils, ScSyncObjs, SdDialogs,
  FunLabyUtils, PlayUtils, FilesUtils, PlayerObjects, SepiReflectionCore,
  UnitFiles, SepiImportsFunLaby, SepiImportsFunLabyTools, FunLabyCoreConsts,
  GR32_Image;

resourcestring
  sFatalErrorTitle = 'Erreur fatale';

  sBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalit�s coeur de FunLabyrinthe avec le '+
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
    BackgroundTasks: TScTaskQueue; /// T�ches d'arri�re-plan

    BaseSepiRoot: TSepiRoot;       /// Racine Sepi de base
    BaseSepiRootLoadTask: TScTask; /// T�che de chargement de la racine de base

    MasterFile: TMasterFile;       /// Fichier ma�tre
    Master: TMaster;               /// Ma�tre FunLabyrinthe
    Controller: TPlayerController; /// Contr�leur du joueur
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
  Gestionnaire d'�v�nement OnCreate
  @param Sender   Object qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Object qui a d�clench� l'�v�nement
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
  ShowDialog(SDescription, MasterFile.Description);
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

end.

