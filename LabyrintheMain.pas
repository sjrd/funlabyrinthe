{*
  Unité principale de Labyrinthe.exe
  Cette unité contient la fiche principale de Labyrinthe.exe.
*}
unit LabyrintheMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ScUtils, ScStrUtils,
  SdDialogs, ShellAPI, FunLabyUtils, PlayerView, FilesUtils, MapFiles;

resourcestring
  sBuoyCount = '%d bouée(s)';
  sPlankCount = '%d planche(s)';
  sSilverKeyCount = '%d clef(s) d''argent';
  sGoldenKeyCount = '%d clef(s) d''or';

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
    { Déclarations privées }
    HiddenBitmap : TBitmap;
    LastFileName : TFileName;

    MasterFile : TMasterFile;
    Master : TMaster;
    View : TPlayerView;

    procedure NewGame(FileName : TFileName);
    procedure EndGame;
    function SaveGame : boolean;

    procedure AdaptSizeToView;
    procedure ShowStatus;
  public
    { Déclarations publiques }
  end;

var
  FormMain : TFormMain; /// Instance de la fiche principale

implementation

{$R *.DFM}

uses
  LiftDialog, PropertiesDialog;

////////////////////////
/// Classe TFormMain ///
////////////////////////

{*
  Commence une nouvelle partie
  @param FileName   Nom du fichier maître à charger
*}
procedure TFormMain.NewGame(FileName : TFileName);
begin
  if MasterFile <> nil then
    EndGame;

  LastFileName := FileName;
  MasterFile := TMasterFile.Create(FileName, fmPlay);
  Master := MasterFile.Master;
  View := TPlayerView.Create(Master.Players[0]);

  Caption := MasterFile.Title;
  MenuReloadGame.Enabled := True;
  MenuSaveGame.Enabled := True;
  MenuDescription.Enabled := True;
  MenuProperties.Enabled := True;
  MenuTips.Checked := False;
  ShowStatus;

  AdaptSizeToView;
  View.Draw(HiddenBitmap.Canvas);
  Image.Picture.Assign(HiddenBitmap);

  OnKeyDown := MovePlayer;
end;

{*
  Termine la partie et libère les classes métier
*}
procedure TFormMain.EndGame;
begin
  MenuSaveGame.Enabled := False;
  MenuDescription.Enabled := False;
  MenuProperties.Enabled := False;

  OnKeyDown := nil;

  View.Free;
  MasterFile.Free;

  View := nil;
  Master := nil;
  MasterFile := nil;
end;

{*
  Enregistre la partie en cours
  @return True si la partie a effectivement été enregistrée, False sinon
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
  Adapte la taille de HiddenBitmap et de la fenêtre à la taille de la vue
*}
procedure TFormMain.AdaptSizeToView;
var ImgSize : integer;
begin
  if View = nil then ImgSize := 270 else
    ImgSize := View.Size * ScrewSize;

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
begin
  if MasterFile = nil then exit;
  StatusBar.Panels[0].Text := Format(sBuoyCount, [0]);
  StatusBar.Panels[1].Text := Format(sPlankCount, [0]);
  StatusBar.Panels[2].Text := Format(sSilverKeyCount, [0]);
  StatusBar.Panels[3].Text := Format(sGoldenKeyCount, [0]);
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
  Gestionnaire d'événement OnCloseQuery
  @param Sender     Objet qui a déclenché l'événement
  @param CanClose   À position à False pour empêcher la fermeture
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
  Gestionnaire d'événement OnKeyDown, qui déplace le joueur
  @param Sender   Objet qui a déclenché l'événement
  @param Key      Touche enfoncée
  @param Shift    État des touches système
*}
procedure TFormMain.MovePlayer(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Dir : TDirection;
    KeyPressed, Redo : boolean;
begin
  if MasterFile = nil then exit;
  case Key of
    VK_UP    : Dir := diNorth;
    VK_RIGHT : Dir := diEast;
    VK_DOWN  : Dir := diSouth;
    VK_LEFT  : Dir := diWest;
    else exit;
  end;

  KeyPressed := True;
  repeat
    View.Player.Move(Dir, KeyPressed, Redo);
    KeyPressed := False;

    View.Draw(HiddenBitmap.Canvas);
    Image.Picture.Assign(HiddenBitmap);

    if Redo then
    begin
      Dir := View.Player.Direction;
      Application.ProcessMessages;
      Sleep(500);
    end;
  until not Redo;
end;

{*
  Gestionnaire d'événement OnDestroy
  @param Sender   Object qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if MasterFile <> nil then
    EndGame;

  HiddenBitmap.Free;
end;

end.

