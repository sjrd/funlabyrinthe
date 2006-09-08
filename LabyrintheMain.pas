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
  sNewGame = 'Nouvelle partie';
  sLoadGame = 'Charger une partir';

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
    Barre: TStatusBar;
    BigMenu: TMainMenu;
    MenuFichier: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuEnregistrer: TMenuItem;
    MenuCharger: TMenuItem;
    Sep2: TMenuItem;
    MenuAide: TMenuItem;
    MenuRubrAide: TMenuItem;
    Sep3: TMenuItem;
    MenuAPropos: TMenuItem;
    Ouvrir: TOpenDialog;
    Sauver: TSaveDialog;
    Sep1: TMenuItem;
    MenuDescription: TMenuItem;
    MenuOptions: TMenuItem;
    MenuIndices: TMenuItem;
    MenuProprietes: TMenuItem;
    MenuPropLabyrinthe: TMenuItem;
    MenuPropJoueur: TMenuItem;
    MenuRecommencer: TMenuItem;
    AboutDialog: TSdAboutDialog;
    procedure FormDestroy(Sender: TObject);
    procedure MovePlayer(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure MenuNouveauClick(Sender: TObject);
    procedure MenuChargerClick(Sender: TObject);
    procedure MenuRubrAideClick(Sender: TObject);
    procedure MenuAProposClick(Sender: TObject);
    procedure MenuEnregistrerClick(Sender: TObject);
    procedure MenuDescriptionClick(Sender: TObject);
    procedure MenuPropLabyrintheClick(Sender: TObject);
    procedure MenuPropJoueurClick(Sender: TObject);
    procedure MenuIndicesClick(Sender: TObject);
    procedure MenuRecommencerClick(Sender: TObject);
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

  MasterFile := TMasterFile.Create(FileName, fmPlay);
  Master := MasterFile.Master;
  View := TPlayerView.Create(Master.Players[0]);

  Caption := MasterFile.Title;
  MenuEnregistrer.Enabled := True;
  MenuDescription.Enabled := True;
  MenuProprietes.Enabled := True;
  MenuIndices.Checked := False;
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
  MenuEnregistrer.Enabled := False;
  MenuDescription.Enabled := False;
  MenuProprietes.Enabled := False;

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
    Sauver.FileName := '';
    Sauver.InitialDir := fSaveguardsDir;
    Result := Sauver.Execute;

    if Result then
      MasterFile.Save(Sauver.FileName);
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
  Barre.Panels[0].Text := Format(sBuoyCount, [0]);
  Barre.Panels[1].Text := Format(sPlankCount, [0]);
  Barre.Panels[2].Text := Format(sSilverKeyCount, [0]);
  Barre.Panels[3].Text := Format(sGoldenKeyCount, [0]);
end;

{*
  Gestionnaire d'événement OnCreate
  @param Sender   Object qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  Menu := BigMenu;

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

procedure TFormMain.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuNouveauClick(Sender: TObject);
begin
  Ouvrir.Title := sNewGame;
  Ouvrir.InitialDir := fLabyrinthsDir;

  if not Ouvrir.Execute then exit;
  if ofExtensionDifferent in Ouvrir.Options then
    RunURL(Ouvrir.FileName)
  else
    NewGame(Ouvrir.FileName);
end;

procedure TFormMain.MenuChargerClick(Sender: TObject);
begin
  Ouvrir.Title := sLoadGame;
  Ouvrir.InitialDir := fSaveguardsDir;

  if not Ouvrir.Execute then exit;
  if ofExtensionDifferent in Ouvrir.Options then
    RunURL(Ouvrir.FileName)
  else
    NewGame(Ouvrir.FileName);
end;

procedure TFormMain.MenuRubrAideClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormMain.MenuAProposClick(Sender: TObject);
begin
  AboutDialog.Execute;
end;

procedure TFormMain.MenuEnregistrerClick(Sender: TObject);
begin
  SaveGame;
end;

procedure TFormMain.MenuDescriptionClick(Sender: TObject);
begin
  ShowDialog(sDescription, MasterFile.Description);
end;

procedure TFormMain.MenuPropLabyrintheClick(Sender: TObject);
begin
  FormProperties.ShowMapProps(View.Player.Map);
end;

procedure TFormMain.MenuPropJoueurClick(Sender: TObject);
begin
  FormProperties.ShowPlayerProps(View.Player);
end;

procedure TFormMain.MenuIndicesClick(Sender: TObject);
begin
  MenuIndices.Checked := not MenuIndices.Checked;
end;

procedure TFormMain.MenuRecommencerClick(Sender: TObject);
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

