{*
  Unit� principale de FunLabyEdit.exe
  Cette unit� contient la fiche principale de FunLabyEdit.exe.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit FunLabyEditMain;

interface

uses
  Windows, SysUtils, Forms, Dialogs, Classes, ActnList, XPStyleActnCtrls,
  ActnMan, ImgList, Controls, MapEditor, ComCtrls, ActnMenus, ToolWin,
  ActnCtrls, ShellAPI, ScUtils, SdDialogs, SepiMetaUnits, FunLabyUtils,
  FilesUtils, UnitFiles, FileProperties, FunLabyEditConsts;

type
  {*
    Fiche principale de FunLabyEdit.exe
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TFormMain = class(TForm)
    Images: TImageList;
    ActionManager: TActionManager;
    ActionExit: TAction;
    ToolBarFile: TActionToolBar;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    ScrewsImages: TImageList;
    ActionOpenFile: TAction;
    ActionSaveFile: TAction;
    ActionSaveFileAs: TAction;
    ActionCloseFile: TAction;
    ActionNewFile: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionFileProperties: TAction;
    SaveMapDialog: TSaveDialog;
    ActionAddMap: TAction;
    ActionRemoveMap: TAction;
    ActionHelpTopics: TAction;
    ActionAbout: TAction;
    ActionTest: TAction;
    FrameMapEditor: TFrameMapEditor;
    procedure ActionTestExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionSaveFileExecute(Sender: TObject);
    procedure ActionSaveFileAsExecute(Sender: TObject);
    procedure ActionCloseFileExecute(Sender: TObject);
    procedure ActionFilePropertiesExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAddMapExecute(Sender: TObject);
    procedure ActionRemoveMapExecute(Sender: TObject);
    procedure ActionHelpTopicsExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
  private
    { D�clarations priv�es }
    /// Manager asynchrone de la racine Sepi
    SepiRootManager : TSepiAsynchronousRootManager;
    SepiRoot : TSepiMetaRoot; /// Racine Sepi

    Modified : boolean;       /// Indique si le fichier a �t� modifi�
    MasterFile : TMasterFile; /// Fichier ma�tre
    Master : TMaster;         /// Ma�tre FunLabyrinthe

    procedure LoadFile;
    procedure UnloadFile;

    procedure NewFile;
    procedure OpenFile(const FileName : TFileName);
    function SaveFile(FileName : TFileName = '') : boolean;
    function CloseFile : boolean;

    procedure MarkModified;
  public
    { D�clarations publiques }
  end;

const {don't localize}
  FunLabyBaseHRef = 'FunLabyBase.bpl'; /// HRef de l'unit� FunLabyCore
  idPlayer = 'Player';                 /// ID de l'unique joueur

var
  FormMain: TFormMain; /// Instance de la fiche principale

implementation

{$R *.dfm}

{------------------}
{ Classe TFormMain }
{------------------}

{*
  Charge le MasterFile cr��
  Charge le MasterFile cr�� dans les autres variables et dans l'interface
  graphique
*}
procedure TFormMain.LoadFile;
begin
  // Un fichier nouvellement charg� n'est modifi� que s'il vient d'�tre cr��
  Modified := MasterFile.FileName = '';

  // Autres variables
  Master := MasterFile.Master;

  // Activation de l'interface utilisateur
  ActionSaveFile.Enabled := True;
  ActionSaveFileAs.Enabled := True;
  ActionCloseFile.Enabled := True;
  ActionFileProperties.Enabled := True;
  ActionTest.Enabled := True;

  MainMenuBar.ActionClient.Items[1].Visible := True; // menu des cartes
  ActionAddMap.Enabled := True;

  // Chargement des cartes
  FrameMapEditor.LoadFile(SepiRoot, MasterFile);
end;

{*
  D�charge le MasterFile
  D�charge le MasterFile de l'interface graphique
*}
procedure TFormMain.UnloadFile;
begin
  // D�chargement des cartes
  FrameMapEditor.UnloadFile;

  // D�sactivation de l'interface utilisateur
  ActionSaveFile.Enabled := False;
  ActionSaveFileAs.Enabled := False;
  ActionCloseFile.Enabled := False;
  ActionFileProperties.Enabled := False;
  ActionTest.Enabled := False;

  MainMenuBar.ActionClient.Items[1].Visible := False; // menu des cartes
  ActionAddMap.Enabled := False;

  // Autres variables
  Master := nil;
end;

{*
  Cr�e un nouveau fichier et le charge
*}
procedure TFormMain.NewFile;
begin
  MasterFile := TMasterFile.CreateNew(SepiRoot);
  if TFormFileProperties.ManageProperties(MasterFile) then
  begin
    MasterFile.AddUnitFile(BPLUnitHandlerGUID, FunLabyBaseHRef);
    TPlayer.Create(MasterFile.Master, idPlayer, sDefaultPlayerName,
      nil, Point3D(0, 0, 0));
    LoadFile;
  end else
  begin
    MasterFile.Free;
    MasterFile := nil;
  end;
end;

{*
  Ouvre un fichier et le charge
  @param FileName   Nom du fichier ma�tre � charger
*}
procedure TFormMain.OpenFile(const FileName : TFileName);
begin
  MasterFile := TMasterFile.Create(SepiRoot, FileName, fmEdit);
  LoadFile;
end;

{*
  Enregistre le fichier
  @param FileName   Fichier dans lequel enregistrer, ou vide pour demander
  @return True si l'enregistrement a bien �t� effectu�, False sinon
*}
function TFormMain.SaveFile(FileName : TFileName = '') : boolean;
var DirName : TFileName;
    I : integer;
    MapFile : TMapFile;
    MapFileName : TFileName;
    MapHRef : string;
begin
  Result := False;

  for I := 0 to Master.PlayerCount-1 do with Master.Players[I] do
  begin
    if Map = nil then
    begin
      ShowDialog(sCantSave, Format(sCantSaveUnplacedPlayer, [ID]), dtError);
      exit;
    end;
  end;

  if FileName = '' then
  begin
    if not SaveDialog.Execute then exit;
    OpenDialog.FileName := SaveDialog.FileName;
    FileName := SaveDialog.FileName;
  end;
  DirName := ExtractFilePath(FileName);

  for I := 0 to MasterFile.MapFileCount-1 do
  begin
    MapFile := MasterFile.MapFiles[I];
    MapFileName := MapFile.FileName;

    if MapFileName = '' then
    begin
      SaveMapDialog.FileName := MapFile.MapID;
      if not SaveMapDialog.Execute then exit;
      MapFileName := SaveMapDialog.FileName;
    end;

    if AnsiSameText(Copy(MapFileName, 1, Length(DirName)), DirName) then
      MapHRef := Copy(MapFileName, Length(DirName)+1, Length(MapFileName)) else
    if AnsiSameText(Copy(MapFileName, 1, Length(fMapsDir)), fMapsDir) then
      MapHRef := Copy(MapFileName, Length(fMapsDir)+1, Length(MapFileName)) else
    MapHRef := MapFileName;

    MapFile.Save(MapHRef, MapFileName);
  end;

  MasterFile.Save(FileName);
  Result := True;
  Modified := False;
end;

{*
  Ferme le fichier
  @return True si le fichier a bien �t� ferm�, False sinon
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

{*
  Marque le fichier comme modifi�
*}
procedure TFormMain.MarkModified;
begin
  Modified := True;
end;

{*
  Gestionnaire d'�v�nement OnCreate
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  SepiRootManager := TSepiAsynchronousRootManager.Create;
  SepiRootManager.LoadUnit('FunLabyUtils');
  SepiRoot := SepiRootManager.Root;

  OpenDialog.InitialDir := fLabyrinthsDir;
  SaveDialog.InitialDir := fLabyrinthsDir;

  MasterFile := nil;

  FrameMapEditor.MarkModified := MarkModified;

  if ParamCount > 0 then
    OpenFile(ParamStr(1));
end;

{*
  Gestionnaire d'�v�nement OnDestroy
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SepiRootManager.Free;
end;

{*
  Gestionnaire d'�v�nement OnCloseQuery
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param CanClose   Indique si la fen�tre peut �tre ferm�e
*}
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Nouveau fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionNewFileExecute(Sender: TObject);
begin
  if CloseFile then NewFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Ouvrir un fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionOpenFileExecute(Sender: TObject);
begin
  if not CloseFile then exit;

  if OpenDialog.Execute then
  begin
    SaveDialog.FileName := OpenDialog.FileName;
    OpenFile(OpenDialog.FileName);
  end;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Enregistrer
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionSaveFileExecute(Sender: TObject);
begin
  SaveFile(MasterFile.FileName);
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Enregistrer sous
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionSaveFileAsExecute(Sender: TObject);
begin
  SaveFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Fermer
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionCloseFileExecute(Sender: TObject);
begin
  CloseFile;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Propri�t�s du fichier
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionFilePropertiesExecute(Sender: TObject);
begin
  if TFormFileProperties.ManageProperties(MasterFile) then
    MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Tester
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionTestExecute(Sender: TObject);
begin
  {don't localize}
  if (not Modified) or SaveFile(MasterFile.FileName) then
    ShellExecute(0, 'open', PChar(Dir+'Labyrinthe.exe'),
      PChar('"'+MasterFile.FileName+'"'), nil, SW_SHOWNORMAL);
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Quitter
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Ajouter une carte
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAddMapExecute(Sender: TObject);
begin
  FrameMapEditor.AddMap;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Retirer une carte
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionRemoveMapExecute(Sender: TObject);
begin
  FrameMapEditor.RemoveCurrentMap;
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action Rubriques d'aide
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionHelpTopicsExecute(Sender: TObject);
begin
//
end;

{*
  Gestionnaire d'�v�nement OnExecute de l'action � propos...
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  ShowFunLabyAbout;
end;

end.

