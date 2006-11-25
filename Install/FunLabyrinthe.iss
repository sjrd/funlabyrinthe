; Installation de FunLabyrinthe 5.0

[Setup]
AppID=FunLabyrinthe
AppName={cm:AppName}
AppVerName={cm:AppVerName}
AppComments={cm:Description}
AppContact=sjrd@redaction-developpez.com
AppPublisher=SJRDoeraene
AppPublisherURL=http://sjrd.developpez.com/programmes/funlaby/
AppSupportURL=http://sjrd.developpez.com/programmes/funlaby/
AppUpdatesURL=http://sjrd.developpez.com/programmes/funlaby/
AppVersion=5.0

VersionInfoDescription=Setup of FunLabyrinthe 5.0
VersionInfoTextVersion=FunLabyrinthe v5.0, copyright 2000-2006 SJRDoeraene
VersionInfoVersion=5.0

DefaultDirName={reg:HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyOld_is1\,InstallLocation|{pf}\SJRDoeraene\{cm:AppName}}
DefaultGroupName={reg:HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyOld_is1\,Inno Setup: Icon Group|{cm:AppName}}

OutputDir=.\
OutputBaseFilename=funlabyrinthe_5_0

Compression=lzma
SolidCompression=yes

[Languages]
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"

[CustomMessages]
fr.AppName=FunLabyrinthe
fr.AppVerName=FunLabyrinthe 5.0
fr.Description=Jeu de labyrinthe très fun avec toutes sortes de gadgets

; Types d'installation

fr.FullInstall=Installation complète
fr.PlayOnlyInstall=Installation légère, sans les programmes d'édition
fr.CustomInstall=Installation personnalisée (recommandé pour les utilisateurs avancés)

; Nom des composants

fr.CompPrograms=Fichiers programme
fr.CompRuntime=Fichiers de runtime de FunLabyrinthe
fr.CompLabyrinthe=Labyrinthe, le programme de jeu
fr.CompGeneLaby=GeneLaby, un générateur de labyrinthes
fr.CompFunLabyEdit=FunLabyEdit, l'éditeur de labyrinthes

fr.CompHelp=Fichiers d'aide
fr.CompRules=Règles du jeu
fr.CompEditHelp=Aide de l'éditeur de labyrinthes
fr.CompMasteringEdit=Maîtriser l'édition de labyrinthes sous toutes ses formes

fr.CompAppData=Fichiers de labyrinthe, cartes, sons, graphismes, etc.

; Texte des raccourcis

fr.IconLabyrinthe=Joueur à FunLabyrinthe
fr.IconGeneLaby=Générateur de labyrinthes
fr.IconFunLabyEdit=Éditeur de labyrinthes

; Tâches

fr.AdditionalTasks=Tâches supplémentaires
fr.RegExtensions=Enregistrer les extensions de fichier liées à FunLabyrinthe
fr.RegFLP=Fichiers projet (.flp)
fr.RegFLM=Fichiers carte (.flm)

; Enregistrement des extensions

fr.FunLabyProject=Projet FunLabyrinthe
fr.Open=&Ouvrir
fr.Edit=&Éditer

; Composants génériques

fr.PleaseSelectDir=Veuillez sélectionner un répertoire

; Page de sélection des répertoires d'installation

fr.AppDataDirPrompt=Les labyrinthes, graphismes, sons, etc. seront enregistrés dans le dossier suivant :
fr.AppDataSelectDirPrompt=Choisissez le répertoire où enregistrer les labyrinthes

; Pages d'import d'anciens labyrinthes

fr.PageImportOldTitle=Importer les labyrinthes depuis une ancienne version
fr.PageImportOldDescription=Vous pouvez importer les labyrinthes que vous aviez créés avec une ancienne version de {cm:AppName}.

fr.OldVersionInstalled=Une ancienne version de {cm:AppName} est installée sur votre ordinateur (v{code:oldversioninfo|version}), dans le dossier suivant :
fr.OldVersionWillBeUninstalled=Celle-ci sera automatiquement désinstallée avant d'installer {cm:AppVerName}.%nCependant, vous pouvez demander à {cm:AppVerName} d'importer automatiquement les labyrinthes que vous aviez créés avec cette ancienne version.

fr.ImportOldLabyrinthsPrompt=Si vous aviez installé précédemment une ancienne version de {cm:AppName}, puis désinstallée, vous avez peut-être conservé les labyrinthes que vous aviez créés. Si c'est le cas, il vous est maintenant possible d'importer ces anciens labyrinthes dans {cm:AppVerName}.

fr.ImportOldLabyrinths=Importer automatiquement mes anciens labyrinthes

fr.OldVersionInstallDirPrompt=Veuillez sélectionner l'ancien répertoire d'installation de {cm:AppName}, celui qui contient le dossier nommé "Labyrinthes", entre autres :
fr.SelectOldVersionInstallDirPrompt=Ancien répertoire d'installation de {cm:AppName}

fr.ImportInfos=Les labyrinthes pré-installés ne peuvent pas être importés : ils sont déjà inclus dans cette nouvelle installation, avec, pour la plupart, des améliorations. De même, les labyrinthes disponibles sur le site Web de {cm:AppName} ont tous déjà été importés : vous pouvez les retélécharger. L'importation sert uniquement pour les labyrinthes que VOUS avez créés.

[Types]
Name: "full"    ; Description: {cm:FullInstall}
Name: "playonly"; Description: {cm:PlayOnlyInstall}
Name: "custom"  ; Description: {cm:CustomInstall}  ; Flags: iscustom

[Components]
Name: "programs"            ; Description: {cm:CompPrograms}   ; Types: full playonly custom
Name: "programs\runtime"    ; Description: {cm:CompRuntime}    ; Types: full playonly custom; Flags: fixed
Name: "programs\labyrinthe" ; Description: {cm:CompLabyrinthe} ; Types: full playonly custom; Flags: fixed
Name: "programs\genelaby"   ; Description: {cm:CompGeneLaby}   ; Types: full playonly custom
Name: "programs\funlabyedit"; Description: {cm:CompFunLabyEdit}; Types: full custom

Name: "help"              ; Description: {cm:CompHelp}         ; Types: full playonly custom
Name: "help\rules"        ; Description: {cm:CompRules}        ; Types: full playonly custom
Name: "help\funlabyedit"  ; Description: {cm:CompEditHelp}     ; Types: full custom
Name: "help\masteringedit"; Description: {cm:CompMasteringEdit}; Types: full

Name: "appdata"; Description: {cm:CompAppData}; Types: full playonly custom; Flags: fixed

[Tasks]
Name: "desktopicon"            ; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "desktopicon\labyrinthe" ; Description: "{cm:IconLabyrinthe}" ; Components: programs\labyrinthe
Name: "desktopicon\genelaby"   ; Description: "{cm:IconGeneLaby}"   ; Components: programs\genelaby
Name: "desktopicon\funlabyedit"; Description: "{cm:IconFunLabyEdit}"; Components: programs\funlabyedit; Flags: unchecked

Name: "regext"    ; Description: "{cm:RegExtensions}"; GroupDescription: {cm:AdditionalTasks}
Name: "regext\flp"; Description: "{cm:RegFLP}"
Name: "regext\flm"; Description: "{cm:RegFLM}"

[Files]
Source: "..\Labyrinthe.exe";  DestDir: "{app}"; Components: programs\labyrinthe ; Flags: ignoreversion
Source: "..\GeneLaby.exe";    DestDir: "{app}"; Components: programs\genelaby   ; Flags: ignoreversion
Source: "..\FunLabyEdit.exe"; DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion

Source: "Runtime\FunLabyCore.bpl"               ; DestDir: "{sys}"; Attribs: system; Components: programs\runtime; Flags: replacesameversion uninsneveruninstall
Source: "Runtime\*"; Excludes: "FunLabyCore.bpl"; DestDir: "{sys}"; Attribs: system; Components: programs\runtime; Flags: sharedfile

Source: "AppData\*"; DestDir: "{code:appdata}"; Flags: sortfilesbyextension ignoreversion recursesubdirs createallsubdirs uninsneveruninstall

; Fichiers temporaires pour l'installation

Source: "Images\*"; flags: dontcopy

[INI]
Filename: "{app}\FunLabyrinthe.ini"; Section: "Directories"; Key: "AppData"; String: "{code:appdata}"

[Registry]
Root: HKCR; SubKey: ".flp"                                    ; ValueType: string; ValueName: ""; ValueData: "FunLabyrinthe.Project"           ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project"                   ; ValueType: string; ValueName: ""; ValueData: "{cm:FunLabyProject}"             ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\DefaultIcon"       ; ValueType: string; ValueName: ""; ValueData: "{app}\Labyrinthe.exe,0"          ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell"             ; ValueType: string; ValueName: ""; ValueData: "open,edit"                       ; Tasks: regext\flp; Components: not programs\funlabyedit; Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell"             ; ValueType: string; ValueName: ""; ValueData: "open,edit"                       ; Tasks: regext\flp; Components: programs\funlabyedit    ; Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\open"        ; ValueType: string; ValueName: ""; ValueData: "{cm:Open}"                       ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Labyrinthe.exe"" ""%1""" ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\edit"        ; ValueType: string; ValueName: ""; ValueData: "{cm:Edit}"                       ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: """{app}\FunLabyEdit.exe"" ""%1"""; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty

[Icons]
Name: "{group}\{cm:IconLabyrinthe}" ; Filename: "{app}\Labyrinthe.exe" ; Components: programs\labyrinthe
Name: "{group}\{cm:IconGeneLaby}"   ; Filename: "{app}\GeneLaby.exe"   ; Components: programs\genelaby
Name: "{group}\{cm:IconFunLabyEdit}"; Filename: "{app}\FunLabyEdit.exe"; Components: programs\funlabyedit

Name: "{commondesktop}\{cm:IconLabyrinthe}" ; Filename: "{app}\Labyrinthe.exe" ; Tasks: desktopicon\labyrinthe
Name: "{commondesktop}\{cm:IconGeneLaby}"   ; Filename: "{app}\GeneLaby.exe"   ; Tasks: desktopicon\genelaby
Name: "{commondesktop}\{cm:IconFunLabyEdit}"; Filename: "{app}\FunLabyEdit.exe"; Tasks: desktopicon\funlabyedit

[Run]
Filename: "{app}\Labyrinthe.exe"; Description: "{cm:LaunchProgram,FunLabyrinthe}"; Flags: nowait postinstall skipifsilent

[Code]
const
  OldVersionRegKey = 'Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyOld_is1\';
  
  InfoBitmapFileName = 'Info.bmp';

var
  InfoBitmap : TBitmap;
  
  HasOldVersion : boolean;
  OldVersionDisplayVersion : string;
  OldVersionInstallDir : string;
  OldVersionUninstallString : string;
  
  PageWidth : integer;
  DirEdits : TStrings;

  SelectDirPage : TNewNotebookPage;
  EditFunLabyAppData : TEdit;
  
  PageImportOld : TWizardPage;
  CheckBoxImportOld : TCheckBox;
  EditOldInstallDir : TEdit;
  
function FunLabyAppData : string;
begin
  Result := EditFunLabyAppData.Text;
end;

function appdata(Param : string) : string;
begin
  Result := FunLabyAppData;
end;

function oldversioninfo(Param : string) : string;
begin
  if Param = 'version' then
    Result := OldVersionDisplayVersion
  else if Param = 'installdir' then
    Result := OldVersionInstallDir
  else if Param = 'uninstallstring' then
    Result := OldVersionUninstallString
  else
    Result := '';
end;

function CheckValidAppData : boolean;
begin
  Result := True;
end;

procedure CheckOldVersion;
begin
  HasOldVersion := RegKeyExists(HKLM, OldVersionRegKey);
  OldVersionDisplayVersion := '';
  OldVersionInstallDir := '';
  OldVersionUninstallString := '';

  if HasOldVersion then
  begin
    RegQueryStringValue(HKLM, OldVersionRegKey, 'DisplayVersion', OldVersionDisplayVersion);
    RegQueryStringValue(HKLM, OldVersionRegKey, 'InstallLocation', OldVersionInstallDir);
    RegQueryStringValue(HKLM, OldVersionRegKey, 'QuietUninstallString', OldVersionUninstallString);
  end;
end;

function ExpandConstants(const Str : string) : string;
var Old : string;
begin
  Result := Str;
  repeat
    Old := Result;
    Result := ExpandConstant(Result);
  until Result = Old;
end;

function AddLabel(AParent : TWinControl; var Bottom : integer;
  const ACaption : string) : TNewStaticText;
begin
  Result := TNewStaticText.Create(AParent)
  with Result do
  begin
    WordWrap := True;
    Top := Bottom + 8;
    Width := PageWidth;
    Caption := ACaption;
    Parent := AParent;
    Bottom := Top + Height;
  end;
end;

function AddBitmapImage(AParent : TWinControl; var Bottom : integer;
  Image : TBitmap) : TBitmapImage;
begin
  Result := TBitmapImage.Create(AParent);
  with Result do
  begin
    Top := Bottom + 8;
    AutoSize := True;
    Bitmap := Image;
    ReplaceColor := clTeal;
    ReplaceWithColor := BackColor;
    Parent := AParent;
    Bottom := Top + Height;
  end;
end;

function AddImgText(AParent : TNewNotebookPage; var Bottom : integer;
  Image : TBitmap; const Text : string) : TNewStaticText;
var LabelBottom : integer;
begin
  LabelBottom := Bottom;

  AddBitmapImage(AParent, Bottom, Image);

  with AddLabel(AParent, LabelBottom, Text) do
  begin
    Left := Image.Width + 8;
    Width := Width - Left;
    Bottom := Top + Height;
  end;
  
  if LabelBottom > Bottom then
    Bottom := LabelBottom;
end;

function AddCheckBox(AParent : TNewNotebookPage; var Bottom : integer;
  const ACaption : string; AChecked : boolean) : TCheckBox;
begin
  Result := TCheckBox.Create(AParent);
  with Result do
  begin
    Top := Bottom + 8;
    Width := PageWidth;
    Caption := ACaption;
    Checked := AChecked;
    Parent := AParent;
    Bottom := Top + Height;
  end;
end;

function AddEdit(AParent : TNewNotebookPage; var Bottom : integer;
  const DefaultText : string) : TEdit;
begin
  Result := TEdit.Create(AParent);
  with Result do
  begin
    Left := 0;
    Top := Bottom + 8;
    Width := PageWidth;
    Text := DefaultText;
    Parent := AParent;
    Bottom := Top + Height;
  end;
end;

procedure BrowseForDirOfEdit(Sender : TObject);
var Index : integer;
    Edit : TEdit;
    Dir : string;
begin
  Index := TComponent(Sender).Tag;
  Edit := TEdit(DirEdits.Objects[Index]);
  
  Dir := Edit.Text;
  if BrowseForFolder(DirEdits[Index], Dir, False) then
    Edit.Text := Dir;
end;

function AddDirEdit(AParent : TNewNotebookPage; var Bottom : integer;
  const DefaultDir, Prompt : string) : TEdit;
var Index : integer;
begin
  Result := AddEdit(AParent, Bottom, DefaultDir);
  Result.Width := WizardForm.DirEdit.Width;
  
  with WizardForm, TButton.Create(AParent) do
  begin
    Left    := DirBrowseButton.Left;
    Top     := DirBrowseButton.Top + Result.Top - DirEdit.Top;
    Width   := DirBrowseButton.Width;
    Height  := DirBrowseButton.Height;
    Caption := DirBrowseButton.Caption;
    Parent := AParent;
    Tag := DirEdits.Add(Prompt);
    DirEdits.Objects[Tag] := Result;
    OnClick := @BrowseForDirOfEdit;
  end;
end;

procedure SetEditReadOnly(Edit : TEdit; ReadOnly : boolean);
var I : integer;
begin
  Edit.ReadOnly := ReadOnly;
  Edit.TabStop := not ReadOnly;
  if ReadOnly then
    Edit.Color := clBtnFace
  else
    Edit.Color := clWindow;

  // Méthode un peu bourrin pour retrouver l'éventuel bouton Parcourir
  for I := 0 to Edit.Owner.ComponentCount-2 do
  begin
    if Edit.Owner.Components[I] = Edit then
    begin
      if Edit.Owner.Components[I+1] is TButton then
        TButton(Edit.Owner.Components[I+1]).Enabled := not ReadOnly;
      Break;
    end;
  end;
end;

procedure AddFunLabyAppDataField;
var Bottom : integer;
begin
  // Ajouter un champ à la page de sélection de répertoire
  
  SelectDirPage := WizardForm.SelectDirPage;

  with WizardForm.DirEdit do
    Bottom := Top + Height + 8;

  AddLabel(SelectDirPage, Bottom,
    ExpandConstants('{cm:AppDataDirPrompt}'));

  EditFunLabyAppData := AddDirEdit(SelectDirPage, Bottom,
    GetPreviousData('FunLabyAppData',
      ExpandConstant('{commonappdata}\SJRDoeraene\FunLabyrinthe\')),
    ExpandConstants('{cm:AppDataSelectDirPrompt}'));
end;

procedure CheckBoxImportOldClick(Sender : TObject);
begin
  SetEditReadOnly(EditOldInstallDir, not CheckBoxImportOld.Checked);
end;

procedure AddImportOldPage;
var Surface : TNewNotebookPage;
    Bottom : integer;
begin
  // Ajouter la page d'import d'anciens labyrinthes

  PageImportOld := CreateCustomPage(wpSelectDir,
    ExpandConstants('{cm:PageImportOldTitle}'),
    ExpandConstants('{cm:PageImportOldDescription}'));
  Surface := PageImportOld.Surface;
  Bottom := -8;
  
  if HasOldVersion then
  begin
    AddLabel(Surface, Bottom,
      ExpandConstants('{cm:OldVersionInstalled}'));

    EditOldInstallDir := AddEdit(Surface, Bottom,
      OldVersionInstallDir);
    SetEditReadOnly(EditOldInstallDir, True);

    AddLabel(Surface, Bottom,
      ExpandConstants('{cm:OldVersionWillBeUninstalled}'));
  end else
  begin
    AddLabel(Surface, Bottom,
      ExpandConstants('{cm:ImportOldLabyrinthsPrompt}'));
  end;

  CheckBoxImportOld := AddCheckBox(Surface, Bottom,
    ExpandConstants('{cm:ImportOldLabyrinths}'),
    HasOldVersion);
  if not HasOldVersion then
    CheckBoxImportOld.OnClick := @CheckBoxImportOldClick;
    
  if not HasOldVersion then
  begin
    AddLabel(Surface, Bottom,
      ExpandConstants('{cm:OldVersionInstallDirPrompt}'));
      
    EditOldInstallDir := AddDirEdit(Surface, Bottom,
      ExpandConstant('{pf}\SJRDoeraene\FunLabyrinthe\'),
      ExpandConstants('{cm:SelectOldVersionInstallDirPrompt}'));
    SetEditReadOnly(EditOldInstallDir, True);
  end;

  AddImgText(Surface, Bottom, InfoBitmap,
    ExpandConstants('{cm:ImportInfos}'));
end;

function CreateBitmap(const FileName : string) : TBitmap;
var TmpFile : string;
begin
  ExtractTemporaryFile(FileName);
  Result := TBitmap.Create;
  Result.LoadFromFile(ExpandConstant('{tmp}\'+FileName));
end;

function InitializeSetup : boolean;
begin
  InfoBitmap := CreateBitmap(InfoBitmapFileName);
  DirEdits := TStringList.Create;
  
  CheckOldVersion;

  Result := True;
end;

procedure DeinitializeSetup;
begin
  DirEdits.Free;
  InfoBitmap.Free;
end;

procedure InitializeWizard;
begin
  PageWidth := WizardForm.DiskSpaceLabel.Width;
  
  AddFunLabyAppDataField;
  AddImportOldPage;
end;

function ShouldSkipPage(PageID : integer) : boolean;
begin
  Result := False;
end;

function NextButtonClick(CurPageID : Integer): Boolean;
begin
  if CurPageID = wpSelectDir then
    Result := CheckValidAppData
  else
    Result := True;
end;

