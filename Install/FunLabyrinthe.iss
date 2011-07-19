; Installation de FunLabyrinthe 5.2

[Setup]
AppID=FunLabyrinthe
AppName={cm:AppName}
AppVerName={cm:AppVerName}
AppComments={cm:Description}
AppContact=info@funlabyrinthe.com
AppPublisher=Sébastien Doeraene
AppCopyright=Copyright (C) 2000-2011 Sébastien Doeraene
AppPublisherURL=http://www.funlabyrinthe.com/
AppSupportURL=http://www.funlabyrinthe.com/forum/
AppUpdatesURL=http://www.funlabyrinthe.com/download/
AppVersion=5.2

VersionInfoProductName=FunLabyrinthe 5.2
VersionInfoDescription=Setup of FunLabyrinthe 5.2
VersionInfoTextVersion=FunLabyrinthe v5.2, copyright 2000-2011 Sébastien Doeraene
VersionInfoVersion=5.2

DefaultDirName={pf}\SJRDoeraene\{cm:AppName}
DefaultGroupName={cm:AppName}

OutputDir=.\
OutputBaseFilename=funlabyrinthe_5_2

Compression=lzma
SolidCompression=yes

ChangesAssociations=yes

[Languages]
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"

[CustomMessages]
fr.AppName=FunLabyrinthe
fr.AppVerName=FunLabyrinthe 5.2
fr.Description=Jeu de labyrinthe très fun avec toutes sortes de gadgets

; Types d'installation

fr.FullInstall=Installation complète
fr.PlayOnlyInstall=Installation légère, sans les programmes d'édition
fr.CustomInstall=Installation personnalisée (recommandé pour les utilisateurs avancés)

; Nom des composants

fr.CompPrograms=Fichiers programme
fr.CompRuntime=Fichiers de runtime de FunLabyrinthe
fr.CompFunLaby=FunLaby, le programme de jeu
fr.CompGeneLaby=GeneLaby, un générateur de labyrinthes
fr.CompFunLabyEdit=FunLabyEdit, l'éditeur de labyrinthes

fr.CompHelp=Fichiers d'aide

fr.CompAppData=Fichiers de labyrinthe, cartes, sons, graphismes, etc.

; Texte des raccourcis

fr.IconFunLaby=Jouer à FunLabyrinthe
fr.IconGeneLaby=Générateur de labyrinthes
fr.IconFunLabyEdit=Éditeur de labyrinthes

; Tâches

fr.AdditionalTasks=Tâches supplémentaires
fr.RegExtensions=Enregistrer les extensions de fichier liées à FunLabyrinthe
fr.RegFLP=Fichiers projet (.flp)

; Enregistrement des extensions

fr.FunLabyProject=Projet FunLabyrinthe

fr.Open=&Ouvrir
fr.Edit=&Éditer

; Composants génériques

fr.PleaseSelectDir=Veuillez sélectionner un répertoire

; Page de sélection des répertoires d'installation

fr.AppDataDirPrompt=Les labyrinthes, graphismes, sons, etc. seront enregistrés dans le dossier suivant :
fr.AppDataSelectDirPrompt=Choisissez le répertoire où enregistrer les labyrinthes

; Pages d'informations sur les versions < 5.0

fr.PageCompatibility4xCaption=Information importante
fr.PageCompatibility4xDescription=Veuillez lire attentivement les informations suivantes avant de poursuivre.
fr.PageCompatibility4xMsg=Une ancienne version de {cm:AppName} est installée sur votre ordinateur (v{code:OldVersionInfo|version}).%n%nDepuis {cm:AppName} 5.2, la transition automatique depuis {cm:AppName} {code:OldVersionInfo|version} n'est plus assurée ! Si vous aviez créé des labyrinthes avec cette version, il est plus que recommandé d'installer d'abord {cm:AppName} 5.1.2, qui est la dernière version à assurer la transition.%n%nDans tous les cas, il est également plus que recommandé de désinstaller {cm:AppName} {code:OldVersionInfo|version} avant de poursuivre l'installation de {cm:AppVerName}. Vous pouvez néanmoins poursuivre l'installation si vous le désirez, à vos propres risques.

; Pages d'informations sur les versions < 5.2

fr.PageBefore52Caption=Information importante
fr.PageBefore52Description=Veuillez lire attentivement les informations suivantes avant de poursuivre.
fr.PageBefore52Msg=Une ancienne version de {cm:AppName} est installée sur votre ordinateur (antérieure à la v5.2).%n%nDepuis {cm:AppName} 5.2, la structure des documents FunLabyrinthe (images, labyrinthes, etc.) a considablement changé, et n'est plus compatible avec celle que vous avez !%n%nVous devriez suivre les indications données sur la page Web http://www.funlabyrinthe.com/download/ afin de faire la transition avant de poursuivre l'installation de {cm:AppVerName}. Vous pouvez néanmoins poursuivre l'installation si vous le désirez, à vos propres risques.

; Pages d'informations si le dossier AppData existe

fr.PageAppDataExistsCaption=Information importante
fr.PageAppDataExistsDescription=Veuillez lire attentivement les informations suivantes avant de poursuivre.
fr.PageAppDataExistsMsg=Le dossier où vous avez choisi d'enregistrer les labyrinthes existe. Afin de préserver vos données, cette installation ne le modifiera pas. Vous pouvez télécharger de nouvelles versions de la bibliothèque et des labyrinthes sur le site de FunLabyrinthe (www.funlabyrinthe.com).

; Recompilation des unités

fr.RecompilingSources=Mise à jour des unités... (entre quelques secondes et une minute)

[Types]
Name: "full"    ; Description: {cm:FullInstall}

[Components]
Name: "programs"            ; Description: {cm:CompPrograms}   ; Types: full
Name: "programs\runtime"    ; Description: {cm:CompRuntime}    ; Types: full; Flags: fixed
Name: "programs\funlaby"    ; Description: {cm:CompFunLaby}    ; Types: full; Flags: fixed
Name: "programs\genelaby"   ; Description: {cm:CompGeneLaby}   ; Types: full
Name: "programs\funlabyedit"; Description: {cm:CompFunLabyEdit}; Types: full

Name: "help"; Description: {cm:CompHelp}; Types: full; Flags: fixed

Name: "appdata"; Description: {cm:CompAppData}; Types: full; Flags: fixed

[Tasks]
Name: "desktopicon"            ; Description: "{cm:CreateDesktopIcon}"                                ; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "desktopicon\funlaby"    ; Description: "{cm:IconFunLaby}"    ; Components: programs\funlaby    ; GroupDescription: "{cm:AdditionalIcons}"
Name: "desktopicon\genelaby"   ; Description: "{cm:IconGeneLaby}"   ; Components: programs\genelaby   ; GroupDescription: "{cm:AdditionalIcons}"
Name: "desktopicon\funlabyedit"; Description: "{cm:IconFunLabyEdit}"; Components: programs\funlabyedit; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

Name: "regext"    ; Description: "{cm:RegExtensions}"; GroupDescription: {cm:AdditionalTasks}
Name: "regext\flp"; Description: "{cm:RegFLP}"       ; GroupDescription: {cm:AdditionalTasks}

[Files]
Source: "..\FunLabyCore.bpl";         DestDir: "{app}"; Components: programs\runtime    ; Flags: ignoreversion
Source: "..\FunLabyTools.bpl";        DestDir: "{app}"; Components: programs\runtime    ; Flags: ignoreversion
Source: "..\FunLabyEditPlugin.bpl";   DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion
Source: "..\FunLabyEditTools.bpl";    DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion
Source: "..\FunLaby.exe";             DestDir: "{app}"; Components: programs\funlaby    ; Flags: ignoreversion
Source: "..\GeneLaby.exe";            DestDir: "{app}"; Components: programs\genelaby   ; Flags: ignoreversion
Source: "..\FunLabyEdit.exe";         DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion
Source: "..\ProjectManager.exe";      DestDir: "{app}"; Components: programs\funlaby    ; Flags: ignoreversion
Source: "..\FunLabyVersionCheck.exe"; DestDir: "{app}"; Components: programs\funlaby    ; Flags: ignoreversion

Source: "..\UnitPackages\*.bpl"; DestDir: "{app}\UnitPackages"; Components: programs\runtime; Flags: ignoreversion
Source: "..\EditPlugins\*.bpl" ; DestDir: "{app}\EditPlugins" ; Components: programs\runtime; Flags: ignoreversion

Source: "Runtime\*"; DestDir: "{sys}"; Attribs: system; Components: programs\runtime; Flags: sharedfile replacesameversion

Source: "..\Templates\*"; DestDir: "{app}\Templates"; Components: programs\funlabyedit; Flags: ignoreversion recursesubdirs createallsubdirs

Source: "..\FunLabyrinthe.chm"; DestDir: "{app}"; Components: help; Flags: ignoreversion

Source: "AppData\*"; DestDir: "{code:AppData}"; Check: GetShouldInstallAppData; Flags: sortfilesbyextension ignoreversion recursesubdirs createallsubdirs uninsneveruninstall

; Fichiers temporaires pour l'installation

Source: "Images\*"; Flags: dontcopy

[INI]
Filename: "{app}\FunLabyrinthe.ini"; Section: "Directories"; Key: "AppData"; String: "{code:AppData}"

[Registry]
Root: HKCR; SubKey: ".flp"                                    ; ValueType: string; ValueName: ""; ValueData: "FunLabyrinthe.Project"           ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project"                   ; ValueType: string; ValueName: ""; ValueData: "{cm:FunLabyProject}"             ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\DefaultIcon"       ; ValueType: string; ValueName: ""; ValueData: "{app}\FunLaby.exe,0"             ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell"             ; ValueType: string; ValueName: ""; ValueData: "open,edit"                       ; Tasks: regext\flp; Components: programs\funlabyedit    ; Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell"             ; ValueType: string; ValueName: ""; ValueData: "open"                            ; Tasks: regext\flp; Components: not programs\funlabyedit; Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\open"        ; ValueType: string; ValueName: ""; ValueData: "{cm:Open}"                       ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\FunLaby.exe"" ""%1"""    ; Tasks: regext\flp;                                       Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\edit"        ; ValueType: string; ValueName: ""; ValueData: "{cm:Edit}"                       ; Tasks: regext\flp; Components: programs\funlabyedit    ; Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCR; SubKey: "FunLabyrinthe.Project\shell\edit\command"; ValueType: string; ValueName: ""; ValueData: """{app}\FunLabyEdit.exe"" ""%1"""; Tasks: regext\flp; Components: programs\funlabyedit    ; Flags: uninsdeletevalue uninsdeletekeyifempty

[Icons]
Name: "{group}\{cm:IconFunLaby}"    ; Filename: "{app}\FunLaby.exe"    ; Components: programs\funlaby
Name: "{group}\{cm:IconGeneLaby}"   ; Filename: "{app}\GeneLaby.exe"   ; Components: programs\genelaby
Name: "{group}\{cm:IconFunLabyEdit}"; Filename: "{app}\FunLabyEdit.exe"; Components: programs\funlabyedit

Name: "{commondesktop}\{cm:IconFunLaby}"    ; Filename: "{app}\FunLaby.exe"    ; Tasks: desktopicon\funlaby
Name: "{commondesktop}\{cm:IconGeneLaby}"   ; Filename: "{app}\GeneLaby.exe"   ; Tasks: desktopicon\genelaby
Name: "{commondesktop}\{cm:IconFunLabyEdit}"; Filename: "{app}\FunLabyEdit.exe"; Tasks: desktopicon\funlabyedit

[Run]
Filename: "{app}\FunLabyEdit.exe"; Parameters: "-autocompile"; WorkingDir: "{app}"; StatusMsg: "{cm:RecompilingSources}"; Flags: runasoriginaluser waituntilidle
Filename: "{app}\FunLaby.exe"; Description: "{cm:LaunchProgram,FunLabyrinthe}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: files; Name: "{app}\FunLabyrinthe.ini"

[Code]
const
  OldVersionRegKey =
    'Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyOld_is1\';

  ThisVersionRegKey =
    'Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyrinthe_is1\';

  InfoBitmapFileName = 'Info.bmp';

var
  InfoBitmap: TBitmap;

  HasOldVersion: Boolean;
  OldVersionDisplayVersion: string;
  OldVersionInstallDir: string;
  OldVersionUninstallString: string;

  IsReinstall: Boolean;
  IsReinstallBefore52: Boolean;

  ShouldInstallAppData: Boolean;

  PageWidth: Integer;
  DirEdits: TStrings;

  PageCompatibility4xInfo: TWizardPage;
  PageBefore52Info: TWizardPage;
  PageAppDataExistsInfo: TWizardPage;

  SelectDirPage: TNewNotebookPage;
  EditFunLabyAppData: TEdit;

function FunLabyAppData: string;
begin
  Result := EditFunLabyAppData.Text;
  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result+'\';
end;

function AppData(Param: string): string;
begin
  Result := FunLabyAppData;
end;

function OldVersionInfo(Param: string): string;
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

function GetShouldInstallAppData: Boolean;
begin
  Result := ShouldInstallAppData;
end;

function CheckValidAppData: Boolean;
begin
  Result := True;

  ShouldInstallAppData := not DirExists(FunLabyAppData);
end;

procedure CheckOldVersion;
begin
  HasOldVersion := RegKeyExists(HKLM, OldVersionRegKey);
  OldVersionDisplayVersion := '';
  OldVersionInstallDir := '';
  OldVersionUninstallString := '';

  if HasOldVersion then
  begin
    RegQueryStringValue(HKLM, OldVersionRegKey, 'DisplayVersion',
      OldVersionDisplayVersion);
    RegQueryStringValue(HKLM, OldVersionRegKey, 'InstallLocation',
      OldVersionInstallDir);
    RegQueryStringValue(HKLM, OldVersionRegKey, 'QuietUninstallString',
      OldVersionUninstallString);
  end;
end;

procedure CheckIsReinstall;
var
  OldVersion: string;
begin
  IsReinstall := RegKeyExists(HKLM, ThisVersionRegKey);

  if IsReinstall then
  begin
    RegQueryStringValue(HKLM, ThisVersionRegKey, 'DisplayVersion',
      OldVersion);
    IsReinstallBefore52 := (OldVersion = '5.0') or (OldVersion = '5.0.1') or
      (OldVersion = '5.1') or (OldVersion = '5.1.1') or (OldVersion = '5.1.2');
  end;
end;

function ExpandConstants(const Str: string): string;
var
  Old: string;
begin
  Result := Str;
  repeat
    Old := Result;
    Result := ExpandConstant(Result);
  until Result = Old;
end;

function AddLabel(AParent: TWinControl; var Bottom: Integer;
  const ACaption: string): TNewStaticText;
begin
  Result := TNewStaticText.Create(AParent);
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

function AddBitmapImage(AParent: TWinControl; var Bottom: Integer;
  Image: TBitmap): TBitmapImage;
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

function AddImgText(AParent: TNewNotebookPage; var Bottom: Integer;
  Image: TBitmap; const Text: string): TNewStaticText;
var
  LabelBottom: Integer;
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

function AddCheckBox(AParent: TNewNotebookPage; var Bottom: Integer;
  const ACaption: string; AChecked: Boolean): TCheckBox;
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

function AddEdit(AParent: TNewNotebookPage; var Bottom: Integer;
  const DefaultText: string): TEdit;
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

procedure BrowseForDirOfEdit(Sender: TObject);
var
  Index: Integer;
  Edit: TEdit;
  Dir: string;
begin
  Index := TComponent(Sender).Tag;
  Edit := TEdit(DirEdits.Objects[Index]);

  Dir := Edit.Text;
  if BrowseForFolder(DirEdits[Index], Dir, False) then
    Edit.Text := Dir;
end;

function AddDirEdit(AParent: TNewNotebookPage; var Bottom: Integer;
  const DefaultDir, Prompt: string): TEdit;
begin
  Result := AddEdit(AParent, Bottom, DefaultDir);
  Result.Width := WizardForm.DirEdit.Width;

  with WizardForm, TButton.Create(AParent) do
  begin
    Left := DirBrowseButton.Left;
    Top := DirBrowseButton.Top + Result.Top - DirEdit.Top;
    Width := DirBrowseButton.Width;
    Height := DirBrowseButton.Height;
    Caption := DirBrowseButton.Caption;
    Parent := AParent;
    Tag := DirEdits.Add(Prompt);
    DirEdits.Objects[Tag] := Result;
    OnClick := @BrowseForDirOfEdit;
  end;
end;

procedure SetEditReadOnly(Edit: TEdit; ReadOnly: Boolean);
var
  I: Integer;
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
var
  Bottom: Integer;
begin
  // Ajouter un champ à la page de sélection de répertoire

  SelectDirPage := WizardForm.SelectDirPage;

  with WizardForm.DirEdit do
    Bottom := Top + Height + 8;

  AddLabel(SelectDirPage, Bottom,
    ExpandConstants('{cm:AppDataDirPrompt}'));

  EditFunLabyAppData := AddDirEdit(SelectDirPage, Bottom,
    GetPreviousData('AppData',
    ExpandConstant('{commondocs}\FunLabyrinthe\')),
    ExpandConstants('{cm:AppDataSelectDirPrompt}'));
end;

procedure AddCompatibility4xInfoPage;
begin
  // Ajouter la page d'information sur FunLabyrinthe 4.x

  PageCompatibility4xInfo := CreateOutputMsgPage(wpWelcome,
    ExpandConstants('{cm:PageCompatibility4xCaption}'),
    ExpandConstants('{cm:PageCompatibility4xDescription}'),
    ExpandConstants('{cm:PageCompatibility4xMsg}'));
end;

procedure AddBefore52InfoPage;
begin
  // Ajouter la page d'information sur FunLabyrinthe < 5.2

  PageBefore52Info := CreateOutputMsgPage(wpWelcome,
    ExpandConstants('{cm:PageBefore52Caption}'),
    ExpandConstants('{cm:PageBefore52Description}'),
    ExpandConstants('{cm:PageBefore52Msg}'));
end;

procedure AddAppDataExistsInfoPage;
begin
  // Ajouter la page d'information sur les AppData existant

  PageAppDataExistsInfo := CreateOutputMsgPage(wpSelectDir,
    ExpandConstants('{cm:PageAppDataExistsCaption}'),
    ExpandConstants('{cm:PageAppDataExistsDescription}'),
    ExpandConstants('{cm:PageAppDataExistsMsg}'));
end;

function CreateBitmap(const FileName: string): TBitmap;
begin
  ExtractTemporaryFile(FileName);
  Result := TBitmap.Create;
  Result.LoadFromFile(ExpandConstant('{tmp}\'+FileName));
end;

function InitializeSetup: Boolean;
begin
  InfoBitmap := CreateBitmap(InfoBitmapFileName);
  DirEdits := TStringList.Create;

  CheckOldVersion;
  CheckIsReinstall;

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
  AddCompatibility4xInfoPage;
  AddBefore52InfoPage;
  AddAppDataExistsInfoPage;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  if PageID = PageCompatibility4xInfo.ID then
    Result := not HasOldVersion
  else if PageID = PageBefore52Info.ID then
    Result := not IsReinstallBefore52
  else if PageID = PageAppDataExistsInfo.ID then
    Result := not DirExists(FunLabyAppData)
  else if PageID = wpSelectComponents then
    Result := True
  else
    Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if CurPageID = wpSelectDir then
    Result := CheckValidAppData
  else
    Result := True;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, 'AppData', FunLabyAppData);
end;
