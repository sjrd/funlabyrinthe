; Installation de FunLabyrinthe 5.1.1

[Setup]
AppID=FunLabyrinthe
AppName={cm:AppName}
AppVerName={cm:AppVerName}
AppComments={cm:Description}
AppContact=sjrd@redaction-developpez.com
AppPublisher=SJRDoeraene
AppCopyright=Copyright (C) 2000-2010 S�bastien Doeraene
AppPublisherURL=http://www.funlabyrinthe.com/
AppSupportURL=http://www.funlabyrinthe.com/forum/
AppUpdatesURL=http://www.funlabyrinthe.com/download/
AppVersion=5.1.1

VersionInfoProductName=FunLabyrinthe 5.1.1
VersionInfoDescription=Setup of FunLabyrinthe 5.1.1
VersionInfoTextVersion=FunLabyrinthe v5.1.1, copyright 2000-2010 SJRDoeraene
VersionInfoVersion=5.1.1

DefaultDirName={reg:HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyOld_is1\,InstallLocation|{pf}\SJRDoeraene\{cm:AppName}}
DefaultGroupName={reg:HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall\FunLabyOld_is1\,Inno Setup: Icon Group|{cm:AppName}}

OutputDir=.\
OutputBaseFilename=funlabyrinthe_5_1_1

Compression=lzma
SolidCompression=yes

ChangesAssociations=yes

[Languages]
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"

[CustomMessages]
fr.AppName=FunLabyrinthe
fr.AppVerName=FunLabyrinthe 5.1.1
fr.Description=Jeu de labyrinthe tr�s fun avec toutes sortes de gadgets

; Types d'installation

fr.FullInstall=Installation compl�te
fr.PlayOnlyInstall=Installation l�g�re, sans les programmes d'�dition
fr.CustomInstall=Installation personnalis�e (recommand� pour les utilisateurs avanc�s)

; Nom des composants

fr.CompPrograms=Fichiers programme
fr.CompRuntime=Fichiers de runtime de FunLabyrinthe
fr.CompFunLaby=FunLaby, le programme de jeu
fr.CompGeneLaby=GeneLaby, un g�n�rateur de labyrinthes
fr.CompFunLabyEdit=FunLabyEdit, l'�diteur de labyrinthes

fr.CompHelp=Fichiers d'aide

fr.CompAppData=Fichiers de labyrinthe, cartes, sons, graphismes, etc.

; Texte des raccourcis

fr.IconFunLaby=Jouer � FunLabyrinthe
fr.IconGeneLaby=G�n�rateur de labyrinthes
fr.IconFunLabyEdit=�diteur de labyrinthes

; T�ches

fr.AdditionalTasks=T�ches suppl�mentaires
fr.RegExtensions=Enregistrer les extensions de fichier li�es � FunLabyrinthe
fr.RegFLP=Fichiers projet (.flp)

; Enregistrement des extensions

fr.FunLabyProject=Projet FunLabyrinthe

fr.Open=&Ouvrir
fr.Edit=&�diter

; Composants g�n�riques

fr.PleaseSelectDir=Veuillez s�lectionner un r�pertoire

; Page de s�lection des r�pertoires d'installation

fr.AppDataDirPrompt=Les labyrinthes, graphismes, sons, etc. seront enregistr�s dans le dossier suivant :
fr.AppDataSelectDirPrompt=Choisissez le r�pertoire o� enregistrer les labyrinthes

; Pages d'import d'anciens labyrinthes

fr.PageImportOldTitle=Importer les labyrinthes depuis une ancienne version
fr.PageImportOldDescription=Vous pouvez importer les labyrinthes que vous aviez cr��s avec une ancienne version de {cm:AppName}.

fr.OldVersionInstalled=Une ancienne version de {cm:AppName} est install�e sur votre ordinateur (v{code:OldVersionInfo|version}), dans le dossier suivant :
fr.OldVersionWillBeUninstalled=Celle-ci sera automatiquement d�sinstall�e avant d'installer {cm:AppVerName}.%nCependant, vous pouvez demander � {cm:AppVerName} d'importer automatiquement les labyrinthes que vous aviez cr��s avec cette ancienne version.

fr.ImportOldLabyrinthsPrompt=Si vous aviez install� pr�c�demment une ancienne version de {cm:AppName}, puis d�sinstall�e, vous avez peut-�tre conserv� les labyrinthes que vous aviez cr��s. Si c'est le cas, il vous est maintenant possible d'importer ces anciens labyrinthes dans {cm:AppVerName}.

fr.ImportOldLabyrinths=Importer automatiquement mes anciens labyrinthes

fr.OldVersionInstallDirPrompt=Veuillez s�lectionner l'ancien r�pertoire d'installation de {cm:AppName}, celui qui contient le dossier nomm� "Labyrinthes", entre autres :
fr.SelectOldVersionInstallDirPrompt=Ancien r�pertoire d'installation de {cm:AppName}

fr.ImportInfos=Les labyrinthes pr�-install�s ne peuvent pas �tre import�s : ils sont d�j� inclus dans cette nouvelle installation, avec, pour la plupart, des am�liorations. De m�me, les labyrinthes disponibles sur le site Web de {cm:AppName} ont tous d�j� �t� import�s : vous pouvez les ret�l�charger. L'importation sert uniquement pour les labyrinthes que VOUS avez cr��s.

fr.InvalidOldInstallDirWhenInstalled=Dossier invalide
fr.InvalidOldInstallDirWhenNotInstalled=Dossier invalide

; Page de s�lection des labyrinthes � importer

fr.PageSelectImportsTitle=S�lection des labyrinthes � importer
fr.PageSelectImportsDescription=Veuillez s�lectionner parmi les labyrinthes disponibles ci-dessous ceux que vous souhaitez importer.
fr.PageSelectImportsPrompt=Veuillez s�lectionner les labyrinthes � importer

; D�sinstallation de l'ancienne version

fr.PageUninstallOldVersionTitle=D�sinstallation de {cm:AppName} {code:OldVersionInfo|version} en cours
fr.PageUninstallOldVersionDescription=Veuillez patienter pendant que l'assistant d�sinstalle votre ancienne version de {cm:AppName}.

fr.UninstallingOldVersion=D�sinstallation de {cm:AppName} {code:OldVersionInfo|version}...

fr.CouldntUninstallOldVersion=L'ancienne version n'a pas pu �tre d�sinstall�e correctement. Il vous est possible n�anmoins de continuer l'installation, mais c'est d�conseill�. Essayez d'abord de d�sinstaller {cm:AppName} {code:OldVersionInfo|version} depuis le panneau de configuration.%nVoulez-vous tout de m�me poursuivre l'installation ?

; Import des anciens labyrinthes

fr.PageImportingOldTitle=Import des anciens labyrinthes en cours
fr.PageImportingOldDescription=L'assistant importe maintenant les anciens labyrinthes que vous avez s�lectionn�s.

fr.ImportingOld=Import des anciens labyrinthes...

; Recompilation des unit�s

fr.RecompilingSources=Mise � jour des unit�s... (entre quelques secondes et une minute)

[Types]
Name: "full"    ; Description: {cm:FullInstall}
;Name: "playonly"; Description: {cm:PlayOnlyInstall}
;Name: "custom"  ; Description: {cm:CustomInstall}  ; Flags: iscustom

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

[InstallDelete]
; Delete old units that were installed with previous versions
Type: files; Name: "{code:AppData}\Units\LevelledGrounds.*"

[Files]
Source: "..\FunLabyCore.bpl";         DestDir: "{app}"; Components: programs\runtime    ; Flags: ignoreversion
Source: "..\FunLabyTools.bpl";        DestDir: "{app}"; Components: programs\runtime    ; Flags: ignoreversion
Source: "..\FunLabyEditPlugin.bpl";   DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion
Source: "..\FunLabyEditTools.bpl";    DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion
Source: "..\FunLaby.exe";             DestDir: "{app}"; Components: programs\funlaby    ; Flags: ignoreversion
Source: "..\GeneLaby.exe";            DestDir: "{app}"; Components: programs\genelaby   ; Flags: ignoreversion
Source: "..\FunLabyEdit.exe";         DestDir: "{app}"; Components: programs\funlabyedit; Flags: ignoreversion
Source: "..\FunLabyVersionCheck.exe"; DestDir: "{app}"; Components: programs\funlaby    ; Flags: ignoreversion

Source: "..\EditPlugins\*.bpl"; DestDir: "{app}\EditPlugins"; Components: programs\runtime; Flags: ignoreversion

Source: "Runtime\*"; DestDir: "{sys}"; Attribs: system; Components: programs\runtime; Flags: sharedfile replacesameversion

Source: "..\FunLabyrinthe.chm"; DestDir: "{app}"; Components: help; Flags: ignoreversion

Source: "AppData\*"; DestDir: "{code:AppData}"; Flags: sortfilesbyextension ignoreversion recursesubdirs createallsubdirs uninsneveruninstall

Source: "{code:OldVersionInfo|installdir}\Sons\*";  DestDir: "{code:AppData}\Sounds";                  Check: MustCopyOldSoundsAndImages; Flags: external skipifsourcedoesntexist onlyifdoesntexist recursesubdirs createallsubdirs uninsneveruninstall
Source: "{code:OldVersionInfo|installdir}\Cases\*"; DestDir: "{code:AppData}\Squares\Compatibility4x"; Check: MustCopyOldSoundsAndImages; Flags: external skipifsourcedoesntexist onlyifdoesntexist recursesubdirs createallsubdirs uninsneveruninstall

; Fichiers temporaires pour l'installation

Source: "Images\*"; Flags: dontcopy
Source: "Import4x.dll"; Flags: dontcopy
Source: "DoNotImportLabs.txt"; Flags: dontcopy

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

  DoNotImportLabsFile = 'DoNotImportLabs.txt';

var
  InfoBitmap: TBitmap;

  HasOldVersion: Boolean;
  OldVersionDisplayVersion: string;
  OldVersionInstallDir: string;
  OldVersionUninstallString: string;

  IsReinstall: Boolean;

  PageWidth: Integer;
  DirEdits: TStrings;

  SelectDirPage: TNewNotebookPage;
  EditFunLabyAppData: TEdit;

  PageImportOld: TWizardPage;
  CheckBoxImportOld: TCheckBox;
  EditOldInstallDir: TEdit;
  ImportDir: string;

  PageSelectImports: TWizardPage;
  CheckListBoxImports: TNewCheckListBox;
  LabyrinthsToImport: TStrings;

  PageUninstallOldVersion: TOutputProgressWizardPage;
  PageImportingOld: TOutputProgressWizardPage;

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

function MustCopyOldSoundsAndImages: Boolean;
begin
  Result := ImportDir <> '';
end;

function CheckValidAppData: Boolean;
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
    RegQueryStringValue(HKLM, OldVersionRegKey, 'DisplayVersion',
      OldVersionDisplayVersion);
    RegQueryStringValue(HKLM, OldVersionRegKey, 'InstallLocation',
      OldVersionInstallDir);
    RegQueryStringValue(HKLM, OldVersionRegKey, 'QuietUninstallString',
      OldVersionUninstallString);
  end;
end;

procedure CheckIsReinstall;
begin
  IsReinstall := RegKeyExists(HKLM, ThisVersionRegKey);
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

  // M�thode un peu bourrin pour retrouver l'�ventuel bouton Parcourir
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
  // Ajouter un champ � la page de s�lection de r�pertoire

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

procedure CheckBoxImportOldClick(Sender: TObject);
begin
  SetEditReadOnly(EditOldInstallDir, not CheckBoxImportOld.Checked);
end;

procedure AddImportOldPage;
var
  Surface: TNewNotebookPage;
  Bottom: Integer;
  Checked: Boolean;
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

  Checked := HasOldVersion and (not IsReinstall);
  CheckBoxImportOld := AddCheckBox(Surface, Bottom,
    ExpandConstants('{cm:ImportOldLabyrinths}'),
    Checked);
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

function CheckValidImportDir: Boolean;
var
  OldImportDir: string;
begin
  OldImportDir := ImportDir;

  if not CheckBoxImportOld.Checked then
  begin
    ImportDir := '';
    Result := True;
  end else
  begin
    ImportDir := EditOldInstallDir.Text;
    if (ImportDir <> '') and (ImportDir[Length(ImportDir)] <> '\') then
      ImportDir := ImportDir + '\';
    Result := DirExists(ImportDir + 'Labyrinthes');
    if not Result then
    begin
      if HasOldVersion then
      begin
        MsgBox(ExpandConstants(
          '{cm:InvalidOldInstallDirWhenInstalled}'), mbError, MB_OK);
      end else
      begin
        MsgBox(ExpandConstants(
          '{cm:InvalidOldInstallDirWhenNotInstalled}'), mbError, MB_OK);
      end;
    end;
  end;
end;

procedure AddSelectImportsPage;
var
  Surface: TNewNotebookPage;
  Bottom: Integer;
begin
  PageSelectImports := CreateCustomPage(PageImportOld.ID,
    ExpandConstants('{cm:PageSelectImportsTitle}'),
    ExpandConstants('{cm:PageSelectImportsDescription}'));
  Surface := PageSelectImports.Surface;
  Bottom := -8;

  AddLabel(Surface, Bottom,
    ExpandConstants('{cm:PageSelectImportsPrompt}'));

  CheckListBoxImports := TNewCheckListBox.Create(Surface);
  with CheckListBoxImports do
  begin
    Width := PageWidth;
    Top := Bottom + 8;
    Bottom := PageSelectImports.SurfaceHeight - 8;
    Height := Bottom - Top;
    Parent := Surface;
  end;
end;

function CanImportLab(const AFileName: string;
  DoNotImportLabs: TStrings): Boolean;
var
  FileName: string;
  I: Integer;
begin
  FileName := AnsiLowercase(Trim(ChangeFileExt(AFileName, '')));
  
  I := Pos('(', FileName);
  if I > 0 then
    FileName := Trim(Copy(FileName, 1, I-1));
    
  Result := False;

  for I := 0 to DoNotImportLabs.Count-1 do
    if FileName = AnsiLowercase(DoNotImportLabs[I]) then
      Exit;
      
  Result := True;
end;

procedure PageSelectImportsShow;
var
  DoNotImportLabs, OldUnchecked: TStrings;
  I: Integer;
  FindRec: TFindRec;
  FileName: string;
begin
  DoNotImportLabs := nil;
  OldUnchecked := TStringList.Create;
  try
    DoNotImportLabs := TStringList.Create;
    ExtractTemporaryFile(DoNotImportLabsFile);
    DoNotImportLabs.LoadFromFile(ExpandConstant('{tmp}\'+DoNotImportLabsFile));
  
    with CheckListBoxImports do
      for I := 0 to Items.Count-1 do
        if not Checked[I] then
          OldUnchecked.Add(Items[I]);

    CheckListBoxImports.Items.Clear;
    if FindFirst(ImportDir + 'Labyrinthes\*.lab', FindRec) then
    begin
      try
        repeat
          FileName := FindRec.Name;
          if CanImportLab(FileName, DoNotImportLabs) then
            CheckListBoxImports.AddCheckBox(FileName, '', 0,
              OldUnchecked.IndexOf(FileName) < 0, True, False, False, nil);
        until not FindNext(FindRec);
      finally
        FindClose(FindRec);
      end;
    end;
  finally
    OldUnchecked.Free;
  end;

  if LabyrinthsToImport <> nil then
  begin
    LabyrinthsToImport.Free;
    LabyrinthsToImport := nil;
  end;
end;

function PageSelectImportsValidate: Boolean;
var
  I: Integer;
begin
  LabyrinthsToImport := TStringList.Create;
  with CheckListBoxImports do
    for I := 0 to Items.Count-1 do
      if Checked[I] then
        LabyrinthsToImport.Add(Items[I]);
  Result := True;
end;

procedure AddUninstallOldVersionPage;
begin
  PageUninstallOldVersion := CreateOutputProgressPage(
    ExpandConstants('{cm:PageUninstallOldVersionTitle}'),
    ExpandConstants('{cm:PageUninstallOldVersionDescription}'));
end;

procedure UninstallOldVersion;
var
  FileName, Params: string;
  I: Integer;
  ResultCode: Integer;
begin
  PageUninstallOldVersion.Show;

  PageUninstallOldVersion.SetText(
    ExpandConstants('{cm:UninstallingOldVersion}'), '');

  FileName := OldVersionUninstallString;
  if FileName[1] = '"' then
  begin
    Delete(FileName, 1, 1);
    I := Pos('"', FileName);
    Params := Copy(FileName, I+2, MaxInt);
    Delete(FileName, I, MaxInt);
  end else
  begin
    I := Pos(' ', FileName);
    Params := Copy(FileName, I+1, MaxInt);
    Delete(FileName, I, MaxInt);
  end;

  if (not Exec(FileName, Params, '', SW_HIDE,
    ewWaitUntilTerminated, ResultCode)) or (ResultCode <> 0) then
  begin
    if MsgBox(ExpandConstants('{cm:CouldntUninstallOldVersion}'),
      mbError, MB_YESNO or MB_DEFBUTTON2) <> idYes then
      Abort;
  end;

  PageUninstallOldVersion.Hide;
end;

procedure AddImportingOldPage;
begin
  PageImportingOld := CreateOutputProgressPage(
    ExpandConstants('{cm:PageImportingOldTitle}'),
    ExpandConstants('{cm:PageImportingOldDescription}'));
end;

function ImportLabyrinth(FileName, AppData: PAnsiChar): Boolean;
  external 'ImportLabyrinth@files:Import4x.dll stdcall delayload setuponly';

procedure ImportOld;
var
  I: Integer;
  FileName: string;
begin
  PageImportingOld.Show;
  try
    for I := 0 to LabyrinthsToImport.Count-1 do
    begin
      FileName := LabyrinthsToImport[I];
      PageImportingOld.SetText(
        ExpandConstants('{cm:ImportingOld}'), FileName);
      PageImportingOld.SetProgress(I, LabyrinthsToImport.Count);

      ImportLabyrinth(PAnsiChar(AnsiString(ImportDir + 'Labyrinthes\' + FileName)),
        PAnsiChar(AnsiString(FunLabyAppData)));
    end;
  finally
    PageImportingOld.Hide;
  end;
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

  ImportDir := '';
  LabyrinthsToImport := nil;

  CheckOldVersion;
  CheckIsReinstall;

  Result := True;
end;

procedure DeinitializeSetup;
begin
  if LabyrinthsToImport <> nil then
    LabyrinthsToImport.Free;

  DirEdits.Free;
  InfoBitmap.Free;
end;

procedure InitializeWizard;
begin
  PageWidth := WizardForm.DiskSpaceLabel.Width;

  AddFunLabyAppDataField;
  AddImportOldPage;
  AddSelectImportsPage;
  AddUninstallOldVersionPage;
  AddImportingOldPage;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  if PageID = wpSelectComponents then
    Result := True
  else if PageID = PageImportOld.ID then
    Result := IsReinstall
  else if PageID = PageSelectImports.ID then
    Result := ImportDir = ''
  else
    Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if CurPageID = wpSelectDir then
    Result := CheckValidAppData
  else if CurPageID = PageImportOld.ID then
    Result := CheckValidImportDir
  else if CurPageID = PageSelectImports.ID then
    Result := PageSelectImportsValidate
  else
    Result := True;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = PageSelectImports.ID then
    PageSelectImportsShow;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  case CurStep of
    ssInstall:
      if HasOldVersion then
        UninstallOldVersion;
    ssPostInstall:
      if ImportDir <> '' then
        ImportOld;
  end;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, 'AppData', FunLabyAppData);
end;






