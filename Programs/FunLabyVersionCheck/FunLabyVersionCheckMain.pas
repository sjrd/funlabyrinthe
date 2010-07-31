unit FunLabyVersionCheckMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DateUtils, ScUtils, ScStrUtils, ScXML, SdDialogs,
  msxml, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdException,
  JvExStdCtrls, JvGroupBox, JvComponentBase, JvAppStorage, JvAppXMLStorage,
  FunLabyCoreConsts;

resourcestring
  SNewVersionAvailableTitle = 'Nouvelle version disponible';
  SNewVersionAvailable =
    'Une nouvelle version de FunLabyrinthe (%s) est disponible sur Internet';

  SNewMinorVersionAvailableTitle = 'Nouvelle version disponible';
  SNewMinorVersionAvailable =
    'Une amélioration mineure de FunLabyrinthe %s est disponible sur Internet';

type
  TFormMain = class(TForm)
    VersionInfoGrabber: TIdHTTP;
    GroupFrequency: TRadioGroup;
    GroupProxy: TJvGroupBox;
    LabelProxyServer: TLabel;
    EditProxyServer: TEdit;
    LabelProxyPort: TLabel;
    EditProxyPort: TEdit;
    LabelProxyUsername: TLabel;
    EditProxyUsername: TEdit;
    LabelProxyPassword: TLabel;
    EditProxyPassword: TEdit;
    GroupCurrentVersion: TGroupBox;
    LabelCurrentVersion: TLabel;
    TextCurrentVersion: TStaticText;
    LabelAvailableVersion: TLabel;
    TextAvailableVersion: TStaticText;
    LinkGoToDownloadPage: TLinkLabel;
    ButtonCheckNow: TButton;
    OptionsStorage: TJvAppXMLFileStorage;
    procedure LinkGoToDownloadPageLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure ButtonCheckNowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure LoadOptions;
    procedure SaveOptions;

    function LoadVersionInfo: IXMLDOMDocument;
    function CheckVersion(MsgOnNewVersion: Boolean = True): Boolean;
  public
    function DoAutoCheckIfNeeded(MsgOnNewVersion: Boolean = True): Boolean;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  VersionInfoURL =
    'http://www.funlabyrinthe.com/download/currentversion.xml';

{*
  Charge les options
*}
procedure TFormMain.LoadOptions;
begin
  GroupFrequency.ItemIndex := OptionsStorage.ReadInteger('checks/frequency');

  GroupProxy.Checked := OptionsStorage.ReadBoolean('proxy/useproxy', False);
  if GroupProxy.Checked then
  begin
    EditProxyServer.Text := OptionsStorage.ReadString('proxy/server');
    EditProxyPort.Text := OptionsStorage.ReadString('proxy/port');
    EditProxyUsername.Text := OptionsStorage.ReadString('proxy/username');
    EditProxyPassword.Text := OptionsStorage.ReadString('proxy/password');
  end;
end;

{*
  Enregistre les options
*}
procedure TFormMain.SaveOptions;
begin
  OptionsStorage.WriteInteger('checks/frequency', GroupFrequency.ItemIndex);

  OptionsStorage.WriteBoolean('proxy/useproxy', GroupProxy.Checked);
  if GroupProxy.Checked then
  begin
    OptionsStorage.WriteString('proxy/server', EditProxyServer.Text);
    OptionsStorage.WriteString('proxy/port', EditProxyPort.Text);
    OptionsStorage.WriteString('proxy/username', EditProxyUsername.Text);
    OptionsStorage.WriteString('proxy/password', EditProxyPassword.Text);
  end;
end;

{*
  Charge les informations de version depuis Internet
  @return Un document XML contenant les informations de version
*}
function TFormMain.LoadVersionInfo: IXMLDOMDocument;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    with VersionInfoGrabber.ProxyParams do
    begin
      if GroupProxy.Checked then
      begin
        ProxyServer := EditProxyServer.Text;
        ProxyPort := StrToIntDef(EditProxyPort.Text, 80);
        ProxyUsername := EditProxyUsername.Text;
        ProxyPassword := EditProxyPassword.Text;
        BasicAuthentication := EditProxyUsername.Text <> '';
      end else
        Clear;
    end;

    VersionInfoGrabber.Get(VersionInfoURL, Stream);
    Stream.Seek(0, soFromBeginning);
    Result := LoadXMLDocumentFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{*
  Vérifie si une nouvelle version est disponible
  @param MsgOnNewVersion   Affiche un message en cas de nouvelle version
  @return True si une nouvelle version est disponible, False sinon
*}
function TFormMain.CheckVersion(MsgOnNewVersion: Boolean = True): Boolean;
var
  VersionInfo: IXMLDOMDocument;
  AvailableVersion: string;
  AvailMinorVersion, Diff: Integer;
begin
  VersionInfo := LoadVersionInfo;

  AvailableVersion := VersionInfo.selectSingleNode('/versioninfo/version').text;
  AvailMinorVersion := StrToInt(VersionInfo.selectSingleNode(
    '/versioninfo/minorversion').text);

  TextAvailableVersion.Caption := Format(SFullVersionNumber,
    [AvailableVersion, AvailMinorVersion]);

  Diff := CompareVersion(AvailableVersion, CurrentVersion);

  Result := Diff > 0;
  if Result and MsgOnNewVersion then
  begin
    ShowDialog(SNewVersionAvailableTitle,
      Format(SNewVersionAvailable, [AvailableVersion]));
  end;

  if Diff = 0 then
  begin
    Result := AvailMinorVersion > CurrentMinorVersion;

    if Result and MsgOnNewVersion then
    begin
    ShowDialog(SNewMinorVersionAvailableTitle,
      Format(SNewMinorVersionAvailable, [AvailableVersion]));
    end;
  end;

  OptionsStorage.WriteDateTime('checks/lastcheck', Now);
end;

{*
  Effectue la vérification automatique, si nécessaire
  @return True si une nouvelle version est disponible
*}
function TFormMain.DoAutoCheckIfNeeded(
  MsgOnNewVersion: Boolean = True): Boolean;
const
  cfDaily = 0;
  cfWeekly = 1;
  cfNever = 2;
var
  Frequency: Integer;
  LastCheck: TDateTime;
begin
  Result := False;

  Frequency := GroupFrequency.ItemIndex;
  if Frequency = cfNever then
    Exit;

  LastCheck := OptionsStorage.ReadDateTime('checks/lastcheck');

  case Frequency of
    cfDaily:
      if DaysBetween(Now, LastCheck) < 1 then
        Exit;

    cfWeekly:
      if WeeksBetween(Now, LastCheck) < 1 then
        Exit;
  end;

  try
    Result := CheckVersion(MsgOnNewVersion);
  except
    on Error: EIdException do;
  end;
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  TextCurrentVersion.Caption := Format(SFullVersionNumber,
    [CurrentVersion, CurrentMinorVersion]);

  LoadOptions;
end;

{*
  Gestionnaire d'événement OnDestroy de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SaveOptions;
end;

{*
  Gestionnaire d'événement OnShow de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.FormShow(Sender: TObject);
begin
  OnShow := nil;

  if TextAvailableVersion.Caption = '?' then
    CheckVersion;
end;

{*
  Gestionnaire d'événement OnClick du bouton Vérifier maintenant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormMain.ButtonCheckNowClick(Sender: TObject);
begin
  CheckVersion;
end;

{*
  Gestionnaire d'événement OnLinkClick du lien vers la page de téléchargement
  @param Sender     Objet qui a déclenché l'événement
  @param Link       Lien cliqué
  @param LinkType   Type de lien
*}
procedure TFormMain.LinkGoToDownloadPageLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  RunURL(Link);
end;

end.

