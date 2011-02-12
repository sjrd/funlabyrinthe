unit FunLabySourceEditorFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SdDialogs, FilesUtils, SourceEditors;

resourcestring
  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a été modifié. Voulez-vous l''enregistrer ?';

type
  {*
    Classe de base pour les frames qui doivent implémenter l'interface
    ISourceEditor50 de FunLabyrinthe.
    @author sjrd
    @version 5.0
  *}
  TFrameFunLabySourceEditor = class(TFrame, ISourceEditor50)
  private
    FFileName: TFileName; /// Nom du fichier source
    FModified: Boolean;   /// Indique si le source a été modifié

    FCompilerDestFileName: TFileName; /// Nom du fichier compilé

    FOnStateChange: TSourceEditorNotifyEvent; /// Événement OnStateChange

    function GetCompilerDestFileName: TFileName;
  protected // for support of additionnal interfaces
    function GetFileName: TFileName;
    function GetControl: TControl;
    function GetModified: Boolean;

    function GetOnStateChange: TSourceEditorNotifyEvent;
    procedure SetOnStateChange(Value: TSourceEditorNotifyEvent);

    procedure ISourceEditor50.Release = Free;
  protected
    procedure SetModified(Value: Boolean); virtual;

    procedure DoStateChange; virtual;

    function GetUnitName: string; virtual;

    procedure LoadFile(const AFileName: TFileName); virtual;
    function SaveFile: Boolean; virtual;
    function CanClose: Boolean; virtual;

    property FileName: TFileName read FFileName;
    property CompilerDestFileName: TFileName read GetCompilerDestFileName;
    property Modified: Boolean read FModified write SetModified;

    property OnStateChange: TSourceEditorNotifyEvent
      read FOnStateChange write FOnStateChange;
  end;

implementation

{$R *.dfm}

uses
  StrUtils;

{----------------------------------}
{ Classe TFrameFunLabySourceEditor }
{----------------------------------}

{*
  Nom de fichier de destination supposé pour la compilation
*}
function TFrameFunLabySourceEditor.GetCompilerDestFileName: TFileName;
var
  SourcesPartPos: Integer;
begin
  if FCompilerDestFileName = '' then
  begin
    FCompilerDestFileName := ChangeFileExt(FileName, '.scu');

    SourcesPartPos := Pos(PathDelim + SourcesDir + PathDelim,
      FCompilerDestFileName);

    if SourcesPartPos > 0 then
    begin
      FCompilerDestFileName := JoinPath([
        Copy(FCompilerDestFileName, 1, SourcesPartPos), UnitsDir,
        ExtractFileName(FCompilerDestFileName)]);
    end;
  end;

  Result := FCompilerDestFileName;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetFileName: TFileName;
begin
  Result := FFileName;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetControl: TControl;
begin
  Result := Self;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetModified: Boolean;
begin
  Result := FModified;
end;

{*
  Modifie la propriété Modified
  Si la valeur de Modified est changée, la méthode DoStateChange est appelée
  pour refléter le changement d'état.
  @param Value   Nouvelle valeur de Modified
*}
procedure TFrameFunLabySourceEditor.SetModified(Value: Boolean);
begin
  if Value <> FModified then
  begin
    FModified := Value;
    DoStateChange;
  end;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.GetOnStateChange: TSourceEditorNotifyEvent;
begin
  Result := FOnStateChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySourceEditor.SetOnStateChange(
  Value: TSourceEditorNotifyEvent);
begin
  FOnStateChange := Value;
end;

{*
  Notifie un changement de l'état
*}
procedure TFrameFunLabySourceEditor.DoStateChange;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

{*
  Nom de l'unité éditée
  @return Nom de l'unité éditée
*}
function TFrameFunLabySourceEditor.GetUnitName: string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
end;

{*
  Charge un fichier source
  @param AFileName   Nom du fichier à charger
*}
procedure TFrameFunLabySourceEditor.LoadFile(const AFileName: TFileName);
begin
  FFileName := AFileName;

  FModified := False;
end;

{*
  Enregistre le fichier source
  Appelle toujours DoStateChange.
*}
function TFrameFunLabySourceEditor.SaveFile: Boolean;
begin
  FModified := False;
  DoStateChange;

  Result := True;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySourceEditor.CanClose: Boolean;
begin
  if not Modified then
    Result := True
  else
  begin
    case ShowDialog(sConfirmExitTitle, sConfirmExit, dtConfirmation,
      dbYesNoCancel) of
      drYes: Result := SaveFile;
      drNo: Result := True;
    else
      Result := False;
    end;
  end;
end;

end.

