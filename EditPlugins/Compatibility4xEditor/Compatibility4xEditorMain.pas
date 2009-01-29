unit Compatibility4xEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SdDialogs, FilesUtils, SourceEditors, Compatibility4xSyn;

resourcestring
  C4xFilter = 'Actions de compatibilité 4.x (*.c4x)|*.c4x';

  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a été modifié. Voulez-vous l''enregistrer ?';

const {don't localize}
  C4xExtension = 'c4x';

type
  {*
    Éditeur de fichiers d'actions de compatibilité FunLabyrinthe 4.x
    @author sjrd
    @version 5.0
  *}
  TFrameCompatibility4xEditor = class(TFrame, ISourceEditor50)
    EditSource: TSynEdit;
    procedure EditSourceChange(Sender: TObject);
  private
    EditSourceHighlighter: TCompatibility4xSyntax; /// Highlighter

    FSourceFile: TSourceFile; /// Fichier source
    FModified: Boolean;       /// Indique si le source a été modifié

    FOnStateChange: TSourceEditorNotifyEvent; /// Événement OnStateChange

    procedure LoadFile(ASourceFile: TSourceFile);
    function SaveFile: Boolean;

    function GetSourceFile: TSourceFile;
    function GetControl: TControl;
    function GetModified: Boolean;

    function GetOnStateChange: TSourceEditorNotifyEvent;
    procedure SetOnStateChange(Value: TSourceEditorNotifyEvent);

    function CanClose: Boolean;
    procedure ISourceEditor50.Release = Free;
  protected
    procedure DoStateChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AfterConstruction; override;

    property SourceFile: TSourceFile read FSourceFile;
    property Modified: Boolean read FModified;

    property OnStateChange: TSourceEditorNotifyEvent
      read FOnStateChange write FOnStateChange;
  end;

implementation

{$R *.dfm}

const
  CreateDelphiSourceInfo: TSourceFileCreatorInfo = (
    Title: 'Source Delphi';
    Description: 'Fichier source Delphi, le langage original de FunLabyrinthe';
    AskForFileName: True;
    Filter: C4xFilter
  );

{*
  Crée un éditeur de source Delphi
  @param SourceFile   Fichier source à éditer
  @return Interface vers l'éditeur créé
*}
function CreateCompatibility4xEditor(SourceFile: TSourceFile): ISourceEditor50;
var
  Editor: TFrameCompatibility4xEditor;
begin
  Editor := TFrameCompatibility4xEditor.Create(nil);
  Editor.LoadFile(SourceFile);
  Result := Editor as ISourceEditor50;
end;

{------------------------------------}
{ Classe TFrameCompatibility4xEditor }
{------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameCompatibility4xEditor.Create(AOwner: TComponent);
begin
  inherited;

  EditSourceHighlighter := TCompatibility4xSyntax.Create(Self);
end;

{*
  [@inheritDoc]
*}
procedure TFrameCompatibility4xEditor.AfterConstruction;
begin
  inherited;

  EditSource.Highlighter := EditSourceHighlighter;
end;

{*
  Charge un fichier source
  @param ASourceFile   Fichier source à charger
*}
procedure TFrameCompatibility4xEditor.LoadFile(ASourceFile: TSourceFile);
begin
  FSourceFile := ASourceFile;

  EditSource.Lines.LoadFromFile(SourceFile.FileName);

  FModified := False;
  EditSource.Modified := False;
end;

{*
  Enregistre le fichier source
*}
function TFrameCompatibility4xEditor.SaveFile: Boolean;
begin
  EditSource.Lines.SaveToFile(SourceFile.FileName);

  FModified := False;
  EditSource.Modified := False;
  DoStateChange;

  Result := True;
end;

{*
  [@inheritDoc]
*}
function TFrameCompatibility4xEditor.GetSourceFile: TSourceFile;
begin
  Result := FSourceFile;
end;

{*
  [@inheritDoc]
*}
function TFrameCompatibility4xEditor.GetControl: TControl;
begin
  Result := Self;
end;

{*
  [@inheritDoc]
*}
function TFrameCompatibility4xEditor.GetModified: Boolean;
begin
  Result := FModified;
end;

{*
  [@inheritDoc]
*}
function TFrameCompatibility4xEditor.GetOnStateChange: TSourceEditorNotifyEvent;
begin
  Result := FOnStateChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameCompatibility4xEditor.SetOnStateChange(
  Value: TSourceEditorNotifyEvent);
begin
  FOnStateChange := Value;
end;

{*
  [@inheritDoc]
*}
function TFrameCompatibility4xEditor.CanClose: Boolean;
begin
  if not EditSource.Modified then
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

{*
  Notifie un changement de l'état
*}
procedure TFrameCompatibility4xEditor.DoStateChange;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameCompatibility4xEditor.EditSourceChange(Sender: TObject);
begin
  if EditSource.Modified <> FModified then
  begin
    FModified := EditSource.Modified;
    DoStateChange;
  end;
end;

initialization
  SourceFileEditors.Add(C4xExtension, CreateCompatibility4xEditor);
  SourceFileEditors.AddFilter(C4xFilter);
finalization
  SourceFileEditors.Remove(C4xExtension);
  SourceFileEditors.RemoveFilter(C4xFilter);
end.

