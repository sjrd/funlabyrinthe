unit Compatibility4xEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SdDialogs, FilesUtils, SourceEditors, Compatibility4xSyn,
  FunLabySourceSynEditEditorFrame;

resourcestring
  C4xFilter = 'Actions de compatibilité 4.x (*.c4x)|*.c4x';

const {don't localize}
  C4xExtension = 'c4x';

type
  {*
    Éditeur de fichiers d'actions de compatibilité FunLabyrinthe 4.x
    @author sjrd
    @version 5.0
  *}
  TFrameCompatibility4xEditor = class(TFrameFunLabySynEditSourceEditor)
  public
    constructor Create(AOwner: TComponent); override;
  published
    SourceHighlighter: TCompatibility4xSyntax; /// Highlighter
  end;

implementation

{$R *.dfm}

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

  SourceHighlighter := TCompatibility4xSyntax.Create(Self);
end;

initialization
  SourceFileEditors.Add(C4xExtension, CreateCompatibility4xEditor);
  SourceFileEditors.AddFilter(C4xFilter);
finalization
  SourceFileEditors.Remove(C4xExtension);
  SourceFileEditors.RemoveFilter(C4xFilter);
end.

