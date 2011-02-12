unit DelphiSourceEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEditHighlighter, SynHighlighterPas, SynEdit, SdDialogs,
  SepiCompiler, SepiParseTrees, SepiCompilerErrors, SepiDelphiCompiler,
  SepiDelphiLexer, SepiDelphiParser, SepiReflectionCore, FilesUtils,
  SourceEditors, FunLabySourceSynEditEditorFrame;

resourcestring
  DelphiFilter = 'Source Delphi (*.pas)|*.pas';

const {don't localize}
  DelphiExtension = 'pas';
  SepiExtension = 'scu';

type
  {*
    Éditeur de source Delphi
    @author sjrd
    @version 5.0
  *}
  TFrameDelphiSourceEditor = class(TFrameFunLabySynEditSourceEditor,
    ISourceCompiler50)
    SourceHighlighter: TSynPasSyn;
  private
    function CompileFile(SepiRoot: TSepiRoot;
      Errors: TSepiCompilerErrorList): TSepiUnit;

    procedure ISourceCompiler50.Release = Free;
  end;

implementation

{$R *.dfm}

{-----------------}
{ Global routines }
{-----------------}

const
  CreateDelphiSourceInfo: TSourceFileCreatorInfo = (
    Title: 'Source Delphi';
    Description: 'Fichier source Delphi, le langage original de FunLabyrinthe';
    AskForFileName: True;
    Extension: DelphiExtension
  );

{*
  Crée un éditeur de source Delphi
  @param SourceFile   Fichier source à éditer
  @return Interface vers l'éditeur créé
*}
function CreateDelphiSourceEditor(const FileName: TFileName): ISourceEditor50;
var
  Editor: TFrameDelphiSourceEditor;
begin
  Editor := TFrameDelphiSourceEditor.Create(nil);
  Editor.LoadFile(FileName);
  Result := Editor as ISourceEditor50;
end;

{*
  Crée un nouveau fichier source Delphi
  @param FileName   Nom du fichier source
  @return True si le fichier a été créé, False sinon
*}
function CreateDelphiSource(var FileName: TFileName): Boolean;
var
  AUnitName: string;
begin
  AUnitName := ChangeFileExt(ExtractFileName(FileName), '');

  with TStringList.Create do
  try
    // Don't localize
    Add(Format('unit %s;', [AUnitName]));
    Add('');
    Add('interface');
    Add('');
    Add('uses');
    Add('  Types, SysUtils, Classes, TypInfo, Graphics, Contnrs, Controls,');
    Add('  Dialogs, ScUtils, GR32, FunLabyUtils, FunLabyCoreConsts,');
    Add('  FunLabyToolsConsts, Generics, GraphicsTools, MapTools;');
    Add('');
    Add('implementation');
    Add('');
    Add('end.');

    SaveToFile(FileName);
  finally
    Free;
  end;

  Result := True;
end;

{------------------------------}
{ TFrameDelphiUnitEditor class }
{------------------------------}

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.CompileFile(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList): TSepiUnit;
var
  DestFileName: TFileName;
begin
  DestFileName := ChangeFileExt(FileName, '.'+SepiExtension);

  Errors.CurrentFileName := FileName;
  Result := CompileDelphiSource(SepiRoot, Errors,
    EditSource.Lines, DestFileName);
end;

initialization
  SourceFileEditors.Add(DelphiExtension, CreateDelphiSourceEditor);
  SourceFileEditors.AddFilter(DelphiFilter);

  SourceFileCreators.Add(CreateDelphiSource, CreateDelphiSourceInfo);
finalization
  SourceFileEditors.Remove(DelphiExtension);
  SourceFileEditors.RemoveFilter(DelphiFilter);

  SourceFileCreators.Remove(CreateDelphiSource);
end.

