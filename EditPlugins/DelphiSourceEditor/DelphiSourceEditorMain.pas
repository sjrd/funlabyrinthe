unit DelphiSourceEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEditHighlighter, SynHighlighterPas, SynEdit, SdDialogs,
  SepiCompiler, SepiParseTrees, SepiCompilerErrors, SepiDelphiCompiler,
  SepiDelphiLexer, SepiDelphiParser, SepiReflectionCore, FilesUtils, UnitFiles,
  SourceEditors, FunLabySourceSynEditEditorFrame;

resourcestring
  DelphiFilter = 'Unit� Delphi (*.pas)|*.pas';

const {don't localize}
  DelphiExtension = 'pas';
  SepiExtension = 'scu';

type
  {*
    �diteur de source Delphi
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
    Filter: DelphiFilter
  );

{*
  Cr�e un �diteur de source Delphi
  @param SourceFile   Fichier source � �diter
  @return Interface vers l'�diteur cr��
*}
function CreateDelphiSourceEditor(SourceFile: TSourceFile): ISourceEditor50;
var
  Editor: TFrameDelphiSourceEditor;
begin
  Editor := TFrameDelphiSourceEditor.Create(nil);
  Editor.LoadFile(SourceFile);
  Result := Editor as ISourceEditor50;
end;

{*
  Cr�e un nouveau fichier source Delphi
  @param FileName   Nom du fichier source
  @return True si le fichier a �t� cr��, False sinon
*}
function CreateDelphiSource(var FileName: TFileName): Boolean;
var
  UnitName: string;
begin
  UnitName := ChangeFileExt(ExtractFileName(FileName), '');

  with TStringList.Create do
  try
    // Don't localize
    Add(Format('unit %s;', [UnitName]));
    Add('');
    Add('interface');
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
  SrcFileName, DestFileName: TFileName;
begin
  SrcFileName := SourceFile.FileName;
  DestFileName := ChangeFileExt(SrcFileName, '.'+SepiExtension);

  Errors.CurrentFileName := SrcFileName;
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
