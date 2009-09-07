unit FunDelphiSourceEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, SdDialogs,
  SepiCompiler, SepiParseTrees, SepiCompilerErrors, SepiFunDelphiCompiler,
  SepiReflectionCore, FilesUtils, UnitFiles, SourceEditors,
  FunLabySourceSynEditEditorFrame, SynHighlighterFunDelphi;

resourcestring
  FunDelphiFilter = 'Unité FunDelphi (*.fnd)|*.fnd';

const {don't localize}
  FunDelphiExtension = 'fnd';
  SepiExtension = 'scu';

type
  {*
    Éditeur de source FunDelphi
    @author sjrd
    @version 5.0
  *}
  TFrameFunDelphiSourceEditor = class(TFrameFunLabySynEditSourceEditor,
    ISourceCompiler50)
  protected
    function CompileFile(SepiRoot: TSepiRoot;
      Errors: TSepiCompilerErrorList): TSepiUnit;

    procedure ISourceCompiler50.Release = Free;
  public
    constructor Create(AOwner: TComponent); override;
  published
    SourceHighlighter: TSynFunDelphiSyn;
  end;

implementation

{$R *.dfm}

{-----------------}
{ Global routines }
{-----------------}

const
  CreateFunDelphiSourceInfo: TSourceFileCreatorInfo = (
    Title: 'Source FunDelphi';
    Description: 'Fichier source FunDelphi, un langage de programmation '+
      'mais puissant adapté particulièrement à FunLabyrinthe';
    AskForFileName: True;
    Filter: FunDelphiFilter
  );

{*
  Crée un éditeur de source FunDelphi
  @param SourceFile   Fichier source à éditer
  @return Interface vers l'éditeur créé
*}
function CreateFunDelphiSourceEditor(SourceFile: TSourceFile): ISourceEditor50;
var
  Editor: TFrameFunDelphiSourceEditor;
begin
  Editor := TFrameFunDelphiSourceEditor.Create(nil);
  Editor.LoadFile(SourceFile);
  Result := Editor as ISourceEditor50;
end;

{*
  Crée un nouveau fichier source FunDelphi
  @param FileName   Nom du fichier source
  @return True si le fichier a été créé, False sinon
*}
function CreateFunDelphiSource(var FileName: TFileName): Boolean;
var
  UnitName: string;
begin
  UnitName := ChangeFileExt(ExtractFileName(FileName), '');

  with TStringList.Create do
  try
    // Don't localize
    Add(Format('unit %s;', [UnitName]));
    Add('');
    Add('end.');

    SaveToFile(FileName);
  finally
    Free;
  end;

  Result := True;
end;

{-----------------------------------}
{ TFrameFunDelphiSourceEditor class }
{-----------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameFunDelphiSourceEditor.Create(AOwner: TComponent);
begin
  inherited;

  SourceHighlighter := TSynFunDelphiSyn.Create(Self);
end;

{*
  [@inheritDoc]
*}
function TFrameFunDelphiSourceEditor.CompileFile(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList): TSepiUnit;
var
  SrcFileName, DestFileName: TFileName;
begin
  SrcFileName := SourceFile.FileName;
  DestFileName := ChangeFileExt(SrcFileName, '.'+SepiExtension);

  Errors.CurrentFileName := SrcFileName;
  Result := CompileFunDelphiSource(SepiRoot, Errors,
    EditSource.Lines, DestFileName);
end;

initialization
  SourceFileEditors.Add(FunDelphiExtension, CreateFunDelphiSourceEditor);
  SourceFileEditors.AddFilter(FunDelphiFilter);

  SourceFileCreators.Add(CreateFunDelphiSource, CreateFunDelphiSourceInfo);
finalization
  SourceFileEditors.Remove(FunDelphiExtension);
  SourceFileEditors.RemoveFilter(FunDelphiFilter);

  SourceFileCreators.Remove(CreateFunDelphiSource);
end.

