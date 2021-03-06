unit FunDelphiSourceEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, SdDialogs,
  SepiCompiler, SepiParseTrees, SepiCompilerErrors, SepiFunDelphiCompiler,
  SepiReflectionCore, FilesUtils, SourceEditors,
  FunLabySourceSynEditEditorFrame, SynHighlighterFunDelphi;

resourcestring
  FunDelphiFilter = 'Source FunDelphi (*.fnd)|*.fnd';

const {don't localize}
  FunDelphiExtension = 'fnd';
  SepiExtension = 'scu';

type
  {*
    �diteur de source FunDelphi
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
      'mais puissant adapt� particuli�rement � FunLabyrinthe';
    AskForFileName: True;
    Extension: FunDelphiExtension
  );

{*
  Cr�e un �diteur de source FunDelphi
  @param SourceFile   Fichier source � �diter
  @return Interface vers l'�diteur cr��
*}
function CreateFunDelphiSourceEditor(
  const FileName: TFileName): ISourceEditor50;
begin
  Result := TFrameFunDelphiSourceEditor.Create(nil) as ISourceEditor50;
end;

{*
  Cr�e un nouveau fichier source FunDelphi
  @param FileName   Nom du fichier source
  @return True si le fichier a �t� cr��, False sinon
*}
function CreateFunDelphiSource(var FileName: TFileName): Boolean;
var
  AUnitName: string;
begin
  AUnitName := ChangeFileExt(ExtractFileName(FileName), '');

  with TStringList.Create do
  try
    // Don't localize
    Add(Format('unit %s;', [AUnitName]));
    Add('');
    Add('uses');
    Add('  FunLabyBase;');
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
begin
  Errors.CurrentFileName := FileName;
  ForceDirectories(ExtractFilePath(CompilerDestFileName));
  Result := CompileFunDelphiSource(SepiRoot, Errors,
    EditSource.Lines, CompilerDestFileName);
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

