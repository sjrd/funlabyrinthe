unit FunLabySourceSynEditEditorFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, SynEdit, SynEditHighlighter, SepiCompilerErrors, FilesUtils,
  FunLabyUtils, FunLabySynEdit, FunLabySourceEditorFrame, FunLabyEditOTA;

type
  {*
    Classe de base pour les frames qui doivent impl�menter l'interface
    ISourceEditor50 de FunLabyrinthe, et qui veulent utiliser un TSynEdit pour
    pour le contenu du fichier source.
    TFrameFunLabySynEditSourceEditor se charge de la cr�ation du composant
    TSynEdit. Si un champ publi� nomm� 'SourceHighlighter' est d�couvert, et que
    sa valeur est une instance de TSynCustomHighlighter, elle est li�e � la
    propri�t� Highlither du TSynEdit cr��.
    @author sjrd
    @version 5.0
  *}
  TFrameFunLabySynEditSourceEditor = class(TFrameFunLabySourceEditor,
    IOTAMapViewerEvents51)
  private
    FEditSource: TFunLabySynEdit; /// �diteur du source

    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
  protected
    class function GetEncoding: TEncoding; virtual;

    procedure EditSourceChange(Sender: TObject); dynamic;

    procedure SetupEditSource; dynamic;
    procedure ShowError(Error: TSepiCompilerError); virtual;

    procedure LoadFile(const AFileName: TFileName); override;
    function SaveFile: Boolean; override;

    procedure ClickSquare(const QPos: TQualifiedPos); virtual;
    procedure DblClickSquare(const QPos: TQualifiedPos); virtual;

    property EditSource: TFunLabySynEdit read FEditSource;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AfterConstruction; override;
  end;

const {don't localize}
  SourceHighlighterName = 'SourceHighlighter';

implementation

{$R *.dfm}

uses
  Clipbrd;

{----------------------------------------}
{ TFrameFunLabySynEditSourceEditor class }
{----------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameFunLabySynEditSourceEditor.Create(AOwner: TComponent);
begin
  inherited;

  FEditSource := TFunLabySynEdit.Create(Self);
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySynEditSourceEditor.CMVisibleChanged(var Msg: TMessage);
var
  ParentForm: TCustomForm;
begin
  inherited;

  if Visible then
  begin
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
      ParentForm.ActiveControl := EditSource;
  end;
end;

{*
  Encodage utilis� par cet �diteur
  @return Encodage utilis� par cet �diteur
*}
class function TFrameFunLabySynEditSourceEditor.GetEncoding: TEncoding;
begin
  Result := FunLabyEncoding;
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameFunLabySynEditSourceEditor.EditSourceChange(Sender: TObject);
begin
  Modified := EditSource.Modified;
end;

{*
  Configure l'�diteur EditSource
*}
procedure TFrameFunLabySynEditSourceEditor.SetupEditSource;
var
  HighlighterField: ^TObject;
begin
  // General change of configuration of EditSource
  with EditSource do
  begin
    Align := alClient;
    TabWidth := 2;
    Gutter.ShowLineNumbers := True;
    Options := Options + [eoAltSetsColumnMode, eoTrimTrailingSpaces];

    OnChange := EditSourceChange;
  end;

  // Find and set the highlighter
  HighlighterField := FieldAddress(SourceHighlighterName);
  if (HighlighterField <> nil) and
    (HighlighterField^ is TSynCustomHighlighter) then
  begin
    EditSource.Highlighter := TSynCustomHighlighter(HighlighterField^);
  end;
end;

{*
  Met en �vidence une erreur de compilation Sepi dans l'�diteur
  @param Error   Erreur � mettre en �vidence
*}
procedure TFrameFunLabySynEditSourceEditor.ShowError(Error: TSepiCompilerError);
begin
  EditSource.ShowError(Error);
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySynEditSourceEditor.LoadFile(const AFileName: TFileName);
begin
  inherited;

  EditSource.Lines.LoadFromFile(FileName, GetEncoding);
  EditSource.Modified := False;
end;

{*
  [@inheritDoc]
*}
function TFrameFunLabySynEditSourceEditor.SaveFile: Boolean;
begin
  EditSource.Lines.SaveToFile(FileName, GetEncoding);
  EditSource.Modified := False;

  Result := inherited SaveFile;
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySynEditSourceEditor.ClickSquare(
  const QPos: TQualifiedPos);
begin
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySynEditSourceEditor.DblClickSquare(
  const QPos: TQualifiedPos);
const
  CodeFormat = 'Point3D(%d, %d, %d)';
var
  Code: string;
begin
  Code := Format(CodeFormat, [QPos.X, QPos.Y, QPos.Z]);
  Clipboard.SetTextBuf(PChar(Code));
  EditSource.PasteFromClipboard;
end;

{*
  [@inheritDoc]
*}
procedure TFrameFunLabySynEditSourceEditor.AfterConstruction;
begin
  inherited;

  SetupEditSource;
  EditSource.Parent := Self;
end;

end.

