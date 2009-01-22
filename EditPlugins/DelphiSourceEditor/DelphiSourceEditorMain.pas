unit DelphiSourceEditorMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEditHighlighter, SynHighlighterPas, SynEdit, SdDialogs,
  SepiCompiler, SepiParseTrees, SepiCompilerErrors, SepiDelphiCompiler,
  SepiDelphiLexer, SepiDelphiParser, SepiReflectionCore, FilesUtils, UnitFiles,
  SourceEditors;

resourcestring
  DelphiFilter = 'Unit� Delphi (*.pas)|*.pas';

  sConfirmExitTitle = 'Enregistrer le fichier';
  sConfirmExit = 'Le fichier a �t� modifi�. Voulez-vous l''enregistrer ?';

  // Compiler errors
  SCantOpenSourceFile = 'Impossible de cr�er le fichier source %s';
  SCantOpenDestFile = 'Impossible de cr�er le fichier de sortie %s';
  SSepiInternalError = 'Erreur interne : %s';

const {don't localize}
  DelphiExtension = 'pas';
  SepiExtension = 'scu';

type
  {*
    �diteur de source Delphi
    @author sjrd
    @version 5.0
  *}
  TFrameDelphiSourceEditor = class(TFrame, ISourceEditor50, ISourceCompiler50)
    EditSource: TSynEdit;
    DelphiHighlither: TSynPasSyn;
    procedure EditSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditSourceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditSourceSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure EditSourceChange(Sender: TObject);
  private
    FSourceFile: TSourceFile; /// Fichier source
    FModified: Boolean;       /// Indique si le source a �t� modifi�

    FOnStateChange: TSourceEditorNotifyEvent; /// �v�nement OnStateChange

    FErrorLine: Integer; /// Ligne d'erreur

    procedure LoadFile(ASourceFile: TSourceFile);
    function SaveFile: Boolean;

    function CompileFile(SepiRoot: TSepiRoot;
      Errors: TSepiCompilerErrorList): TSepiUnit;
    procedure ShowError(Error: TSepiCompilerError);

    function GetSourceFile: TSourceFile;
    function GetControl: TControl;
    function GetModified: Boolean;

    function GetOnStateChange: TSourceEditorNotifyEvent;
    procedure SetOnStateChange(Value: TSourceEditorNotifyEvent);

    function GetUnitName: string;

    function CanClose: Boolean;
    procedure ISourceEditor50.Release = Free;
    procedure ISourceCompiler50.Release = Free;

    procedure SetErrorLine(Value: Integer);

    property ErrorLine: Integer read FErrorLine write SetErrorLine;
  protected
    procedure DoStateChange; virtual;
  public
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

{-------------------------------}
{ Classe TFrameDelphiUnitEditor }
{-------------------------------}

{*
  Charge un fichier source
  @param ASourceFile   Fichier source � charger
*}
procedure TFrameDelphiSourceEditor.LoadFile(ASourceFile: TSourceFile);
begin
  FSourceFile := ASourceFile;

  EditSource.Lines.LoadFromFile(SourceFile.FileName);

  FModified := False;
  EditSource.Modified := False;
end;

{*
  Enregistre le fichier source
*}
function TFrameDelphiSourceEditor.SaveFile: Boolean;
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
function TFrameDelphiSourceEditor.CompileFile(SepiRoot: TSepiRoot;
  Errors: TSepiCompilerErrorList): TSepiUnit;
var
  SrcFileName, DestFileName: TFileName;
begin
  SrcFileName := Self.SourceFile.FileName;
  DestFileName := ChangeFileExt(SrcFileName, '.'+SepiExtension);

  Errors.CurrentFileName := SrcFileName;
  Result := CompileDelphiSource(SepiRoot, Errors,
    EditSource.Lines, DestFileName);
end;

{*
  [@inheritDoc]
*}
procedure TFrameDelphiSourceEditor.ShowError(Error: TSepiCompilerError);
begin
  EditSource.CaretX := Error.Col;
  EditSource.CaretY := Error.Line;
  EditSource.EnsureCursorPosVisible;
  ErrorLine := Error.Line;
  EditSource.SetFocus;
end;

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.GetSourceFile: TSourceFile;
begin
  Result := FSourceFile;
end;

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.GetControl: TControl;
begin
  Result := Self;
end;

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.GetModified: Boolean;
begin
  Result := FModified;
end;

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.GetOnStateChange: TSourceEditorNotifyEvent;
begin
  Result := FOnStateChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameDelphiSourceEditor.SetOnStateChange(
  Value: TSourceEditorNotifyEvent);
begin
  FOnStateChange := Value;
end;

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.GetUnitName: string;
begin
  Result := ChangeFileExt(ExtractFileName(SourceFile.FileName), '');
end;

{*
  [@inheritDoc]
*}
function TFrameDelphiSourceEditor.CanClose: Boolean;
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
  Notifie un changement de l'�tat
*}
procedure TFrameDelphiSourceEditor.DoStateChange;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

{*
  Modifie le num�ro de ligne d'erreur
  @param Value   Nouvelle ligne d'erreur
*}
procedure TFrameDelphiSourceEditor.SetErrorLine(Value: Integer);
begin
  if Value = FErrorLine then
    Exit;

  if FErrorLine <> 0 then
    EditSource.InvalidateLine(FErrorLine);
  FErrorLine := Value;
  if FErrorLine <> 0 then
    EditSource.InvalidateLine(FErrorLine);
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameDelphiSourceEditor.EditSourceChange(Sender: TObject);
begin
  if EditSource.Modified <> FModified then
  begin
    FModified := EditSource.Modified;
    DoStateChange;
  end;
end;

{*
  D�termine les lignes ayant une couleur sp�ciale dans l'�diteur
  @param Sender    Objet qui a d�clench� l'�v�nement
  @param Line      Ligne � examiner
  @param Special   Position � True pour indiquer que la ligne est sp�ciale
  @param FG        En sortie : couleur du texte
  @param BG        En sortie : couleur de fond
*}
procedure TFrameDelphiSourceEditor.EditSourceSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Line = ErrorLine then
  begin
    Special := True;
    FG := clWhite;
    BG := clMaroon;
  end;
end;

{*
  Gestionnaire d'�v�nement OnKeyDown de l'�diteur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Key      Touche enfonc�e
  @param Shift    �tat des touches sp�ciales
*}
procedure TFrameDelphiSourceEditor.EditSourceKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ErrorLine := 0;
end;

{*
  Gestionnaire d'�v�nement OnMouseDown de l'�diteur
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   Bouton enfonc�
  @param Shift    �tat des touches sp�ciales
  @param X        Abscisse de la souris
  @param Y        Ordonn�e de la souris
*}
procedure TFrameDelphiSourceEditor.EditSourceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ErrorLine := 0;
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

