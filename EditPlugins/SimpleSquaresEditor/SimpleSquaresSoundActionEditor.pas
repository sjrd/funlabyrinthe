unit SimpleSquaresSoundActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SimpleSquaresActionEditor, ImgList, StdCtrls, ExtCtrls,
  SimpleSquaresActions, FunLabyUtils, FilesUtils, StrUtils;

type
  {*
    Cadre d'édition d'une action Jouer un son
    @author sjrd
    @version 5.1
  *}
  TFrameSoundActionEditor = class(TFrameActionEditor)
    LabelSound: TLabel;
    EditSound: TButtonedEdit;
    ButtonImages: TImageList;
    OpenSoundDialog: TOpenDialog;
    procedure EditSoundChange(Sender: TObject);
    procedure EditSoundLeftButtonClick(Sender: TObject);
  private
    FCurrentAction: TSoundAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  public
    procedure AfterConstruction; override;
  published
    property CurrentAction: TSoundAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{-------------------------------}
{ TFrameSoundActionEditor class }
{-------------------------------}

{*
  [@inheritDoc]
*}
procedure TFrameSoundActionEditor.ActivateAction;
begin
  EditSound.Text := CurrentAction.Sound;

  EditSound.OnChange := EditSoundChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSoundActionEditor.DeactivateAction;
begin
  EditSound.OnChange := nil;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSoundActionEditor.AfterConstruction;
begin
  inherited;

  OpenSoundDialog.InitialDir :=
    JoinPath([FunLabyAppDataDir, ResourcesDir, ResourceKindToDir[rkSound]]);
end;

{*
  Gestionnaire d'événement OnChange de la zone de texte Son
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameSoundActionEditor.EditSoundChange(Sender: TObject);
begin
  CurrentAction.Sound := EditSound.Text;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnLeftButtonClick de la zone de texte Son
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameSoundActionEditor.EditSoundLeftButtonClick(Sender: TObject);
var
  Dir: TFileName;
  Sound: string;
begin
  if OpenSoundDialog.Execute then
  begin
    Sound := OpenSoundDialog.FileName;

    Dir := IncludeTrailingPathDelimiter(
      JoinPath([FunLabyAppDataDir, ResourcesDir, ResourceKindToDir[rkSound]]));

    if AnsiStartsText(Dir, Sound) then
      Delete(Sound, 1, Length(Dir));

    Sound := AnsiReplaceStr(Sound, '\', '/');

    EditSound.Text := Sound;
  end;
end;

end.

