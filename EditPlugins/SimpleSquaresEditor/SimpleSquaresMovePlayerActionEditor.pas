unit SimpleSquaresMovePlayerActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor, SimpleSquaresEditorPart,
  SimpleSquaresSquarePosEditor;

type
  {*
    Cadre d'édition d'une action Déplacer le joueur
    @author sjrd
    @version 5.3
  *}
  TFrameMovePlayerActionEditor = class(TFrameActionEditor)
    FrameSquarePosEditor: TFrameSquarePosEditor;
  private
    FCurrentAction: TMovePlayerAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TMovePlayerAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{------------------------------------}
{ TFrameMovePlayerActionEditor class }
{------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFrameMovePlayerActionEditor.ActivateAction;
begin
  FrameSquarePosEditor.CurrentSquarePos := CurrentAction.SquarePos;
  FrameSquarePosEditor.Activate;
end;

{*
  [@inheritDoc]
*}
procedure TFrameMovePlayerActionEditor.DeactivateAction;
begin
  FrameSquarePosEditor.Deactivate;
end;

end.

