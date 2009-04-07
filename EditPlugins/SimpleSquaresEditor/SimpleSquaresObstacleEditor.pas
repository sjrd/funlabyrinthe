unit SimpleSquaresObstacleEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresEditorPart;

type
  {*
    Cadre d'édition d'un obstacle
    @author sjrd
    @version 5.0
  *}
  TFrameObstacleEditor = class(TFrameSimpleSquaresEditorPart)
    ButtonAlways: TRadioButton;
    ButtonNever: TRadioButton;
    ButtonPlayerAction: TRadioButton;
    EditPlayerAction: TEdit;
    LabelFailMessage: TLabel;
    EditFailMessage: TMemo;
    procedure ButtonConditionKindClick(Sender: TObject);
    procedure EditPlayerActionChange(Sender: TObject);
    procedure EditFailMessageChange(Sender: TObject);
  private
    FCurrentObstacle: TSimpleObstacle; /// Obstacle en cours d'édition

    procedure SetCurrentObstacle(Value: TSimpleObstacle);
  public
    property CurrentObstacle: TSimpleObstacle
      read FCurrentObstacle write SetCurrentObstacle;
  end;

implementation

{$R *.dfm}

{----------------------------}
{ TFrameObstacleEditor class }
{----------------------------}

{*
  Modifie l'obstacle à éditer
  @param Value   Nouvel objet
*}
procedure TFrameObstacleEditor.SetCurrentObstacle(Value: TSimpleObstacle);
begin
  if Value = CurrentObstacle then
    Exit;

  // Deactivate previous obstacle
  if CurrentObstacle <> nil then
  begin
    Visible := False;

    ButtonAlways.OnClick := nil;
    ButtonNever.OnClick := nil;
    ButtonPlayerAction.OnClick := nil;
    EditPlayerAction.OnChange := nil;
    EditFailMessage.OnChange := nil;
  end;

  FCurrentObstacle := Value;

  // Activate new obstacle
  if CurrentObstacle <> nil then
  begin
    case CurrentObstacle.ConditionKind of
      ockAlways: ButtonAlways.Checked := True;
      ockNever: ButtonNever.Checked := True;
      ockPlayerAction: ButtonPlayerAction.Checked := True;
    end;

    EditPlayerAction.Enabled := CurrentObstacle.ConditionKind = ockPlayerAction;
    EditPlayerAction.Text := CurrentObstacle.PlayerAction;
    EditFailMessage.Text := CurrentObstacle.FailMessage;

    ButtonAlways.OnClick := ButtonConditionKindClick;
    ButtonNever.OnClick := ButtonConditionKindClick;
    ButtonPlayerAction.OnClick := ButtonConditionKindClick;
    EditPlayerAction.OnChange := EditPlayerActionChange;
    EditFailMessage.OnChange := EditFailMessageChange;

    Visible := True;
  end;
end;

{*
  Gestionnaire d'événement OnClick des boutons type de condition
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObstacleEditor.ButtonConditionKindClick(Sender: TObject);
begin
  CurrentObstacle.ConditionKind :=
    TObstacleConditionKind(TComponent(Sender).Tag);
  EditPlayerAction.Enabled := CurrentObstacle.ConditionKind = ockPlayerAction;
  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur action du joueur
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObstacleEditor.EditPlayerActionChange(Sender: TObject);
begin
  CurrentObstacle.PlayerAction := EditPlayerAction.Text;
  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur FailMessage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObstacleEditor.EditFailMessageChange(Sender: TObject);
begin
  CurrentObstacle.FailMessage := EditFailMessage.Text;
  MarkModified;
end;

end.

