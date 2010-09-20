unit SimpleSquaresObstacleEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresEditorPart;

resourcestring
  SLabelMessageTextCaptionIfAlways =
    'Message à afficher lorsque le joueur a détruit l''obstacle :';
  SLabelMessageTextCaptionIfNever =
    'Message à afficher lorsque le joueur pousse sur l''obstacle :';
  SLabelMessageTextCaptionIfAction =
    'Message à afficher lorsque le joueur tente, sans succès, de détruire '+
    'l''obstacle :';

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
    LabelMessageText: TLabel;
    EditMessageText: TMemo;
    procedure ButtonConditionKindClick(Sender: TObject);
    procedure EditPlayerActionChange(Sender: TObject);
    procedure EditMessageTextChange(Sender: TObject);
  private
    FCurrentObstacle: TSimpleObstacle; /// Obstacle en cours d'édition

    procedure UpdateLabelMessageTextCaption;

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
  Met à jour le caption du label introduisant le message
*}
procedure TFrameObstacleEditor.UpdateLabelMessageTextCaption;
begin
  case CurrentObstacle.ConditionKind of
    ockAlways:
      LabelMessageText.Caption := SLabelMessageTextCaptionIfAlways;
    ockNever:
      LabelMessageText.Caption := SLabelMessageTextCaptionIfNever;
    ockPlayerAction:
      LabelMessageText.Caption := SLabelMessageTextCaptionIfAction;
  end;
end;

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
    EditMessageText.OnChange := nil;
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
    EditMessageText.Text := CurrentObstacle.MessageText;

    ButtonAlways.OnClick := ButtonConditionKindClick;
    ButtonNever.OnClick := ButtonConditionKindClick;
    ButtonPlayerAction.OnClick := ButtonConditionKindClick;
    EditPlayerAction.OnChange := EditPlayerActionChange;
    EditMessageText.OnChange := EditMessageTextChange;

    UpdateLabelMessageTextCaption;

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
  UpdateLabelMessageTextCaption;
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
procedure TFrameObstacleEditor.EditMessageTextChange(Sender: TObject);
begin
  CurrentObstacle.MessageText := EditMessageText.Text;
  MarkModified;
end;

end.

