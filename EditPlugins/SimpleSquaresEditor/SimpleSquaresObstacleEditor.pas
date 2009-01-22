unit SimpleSquaresObstacleEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils, StdCtrls;

type
  {*
    Cadre d'�dition d'un obstacle
    @author sjrd
    @version 5.0
  *}
  TFrameObstacleEditor = class(TFrame, ISimpleSquaresEditor)
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
    FCurrentObstacle: TSimpleObstacle; /// Obstacle en cours d'�dition

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentObstacle(Value: TSimpleObstacle);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentObstacle: TSimpleObstacle
      read FCurrentObstacle write SetCurrentObstacle;
  end;

implementation

{$R *.dfm}

{----------------------------}
{ TFrameObstacleEditor class }
{----------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameObstacleEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  [@inheritDoc]
*}
function TFrameObstacleEditor.GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'obstacle � �diter
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
  [@inheritDoc]
*}
procedure TFrameObstacleEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick des boutons type de condition
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObstacleEditor.ButtonConditionKindClick(Sender: TObject);
begin
  CurrentObstacle.ConditionKind :=
    TObstacleConditionKind(TComponent(Sender).Tag);
  EditPlayerAction.Enabled := CurrentObstacle.ConditionKind = ockPlayerAction;
  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur action du joueur
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObstacleEditor.EditPlayerActionChange(Sender: TObject);
begin
  CurrentObstacle.PlayerAction := EditPlayerAction.Text;
  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur FailMessage
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObstacleEditor.EditFailMessageChange(Sender: TObject);
begin
  CurrentObstacle.FailMessage := EditFailMessage.Text;
  MarkModified;
end;

end.

