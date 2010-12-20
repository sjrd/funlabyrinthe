unit SimpleSquaresFieldEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresEditorPart;

type
  {*
    Cadre d'édition d'un terrain simple
    @author sjrd
    @version 5.0
  *}
  TFrameFieldEditor = class(TFrameSimpleSquaresEditorPart)
    LabelMessageText: TLabel;
    ButtonAlways: TRadioButton;
    ButtonNever: TRadioButton;
    ButtonPlayerAction: TRadioButton;
    EditPlayerAction: TEdit;
    EditMessageText: TMemo;
    procedure ButtonConditionKindClick(Sender: TObject);
    procedure EditPlayerActionChange(Sender: TObject);
    procedure EditMessageTextChange(Sender: TObject);
  private
    FCurrentField: TSimpleField; /// Terrain en cours d'édition

    procedure SetCurrentField(Value: TSimpleField);
  public
    property CurrentField: TSimpleField
      read FCurrentField write SetCurrentField;
  end;

implementation

{$R *.dfm}

{-------------------------}
{ TFrameFieldEditor class }
{-------------------------}

{*
  Modifie le terrain à éditer
  @param Value   Nouvel objet
*}
procedure TFrameFieldEditor.SetCurrentField(Value: TSimpleField);
begin
  if Value = CurrentField then
    Exit;

  // Deactivate previous field
  if CurrentField <> nil then
  begin
    Visible := False;

    ButtonAlways.OnClick := nil;
    ButtonNever.OnClick := nil;
    ButtonPlayerAction.OnClick := nil;
    EditPlayerAction.OnChange := nil;
    EditMessageText.OnChange := nil;
  end;

  FCurrentField := Value;

  // Activate new field
  if CurrentField <> nil then
  begin
    case CurrentField.ConditionKind of
      fckAlways: ButtonAlways.Checked := True;
      fckNever: ButtonNever.Checked := True;
      fckPlayerAction: ButtonPlayerAction.Checked := True;
    end;

    EditPlayerAction.Enabled := CurrentField.ConditionKind = fckPlayerAction;
    EditPlayerAction.Text := CurrentField.PlayerAction;
    EditMessageText.Text := CurrentField.MessageText;

    ButtonAlways.OnClick := ButtonConditionKindClick;
    ButtonNever.OnClick := ButtonConditionKindClick;
    ButtonPlayerAction.OnClick := ButtonConditionKindClick;
    EditPlayerAction.OnChange := EditPlayerActionChange;
    EditMessageText.OnChange := EditMessageTextChange;

    Visible := True;
  end;
end;

{*
  Gestionnaire d'événement OnClick des boutons type de condition
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameFieldEditor.ButtonConditionKindClick(Sender: TObject);
begin
  CurrentField.ConditionKind :=
    TFieldConditionKind(TComponent(Sender).Tag);
  EditPlayerAction.Enabled := CurrentField.ConditionKind = fckPlayerAction;
  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur action du joueur
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameFieldEditor.EditPlayerActionChange(Sender: TObject);
begin
  CurrentField.PlayerAction := EditPlayerAction.Text;
  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur FailMessage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameFieldEditor.EditMessageTextChange(Sender: TObject);
begin
  CurrentField.MessageText := EditMessageText.Text;
  MarkModified;
end;

end.

