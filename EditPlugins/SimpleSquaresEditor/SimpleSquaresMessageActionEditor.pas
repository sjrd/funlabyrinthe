unit SimpleSquaresMessageActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor;

type
  {*
    Cadre d'édition d'une action Afficher un message
    @author sjrd
    @version 5.0
  *}
  TFrameMessageActionEditor = class(TFrameActionEditor)
    RadioGroupKind: TRadioGroup;
    EditDialogTitle: TEdit;
    EditText: TMemo;
    CheckBoxOnlyFirstTime: TCheckBox;
    LabelText: TStaticText;
    LabelDialogTitle: TStaticText;
    procedure CheckBoxOnlyFirstTimeClick(Sender: TObject);
    procedure RadioGroupKindClick(Sender: TObject);
    procedure EditDialogTitleChange(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
  private
    FCurrentAction: TMessageAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TMessageAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{---------------------------------}
{ TFrameMessageActionEditor class }
{---------------------------------}

{*
  [@inheritDoc]
*}
procedure TFrameMessageActionEditor.ActivateAction;
begin
  RadioGroupKind.ItemIndex := Ord(CurrentAction.Kind);
  EditDialogTitle.Text := CurrentAction.DialogTitle;
  EditText.Text := CurrentAction.Text;
  CheckBoxOnlyFirstTime.Checked := CurrentAction.OnlyFirstTime;

  RadioGroupKind.OnClick := RadioGroupKindClick;
  EditDialogTitle.OnChange := EditDialogTitleChange;
  EditText.OnChange := EditTextChange;
  CheckBoxOnlyFirstTime.OnClick := CheckBoxOnlyFirstTimeClick;
end;

{*
  [@inheritDoc]
*}
procedure TFrameMessageActionEditor.DeactivateAction;
begin
  RadioGroupKind.OnClick := nil;
  EditDialogTitle.OnChange := nil;
  EditText.OnChange := nil;
  CheckBoxOnlyFirstTime.OnClick := nil;
end;

{*
  Gestionnaire d'événement OnClick du radio-group type de message
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMessageActionEditor.RadioGroupKindClick(Sender: TObject);
begin
  CurrentAction.Kind := TSimpleMessageKind(RadioGroupKind.ItemIndex);

  if CurrentAction.Kind <> mkCustom then
  begin
    EditDialogTitle.OnChange := nil;
    EditDialogTitle.Text := CurrentAction.DialogTitle;
    EditDialogTitle.OnChange := EditDialogTitleChange;
  end;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange du titre du message
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMessageActionEditor.EditDialogTitleChange(Sender: TObject);
begin
  CurrentAction.DialogTitle := EditDialogTitle.Text;
  RadioGroupKind.ItemIndex := Ord(CurrentAction.Kind);

  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange du texte du message
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMessageActionEditor.EditTextChange(Sender: TObject);
begin
  CurrentAction.Text := EditText.Text;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick de la check-box Premier passage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMessageActionEditor.CheckBoxOnlyFirstTimeClick(Sender: TObject);
begin
  CurrentAction.OnlyFirstTime := CheckBoxOnlyFirstTime.Checked;

  MarkModified;
end;

end.

