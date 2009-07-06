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
    EditText: TMemo;
    CheckBoxOnlyFirstTime: TCheckBox;
    LabelText: TStaticText;
    procedure EditTextChange(Sender: TObject);
    procedure CheckBoxOnlyFirstTimeClick(Sender: TObject);
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
  EditText.Text := CurrentAction.Text;
  CheckBoxOnlyFirstTime.Checked := CurrentAction.OnlyFirstTime;

  EditText.OnChange := EditTextChange;
  CheckBoxOnlyFirstTime.OnClick := CheckBoxOnlyFirstTimeClick;
end;

{*
  [@inheritDoc]
*}
procedure TFrameMessageActionEditor.DeactivateAction;
begin
  EditText.OnChange := nil;
  CheckBoxOnlyFirstTime.OnClick := nil;
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

