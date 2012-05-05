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
    CheckBoxFullScreen: TCheckBox;
    procedure AnyPropertyChange(Sender: TObject);
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
  CheckBoxFullScreen.Checked := CurrentAction.FullScreen;

  EditText.OnChange := AnyPropertyChange;
  CheckBoxOnlyFirstTime.OnClick := AnyPropertyChange;
  CheckBoxFullScreen.OnClick := AnyPropertyChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameMessageActionEditor.DeactivateAction;
begin
  EditText.OnChange := nil;
  CheckBoxOnlyFirstTime.OnClick := nil;
  CheckBoxFullScreen.OnClick := nil;
end;

{*
  Gestionnaire d'événement OnChange des éléments d'UI
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameMessageActionEditor.AnyPropertyChange(Sender: TObject);
begin
  CurrentAction.Text := EditText.Text;
  CurrentAction.OnlyFirstTime := CheckBoxOnlyFirstTime.Checked;
  CurrentAction.FullScreen := CheckBoxFullScreen.Checked;

  MarkModified;
end;

end.

