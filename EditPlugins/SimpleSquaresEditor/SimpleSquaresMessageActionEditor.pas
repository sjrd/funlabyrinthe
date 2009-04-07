unit SimpleSquaresMessageActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, StdCtrls, ExtCtrls;

type
  {*
    Cadre d'édition d'une action Afficher un message
    @author sjrd
    @version 5.0
  *}
  TFrameMessageActionEditor = class(TFrame, ISimpleSquaresEditor)
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

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentAction(Value: TMessageAction);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentAction: TMessageAction
      read FCurrentAction write SetCurrentAction;
  end;

implementation

{$R *.dfm}

{---------------------------------}
{ TFrameMessageActionEditor class }
{---------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameMessageActionEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  [@inheritDoc]
*}
function TFrameMessageActionEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action
*}
procedure TFrameMessageActionEditor.SetCurrentAction(
  Value: TMessageAction);
begin
  if CurrentAction <> nil then
  begin
    Visible := False;

    RadioGroupKind.OnClick := nil;
    EditDialogTitle.OnChange := nil;
    EditText.OnChange := nil;
    CheckBoxOnlyFirstTime.OnClick := nil;
  end;

  FCurrentAction := Value;

  if CurrentAction <> nil then
  begin
    RadioGroupKind.ItemIndex := Ord(CurrentAction.Kind);
    EditDialogTitle.Text := CurrentAction.DialogTitle;
    EditText.Text := CurrentAction.Text;
    CheckBoxOnlyFirstTime.Checked := CurrentAction.OnlyFirstTime;

    RadioGroupKind.OnClick := RadioGroupKindClick;
    EditDialogTitle.OnChange := EditDialogTitleChange;
    EditText.OnChange := EditTextChange;
    CheckBoxOnlyFirstTime.OnClick := CheckBoxOnlyFirstTimeClick;

    Visible := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameMessageActionEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
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

