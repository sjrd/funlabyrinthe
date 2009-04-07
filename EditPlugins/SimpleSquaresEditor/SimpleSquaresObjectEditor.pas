unit SimpleSquaresObjectEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Spin, StdCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresEditorPart;

type
  {*
    Cadre d'�dition d'un objet
    @author sjrd
    @version 5.0
  *}
  TFrameObjectEditor = class(TFrameSimpleSquaresEditorPart)
    LabelFindMessage: TLabel;
    EditFindMessage: TMemo;
    CheckBoxHandleAction: TCheckBox;
    EditHandledAction: TEdit;
    LabelMinimumCount: TLabel;
    EditMinimumCount: TSpinEdit;
    CheckBoxDecrementOnUse: TCheckBox;
    procedure EditFindMessageChange(Sender: TObject);
    procedure CheckBoxHandleActionClick(Sender: TObject);
    procedure EditHandledActionChange(Sender: TObject);
    procedure EditMinimumCountChange(Sender: TObject);
    procedure CheckBoxDecrementOnUseClick(Sender: TObject);
  private
    FCurrentObject: TSimpleObject; /// Objet en cours d'�dition

    procedure UpdateHandledActionControls;

    procedure SetCurrentObject(Value: TSimpleObject);
  public
    property CurrentObject: TSimpleObject
      read FCurrentObject write SetCurrentObject;
  end;

implementation

{$R *.dfm}

{--------------------------}
{ TFrameObjectEditor class }
{--------------------------}

{*
  Met � jour les contr�les de l'action g�r�e
*}
procedure TFrameObjectEditor.UpdateHandledActionControls;
begin
  EditHandledAction.Enabled := CheckBoxHandleAction.Checked;
  EditMinimumCount.Enabled := CheckBoxHandleAction.Checked;
  CheckBoxDecrementOnUse.Enabled := CheckBoxHandleAction.Checked;
end;

{*
  Modifie l'objet � �diter
  @param Value   Nouvel objet
*}
procedure TFrameObjectEditor.SetCurrentObject(Value: TSimpleObject);
begin
  if Value = CurrentObject then
    Exit;

  // Deactivate previous effect
  if CurrentObject <> nil then
  begin
    Visible := False;

    EditFindMessage.OnChange := nil;
    CheckBoxHandleAction.OnClick := nil;
    EditMinimumCount.OnChange := nil;
    CheckBoxDecrementOnUse.OnClick := nil;
  end;

  FCurrentObject := Value;

  // Activate new objet
  if CurrentObject <> nil then
  begin
    EditFindMessage.Text := CurrentObject.FindMessage;
    CheckBoxHandleAction.Checked := CurrentObject.HandledAction <> '';
    EditHandledAction.Text := CurrentObject.HandledAction;
    EditMinimumCount.Value := CurrentObject.MinimumCount;
    CheckBoxDecrementOnUse.Checked := CurrentObject.DecrementOnUse;

    UpdateHandledActionControls;

    EditFindMessage.OnChange := EditFindMessageChange;
    CheckBoxHandleAction.OnClick := CheckBoxHandleActionClick;
    EditHandledAction.OnChange := EditHandledActionChange;
    EditMinimumCount.OnChange := EditMinimumCountChange;
    CheckBoxDecrementOnUse.OnClick := CheckBoxDecrementOnUseClick;

    Visible := True;
  end;
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur FindMessage
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObjectEditor.EditFindMessageChange(Sender: TObject);
begin
  CurrentObject.FindMessage := EditFindMessage.Text;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick de la check-box G�re une action
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObjectEditor.CheckBoxHandleActionClick(Sender: TObject);
begin
  UpdateHandledActionControls;

  if not CheckBoxHandleAction.Checked then
    EditHandledAction.Text := '';
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur Action g�r�e
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObjectEditor.EditHandledActionChange(Sender: TObject);
begin
  CurrentObject.HandledAction := EditHandledAction.Text;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnChange de l'�diteur Nombre minimum d'objets
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObjectEditor.EditMinimumCountChange(Sender: TObject);
begin
  CurrentObject.MinimumCount := EditMinimumCount.Value;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick de la check-box D�cr�menter � l'usage
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameObjectEditor.CheckBoxDecrementOnUseClick(Sender: TObject);
begin
  CurrentObject.DecrementOnUse := CheckBoxDecrementOnUse.Checked;

  MarkModified;
end;

end.

