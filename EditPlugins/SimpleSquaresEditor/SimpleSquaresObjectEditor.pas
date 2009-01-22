unit SimpleSquaresObjectEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils, Spin, StdCtrls;

type
  {*
    Cadre d'édition d'un objet
    @author sjrd
    @version 5.0
  *}
  TFrameObjectEditor = class(TFrame, ISimpleSquaresEditor)
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
    FCurrentObject: TSimpleObject; /// Objet en cours d'édition

    procedure UpdateHandledActionControls;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentObject(Value: TSimpleObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentObject: TSimpleObject
      read FCurrentObject write SetCurrentObject;
  end;

implementation

{$R *.dfm}

{--------------------------}
{ TFrameObjectEditor class }
{--------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameObjectEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  Met à jour les contrôles de l'action gérée
*}
procedure TFrameObjectEditor.UpdateHandledActionControls;
begin
  EditHandledAction.Enabled := CheckBoxHandleAction.Checked;
  EditMinimumCount.Enabled := CheckBoxHandleAction.Checked;
  CheckBoxDecrementOnUse.Enabled := CheckBoxHandleAction.Checked;
end;

{*
  [@inheritDoc]
*}
function TFrameObjectEditor.GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'objet à éditer
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
  [@inheritDoc]
*}
procedure TFrameObjectEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur FindMessage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObjectEditor.EditFindMessageChange(Sender: TObject);
begin
  CurrentObject.FindMessage := EditFindMessage.Text;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick de la check-box Gère une action
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObjectEditor.CheckBoxHandleActionClick(Sender: TObject);
begin
  UpdateHandledActionControls;

  if not CheckBoxHandleAction.Checked then
    EditHandledAction.Text := '';
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur Action gérée
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObjectEditor.EditHandledActionChange(Sender: TObject);
begin
  CurrentObject.HandledAction := EditHandledAction.Text;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'éditeur Nombre minimum d'objets
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObjectEditor.EditMinimumCountChange(Sender: TObject);
begin
  CurrentObject.MinimumCount := EditMinimumCount.Value;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick de la check-box Décrémenter à l'usage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameObjectEditor.CheckBoxDecrementOnUseClick(Sender: TObject);
begin
  CurrentObject.DecrementOnUse := CheckBoxDecrementOnUse.Checked;
end;

end.

