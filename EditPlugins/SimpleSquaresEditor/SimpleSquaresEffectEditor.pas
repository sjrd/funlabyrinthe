unit SimpleSquaresEffectEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils, StdCtrls, ExtCtrls,
  Buttons, SimpleSquaresActions, SimpleSquaresReplaceSquareActionEditor,
  SimpleSquaresMessageActionEditor, SimpleSquaresDeactivateEffectActionEditor,
  SimpleSquaresPlayerColorActionEditor, SimpleSquaresActionEditor,
  SimpleSquaresEditorPart;

type
  {*
    Cadre d'édition d'un effet
    @author sjrd
    @version 5.0
  *}
  TFrameEffectEditor = class(TFrameSimpleSquaresEditorPart)
    PanelActionList: TPanel;
    SplitterActionList: TSplitter;
    LabelActionList: TLabel;
    ListBoxActions: TListBox;
    ComboBoxNewAction: TComboBox;
    ButtonDeleteAction: TSpeedButton;
    ButtonUpAction: TSpeedButton;
    ButtonDownAction: TSpeedButton;
    procedure ListBoxActionsData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure ListBoxActionsClick(Sender: TObject);
    procedure ComboBoxNewActionChange(Sender: TObject);
    procedure ButtonDownActionClick(Sender: TObject);
    procedure ButtonUpActionClick(Sender: TObject);
    procedure ButtonDeleteActionClick(Sender: TObject);
  private
    FCurrentEffect: TSimpleEffect; /// Effet en cours d'édition
    Actions: TSimpleActionList;    /// Actions de l'effet en cours d'édition
    FCurrentAction: TSimpleAction; /// Action courante

    MapViewer: IOTAMapViewer50;

    /// Éditeur d'action Remplacement de case
    ReplaceSquareActionEditor: TFrameActionEditor;
    /// Éditeur d'action Remplacement de case
    DeactivateEffectActionEditor: TFrameActionEditor;
    /// Éditeur d'action Afficher un message
    MessageActionEditor: TFrameActionEditor;
    /// Éditeur d'action Changer la couleur du pion
    PlayerColorActionEditor: TFrameActionEditor;

    procedure UpdateActions;

    procedure SetCurrentEffect(Value: TSimpleEffect);
    procedure SetCurrentAction(Value: TSimpleAction);

    property CurrentAction: TSimpleAction
      read FCurrentAction write SetCurrentAction;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AfterConstruction; override;

    procedure MarkModified; override;

    property FunLabyEditMainForm: IOTAFunLabyEditMainForm50
      read GetFunLabyEditMainForm;

    property CurrentEffect: TSimpleEffect
      read FCurrentEffect write SetCurrentEffect;
  end;

implementation

{$R *.dfm}

{--------------------------}
{ TFrameEffectEditor class }
{--------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameEffectEditor.Create(AOwner: TComponent);
begin
  inherited;

  ReplaceSquareActionEditor := TFrameReplaceSquareActionEditor.Create(Self);
  ReplaceSquareActionEditor.Parent := Self;

  DeactivateEffectActionEditor :=
    TFrameDeactivateEffectActionEditor.Create(Self);
  DeactivateEffectActionEditor.Parent := Self;

  MessageActionEditor := TFrameMessageActionEditor.Create(Self);
  MessageActionEditor.Parent := Self;

  PlayerColorActionEditor := TFramePlayerColorActionEditor.Create(Self);
  PlayerColorActionEditor.Parent := Self;
end;

{*
  Met à jour les contrôles en lien avec les actions
*}
procedure TFrameEffectEditor.UpdateActions;
var
  Index: Integer;
begin
  Index := ListBoxActions.ItemIndex;

  if (Index < 0) and (Actions.Count > 0) then
  begin
    ListBoxActions.ItemIndex := 0;
    Index := 0;
  end;

  ButtonDeleteAction.Enabled := Index >= 0;
  ButtonUpAction.Enabled := Index > 0;
  ButtonDownAction.Enabled := Index < Actions.Count-1;

  if Index < 0 then
    CurrentAction := nil
  else
    CurrentAction := Actions[Index];
end;

{*
  Modifie l'effet à éditer
  @param Value   Nouvel effet
*}
procedure TFrameEffectEditor.SetCurrentEffect(Value: TSimpleEffect);
begin
  if Value = CurrentEffect then
    Exit;

  // Deactivate previous effect
  if CurrentEffect <> nil then
  begin
    Visible := False;

    CurrentAction := nil;
    ListBoxActions.Count := 0;
    Actions := nil;
  end;

  FCurrentEffect := Value;

  // Activate new effect
  if CurrentEffect <> nil then
  begin
    if MapViewer = nil then
      MapViewer := GetFunLabyEditMainForm.MapViewer;

    Actions := CurrentEffect.Actions;
    ListBoxActions.Count := Actions.Count;

    Visible := True;
  end;
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action
*}
procedure TFrameEffectEditor.SetCurrentAction(Value: TSimpleAction);
begin
  if CurrentAction is TReplaceSquareAction then
    ReplaceSquareActionEditor.CurrentAction := nil
  else if CurrentAction is TDeactivateEffectAction then
    DeactivateEffectActionEditor.CurrentAction := nil
  else if CurrentAction is TMessageAction then
    MessageActionEditor.CurrentAction := nil
  else if CurrentAction is TPlayerColorAction then
    PlayerColorActionEditor.CurrentAction := nil;

  FCurrentAction := Value;

  if CurrentAction is TSimpleActionWithSquare then
  begin
    MapViewer.SelectedSquare :=
      TSimpleActionWithSquare(CurrentAction).SquarePos.GetQPos(
        Self.FunLabyEditMainForm.MasterFile.Master);
    MapViewer.ShowPosition(MapViewer.SelectedSquare);
  end;

  if CurrentAction is TReplaceSquareAction then
    ReplaceSquareActionEditor.CurrentAction := CurrentAction
  else if CurrentAction is TDeactivateEffectAction then
    DeactivateEffectActionEditor.CurrentAction := CurrentAction
  else if CurrentAction is TMessageAction then
    MessageActionEditor.CurrentAction := CurrentAction
  else if CurrentAction is TPlayerColorAction then
    PlayerColorActionEditor.CurrentAction := CurrentAction;
end;

{*
  [@inheritDoc]
*}
procedure TFrameEffectEditor.AfterConstruction;
begin
  inherited;

  with ComboBoxNewAction.Items do
  begin
    AddObject('Remplacer la case sélectionnée par...',
      TObject(TReplaceSquareAction));
    AddObject('Désactiver l''effet', TObject(TDeactivateEffectAction));
    AddObject('Afficher un message', TObject(TMessageAction));
    AddObject('Changer la couleur du pion', TObject(TPlayerColorAction));
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameEffectEditor.MarkModified;
begin
  ListBoxActions.Invalidate;

  inherited;
end;

{*
  Gestionnaire d'événement OnData de la liste des actions
  @param Control   Contrôle qui a déclenché l'événement
  @param Index     Index de la chaîne demandée
  @param Data      En sortie : chaîne à la position indiquée
*}
procedure TFrameEffectEditor.ListBoxActionsData(Control: TWinControl;
  Index: Integer; var Data: string);
begin
  Assert(CurrentEffect <> nil);
  Data := CurrentEffect.Actions[Index].Title;
end;

{*
  Gestionnaire d'événement OnClick de la liste des actions
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEffectEditor.ListBoxActionsClick(Sender: TObject);
begin
  UpdateActions;
end;

{*
  Gestionnaire d'événement OnChange de la combo Ajouter une action
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEffectEditor.ComboBoxNewActionChange(Sender: TObject);
var
  ActionClass: TSimpleActionClass;
  Action: TSimpleAction;
begin
  ActionClass := TSimpleActionClass(
    ComboBoxNewAction.Items.Objects[ComboBoxNewAction.ItemIndex]);

  if ActionClass <> nil then
  begin
    ComboBoxNewAction.ItemIndex := 0;

    if ActionClass.InheritsFrom(TSimpleActionWithSquare) and
      ((not MapViewer.Visible) or (MapViewer.SelectedMap = nil)) then
    begin
      MapViewer.Visible := True;
      Exit;
    end;

    Action := Actions.AddNew(ActionClass);
    ListBoxActions.Count := Actions.Count;

    if Action is TSimpleActionWithSquare then
    begin
      TSimpleActionWithSquare(Action).SquarePos.SetToQPos(
        MapViewer.SelectedSquare);
    end;

    MarkModified;

    ListBoxActions.Count := Actions.Count;
    ListBoxActions.ItemIndex := Actions.Count-1;

    UpdateActions;
  end;
end;

{*
  Gestionnaire d'événement OnClick du bouton Supprimer l'action
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEffectEditor.ButtonDeleteActionClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxActions.ItemIndex;
  if Index < 0 then
    Exit;

  CurrentAction := nil;
  Actions.Delete(Index);
  ListBoxActions.Count := Actions.Count;

  MarkModified;

  if Index < Actions.Count then
    ListBoxActions.ItemIndex := Index
  else
    ListBoxActions.ItemIndex := Index-1;

  UpdateActions;
end;

{*
  Gestionnaire d'événement OnClick du bouton Monter l'action
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEffectEditor.ButtonUpActionClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxActions.ItemIndex;
  if Index <= 0 then
    Exit;

  Actions.Exchange(Index, Index-1);
  ListBoxActions.ItemIndex := Index-1;

  MarkModified;

  UpdateActions;
end;

{*
  Gestionnaire d'événement OnClick du bouton Descendre l'action
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEffectEditor.ButtonDownActionClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxActions.ItemIndex;
  if (Index < 0) or (Index >= Actions.Count-1) then
    Exit;

  Actions.Exchange(Index, Index+1);
  ListBoxActions.ItemIndex := Index+1;

  MarkModified;

  UpdateActions;
end;

end.

