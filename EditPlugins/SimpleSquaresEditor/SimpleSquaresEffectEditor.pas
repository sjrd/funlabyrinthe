unit SimpleSquaresEffectEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, StdCtrls, ExtCtrls, Buttons, FunLabyUtils, FunLabyEditOTA,
  SimpleSquaresUtils, SimpleSquaresEditorPart, SimpleSquaresActions,
  SimpleSquaresReplaceSquareActionEditor, SimpleSquaresMessageActionEditor,
  SimpleSquaresSoundActionEditor,
  SimpleSquaresChangeEffectEnabledActionEditor,
  SimpleSquaresPlayerColorActionEditor, SimpleSquaresActionEditor,
  SimpleSquaresMovePlayerActionEditor, SimpleSquaresSimpleMethodActionEditor;

type
  {*
    Cadre d'�dition d'un effet
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
    PanelEvent: TPanel;
    LabelEvent: TLabel;
    ComboEvent: TComboBox;
    procedure ListBoxActionsData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure ListBoxActionsClick(Sender: TObject);
    procedure ComboBoxNewActionChange(Sender: TObject);
    procedure ButtonDownActionClick(Sender: TObject);
    procedure ButtonUpActionClick(Sender: TObject);
    procedure ButtonDeleteActionClick(Sender: TObject);
    procedure ComboEventChange(Sender: TObject);
  private
    FCurrentEffect: TSimpleEffect; /// Effet en cours d'�dition
    Actions: TSimpleActionList;    /// Actions de l'�v�nement en cours d'�dition
    FCurrentAction: TSimpleAction; /// Action courante

    MapViewer: IOTAMapViewer50; /// Visualisateur de cartes

    ActionEditors: TObjectList;              /// �diteurs d'actions disponibles
    CurrentActionEditor: TFrameActionEditor; /// �diteur d'action courante

    procedure UpdateActions;

    procedure AddActionEditor(ActionEditor: TFrameActionEditor);
    function GetActionEditor(Action: TSimpleAction): TFrameActionEditor;

    procedure SetCurrentEffect(Value: TSimpleEffect);
    procedure SetCurrentAction(Value: TSimpleAction);

    property CurrentAction: TSimpleAction
      read FCurrentAction write SetCurrentAction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

  ActionEditors := TObjectList.Create(False);

  AddActionEditor(TFrameReplaceSquareActionEditor.Create(Self));
  AddActionEditor(TFrameChangeEffectEnabledActionEditor.Create(Self));
  AddActionEditor(TFrameMessageActionEditor.Create(Self));
  AddActionEditor(TFrameSoundActionEditor.Create(Self));
  AddActionEditor(TFramePlayerColorActionEditor.Create(Self));
  AddActionEditor(TFrameMovePlayerActionEditor.Create(Self));
  AddActionEditor(TFrameSimpleMethodActionEditor.Create(Self));
end;

{*
  [@inheritDoc]
*}
destructor TFrameEffectEditor.Destroy;
begin
  ActionEditors.Free;

  inherited;
end;

{*
  Met � jour les contr�les en lien avec les actions
*}
procedure TFrameEffectEditor.UpdateActions;
var
  Index, ActionCount: Integer;
begin
  Index := ListBoxActions.ItemIndex;

  if Actions = nil then
    ActionCount := 0
  else
    ActionCount := Actions.Count;

  if (Index < 0) and (ActionCount > 0) then
  begin
    ListBoxActions.ItemIndex := 0;
    Index := 0;
  end;

  ButtonDeleteAction.Enabled := Index >= 0;
  ButtonUpAction.Enabled := Index > 0;
  ButtonDownAction.Enabled := Index < ActionCount-1;

  if Index < 0 then
    CurrentAction := nil
  else
    CurrentAction := Actions[Index];
end;

{*
  Ajoute un �diteur d'actions
  @param ActionEditor   �diteur d'action � ajouter
*}
procedure TFrameEffectEditor.AddActionEditor(ActionEditor: TFrameActionEditor);
begin
  ActionEditor.Parent := Self;
  ActionEditors.Add(ActionEditor);
end;

{*
  Obtient un �diteur d'action pour une action donn�e
  @param Action   Action pour laquelle obtenir un �diteur
  @return �diteur d'action, ou nil si aucun �diteur d'action n'est adapt�
*}
function TFrameEffectEditor.GetActionEditor(
  Action: TSimpleAction): TFrameActionEditor;
var
  I: Integer;
begin
  for I := 0 to ActionEditors.Count-1 do
  begin
    Result := TFrameActionEditor(ActionEditors.Items[I]);
    if Action is Result.ActionClass then
      Exit;
  end;

  Result := nil;
end;

{*
  Modifie l'effet � �diter
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
    ComboEvent.ItemIndex := -1;
    ComboEventChange(nil);
    ComboEvent.Items.Clear;
  end;

  FCurrentEffect := Value;

  // Activate new effect
  if CurrentEffect <> nil then
  begin
    if MapViewer = nil then
      MapViewer := GetFunLabyEditMainForm.MapViewer;

    CurrentEffect.GetEventsAndActions(ComboEvent.Items);
    ComboEvent.ItemIndex := ComboEvent.Items.IndexOf(
      CurrentEffect.DefaultEvent);
    ComboEventChange(nil);

    Visible := True;
  end;
end;

{*
  Modifie l'action � �diter
  @param Value   Nouvelle action
*}
procedure TFrameEffectEditor.SetCurrentAction(Value: TSimpleAction);
var
  SquarePos: TSquarePos;
  Master: TMaster;
begin
  if CurrentActionEditor <> nil then
    CurrentActionEditor.CurrentAction := nil;

  FCurrentAction := Value;

  if CurrentAction is TSimpleActionWithSquare then
  begin
    SquarePos := TSimpleActionWithSquare(CurrentAction).SquarePos;
    Master := FunLabyEditMainForm.MasterFile.Master;

    if (SquarePos.Origin = poAbsolute) and
      Master.ComponentExists(SquarePos.MapID) then
    begin
      MapViewer.SelectedSquare := SquarePos.GetQPos(Master);
      MapViewer.ShowPosition(MapViewer.SelectedSquare);
    end;
  end;

  CurrentActionEditor := GetActionEditor(CurrentAction);
  if CurrentActionEditor <> nil then
    CurrentActionEditor.CurrentAction := CurrentAction;
end;

{*
  [@inheritDoc]
*}
procedure TFrameEffectEditor.AfterConstruction;
begin
  inherited;

  with ComboBoxNewAction.Items do
  begin
    AddObject('Modifier une case', TObject(TReplaceSquareAction));
    AddObject('Activer/d�sactiver un effet',
      TObject(TChangeEffectEnabledAction));
    AddObject('Afficher un message', TObject(TMessageAction));
    AddObject('Jouer un son', TObject(TSoundAction));
    AddObject('Changer la couleur du pion', TObject(TPlayerColorAction));
    AddObject('D�placer le joueur', TObject(TMovePlayerAction));
    AddObject('Action simple', TObject(TSimpleMethodAction));
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
  Gestionnaire d'�v�nement OnChange de la liste des �v�nements
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameEffectEditor.ComboEventChange(Sender: TObject);
begin
  if ComboEvent.ItemIndex < 0 then
  begin
    ListBoxActions.Count := 0;
    Actions := nil;
  end else
  begin
    Actions := CurrentEffect.EventActions[ComboEvent.ItemIndex];
    ListBoxActions.Count := Actions.Count;
  end;

  UpdateActions;
end;

{*
  Gestionnaire d'�v�nement OnData de la liste des actions
  @param Control   Contr�le qui a d�clench� l'�v�nement
  @param Index     Index de la cha�ne demand�e
  @param Data      En sortie : cha�ne � la position indiqu�e
*}
procedure TFrameEffectEditor.ListBoxActionsData(Control: TWinControl;
  Index: Integer; var Data: string);
begin
  Assert(Actions <> nil);
  Data := Actions[Index].Title;
end;

{*
  Gestionnaire d'�v�nement OnClick de la liste des actions
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameEffectEditor.ListBoxActionsClick(Sender: TObject);
begin
  UpdateActions;
end;

{*
  Gestionnaire d'�v�nement OnChange de la combo Ajouter une action
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameEffectEditor.ComboBoxNewActionChange(Sender: TObject);
var
  ActionClass: TSimpleActionClass;
begin
  ActionClass := TSimpleActionClass(
    ComboBoxNewAction.Items.Objects[ComboBoxNewAction.ItemIndex]);

  if ActionClass <> nil then
  begin
    ComboBoxNewAction.ItemIndex := 0;

    Actions.AddNew(ActionClass);
    ListBoxActions.Count := Actions.Count;
    ListBoxActions.ItemIndex := Actions.Count-1;

    MarkModified;

    UpdateActions;
  end;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Supprimer l'action
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnClick du bouton Monter l'action
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnClick du bouton Descendre l'action
  @param Sender   Objet qui a d�clench� l'�v�nement
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

