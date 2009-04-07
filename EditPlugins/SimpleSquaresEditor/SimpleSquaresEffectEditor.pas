unit SimpleSquaresEffectEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, StdCtrls, ExtCtrls, Buttons, FunLabyUtils, FunLabyEditOTA,
  SimpleSquaresUtils, SimpleSquaresEditorPart, SimpleSquaresActions,
  SimpleSquaresReplaceSquareActionEditor, SimpleSquaresMessageActionEditor,
  SimpleSquaresDeactivateEffectActionEditor,
  SimpleSquaresPlayerColorActionEditor, SimpleSquaresActionEditor,
  SimpleSquaresSimpleMethodActionEditor;

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
    procedure ListBoxActionsData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure ListBoxActionsClick(Sender: TObject);
    procedure ComboBoxNewActionChange(Sender: TObject);
    procedure ButtonDownActionClick(Sender: TObject);
    procedure ButtonUpActionClick(Sender: TObject);
    procedure ButtonDeleteActionClick(Sender: TObject);
  private
    FCurrentEffect: TSimpleEffect; /// Effet en cours d'�dition
    Actions: TSimpleActionList;    /// Actions de l'effet en cours d'�dition
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
  AddActionEditor(TFrameDeactivateEffectActionEditor.Create(Self));
  AddActionEditor(TFrameMessageActionEditor.Create(Self));
  AddActionEditor(TFramePlayerColorActionEditor.Create(Self));
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
  Modifie l'action � �diter
  @param Value   Nouvelle action
*}
procedure TFrameEffectEditor.SetCurrentAction(Value: TSimpleAction);
begin
  if CurrentActionEditor <> nil then
    CurrentActionEditor.CurrentAction := nil;

  FCurrentAction := Value;

  if CurrentAction is TSimpleActionWithSquare then
  begin
    MapViewer.SelectedSquare :=
      TSimpleActionWithSquare(CurrentAction).SquarePos.GetQPos(
        Self.FunLabyEditMainForm.MasterFile.Master);
    MapViewer.ShowPosition(MapViewer.SelectedSquare);
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
    AddObject('Remplacer la case s�lectionn�e par...',
      TObject(TReplaceSquareAction));
    AddObject('D�sactiver l''effet', TObject(TDeactivateEffectAction));
    AddObject('Afficher un message', TObject(TMessageAction));
    AddObject('Changer la couleur du pion', TObject(TPlayerColorAction));
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
  Gestionnaire d'�v�nement OnData de la liste des actions
  @param Control   Contr�le qui a d�clench� l'�v�nement
  @param Index     Index de la cha�ne demand�e
  @param Data      En sortie : cha�ne � la position indiqu�e
*}
procedure TFrameEffectEditor.ListBoxActionsData(Control: TWinControl;
  Index: Integer; var Data: string);
begin
  Assert(CurrentEffect <> nil);
  Data := CurrentEffect.Actions[Index].Title;
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

