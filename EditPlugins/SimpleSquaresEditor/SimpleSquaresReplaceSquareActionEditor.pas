unit SimpleSquaresReplaceSquareActionEditor;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, Spin, ScUtils, ScStrUtils, SdDialogs,
  GR32, FunLabyUtils, FilesUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresConsts, SimpleSquaresActionEditor,
  FunLabyControls, SimpleSquaresEditorPart, SimpleSquaresSquarePosEditor;

type
  {*
    Cadre d'édition d'une action Remplacer une case par une autre
    @author sjrd
    @version 5.0
  *}
  TFrameReplaceSquareActionEditor = class(TFrameActionEditor)
    GroupBoxReplaceBy: TGroupBox;
    ButtonCopySelectedSquare: TButton;
    CheckBoxReplaceField: TCheckBox;
    CheckBoxReplaceEffect: TCheckBox;
    CheckBoxReplaceTool: TCheckBox;
    CheckBoxReplaceObstacle: TCheckBox;
    ComboBoxField: TFLComponentComboBox;
    ComboBoxEffect: TFLComponentComboBox;
    ComboBoxTool: TFLComponentComboBox;
    ComboBoxObstacle: TFLComponentComboBox;
    FrameSquarePosEditor: TFrameSquarePosEditor;
    procedure CheckBoxReplaceComponentClick(Sender: TObject);
    procedure EditComponentIDChange(Sender: TObject);
    procedure ButtonCopySelectedSquareClick(Sender: TObject);
  private
    CheckBoxReplaceComponent: array[0..3] of TCheckBox;
    EditComponentID: array[0..3] of TFLComponentComboBox;

    MasterFile: TMasterFile;    /// Fichier maître
    MapViewer: IOTAMapViewer50; /// Visualisateur de cartes

    FCurrentAction: TReplaceSquareAction; /// Action courante

    procedure FillIDEdit(CheckBox: TCheckBox; Edit: TFLComponentComboBox;
      ReplaceBy: TReplaceSquareComponent);
    procedure FillIDEdits;
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  public
    procedure AfterConstruction; override;
  published
    property CurrentAction: TReplaceSquareAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{---------------------------------------}
{ TFrameReplaceSquareActionEditor class }
{---------------------------------------}

{*
  Renseigne un éditeur d'ID de composant
  @param Edit        Éditeur à renseigner
  @param ReplaceBy   Remplacement à renseigner dans cet éditeur
*}
procedure TFrameReplaceSquareActionEditor.FillIDEdit(CheckBox: TCheckBox;
  Edit: TFLComponentComboBox; ReplaceBy: TReplaceSquareComponent);
begin
  CheckBox.Checked := ReplaceBy.IsReplaced;
  Edit.Enabled := ReplaceBy.IsReplaced;

  if ReplaceBy.IsReplaced then
  begin
    Edit.Selected := MasterFile.Master.Component[ReplaceBy.ComponentID];
  end else
  begin
    Edit.ItemIndex := -1;
  end;

  CheckBox.OnClick := CheckBoxReplaceComponentClick;
  Edit.OnChange := EditComponentIDChange;
end;

{*
  Renseigne les éditeurs ID de composant d'après l'action courante
*}
procedure TFrameReplaceSquareActionEditor.FillIDEdits;
var
  I: Integer;
begin
  for I := 0 to 3 do
    FillIDEdit(CheckBoxReplaceComponent[I], EditComponentID[I],
      CurrentAction.Components[I]);
end;

{*
  [@inheritDoc]
*}
procedure TFrameReplaceSquareActionEditor.ActivateAction;
var
  I: Integer;
begin
  // Initialize
  if MasterFile = nil then // first time
  begin
    MasterFile := GetFunLabyEditMainForm.MasterFile;

    for I := 0 to 3 do
      EditComponentID[I].Master := MasterFile.Master;

    MapViewer := GetFunLabyEditMainForm.MapViewer;
  end;

  // Activate action

  FrameSquarePosEditor.CurrentSquarePos := CurrentAction.SquarePos;
  FrameSquarePosEditor.Activate;

  FillIDEdits;
end;

{*
  [@inheritDoc]
*}
procedure TFrameReplaceSquareActionEditor.DeactivateAction;
var
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    CheckBoxReplaceComponent[I].OnClick := nil;
    EditComponentID[I].OnChange := nil;
  end;

  FrameSquarePosEditor.Deactivate;
end;

{*
  [@inheritDoc]
*}
procedure TFrameReplaceSquareActionEditor.AfterConstruction;
begin
  inherited;

  CheckBoxReplaceComponent[0] := CheckBoxReplaceField;
  CheckBoxReplaceComponent[1] := CheckBoxReplaceEffect;
  CheckBoxReplaceComponent[2] := CheckBoxReplaceTool;
  CheckBoxReplaceComponent[3] := CheckBoxReplaceObstacle;

  EditComponentID[0] := ComboBoxField;
  EditComponentID[1] := ComboBoxEffect;
  EditComponentID[2] := ComboBoxTool;
  EditComponentID[3] := ComboBoxObstacle;

  ComboBoxField.ComponentClass := TField;
  ComboBoxEffect.ComponentClass := TEffect;
  ComboBoxTool.ComponentClass := TTool;
  ComboBoxObstacle.ComponentClass := TObstacle;
end;

{*
  Gestionnaire d'événement OnClick d'une des check box de remplacement
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.CheckBoxReplaceComponentClick(
  Sender: TObject);
var
  Index: Integer;
  Checked: Boolean;
begin
  Index := (Sender as TCheckBox).Tag;
  Checked := (Sender as TCheckBox).Checked;

  EditComponentID[Index].ItemIndex := IIF(Checked, 0, -1);
  EditComponentID[Index].Enabled := Checked;
  CurrentAction.Components[Index].IsReplaced := Checked;

  if Checked then
    EditComponentIDChange(EditComponentID[Index]);

  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.EditComponentIDChange(
  Sender: TObject);
var
  Index: Integer;
  Component: TFunLabyComponent;
begin
  Index := (Sender as TFLComponentComboBox).Tag;
  Component := (Sender as TFLComponentComboBox).Selected;

  CurrentAction.Components[Index].ComponentID := Component.SafeID;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Imiter la case sélectionnée
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.ButtonCopySelectedSquareClick(
  Sender: TObject);
var
  Square: TSquare;
  I: Integer;
begin
  if (not MapViewer.Visible) or (MapViewer.SelectedMap = nil) then
  begin
    // Nothing selected
    MapViewer.Visible := True;
    ShowDialog(SNoSquareSelectedTitle, SNoSquareSelected, dtError);
    Exit;
  end;

  Square := MapViewer.SelectedSquare.Square;
  for I := 0 to 3 do
    CurrentAction.Components[I].ComponentID := Square.Components[I].SafeID;

  FillIDEdits;
  MarkModified;
end;

end.

