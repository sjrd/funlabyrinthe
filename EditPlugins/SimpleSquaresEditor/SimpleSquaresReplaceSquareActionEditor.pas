unit SimpleSquaresReplaceSquareActionEditor;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, Spin, ScUtils, ScStrUtils, SdDialogs,
  GR32, FunLabyUtils, FilesUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresConsts, SimpleSquaresActionEditor;

type
  {*
    Cadre d'édition d'une action Remplacer une case par une autre
    @author sjrd
    @version 5.0
  *}
  TFrameReplaceSquareActionEditor = class(TFrameActionEditor)
    GroupBoxReplaceBy: TGroupBox;
    EditEffectID: TComboBoxEx;
    SquaresImages: TImageList;
    EditFieldID: TComboBoxEx;
    EditToolID: TComboBoxEx;
    EditObstacleID: TComboBoxEx;
    ButtonCopySelectedSquare: TButton;
    GroupBoxSquarePos: TGroupBox;
    ButtonCurrentSquare: TRadioButton;
    ButtonAbsolutePos: TRadioButton;
    ButtonResetSquarePos: TButton;
    LabelMap: TLabel;
    EditMapID: TComboBox;
    LabelAbsolutePosX: TLabel;
    CheckBoxReplaceField: TCheckBox;
    CheckBoxReplaceEffect: TCheckBox;
    CheckBoxReplaceTool: TCheckBox;
    CheckBoxReplaceObstacle: TCheckBox;
    EditAbsolutePosX: TSpinEdit;
    EditAbsolutePosY: TSpinEdit;
    EditAbsolutePosZ: TSpinEdit;
    LabelAbsolutePosY: TLabel;
    LabelAbolutePosZ: TLabel;
    procedure ButtonCurrentSquareClick(Sender: TObject);
    procedure ButtonAbsolutePosClick(Sender: TObject);
    procedure ButtonResetSquarePosClick(Sender: TObject);
    procedure CheckBoxReplaceComponentClick(Sender: TObject);
    procedure EditComponentIDChange(Sender: TObject);
    procedure ButtonCopySelectedSquareClick(Sender: TObject);
    procedure EditAbsolutePosEnter(Sender: TObject);
    procedure EditAbsolutePosChange(Sender: TObject);
  private
    CheckBoxReplaceComponent: array[0..3] of TCheckBox;
    EditComponentID: array[0..3] of TComboBoxEx;

    MasterFile: TMasterFile;    /// Fichier maître
    MapViewer: IOTAMapViewer50; /// Visualisateur de cartes

    FCurrentAction: TReplaceSquareAction; /// Action courante

    procedure RegisterComponent(Component: TFunLabyComponent);

    procedure FillSquarePosControls;

    procedure FillIDEdit(CheckBox: TCheckBox; Edit: TComboBoxEx;
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
  Enregistre un composant
  @param Component   Le composant à enregistrer
*}
procedure TFrameReplaceSquareActionEditor.RegisterComponent(
  Component: TFunLabyComponent);
var
  SquareBmp: TBitmap;
  ImageIndex: Integer;
  Category: TComboExItems;
  Item: TComboExItem;
begin
  // Choix de la catégorie
  if Component is TField then
    Category := EditFieldID.ItemsEx
  else if Component is TEffect then
    Category := EditEffectID.ItemsEx
  else if Component is TTool then
    Category := EditToolID.ItemsEx
  else if Component is TObstacle then
    Category := EditObstacleID.ItemsEx
  else
    Exit; // ignore

  // Ajout de l'image du composant dans la liste d'images
  SquareBmp := TBitmap.Create;
  try
    SquareBmp.SetSize(SquareSize, SquareSize);
    Component.DrawIconToCanvas(SquareBmp.Canvas, BaseSquareRect,
      EditFieldID.Color);

    ImageIndex := SquaresImages.Add(SquareBmp, nil);
  finally
    SquareBmp.Free;
  end;

  // Ajout du bouton
  Item := Category.Add;
  Item.Caption := Component.ID;
  Item.ImageIndex := ImageIndex;
end;

{*
  Renseigne les contrôles qui commande la position de la case à modifier
*}
procedure TFrameReplaceSquareActionEditor.FillSquarePosControls;
begin
  ButtonCurrentSquare.OnClick := nil;
  ButtonAbsolutePos.OnClick := nil;
  EditMapID.OnChange := nil;
  EditAbsolutePosX.OnChange := nil;
  EditAbsolutePosY.OnChange := nil;
  EditAbsolutePosZ.OnChange := nil;

  with CurrentAction.SquarePos do
  begin
    case Origin of
      poCurrent: ButtonCurrentSquare.Checked := True;
      poAbsolute: ButtonAbsolutePos.Checked := True;
    else
      Assert(False);
    end;

    EditMapID.Text := MapID;
    EditAbsolutePosX.Value := Offset.X;
    EditAbsolutePosY.Value := Offset.Y;
    EditAbsolutePosZ.Value := Offset.Z;
  end;

  ButtonCurrentSquare.OnClick := ButtonCurrentSquareClick;
  ButtonAbsolutePos.OnClick := ButtonAbsolutePosClick;
  EditMapID.OnChange := EditAbsolutePosChange;
  EditAbsolutePosX.OnChange := EditAbsolutePosChange;
  EditAbsolutePosY.OnChange := EditAbsolutePosChange;
  EditAbsolutePosZ.OnChange := EditAbsolutePosChange;
end;

{*
  Renseigne un éditeur d'ID de composant
  @param Edit        Éditeur à renseigner
  @param ReplaceBy   Remplacement à renseigner dans cet éditeur
*}
procedure TFrameReplaceSquareActionEditor.FillIDEdit(CheckBox: TCheckBox;
  Edit: TComboBoxEx; ReplaceBy: TReplaceSquareComponent);
begin
  CheckBox.Checked := ReplaceBy.IsReplaced;
  Edit.Enabled := ReplaceBy.IsReplaced;

  if ReplaceBy.IsReplaced then
  begin
    Edit.Text := IIF(ReplaceBy.ComponentID = '', SNone, ReplaceBy.ComponentID);
    Edit.ItemIndex := Edit.Items.IndexOf(Edit.Text);
    Edit.OnChange := EditComponentIDChange;
  end else
  begin
    Edit.Text := '';
    Edit.ItemIndex := -1;
  end;

  CheckBox.OnClick := CheckBoxReplaceComponentClick;
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
    for I := 1 to 3 do
      EditComponentID[I].ItemsEx.Add.Caption := SNone;

    MasterFile := GetFunLabyEditMainForm.MasterFile;
    MasterFile.Master.RegisterComponents(RegisterComponent);

    MapViewer := GetFunLabyEditMainForm.MapViewer;
  end;

  // Activate action

  with MasterFile.Master, EditMapID.Items do
  begin
    Clear;
    for I := 0 to MapCount-1 do
      Add(Maps[I].ID);
  end;

  FillSquarePosControls;
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

  ButtonCurrentSquare.OnClick := nil;
  ButtonAbsolutePos.OnClick := nil;
  EditMapID.OnChange := nil;
  EditAbsolutePosX.OnChange := nil;
  EditAbsolutePosY.OnChange := nil;
  EditAbsolutePosZ.OnChange := nil;
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

  EditComponentID[0] := EditFieldID;
  EditComponentID[1] := EditEffectID;
  EditComponentID[2] := EditToolID;
  EditComponentID[3] := EditObstacleID;
end;

{*
  Gestionnaire d'événement OnClick du bouton Case courante
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.ButtonCurrentSquareClick(
  Sender: TObject);
begin
  EditMapID.Text := '';
  EditAbsolutePosX.Value := 0;
  EditAbsolutePosY.Value := 0;
  EditAbsolutePosZ.Value := 0;

  with CurrentAction.SquarePos do
  begin
    Origin := poCurrent;
    MapID := '';
    Offset := Point3D(0, 0, 0);
  end;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Position absolue
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.ButtonAbsolutePosClick(
  Sender: TObject);
begin
  CurrentAction.SquarePos.Origin := poAbsolute;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Resélectionner la case
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.ButtonResetSquarePosClick(
  Sender: TObject);
begin
  if (not MapViewer.Visible) or (MapViewer.SelectedMap = nil) then
  begin
    // Nothing selected
    MapViewer.Visible := True;
    ShowDialog(SNoSquareSelectedTitle, SNoSquareSelected, dtError);
    Exit;
  end;

  CurrentAction.SquarePos.SetToQPos(MapViewer.SelectedSquare);
  FillSquarePosControls;
  MarkModified;
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

  EditComponentID[Index].Text := '';
  EditComponentID[Index].Enabled := Checked;
  CurrentAction.Components[Index].IsReplaced := Checked;

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
  NewID: TComponentID;
begin
  Index := (Sender as TComboBoxEx).Tag;
  NewID := (Sender as TComboBoxEx).Text;
  if NewID = SNone then
    NewID := '';

  CurrentAction.Components[Index].ComponentID := NewID;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnEnter des contrôles de position absolue
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.EditAbsolutePosEnter(Sender: TObject);
begin
  ButtonAbsolutePos.Checked := True;
end;

{*
  Gestionnaire d'événement OnChange des contrôles de position absolue
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.EditAbsolutePosChange(
  Sender: TObject);
begin
  with CurrentAction.SquarePos do
  begin
    MapID := EditMapID.Text;

    Offset := Point3D(EditAbsolutePosX.Value, EditAbsolutePosY.Value,
      EditAbsolutePosZ.Value);
  end;

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

