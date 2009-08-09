unit SimpleSquaresReplaceSquareActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions,
  SimpleSquaresConsts, SimpleSquaresActionEditor, GR32;

type
  {*
    Cadre d'édition d'une action Remplacer une case par une autre
    @author sjrd
    @version 5.0
  *}
  TFrameReplaceSquareActionEditor = class(TFrameActionEditor)
    ButtonResetSquarePos: TButton;
    GroupBoxReplaceBy: TGroupBox;
    LabelFieldID: TLabel;
    LabelEffectID: TLabel;
    EditEffectID: TComboBoxEx;
    SquaresImages: TImageList;
    EditFieldID: TComboBoxEx;
    LabelToolID: TLabel;
    EditToolID: TComboBoxEx;
    LabelObstacleID: TLabel;
    EditObstacleID: TComboBoxEx;
    ButtonCopySelectedSquare: TButton;
    procedure ButtonResetSquarePosClick(Sender: TObject);
    procedure ButtonCopySelectedSquareClick(Sender: TObject);
    procedure EditComponentIDChange(Sender: TObject);
  private
    MasterFile: TMasterFile;    /// Fichier maître
    MapViewer: IOTAMapViewer50; /// Visualisateur de cartes

    FCurrentAction: TReplaceSquareAction; /// Action courante

    procedure RegisterComponent(Component: TFunLabyComponent);

    procedure FillIDEdits;
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
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
      WinColor(clBmpTransparent32));

    ImageIndex := SquaresImages.AddMasked(SquareBmp,
      WinColor(clBmpTransparent32));
  finally
    SquareBmp.Free;
  end;

  // Ajout du bouton
  Item := Category.Add;
  Item.Caption := Component.ID;
  Item.ImageIndex := ImageIndex;
end;

{*
  Renseigne les éditeurs ID de composant d'après l'action courante
*}
procedure TFrameReplaceSquareActionEditor.FillIDEdits;
begin
  with CurrentAction.ReplaceBy do
  begin
    EditFieldID.Text := FieldID;
    EditFieldID.ItemIndex := EditFieldID.Items.IndexOf(EditFieldID.Text);

    EditEffectID.Text := IIF(EffectID = '', SNone, EffectID);
    EditEffectID.ItemIndex := EditEffectID.Items.IndexOf(EditEffectID.Text);

    EditToolID.Text := IIF(ToolID = '', SNone, ToolID);
    EditToolID.ItemIndex := EditToolID.Items.IndexOf(EditToolID.Text);

    EditObstacleID.Text := IIF(ObstacleID = '', SNone, ObstacleID);
    EditObstacleID.ItemIndex :=
      EditObstacleID.Items.IndexOf(EditObstacleID.Text);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameReplaceSquareActionEditor.ActivateAction;
begin
  // Initialize
  if MasterFile = nil then // first time
  begin
    EditEffectID.ItemsEx.Add.Caption := SNone;
    EditToolID.ItemsEx.Add.Caption := SNone;
    EditObstacleID.ItemsEx.Add.Caption := SNone;

    MasterFile := GetFunLabyEditMainForm.MasterFile;
    MasterFile.Master.RegisterComponents(RegisterComponent);

    MapViewer := GetFunLabyEditMainForm.MapViewer;
  end;

  // Never have an action with FieldID = ''
  with CurrentAction do
  begin
    if ReplaceBy.FieldID = '' then
    begin
      // Just created - use selected square as base
      ReplaceBy.SetToSquare(
        MasterFile.Master.Map[SquarePos.MapID].Map[SquarePos.Position]);
    end;
  end;

  // Activate action
  
  FillIDEdits;

  EditFieldID.OnChange := EditComponentIDChange;
  EditEffectID.OnChange := EditComponentIDChange;
  EditToolID.OnChange := EditComponentIDChange;
  EditObstacleID.OnChange := EditComponentIDChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameReplaceSquareActionEditor.DeactivateAction;
begin
  EditFieldID.OnChange := nil;
  EditEffectID.OnChange := nil;
  EditToolID.OnChange := nil;
  EditObstacleID.OnChange := nil;
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
    Exit;
  end;

  CurrentAction.SquarePos.SetToQPos(MapViewer.SelectedSquare);
  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Imiter la case sélectionnée
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.ButtonCopySelectedSquareClick(
  Sender: TObject);
begin
  if (not MapViewer.Visible) or (MapViewer.SelectedMap = nil) then
  begin
    // Nothing selected
    MapViewer.Visible := True;
    Exit;
  end;

  CurrentAction.ReplaceBy.SetToSquare(
    MapViewer.SelectedMap[MapViewer.SelectedPos]);
  FillIDEdits;
  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameReplaceSquareActionEditor.EditComponentIDChange(
  Sender: TObject);
var
  NewID: TComponentID;
begin
  NewID := (Sender as TComboBoxEx).Text;
  if NewID = SNone then
    NewID := '';

  with CurrentAction.ReplaceBy do
  begin
    if Sender = EditFieldID then
      FieldID := NewID
    else if Sender = EditEffectID then
      EffectID := NewID
    else if Sender = EditToolID then
      ToolID := NewID
    else
      ObstacleID := NewID;
  end;

  MarkModified;
end;

end.

