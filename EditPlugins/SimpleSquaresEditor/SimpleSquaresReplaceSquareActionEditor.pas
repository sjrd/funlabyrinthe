unit SimpleSquaresReplaceSquareActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions,
  SimpleSquaresConsts;

type
  {*
    Cadre d'édition d'une action Remplacer une case par une autre
    @author sjrd
    @version 5.0
  *}
  TFrameReplaceSquareActionEditor = class(TFrame, ISimpleSquaresEditor)
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

    procedure RegisterSingleComponent(Component: TSquareComponent); stdcall;
    procedure RegisterComponentSet(Template: TSquareComponent;
      const Components: array of TSquareComponent; BaseIndex: Integer;
      const DialogTitle, DialogPrompt: string); stdcall;

    procedure FillIDEdits;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentAction(Value: TReplaceSquareAction);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentAction: TReplaceSquareAction
      read FCurrentAction write SetCurrentAction;
  end;

implementation

{$R *.dfm}

{---------------------------------------}
{ TFrameReplaceSquareActionEditor class }
{---------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameReplaceSquareActionEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  Enregistre un unique composant
  @param Component   Le composant à enregistrer
*}
procedure TFrameReplaceSquareActionEditor.RegisterSingleComponent(
  Component: TSquareComponent);
var
  SquareBmp: TSquareBitmap;
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
  SquareBmp := TSquareBitmap.Create;
  try
    Component.Draw(NoQPos, SquareBmp.Canvas);
    ImageIndex := SquaresImages.AddMasked(SquareBmp, clTransparent);
  finally
    SquareBmp.Free;
  end;

  // Ajout du bouton
  Item := Category.Add;
  Item.Caption := Component.ID;
  Item.ImageIndex := ImageIndex;
end;

{*
  Enregistre un ensemble de composants
  @param Template       Composant modèle pour l'image et le nom à afficher
  @param Components     Liste des composants faisant partie de l'ensemble
  @param DialogTitle    Titre de la boîte de dialogue du choix du numéro
  @param DialogPrompt   Invite de la boîte de dialogue du choix du numéro
*}
procedure TFrameReplaceSquareActionEditor.RegisterComponentSet(
  Template: TSquareComponent; const Components: array of TSquareComponent;
  BaseIndex: Integer; const DialogTitle, DialogPrompt: string);
begin
  // No support for component sets
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
function TFrameReplaceSquareActionEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action
*}
procedure TFrameReplaceSquareActionEditor.SetCurrentAction(
  Value: TReplaceSquareAction);
begin
  if CurrentAction <> nil then
  begin
    Visible := False;

    EditFieldID.OnChange := nil;
    EditEffectID.OnChange := nil;
    EditToolID.OnChange := nil;
    EditObstacleID.OnChange := nil;
  end;

  FCurrentAction := Value;

  if CurrentAction <> nil then
  begin
    if MasterFile = nil then // first time
    begin
      EditEffectID.ItemsEx.Add.Caption := SNone;
      EditToolID.ItemsEx.Add.Caption := SNone;
      EditObstacleID.ItemsEx.Add.Caption := SNone;

      MasterFile := GetFunLabyEditMainForm.MasterFile;
      MasterFile.RegisterComponents(RegisterSingleComponent,
        RegisterComponentSet);

      MapViewer := GetFunLabyEditMainForm.MapViewer;
    end;

    with CurrentAction do
    begin
      if ReplaceBy.FieldID = '' then
      begin
        // Just created - use selected square as base
        ReplaceBy.SetToSquare(
          MasterFile.Master.Map[SquarePos.MapID].Map[SquarePos.Position]);
      end;
    end;

    FillIDEdits;

    EditFieldID.OnChange := EditComponentIDChange;
    EditEffectID.OnChange := EditComponentIDChange;
    EditToolID.OnChange := EditComponentIDChange;
    EditObstacleID.OnChange := EditComponentIDChange;

    Visible := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameReplaceSquareActionEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
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

