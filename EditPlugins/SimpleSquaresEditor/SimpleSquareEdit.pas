unit SimpleSquareEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ExtDlgs, StrUtils, SdDialogs,
  FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresEffectEditor,
  SimpleSquaresObjectEditor, SimpleSquaresObstacleEditor, FunLabyCoreConsts,
  GraphicEx, PainterEditor;

type
  {*
    Cadre d'édition d'un composant de case simple
    @author sjrd
    @version 5.0
  *}
  TFrameEditSimpleSquare = class(TFrame, ISimpleSquaresEditor)
    PanelCommon: TPanel;
    LabelID: TLabel;
    LabelName: TLabel;
    PaintBoxImage: TPaintBox;
    LabelParentClass: TLabel;
    EditID: TEdit;
    EditName: TEdit;
    EditParentClass: TEdit;
    LabelImage: TLabel;
    ButtonEditImage: TSpeedButton;
    procedure EditNameChange(Sender: TObject);
    procedure PaintBoxImagePaint(Sender: TObject);
    procedure ButtonEditImageClick(Sender: TObject);
  private
    FCurrentSquare: TSimpleSquare; /// Case courante

    /// Déclenché lorsque le nom ou l'image du composant change
    FOnNameImageChange: TNotifyEvent;

    EffectEditor: TFrameEffectEditor;     /// Éditeur d'effet
    ObjectEditor: TFrameObjectEditor;     /// Éditeur d'objet
    ObstacleEditor: TFrameObstacleEditor; /// Éditeur d'obstacle

    procedure ImageChanged;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentSquare(const Value: TSimpleSquare);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentSquare: TSimpleSquare
      read FCurrentSquare write SetCurrentSquare;
  published
    property OnNameImageChange: TNotifyEvent
      read FOnNameImageChange write FOnNameImageChange;
  end;

implementation

{$R *.dfm}

{*
  [@inheritDoc]
*}
constructor TFrameEditSimpleSquare.Create(AOwner: TComponent);
begin
  inherited;

  EffectEditor := TFrameEffectEditor.Create(Self);
  EffectEditor.Parent := Self;

  ObjectEditor := TFrameObjectEditor.Create(Self);
  ObjectEditor.Parent := Self;

  ObstacleEditor := TFrameObstacleEditor.Create(Self);
  ObstacleEditor.Parent := Self;
end;

{*
  Signale que l'image a changé
*}
procedure TFrameEditSimpleSquare.ImageChanged;
begin
  PaintBoxImage.Invalidate;
  if Assigned(FOnNameImageChange) then
    FOnNameImageChange(Self);
  MarkModified;
end;

{*
  [@inheritDoc]
*}
function TFrameEditSimpleSquare.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie la case courante
  @param Value   Nouvelle case courante
*}
procedure TFrameEditSimpleSquare.SetCurrentSquare(const Value: TSimpleSquare);
begin
  EffectEditor.CurrentEffect := nil;
  ObjectEditor.CurrentObject := nil;
  ObstacleEditor.CurrentObstacle := nil;

  EditName.OnChange := nil;

  FCurrentSquare := Value;

  if FCurrentSquare = nil then
  begin
    Enabled := False;

    EditID.Clear;
    EditName.Clear;
    EditParentClass.Clear;
    ButtonEditImage.Enabled := False;
  end else
  begin
    Enabled := True;

    EditID.Text := CurrentSquare.ID;
    EditName.Text := CurrentSquare.Name;
    EditParentClass.Text := CurrentSquare.ParentClassName;
    ButtonEditImage.Enabled := CurrentSquare.CanEditPainter;

    EditName.OnChange := EditNameChange;

    if CurrentSquare is TSimpleEffect then
      EffectEditor.CurrentEffect := TSimpleEffect(CurrentSquare)
    else if CurrentSquare is TSimpleObject then
      ObjectEditor.CurrentObject := TSimpleObject(CurrentSquare)
    else if CurrentSquare is TSimpleObstacle then
      ObstacleEditor.CurrentObstacle := TSimpleObstacle(CurrentSquare);
  end;

  PaintBoxImage.Invalidate;
end;

{*
  [@inheritDoc]
*}
procedure TFrameEditSimpleSquare.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

{*
  Gestionnaire d'événement OnChange de l'edit du nom du composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.EditNameChange(Sender: TObject);
begin
  CurrentSquare.Name := EditName.Text;
  if Assigned(FOnNameImageChange) then
    FOnNameImageChange(Self);
  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Modifier l'image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.ButtonEditImageClick(Sender: TObject);
begin
  Assert(CurrentSquare.CanEditPainter);

  if TFormPainterEditor.EditPainter(CurrentSquare.Painter) then
    ImageChanged;
end;

{*
  Gestionnaire d'événement OnPaint de l'image du composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.PaintBoxImagePaint(Sender: TObject);
begin
  if CurrentSquare <> nil then
    CurrentSquare.Draw(PaintBoxImage.Canvas, PanelCommon.Color);
end;

end.

