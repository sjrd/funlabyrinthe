unit SimpleSquareEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ExtDlgs, StrUtils, SdDialogs,
  FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresEffectEditor,
  SimpleSquaresObjectEditor, SimpleSquaresObstacleEditor, FunLabyCoreConsts,
  GraphicEx;

resourcestring
  SInvalidImageFileName =
    'Les images doivent être sélectionnée dans le dossier d''images de '+
    'FunLabyrinthe';

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
    LabelImages: TLabel;
    ButtonNewImage: TSpeedButton;
    ButtonRemoveImage: TSpeedButton;
    ButtonUpImage: TSpeedButton;
    ButtonDownImage: TSpeedButton;
    PaintBoxImage: TPaintBox;
    LabelParentClass: TLabel;
    EditID: TEdit;
    EditName: TEdit;
    ListBoxImages: TListBox;
    EditParentClass: TEdit;
    OpenImageDialog: TOpenPictureDialog;
    procedure ListBoxImagesClick(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure ListBoxImagesData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure ButtonNewImageClick(Sender: TObject);
    procedure ButtonRemoveImageClick(Sender: TObject);
    procedure ButtonUpImageClick(Sender: TObject);
    procedure ButtonDownImageClick(Sender: TObject);
    procedure PaintBoxImagePaint(Sender: TObject);
  private
    FCurrentSquare: TSimpleSquare; /// Case courante

    /// Déclenché lorsque le nom ou l'image du composant change
    FOnNameImageChange: TNotifyEvent;

    EffectEditor: TFrameEffectEditor;     /// Éditeur d'effet
    ObjectEditor: TFrameObjectEditor;     /// Éditeur d'objet
    ObstacleEditor: TFrameObstacleEditor; /// Éditeur d'obstacle

    procedure UpdateImagesButtons;
    procedure ImageChanged;

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentSquare(const Value: TSimpleSquare);
  public
    constructor Create(AOwner: TComponent); override;

    procedure AfterConstruction; override;

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
  Met à jour les boutons d'édition des images
*}
procedure TFrameEditSimpleSquare.UpdateImagesButtons;
var
  Index: Integer;
begin
  Index := ListBoxImages.ItemIndex;

  if (CurrentSquare = nil) or (not CurrentSquare.CanEditImgNames) then
  begin
    ButtonNewImage.Enabled := False;
    ButtonRemoveImage.Enabled := False;
    ButtonUpImage.Enabled := False;
    ButtonDownImage.Enabled := False;
  end else
  begin
    ButtonNewImage.Enabled := True;
    ButtonRemoveImage.Enabled := Index >= 0;
    ButtonUpImage.Enabled := Index > 0;
    ButtonDownImage.Enabled := Index < CurrentSquare.ImgNames.Count-1;
  end;
end;

{*
  Signale que l'image a changé
*}
procedure TFrameEditSimpleSquare.ImageChanged;
begin
  ListBoxImages.Count := CurrentSquare.ImgNames.Count;
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

  ListBoxImages.Count := 0;
  EditName.OnChange := nil;

  FCurrentSquare := Value;

  if FCurrentSquare = nil then
  begin
    Enabled := False;

    EditID.Clear;
    EditName.Clear;
    EditParentClass.Clear;
  end else
  begin
    Enabled := True;

    EditID.Text := CurrentSquare.ID;
    EditName.Text := CurrentSquare.Name;
    EditParentClass.Text := CurrentSquare.ParentClassName;

    if CurrentSquare.ImgNames.Count > 0 then
    begin
      ListBoxImages.Count := CurrentSquare.ImgNames.Count;
      ListBoxImages.ItemIndex := 0;
    end;

    EditName.OnChange := EditNameChange;

    if CurrentSquare is TSimpleEffect then
      EffectEditor.CurrentEffect := TSimpleEffect(CurrentSquare)
    else if CurrentSquare is TSimpleObject then
      ObjectEditor.CurrentObject := TSimpleObject(CurrentSquare)
    else if CurrentSquare is TSimpleObstacle then
      ObstacleEditor.CurrentObstacle := TSimpleObstacle(CurrentSquare);
  end;

  PaintBoxImage.Invalidate;
  UpdateImagesButtons;
end;

{*
  [@inheritDoc]
*}
procedure TFrameEditSimpleSquare.AfterConstruction;
begin
  inherited;

  OpenImageDialog.InitialDir := fSquaresDir;
  OpenImageDialog.Filter := FileFormatList.GetGraphicFilter(
    [], fstDescription, [foCompact, foIncludeAll, foIncludeExtension], nil);
  OpenImageDialog.DefaultExt := PreferredImageExtension;
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
  Gestionnaire d'événement OnData de la liste des images
  @param Control   Contrôle qui a déclenché l'événement
  @param Index     Index de la chaîne demandée
  @param Data      En sortie : chaîne à la position indiquée
*}
procedure TFrameEditSimpleSquare.ListBoxImagesData(Control: TWinControl;
  Index: Integer; var Data: string);
begin
  Assert(CurrentSquare <> nil);
  Data := CurrentSquare.ImgNames[Index];
end;

{*
  Gestionnaire d'événement OnClick de la liste des images
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.ListBoxImagesClick(Sender: TObject);
begin
  UpdateImagesButtons;
end;

{*
  Gestionnaire d'événement OnClick du bouton Ajouter une image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.ButtonNewImageClick(Sender: TObject);
var
  I: Integer;
  FileName: TFileName;
begin
  Assert(CurrentSquare.CanEditImgNames);

  if OpenImageDialog.Execute then
  begin
    CurrentSquare.ImgNames.BeginUpdate;
    try
      for I := 0 to OpenImageDialog.Files.Count-1 do
      begin
        FileName := OpenImageDialog.Files[I];

        if not AnsiStartsText(fSquaresDir, FileName) then
        begin
          ShowDialog(SError, SInvalidImageFileName, dtError);
          Exit;
        end;

        Delete(FileName, 1, Length(fSquaresDir));
        FileName := ChangeFileExt(FileName, '');

        CurrentSquare.ImgNames.Add(FileName);
      end;
    finally
      CurrentSquare.ImgNames.EndUpdate;
    end;

    ImageChanged;
  end;

  ListBoxImages.ItemIndex := CurrentSquare.ImgNames.Count-1;
  UpdateImagesButtons;
end;

{*
  Gestionnaire d'événement OnClick du bouton Supprimer l'image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.ButtonRemoveImageClick(Sender: TObject);
var
  Index: Integer;
begin
  Assert(CurrentSquare.CanEditImgNames);

  Index := ListBoxImages.ItemIndex;
  if Index >= 0 then
  begin
    CurrentSquare.ImgNames.Delete(Index);
    ImageChanged;

    if Index < CurrentSquare.ImgNames.Count then
      ListBoxImages.ItemIndex := Index
    else
      ListBoxImages.ItemIndex := Index-1;
  end;

  UpdateImagesButtons;
end;

{*
  Gestionnaire d'événement OnClick du bouton Monter l'image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.ButtonUpImageClick(Sender: TObject);
var
  Index: Integer;
begin
  Assert(CurrentSquare.CanEditImgNames);

  Index := ListBoxImages.ItemIndex;
  if Index > 0 then
  begin
    CurrentSquare.ImgNames.Exchange(Index, Index-1);
    ImageChanged;

    ListBoxImages.ItemIndex := Index-1;
  end;

  UpdateImagesButtons;
end;

{*
  Gestionnaire d'événement OnClick du bouton Descendre l'image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameEditSimpleSquare.ButtonDownImageClick(Sender: TObject);
var
  Index: Integer;
begin
  Assert(CurrentSquare.CanEditImgNames);

  Index := ListBoxImages.ItemIndex;
  if Index < CurrentSquare.ImgNames.Count-1 then
  begin
    CurrentSquare.ImgNames.Exchange(Index, Index+1);
    ImageChanged;

    ListBoxImages.ItemIndex := Index+1;
  end;

  UpdateImagesButtons;
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

