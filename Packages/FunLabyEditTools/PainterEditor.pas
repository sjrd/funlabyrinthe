unit PainterEditor;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtDlgs, StdCtrls, ScStrUtils, SdDialogs, GR32, GR32_Image,
  GR32_Layers, FunLabyUtils, FilesUtils, FunLabyCoreConsts;

resourcestring
  SInvalidImageFileNameTitle = 'Fichier d''image invalide';
  SInvalidImageFileName =
    'Les images doivent être sélectionnée dans le dossier d''images de '+
    'FunLabyrinthe';

type
  {*
    Peintre en édition
    @author sjrd
    @version 5.0
  *}
  TEditPainter = class(TPainter)
  public
    property Description;
  end;

  {*
    Boîte de dialogue d'édition de peintre
    @author sjrd
    @version 5.0
  *}
  TFormPainterEditor = class(TForm)
    LabelPreview: TLabel;
    PaintBoxPreview: TPaintBox32;
    LabelImgNames: TLabel;
    ListBoxImgNames: TListBox;
    ButtonAddImage: TSpeedButton;
    ButtonRemoveImage: TSpeedButton;
    ButtonMoveImageUp: TSpeedButton;
    ButtonMoveImageDown: TSpeedButton;
    ImgViewSelectedImage: TImgView32;
    LabelSelectedImage: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    OpenImageDialog: TOpenPictureDialog;
    procedure PaintBoxPreviewPaintBuffer(Sender: TObject);
    procedure ListBoxImgNamesClick(Sender: TObject);
    procedure ButtonAddImageClick(Sender: TObject);
    procedure ButtonRemoveImageClick(Sender: TObject);
    procedure ButtonMoveImageUpClick(Sender: TObject);
    procedure ButtonMoveImageDownClick(Sender: TObject);
    procedure ImgViewSelectedImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure FormCreate(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier maître

    FPainter: TEditPainter; /// Peintre à éditer

    procedure UpdateListBoxImgNames;
    procedure UpdateSelectedImage(const ImgName: string);
    procedure UpdateControls;

    function DoEditPainter(AMasterFile: TMasterFile;
      APainter: TPainter): Boolean;

    property Painter: TEditPainter read FPainter;
  public
    class function EditPainter(MasterFile: TMasterFile;
      Painter: TPainter): Boolean;
  end;

var
  FormPainterEditor: TFormPainterEditor;

implementation

{$R *.dfm}

uses
  GraphicEx;

{--------------------------}
{ TFormPainterEditor class }
{--------------------------}

{*
  Met à jour la liste des noms d'images
*}
procedure TFormPainterEditor.UpdateListBoxImgNames;
begin
  ListBoxImgNames.Items.Assign(Painter.Description);
  PaintBoxPreview.Invalidate;
end;

{*
  Met à jour la vue de l'image sélectionnée
*}
procedure TFormPainterEditor.UpdateSelectedImage(const ImgName: string);
var
  Bitmap: TBitmap32;
  SubRect: TRect;
begin
  Painter.ParseDescriptionLine(ImgName, Bitmap, SubRect);

  with ImgViewSelectedImage.Bitmap do
  begin
    Assign(Bitmap);

    if not SameRect(BoundsRect, SubRect) then
    begin
      FrameRectS(SubRect, clYellow32);
      InflateRect(SubRect, 1, 1);
      FrameRectS(SubRect, clYellow32);
      InflateRect(SubRect, -2, -2);
      FrameRectS(SubRect, clYellow32);

      ImgViewSelectedImage.ScrollToCenter(
        (SubRect.Left+SubRect.Right) div 2, (SubRect.Top+SubRect.Bottom) div 2);
    end;
  end;
end;

{*
  Met à jour les différents contrôles
*}
procedure TFormPainterEditor.UpdateControls;
var
  ImgIndex: Integer;
begin
  ImgIndex := ListBoxImgNames.ItemIndex;

  ButtonRemoveImage.Enabled := ImgIndex >= 0;
  ButtonMoveImageUp.Enabled := ImgIndex > 0;
  ButtonMoveImageDown.Enabled := ImgIndex < ListBoxImgNames.Items.Count-1;

  ImgViewSelectedImage.Bitmap.SetSize(0, 0);
  if ImgIndex >= 0 then
    UpdateSelectedImage(ListBoxImgNames.Items[ImgIndex]);
end;

{*
  Édite un peintre
  @param AMasterFile   Fichier maître
  @param APainter      Peintre à éditer
  @return True si le peintre a été modifié, False sinon
*}
function TFormPainterEditor.DoEditPainter(AMasterFile: TMasterFile;
  APainter: TPainter): Boolean;
begin
  MasterFile := AMasterFile;

  FPainter := TEditPainter.Create(APainter.Master);
  try
    FPainter.Assign(APainter);

    UpdateListBoxImgNames;
    if ListBoxImgNames.Items.Count > 0 then
      ListBoxImgNames.ItemIndex := 0;
    UpdateControls;

    Result := ShowModal = mrOK;

    if Result then
      APainter.Assign(FPainter);
  finally
    FreeAndNil(FPainter);
  end;
end;

{*
  Édite un peintre
  @param MasterFile   Fichier maître
  @param Painter      Peintre à éditer
  @return True si le peintre a été modifié, False sinon
*}
class function TFormPainterEditor.EditPainter(MasterFile: TMasterFile;
  Painter: TPainter): Boolean;
begin
  if FormPainterEditor = nil then
    Application.CreateForm(TFormPainterEditor, FormPainterEditor);

  Result := FormPainterEditor.DoEditPainter(MasterFile, Painter);
end;

{*
  Gestionnaire d'événement OnCreate de la fiche
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.FormCreate(Sender: TObject);
var
  Filter: string;
  I: Integer;
begin
  Filter := FileFormatList.GetGraphicFilter([], fstBoth,
    [foCompact, foIncludeAll, foIncludeExtension], nil);

  I := PosEx('|', Filter, Pos('|', Filter)+1);
  Insert(';*.' + PainterExtension, Filter, I);

  Filter := Filter + '|' + SPainterFilter;

  OpenImageDialog.Filter := Filter;
  OpenImageDialog.InitialDir := JoinPath([FunLabyAppDataDir,
    ResourceKindToDir[rkImage]]);
end;

{*
  Gestionnaire d'événement OnPaintBuffer de l'aperçu
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.PaintBoxPreviewPaintBuffer(Sender: TObject);
var
  Buffer: TBitmap32;
  Context: TDrawSquareContext;
begin
  Buffer := PaintBoxPreview.Buffer;
  Buffer.Clear(Color32(Self.Color));

  if Painter = nil then
    Exit;

  Context := TDrawSquareContext.Create(Buffer);
  try
    Painter.Draw(Context);
  finally
    Context.Free;
  end;
end;

{*
  Gestionnaire d'événement OnClick de la liste d'images
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.ListBoxImgNamesClick(Sender: TObject);
begin
  UpdateControls;
end;

{*
  Gestionnaire d'événement OnClick du bouton Ajouter une image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.ButtonAddImageClick(Sender: TObject);
var
  ImagesDir: TFileName;
  I: Integer;
  FileName: TFileName;
  HRef: string;
begin
  if OpenImageDialog.Execute then
  begin
    ImagesDir := IncludeTrailingPathDelimiter(JoinPath([FunLabyAppDataDir,
      ResourceKindToDir[rkImage]]));

    Painter.Description.BeginUpdate;
    try
      for I := 0 to OpenImageDialog.Files.Count-1 do
      begin
        FileName := OpenImageDialog.Files[I];
        HRef := MasterFile.MakeResourceHRef(FileName, rkImage);

        Painter.Description.Add(ChangeFileExt(HRef, ''));
      end;
    finally
      Painter.Description.EndUpdate;
    end;
  end;

  UpdateListBoxImgNames;
  ListBoxImgNames.ItemIndex := ListBoxImgNames.Items.Count-1;
  UpdateControls;
end;

{*
  Gestionnaire d'événement OnClick du bouton Retirer une image
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.ButtonRemoveImageClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxImgNames.ItemIndex;
  if Index < 0 then
    Exit;

  Painter.Description.Delete(Index);
  UpdateListBoxImgNames;

  if Index < Painter.Description.Count then
    ListBoxImgNames.ItemIndex := Index
  else
    ListBoxImgNames.ItemIndex := Index-1;

  UpdateControls;
end;

{*
  Gestionnaire d'événement OnClick du bouton Déplacer une image vers le haut
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.ButtonMoveImageUpClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxImgNames.ItemIndex;
  if Index <= 0 then
    Exit;

  Painter.Description.Exchange(Index, Index-1);
  UpdateListBoxImgNames;

  ListBoxImgNames.ItemIndex := Index-1;

  UpdateControls;
end;

{*
  Gestionnaire d'événement OnClick du bouton Déplacer une image vers le bas
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFormPainterEditor.ButtonMoveImageDownClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxImgNames.ItemIndex;
  if Index >= Painter.Description.Count-1 then
    Exit;

  Painter.Description.Exchange(Index, Index+1);
  UpdateListBoxImgNames;

  ListBoxImgNames.ItemIndex := Index+1;

  UpdateControls;
end;

{*
  Gestionnaire d'événement OnMouseDown de l'image sélectionnée
  @param Sender   Objet qui a déclenché l'événement
  @param Button   Bouton de la souris qui a été enfoncé
  @param Shift    État des touches spéciales
  @param X        Abscisse du point cliqué
  @param Y        Ordonnée du point cliqué
  @param Layer    Calque cliqué
*}
procedure TFormPainterEditor.ImgViewSelectedImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  SelectedImage: TBitmap32;
  ImgPoint: TPoint;
  SelIndex: Integer;
  BaseImgName, Dummy: string;
begin
  SelectedImage := ImgViewSelectedImage.Bitmap;
  if SelectedImage.Empty or
    SameRect(SelectedImage.BoundsRect, BaseSquareRect) then
    Exit;

  ImgPoint := ImgViewSelectedImage.ControlToBitmap(Point(X, Y));
  if not PtInRect(SelectedImage.BoundsRect, ImgPoint) then
    Exit;
  Dec(ImgPoint.X, ImgPoint.X mod SquareSize);
  Dec(ImgPoint.Y, ImgPoint.Y mod SquareSize);

  SelIndex := ListBoxImgNames.ItemIndex;

  SplitToken(Painter.Description[SelIndex], '@', BaseImgName, Dummy);
  Painter.Description[SelIndex] := Format('%s@%d,%d:%d,%d',
    [BaseImgName, ImgPoint.X, ImgPoint.Y,
    ImgPoint.X+SquareSize, ImgPoint.Y+SquareSize]);

  UpdateListBoxImgNames;
  ListBoxImgNames.ItemIndex := SelIndex;
  UpdateControls;
end;

end.

