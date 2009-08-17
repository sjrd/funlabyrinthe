unit PainterEditor;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtDlgs, StdCtrls, ScStrUtils, SdDialogs, GR32, GR32_Image,
  GR32_Layers, FunLabyUtils;

resourcestring
  SInvalidImageFileNameTitle = 'Fichier d''image invalide';
  SInvalidImageFileName =
    'Les images doivent �tre s�lectionn�e dans le dossier d''images de '+
    'FunLabyrinthe';

type
  {*
    Bo�te de dialogue d'�dition de peintre
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
    FPainter: TPainter; /// Peintre � �diter

    procedure UpdateListBoxImgNames;
    procedure UpdateSelectedImage(const ImgName: string);
    procedure UpdateControls;

    function DoEditPainter(APainter: TPainter): Boolean;

    property Painter: TPainter read FPainter;
  public
    class function EditPainter(Painter: TPainter): Boolean;
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
  Met � jour la liste des noms d'images
*}
procedure TFormPainterEditor.UpdateListBoxImgNames;
begin
  ListBoxImgNames.Items.Assign(Painter.ImgNames);
  PaintBoxPreview.Invalidate;
end;

{*
  Met � jour la vue de l'image s�lectionn�e
*}
procedure TFormPainterEditor.UpdateSelectedImage(const ImgName: string);
var
  FileName: TFileName;
  SubRect: TRect;
begin
  Painter.Master.ParseImgName(ImgName, FileName, SubRect);
  if FileName = '' then
    Exit;

  with ImgViewSelectedImage.Bitmap do
  begin
    LoadFromFile(FileName);

    if not SameRect(BoundsRect, SubRect) then
    begin
      FrameRectS(SubRect, clYellow32);
      InflateRect(SubRect, 1, 1);
      FrameRectS(SubRect, clYellow32);
      InflateRect(SubRect, -2, -2);
      FrameRectS(SubRect, clYellow32);
    end;
  end;
end;

{*
  Met � jour les diff�rents contr�les
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
  �dite un peintre
  @param APainter   Peintre � �diter
  @return True si le peintre a �t� modifi�, False sinon
*}
function TFormPainterEditor.DoEditPainter(APainter: TPainter): Boolean;
begin
  FPainter := TPainter.Create(APainter.Master);
  try
    FPainter.ImgNames.Assign(APainter.ImgNames);

    UpdateListBoxImgNames;
    if ListBoxImgNames.Items.Count > 0 then
      ListBoxImgNames.ItemIndex := 0;
    UpdateControls;

    Result := ShowModal = mrOK;

    if Result then
      APainter.ImgNames.Assign(FPainter.ImgNames);
  finally
    FreeAndNil(FPainter);
  end;
end;

{*
  �dite un peintre
  @param Painter   Peintre � �diter
  @return True si le peintre a �t� modifi�, False sinon
*}
class function TFormPainterEditor.EditPainter(Painter: TPainter): Boolean;
begin
  if FormPainterEditor = nil then
    Application.CreateForm(TFormPainterEditor, FormPainterEditor);

  Result := FormPainterEditor.DoEditPainter(Painter);
end;

{*
  Gestionnaire d'�v�nement OnCreate de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormPainterEditor.FormCreate(Sender: TObject);
begin
  OpenImageDialog.Filter := FileFormatList.GetGraphicFilter([], fstBoth,
    [foCompact, foIncludeAll, foIncludeExtension], nil);
  OpenImageDialog.InitialDir := fSquaresDir;
end;

{*
  Gestionnaire d'�v�nement OnPaintBuffer de l'aper�u
  @param Sender   Objet qui a d�clench� l'�v�nement
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
  Gestionnaire d'�v�nement OnClick de la liste d'images
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormPainterEditor.ListBoxImgNamesClick(Sender: TObject);
begin
  UpdateControls;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Ajouter une image
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormPainterEditor.ButtonAddImageClick(Sender: TObject);
var
  I: Integer;
  FileName: TFileName;
begin
  if OpenImageDialog.Execute then
  begin
    Painter.ImgNames.BeginUpdate;
    try
      for I := 0 to OpenImageDialog.Files.Count-1 do
      begin
        FileName := OpenImageDialog.Files[I];

        if not AnsiStartsText(fSquaresDir, FileName) then
        begin
          ShowDialog(SInvalidImageFileNameTitle, SInvalidImageFileName,
            dtError);
          Exit;
        end;

        Delete(FileName, 1, Length(fSquaresDir));
        FileName := ChangeFileExt(FileName, '');
        FileName := AnsiReplaceStr(FileName, '\', '/');

        Painter.ImgNames.Add(FileName);
      end;
    finally
      Painter.ImgNames.EndUpdate;
    end;
  end;

  UpdateListBoxImgNames;
  ListBoxImgNames.ItemIndex := ListBoxImgNames.Items.Count-1;
  UpdateControls;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Retirer une image
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormPainterEditor.ButtonRemoveImageClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxImgNames.ItemIndex;
  if Index < 0 then
    Exit;

  Painter.ImgNames.Delete(Index);
  UpdateListBoxImgNames;

  if Index < Painter.ImgNames.Count then
    ListBoxImgNames.ItemIndex := Index
  else
    ListBoxImgNames.ItemIndex := Index-1;

  UpdateControls;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton D�placer une image vers le haut
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormPainterEditor.ButtonMoveImageUpClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxImgNames.ItemIndex;
  if Index <= 0 then
    Exit;

  Painter.ImgNames.Exchange(Index, Index-1);
  UpdateListBoxImgNames;

  ListBoxImgNames.ItemIndex := Index-1;

  UpdateControls;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton D�placer une image vers le bas
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormPainterEditor.ButtonMoveImageDownClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBoxImgNames.ItemIndex;
  if Index >= Painter.ImgNames.Count-1 then
    Exit;

  Painter.ImgNames.Exchange(Index, Index+1);
  UpdateListBoxImgNames;

  ListBoxImgNames.ItemIndex := Index+1;

  UpdateControls;
end;

{*
  Gestionnaire d'�v�nement OnMouseDown de l'image s�lectionn�e
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Button   Bouton de la souris qui a �t� enfonc�
  @param Shift    �tat des touches sp�ciales
  @param X        Abscisse du point cliqu�
  @param Y        Ordonn�e du point cliqu�
  @param Layer    Calque cliqu�
*}
procedure TFormPainterEditor.ImgViewSelectedImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  SelectedImage: TBitmap32;
  ImgPoint: TPoint;
  XIndex, YIndex, SelIndex: Integer;
  BaseImgName, Dummy: string;
begin
  SelectedImage := ImgViewSelectedImage.Bitmap;
  if SelectedImage.Empty or
    SameRect(SelectedImage.BoundsRect, BaseSquareRect) then
    Exit;

  ImgPoint := ImgViewSelectedImage.ControlToBitmap(Point(X, Y));
  if not PtInRect(SelectedImage.BoundsRect, ImgPoint) then
    Exit;

  XIndex := ImgPoint.X div SquareSize;
  YIndex := ImgPoint.Y div SquareSize;
  SelIndex := ListBoxImgNames.ItemIndex;

  SplitToken(Painter.ImgNames[SelIndex], '@', BaseImgName, Dummy);
  Painter.ImgNames[SelIndex] := Format('%s@%d,%d',
    [BaseImgName, XIndex, YIndex]);

  UpdateListBoxImgNames;
  ListBoxImgNames.ItemIndex := SelIndex;
  UpdateControls;
end;

end.

