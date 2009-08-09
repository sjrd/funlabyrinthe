unit SimpleSquaresDeactivateEffectActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions, SimpleSquaresConsts,
  SimpleSquaresActionEditor, GR32;

type
  {*
    Cadre d'édition d'une action Désactiver un effet
    @author sjrd
    @version 5.0
  *}
  TFrameDeactivateEffectActionEditor = class(TFrameActionEditor)
    LabelEffectID: TLabel;
    SquaresImages: TImageList;
    EditEffectID: TComboBoxEx;
    procedure EditEffectIDChange(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier maître

    FCurrentAction: TDeactivateEffectAction; /// Action courante

    procedure RegisterComponent(Component: TFunLabyComponent);

    procedure FillIDEdits;
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TDeactivateEffectAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{------------------------------------------}
{ TFrameDeactivateEffectActionEditor class }
{------------------------------------------}

{*
  Enregistre un composant
  @param Component   Le composant à enregistrer
*}
procedure TFrameDeactivateEffectActionEditor.RegisterComponent(
  Component: TFunLabyComponent);
var
  SquareBmp: TBitmap;
  ImageIndex: Integer;
  Category: TComboExItems;
  Item: TComboExItem;
begin
  if not (Component is TEffect) then
    Exit;

  Category := EditEffectID.ItemsEx;

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
procedure TFrameDeactivateEffectActionEditor.FillIDEdits;
begin
  with CurrentAction do
  begin
    EditEffectID.Text := IIF(EffectID = '', SNone, EffectID);
    EditEffectID.ItemIndex := EditEffectID.Items.IndexOf(EditEffectID.Text);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameDeactivateEffectActionEditor.ActivateAction;
begin
  // Initialize
  if MasterFile = nil then // first time
  begin
    EditEffectID.ItemsEx.Add.Caption := SNone;

    MasterFile := GetFunLabyEditMainForm.MasterFile;
    MasterFile.Master.RegisterComponents(RegisterComponent);
  end;

  // Activate action

  FillIDEdits;

  EditEffectID.OnChange := EditEffectIDChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameDeactivateEffectActionEditor.DeactivateAction;
begin
  EditEffectID.OnChange := nil;
end;

{*
  Gestionnaire d'événement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameDeactivateEffectActionEditor.EditEffectIDChange(
  Sender: TObject);
var
  NewID: TComponentID;
begin
  NewID := (Sender as TComboBoxEx).Text;
  if NewID = SNone then
    NewID := '';

  CurrentAction.EffectID := NewID;

  MarkModified;
end;

end.

