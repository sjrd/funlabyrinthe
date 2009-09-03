unit SimpleSquaresChangeEffectEnabledActionEditor;

interface

uses
  Windows, Messages, SysUtils, TypInfo, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions, SimpleSquaresConsts,
  SimpleSquaresActionEditor, GR32, ExtCtrls;

type
  {*
    Cadre d'édition d'une action Activer ou désactiver un effet
    @author sjrd
    @version 5.0
  *}
  TFrameChangeEffectEnabledActionEditor = class(TFrameActionEditor)
    RadioGroupEnabledValue: TRadioGroup;
    GroupBoxEffect: TGroupBox;
    EditEffectID: TComboBoxEx;
    SquaresImages: TImageList;
    ButtonCurrentEffect: TRadioButton;
    ButtonAnyEffect: TRadioButton;
    procedure EditEffectIDChange(Sender: TObject);
    procedure EditEffectIDEnter(Sender: TObject);
    procedure RadioGroupEnabledValueClick(Sender: TObject);
    procedure ButtonCurrentEffectClick(Sender: TObject);
    procedure ButtonAnyEffectClick(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier maître

    FCurrentAction: TChangeEffectEnabledAction; /// Action courante

    procedure RegisterComponent(Component: TFunLabyComponent);
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TChangeEffectEnabledAction
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
procedure TFrameChangeEffectEnabledActionEditor.RegisterComponent(
  Component: TFunLabyComponent);
var
  SquareBmp: TBitmap;
  ImageIndex: Integer;
  Category: TComboExItems;
  Item: TComboExItem;
begin
  if not ((Component is TEffect) and IsPublishedProp(Component, 'Enabled')) then
    Exit;

  Category := EditEffectID.ItemsEx;

  // Ajout de l'image du composant dans la liste d'images
  SquareBmp := TBitmap.Create;
  try
    SquareBmp.SetSize(SquareSize, SquareSize);
    Component.DrawIconToCanvas(SquareBmp.Canvas, BaseSquareRect,
      EditEffectID.Color);

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
  [@inheritDoc]
*}
procedure TFrameChangeEffectEnabledActionEditor.ActivateAction;
begin
  // Initialize
  if MasterFile = nil then // first time
  begin
    MasterFile := GetFunLabyEditMainForm.MasterFile;
    MasterFile.Master.RegisterComponents(RegisterComponent);
  end;

  // Activate action

  RadioGroupEnabledValue.ItemIndex := Byte(CurrentAction.EnabledValue);
  if CurrentAction.EffectID = '' then
    ButtonCurrentEffect.Checked := True
  else
    ButtonAnyEffect.Checked := True;

  EditEffectID.Text := CurrentAction.EffectID;
  EditEffectID.ItemIndex := EditEffectID.Items.IndexOf(EditEffectID.Text);

  RadioGroupEnabledValue.OnClick := RadioGroupEnabledValueClick;
  ButtonCurrentEffect.OnClick := ButtonCurrentEffectClick;
  ButtonAnyEffect.OnClick := ButtonAnyEffectClick;
  EditEffectID.OnChange := EditEffectIDChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameChangeEffectEnabledActionEditor.DeactivateAction;
begin
  RadioGroupEnabledValue.OnClick := nil;
  ButtonCurrentEffect.OnClick := nil;
  ButtonAnyEffect.OnClick := nil;
  EditEffectID.OnChange := nil;
end;

{*
  Gestionnaire d'événement OnClick des radio EnabledValue
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameChangeEffectEnabledActionEditor.RadioGroupEnabledValueClick(
  Sender: TObject);
begin
  CurrentAction.EnabledValue := Boolean(Byte(RadioGroupEnabledValue.ItemIndex));

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Cet effet
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameChangeEffectEnabledActionEditor.ButtonCurrentEffectClick(
  Sender: TObject);
begin
  EditEffectID.Text := '';
  EditEffectID.ItemIndex := -1;
  CurrentAction.EffectID := '';

  MarkModified;
end;

{*
  Gestionnaire d'événement OnClick du bouton Autre effet
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameChangeEffectEnabledActionEditor.ButtonAnyEffectClick(
  Sender: TObject);
begin
  MarkModified;
end;

{*
  Gestionnaire d'événement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameChangeEffectEnabledActionEditor.EditEffectIDChange(
  Sender: TObject);
begin
  CurrentAction.EffectID := (Sender as TComboBoxEx).Text;

  MarkModified;
end;

{*
  Gestionnaire d'événement OnEnter de l'éditeur d'ID de l'effet
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameChangeEffectEnabledActionEditor.EditEffectIDEnter(
  Sender: TObject);
begin
  ButtonAnyEffect.Checked := True;
end;

end.

