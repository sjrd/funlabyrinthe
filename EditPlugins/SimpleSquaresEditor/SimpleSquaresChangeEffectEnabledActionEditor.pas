unit SimpleSquaresChangeEffectEnabledActionEditor;

interface

uses
  Windows, Messages, SysUtils, TypInfo, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ScUtils, FunLabyUtils, FilesUtils,
  FunLabyEditOTA, SimpleSquaresUtils, SimpleSquaresActions, SimpleSquaresConsts,
  SimpleSquaresActionEditor, GR32, ExtCtrls, FunLabyControls;

type
  {*
    Cadre d'�dition d'une action Activer ou d�sactiver un effet
    @author sjrd
    @version 5.0
  *}
  TFrameChangeEffectEnabledActionEditor = class(TFrameActionEditor)
    RadioGroupEnabledValue: TRadioGroup;
    GroupBoxEffect: TGroupBox;
    ButtonCurrentEffect: TRadioButton;
    ButtonAnyEffect: TRadioButton;
    ComboBoxEffect: TFLComponentComboBox;
    procedure ComboBoxEffectChange(Sender: TObject);
    procedure ComboBoxEffectEnter(Sender: TObject);
    procedure RadioGroupEnabledValueClick(Sender: TObject);
    procedure ButtonCurrentEffectClick(Sender: TObject);
    procedure ButtonAnyEffectClick(Sender: TObject);
    procedure ComboBoxEffectFilterComponent(Sender: TObject;
      Component: TFunLabyComponent; var Accept: Boolean);
  private
    MasterFile: TMasterFile; /// Fichier ma�tre

    FCurrentAction: TChangeEffectEnabledAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TChangeEffectEnabledAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{---------------------------------------------}
{ TFrameChangeEffectEnabledActionEditor class }
{---------------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFrameChangeEffectEnabledActionEditor.ActivateAction;
begin
  // Initialize
  if MasterFile = nil then // first time
    MasterFile := GetFunLabyEditMainForm.MasterFile;

  // Activate action

  RadioGroupEnabledValue.ItemIndex := Byte(CurrentAction.EnabledValue);
  if CurrentAction.EffectID = '' then
    ButtonCurrentEffect.Checked := True
  else
    ButtonAnyEffect.Checked := True;

  ComboBoxEffect.ComponentClass := TEffect;
  ComboBoxEffect.Master := MasterFile.Master;
  ComboBoxEffect.Selected := MasterFile.Master.Effect[CurrentAction.EffectID];

  RadioGroupEnabledValue.OnClick := RadioGroupEnabledValueClick;
  ButtonCurrentEffect.OnClick := ButtonCurrentEffectClick;
  ButtonAnyEffect.OnClick := ButtonAnyEffectClick;
  ComboBoxEffect.OnChange := ComboBoxEffectChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameChangeEffectEnabledActionEditor.DeactivateAction;
begin
  RadioGroupEnabledValue.OnClick := nil;
  ButtonCurrentEffect.OnClick := nil;
  ButtonAnyEffect.OnClick := nil;
  ComboBoxEffect.OnChange := nil;
end;

{*
  Gestionnaire d'�v�nement OnClick des radio EnabledValue
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameChangeEffectEnabledActionEditor.RadioGroupEnabledValueClick(
  Sender: TObject);
begin
  CurrentAction.EnabledValue := Boolean(Byte(RadioGroupEnabledValue.ItemIndex));

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Cet effet
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameChangeEffectEnabledActionEditor.ButtonCurrentEffectClick(
  Sender: TObject);
begin
  ComboBoxEffect.Selected := nil;
  CurrentAction.EffectID := '';

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnClick du bouton Autre effet
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameChangeEffectEnabledActionEditor.ButtonAnyEffectClick(
  Sender: TObject);
begin
  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnFilterComponent de la combobox d'effet
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameChangeEffectEnabledActionEditor.ComboBoxEffectFilterComponent(
  Sender: TObject; Component: TFunLabyComponent; var Accept: Boolean);
begin
  Accept := IsPublishedProp(Component, 'Enabled');
end;

{*
  Gestionnaire d'�v�nement OnChange d'un des edit d'ID de composant
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameChangeEffectEnabledActionEditor.ComboBoxEffectChange(
  Sender: TObject);
begin
  CurrentAction.EffectID := ComboBoxEffect.Selected.SafeID;

  MarkModified;
end;

{*
  Gestionnaire d'�v�nement OnEnter de l'�diteur d'ID de l'effet
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameChangeEffectEnabledActionEditor.ComboBoxEffectEnter(
  Sender: TObject);
begin
  ButtonAnyEffect.Checked := True;
end;

end.

