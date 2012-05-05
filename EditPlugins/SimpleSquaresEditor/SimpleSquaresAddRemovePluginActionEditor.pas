unit SimpleSquaresAddRemovePluginActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor, FilesUtils, StdCtrls,
  ExtCtrls;

type
  {*
    Cadre d'édition d'une action Ajouter ou retirer un plugin
    @author sjrd
    @version 5.3
  *}
  TFrameAddRemovePluginActionEditor = class(TFrameActionEditor)
    RadioGroupKind: TRadioGroup;
    LabelPluginID: TLabel;
    EditPluginID: TComboBox;
    procedure AnyPropertyChange(Sender: TObject);
  private
    MasterFile: TMasterFile; /// Fichier maître

    FCurrentAction: TAddRemovePluginAction; /// Action courante

    procedure FillControls;
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TAddRemovePluginAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{-----------------------------------------}
{ TFrameAddRemovePluginActionEditor class }
{-----------------------------------------}

{*
  Gestionnaire d'événement OnClick/OnChange des contrôles
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameAddRemovePluginActionEditor.AnyPropertyChange(Sender: TObject);
begin
  CurrentAction.Kind := TAddRemovePluginKind(RadioGroupKind.ItemIndex);
  CurrentAction.PluginID := EditPluginID.Text;

  MarkModified;
end;

{*
  Fill the controls from the action
*}
procedure TFrameAddRemovePluginActionEditor.FillControls;
begin
  RadioGroupKind.OnClick := nil;
  EditPluginID.OnChange := nil;

  RadioGroupKind.ItemIndex := Ord(CurrentAction.Kind);
  EditPluginID.Text := CurrentAction.PluginID;

  RadioGroupKind.OnClick := AnyPropertyChange;
  EditPluginID.OnChange := AnyPropertyChange;
end;

{*
  [@inheritDoc]
*}
procedure TFrameAddRemovePluginActionEditor.ActivateAction;
var
  I: Integer;
begin
  // Initialize
  if MasterFile = nil then // first time
    MasterFile := GetFunLabyEditMainForm.MasterFile;

  // Activate action

  with MasterFile.Master, EditPluginID.Items do
  begin
    Clear;
    for I := 0 to PluginCount-1 do
      Add(Plugins[I].ID);
  end;

  FillControls;
end;

{*
  [@inheritDoc]
*}
procedure TFrameAddRemovePluginActionEditor.DeactivateAction;
begin
  RadioGroupKind.OnClick := nil;
  EditPluginID.OnChange := nil;
end;

end.

