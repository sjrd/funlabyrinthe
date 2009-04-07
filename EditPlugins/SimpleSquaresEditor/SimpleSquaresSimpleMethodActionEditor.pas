unit SimpleSquaresSimpleMethodActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions;

type
  {*
    Cadre d'édition d'une action Méthode simple
    @author sjrd
    @version 5.0
  *}
  TFrameSimpleMethodActionEditor = class(TFrame, ISimpleSquaresEditor)
    RadioGroupKind: TRadioGroup;
    procedure RadioGroupKindClick(Sender: TObject);
  private
    FCurrentAction: TSimpleMethodAction; /// Action courante

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentAction(Value: TSimpleMethodAction);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentAction: TSimpleMethodAction
      read FCurrentAction write SetCurrentAction;
  end;

implementation

{$R *.dfm}

{--------------------------------------}
{ TFrameSimpleMethodActionEditor class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameSimpleMethodActionEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  [@inheritDoc]
*}
function TFrameSimpleMethodActionEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action
*}
procedure TFrameSimpleMethodActionEditor.SetCurrentAction(
  Value: TSimpleMethodAction);
begin
  if CurrentAction <> nil then
  begin
    Visible := False;

    RadioGroupKind.OnClick := nil;
  end;

  FCurrentAction := Value;

  if CurrentAction <> nil then
  begin
    RadioGroupKind.ItemIndex := Ord(CurrentAction.Kind);

    RadioGroupKind.OnClick := RadioGroupKindClick;

    Visible := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleMethodActionEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

{*
  Gestionnaire d'événement OnClick de la group-box Type d'action
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFrameSimpleMethodActionEditor.RadioGroupKindClick(Sender: TObject);
begin
  CurrentAction.Kind := TSimpleMethodKind(RadioGroupKind.ItemIndex);

  MarkModified;
end;

end.

