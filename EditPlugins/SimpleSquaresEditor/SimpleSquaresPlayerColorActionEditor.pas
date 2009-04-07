unit SimpleSquaresPlayerColorActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions;

type
  {*
    Cadre d'édition d'une action Changer la couleur du pion
    @author sjrd
    @version 5.0
  *}
  TFramePlayerColorActionEditor = class(TFrame, ISimpleSquaresEditor)
    LabelColor: TStaticText;
    ListBoxColor: TColorBox;
    procedure ListBoxColorChange(Sender: TObject);
  private
    FCurrentAction: TPlayerColorAction; /// Action courante

    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure SetCurrentAction(Value: TPlayerColorAction);
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified;

    property CurrentAction: TPlayerColorAction
      read FCurrentAction write SetCurrentAction;
  end;

implementation

{$R *.dfm}

{-------------------------------------}
{ TFramePlayerColorActionEditor class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
constructor TFramePlayerColorActionEditor.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
end;

{*
  [@inheritDoc]
*}
function TFramePlayerColorActionEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action
*}
procedure TFramePlayerColorActionEditor.SetCurrentAction(
  Value: TPlayerColorAction);
begin
  if CurrentAction <> nil then
  begin
    Visible := False;

    ListBoxColor.OnChange := nil;
  end;

  FCurrentAction := Value;

  if CurrentAction <> nil then
  begin
    ListBoxColor.HandleNeeded; // work around bug of SetSelected
    ListBoxColor.Selected := CurrentAction.Color;

    ListBoxColor.OnChange := ListBoxColorChange;

    Visible := True;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TFramePlayerColorActionEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

{*
  Gestionnaire d'événement OnClick de la list-box Premier passage
  @param Sender   Objet qui a déclenché l'événement
*}
procedure TFramePlayerColorActionEditor.ListBoxColorChange(Sender: TObject);
begin
  CurrentAction.Color := ListBoxColor.Selected;

  MarkModified;
end;

end.

