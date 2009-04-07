unit SimpleSquaresPlayerColorActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor;

type
  {*
    Cadre d'édition d'une action Changer la couleur du pion
    @author sjrd
    @version 5.0
  *}
  TFramePlayerColorActionEditor = class(TFrameActionEditor)
    LabelColor: TStaticText;
    ListBoxColor: TColorBox;
    procedure ListBoxColorChange(Sender: TObject);
  private
    FCurrentAction: TPlayerColorAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TPlayerColorAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{-------------------------------------}
{ TFramePlayerColorActionEditor class }
{-------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFramePlayerColorActionEditor.ActivateAction;
begin
  ListBoxColor.HandleNeeded; // work around bug of SetSelected
  ListBoxColor.Selected := CurrentAction.Color;

  ListBoxColor.OnChange := ListBoxColorChange;
end;

{*
  [@inheritDoc]
*}
procedure TFramePlayerColorActionEditor.DeactivateAction;
begin
  ListBoxColor.OnChange := nil;
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

