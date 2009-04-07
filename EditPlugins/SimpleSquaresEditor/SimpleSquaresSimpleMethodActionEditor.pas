unit SimpleSquaresSimpleMethodActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, FunLabyUtils, FunLabyEditOTA, SimpleSquaresUtils,
  SimpleSquaresActions, SimpleSquaresActionEditor;

type
  {*
    Cadre d'�dition d'une action M�thode simple
    @author sjrd
    @version 5.0
  *}
  TFrameSimpleMethodActionEditor = class(TFrameActionEditor)
    RadioGroupKind: TRadioGroup;
    procedure RadioGroupKindClick(Sender: TObject);
  private
    FCurrentAction: TSimpleMethodAction; /// Action courante
  protected
    procedure ActivateAction; override;
    procedure DeactivateAction; override;
  published
    property CurrentAction: TSimpleMethodAction
      read FCurrentAction write FCurrentAction;
  end;

implementation

{$R *.dfm}

{--------------------------------------}
{ TFrameSimpleMethodActionEditor class }
{--------------------------------------}

{*
  [@inheritDoc]
*}
procedure TFrameSimpleMethodActionEditor.ActivateAction;
begin
  RadioGroupKind.ItemIndex := Ord(CurrentAction.Kind);

  RadioGroupKind.OnClick := RadioGroupKindClick;
end;

{*
  [@inheritDoc]
*}
procedure TFrameSimpleMethodActionEditor.DeactivateAction;
begin
  RadioGroupKind.OnClick := nil;
end;

{*
  Gestionnaire d'�v�nement OnClick de la group-box Type d'action
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFrameSimpleMethodActionEditor.RadioGroupKindClick(Sender: TObject);
begin
  CurrentAction.Kind := TSimpleMethodKind(RadioGroupKind.ItemIndex);

  MarkModified;
end;

end.

