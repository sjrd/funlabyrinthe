unit SimpleSquaresActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo, ScCompilerMagic, FunLabyUtils, FunLabyEditOTA,
  SimpleSquaresUtils, SimpleSquaresEditorPart;

type
  {*
    Classe de base pour les cadres d'édition d'une action
    TFrameActionEditor est une classe abstraite. Toute classe concrète
    descendante de TFrameActionEditor doit avoir une propriété "CurrentAction"
    publiée, accessible en lecture et en écriture, d'un type classe descendant
    de TSimpleAction.
    Toute tentative de création d'une classe qui ne satisferait pas cette
    condition résultera en une exception EAbstractError.
    @author sjrd
    @version 5.0
  *}
  TFrameActionEditor = class(TFrameSimpleSquaresEditorPart)
  private
    FCurrentActionProp: PPropInfo;    /// PropInfo de la propriété CurrentAction
    FActionClass: TSimpleActionClass; /// Classe d'actions prises en charge

    function GetCurrentAction: TSimpleAction;
    procedure SetCurrentAction(Value: TSimpleAction);
  protected
    procedure ActivateAction; virtual;
    procedure DeactivateAction; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property ActionClass: TSimpleActionClass read FActionClass;
    property CurrentAction: TSimpleAction
      read GetCurrentAction write SetCurrentAction;
  end;

implementation

{$R *.dfm}

{--------------------------}
{ TFrameActionEditor class }
{--------------------------}

{*
  [@inheritDoc]
*}
constructor TFrameActionEditor.Create(AOwner: TComponent);
const {don't localize}
  CurrentActionPropName = 'CurrentAction';
var
  TempClass: TClass;
begin
  inherited;

  // Fetch FCurrentActionProp and FActionClass from RTTI

  FCurrentActionProp := GetPropInfo(Self, CurrentActionPropName, [tkClass]);
  if FCurrentActionProp = nil then
    AbstractError;

  TempClass := GetObjectPropClass(FCurrentActionProp);
  if not TempClass.InheritsFrom(TSimpleAction) then
    AbstractError;
  FActionClass := TSimpleActionClass(TempClass);
end;

{*
  Action courante
  @return Action en cours d'édition
*}
function TFrameActionEditor.GetCurrentAction: TSimpleAction;
begin
  Result := TSimpleAction(
    GetObjectProp(Self, FCurrentActionProp, TSimpleAction));
end;

{*
  Modifie l'action à éditer
  @param Value   Nouvelle action (doit être une instance de ActionClass)
*}
procedure TFrameActionEditor.SetCurrentAction(Value: TSimpleAction);
begin
  if GetCurrentAction <> nil then
  begin
    Visible := False;
    DeactivateAction;
  end;

  SetObjectProp(Self, FCurrentActionProp, Value as ActionClass, False);

  if Value <> nil then
  begin
    ActivateAction;
    Visible := True;
  end;
end;

{*
  Active l'action courante pour l'éditer
*}
procedure TFrameActionEditor.ActivateAction;
begin
end;

{*
  Désactive l'action en cours d'édition
*}
procedure TFrameActionEditor.DeactivateAction;
begin
end;

end.

