unit SimpleSquaresActionEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo, ScCompilerMagic, FunLabyUtils, FunLabyEditOTA,
  SimpleSquaresUtils;

type
  {*
    Classe de base pour les cadres d'�dition d'une action
    TFrameActionEditor est une classe abstraite. Toute classe concr�te
    descendante de TFrameActionEditor doit avoir une propri�t� "CurrentAction"
    publi�e, accessible en lecture et en �criture, d'un type classe descendant
    de TSimpleAction.
    Toute tentative de cr�ation d'une classe qui ne satisferait pas cette
    condition r�sultera en une exception EAbstractError.
    @author sjrd
    @version 5.0
  *}
  TFrameActionEditor = class(TFrame, ISimpleSquaresEditor)
  private
    FCurrentActionProp: PPropInfo;    /// PropInfo de la propri�t� CurrentAction
    FActionClass: TSimpleActionClass; /// Classe d'actions prises en charge

    function GetCurrentAction: TSimpleAction;
    procedure SetCurrentAction(Value: TSimpleAction);
  protected
    function GetFunLabyEditMainForm: IOTAFunLabyEditMainForm50;

    procedure ActivateAction; virtual;
    procedure DeactivateAction; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure MarkModified; virtual;

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

  Visible := False;
  Align := alClient;

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
  [@inheritDoc]
*}
function TFrameActionEditor.GetFunLabyEditMainForm:
  IOTAFunLabyEditMainForm50;
begin
  Result := (Owner as ISimpleSquaresEditor).FunLabyEditMainForm;
end;

{*
  Action courante
  @return Action en cours d'�dition
*}
function TFrameActionEditor.GetCurrentAction: TSimpleAction;
begin
  Result := TSimpleAction(
    GetObjectProp(Self, FCurrentActionProp, TSimpleAction));
end;

{*
  Modifie l'action � �diter
  @param Value   Nouvelle action (doit �tre une instance de ActionClass)
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
  Active l'action courante pour l'�diter
*}
procedure TFrameActionEditor.ActivateAction;
begin
end;

{*
  D�sactive l'action en cours d'�dition
*}
procedure TFrameActionEditor.DeactivateAction;
begin
end;

{*
  [@inheritDoc]
*}
procedure TFrameActionEditor.MarkModified;
begin
  (Owner as ISimpleSquaresEditor).MarkModified;
end;

end.

