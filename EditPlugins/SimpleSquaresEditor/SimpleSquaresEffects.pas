unit SimpleSquaresEffects;

interface

uses
  Classes, FunLabyUtils, SimpleSquaresUtils;

resourcestring
  SSimpleButtonTitle = 'Bouton poussoir';
  SSimpleSwitchTitle = 'Interrupteur';

type
  {*
    Effet simple de type bouton poussoir
    @author sjrd
    @version 5.0
  *}
  TSimpleButton = class(TSimpleEffect)
  protected
    function GetParentClassName: string; override;

    function GetCanEditPainter: Boolean; override;

    procedure EnumEvents(AEvents: TStrings); override;
    function GetDefaultEvent: string; override;
  public
    constructor Create(AImagesMaster: TImagesMaster); override;

    class function ClassTitle: string; override;
  end;

  {*
    Effet simple de type interrupteur
    @author sjrd
    @version 5.0
  *}
  TSimpleSwitch = class(TSimpleEffect)
  protected
    function GetParentClassName: string; override;

    function GetCanEditPainter: Boolean; override;

    procedure EnumEvents(AEvents: TStrings); override;
    function GetDefaultEvent: string; override;
  public
    constructor Create(AImagesMaster: TImagesMaster); override;

    class function ClassTitle: string; override;
  end;

implementation

{---------------------}
{ TSimpleButton class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleButton.Create(AImagesMaster: TImagesMaster);
begin
  inherited;

  Painter.AddImage('Buttons/Button'); {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleButton.GetParentClassName: string;
begin
  Result := 'TPushButton'; {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleButton.GetCanEditPainter: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleButton.EnumEvents(AEvents: TStrings);
begin
  AEvents.Add('ButtonDown'); {don't localize}
  AEvents.Add('ButtonUp'); {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleButton.GetDefaultEvent: string;
begin
  Result := 'ButtonDown'; {don't localize}
end;

{*
  [@inheritDoc]
*}
class function TSimpleButton.ClassTitle: string;
begin
  Result := SSimpleButtonTitle;
end;

{---------------------}
{ TSimpleSwitch class }
{---------------------}

{*
  [@inheritDoc]
*}
constructor TSimpleSwitch.Create(AImagesMaster: TImagesMaster);
begin
  inherited;

  Painter.AddImage('Buttons/SwitchOff'); {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleSwitch.GetParentClassName: string;
begin
  Result := 'TSwitch'; {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleSwitch.GetCanEditPainter: Boolean;
begin
  Result := False;
end;

{*
  [@inheritDoc]
*}
procedure TSimpleSwitch.EnumEvents(AEvents: TStrings);
begin
  AEvents.Add('SwitchOn'); {don't localize}
  AEvents.Add('SwitchOff'); {don't localize}
end;

{*
  [@inheritDoc]
*}
function TSimpleSwitch.GetDefaultEvent: string;
begin
  Result := 'SwitchOn'; {don't localize}
end;

{*
  [@inheritDoc]
*}
class function TSimpleSwitch.ClassTitle: string;
begin
  Result := SSimpleSwitchTitle;
end;

initialization
  FunLabyRegisterClasses([
    TSimpleButton, TSimpleSwitch
  ]);
finalization
  FunLabyUnRegisterClasses([
    TSimpleButton, TSimpleSwitch
  ]);
end.

