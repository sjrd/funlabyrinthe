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

    function GetCanEditImgNames: Boolean; override;
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

    function GetCanEditImgNames: Boolean; override;
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

  ImgNames.Add('Button'); {don't localize}
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
function TSimpleButton.GetCanEditImgNames: Boolean;
begin
  Result := False;
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

  ImgNames.Add('SwitchOff'); {don't localize}
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
function TSimpleSwitch.GetCanEditImgNames: Boolean;
begin
  Result := False;
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

