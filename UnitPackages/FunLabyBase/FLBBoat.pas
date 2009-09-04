{*
  D�crit le comportement complet de la barque
  L'unit� FLBBoat regroupe tous les composants intervenant dans le
  fonctionnement de la barque.
  @author sjrd
  @version 5.0
*}
unit FLBBoat;

interface

uses
  SysUtils, Graphics, StrUtils, ScUtils, FunLabyUtils, Generics, GraphicsTools,
  MapTools, FLBCommon, FLBFields, GR32;

const {don't localize}
  idBoatPlugin = 'BoatPlugin'; /// ID du plug-in barque

resourcestring
  sBoat = 'Barque'; /// Nom de la barque

resourcestring
  /// Hint du cr�ateur de barques
  sBoatCreatorHint = 'Cr�er une nouvelle barque';

const {don't localize}
  idBoatCreator = 'BoatCreator'; /// ID du cr�ateur de barques

const {don't localize}
  fBoat = 'Vehicles/Boat'; /// Fichier de la barque

  fBoatByDir: array[TDirection] of string = (
    'Vehicles/Boat', 'Vehicles/BoatNorth', 'Vehicles/BoatEast',
    'Vehicles/BoatSouth', 'Vehicles/BoatWest'
  );

type
  {*
    Barque
    La barque est un v�hicule permettant d'aller sur l'eau.
    @author sjrd
    @version 5.0
  *}
  TBoat = class(TVehicle)
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;

    procedure Entering(Context: TMoveContext); override;
    procedure Entered(Context: TMoveContext); override;

    procedure Moving(Context: TMoveContext); override;
    procedure Moved(Context: TMoveContext); override;

    function AbleTo(Player: TPlayer; const Action: TPlayerAction;
      Param: Integer): Boolean; override;
  end;

  {*
    Cr�ateur de barques
    @author sjrd
    @version 5.0
  *}
  TBoatCreator = class(TComponentCreator)
  protected
    function GetHint: string; override;

    function DoCreateComponent(
      const AID: TComponentID): TFunLabyComponent; override;
  public
    constructor Create(AMaster: TMaster; const AID: TComponentID); override;
  end;

var { FunDelphi codegen }
  compBoatCreator: TBoatCreator;

implementation

{--------------}
{ Classe TBoat }
{--------------}

{*
  [@inheritDoc]
*}
constructor TBoat.Create(AMaster: TMaster; const AID: TComponentID);
var
  Dir: TDirection;
begin
  inherited;

  Name := SBoat;

  for Dir := diNone to diWest do
    DirPainters[Dir].AddImage(fBoatByDir[Dir]);
end;

{*
  [@inheritDoc]
*}
procedure TBoat.Entering(Context: TMoveContext);
begin
end;

{*
  [@inheritDoc]
*}
procedure TBoat.Entered(Context: TMoveContext);
begin
  AttachController(Context.Player);

  inherited;
end;

{*
  [@inheritDoc]
*}
procedure TBoat.Moving(Context: TMoveContext);
begin
  with Context do
  begin
    if Player.Direction <> OldDirection then
      Cancel;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TBoat.Moved(Context: TMoveContext);
begin
  with Context do
  begin
    if not (DestSquare.Field is TWater) then
      DetachController(SrcQPos);
  end;
end;

{*
  [@inheritDoc]
*}
function TBoat.AbleTo(Player: TPlayer; const Action: TPlayerAction;
  Param: Integer): Boolean;
begin
  Result := Action = actGoOnWater;
end;

{--------------------}
{ TBoatCreator class }
{--------------------}

{*
  [@inheritDoc]
*}
constructor TBoatCreator.Create(AMaster: TMaster; const AID: TComponentID);
begin
  inherited;

  IconPainter.AddImage(fBoat);
end;

{*
  [@inheritDoc]
*}
function TBoatCreator.GetHint: string;
begin
  Result := SBoatCreatorHint;
end;

{*
  [@inheritDoc]
*}
function TBoatCreator.DoCreateComponent(
  const AID: TComponentID): TFunLabyComponent;
begin
  Result := TBoat.Create(Master, AID);
end;

end.

