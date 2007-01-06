{*
  D�crit les terrains de compatibilit� 4.x
  L'unit� C4xFields regroupe les d�finitions des terrains de compatiblit� 4.x.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xFields;

interface

uses
  ScUtils, FunLabyUtils, FLBFields, FLBCommon, C4xComponents;

const
  idOldWater = 'OldWater'; /// ID de l'eau ancienne version
  idOldHole = 'OldHole';   /// ID du trou ancienne version

type
  {*
    Eau (ancienne version)
    Elle fonctionne comme l'eau de base, sauf que le test de la planche est
    conforme au test de la version 4.x.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TOldWater = class(TWater)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Trou (ancienne version)
    Il fonctionne comme le trou de base, sauf que le test de la planche est
    conforme au test de la version 4.x.
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TOldHole = class(THole)
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

implementation

{------------------}
{ Classe TOldWater }
{------------------}

{*
  [@inheritDoc]
*}
procedure TOldWater.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
var Behind : T3DPoint;
    OK : boolean;
begin
  with Player do
  begin
    if CanYou(actGoOnWater) then exit;

    Behind := PointBehind(Pos, Direction);
    if Map[Behind].Field is TGrass then
    begin
      if Map[Behind].Obstacle = Map[Src].Obstacle then OK := True else
      begin
        OK := (Map[Behind].Obstacle is TActionsObstacle) and
          (TActionsObstacle(Map[Behind].Obstacle).Actions.Kind <> akObstacle);
      end;
      if OK and CanYou(actPassOverScrew) then exit;
    end;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnWater, dtError);
    Cancel := True;
  end;
end;

{-----------------}
{ Classe TOldHole }
{-----------------}

{*
  [@inheritDoc]
*}
procedure TOldHole.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
var Behind : T3DPoint;
    OK : boolean;
begin
  with Player do
  begin
    Behind := PointBehind(Pos, Direction);
    if Map[Behind].Field is TGrass then
    begin
      if Map[Behind].Obstacle = Map[Src].Obstacle then OK := True else
      begin
        OK := (Map[Behind].Obstacle is TActionsObstacle) and
          (TActionsObstacle(Map[Behind].Obstacle).Actions.Kind <> akObstacle);
      end;
      if OK and CanYou(actPassOverScrew) then exit;
    end;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnHole, dtError);
    Cancel := True;
  end;
end;

end.

