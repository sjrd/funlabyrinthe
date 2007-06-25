{*
  Décrit les terrains de compatibilité 4.x
  L'unité C4xFields regroupe les définitions des terrains de compatiblité 4.x.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xFields;

interface

uses
  ScUtils, SdDialogs, FunLabyUtils, FLBFields, FLBPlank, FLBCommon,
  C4xComponents;

const
  idOldWater = 'OldWater'; /// ID de l'eau ancienne version
  idOldHole = 'OldHole';   /// ID du trou ancienne version

type
  {*
    Eau (ancienne version)
    Elle fonctionne comme l'eau de base, sauf que le test de la planche est
    conforme au test de la version 4.x.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TOldWater = class(TWater)
  private
    procedure PlankMessage(var Msg : TPlankMessage); message msgPlank;
  public
    procedure Entering(Player : TPlayer; OldDirection : TDirection;
      KeyPressed : boolean; const Src, Pos : T3DPoint;
      var Cancel : boolean); override;
  end;

  {*
    Trou (ancienne version)
    Il fonctionne comme le trou de base, sauf que le test de la planche est
    conforme au test de la version 4.x.
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TOldHole = class(THole)
  private
    procedure PlankMessage(var Msg : TPlankMessage); message msgPlank;
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
  Gestionnaire de message msgPlank
  TOldWater anihile le comportement de FunLabyBase envers la planche.
  @param Msg   Message
*}
procedure TOldWater.PlankMessage(var Msg : TPlankMessage);
begin
end;

{*
  [@inheritDoc]
*}
procedure TOldWater.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
var Behind : T3DPoint;
    SrcObstacle, DestObstacle : TObstacle;
begin
  with Player do
  begin
    if DoAction(actGoOnWater) then exit;

    Behind := PointBehind(Pos, Direction);
    if Map[Behind].Field is TGround then
    begin
      SrcObstacle := Map[Src].Obstacle;
      if (SrcObstacle is TActionsObstacle) and
         (TActionsObstacle(SrcObstacle).Actions.Kind <> akObstacle) then
        SrcObstacle := nil;

      DestObstacle := Map[Behind].Obstacle;
      if (DestObstacle is TActionsObstacle) and
         (TActionsObstacle(DestObstacle).Actions.Kind <> akObstacle) then
        DestObstacle := nil;

      if (DestObstacle = SrcObstacle) and
         (Master.ObjectDef[idPlanks].Count[Player] > 0) then
      begin
        TPlankScrew.Create(Master, Map, Pos, Player);
        Master.Temporize;
        exit;
      end;
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
  Gestionnaire de message msgPlank
  TOldHole anihile le comportement de FunLabyBase envers la planche.
  @param Msg   Message
*}
procedure TOldHole.PlankMessage(var Msg : TPlankMessage);
begin
end;

{*
  [@inheritDoc]
*}
procedure TOldHole.Entering(Player : TPlayer; OldDirection : TDirection;
  KeyPressed : boolean; const Src, Pos : T3DPoint;
  var Cancel : boolean);
var Behind : T3DPoint;
    SrcObstacle, DestObstacle : TObstacle;
begin
  with Player do
  begin
    Behind := PointBehind(Pos, Direction);
    if Map[Behind].Field is TGround then
    begin
      SrcObstacle := Map[Src].Obstacle;
      if (SrcObstacle is TActionsObstacle) and
         (TActionsObstacle(SrcObstacle).Actions.Kind <> akObstacle) then
        SrcObstacle := nil;

      DestObstacle := Map[Behind].Obstacle;
      if (DestObstacle is TActionsObstacle) and
         (TActionsObstacle(DestObstacle).Actions.Kind <> akObstacle) then
        DestObstacle := nil;

      if (DestObstacle = SrcObstacle) and
         (Master.ObjectDef[idPlanks].Count[Player] > 0) then
      begin
        TPlankScrew.Create(Master, Map, Pos, Player);
        Master.Temporize;
        exit;
      end;
    end;

    if KeyPressed then
      Player.ShowDialog(sBlindAlley, sCantGoOnHole, dtError);
    Cancel := True;
  end;
end;

end.

