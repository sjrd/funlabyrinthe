{*
  D�crit les terrains de compatibilit� 4.x
  L'unit� C4xFields regroupe les d�finitions des terrains de compatiblit� 4.x.
  @author sjrd
  @version 5.0
*}
unit C4xFields;

interface

uses
  Windows, ScUtils, SdDialogs, FunLabyUtils, MapTools, Generics,
  FLBFields, FLBPlank, FLBCommon, C4xComponents;

type
  {*
    Eau (ancienne version)
    Elle fonctionne comme l'eau de base, sauf que le test de la planche est
    conforme au test de la version 4.x.
    @author sjrd
    @version 5.0
  *}
  TOldWater = class(TWater)
  private
    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    procedure Entering(Context: TMoveContext); override;
  end;

  {*
    Trou (ancienne version)
    Il fonctionne comme le trou de base, sauf que le test de la planche est
    conforme au test de la version 4.x.
    @author sjrd
    @version 5.0
  *}
  TOldHole = class(THole)
  private
    procedure PlankMessage(var Msg: TPlankMessage); message msgPlank;
  protected
    procedure DoDraw(Context: TDrawSquareContext); override;
  public
    procedure Entering(Context: TMoveContext); override;
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
procedure TOldWater.PlankMessage(var Msg: TPlankMessage);
begin
end;

{*
  [@inheritDoc]
*}
procedure TOldWater.DoDraw(Context: TDrawSquareContext);
begin
  Painter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TOldWater.Entering(Context: TMoveContext);
var
  Behind: T3DPoint;
  SrcObstacle, DestObstacle: TObstacle;
begin
  with Context do
  begin
    if Player.DoAction(actGoOnWater) then
      Exit;

    if Master.ObjectDef[idPlanks].Count[Player] > 0 then
    begin
      Behind := PointBehind(Pos, Player.Direction);

      if Map[Behind].Field is TGround then
      begin
        SrcObstacle := SrcSquare.Obstacle;
        if (SrcObstacle is TActionsObstacle) and
          (TActionsObstacle(SrcObstacle).Actions.Kind <> akObstacle) then
          SrcObstacle := nil;

        DestObstacle := Map[Behind].Obstacle;
        if (DestObstacle is TActionsObstacle) and
          (TActionsObstacle(DestObstacle).Actions.Kind <> akObstacle) then
          DestObstacle := nil;

        if DestObstacle = SrcObstacle then
        begin
          TPlankSquare.Create(Master, Map, Pos, Player);
          UpdateView(Player);
          Temporize;
          Exit;
        end;
      end;
    end;

    if KeyPressed then
      Player.ShowMessage(sCantGoOnWater);
    Cancel;
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
procedure TOldHole.PlankMessage(var Msg: TPlankMessage);
begin
end;

{*
  [@inheritDoc]
*}
procedure TOldHole.DoDraw(Context: TDrawSquareContext);
begin
  Painter.Draw(Context);
end;

{*
  [@inheritDoc]
*}
procedure TOldHole.Entering(Context: TMoveContext);
var
  Behind: T3DPoint;
  SrcObstacle, DestObstacle: TObstacle;
begin
  with Context do
  begin
    Behind := PointBehind(Pos, Player.Direction);

    if Master.ObjectDef[idPlanks].Count[Player] > 0 then
    begin
      if Map[Behind].Field is TGround then
      begin
        SrcObstacle := SrcSquare.Obstacle;
        if (SrcObstacle is TActionsObstacle) and
          (TActionsObstacle(SrcObstacle).Actions.Kind <> akObstacle) then
          SrcObstacle := nil;

        DestObstacle := Map[Behind].Obstacle;
        if (DestObstacle is TActionsObstacle) and
          (TActionsObstacle(DestObstacle).Actions.Kind <> akObstacle) then
          DestObstacle := nil;

        if DestObstacle = SrcObstacle then
        begin
          TPlankSquare.Create(Master, Map, Pos, Player);
          UpdateView(Player);
          Temporize;
          Exit;
        end;
      end;

      if (Map[Behind].Field is TOldWater) and
        (SrcSquare.Field is TOldWater) then
      begin
        TPlankSquare.Create(Master, Map, Pos, Player);
        UpdateView(Player);
        Temporize;
        Exit;
      end;
    end;

    if KeyPressed then
      Player.ShowMessage(sCantGoOnHole);
    Cancel;
  end;
end;

end.

