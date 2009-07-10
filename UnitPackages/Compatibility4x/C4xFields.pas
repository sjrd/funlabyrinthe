{*
  Décrit les terrains de compatibilité 4.x
  L'unité C4xFields regroupe les définitions des terrains de compatiblité 4.x.
  @author sjrd
  @version 5.0
*}
unit C4xFields;

interface

uses
  ScUtils, SdDialogs, FunLabyUtils, MapTools, FLBFields, FLBPlank, FLBCommon,
  C4xComponents;

const
  idOldWater = 'OldWater'; /// ID de l'eau ancienne version
  idOldHole = 'OldHole';   /// ID du trou ancienne version

  /// ID du plug-in de hacks de compatibilité
  idCompatibilityHacksPlugin = 'CompatibilityHacksPlugin';

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
  public
    procedure Entering(Context: TMoveContext); override;
  end;

  {*
    Plug-in effectuant quelques "hacks" pour rester compatible avec la 4.x
    @author sjrd
    @version 5.0
  *}
  TCompatibilityHacksPlugin = class(TPlugin)
  public
    procedure Moving(Context: TMoveContext); override;
    procedure Moved(Context: TMoveContext); override;
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
          Master.Temporize;
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
          Master.Temporize;
          Exit;
        end;
      end;

      if (Map[Behind].Field is TOldWater) and (SrcSquare.Field is TOldWater) then
      begin
        TPlankSquare.Create(Master, Map, Pos, Player);
        Master.Temporize;
        Exit;
      end;
    end;

    if KeyPressed then
      Player.ShowMessage(sCantGoOnHole);
    Cancel;
  end;
end;

{---------------------------------}
{ TCompatibilityHacksPlugin class }
{---------------------------------}

{*
  [@inheritDoc]
*}
procedure TCompatibilityHacksPlugin.Moved(Context: TMoveContext);
begin
  with Context do
  begin
    if Square.Field is TOldWater then
      Player.DoAction(actGoOnWater);
  end;
end;

{*
  [@inheritDoc]
*}
procedure TCompatibilityHacksPlugin.Moving(Context: TMoveContext);
begin
  with Context do
  begin
    if (Square.Field is TWater) and (not (Square.Field is TOldWater)) then
      Square := ChangeField(Square, idOldWater);
  end;
end;

end.

