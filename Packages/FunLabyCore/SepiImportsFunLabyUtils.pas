{*
  Importe l'unité FunLabyUtils dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsFunLabyUtils;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, Classes, Graphics, Controls, Contnrs, ScUtils, FunLabyUtils;

implementation

{ You must not localize any of the strings this unit contains! }

type
  TSepiImportsEComponentNotFound = class(EComponentNotFound)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsEUnsupportedCommand = class(EUnsupportedCommand)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTImagesMaster = class(TImagesMaster)
  private
    procedure Draw_0(Index : integer; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 );
    procedure Draw_1(const ImgName : string; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 );
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScrewBitmap = class(TScrewBitmap)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPainter = class(TPainter)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTFunLabyComponent = class(TFunLabyComponent)
  private
    function GetSafeID: TComponentID;
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTVisualComponent = class(TVisualComponent)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPlugin = class(TPlugin)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTObjectDef = class(TObjectDef)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScrewComponent = class(TScrewComponent)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTField = class(TField)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTEffect = class(TEffect)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTTool = class(TTool)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTObstacle = class(TObstacle)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTScrew = class(TScrew)
  private
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TUnnamed_1 = array of TScrew;

  TSepiImportsTMap = class(TMap)
  private
    procedure SetMaxViewSize(Value : integer);
    function GetMap(const Position : T3DPoint) : TScrew;
    procedure SetMap(const Position : T3DPoint; Value : TScrew);
    function GetOutside(Floor : integer) : TScrew;
    procedure SetOutside(Floor : integer; Value : TScrew);
    function GetLinearMapCount: integer;
    function GetLinearMap(Index : integer) : TScrew;
    procedure SetLinearMap(Index : integer; Value : TScrew);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTPlayer = class(TPlayer)
  private
    function GetVisible: boolean;
    procedure MoveTo_0(const Dest : T3DPoint; Execute : boolean; out Redo : boolean );
    procedure MoveTo_1(const Dest : T3DPoint);
    procedure MoveTo_2(const Dest : TQualifiedPos; Execute : boolean; out Redo : boolean );
    procedure MoveTo_3(const Dest : TQualifiedPos);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

  TSepiImportsTMaster = class(TMaster)
  private
    function GetComponent(const ID : TComponentID) : TFunLabyComponent;
    function GetScrewComponent(const ID : TComponentID) : TScrewComponent;
    function GetPlugin(const ID : TComponentID) : TPlugin;
    function GetObjectDef(const ID : TComponentID) : TObjectDef;
    function GetField(const ID : TComponentID) : TField;
    function GetEffect(const ID : TComponentID) : TEffect;
    function GetTool(const ID : TComponentID) : TTool;
    function GetObstacle(const ID : TComponentID) : TObstacle;
    function GetScrew(const ID : TComponentID) : TScrew;
    function GetMap(const ID : TComponentID) : TMap;
    function GetPlayer(const ID : TComponentID) : TPlayer;
    function GetPluginCount: integer;
    function GetPlugins(Index : integer) : TPlugin;
    function GetObjectDefCount: integer;
    function GetObjectDefs(Index : integer) : TObjectDef;
    function GetFieldCount: integer;
    function GetFields(Index : integer) : TField;
    function GetEffectCount: integer;
    function GetEffects(Index : integer) : TEffect;
    function GetToolCount: integer;
    function GetTools(Index : integer) : TTool;
    function GetObstacleCount: integer;
    function GetObstacles(Index : integer) : TObstacle;
    function GetScrewCount: integer;
    function GetScrews(Index : integer) : TScrew;
    function GetMapCount: integer;
    function GetMaps(Index : integer) : TMap;
    function GetPlayerCount: integer;
    function GetPlayers(Index : integer) : TPlayer;
    procedure SetTemporization(Value : integer);
    class function SepiImport(Owner : TSepiMetaUnit) : TSepiClass;
  end;

{---------------------------}
{ EComponentNotFound import }
{---------------------------}

class function TSepiImportsEComponentNotFound.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EComponentNotFound));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------------}
{ EUnsupportedCommand import }
{----------------------------}

class function TSepiImportsEUnsupportedCommand.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(EUnsupportedCommand));

  with Result do
  begin

    Complete;
  end;
end;

{----------------------}
{ TQualifiedPos import }
{----------------------}

function SepiImportTQualifiedPos(Owner : TSepiMetaUnit) : TSepiRecordType;
begin
  Result := TSepiRecordType.Create(Owner, 'TQualifiedPos', False, True);

  with Result do
  begin
    AddField('Map', System.TypeInfo(TMap));
    AddField('Position', 'T3DPoint');

    Complete;
  end;
end;

{----------------------}
{ TImagesMaster import }
{----------------------}

procedure TSepiImportsTImagesMaster.Draw_0(Index : integer; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 );
begin
  Draw(Index, Canvas, X, Y);
end;

procedure TSepiImportsTImagesMaster.Draw_1(const ImgName : string; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 );
begin
  Draw(ImgName, Canvas, X, Y);
end;

class function TSepiImportsTImagesMaster.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TImagesMaster));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FImgList', System.TypeInfo(TImageList));
    AddField('FImgNames', System.TypeInfo(TStrings));

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTImagesMaster.Create,
      'constructor');
    AddMethod('Destroy', @TSepiImportsTImagesMaster.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Add', @TSepiImportsTImagesMaster.Add,
      'function(const ImgName : string; Bitmap : TBitmap) : integer');
    AddMethod('IndexOf', @TSepiImportsTImagesMaster.IndexOf,
      'function(const ImgName : string) : integer');
    AddMethod('OL$Draw$0', @TSepiImportsTImagesMaster.Draw_0,
      'procedure(Index : integer; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )');
    AddMethod('OL$Draw$1', @TSepiImportsTImagesMaster.Draw_1,
      'procedure(const ImgName : string; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )');

    Complete;
  end;
end;

{---------------------}
{ TScrewBitmap import }
{---------------------}

class function TSepiImportsTScrewBitmap.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScrewBitmap));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScrewBitmap.Create,
      'constructor',
      mlkOverride);
    AddMethod('EmptyScrew', @TSepiImportsTScrewBitmap.EmptyScrew,
      'procedure');
    AddMethod('DrawScrew', @TSepiImportsTScrewBitmap.DrawScrew,
      'procedure(Canvas : TCanvas; X : integer = 0; Y : integer = 0)');

    Complete;
  end;
end;

{-----------------}
{ TPainter import }
{-----------------}

class function TSepiImportsTPainter.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPainter));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMaster', System.TypeInfo(TImagesMaster));
    AddField('FImgNames', System.TypeInfo(TStrings));
    AddField('FCachedImg', System.TypeInfo(TScrewBitmap));

    AddMethod('ImgNamesChange', nil,
      'procedure(Sender : TObject)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPainter.Create,
      'constructor(AMaster : TImagesMaster)');
    AddMethod('Destroy', @TSepiImportsTPainter.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Draw', @TSepiImportsTPainter.Draw,
      'procedure(Canvas : TCanvas; X : integer = 0; Y : integer = 0)');

    AddProperty('ImgNames', 'property: TStrings',
      'FImgNames', '');

    Complete;
  end;
end;

{--------------------------}
{ TFunLabyComponent import }
{--------------------------}

function TSepiImportsTFunLabyComponent.GetSafeID: TComponentID;
begin
  Result := SafeID;
end;

class function TSepiImportsTFunLabyComponent.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TFunLabyComponent));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMaster', System.TypeInfo(TMaster));
    AddField('FID', System.TypeInfo(TComponentID));
    AddField('FTag', System.TypeInfo(integer));

    AddMethod('GetSafeID', @TSepiImportsTFunLabyComponent.GetSafeID,
      'function: TComponentID');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTFunLabyComponent.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID)');
    AddMethod('Destroy', @TSepiImportsTFunLabyComponent.Destroy,
      'destructor',
      mlkOverride);

    AddProperty('Master', 'property: TMaster',
      'FMaster', '');
    AddProperty('ID', 'property: TComponentID',
      'FID', '');
    AddProperty('SafeID', 'property: TComponentID',
      'GetSafeID', '');
    AddProperty('Tag', 'property: integer',
      'FTag', 'FTag');

    Complete;
  end;
end;

{-------------------------}
{ TVisualComponent import }
{-------------------------}

class function TSepiImportsTVisualComponent.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TVisualComponent));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FName', System.TypeInfo(string));
    AddField('FPainter', System.TypeInfo(TPainter));
    AddField('FCachedImg', System.TypeInfo(TScrewBitmap));

    AddMethod('PrivDraw', nil,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkVirtual);

    CurrentVisibility := mvProtected;

    AddField('FStaticDraw', System.TypeInfo(boolean));

    AddMethod('DoDraw', @TSepiImportsTVisualComponent.DoDraw,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkVirtual);

    AddProperty('Painter', 'property: TPainter',
      'FPainter', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTVisualComponent.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; const AName : string )');
    AddMethod('Destroy', @TSepiImportsTVisualComponent.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTVisualComponent.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('Draw', @TSepiImportsTVisualComponent.Draw,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )');

    AddProperty('Name', 'property: string',
      'FName', '');
    AddProperty('StaticDraw', 'property: boolean',
      'FStaticDraw', '');

    Complete;
  end;
end;

{----------------}
{ TPlugin import }
{----------------}

class function TSepiImportsTPlugin.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TPlugin));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FPainterBefore', System.TypeInfo(TPainter));
    AddField('FPainterAfter', System.TypeInfo(TPainter));

    CurrentVisibility := mvProtected;

    AddProperty('PainterBefore', 'property: TPainter',
      'FPainterBefore', '');
    AddProperty('PainterAfter', 'property: TPainter',
      'FPainterAfter', '');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPlugin.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID)');
    AddMethod('Destroy', @TSepiImportsTPlugin.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('AfterConstruction', @TSepiImportsTPlugin.AfterConstruction,
      'procedure',
      mlkOverride);
    AddMethod('DrawBefore', @TSepiImportsTPlugin.DrawBefore,
      'procedure(Player : TPlayer; const QPos : TQualifiedPos; Canvas : TCanvas ; X : integer = 0 ; Y : integer = 0 )',
      mlkVirtual);
    AddMethod('DrawAfter', @TSepiImportsTPlugin.DrawAfter,
      'procedure(Player : TPlayer; const QPos : TQualifiedPos; Canvas : TCanvas ; X : integer = 0 ; Y : integer = 0 )',
      mlkVirtual);
    AddMethod('Moving', @TSepiImportsTPlugin.Moving,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Src, Dest : T3DPoint ; var Cancel : boolean )',
      mlkVirtual);
    AddMethod('Moved', @TSepiImportsTPlugin.Moved,
      'procedure(Player : TPlayer; const Src, Dest : T3DPoint)',
      mlkVirtual);
    AddMethod('AbleTo', @TSepiImportsTPlugin.AbleTo,
      'function(Player : TPlayer; const Action : TPlayerAction ) : boolean',
      mlkVirtual);

    Complete;
  end;
end;

{-------------------}
{ TObjectDef import }
{-------------------}

class function TSepiImportsTObjectDef.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObjectDef));

  with Result do
  begin
    CurrentVisibility := mvProtected;

    AddMethod('GetCount', @TSepiImportsTObjectDef.GetCount,
      'function(Player : TPlayer) : integer',
      mlkVirtual);
    AddMethod('SetCount', @TSepiImportsTObjectDef.SetCount,
      'procedure(Player : TPlayer; Value : integer)',
      mlkVirtual);
    AddMethod('GetShownInfos', @TSepiImportsTObjectDef.GetShownInfos,
      'function(Player : TPlayer) : string',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('AbleTo', @TSepiImportsTObjectDef.AbleTo,
      'function(Player : TPlayer; const Action : TPlayerAction ) : boolean',
      mlkVirtual);
    AddMethod('UseFor', @TSepiImportsTObjectDef.UseFor,
      'procedure(Player : TPlayer; const Action : TPlayerAction)',
      mlkVirtual);

    AddProperty('Count', 'property[Player : TPlayer] : integer',
      'GetCount', 'SetCount');
    AddProperty('ShownInfos', 'property[Player : TPlayer] : string',
      'GetShownInfos', '');

    Complete;
  end;
end;

{------------------------}
{ TScrewComponent import }
{------------------------}

class function TSepiImportsTScrewComponent.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TScrewComponent'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TScrewComponent));

  with Result do
  begin

    Complete;
  end;
end;

{---------------}
{ TField import }
{---------------}

class function TSepiImportsTField.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TField));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDelegateDrawTo', System.TypeInfo(TField));

    AddMethod('PrivDraw', nil,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTField.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; const AName : string ; ADelegateDrawTo : TField = nil )');
    AddMethod('Entering', @TSepiImportsTField.Entering,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Src, Pos : T3DPoint ; var Cancel : boolean )',
      mlkVirtual);
    AddMethod('Exiting', @TSepiImportsTField.Exiting,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Pos, Dest : T3DPoint ; var Cancel : boolean )',
      mlkVirtual);
    AddMethod('Entered', @TSepiImportsTField.Entered,
      'procedure(Player : TPlayer; const Src, Pos : T3DPoint)',
      mlkVirtual);
    AddMethod('Exited', @TSepiImportsTField.Exited,
      'procedure(Player : TPlayer; const Pos, Dest : T3DPoint)',
      mlkVirtual);

    Complete;
  end;
end;

{----------------}
{ TEffect import }
{----------------}

class function TSepiImportsTEffect.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TEffect));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Entered', @TSepiImportsTEffect.Entered,
      'procedure(Player : TPlayer; const Src, Pos : T3DPoint)',
      mlkVirtual);
    AddMethod('Exited', @TSepiImportsTEffect.Exited,
      'procedure(Player : TPlayer; const Pos, Dest : T3DPoint)',
      mlkVirtual);
    AddMethod('Execute', @TSepiImportsTEffect.Execute,
      'procedure(Player : TPlayer; const Pos : T3DPoint; var GoOnMoving : boolean )',
      mlkVirtual);

    Complete;
  end;
end;

{--------------}
{ TTool import }
{--------------}

class function TSepiImportsTTool.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TTool));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Find', @TSepiImportsTTool.Find,
      'procedure(Player : TPlayer; const Pos : T3DPoint)',
      mlkVirtual);

    Complete;
  end;
end;

{------------------}
{ TObstacle import }
{------------------}

class function TSepiImportsTObstacle.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TObstacle));

  with Result do
  begin
    CurrentVisibility := mvPublic;

    AddMethod('Pushing', @TSepiImportsTObstacle.Pushing,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Src, Pos : T3DPoint ; var Cancel, AbortExecute : boolean )',
      mlkVirtual);

    Complete;
  end;
end;

{---------------}
{ TScrew import }
{---------------}

class function TSepiImportsTScrew.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass.RegisterTypeInfo(
    Owner, TypeInfo(TScrew));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FField', System.TypeInfo(TField));
    AddField('FEffect', System.TypeInfo(TEffect));
    AddField('FTool', System.TypeInfo(TTool));
    AddField('FObstacle', System.TypeInfo(TObstacle));

    CurrentVisibility := mvProtected;

    AddField('FRefCount', System.TypeInfo(integer));

    AddMethod('DoDraw', @TSepiImportsTScrew.DoDraw,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkOverride);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTScrew.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; const AName : string ; AField : TField ; AEffect : TEffect ; ATool : TTool ; AObstacle : TObstacle )');
    AddMethod('BeforeDestruction', @TSepiImportsTScrew.BeforeDestruction,
      'procedure',
      mlkOverride);
    AddMethod('DefaultHandler', @TSepiImportsTScrew.DefaultHandler,
      'procedure(var Msg)',
      mlkOverride);
    AddMethod('Entering', @TSepiImportsTScrew.Entering,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Src, Pos : T3DPoint ; var Cancel : boolean )',
      mlkVirtual);
    AddMethod('Exiting', @TSepiImportsTScrew.Exiting,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Pos, Dest : T3DPoint ; var Cancel : boolean )',
      mlkVirtual);
    AddMethod('Entered', @TSepiImportsTScrew.Entered,
      'procedure(Player : TPlayer; const Src, Pos : T3DPoint)',
      mlkVirtual);
    AddMethod('Exited', @TSepiImportsTScrew.Exited,
      'procedure(Player : TPlayer; const Pos, Dest : T3DPoint)',
      mlkVirtual);
    AddMethod('Execute', @TSepiImportsTScrew.Execute,
      'procedure(Player : TPlayer; const Pos : T3DPoint; var GoOnMoving : boolean )',
      mlkVirtual);
    AddMethod('Pushing', @TSepiImportsTScrew.Pushing,
      'procedure(Player : TPlayer; OldDirection : TDirection; KeyPressed : boolean ; const Src, Pos : T3DPoint ; var Cancel, AbortExecute : boolean )',
      mlkVirtual);
    AddMethod('AddRef', @TSepiImportsTScrew.AddRef,
      'function: integer',
      mlkVirtual);
    AddMethod('Release', @TSepiImportsTScrew.Release,
      'function: integer',
      mlkVirtual);

    AddProperty('Field', 'property: TField',
      'FField', '');
    AddProperty('Effect', 'property: TEffect',
      'FEffect', '');
    AddProperty('Tool', 'property: TTool',
      'FTool', '');
    AddProperty('Obstacle', 'property: TObstacle',
      'FObstacle', '');
    AddProperty('RefCount', 'property: integer',
      'FRefCount', '');

    Complete;
  end;
end;

{-------------}
{ TMap import }
{-------------}

procedure TSepiImportsTMap.SetMaxViewSize(Value : integer);
begin
  MaxViewSize := Value;
end;

function TSepiImportsTMap.GetMap(const Position : T3DPoint) : TScrew;
begin
  Result := Map[Position];
end;

procedure TSepiImportsTMap.SetMap(const Position : T3DPoint; Value : TScrew);
begin
  Map[Position] := Value;
end;

function TSepiImportsTMap.GetOutside(Floor : integer) : TScrew;
begin
  Result := Outside[Floor];
end;

procedure TSepiImportsTMap.SetOutside(Floor : integer; Value : TScrew);
begin
  Outside[Floor] := Value;
end;

function TSepiImportsTMap.GetLinearMapCount: integer;
begin
  Result := LinearMapCount;
end;

function TSepiImportsTMap.GetLinearMap(Index : integer) : TScrew;
begin
  Result := LinearMap[Index];
end;

procedure TSepiImportsTMap.SetLinearMap(Index : integer; Value : TScrew);
begin
  LinearMap[Index] := Value;
end;

class function TSepiImportsTMap.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMap'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMap));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FDimensions', 'T3DPoint');
    AddField('FZoneWidth', System.TypeInfo(integer));
    AddField('FZoneHeight', System.TypeInfo(integer));
    AddField('FMaxViewSize', System.TypeInfo(integer));
    AddField('FMap', System.TypeInfo(TUnnamed_1));
    AddField('FOutsideOffset', System.TypeInfo(integer));

    AddMethod('SetMaxViewSize', @TSepiImportsTMap.SetMaxViewSize,
      'procedure(Value : integer)');
    AddMethod('GetMap', @TSepiImportsTMap.GetMap,
      'function(const Position : T3DPoint) : TScrew');
    AddMethod('SetMap', @TSepiImportsTMap.SetMap,
      'procedure(const Position : T3DPoint; Value : TScrew)');
    AddMethod('GetOutside', @TSepiImportsTMap.GetOutside,
      'function(Floor : integer) : TScrew');
    AddMethod('SetOutside', @TSepiImportsTMap.SetOutside,
      'procedure(Floor : integer; Value : TScrew)');
    AddMethod('GetLinearMapCount', @TSepiImportsTMap.GetLinearMapCount,
      'function: integer');
    AddMethod('GetLinearMap', @TSepiImportsTMap.GetLinearMap,
      'function(Index : integer) : TScrew');
    AddMethod('SetLinearMap', @TSepiImportsTMap.SetLinearMap,
      'procedure(Index : integer; Value : TScrew)');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMap.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; ADimensions : T3DPoint ; AZoneWidth, AZoneHeight : integer )');
    AddMethod('InMap', @TSepiImportsTMap.InMap,
      'function(const Position : T3DPoint) : boolean');
    AddMethod('PlayersOn', @TSepiImportsTMap.PlayersOn,
      'function(const Position : T3DPoint) : integer');

    AddProperty('Dimensions', 'property: T3DPoint',
      'FDimensions', '');
    AddProperty('ZoneWidth', 'property: integer',
      'FZoneWidth', '');
    AddProperty('ZoneHeight', 'property: integer',
      'FZoneHeight', '');
    AddProperty('MaxViewSize', 'property: integer',
      'FMaxViewSize', 'SetMaxViewSize');
    AddProperty('Map', 'property[const Position : T3DPoint] : TScrew',
      'GetMap', 'SetMap', True);
    AddProperty('Outside', 'property[Floor : integer] : TScrew',
      'GetOutside', 'SetOutside');
    AddProperty('LinearMapCount', 'property: integer',
      'GetLinearMapCount', '');
    AddProperty('LinearMap', 'property[index : integer] : TScrew',
      'GetLinearMap', 'SetLinearMap');

    Complete;
  end;
end;

{----------------}
{ TPlayer import }
{----------------}

function TSepiImportsTPlayer.GetVisible: boolean;
begin
  Result := Visible;
end;

procedure TSepiImportsTPlayer.MoveTo_0(const Dest : T3DPoint; Execute : boolean; out Redo : boolean );
begin
  MoveTo(Dest, Execute, Redo);
end;

procedure TSepiImportsTPlayer.MoveTo_1(const Dest : T3DPoint);
begin
  MoveTo(Dest);
end;

procedure TSepiImportsTPlayer.MoveTo_2(const Dest : TQualifiedPos; Execute : boolean; out Redo : boolean );
begin
  MoveTo(Dest, Execute, Redo);
end;

procedure TSepiImportsTPlayer.MoveTo_3(const Dest : TQualifiedPos);
begin
  MoveTo(Dest);
end;

class function TSepiImportsTPlayer.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TPlayer'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TPlayer));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FMap', System.TypeInfo(TMap));
    AddField('FPosition', 'T3DPoint');
    AddField('FDirection', System.TypeInfo(TDirection));
    AddField('FShowCounter', System.TypeInfo(integer));
    AddField('FColor', System.TypeInfo(TColor));
    AddField('FPlugins', System.TypeInfo(TObjectList));
    AddField('FAttributes', System.TypeInfo(TStrings));
    AddField('FOnSendCommand', System.TypeInfo(TSendCommandEvent));
    AddField('FPlayState', System.TypeInfo(TPlayState));

    AddMethod('PrivDraw', nil,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkOverride);
    AddMethod('GetVisible', @TSepiImportsTPlayer.GetVisible,
      'function: boolean');
    AddMethod('GetPluginCount', nil,
      'function: integer');
    AddMethod('GetPlugins', nil,
      'function(Index : integer) : TPlugin');

    AddProperty('PluginCount', 'property: integer',
      'GetPluginCount', '');
    AddProperty('Plugins', 'property[index : integer] : TPlugin',
      'GetPlugins', '');

    CurrentVisibility := mvProtected;

    AddMethod('DoDraw', @TSepiImportsTPlayer.DoDraw,
      'procedure(const QPos : TQualifiedPos; Canvas : TCanvas; X : integer = 0 ; Y : integer = 0 )',
      mlkOverride);
    AddMethod('GetAttribute', @TSepiImportsTPlayer.GetAttribute,
      'function(const AttrName : string) : integer',
      mlkVirtual);
    AddMethod('SetAttribute', @TSepiImportsTPlayer.SetAttribute,
      'procedure(const AttrName : string; Value : integer)',
      mlkVirtual);

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTPlayer.Create,
      'constructor(AMaster : TMaster; const AID : TComponentID; const AName : string ; AMap : TMap ; const APosition : T3DPoint )');
    AddMethod('Destroy', @TSepiImportsTPlayer.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('GetAttributes', @TSepiImportsTPlayer.GetAttributes,
      'procedure(Attributes : TStrings)',
      mlkVirtual);
    AddMethod('GetPluginIDs', @TSepiImportsTPlayer.GetPluginIDs,
      'procedure(PluginIDs : TStrings)');
    AddMethod('DrawInPlace', @TSepiImportsTPlayer.DrawInPlace,
      'procedure(Canvas : TCanvas; X : integer = 0; Y : integer = 0 )');
    AddMethod('AddPlugin', @TSepiImportsTPlayer.AddPlugin,
      'procedure(Plugin : TPlugin)');
    AddMethod('RemovePlugin', @TSepiImportsTPlayer.RemovePlugin,
      'procedure(Plugin : TPlugin)');
    AddMethod('AbleTo', @TSepiImportsTPlayer.AbleTo,
      'function(const Action : TPlayerAction) : boolean');
    AddMethod('DoAction', @TSepiImportsTPlayer.DoAction,
      'function(const Action : TPlayerAction) : boolean');
    AddMethod('Move', @TSepiImportsTPlayer.Move,
      'procedure(Dir : TDirection; KeyPressed : boolean; out Redo : boolean )');
    AddMethod('OL$MoveTo$0', @TSepiImportsTPlayer.MoveTo_0,
      'procedure(const Dest : T3DPoint; Execute : boolean; out Redo : boolean )');
    AddMethod('OL$MoveTo$1', @TSepiImportsTPlayer.MoveTo_1,
      'procedure(const Dest : T3DPoint)');
    AddMethod('OL$MoveTo$2', @TSepiImportsTPlayer.MoveTo_2,
      'procedure(const Dest : TQualifiedPos; Execute : boolean; out Redo : boolean )');
    AddMethod('OL$MoveTo$3', @TSepiImportsTPlayer.MoveTo_3,
      'procedure(const Dest : TQualifiedPos)');
    AddMethod('NaturalMoving', @TSepiImportsTPlayer.NaturalMoving,
      'procedure');
    AddMethod('ChangePosition', @TSepiImportsTPlayer.ChangePosition,
      'procedure(AMap : TMap; const APosition : T3DPoint)');
    AddMethod('Show', @TSepiImportsTPlayer.Show,
      'procedure');
    AddMethod('Hide', @TSepiImportsTPlayer.Hide,
      'procedure');
    AddMethod('SendCommand', @TSepiImportsTPlayer.SendCommand,
      'function(const Command : string; const Params : string = '''' ) : string');
    AddMethod('ShowDialog', @TSepiImportsTPlayer.ShowDialog,
      'function(const Title, Text : string; DlgType : TDialogType = dtInformation ; DlgButtons : TDialogButtons = dbOK ; DefButton : Byte = 1 ; AddFlags : LongWord = 0 ) : TDialogResult');
    AddMethod('ShowDialogRadio', @TSepiImportsTPlayer.ShowDialogRadio,
      'function(const Title, Text : string; DlgType : TMsgDlgType; DlgButtons : TMsgDlgButtons ; DefButton : TModalResult ; const RadioTitles : array of string ; var Selected : integer ; OverButtons : boolean = False ) : Word');
    AddMethod('ChooseNumber', @TSepiImportsTPlayer.ChooseNumber,
      'function(const Title, Prompt : string; Default, Min, Max : integer ) : integer');
    AddMethod('Win', @TSepiImportsTPlayer.Win,
      'procedure');
    AddMethod('Lose', @TSepiImportsTPlayer.Lose,
      'procedure');

    AddProperty('Map', 'property: TMap',
      'FMap', '');
    AddProperty('Position', 'property: T3DPoint',
      'FPosition', '');
    AddProperty('Direction', 'property: TDirection',
      'FDirection', 'FDirection');
    AddProperty('Visible', 'property: boolean',
      'GetVisible', '');
    AddProperty('Color', 'property: TColor',
      'FColor', 'FColor');
    AddProperty('Attribute', 'property[const AttrName : string] : integer',
      'GetAttribute', 'SetAttribute');
    AddProperty('OnSendCommand', 'property: TSendCommandEvent',
      'FOnSendCommand', 'FOnSendCommand');
    AddProperty('PlayState', 'property: TPlayState',
      'FPlayState', '');

    Complete;
  end;
end;

{----------------}
{ TMaster import }
{----------------}

function TSepiImportsTMaster.GetComponent(const ID : TComponentID) : TFunLabyComponent;
begin
  Result := Component[ID];
end;

function TSepiImportsTMaster.GetScrewComponent(const ID : TComponentID) : TScrewComponent;
begin
  Result := ScrewComponent[ID];
end;

function TSepiImportsTMaster.GetPlugin(const ID : TComponentID) : TPlugin;
begin
  Result := Plugin[ID];
end;

function TSepiImportsTMaster.GetObjectDef(const ID : TComponentID) : TObjectDef;
begin
  Result := ObjectDef[ID];
end;

function TSepiImportsTMaster.GetField(const ID : TComponentID) : TField;
begin
  Result := Field[ID];
end;

function TSepiImportsTMaster.GetEffect(const ID : TComponentID) : TEffect;
begin
  Result := Effect[ID];
end;

function TSepiImportsTMaster.GetTool(const ID : TComponentID) : TTool;
begin
  Result := Tool[ID];
end;

function TSepiImportsTMaster.GetObstacle(const ID : TComponentID) : TObstacle;
begin
  Result := Obstacle[ID];
end;

function TSepiImportsTMaster.GetScrew(const ID : TComponentID) : TScrew;
begin
  Result := Screw[ID];
end;

function TSepiImportsTMaster.GetMap(const ID : TComponentID) : TMap;
begin
  Result := Map[ID];
end;

function TSepiImportsTMaster.GetPlayer(const ID : TComponentID) : TPlayer;
begin
  Result := Player[ID];
end;

function TSepiImportsTMaster.GetPluginCount: integer;
begin
  Result := PluginCount;
end;

function TSepiImportsTMaster.GetPlugins(Index : integer) : TPlugin;
begin
  Result := Plugins[Index];
end;

function TSepiImportsTMaster.GetObjectDefCount: integer;
begin
  Result := ObjectDefCount;
end;

function TSepiImportsTMaster.GetObjectDefs(Index : integer) : TObjectDef;
begin
  Result := ObjectDefs[Index];
end;

function TSepiImportsTMaster.GetFieldCount: integer;
begin
  Result := FieldCount;
end;

function TSepiImportsTMaster.GetFields(Index : integer) : TField;
begin
  Result := Fields[Index];
end;

function TSepiImportsTMaster.GetEffectCount: integer;
begin
  Result := EffectCount;
end;

function TSepiImportsTMaster.GetEffects(Index : integer) : TEffect;
begin
  Result := Effects[Index];
end;

function TSepiImportsTMaster.GetToolCount: integer;
begin
  Result := ToolCount;
end;

function TSepiImportsTMaster.GetTools(Index : integer) : TTool;
begin
  Result := Tools[Index];
end;

function TSepiImportsTMaster.GetObstacleCount: integer;
begin
  Result := ObstacleCount;
end;

function TSepiImportsTMaster.GetObstacles(Index : integer) : TObstacle;
begin
  Result := Obstacles[Index];
end;

function TSepiImportsTMaster.GetScrewCount: integer;
begin
  Result := ScrewCount;
end;

function TSepiImportsTMaster.GetScrews(Index : integer) : TScrew;
begin
  Result := Screws[Index];
end;

function TSepiImportsTMaster.GetMapCount: integer;
begin
  Result := MapCount;
end;

function TSepiImportsTMaster.GetMaps(Index : integer) : TMap;
begin
  Result := Maps[Index];
end;

function TSepiImportsTMaster.GetPlayerCount: integer;
begin
  Result := PlayerCount;
end;

function TSepiImportsTMaster.GetPlayers(Index : integer) : TPlayer;
begin
  Result := Players[Index];
end;

procedure TSepiImportsTMaster.SetTemporization(Value : integer);
begin
  Temporization := Value;
end;

class function TSepiImportsTMaster.SepiImport(
  Owner : TSepiMetaUnit) : TSepiClass;
begin
  Result := TSepiClass(Owner.FindMeta('TMaster'));
  Result.RegisterTypeInfo(
    Owner, TypeInfo(TMaster));

  with Result do
  begin
    CurrentVisibility := mvPrivate;

    AddField('FImagesMaster', System.TypeInfo(TImagesMaster));
    AddField('FComponents', System.TypeInfo(TStrings));
    AddField('FPlugins', System.TypeInfo(TObjectList));
    AddField('FObjectDefs', System.TypeInfo(TObjectList));
    AddField('FFields', System.TypeInfo(TObjectList));
    AddField('FEffects', System.TypeInfo(TObjectList));
    AddField('FTools', System.TypeInfo(TObjectList));
    AddField('FObstacles', System.TypeInfo(TObjectList));
    AddField('FScrews', System.TypeInfo(TObjectList));
    AddField('FMaps', System.TypeInfo(TObjectList));
    AddField('FPlayers', System.TypeInfo(TObjectList));
    AddField('FEditing', System.TypeInfo(boolean));
    AddField('FTemporization', System.TypeInfo(integer));
    AddField('FBeginTickCount', System.TypeInfo(Cardinal));
    AddField('FTickCount', System.TypeInfo(Cardinal));
    AddField('FTerminated', System.TypeInfo(boolean));

    AddMethod('GetComponent', @TSepiImportsTMaster.GetComponent,
      'function(const ID : TComponentID) : TFunLabyComponent');
    AddMethod('GetScrewComponent', @TSepiImportsTMaster.GetScrewComponent,
      'function(const ID : TComponentID) : TScrewComponent');
    AddMethod('GetPlugin', @TSepiImportsTMaster.GetPlugin,
      'function(const ID : TComponentID) : TPlugin');
    AddMethod('GetObjectDef', @TSepiImportsTMaster.GetObjectDef,
      'function(const ID : TComponentID) : TObjectDef');
    AddMethod('GetField', @TSepiImportsTMaster.GetField,
      'function(const ID : TComponentID) : TField');
    AddMethod('GetEffect', @TSepiImportsTMaster.GetEffect,
      'function(const ID : TComponentID) : TEffect');
    AddMethod('GetTool', @TSepiImportsTMaster.GetTool,
      'function(const ID : TComponentID) : TTool');
    AddMethod('GetObstacle', @TSepiImportsTMaster.GetObstacle,
      'function(const ID : TComponentID) : TObstacle');
    AddMethod('GetScrew', @TSepiImportsTMaster.GetScrew,
      'function(const ID : TComponentID) : TScrew');
    AddMethod('GetMap', @TSepiImportsTMaster.GetMap,
      'function(const ID : TComponentID) : TMap');
    AddMethod('GetPlayer', @TSepiImportsTMaster.GetPlayer,
      'function(const ID : TComponentID) : TPlayer');
    AddMethod('GetPluginCount', @TSepiImportsTMaster.GetPluginCount,
      'function: integer');
    AddMethod('GetPlugins', @TSepiImportsTMaster.GetPlugins,
      'function(Index : integer) : TPlugin');
    AddMethod('GetObjectDefCount', @TSepiImportsTMaster.GetObjectDefCount,
      'function: integer');
    AddMethod('GetObjectDefs', @TSepiImportsTMaster.GetObjectDefs,
      'function(Index : integer) : TObjectDef');
    AddMethod('GetFieldCount', @TSepiImportsTMaster.GetFieldCount,
      'function: integer');
    AddMethod('GetFields', @TSepiImportsTMaster.GetFields,
      'function(Index : integer) : TField');
    AddMethod('GetEffectCount', @TSepiImportsTMaster.GetEffectCount,
      'function: integer');
    AddMethod('GetEffects', @TSepiImportsTMaster.GetEffects,
      'function(Index : integer) : TEffect');
    AddMethod('GetToolCount', @TSepiImportsTMaster.GetToolCount,
      'function: integer');
    AddMethod('GetTools', @TSepiImportsTMaster.GetTools,
      'function(Index : integer) : TTool');
    AddMethod('GetObstacleCount', @TSepiImportsTMaster.GetObstacleCount,
      'function: integer');
    AddMethod('GetObstacles', @TSepiImportsTMaster.GetObstacles,
      'function(Index : integer) : TObstacle');
    AddMethod('GetScrewCount', @TSepiImportsTMaster.GetScrewCount,
      'function: integer');
    AddMethod('GetScrews', @TSepiImportsTMaster.GetScrews,
      'function(Index : integer) : TScrew');
    AddMethod('GetMapCount', @TSepiImportsTMaster.GetMapCount,
      'function: integer');
    AddMethod('GetMaps', @TSepiImportsTMaster.GetMaps,
      'function(Index : integer) : TMap');
    AddMethod('GetPlayerCount', @TSepiImportsTMaster.GetPlayerCount,
      'function: integer');
    AddMethod('GetPlayers', @TSepiImportsTMaster.GetPlayers,
      'function(Index : integer) : TPlayer');
    AddMethod('SetTemporization', @TSepiImportsTMaster.SetTemporization,
      'procedure(Value : integer)');
    AddMethod('AddComponent', nil,
      'procedure(Component : TFunLabyComponent)');
    AddMethod('RemoveComponent', nil,
      'procedure(Component : TFunLabyComponent)');
    AddMethod('Terminate', nil,
      'procedure');

    CurrentVisibility := mvPublic;

    AddMethod('Create', @TSepiImportsTMaster.Create,
      'constructor(AEditing : boolean)');
    AddMethod('Destroy', @TSepiImportsTMaster.Destroy,
      'destructor',
      mlkOverride);
    AddMethod('Temporize', @TSepiImportsTMaster.Temporize,
      'procedure');
    AddMethod('UpdateTickCount', @TSepiImportsTMaster.UpdateTickCount,
      'procedure');
    AddMethod('ScrewByComps', @TSepiImportsTMaster.ScrewByComps,
      'function( const Field, Effect, Tool, Obstacle : TComponentID ) : TScrew');

    AddProperty('ImagesMaster', 'property: TImagesMaster',
      'FImagesMaster', '');
    AddProperty('Component', 'property[const ID : TComponentID] : TFunLabyComponent',
      'GetComponent', '');
    AddProperty('ScrewComponent', 'property[const ID : TComponentID] : TScrewComponent',
      'GetScrewComponent', '');
    AddProperty('Plugin', 'property[const ID : TComponentID] : TPlugin',
      'GetPlugin', '');
    AddProperty('ObjectDef', 'property[const ID : TComponentID] : TObjectDef',
      'GetObjectDef', '');
    AddProperty('Field', 'property[const ID : TComponentID] : TField',
      'GetField', '');
    AddProperty('Effect', 'property[const ID : TComponentID] : TEffect',
      'GetEffect', '');
    AddProperty('Tool', 'property[const ID : TComponentID] : TTool',
      'GetTool', '');
    AddProperty('Obstacle', 'property[const ID : TComponentID] : TObstacle',
      'GetObstacle', '');
    AddProperty('Screw', 'property[const ID : TComponentID] : TScrew',
      'GetScrew', '');
    AddProperty('Map', 'property[const ID : TComponentID] : TMap',
      'GetMap', '');
    AddProperty('Player', 'property[const ID : TComponentID] : TPlayer',
      'GetPlayer', '');
    AddProperty('PluginCount', 'property: integer',
      'GetPluginCount', '');
    AddProperty('Plugins', 'property[index : integer] : TPlugin',
      'GetPlugins', '');
    AddProperty('ObjectDefCount', 'property: integer',
      'GetObjectDefCount', '');
    AddProperty('ObjectDefs', 'property[index : integer] : TObjectDef',
      'GetObjectDefs', '');
    AddProperty('FieldCount', 'property: integer',
      'GetFieldCount', '');
    AddProperty('Fields', 'property[index : integer] : TField',
      'GetFields', '');
    AddProperty('EffectCount', 'property: integer',
      'GetEffectCount', '');
    AddProperty('Effects', 'property[index : integer] : TEffect',
      'GetEffects', '');
    AddProperty('ToolCount', 'property: integer',
      'GetToolCount', '');
    AddProperty('Tools', 'property[index : integer] : TTool',
      'GetTools', '');
    AddProperty('ObstacleCount', 'property: integer',
      'GetObstacleCount', '');
    AddProperty('Obstacles', 'property[index : integer] : TObstacle',
      'GetObstacles', '');
    AddProperty('ScrewCount', 'property: integer',
      'GetScrewCount', '');
    AddProperty('Screws', 'property[index : integer] : TScrew',
      'GetScrews', '');
    AddProperty('MapCount', 'property: integer',
      'GetMapCount', '');
    AddProperty('Maps', 'property[index : integer] : TMap',
      'GetMaps', '');
    AddProperty('PlayerCount', 'property: integer',
      'GetPlayerCount', '');
    AddProperty('Players', 'property[index : integer] : TPlayer',
      'GetPlayers', '');
    AddProperty('Editing', 'property: boolean',
      'FEditing', '');
    AddProperty('Temporization', 'property: integer',
      'FTemporization', 'SetTemporization');
    AddProperty('TickCount', 'property: Cardinal',
      'FTickCount', '');
    AddProperty('Terminated', 'property: boolean',
      'FTerminated', '');

    Complete;
  end;
end;

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'FunLabyUtils',
    ['Windows', 'Types', 'SysUtils', 'Classes', 'Graphics', 'Contnrs',
     'Controls', 'Dialogs', 'TypInfo', 'ScUtils', 'SdDialogs']);

  // Constants
  TSepiConstant.Create(Result, 'sDefaultObjectInfos', sDefaultObjectInfos);
  TSepiConstant.Create(Result, 'sNothing', sNothing);
  TSepiConstant.Create(Result, 'sEffectName', sEffectName);
  TSepiConstant.Create(Result, 'sToolName', sToolName);
  TSepiConstant.Create(Result, 'sObstacleName', sObstacleName);
  TSepiConstant.Create(Result, 'sWhichObject', sWhichObject);
  TSepiConstant.Create(Result, 'sComponentNotFound', sComponentNotFound);
  TSepiConstant.Create(Result, 'sUnsupportedCommand', sUnsupportedCommand);
  TSepiConstant.Create(Result, 'sCantLaunchThroughNetwork', sCantLaunchThroughNetwork);
  TSepiConstant.Create(Result, 'sDescription', sDescription);
  TSepiConstant.Create(Result, 'sMessage', sMessage);
  TSepiConstant.Create(Result, 'sTip', sTip);
  TSepiConstant.Create(Result, 'sChoice', sChoice);
  TSepiConstant.Create(Result, 'sError', sError);
  TSepiConstant.Create(Result, 'sFailure', sFailure);
  TSepiConstant.Create(Result, 'sBlindAlley', sBlindAlley);
  TSepiConstant.Create(Result, 'sWon', sWon);
  TSepiConstant.Create(Result, 'sLost', sLost);

  // Constants
  TSepiConstant.Create(Result, 'CurrentVersion', CurrentVersion);
  TSepiConstant.Create(Result, 'FunLabyAuthorName', FunLabyAuthorName);
  TSepiConstant.Create(Result, 'FunLabyAuthorEMail', FunLabyAuthorEMail);
  TSepiConstant.Create(Result, 'FunLabyWebSite', FunLabyWebSite);
  TSepiConstant.Create(Result, 'ScrewSize', ScrewSize);
  TSepiConstant.Create(Result, 'MinViewSize', MinViewSize);
  TSepiConstant.Create(Result, 'clTransparent', clTransparent);
  TSepiConstant.Create(Result, 'NoRefCount', NoRefCount);
  TSepiConstant.Create(Result, 'attrColor', attrColor);
  TSepiConstant.Create(Result, 'attrShowCounter', attrShowCounter);
  TSepiConstant.Create(Result, 'CommandShowDialog', CommandShowDialog);
  TSepiConstant.Create(Result, 'CommandShowDialogRadio', CommandShowDialogRadio);
  TSepiConstant.Create(Result, 'CommandChooseNumber', CommandChooseNumber);
  TSepiConstant.Create(Result, 'ScrewIDDelim', ScrewIDDelim);
  TSepiConstant.Create(Result, 'ScrewIDFormat', ScrewIDFormat);

  // Types
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TComponentID));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TDirection));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPlayerAction));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TPlayState));
  TSepiImportsEComponentNotFound.SepiImport(Result);
  TSepiImportsEUnsupportedCommand.SepiImport(Result);
  TSepiClass.ForwardDecl(Result, TypeInfo(TScrewComponent));
  TSepiClass.ForwardDecl(Result, TypeInfo(TPlayer));
  TSepiClass.ForwardDecl(Result, TypeInfo(TMap));
  TSepiClass.ForwardDecl(Result, TypeInfo(TMaster));
  SepiImportTQualifiedPos(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TSendCommandEvent));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRegisterSingleComponentProc));
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TRegisterComponentSetProc));
  TSepiImportsTImagesMaster.SepiImport(Result);
  TSepiImportsTScrewBitmap.SepiImport(Result);
  TSepiImportsTPainter.SepiImport(Result);
  TSepiImportsTFunLabyComponent.SepiImport(Result);
  TSepiImportsTVisualComponent.SepiImport(Result);
  TSepiImportsTPlugin.SepiImport(Result);
  TSepiImportsTObjectDef.SepiImport(Result);
  TSepiImportsTScrewComponent.SepiImport(Result);
  TSepiImportsTField.SepiImport(Result);
  TSepiImportsTEffect.SepiImport(Result);
  TSepiImportsTTool.SepiImport(Result);
  TSepiImportsTObstacle.SepiImport(Result);
  TSepiImportsTScrew.SepiImport(Result);
  TSepiType.LoadFromTypeInfo(Result, TypeInfo(TUnnamed_1));
  TSepiImportsTMap.SepiImport(Result);
  TSepiImportsTPlayer.SepiImport(Result);
  TSepiImportsTMaster.SepiImport(Result);

  // Constants
  TSepiConstant.Create(Result, 'fIniFileName', fIniFileName);
  TSepiVariable.Create(Result, 'NoQPos',
     NoQPos, 'TQualifiedPos', True);
  TSepiConstant.Create(Result, 'DefaultTemporization', DefaultTemporization);
  TSepiConstant.Create(Result, 'DefaultPlayerColor', DefaultPlayerColor);
  TSepiArrayType.Create(Result, '$2',
    [Integer(Low(TDirection)), Integer(High(TDirection))], TypeInfo(TDirection), True);
  TSepiVariable.Create(Result, 'NegDir',
     NegDir, '$2', True);

  // Global variables
  TSepiVariable.Create(Result, 'fFunLabyAppData',
     fFunLabyAppData, TypeInfo(string));
  TSepiVariable.Create(Result, 'fScrewsDir',
     fScrewsDir, TypeInfo(string));
  TSepiVariable.Create(Result, 'fSoundsDir',
     fSoundsDir, TypeInfo(string));
  TSepiVariable.Create(Result, 'fUnitsDir',
     fUnitsDir, TypeInfo(string));
  TSepiVariable.Create(Result, 'fMapsDir',
     fMapsDir, TypeInfo(string));
  TSepiVariable.Create(Result, 'fLabyrinthsDir',
     fLabyrinthsDir, TypeInfo(string));
  TSepiVariable.Create(Result, 'fSaveguardsDir',
     fSaveguardsDir, TypeInfo(string));
  TSepiVariable.Create(Result, 'fScrewFileName',
     fScrewFileName, TypeInfo(string));

  // Routines
  TSepiMetaMethod.Create(Result, 'CheckValidLaunch', @CheckValidLaunch,
    'function: boolean');
  TSepiMetaMethod.Create(Result, 'ShowFunLabyAbout', @ShowFunLabyAbout,
    'procedure');
  TSepiMetaMethod.Create(Result, 'PointBehind', @PointBehind,
    'function(const Src : T3DPoint; Dir : TDirection) : T3DPoint');
  TSepiMetaMethod.Create(Result, 'ScrewRect', @ScrewRect,
    'function(X : integer = 0; Y : integer = 0) : TRect');
  TSepiMetaMethod.Create(Result, 'EmptyRect', @EmptyRect,
    'procedure(Canvas : TCanvas; Rect : TRect)');
  TSepiMetaMethod.Create(Result, 'EmptyScrewRect', @EmptyScrewRect,
    'procedure(Canvas : TCanvas; X : integer = 0; Y : integer = 0)');
  TSepiMetaMethod.Create(Result, 'IsNoQPos', @IsNoQPos,
    'function(const QPos : TQualifiedPos) : boolean');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('FunLabyUtils', ImportUnit);
end.

