{*
  Importe l'unité FunLabyUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFunLabyUtils;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  Graphics, ScUtils, FunLabyUtils;

var
  SepiImportsFunLabyUtilsLazyLoad: Boolean = False;

implementation

{$R *.res}

const // don't localize
  UnitName = 'FunLabyUtils';
  ResourceName = 'SepiImportsFunLabyUtils';
  TypeCount = 22;
  MethodCount = 93;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;

type
  TSepiImportsTImagesMaster = class(TImagesMaster)
  private
    procedure Draw_0(Index: Integer; Canvas: TCanvas; X: Integer; Y: Integer);
    procedure Draw_1(const ImgName: string; Canvas: TCanvas; X: Integer; Y: Integer);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTSquareBitmap = class(TSquareBitmap)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTPainter = class(TPainter)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTFunLabyComponent = class(TFunLabyComponent)
  private
    function GetSafeID: TComponentID;
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTVisualComponent = class(TVisualComponent)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTPlugin = class(TPlugin)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTField = class(TField)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTSquare = class(TSquare)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTMap = class(TMap)
  private
    procedure SetMaxViewSize(Value: Integer);
    function GetMap(const Position: T3DPoint): TSquare;
    procedure SetMap(const Position: T3DPoint; Value: TSquare);
    function GetOutside(Floor: Integer): TSquare;
    procedure SetOutside(Floor: Integer; Value: TSquare);
    function GetLinearMapCount: Integer;
    function GetLinearMap(Index: Integer): TSquare;
    procedure SetLinearMap(Index: Integer; Value: TSquare);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTPlayer = class(TPlayer)
  private
    function GetVisible: Boolean;
    procedure MoveTo_0(const Dest: T3DPoint; Execute: Boolean; out Redo: Boolean);
    procedure MoveTo_1(const Dest: T3DPoint);
    procedure MoveTo_2(const Dest: TQualifiedPos; Execute: Boolean; out Redo: Boolean);
    procedure MoveTo_3(const Dest: TQualifiedPos);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTMaster = class(TMaster)
  private
    function GetComponent(const ID: TComponentID): TFunLabyComponent;
    function GetSquareComponent(const ID: TComponentID): TSquareComponent;
    function GetPlugin(const ID: TComponentID): TPlugin;
    function GetObjectDef(const ID: TComponentID): TObjectDef;
    function GetField(const ID: TComponentID): TField;
    function GetEffect(const ID: TComponentID): TEffect;
    function GetTool(const ID: TComponentID): TTool;
    function GetObstacle(const ID: TComponentID): TObstacle;
    function GetSquare(const ID: TComponentID): TSquare;
    function GetMap(const ID: TComponentID): TMap;
    function GetPlayer(const ID: TComponentID): TPlayer;
    function GetPluginCount: Integer;
    function GetPlugins(Index: Integer): TPlugin;
    function GetObjectDefCount: Integer;
    function GetObjectDefs(Index: Integer): TObjectDef;
    function GetFieldCount: Integer;
    function GetFields(Index: Integer): TField;
    function GetEffectCount: Integer;
    function GetEffects(Index: Integer): TEffect;
    function GetToolCount: Integer;
    function GetTools(Index: Integer): TTool;
    function GetObstacleCount: Integer;
    function GetObstacles(Index: Integer): TObstacle;
    function GetSquareCount: Integer;
    function GetSquares(Index: Integer): TSquare;
    function GetMapCount: Integer;
    function GetMaps(Index: Integer): TMap;
    function GetPlayerCount: Integer;
    function GetPlayers(Index: Integer): TPlayer;
    procedure SetTemporization(Value: Integer);
    class procedure InitMethodAddresses;
  end;

{----------------------}
{ TImagesMaster import }
{----------------------}

procedure TSepiImportsTImagesMaster.Draw_0(Index: Integer; Canvas: TCanvas; X: Integer; Y: Integer);
begin
  Draw(Index, Canvas, X, Y);
end;

procedure TSepiImportsTImagesMaster.Draw_1(const ImgName: string; Canvas: TCanvas; X: Integer; Y: Integer);
begin
  Draw(ImgName, Canvas, X, Y);
end;

class procedure TSepiImportsTImagesMaster.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTImagesMaster.Create;
  MethodAddresses[1] := @TSepiImportsTImagesMaster.Add;
  MethodAddresses[2] := @TSepiImportsTImagesMaster.IndexOf;
  MethodAddresses[3] := @TSepiImportsTImagesMaster.Draw_0;
  MethodAddresses[4] := @TSepiImportsTImagesMaster.Draw_1;
end;

{----------------------}
{ TSquareBitmap import }
{----------------------}

class procedure TSepiImportsTSquareBitmap.InitMethodAddresses;
begin
  MethodAddresses[5] := @TSepiImportsTSquareBitmap.EmptySquare;
  MethodAddresses[6] := @TSepiImportsTSquareBitmap.DrawSquare;
end;

{-----------------}
{ TPainter import }
{-----------------}

class procedure TSepiImportsTPainter.InitMethodAddresses;
begin
  MethodAddresses[7] := @TSepiImportsTPainter.Create;
  MethodAddresses[8] := @TSepiImportsTPainter.Draw;
end;

{--------------------------}
{ TFunLabyComponent import }
{--------------------------}

function TSepiImportsTFunLabyComponent.GetSafeID: TComponentID;
begin
  Result := SafeID;
end;

class procedure TSepiImportsTFunLabyComponent.InitMethodAddresses;
begin
  MethodAddresses[9] := @TSepiImportsTFunLabyComponent.GetSafeID;
  MethodAddresses[10] := @TSepiImportsTFunLabyComponent.Create;
end;

{-------------------------}
{ TVisualComponent import }
{-------------------------}

class procedure TSepiImportsTVisualComponent.InitMethodAddresses;
begin
  MethodAddresses[11] := @TSepiImportsTVisualComponent.Create;
  MethodAddresses[12] := @TSepiImportsTVisualComponent.Draw;
end;

{----------------}
{ TPlugin import }
{----------------}

class procedure TSepiImportsTPlugin.InitMethodAddresses;
begin
  MethodAddresses[13] := @TSepiImportsTPlugin.Create;
end;

{---------------}
{ TField import }
{---------------}

class procedure TSepiImportsTField.InitMethodAddresses;
begin
  MethodAddresses[14] := @TSepiImportsTField.Create;
end;

{----------------}
{ TSquare import }
{----------------}

class procedure TSepiImportsTSquare.InitMethodAddresses;
begin
  MethodAddresses[15] := @TSepiImportsTSquare.Create;
end;

{-------------}
{ TMap import }
{-------------}

procedure TSepiImportsTMap.SetMaxViewSize(Value: Integer);
begin
  MaxViewSize := Value;
end;

function TSepiImportsTMap.GetMap(const Position: T3DPoint): TSquare;
begin
  Result := Map[Position];
end;

procedure TSepiImportsTMap.SetMap(const Position: T3DPoint; Value: TSquare);
begin
  Map[Position] := Value;
end;

function TSepiImportsTMap.GetOutside(Floor: Integer): TSquare;
begin
  Result := Outside[Floor];
end;

procedure TSepiImportsTMap.SetOutside(Floor: Integer; Value: TSquare);
begin
  Outside[Floor] := Value;
end;

function TSepiImportsTMap.GetLinearMapCount: Integer;
begin
  Result := LinearMapCount;
end;

function TSepiImportsTMap.GetLinearMap(Index: Integer): TSquare;
begin
  Result := LinearMap[Index];
end;

procedure TSepiImportsTMap.SetLinearMap(Index: Integer; Value: TSquare);
begin
  LinearMap[Index] := Value;
end;

class procedure TSepiImportsTMap.InitMethodAddresses;
begin
  MethodAddresses[16] := @TSepiImportsTMap.SetMaxViewSize;
  MethodAddresses[17] := @TSepiImportsTMap.GetMap;
  MethodAddresses[18] := @TSepiImportsTMap.SetMap;
  MethodAddresses[19] := @TSepiImportsTMap.GetOutside;
  MethodAddresses[20] := @TSepiImportsTMap.SetOutside;
  MethodAddresses[21] := @TSepiImportsTMap.GetLinearMapCount;
  MethodAddresses[22] := @TSepiImportsTMap.GetLinearMap;
  MethodAddresses[23] := @TSepiImportsTMap.SetLinearMap;
  MethodAddresses[24] := @TSepiImportsTMap.Create;
  MethodAddresses[25] := @TSepiImportsTMap.InMap;
  MethodAddresses[26] := @TSepiImportsTMap.PlayersOn;
end;

{----------------}
{ TPlayer import }
{----------------}

function TSepiImportsTPlayer.GetVisible: Boolean;
begin
  Result := Visible;
end;

procedure TSepiImportsTPlayer.MoveTo_0(const Dest: T3DPoint; Execute: Boolean; out Redo: Boolean);
begin
  MoveTo(Dest, Execute, Redo);
end;

procedure TSepiImportsTPlayer.MoveTo_1(const Dest: T3DPoint);
begin
  MoveTo(Dest);
end;

procedure TSepiImportsTPlayer.MoveTo_2(const Dest: TQualifiedPos; Execute: Boolean; out Redo: Boolean);
begin
  MoveTo(Dest, Execute, Redo);
end;

procedure TSepiImportsTPlayer.MoveTo_3(const Dest: TQualifiedPos);
begin
  MoveTo(Dest);
end;

class procedure TSepiImportsTPlayer.InitMethodAddresses;
begin
  MethodAddresses[27] := @TSepiImportsTPlayer.GetVisible;
  MethodAddresses[28] := @TSepiImportsTPlayer.Create;
  MethodAddresses[29] := @TSepiImportsTPlayer.GetPluginIDs;
  MethodAddresses[30] := @TSepiImportsTPlayer.DrawInPlace;
  MethodAddresses[31] := @TSepiImportsTPlayer.AddPlugin;
  MethodAddresses[32] := @TSepiImportsTPlayer.RemovePlugin;
  MethodAddresses[33] := @TSepiImportsTPlayer.AbleTo;
  MethodAddresses[34] := @TSepiImportsTPlayer.DoAction;
  MethodAddresses[35] := @TSepiImportsTPlayer.Move;
  MethodAddresses[36] := @TSepiImportsTPlayer.MoveTo_0;
  MethodAddresses[37] := @TSepiImportsTPlayer.MoveTo_1;
  MethodAddresses[38] := @TSepiImportsTPlayer.MoveTo_2;
  MethodAddresses[39] := @TSepiImportsTPlayer.MoveTo_3;
  MethodAddresses[40] := @TSepiImportsTPlayer.Temporize;
  MethodAddresses[41] := @TSepiImportsTPlayer.NaturalMoving;
  MethodAddresses[42] := @TSepiImportsTPlayer.ChangePosition;
  MethodAddresses[43] := @TSepiImportsTPlayer.Show;
  MethodAddresses[44] := @TSepiImportsTPlayer.Hide;
  MethodAddresses[45] := @TSepiImportsTPlayer.SendCommand;
  MethodAddresses[46] := @TSepiImportsTPlayer.ShowDialog;
  MethodAddresses[47] := @TSepiImportsTPlayer.ShowDialogRadio;
  MethodAddresses[48] := @TSepiImportsTPlayer.ChooseNumber;
  MethodAddresses[49] := @TSepiImportsTPlayer.Win;
  MethodAddresses[50] := @TSepiImportsTPlayer.Lose;
end;

{----------------}
{ TMaster import }
{----------------}

function TSepiImportsTMaster.GetComponent(const ID: TComponentID): TFunLabyComponent;
begin
  Result := Component[ID];
end;

function TSepiImportsTMaster.GetSquareComponent(const ID: TComponentID): TSquareComponent;
begin
  Result := SquareComponent[ID];
end;

function TSepiImportsTMaster.GetPlugin(const ID: TComponentID): TPlugin;
begin
  Result := Plugin[ID];
end;

function TSepiImportsTMaster.GetObjectDef(const ID: TComponentID): TObjectDef;
begin
  Result := ObjectDef[ID];
end;

function TSepiImportsTMaster.GetField(const ID: TComponentID): TField;
begin
  Result := Field[ID];
end;

function TSepiImportsTMaster.GetEffect(const ID: TComponentID): TEffect;
begin
  Result := Effect[ID];
end;

function TSepiImportsTMaster.GetTool(const ID: TComponentID): TTool;
begin
  Result := Tool[ID];
end;

function TSepiImportsTMaster.GetObstacle(const ID: TComponentID): TObstacle;
begin
  Result := Obstacle[ID];
end;

function TSepiImportsTMaster.GetSquare(const ID: TComponentID): TSquare;
begin
  Result := Square[ID];
end;

function TSepiImportsTMaster.GetMap(const ID: TComponentID): TMap;
begin
  Result := Map[ID];
end;

function TSepiImportsTMaster.GetPlayer(const ID: TComponentID): TPlayer;
begin
  Result := Player[ID];
end;

function TSepiImportsTMaster.GetPluginCount: Integer;
begin
  Result := PluginCount;
end;

function TSepiImportsTMaster.GetPlugins(Index: Integer): TPlugin;
begin
  Result := Plugins[Index];
end;

function TSepiImportsTMaster.GetObjectDefCount: Integer;
begin
  Result := ObjectDefCount;
end;

function TSepiImportsTMaster.GetObjectDefs(Index: Integer): TObjectDef;
begin
  Result := ObjectDefs[Index];
end;

function TSepiImportsTMaster.GetFieldCount: Integer;
begin
  Result := FieldCount;
end;

function TSepiImportsTMaster.GetFields(Index: Integer): TField;
begin
  Result := Fields[Index];
end;

function TSepiImportsTMaster.GetEffectCount: Integer;
begin
  Result := EffectCount;
end;

function TSepiImportsTMaster.GetEffects(Index: Integer): TEffect;
begin
  Result := Effects[Index];
end;

function TSepiImportsTMaster.GetToolCount: Integer;
begin
  Result := ToolCount;
end;

function TSepiImportsTMaster.GetTools(Index: Integer): TTool;
begin
  Result := Tools[Index];
end;

function TSepiImportsTMaster.GetObstacleCount: Integer;
begin
  Result := ObstacleCount;
end;

function TSepiImportsTMaster.GetObstacles(Index: Integer): TObstacle;
begin
  Result := Obstacles[Index];
end;

function TSepiImportsTMaster.GetSquareCount: Integer;
begin
  Result := SquareCount;
end;

function TSepiImportsTMaster.GetSquares(Index: Integer): TSquare;
begin
  Result := Squares[Index];
end;

function TSepiImportsTMaster.GetMapCount: Integer;
begin
  Result := MapCount;
end;

function TSepiImportsTMaster.GetMaps(Index: Integer): TMap;
begin
  Result := Maps[Index];
end;

function TSepiImportsTMaster.GetPlayerCount: Integer;
begin
  Result := PlayerCount;
end;

function TSepiImportsTMaster.GetPlayers(Index: Integer): TPlayer;
begin
  Result := Players[Index];
end;

procedure TSepiImportsTMaster.SetTemporization(Value: Integer);
begin
  Temporization := Value;
end;

class procedure TSepiImportsTMaster.InitMethodAddresses;
begin
  MethodAddresses[51] := @TSepiImportsTMaster.GetComponent;
  MethodAddresses[52] := @TSepiImportsTMaster.GetSquareComponent;
  MethodAddresses[53] := @TSepiImportsTMaster.GetPlugin;
  MethodAddresses[54] := @TSepiImportsTMaster.GetObjectDef;
  MethodAddresses[55] := @TSepiImportsTMaster.GetField;
  MethodAddresses[56] := @TSepiImportsTMaster.GetEffect;
  MethodAddresses[57] := @TSepiImportsTMaster.GetTool;
  MethodAddresses[58] := @TSepiImportsTMaster.GetObstacle;
  MethodAddresses[59] := @TSepiImportsTMaster.GetSquare;
  MethodAddresses[60] := @TSepiImportsTMaster.GetMap;
  MethodAddresses[61] := @TSepiImportsTMaster.GetPlayer;
  MethodAddresses[62] := @TSepiImportsTMaster.GetPluginCount;
  MethodAddresses[63] := @TSepiImportsTMaster.GetPlugins;
  MethodAddresses[64] := @TSepiImportsTMaster.GetObjectDefCount;
  MethodAddresses[65] := @TSepiImportsTMaster.GetObjectDefs;
  MethodAddresses[66] := @TSepiImportsTMaster.GetFieldCount;
  MethodAddresses[67] := @TSepiImportsTMaster.GetFields;
  MethodAddresses[68] := @TSepiImportsTMaster.GetEffectCount;
  MethodAddresses[69] := @TSepiImportsTMaster.GetEffects;
  MethodAddresses[70] := @TSepiImportsTMaster.GetToolCount;
  MethodAddresses[71] := @TSepiImportsTMaster.GetTools;
  MethodAddresses[72] := @TSepiImportsTMaster.GetObstacleCount;
  MethodAddresses[73] := @TSepiImportsTMaster.GetObstacles;
  MethodAddresses[74] := @TSepiImportsTMaster.GetSquareCount;
  MethodAddresses[75] := @TSepiImportsTMaster.GetSquares;
  MethodAddresses[76] := @TSepiImportsTMaster.GetMapCount;
  MethodAddresses[77] := @TSepiImportsTMaster.GetMaps;
  MethodAddresses[78] := @TSepiImportsTMaster.GetPlayerCount;
  MethodAddresses[79] := @TSepiImportsTMaster.GetPlayers;
  MethodAddresses[80] := @TSepiImportsTMaster.SetTemporization;
  MethodAddresses[81] := @TSepiImportsTMaster.Create;
  MethodAddresses[82] := @TSepiImportsTMaster.Temporize;
  MethodAddresses[83] := @TSepiImportsTMaster.UpdateTickCount;
  MethodAddresses[84] := @TSepiImportsTMaster.SquareByComps;
end;

{---------------------}
{ Overloaded routines }
{---------------------}

{-------------}
{ Unit import }
{-------------}

procedure GetMethodCode(Self, Sender: TObject; var Code: Pointer;
  var CodeHandler: TObject);
var
  Index: Integer;
begin
  Index := (Sender as TSepiMethod).Tag;
  if Index >= 0 then
    Code := MethodAddresses[Index];
end;

procedure GetTypeInfo(Self, Sender: TObject; var TypeInfo: PTypeInfo;
  var Found: Boolean);
var
  Index: Integer;
begin
  Index := (Sender as TSepiType).Tag;
  Found := Index >= -1;

  if Index >= 0 then
    TypeInfo := TypeInfoArray[Index];
end;

const
  GetMethodCodeEvent: TMethod = (Code: @GetMethodCode; Data: nil);
  GetTypeInfoEvent: TMethod = (Code: @GetTypeInfo; Data: nil);

function ImportUnit(Root: TSepiRoot): TSepiUnit;
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(SysInit.HInstance,
    ResourceName, RT_RCDATA);
  try
    Result := TSepiUnit.LoadFromStream(Root, Stream,
      SepiImportsFunLabyUtilsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent));

    if SepiImportsFunLabyUtilsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TComponentID);
  TypeInfoArray[1] := TypeInfo(TDirection);
  TypeInfoArray[2] := TypeInfo(TPlayerAction);
  TypeInfoArray[3] := TypeInfo(TPlayState);
  TypeInfoArray[4] := TypeInfo(EComponentNotFound);
  TypeInfoArray[5] := TypeInfo(EUnsupportedCommand);
  TypeInfoArray[6] := TypeInfo(TImagesMaster);
  TypeInfoArray[7] := TypeInfo(TSquareBitmap);
  TypeInfoArray[8] := TypeInfo(TPainter);
  TypeInfoArray[9] := TypeInfo(TFunLabyComponent);
  TypeInfoArray[10] := TypeInfo(TVisualComponent);
  TypeInfoArray[11] := TypeInfo(TPlugin);
  TypeInfoArray[12] := TypeInfo(TObjectDef);
  TypeInfoArray[13] := TypeInfo(TSquareComponent);
  TypeInfoArray[14] := TypeInfo(TField);
  TypeInfoArray[15] := TypeInfo(TEffect);
  TypeInfoArray[16] := TypeInfo(TTool);
  TypeInfoArray[17] := TypeInfo(TObstacle);
  TypeInfoArray[18] := TypeInfo(TSquare);
  TypeInfoArray[19] := TypeInfo(TMap);
  TypeInfoArray[20] := TypeInfo(TPlayer);
  TypeInfoArray[21] := TypeInfo(TMaster);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTImagesMaster.InitMethodAddresses;
  TSepiImportsTSquareBitmap.InitMethodAddresses;
  TSepiImportsTPainter.InitMethodAddresses;
  TSepiImportsTFunLabyComponent.InitMethodAddresses;
  TSepiImportsTVisualComponent.InitMethodAddresses;
  TSepiImportsTPlugin.InitMethodAddresses;
  TSepiImportsTField.InitMethodAddresses;
  TSepiImportsTSquare.InitMethodAddresses;
  TSepiImportsTMap.InitMethodAddresses;
  TSepiImportsTPlayer.InitMethodAddresses;
  TSepiImportsTMaster.InitMethodAddresses;
  MethodAddresses[85] := @CheckValidLaunch;
  MethodAddresses[86] := @ShowFunLabyAbout;
  MethodAddresses[87] := @PointBehind;
  MethodAddresses[88] := @PointBefore;
  MethodAddresses[89] := @SquareRect;
  MethodAddresses[90] := @EmptyRect;
  MethodAddresses[91] := @EmptySquareRect;
  MethodAddresses[92] := @IsNoQPos;
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;

  SepiRegisterImportedUnit('FunLabyUtils', ImportUnit);
end.

