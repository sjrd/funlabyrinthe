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

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FunLabyUtils';
  ResourceName = 'SepiImportsFunLabyUtils';
  TypeCount = 26;
  MethodCount = 109;
  VariableCount = 12;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTDrawSquareContext = class(TDrawSquareContext)
  private
    constructor Create_0(ACanvas: TCanvas);
    constructor Create_1(ACanvas: TCanvas; X: Integer; Y: Integer);
    constructor Create_2(ACanvas: TCanvas; X: Integer; Y: Integer; const AQPos: TQualifiedPos);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTImagesMaster = class(TImagesMaster)
  private
    procedure Draw_0(Index: Integer; Context: TDrawSquareContext);
    procedure Draw_1(const ImgName: string; Context: TDrawSquareContext);
    procedure Draw_2(Index: Integer; Canvas: TCanvas; X: Integer; Y: Integer);
    procedure Draw_3(const ImgName: string; Canvas: TCanvas; X: Integer; Y: Integer);
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

  TSepiImportsTMoveContext = class(TMoveContext)
  private
    function GetSrcSquare: TSquare;
    procedure SetSrcSquare(Value: TSquare);
    function GetDestSquare: TSquare;
    procedure SetDestSquare(Value: TSquare);
    function GetSquare: TSquare;
    procedure SetSquare(Value: TSquare);
    constructor Create_0(APlayer: TPlayer; const ADest: TQualifiedPos; AKeyPressed: Boolean);
    constructor Create_1(APlayer: TPlayer; const ADest: T3DPoint; AKeyPressed: Boolean);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTFunLabyComponent = class(TFunLabyComponent)
  private
    function GetSafeID: TComponentID;
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTVisualComponent = class(TVisualComponent)
  private
    procedure Draw_0(Context: TDrawSquareContext);
    procedure Draw_1(const QPos: TQualifiedPos; Canvas: TCanvas; X: Integer; Y: Integer);
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
    procedure MoveTo_1(const Dest: T3DPoint; Execute: Boolean; out Redo: Boolean);
    procedure MoveTo_2(const Dest: T3DPoint);
    procedure MoveTo_3(const Dest: TQualifiedPos; Execute: Boolean; out Redo: Boolean);
    procedure MoveTo_4(const Dest: TQualifiedPos);
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

{---------------------------}
{ TDrawSquareContext import }
{---------------------------}

constructor TSepiImportsTDrawSquareContext.Create_0(ACanvas: TCanvas);
begin
  Create(ACanvas);
end;

constructor TSepiImportsTDrawSquareContext.Create_1(ACanvas: TCanvas; X: Integer; Y: Integer);
begin
  Create(ACanvas, X, Y);
end;

constructor TSepiImportsTDrawSquareContext.Create_2(ACanvas: TCanvas; X: Integer; Y: Integer; const AQPos: TQualifiedPos);
begin
  Create(ACanvas, X, Y, AQPos);
end;

class procedure TSepiImportsTDrawSquareContext.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTDrawSquareContext.Create_0;
  MethodAddresses[1] := @TSepiImportsTDrawSquareContext.Create_1;
  MethodAddresses[2] := @TSepiImportsTDrawSquareContext.Create_2;
end;

{----------------------}
{ TImagesMaster import }
{----------------------}

procedure TSepiImportsTImagesMaster.Draw_0(Index: Integer; Context: TDrawSquareContext);
begin
  Draw(Index, Context);
end;

procedure TSepiImportsTImagesMaster.Draw_1(const ImgName: string; Context: TDrawSquareContext);
begin
  Draw(ImgName, Context);
end;

procedure TSepiImportsTImagesMaster.Draw_2(Index: Integer; Canvas: TCanvas; X: Integer; Y: Integer);
begin
  Draw(Index, Canvas, X, Y);
end;

procedure TSepiImportsTImagesMaster.Draw_3(const ImgName: string; Canvas: TCanvas; X: Integer; Y: Integer);
begin
  Draw(ImgName, Canvas, X, Y);
end;

class procedure TSepiImportsTImagesMaster.InitMethodAddresses;
begin
  MethodAddresses[3] := @TSepiImportsTImagesMaster.Create;
  MethodAddresses[4] := @TSepiImportsTImagesMaster.Add;
  MethodAddresses[5] := @TSepiImportsTImagesMaster.IndexOf;
  MethodAddresses[6] := @TSepiImportsTImagesMaster.Draw_0;
  MethodAddresses[7] := @TSepiImportsTImagesMaster.Draw_1;
  MethodAddresses[8] := @TSepiImportsTImagesMaster.Draw_2;
  MethodAddresses[9] := @TSepiImportsTImagesMaster.Draw_3;
end;

{----------------------}
{ TSquareBitmap import }
{----------------------}

class procedure TSepiImportsTSquareBitmap.InitMethodAddresses;
begin
  MethodAddresses[10] := @TSepiImportsTSquareBitmap.EmptySquare;
  MethodAddresses[11] := @TSepiImportsTSquareBitmap.DrawSquare;
end;

{-----------------}
{ TPainter import }
{-----------------}

class procedure TSepiImportsTPainter.InitMethodAddresses;
begin
  MethodAddresses[12] := @TSepiImportsTPainter.Create;
  MethodAddresses[13] := @TSepiImportsTPainter.Draw;
end;

{---------------------}
{ TMoveContext import }
{---------------------}

function TSepiImportsTMoveContext.GetSrcSquare: TSquare;
begin
  Result := SrcSquare;
end;

procedure TSepiImportsTMoveContext.SetSrcSquare(Value: TSquare);
begin
  SrcSquare := Value;
end;

function TSepiImportsTMoveContext.GetDestSquare: TSquare;
begin
  Result := DestSquare;
end;

procedure TSepiImportsTMoveContext.SetDestSquare(Value: TSquare);
begin
  DestSquare := Value;
end;

function TSepiImportsTMoveContext.GetSquare: TSquare;
begin
  Result := Square;
end;

procedure TSepiImportsTMoveContext.SetSquare(Value: TSquare);
begin
  Square := Value;
end;

constructor TSepiImportsTMoveContext.Create_0(APlayer: TPlayer; const ADest: TQualifiedPos; AKeyPressed: Boolean);
begin
  Create(APlayer, ADest, AKeyPressed);
end;

constructor TSepiImportsTMoveContext.Create_1(APlayer: TPlayer; const ADest: T3DPoint; AKeyPressed: Boolean);
begin
  Create(APlayer, ADest, AKeyPressed);
end;

class procedure TSepiImportsTMoveContext.InitMethodAddresses;
begin
  MethodAddresses[14] := @TSepiImportsTMoveContext.GetSrcSquare;
  MethodAddresses[15] := @TSepiImportsTMoveContext.SetSrcSquare;
  MethodAddresses[16] := @TSepiImportsTMoveContext.GetDestSquare;
  MethodAddresses[17] := @TSepiImportsTMoveContext.SetDestSquare;
  MethodAddresses[18] := @TSepiImportsTMoveContext.GetSquare;
  MethodAddresses[19] := @TSepiImportsTMoveContext.SetSquare;
  MethodAddresses[20] := @TSepiImportsTMoveContext.Create_0;
  MethodAddresses[21] := @TSepiImportsTMoveContext.Create_1;
  MethodAddresses[22] := @TSepiImportsTMoveContext.Cancel;
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
  MethodAddresses[23] := @TSepiImportsTFunLabyComponent.GetSafeID;
  MethodAddresses[24] := @TSepiImportsTFunLabyComponent.Create;
end;

{-------------------------}
{ TVisualComponent import }
{-------------------------}

procedure TSepiImportsTVisualComponent.Draw_0(Context: TDrawSquareContext);
begin
  Draw(Context);
end;

procedure TSepiImportsTVisualComponent.Draw_1(const QPos: TQualifiedPos; Canvas: TCanvas; X: Integer; Y: Integer);
begin
  Draw(QPos, Canvas, X, Y);
end;

class procedure TSepiImportsTVisualComponent.InitMethodAddresses;
begin
  MethodAddresses[25] := @TSepiImportsTVisualComponent.Create;
  MethodAddresses[26] := @TSepiImportsTVisualComponent.Draw_0;
  MethodAddresses[27] := @TSepiImportsTVisualComponent.Draw_1;
end;

{----------------}
{ TPlugin import }
{----------------}

class procedure TSepiImportsTPlugin.InitMethodAddresses;
begin
  MethodAddresses[28] := @TSepiImportsTPlugin.Create;
end;

{---------------}
{ TField import }
{---------------}

class procedure TSepiImportsTField.InitMethodAddresses;
begin
  MethodAddresses[29] := @TSepiImportsTField.Create;
end;

{----------------}
{ TSquare import }
{----------------}

class procedure TSepiImportsTSquare.InitMethodAddresses;
begin
  MethodAddresses[30] := @TSepiImportsTSquare.Create;
  MethodAddresses[31] := @TSepiImportsTSquare.Contains;
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
  MethodAddresses[32] := @TSepiImportsTMap.SetMaxViewSize;
  MethodAddresses[33] := @TSepiImportsTMap.GetMap;
  MethodAddresses[34] := @TSepiImportsTMap.SetMap;
  MethodAddresses[35] := @TSepiImportsTMap.GetOutside;
  MethodAddresses[36] := @TSepiImportsTMap.SetOutside;
  MethodAddresses[37] := @TSepiImportsTMap.GetLinearMapCount;
  MethodAddresses[38] := @TSepiImportsTMap.GetLinearMap;
  MethodAddresses[39] := @TSepiImportsTMap.SetLinearMap;
  MethodAddresses[40] := @TSepiImportsTMap.Create;
  MethodAddresses[41] := @TSepiImportsTMap.InMap;
  MethodAddresses[42] := @TSepiImportsTMap.PlayersOn;
end;

{----------------}
{ TPlayer import }
{----------------}

function TSepiImportsTPlayer.GetVisible: Boolean;
begin
  Result := Visible;
end;

procedure TSepiImportsTPlayer.MoveTo_1(const Dest: T3DPoint; Execute: Boolean; out Redo: Boolean);
begin
  MoveTo(Dest, Execute, Redo);
end;

procedure TSepiImportsTPlayer.MoveTo_2(const Dest: T3DPoint);
begin
  MoveTo(Dest);
end;

procedure TSepiImportsTPlayer.MoveTo_3(const Dest: TQualifiedPos; Execute: Boolean; out Redo: Boolean);
begin
  MoveTo(Dest, Execute, Redo);
end;

procedure TSepiImportsTPlayer.MoveTo_4(const Dest: TQualifiedPos);
begin
  MoveTo(Dest);
end;

class procedure TSepiImportsTPlayer.InitMethodAddresses;
begin
  MethodAddresses[43] := @TSepiImportsTPlayer.GetVisible;
  MethodAddresses[44] := @TSepiImportsTPlayer.Create;
  MethodAddresses[45] := @TSepiImportsTPlayer.GetPluginIDs;
  MethodAddresses[46] := @TSepiImportsTPlayer.DrawInPlace;
  MethodAddresses[47] := @TSepiImportsTPlayer.AddPlugin;
  MethodAddresses[48] := @TSepiImportsTPlayer.RemovePlugin;
  MethodAddresses[49] := @TSepiImportsTPlayer.AbleTo;
  MethodAddresses[50] := @TSepiImportsTPlayer.DoAction;
  MethodAddresses[51] := @TSepiImportsTPlayer.Move;
  MethodAddresses[52] := @TSepiImportsTPlayer.MoveTo_1;
  MethodAddresses[53] := @TSepiImportsTPlayer.MoveTo_2;
  MethodAddresses[54] := @TSepiImportsTPlayer.MoveTo_3;
  MethodAddresses[55] := @TSepiImportsTPlayer.MoveTo_4;
  MethodAddresses[56] := @TSepiImportsTPlayer.Temporize;
  MethodAddresses[57] := @TSepiImportsTPlayer.NaturalMoving;
  MethodAddresses[58] := @TSepiImportsTPlayer.ChangePosition;
  MethodAddresses[59] := @TSepiImportsTPlayer.Show;
  MethodAddresses[60] := @TSepiImportsTPlayer.Hide;
  MethodAddresses[61] := @TSepiImportsTPlayer.SendCommand;
  MethodAddresses[62] := @TSepiImportsTPlayer.ShowDialog;
  MethodAddresses[63] := @TSepiImportsTPlayer.ShowDialogRadio;
  MethodAddresses[64] := @TSepiImportsTPlayer.ChooseNumber;
  MethodAddresses[65] := @TSepiImportsTPlayer.Win;
  MethodAddresses[66] := @TSepiImportsTPlayer.Lose;
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
  MethodAddresses[67] := @TSepiImportsTMaster.GetComponent;
  MethodAddresses[68] := @TSepiImportsTMaster.GetSquareComponent;
  MethodAddresses[69] := @TSepiImportsTMaster.GetPlugin;
  MethodAddresses[70] := @TSepiImportsTMaster.GetObjectDef;
  MethodAddresses[71] := @TSepiImportsTMaster.GetField;
  MethodAddresses[72] := @TSepiImportsTMaster.GetEffect;
  MethodAddresses[73] := @TSepiImportsTMaster.GetTool;
  MethodAddresses[74] := @TSepiImportsTMaster.GetObstacle;
  MethodAddresses[75] := @TSepiImportsTMaster.GetSquare;
  MethodAddresses[76] := @TSepiImportsTMaster.GetMap;
  MethodAddresses[77] := @TSepiImportsTMaster.GetPlayer;
  MethodAddresses[78] := @TSepiImportsTMaster.GetPluginCount;
  MethodAddresses[79] := @TSepiImportsTMaster.GetPlugins;
  MethodAddresses[80] := @TSepiImportsTMaster.GetObjectDefCount;
  MethodAddresses[81] := @TSepiImportsTMaster.GetObjectDefs;
  MethodAddresses[82] := @TSepiImportsTMaster.GetFieldCount;
  MethodAddresses[83] := @TSepiImportsTMaster.GetFields;
  MethodAddresses[84] := @TSepiImportsTMaster.GetEffectCount;
  MethodAddresses[85] := @TSepiImportsTMaster.GetEffects;
  MethodAddresses[86] := @TSepiImportsTMaster.GetToolCount;
  MethodAddresses[87] := @TSepiImportsTMaster.GetTools;
  MethodAddresses[88] := @TSepiImportsTMaster.GetObstacleCount;
  MethodAddresses[89] := @TSepiImportsTMaster.GetObstacles;
  MethodAddresses[90] := @TSepiImportsTMaster.GetSquareCount;
  MethodAddresses[91] := @TSepiImportsTMaster.GetSquares;
  MethodAddresses[92] := @TSepiImportsTMaster.GetMapCount;
  MethodAddresses[93] := @TSepiImportsTMaster.GetMaps;
  MethodAddresses[94] := @TSepiImportsTMaster.GetPlayerCount;
  MethodAddresses[95] := @TSepiImportsTMaster.GetPlayers;
  MethodAddresses[96] := @TSepiImportsTMaster.SetTemporization;
  MethodAddresses[97] := @TSepiImportsTMaster.Create;
  MethodAddresses[98] := @TSepiImportsTMaster.Temporize;
  MethodAddresses[99] := @TSepiImportsTMaster.UpdateTickCount;
  MethodAddresses[100] := @TSepiImportsTMaster.SquareByComps;
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

procedure GetVarAddress(Self, Sender: TObject; var VarAddress: Pointer);
var
  Index: Integer;
begin
  Index := (Sender as TSepiVariable).Tag;
  if Index >= 0 then
    VarAddress := VarAddresses[Index];
end;

const
  GetMethodCodeEvent: TMethod = (Code: @GetMethodCode; Data: nil);
  GetTypeInfoEvent: TMethod = (Code: @GetTypeInfo; Data: nil);
  GetVarAddressEvent: TMethod = (Code: @GetVarAddress; Data: nil);

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
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFunLabyUtilsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TComponentID);
  TypeInfoArray[1] := TypeInfo(TDirection);
  TypeInfoArray[2] := TypeInfo(TPlayerAction);
  TypeInfoArray[3] := TypeInfo(TPlayState);
  TypeInfoArray[4] := TypeInfo(EFunLabyException);
  TypeInfoArray[5] := TypeInfo(EComponentNotFound);
  TypeInfoArray[6] := TypeInfo(EUnsupportedCommand);
  TypeInfoArray[7] := TypeInfo(EBadSquareDefException);
  TypeInfoArray[8] := TypeInfo(TDrawSquareContext);
  TypeInfoArray[9] := TypeInfo(TImagesMaster);
  TypeInfoArray[10] := TypeInfo(TSquareBitmap);
  TypeInfoArray[11] := TypeInfo(TPainter);
  TypeInfoArray[12] := TypeInfo(TMoveContext);
  TypeInfoArray[13] := TypeInfo(TFunLabyComponent);
  TypeInfoArray[14] := TypeInfo(TVisualComponent);
  TypeInfoArray[15] := TypeInfo(TPlugin);
  TypeInfoArray[16] := TypeInfo(TObjectDef);
  TypeInfoArray[17] := TypeInfo(TSquareComponent);
  TypeInfoArray[18] := TypeInfo(TField);
  TypeInfoArray[19] := TypeInfo(TEffect);
  TypeInfoArray[20] := TypeInfo(TTool);
  TypeInfoArray[21] := TypeInfo(TObstacle);
  TypeInfoArray[22] := TypeInfo(TSquare);
  TypeInfoArray[23] := TypeInfo(TMap);
  TypeInfoArray[24] := TypeInfo(TPlayer);
  TypeInfoArray[25] := TypeInfo(TMaster);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDrawSquareContext.InitMethodAddresses;
  TSepiImportsTImagesMaster.InitMethodAddresses;
  TSepiImportsTSquareBitmap.InitMethodAddresses;
  TSepiImportsTPainter.InitMethodAddresses;
  TSepiImportsTMoveContext.InitMethodAddresses;
  TSepiImportsTFunLabyComponent.InitMethodAddresses;
  TSepiImportsTVisualComponent.InitMethodAddresses;
  TSepiImportsTPlugin.InitMethodAddresses;
  TSepiImportsTField.InitMethodAddresses;
  TSepiImportsTSquare.InitMethodAddresses;
  TSepiImportsTMap.InitMethodAddresses;
  TSepiImportsTPlayer.InitMethodAddresses;
  TSepiImportsTMaster.InitMethodAddresses;
  MethodAddresses[101] := @CheckValidLaunch;
  MethodAddresses[102] := @ShowFunLabyAbout;
  MethodAddresses[103] := @PointBehind;
  MethodAddresses[104] := @PointBefore;
  MethodAddresses[105] := @SquareRect;
  MethodAddresses[106] := @EmptyRect;
  MethodAddresses[107] := @EmptySquareRect;
  MethodAddresses[108] := @IsNoQPos;
end;

procedure InitVarAddresses;
begin
  VarAddresses[0] := @NoQPos;
  VarAddresses[1] := @BaseSquareRect;
  VarAddresses[2] := @NegDir;
  VarAddresses[3] := @fFunLabyAppData;
  VarAddresses[4] := @fSquaresDir;
  VarAddresses[5] := @fSoundsDir;
  VarAddresses[6] := @fUnitsDir;
  VarAddresses[7] := @fMapsDir;
  VarAddresses[8] := @fLabyrinthsDir;
  VarAddresses[9] := @fSaveguardsDir;
  VarAddresses[10] := @fEditPluginDir;
  VarAddresses[11] := @fSquareFileName;
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FunLabyUtils', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FunLabyUtils');
end.

