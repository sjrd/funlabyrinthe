{*
  Importe l'unité FunLabyUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFunLabyUtils;

interface

uses
  Windows, SysUtils, TypInfo, SepiReflectionCore, SepiMembers, Classes, 
  Graphics, ScUtils, FunLabyUtils;

var
  SepiImportsFunLabyUtilsLazyLoad: Boolean = False;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FunLabyUtils';
  ResourceName = 'SepiImportsFunLabyUtils';
  TypeCount = 39;
  MethodCount = 166;
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

  TSepiImportsTDrawViewContext = class(TDrawViewContext)
  private
    function IsSquareVisible_0(const QPos: TQualifiedPos): Boolean;
    function IsSquareVisible_1(Map: TMap; const Position: T3DPoint): Boolean;
    function IsSquareVisible_2(const Position: T3DPoint): Boolean;
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTKeyEventContext = class(TKeyEventContext)
  private
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

  TSepiImportsTFunLabyCollection = class(TFunLabyCollection)
  private
    function GetCount: Integer;
    function GetItems(Index: Integer): TFunLabyPersistent;
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTFunLabyFiler = class(TFunLabyFiler)
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

  TSepiImportsTPlayerMode = class(TPlayerMode)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTLabyrinthPlayerMode = class(TLabyrinthPlayerMode)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTPlayer = class(TPlayer)
  private
    function GetVisible: Boolean;
    procedure MoveTo_1(const Dest: T3DPoint; Execute: Boolean; out Redo: Boolean);
    procedure MoveTo_2(const Dest: T3DPoint);
    procedure MoveTo_3(const Dest: TQualifiedPos; Execute: Boolean; out Redo: Boolean);
    procedure MoveTo_4(const Dest: TQualifiedPos);
    function ShowSelectionMsg_0(const Prompt: string; const Answers: array of string; Default: Integer; ShowOnlySelected: Boolean): Integer;
    function ShowSelectionMsg_1(const Prompt: string; Answers: TStrings; Default: Integer; ShowOnlySelected: Boolean): Integer;
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
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TFunLabyComponent;
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
    function GetTickCount: Cardinal;
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
  MethodAddresses[3] := @TSepiImportsTDrawSquareContext.SetTickCount;
end;

{-------------------------}
{ TDrawViewContext import }
{-------------------------}

function TSepiImportsTDrawViewContext.IsSquareVisible_0(const QPos: TQualifiedPos): Boolean;
begin
  Result := IsSquareVisible(QPos);
end;

function TSepiImportsTDrawViewContext.IsSquareVisible_1(Map: TMap; const Position: T3DPoint): Boolean;
begin
  Result := IsSquareVisible(Map, Position);
end;

function TSepiImportsTDrawViewContext.IsSquareVisible_2(const Position: T3DPoint): Boolean;
begin
  Result := IsSquareVisible(Position);
end;

class procedure TSepiImportsTDrawViewContext.InitMethodAddresses;
begin
  MethodAddresses[4] := @TSepiImportsTDrawViewContext.Create;
  MethodAddresses[5] := @TSepiImportsTDrawViewContext.IsSquareVisible_0;
  MethodAddresses[6] := @TSepiImportsTDrawViewContext.IsSquareVisible_1;
  MethodAddresses[7] := @TSepiImportsTDrawViewContext.IsSquareVisible_2;
end;

{-------------------------}
{ TKeyEventContext import }
{-------------------------}

class procedure TSepiImportsTKeyEventContext.InitMethodAddresses;
begin
  MethodAddresses[8] := @TSepiImportsTKeyEventContext.Create;
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
  MethodAddresses[9] := @TSepiImportsTImagesMaster.Create;
  MethodAddresses[10] := @TSepiImportsTImagesMaster.Add;
  MethodAddresses[11] := @TSepiImportsTImagesMaster.IndexOf;
  MethodAddresses[12] := @TSepiImportsTImagesMaster.Draw_0;
  MethodAddresses[13] := @TSepiImportsTImagesMaster.Draw_1;
  MethodAddresses[14] := @TSepiImportsTImagesMaster.Draw_2;
  MethodAddresses[15] := @TSepiImportsTImagesMaster.Draw_3;
end;

{----------------------}
{ TSquareBitmap import }
{----------------------}

class procedure TSepiImportsTSquareBitmap.InitMethodAddresses;
begin
  MethodAddresses[16] := @TSepiImportsTSquareBitmap.EmptySquare;
  MethodAddresses[17] := @TSepiImportsTSquareBitmap.DrawSquare;
end;

{-----------------}
{ TPainter import }
{-----------------}

class procedure TSepiImportsTPainter.InitMethodAddresses;
begin
  MethodAddresses[18] := @TSepiImportsTPainter.Create;
  MethodAddresses[19] := @TSepiImportsTPainter.Draw;
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
  MethodAddresses[20] := @TSepiImportsTMoveContext.GetSrcSquare;
  MethodAddresses[21] := @TSepiImportsTMoveContext.SetSrcSquare;
  MethodAddresses[22] := @TSepiImportsTMoveContext.GetDestSquare;
  MethodAddresses[23] := @TSepiImportsTMoveContext.SetDestSquare;
  MethodAddresses[24] := @TSepiImportsTMoveContext.GetSquare;
  MethodAddresses[25] := @TSepiImportsTMoveContext.SetSquare;
  MethodAddresses[26] := @TSepiImportsTMoveContext.Create_0;
  MethodAddresses[27] := @TSepiImportsTMoveContext.Create_1;
  MethodAddresses[28] := @TSepiImportsTMoveContext.Cancel;
end;

{---------------------------}
{ TFunLabyCollection import }
{---------------------------}

function TSepiImportsTFunLabyCollection.GetCount: Integer;
begin
  Result := Count;
end;

function TSepiImportsTFunLabyCollection.GetItems(Index: Integer): TFunLabyPersistent;
begin
  Result := Items[Index];
end;

class procedure TSepiImportsTFunLabyCollection.InitMethodAddresses;
begin
  MethodAddresses[29] := @TSepiImportsTFunLabyCollection.GetCount;
  MethodAddresses[30] := @TSepiImportsTFunLabyCollection.GetItems;
  MethodAddresses[31] := @TSepiImportsTFunLabyCollection.AddItem;
  MethodAddresses[32] := @TSepiImportsTFunLabyCollection.InsertItem;
  MethodAddresses[33] := @TSepiImportsTFunLabyCollection.Create;
  MethodAddresses[34] := @TSepiImportsTFunLabyCollection.Clear;
  MethodAddresses[35] := @TSepiImportsTFunLabyCollection.Add;
  MethodAddresses[36] := @TSepiImportsTFunLabyCollection.Insert;
  MethodAddresses[37] := @TSepiImportsTFunLabyCollection.Delete;
  MethodAddresses[38] := @TSepiImportsTFunLabyCollection.Remove;
  MethodAddresses[39] := @TSepiImportsTFunLabyCollection.Exchange;
  MethodAddresses[40] := @TSepiImportsTFunLabyCollection.Move;
  MethodAddresses[41] := @TSepiImportsTFunLabyCollection.IndexOf;
end;

{----------------------}
{ TFunLabyFiler import }
{----------------------}

class procedure TSepiImportsTFunLabyFiler.InitMethodAddresses;
begin
  MethodAddresses[42] := @TSepiImportsTFunLabyFiler.EnumProperties;
  MethodAddresses[43] := @TSepiImportsTFunLabyFiler.HasPlayerData;
  MethodAddresses[44] := @TSepiImportsTFunLabyFiler.GetPlayerData;
  MethodAddresses[45] := @TSepiImportsTFunLabyFiler.Create;
  MethodAddresses[46] := @TSepiImportsTFunLabyFiler.DefinePublishedProperty;
  MethodAddresses[47] := @TSepiImportsTFunLabyFiler.DefineProcProperty;
  MethodAddresses[48] := @TSepiImportsTFunLabyFiler.DefineFieldProcProperty;
  MethodAddresses[49] := @TSepiImportsTFunLabyFiler.DefineFieldProperty;
  MethodAddresses[50] := @TSepiImportsTFunLabyFiler.DefinePersistent;
  MethodAddresses[51] := @TSepiImportsTFunLabyFiler.DefineStrings;
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
  MethodAddresses[52] := @TSepiImportsTFunLabyComponent.GetSafeID;
  MethodAddresses[53] := @TSepiImportsTFunLabyComponent.HasPlayerData;
  MethodAddresses[54] := @TSepiImportsTFunLabyComponent.GetPlayerData;
  MethodAddresses[55] := @TSepiImportsTFunLabyComponent.Create;
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
  MethodAddresses[56] := @TSepiImportsTVisualComponent.Create;
  MethodAddresses[57] := @TSepiImportsTVisualComponent.Draw_0;
  MethodAddresses[58] := @TSepiImportsTVisualComponent.Draw_1;
end;

{----------------}
{ TPlugin import }
{----------------}

class procedure TSepiImportsTPlugin.InitMethodAddresses;
begin
  MethodAddresses[59] := @TSepiImportsTPlugin.Create;
end;

{---------------}
{ TField import }
{---------------}

class procedure TSepiImportsTField.InitMethodAddresses;
begin
  MethodAddresses[60] := @TSepiImportsTField.Create;
end;

{----------------}
{ TSquare import }
{----------------}

class procedure TSepiImportsTSquare.InitMethodAddresses;
begin
  MethodAddresses[61] := @TSepiImportsTSquare.Create;
  MethodAddresses[62] := @TSepiImportsTSquare.Contains;
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
  MethodAddresses[63] := @TSepiImportsTMap.SetMaxViewSize;
  MethodAddresses[64] := @TSepiImportsTMap.GetMap;
  MethodAddresses[65] := @TSepiImportsTMap.SetMap;
  MethodAddresses[66] := @TSepiImportsTMap.GetOutside;
  MethodAddresses[67] := @TSepiImportsTMap.SetOutside;
  MethodAddresses[68] := @TSepiImportsTMap.GetLinearMapCount;
  MethodAddresses[69] := @TSepiImportsTMap.GetLinearMap;
  MethodAddresses[70] := @TSepiImportsTMap.SetLinearMap;
  MethodAddresses[71] := @TSepiImportsTMap.Create;
  MethodAddresses[72] := @TSepiImportsTMap.InMap;
  MethodAddresses[73] := @TSepiImportsTMap.PlayersOn;
end;

{--------------------}
{ TPlayerMode import }
{--------------------}

class procedure TSepiImportsTPlayerMode.InitMethodAddresses;
begin
  MethodAddresses[74] := @TSepiImportsTPlayerMode.GetModeClass;
  MethodAddresses[75] := @TSepiImportsTPlayerMode.GetPlayer;
end;

{-----------------------------}
{ TLabyrinthPlayerMode import }
{-----------------------------}

class procedure TSepiImportsTLabyrinthPlayerMode.InitMethodAddresses;
begin
  MethodAddresses[76] := @TSepiImportsTLabyrinthPlayerMode.InvalidateCache;
  MethodAddresses[77] := @TSepiImportsTLabyrinthPlayerMode.InvalidateCacheIfNeeded;
  MethodAddresses[78] := @TSepiImportsTLabyrinthPlayerMode.UpdateCache;
  MethodAddresses[79] := @TSepiImportsTLabyrinthPlayerMode.DrawPlayers;
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

function TSepiImportsTPlayer.ShowSelectionMsg_0(const Prompt: string; const Answers: array of string; Default: Integer; ShowOnlySelected: Boolean): Integer;
begin
  Result := ShowSelectionMsg(Prompt, Answers, Default, ShowOnlySelected);
end;

function TSepiImportsTPlayer.ShowSelectionMsg_1(const Prompt: string; Answers: TStrings; Default: Integer; ShowOnlySelected: Boolean): Integer;
begin
  Result := ShowSelectionMsg(Prompt, Answers, Default, ShowOnlySelected);
end;

class procedure TSepiImportsTPlayer.InitMethodAddresses;
begin
  MethodAddresses[80] := @TSepiImportsTPlayer.GetVisible;
  MethodAddresses[81] := @TSepiImportsTPlayer.Create;
  MethodAddresses[82] := @TSepiImportsTPlayer.GetPluginIDs;
  MethodAddresses[83] := @TSepiImportsTPlayer.DrawInPlace;
  MethodAddresses[84] := @TSepiImportsTPlayer.ChangeMode;
  MethodAddresses[85] := @TSepiImportsTPlayer.BeginTempMode;
  MethodAddresses[86] := @TSepiImportsTPlayer.EndTempMode;
  MethodAddresses[87] := @TSepiImportsTPlayer.AddPlugin;
  MethodAddresses[88] := @TSepiImportsTPlayer.RemovePlugin;
  MethodAddresses[89] := @TSepiImportsTPlayer.AbleTo;
  MethodAddresses[90] := @TSepiImportsTPlayer.DoAction;
  MethodAddresses[91] := @TSepiImportsTPlayer.Move;
  MethodAddresses[92] := @TSepiImportsTPlayer.MoveTo_1;
  MethodAddresses[93] := @TSepiImportsTPlayer.MoveTo_2;
  MethodAddresses[94] := @TSepiImportsTPlayer.MoveTo_3;
  MethodAddresses[95] := @TSepiImportsTPlayer.MoveTo_4;
  MethodAddresses[96] := @TSepiImportsTPlayer.Temporize;
  MethodAddresses[97] := @TSepiImportsTPlayer.NaturalMoving;
  MethodAddresses[98] := @TSepiImportsTPlayer.ChangePosition;
  MethodAddresses[99] := @TSepiImportsTPlayer.Show;
  MethodAddresses[100] := @TSepiImportsTPlayer.Hide;
  MethodAddresses[101] := @TSepiImportsTPlayer.SendCommand;
  MethodAddresses[102] := @TSepiImportsTPlayer.ShowDialog;
  MethodAddresses[103] := @TSepiImportsTPlayer.ShowDialogRadio;
  MethodAddresses[104] := @TSepiImportsTPlayer.ChooseNumber;
  MethodAddresses[105] := @TSepiImportsTPlayer.ShowMessage;
  MethodAddresses[106] := @TSepiImportsTPlayer.ShowSelectionMsg_0;
  MethodAddresses[107] := @TSepiImportsTPlayer.ShowSelectionMsg_1;
  MethodAddresses[108] := @TSepiImportsTPlayer.ShowSelectNumberMsg;
  MethodAddresses[109] := @TSepiImportsTPlayer.Win;
  MethodAddresses[110] := @TSepiImportsTPlayer.Lose;
  MethodAddresses[111] := @TSepiImportsTPlayer.PressKey;
  MethodAddresses[112] := @TSepiImportsTPlayer.SendMessage;
  MethodAddresses[113] := @TSepiImportsTPlayer.WaitForKey;
  MethodAddresses[114] := @TSepiImportsTPlayer.WaitForSpecificKey;
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

function TSepiImportsTMaster.GetComponentCount: Integer;
begin
  Result := ComponentCount;
end;

function TSepiImportsTMaster.GetComponents(Index: Integer): TFunLabyComponent;
begin
  Result := Components[Index];
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

function TSepiImportsTMaster.GetTickCount: Cardinal;
begin
  Result := TickCount;
end;

class procedure TSepiImportsTMaster.InitMethodAddresses;
begin
  MethodAddresses[115] := @TSepiImportsTMaster.GetComponent;
  MethodAddresses[116] := @TSepiImportsTMaster.GetSquareComponent;
  MethodAddresses[117] := @TSepiImportsTMaster.GetPlugin;
  MethodAddresses[118] := @TSepiImportsTMaster.GetObjectDef;
  MethodAddresses[119] := @TSepiImportsTMaster.GetField;
  MethodAddresses[120] := @TSepiImportsTMaster.GetEffect;
  MethodAddresses[121] := @TSepiImportsTMaster.GetTool;
  MethodAddresses[122] := @TSepiImportsTMaster.GetObstacle;
  MethodAddresses[123] := @TSepiImportsTMaster.GetSquare;
  MethodAddresses[124] := @TSepiImportsTMaster.GetMap;
  MethodAddresses[125] := @TSepiImportsTMaster.GetPlayer;
  MethodAddresses[126] := @TSepiImportsTMaster.GetComponentCount;
  MethodAddresses[127] := @TSepiImportsTMaster.GetComponents;
  MethodAddresses[128] := @TSepiImportsTMaster.GetPluginCount;
  MethodAddresses[129] := @TSepiImportsTMaster.GetPlugins;
  MethodAddresses[130] := @TSepiImportsTMaster.GetObjectDefCount;
  MethodAddresses[131] := @TSepiImportsTMaster.GetObjectDefs;
  MethodAddresses[132] := @TSepiImportsTMaster.GetFieldCount;
  MethodAddresses[133] := @TSepiImportsTMaster.GetFields;
  MethodAddresses[134] := @TSepiImportsTMaster.GetEffectCount;
  MethodAddresses[135] := @TSepiImportsTMaster.GetEffects;
  MethodAddresses[136] := @TSepiImportsTMaster.GetToolCount;
  MethodAddresses[137] := @TSepiImportsTMaster.GetTools;
  MethodAddresses[138] := @TSepiImportsTMaster.GetObstacleCount;
  MethodAddresses[139] := @TSepiImportsTMaster.GetObstacles;
  MethodAddresses[140] := @TSepiImportsTMaster.GetSquareCount;
  MethodAddresses[141] := @TSepiImportsTMaster.GetSquares;
  MethodAddresses[142] := @TSepiImportsTMaster.GetMapCount;
  MethodAddresses[143] := @TSepiImportsTMaster.GetMaps;
  MethodAddresses[144] := @TSepiImportsTMaster.GetPlayerCount;
  MethodAddresses[145] := @TSepiImportsTMaster.GetPlayers;
  MethodAddresses[146] := @TSepiImportsTMaster.SetTemporization;
  MethodAddresses[147] := @TSepiImportsTMaster.GetTickCount;
  MethodAddresses[148] := @TSepiImportsTMaster.Create;
  MethodAddresses[149] := @TSepiImportsTMaster.Temporize;
  MethodAddresses[150] := @TSepiImportsTMaster.SquareByComps;
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
  TypeInfoArray[8] := TypeInfo(TPlayerShowMsgMessage);
  TypeInfoArray[9] := TypeInfo(TDrawSquareContext);
  TypeInfoArray[10] := TypeInfo(TDrawViewContext);
  TypeInfoArray[11] := TypeInfo(TKeyEventContext);
  TypeInfoArray[12] := TypeInfo(TImagesMaster);
  TypeInfoArray[13] := TypeInfo(TSquareBitmap);
  TypeInfoArray[14] := TypeInfo(TPainter);
  TypeInfoArray[15] := TypeInfo(TMoveContext);
  TypeInfoArray[16] := TypeInfo(TFunLabyPersistent);
  TypeInfoArray[17] := TypeInfo(TFunLabyCollection);
  TypeInfoArray[18] := TypeInfo(TStringsObjectKind);
  TypeInfoArray[19] := TypeInfo(TFunLabyFiler);
  TypeInfoArray[20] := TypeInfo(TPlayerData);
  TypeInfoArray[21] := TypeInfo(TFunLabyComponent);
  TypeInfoArray[22] := TypeInfo(TVisualComponent);
  TypeInfoArray[23] := TypeInfo(TPlugin);
  TypeInfoArray[24] := TypeInfo(TPluginDynArray);
  TypeInfoArray[25] := TypeInfo(TObjectDefPlayerData);
  TypeInfoArray[26] := TypeInfo(TObjectDef);
  TypeInfoArray[27] := TypeInfo(TSquareComponent);
  TypeInfoArray[28] := TypeInfo(TField);
  TypeInfoArray[29] := TypeInfo(TEffect);
  TypeInfoArray[30] := TypeInfo(TTool);
  TypeInfoArray[31] := TypeInfo(TObstacle);
  TypeInfoArray[32] := TypeInfo(TSquare);
  TypeInfoArray[33] := TypeInfo(TMap);
  TypeInfoArray[34] := TypeInfo(IPlayerMode);
  TypeInfoArray[35] := TypeInfo(TPlayerMode);
  TypeInfoArray[36] := TypeInfo(TLabyrinthPlayerMode);
  TypeInfoArray[37] := TypeInfo(TPlayer);
  TypeInfoArray[38] := TypeInfo(TMaster);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDrawSquareContext.InitMethodAddresses;
  TSepiImportsTDrawViewContext.InitMethodAddresses;
  TSepiImportsTKeyEventContext.InitMethodAddresses;
  TSepiImportsTImagesMaster.InitMethodAddresses;
  TSepiImportsTSquareBitmap.InitMethodAddresses;
  TSepiImportsTPainter.InitMethodAddresses;
  TSepiImportsTMoveContext.InitMethodAddresses;
  TSepiImportsTFunLabyCollection.InitMethodAddresses;
  TSepiImportsTFunLabyFiler.InitMethodAddresses;
  TSepiImportsTFunLabyComponent.InitMethodAddresses;
  TSepiImportsTVisualComponent.InitMethodAddresses;
  TSepiImportsTPlugin.InitMethodAddresses;
  TSepiImportsTField.InitMethodAddresses;
  TSepiImportsTSquare.InitMethodAddresses;
  TSepiImportsTMap.InitMethodAddresses;
  TSepiImportsTPlayerMode.InitMethodAddresses;
  TSepiImportsTLabyrinthPlayerMode.InitMethodAddresses;
  TSepiImportsTPlayer.InitMethodAddresses;
  TSepiImportsTMaster.InitMethodAddresses;
  MethodAddresses[151] := @CheckValidLaunch;
  MethodAddresses[152] := @ShowFunLabyAbout;
  MethodAddresses[153] := @PointBehind;
  MethodAddresses[154] := @PointBefore;
  MethodAddresses[155] := @SquareRect;
  MethodAddresses[156] := @EmptyRect;
  MethodAddresses[157] := @EmptySquareRect;
  MethodAddresses[158] := @SameRect;
  MethodAddresses[159] := @IsNoQPos;
  MethodAddresses[160] := @FunLabyRegisterClass;
  MethodAddresses[161] := @FunLabyUnregisterClass;
  MethodAddresses[162] := @FunLabyRegisterClasses;
  MethodAddresses[163] := @FunLabyUnregisterClasses;
  MethodAddresses[164] := @FunLabyGetClass;
  MethodAddresses[165] := @FunLabyFindClass;
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

