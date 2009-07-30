{*
  Importe l'unité FunLabyUtils dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFunLabyUtils;

interface

uses
  Windows, SysUtils, TypInfo, SepiReflectionCore, SepiMembers, Classes, 
  ScUtils, GR32, FunLabyUtils;

var
  SepiImportsFunLabyUtilsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FunLabyUtils';
  ResourceName = 'SepiImportsFunLabyUtils';
  TypeCount = 44;
  MethodCount = 176;
  VariableCount = 10;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTDrawSquareContext = class(TDrawSquareContext)
  private
    constructor Create_0(ABitmap: TBitmap32);
    constructor Create_1(ABitmap: TBitmap32; X: Integer; Y: Integer);
    constructor Create_2(ABitmap: TBitmap32; X: Integer; Y: Integer; const AQPos: TQualifiedPos);
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
    procedure Draw_2(Index: Integer; Bitmap: TBitmap32; X: Integer; Y: Integer);
    procedure Draw_3(const ImgName: string; Bitmap: TBitmap32; X: Integer; Y: Integer);
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

  TSepiImportsTInterfacedFunLabyPersistent = class(TInterfacedFunLabyPersistent)
  private
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

  TSepiImportsTFunLabyReader = class(TFunLabyReader)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTFunLabyWriter = class(TFunLabyWriter)
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
    procedure Draw_1(const QPos: TQualifiedPos; Bitmap: TBitmap32; X: Integer; Y: Integer);
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
    constructor Create_0(AMaster: TMaster; const AID: TComponentID);
    constructor Create_1(AMaster: TMaster; const AID: TComponentID; ADimensions: T3DPoint; AZoneWidth: Integer; AZoneHeight: Integer);
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

constructor TSepiImportsTDrawSquareContext.Create_0(ABitmap: TBitmap32);
begin
  Create(ABitmap);
end;

constructor TSepiImportsTDrawSquareContext.Create_1(ABitmap: TBitmap32; X: Integer; Y: Integer);
begin
  Create(ABitmap, X, Y);
end;

constructor TSepiImportsTDrawSquareContext.Create_2(ABitmap: TBitmap32; X: Integer; Y: Integer; const AQPos: TQualifiedPos);
begin
  Create(ABitmap, X, Y, AQPos);
end;

class procedure TSepiImportsTDrawSquareContext.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTDrawSquareContext.Create_0;
  MethodAddresses[1] := @TSepiImportsTDrawSquareContext.Create_1;
  MethodAddresses[2] := @TSepiImportsTDrawSquareContext.Create_2;
  MethodAddresses[3] := @TSepiImportsTDrawSquareContext.SetTickCount;
  MethodAddresses[4] := @TSepiImportsTDrawSquareContext.DrawSquareBitmap;
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
  MethodAddresses[5] := @TSepiImportsTDrawViewContext.Create;
  MethodAddresses[6] := @TSepiImportsTDrawViewContext.IsSquareVisible_0;
  MethodAddresses[7] := @TSepiImportsTDrawViewContext.IsSquareVisible_1;
  MethodAddresses[8] := @TSepiImportsTDrawViewContext.IsSquareVisible_2;
end;

{-------------------------}
{ TKeyEventContext import }
{-------------------------}

class procedure TSepiImportsTKeyEventContext.InitMethodAddresses;
begin
  MethodAddresses[9] := @TSepiImportsTKeyEventContext.Create;
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

procedure TSepiImportsTImagesMaster.Draw_2(Index: Integer; Bitmap: TBitmap32; X: Integer; Y: Integer);
begin
  Draw(Index, Bitmap, X, Y);
end;

procedure TSepiImportsTImagesMaster.Draw_3(const ImgName: string; Bitmap: TBitmap32; X: Integer; Y: Integer);
begin
  Draw(ImgName, Bitmap, X, Y);
end;

class procedure TSepiImportsTImagesMaster.InitMethodAddresses;
begin
  MethodAddresses[10] := @TSepiImportsTImagesMaster.Create;
  MethodAddresses[11] := @TSepiImportsTImagesMaster.IndexOf;
  MethodAddresses[12] := @TSepiImportsTImagesMaster.Draw_0;
  MethodAddresses[13] := @TSepiImportsTImagesMaster.Draw_1;
  MethodAddresses[14] := @TSepiImportsTImagesMaster.Draw_2;
  MethodAddresses[15] := @TSepiImportsTImagesMaster.Draw_3;
  MethodAddresses[16] := @TSepiImportsTImagesMaster.GetInternalBitmap;
end;

{-----------------}
{ TPainter import }
{-----------------}

class procedure TSepiImportsTPainter.InitMethodAddresses;
begin
  MethodAddresses[17] := @TSepiImportsTPainter.Create;
  MethodAddresses[18] := @TSepiImportsTPainter.Draw;
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
  MethodAddresses[19] := @TSepiImportsTMoveContext.GetSrcSquare;
  MethodAddresses[20] := @TSepiImportsTMoveContext.SetSrcSquare;
  MethodAddresses[21] := @TSepiImportsTMoveContext.GetDestSquare;
  MethodAddresses[22] := @TSepiImportsTMoveContext.SetDestSquare;
  MethodAddresses[23] := @TSepiImportsTMoveContext.GetSquare;
  MethodAddresses[24] := @TSepiImportsTMoveContext.SetSquare;
  MethodAddresses[25] := @TSepiImportsTMoveContext.Create_0;
  MethodAddresses[26] := @TSepiImportsTMoveContext.Create_1;
  MethodAddresses[27] := @TSepiImportsTMoveContext.Cancel;
end;

{-------------------------------------}
{ TInterfacedFunLabyPersistent import }
{-------------------------------------}

class procedure TSepiImportsTInterfacedFunLabyPersistent.InitMethodAddresses;
begin
  MethodAddresses[28] := @TSepiImportsTInterfacedFunLabyPersistent.QueryInterface;
  MethodAddresses[29] := @TSepiImportsTInterfacedFunLabyPersistent._AddRef;
  MethodAddresses[30] := @TSepiImportsTInterfacedFunLabyPersistent._Release;
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
  MethodAddresses[31] := @TSepiImportsTFunLabyCollection.GetCount;
  MethodAddresses[32] := @TSepiImportsTFunLabyCollection.GetItems;
  MethodAddresses[33] := @TSepiImportsTFunLabyCollection.AddItem;
  MethodAddresses[34] := @TSepiImportsTFunLabyCollection.InsertItem;
  MethodAddresses[35] := @TSepiImportsTFunLabyCollection.Create;
  MethodAddresses[36] := @TSepiImportsTFunLabyCollection.Clear;
  MethodAddresses[37] := @TSepiImportsTFunLabyCollection.Add;
  MethodAddresses[38] := @TSepiImportsTFunLabyCollection.Insert;
  MethodAddresses[39] := @TSepiImportsTFunLabyCollection.Delete;
  MethodAddresses[40] := @TSepiImportsTFunLabyCollection.Remove;
  MethodAddresses[41] := @TSepiImportsTFunLabyCollection.Exchange;
  MethodAddresses[42] := @TSepiImportsTFunLabyCollection.Move;
  MethodAddresses[43] := @TSepiImportsTFunLabyCollection.IndexOf;
end;

{----------------------}
{ TFunLabyFiler import }
{----------------------}

class procedure TSepiImportsTFunLabyFiler.InitMethodAddresses;
begin
  MethodAddresses[44] := @TSepiImportsTFunLabyFiler.EnumProperties;
  MethodAddresses[45] := @TSepiImportsTFunLabyFiler.InstanceBeginState;
  MethodAddresses[46] := @TSepiImportsTFunLabyFiler.InstanceEndState;
  MethodAddresses[47] := @TSepiImportsTFunLabyFiler.HasPlayerData;
  MethodAddresses[48] := @TSepiImportsTFunLabyFiler.GetPlayerData;
  MethodAddresses[49] := @TSepiImportsTFunLabyFiler.Create;
  MethodAddresses[50] := @TSepiImportsTFunLabyFiler.DefinePublishedProperty;
  MethodAddresses[51] := @TSepiImportsTFunLabyFiler.DefineProcProperty;
  MethodAddresses[52] := @TSepiImportsTFunLabyFiler.DefineFieldProcProperty;
  MethodAddresses[53] := @TSepiImportsTFunLabyFiler.DefineFieldProperty;
  MethodAddresses[54] := @TSepiImportsTFunLabyFiler.DefinePersistent;
  MethodAddresses[55] := @TSepiImportsTFunLabyFiler.DefineStrings;
  MethodAddresses[56] := @TSepiImportsTFunLabyFiler.DefineBinaryProperty;
end;

{-----------------------}
{ TFunLabyReader import }
{-----------------------}

class procedure TSepiImportsTFunLabyReader.InitMethodAddresses;
begin
  MethodAddresses[57] := @TSepiImportsTFunLabyReader.Create;
end;

{-----------------------}
{ TFunLabyWriter import }
{-----------------------}

class procedure TSepiImportsTFunLabyWriter.InitMethodAddresses;
begin
  MethodAddresses[58] := @TSepiImportsTFunLabyWriter.Create;
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
  MethodAddresses[59] := @TSepiImportsTFunLabyComponent.GetSafeID;
  MethodAddresses[60] := @TSepiImportsTFunLabyComponent.HasPlayerData;
  MethodAddresses[61] := @TSepiImportsTFunLabyComponent.GetPlayerData;
  MethodAddresses[62] := @TSepiImportsTFunLabyComponent.Create;
end;

{-------------------------}
{ TVisualComponent import }
{-------------------------}

procedure TSepiImportsTVisualComponent.Draw_0(Context: TDrawSquareContext);
begin
  Draw(Context);
end;

procedure TSepiImportsTVisualComponent.Draw_1(const QPos: TQualifiedPos; Bitmap: TBitmap32; X: Integer; Y: Integer);
begin
  Draw(QPos, Bitmap, X, Y);
end;

class procedure TSepiImportsTVisualComponent.InitMethodAddresses;
begin
  MethodAddresses[63] := @TSepiImportsTVisualComponent.Create;
  MethodAddresses[64] := @TSepiImportsTVisualComponent.Draw_0;
  MethodAddresses[65] := @TSepiImportsTVisualComponent.Draw_1;
  MethodAddresses[66] := @TSepiImportsTVisualComponent.DrawToCanvas;
end;

{----------------}
{ TPlugin import }
{----------------}

class procedure TSepiImportsTPlugin.InitMethodAddresses;
begin
  MethodAddresses[67] := @TSepiImportsTPlugin.Create;
end;

{---------------}
{ TField import }
{---------------}

class procedure TSepiImportsTField.InitMethodAddresses;
begin
  MethodAddresses[68] := @TSepiImportsTField.Create;
end;

{----------------}
{ TSquare import }
{----------------}

class procedure TSepiImportsTSquare.InitMethodAddresses;
begin
  MethodAddresses[69] := @TSepiImportsTSquare.Create;
  MethodAddresses[70] := @TSepiImportsTSquare.Contains;
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

constructor TSepiImportsTMap.Create_0(AMaster: TMaster; const AID: TComponentID);
begin
  Create(AMaster, AID);
end;

constructor TSepiImportsTMap.Create_1(AMaster: TMaster; const AID: TComponentID; ADimensions: T3DPoint; AZoneWidth: Integer; AZoneHeight: Integer);
begin
  Create(AMaster, AID, ADimensions, AZoneWidth, AZoneHeight);
end;

class procedure TSepiImportsTMap.InitMethodAddresses;
begin
  MethodAddresses[71] := @TSepiImportsTMap.SetMaxViewSize;
  MethodAddresses[72] := @TSepiImportsTMap.GetMap;
  MethodAddresses[73] := @TSepiImportsTMap.SetMap;
  MethodAddresses[74] := @TSepiImportsTMap.GetOutside;
  MethodAddresses[75] := @TSepiImportsTMap.SetOutside;
  MethodAddresses[76] := @TSepiImportsTMap.GetLinearMapCount;
  MethodAddresses[77] := @TSepiImportsTMap.GetLinearMap;
  MethodAddresses[78] := @TSepiImportsTMap.SetLinearMap;
  MethodAddresses[79] := @TSepiImportsTMap.Create_0;
  MethodAddresses[80] := @TSepiImportsTMap.Create_1;
  MethodAddresses[81] := @TSepiImportsTMap.InMap;
  MethodAddresses[82] := @TSepiImportsTMap.PlayersOn;
end;

{--------------------}
{ TPlayerMode import }
{--------------------}

class procedure TSepiImportsTPlayerMode.InitMethodAddresses;
begin
  MethodAddresses[83] := @TSepiImportsTPlayerMode.GetModeClass;
  MethodAddresses[84] := @TSepiImportsTPlayerMode.GetPlayer;
end;

{-----------------------------}
{ TLabyrinthPlayerMode import }
{-----------------------------}

class procedure TSepiImportsTLabyrinthPlayerMode.InitMethodAddresses;
begin
  MethodAddresses[85] := @TSepiImportsTLabyrinthPlayerMode.InvalidateCache;
  MethodAddresses[86] := @TSepiImportsTLabyrinthPlayerMode.InvalidateCacheIfNeeded;
  MethodAddresses[87] := @TSepiImportsTLabyrinthPlayerMode.UpdateCache;
  MethodAddresses[88] := @TSepiImportsTLabyrinthPlayerMode.DrawPlayers;
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
  MethodAddresses[89] := @TSepiImportsTPlayer.GetVisible;
  MethodAddresses[90] := @TSepiImportsTPlayer.Create;
  MethodAddresses[91] := @TSepiImportsTPlayer.GetPluginIDs;
  MethodAddresses[92] := @TSepiImportsTPlayer.DrawInPlace;
  MethodAddresses[93] := @TSepiImportsTPlayer.ChangeMode;
  MethodAddresses[94] := @TSepiImportsTPlayer.BeginTempMode;
  MethodAddresses[95] := @TSepiImportsTPlayer.EndTempMode;
  MethodAddresses[96] := @TSepiImportsTPlayer.AddPlugin;
  MethodAddresses[97] := @TSepiImportsTPlayer.RemovePlugin;
  MethodAddresses[98] := @TSepiImportsTPlayer.AbleTo;
  MethodAddresses[99] := @TSepiImportsTPlayer.DoAction;
  MethodAddresses[100] := @TSepiImportsTPlayer.Move;
  MethodAddresses[101] := @TSepiImportsTPlayer.MoveTo_1;
  MethodAddresses[102] := @TSepiImportsTPlayer.MoveTo_2;
  MethodAddresses[103] := @TSepiImportsTPlayer.MoveTo_3;
  MethodAddresses[104] := @TSepiImportsTPlayer.MoveTo_4;
  MethodAddresses[105] := @TSepiImportsTPlayer.Temporize;
  MethodAddresses[106] := @TSepiImportsTPlayer.NaturalMoving;
  MethodAddresses[107] := @TSepiImportsTPlayer.ChangePosition;
  MethodAddresses[108] := @TSepiImportsTPlayer.Show;
  MethodAddresses[109] := @TSepiImportsTPlayer.Hide;
  MethodAddresses[110] := @TSepiImportsTPlayer.SendCommand;
  MethodAddresses[111] := @TSepiImportsTPlayer.ShowDialog;
  MethodAddresses[112] := @TSepiImportsTPlayer.ShowDialogRadio;
  MethodAddresses[113] := @TSepiImportsTPlayer.ChooseNumber;
  MethodAddresses[114] := @TSepiImportsTPlayer.ShowMessage;
  MethodAddresses[115] := @TSepiImportsTPlayer.ShowSelectionMsg_0;
  MethodAddresses[116] := @TSepiImportsTPlayer.ShowSelectionMsg_1;
  MethodAddresses[117] := @TSepiImportsTPlayer.ShowSelectNumberMsg;
  MethodAddresses[118] := @TSepiImportsTPlayer.Win;
  MethodAddresses[119] := @TSepiImportsTPlayer.Lose;
  MethodAddresses[120] := @TSepiImportsTPlayer.PressKey;
  MethodAddresses[121] := @TSepiImportsTPlayer.SendMessage;
  MethodAddresses[122] := @TSepiImportsTPlayer.WaitForKey;
  MethodAddresses[123] := @TSepiImportsTPlayer.WaitForSpecificKey;
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
  MethodAddresses[124] := @TSepiImportsTMaster.GetComponent;
  MethodAddresses[125] := @TSepiImportsTMaster.GetSquareComponent;
  MethodAddresses[126] := @TSepiImportsTMaster.GetPlugin;
  MethodAddresses[127] := @TSepiImportsTMaster.GetObjectDef;
  MethodAddresses[128] := @TSepiImportsTMaster.GetField;
  MethodAddresses[129] := @TSepiImportsTMaster.GetEffect;
  MethodAddresses[130] := @TSepiImportsTMaster.GetTool;
  MethodAddresses[131] := @TSepiImportsTMaster.GetObstacle;
  MethodAddresses[132] := @TSepiImportsTMaster.GetSquare;
  MethodAddresses[133] := @TSepiImportsTMaster.GetMap;
  MethodAddresses[134] := @TSepiImportsTMaster.GetPlayer;
  MethodAddresses[135] := @TSepiImportsTMaster.GetComponentCount;
  MethodAddresses[136] := @TSepiImportsTMaster.GetComponents;
  MethodAddresses[137] := @TSepiImportsTMaster.GetPluginCount;
  MethodAddresses[138] := @TSepiImportsTMaster.GetPlugins;
  MethodAddresses[139] := @TSepiImportsTMaster.GetObjectDefCount;
  MethodAddresses[140] := @TSepiImportsTMaster.GetObjectDefs;
  MethodAddresses[141] := @TSepiImportsTMaster.GetFieldCount;
  MethodAddresses[142] := @TSepiImportsTMaster.GetFields;
  MethodAddresses[143] := @TSepiImportsTMaster.GetEffectCount;
  MethodAddresses[144] := @TSepiImportsTMaster.GetEffects;
  MethodAddresses[145] := @TSepiImportsTMaster.GetToolCount;
  MethodAddresses[146] := @TSepiImportsTMaster.GetTools;
  MethodAddresses[147] := @TSepiImportsTMaster.GetObstacleCount;
  MethodAddresses[148] := @TSepiImportsTMaster.GetObstacles;
  MethodAddresses[149] := @TSepiImportsTMaster.GetSquareCount;
  MethodAddresses[150] := @TSepiImportsTMaster.GetSquares;
  MethodAddresses[151] := @TSepiImportsTMaster.GetMapCount;
  MethodAddresses[152] := @TSepiImportsTMaster.GetMaps;
  MethodAddresses[153] := @TSepiImportsTMaster.GetPlayerCount;
  MethodAddresses[154] := @TSepiImportsTMaster.GetPlayers;
  MethodAddresses[155] := @TSepiImportsTMaster.SetTemporization;
  MethodAddresses[156] := @TSepiImportsTMaster.GetTickCount;
  MethodAddresses[157] := @TSepiImportsTMaster.Create;
  MethodAddresses[158] := @TSepiImportsTMaster.Temporize;
  MethodAddresses[159] := @TSepiImportsTMaster.SquareByComps;
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
  TypeInfoArray[9] := TypeInfo(TEditMapSquareFlag);
  TypeInfoArray[10] := TypeInfo(TEditMapSquareFlags);
  TypeInfoArray[11] := TypeInfo(TDrawSquareContext);
  TypeInfoArray[12] := TypeInfo(TDrawViewContext);
  TypeInfoArray[13] := TypeInfo(TKeyEventContext);
  TypeInfoArray[14] := TypeInfo(TImagesMaster);
  TypeInfoArray[15] := TypeInfo(TPainter);
  TypeInfoArray[16] := TypeInfo(TMoveContext);
  TypeInfoArray[17] := TypeInfo(TPersistentStateItem);
  TypeInfoArray[18] := TypeInfo(TPersistentState);
  TypeInfoArray[19] := TypeInfo(TFunLabyPersistent);
  TypeInfoArray[20] := TypeInfo(TInterfacedFunLabyPersistent);
  TypeInfoArray[21] := TypeInfo(TFunLabyCollection);
  TypeInfoArray[22] := TypeInfo(TFunLabyFiler);
  TypeInfoArray[23] := TypeInfo(TFunLabyReader);
  TypeInfoArray[24] := TypeInfo(TFunLabyWriter);
  TypeInfoArray[25] := TypeInfo(TPlayerData);
  TypeInfoArray[26] := TypeInfo(TFunLabyComponent);
  TypeInfoArray[27] := TypeInfo(TVisualComponent);
  TypeInfoArray[28] := TypeInfo(TPlugin);
  TypeInfoArray[29] := TypeInfo(TPluginDynArray);
  TypeInfoArray[30] := TypeInfo(TObjectDefPlayerData);
  TypeInfoArray[31] := TypeInfo(TObjectDef);
  TypeInfoArray[32] := TypeInfo(TSquareComponent);
  TypeInfoArray[33] := TypeInfo(TField);
  TypeInfoArray[34] := TypeInfo(TEffect);
  TypeInfoArray[35] := TypeInfo(TTool);
  TypeInfoArray[36] := TypeInfo(TObstacle);
  TypeInfoArray[37] := TypeInfo(TSquare);
  TypeInfoArray[38] := TypeInfo(TMap);
  TypeInfoArray[39] := TypeInfo(IPlayerMode);
  TypeInfoArray[40] := TypeInfo(TPlayerMode);
  TypeInfoArray[41] := TypeInfo(TLabyrinthPlayerMode);
  TypeInfoArray[42] := TypeInfo(TPlayer);
  TypeInfoArray[43] := TypeInfo(TMaster);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDrawSquareContext.InitMethodAddresses;
  TSepiImportsTDrawViewContext.InitMethodAddresses;
  TSepiImportsTKeyEventContext.InitMethodAddresses;
  TSepiImportsTImagesMaster.InitMethodAddresses;
  TSepiImportsTPainter.InitMethodAddresses;
  TSepiImportsTMoveContext.InitMethodAddresses;
  TSepiImportsTInterfacedFunLabyPersistent.InitMethodAddresses;
  TSepiImportsTFunLabyCollection.InitMethodAddresses;
  TSepiImportsTFunLabyFiler.InitMethodAddresses;
  TSepiImportsTFunLabyReader.InitMethodAddresses;
  TSepiImportsTFunLabyWriter.InitMethodAddresses;
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
  MethodAddresses[160] := @ShowFunLabyAbout;
  MethodAddresses[161] := @PointBehind;
  MethodAddresses[162] := @PointBefore;
  MethodAddresses[163] := @CreateEmptySquareBitmap;
  MethodAddresses[164] := @SquareRect;
  MethodAddresses[165] := @EmptyRect;
  MethodAddresses[166] := @EmptySquareRect;
  MethodAddresses[167] := @DrawBitmap32ToCanvas;
  MethodAddresses[168] := @SameRect;
  MethodAddresses[169] := @IsNoQPos;
  MethodAddresses[170] := @FunLabyRegisterClass;
  MethodAddresses[171] := @FunLabyUnregisterClass;
  MethodAddresses[172] := @FunLabyRegisterClasses;
  MethodAddresses[173] := @FunLabyUnregisterClasses;
  MethodAddresses[174] := @FunLabyGetClass;
  MethodAddresses[175] := @FunLabyFindClass;
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
  VarAddresses[7] := @fLabyrinthsDir;
  VarAddresses[8] := @fSaveguardsDir;
  VarAddresses[9] := @fEditPluginDir;
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTComponentID = record
    Dummy: Byte;
    Field: TComponentID;
  end;

{$IF SizeOf(TCheckAlignmentForTComponentID) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TComponentID n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDirection = record
    Dummy: Byte;
    Field: TDirection;
  end;

{$IF SizeOf(TCheckAlignmentForTDirection) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TDirection n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayerAction = record
    Dummy: Byte;
    Field: TPlayerAction;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerAction) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlayerAction n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayState = record
    Dummy: Byte;
    Field: TPlayState;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayState) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TPlayState n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForEFunLabyException = record
    Dummy: Byte;
    Field: EFunLabyException;
  end;

{$IF SizeOf(TCheckAlignmentForEFunLabyException) <> (4 + 4)}
  {$MESSAGE WARN 'Le type EFunLabyException n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForEComponentNotFound = record
    Dummy: Byte;
    Field: EComponentNotFound;
  end;

{$IF SizeOf(TCheckAlignmentForEComponentNotFound) <> (4 + 4)}
  {$MESSAGE WARN 'Le type EComponentNotFound n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForEUnsupportedCommand = record
    Dummy: Byte;
    Field: EUnsupportedCommand;
  end;

{$IF SizeOf(TCheckAlignmentForEUnsupportedCommand) <> (4 + 4)}
  {$MESSAGE WARN 'Le type EUnsupportedCommand n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForEBadSquareDefException = record
    Dummy: Byte;
    Field: EBadSquareDefException;
  end;

{$IF SizeOf(TCheckAlignmentForEBadSquareDefException) <> (4 + 4)}
  {$MESSAGE WARN 'Le type EBadSquareDefException n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IF SizeOf(TQualifiedPos) <> 16}
  {$MESSAGE WARN 'Le type TQualifiedPos n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTQualifiedPos = record
    Dummy: Byte;
    Field: TQualifiedPos;
  end;

{$IF SizeOf(TCheckAlignmentForTQualifiedPos) <> (4 + 16)}
  {$MESSAGE WARN 'Le type TQualifiedPos n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

{$IF SizeOf(TPlayerMessage) <> 8}
  {$MESSAGE WARN 'Le type TPlayerMessage n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTPlayerMessage = record
    Dummy: Byte;
    Field: TPlayerMessage;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerMessage) <> (4 + 8)}
  {$MESSAGE WARN 'Le type TPlayerMessage n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

{$IF SizeOf(TPlayerShowMsgMessage) <> 24}
  {$MESSAGE WARN 'Le type TPlayerShowMsgMessage n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTPlayerShowMsgMessage = record
    Dummy: Byte;
    Field: TPlayerShowMsgMessage;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerShowMsgMessage) <> (4 + 24)}
  {$MESSAGE WARN 'Le type TPlayerShowMsgMessage n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

type
  TCheckAlignmentForTEditMapSquareFlag = record
    Dummy: Byte;
    Field: TEditMapSquareFlag;
  end;

{$IF SizeOf(TCheckAlignmentForTEditMapSquareFlag) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TEditMapSquareFlag n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IF SizeOf(TEditMapSquareFlags) <> 1}
  {$MESSAGE WARN 'Le type TEditMapSquareFlags n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTEditMapSquareFlags = record
    Dummy: Byte;
    Field: TEditMapSquareFlags;
  end;

{$IF SizeOf(TCheckAlignmentForTEditMapSquareFlags) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TEditMapSquareFlags n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

{$IF SizeOf(TEditMapSquareMessage) <> 20}
  {$MESSAGE WARN 'Le type TEditMapSquareMessage n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTEditMapSquareMessage = record
    Dummy: Byte;
    Field: TEditMapSquareMessage;
  end;

{$IF SizeOf(TCheckAlignmentForTEditMapSquareMessage) <> (4 + 20)}
  {$MESSAGE WARN 'Le type TEditMapSquareMessage n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

type
  TCheckAlignmentForTSendCommandEvent = record
    Dummy: Byte;
    Field: TSendCommandEvent;
  end;

{$IF SizeOf(TCheckAlignmentForTSendCommandEvent) <> (8 + 8)}
  {$MESSAGE WARN 'Le type TSendCommandEvent n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTRegisterSingleComponentProc = record
    Dummy: Byte;
    Field: TRegisterSingleComponentProc;
  end;

{$IF SizeOf(TCheckAlignmentForTRegisterSingleComponentProc) <> (8 + 8)}
  {$MESSAGE WARN 'Le type TRegisterSingleComponentProc n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTRegisterComponentSetProc = record
    Dummy: Byte;
    Field: TRegisterComponentSetProc;
  end;

{$IF SizeOf(TCheckAlignmentForTRegisterComponentSetProc) <> (8 + 8)}
  {$MESSAGE WARN 'Le type TRegisterComponentSetProc n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDrawSquareContext = record
    Dummy: Byte;
    Field: TDrawSquareContext;
  end;

{$IF SizeOf(TCheckAlignmentForTDrawSquareContext) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDrawSquareContext n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDrawViewContext = record
    Dummy: Byte;
    Field: TDrawViewContext;
  end;

{$IF SizeOf(TCheckAlignmentForTDrawViewContext) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDrawViewContext n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTKeyEventContext = record
    Dummy: Byte;
    Field: TKeyEventContext;
  end;

{$IF SizeOf(TCheckAlignmentForTKeyEventContext) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TKeyEventContext n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTImagesMaster = record
    Dummy: Byte;
    Field: TImagesMaster;
  end;

{$IF SizeOf(TCheckAlignmentForTImagesMaster) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TImagesMaster n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPainter = record
    Dummy: Byte;
    Field: TPainter;
  end;

{$IF SizeOf(TCheckAlignmentForTPainter) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPainter n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTMoveContext = record
    Dummy: Byte;
    Field: TMoveContext;
  end;

{$IF SizeOf(TCheckAlignmentForTMoveContext) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TMoveContext n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPersistentStateItem = record
    Dummy: Byte;
    Field: TPersistentStateItem;
  end;

{$IF SizeOf(TCheckAlignmentForTPersistentStateItem) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TPersistentStateItem n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IF SizeOf(TPersistentState) <> 1}
  {$MESSAGE WARN 'Le type TPersistentState n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTPersistentState = record
    Dummy: Byte;
    Field: TPersistentState;
  end;

{$IF SizeOf(TCheckAlignmentForTPersistentState) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TPersistentState n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

type
  TCheckAlignmentForTFunLabyPersistent = record
    Dummy: Byte;
    Field: TFunLabyPersistent;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyPersistent) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyPersistent n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyPersistentClass = record
    Dummy: Byte;
    Field: TFunLabyPersistentClass;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyPersistentClass) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyPersistentClass n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTInterfacedFunLabyPersistent = record
    Dummy: Byte;
    Field: TInterfacedFunLabyPersistent;
  end;

{$IF SizeOf(TCheckAlignmentForTInterfacedFunLabyPersistent) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TInterfacedFunLabyPersistent n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyCollection = record
    Dummy: Byte;
    Field: TFunLabyCollection;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyCollection) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyCollection n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyFiler = record
    Dummy: Byte;
    Field: TFunLabyFiler;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyFiler) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyFiler n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyReader = record
    Dummy: Byte;
    Field: TFunLabyReader;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyReader) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyReader n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyWriter = record
    Dummy: Byte;
    Field: TFunLabyWriter;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyWriter) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyWriter n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayerData = record
    Dummy: Byte;
    Field: TPlayerData;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerData) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlayerData n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayerDataClass = record
    Dummy: Byte;
    Field: TPlayerDataClass;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerDataClass) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlayerDataClass n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyComponent = record
    Dummy: Byte;
    Field: TFunLabyComponent;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyComponent) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyComponent n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFunLabyComponentClass = record
    Dummy: Byte;
    Field: TFunLabyComponentClass;
  end;

{$IF SizeOf(TCheckAlignmentForTFunLabyComponentClass) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFunLabyComponentClass n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTVisualComponent = record
    Dummy: Byte;
    Field: TVisualComponent;
  end;

{$IF SizeOf(TCheckAlignmentForTVisualComponent) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TVisualComponent n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlugin = record
    Dummy: Byte;
    Field: TPlugin;
  end;

{$IF SizeOf(TCheckAlignmentForTPlugin) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlugin n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPluginDynArray = record
    Dummy: Byte;
    Field: TPluginDynArray;
  end;

{$IF SizeOf(TCheckAlignmentForTPluginDynArray) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPluginDynArray n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTObjectDefPlayerData = record
    Dummy: Byte;
    Field: TObjectDefPlayerData;
  end;

{$IF SizeOf(TCheckAlignmentForTObjectDefPlayerData) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TObjectDefPlayerData n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTObjectDef = record
    Dummy: Byte;
    Field: TObjectDef;
  end;

{$IF SizeOf(TCheckAlignmentForTObjectDef) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TObjectDef n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTSquareComponent = record
    Dummy: Byte;
    Field: TSquareComponent;
  end;

{$IF SizeOf(TCheckAlignmentForTSquareComponent) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TSquareComponent n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTField = record
    Dummy: Byte;
    Field: TField;
  end;

{$IF SizeOf(TCheckAlignmentForTField) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TField n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTEffect = record
    Dummy: Byte;
    Field: TEffect;
  end;

{$IF SizeOf(TCheckAlignmentForTEffect) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TEffect n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTTool = record
    Dummy: Byte;
    Field: TTool;
  end;

{$IF SizeOf(TCheckAlignmentForTTool) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TTool n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTObstacle = record
    Dummy: Byte;
    Field: TObstacle;
  end;

{$IF SizeOf(TCheckAlignmentForTObstacle) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TObstacle n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTSquare = record
    Dummy: Byte;
    Field: TSquare;
  end;

{$IF SizeOf(TCheckAlignmentForTSquare) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TSquare n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTMap = record
    Dummy: Byte;
    Field: TMap;
  end;

{$IF SizeOf(TCheckAlignmentForTMap) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TMap n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayerModeClass = record
    Dummy: Byte;
    Field: TPlayerModeClass;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerModeClass) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlayerModeClass n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForIPlayerMode = record
    Dummy: Byte;
    Field: IPlayerMode;
  end;

{$IF SizeOf(TCheckAlignmentForIPlayerMode) <> (4 + 4)}
  {$MESSAGE WARN 'Le type IPlayerMode n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayerMode = record
    Dummy: Byte;
    Field: TPlayerMode;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayerMode) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlayerMode n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTLabyrinthPlayerMode = record
    Dummy: Byte;
    Field: TLabyrinthPlayerMode;
  end;

{$IF SizeOf(TCheckAlignmentForTLabyrinthPlayerMode) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TLabyrinthPlayerMode n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlayer = record
    Dummy: Byte;
    Field: TPlayer;
  end;

{$IF SizeOf(TCheckAlignmentForTPlayer) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlayer n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTMaster = record
    Dummy: Byte;
    Field: TMaster;
  end;

{$IF SizeOf(TCheckAlignmentForTMaster) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TMaster n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FunLabyUtils;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(EFunLabyException, 12, 12);
  CheckInstanceSize(EComponentNotFound, 12, 12);
  CheckInstanceSize(EUnsupportedCommand, 12, 12);
  CheckInstanceSize(EBadSquareDefException, 12, 12);
  CheckInstanceSize(TDrawSquareContext, 60, 4);
  CheckInstanceSize(TDrawViewContext, 72, 4);
  CheckInstanceSize(TKeyEventContext, 12, 4);
  CheckInstanceSize(TImagesMaster, 16, 4);
  CheckInstanceSize(TPainter, 20, 4);
  CheckInstanceSize(TMoveContext, 60, 4);
  CheckInstanceSize(TFunLabyPersistent, 8, 4);
  CheckInstanceSize(TInterfacedFunLabyPersistent, 16, 8);
  CheckInstanceSize(TFunLabyCollection, 12, 8);
  CheckInstanceSize(TFunLabyFiler, 16, 4);
  CheckInstanceSize(TFunLabyReader, 16, 16);
  CheckInstanceSize(TFunLabyWriter, 16, 16);
  CheckInstanceSize(TPlayerData, 16, 8);
  CheckInstanceSize(TFunLabyComponent, 28, 8);
  CheckInstanceSize(TVisualComponent, 44, 28);
  CheckInstanceSize(TPlugin, 40, 28);
  CheckInstanceSize(TObjectDefPlayerData, 20, 16);
  CheckInstanceSize(TObjectDef, 44, 44);
  CheckInstanceSize(TSquareComponent, 44, 44);
  CheckInstanceSize(TField, 48, 44);
  CheckInstanceSize(TEffect, 44, 44);
  CheckInstanceSize(TTool, 44, 44);
  CheckInstanceSize(TObstacle, 44, 44);
  CheckInstanceSize(TSquare, 60, 44);
  CheckInstanceSize(TMap, 60, 28);
  CheckInstanceSize(TPlayerMode, 28, 16);
  CheckInstanceSize(TLabyrinthPlayerMode, 60, 28);
  CheckInstanceSize(TPlayer, 128, 44);
  CheckInstanceSize(TMaster, 68, 8);
  {$ASSERTIONS OFF}
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

