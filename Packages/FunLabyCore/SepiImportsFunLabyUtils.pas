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
  TypeCount = 46;
  MethodCount = 200;
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
    constructor Create_3(Source: TDrawSquareContext; const AQPos: TQualifiedPos);
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
    function GetIsEmpty: Boolean;
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

  TSepiImportsTComponentCreator = class(TComponentCreator)
  private
    function GetCreatedComponentCount: Integer;
    function GetCreatedComponents(Index: Integer): TFunLabyComponent;
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
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TSquareComponent;
    function GetComponentClasses(Index: Integer): TSquareComponentClass;
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
    function GetAttribute(const AttrName: string): Integer;
    procedure SetAttribute(const AttrName: string; Value: Integer);
    function IsMoveAllowed_0(Context: TMoveContext): Boolean;
    function IsMoveAllowed_1(const Dest: T3DPoint; KeyPressed: Boolean): Boolean;
    procedure MoveTo_0(Context: TMoveContext; Execute: Boolean);
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
    function SquareByComps_0(const Field: TComponentID; const Effect: TComponentID; const Tool: TComponentID; const Obstacle: TComponentID): TSquare;
    function SquareByComps_1(Field: TField; Effect: TEffect; Tool: TTool; Obstacle: TObstacle): TSquare;
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

constructor TSepiImportsTDrawSquareContext.Create_3(Source: TDrawSquareContext; const AQPos: TQualifiedPos);
begin
  Create(Source, AQPos);
end;

class procedure TSepiImportsTDrawSquareContext.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTDrawSquareContext.Create_0;
  MethodAddresses[1] := @TSepiImportsTDrawSquareContext.Create_1;
  MethodAddresses[2] := @TSepiImportsTDrawSquareContext.Create_2;
  MethodAddresses[3] := @TSepiImportsTDrawSquareContext.Create_3;
  MethodAddresses[4] := @TSepiImportsTDrawSquareContext.SetDrawViewContext;
  MethodAddresses[5] := @TSepiImportsTDrawSquareContext.SetTickCount;
  MethodAddresses[6] := @TSepiImportsTDrawSquareContext.Assign;
  MethodAddresses[7] := @TSepiImportsTDrawSquareContext.DrawSquareBitmap;
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
  MethodAddresses[8] := @TSepiImportsTDrawViewContext.Create;
  MethodAddresses[9] := @TSepiImportsTDrawViewContext.IsSquareVisible_0;
  MethodAddresses[10] := @TSepiImportsTDrawViewContext.IsSquareVisible_1;
  MethodAddresses[11] := @TSepiImportsTDrawViewContext.IsSquareVisible_2;
end;

{-------------------------}
{ TKeyEventContext import }
{-------------------------}

class procedure TSepiImportsTKeyEventContext.InitMethodAddresses;
begin
  MethodAddresses[12] := @TSepiImportsTKeyEventContext.Create;
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
  MethodAddresses[13] := @TSepiImportsTImagesMaster.Create;
  MethodAddresses[14] := @TSepiImportsTImagesMaster.IndexOf;
  MethodAddresses[15] := @TSepiImportsTImagesMaster.Draw_0;
  MethodAddresses[16] := @TSepiImportsTImagesMaster.Draw_1;
  MethodAddresses[17] := @TSepiImportsTImagesMaster.Draw_2;
  MethodAddresses[18] := @TSepiImportsTImagesMaster.Draw_3;
  MethodAddresses[19] := @TSepiImportsTImagesMaster.GetInternalBitmap;
end;

{-----------------}
{ TPainter import }
{-----------------}

function TSepiImportsTPainter.GetIsEmpty: Boolean;
begin
  Result := IsEmpty;
end;

class procedure TSepiImportsTPainter.InitMethodAddresses;
begin
  MethodAddresses[20] := @TSepiImportsTPainter.GetIsEmpty;
  MethodAddresses[21] := @TSepiImportsTPainter.Create;
  MethodAddresses[22] := @TSepiImportsTPainter.Draw;
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
  MethodAddresses[23] := @TSepiImportsTMoveContext.GetSrcSquare;
  MethodAddresses[24] := @TSepiImportsTMoveContext.SetSrcSquare;
  MethodAddresses[25] := @TSepiImportsTMoveContext.GetDestSquare;
  MethodAddresses[26] := @TSepiImportsTMoveContext.SetDestSquare;
  MethodAddresses[27] := @TSepiImportsTMoveContext.GetSquare;
  MethodAddresses[28] := @TSepiImportsTMoveContext.SetSquare;
  MethodAddresses[29] := @TSepiImportsTMoveContext.Create_0;
  MethodAddresses[30] := @TSepiImportsTMoveContext.Create_1;
  MethodAddresses[31] := @TSepiImportsTMoveContext.Cancel;
end;

{-------------------------------------}
{ TInterfacedFunLabyPersistent import }
{-------------------------------------}

class procedure TSepiImportsTInterfacedFunLabyPersistent.InitMethodAddresses;
begin
  MethodAddresses[32] := @TSepiImportsTInterfacedFunLabyPersistent.QueryInterface;
  MethodAddresses[33] := @TSepiImportsTInterfacedFunLabyPersistent._AddRef;
  MethodAddresses[34] := @TSepiImportsTInterfacedFunLabyPersistent._Release;
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
  MethodAddresses[35] := @TSepiImportsTFunLabyCollection.GetCount;
  MethodAddresses[36] := @TSepiImportsTFunLabyCollection.GetItems;
  MethodAddresses[37] := @TSepiImportsTFunLabyCollection.AddItem;
  MethodAddresses[38] := @TSepiImportsTFunLabyCollection.InsertItem;
  MethodAddresses[39] := @TSepiImportsTFunLabyCollection.Create;
  MethodAddresses[40] := @TSepiImportsTFunLabyCollection.Clear;
  MethodAddresses[41] := @TSepiImportsTFunLabyCollection.Add;
  MethodAddresses[42] := @TSepiImportsTFunLabyCollection.AddDefault;
  MethodAddresses[43] := @TSepiImportsTFunLabyCollection.Insert;
  MethodAddresses[44] := @TSepiImportsTFunLabyCollection.Delete;
  MethodAddresses[45] := @TSepiImportsTFunLabyCollection.Remove;
  MethodAddresses[46] := @TSepiImportsTFunLabyCollection.Exchange;
  MethodAddresses[47] := @TSepiImportsTFunLabyCollection.Move;
  MethodAddresses[48] := @TSepiImportsTFunLabyCollection.IndexOf;
  MethodAddresses[49] := @TSepiImportsTFunLabyCollection.HasDefault;
end;

{----------------------}
{ TFunLabyFiler import }
{----------------------}

class procedure TSepiImportsTFunLabyFiler.InitMethodAddresses;
begin
  MethodAddresses[50] := @TSepiImportsTFunLabyFiler.EnumProperties;
  MethodAddresses[51] := @TSepiImportsTFunLabyFiler.InstanceBeginState;
  MethodAddresses[52] := @TSepiImportsTFunLabyFiler.InstanceEndState;
  MethodAddresses[53] := @TSepiImportsTFunLabyFiler.HasPlayerData;
  MethodAddresses[54] := @TSepiImportsTFunLabyFiler.GetPlayerData;
  MethodAddresses[55] := @TSepiImportsTFunLabyFiler.Create;
  MethodAddresses[56] := @TSepiImportsTFunLabyFiler.DefinePublishedProperty;
  MethodAddresses[57] := @TSepiImportsTFunLabyFiler.DefineProcProperty;
  MethodAddresses[58] := @TSepiImportsTFunLabyFiler.DefineFieldProcProperty;
  MethodAddresses[59] := @TSepiImportsTFunLabyFiler.DefineFieldProperty;
  MethodAddresses[60] := @TSepiImportsTFunLabyFiler.DefinePersistent;
  MethodAddresses[61] := @TSepiImportsTFunLabyFiler.DefineStrings;
  MethodAddresses[62] := @TSepiImportsTFunLabyFiler.DefineBinaryProperty;
end;

{-----------------------}
{ TFunLabyReader import }
{-----------------------}

class procedure TSepiImportsTFunLabyReader.InitMethodAddresses;
begin
  MethodAddresses[63] := @TSepiImportsTFunLabyReader.Create;
end;

{-----------------------}
{ TFunLabyWriter import }
{-----------------------}

class procedure TSepiImportsTFunLabyWriter.InitMethodAddresses;
begin
  MethodAddresses[64] := @TSepiImportsTFunLabyWriter.Create;
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
  MethodAddresses[65] := @TSepiImportsTFunLabyComponent.GetSafeID;
  MethodAddresses[66] := @TSepiImportsTFunLabyComponent.HasPlayerData;
  MethodAddresses[67] := @TSepiImportsTFunLabyComponent.GetPlayerData;
  MethodAddresses[68] := @TSepiImportsTFunLabyComponent.Create;
  MethodAddresses[69] := @TSepiImportsTFunLabyComponent.UsePlayerData;
  MethodAddresses[70] := @TSepiImportsTFunLabyComponent.DrawIconToCanvas;
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
  MethodAddresses[71] := @TSepiImportsTVisualComponent.Create;
  MethodAddresses[72] := @TSepiImportsTVisualComponent.Draw_0;
  MethodAddresses[73] := @TSepiImportsTVisualComponent.Draw_1;
end;

{--------------------------}
{ TComponentCreator import }
{--------------------------}

function TSepiImportsTComponentCreator.GetCreatedComponentCount: Integer;
begin
  Result := CreatedComponentCount;
end;

function TSepiImportsTComponentCreator.GetCreatedComponents(Index: Integer): TFunLabyComponent;
begin
  Result := CreatedComponents[Index];
end;

class procedure TSepiImportsTComponentCreator.InitMethodAddresses;
begin
  MethodAddresses[74] := @TSepiImportsTComponentCreator.GetCreatedComponentCount;
  MethodAddresses[75] := @TSepiImportsTComponentCreator.GetCreatedComponents;
  MethodAddresses[76] := @TSepiImportsTComponentCreator.Create;
  MethodAddresses[77] := @TSepiImportsTComponentCreator.CreateComponent;
end;

{----------------}
{ TPlugin import }
{----------------}

class procedure TSepiImportsTPlugin.InitMethodAddresses;
begin
  MethodAddresses[78] := @TSepiImportsTPlugin.Create;
end;

{---------------}
{ TField import }
{---------------}

class procedure TSepiImportsTField.InitMethodAddresses;
begin
  MethodAddresses[79] := @TSepiImportsTField.Create;
  MethodAddresses[80] := @TSepiImportsTField.NewDelegateDraw;
  MethodAddresses[81] := @TSepiImportsTField.DrawCeiling;
end;

{----------------}
{ TSquare import }
{----------------}

function TSepiImportsTSquare.GetComponentCount: Integer;
begin
  Result := ComponentCount;
end;

function TSepiImportsTSquare.GetComponents(Index: Integer): TSquareComponent;
begin
  Result := Components[Index];
end;

function TSepiImportsTSquare.GetComponentClasses(Index: Integer): TSquareComponentClass;
begin
  Result := ComponentClasses[Index];
end;

class procedure TSepiImportsTSquare.InitMethodAddresses;
begin
  MethodAddresses[82] := @TSepiImportsTSquare.GetComponentCount;
  MethodAddresses[83] := @TSepiImportsTSquare.GetComponents;
  MethodAddresses[84] := @TSepiImportsTSquare.GetComponentClasses;
  MethodAddresses[85] := @TSepiImportsTSquare.Create;
  MethodAddresses[86] := @TSepiImportsTSquare.Contains;
  MethodAddresses[87] := @TSepiImportsTSquare.DrawCeiling;
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
  MethodAddresses[88] := @TSepiImportsTMap.SetMaxViewSize;
  MethodAddresses[89] := @TSepiImportsTMap.GetMap;
  MethodAddresses[90] := @TSepiImportsTMap.SetMap;
  MethodAddresses[91] := @TSepiImportsTMap.GetOutside;
  MethodAddresses[92] := @TSepiImportsTMap.SetOutside;
  MethodAddresses[93] := @TSepiImportsTMap.GetLinearMapCount;
  MethodAddresses[94] := @TSepiImportsTMap.GetLinearMap;
  MethodAddresses[95] := @TSepiImportsTMap.SetLinearMap;
  MethodAddresses[96] := @TSepiImportsTMap.Create_0;
  MethodAddresses[97] := @TSepiImportsTMap.Create_1;
  MethodAddresses[98] := @TSepiImportsTMap.InMap;
  MethodAddresses[99] := @TSepiImportsTMap.PlayersOn;
end;

{--------------------}
{ TPlayerMode import }
{--------------------}

class procedure TSepiImportsTPlayerMode.InitMethodAddresses;
begin
  MethodAddresses[100] := @TSepiImportsTPlayerMode.GetModeClass;
  MethodAddresses[101] := @TSepiImportsTPlayerMode.GetPlayer;
end;

{-----------------------------}
{ TLabyrinthPlayerMode import }
{-----------------------------}

class procedure TSepiImportsTLabyrinthPlayerMode.InitMethodAddresses;
begin
  MethodAddresses[102] := @TSepiImportsTLabyrinthPlayerMode.InvalidateCache;
  MethodAddresses[103] := @TSepiImportsTLabyrinthPlayerMode.InvalidateCacheIfNeeded;
  MethodAddresses[104] := @TSepiImportsTLabyrinthPlayerMode.UpdateCache;
  MethodAddresses[105] := @TSepiImportsTLabyrinthPlayerMode.DrawPlayers;
  MethodAddresses[106] := @TSepiImportsTLabyrinthPlayerMode.DrawCeiling;
end;

{----------------}
{ TPlayer import }
{----------------}

function TSepiImportsTPlayer.GetVisible: Boolean;
begin
  Result := Visible;
end;

function TSepiImportsTPlayer.GetAttribute(const AttrName: string): Integer;
begin
  Result := Attribute[AttrName];
end;

procedure TSepiImportsTPlayer.SetAttribute(const AttrName: string; Value: Integer);
begin
  Attribute[AttrName] := Value;
end;

function TSepiImportsTPlayer.IsMoveAllowed_0(Context: TMoveContext): Boolean;
begin
  Result := IsMoveAllowed(Context);
end;

function TSepiImportsTPlayer.IsMoveAllowed_1(const Dest: T3DPoint; KeyPressed: Boolean): Boolean;
begin
  Result := IsMoveAllowed(Dest, KeyPressed);
end;

procedure TSepiImportsTPlayer.MoveTo_0(Context: TMoveContext; Execute: Boolean);
begin
  MoveTo(Context, Execute);
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
  MethodAddresses[107] := @TSepiImportsTPlayer.GetVisible;
  MethodAddresses[108] := @TSepiImportsTPlayer.GetAttribute;
  MethodAddresses[109] := @TSepiImportsTPlayer.SetAttribute;
  MethodAddresses[110] := @TSepiImportsTPlayer.Create;
  MethodAddresses[111] := @TSepiImportsTPlayer.GetAttributes;
  MethodAddresses[112] := @TSepiImportsTPlayer.GetPluginIDs;
  MethodAddresses[113] := @TSepiImportsTPlayer.DrawInPlace;
  MethodAddresses[114] := @TSepiImportsTPlayer.ChangeMode;
  MethodAddresses[115] := @TSepiImportsTPlayer.BeginTempMode;
  MethodAddresses[116] := @TSepiImportsTPlayer.EndTempMode;
  MethodAddresses[117] := @TSepiImportsTPlayer.AddPlugin;
  MethodAddresses[118] := @TSepiImportsTPlayer.RemovePlugin;
  MethodAddresses[119] := @TSepiImportsTPlayer.HasPlugin;
  MethodAddresses[120] := @TSepiImportsTPlayer.AbleTo;
  MethodAddresses[121] := @TSepiImportsTPlayer.DoAction;
  MethodAddresses[122] := @TSepiImportsTPlayer.Move;
  MethodAddresses[123] := @TSepiImportsTPlayer.IsMoveAllowed_0;
  MethodAddresses[124] := @TSepiImportsTPlayer.IsMoveAllowed_1;
  MethodAddresses[125] := @TSepiImportsTPlayer.MoveTo_0;
  MethodAddresses[126] := @TSepiImportsTPlayer.MoveTo_1;
  MethodAddresses[127] := @TSepiImportsTPlayer.MoveTo_2;
  MethodAddresses[128] := @TSepiImportsTPlayer.MoveTo_3;
  MethodAddresses[129] := @TSepiImportsTPlayer.MoveTo_4;
  MethodAddresses[130] := @TSepiImportsTPlayer.Temporize;
  MethodAddresses[131] := @TSepiImportsTPlayer.NaturalMoving;
  MethodAddresses[132] := @TSepiImportsTPlayer.ChangePosition;
  MethodAddresses[133] := @TSepiImportsTPlayer.Show;
  MethodAddresses[134] := @TSepiImportsTPlayer.Hide;
  MethodAddresses[135] := @TSepiImportsTPlayer.ShowMessage;
  MethodAddresses[136] := @TSepiImportsTPlayer.ShowSelectionMsg_0;
  MethodAddresses[137] := @TSepiImportsTPlayer.ShowSelectionMsg_1;
  MethodAddresses[138] := @TSepiImportsTPlayer.ShowSelectNumberMsg;
  MethodAddresses[139] := @TSepiImportsTPlayer.Win;
  MethodAddresses[140] := @TSepiImportsTPlayer.Lose;
  MethodAddresses[141] := @TSepiImportsTPlayer.PressKey;
  MethodAddresses[142] := @TSepiImportsTPlayer.SendMessage;
  MethodAddresses[143] := @TSepiImportsTPlayer.WaitForKey;
  MethodAddresses[144] := @TSepiImportsTPlayer.WaitForSpecificKey;
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

function TSepiImportsTMaster.SquareByComps_0(const Field: TComponentID; const Effect: TComponentID; const Tool: TComponentID; const Obstacle: TComponentID): TSquare;
begin
  Result := SquareByComps(Field, Effect, Tool, Obstacle);
end;

function TSepiImportsTMaster.SquareByComps_1(Field: TField; Effect: TEffect; Tool: TTool; Obstacle: TObstacle): TSquare;
begin
  Result := SquareByComps(Field, Effect, Tool, Obstacle);
end;

class procedure TSepiImportsTMaster.InitMethodAddresses;
begin
  MethodAddresses[145] := @TSepiImportsTMaster.GetComponent;
  MethodAddresses[146] := @TSepiImportsTMaster.GetSquareComponent;
  MethodAddresses[147] := @TSepiImportsTMaster.GetPlugin;
  MethodAddresses[148] := @TSepiImportsTMaster.GetObjectDef;
  MethodAddresses[149] := @TSepiImportsTMaster.GetField;
  MethodAddresses[150] := @TSepiImportsTMaster.GetEffect;
  MethodAddresses[151] := @TSepiImportsTMaster.GetTool;
  MethodAddresses[152] := @TSepiImportsTMaster.GetObstacle;
  MethodAddresses[153] := @TSepiImportsTMaster.GetSquare;
  MethodAddresses[154] := @TSepiImportsTMaster.GetMap;
  MethodAddresses[155] := @TSepiImportsTMaster.GetPlayer;
  MethodAddresses[156] := @TSepiImportsTMaster.GetComponentCount;
  MethodAddresses[157] := @TSepiImportsTMaster.GetComponents;
  MethodAddresses[158] := @TSepiImportsTMaster.GetPluginCount;
  MethodAddresses[159] := @TSepiImportsTMaster.GetPlugins;
  MethodAddresses[160] := @TSepiImportsTMaster.GetObjectDefCount;
  MethodAddresses[161] := @TSepiImportsTMaster.GetObjectDefs;
  MethodAddresses[162] := @TSepiImportsTMaster.GetFieldCount;
  MethodAddresses[163] := @TSepiImportsTMaster.GetFields;
  MethodAddresses[164] := @TSepiImportsTMaster.GetEffectCount;
  MethodAddresses[165] := @TSepiImportsTMaster.GetEffects;
  MethodAddresses[166] := @TSepiImportsTMaster.GetToolCount;
  MethodAddresses[167] := @TSepiImportsTMaster.GetTools;
  MethodAddresses[168] := @TSepiImportsTMaster.GetObstacleCount;
  MethodAddresses[169] := @TSepiImportsTMaster.GetObstacles;
  MethodAddresses[170] := @TSepiImportsTMaster.GetSquareCount;
  MethodAddresses[171] := @TSepiImportsTMaster.GetSquares;
  MethodAddresses[172] := @TSepiImportsTMaster.GetMapCount;
  MethodAddresses[173] := @TSepiImportsTMaster.GetMaps;
  MethodAddresses[174] := @TSepiImportsTMaster.GetPlayerCount;
  MethodAddresses[175] := @TSepiImportsTMaster.GetPlayers;
  MethodAddresses[176] := @TSepiImportsTMaster.SetTemporization;
  MethodAddresses[177] := @TSepiImportsTMaster.GetTickCount;
  MethodAddresses[178] := @TSepiImportsTMaster.Create;
  MethodAddresses[179] := @TSepiImportsTMaster.Temporize;
  MethodAddresses[180] := @TSepiImportsTMaster.ComponentExists;
  MethodAddresses[181] := @TSepiImportsTMaster.SquareByComps_0;
  MethodAddresses[182] := @TSepiImportsTMaster.SquareByComps_1;
  MethodAddresses[183] := @TSepiImportsTMaster.RegisterComponents;
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
  TypeInfoArray[9] := TypeInfo(TEditMapSquarePhase);
  TypeInfoArray[10] := TypeInfo(TEditMapSquareFlag);
  TypeInfoArray[11] := TypeInfo(TEditMapSquareFlags);
  TypeInfoArray[12] := TypeInfo(TDrawSquareContext);
  TypeInfoArray[13] := TypeInfo(TDrawViewContext);
  TypeInfoArray[14] := TypeInfo(TKeyEventContext);
  TypeInfoArray[15] := TypeInfo(TImagesMaster);
  TypeInfoArray[16] := TypeInfo(TPainter);
  TypeInfoArray[17] := TypeInfo(TMoveContext);
  TypeInfoArray[18] := TypeInfo(TPersistentStateItem);
  TypeInfoArray[19] := TypeInfo(TPersistentState);
  TypeInfoArray[20] := TypeInfo(TFunLabyPersistent);
  TypeInfoArray[21] := TypeInfo(TInterfacedFunLabyPersistent);
  TypeInfoArray[22] := TypeInfo(TFunLabyCollection);
  TypeInfoArray[23] := TypeInfo(TFunLabyFiler);
  TypeInfoArray[24] := TypeInfo(TFunLabyReader);
  TypeInfoArray[25] := TypeInfo(TFunLabyWriter);
  TypeInfoArray[26] := TypeInfo(TPlayerData);
  TypeInfoArray[27] := TypeInfo(TFunLabyComponent);
  TypeInfoArray[28] := TypeInfo(TVisualComponent);
  TypeInfoArray[29] := TypeInfo(TComponentCreator);
  TypeInfoArray[30] := TypeInfo(TPlugin);
  TypeInfoArray[31] := TypeInfo(TPluginDynArray);
  TypeInfoArray[32] := TypeInfo(TObjectDefPlayerData);
  TypeInfoArray[33] := TypeInfo(TObjectDef);
  TypeInfoArray[34] := TypeInfo(TSquareComponent);
  TypeInfoArray[35] := TypeInfo(TField);
  TypeInfoArray[36] := TypeInfo(TEffect);
  TypeInfoArray[37] := TypeInfo(TTool);
  TypeInfoArray[38] := TypeInfo(TObstacle);
  TypeInfoArray[39] := TypeInfo(TSquare);
  TypeInfoArray[40] := TypeInfo(TMap);
  TypeInfoArray[41] := TypeInfo(IPlayerMode);
  TypeInfoArray[42] := TypeInfo(TPlayerMode);
  TypeInfoArray[43] := TypeInfo(TLabyrinthPlayerMode);
  TypeInfoArray[44] := TypeInfo(TPlayer);
  TypeInfoArray[45] := TypeInfo(TMaster);
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
  TSepiImportsTComponentCreator.InitMethodAddresses;
  TSepiImportsTPlugin.InitMethodAddresses;
  TSepiImportsTField.InitMethodAddresses;
  TSepiImportsTSquare.InitMethodAddresses;
  TSepiImportsTMap.InitMethodAddresses;
  TSepiImportsTPlayerMode.InitMethodAddresses;
  TSepiImportsTLabyrinthPlayerMode.InitMethodAddresses;
  TSepiImportsTPlayer.InitMethodAddresses;
  TSepiImportsTMaster.InitMethodAddresses;
  MethodAddresses[184] := @ShowFunLabyAbout;
  MethodAddresses[185] := @PointBehind;
  MethodAddresses[186] := @PointBefore;
  MethodAddresses[187] := @CreateEmptySquareBitmap;
  MethodAddresses[188] := @SquareRect;
  MethodAddresses[189] := @EmptyRect;
  MethodAddresses[190] := @EmptySquareRect;
  MethodAddresses[191] := @DrawBitmap32ToCanvas;
  MethodAddresses[192] := @SameRect;
  MethodAddresses[193] := @IsNoQPos;
  MethodAddresses[194] := @FunLabyRegisterClass;
  MethodAddresses[195] := @FunLabyUnregisterClass;
  MethodAddresses[196] := @FunLabyRegisterClasses;
  MethodAddresses[197] := @FunLabyUnregisterClasses;
  MethodAddresses[198] := @FunLabyGetClass;
  MethodAddresses[199] := @FunLabyFindClass;
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
  TCheckAlignmentForTEditMapSquarePhase = record
    Dummy: Byte;
    Field: TEditMapSquarePhase;
  end;

{$IF SizeOf(TCheckAlignmentForTEditMapSquarePhase) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TEditMapSquarePhase n''a pas l''alignement calculé par Sepi'}
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

{$IF SizeOf(TEditMapSquareMessage) <> 24}
  {$MESSAGE WARN 'Le type TEditMapSquareMessage n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTEditMapSquareMessage = record
    Dummy: Byte;
    Field: TEditMapSquareMessage;
  end;

{$IF SizeOf(TCheckAlignmentForTEditMapSquareMessage) <> (4 + 24)}
  {$MESSAGE WARN 'Le type TEditMapSquareMessage n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

type
  TCheckAlignmentForTRegisterComponentProc = record
    Dummy: Byte;
    Field: TRegisterComponentProc;
  end;

{$IF SizeOf(TCheckAlignmentForTRegisterComponentProc) <> (8 + 8)}
  {$MESSAGE WARN 'Le type TRegisterComponentProc n''a pas l''alignement calculé par Sepi'}
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
  TCheckAlignmentForTComponentCreator = record
    Dummy: Byte;
    Field: TComponentCreator;
  end;

{$IF SizeOf(TCheckAlignmentForTComponentCreator) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TComponentCreator n''a pas l''alignement calculé par Sepi'}
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
  TCheckAlignmentForTSquareComponentClass = record
    Dummy: Byte;
    Field: TSquareComponentClass;
  end;

{$IF SizeOf(TCheckAlignmentForTSquareComponentClass) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TSquareComponentClass n''a pas l''alignement calculé par Sepi'}
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
  CheckInstanceSize(TDrawSquareContext, 64, 4);
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
  CheckInstanceSize(TVisualComponent, 48, 28);
  CheckInstanceSize(TComponentCreator, 36, 28);
  CheckInstanceSize(TPlugin, 40, 28);
  CheckInstanceSize(TObjectDefPlayerData, 20, 16);
  CheckInstanceSize(TObjectDef, 48, 48);
  CheckInstanceSize(TSquareComponent, 48, 48);
  CheckInstanceSize(TField, 52, 48);
  CheckInstanceSize(TEffect, 48, 48);
  CheckInstanceSize(TTool, 48, 48);
  CheckInstanceSize(TObstacle, 48, 48);
  CheckInstanceSize(TSquare, 68, 48);
  CheckInstanceSize(TMap, 60, 28);
  CheckInstanceSize(TPlayerMode, 28, 16);
  CheckInstanceSize(TLabyrinthPlayerMode, 60, 28);
  CheckInstanceSize(TPlayer, 120, 48);
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

