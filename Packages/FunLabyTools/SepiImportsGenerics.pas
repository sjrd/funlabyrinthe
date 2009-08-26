{*
  Importe l'unité Generics dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsGenerics;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FunLabyUtils, Generics;

var
  SepiImportsGenericsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'Generics';
  ResourceName = 'SepiImportsGenerics';
  TypeCount = 10;
  MethodCount = 9;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTGround = class(TGround)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTDecorativeEffect = class(TDecorativeEffect)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTCounterEffect = class(TCounterEffect)
  private
    function GetCounter(Player: TPlayer): Integer;
    procedure SetCounter(Player: TPlayer; Value: Integer);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTDecorativeObstacle = class(TDecorativeObstacle)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTObjectTool = class(TObjectTool)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTOverriddenSquare = class(TOverriddenSquare)
  private
    class procedure InitMethodAddresses;
  end;

{----------------}
{ TGround import }
{----------------}

class procedure TSepiImportsTGround.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTGround.CreateGround;
end;

{--------------------------}
{ TDecorativeEffect import }
{--------------------------}

class procedure TSepiImportsTDecorativeEffect.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTDecorativeEffect.CreateDeco;
end;

{-----------------------}
{ TCounterEffect import }
{-----------------------}

function TSepiImportsTCounterEffect.GetCounter(Player: TPlayer): Integer;
begin
  Result := Counter[Player];
end;

procedure TSepiImportsTCounterEffect.SetCounter(Player: TPlayer; Value: Integer);
begin
  Counter[Player] := Value;
end;

class procedure TSepiImportsTCounterEffect.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTCounterEffect.GetCounter;
  MethodAddresses[3] := @TSepiImportsTCounterEffect.SetCounter;
  MethodAddresses[4] := @TSepiImportsTCounterEffect.IncCounter;
  MethodAddresses[5] := @TSepiImportsTCounterEffect.IsFirstTime;
end;

{----------------------------}
{ TDecorativeObstacle import }
{----------------------------}

class procedure TSepiImportsTDecorativeObstacle.InitMethodAddresses;
begin
  MethodAddresses[6] := @TSepiImportsTDecorativeObstacle.CreateDeco;
end;

{--------------------}
{ TObjectTool import }
{--------------------}

class procedure TSepiImportsTObjectTool.InitMethodAddresses;
begin
  MethodAddresses[7] := @TSepiImportsTObjectTool.CreateTool;
end;

{--------------------------}
{ TOverriddenSquare import }
{--------------------------}

class procedure TSepiImportsTOverriddenSquare.InitMethodAddresses;
begin
  MethodAddresses[8] := @TSepiImportsTOverriddenSquare.Create;
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
      SepiImportsGenericsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsGenericsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TPlankMessageKind);
  TypeInfoArray[1] := TypeInfo(TGround);
  TypeInfoArray[2] := TypeInfo(TDecorativeEffect);
  TypeInfoArray[3] := TypeInfo(TCounterEffectPlayerData);
  TypeInfoArray[4] := TypeInfo(TCounterEffect);
  TypeInfoArray[5] := TypeInfo(TPushButton);
  TypeInfoArray[6] := TypeInfo(TSwitch);
  TypeInfoArray[7] := TypeInfo(TDecorativeObstacle);
  TypeInfoArray[8] := TypeInfo(TObjectTool);
  TypeInfoArray[9] := TypeInfo(TOverriddenSquare);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTGround.InitMethodAddresses;
  TSepiImportsTDecorativeEffect.InitMethodAddresses;
  TSepiImportsTCounterEffect.InitMethodAddresses;
  TSepiImportsTDecorativeObstacle.InitMethodAddresses;
  TSepiImportsTObjectTool.InitMethodAddresses;
  TSepiImportsTOverriddenSquare.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTPlankMessageKind = record
    Dummy: Byte;
    Field: TPlankMessageKind;
  end;

{$IF SizeOf(TCheckAlignmentForTPlankMessageKind) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TPlankMessageKind n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IF SizeOf(TPlankMessage) <> 44}
  {$MESSAGE WARN 'Le type TPlankMessage n''a pas la taille calculée par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTPlankMessage = record
    Dummy: Byte;
    Field: TPlankMessage;
  end;

{$IF SizeOf(TCheckAlignmentForTPlankMessage) <> (4 + 44)}
  {$MESSAGE WARN 'Le type TPlankMessage n''a pas l''alignement calculé par Sepi'}
{$IFEND}

{$IFEND}

type
  TCheckAlignmentForTGround = record
    Dummy: Byte;
    Field: TGround;
  end;

{$IF SizeOf(TCheckAlignmentForTGround) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TGround n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDecorativeEffect = record
    Dummy: Byte;
    Field: TDecorativeEffect;
  end;

{$IF SizeOf(TCheckAlignmentForTDecorativeEffect) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDecorativeEffect n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTCounterEffectPlayerData = record
    Dummy: Byte;
    Field: TCounterEffectPlayerData;
  end;

{$IF SizeOf(TCheckAlignmentForTCounterEffectPlayerData) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TCounterEffectPlayerData n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTCounterEffect = record
    Dummy: Byte;
    Field: TCounterEffect;
  end;

{$IF SizeOf(TCheckAlignmentForTCounterEffect) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TCounterEffect n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPushButton = record
    Dummy: Byte;
    Field: TPushButton;
  end;

{$IF SizeOf(TCheckAlignmentForTPushButton) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPushButton n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTSwitch = record
    Dummy: Byte;
    Field: TSwitch;
  end;

{$IF SizeOf(TCheckAlignmentForTSwitch) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TSwitch n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDecorativeObstacle = record
    Dummy: Byte;
    Field: TDecorativeObstacle;
  end;

{$IF SizeOf(TCheckAlignmentForTDecorativeObstacle) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDecorativeObstacle n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTObjectTool = record
    Dummy: Byte;
    Field: TObjectTool;
  end;

{$IF SizeOf(TCheckAlignmentForTObjectTool) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TObjectTool n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTOverriddenSquare = record
    Dummy: Byte;
    Field: TOverriddenSquare;
  end;

{$IF SizeOf(TCheckAlignmentForTOverriddenSquare) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TOverriddenSquare n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;Generics;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TGround, 52, 52);
  CheckInstanceSize(TDecorativeEffect, 48, 48);
  CheckInstanceSize(TCounterEffectPlayerData, 20, 16);
  CheckInstanceSize(TCounterEffect, 52, 48);
  CheckInstanceSize(TPushButton, 56, 52);
  CheckInstanceSize(TSwitch, 64, 52);
  CheckInstanceSize(TDecorativeObstacle, 48, 48);
  CheckInstanceSize(TObjectTool, 64, 48);
  CheckInstanceSize(TOverriddenSquare, 88, 68);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('Generics', ImportUnit);
finalization
  SepiUnregisterImportedUnit('Generics');
end.

