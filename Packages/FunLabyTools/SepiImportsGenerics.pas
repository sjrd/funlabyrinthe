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
  TypeCount = 8;
  MethodCount = 10;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
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

  TSepiImportsTPushButton = class(TPushButton)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTSwitch = class(TSwitch)
  private
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

{--------------------------}
{ TDecorativeEffect import }
{--------------------------}

class procedure TSepiImportsTDecorativeEffect.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTDecorativeEffect.Create;
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
  MethodAddresses[1] := @TSepiImportsTCounterEffect.GetCounter;
  MethodAddresses[2] := @TSepiImportsTCounterEffect.SetCounter;
  MethodAddresses[3] := @TSepiImportsTCounterEffect.IncCounter;
  MethodAddresses[4] := @TSepiImportsTCounterEffect.IsFirstTime;
end;

{--------------------}
{ TPushButton import }
{--------------------}

class procedure TSepiImportsTPushButton.InitMethodAddresses;
begin
  MethodAddresses[5] := @TSepiImportsTPushButton.Create;
end;

{----------------}
{ TSwitch import }
{----------------}

class procedure TSepiImportsTSwitch.InitMethodAddresses;
begin
  MethodAddresses[6] := @TSepiImportsTSwitch.Create;
end;

{----------------------------}
{ TDecorativeObstacle import }
{----------------------------}

class procedure TSepiImportsTDecorativeObstacle.InitMethodAddresses;
begin
  MethodAddresses[7] := @TSepiImportsTDecorativeObstacle.Create;
end;

{--------------------}
{ TObjectTool import }
{--------------------}

class procedure TSepiImportsTObjectTool.InitMethodAddresses;
begin
  MethodAddresses[8] := @TSepiImportsTObjectTool.Create;
end;

{--------------------------}
{ TOverriddenSquare import }
{--------------------------}

class procedure TSepiImportsTOverriddenSquare.InitMethodAddresses;
begin
  MethodAddresses[9] := @TSepiImportsTOverriddenSquare.Create;
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
  TypeInfoArray[0] := TypeInfo(TDecorativeEffect);
  TypeInfoArray[1] := TypeInfo(TCounterEffectPlayerData);
  TypeInfoArray[2] := TypeInfo(TCounterEffect);
  TypeInfoArray[3] := TypeInfo(TPushButton);
  TypeInfoArray[4] := TypeInfo(TSwitch);
  TypeInfoArray[5] := TypeInfo(TDecorativeObstacle);
  TypeInfoArray[6] := TypeInfo(TObjectTool);
  TypeInfoArray[7] := TypeInfo(TOverriddenSquare);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDecorativeEffect.InitMethodAddresses;
  TSepiImportsTCounterEffect.InitMethodAddresses;
  TSepiImportsTPushButton.InitMethodAddresses;
  TSepiImportsTSwitch.InitMethodAddresses;
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
  CheckInstanceSize(TDecorativeEffect, 48, 48);
  CheckInstanceSize(TCounterEffectPlayerData, 20, 16);
  CheckInstanceSize(TCounterEffect, 52, 48);
  CheckInstanceSize(TPushButton, 56, 52);
  CheckInstanceSize(TSwitch, 64, 52);
  CheckInstanceSize(TDecorativeObstacle, 48, 48);
  CheckInstanceSize(TObjectTool, 56, 48);
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

