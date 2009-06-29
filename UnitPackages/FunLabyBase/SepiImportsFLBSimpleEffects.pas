{*
  Importe l'unité FLBSimpleEffects dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBSimpleEffects;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBSimpleEffects;

var
  SepiImportsFLBSimpleEffectsLazyLoad: Boolean = False;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBSimpleEffects';
  ResourceName = 'SepiImportsFLBSimpleEffects';
  TypeCount = 8;
  MethodCount = 7;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTArrow = class(TArrow)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTTransporter = class(TTransporter)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTStairs = class(TStairs)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTDirectTurnstile = class(TDirectTurnstile)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTIndirectTurnstile = class(TIndirectTurnstile)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTOutside = class(TOutside)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTTreasure = class(TTreasure)
  private
    class procedure InitMethodAddresses;
  end;

{---------------}
{ TArrow import }
{---------------}

class procedure TSepiImportsTArrow.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTArrow.Create;
end;

{---------------------}
{ TTransporter import }
{---------------------}

class procedure TSepiImportsTTransporter.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTTransporter.Create;
end;

{----------------}
{ TStairs import }
{----------------}

class procedure TSepiImportsTStairs.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTStairs.Create;
end;

{-------------------------}
{ TDirectTurnstile import }
{-------------------------}

class procedure TSepiImportsTDirectTurnstile.InitMethodAddresses;
begin
  MethodAddresses[3] := @TSepiImportsTDirectTurnstile.Create;
end;

{---------------------------}
{ TIndirectTurnstile import }
{---------------------------}

class procedure TSepiImportsTIndirectTurnstile.InitMethodAddresses;
begin
  MethodAddresses[4] := @TSepiImportsTIndirectTurnstile.Create;
end;

{-----------------}
{ TOutside import }
{-----------------}

class procedure TSepiImportsTOutside.InitMethodAddresses;
begin
  MethodAddresses[5] := @TSepiImportsTOutside.Create;
end;

{------------------}
{ TTreasure import }
{------------------}

class procedure TSepiImportsTTreasure.InitMethodAddresses;
begin
  MethodAddresses[6] := @TSepiImportsTTreasure.Create;
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
      SepiImportsFLBSimpleEffectsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBSimpleEffectsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TTransporterKind);
  TypeInfoArray[1] := TypeInfo(TArrow);
  TypeInfoArray[2] := TypeInfo(TTransporter);
  TypeInfoArray[3] := TypeInfo(TStairs);
  TypeInfoArray[4] := TypeInfo(TDirectTurnstile);
  TypeInfoArray[5] := TypeInfo(TIndirectTurnstile);
  TypeInfoArray[6] := TypeInfo(TOutside);
  TypeInfoArray[7] := TypeInfo(TTreasure);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTArrow.InitMethodAddresses;
  TSepiImportsTTransporter.InitMethodAddresses;
  TSepiImportsTStairs.InitMethodAddresses;
  TSepiImportsTDirectTurnstile.InitMethodAddresses;
  TSepiImportsTIndirectTurnstile.InitMethodAddresses;
  TSepiImportsTOutside.InitMethodAddresses;
  TSepiImportsTTreasure.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBSimpleEffects', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBSimpleEffects');
end.

