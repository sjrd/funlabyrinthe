{*
  Importe l'unité FLBObstacles dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBObstacles;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBObstacles;

var
  SepiImportsFLBObstaclesLazyLoad: Boolean = False;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBObstacles';
  ResourceName = 'SepiImportsFLBObstacles';
  TypeCount = 3;
  MethodCount = 3;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTSilverBlock = class(TSilverBlock)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTGoldenBlock = class(TGoldenBlock)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTSecretWay = class(TSecretWay)
  private
    class procedure InitMethodAddresses;
  end;

{---------------------}
{ TSilverBlock import }
{---------------------}

class procedure TSepiImportsTSilverBlock.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTSilverBlock.Create;
end;

{---------------------}
{ TGoldenBlock import }
{---------------------}

class procedure TSepiImportsTGoldenBlock.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTGoldenBlock.Create;
end;

{-------------------}
{ TSecretWay import }
{-------------------}

class procedure TSepiImportsTSecretWay.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTSecretWay.Create;
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
      SepiImportsFLBObstaclesLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBObstaclesLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TSilverBlock);
  TypeInfoArray[1] := TypeInfo(TGoldenBlock);
  TypeInfoArray[2] := TypeInfo(TSecretWay);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTSilverBlock.InitMethodAddresses;
  TSepiImportsTGoldenBlock.InitMethodAddresses;
  TSepiImportsTSecretWay.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBObstacles', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBObstacles');
end.

