{*
  Importe l'unit� FLBSimpleObjects dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBSimpleObjects;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBSimpleObjects;

var
  SepiImportsFLBSimpleObjectsLazyLoad: Boolean = False;

implementation

{$R *.res}

const // don't localize
  UnitName = 'FLBSimpleObjects';
  ResourceName = 'SepiImportsFLBSimpleObjects';
  TypeCount = 4;
  MethodCount = 3;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTBuoys = class(TBuoys)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTSilverKeys = class(TSilverKeys)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTGoldenKeys = class(TGoldenKeys)
  private
    class procedure InitMethodAddresses;
  end;

{---------------}
{ TBuoys import }
{---------------}

class procedure TSepiImportsTBuoys.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTBuoys.Create;
end;

{--------------------}
{ TSilverKeys import }
{--------------------}

class procedure TSepiImportsTSilverKeys.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTSilverKeys.Create;
end;

{--------------------}
{ TGoldenKeys import }
{--------------------}

class procedure TSepiImportsTGoldenKeys.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTGoldenKeys.Create;
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
      SepiImportsFLBSimpleObjectsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBSimpleObjectsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TBuoyPlugin);
  TypeInfoArray[1] := TypeInfo(TBuoys);
  TypeInfoArray[2] := TypeInfo(TSilverKeys);
  TypeInfoArray[3] := TypeInfo(TGoldenKeys);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTBuoys.InitMethodAddresses;
  TSepiImportsTSilverKeys.InitMethodAddresses;
  TSepiImportsTGoldenKeys.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBSimpleObjects', ImportUnit);
end.

