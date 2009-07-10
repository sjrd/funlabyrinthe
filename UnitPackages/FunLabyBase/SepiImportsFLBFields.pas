{*
  Importe l'unité FLBFields dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBFields;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBFields;

var
  SepiImportsFLBFieldsLazyLoad: Boolean = False;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBFields';
  ResourceName = 'SepiImportsFLBFields';
  TypeCount = 6;
  MethodCount = 6;
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

  TSepiImportsTWall = class(TWall)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTWater = class(TWater)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTHole = class(THole)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTSky = class(TSky)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTOutside = class(TOutside)
  private
    class procedure InitMethodAddresses;
  end;

{----------------}
{ TGround import }
{----------------}

class procedure TSepiImportsTGround.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTGround.Create;
end;

{--------------}
{ TWall import }
{--------------}

class procedure TSepiImportsTWall.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTWall.Create;
end;

{---------------}
{ TWater import }
{---------------}

class procedure TSepiImportsTWater.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTWater.Create;
end;

{--------------}
{ THole import }
{--------------}

class procedure TSepiImportsTHole.InitMethodAddresses;
begin
  MethodAddresses[3] := @TSepiImportsTHole.Create;
end;

{-------------}
{ TSky import }
{-------------}

class procedure TSepiImportsTSky.InitMethodAddresses;
begin
  MethodAddresses[4] := @TSepiImportsTSky.Create;
end;

{-----------------}
{ TOutside import }
{-----------------}

class procedure TSepiImportsTOutside.InitMethodAddresses;
begin
  MethodAddresses[5] := @TSepiImportsTOutside.Create;
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
      SepiImportsFLBFieldsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBFieldsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TGround);
  TypeInfoArray[1] := TypeInfo(TWall);
  TypeInfoArray[2] := TypeInfo(TWater);
  TypeInfoArray[3] := TypeInfo(THole);
  TypeInfoArray[4] := TypeInfo(TSky);
  TypeInfoArray[5] := TypeInfo(TOutside);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTGround.InitMethodAddresses;
  TSepiImportsTWall.InitMethodAddresses;
  TSepiImportsTWater.InitMethodAddresses;
  TSepiImportsTHole.InitMethodAddresses;
  TSepiImportsTSky.InitMethodAddresses;
  TSepiImportsTOutside.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBFields', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBFields');
end.

