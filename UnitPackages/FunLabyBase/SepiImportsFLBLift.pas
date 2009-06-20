{*
  Importe l'unité FLBLift dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBLift;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, FLBLift;

var
  SepiImportsFLBLiftLazyLoad: Boolean = False;

implementation

{$R *.res}

const // don't localize
  UnitName = 'FLBLift';
  ResourceName = 'SepiImportsFLBLift';
  TypeCount = 2;
  MethodCount = 2;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTEngagedLiftSquare = class(TEngagedLiftSquare)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTLift = class(TLift)
  private
    class procedure InitMethodAddresses;
  end;

{---------------------------}
{ TEngagedLiftSquare import }
{---------------------------}

class procedure TSepiImportsTEngagedLiftSquare.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTEngagedLiftSquare.Create;
end;

{--------------}
{ TLift import }
{--------------}

class procedure TSepiImportsTLift.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTLift.Create;
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
      SepiImportsFLBLiftLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBLiftLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TEngagedLiftSquare);
  TypeInfoArray[1] := TypeInfo(TLift);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTEngagedLiftSquare.InitMethodAddresses;
  TSepiImportsTLift.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBLift', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBLift');
end.

