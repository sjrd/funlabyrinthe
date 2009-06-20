{*
  Importe l'unité FLBPlank dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBPlank;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBPlank;

var
  SepiImportsFLBPlankLazyLoad: Boolean = False;

implementation

{$R *.res}

const // don't localize
  UnitName = 'FLBPlank';
  ResourceName = 'SepiImportsFLBPlank';
  TypeCount = 3;
  MethodCount = 2;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTPlanks = class(TPlanks)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTPlankSquare = class(TPlankSquare)
  private
    class procedure InitMethodAddresses;
  end;

{----------------}
{ TPlanks import }
{----------------}

class procedure TSepiImportsTPlanks.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTPlanks.Create;
end;

{---------------------}
{ TPlankSquare import }
{---------------------}

class procedure TSepiImportsTPlankSquare.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTPlankSquare.Create;
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
      SepiImportsFLBPlankLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBPlankLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TPlankPlugin);
  TypeInfoArray[1] := TypeInfo(TPlanks);
  TypeInfoArray[2] := TypeInfo(TPlankSquare);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTPlanks.InitMethodAddresses;
  TSepiImportsTPlankSquare.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBPlank', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBPlank');
end.

