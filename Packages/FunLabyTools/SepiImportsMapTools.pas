{*
  Importe l'unité MapTools dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMapTools;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FunLabyUtils, MapTools;

var
  SepiImportsMapToolsLazyLoad: Boolean = False;

implementation

{$R *.res}

const // don't localize
  UnitName = 'MapTools';
  ResourceName = 'SepiImportsMapTools';
  TypeCount = 1;
  MethodCount = 9;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

{---------------------}
{ Overloaded routines }
{---------------------}

function ChangeComp_0(Square: TSquare; NewComp: TSquareComponent): TSquare;
begin
  Result := ChangeComp(Square, NewComp);
end;

function ChangeComp_1(Square: TSquare; const NewComp: TComponentID): TSquare;
begin
  Result := ChangeComp(Square, NewComp);
end;

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
      SepiImportsMapToolsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsMapToolsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
end;

procedure InitMethodAddresses;
begin
  MethodAddresses[0] := @ChangeField;
  MethodAddresses[1] := @ChangeEffect;
  MethodAddresses[2] := @ChangeTool;
  MethodAddresses[3] := @ChangeObstacle;
  MethodAddresses[4] := @ChangeComp_0;
  MethodAddresses[5] := @ChangeComp_1;
  MethodAddresses[6] := @FindNextSquare;
  MethodAddresses[7] := @FindPreviousSquare;
  MethodAddresses[8] := @FindSquareAtRandom;
end;

procedure InitVarAddresses;
begin
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('MapTools', ImportUnit);
end.

