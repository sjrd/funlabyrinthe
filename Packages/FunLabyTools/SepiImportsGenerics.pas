{*
  Importe l'unité Generics dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsGenerics;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  Generics;

var
  SepiImportsGenericsLazyLoad: Boolean = False;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'Generics';
  ResourceName = 'SepiImportsGenerics';
  TypeCount = 4;
  MethodCount = 4;
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

{----------------------------}
{ TDecorativeObstacle import }
{----------------------------}

class procedure TSepiImportsTDecorativeObstacle.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTDecorativeObstacle.Create;
end;

{--------------------}
{ TObjectTool import }
{--------------------}

class procedure TSepiImportsTObjectTool.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTObjectTool.Create;
end;

{--------------------------}
{ TOverriddenSquare import }
{--------------------------}

class procedure TSepiImportsTOverriddenSquare.InitMethodAddresses;
begin
  MethodAddresses[3] := @TSepiImportsTOverriddenSquare.Create;
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
  TypeInfoArray[1] := TypeInfo(TDecorativeObstacle);
  TypeInfoArray[2] := TypeInfo(TObjectTool);
  TypeInfoArray[3] := TypeInfo(TOverriddenSquare);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDecorativeEffect.InitMethodAddresses;
  TSepiImportsTDecorativeObstacle.InitMethodAddresses;
  TSepiImportsTObjectTool.InitMethodAddresses;
  TSepiImportsTOverriddenSquare.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
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

