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

const // don't localize
  UnitName = 'Generics';
  ResourceName = 'SepiImportsGenerics';
  TypeCount = 3;
  MethodCount = 3;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;

type
  TSepiImportsTDecorativeEffect = class(TDecorativeEffect)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTObjectTool = class(TObjectTool)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTOverriddenScrew = class(TOverriddenScrew)
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

{--------------------}
{ TObjectTool import }
{--------------------}

class procedure TSepiImportsTObjectTool.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTObjectTool.Create;
end;

{-------------------------}
{ TOverriddenScrew import }
{-------------------------}

class procedure TSepiImportsTOverriddenScrew.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTOverriddenScrew.Create;
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

const
  GetMethodCodeEvent: TMethod = (Code: @GetMethodCode; Data: nil);
  GetTypeInfoEvent: TMethod = (Code: @GetTypeInfo; Data: nil);

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
      TGetTypeInfoEvent(GetTypeInfoEvent));

    if SepiImportsGenericsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TDecorativeEffect);
  TypeInfoArray[1] := TypeInfo(TObjectTool);
  TypeInfoArray[2] := TypeInfo(TOverriddenScrew);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDecorativeEffect.InitMethodAddresses;
  TSepiImportsTObjectTool.InitMethodAddresses;
  TSepiImportsTOverriddenScrew.InitMethodAddresses;
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;

  SepiRegisterImportedUnit('Generics', ImportUnit);
end.

