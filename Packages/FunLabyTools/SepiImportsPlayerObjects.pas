{*
  Importe l'unité PlayerObjects dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsPlayerObjects;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  PlayerObjects;

var
  SepiImportsPlayerObjectsLazyLoad: Boolean = False;

implementation

{$R *.res}

const // don't localize
  UnitName = 'PlayerObjects';
  ResourceName = 'SepiImportsPlayerObjects';
  TypeCount = 1;
  MethodCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;

type
  TSepiImportsTFormObjects = class(TFormObjects)
  private
    class procedure InitMethodAddresses;
  end;

{---------------------}
{ TFormObjects import }
{---------------------}

class procedure TSepiImportsTFormObjects.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTFormObjects.ShowObjects;
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
      SepiImportsPlayerObjectsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent));

    if SepiImportsPlayerObjectsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TFormObjects);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTFormObjects.InitMethodAddresses;
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;

  SepiRegisterImportedUnit('PlayerObjects', ImportUnit);
end.

