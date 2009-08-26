{*
  Importe l'unité FLBBoat dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBBoat;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, FLBBoat;

var
  SepiImportsFLBBoatLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBBoat';
  ResourceName = 'SepiImportsFLBBoat';
  TypeCount = 5;
  MethodCount = 2;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTBoatPlugin = class(TBoatPlugin)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTBoat = class(TBoat)
  private
    class procedure InitMethodAddresses;
  end;

{--------------------}
{ TBoatPlugin import }
{--------------------}

class procedure TSepiImportsTBoatPlugin.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTBoatPlugin.GetUsedBoat;
end;

{--------------}
{ TBoat import }
{--------------}

class procedure TSepiImportsTBoat.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTBoat.ToUnderBoatWater;
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
      SepiImportsFLBBoatLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBBoatLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TBoatPluginPlayerData);
  TypeInfoArray[1] := TypeInfo(TBoatPlugin);
  TypeInfoArray[2] := TypeInfo(TBoat);
  TypeInfoArray[3] := TypeInfo(TBoatCreator);
  TypeInfoArray[4] := TypeInfo(TUnderBoatCreator);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTBoatPlugin.InitMethodAddresses;
  TSepiImportsTBoat.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTBoatPluginPlayerData = record
    Dummy: Byte;
    Field: TBoatPluginPlayerData;
  end;

{$IF SizeOf(TCheckAlignmentForTBoatPluginPlayerData) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBoatPluginPlayerData n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTBoatPlugin = record
    Dummy: Byte;
    Field: TBoatPlugin;
  end;

{$IF SizeOf(TCheckAlignmentForTBoatPlugin) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBoatPlugin n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTBoat = record
    Dummy: Byte;
    Field: TBoat;
  end;

{$IF SizeOf(TCheckAlignmentForTBoat) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBoat n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTBoatCreator = record
    Dummy: Byte;
    Field: TBoatCreator;
  end;

{$IF SizeOf(TCheckAlignmentForTBoatCreator) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBoatCreator n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTUnderBoatCreator = record
    Dummy: Byte;
    Field: TUnderBoatCreator;
  end;

{$IF SizeOf(TCheckAlignmentForTUnderBoatCreator) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TUnderBoatCreator n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FLBBoat;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TBoatPluginPlayerData, 20, 16);
  CheckInstanceSize(TBoatPlugin, 40, 40);
  CheckInstanceSize(TBoat, 48, 48);
  CheckInstanceSize(TBoatCreator, 36, 36);
  CheckInstanceSize(TUnderBoatCreator, 36, 36);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBBoat', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBBoat');
end.

