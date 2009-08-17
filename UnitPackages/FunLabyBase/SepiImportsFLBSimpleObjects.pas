{*
  Importe l'unité FLBSimpleObjects dans un environnement Sepi
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

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBSimpleObjects';
  ResourceName = 'SepiImportsFLBSimpleObjects';
  TypeCount = 4;
  MethodCount = 1;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

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

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TBuoyPlugin);
  TypeInfoArray[1] := TypeInfo(TBuoys);
  TypeInfoArray[2] := TypeInfo(TSilverKeys);
  TypeInfoArray[3] := TypeInfo(TGoldenKeys);
end;

procedure InitMethodAddresses;
begin
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTBuoyPlugin = record
    Dummy: Byte;
    Field: TBuoyPlugin;
  end;

{$IF SizeOf(TCheckAlignmentForTBuoyPlugin) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBuoyPlugin n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTBuoys = record
    Dummy: Byte;
    Field: TBuoys;
  end;

{$IF SizeOf(TCheckAlignmentForTBuoys) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBuoys n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTSilverKeys = record
    Dummy: Byte;
    Field: TSilverKeys;
  end;

{$IF SizeOf(TCheckAlignmentForTSilverKeys) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TSilverKeys n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTGoldenKeys = record
    Dummy: Byte;
    Field: TGoldenKeys;
  end;

{$IF SizeOf(TCheckAlignmentForTGoldenKeys) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TGoldenKeys n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FLBSimpleObjects;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TBuoyPlugin, 40, 40);
  CheckInstanceSize(TBuoys, 52, 52);
  CheckInstanceSize(TSilverKeys, 52, 52);
  CheckInstanceSize(TGoldenKeys, 52, 52);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBSimpleObjects', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBSimpleObjects');
end.

