{*
  Importe l'unité FLBShowMessage dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBShowMessage;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBShowMessage;

var
  SepiImportsFLBShowMessageLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBShowMessage';
  ResourceName = 'SepiImportsFLBShowMessage';
  TypeCount = 3;
  MethodCount = 3;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTDefaultShowMessagePluginPlayerData = class(TDefaultShowMessagePluginPlayerData)
  private
    class procedure InitMethodAddresses;
  end;

{--------------------------------------------}
{ TDefaultShowMessagePluginPlayerData import }
{--------------------------------------------}

class procedure TSepiImportsTDefaultShowMessagePluginPlayerData.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTDefaultShowMessagePluginPlayerData.Activate;
  MethodAddresses[1] := @TSepiImportsTDefaultShowMessagePluginPlayerData.NextLines;
  MethodAddresses[2] := @TSepiImportsTDefaultShowMessagePluginPlayerData.Deactivate;
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
      SepiImportsFLBShowMessageLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBShowMessageLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TCustomShowMessagePlugin);
  TypeInfoArray[1] := TypeInfo(TDefaultShowMessagePluginPlayerData);
  TypeInfoArray[2] := TypeInfo(TDefaultShowMessagePlugin);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTDefaultShowMessagePluginPlayerData.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTCustomShowMessagePlugin = record
    Dummy: Byte;
    Field: TCustomShowMessagePlugin;
  end;

{$IF SizeOf(TCheckAlignmentForTCustomShowMessagePlugin) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TCustomShowMessagePlugin n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDefaultShowMessagePluginPlayerData = record
    Dummy: Byte;
    Field: TDefaultShowMessagePluginPlayerData;
  end;

{$IF SizeOf(TCheckAlignmentForTDefaultShowMessagePluginPlayerData) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDefaultShowMessagePluginPlayerData n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDefaultShowMessagePlugin = record
    Dummy: Byte;
    Field: TDefaultShowMessagePlugin;
  end;

{$IF SizeOf(TCheckAlignmentForTDefaultShowMessagePlugin) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDefaultShowMessagePlugin n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FLBShowMessage;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TCustomShowMessagePlugin, 44, 44);
  CheckInstanceSize(TDefaultShowMessagePluginPlayerData, 100, 16);
  CheckInstanceSize(TDefaultShowMessagePlugin, 44, 44);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBShowMessage', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBShowMessage');
end.

