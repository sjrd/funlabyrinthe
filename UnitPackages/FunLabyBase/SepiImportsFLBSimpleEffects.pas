{*
  Importe l'unité FLBSimpleEffects dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBSimpleEffects;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FLBSimpleEffects;

var
  SepiImportsFLBSimpleEffectsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBSimpleEffects';
  ResourceName = 'SepiImportsFLBSimpleEffects';
  TypeCount = 9;
  MethodCount = 2;
  VariableCount = 13;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTArrow = class(TArrow)
  private
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTStairs = class(TStairs)
  private
    class procedure InitMethodAddresses;
  end;

{---------------}
{ TArrow import }
{---------------}

class procedure TSepiImportsTArrow.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTArrow.CreateArrow;
end;

{----------------}
{ TStairs import }
{----------------}

class procedure TSepiImportsTStairs.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTStairs.CreateStairs;
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
      SepiImportsFLBSimpleEffectsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBSimpleEffectsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TTransporterKind);
  TypeInfoArray[1] := TypeInfo(TArrow);
  TypeInfoArray[2] := TypeInfo(TTransporter);
  TypeInfoArray[3] := TypeInfo(TInactiveTransporter);
  TypeInfoArray[4] := TypeInfo(TTransporterCreator);
  TypeInfoArray[5] := TypeInfo(TStairs);
  TypeInfoArray[6] := TypeInfo(TDirectTurnstile);
  TypeInfoArray[7] := TypeInfo(TIndirectTurnstile);
  TypeInfoArray[8] := TypeInfo(TTreasure);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTArrow.InitMethodAddresses;
  TSepiImportsTStairs.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
  VarAddresses[0] := @compNorthArrow;
  VarAddresses[1] := @compEastArrow;
  VarAddresses[2] := @compSouthArrow;
  VarAddresses[3] := @compWestArrow;
  VarAddresses[4] := @compCrossroads;
  VarAddresses[5] := @compInactiveTransporter;
  VarAddresses[6] := @compTransporterCreator;
  VarAddresses[7] := @compUpStairs;
  VarAddresses[8] := @compDownStairs;
  VarAddresses[9] := @compDirectTurnstile;
  VarAddresses[10] := @compIndirectTurnstile;
  VarAddresses[11] := @compTreasure;
  VarAddresses[12] := @compSunkenButton;
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTTransporterKind = record
    Dummy: Byte;
    Field: TTransporterKind;
  end;

{$IF SizeOf(TCheckAlignmentForTTransporterKind) <> (1 + 1)}
  {$MESSAGE WARN 'Le type TTransporterKind n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTArrow = record
    Dummy: Byte;
    Field: TArrow;
  end;

{$IF SizeOf(TCheckAlignmentForTArrow) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TArrow n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTTransporter = record
    Dummy: Byte;
    Field: TTransporter;
  end;

{$IF SizeOf(TCheckAlignmentForTTransporter) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TTransporter n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTInactiveTransporter = record
    Dummy: Byte;
    Field: TInactiveTransporter;
  end;

{$IF SizeOf(TCheckAlignmentForTInactiveTransporter) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TInactiveTransporter n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTTransporterCreator = record
    Dummy: Byte;
    Field: TTransporterCreator;
  end;

{$IF SizeOf(TCheckAlignmentForTTransporterCreator) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TTransporterCreator n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTStairs = record
    Dummy: Byte;
    Field: TStairs;
  end;

{$IF SizeOf(TCheckAlignmentForTStairs) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TStairs n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTDirectTurnstile = record
    Dummy: Byte;
    Field: TDirectTurnstile;
  end;

{$IF SizeOf(TCheckAlignmentForTDirectTurnstile) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TDirectTurnstile n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTIndirectTurnstile = record
    Dummy: Byte;
    Field: TIndirectTurnstile;
  end;

{$IF SizeOf(TCheckAlignmentForTIndirectTurnstile) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TIndirectTurnstile n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTTreasure = record
    Dummy: Byte;
    Field: TTreasure;
  end;

{$IF SizeOf(TCheckAlignmentForTTreasure) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TTreasure n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FLBSimpleEffects;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TArrow, 60, 56);
  CheckInstanceSize(TTransporter, 64, 56);
  CheckInstanceSize(TInactiveTransporter, 64, 64);
  CheckInstanceSize(TTransporterCreator, 36, 36);
  CheckInstanceSize(TStairs, 60, 56);
  CheckInstanceSize(TDirectTurnstile, 56, 56);
  CheckInstanceSize(TIndirectTurnstile, 56, 56);
  CheckInstanceSize(TTreasure, 56, 56);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBSimpleEffects', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBSimpleEffects');
end.

