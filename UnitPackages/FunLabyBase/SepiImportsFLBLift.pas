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

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBLift';
  ResourceName = 'SepiImportsFLBLift';
  TypeCount = 2;
  MethodCount = 1;
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

{---------------------------}
{ TEngagedLiftSquare import }
{---------------------------}

class procedure TSepiImportsTEngagedLiftSquare.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTEngagedLiftSquare.Create;
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

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TEngagedLiftSquare);
  TypeInfoArray[1] := TypeInfo(TLift);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTEngagedLiftSquare.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTEngagedLiftSquare = record
    Dummy: Byte;
    Field: TEngagedLiftSquare;
  end;

{$IF SizeOf(TCheckAlignmentForTEngagedLiftSquare) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TEngagedLiftSquare n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTLift = record
    Dummy: Byte;
    Field: TLift;
  end;

{$IF SizeOf(TCheckAlignmentForTLift) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TLift n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FLBLift;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TEngagedLiftSquare, 92, 88);
  CheckInstanceSize(TLift, 48, 48);
  {$ASSERTIONS OFF}
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

