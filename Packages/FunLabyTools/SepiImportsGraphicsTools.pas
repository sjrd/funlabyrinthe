{*
  Importe l'unit� GraphicsTools dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsGraphicsTools;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FunLabyUtils, GraphicsTools;

var
  SepiImportsGraphicsToolsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'GraphicsTools';
  ResourceName = 'SepiImportsGraphicsTools';
  TypeCount = 2;
  MethodCount = 6;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

{---------------------}
{ Overloaded routines }
{---------------------}

procedure DissipateNeighbors_0(Context: TDrawSquareContext; const Predicate: TFieldPredicate);
begin
  DissipateNeighbors(Context, Predicate);
end;

procedure DissipateNeighbors_1(Context: TDrawSquareContext);
begin
  DissipateNeighbors(Context);
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
      SepiImportsGraphicsToolsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsGraphicsToolsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TFieldPredicate);
  TypeInfoArray[1] := TypeInfo(TAvoidInfiniteRecursionBitmap32);
end;

procedure InitMethodAddresses;
begin
  MethodAddresses[0] := @DrawSquareText;
  MethodAddresses[1] := @DrawSquareNumber;
  MethodAddresses[2] := @DissipateNeighbors_0;
  MethodAddresses[3] := @DissipateNeighbors_1;
  MethodAddresses[4] := @DissipateGroundNeighbors;
  MethodAddresses[5] := @CleanRectAlpha;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTFieldPredicate = record
    Dummy: Byte;
    Field: TFieldPredicate;
  end;

{$IF SizeOf(TCheckAlignmentForTFieldPredicate) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFieldPredicate n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTAvoidInfiniteRecursionBitmap32 = record
    Dummy: Byte;
    Field: TAvoidInfiniteRecursionBitmap32;
  end;

{$IF SizeOf(TCheckAlignmentForTAvoidInfiniteRecursionBitmap32) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TAvoidInfiniteRecursionBitmap32 n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if AClass.InstanceSize = SepiInstSize then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;GraphicsTools;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TAvoidInfiniteRecursionBitmap32, 276, 276);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('GraphicsTools', ImportUnit);
finalization
  SepiUnregisterImportedUnit('GraphicsTools');
end.

