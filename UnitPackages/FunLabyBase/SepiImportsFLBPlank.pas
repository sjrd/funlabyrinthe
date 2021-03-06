{*
  Importe l'unit� FLBPlank dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFLBPlank;

interface

uses
  Windows, SysUtils, TypInfo, SepiReflectionCore, SepiMembers, Classes, 
  FLBPlank;

var
  SepiImportsFLBPlankLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FLBPlank';
  ResourceName = 'SepiImportsFLBPlank';
  TypeCount = 4;
  MethodCount = 2;
  VariableCount = 4;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTPlanks = class(TPlanks)
  private
    procedure SetRequiredShift(Value: TShiftState);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTPlankSquare = class(TPlankSquare)
  private
    class procedure InitMethodAddresses;
  end;

{----------------}
{ TPlanks import }
{----------------}

procedure TSepiImportsTPlanks.SetRequiredShift(Value: TShiftState);
begin
  RequiredShift := Value;
end;

class procedure TSepiImportsTPlanks.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTPlanks.SetRequiredShift;
end;

{---------------------}
{ TPlankSquare import }
{---------------------}

class procedure TSepiImportsTPlankSquare.InitMethodAddresses;
begin
  MethodAddresses[1] := @TSepiImportsTPlankSquare.Create;
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
      SepiImportsFLBPlankLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFLBPlankLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TPlankPlugin);
  TypeInfoArray[1] := TypeInfo(TPlanks);
  TypeInfoArray[2] := TypeInfo(TPlankTool);
  TypeInfoArray[3] := TypeInfo(TPlankSquare);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTPlanks.InitMethodAddresses;
  TSepiImportsTPlankSquare.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
  VarAddresses[0] := @compPlankPlugin;
  VarAddresses[1] := @compPlanks;
  VarAddresses[2] := @compPlank;
  VarAddresses[3] := @attrtypeUsePlank;
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTPlankPlugin = record
    Dummy: Byte;
    Field: TPlankPlugin;
  end;

{$IF SizeOf(TCheckAlignmentForTPlankPlugin) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlankPlugin n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlanks = record
    Dummy: Byte;
    Field: TPlanks;
  end;

{$IF SizeOf(TCheckAlignmentForTPlanks) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlanks n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlankTool = record
    Dummy: Byte;
    Field: TPlankTool;
  end;

{$IF SizeOf(TCheckAlignmentForTPlankTool) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlankTool n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPlankSquare = record
    Dummy: Byte;
    Field: TPlankSquare;
  end;

{$IF SizeOf(TCheckAlignmentForTPlankSquare) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPlankSquare n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if AClass.InstanceSize = SepiInstSize then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FLBPlank;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TPlankPlugin, 56, 52);
  CheckInstanceSize(TPlanks, 68, 64);
  CheckInstanceSize(TPlankTool, 76, 76);
  CheckInstanceSize(TPlankSquare, 104, 100);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FLBPlank', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FLBPlank');
end.

