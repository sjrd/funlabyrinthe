{*
  Importe l'unit� PlayerObjects dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsPlayerObjects;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FunLabyUtils, PlayerObjects;

var
  SepiImportsPlayerObjectsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'PlayerObjects';
  ResourceName = 'SepiImportsPlayerObjects';
  TypeCount = 3;
  MethodCount = 5;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTFormObjects = class(TFormObjects)
  private
    procedure SetPlayer(Value: TPlayer);
    class procedure InitMethodAddresses;
  end;

{---------------------}
{ TFormObjects import }
{---------------------}

procedure TSepiImportsTFormObjects.SetPlayer(Value: TPlayer);
begin
  Player := Value;
end;

class procedure TSepiImportsTFormObjects.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTFormObjects.FormShow;
  MethodAddresses[1] := @TSepiImportsTFormObjects.ListViewObjectsCustomDrawItem;
  MethodAddresses[2] := @TSepiImportsTFormObjects.ButtonCloseClick;
  MethodAddresses[3] := @TSepiImportsTFormObjects.SetPlayer;
  MethodAddresses[4] := @TSepiImportsTFormObjects.UpdateObjects;
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
      SepiImportsPlayerObjectsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsPlayerObjectsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TObjectDefData);
  TypeInfoArray[1] := TypeInfo(TObjectDefDataList);
  TypeInfoArray[2] := TypeInfo(TFormObjects);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTFormObjects.InitMethodAddresses;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

{$IF SizeOf(TObjectDefData) <> 8}
  {$MESSAGE WARN 'Le type TObjectDefData n''a pas la taille calcul�e par Sepi'}
{$ELSE}

type
  TCheckAlignmentForTObjectDefData = record
    Dummy: Byte;
    Field: TObjectDefData;
  end;

{$IF SizeOf(TCheckAlignmentForTObjectDefData) <> (4 + 8)}
  {$MESSAGE WARN 'Le type TObjectDefData n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

{$IFEND}

type
  TCheckAlignmentForTObjectDefDataList = record
    Dummy: Byte;
    Field: TObjectDefDataList;
  end;

{$IF SizeOf(TCheckAlignmentForTObjectDefDataList) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TObjectDefDataList n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTFormObjects = record
    Dummy: Byte;
    Field: TFormObjects;
  end;

{$IF SizeOf(TCheckAlignmentForTFormObjects) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFormObjects n''a pas l''alignement calcul� par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if AClass.InstanceSize = SepiInstSize then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;PlayerObjects;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TFormObjects, 924, 892);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('PlayerObjects', ImportUnit);
finalization
  SepiUnregisterImportedUnit('PlayerObjects');
end.

