{*
  Importe l'unité MapTools dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsMapTools;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, 
  FunLabyUtils, MapTools;

var
  SepiImportsMapToolsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'MapTools';
  ResourceName = 'SepiImportsMapTools';
  TypeCount = 1;
  MethodCount = 21;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

{---------------------}
{ Overloaded routines }
{---------------------}

function ChangeField_0(Square: TSquare; NewField: TField): TSquare;
begin
  Result := ChangeField(Square, NewField);
end;

function ChangeField_1(Square: TSquare; const NewField: TComponentID): TSquare;
begin
  Result := ChangeField(Square, NewField);
end;

function ChangeEffect_0(Square: TSquare; NewEffect: TEffect): TSquare;
begin
  Result := ChangeEffect(Square, NewEffect);
end;

function ChangeEffect_1(Square: TSquare; const NewEffect: TComponentID): TSquare;
begin
  Result := ChangeEffect(Square, NewEffect);
end;

function ChangeTool_0(Square: TSquare; NewTool: TTool): TSquare;
begin
  Result := ChangeTool(Square, NewTool);
end;

function ChangeTool_1(Square: TSquare; const NewTool: TComponentID): TSquare;
begin
  Result := ChangeTool(Square, NewTool);
end;

function ChangeObstacle_0(Square: TSquare; NewObstacle: TObstacle): TSquare;
begin
  Result := ChangeObstacle(Square, NewObstacle);
end;

function ChangeObstacle_1(Square: TSquare; const NewObstacle: TComponentID): TSquare;
begin
  Result := ChangeObstacle(Square, NewObstacle);
end;

function ChangeComp_0(Square: TSquare; NewComp: TSquareComponent): TSquare;
begin
  Result := ChangeComp(Square, NewComp);
end;

function ChangeComp_1(Square: TSquare; const NewComp: TComponentID): TSquare;
begin
  Result := ChangeComp(Square, NewComp);
end;

function IsAnyPosComponent_0(const QPos: TQualifiedPos; const Predicate: TPosComponentPredicate): Boolean;
begin
  Result := IsAnyPosComponent(QPos, Predicate);
end;

function IsAnyPosComponent_1(const QPos: TQualifiedPos): Boolean;
begin
  Result := IsAnyPosComponent(QPos);
end;

function IsAnyPosComponent_2(const QPos: TQualifiedPos; ComponentClass: TPosComponentClass): Boolean;
begin
  Result := IsAnyPosComponent(QPos, ComponentClass);
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
      SepiImportsMapToolsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsMapToolsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TPosComponentPredicate);
end;

procedure InitMethodAddresses;
begin
  MethodAddresses[0] := @ChangeField_0;
  MethodAddresses[1] := @ChangeField_1;
  MethodAddresses[2] := @ChangeEffect_0;
  MethodAddresses[3] := @ChangeEffect_1;
  MethodAddresses[4] := @RemoveEffect;
  MethodAddresses[5] := @ChangeTool_0;
  MethodAddresses[6] := @ChangeTool_1;
  MethodAddresses[7] := @RemoveTool;
  MethodAddresses[8] := @ChangeObstacle_0;
  MethodAddresses[9] := @ChangeObstacle_1;
  MethodAddresses[10] := @RemoveObstacle;
  MethodAddresses[11] := @ChangeComp_0;
  MethodAddresses[12] := @ChangeComp_1;
  MethodAddresses[13] := @MakeSquare;
  MethodAddresses[14] := @FindNextSquare;
  MethodAddresses[15] := @FindPreviousSquare;
  MethodAddresses[16] := @FindSquareAtRandom;
  MethodAddresses[17] := @IsAnyPosComponent_0;
  MethodAddresses[18] := @IsAnyPosComponent_1;
  MethodAddresses[19] := @IsAnyPosComponent_2;
  MethodAddresses[20] := @IsAnySquareModifier;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTFindSquareProc = record
    Dummy: Byte;
    Field: TFindSquareProc;
  end;

{$IF SizeOf(TCheckAlignmentForTFindSquareProc) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TFindSquareProc n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTPosComponentPredicate = record
    Dummy: Byte;
    Field: TPosComponentPredicate;
  end;

{$IF SizeOf(TCheckAlignmentForTPosComponentPredicate) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TPosComponentPredicate n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if AClass.InstanceSize = SepiInstSize then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;MapTools;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('MapTools', ImportUnit);
finalization
  SepiUnregisterImportedUnit('MapTools');
end.

