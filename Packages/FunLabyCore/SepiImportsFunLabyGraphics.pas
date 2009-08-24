{*
  Importe l'unité FunLabyGraphics dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsFunLabyGraphics;

interface

uses
  Windows, SysUtils, Classes, TypInfo, SepiReflectionCore, SepiMembers, Types, 
  GR32, FunLabyGraphics;

var
  SepiImportsFunLabyGraphicsLazyLoad: Boolean = False;

procedure DelphiSepiConsistencyAssertions;

implementation

{$R *.res}

{$WARN SYMBOL_DEPRECATED OFF}

const // don't localize
  UnitName = 'FunLabyGraphics';
  ResourceName = 'SepiImportsFunLabyGraphics';
  TypeCount = 2;
  MethodCount = 22;
  VariableCount = 1;

var
  TypeInfoArray: array[0..TypeCount-1] of PTypeInfo;
  MethodAddresses: array[0..MethodCount-1] of Pointer;
  VarAddresses: array[0..VariableCount-1] of Pointer;

type
  TSepiImportsTBitmap32Frame = class(TBitmap32Frame)
  private
    procedure SetDelay(Value: Integer);
    class procedure InitMethodAddresses;
  end;

  TSepiImportsTAnimatedBitmap32 = class(TAnimatedBitmap32)
  private
    function GetFrameCount: Integer;
    procedure SetFrameCount(Value: Integer);
    function GetFrames(Index: Integer): TBitmap32Frame;
    procedure DrawAtTimeTo_0(TickCount: Cardinal; Dst: TBitmap32);
    procedure DrawAtTimeTo_1(TickCount: Cardinal; Dst: TBitmap32; DstX: Integer; DstY: Integer; const SrcRect: TRect);
    procedure DrawAtTimeTo_2(TickCount: Cardinal; Dst: TBitmap32; DstX: Integer; DstY: Integer);
    procedure DrawAtTimeTo_3(TickCount: Cardinal; Dst: TBitmap32; const DstRect: TRect);
    procedure DrawAtTimeTo_4(TickCount: Cardinal; Dst: TBitmap32; const DstRect: TRect; const SrcRect: TRect);
    class procedure InitMethodAddresses;
  end;

{-----------------------}
{ TBitmap32Frame import }
{-----------------------}

procedure TSepiImportsTBitmap32Frame.SetDelay(Value: Integer);
begin
  Delay := Value;
end;

class procedure TSepiImportsTBitmap32Frame.InitMethodAddresses;
begin
  MethodAddresses[0] := @TSepiImportsTBitmap32Frame.SetDelay;
  MethodAddresses[1] := @TSepiImportsTBitmap32Frame.CreateFrame;
end;

{--------------------------}
{ TAnimatedBitmap32 import }
{--------------------------}

function TSepiImportsTAnimatedBitmap32.GetFrameCount: Integer;
begin
  Result := FrameCount;
end;

procedure TSepiImportsTAnimatedBitmap32.SetFrameCount(Value: Integer);
begin
  FrameCount := Value;
end;

function TSepiImportsTAnimatedBitmap32.GetFrames(Index: Integer): TBitmap32Frame;
begin
  Result := Frames[Index];
end;

procedure TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_0(TickCount: Cardinal; Dst: TBitmap32);
begin
  DrawAtTimeTo(TickCount, Dst);
end;

procedure TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_1(TickCount: Cardinal; Dst: TBitmap32; DstX: Integer; DstY: Integer; const SrcRect: TRect);
begin
  DrawAtTimeTo(TickCount, Dst, DstX, DstY, SrcRect);
end;

procedure TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_2(TickCount: Cardinal; Dst: TBitmap32; DstX: Integer; DstY: Integer);
begin
  DrawAtTimeTo(TickCount, Dst, DstX, DstY);
end;

procedure TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_3(TickCount: Cardinal; Dst: TBitmap32; const DstRect: TRect);
begin
  DrawAtTimeTo(TickCount, Dst, DstRect);
end;

procedure TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_4(TickCount: Cardinal; Dst: TBitmap32; const DstRect: TRect; const SrcRect: TRect);
begin
  DrawAtTimeTo(TickCount, Dst, DstRect, SrcRect);
end;

class procedure TSepiImportsTAnimatedBitmap32.InitMethodAddresses;
begin
  MethodAddresses[2] := @TSepiImportsTAnimatedBitmap32.GetFrameCount;
  MethodAddresses[3] := @TSepiImportsTAnimatedBitmap32.SetFrameCount;
  MethodAddresses[4] := @TSepiImportsTAnimatedBitmap32.GetFrames;
  MethodAddresses[5] := @TSepiImportsTAnimatedBitmap32.AssignFromAnimatedBitmap32;
  MethodAddresses[6] := @TSepiImportsTAnimatedBitmap32.AssignFromGIFImage;
  MethodAddresses[7] := @TSepiImportsTAnimatedBitmap32.GetFrameAtTime;
  MethodAddresses[8] := @TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_0;
  MethodAddresses[9] := @TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_1;
  MethodAddresses[10] := @TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_2;
  MethodAddresses[11] := @TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_3;
  MethodAddresses[12] := @TSepiImportsTAnimatedBitmap32.DrawAtTimeTo_4;
end;

{---------------------}
{ Overloaded routines }
{---------------------}

function AnimateBitmap_0(Bitmap: TBitmap32; const FrameDelays: array of Cardinal): TAnimatedBitmap32;
begin
  Result := AnimateBitmap(Bitmap, FrameDelays);
end;

function AnimateBitmap_1(Bitmap: TBitmap32; FrameCount: Integer; FrameDelay: Cardinal): TAnimatedBitmap32;
begin
  Result := AnimateBitmap(Bitmap, FrameCount, FrameDelay);
end;

function AnimateBitmap_2(Bitmap: TBitmap32; FrameDelay: Cardinal): TAnimatedBitmap32;
begin
  Result := AnimateBitmap(Bitmap, FrameDelay);
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
      SepiImportsFunLabyGraphicsLazyLoad,
      TGetMethodCodeEvent(GetMethodCodeEvent),
      TGetTypeInfoEvent(GetTypeInfoEvent),
      TGetVarAddressEvent(GetVarAddressEvent));

    if SepiImportsFunLabyGraphicsLazyLoad then
      Result.AcquireObjResource(Stream);
  finally
    Stream.Free;
  end;
end;

procedure InitTypeInfoArray;
begin
  TypeInfoArray[0] := TypeInfo(TBitmap32Frame);
  TypeInfoArray[1] := TypeInfo(TAnimatedBitmap32);
end;

procedure InitMethodAddresses;
begin
  TSepiImportsTBitmap32Frame.InitMethodAddresses;
  TSepiImportsTAnimatedBitmap32.InitMethodAddresses;
  MethodAddresses[13] := @GCD;
  MethodAddresses[14] := @LCM;
  MethodAddresses[15] := @ReplaceColorInBitmap32;
  MethodAddresses[16] := @HandleBmpTransparent;
  MethodAddresses[17] := @LoadBitmapFromFile;
  MethodAddresses[18] := @AnimateBitmap_0;
  MethodAddresses[19] := @AnimateBitmap_1;
  MethodAddresses[20] := @AnimateBitmap_2;
  MethodAddresses[21] := @DrawRepeat;
end;

procedure InitVarAddresses;
begin
end;

{------------------------------------}
{ Delphi-Sepi consistency assertions }
{------------------------------------}

type
  TCheckAlignmentForTBitmap32Frame = record
    Dummy: Byte;
    Field: TBitmap32Frame;
  end;

{$IF SizeOf(TCheckAlignmentForTBitmap32Frame) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TBitmap32Frame n''a pas l''alignement calculé par Sepi'}
{$IFEND}

type
  TCheckAlignmentForTAnimatedBitmap32 = record
    Dummy: Byte;
    Field: TAnimatedBitmap32;
  end;

{$IF SizeOf(TCheckAlignmentForTAnimatedBitmap32) <> (4 + 4)}
  {$MESSAGE WARN 'Le type TAnimatedBitmap32 n''a pas l''alignement calculé par Sepi'}
{$IFEND}

procedure CheckInstanceSize(AClass: TClass;
  SepiInstSize, ParentSepiInstSize: Longint);
begin
  if (AClass.InstanceSize - SepiInstSize) =
    (AClass.ClassParent.InstanceSize - ParentSepiInstSize) then
    Exit;

  WriteLn(ErrOutput, Format('InstanceSize;%d;%d;FunLabyGraphics;%s;%s',
    [SepiInstSize, AClass.InstanceSize, AClass.ClassName,
    AClass.ClassParent.ClassName]));
end;

procedure DelphiSepiConsistencyAssertions;
begin
  {$ASSERTIONS ON}
  CheckInstanceSize(TBitmap32Frame, 276, 268);
  CheckInstanceSize(TAnimatedBitmap32, 292, 276);
  {$ASSERTIONS OFF}
end;

{$WARN SYMBOL_DEPRECATED ON}

initialization
  InitTypeInfoArray;
  InitMethodAddresses;
  InitVarAddresses;

  SepiRegisterImportedUnit('FunLabyGraphics', ImportUnit);
finalization
  SepiUnregisterImportedUnit('FunLabyGraphics');
end.

