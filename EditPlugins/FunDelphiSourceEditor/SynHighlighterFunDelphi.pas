{+-----------------------------------------------------------------------------+
 | Class:       TSynFunDelphiSyn
 | Created:     2009-08-06
 | Last change: 2009-08-06
 | Author:      sjrd
 | Description: Syntax Highlighter for FunDelphi
 | Version:     5.0
 |
 | Copyright (c) 2009 sjrd. All rights reserved.
 |
 | Generated with SynGen.
 +----------------------------------------------------------------------------+}

unit SynHighlighterFunDelphi;

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkString,
    tkUnknown);

  TRangeState = (rsUnKnown, rsBraceComment, rsLineComment, rsString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 205;

type
  TSynFunDelphiSyn = class(TSynCustomHighlighter)
  private
    fLineRef: string;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRange: TRangeState;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func17: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func144: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func205: TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure BraceCommentOpenProc;
    procedure BraceCommentProc;
    procedure LineCommentOpenProc;
    procedure LineCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SYNS_FilterFunDelphi = 'FunDelphi source file (*.fnd)|*.fnd';
  SYNS_LangFunDelphi = 'FunDelphi';

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            mHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mHashTable[I] := Ord(I) - 95;
        end;
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynFunDelphiSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[144] := Func144;
  fIdentFuncTable[145] := Func145;
  fIdentFuncTable[153] := Func153;
  fIdentFuncTable[177] := Func177;
  fIdentFuncTable[205] := Func205;
end;

function TSynFunDelphiSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynFunDelphiSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

function TSynFunDelphiSyn.Func17: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func21: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else
    if KeyComp('can') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func22: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else
    if KeyComp('as') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func23: TtkTokenKind;
begin
  if KeyComp('of') then Result := tkKey else
    if KeyComp('at') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func25: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func26: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func30: TtkTokenKind;
begin
  if KeyComp('is') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func31: TtkTokenKind;
begin
  if KeyComp('on') then Result := tkKey else
    if KeyComp('has') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func32: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func35: TtkTokenKind;
begin
  if KeyComp('or') then Result := tkKey else
    if KeyComp('mod') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func37: TtkTokenKind;
begin
  if KeyComp('to') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func38: TtkTokenKind;
begin
  if KeyComp('div') then Result := tkKey else
    if KeyComp('nil') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func40: TtkTokenKind;
begin
  if KeyComp('image') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func41: TtkTokenKind;
begin
  if KeyComp('field') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
    if KeyComp('begin') then Result := tkKey else
      if KeyComp('shl') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func44: TtkTokenKind;
begin
  if KeyComp('var') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func45: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func47: TtkTokenKind;
begin
  if KeyComp('than') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func48: TtkTokenKind;
begin
  if KeyComp('shr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func51: TtkTokenKind;
begin
  if KeyComp('effect') then Result := tkKey else
    if KeyComp('then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func52: TtkTokenKind;
begin
  if KeyComp('not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func55: TtkTokenKind;
begin
  if KeyComp('more') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func57: TtkTokenKind;
begin
  if KeyComp('raise') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func59: TtkTokenKind;
begin
  if KeyComp('less') then Result := tkKey else
    if KeyComp('out') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func60: TtkTokenKind;
begin
  if KeyComp('xor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func61: TtkTokenKind;
begin
  if KeyComp('object') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func62: TtkTokenKind;
begin
  if KeyComp('least') then Result := tkKey else
    if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func66: TtkTokenKind;
begin
  if KeyComp('try') then Result := tkKey else
    if KeyComp('tool') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func68: TtkTokenKind;
begin
  if KeyComp('uses') then Result := tkKey else
    if KeyComp('action') then Result := tkKey else
      if KeyComp('unit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func69: TtkTokenKind;
begin
  if KeyComp('public') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func71: TtkTokenKind;
begin
  if KeyComp('most') then Result := tkKey else
    if KeyComp('repeat') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func76: TtkTokenKind;
begin
  if KeyComp('const') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func79: TtkTokenKind;
begin
  if KeyComp('except') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func81: TtkTokenKind;
begin
  if KeyComp('until') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func85: TtkTokenKind;
begin
  if KeyComp('plugin') then Result := tkKey else
    if KeyComp('obstacle') then Result := tkKey else
      if KeyComp('discards') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func86: TtkTokenKind;
begin
  if KeyComp('finally') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func88: TtkTokenKind;
begin
  if KeyComp('actions') then Result := tkKey else
    if KeyComp('zindex') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func92: TtkTokenKind;
begin
  if KeyComp('forward') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func93: TtkTokenKind;
begin
  if KeyComp('string') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func94: TtkTokenKind;
begin
  if KeyComp('receives') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func97: TtkTokenKind;
begin
  if KeyComp('downto') then Result := tkKey else
    if KeyComp('exactly') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func98: TtkTokenKind;
begin
  if KeyComp('private') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func101: TtkTokenKind;
begin
  if KeyComp('inherited') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func110: TtkTokenKind;
begin
  if KeyComp('function') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func114: TtkTokenKind;
begin
  if KeyComp('procedure') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func144: TtkTokenKind;
begin
  if KeyComp('components') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func145: TtkTokenKind;
begin
  if KeyComp('attributes') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func153: TtkTokenKind;
begin
  if KeyComp('destructor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func177: TtkTokenKind;
begin
  if KeyComp('constructor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.Func205: TtkTokenKind;
begin
  if KeyComp('resourcestring') then Result := tkKey else Result := tkIdentifier;
end;

function TSynFunDelphiSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynFunDelphiSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynFunDelphiSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '{': fProcTable[I] := BraceCommentOpenProc;
      '/': fProcTable[I] := LineCommentOpenProc;
      '''': fProcTable[I] := StringOpenProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynFunDelphiSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynFunDelphiSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynFunDelphiSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynFunDelphiSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynFunDelphiSyn.BraceCommentOpenProc;
begin
  Inc(Run);
  fRange := rsBraceComment;
  BraceCommentProc;
  fTokenID := tkComment;
end;

procedure TSynFunDelphiSyn.BraceCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '}') then
        begin
          Inc(Run, 1);
          fRange := rsUnKnown;
          Break;
        end;
        if not (fLine[Run] in [#0, #10, #13]) then
          Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynFunDelphiSyn.LineCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') then
  begin
    fRange := rsLineComment;
    LineCommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynFunDelphiSyn.LineCommentProc;
begin
  fTokenID := tkComment;
  while not (fLine[Run] in [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynFunDelphiSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynFunDelphiSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '''') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

constructor TSynFunDelphiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterFunDelphi;
  fRange := rsUnknown;
end;

procedure TSynFunDelphiSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynFunDelphiSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynFunDelphiSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynFunDelphiSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsBraceComment: BraceCommentProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynFunDelphiSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynFunDelphiSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynFunDelphiSyn.GetKeyWords: string;
begin
  Result := 
    'action,actions,and,as,at,attributes,begin,can,case,components,const,c' +
    'onstructor,destructor,discards,div,do,downto,effect,else,end,exactly,e' +
    'xcept,field,finally,for,forward,function,has,if,image,in,inherited,is,' +
    'least,less,mod,more,most,nil,not,object,obstacle,of,on,or,out,plugin,p' +
    'rivate,procedure,public,raise,receives,repeat,resourcestring,shl,shr,s' +
    'tring,than,then,to,tool,try,unit,until,uses,var,while,xor,zindex';
end;

function TSynFunDelphiSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynFunDelphiSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynFunDelphiSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynFunDelphiSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynFunDelphiSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynFunDelphiSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z'];
end;

function TSynFunDelphiSyn.GetSampleSource: string;
begin
  Result := 'unit Sample;'#13#10 +
            #13#10 +
            'uses'#13#10 +
            '  Generics, FLBCommon;'#13#10 +
            #13#10 +
            '// Objet qui permet d''ouvrir un bloc en or ou en argent'#13#10 +
            'object TSpecialKey'#13#10 +
            '  image ''SpecialKey'';'#13#10 +
            '  '#13#10 +
            '  action OpenSilverBlock then'#13#10 +
            '    Player discards 1 Self;'#13#10 +
            #13#10 +
            '  action OpenGoldenBlock then'#13#10 +
            '    Player discards 1 Self;'#13#10 +
            'end;'#13#10 +
            #13#10 +
            'end.';
end;

function TSynFunDelphiSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterFunDelphi;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynFunDelphiSyn.GetLanguageName: string;
begin
  Result := SYNS_LangFunDelphi;
end;

procedure TSynFunDelphiSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynFunDelphiSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynFunDelphiSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynFunDelphiSyn);
{$ENDIF}
end.
