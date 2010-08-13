{$IFNDEF QSYNEDITFUNDELPHIGRAMMAR}
unit SynHighlighterFunDelphi;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
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
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynFunDelphiSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..226] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncAction(Index: Integer): TtkTokenKind;
    function FuncActions(Index: Integer): TtkTokenKind;
    function FuncAnd(Index: Integer): TtkTokenKind;
    function FuncAs(Index: Integer): TtkTokenKind;
    function FuncAt(Index: Integer): TtkTokenKind;
    function FuncAttributes(Index: Integer): TtkTokenKind;
    function FuncBegin(Index: Integer): TtkTokenKind;
    function FuncCan(Index: Integer): TtkTokenKind;
    function FuncCannot(Index: Integer): TtkTokenKind;
    function FuncCase(Index: Integer): TtkTokenKind;
    function FuncCategory(Index: Integer): TtkTokenKind;
    function FuncClass(Index: Integer): TtkTokenKind;
    function FuncComponent(Index: Integer): TtkTokenKind;
    function FuncComponents(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncCreator(Index: Integer): TtkTokenKind;
    function FuncDiscards(Index: Integer): TtkTokenKind;
    function FuncDiv(Index: Integer): TtkTokenKind;
    function FuncDo(Index: Integer): TtkTokenKind;
    function FuncDownto(Index: Integer): TtkTokenKind;
    function FuncEffect(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncExactly(Index: Integer): TtkTokenKind;
    function FuncExcept(Index: Integer): TtkTokenKind;
    function FuncField(Index: Integer): TtkTokenKind;
    function FuncFinally(Index: Integer): TtkTokenKind;
    function FuncFor(Index: Integer): TtkTokenKind;
    function FuncForward(Index: Integer): TtkTokenKind;
    function FuncFunction(Index: Integer): TtkTokenKind;
    function FuncHas(Index: Integer): TtkTokenKind;
    function FuncHint(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncImage(Index: Integer): TtkTokenKind;
    function FuncIn(Index: Integer): TtkTokenKind;
    function FuncInherited(Index: Integer): TtkTokenKind;
    function FuncIs(Index: Integer): TtkTokenKind;
    function FuncLeast(Index: Integer): TtkTokenKind;
    function FuncLess(Index: Integer): TtkTokenKind;
    function FuncMessage(Index: Integer): TtkTokenKind;
    function FuncMessages(Index: Integer): TtkTokenKind;
    function FuncMod(Index: Integer): TtkTokenKind;
    function FuncMore(Index: Integer): TtkTokenKind;
    function FuncMost(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNil(Index: Integer): TtkTokenKind;
    function FuncNot(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncObstacle(Index: Integer): TtkTokenKind;
    function FuncOf(Index: Integer): TtkTokenKind;
    function FuncOn(Index: Integer): TtkTokenKind;
    function FuncOr(Index: Integer): TtkTokenKind;
    function FuncOut(Index: Integer): TtkTokenKind;
    function FuncPlugin(Index: Integer): TtkTokenKind;
    function FuncPoscomponent(Index: Integer): TtkTokenKind;
    function FuncPrivate(Index: Integer): TtkTokenKind;
    function FuncProcedure(Index: Integer): TtkTokenKind;
    function FuncProperty(Index: Integer): TtkTokenKind;
    function FuncPublic(Index: Integer): TtkTokenKind;
    function FuncRaise(Index: Integer): TtkTokenKind;
    function FuncReceives(Index: Integer): TtkTokenKind;
    function FuncRepeat(Index: Integer): TtkTokenKind;
    function FuncResourcestring(Index: Integer): TtkTokenKind;
    function FuncShl(Index: Integer): TtkTokenKind;
    function FuncShr(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncThan(Index: Integer): TtkTokenKind;
    function FuncThen(Index: Integer): TtkTokenKind;
    function FuncTo(Index: Integer): TtkTokenKind;
    function FuncTool(Index: Integer): TtkTokenKind;
    function FuncTry(Index: Integer): TtkTokenKind;
    function FuncUnit(Index: Integer): TtkTokenKind;
    function FuncUntil(Index: Integer): TtkTokenKind;
    function FuncUses(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncVehicle(Index: Integer): TtkTokenKind;
    function FuncWhile(Index: Integer): TtkTokenKind;
    function FuncWith(Index: Integer): TtkTokenKind;
    function FuncXor(Index: Integer): TtkTokenKind;
    function FuncZindex(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
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
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
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

resourcestring
  SYNS_FilterFunDelphi = 'FunDelphi files (*.fnd)|*.fnd';
  SYNS_LangFunDelphi = 'FunDelphi';
  SYNS_FriendlyLangFunDelphi = 'FunDelphi';

const
  KeyWords: array[0..79] of UnicodeString = (
    'action', 'actions', 'and', 'as', 'at', 'attributes', 'begin', 'can', 
    'cannot', 'case', 'category', 'class', 'component', 'components', 'const', 
    'creator', 'discards', 'div', 'do', 'downto', 'effect', 'else', 'end', 
    'exactly', 'except', 'field', 'finally', 'for', 'forward', 'function', 
    'has', 'hint', 'if', 'image', 'in', 'inherited', 'is', 'least', 'less', 
    'message', 'messages', 'mod', 'more', 'most', 'name', 'nil', 'not', 
    'object', 'obstacle', 'of', 'on', 'or', 'out', 'plugin', 'poscomponent', 
    'private', 'procedure', 'property', 'public', 'raise', 'receives', 'repeat', 
    'resourcestring', 'shl', 'shr', 'string', 'than', 'then', 'to', 'tool', 
    'try', 'unit', 'until', 'uses', 'var', 'vehicle', 'while', 'with', 'xor', 
    'zindex' 
  );

  KeyIndices: array[0..226] of Integer = (
    35, -1, 36, 5, -1, 54, -1, -1, 53, -1, 69, 51, -1, -1, -1, 79, -1, -1, 70, 
    -1, -1, 57, -1, 13, -1, 34, 77, -1, -1, -1, 27, -1, 12, -1, 28, -1, -1, -1, 
    -1, -1, -1, -1, -1, 47, -1, -1, 2, 18, -1, -1, -1, 25, -1, -1, -1, -1, 38, 
    -1, -1, 20, 66, 6, -1, -1, -1, -1, -1, -1, -1, 22, 31, 78, -1, -1, -1, -1, 
    -1, -1, 16, 55, 14, -1, 23, 44, -1, -1, -1, -1, 65, 68, -1, -1, -1, -1, -1, 
    -1, -1, 0, -1, -1, 73, -1, 24, 8, -1, 21, -1, 56, 48, 64, -1, 52, -1, -1, 
    -1, 33, -1, -1, -1, -1, 58, -1, -1, -1, -1, 30, -1, 11, -1, -1, -1, -1, -1, 
    -1, 75, -1, -1, 61, -1, 59, -1, 26, -1, -1, -1, -1, 41, -1, -1, -1, 1, -1, 
    -1, -1, -1, -1, -1, 49, 4, -1, -1, -1, -1, 37, -1, 10, -1, 40, -1, -1, -1, 
    -1, 43, 39, -1, 72, 7, -1, -1, -1, -1, -1, 63, -1, 67, -1, -1, 15, -1, -1, 
    45, -1, -1, 74, -1, -1, 62, 76, 32, -1, 17, -1, 71, 46, 60, -1, 29, -1, 3, 
    -1, 42, 50, -1, -1, -1, -1, -1, -1, -1, -1, 19, -1, 9, -1, -1, -1, -1 
  );

procedure TSynFunDelphiSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[97] := FuncAction;
  fIdentFuncTable[150] := FuncActions;
  fIdentFuncTable[46] := FuncAnd;
  fIdentFuncTable[208] := FuncAs;
  fIdentFuncTable[158] := FuncAt;
  fIdentFuncTable[3] := FuncAttributes;
  fIdentFuncTable[61] := FuncBegin;
  fIdentFuncTable[176] := FuncCan;
  fIdentFuncTable[103] := FuncCannot;
  fIdentFuncTable[222] := FuncCase;
  fIdentFuncTable[165] := FuncCategory;
  fIdentFuncTable[127] := FuncClass;
  fIdentFuncTable[32] := FuncComponent;
  fIdentFuncTable[23] := FuncComponents;
  fIdentFuncTable[80] := FuncConst;
  fIdentFuncTable[187] := FuncCreator;
  fIdentFuncTable[78] := FuncDiscards;
  fIdentFuncTable[200] := FuncDiv;
  fIdentFuncTable[47] := FuncDo;
  fIdentFuncTable[220] := FuncDownto;
  fIdentFuncTable[59] := FuncEffect;
  fIdentFuncTable[105] := FuncElse;
  fIdentFuncTable[69] := FuncEnd;
  fIdentFuncTable[82] := FuncExactly;
  fIdentFuncTable[102] := FuncExcept;
  fIdentFuncTable[51] := FuncField;
  fIdentFuncTable[141] := FuncFinally;
  fIdentFuncTable[30] := FuncFor;
  fIdentFuncTable[34] := FuncForward;
  fIdentFuncTable[206] := FuncFunction;
  fIdentFuncTable[125] := FuncHas;
  fIdentFuncTable[70] := FuncHint;
  fIdentFuncTable[198] := FuncIf;
  fIdentFuncTable[115] := FuncImage;
  fIdentFuncTable[25] := FuncIn;
  fIdentFuncTable[0] := FuncInherited;
  fIdentFuncTable[2] := FuncIs;
  fIdentFuncTable[163] := FuncLeast;
  fIdentFuncTable[56] := FuncLess;
  fIdentFuncTable[173] := FuncMessage;
  fIdentFuncTable[167] := FuncMessages;
  fIdentFuncTable[146] := FuncMod;
  fIdentFuncTable[210] := FuncMore;
  fIdentFuncTable[172] := FuncMost;
  fIdentFuncTable[83] := FuncName;
  fIdentFuncTable[190] := FuncNil;
  fIdentFuncTable[203] := FuncNot;
  fIdentFuncTable[43] := FuncObject;
  fIdentFuncTable[108] := FuncObstacle;
  fIdentFuncTable[157] := FuncOf;
  fIdentFuncTable[211] := FuncOn;
  fIdentFuncTable[11] := FuncOr;
  fIdentFuncTable[111] := FuncOut;
  fIdentFuncTable[8] := FuncPlugin;
  fIdentFuncTable[5] := FuncPoscomponent;
  fIdentFuncTable[79] := FuncPrivate;
  fIdentFuncTable[107] := FuncProcedure;
  fIdentFuncTable[21] := FuncProperty;
  fIdentFuncTable[120] := FuncPublic;
  fIdentFuncTable[139] := FuncRaise;
  fIdentFuncTable[204] := FuncReceives;
  fIdentFuncTable[137] := FuncRepeat;
  fIdentFuncTable[196] := FuncResourcestring;
  fIdentFuncTable[182] := FuncShl;
  fIdentFuncTable[109] := FuncShr;
  fIdentFuncTable[88] := FuncString;
  fIdentFuncTable[60] := FuncThan;
  fIdentFuncTable[184] := FuncThen;
  fIdentFuncTable[89] := FuncTo;
  fIdentFuncTable[10] := FuncTool;
  fIdentFuncTable[18] := FuncTry;
  fIdentFuncTable[202] := FuncUnit;
  fIdentFuncTable[175] := FuncUntil;
  fIdentFuncTable[100] := FuncUses;
  fIdentFuncTable[193] := FuncVar;
  fIdentFuncTable[134] := FuncVehicle;
  fIdentFuncTable[197] := FuncWhile;
  fIdentFuncTable[26] := FuncWith;
  fIdentFuncTable[71] := FuncXor;
  fIdentFuncTable[15] := FuncZindex;
end;

{$Q-}
function TSynFunDelphiSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 467 + Ord(Str^) * 177;
    inc(Str);
  end;
  Result := Result mod 227;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynFunDelphiSyn.FuncAction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncActions(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncAnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncAs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncAt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncAttributes(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncBegin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncCan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncCannot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncCase(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncCategory(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncClass(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncComponent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncComponents(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncCreator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncDiscards(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncDiv(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncDo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncDownto(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncEffect(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncExactly(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncExcept(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncField(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncFinally(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncFor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncForward(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncFunction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncHas(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncHint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncImage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncIn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncInherited(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncIs(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncLeast(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncLess(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncMessage(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncMessages(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncMod(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncMore(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncMost(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncName(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncNil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncNot(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncObstacle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncOf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncOn(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncOr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncOut(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncPlugin(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncPoscomponent(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncPrivate(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncProcedure(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncProperty(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncPublic(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncRaise(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncReceives(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncRepeat(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncResourcestring(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncShl(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncShr(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncThan(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncThen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncTo(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncTool(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncTry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncUnit(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncUntil(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncUses(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncVehicle(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncWhile(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncWith(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncXor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.FuncZindex(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynFunDelphiSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynFunDelphiSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynFunDelphiSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynFunDelphiSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
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
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynFunDelphiSyn.LineCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') then
  begin
    Inc(Run, 1);
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
  while not IsLineEnd(Run) do
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
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

constructor TSynFunDelphiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := True;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterFunDelphi;
  fRange := rsUnknown;
end;

procedure TSynFunDelphiSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynFunDelphiSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynFunDelphiSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsBraceComment: BraceCommentProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '{': BraceCommentOpenProc;
      '/': LineCommentOpenProc;
      '''': StringOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynFunDelphiSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynFunDelphiSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynFunDelphiSyn.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := 
    'action,actions,and,as,at,attributes,begin,can,cannot,case,category,cl' +
    'ass,component,components,const,creator,discards,div,do,downto,effect,e' +
    'lse,end,exactly,except,field,finally,for,forward,function,has,hint,if,' +
    'image,in,inherited,is,least,less,message,messages,mod,more,most,name,n' +
    'il,not,object,obstacle,of,on,or,out,plugin,poscomponent,private,proced' +
    'ure,property,public,raise,receives,repeat,resourcestring,shl,shr,strin' +
    'g,than,then,to,tool,try,unit,until,uses,var,vehicle,while,with,xor,zin' +
    'dex';
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

function TSynFunDelphiSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynFunDelphiSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynFunDelphiSyn.GetSampleSource: UnicodeString;
begin
  Result := 
    'unit Sample;'#13#10 +
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
    'end.';
end;

function TSynFunDelphiSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterFunDelphi;
end;

class function TSynFunDelphiSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangFunDelphi;
end;

class function TSynFunDelphiSyn.GetLanguageName: string;
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
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynFunDelphiSyn);
{$ENDIF}
end.
