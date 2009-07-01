{*
  Analyseur lexical d'une unité FunDelphi
  @author sjrd
  @version 5.0
*}
unit SepiFunDelphiLexer;

interface

{$D-,L-}

uses
  Types, Classes, SysUtils, StrUtils, ScStrUtils, SepiCompilerErrors,
  SepiParseTrees, SepiLexerUtils, SepiCompilerConsts;

const
  FirstTerminal = 0;
  LastTerminal = 94;

  tkEof = SepiLexerUtils.tkEof; /// Fin de fichier

  tkIdentifier = 1; // Identifier
  tkInteger = 2;    // Integer
  tkFloat = 3;      // Floating point number
  tkStringCst = 4;  // String

  tkOpenBracket = 5;    // (
  tkCloseBracket = 6;   // )
  tkOpenSqBracket = 7;  // [
  tkCloseSqBracket = 8; // ]
  tkEquals = 9;         // =
  tkComma = 10;         // ,
  tkColon = 11;         // :
  tkSemiColon = 12;     // ;
  tkDot = 13;           // .
  tkRange = 14;         // ..
  tkHat = 15;           // ^
  tkAt = 16;            // @
  tkAssign = 17;        // :=

  tkUnit = 18;           // unit
  tkUses = 19;           // uses
  tkConst = 20;          // const
  tkResourceString = 21; // resourcestring
  tkVar = 22;            // var
  tkOut = 23;            // out

  tkProcedure = 24; // procedure
  tkFunction = 25;  // function
  tkPrivate = 26;   // private
  tkPublic = 27;    // public
  tkForward = 28;   // forward

  tkPlugin = 29;     // plugin
  tkObject = 30;     // object
  tkField = 31;      // field
  tkEffect = 32;     // effect
  tkTool = 33;       // tool
  tkObstacle = 34;   // obstacle
  tkComponents = 35; // components
  tkAttributes = 36; // attributes
  tkActions = 37;    // actions

  tkConstructor = 38; // constructor
  tkDestructor = 39;  // destructor
  tkZIndex = 40;      // zindex
  tkImage = 41;       // image
  tkAction = 42;      // action

  tkPlus = 43;        // +
  tkMinus = 44;       // -
  tkTimes = 45;       // *
  tkDivide = 46;      // /
  tkDiv = 47;         // div
  tkMod = 48;         // mod
  tkShl = 49;         // shl
  tkShr = 50;         // shr
  tkOr = 51;          // or
  tkAnd = 52;         // and
  tkXor = 53;         // xor
  tkNot = 54;         // not
  tkLowerThan = 55;   // <
  tkLowerEq = 56;     // <=
  tkGreaterThan = 57; // >
  tkGreaterEq = 58;   // >=
  tkNotEqual = 59;    // <>
  tkIs = 60;          // is
  tkAs = 61;          // as

  tkBegin = 62; // begin
  tkEnd = 63;   // end

  tkString = 64; // string
  tkNil = 65;    // nil

  tkIf = 66;        // if
  tkThen = 67;      // then
  tkElse = 68;      // else
  tkWhile = 69;     // while
  tkDo = 70;        // do
  tkRepeat = 71;    // repeat
  tkUntil = 72;     // until
  tkFor = 73;       // for
  tkTo = 74;        // to
  tkDownTo = 75;    // downto
  tkCase = 76;      // case
  tkOf = 77;        // of
  tkTry = 78;       // try
  tkExcept = 79;    // except
  tkOn = 80;        // on
  tkFinally = 81;   // finally
  tkRaise = 82;     // raise
  tkInherited = 83; // inherited

  tkCan = 84;      // can
  tkHas = 85;      // has
  tkAtKw = 86;     // at
  tkLeast = 87;    // least
  tkMost = 88;     // most
  tkMore = 89;     // more
  tkLess = 90;     // less
  tkThan = 91;     // than
  tkExactly = 92;  // exactly
  tkReceives = 93; // receives
  tkDiscards = 94; // discards

type
  {*
    Analyseur lexical pour le langage FunDelphi
    @author sjrd
    @version 5.0
  *}
  TSepiFunDelphiLexer = class(TSepiCustomManualLexer)
  protected
    procedure IdentifyKeyword(const OrigKey: string;
      var SymbolClass: TSepiSymbolClass); override;

    procedure InitLexingFuncs; override;

    function ActionSymbol: Boolean;
    function ActionIdentifier: Boolean;
    function ActionNumber: Boolean;
    function ActionString: Boolean;
    function ActionSingleLineComment: Boolean;
    function ActionMultiLineComment: Boolean;
  end;

const
  IdentChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];
  NumberChars = ['0'..'9'];
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
  StringChars = ['''', '#'];

var
  /// Symbol class names, indexed by their IDs
  SymbolClassNames: TStringDynArray;

implementation

{---------------------------}
{ TSepiFunDelphiLexer class }
{---------------------------}

{*
  [@inheritDoc]
*}
procedure TSepiFunDelphiLexer.IdentifyKeyword(const OrigKey: string;
  var SymbolClass: TSepiSymbolClass);
var
  Key: string;
begin
  Key := OrigKey;

  case Key[1] of
    'a' : if Key = 'action'         then SymbolClass := tkAction else
          if Key = 'actions'        then SymbolClass := tkActions else
          if Key = 'at'             then SymbolClass := tkAtKw else
          if Key = 'attributes'     then SymbolClass := tkAttributes else
          if Key = 'and'            then SymbolClass := tkAnd else
          if Key = 'as'             then SymbolClass := tkAs;
    'b' : if Key = 'begin'          then SymbolClass := tkBegin;
    'c' : if Key = 'can'            then SymbolClass := tkCan else
          if Key = 'case'           then SymbolClass := tkCase else
          if Key = 'components'     then SymbolClass := tkComponents else
          if Key = 'const'          then SymbolClass := tkConst else
          if Key = 'constructor'    then SymbolClass := tkConstructor;
    'd' : if Key = 'destructor'     then SymbolClass := tkDestructor else
          if Key = 'div'            then SymbolClass := tkDiv else
          if Key = 'discards'       then SymbolClass := tkDiscards else
          if Key = 'do'             then SymbolClass := tkDo else
          if Key = 'downto'         then SymbolClass := tkDownto;
    'e' : if Key = 'effect'         then SymbolClass := tkEffect else
          if Key = 'else'           then SymbolClass := tkElse else
          if Key = 'end'            then SymbolClass := tkEnd else
          if Key = 'exactly'        then SymbolClass := tkExactly else
          if Key = 'except'         then SymbolClass := tkExcept;
    'f' : if Key = 'field'          then SymbolClass := tkField else
          if Key = 'finally'        then SymbolClass := tkFinally else
          if Key = 'for'            then SymbolClass := tkFor else
          if Key = 'forward'        then SymbolClass := tkForward else
          if Key = 'function'       then SymbolClass := tkFunction;
    'h' : if Key = 'has'            then SymbolClass := tkHas;
    'i' : if Key = 'if'             then SymbolClass := tkIf else
          if Key = 'image'          then SymbolClass := tkImage else
          if Key = 'inherited'      then SymbolClass := tkInherited else
          if Key = 'is'             then SymbolClass := tkIs;
    'l' : if Key = 'least'          then SymbolClass := tkLeast else
          if Key = 'less'           then SymbolClass := tkLess;
    'm' : if Key = 'mod'            then SymbolClass := tkMod else
          if Key = 'more'           then SymbolClass := tkMore else
          if Key = 'most'           then SymbolClass := tkMost;
    'n' : if Key = 'nil'            then SymbolClass := tkNil else
          if Key = 'not'            then SymbolClass := tkNot;
    'o' : if Key = 'object'         then SymbolClass := tkObject else
          if Key = 'obstacle'       then SymbolClass := tkObstacle else
          if Key = 'of'             then SymbolClass := tkOf else
          if Key = 'on'             then SymbolClass := tkOn else
          if Key = 'or'             then SymbolClass := tkOr else
          if Key = 'out'            then SymbolClass := tkOut;
    'p' : if Key = 'plugin'         then SymbolClass := tkPlugin else
          if Key = 'private'        then SymbolClass := tkPrivate else
          if Key = 'procedure'      then SymbolClass := tkProcedure else
          if Key = 'public'         then SymbolClass := tkPublic;
    'r' : if Key = 'raise'          then SymbolClass := tkRaise else
          if Key = 'receives'       then SymbolClass := tkReceives else
          if Key = 'repeat'         then SymbolClass := tkRepeat else
          if Key = 'resourcestring' then SymbolClass := tkResourceString;
    's' : if Key = 'shl'            then SymbolClass := tkShl else
          if Key = 'shr'            then SymbolClass := tkShr else
          if Key = 'string'         then SymbolClass := tkString;
    't' : if Key = 'than'           then SymbolClass := tkThan else
          if Key = 'then'           then SymbolClass := tkThen else
          if Key = 'to'             then SymbolClass := tkTo else
          if Key = 'tool'           then SymbolClass := tkTool else
          if Key = 'try'            then SymbolClass := tkTry;
    'u' : if Key = 'unit'           then SymbolClass := tkUnit else
          if Key = 'until'          then SymbolClass := tkUntil else
          if Key = 'uses'           then SymbolClass := tkUses;
    'v' : if Key = 'var'            then SymbolClass := tkVar;
    'w' : if Key = 'while'          then SymbolClass := tkWhile;
    'x' : if Key = 'xor'            then SymbolClass := tkXor;
    'z' : if Key = 'zindex'         then SymbolClass := tkZIndex;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TSepiFunDelphiLexer.InitLexingFuncs;
var
  C: Char;
begin
  inherited;

  for C := #0 to #255 do
  begin
    case C of
      '(', ')', '[', ']', '=', ',', ':', ';', '.',
        '^', '@', '+', '-', '*', '/', '<', '>':
        LexingFuncs[C] := ActionSymbol;
      'a'..'z', 'A'..'Z', '_', '&':
        LexingFuncs[C] := ActionIdentifier;
      '0'..'9', '$':
        LexingFuncs[C] := ActionNumber;
      '''', '#':
        LexingFuncs[C] := ActionString;
      '{':
        LexingFuncs[C] := ActionMultiLineComment;
    end;
  end;
end;

{*
  Analyzes a symbol or a comment
  @return True for a symbol, False for a comment
*}
function TSepiFunDelphiLexer.ActionSymbol: Boolean;
var
  Repr: string;
  SymbolClass: TSepiSymbolClass;
begin
  case Code[Cursor] of
    '(':
    begin
      case Code[Cursor+1] of
        '*':
        begin
          Result := ActionMultiLineComment;
          Exit;
        end;
      else
        CursorForward;
        Repr := '(';
        SymbolClass := tkOpenBracket;
      end;
    end;
    ')':
    begin
      CursorForward;
      Repr := ')';
      SymbolClass := tkCloseBracket;
    end;
    '[':
    begin
      CursorForward;
      Repr := '[';
      SymbolClass := tkOpenSqBracket;
    end;
    ']':
    begin
      CursorForward;
      Repr := ']';
      SymbolClass := tkCloseSqBracket;
    end;
    '=':
    begin
      CursorForward;
      Repr := '=';
      SymbolClass := tkEquals;
    end;
    ',':
    begin
      CursorForward;
      Repr := ',';
      SymbolClass := tkComma;
    end;
    ':':
    begin
      CursorForward;
      if Code[Cursor] = '=' then
      begin
        CursorForward;
        Repr := ':=';
        SymbolClass := tkAssign;
      end else
      begin
        Repr := ':';
        SymbolClass := tkColon;
      end;
    end;
    ';':
    begin
      CursorForward;
      Repr := ';';
      SymbolClass := tkSemiColon;
    end;
    '.':
    begin
      case Code[Cursor+1] of
        '.':
        begin
          CursorForward(2);
          Repr := '..';
          SymbolClass := tkRange;
        end;
      else
        CursorForward;
        Repr := '.';
        SymbolClass := tkDot;
      end;
    end;
    '^':
    begin
      CursorForward;
      Repr := '^';
      SymbolClass := tkHat;
    end;
    '@':
    begin
      CursorForward;
      Repr := '@';
      SymbolClass := tkAt;
    end;
    '+':
    begin
      CursorForward;
      Repr := '+';
      SymbolClass := tkPlus;
    end;
    '-':
    begin
      CursorForward;
      Repr := '-';
      SymbolClass := tkMinus;
    end;
    '*':
    begin
      CursorForward;
      Repr := '*';
      SymbolClass := tkTimes;
    end;
    '/':
    begin
      case Code[Cursor+1] of
        '/':
        begin
          Result := ActionSingleLineComment;
          Exit;
        end;
      else
        CursorForward;
        Repr := '/';
        SymbolClass := tkDivide;
      end;
    end;
    '<':
    begin
      case Code[Cursor+1] of
        '=':
        begin
          CursorForward(2);
          Repr := '<=';
          SymbolClass := tkLowerEq;
        end;
        '>':
        begin
          CursorForward(2);
          Repr := '<>';
          SymbolClass := tkNotEqual;
        end;
      else
        CursorForward;
        Repr := '<';
        SymbolClass := tkLowerThan;
      end;
    end;
    '>':
    begin
      case Code[Cursor+1] of
        '=':
        begin
          CursorForward(2);
          Repr := '>=';
          SymbolClass := tkGreaterEq;
        end;
      else
        CursorForward;
        Repr := '>';
        SymbolClass := tkGreaterThan;
      end;
    end;
  else
    Assert(False);
    Result := False;
    Exit;
  end;

  TerminalParsed(SymbolClass, Repr);
  Result := True;
end;

{*
  Analyzes an identifier
  @return True
*}
function TSepiFunDelphiLexer.ActionIdentifier: Boolean;
var
  ForceIdent: Boolean;
  BeginPos: Integer;
  Repr: string;
  SymbolClass: TSepiSymbolClass;
begin
  ForceIdent := Code[Cursor] = '&';
  if ForceIdent then
    CursorForward;

  BeginPos := Cursor;
  CursorForward;
  while Code[Cursor] in IdentChars do
    CursorForward;

  Repr := Copy(Code, BeginPos, Cursor-BeginPos);
  SymbolClass := tkIdentifier;

  if not ForceIdent then
    IdentifyKeyword(Repr, SymbolClass);

  TerminalParsed(SymbolClass, Repr);
  Result := True;
end;

{*
  Analyzes a number
  @return True
*}
function TSepiFunDelphiLexer.ActionNumber: Boolean;
var
  BeginPos: Integer;
  SymbolClass: TSepiSymbolClass;
begin
  BeginPos := Cursor;
  SymbolClass := tkInteger;

  if Code[Cursor] = '$' then
  begin
    repeat
      CursorForward;
    until not (Code[Cursor] in HexChars);
  end else
  begin
    while Code[Cursor] in NumberChars do
      CursorForward;

    if (Code[Cursor] = '.') and (Code[Cursor+1] in NumberChars) then
    begin
      SymbolClass := tkFloat;
      repeat
        CursorForward;
      until not (Code[Cursor] in NumberChars);
    end;

    if Code[Cursor] in ['e', 'E'] then
    begin
      SymbolClass := tkFloat;
      CursorForward;
      if Code[Cursor] in ['+', '-'] then
        CursorForward;

      while Code[Cursor] in NumberChars do
        CursorForward;
    end;
  end;

  TerminalParsed(SymbolClass, Copy(Code, BeginPos, Cursor-BeginPos));
  Result := True;
end;

{*
  Analyzes a string
  @return True
*}
function TSepiFunDelphiLexer.ActionString: Boolean;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  while Code[Cursor] in StringChars do
  begin
    case Code[Cursor] of
      '''':
      begin
        CursorForward;
        while Code[Cursor] <> '''' do
        begin
          if Code[Cursor] in [#0, #10, #13] then
            MakeError(SStringNotTerminated, ekFatalError)
          else
            CursorForward;
        end;
        CursorForward;
      end;
      '#':
      begin
        CursorForward;
        if Code[Cursor] = '$' then
          CursorForward;
        if not (Code[Cursor] in HexChars) then
          MakeError(SStringNotTerminated, ekFatalError);
        while Code[Cursor] in HexChars do
          CursorForward;
      end;
    end;
  end;

  TerminalParsed(tkStringCst, Copy(Code, BeginPos, Cursor-BeginPos));
  Result := True;
end;

{*
  Analyzes a single-line comment
  @return False
*}
function TSepiFunDelphiLexer.ActionSingleLineComment: Boolean;
begin
  while not (Code[Cursor] in [#0, #13, #10]) do
    CursorForward;

  NoTerminalParsed;
  Result := False;
end;

{*
  Analyzes a multi-line comment
  @return True for a pre-processor instruction, False otherwise
*}
function TSepiFunDelphiLexer.ActionMultiLineComment: Boolean;
begin
  if Code[Cursor] = '{' then
  begin
    while not (Code[Cursor] in [#0, '}']) do
      CursorForward;
    CursorForward
  end else
  begin
    while ((Code[Cursor] <> '*') or (Code[Cursor+1] <> ')')) and
      (Code[Cursor] <> #0) do
      CursorForward;
    CursorForward(2);
  end;

  NoTerminalParsed;
  Result := False;
end;

initialization
  if Length(SymbolClassNames) < LastTerminal+1 then
    SetLength(SymbolClassNames, LastTerminal+1);

  SymbolClassNames[tkEof] := 'tkEof';
  SymbolClassNames[tkIdentifier] := 'tkIdentifier';
  SymbolClassNames[tkInteger] := 'tkInteger';
  SymbolClassNames[tkFloat] := 'tkFloat';
  SymbolClassNames[tkStringCst] := 'tkStringCst';

  SymbolClassNames[tkOpenBracket] := 'tkOpenBracket';
  SymbolClassNames[tkCloseBracket] := 'tkCloseBracket';
  SymbolClassNames[tkOpenSqBracket] := 'tkOpenSqBracket';
  SymbolClassNames[tkCloseSqBracket] := 'tkCloseSqBracket';
  SymbolClassNames[tkEquals] := 'tkEquals';
  SymbolClassNames[tkComma] := 'tkComma';
  SymbolClassNames[tkColon] := 'tkColon';
  SymbolClassNames[tkSemiColon] := 'tkSemiColon';
  SymbolClassNames[tkDot] := 'tkDot';
  SymbolClassNames[tkRange] := 'tkRange';
  SymbolClassNames[tkHat] := 'tkHat';
  SymbolClassNames[tkAt] := 'tkAt';
  SymbolClassNames[tkAssign] := 'tkAssign';

  SymbolClassNames[tkUnit] := 'tkUnit';
  SymbolClassNames[tkUses] := 'tkUses';
  SymbolClassNames[tkConst] := 'tkConst';
  SymbolClassNames[tkResourceString] := 'tkResourceString';
  SymbolClassNames[tkVar] := 'tkVar';
  SymbolClassNames[tkOut] := 'tkOut';

  SymbolClassNames[tkProcedure] := 'tkProcedure';
  SymbolClassNames[tkFunction] := 'tkFunction';
  SymbolClassNames[tkPrivate] := 'tkPrivate';
  SymbolClassNames[tkPublic] := 'tkPublic';
  SymbolClassNames[tkForward] := 'tkForward';

  SymbolClassNames[tkPlugin] := 'tkPlugin';
  SymbolClassNames[tkObject] := 'tkObject';
  SymbolClassNames[tkField] := 'tkField';
  SymbolClassNames[tkEffect] := 'tkEffect';
  SymbolClassNames[tkTool] := 'tkTool';
  SymbolClassNames[tkObstacle] := 'tkObstacle';
  SymbolClassNames[tkComponents] := 'tkComponents';
  SymbolClassNames[tkAttributes] := 'tkAttributes';
  SymbolClassNames[tkActions] := 'tkActions';

  SymbolClassNames[tkConstructor] := 'tkConstructor';
  SymbolClassNames[tkDestructor] := 'tkDestructor';
  SymbolClassNames[tkZIndex] := 'tkZIndex';
  SymbolClassNames[tkImage] := 'tkImage';
  SymbolClassNames[tkAction] := 'tkAction';

  SymbolClassNames[tkPlus] := 'tkPlus';
  SymbolClassNames[tkMinus] := 'tkMinus';
  SymbolClassNames[tkTimes] := 'tkTimes';
  SymbolClassNames[tkDivide] := 'tkDivide';
  SymbolClassNames[tkDiv] := 'tkDiv';
  SymbolClassNames[tkMod] := 'tkMod';
  SymbolClassNames[tkShl] := 'tkShl';
  SymbolClassNames[tkShr] := 'tkShr';
  SymbolClassNames[tkOr] := 'tkOr';
  SymbolClassNames[tkAnd] := 'tkAnd';
  SymbolClassNames[tkXor] := 'tkXor';
  SymbolClassNames[tkNot] := 'tkNot';
  SymbolClassNames[tkLowerThan] := 'tkLowerThan';
  SymbolClassNames[tkLowerEq] := 'tkLowerEq';
  SymbolClassNames[tkGreaterThan] := 'tkGreaterThan';
  SymbolClassNames[tkGreaterEq] := 'tkGreaterEq';
  SymbolClassNames[tkNotEqual] := 'tkNotEqual';
  SymbolClassNames[tkIs] := 'tkIs';
  SymbolClassNames[tkAs] := 'tkAs';

  SymbolClassNames[tkBegin] := 'tkBegin';
  SymbolClassNames[tkEnd] := 'tkEnd';

  SymbolClassNames[tkString] := 'tkString';
  SymbolClassNames[tkNil] := 'tkNil';

  SymbolClassNames[tkIf] := 'tkIf';
  SymbolClassNames[tkThen] := 'tkThen';
  SymbolClassNames[tkElse] := 'tkElse';
  SymbolClassNames[tkWhile] := 'tkWhile';
  SymbolClassNames[tkDo] := 'tkDo';
  SymbolClassNames[tkRepeat] := 'tkRepeat';
  SymbolClassNames[tkUntil] := 'tkUntil';
  SymbolClassNames[tkFor] := 'tkFor';
  SymbolClassNames[tkTo] := 'tkTo';
  SymbolClassNames[tkDownTo] := 'tkDownTo';
  SymbolClassNames[tkCase] := 'tkCase';
  SymbolClassNames[tkOf] := 'tkOf';
  SymbolClassNames[tkTry] := 'tkTry';
  SymbolClassNames[tkExcept] := 'tkExcept';
  SymbolClassNames[tkOn] := 'tkOn';
  SymbolClassNames[tkFinally] := 'tkFinally';
  SymbolClassNames[tkRaise] := 'tkRaise';
  SymbolClassNames[tkInherited] := 'tkInherited';

  SymbolClassNames[tkCan] := 'tkCan';
  SymbolClassNames[tkHas] := 'tkHas';
  SymbolClassNames[tkAtKw] := 'tkAtKw';
  SymbolClassNames[tkLeast] := 'tkLeast';
  SymbolClassNames[tkMost] := 'tkMost';
  SymbolClassNames[tkMore] := 'tkMore';
  SymbolClassNames[tkLess] := 'tkLess';
  SymbolClassNames[tkThan] := 'tkThan';
  SymbolClassNames[tkExactly] := 'tkExactly';
  SymbolClassNames[tkReceives] := 'tkReceives';
  SymbolClassNames[tkDiscards] := 'tkDiscards';
end.

