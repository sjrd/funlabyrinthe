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
  LastTerminal = 96;

  tkEof = SepiLexerUtils.tkEof;         /// Fin de fichier
  tkBlank = SepiLexerUtils.tkBlank;     /// Lexème blanc
  tkComment = SepiLexerUtils.tkComment; /// Lexème commentaire

  tkIdentifier = 3; // Identifier
  tkInteger = 4;    // Integer
  tkFloat = 5;      // Floating point number
  tkStringCst = 6;  // String

  tkOpenBracket = 7;    // (
  tkCloseBracket = 8;   // )
  tkOpenSqBracket = 9;  // [
  tkCloseSqBracket = 10;// ]
  tkEquals = 11;        // =
  tkComma = 12;         // ,
  tkColon = 13;         // :
  tkSemiColon = 14;     // ;
  tkDot = 15;           // .
  tkRange = 16;         // ..
  tkHat = 17;           // ^
  tkAt = 18;            // @
  tkAssign = 19;        // :=

  tkUnit = 20;           // unit
  tkUses = 21;           // uses
  tkConst = 22;          // const
  tkResourceString = 23; // resourcestring
  tkVar = 24;            // var
  tkOut = 25;            // out

  tkProcedure = 26; // procedure
  tkFunction = 27;  // function
  tkPrivate = 28;   // private
  tkPublic = 29;    // public
  tkForward = 30;   // forward

  tkPlugin = 31;     // plugin
  tkObject = 32;     // object
  tkField = 33;      // field
  tkEffect = 34;     // effect
  tkTool = 35;       // tool
  tkObstacle = 36;   // obstacle
  tkComponents = 37; // components
  tkAttributes = 38; // attributes
  tkActions = 39;    // actions

  tkConstructor = 40; // constructor
  tkDestructor = 41;  // destructor
  tkZIndex = 42;      // zindex
  tkImage = 43;       // image
  tkAction = 44;      // action

  tkPlus = 45;        // +
  tkMinus = 46;       // -
  tkTimes = 47;       // *
  tkDivide = 48;      // /
  tkDiv = 49;         // div
  tkMod = 50;         // mod
  tkShl = 51;         // shl
  tkShr = 52;         // shr
  tkOr = 53;          // or
  tkAnd = 54;         // and
  tkXor = 55;         // xor
  tkNot = 56;         // not
  tkLowerThan = 57;   // <
  tkLowerEq = 58;     // <=
  tkGreaterThan = 59; // >
  tkGreaterEq = 60;   // >=
  tkNotEqual = 61;    // <>
  tkIs = 62;          // is
  tkAs = 63;          // as

  tkBegin = 64; // begin
  tkEnd = 65;   // end

  tkString = 66; // string
  tkNil = 67;    // nil

  tkIf = 68;        // if
  tkThen = 69;      // then
  tkElse = 70;      // else
  tkWhile = 71;     // while
  tkDo = 72;        // do
  tkRepeat = 73;    // repeat
  tkUntil = 74;     // until
  tkFor = 75;       // for
  tkTo = 76;        // to
  tkDownTo = 77;    // downto
  tkCase = 78;      // case
  tkOf = 79;        // of
  tkTry = 80;       // try
  tkExcept = 81;    // except
  tkOn = 82;        // on
  tkFinally = 83;   // finally
  tkRaise = 84;     // raise
  tkInherited = 85; // inherited

  tkCan = 86;      // can
  tkHas = 87;      // has
  tkAtKw = 88;     // at
  tkLeast = 89;    // least
  tkMost = 90;     // most
  tkMore = 91;     // more
  tkLess = 92;     // less
  tkThan = 93;     // than
  tkExactly = 94;  // exactly
  tkReceives = 95; // receives
  tkDiscards = 96; // discards

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

    procedure InitLexingProcs; override;

    procedure ActionSymbol;
    procedure ActionIdentifier;
    procedure ActionNumber;
    procedure ActionString;
    procedure ActionSingleLineComment;
    procedure ActionMultiLineComment;
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
procedure TSepiFunDelphiLexer.InitLexingProcs;
var
  C: Char;
begin
  inherited;

  for C := #0 to #255 do
  begin
    case C of
      '(', ')', '[', ']', '=', ',', ':', ';', '.',
        '^', '@', '+', '-', '*', '/', '<', '>':
        LexingProcs[C] := ActionSymbol;
      'a'..'z', 'A'..'Z', '_', '&':
        LexingProcs[C] := ActionIdentifier;
      '0'..'9', '$':
        LexingProcs[C] := ActionNumber;
      '''', '#':
        LexingProcs[C] := ActionString;
      '{':
        LexingProcs[C] := ActionMultiLineComment;
    end;
  end;
end;

{*
  Analyse un symbole ou un commentaire
  @return True pour un symbole, False pour un commentaire
*}
procedure TSepiFunDelphiLexer.ActionSymbol;
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
          ActionMultiLineComment;
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
          ActionSingleLineComment;
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
    Exit;
  end;

  TerminalParsed(SymbolClass, Repr);
end;

{*
  Analyse un identificateur
  @return True
*}
procedure TSepiFunDelphiLexer.ActionIdentifier;
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
end;

{*
  Analyse un nombre
  @return True
*}
procedure TSepiFunDelphiLexer.ActionNumber;
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
end;

{*
  Analyse une chaîne de caractères
  @return True
*}
procedure TSepiFunDelphiLexer.ActionString;
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
end;

{*
  Analyse un commentaire sur une ligne
  @return False
*}
procedure TSepiFunDelphiLexer.ActionSingleLineComment;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;
  while not (Code[Cursor] in [#0, #13, #10]) do
    CursorForward;

  TerminalParsed(tkComment, Copy(Code, BeginPos, Cursor-BeginPos));
end;

{*
  Analyse un commentaire sur plusieurs lignes
  @return True pour une instruction du pré-processuer, False sinon
*}
procedure TSepiFunDelphiLexer.ActionMultiLineComment;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  // Find end of comment
  if Code[Cursor] = '{' then
  begin
    while not (Code[Cursor] in [#0, '}']) do
      CursorForward;
    CursorForward;
  end else
  begin
    while ((Code[Cursor] <> '*') or (Code[Cursor+1] <> ')')) and
      (Code[Cursor] <> #0) do
      CursorForward;
    CursorForward(2);
  end;

  TerminalParsed(tkComment, Copy(Code, BeginPos, Cursor-BeginPos));
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

