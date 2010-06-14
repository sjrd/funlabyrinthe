{*
  Analyseur lexical d'une unit� FunDelphi
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
  LastTerminal = 101;

  tkEof = SepiLexerUtils.tkEof;         /// Fin de fichier
  tkBlank = SepiLexerUtils.tkBlank;     /// Lex�me blanc
  tkComment = SepiLexerUtils.tkComment; /// Lex�me commentaire

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

  tkPlugin = 31;       // plugin
  tkObject = 32;       // object
  tkField = 33;        // field
  tkEffect = 34;       // effect
  tkTool = 35;         // tool
  tkObstacle = 36;     // obstacle
  tkPosComponent = 37; // poscomponent
  tkVehicle = 38;      // vehicle
  tkComponents = 39;   // components
  tkAttributes = 40;   // attributes
  tkActions = 41;      // actions
  tkMessages = 42;     // messages

  tkMessage = 43; // message
  tkName = 44;    // name
  tkZIndex = 45;  // zindex
  tkImage = 46;   // image
  tkAction = 47;  // action

  tkPlus = 48;        // +
  tkMinus = 49;       // -
  tkTimes = 50;       // *
  tkDivide = 51;      // /
  tkDiv = 52;         // div
  tkMod = 53;         // mod
  tkShl = 54;         // shl
  tkShr = 55;         // shr
  tkOr = 56;          // or
  tkAnd = 57;         // and
  tkXor = 58;         // xor
  tkNot = 59;         // not
  tkLowerThan = 60;   // <
  tkLowerEq = 61;     // <=
  tkGreaterThan = 62; // >
  tkGreaterEq = 63;   // >=
  tkNotEqual = 64;    // <>
  tkIn = 65;          // in
  tkIs = 66;          // is
  tkAs = 67;          // as

  tkBegin = 68; // begin
  tkEnd = 69;   // end

  tkString = 70; // string
  tkNil = 71;    // nil

  tkIf = 72;        // if
  tkThen = 73;      // then
  tkElse = 74;      // else
  tkWhile = 75;     // while
  tkDo = 76;        // do
  tkRepeat = 77;    // repeat
  tkUntil = 78;     // until
  tkFor = 79;       // for
  tkTo = 80;        // to
  tkDownTo = 81;    // downto
  tkCase = 82;      // case
  tkOf = 83;        // of
  tkTry = 84;       // try
  tkExcept = 85;    // except
  tkOn = 86;        // on
  tkFinally = 87;   // finally
  tkRaise = 88;     // raise
  tkInherited = 89; // inherited
  tkWith = 90;      // with

  tkCan = 91;       // can
  tkHas = 92;       // has
  tkAtKw = 93;      // at
  tkLeast = 94;     // least
  tkMost = 95;      // most
  tkMore = 96;      // more
  tkLess = 97;      // less
  tkThan = 98;      // than
  tkExactly = 99;   // exactly
  tkReceives = 100; // receives
  tkDiscards = 101; // discards

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
          if Key = 'const'          then SymbolClass := tkConst;
    'd' : if Key = 'div'            then SymbolClass := tkDiv else
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
          if Key = 'in'             then SymbolClass := tkIn else
          if Key = 'inherited'      then SymbolClass := tkInherited else
          if Key = 'is'             then SymbolClass := tkIs;
    'l' : if Key = 'least'          then SymbolClass := tkLeast else
          if Key = 'less'           then SymbolClass := tkLess;
    'm' : if Key = 'message'        then SymbolClass := tkMessage else
          if Key = 'messages'       then SymbolClass := tkMessages else
          if Key = 'mod'            then SymbolClass := tkMod else
          if Key = 'more'           then SymbolClass := tkMore else
          if Key = 'most'           then SymbolClass := tkMost;
    'n' : if Key = 'name'           then SymbolClass := tkName else
          if Key = 'nil'            then SymbolClass := tkNil else
          if Key = 'not'            then SymbolClass := tkNot;
    'o' : if Key = 'object'         then SymbolClass := tkObject else
          if Key = 'obstacle'       then SymbolClass := tkObstacle else
          if Key = 'of'             then SymbolClass := tkOf else
          if Key = 'on'             then SymbolClass := tkOn else
          if Key = 'or'             then SymbolClass := tkOr else
          if Key = 'out'            then SymbolClass := tkOut;
    'p' : if Key = 'plugin'         then SymbolClass := tkPlugin else
          if Key = 'poscomponent'   then SymbolClass := tkPosComponent else
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
    'v' : if Key = 'var'            then SymbolClass := tkVar else
          if Key = 'vehicle'        then SymbolClass := tkVehicle;
    'w' : if Key = 'with'           then SymbolClass := tkWith else
          if Key = 'while'          then SymbolClass := tkWhile;
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
  while CharInSet(Code[Cursor], IdentChars) do
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
    until not CharInSet(Code[Cursor], HexChars);
  end else
  begin
    while CharInSet(Code[Cursor], NumberChars) do
      CursorForward;

    if (Code[Cursor] = '.') and CharInSet(Code[Cursor+1], NumberChars) then
    begin
      SymbolClass := tkFloat;
      repeat
        CursorForward;
      until not CharInSet(Code[Cursor], NumberChars);
    end;

    if CharInSet(Code[Cursor], ['e', 'E']) then
    begin
      SymbolClass := tkFloat;
      CursorForward;
      if CharInSet(Code[Cursor], ['+', '-']) then
        CursorForward;

      while CharInSet(Code[Cursor], NumberChars) do
        CursorForward;
    end;
  end;

  TerminalParsed(SymbolClass, Copy(Code, BeginPos, Cursor-BeginPos));
end;

{*
  Analyse une cha�ne de caract�res
  @return True
*}
procedure TSepiFunDelphiLexer.ActionString;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  while CharInSet(Code[Cursor], StringChars) do
  begin
    case Code[Cursor] of
      '''':
      begin
        CursorForward;
        while Code[Cursor] <> '''' do
        begin
          if CharInSet(Code[Cursor], [#0, #10, #13]) then
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
        if not CharInSet(Code[Cursor], HexChars) then
          MakeError(SStringNotTerminated, ekFatalError);
        while CharInSet(Code[Cursor], HexChars) do
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
  while not CharInSet(Code[Cursor], [#0, #13, #10]) do
    CursorForward;

  TerminalParsed(tkComment, Copy(Code, BeginPos, Cursor-BeginPos));
end;

{*
  Analyse un commentaire sur plusieurs lignes
  @return True pour une instruction du pr�-processuer, False sinon
*}
procedure TSepiFunDelphiLexer.ActionMultiLineComment;
var
  BeginPos: Integer;
begin
  BeginPos := Cursor;

  // Find end of comment
  if Code[Cursor] = '{' then
  begin
    while not CharInSet(Code[Cursor], [#0, '}']) do
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
  SymbolClassNames[tkPosComponent] := 'tkPosComponent';
  SymbolClassNames[tkVehicle] := 'tkVehicle';
  SymbolClassNames[tkComponents] := 'tkComponents';
  SymbolClassNames[tkAttributes] := 'tkAttributes';
  SymbolClassNames[tkActions] := 'tkActions';
  SymbolClassNames[tkMessages] := 'tkMessages';

  SymbolClassNames[tkMessage] := 'tkMessage';
  SymbolClassNames[tkName] := 'tkName';
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
  SymbolClassNames[tkIn] := 'tkIn';
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
  SymbolClassNames[tkWith] := 'tkWith';

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

