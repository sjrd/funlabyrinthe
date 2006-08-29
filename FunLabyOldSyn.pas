unit FunLabyOldSyn;

{$I SynEdit.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF SYN_CLX}
  QControls,
  QGraphics,
{$ELSE}
  Windows,
  Controls,
  Graphics,
{$ENDIF}
  SynEditTypes,
  SynEditHighlighter;

type
  TFLTokenKind = (tkCommand, tkComment, tkIdentifier, tkIfThenElse, tkKey,
                  tkNull, tkRemark, tkSection, tkSpace, tkString, tkSubCommand,
                  tkSubSection, tkSymbol, tkUnknown);

  TFLRangeState = (rsUnknown, rsComment);

  TProcTableProc = procedure of object;

type
  TFunLabyOldSyntax = class(TSynCustomHighlighter)
  private
    FLine : PChar;                                  // Ligne en cours
    FLineNumber : Integer;                          // N° de la ligne en cours
    FProcTable : array[#0..#255] of TProcTableProc;
      // Tableau de procédures pour gérer les « tokens »
    FRange : TFLRangeState;                      // RangeState
    Run : LongInt;                               // Index du caractère courant
    FTokenPos : Integer;                         // Position du dernier token
    FTokenID : TFLTokenKind;                     // Kind du dernier token
    FCommandAttri : TSynHighlighterAttributes;      // Attributs 'Command'
    FCommentAttri : TSynHighlighterAttributes;      // Attributs 'Comment'
    FIdentifierAttri : TSynHighlighterAttributes;   // Attributs 'Identifier'
    FIfThenElseAttri : TSynHighlighterAttributes;   // Attributs 'IfThenElse'
    FKeyAttri : TSynHighlighterAttributes;          // Attributs 'Key'
    FRemarkAttri : TSynHighlighterAttributes;       // Attributs 'Remark'
    FSectionAttri : TSynHighlighterAttributes;      // Attributs 'Section'
    FSpaceAttri : TSynHighlighterAttributes;        // Attributs 'Space'
    FStringAttri : TSynHighlighterAttributes;       // Attributs 'String'
    FSubCommandAttri : TSynHighlighterAttributes;   // Attributs 'SubCommand'
    FSubSectionAttri : TSynHighlighterAttributes;   // Attributs 'SubSection'
    FSymbolAttri : TSynHighlighterAttributes;       // Attributs 'Symbol'
    FUnknownAttri : TSynHighlighterAttributes;      // Attributs 'Unknown'
    function ReadIdent : string;
      // Lit le nom de l'identificateur qui suit
    function KeyComp(const AKey : string) : boolean;
      // Compare le mot qui suit avec AKey
    function KeyFunc : TFLTokenKind;
      // Compare le mot qui suit aux mots-clés
    procedure MakeMethodTables;                   // Initialize FProcTable
    procedure NullProc;                           // Procédure pour 'Null'
    procedure SpaceProc;                          // Procédure pour 'Space'
    procedure CRProc;                             // Procédure pour 'CR'        (Carbage Return)
    procedure LFProc;                             // Procédure pour 'LF'        (Line Feed)
    procedure CommentProc;                        // Procédure pour 'Comment'
    procedure SectionProc;                        // Procédure pour 'Section'
    procedure StringProc;                         // Procédure pour 'String'
    procedure SubSectionProc;                     // Procédure pour 'SubSection'
    procedure IdentProc;                          // Procédure pour les divers
    procedure SymbolProc;                         // Procédure pour 'Symbol'
    procedure UnknownProc;                        // Procédure pour inconnu
  protected
    function GetIdentChars : TSynIdentChars; override; // Caractères Identifiers
    function GetSampleSource : string; override;       // Exemple source
    function IsFilterStored : boolean; override;       // Filtre enregistrer ?
  public
    constructor Create(AOwner : TComponent); override; // Constructeur
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName : string; override;       // Nom du langage
    function GetRange : Pointer; override;             // RangeState
    procedure ResetRange; override;                    // Reset RangeState
    procedure SetRange(Value : Pointer); override;     // Set RangeState
    function GetDefaultAttribute(Index: integer):
      TSynHighlighterAttributes; override;             // Attr par défaut
    function GetEol : boolean; override;               // Fin de la ligne ?
    procedure SetLine(NewValue : string; LineNumber : Integer); override;
      // Change la ligne courante
    function GetToken : string; override;              // Dernier token
    function GetTokenID : TFLTokenKind;                // Kind du dernier token
    function GetTokenAttribute : TSynHighlighterAttributes; override;
      // Attributs du dernier token
    function GetTokenKind : integer; override;        // Kind du dernier token
    function GetTokenPos : integer; override;         // Pos du dernier token
    procedure Next; override;                         // Token suivant
  published
    property CommandAttri : TSynHighlighterAttributes
      read FCommandAttri write FCommandAttri;
    property CommentAttri : TSynHighlighterAttributes
      read FCommentAttri write FCommentAttri;
    property IdentifierAttri : TSynHighlighterAttributes
      read FIdentifierAttri write FIdentifierAttri;
    property IfThenElseAttri : TSynHighlighterAttributes
      read FIfThenElseAttri write FIfThenElseAttri;
    property KeyAttri : TSynHighlighterAttributes
      read FKeyAttri write FKeyAttri;
    property SectionAttri : TSynHighlighterAttributes
      read FSectionAttri write FSectionAttri;
    property SpaceAttri : TSynHighlighterAttributes
      read FSpaceAttri write FSpaceAttri;
    property StringAttri : TSynHighlighterAttributes
      read FStringAttri write FStringAttri;
    property SubCommandAttri : TSynHighlighterAttributes
      read FSubCommandAttri write FSubCommandAttri;
    property SubSectionAttri : TSynHighlighterAttributes
      read FSubSectionAttri write FSubSectionAttri;
    property SymbolAttri : TSynHighlighterAttributes
      read FSymbolAttri write FSymbolAttri;
    property UnknownAttri : TSynHighlighterAttributes
      read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SYNS_FilterFunLabyOld = 'Labyrinthes (*.lab)|*.lab';
  SYNS_LangFunLabyOld = 'FunLabyOld';
  SYNS_Command = 'Command';
  SYNS_IfThenElse = 'IfThenElse';
  SYNS_Remark = 'Remark';
  SYNS_Section = 'Section';
  SYNS_SubCommand = 'SubCommand';
  SYNS_SubSection = 'SubSection';
  SYNS_AttrUnknown = 'Unknown';

{$IFDEF SYN_COMPILER_3_UP}
const
{$ENDIF}
  SymbolsChars = ['[', ']', '=', '<', '>'];

constructor TFunLabyOldSyntax.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  WordBreakChars := SymbolsChars + [#0..#32];

  FCommandAttri := TSynHighlighterAttributes.Create(SYNS_Command);
  FCommandAttri.Style := [fsBold];
  AddAttribute(FCommandAttri);

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FIfThenElseAttri := TSynHighlighterAttributes.Create(SYNS_IfThenElse);
  FIfThenElseAttri.Style := [fsBold];
  AddAttribute(FIfThenElseAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FRemarkAttri := TSynHighlighterAttributes.Create(SYNS_Remark);
  FRemarkAttri.Style := [fsBold];
  FRemarkAttri.Foreground := clGreen;
  AddAttribute(FRemarkAttri);

  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_Section);
  FSectionAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FSectionAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FSubCommandAttri := TSynHighlighterAttributes.Create(SYNS_SubCommand);
  FSubCommandAttri.Style := [fsBold];
  AddAttribute(FSubCommandAttri);

  FSubSectionAttri := TSynHighlighterAttributes.Create(SYNS_SubSection);
  FSubSectionAttri.Style := [fsUnderline];
  AddAttribute(FSubSectionAttri);

  FSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknown);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  FDefaultFilter := SYNS_FilterFunLabyOld;
  FRange := rsUnknown;
end;

function TFunLabyOldSyntax.ReadIdent : string;
var Temp : PChar;
begin
  Temp := FLine+Run;
  Result := '';
  while not (Temp^ in WordBreakChars) do
  begin
    Result := Result + Temp^;
    inc(Temp);
  end;
end;

function TFunLabyOldSyntax.KeyComp(const AKey: String): Boolean;
var I : integer;
    Temp : PChar;
begin
  Temp := FLine+Run;
  Result := False;
  for I := 1 to Length(AKey) do
    if Temp^ <> AKey[I] then exit else inc(Temp);
  Result := Temp^ in WordBreakChars;
end;

function TFunLabyOldSyntax.KeyFunc : TFLTokenKind;
begin
  case FLine[Run] of
    'A' : if KeyComp('Aleatoire') then Result := tkSubCommand else
          if KeyComp('Alors') then Result := tkIfThenElse else
          if KeyComp('AllerA') then Result := tkCommand else
          if KeyComp('Arreter') then Result := tkCommand else
          if KeyComp('AutoriserPlanche') then Result := tkCommand else
          Result := tkIdentifier;
    'B' : if KeyComp('Bord') then Result := tkSubCommand else
          Result := tkIdentifier;
    'C' : if KeyComp('Case') then Result := tkSubCommand else
          if KeyComp('Choix') then Result := tkCommand else
          if KeyComp('Continuer') then Result := tkCommand else
          if KeyComp('Convertir') then Result := tkCommand else
          Result := tkIdentifier;
    'D' : if KeyComp('Decrementer') then Result := tkCommand else
          if KeyComp('Deplacer') then Result := tkCommand else
          if KeyComp('Derriere') then Result := tkSubCommand else
          if KeyComp('Desactiver') then Result := tkCommand else
          if KeyComp('Description') then Result := tkCommand else
          if KeyComp('Devant') then Result := tkSubCommand else
          Result := tkIdentifier;
    'E' : if KeyComp('Echec') then Result := tkCommand else
          if KeyComp('Et') then Result := tkIfThenElse else
          Result := tkIdentifier;
    'F' : if KeyComp('FinSi') then Result := tkIfThenElse else
          Result := tkIdentifier;
    'G' : if KeyComp('Gagner') then Result := tkCommand else
          Result := tkIdentifier;
    'I' : if KeyComp('Ici') then Result := tkSubCommand else
          if KeyComp('Impasse') then Result := tkCommand else
          if KeyComp('Incrementer') then Result := tkCommand else
          if KeyComp('Indice') then Result := tkCommand else
          Result := tkIdentifier;
    'L' : if KeyComp('LaisserPasser') then Result := tkCommand else
          Result := tkIdentifier;
    'M' : if KeyComp('Message') then Result := tkCommand else
          if KeyComp('Multiplier') then Result := tkCommand else
          Result := tkIdentifier;
    'O' : if KeyComp('Ou') then Result := tkIfThenElse else
          Result := tkIdentifier;
    'P' : if KeyComp('Precedent') then Result := tkSubCommand else
          if KeyComp('Poursuivre') then Result := tkCommand else
          Result := tkIdentifier;
    'R' : if KeyComp('Remarque') then
          begin
            Result := tkRemark;
            FRange := rsComment;
          end else
          if KeyComp('Remplacer') then Result := tkCommand else
          if KeyComp('Rien') then Result := tkSubCommand else
          Result := tkIdentifier;
    'S' : if KeyComp('Saute') then Result := tkCommand else
          if KeyComp('Si') then Result := tkIfThenElse else
          if KeyComp('Sinon') then Result := tkIfThenElse else
          if KeyComp('Son') then Result := tkCommand else
          if KeyComp('Suivant') then Result := tkSubCommand else
          if KeyComp('Stop') then Result := tkCommand else
          Result := tkIdentifier;
    'U' : if KeyComp('Unique') then Result := tkIfThenElse else
          Result := tkIdentifier;
    else Result := tkIdentifier;
  end;
end;

procedure TFunLabyOldSyntax.MakeMethodTables;
var I : Char;
begin
  for I := #0 to #255 do
    case I of
      #0  : FProcTable[I] := NullProc;
      #10 : FProcTable[I] := LFProc;
      #13 : FProcTable[I] := CRProc;
      '#' : FProcTable[I] := CommentProc;
      '[' : FProcTable[I] := SectionProc;
      '{' : FProcTable[I] := StringProc;
      '/' : FProcTable[I] := SubSectionProc;
      #1..#9, #11, #12, #14..#32 : FProcTable[I] := SpaceProc;
      '_', 'A'..'Z', 'a'..'z' : FProcTable[I] := IdentProc;
      else if I in SymbolsChars then FProcTable[I] := SymbolProc
                                else FProcTable[I] := UnknownProc;
    end;
end;

procedure TFunLabyOldSyntax.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TFunLabyOldSyntax.CRProc;
begin
  inc(Run);
  if FLine[Run] = #10 then inc(Run);
  FTokenID := tkSpace;
end;

procedure TFunLabyOldSyntax.LFProc;
begin
  inc(Run);
  FTokenID := tkSpace;
end;

procedure TFunLabyOldSyntax.SpaceProc;
begin
  repeat inc(Run) until not (FLine[Run] in [#1..#32]);
  FTokenID := tkSpace;
end;

procedure TFunLabyOldSyntax.CommentProc;
begin
  if (FRange <> rsComment) and (Run > 0) then UnknownProc else
  begin
    while not (FLine[Run] in [#0, #10, #13]) do inc(Run);
    FTokenID := tkComment;
  end;
end;

procedure TFunLabyOldSyntax.SectionProc;
begin
  if Run > 0 then SymbolProc else
  begin
    while not (FLine[Run] in [#0, #10, #13, ']']) do inc(Run);
    if FLine[Run] = ']' then inc(Run);
    FTokenID := tkSection;
  end;
end;

procedure TFunLabyOldSyntax.StringProc;
begin
  while not (FLine[Run] in [#0, #10, #13, '}']) do inc(Run);
  if FLine[Run] = '}' then inc(Run);
  FTokenID := tkString;
end;

procedure TFunLabyOldSyntax.SubSectionProc;
begin
  if Run > 0 then UnknownProc else
  begin
    while not (FLine[Run] in [#0, #10, #13]) do inc(Run);
    FTokenID := tkSubSection;
  end;
end;

procedure TFunLabyOldSyntax.IdentProc;
begin
  FTokenID := KeyFunc;
  inc(Run, Length(ReadIdent));
end;

procedure TFunLabyOldSyntax.SymbolProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TFunLabyOldSyntax.UnknownProc;
begin
  inc(Run, Length(ReadIdent));
  FTokenID := tkUnknown;
end;

function TFunLabyOldSyntax.GetIdentChars : TSynIdentChars;
begin
  Result := ['_', 'A'..'Z', 'a'..'z'];
end;

function TFunLabyOldSyntax.GetSampleSource: string;
begin
  Result := '# Mise en évidence de la syntaxe';
end;

function TFunLabyOldSyntax.IsFilterStored : boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterFunLabyOld;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TFunLabyOldSyntax.GetLanguageName : string;
begin
  Result := SYNS_LangFunLabyOld;
end;

function TFunLabyOldSyntax.GetRange : Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TFunLabyOldSyntax.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TFunLabyOldSyntax.SetRange(Value : Pointer);
begin
  FRange := TFLRangeState(Value);
end;

function TFunLabyOldSyntax.GetDefaultAttribute(Index : integer) : TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
    SYN_ATTR_SYMBOL     : Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TFunLabyOldSyntax.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

procedure TFunLabyOldSyntax.SetLine(NewValue: String; LineNumber: Integer);
begin
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  FRange := rsUnknown;
  Next;
end;

function TFunLabyOldSyntax.GetToken : string;
var Len : LongInt;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

function TFunLabyOldSyntax.GetTokenID : TFLTokenKind;
begin
  Result := FTokenId;
end;

function TFunLabyOldSyntax.GetTokenAttribute : TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkCommand : Result := FCommandAttri;
    tkComment : Result := FCommentAttri;
    tkIdentifier : Result := FIdentifierAttri;
    tkIfThenElse : Result := FIfThenElseAttri;
    tkKey : Result := FKeyAttri;
    tkRemark : Result := FRemarkAttri;
    tkSection : Result := FSectionAttri;
    tkSpace : Result := FSpaceAttri;
    tkString : Result := FStringAttri;
    tkSubCommand : Result := FSubCommandAttri;
    tkSubSection : Result := FSubSectionAttri;
    tkSymbol : Result := FSymbolAttri;
    tkUnknown : Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TFunLabyOldSyntax.GetTokenKind: integer;
begin
  Result := Ord(FTokenId);
end;

function TFunLabyOldSyntax.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TFunLabyOldSyntax.Next;
begin
  FTokenPos := Run;
  if FRange = rsComment then
  begin
    CommentProc;
    FRange := rsUnknown;
  end else FProcTable[FLine[Run]];
end;

initialization
  DecimalSeparator := '.';
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TFunLabyOldSyntax);
{$ENDIF}
end.
