unit Compatibility4xSyn;

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
  TTokenKind = (
    tkCommand, tkComment, tkIdentifier, tkIfThenElse, tkKey, tkNull, tkRemark,
    tkSection, tkSpace, tkString, tkSubCommand, tkSubSection, tkSymbol,
    tkUnknown
  );

  TRangeState = (rsUnknown, rsComment);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function(Index: Integer): TTokenKind of object;

type
  TCompatibility4xSyntax = class(TSynCustomHighlighter)
  private
    FProcTable: array[AnsiChar] of TProcTableProc; // Table d'indirection
    FRange: TRangeState;                           // RangeState
    FTokenID: TTokenKind;                          // Kind du dernier token

    FCommandAttri: TSynHighlighterAttributes;      // Attributs 'Command'
    FCommentAttri: TSynHighlighterAttributes;      // Attributs 'Comment'
    FIdentifierAttri: TSynHighlighterAttributes;   // Attributs 'Identifier'
    FIfThenElseAttri: TSynHighlighterAttributes;   // Attributs 'IfThenElse'
    FKeyAttri: TSynHighlighterAttributes;          // Attributs 'Key'
    FRemarkAttri: TSynHighlighterAttributes;       // Attributs 'Remark'
    FSectionAttri: TSynHighlighterAttributes;      // Attributs 'Section'
    FSpaceAttri: TSynHighlighterAttributes;        // Attributs 'Space'
    FStringAttri: TSynHighlighterAttributes;       // Attributs 'String'
    FSubCommandAttri: TSynHighlighterAttributes;   // Attributs 'SubCommand'
    FSubSectionAttri: TSynHighlighterAttributes;   // Attributs 'SubSection'
    FSymbolAttri: TSynHighlighterAttributes;       // Attributs 'Symbol'
    FUnknownAttri: TSynHighlighterAttributes;      // Attributs 'Unknown'

    function ReadIdent: UnicodeString;
      // Lit le nom de l'identificateur qui suit
    function KeyComp(const AKey: UnicodeString): Boolean;
      // Compare le mot qui suit avec AKey
    function KeyFunc: TTokenKind;
      // Compare le mot qui suit aux mots-clés

    procedure MakeMethodTables; // Initialize FProcTable
    procedure NullProc;         // Procédure pour 'Null'
    procedure SpaceProc;        // Procédure pour 'Space'
    procedure CRProc;           // Procédure pour 'CR'        (Carbage Return)
    procedure LFProc;           // Procédure pour 'LF'        (Line Feed)
    procedure CommentProc;      // Procédure pour 'Comment'
    procedure SectionProc;      // Procédure pour 'Section'
    procedure StringProc;       // Procédure pour 'String'
    procedure SubSectionProc;   // Procédure pour 'SubSection'
    procedure IdentProc;        // Procédure pour les divers
    procedure SymbolProc;       // Procédure pour 'Symbol'
    procedure UnknownProc;      // Procédure pour inconnu
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;

    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(
      Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: Integer): UnicodeString; override;
    function GetTokenID: TTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommandAttri: TSynHighlighterAttributes
      read FCommandAttri write FCommandAttri;
    property CommentAttri: TSynHighlighterAttributes
      read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes
      read FIdentifierAttri write FIdentifierAttri;
    property IfThenElseAttri: TSynHighlighterAttributes
      read FIfThenElseAttri write FIfThenElseAttri;
    property KeyAttri: TSynHighlighterAttributes
      read FKeyAttri write FKeyAttri;
    property SectionAttri: TSynHighlighterAttributes
      read FSectionAttri write FSectionAttri;
    property SpaceAttri: TSynHighlighterAttributes
      read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes
      read FStringAttri write FStringAttri;
    property SubCommandAttri: TSynHighlighterAttributes
      read FSubCommandAttri write FSubCommandAttri;
    property SubSectionAttri: TSynHighlighterAttributes
      read FSubSectionAttri write FSubSectionAttri;
    property SymbolAttri: TSynHighlighterAttributes
      read FSymbolAttri write FSymbolAttri;
    property UnknownAttri: TSynHighlighterAttributes
      read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

resourcestring
  SYNS_FilterCompatibility4x =
    'Fichiers d''actions de compatibilité 4.x (*.c4x)|*.c4x';
  SYNS_FriendlyLangCompatibility4x = 'Fichiers d''actions de compatibilité 4.x';
  SYNS_LangCompatibility4x = 'Compatibility4x';

  SYNS_Command = 'Command';
  SYNS_IfThenElse = 'IfThenElse';
  SYNS_Remark = 'Remark';
  SYNS_Section = 'Section';
  SYNS_SubCommand = 'SubCommand';
  SYNS_SubSection = 'SubSection';
  SYNS_AttrUnknown = 'Unknown';

  SYNS_FriendlyCommand = 'Command';
  SYNS_FriendlyIfThenElse = 'IfThenElse';
  SYNS_FriendlyRemark = 'Remark';
  SYNS_FriendlySection = 'Section';
  SYNS_FriendlySubCommand = 'SubCommand';
  SYNS_FriendlySubSection = 'SubSection';
  SYNS_FriendlyAttrUnknown = 'Unknown';

const
  SymbolsChars = ['[', ']', '=', '<', '>'];

constructor TCompatibility4xSyntax.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FCommandAttri := TSynHighlighterAttributes.Create(SYNS_Command,
    SYNS_FriendlyCommand);
  FCommandAttri.Style := [fsBold];
  AddAttribute(FCommandAttri);

  FCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment,
    SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier,
    SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FIfThenElseAttri := TSynHighlighterAttributes.Create(SYNS_IfThenElse,
    SYNS_FriendlyIfThenElse);
  FIfThenElseAttri.Style := [fsBold];
  AddAttribute(FIfThenElseAttri);

  FKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord,
    SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FRemarkAttri := TSynHighlighterAttributes.Create(SYNS_Remark,
    SYNS_FriendlyRemark);
  FRemarkAttri.Style := [fsBold];
  FRemarkAttri.Foreground := clGreen;
  AddAttribute(FRemarkAttri);

  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_Section,
    SYNS_FriendlySection);
  FSectionAttri.Style := [fsBold, fsUnderline];
  AddAttribute(FSectionAttri);

  FSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace,
    SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString,
    SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FSubCommandAttri := TSynHighlighterAttributes.Create(SYNS_SubCommand,
    SYNS_FriendlySubCommand);
  FSubCommandAttri.Style := [fsBold];
  AddAttribute(FSubCommandAttri);

  FSubSectionAttri := TSynHighlighterAttributes.Create(SYNS_SubSection,
    SYNS_FriendlySubSection);
  FSubSectionAttri.Style := [fsUnderline];
  AddAttribute(FSubSectionAttri);

  FSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol,
    SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknown,
    SYNS_FriendlyAttrUnknown);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  FDefaultFilter := SYNS_FilterCompatibility4x;
  FRange := rsUnknown;
end;

function TCompatibility4xSyntax.ReadIdent: UnicodeString;
var
  Temp: PWideChar;
begin
  Temp := FLine+Run;
  Result := '';
  while IsIdentChar(Temp^) do
  begin
    Result := Result + Temp^;
    Inc(Temp);
  end;
end;

function TCompatibility4xSyntax.KeyComp(const AKey: UnicodeString): Boolean;
var
  I: Integer;
  Temp: PWideChar;
begin
  Temp := FLine+Run;
  Result := False;

  for I := 1 to Length(AKey) do
  begin
    if Temp^ <> AKey[I] then
      Exit
    else
      Inc(Temp);
  end;

  Result := not IsIdentChar(Temp^);
end;

function TCompatibility4xSyntax.KeyFunc: TTokenKind;
begin
  case FLine[Run] of
    'A': if KeyComp('Aleatoire') then Result := tkSubCommand else
         if KeyComp('Alors') then Result := tkIfThenElse else
         if KeyComp('AllerA') then Result := tkCommand else
         if KeyComp('Arreter') then Result := tkCommand else
         if KeyComp('AutoriserPlanche') then Result := tkCommand else
         Result := tkIdentifier;
    'B': if KeyComp('Bord') then Result := tkSubCommand else
         Result := tkIdentifier;
    'C': if KeyComp('Case') then Result := tkSubCommand else
         if KeyComp('Choix') then Result := tkCommand else
         if KeyComp('Continuer') then Result := tkCommand else
         if KeyComp('Convertir') then Result := tkCommand else
         Result := tkIdentifier;
    'D': if KeyComp('Decrementer') then Result := tkCommand else
         if KeyComp('Deplacer') then Result := tkCommand else
         if KeyComp('Derriere') then Result := tkSubCommand else
         if KeyComp('Desactiver') then Result := tkCommand else
         if KeyComp('Description') then Result := tkCommand else
         if KeyComp('Devant') then Result := tkSubCommand else
         Result := tkIdentifier;
    'E': if KeyComp('Echec') then Result := tkCommand else
         if KeyComp('Et') then Result := tkIfThenElse else
         Result := tkIdentifier;
    'F': if KeyComp('FinSi') then Result := tkIfThenElse else
         Result := tkIdentifier;
    'G': if KeyComp('Gagner') then Result := tkCommand else
         Result := tkIdentifier;
    'I': if KeyComp('Ici') then Result := tkSubCommand else
         if KeyComp('Impasse') then Result := tkCommand else
         if KeyComp('Incrementer') then Result := tkCommand else
         if KeyComp('Indice') then Result := tkCommand else
         Result := tkIdentifier;
    'L': if KeyComp('LaisserPasser') then Result := tkCommand else
         Result := tkIdentifier;
    'M': if KeyComp('Message') then Result := tkCommand else
         if KeyComp('Multiplier') then Result := tkCommand else
         Result := tkIdentifier;
    'O': if KeyComp('Ou') then Result := tkIfThenElse else
         Result := tkIdentifier;
    'P': if KeyComp('Precedent') then Result := tkSubCommand else
         if KeyComp('Poursuivre') then Result := tkCommand else
         Result := tkIdentifier;
    'R': if KeyComp('Remarque') then
         begin
           Result := tkRemark;
           FRange := rsComment;
         end else
         if KeyComp('Remplacer') then Result := tkCommand else
         if KeyComp('Rien') then Result := tkSubCommand else
         Result := tkIdentifier;
    'S': if KeyComp('Saute') then Result := tkCommand else
         if KeyComp('Si') then Result := tkIfThenElse else
         if KeyComp('Sinon') then Result := tkIfThenElse else
         if KeyComp('Son') then Result := tkCommand else
         if KeyComp('Suivant') then Result := tkSubCommand else
         if KeyComp('Stop') then Result := tkCommand else
         Result := tkIdentifier;
    'U': if KeyComp('Unique') then Result := tkIfThenElse else
         Result := tkIdentifier;
  else
    Result := tkIdentifier;
  end;
end;

procedure TCompatibility4xSyntax.MakeMethodTables;
var
  I: AnsiChar;
begin
  for I := Low(AnsiChar) to High(AnsiChar) do
    case I of
      #0:  FProcTable[I] := NullProc;
      #10: FProcTable[I] := LFProc;
      #13: FProcTable[I] := CRProc;
      '#': FProcTable[I] := CommentProc;
      '[': FProcTable[I] := SectionProc;
      '{': FProcTable[I] := StringProc;
      '/': FProcTable[I] := SubSectionProc;

      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := SpaceProc;

      '_', 'A'..'Z', 'a'..'z':
        FProcTable[I] := IdentProc;
    else
      if I in SymbolsChars then
        FProcTable[I] := SymbolProc
      else
        FProcTable[I] := UnknownProc;
    end;
end;

procedure TCompatibility4xSyntax.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TCompatibility4xSyntax.CRProc;
begin
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
  FTokenID := tkSpace;
end;

procedure TCompatibility4xSyntax.LFProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
end;

procedure TCompatibility4xSyntax.SpaceProc;
begin
  Inc(Run);
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do
    Inc(Run);
  FTokenID := tkSpace;
end;

procedure TCompatibility4xSyntax.CommentProc;
begin
  if (FRange <> rsComment) and (Run > 0) then
    UnknownProc
  else
  begin
    while not IsLineEnd(Run) do
      Inc(Run);
    FTokenID := tkComment;
  end;
end;

procedure TCompatibility4xSyntax.SectionProc;
begin
  if Run > 0 then
    SymbolProc
  else
  begin
    while not IsLineEnd(Run) and (FLine[Run] <> ']') do
      Inc(Run);
    if FLine[Run] = ']' then
      Inc(Run);
    FTokenID := tkSection;
  end;
end;

procedure TCompatibility4xSyntax.StringProc;
begin
  while not IsLineEnd(Run) and (FLine[Run] <> '}') do
    Inc(Run);
  if FLine[Run] = '}' then
    Inc(Run);
  FTokenID := tkString;
end;

procedure TCompatibility4xSyntax.SubSectionProc;
begin
  if Run > 0 then
    UnknownProc
  else
  begin
    while not IsLineEnd(Run) do
      Inc(Run);
    FTokenID := tkSubSection;
  end;
end;

procedure TCompatibility4xSyntax.IdentProc;
begin
  FTokenID := KeyFunc;
  Inc(Run, Length(ReadIdent));
end;

procedure TCompatibility4xSyntax.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TCompatibility4xSyntax.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

function TCompatibility4xSyntax.GetSampleSource: string;
begin
  Result := '# Mise en évidence de la syntaxe';
end;

function TCompatibility4xSyntax.IsFilterStored: boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterCompatibility4x;
end;

class function TCompatibility4xSyntax.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangCompatibility4x;
end;

class function TCompatibility4xSyntax.GetLanguageName: string;
begin
  Result := SYNS_LangCompatibility4x;
end;

function TCompatibility4xSyntax.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TCompatibility4xSyntax.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TCompatibility4xSyntax.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TCompatibility4xSyntax.GetDefaultAttribute(
  Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT:    Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD:    Result := fKeyAttri;
    SYN_ATTR_STRING:     Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL:     Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TCompatibility4xSyntax.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TCompatibility4xSyntax.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := '';
end;

function TCompatibility4xSyntax.GetTokenID: TTokenKind;
begin
  Result := FTokenId;
end;

function TCompatibility4xSyntax.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkCommand: Result := FCommandAttri;
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkIfThenElse: Result := FIfThenElseAttri;
    tkKey: Result := FKeyAttri;
    tkRemark: Result := FRemarkAttri;
    tkSection: Result := FSectionAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSubCommand: Result := FSubCommandAttri;
    tkSubSection: Result := FSubSectionAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TCompatibility4xSyntax.GetTokenKind: integer;
begin
  Result := Ord(FTokenId);
end;

function TCompatibility4xSyntax.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TCompatibility4xSyntax.Next;
begin
  FTokenPos := Run;

  if FRange = rsComment then
  begin
    CommentProc;
    FRange := rsUnknown;
  end else if FLine[Run] <= High(AnsiChar) then
    FProcTable[AnsiChar(FLine[Run])]
  else
    UnknownProc;

  inherited;
end;

end.

