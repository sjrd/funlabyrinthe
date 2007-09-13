{*
  Interpr�tation des actions
  L'unit� C4xInterpreter d�crit la classe TActionsInterpreter, qui se charge
  d'interpr�ter les actions.
  @author sjrd
  @version 5.0
*}
unit C4xInterpreter;

interface

uses
  SysUtils, Classes, Graphics, StrUtils, Contnrs, ScUtils, ScStrUtils,
  ScWindows, SdDialogs, FunLabyUtils, FilesUtils, MapTools, FLBFields,
  FLBSimpleObjects, FLBPlank, FLBBoat, FLBCommon, C4xComponents, C4xScrewsTable,
  C4xCommon;

resourcestring
  sIfStatCannotMixAndOr = 'Ne peut m�langer les Et et les Ou';

  sIfStatMustBeIfAndThen = 'Une instruction Si doit avoir un Si et un Sinon';
  sIfStatBadOrder = 'Les op�rateurs doivent dans l''ordre Si-Alors-Sinon-FinSi';
  sIfStatInvalidThenClause = 'Clause Alors invalide';
  sIfStatInvalidElseClause = 'Clause Sinon invalide';

  sInvalidParameter = 'Param�tre de commande invalide';
  sInvalidCommand = 'Commande inconnue';
  sInvalidRandomParameter = 'Param�tre de Aleatoire invalide';
  sBadNextPreviousRandom = 'Mauvais argument de sous-commande %s';

  sUnknownLabel = 'Le label %s n''a pas pu �tre trouv�';

const
  /// Version de l'interpr�teur d'actions
  InterpreterVersion = 50;

type
  {*
    G�n�r�e lorsqu'une action est invalide
    @author sjrd
    @version 5.0
  *}
  EInvalidAction = class(Exception);

  {*
    G�n�r�e lorsqu'une instruction Si est invalide
    @author sjrd
    @version 5.0
  *}
  EInvalidIf = class(EInvalidAction);

  {*
    G�n�r�e lorsqu'un param�tre d'une action est invalide
    @author sjrd
    @version 5.0
  *}
  EBadParam = class(EInvalidAction);

  /// Type de modification d'une r�f�rence � une variable enti�re
  TModificationKind = (mkSet, mkAdd, mkSubstract, mkMultiply);

  {*
    Interpr�te des actions
    @author sjrd
    @version 5.0
  *}
  TActionsInterpreter = class
  private
    Counter: PInteger;    /// Pointeur vers le compteur des actions courantes
    Actions: TStrings;    /// Actions � ex�cuter
    Master: TMaster;      /// Ma�tre FunLabyrinthe
    Infos: TC4xInfos;     /// Infos C4x
    Phase: Integer;       /// Phase courante (phPushing ou phExecute)
    Player: TPlayer;      /// Joueur concern�
    PlayerPos: T3DPoint;  /// Position du joueur
    Map: TMap;            /// Carte courante
    KeyPressed: Boolean;  /// True si une touche a �t� press�e
    Position: T3DPoint;   /// Position de la case
    DoNextPhase: Boolean; /// Indique s'il faut ex�cuter la phase suivante
    HasMoved: Boolean;    /// Indique si un AllerA a �t� fait
    HasShownMsg: Boolean; /// Indique si un message a �t� affich�
    Successful: Boolean;  /// �tat de r�ussite
    Inactive: TScrew;     /// Case � utiliser lors d'un Desactiver
    AllowPlank: Boolean;  /// Indique si un AutoriserPlanche a �t� fait

    StrHere: string;   /// Case courante
    StrBefore: string; /// Case devant
    StrBehind: string; /// Case derri�re
    Answer: Integer;   /// R�ponse d'un Choix
    Boat: Integer;     /// Num�ro de la barque qu'a le joueur

    ReferencesStrings: array of string; /// Tableau des r�f�rences

    procedure TreatVariables(var Line: string);

    function EvalBool(var Condition: string): Boolean;
    function EvalBoolExpression(var Condition: string): Boolean;
    procedure TreatIfStatement(var Line: string);

    procedure ModifyReference(Reference, Value: Integer;
      ModificationKind: TModificationKind);
    procedure ConvertScrews(FromScrew, ToScrew: TScrew);

    function Information(var Params: string; const Title: string;
      DlgType: TDialogType;
      DlgButtons: TDialogButtons = dbOK): TDialogResult;

    procedure ReplaceCmd    (var Params: string);
    procedure ConvertCmd    (var Params: string);
    procedure MoveCmd       (var Params: string);
    procedure DeactivateCmd (var Params: string);
    procedure IncrementCmd  (var Params: string);
    procedure DecrementCmd  (var Params: string);
    procedure MultiplyCmd   (var Params: string);
    procedure MessageCmd    (var Params: string);
    procedure TipCmd        (var Params: string);
    procedure FailureCmd    (var Params: string);
    procedure BlindAlleyCmd (var Params: string);
    procedure ChoiceCmd     (var Params: string);
    procedure DescriptionCmd(var Params: string);
    procedure WinCmd        (var Params: string);
    procedure SoundCmd      (var Params: string);
    procedure GoToCmd       (var Params: string);
    procedure LetPassCmd    (var Params: string);
    procedure AllowPlankCmd (var Params: string);
    procedure DontGoOnCmd   (var Params: string);
    procedure GoOnCmd       (var Params: string);

    procedure ExecuteActions;
  public
    constructor Create;

    class procedure Execute(ACounter: PInteger; AActions: TStrings;
      AMaster: TMaster; APhase: Integer; APlayer: TPlayer;
      AKeyPressed: Boolean; const APos: T3DPoint; var ADoNextPhase: Boolean;
      out AHasMoved, AHasShownMsg, ASuccessful: Boolean;
      const AInactive: TComponentID);
  end;

implementation

{ Don't localize any of the strings in this implementation! }

const
  cBegin       = 0;  /// D�but des commandes
  cReplace     = 0;  /// Commande Remplacer
  cConvert     = 1;  /// Commande Convertir
  cMove        = 2;  /// Commande Deplacer
  cDeactivate  = 3;  /// Commande Desactiver
  cIncrement   = 4;  /// Commande Incrementer
  cDecrement   = 5;  /// Commande Decrementer
  cMultiply    = 6;  /// Commande Multiplier
  cMessage     = 7;  /// Commande Message
  cTip         = 8;  /// Commande Indice
  cFailure     = 9;  /// Commande Echec
  cBlindAlley  = 10; /// Commande Impasse
  cChoice      = 11; /// Commande Choix
  cDescription = 12; /// Commande Description
  cWin         = 13; /// Commande Gagner
  cSound       = 14; /// Commande Son
  cGoTo        = 15; /// Commande AllerA
  cLetPass     = 16; /// Commande LaisserPasser
  cAllowPlank  = 17; /// Commande AutoriserPlanche
  cDontGoOn    = 18; /// Commande Arreter
  cGoOn        = 19; /// Commande Poursuivre
  cContinue    = 20; /// Commande Continuer
  cClassicEnd  = 20; /// Fin des commandes classiques
  cJump        = 21; /// Commande Saute
  cStop        = 22; /// Commande Stop
  cRemark      = 23; /// Remarque
  cEnd         = 23; /// Fin des commandes

  /// Tableaux des cha�nes de commandes
  CommandStrings: array[cBegin..cEnd] of string = (
    'Remplacer', 'Convertir', 'Deplacer', 'Desactiver', 'Incrementer',
    'Decrementer', 'Multiplier', 'Message', 'Indice', 'Echec', 'Impasse',
    'Choix', 'Description', 'Gagner', 'Son', 'AllerA', 'LaisserPasser',
    'AutoriserPlanche', 'Arreter', 'Poursuivre', 'Continuer', 'Saute', 'Stop',
    'Remarque'
  );

  srBegin         = 0;  /// D�but des r�f�rences simples
  srCounter       = 0;  /// R�f�rence � Compteur
  srBuoy          = 1;  /// R�f�rence � Bouee
  srBuoys         = 2;  /// R�f�rence � Bouees
  srPlank         = 3;  /// R�f�rence � Planche
  srPlanks        = 4;  /// R�f�rence � Planches
  srSilverKeys    = 5;  /// R�f�rence � ClesArgent
  srGoldenKeys    = 6;  /// R�f�rence � ClesOr
  srColor         = 7;  /// R�f�rence � Couleur
  srTemporization = 8;  /// R�f�rence � Temporisation
  srX             = 9;  /// R�f�rence � X
  srY             = 10; /// R�f�rence � Y
  srZ             = 11; /// R�f�rence � Z
  srDirection     = 12; /// R�f�rence � Direction
  srSuccess       = 13; /// R�f�rence � Reussite
  srEndCounter    = 14; /// R�f�rence au compteur du dehors
  srEnd           = 14; /// Fin des r�f�rences simples

  /// Tableaux des r�f�rences simples
  SimpleReferencesStrings: array[srBegin..srEnd] of string = (
    '&Compteur', '&Bouee', '&Bouees', '&Planche', '&Planches', '&ClesArgent',
    '&ClesOr', '&Couleur', '&Temporisation', '&X', '&Y', '&Z', '&Direction',
    '&Reussite', '&CompteurFin'
  );

  /// D�but des r�f�rences indic�es aux boutons
  irButtonsBegin = srEnd + 1;
  /// Fin des r�f�rences indic�es aux boutons
  irButtonsEnd = irButtonsBegin + 45 - 1;

  /// D�but des r�f�rences indic�es aux t�l�porteurs
  irTransportersBegin = irButtonsEnd + 1;
  /// Fin des r�f�rences indic�es aux t�l�porteurs
  irTransportersEnd = irTransportersBegin + 30 - 1;

  /// D�but des r�f�rences indic�es aux variables
  irVariablesBegin = irTransportersEnd + 1;
  /// Fin des r�f�rences indic�es aux variables
  irVariablesEnd = irVariablesBegin + MaxVar - 1;

  /// D�but des r�f�rences indic�es aux compteurs des actions
  irActionsCounterBegin = irVariablesEnd + 1;

  EndCounterIndex = 76;          /// Index du compteur de fin
  ButtonCounterOffset = 1;       /// D�calage des index des boutons
  TransporterCounterOffset = 46; /// D�calage des index des t�l�porteurs

  /// Nom des couleurs
  ColorStrings: array[0..6] of string = (
    'Bleu', 'Rouge', 'Vert', 'Jaune', 'Noir', 'Blanc', 'Invisible'
  );
  /// Valeur des couleurs
  ColorValues: array[0..6] of Integer = (
    clBlue, clRed, clLime, clYellow, clBlack, clWhite, clTransparent
  );

type
  {*
    Type d'une proc�dure qui recherche une sous-cha�ne dans un cha�ne
    @param SubStr   Sous-cha�ne � rechercher
    @param Str      Cha�ne dans laquelle chercher SubStr
    @return Index de la premi�re occurence de SubStr dans Str (0 si non trouv�e)
  *}
  TPosProc = function(const SubStr, Str: string): Integer;

  {*
    Type d'une proc�dure qui remplace toutes les occurences d'une variable par
    sa valeur
    @param Str        Cha�ne dans laquelle remplacer les variables
    @param Variable   Variable � remplacer
    @param Value      Valeur enti�re de la variable
  *}
  TReplaceVarProc = procedure(var Str: string; const Variable: string;
    Value: Integer);

  {*
    Type proc�dural correspondant aux m�thodes d'ex�cution de commande
    @param Self     R�f�rence � l'objet courant
    @param Params   Param�tres de la commande
  *}
  TCommandProc = procedure(Self: TObject; var Params: string);

var
  /// Tableau des m�thodes d'ex�cution de commande
  CommandProcs: array[cBegin..cClassicEnd] of TCommandProc;

{-------------------}
{ Routines globales }
{-------------------}

{*
  Recherche une variable hors d'une cha�ne dans une instruction
  Une variable doit �tre entour�e d'espaces ou de crochets, et ne pas se trouver
  entre une paire d'accolades, pour �tre reconnue comme telle.
  @param Variable   Variable � chercher
  @param Str        Cha�ne dans laquelle chercher Variable
  @return Index de la premi�re occurence de Variable dans Str (0 si non trouv�e)
*}
function PosNonStrVariable(const Variable, Str: string): Integer;
var
  Str2: string;
  ReplaceBrackets: Boolean;
  I, Len: Integer;
begin
  Str2 := ' '+Str+' ';
  ReplaceBrackets := Pos('[', Variable) = 0;
  I := 2;
  Len := Length(Str2);
  while I < Len do
  begin
    case Str2[I] of
      '[', ']':
      begin
        if ReplaceBrackets then
          Str2[I] := ' ';
        Inc(I);
      end;
      '{':
      begin
        while (I < Len) and (Str2[I] <> '}') do
        begin
          Str2[I] := ' ';
          Inc(I);
        end;
      end;
    else
      Inc(I);
    end;
  end;

  Result := Pos(' '+Variable+' ', Str2);
end;

{*
  Recherche une variable dans une cha�ne d'une instruction
  Une variable dans une cha�ne est englob�e dans des accolades.
  @param Variable   Variable � chercher
  @param Str        Cha�ne dans laquelle chercher Variable
  @return Index de la premi�re occurence de Variable dans Str (0 si non trouv�e)
*}
function PosStrVariable(const Variable, Str: string): Integer;
var
  Str2: string;
  I, Len: Integer;
begin
  Str2 := '}'+Str;

  I := 1;
  Len := Length(Str2);
  while I <= Len do
  begin
    case Str2[I] of
      '}':
      begin
        while (I <= Len) and (Str2[I] <> '{') do
        begin
          Str2[I] := ' ';
          Inc(I);
        end;
      end;
    else
      Inc(I);
    end;
  end;

  Result := Pos(Variable, Str2);
  if Result > 0 then
    Dec(Result);
end;

{*
  Remplace toutes les occurences d'une sous-cha�ne par une autre
  @param Str        Cha�ne � modifier
  @param FromText   Texte � chercher
  @param ToText     Texte � ins�rer � la place de FromText
  @param PosProc    Routine de recherche � utiliser
*}
procedure GenericReplace(var Str: string; const FromText, ToText: string;
  PosProc: TPosProc);
var
  Len, Index: Integer;
begin
  Len := Length(FromText);
  repeat
    Index := PosProc(FromText, Str);
    if Index > 0 then
    begin
      Delete(Str, Index, Len);
      Insert(ToText, Str, Index);
    end;
  until Index = 0;
end;

{*
  Remplace toutes les occurences d'une sous-cha�ne donn�e par une autre
  @param Str        Cha�ne dans laquelle remplacer les sous-cha�ne
  @param FromText   Sous-cha�ne � remplacer
  @param ToText     Texte � ins�rer � la place de FromText
*}
procedure Replace(var Str: string; const FromText, ToText: string);
begin
  GenericReplace(Str, FromText, ToText, Pos);
end;

{*
  Remplace les occurences d'une variable par sa valeur (hors cha�nes)
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur de la variable
  @see PosNonStrVariable
*}
procedure ReplaceNonStrVariable(var Str: string;
  const Variable, Value: string); overload;
begin
  GenericReplace(Str, Variable, Value, PosNonStrVariable);
end;

{*
  Remplace les occurences d'une variable par sa valeur enti�re (hors cha�nes)
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur enti�re de la variable
  @see PosNonStrVariable
*}
procedure ReplaceNonStrVariable(var Str: string; const Variable: string;
  Value: Integer); overload;
begin
  GenericReplace(Str, Variable, IntToStr(Value), PosNonStrVariable);
end;

{*
  Remplace les occurences d'une variable par sa valeur (en cha�nes)
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur de la variable
  @see PosStrVariable
*}
procedure ReplaceStrVariable(var Str: string;
  const Variable, Value: string); overload;
begin
  GenericReplace(Str, Variable, Value, PosStrVariable);
end;

{*
  Remplace les occurences d'une variable par sa valeur enti�re (en cha�nes)
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur enti�re de la variable
  @see PosStrVariable
*}
procedure ReplaceStrVariable(var Str: string; const Variable: string;
  Value: Integer); overload;
begin
  GenericReplace(Str, Variable, IntToStr(Value), PosStrVariable);
end;

{*
  Remplace les occurences d'une variable par sa valeur (partout)
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur de la variable
  @see ReplaceNonStrVariable
  @see ReplaceStrVariable
*}
procedure ReplaceVariable(var Str: string;
  const Variable, Value: string); overload;
begin
  ReplaceNonStrVariable(Str, Variable, Value);
  ReplaceStrVariable(Str, '$'+Variable, Value);
end;

{*
  Remplace les occurences d'une variable par sa valeur enti�re (partout)
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur enti�re de la variable
  @see ReplaceNonStrVariable
  @see ReplaceStrVariable
*}
procedure ReplaceVariable(var Str: string; const Variable: string;
  Value: Integer); overload;
begin
  ReplaceVariable(Str, Variable, IntToStr(Value));
end;

{*
  Passe outre les espaces en d�but de cha�ne
  @param Params   Liste des param�tres
  @return Index du premier non-espace
*}
function SkipSpaces(const Params: string): Integer;
begin
  Result := 1;
  while (Result <= Length(Params)) and (Params[Result] = ' ') do
    Inc(Result);
end;

{*
  Avance jusqu'au prochain caract�re sp�cifi�
  @param Params    Liste des param�tres
  @param StartAt   Index � partir duquel commencer la recherche
  @param C         Caract�re � rechercher
  @return Index du premier caract�re C donn�
*}
function ToNextChar(const Params: string; StartAt: Integer;
  C: Char): Integer;
begin
  Result := StartAt;
  while (Result <= Length(Params)) and (Params[Result] <> C) do
    Inc(Result);
end;

{*
  R�cup�re une commande dans la liste des param�tres
  @param Params           Liste des param�tres
  @param Commands         Liste des sous-commandes valides
  @param RaiseException   D�clencher une exception si sous-commande invalide
  @return Sous-commande lue, ou -1 si la sous-commande est invalide
  @throws EBadParam : Le premier param�tre n'est pas une sous-commande valide
*}
function GetCommandIndex(var Params: string;
  const Commands: array of string;
  RaiseException: Boolean = True): Integer;
var
  StartIndex, EndIndex: Integer;
begin
  StartIndex := SkipSpaces(Params);
  EndIndex := ToNextChar(Params, StartIndex, ' ');

  Result := AnsiIndexStr(
    Copy(Params, StartIndex, EndIndex-StartIndex), Commands);

  if Result >= 0 then
    Delete(Params, 1, EndIndex-1)
  else if RaiseException then
    raise EBadParam.Create(sInvalidCommand);
end;

{*
  R�cup�re un param�tre de type entier dans la liste des param�tres
  @param Params   Liste des param�tres
  @return Valeur enti�re lue
  @throws EBadParam : Le type du premier param�tre n'est pas un entier
*}
function GetIntParam(var Params: string): Integer;
var
  ColorIndex: Integer;
  IsRandom: Boolean;
  StartIndex, EndIndex: Integer;
begin
  ColorIndex := GetCommandIndex(Params, ColorStrings, False);
  if ColorIndex >= 0 then
  begin
    Result := ColorValues[ColorIndex];
    Exit;
  end;

  IsRandom := GetCommandIndex(Params, ['EntierAleatoire'], False) = 0;

  StartIndex := SkipSpaces(Params);
  EndIndex := ToNextChar(Params, StartIndex, ' ');

  try
    try
      Result := StrToInt(Copy(Params, StartIndex, EndIndex-StartIndex));

      if IsRandom then
      begin
        if Result > 0 then
          Result := Random(Result)
        else
          raise EBadParam.Create(sInvalidRandomParameter);
      end;

      Delete(Params, 1, EndIndex-1);
    except
      on Error: EConvertError do
        raise EBadParam.Create(Error.Message);
    end;
  except
    if IsRandom then
      Params := 'EntierAleatoire' + Params;
    raise;
  end;
end;

{*
  R�cup�re un param�tre de type cha�ne dans la liste des param�tres
  @param Params   Liste des param�tres
  @return Valeur cha�ne lue
  @throws EBadParam : Le type du premier param�tre n'est pas une cha�ne
*}
function GetStringParam(var Params: string): string;
var
  Len, StartIndex, EndIndex: Integer;
begin
  if GetCommandIndex(Params, ['Rien'], False) = 0 then
  begin
    Result := '';
    Exit;
  end;

  Len := Length(Params);
  StartIndex := SkipSpaces(Params);
  if (StartIndex > Len) or (Params[StartIndex] <> '{') then
    raise EBadParam.Create(sInvalidParameter);
  Inc(StartIndex);

  EndIndex := ToNextChar(Params, StartIndex, '}');
  if EndIndex > Len then
    raise EBadParam.Create(sInvalidParameter);

  Result := AnsiReplaceStr(
    Copy(Params, StartIndex, EndIndex-StartIndex), '\', #10);
  Delete(Params, 1, EndIndex);
end;

{*
  R�cup�re une r�f�rence � une case dans la liste des param�tres
  Attention ! GetScrewReference avance sur les sous-param�tres au fur et �
  mesure : si le premier sous-param�tre est bien Case, mais que les suivants
  sont incorrects, l'entr�e sera corrompue. Il faut n'appeler GetScrewReference
  qu'en dernier recours.
  @param Params        Liste des param�tres
  @param NoneAllowed   Indique si la r�f�rence Rien est admise
  @return R�f�rence � la case lue
  @throws EBadParam : Le type du premier param�tre n'est pas une case
*}
function GetScrewReference(var Params: string;
  NoneAllowed: Boolean = False): T3DPoint;
begin
  if NoneAllowed and (GetCommandIndex(Params, ['Rien'], False) = 0) then
    Result := No3DPoint
  else
  begin
    GetCommandIndex(Params, ['Case']);
    Result.X := GetIntParam(Params);
    Result.Y := GetIntParam(Params);
    Result.Z := GetIntParam(Params);
  end;
end;

{*
  R�cup�re un param�tre de type case dans la liste des param�tres
  @param Params   Liste des param�tres
  @param Master   Ma�tre FunLabyrinthe
  @return Valeur case lue
  @throws EBadParam : Le type du premier param�tre n'est pas une case
*}
function GetScrewParam(var Params: string; Map: TMap): TScrew;
var
  Master: TMaster;
  StartIndex, EndIndex: Integer;
  Param: string;
begin
  Master := Map.Master;
  StartIndex := SkipSpaces(Params);

  if (StartIndex < Length(Params)) and (Params[StartIndex] = '[') then
  begin
    // Case r�f�renc�e par une position de la carte ou par son caract�re
    Inc(StartIndex);
    EndIndex := ToNextChar(Params, StartIndex, ']');
    Param := Copy(Params, StartIndex, EndIndex-StartIndex);
    if Length(Param) = 1 then
      Result := Master.Screw[ScrewsTable[Param[1]]]
    else
      Result := Map[GetScrewReference(Param)];
    Delete(Params, 1, EndIndex);
  end else
  begin
    // Case donn�e par son caract�re
    try
      Result := Master.Screw[ScrewsTable[Params[StartIndex]]];
      Delete(Params, 1, StartIndex);
    except
      on Error: EComponentNotFound do
        raise EBadParam.Create(Error.Message);
    end;
  end;
end;

{----------------------------}
{ Classe TActionsInterpreter }
{----------------------------}

{*
  Cr�e une instance de TActionsInterpreter
*}
constructor TActionsInterpreter.Create;
begin
  inherited Create;

  Counter := nil;
  Actions := nil;
  Master := nil;
  Phase := 0;
  Player := nil;
  Map := nil;
  KeyPressed := False;
  Position := No3DPoint;
  DoNextPhase := False;
  HasMoved := False;
  HasShownMsg := False;
  Inactive := nil;
  AllowPlank := False;

  StrHere := '';
  StrBefore := '';
  StrBehind := '';
  Answer := 0;
end;

{*
  Traite les valeurs de variables dans l'instruction
  @param Line   Instruction � traiter
*}
procedure TActionsInterpreter.TreatVariables(var Line: string);

  {*
    Traite toutes les sous-expressions de type Suivant/Precedent/Aleatoire
    @param Line       Instruction � traiter
    @param Kind       Nom de la sous-instruction � traiter
    @param FindProc   Routine de recherche de la case � utiliser
  *}
  procedure TreatNextPreviousRandom(var Line: string; const Kind: string;
    FindProc: TFindScrewProc);
  var
    Len, ExprStart, IDPos: Integer;
    ID: TComponentID;
    Dest: T3DPoint;
  begin
    Len := Length(Kind);
    while True do
    begin
      ExprStart := PosNonStrVariable(Kind, Line);
      if ExprStart = 0 then
        Break;

      IDPos := ExprStart+Len+1;
      while (IDPos <= Length(Line)) and (Line[IDPos] in [' ', '[', ']']) do
        Inc(IDPos);

      if IDPos > Length(Line) then
        raise EInvalidAction.CreateFmt(sBadNextPreviousRandom, [Kind]);

      ID := ScrewsTable[Line[IDPos]];
      Dest := Position;
      FindProc(Map, Dest, Master.ScrewComponent[ID]);

      Delete(Line, ExprStart, IDPos-ExprStart+1);
      Insert('Case ' + Point3DToString(Dest), Line, ExprStart);
    end;
  end;

  {*
    Remplace toutes les occurences de compteurs index�s
    @param Prefix          Pr�fixe � rechercher
    @param Count           Nombre de compteurs
    @param IndexOffset     Index du premier compteur tel qu'�crit
    @param ActionsOffset   Index du premier compteur dans les actions
    @param Replace         Routine de remplacement � utiliser
  *}
  procedure ReplaceIndexedCounter(const Pattern: string;
    Count, IndexOffset, ActionsOffset: Integer; ReplaceVar: TReplaceVarProc);
  var
    I: Integer;
  begin
    if Pos(GetFirstToken(Pattern, '%'), Line) > 0 then
    begin
      for I := Count-1 downto 0 do
      begin
        ReplaceVar(Line, Format(Pattern, [I + IndexOffset]),
          Infos.Actions[I + ActionsOffset].Counter);
      end;
    end;
  end;

var
  I: Integer;
begin
  ReplaceNonStrVariable(Line, 'Ici', StrHere);
  ReplaceNonStrVariable(Line, 'Devant', StrBefore);
  ReplaceNonStrVariable(Line, 'Derriere', StrBehind);

  TreatNextPreviousRandom(Line, 'Suivant', FindNextScrew);
  TreatNextPreviousRandom(Line, 'Precedent', FindPreviousScrew);
  TreatNextPreviousRandom(Line, 'Aleatoire', FindScrewAtRandom);

  ReplaceNonStrVariable(Line, 'X'        , PlayerPos.X);
  ReplaceNonStrVariable(Line, 'Y'        , PlayerPos.Y);
  ReplaceNonStrVariable(Line, 'Z'        , PlayerPos.Z);
  ReplaceNonStrVariable(Line, 'Direction', Integer(Player.Direction));
  ReplaceNonStrVariable(Line, 'Reponse'  , Answer);
  ReplaceNonStrVariable(Line, 'Reussite' , Integer(Successful));
  ReplaceNonStrVariable(Line, 'Touche'   , Integer(KeyPressed));
  ReplaceNonStrVariable(Line, 'Phase'    , Phase);

  with Master.ObjectDef[idBuoys] do
  begin
    ReplaceVariable(Line, 'Bouees', Count[Player]);
    ReplaceNonStrVariable(Line, 'Bouee', Count[Player]);
  end;
  with Master.ObjectDef[idPlanks] do
  begin
    ReplaceVariable(Line, 'Planches', Count[Player]);
    ReplaceNonStrVariable(Line, 'Planche', Count[Player]);
  end;
  with Master.ObjectDef[idSilverKeys] do
    ReplaceVariable(Line, 'ClesArgent', Count[Player]);
  with Master.ObjectDef[idGoldenKeys] do
    ReplaceVariable(Line, 'ClesOr', Count[Player]);
  ReplaceNonStrVariable(Line, 'Barque', Boat);

  ReplaceNonStrVariable(Line, 'Version', InterpreterVersion);
  ReplaceNonStrVariable(Line, 'Couleur', Player.Color);

  ReplaceIndexedCounter('CompteurActions_%d',
    Infos.ActionsCount, 0, 0, ReplaceVariable);
  ReplaceIndexedCounter('CompteurBouton_%d',
    45, 1, ButtonCounterOffset, ReplaceVariable);
  ReplaceIndexedCounter('CompteurTeleporteur_%d',
    30, 1, TransporterCounterOffset, ReplaceVariable);

  if Pos('Variable_', Line) > 0 then
  begin
    for I := Infos.ActionsCount-1 downto 0 do
      ReplaceVariable(Line, Format('Variable_%d', [I]), Infos.Variables[I]);
  end;

  ReplaceVariable(Line, 'CompteurFin', Infos.Actions[EndCounterIndex].Counter);
  ReplaceVariable(Line, 'Compteur', Counter^);

  ReplaceIndexedCounter('[CompteurActions %d]',
    Infos.ActionsCount, 0, 0, ReplaceNonStrVariable);
  ReplaceIndexedCounter('[CompteurBouton %d]',
    45, 1, ButtonCounterOffset, ReplaceNonStrVariable);
  ReplaceIndexedCounter('[CompteurTeleporteur %d]',
    30, 1, TransporterCounterOffset, ReplaceNonStrVariable);

  if Pos('Variable ', Line) > 0 then
  begin
    for I := 0 to Infos.ActionsCount-1 do
    begin
      ReplaceNonStrVariable(Line, Format('[Variable %d]', [I]),
        Infos.Variables[I]);
    end;
  end;

  Replace(Line, '&CompteurActions ', '&CompteurActions_');
  Replace(Line, '&CompteurBouton ', '&CompteurBouton_');
  Replace(Line, '&CompteurTeleporteur ', '&CompteurTeleporteur_');
end;

{*
  �value une condition form�e d'un seul bool�en
  @param Condition   Condition � �valuer
  @return R�sultat bool�en de l'�valuation
*}
function TActionsInterpreter.EvalBool(var Condition: string): Boolean;
var
  Operation: Integer;
  IntOp1, IntOp2: Integer;
begin
  Result := False;

  try
    IntOp1 := GetIntParam(Condition);
  except
    IntOp1 := AnsiIndexText(GetScrewParam(Condition, Map).ID, ScrewsTable);
  end;

  Operation := GetCommandIndex(Condition,
    ['=', '<>', '<', '>', 'DivisiblePar']);

  try
    IntOp2 := GetIntParam(Condition);
  except
    IntOp2 := AnsiIndexText(GetScrewParam(Condition, Map).ID, ScrewsTable);
  end;

  case Operation of
    0: Result := IntOp1 =  IntOp2;
    1: Result := IntOp1 <> IntOp2;
    2: Result := IntOp1 <  IntOp2;
    3: Result := IntOp1 >  IntOp2;
    4: Result := IntOp1 mod IntOp2 = 0;
  end;
end;

{*
  �value une condition form�e de plusieurs bool�ens li�s par des Et ou des Ou
  @param Condition   Condition � �valuer
  @return R�sultat bool�en de l'�valuation
*}
function TActionsInterpreter.EvalBoolExpression(
  var Condition: string): Boolean;
var
  Operation: (ckAnd, ckOr);
  StrOp, SimpleCond: string;
  OpPos: Integer;
begin
  // V�rifier qu'il y a soit des Et, soit des Ou, mais pas les deux
  if PosNonStrVariable('Et', Condition) > 0 then
  begin
    Operation := ckAnd;
    StrOp := 'Et';
  end else
  begin
    Operation := ckOr;
    StrOp := 'Ou';
  end;
  if (Operation = ckAnd) and (PosNonStrVariable('Ou', Condition) > 0) then
    raise EInvalidIf.Create(sIfStatCannotMixAndOr);

  // Boucle sur les �valuation successives
  Result := Operation = ckOr;
  repeat
    OpPos := PosNonStrVariable(StrOp, Condition);
    if OpPos = 0 then
      OpPos := Length(Condition) + 1;

    SimpleCond := Trim(Copy(Condition, 1, OpPos-1));
    Delete(Condition, 1, OpPos+1);
    Condition := TrimLeft(Condition);

    if EvalBool(SimpleCond) = Result then
      Exit;
  until Condition = '';
  Result := not Result;
end;

{*
  Traite une �ventuelle condition dans l'instruction
  @param Line   Instruction � traiter
*}
procedure TActionsInterpreter.TreatIfStatement(var Line: string);
var
  IfPos, ThenPos, ElsePos, EndIfPos: Integer;
  BeforeIf, Condition, TrueStatement, FalseStatement, AfterIf: string;
begin
  // R�cup�ration des positions des op�rateurs du Si
  IfPos    := PosNonStrVariable('Si'   , Line);
  ThenPos  := PosNonStrVariable('Alors', Line);
  ElsePos  := PosNonStrVariable('Sinon', Line);
  EndIfPos := PosNonStrVariable('FinSi', Line);

  // Contr�le de la pr�sence correcte de Si et de Alors
  if (IfPos = 0) and (ThenPos = 0) and (ElsePos = 0) and (EndIfPos = 0) then
    Exit;
  if (IfPos = 0) or (ThenPos = 0) then
    raise EInvalidIf.Create(sIfStatMustBeIfAndThen);

  // Les op�rateurs Sinon et FinSi sont optionnels
  if ElsePos = 0 then
    ElsePos := Length(Line) + 1;
  if EndIfPos = 0 then
    EndIfPos := Length(Line) + 2;

  // Contr�le de l'ordre des op�rateurs
  if (IfPos >= ThenPos) or (ThenPos >= ElsePos) or (ElsePos >= EndIfPos) then
    raise EInvalidIf.Create(sIfStatBadOrder);

  // S�paration du Si en ses sous-expression
  BeforeIf       := Trim(Copy(Line,            1, IfPos              - 1));
  Condition      := Trim(Copy(Line, IfPos    + 2, ThenPos  - IfPos   - 2));
  TrueStatement  := Trim(Copy(Line, ThenPos  + 5, ElsePos  - ThenPos - 5));
  FalseStatement := Trim(Copy(Line, ElsePos  + 5, EndIfPos - ElsePos - 5));
  AfterIf        := Trim(Copy(Line, EndIfPos + 5, MaxInt));

  // Contr�le de l'int�grit� des instructions pour vrai et faux
  if (TrueStatement = '') or (TrueStatement[1] <> '[') or
    (TrueStatement[Length(TrueStatement)] <> ']') then
    raise EInvalidIf.Create(sIfStatInvalidThenClause);
  if (FalseStatement <> '') and ((FalseStatement[1] <> '[') or
    (FalseStatement[Length(FalseStatement)] <> ']')) then
    raise EInvalidIf.Create(sIfStatInvalidElseClause);

  // Suppression des crochets des instructions pour vrai et faux
  TrueStatement := Trim(Copy(TrueStatement, 2, Length(TrueStatement)-2));
  if FalseStatement <> '' then
    FalseStatement := Trim(Copy(FalseStatement, 2, Length(FalseStatement)-2));

  // Evaluation de la condition et modification de la ligne en cons�quence
  if EvalBoolExpression(Condition) then
    Line := TrueStatement
  else
    Line := FalseStatement;

  // Ajout des �ventuels bouts d'instruction avant et apr�s
  if BeforeIf <> '' then
    Line := BeforeIf + ' ' + Line;
  if AfterIf <> '' then
    Line := Line + ' ' + AfterIf;
end;

{*
  Modifie un entier r�f�renc�
  @param Reference          R�f�rence � l'entier � modifier
  @param Value              Valeur de modification
  @param ModificationKind   Type de modification � effectuer
*}
procedure TActionsInterpreter.ModifyReference(Reference, Value: Integer;
  ModificationKind: TModificationKind);

  {*
    Transforme la valeur lue selon le type de modification � effectuer
    @param OldValue   Ancienne valeur
    @return Valeur transform�e
  *}
  function TransformValue(OldValue: Integer): Integer;
  begin
    case ModificationKind of
      mkAdd:       Result := OldValue + Value;
      mkSubstract: Result := OldValue - Value;
      mkMultiply:  Result := OldValue * Value;
    else
      Result := Value;
    end;
  end;

var
  ActionsIndex: Integer;
begin
  ActionsIndex := -1;

  case Reference of
    srCounter: Counter^ := TransformValue(Counter^);

    srBuoy, srBuoys:
      with Master.ObjectDef[idBuoys] do
        Count[Player] := TransformValue(Count[Player]);
    srPlank, srPlanks:
      with Master.ObjectDef[idPlanks] do
        Count[Player] := TransformValue(Count[Player]);
    srSilverKeys:
      with Master.ObjectDef[idSilverKeys] do
        Count[Player] := TransformValue(Count[Player]);
    srGoldenKeys:
      with Master.ObjectDef[idGoldenKeys] do
        Count[Player] := TransformValue(Count[Player]);

    srColor: Player.Color := TransformValue(Player.Color);
    srTemporization:
      Master.Temporization := TransformValue(Master.Temporization);

    srX, srY, srZ:
    begin
      case Reference of
        srX: PlayerPos.X := TransformValue(PlayerPos.X);
        srY: PlayerPos.Y := TransformValue(PlayerPos.Y);
        srZ: PlayerPos.Z := TransformValue(PlayerPos.Z);
      end;
      HasMoved := True;
    end;

    srDirection:
      Player.Direction := TDirection(TransformValue(Integer(Player.Direction)));

    srSuccess: { TODO 1 : Modification de Reussite };

    srEndCounter: ActionsIndex := EndCounterIndex;

    irButtonsBegin..irButtonsEnd:
      ActionsIndex := Reference-irButtonsBegin+ButtonCounterOffset;

    irTransportersBegin..irTransportersEnd:
      ActionsIndex := Reference-irButtonsBegin+TransporterCounterOffset;

    irVariablesBegin..irVariablesEnd:
      Infos.Variables[Reference-irVariablesBegin+1] :=
        TransformValue(Infos.Variables[Reference-irVariablesBegin+1]);

  else
    ActionsIndex := Reference-irActionsCounterBegin;
  end;

  if ActionsIndex >= 0 then
    Infos.Actions[ActionsIndex].Counter :=
      TransformValue(Infos.Actions[ActionsIndex].Counter);
end;

{*
  Convertit toutes les occurences d'une case en une autre
  @param FromScrew   Case � rechercher
  @param ToScrew     Case de remplacement
*}
procedure TActionsInterpreter.ConvertScrews(FromScrew, ToScrew: TScrew);
var
  X, Y, Z: Integer;
begin
  for X := 0 to Map.Dimensions.X-1 do
    for Y := 0 to Map.Dimensions.Y-1 do
      for Z := 0 to Map.Dimensions.Z-1 do
        if Map[Point3D(X, Y, Z)] = FromScrew then
          Map[Point3D(X, Y, Z)] := ToScrew;
end;

{*
  Affiche une bo�te de dialogue dont le texte est r�cup�r� en param�tre
  @param Params       Param�tres de la commande
  @param Title        Titre de la bo�te de dialogue
  @param DlgType      Type de bo�te de dialogue
  @param DlgButtons   Boutons
  @return Code de r�sultat du bouton cliqu�
*}
function TActionsInterpreter.Information(var Params: string;
  const Title: string; DlgType: TDialogType;
  DlgButtons: TDialogButtons = dbOK): TDialogResult;
var
  Text: string;
begin
  Text := GetStringParam(Params);
  if Text = '' then
    Result := drOK
  else
    Result := Player.ShowDialog(Title, Text, DlgType, DlgButtons);
  HasShownMsg := True;
end;

{*
  Commande 'Remplacer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.ReplaceCmd(var Params: string);
var
  Reference, Floor, Index: Integer;
  Screw: TScrew;
  ScrewRef: T3DPoint;
  Replacements: TObjectList;
begin
  Reference := GetCommandIndex(Params, ReferencesStrings, False);

  // R�f�rence � un entier
  if Reference >= 0 then
    ModifyReference(Reference, GetIntParam(Params), mkSet)
  else
  // Bord
  if GetCommandIndex(Params, ['Bord'], False) = 0 then
  begin
    Floor := 1;
    repeat
      Screw := GetScrewParam(Params, Map);
      Map.Outside[Floor] := Screw;
      Inc(Floor);
    until Params = '';

    while Floor < Map.Dimensions.Z do
    begin
      Map.Outside[Floor] := Screw;
      Inc(Floor);
    end;
  end else

  // Case
  begin
    ScrewRef := GetScrewReference(Params);
    Replacements := TObjectList.Create(False);
    try
      repeat
        Replacements.Add(GetScrewParam(Params, Map));
      until Params = '';

      if Replacements.Count = 1 then
        Map[ScrewRef] := TScrew(Replacements.First)
      else
      begin
        Index := Replacements.IndexOf(Map[ScrewRef]);
        if Index >= 0 then
        begin
          Map[ScrewRef] :=
            TScrew(Replacements[(Index + 1) mod Replacements.Count]);
        end;
      end;
    finally
      Replacements.Free;
    end;
  end;
end;

{*
  Commande 'Convertir'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.ConvertCmd(var Params: string);
var
  FromScrew, ToScrew: TScrew;
begin
  FromScrew := GetScrewParam(Params, Map);
  ToScrew := GetScrewParam(Params, Map);
  ConvertScrews(FromScrew, ToScrew);
end;

{*
  Commande 'Deplacer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.MoveCmd(var Params: string);
var
  Screw, Replacement: TScrew;
  ScrewRef: T3DPoint;
begin
  Screw := GetScrewParam(Params, Map);
  ScrewRef := GetScrewReference(Params, True);

  if Copy(Screw.ID, 1, Length(idGroundWater)) = idGroundWater then
    Replacement := Master.Screw[idWaterScrew]
  else
    Replacement := Master.Screw[idGrassScrew];
  ConvertScrews(Screw, Replacement);

  if not IsNo3DPoint(ScrewRef) then
    Map[ScrewRef] := Screw;
end;

{*
  Commande 'Desactiver'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DeactivateCmd(var Params: string);
var
  Screw: TScrew;
begin
  if Params = '' then
    Screw := Inactive
  else
    Screw := GetScrewParam(Params, Map);

  Map[Position] := Screw;
end;

{*
  Commande 'Incrementer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.IncrementCmd(var Params: string);
var
  Reference, Value: Integer;
begin
  Reference := GetCommandIndex(Params, ReferencesStrings);
  if Params = '' then
    Value := 1
  else
    Value := GetIntParam(Params);
  ModifyReference(Reference, Value, mkAdd);
end;

{*
  Commande 'Decrementer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DecrementCmd(var Params: string);
var
  Reference, Value: Integer;
begin
  Reference := GetCommandIndex(Params, ReferencesStrings);
  if Params = '' then
    Value := 1
  else
    Value := GetIntParam(Params);
  ModifyReference(Reference, Value, mkSubstract);
end;

{*
  Commande 'Multiplier'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.MultiplyCmd(var Params: string);
begin
  ModifyReference(GetCommandIndex(Params, ReferencesStrings),
    GetIntParam(Params), mkMultiply);
end;

{*
  Commande 'Message'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.MessageCmd(var Params: string);
begin
  Information(Params, sMessage, dtInformation);
end;

{*
  Commande 'Indice'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.TipCmd(var Params: string);
begin
  if Infos.ShowTips then
    Information(Params, sTip, dtWarning);
end;

{*
  Commande 'Echec'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.FailureCmd(var Params: string);
begin
  Information(Params, sFailure, dtError);
end;

{*
  Commande 'Impasse'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.BlindAlleyCmd(var Params: string);
begin
  if KeyPressed then
    Information(Params, sBlindAlley, dtError);
end;

{*
  Commande 'Choix'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.ChoiceCmd(var Params: string);
var
  DlgButtons: TDialogButtons;
begin
  case GetCommandIndex(Params,
      ['Oui-Non', 'Oui-Non-Annuler', 'OK-Annuler']) of
    0: DlgButtons := dbYesNo;
    1: DlgButtons := dbYesNoCancel;
  else
    DlgButtons := dbOKCancel;
  end;

  case Information(Params, sChoice, dtConfirmation, DlgButtons) of
    drNo: Answer := 0;
    drOK, drYes: Answer := 1;
    drCancel: Answer := 2;
  end;
end;

{*
  Commande 'Description'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DescriptionCmd(var Params: string);
var
  Text: string;
begin
  with Infos.MasterFile do
  begin
    Text := Description;
    if Difficulty <> '' then
      Text := Text + #10#10'Difficult� : ' + Difficulty;
    if Author <> '' then
      Text := Text + #10#10'Auteur : ' + Author;

    Player.ShowDialog(Title, Text);
  end;
end;

{*
  Commande 'Gagner'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.WinCmd(var Params: string);
begin
  Player.Win;
  if Params <> '' then
    Information(Params, sWon, dtInformation);
end;

{*
  Commande 'Son'
  @param Params   Param�tres de la commande
  @throws EBadParam : Le son en param�tre n'a pas pu �tre jou�
*}
{$WARNINGS OFF}
procedure TActionsInterpreter.SoundCmd(var Params: string);
var
  Sound: string;
begin
  Sound := GetStringParam(Params);
  if Sound = 'Information' then
    Sound := 'SystemAsterisk'
  else if Sound = 'Question' then
    Sound := 'SystemQuestion'
  else if Sound = 'Danger' then
    Sound := 'SystemExclamation'
  else if Sound = 'Erreur' then
    Sound := 'SystemHand';

  try
    if not ExecuteSound(Sound, stSysSound) then
      ExecuteSound(Infos.MasterFile.ResolveHRef(Sound, fSoundsDir));
  except
    on Error: EFileError do
      raise EBadParam.Create(Error.Message);
  end;
end;
{$WARNINGS ON}

{*
  Commande 'AllerA'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.GoToCmd(var Params: string);
var
  ScrewRef: T3DPoint;
begin
  ScrewRef := GetScrewReference(Params, True);
  if not IsNo3DPoint(ScrewRef) then
    PlayerPos := ScrewRef;
  HasMoved := True;
end;

{*
  Commande 'LaisserPasser'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.LetPassCmd(var Params: string);
begin
  PlayerPos := Position;
  Successful := True;
end;

{*
  Commande 'AutoriserPlanche'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.AllowPlankCmd(var Params: string);
begin
  if Phase = phPushing then
    AllowPlank := True;
end;

{*
  Commande 'Arreter'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DontGoOnCmd(var Params: string);
begin
  DoNextPhase := False;
end;

{*
  Commandes 'Poursuivre' et 'Continuer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.GoOnCmd(var Params: string);
begin
  DoNextPhase := True;
end;

{*
  Ex�cute les actions dans le contexte donn�
*}
procedure TActionsInterpreter.ExecuteActions;
var
  Current: Integer;
  Line: string;
  IntCommand, Index: Integer;
begin
  Current := 0;
  while Current < Actions.Count do
  try
    Line := Trim(Actions[Current]);
    Inc(Current);

    // Lignes de commentaires � �liminer
    if (Line = '') or (Line[1] = '#') or (GetXWord(Line, 1) = 'Remarque') then
      Continue;

    // Traitement des valeurs des variables
    TreatVariables(Line);

    // Traitement de l'�ventuelle condition
    TreatIfStatement(Line);

    // Traitement d'un �ventuel Unique
    if GetXWord(Line, 1) = 'Unique' then
    begin
      if Counter^ <> 1 then
        Continue
      else
        Delete(Line, 1, 7);
    end;

    // Les conditions peuvent avoir rendu la ligne vide
    if Line = '' then
      Continue;

    // Ex�cution de la commande
    IntCommand := GetCommandIndex(Line, CommandStrings);
    case IntCommand of
      cBegin..cClassicEnd: CommandProcs[IntCommand](Self, Line);
      cJump:
      begin
        Index := Actions.IndexOf('#'+Trim(Line));
        if Index >= 0 then
          Current := Index;
      end;
      cStop: Break;
      cRemark: ;
    end;
  except
    on Error: Exception do
    begin
      ShowDialog(Error.ClassName,
        Actions[Current-1]+#10+Error.Message, dtError);
      if not (Error is EInvalidAction) then
        Exit;
    end;
  end;

  if AllowPlank and Same3DPoint(PlayerPos, Position) and (not Successful) and
    (Master.ObjectDef[idPlanks].Count[Player] > 0) then
  begin
    TPlankScrew.Create(Master, Map, Position, Player);
    Master.Temporize;

    Successful := True;
    DoNextPhase := True;
  end;

  if HasMoved and (not Same3DPoint(Player.Position, PlayerPos)) then
  begin
    Master.Temporize;
    Player.MoveTo(PlayerPos);
    if DoNextPhase then
      Player.Direction := diNone;
  end;
end;

{*
  Ex�cute une liste d'actions
  @param ACounter       Pointeur vers le compteur des actions courantes
  @param AActions       Actions � ex�cuter
  @param AMaster        Ma�tre FunLabyrinthe
  @param APhase         Phase courante (phPushing ou phExecute)
  @param APlayer        Joueur concern�
  @param AKeyPressed    True si une touche a �t� press�e pour le d�placement
  @param APos           Position de la case
  @param ADoNextPhase   Indique s'il faut ex�cuter la phase suivante
  @param AHasMoved      Indique si un AllerA a �t� fait
  @param AHasShownMsg   Indique si un message a �t� affich�
  @param ASuccessful    Indique si la manoeuvre est r�ussie
  @parma AInactive      Effet � utiliser lors d'un Desactiver
*}
class procedure TActionsInterpreter.Execute(ACounter: PInteger;
  AActions: TStrings; AMaster: TMaster; APhase: Integer; APlayer: TPlayer;
  AKeyPressed: Boolean; const APos: T3DPoint; var ADoNextPhase: Boolean;
  out AHasMoved, AHasShownMsg, ASuccessful: Boolean;
  const AInactive: TComponentID);
var
  PluginIDs: TStrings;
  I: Integer;
begin
  with Create do
  try
    Counter := ACounter;
    Actions := AActions;
    Master := AMaster;
    Infos := Master.Component[idC4xInfos] as TC4xInfos;
    Phase := APhase;
    Player := APlayer;
    PlayerPos := APos;
    Map := Player.Map;
    KeyPressed := AKeyPressed;
    Position := APos;
    DoNextPhase := ADoNextPhase;
    Inactive := Master.Screw[idGrass+'-'+AInactive+'--'];

    // D�termination des Ici, Devant et Derriere
    StrHere := 'Case '+Point3DToString(Position);
    StrBefore := 'Case '+
      Point3DToString(PointBehind(Position, NegDir[Player.Direction]));
    StrBehind := 'Case '+
      Point3DToString(PointBehind(Position, Player.Direction));

    // D�termination du num�ro de la barque
    PluginIDs := TStringList.Create;
    try
      Player.GetPluginIDs(PluginIDs);
      Boat := 10;
      while Boat > 0 do
      begin
        if PluginIDs.IndexOf(Format(idBoat, [Boat])) < 0 then
          Dec(Boat)
        else
          Break;
      end;
    finally
      PluginIDs.Free;
    end;

    // Construction du tableau des r�f�rences
    SetLength(ReferencesStrings,
      irActionsCounterBegin + Infos.ActionsCount);
    for I := srBegin to srEnd do
      ReferencesStrings[I] := SimpleReferencesStrings[I];

    for I := irButtonsBegin to irButtonsEnd do
    begin
      ReferencesStrings[I] :=
        '&CompteurBouton_' + IntToStr(I - irButtonsBegin + 1);
    end;
    for I := irTransportersBegin to irTransportersEnd do
    begin
      ReferencesStrings[I] :=
        '&CompteurTeleporteur_' + IntToStr(I - irTransportersBegin + 1);
    end;

    for I := irVariablesBegin to irVariablesEnd do
      ReferencesStrings[I] := '&Variable_' + IntToStr(I - irVariablesBegin + 1);
    for I := irActionsCounterBegin to Length(ReferencesStrings)-1 do
    begin
      ReferencesStrings[I] :=
        '&CompteurActions_' + IntToStr(I - irActionsCounterBegin);
    end;

    ExecuteActions;

    ADoNextPhase := DoNextPhase;
    AHasMoved := HasMoved;
    AHasShownMsg := HasShownMsg;
    ASuccessful := Successful;
  finally
    Free;
  end;
end;

initialization
  CommandProcs[cReplace]     := @TActionsInterpreter.ReplaceCmd;
  CommandProcs[cConvert]     := @TActionsInterpreter.ConvertCmd;
  CommandProcs[cMove]        := @TActionsInterpreter.MoveCmd;
  CommandProcs[cDeactivate]  := @TActionsInterpreter.DeactivateCmd;
  CommandProcs[cIncrement]   := @TActionsInterpreter.IncrementCmd;
  CommandProcs[cDecrement]   := @TActionsInterpreter.DecrementCmd;
  CommandProcs[cMultiply]    := @TActionsInterpreter.MultiplyCmd;
  CommandProcs[cMessage]     := @TActionsInterpreter.MessageCmd;
  CommandProcs[cTip]         := @TActionsInterpreter.TipCmd;
  CommandProcs[cFailure]     := @TActionsInterpreter.FailureCmd;
  CommandProcs[cBlindAlley]  := @TActionsInterpreter.BlindAlleyCmd;
  CommandProcs[cChoice]      := @TActionsInterpreter.ChoiceCmd;
  CommandProcs[cDescription] := @TActionsInterpreter.DescriptionCmd;
  CommandProcs[cWin]         := @TActionsInterpreter.WinCmd;
  CommandProcs[cSound]       := @TActionsInterpreter.SoundCmd;
  CommandProcs[cGoTo]        := @TActionsInterpreter.GoToCmd;
  CommandProcs[cLetPass]     := @TActionsInterpreter.LetPassCmd;
  CommandProcs[cAllowPlank]  := @TActionsInterpreter.AllowPlankCmd;
  CommandProcs[cDontGoOn]    := @TActionsInterpreter.DontGoOnCmd;
  CommandProcs[cGoOn]        := @TActionsInterpreter.GoOnCmd;
  CommandProcs[cContinue]    := @TActionsInterpreter.GoOnCmd;
end.

