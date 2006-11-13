{*
  Interpr�tation des actions
  L'unit� C4xInterpreter d�crit la classe TActionsInterpreter, qui se charge
  d'interpr�ter les actions.
  @author S�bastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xInterpreter;

interface

uses
  SysUtils, Classes, StrUtils, ScUtils, ScStrUtils, FunLabyUtils, C4xComponents,
  C4xCommon;

resourcestring
  sUnknownLabel = 'Le label %s n''a pas pu �tre trouv�';
  sUnknownCommand = 'Commande inconnue � la ligne %d';

const
  /// Version de l'interpr�teur d'actions
  InterpreterVersion = 50;

type
  {*
    G�n�r�e lorsqu'une action est invalide
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  EInvalidAction = class(Exception);

  {*
    Interpr�te des actions
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsInterpreter = class
  private
    Counter : PInteger;    /// Pointeur vers le compteur des actions courantes
    Actions : TStrings;    /// Actions � ex�cuter
    Master : TMaster;      /// Ma�tre FunLabyrinthe
    Infos : TC4xInfos;     /// Infos C4x
    Phase : integer;       /// Phase courante (phPushing ou phExecute)
    Player : TPlayer;      /// Joueur concern�
    Map : TMap;            /// Carte courante
    KeyPressed : boolean;  /// True si une touche a �t� press�e
    Position : T3DPoint;   /// Position de la case
    DoNextPhase : boolean; /// Indique s'il faut ex�cuter la phase suivante
    HasMoved : boolean;    /// Indique si un AllerA a �t� fait
    HasShownMsg : boolean; /// Indique si un message a �t� affich�
    Inactive : string;     /// Case � utiliser lors d'un Desactiver

    StrHere : string;       /// Case courante
    StrBefore : string;     /// Case devant
    StrBehind : string;     /// Case derri�re
    Answer : integer;       /// R�ponse d'un Choix
    Boat : integer;         /// Num�ro de la barque qu'a le joueur

    procedure TreatIfStatement(var Line : string);
    procedure TreatVariables(var Line : string);

    procedure ReplaceCmd    (var Params : string);
    procedure MoveCmd       (var Params : string);
    procedure DeactivateCmd (var Params : string);
    procedure IncrementCmd  (var Params : string);
    procedure DecrementCmd  (var Params : string);
    procedure MultiplyCmd   (var Params : string);
    procedure MessageCmd    (var Params : string);
    procedure TipCmd        (var Params : string);
    procedure FailureCmd    (var Params : string);
    procedure BlindAlleyCmd (var Params : string);
    procedure ChoiceCmd     (var Params : string);
    procedure SoundCmd      (var Params : string);
    procedure GoToCmd       (var Params : string);
    procedure LetPassCmd    (var Params : string);
    procedure AllowPlankCmd (var Params : string);
    procedure DontGoOnCmd   (var Params : string);
    procedure GoOnCmd       (var Params : string);
    procedure ContinueCmd   (var Params : string);
    procedure DescriptionCmd(var Params : string);
    procedure WinCmd        (var Params : string);

    procedure ExecuteActions;
  public
    constructor Create;

    class procedure Execute(ACounter : PInteger; AActions : TStrings;
      AMaster : TMaster; APhase : integer; APlayer : TPlayer;
      AKeyPressed : boolean; const APos : T3DPoint; var ADoNextPhase : boolean;
      out AHasMoved, AHasShownMsg : boolean; AInactive : TComponentID);
  end;

implementation

{ Don't localize any of the strings in this implementation! }

const
  cReplace     = 0;  /// Commande Remplacer
  cMove        = 1;  /// Commande Deplacer
  cDeactivate  = 2;  /// Commande Desactiver
  cIncrement   = 3;  /// Commande Incrementer
  cDecrement   = 4;  /// Commande Decrementer
  cMultiply    = 5;  /// Commande Multiplier
  cMessage     = 6;  /// Commande Message
  cTip         = 7;  /// Commande Indice
  cFailure     = 8;  /// Commande Echec
  cBlindAlley  = 9;  /// Commande Impasse
  cChoice      = 10; /// Commande Choix
  cSound       = 11; /// Commande Son
  cGoTo        = 12; /// Commande AllerA
  cLetPass     = 13; /// Commande LaisserPasser
  cAllowPlank  = 14; /// Commande AutoriserPlanche
  cDontGoOn    = 15; /// Commande Arreter
  cGoOn        = 16; /// Commande Poursuivre
  cContinue    = 17; /// Commande Continuer
  cDescription = 18; /// Commande Description
  cWin         = 19; /// Commande Gagner
  cJump        = 20; /// Commande Saute
  cStop        = 21; /// Commande Stop

  /// Tableaux des cha�nes de commandes
  CommandStrings : array[cReplace..cStop] of string = (
    'Remplacer', 'Deplacer', 'Desactiver', 'Incrementer', 'Decrementer',
    'Multiplier', 'Message', 'Indice', 'Echec', 'Impasse', 'Choix', 'Son',
    'AllerA', 'LaisserPasser', 'AutoriserPlanche', 'Arreter', 'Poursuivre',
    'Continuer', 'Description', 'Gagner', 'Saute', 'Stop'
  );

type
  {*
    Type d'une proc�dure qui recherche une sous-cha�ne dans un cha�ne
    @param SubStr   Sous-cha�ne � rechercher
    @param Str      Cha�ne dans laquelle chercher SubStr
    @return Index de la premi�re occurence de SubStr dans Str (0 si non trouv�e)
  *}
  TPosProc = function(const SubStr, Str : string) : integer;

  {*
    Type proc�dural correspondant aux m�thodes d'ex�cution de commande
    @param Self     R�f�rence � l'objet courant
    @param Params   Param�tres de la commande
  *}
  TCommandProc = procedure(Self : TObject; var Params : string);

var
  /// Tableau des m�thodes d'ex�cution de commande
  CommandProcs : array[cReplace..cWin] of TCommandProc;

{-------------------}
{ Routines globales }
{-------------------}

{*
  Recherche une variable dans une instruction
  Une variable doit �tre entour�e d'espaces ou de crochets, et ne pas se trouver
  entre une paire d'accolades, pour �tre reconnue comme telle.
  @param Variable   Variable � chercher
  @param Str        Cha�ne dans laquelle chercher Variable
  @return Index de la premi�re occurence de Variable dans Str (0 si non trouv�e)
*}
function PosVariable(const Variable, Str : string) : integer;
var Str2 : string;
    I : integer;
begin
  Str2 := ' '+Str+' ';
  I := 2;
  while I < Length(Str2) do case Str2[I] of
    '[', ']' :
    begin
      Str2[I] := ' ';
      inc(I);
    end;
    '{' :
    begin
      while (I < Length(Str2)) and (Str2[I] <> '}') do
      begin
        Str2[I] := ' ';
        inc(I);
      end;
    end;
    else inc(I);
  end;

  Result := Pos(' '+Variable+' ', Str2);
end;

{*
  Remplace toutes les occurences d'une sous-cha�ne par une autre
  @param Str        Cha�ne � modifier
  @param FromText   Texte � chercher
  @param ToText     Texte � ins�rer � la place de FromText
  @param PosProc    Routine de recherche � utiliser
*}
procedure GenericReplace(var Str : string; const FromText, ToText : string;
  PosProc : TPosProc);
var Len, Index : integer;
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
procedure Replace(var Str : string; const FromText, ToText : string);
begin
  GenericReplace(Str, FromText, ToText, Pos);
end;

{*
  Remplace toutes les occurences d'une variable donn�e par sa valeur
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur de la variable
  @see PosVariable
*}
procedure ReplaceVariable(var Str : string;
  const Variable, Value : string); overload;
begin
  GenericReplace(Str, Variable, Value, PosVariable);
end;

{*
  Remplace toutes les occurences d'une variable donn�e par sa valeur enti�re
  @param Str        Cha�ne dans laquelle remplacer les variables
  @param Variable   Variable � remplacer
  @param Value      Valeur enti�re de la variable
  @see PosVariable
*}
procedure ReplaceVariable(var Str : string; const Variable : string;
  Value : integer); overload;
begin
  GenericReplace(Str, Variable, IntToStr(Value), PosVariable);
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
  Inactive := '';

  StrHere := '';
  StrBefore := '';
  StrBehind := '';
  Answer := 0;
end;

{*
  Traite une �ventuelle condition dans l'instruction
  @param Line   Instruction � traiter
*}
procedure TActionsInterpreter.TreatIfStatement(var Line : string);
begin
end;

{*
  Traite les valeurs de variables dans l'instruction
  @param Line   Instruction � traiter
*}
procedure TActionsInterpreter.TreatVariables(var Line : string);
var I : integer;
begin
  ReplaceVariable(Line, 'Ici', StrHere);
  ReplaceVariable(Line, 'Devant', StrBefore);
  ReplaceVariable(Line, 'Derriere', StrBehind);

  ReplaceVariable(Line, 'X'        , Position.X);
  ReplaceVariable(Line, 'Y'        , Position.Y);
  ReplaceVariable(Line, 'Z'        , Position.Z);
  ReplaceVariable(Line, 'Direction', integer(Player.Direction));
  ReplaceVariable(Line, 'Reponse'  , Answer);
  ReplaceVariable(Line, 'Reussite' , 1);
  ReplaceVariable(Line, 'Touche'   , integer(KeyPressed));
  ReplaceVariable(Line, 'Phase'    , Phase);

  with Master.ObjectDef[idBuoys] do
  begin
    ReplaceVariable(Line, 'Bouees', Count[Player]);
    ReplaceVariable(Line, 'Bouee', Count[Player]);
  end;
  with Master.ObjectDef[idPlanks] do
  begin
    ReplaceVariable(Line, 'Planches', Count[Player]);
    ReplaceVariable(Line, 'Planche', Count[Player]);
  end;
  with Master.ObjectDef[idSilverKeys] do
    ReplaceVariable(Line, 'ClesArgent', Count[Player]);
  with Master.ObjectDef[idGoldenKeys] do
    ReplaceVariable(Line, 'ClesOr', Count[Player]);
  ReplaceVariable(Line, 'Barque', Boat);

  ReplaceVariable(Line, 'VersionInterpreteur', InterpreterVersion);
  ReplaceVariable(Line, 'Couleur', Player.Color);

  if Pos('CompteurActions_', Line) > 0 then
  begin
    for I := 0 to Infos.ActionsCount-1 do
    begin
      ReplaceVariable(Line, Format('CompteurActions_%d', [I]),
        Infos.Actions[I].Counter);
    end;
  end;

  if Pos('Variable_', Line) > 0 then
  begin
    for I := 0 to Infos.ActionsCount-1 do
      ReplaceVariable(Line, Format('Variable_%d', [I]), Infos.Variables[I]);
  end;

  ReplaceVariable(Line, 'Compteur', Counter^);

  if Pos('CompteurActions ', Line) > 0 then
  begin
    for I := 0 to Infos.ActionsCount-1 do
    begin
      ReplaceVariable(Line, Format('CompteurActions %d', [I]),
        Infos.Actions[I].Counter);
    end;
  end;

  if Pos('Variable ', Line) > 0 then
  begin
    for I := 0 to Infos.ActionsCount-1 do
      ReplaceVariable(Line, Format('Variable %d', [I]), Infos.Variables[I]);
  end;

  Replace(Line, '&CompteurActions ', '&CompteurActions_');
end;

{*
  Commande 'Remplacer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.ReplaceCmd(var Params : string);
begin
end;

{*
  Commande 'Deplacer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.MoveCmd(var Params : string);
begin
end;

{*
  Commande 'Desactiver'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DeactivateCmd(var Params : string);
begin
end;

{*
  Commande 'Incrementer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.IncrementCmd(var Params : string);
begin
end;

{*
  Commande 'Decrementer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DecrementCmd(var Params : string);
begin
end;

{*
  Commande 'Multiplier'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.MultiplyCmd(var Params : string);
begin
end;

{*
  Commande 'Message'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.MessageCmd(var Params : string);
begin
end;

{*
  Commande 'Indice'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.TipCmd(var Params : string);
begin
end;

{*
  Commande 'Echec'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.FailureCmd(var Params : string);
begin
end;

{*
  Commande 'Impasse'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.BlindAlleyCmd(var Params : string);
begin
end;

{*
  Commande 'Choix'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.ChoiceCmd(var Params : string);
begin
end;

{*
  Commande 'Son'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.SoundCmd(var Params : string);
begin
end;

{*
  Commande 'AllerA'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.GoToCmd(var Params : string);
begin
end;

{*
  Commande 'LaisserPasser'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.LetPassCmd(var Params : string);
begin
end;

{*
  Commande 'AutoriserPlanche'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.AllowPlankCmd(var Params : string);
begin
end;

{*
  Commande 'Arreter'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DontGoOnCmd(var Params : string);
begin
end;

{*
  Commande 'Poursuivre'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.GoOnCmd(var Params : string);
begin
end;

{*
  Commande 'Continuer'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.ContinueCmd(var Params : string);
begin
end;

{*
  Commande 'Description'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.DescriptionCmd(var Params : string);
begin
end;

{*
  Commande 'Gagner'
  @param Params   Param�tres de la commande
*}
procedure TActionsInterpreter.WinCmd(var Params : string);
begin
end;

{*
  Ex�cute les actions dans le contexte donn�
*}
procedure TActionsInterpreter.ExecuteActions;
var Current : integer;
    Line, Command : string;
    IntCommand, Index : integer;
begin
  Current := 0;
  while Current < Actions.Count do
  try
    Line := Actions[Current];
    inc(Current);

    // Lignes de commentaires � �liminer
    if (Line = '') or (Line[1] = '#') or (GetXWord(Line, 1) = 'Remarque') then
      Continue;

    // Traitement de l'�ventuelle condition
    TreatIfStatement(Line);

    // Traitement d'un �ventuel Unique
    if GetXWord(Line, 1) = 'Unique' then
    begin
      if Counter^ <> 1 then Continue else
        Delete(Line, 1, 7);
    end;

    // S�paration de la commande du reste de la ligne
    Command := GetXWord(Line, 1);
    Delete(Line, 1, Length(Command)+1);

    // Traitement des valeurs des variables
    TreatVariables(Line);

    // Ex�cution de la commande
    IntCommand := AnsiIndexStr(Command, CommandStrings);
    case IntCommand of
      cReplace..cTip : CommandProcs[IntCommand](Self, Line);
      cJump :
      begin
        Index := Actions.IndexOf('#'+Line);
        if Index < 0 then
          raise EInvalidAction.CreateFmt(sUnknownLabel, [Line]);
        Current := Index;
      end;
      cStop : Break;
      else raise EInvalidAction.CreateFmt(sUnknownCommand, [Current]);
    end;
  except
    on Error : EInvalidAction do;
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
  @parma AInactive      Effet � utiliser lors d'un Desactiver
*}
class procedure TActionsInterpreter.Execute(ACounter : PInteger;
  AActions : TStrings; AMaster : TMaster; APhase : integer; APlayer : TPlayer;
  AKeyPressed : boolean; const APos : T3DPoint; var ADoNextPhase : boolean;
  out AHasMoved, AHasShownMsg : boolean; AInactive : TComponentID);
var PluginIDs : TStrings;
begin
  with Create do
  try
    Counter := ACounter;
    Actions := AActions;
    Master := AMaster;
    Infos := Master.Component[idC4xInfos] as TC4xInfos;
    Phase := APhase;
    Player := APlayer;
    Map := Player.Map;
    KeyPressed := AKeyPressed;
    Position := APos;
    DoNextPhase := ADoNextPhase;
    Inactive := idGrass+'-'+AInactive+'-';

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
          dec(Boat)
        else
          Break;
      end;
    finally
      PluginIDs.Free;
    end;

    ExecuteActions;

    ADoNextPhase := DoNextPhase;
    AHasMoved := HasMoved;
    AHasShownMsg := HasShownMsg;
  finally
    Free;
  end;
end;

initialization
  CommandProcs[cReplace]     := @TActionsInterpreter.ReplaceCmd;
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
  CommandProcs[cSound]       := @TActionsInterpreter.SoundCmd;
  CommandProcs[cGoTo]        := @TActionsInterpreter.GoToCmd;
  CommandProcs[cLetPass]     := @TActionsInterpreter.LetPassCmd;
  CommandProcs[cAllowPlank]  := @TActionsInterpreter.AllowPlankCmd;
  CommandProcs[cDontGoOn]    := @TActionsInterpreter.DontGoOnCmd;
  CommandProcs[cGoOn]        := @TActionsInterpreter.GoOnCmd;
  CommandProcs[cContinue]    := @TActionsInterpreter.ContinueCmd;
  CommandProcs[cDescription] := @TActionsInterpreter.DescriptionCmd;
  CommandProcs[cWin]         := @TActionsInterpreter.WinCmd;
end.

