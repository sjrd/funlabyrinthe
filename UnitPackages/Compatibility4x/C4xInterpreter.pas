{*
  Interprétation des actions
  L'unité C4xInterpreter décrit la classe TActionsInterpreter, qui se charge
  d'interpréter les actions.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit C4xInterpreter;

interface

uses
  SysUtils, Classes, StrUtils, ScUtils, ScStrUtils, FunLabyUtils, C4xCommon;

resourcestring
  sUnknownLabel = 'Le label %s n''a pas pu être trouvé';
  sUnknownCommand = 'Commande inconnue à la ligne %d';

type
  {*
    Générée lorsqu'une action est invalide
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  EInvalidAction = class(Exception);

  {*
    Interprète des actions
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsInterpreter = class
  private
    Counter : PInteger;    /// Pointeur vers le compteur des actions courantes
    Actions : TStrings;    /// Actions à exécuter
    Master : TMaster;      /// Maître FunLabyrinthe
    Phase : integer;       /// Phase courante (phPushing ou phExecute)
    Player : TPlayer;      /// Joueur concerné
    Map : TMap;            /// Carte courante
    KeyPressed : boolean;  /// True si une touche a été pressée
    Position : T3DPoint;   /// Position de la case
    DoNextPhase : boolean; /// Indique s'il faut exécuter la phase suivante
    HasMoved : boolean;    /// Indique si un AllerA a été fait
    HasShownMsg : boolean; /// Indique si un message a été affiché
    Inactive : string;     /// Case à utiliser lors d'un Desactiver

    procedure TreatIfStatement(var Line : string);
    procedure TreatVariables(var Line : string);

    procedure ReplaceCmd    (var Params : string);
    procedure MoveCmd       (var Params : string);
    procedure DeactivateCmd (var Params : string);
    procedure IncrementCmd  (var Params : string);
    procedure DecrementCmd  (var Params : string);
    procedure MultiplyCmd   (var Params : string);
    procedure MessageCmd    (var Params : string);
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
    procedure TipCmd        (var Params : string);

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
  cReplace     = 0;
  cMove        = 1;
  cDeactivate  = 2;
  cIncrement   = 3;
  cDecrement   = 4;
  cMultiply    = 5;
  cMessage     = 6;
  cTip         = 7;
  cFailure     = 8;
  cBlindAlley  = 9;
  cChoice      = 10;
  cSound       = 11;
  cGoTo        = 12;
  cLetPass     = 13;
  cAllowPlank  = 14;
  cDontGoOn    = 15;
  cGoOn        = 16;
  cContinue    = 17;
  cDescription = 18;
  cWin         = 19;
  cJump        = 20;
  cStop        = 21;

  CommandStrings : array[cReplace..cStop] of string = (
    'Remplacer', 'Deplacer', 'Desactiver', 'Incrementer', 'Decrementer',
    'Multiplier', 'Message', 'Indice', 'Echec', 'Impasse', 'Choix', 'Son',
    'AllerA', 'LaisserPasser', 'AutoriserPlanche', 'Arreter', 'Poursuivre',
    'Continuer', 'Description', 'Gagner', 'Saute', 'Stop'
  );

type
  TCommandProc = procedure(Self : TObject; var Params : string);

var
  CommandProcs : array[cReplace..cWin] of TCommandProc;

{----------------------------}
{ Classe TActionsInterpreter }
{----------------------------}

{*
  Crée une instance de TActionsInterpreter
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
end;

{*
  Traite une éventuelle condition dans l'instruction
  @param Line   Instruction à traiter
*}
procedure TActionsInterpreter.TreatIfStatement(var Line : string);
begin
end;

{*
  Traite les valeurs de variables dans l'instruction
  @param Line   Instruction à traiter
*}
procedure TActionsInterpreter.TreatVariables(var Line : string);
begin
end;

{*
  Commande 'Remplacer'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.ReplaceCmd(var Params : string);
begin
end;

{*
  Commande 'Deplacer'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.MoveCmd(var Params : string);
begin
end;

{*
  Commande 'Desactiver'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.DeactivateCmd(var Params : string);
begin
end;

{*
  Commande 'Incrementer'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.IncrementCmd(var Params : string);
begin
end;

{*
  Commande 'Decrementer'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.DecrementCmd(var Params : string);
begin
end;

{*
  Commande 'Multiplier'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.MultiplyCmd(var Params : string);
begin
end;

{*
  Commande 'Message'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.MessageCmd(var Params : string);
begin
end;

{*
  Commande 'Indice'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.TipCmd(var Params : string);
begin
end;

{*
  Commande 'Echec'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.FailureCmd(var Params : string);
begin
end;

{*
  Commande 'Impasse'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.BlindAlleyCmd(var Params : string);
begin
end;

{*
  Commande 'Choix'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.ChoiceCmd(var Params : string);
begin
end;

{*
  Commande 'Son'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.SoundCmd(var Params : string);
begin
end;

{*
  Commande 'AllerA'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.GoToCmd(var Params : string);
begin
end;

{*
  Commande 'LaisserPasser'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.LetPassCmd(var Params : string);
begin
end;

{*
  Commande 'AutoriserPlanche'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.AllowPlankCmd(var Params : string);
begin
end;

{*
  Commande 'Arreter'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.DontGoOnCmd(var Params : string);
begin
end;

{*
  Commande 'Poursuivre'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.GoOnCmd(var Params : string);
begin
end;

{*
  Commande 'Continuer'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.ContinueCmd(var Params : string);
begin
end;

{*
  Commande 'Description'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.DescriptionCmd(var Params : string);
begin
end;

{*
  Commande 'Gagner'
  @param Params   Paramètres de la commande
*}
procedure TActionsInterpreter.WinCmd(var Params : string);
begin
end;

{*
  Exécute les actions dans le contexte donné
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

    // Lignes de commentaires à éliminer
    if (Line = '') or (Line[1] = '#') or (GetXWord(Line, 1) = 'Remarque') then
      Continue;

    // Traitement de l'éventuelle condition
    TreatIfStatement(Line);

    // Traitement d'un éventuel Unique
    if GetXWord(Line, 1) = 'Unique' then
    begin
      if Counter^ <> 1 then Continue else
        Delete(Line, 1, 7);
    end;

    // Séparation de la commande du reste de la ligne
    Command := GetXWord(Line, 1);
    Delete(Line, 1, Length(Command)+1);

    // Traitement des valeurs des variables
    TreatVariables(Line);

    // Exécution de la commande
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
  Exécute une liste d'actions
  @param ACounter       Pointeur vers le compteur des actions courantes
  @param AActions       Actions à exécuter
  @param AMaster        Maître FunLabyrinthe
  @param APhase         Phase courante (phPushing ou phExecute)
  @param APlayer        Joueur concerné
  @param AKeyPressed    True si une touche a été pressée pour le déplacement
  @param APos           Position de la case
  @param ADoNextPhase   Indique s'il faut exécuter la phase suivante
  @param AHasMoved      Indique si un AllerA a été fait
  @param AHasShownMsg   Indique si un message a été affiché
*}
class procedure TActionsInterpreter.Execute(ACounter : PInteger;
  AActions : TStrings; AMaster : TMaster; APhase : integer; APlayer : TPlayer;
  AKeyPressed : boolean; const APos : T3DPoint; var ADoNextPhase : boolean;
  out AHasMoved, AHasShownMsg : boolean; AInactive : TComponentID);
begin
  with Create do
  try
    Counter := ACounter;
    Actions := AActions;
    Master := AMaster;
    Phase := APhase;
    Player := APlayer;
    Map := Player.Map;
    KeyPressed := AKeyPressed;
    Position := APos;
    DoNextPhase := ADoNextPhase;
    Inactive := idGrass+'-'+AInactive+'-';

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

