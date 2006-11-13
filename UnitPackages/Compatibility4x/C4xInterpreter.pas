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
  SysUtils, Classes, StrUtils, ScUtils, ScStrUtils, FunLabyUtils, C4xCommon;

resourcestring
  sUnknownLabel = 'Le label %s n''a pas pu �tre trouv�';
  sUnknownCommand = 'Commande inconnue � la ligne %d';

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
    Phase : integer;       /// Phase courante (phPushing ou phExecute)
    Player : TPlayer;      /// Joueur concern�
    Map : TMap;            /// Carte courante
    KeyPressed : boolean;  /// True si une touche a �t� press�e
    Position : T3DPoint;   /// Position de la case
    DoNextPhase : boolean; /// Indique s'il faut ex�cuter la phase suivante
    HasMoved : boolean;    /// Indique si un AllerA a �t� fait
    HasShownMsg : boolean; /// Indique si un message a �t� affich�
    Inactive : string;     /// Case � utiliser lors d'un Desactiver

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
begin
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

