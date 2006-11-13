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
  Classes, ScUtils, FunLabyUtils;

type
  {*
    Interprète des actions
    @author Sébastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsInterpreter = class
  private
    Actions : TStrings;    /// Actions à exécuter
    Master : TMaster;      /// Maître FunLabyrinthe
    Phase : integer;       /// Phase courante (phPushing ou phExecute)
    Player : TPlayer;      /// Joueur concerné
    KeyPressed : boolean;  /// True si une touche a été pressée
    Position : T3DPoint;   /// Position de la case
    DoNextPhase : boolean; /// Indique s'il faut exécuter la phase suivante
    HasMoved : boolean;    /// Indique si un AllerA a été fait
    HasShownMsg : boolean; /// Indique si un message a été affiché

    procedure ExecuteActions;
  public
    constructor Create;

    class procedure Execute(AActions : TStrings; AMaster : TMaster;
      APhase : integer; APlayer : TPlayer; AKeyPressed : boolean;
      const APos : T3DPoint; var ADoNextPhase : boolean;
      out AHasMoved, AHasShownMsg : boolean);
  end;

implementation

{----------------------------}
{ Classe TActionsInterpreter }
{----------------------------}

{*
  Crée une instance de TActionsInterpreter
*}
constructor TActionsInterpreter.Create;
begin
  inherited Create;

  Actions := nil;
  Master := nil;
  Phase := 0;
  Player := nil;
  KeyPressed := False;
  Position := No3DPoint;
  DoNextPhase := False;
  HasMoved := False;
  HasShownMsg := False;
end;

{*
  Exécute les actions dans le contexte donné
*}
procedure TActionsInterpreter.ExecuteActions;
begin
end;

{*
  Exécute une liste d'actions
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
class procedure TActionsInterpreter.Execute(AActions : TStrings;
  AMaster : TMaster; APhase : integer; APlayer : TPlayer; AKeyPressed : boolean;
  const APos : T3DPoint; var ADoNextPhase : boolean;
  out AHasMoved, AHasShownMsg : boolean);
begin
  with Create do
  try
    Actions := AActions;
    Master := AMaster;
    Phase := APhase;
    Player := APlayer;
    KeyPressed := AKeyPressed;
    Position := APos;
    DoNextPhase := ADoNextPhase;

    ExecuteActions;

    ADoNextPhase := DoNextPhase;
    AHasMoved := HasMoved;
    AHasShownMsg := HasShownMsg;
  finally
    Free;
  end;
end;

end.

