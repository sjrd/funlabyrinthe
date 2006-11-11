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
  Classes, ScUtils, FunLabyUtils;

type
  {*
    Interpr�te des actions
    @author S�bastien Jean Robert Doeraene
    @version 5.0
  *}
  TActionsInterpreter = class
  private
    Actions : TStrings;    /// Actions � ex�cuter
    Master : TMaster;      /// Ma�tre FunLabyrinthe
    Phase : integer;       /// Phase courante (phPushing ou phExecute)
    Player : TPlayer;      /// Joueur concern�
    KeyPressed : boolean;  /// True si une touche a �t� press�e
    Position : T3DPoint;   /// Position de la case
    DoNextPhase : boolean; /// Indique s'il faut ex�cuter la phase suivante

    procedure ExecuteActions;
  public
    constructor Create;

    class procedure Execute(AActions : TStrings; AMaster : TMaster;
      APhase : integer; APlayer : TPlayer; AKeyPressed : boolean;
      const APos : T3DPoint; var ADoNextPhase : boolean);
  end;

implementation

{----------------------------}
{ Classe TActionsInterpreter }
{----------------------------}

{*
  Cr�e une instance de TActionsInterpreter
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
end;

{*
  Ex�cute les actions dans le contexte donn�
*}
procedure TActionsInterpreter.ExecuteActions;
begin
end;

{*
  Ex�cute une liste d'actions
  @param AActions       Actions � ex�cuter
  @param AMaster        Ma�tre FunLabyrinthe
  @param APhase         Phase courante (phPushing ou phExecute)
  @param APlayer        Joueur concern�
  @param AKeyPressed    True si une touche a �t� press�e pour le d�placement
  @param APos           Position de la case
  @param ADoNextPhase   Indique s'il faut ex�cuter la phase suivante
*}
class procedure TActionsInterpreter.Execute(AActions : TStrings;
  AMaster : TMaster; APhase : integer; APlayer : TPlayer; AKeyPressed : boolean;
  const APos : T3DPoint; var ADoNextPhase : boolean);
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
  finally
    Free;
  end;
end;

end.

