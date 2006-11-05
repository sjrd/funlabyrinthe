unit C4xInterpreter;

interface

uses
  Classes, ScUtils, FunLabyUtils;

type
  TActionsInterpreter = class
  private
    FMaster : TMaster;   /// Maître FunLabyrinthe
    FActions : TStrings; /// Actions à exécuter

    procedure SetActions(New : TStrings);
  public
    constructor Create(AMaster : TMaster; AActions : TStrings);
    destructor Destroy; override;

    property Actions : TStrings read FActions write SetActions;
  end;

implementation

{----------------------------}
{ Classe TActionsInterpreter }
{----------------------------}

{*
  Crée une instance de TActionsInterpreter
  @param AMaster    Maître FunLabyrinthe
  @param AActions   Actions à exécuter
*}
constructor TActionsInterpreter.Create(AMaster : TMaster; AActions : TStrings);
begin
  inherited Create;
  FMaster := AMaster;
  FActions := TStringList.Create;
  FActions.Assign(AActions);
end;

{*
  Détruit l'instance
*}
destructor TActionsInterpreter.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{*
  Modifie les actions à exécuter
  @param New   Nouvelles actions
*}
procedure TActionsInterpreter.SetActions(New : TStrings);
begin
  FActions.Assign(New);
end;

end.
