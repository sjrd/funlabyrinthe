unit C4xInterpreter;

interface

uses
  Classes, ScUtils, FunLabyUtils;

type
  TActionsInterpreter = class
  private
    FMaster : TMaster;   /// Ma�tre FunLabyrinthe
    FActions : TStrings; /// Actions � ex�cuter

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
  Cr�e une instance de TActionsInterpreter
  @param AMaster    Ma�tre FunLabyrinthe
  @param AActions   Actions � ex�cuter
*}
constructor TActionsInterpreter.Create(AMaster : TMaster; AActions : TStrings);
begin
  inherited Create;
  FMaster := AMaster;
  FActions := TStringList.Create;
  FActions.Assign(AActions);
end;

{*
  D�truit l'instance
*}
destructor TActionsInterpreter.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{*
  Modifie les actions � ex�cuter
  @param New   Nouvelles actions
*}
procedure TActionsInterpreter.SetActions(New : TStrings);
begin
  FActions.Assign(New);
end;

end.
