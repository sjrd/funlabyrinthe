unit RegisterFLOldSyn;

interface

uses Classes, FunLabyOldSyn;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SynEdit Highlighters', [TFunLabyOldSyntax]);
end;

end.
