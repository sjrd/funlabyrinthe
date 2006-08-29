unit RegFunLabyCommon;

interface

uses Classes, FunLabySyn;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SynEdit Highlighters', [TFunLabySyntax]);
end;

end.
