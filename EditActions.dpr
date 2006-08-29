program EditActions;

uses
  XPMan,
  Forms,
  WinHelpViewer,
  EditActionsMain in 'EditActionsMain.pas' {FormPrincipale},
  LabMap in 'LabMap.pas' {FormPlan},
  ScrewCodes in 'ScrewCodes.pas' {FormCodesCases},
  LabyrintheUtils in 'LabyrintheUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Editeur d''actions';
  Application.Tag := 3;
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.CreateForm(TFormPlan, FormPlan);
  Application.CreateForm(TFormCodesCases, FormCodesCases);
  Application.Run;
end.
