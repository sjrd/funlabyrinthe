program EditLabyrinthe;

uses
  Forms,
  WinHelpViewer,
  NewLabDialog in 'NewLabDialog.pas' {FormNouveau},
  DescriptionDialog in 'DescriptionDialog.pas' {FormDescription},
  ButtonActions in 'ButtonActions.pas' {FormModifieBoutons},
  ReplacementScrewDialog in 'ReplacementScrewDialog.pas' {FormNewCase},
  NewLiftDialog in 'NewLiftDialog.pas' {FormAscenseur},
  ScrewNumberDialog in 'ScrewNumberDialog.pas' {FormNumeroCase},
  EditLabyrintheMain in 'EditLabyrintheMain.pas' {FormPrincipale};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Editeur de Labyrinthes';
  Application.Tag := 2;
  Application.CreateForm(TFormPrincipale, FormPrincipale);
  Application.CreateForm(TFormNouveau, FormNouveau);
  Application.CreateForm(TFormDescription, FormDescription);
  Application.CreateForm(TFormModifieBoutons, FormModifieBoutons);
  Application.CreateForm(TFormNewCase, FormNewCase);
  Application.CreateForm(TFormAscenseur, FormAscenseur);
  Application.CreateForm(TFormNumeroCase, FormNumeroCase);
  Application.Run;
end.
