unit EditActionsMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, LabyrintheUtils, StdCtrls, SynEditHighlighter, ScUtils,
  SynEdit, ToolWin, ComCtrls, StdActns, ActnList, ImgList, FunLabyOldSyn,
  SdDialogs, SynEditTypes, ShellAPI;

type
  TFormPrincipale = class(TForm)
    BigMenu: TMainMenu;
    BigMenuFichier: TMenuItem;
    MenuOuvrir: TMenuItem;
    MenuFermer: TMenuItem;
    MenuEnregistrer: TMenuItem;
    MenuEnregSous: TMenuItem;
    Sep1: TMenuItem;
    MenuQuitter: TMenuItem;
    BigMenuAide: TMenuItem;
    MenuRubrAide: TMenuItem;
    Sep2: TMenuItem;
    MenuAPropos: TMenuItem;
    BigMenuVoir: TMenuItem;
    MenuVoirPlanLaby: TMenuItem;
    MenuTest: TMenuItem;
    Sep3: TMenuItem;
    EnTete: TSynEdit;
    Actions: TActionList;
    Images: TImageList;
    Ouvrir: TFileOpen;
    Enregistrer: TAction;
    EnregSous: TFileSaveAs;
    Tester: TAction;
    Quitter: TAction;
    Fermer: TAction;
    Couper: TAction;
    Copier: TAction;
    Coller: TAction;
    Defaire: TAction;
    Refaire: TAction;
    RubriquesAide: TAction;
    APropos: TAction;
    Outils: TToolBar;
    BoutonOuvrir: TToolButton;
    BoutonEnregistrer: TToolButton;
    BoutonFermer: TToolButton;
    BoutonSep1: TToolButton;
    BoutonTester: TToolButton;
    BoutonSep2: TToolButton;
    BoutonQuitter: TToolButton;
    BoutonSep3: TToolButton;
    BoutonCouper: TToolButton;
    BoutonCopier: TToolButton;
    BoutonColler: TToolButton;
    BoutonDefaire: TToolButton;
    BoutonRefaire: TToolButton;
    BoutonSep4: TToolButton;
    BoutonChercher: TToolButton;
    BoutonOccSuivante: TToolButton;
    BoutonSep5: TToolButton;
    BoutonRubriquesAide: TToolButton;
    BoutonAPropos: TToolButton;
    VoirPlanLaby: TAction;
    BigMenuEdition: TMenuItem;
    Couper1: TMenuItem;
    Copier1: TMenuItem;
    Coller1: TMenuItem;
    Dfaire1: TMenuItem;
    Refaire1: TMenuItem;
    Statut: TStatusBar;
    BigMenuRechercher: TMenuItem;
    MenuChercher: TMenuItem;
    MenuOccSuivante: TMenuItem;
    FunLabyOldSyntax: TFunLabyOldSyntax;
    Chercher: TAction;
    OccSuivante: TAction;
    FindDialog: TFindDialog;
    VoirCodesCases: TAction;
    MenuVoirCodesCases: TMenuItem;
    BoutonVoirPlanLaby: TToolButton;
    BoutonVoirCodesCases: TToolButton;
    ToolButton3: TToolButton;
    AboutDialog: TSdAboutDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OuvrirExecute(Sender: TObject);
    procedure EnregistrerExecute(Sender: TObject);
    procedure EnregSousExecute(Sender: TObject);
    procedure FermerExecute(Sender: TObject);
    procedure TesterExecute(Sender: TObject);
    procedure QuitterExecute(Sender: TObject);
    procedure CouperExecute(Sender: TObject);
    procedure CopierExecute(Sender: TObject);
    procedure CollerExecute(Sender: TObject);
    procedure DefaireExecute(Sender: TObject);
    procedure RefaireExecute(Sender: TObject);
    procedure ChercherExecute(Sender: TObject);
    procedure OccSuivanteExecute(Sender: TObject);
    procedure FindDialogShow(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure VoirPlanLabyExecute(Sender: TObject);
    procedure RubriquesAideExecute(Sender: TObject);
    procedure AProposExecute(Sender: TObject);
    procedure EnvoyerEMailExecute(Sender: TObject);
    procedure VoirCodesCasesExecute(Sender: TObject);
  private
    { Déclarations privées }
    procedure UpdateEditCommands;
    procedure OpenLab(FileName : string);
    procedure SaveLab(FileName : string = '');
    function CloseLab : boolean;
  public
    { Déclarations publiques }
  end;

var
  FormPrincipale : TFormPrincipale;
  Labyrinthe : TLabyrinthe;

implementation

{$R *.DFM}

uses ScStrUtils, LabMap, ScrewCodes;

function SynEditSearch(SynEdit : TSynEdit; FindText : string;
                       Down, MatchCase, WholeWord : boolean) : boolean;
var Text : TStrings;
    Line, Column : integer;
    Found : boolean;
  procedure GoDown;
  begin
    inc(Column);
    if (Line < Text.Count) and (Column > Length(Text[Line])) then
    begin
      inc(Line);
      Column := 1;
    end;
  end;
  procedure GoUp;
  begin
    dec(Column);
    if Column < 1 then
    begin
      dec(Line);
      if Line >= 0 then Column := Length(Text[Line]);
    end;
  end;
  function IsCorrect(LineText : string; Pos, Len : integer) : boolean;
  var WordBreakChars : TSynIdentChars;
  begin
    if MatchCase then
      Result := Copy(LineText, Pos, Len) = FindText
    else
      Result := AnsiLowerCase(Copy(LineText, Pos, Len)) = AnsiLowerCase(FindText);
    if Result then if not WholeWord then Found := True else
    begin
      WordBreakChars := SynEdit.Highlighter.WordBreakChars;
      if (Pos > 1) and (not (LineText[Pos-1] in WordBreakChars)) then Result := False else
      if (Pos+Len <= Length(LineText)) and (not (LineText[Pos+Len] in WordBreakChars)) then Result := False else
      Found := True;
    end;
  end;
begin
  if SynEdit.SelAvail then
    if Down then SynEdit.CaretXY := SynEdit.BlockEnd
            else SynEdit.CaretXY := SynEdit.BlockBegin;
  Line := SynEdit.CaretY-1;
  Column := SynEdit.CaretX;
  Found := False;
  Text := TStringList.Create;
  Text.Assign(SynEdit.Lines);
  repeat
    if Down then
    begin
      GoDown;
      if Line >= Text.Count then Break;
    end else
    begin
      GoUp;
      if Line < 0 then Break;
    end;
  until IsCorrect(Text[Line], Column, Length(FindText));
  Text.Free;
  if Found then
  begin
    SynEdit.CaretXY := TBufferCoord(Point(Column+Length(FindText), Line+1));
    SynEdit.BlockBegin := TBufferCoord(Point(Column, Line+1));
    SynEdit.BlockEnd := TBufferCoord(Point(Column+Length(FindText), Line+1));
    Result := True;
  end else Result := False;
end;

procedure TFormPrincipale.UpdateEditCommands;
begin
  Couper.Enabled := True;
  Copier.Enabled := True;
  Coller.Enabled := True;
  Defaire.Enabled := True;
  Refaire.Enabled := True;
end;

procedure TFormPrincipale.OpenLab(FileName : string);
begin
  if not FileExists(FileName) then exit;
  try
    Labyrinthe := TLabyrinthe.CreateActions(FileName, EnTete.Lines);
  except
    on E : EIsSauvegarde do
    begin
      Labyrinthe.Free;
      ShowMes('Sauvegarde', E.Message, MB_OK or MB_ICONERROR);
      exit;
    end;
  end;
  Caption := Application.Title+' - ['+Labyrinthe.NomLab+']';
  FormPlan.Ouvre;
  FormPlan.Show;
  Show;
  Ouvrir.Enabled := False;
  Enregistrer.Enabled := True;
  EnregSous.Enabled := True;
  Fermer.Enabled := True;
  Tester.Enabled := True;
  BigMenuEdition.Visible := True;
  UpdateEditCommands;
  BigMenuRechercher.Visible := True;
  Chercher.Enabled := True;
  BigMenuVoir.Visible := True;
  VoirPlanLaby.Enabled := True;
  VoirPlanLaby.Checked := True;
  EnTete.Enabled := True;
  EnTete.Modified := False;
  EnTete.SetFocus;
end;

procedure TFormPrincipale.SaveLab(FileName : string = '');
begin
  if FileName = '' then FileName := Labyrinthe.FileName;
  Labyrinthe.EnregistrerActions(FileName);
  EnTete.Modified := False;
  Caption := Application.Title+' - ['+Labyrinthe.NomLab+']';
end;

function TFormPrincipale.CloseLab : boolean;
begin
  Result := False;
  if EnTete.Modified then
  begin
    case ShowDialog(Application.Title, 'Voulez-vous enregistrer les'+#10+
                    'modifications apportées à '+Labyrinthe.NomLab+' ?',
                    dtConfirmation, dbYesNoCancel) of
      drYes : SaveLab;
      drCancel : exit;
    end;
  end;
  Result := True;
  FormPlan.Hide;
  Labyrinthe.Free;
  Labyrinthe := nil;
  Ouvrir.Enabled := True;
  Enregistrer.Enabled := False;
  EnregSous.Enabled := False;
  Fermer.Enabled := False;
  Tester.Enabled := False;
  BigMenuEdition.Visible := False;
  Couper.Enabled := False;
  Copier.Enabled := False;
  Coller.Enabled := False;
  Defaire.Enabled := False;
  Refaire.Enabled := False;
  BigMenuRechercher.Visible := False;
  Chercher.Enabled := False;
  OccSuivante.Enabled := False;
  BigMenuVoir.Visible := False;
  VoirPlanLaby.Checked := False;
  VoirPlanLaby.Enabled := False;
  EnTete.Lines.Clear;
  EnTete.Enabled := False;
  EnTete.Modified := False;
  Caption := Application.Title;
end;

procedure TFormPrincipale.FormCreate(Sender: TObject);
begin
  {$I-}
    MkDir(Dir+'Labyrinthes');
  {$I+}
  Application.HelpFile := Dir+'EditActions.hlp';
  Menu := BigMenu;
  Ouvrir.Dialog.InitialDir := Dir+'Labyrinthes';
  EnregSous.Dialog.InitialDir := Dir+'Labyrinthes';
  Labyrinthe := nil;
end;

procedure TFormPrincipale.FormShow(Sender: TObject);
begin
  OnShow := nil;
  FormPlan.Form := Self;
  if ParamCount > 0 then OpenLab(ParamStr(1));
end;

procedure TFormPrincipale.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (Labyrinthe <> nil) and (not CloseLab) then CanClose := False;
end;

procedure TFormPrincipale.OuvrirExecute(Sender: TObject);
begin
  OpenLab(Ouvrir.Dialog.FileName);
end;

procedure TFormPrincipale.EnregistrerExecute(Sender: TObject);
begin
  SaveLab;
end;

procedure TFormPrincipale.EnregSousExecute(Sender: TObject);
begin
  SaveLab(EnregSous.Dialog.FileName);
end;

procedure TFormPrincipale.FermerExecute(Sender: TObject);
begin
  CloseLab;
end;

procedure TFormPrincipale.TesterExecute(Sender: TObject);
begin
  if EnTete.Modified then SaveLab;
  ShellExecute(0, nil, PChar(Dir+'Labyrinthe.exe'),
    PChar('"'+Labyrinthe.FileName+'" TestActions'), nil, SW_SHOWNORMAL);
  Close;
end;

procedure TFormPrincipale.QuitterExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormPrincipale.CouperExecute(Sender: TObject);
begin
  EnTete.CutToClipboard;
end;

procedure TFormPrincipale.CopierExecute(Sender: TObject);
begin
  EnTete.CopyToClipboard;
end;

procedure TFormPrincipale.CollerExecute(Sender: TObject);
begin
  EnTete.PasteFromClipboard;
end;

procedure TFormPrincipale.DefaireExecute(Sender: TObject);
begin
  EnTete.Undo;
end;

procedure TFormPrincipale.RefaireExecute(Sender: TObject);
begin
  EnTete.Redo;
end;

procedure TFormPrincipale.ChercherExecute(Sender: TObject);
begin
  OccSuivante.Enabled := False;
  FindDialog.FindText := EnTete.WordAtCursor;
  FindDialog.Execute;
end;

procedure TFormPrincipale.OccSuivanteExecute(Sender: TObject);
begin
  FindDialog.CloseDialog;
  OccSuivante.Enabled := True;
  if not SynEditSearch(EnTete, FindDialog.FindText,
                       frDown in FindDialog.Options,
                       frMatchCase in FindDialog.Options,
                       frWholeWord in FindDialog.Options) then
    ShowDialog('Recherche', 'Le texte recherché '''+FindDialog.FindText+
               ''' n''a pas été trouvé', dtInformation, dbOK);
end;

procedure TFormPrincipale.FindDialogShow(Sender: TObject);
begin
  OccSuivante.Enabled := False;
end;

procedure TFormPrincipale.FindDialogClose(Sender: TObject);
begin
  OccSuivante.Enabled := FindDialog.FindText <> '';
end;

procedure TFormPrincipale.VoirPlanLabyExecute(Sender: TObject);
begin
  VoirPlanLaby.Checked := not VoirPlanLaby.Checked;
  if VoirPlanLaby.Checked then FormPlan.Show else FormPlan.Hide;
end;

procedure TFormPrincipale.VoirCodesCasesExecute(Sender: TObject);
begin
  VoirCodesCases.Checked := not VoirCodesCases.Checked;
  if VoirCodesCases.Checked then FormCodesCases.Show else FormCodesCases.Hide;
end;

procedure TFormPrincipale.RubriquesAideExecute(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormPrincipale.AProposExecute(Sender: TObject);
begin
  AboutDialog.Execute;
end;

procedure TFormPrincipale.EnvoyerEMailExecute(Sender: TObject);
begin
  RunURL('mailto:seb.doeraene@belgacom.net?subject=A propos de FunLabyrinthe');
end;

end.
