unit LabyrintheMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, LabyrintheUtils, ScUtils, ScStrUtils,
  SdDialogs, ShellAPI;

type
  TFormPrincipale = class(TForm)
    Image: TImage;
    Barre: TStatusBar;
    BigMenu: TMainMenu;
    MenuFichier: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuNouveau: TMenuItem;
    MenuEnregistrer: TMenuItem;
    MenuCharger: TMenuItem;
    Sep2: TMenuItem;
    MenuAide: TMenuItem;
    MenuRubrAide: TMenuItem;
    Sep3: TMenuItem;
    MenuAPropos: TMenuItem;
    Ouvrir: TOpenDialog;
    Sauver: TSaveDialog;
    Sep1: TMenuItem;
    MenuDescription: TMenuItem;
    MenuOptions: TMenuItem;
    MenuIndices: TMenuItem;
    MenuProprietes: TMenuItem;
    MenuPropLabyrinthe: TMenuItem;
    MenuPropJoueur: TMenuItem;
    MenuRecommencer: TMenuItem;
    AboutDialog: TSdAboutDialog;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure MenuNouveauClick(Sender: TObject);
    procedure MenuChargerClick(Sender: TObject);
    procedure MenuRubrAideClick(Sender: TObject);
    procedure MenuAProposClick(Sender: TObject);
    procedure MenuEnregistrerClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuDescriptionClick(Sender: TObject);
    procedure MenuPropLabyrintheClick(Sender: TObject);
    procedure MenuPropJoueurClick(Sender: TObject);
    procedure MenuIndicesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BarreEtat;
    procedure MenuRecommencerClick(Sender: TObject);
  private
    { Déclarations privées }
    HiddenBitmap : TBitmap;
    function GetEcran(X, Y : integer) : integer;
    procedure SetEcran(X, Y, SC : integer);
    function SaveCurrentGame : boolean;
  public
    { Déclarations publiques }
    PartieEnCours : boolean;
    procedure Commencer(FileName : string);
    procedure Terminer;
    procedure Affiche(Jou, Reel, InverserComm : boolean);
    property Ecran[X, Y : integer] : integer read GetEcran write SetEcran;
  end;

  //TJoueur = class
  //private
    //function GetBouee : boolean;
    //function GetPlanche : boolean;
  //public
    //property ABouee         : boolean read GetBouee;
    //property APlanche       : boolean read GetPlanche;
    procedure TrouveObjet (Obj : integer);
    procedure UtiliseObjet(Obj : integer);
    function AObjet(Obj : integer) : boolean;
    function Blocage(Terrain : integer) : boolean;
  //end;

const
  clBrun         = $00004080;
  clBrunClair    = $00006699;
  InterditPlanche = -1;
  CasesPlanche   = [Eau, Trou, Ciel];
  Blocs          = [BlocArgent, BlocOr];
  Fleches        = [Nord, Est, Sud, Ouest, Carrefour, MoulinDirect, MoulinIndirect];
  Teleportent    = TeleporteurS + TeleporteurP + TeleporteurA +
                   Escalier + [EscalierM, EscalierD,
                   EscalierMontant, EscalierDescendant];
  Objets         = [Bouee, Planche, CleArgent, CleOr];
  Epreuves       = [Eau, Trou, BlocArgent, BlocOr, Ciel];

var
  FormPrincipale : TFormPrincipale;
  Labyrinthe : TLabyrinthe;
  //Joueur : TJoueur;
  NomDuFichier : string;

  LaCase : T3DPoint;
  LaPosition : TPoint;
  Cols, Rows, Etas : integer;

  EpreuvesPerso, CasesSansHerbe : set of Byte;

function GetRect(X, Y : integer) : TRect;
function MessageRate(SorteCase : integer) : string;
function MessageTrouve(SorteCase : integer) : string;
function Montant(X, Y, Z : integer) : boolean;

procedure Suivant  (var X, Y, Z : integer);
procedure BougeAscenseur(X, Y : integer; var Z : integer);
procedure DessineBarque(Endroit : TRect; Canvas : TCanvas);

implementation

{$R *.DFM}

uses LiftDialog, PropertiesDialog;

////////////////////////////////////////
/// Procédures et fonctions globales ///
////////////////////////////////////////

function GetRect(X, Y : integer) : TRect;
begin
  Result := Rect(X*30+30, Y*30+30, X*30+60, Y*30+60);
end;

function MessageRate(SorteCase : integer) : string;
begin
  case SorteCase of
    InterditPlanche : Result := 'Impossible de passer ici avec la planche !';
    Trou       : Result := 'T''es pas bien de vouloir sauter dans ce trou !?';
    Eau        : Result := 'Sans bouée, on coule dans l''eau.';
    BlocArgent : Result := 'Ce bloc ne disparaîtra qu''avec une clé d''argent.';
    BlocOr     : Result := 'Ce bloc ne disparaîtra qu''avec une clé d''or.';
    Ciel       : Result := 'Tu ne peux pas voler !';
    else Result := '';
  end;
end;

function MessageTrouve(SorteCase : integer) : string;
begin
  case SorteCase of
    Bouee     : Result := 'Tu as trouvé une bouée.'+#10+
                          'Tu peux aller dans l''eau.';
    Planche   : Result := 'Tu as trouvé une planche.'+#10+
                          'Tu peux franchir certains obstacles.';
    CleArgent : Result := 'Tu as trouvé une clé d''argent.'+#10+
                          'Tu peux faire disparaître un bloc en argent.';
    CleOr :     Result := 'Tu as trouvé une clé d''or.'+#10+
                          'Tu peux faire disparaître un bloc en or.';
    else Result := '';
  end;
end;

function Montant(X, Y, Z : integer) : boolean;
var AncX, AncY, AncZ : integer;
begin
  AncX := X;
  AncY := Y;
  AncZ := Z;
  Suivant(X, Y, Z);
  Result := True;
  if Z > AncZ then exit;
  if (Z = AncZ) and (Y > AncY) then exit;
  if (Z = AncZ) and (Y = AncY) and (X >= AncX) then exit;
  Result := False;
end;

procedure Suivant(var X, Y, Z : integer);
var Anc : integer;
begin
  Anc := Labyrinthe[X, Y, Z];
  repeat
    inc(X);
    if X = Cols*7 then
    begin
      X := 0;
      inc(Y);
      if Y = Rows*7 then
      begin
        Y := 0;
        inc(Z);
        if Z > Etas then Z := 1;
      end;
    end;
  until Labyrinthe[X, Y, Z] = Anc;
end;

procedure BougeAscenseur(X, Y : integer; var Z : integer);
var Debut, Fin : integer;
begin
  Debut := Z;
  Fin := Z;
  repeat inc(Fin  ) until Labyrinthe[X, Y, Fin  ] <> Ascenseur;
  repeat dec(Debut) until Labyrinthe[X, Y, Debut] <> Ascenseur;
  FormBougeAscenseur.QueryEtage(Debut+1, Fin-1, Z);
end;

procedure DessineBarque(Endroit : TRect; Canvas : TCanvas);
var B : TBitmap;
begin
  B := TBitmap.Create;
  B.Height := 30;
  B.Width := 30;
  with B.Canvas do
  begin
    CopyRect(Rect(0, 0, 30, 30), Canvas, Endroit);
    case Direction of
      Haut :
      begin
        Pen.Color := clBrun;
        Pen.Width := 2;
        Arc(-5, -2, 25, 24, 25, 12, 15,  2);
        Arc( 5, -2, 35, 24, 15,  2,  5, 12);
        MoveTo( 5, 12);
        LineTo( 5, 29);
        LineTo(25, 29);
        LineTo(25, 12);
        Brush.Color := clBrunClair;
        FloodFill(15, 15, clBrun, fsBorder);
      end;
      Droite :
      begin
        Pen.Color := clBrun;
        Pen.Width := 2;
        Arc(4, -5, 32, 25, 18, 25, 30, 15);
        Arc(4,  5, 32, 35, 30, 15, 18,  5);
        MoveTo(18,  5);
        LineTo( 1,  5);
        LineTo( 1, 25);
        LineTo(18, 25);
        Brush.Color := clBrunClair;
        FloodFill(15, 15, clBrun, fsBorder);
      end;
      Bas :
      begin
        Pen.Color := clBrun;
        Pen.Width := 2;
        Arc( 5, 4, 35, 32,  5, 18, 15, 30);
        Arc(-5, 4, 25, 32, 15, 30, 25, 18);
        MoveTo( 5, 18);
        LineTo( 5,  1);
        LineTo(25,  1);
        LineTo(25, 18);
        Brush.Color := clBrunClair;
        FloodFill(15, 15, clBrun, fsBorder);
      end;
      else
      begin
        Pen.Color := clBrun;
        Pen.Width := 2;
        Arc(-2,  5, 26, 35, 12,  5,  0, 15);
        Arc(-2, -5, 26, 25,  0, 15, 12, 25);
        MoveTo(10,  5);
        LineTo(29,  5);
        LineTo(29, 25);
        LineTo(12, 25);
        Brush.Color := clBrunClair;
        FloodFill(15, 15, clBrun, fsBorder);
      end;
    end;
  end;
  Canvas.CopyRect(Endroit, B.Canvas, Rect(0, 0, 30, 30));
  B.Free;
end;

//////////////////////////////////////////
/// Procédures et fonctions de TJoueur ///
//////////////////////////////////////////

{function TJoueur.GetBouee : boolean;
begin
  Result := Bouees > 0;
end;

function TJoueur.GetPlanche : boolean;
begin
  Result := Planches > 0;
end; }

procedure TrouveObjet(Obj : integer);
begin
  case Obj of
    Bouee     : inc(Bouees);
    Planche   : inc(Planches);
    CleArgent : inc(ClesArgent);
    CleOr     : inc(ClesOr);
  end;
end;

procedure UtiliseObjet(Obj : integer);
begin
  case Obj of
    CleArgent, BlocArgent : if ClesArgent > 0 then dec(ClesArgent);
    CleOr,     BlocOr     : if ClesOr     > 0 then dec(ClesOr);
  end;
end;

function AObjet(Obj : integer) : boolean;
begin
  case Obj of
    Bouee                 : Result := Bouees > 0;
    193..202              : Result := NoBarque > 0;
    Eau                   : Result := (Bouees > 0) or (NoBarque > 0);
    Planche,   Trou, Ciel : Result := Planches > 0;
    CleArgent, BlocArgent : Result := ClesArgent > 0;
    CleOr,     BlocOr     : Result := ClesOr > 0;
    else Result := True;
  end;
end;

function Blocage(Terrain : integer) : boolean;
begin
  case Terrain of
    Eau, Trou, Ciel : Result := not AObjet(Terrain);
    BlocArgent, BlocOr, FauxMur : Result := not (AObjet(Terrain) and TouchePressee);
    else Result := False;
  end;
end;

//////////////////////////////////////////////////
/// Procédures et fonctions de TFormPrincipale ///
//////////////////////////////////////////////////

function TFormPrincipale.SaveCurrentGame : boolean;
begin
  if not PartieEnCours then Result := True else
  begin
    {$I-}
      MkDir(Dir+'Sauvegardes');
    {$I+}
    Sauver.FileName := '';
    Sauver.InitialDir := Dir+'Sauvegardes';
    if Sauver.Execute then
    begin
      Labyrinthe.Sauvegarder
      (
        Sauver.FileName,
        LaCase.X*7+LaPosition.X, LaCase.Y*7+LaPosition.Y, LaCase.Z,
        Bouees, Planches, ClesArgent, ClesOr,
        NoBarque, MenuIndices.Checked
      );
      Result := True;
    end else Result := False;
  end;
end;

procedure TFormPrincipale.Commencer(FileName : string);
var I : integer;
begin
  MenuRecommencer.Enabled := False;
  if not FileExists(FileName) then exit;
  if PartieEnCours then Terminer;
  Labyrinthe.Free;
  Direction := SansDirection;
  AncDirection := SansDirection;
  TouchePressee := False;
  Labyrinthe := TLabyrinthe.CreateOpen(FileName);
  EpreuvesPerso := [];
  for I := 1 to 45 do if Labyrinthe.Boutons[I].Style = sEpreuve then
  case I of
    1..15  : EpreuvesPerso := EpreuvesPerso + [I+32];
    16..45 : EpreuvesPerso := EpreuvesPerso + [I+145]
  end;
  for I := 1 to 30 do if Labyrinthe.Teleporteurs[I].Style = sEpreuve then
    EpreuvesPerso := EpreuvesPerso + [I+96];
  CasesSansHerbe := [Mur, Eau, Trou, BlocArgent, BlocOr, Ciel] + EpreuvesPerso;
  NomDuFichier := FileName;
  PartieEnCours := True;
  MenuEnregistrer.Enabled := True;
  MenuDescription.Enabled := True;
  MenuProprietes.Enabled := True;
  Caption := Labyrinthe.NomLab;
  Cols := Labyrinthe.Dimensions.Colonnes;
  Rows := Labyrinthe.Dimensions.Lignes;
  Etas := Labyrinthe.Dimensions.Etages;
  //Joueur := TJoueur.Create;
  //Joueur.Barque := Labyrinthe.InfosJeu.Barque;
  BarreEtat;
  MenuIndices.Checked := Indices;
  LaCase     := Labyrinthe.Position.LaCase;
  LaPosition := Labyrinthe.Position.LaPosition;
  if Ecran[LaPosition.X, LaPosition.Y] in Barque then
  begin
    NoBarque := Ecran[LaPosition.X, LaPosition.Y]-192;
    Ecran[LaPosition.X, LaPosition.Y] := Eau;
  end;
  Affiche(True, True, False);
  MenuRecommencer.Enabled := True;
end;

procedure TFormPrincipale.Terminer;
begin
  //Joueur.Free;
  //Labyrinthe.Free;
  PartieEnCours := False;
  MenuEnregistrer.Enabled := False;
  MenuDescription.Enabled := False;
  MenuProprietes.Enabled := False;
  //Caption := 'FunLabyrinthe';
end;

procedure TFormPrincipale.Affiche(Jou, Reel, InverserComm : boolean);
var X, Y : integer;
    StBouton : TStyleBouton;
begin
  if PartieEnCours then with HiddenBitmap.Canvas do
  begin
    for X := -1 to 7 do for Y := -1 to 7 do
    begin
      StBouton := Labyrinthe.StyleBouton(Ecran[X,Y]);
      if (StBouton in StylesPerso) then
        CopyRect(GetRect(X, Y), Labyrinthe.CodeToImage(Ecran[X, Y]).Canvas, Rect(0, 0, 30, 30)) else
        CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(Ecran[X, Y], Labyrinthe.StyleBouton(Ecran[X, Y])));
    end;
    if Jou then
    begin
      X := LaPosition.X;
      Y := LaPosition.Y;
      if (Ecran[X, Y] = Eau) and (NoBarque = 0) and (Bouees > 0) and (CouleurPion <> clInvisible) then
      begin
        Brush.Color := clYellow;
        Pen.Color := clYellow;
        Ellipse(X*30+31, Y*30+31, X*30+59, Y*30+59);
      end else if Ecran[X, Y] = Ascenseur then
        CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(AscenseurOuvert, sPasBouton))
      else if (Ecran[X, Y] in Bouton) then
      begin
        StBouton := Labyrinthe.StyleBouton(Ecran[X,Y]);
        case StBouton of
          sPoussoir       : CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(BoutonEnfonce, sPasBouton));
          sCommutateurOff : if InverserComm then
            CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(33, sCommutateurOn));
          sCommutateurOn  : if InverserComm then
            CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(33, sCommutateurOff));
        end;
      end;
      if CouleurPion <> clInvisible then
      begin
        if NoBarque <> 0 then
          DessineBarque(GetRect(X, Y), HiddenBitmap.Canvas);
        Brush.Color := CouleurPion;
        Pen.Color := CouleurPion;
        Ellipse(X*30+36, Y*30+36, X*30+54, Y*30+54);
      end;
    end;
  end;
  if Reel then Image.Picture.Assign(HiddenBitmap);
end;

procedure TFormPrincipale.FormCreate(Sender: TObject);
begin
  Application.HelpFile := Dir+'Labyrinthe.hlp';
  Menu := BigMenu;
  ClientHeight := 289;
  HiddenBitmap := TBitmap.Create;
  with HiddenBitmap do
  begin
    Width := 270;
    Height := 270;
    with Canvas do
    begin
      Brush.Color := clBlack;
      Pen.Color := clBlack;
      Rectangle(0, 0, 270, 270);
    end;
  end;
  Image.Picture.Assign(HiddenBitmap);
  PartieEnCours := False;
  if ParamStr(2) = 'TestActions' then TestActions := True;
  Commencer(ParamStr(1));
end;

function TFormPrincipale.GetEcran(X, Y : integer) : integer;
begin
  inc(X, LaCase.X*7);
  inc(Y, LaCase.Y*7);
  Result := Labyrinthe[X, Y, LaCase.Z];
  if Result in Escalier then
    if Montant(X, Y, LaCase.Z) then Result := EscalierM
                               else Result := EscalierD;
end;

procedure TFormPrincipale.SetEcran(X, Y, SC : integer);
begin
  Labyrinthe[LaCase.X*7 + X, LaCase.Y*7 + Y, LaCase.Z] := SC;
end;

procedure TFormPrincipale.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPrincipale.MenuNouveauClick(Sender: TObject);
begin
  {$I-}
    MkDir(Dir+'Labyrinthes');
  {$I+}
  Ouvrir.Title := 'Nouvelle partie';
  Ouvrir.FileName := '';
  Ouvrir.InitialDir := Dir+'Labyrinthes';
  if not Ouvrir.Execute then exit;
  if ofExtensionDifferent in Ouvrir.Options then
    RunURL(Ouvrir.FileName)
  else
  begin
    TestActions := False;
    Commencer(Ouvrir.FileName);
  end;
end;

procedure TFormPrincipale.MenuChargerClick(Sender: TObject);
begin
  {$I-}
    MkDir(Dir+'Sauvegardes');
  {$I+}
  Ouvrir.Title := 'Charger une partie';
  Ouvrir.FileName := '';
  Ouvrir.InitialDir := Dir+'Sauvegardes';
  if not Ouvrir.Execute then exit;
  if ofExtensionDifferent in Ouvrir.Options then
    RunURL(Ouvrir.FileName)
  else
    Commencer(Ouvrir.FileName);
end;

procedure TFormPrincipale.MenuRubrAideClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormPrincipale.MenuAProposClick(Sender: TObject);
begin
  AboutDialog.Execute;
end;

procedure TFormPrincipale.MenuEnregistrerClick(Sender: TObject);
begin
  SaveCurrentGame;
end;

procedure TFormPrincipale.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
/// VARIABLES
var X, _X, X_, Y, _Y, Y_, Z, _Z : integer;
    No : integer;
    SC : integer;
    Points : array [0..3] of TPoint;
    Point3D : T3DPoint;
    //Infos : TInfosJeu;
    //AncDirection : TFleche;
    Bouge, ChangeDeZone : boolean;
    EstDansTourniquet : integer;
/// PROCEDURES
procedure AvantCaseActive;
begin
 {Infos.Bouees     := Joueur.Bouees;
 Infos.Planches   := Joueur.Planches;
 Infos.ClesArgent := Joueur.NbreClesArgent;
 Infos.ClesOr     := Joueur.NbreClesOr;
 Infos.Barque     := Joueur.Barque;  }
 Indices    := MenuIndices.Checked;
 Point3D.X := LaCase.X*7 + X;
 Point3D.Y := LaCase.Y*7 + Y;
 Point3D.Z := LaCase.Z;
 Labyrinthe.Reussite := 0;
end;  //AvantCaseActive
procedure ApresCaseActive(Epreuve, MesFin, ABouge : boolean);
var ChangeDeZone, SurLeBord : boolean;
begin
 ChangeDeZone := False;
 {Joueur.Bouees         := Infos.Bouees;
 Joueur.Planches       := Infos.Planches;
 Joueur.NbreClesArgent := Infos.ClesArgent;
 Joueur.NbreClesOr     := Infos.ClesOr; }
 if ABouge then
 begin
   if (LaCase.X <> Div7(Point3D.X)) or
      (LaCase.Y <> Div7(Point3D.Y)) or
      (LaCase.Z <> Point3D.Z) then ChangeDeZone := True;
   LaCase.X := Div7(Point3D.X);
   LaCase.Y := Div7(Point3D.Y);
   LaCase.Z := Point3D.Z;
   LaPosition.X := Mod7(Point3D.X);
   LaPosition.Y := Mod7(Point3D.Y);
 end;
 BarreEtat;
 SurLeBord := False;
 if Ecran[LaPosition.X, LaPosition.Y] = Vide then
 begin
   if LaCase.X = -1 then
   begin
     LaCase.X := 0;
     LaPosition.X := -1;
     SurLeBord := True;
   end;
   if LaCase.Y = -1 then
   begin
     LaCase.Y := 0;
     LaPosition.Y := -1;
     SurLeBord := True;
   end;
   if LaCase.X = Labyrinthe.Dimensions.Colonnes then
   begin
     LaCase.X := Labyrinthe.Dimensions.Colonnes - 1;
     LaPosition.X := 7;
     SurLeBord := True;
   end;
   if LaCase.Y = Labyrinthe.Dimensions.Lignes then
   begin
     LaCase.Y := Labyrinthe.Dimensions.Lignes - 1;
     LaPosition.Y := 7;
     SurLeBord := True;
   end;
 end;
 if ChangeDeZone and (not SurLeBord) and Labyrinthe.ActionsZoneExists(LaCase.X, LaCase.Y, LaCase.Z) then
 begin
   Labyrinthe.ExecuteChangeDeZone(Point3D, LaCase.X, LaCase.Y, LaCase.Z);
   ApresCaseActive(False, False, True);
 end;
end;  //ApresCaseActive
procedure TerminerSiGagne;
begin
  if AGagne then
  begin
    Labyrinthe.Reussite := 1; //pour le tourniquet
    Labyrinthe.AutorisePlanche := False;
    Labyrinthe.Poursuivre(False);
    if Labyrinthe.FileInfos.Sauvegarde then
    begin
      if MessageDlg('Voulez-vous détruire'+#10+'la sauvegarde ?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        DeleteFile(NomDuFichier);
        MenuRecommencer.Enabled := False;
      end;
    end;
    Terminer;
  end;
end; //TerminerSiGagne
procedure AugmenteDirection;
begin
 Direction := TFleche(integer(Direction) mod 4 + 1);
end;
procedure DiminueDirection;
begin
 Direction := TFleche((integer(Direction) + 2) mod 4 + 1);
end;
procedure SortDuTourniquet;
begin
 if Ecran[LaPosition.X, LaPosition.Y] = MoulinDirect then
 begin
   Ecran[LaPosition.X, LaPosition.Y] := MoulinIndirect;
   Affiche(True, True, False);
 end else
 if Ecran[LaPosition.X, LaPosition.Y] = MoulinIndirect then
 begin
   Ecran[LaPosition.X, LaPosition.Y] := MoulinDirect;
   Affiche(True, True, False);
 end;
 EstDansTourniquet := 0;
end;
function ChangeZone : boolean;
begin
  if Ecran[X, Y] = Vide then
  begin
    LaPosition.X := X;
    LaPosition.Y := Y;
    result := true;
    exit;
  end;
  ChangeDeZone := False;
  if X < 0 then
  begin
    LaPosition.X := 7+X;
    dec(LaCase.X);
    ChangeDeZone := True;
  end else if X > 6 then
  begin
    LaPosition.X := X-7;
    inc(LaCase.X);
    ChangeDeZone := True;
  end else LaPosition.X := X;
  if Y < 0 then
  begin
    LaPosition.Y := 7+Y;
    dec(LaCase.Y);
    ChangeDeZone := True;
  end else if Y > 6 then
  begin
    LaPosition.Y := Y-7;
    inc(LaCase.Y);
    ChangeDeZone := True;
  end else LaPosition.Y := Y;
  if ChangeDeZone then
  begin
    X := LaPosition.X;
    Y := LaPosition.Y;
    if Labyrinthe.ActionsZoneExists(LaCase.X, LaCase.Y, LaCase.Z) then
    begin
      AvantCaseActive;
      Labyrinthe.ExecuteChangeDeZone(Point3D, LaCase.X, LaCase.Y, LaCase.Z);
      ApresCaseActive(False, False, True);
    end;
  end;
  result := false;
end;
/// LABELS
label OnRCommence, Fait, SuitePlanche, UtilisePlanche;
/// CORPS
begin
  if not PartieEnCours then exit;
  TouchePressee := True;
  EstDansTourniquet := 0;
  AncDirection := Direction;
  case Key of
    vk_Up    : Direction := Haut;
    vk_Down  : Direction := Bas;
    vk_Left  : Direction := Gauche;
    vk_Right : Direction := Droite;
    else exit;
  end;
  X := LaPosition.X;
  Y := LaPosition.Y;
OnRCommence :
  Labyrinthe.Attendu := False;
  if Direction = SansDirection then goto fait; //4.2.3
  Labyrinthe.Etape := 1;         //4.2.2
  Labyrinthe.Continuer := False; //4.2.2
  case Direction of
    Haut   : dec(Y);
    Bas    : inc(Y);
    Gauche : dec(X);
    Droite : inc(X);
  end;
  if (NoBarque <> 0) and (Direction <> AncDirection) and (Ecran[X,Y] in CasesSansHerbe) then
  begin
    Affiche(True, True, False);
    exit;
  end;
  if Ecran[X, Y] = Mur then if (EstDansTourniquet > 0) then goto Fait else exit;
  Labyrinthe.AutorisePlanche := ((Ecran[X, Y] = Trou) or (Ecran[X,Y] = Ciel) or
     ((Ecran[X, Y] = Eau) and (Bouees = 0) and (NoBarque = 0)));
UtilisePlanche:
  if (Planches > 0) and Labyrinthe.AutorisePlanche then
  begin
    _X := X; X_ := X;
    _Y := Y; Y_ := Y;
    case Direction of
      Haut   : begin dec(_Y); inc (Y_) end;
      Bas    : begin inc(_Y); dec (Y_) end;
      Gauche : begin dec(_X); inc (X_) end;
      Droite : begin inc(_X); dec (X_) end;
    end;
    if (Ecran[X,Y] in CasesPlanche) and (Ecran[_X,_Y] in CasesSansHerbe) and (Ecran[X_,Y_] <> Ecran[_X,_Y]) then  //4.3
    begin
      if TouchePressee then
      begin
        if (Ecran [X,Y] = Eau) and (Ecran[_X, _Y] = Eau) then SC := Eau
        else if (Ecran [X,Y] = Ciel) and (Ecran[_X, _Y] = Ciel) then SC := Ciel
        else SC := InterditPlanche;
        ShowMes('Impasse', MessageRate(SC), MB_OK or MB_ICONSTOP);
      end;
      if (EstDansTourniquet > 0) then goto Fait else exit;
    end;
    {if (_X = -1) or (_Y = -1) or (_X = 7) or (_Y = 7) then
    begin
      if TouchePressee then
        ShowMes('Impasse', MessageRate(InterditPlanche), MB_OK or MB_ICONSTOP);
      if (EstDansTourniquet > 0) then goto Fait else exit;
    end; }
    OnKeyDown := nil;
    if NoBarque <> 0 then
    begin
      Ecran[LaPosition.X, LaPosition.Y] := 192+NoBarque;
      NoBarque := 0;
    end;
    if X <> _X then
    begin
      Points[0].X := X*30+27; Points[0].Y := Y*30+36;
      Points[1].X := X*30+63; Points[1].Y := Y*30+36;
      Points[2].X := X*30+63; Points[2].Y := Y*30+54;
      Points[3].X := X*30+27; Points[3].Y := Y*30+54;
    end else
    begin
      Points[0].X := X*30+36; Points[0].Y := Y*30+27;
      Points[1].X := X*30+36; Points[1].Y := Y*30+63;
      Points[2].X := X*30+54; Points[2].Y := Y*30+63;
      Points[3].X := X*30+54; Points[3].Y := Y*30+27;
    end;
    Affiche(True, False, False);
    with HiddenBitmap.Canvas do
    begin
      Brush.Color := clBrun;
      Pen.Color := clBrun;
      Polygon(Points);
    end;
    Image.Picture.Assign(HiddenBitmap);
    Application.ProcessMessages;
    Sleep(Temporisation);
    if EstDansTourniquet > 0 then SortDuTourniquet;
    Affiche(False, False, False);
    with HiddenBitmap.Canvas do
    begin
      Brush.Color := clBrun;
      Pen.Color := clBrun;
      Polygon(Points);
      if CouleurPion <> clInvisible then
      begin
        Brush.Color := CouleurPion;
        Pen.Color := CouleurPion;
        Ellipse(X*30+36, Y*30+36, X*30+54, Y*30+54);
      end;
    end;
    Image.Picture.Assign(HiddenBitmap);
    Application.ProcessMessages;
    Sleep(Temporisation);
    //if X <> _X then LaPosition.X := _X else LaPosition.Y := _Y;
    X := _X; Y := _Y;
    OnKeyDown := FormKeyDown;
    goto SuitePlanche;
  end;
  if Blocage(Ecran[X, Y]) then
  begin
    if TouchePressee then
      ShowMes('Impasse', MessageRate(Ecran[X, Y]), MB_OK or MB_ICONSTOP);
    if EstDansTourniquet > 0 then goto Fait else exit;
  end;
  if Ecran[X, Y] in EpreuvesPerso then
  begin
    AvantCaseActive;
    if (Ecran[X,Y] in Bouton) then Bouge := Labyrinthe.ExecuteBouton(CodeToNoBouton(Ecran[X, Y]), Point3D)
                              else Bouge := Labyrinthe.ExecuteTeleporteur(Ecran[X,Y]-96, Point3D);
    if Bouge and (NoBarque > 0) then
    begin
      if (Labyrinthe[Point3D.X,Point3D.Y,Point3D.Z] = Eau) then Direction := AncDirection else
      begin
        Ecran[LaPosition.X, LaPosition.Y] := 192+NoBarque;
        NoBarque := 0;
      end;
    end;
    if (Labyrinthe.Reussite = 1) and (EstDansTourniquet > 0) then SortDuTourniquet;
    ApresCaseActive(True, False, Bouge);
    Affiche(True, True, False);
    TerminerSiGagne;
    if Labyrinthe.AutorisePlanche and (Planches > 0) then goto UtilisePlanche;
    if Labyrinthe.Continuer then     //4.2.2
    begin                            //4.2.2
      Labyrinthe.Attendu := False;   //4.2.2
      goto Fait;                     //4.2.2
    end;                             //4.2.2
    if (Labyrinthe.Reussite = 1) or (EstDansTourniquet = 0) then exit else goto Fait;
  end;
  if EstDansTourniquet > 0 then SortDuTourniquet;
  if Ecran[X, Y] in Blocs then
  begin
    UtiliseObjet(Ecran[X, Y]);
    case Ecran[X, Y] of
      BlocArgent : Barre.Panels[2].Text := IntToStr(ClesArgent)+' clé(s) d''argent';
      BlocOr     : Barre.Panels[3].Text := IntToStr(ClesOr)    +' clé(s) d''or';
    end;
    Ecran[X, Y] := Herbe;
    Affiche(True, True, False);
    exit;
  end;
  if Ecran[X, Y] = FauxMur then
  begin
    Ecran[X, Y] := Herbe;
    Affiche(True, True, False);
    exit;
  end;
  if (NoBarque <> 0) and (Ecran[X, Y] <> Eau) then
  begin
    Ecran[LaPosition.X, LaPosition.Y] := 192+NoBarque;
    NoBarque := 0;
  end;
SuitePlanche:
  ChangeZone;
Fait:
  Labyrinthe.Etape := 2; //4.2.2
  Affiche(True, True, True);
  X := LaPosition.X;
  Y := LaPosition.Y;
  if (Ecran[X,Y] = Vide) or (Ecran[X,Y] = Tresor)
    or ( Labyrinthe.Continuer and
    ( ((Ecran[X,Y] = Eau) and (NoBarque = 0) and (Bouees = 0)) //4.3
    or (Ecran[X,Y] = Trou) or (Ecran[X,Y] = Ciel) ) ) then
  begin
    OnKeyDown := nil;
    Application.ProcessMessages;
    OnKeyDown := FormKeyDown;
    AvantCaseActive;
    Labyrinthe.MessageFin(Point3D, Ecran[X,Y]);
    ApresCaseActive(False, True, True);
    X := LaPosition.X;
    Y := LaPosition.Y;
    if Ecran[X, Y] in Barque then
    begin
      NoBarque := Ecran[X,Y]-192;
      Ecran[X,Y] := Eau;
      //Affiche(True, True, False);
    end;
    Affiche(True, True, False);
    if (Ecran[X,Y] = Vide) or (Ecran[X,Y] = Tresor) then AGagne := True;
    TerminerSiGagne;
    exit;
  end;
  if Ecran[X, Y] in Fleches then
  begin
    OnKeyDown := nil;
    Application.ProcessMessages;
    OnKeyDown := FormKeyDown;
    if EstDansTourniquet = 0 then Sleep(Temporisation);
    case Ecran[X, Y] of
      Nord  : Direction := Haut;
      Est   : Direction := Droite;
      Sud   : Direction := Bas;
      Ouest : Direction := Gauche;
      MoulinDirect   :
        begin
         inc(EstDansTourniquet);
         if EstDansTourniquet = 1 then AugmenteDirection else DiminueDirection;
        end;
      MoulinIndirect :
        begin
         inc(EstDansTourniquet);
         if EstDansTourniquet = 1 then DiminueDirection else AugmenteDirection;
        end;
    end;
    TouchePressee := False;
    goto OnRCommence;
  end;
  if Ecran[X, Y] in Teleportent then
  begin
    OnKeyDown := nil;
    Application.ProcessMessages;
    OnKeyDown := FormKeyDown;
    if (Ecran[X,Y] in Teleporteur) and (not (Ecran[X,Y] in EpreuvesPerso) or Labyrinthe.Continuer) then //4.2.2
    begin
      AvantCaseActive;
      No := Ecran[X,Y]-96;
      Labyrinthe.Poursuivre(Labyrinthe.StyleBouton(Ecran[X,Y]) = sDirection);
      Labyrinthe.ExecuteTeleporteur(No, Point3D);
      ApresCaseActive(False, False, True);
      X := LaPosition.X;
      Y := LaPosition.Y;
      if Ecran[X, Y] in Barque then
      begin
        NoBarque := Ecran[X, Y]-192;
        Ecran[X, Y] := Eau;
        //Affiche(True, True, False);
      end;
      Affiche(True, True, False);
      TerminerSiGagne;
      if Labyrinthe.ActiveCaseSuivante then
      begin
        if (not Labyrinthe.Attendu) then Sleep(Temporisation);
        TouchePressee := False;
        goto OnRCommence;
      end;
      exit;
    end;
    Sleep(Temporisation);
    inc(X, LaCase.X*7);
    inc(Y, LaCase.Y*7);
    ChangeDeZone := False;
    if Ecran[LaPosition.X, LaPosition.Y] = EscalierMontant then
    begin
      if (Labyrinthe[X, Y, LaCase.Z+1] = EscalierDescendant) or
         (Labyrinthe[X, Y, LaCase.Z+1] = EscalierMontant)    then
      begin
        inc(LaCase.Z);
        ChangeDeZone := True;
      end;
    end else
    if Ecran[LaPosition.X, LaPosition.Y] = EscalierDescendant then
    begin
      if (Labyrinthe[X, Y, LaCase.Z-1] = EscalierMontant)    or
         (Labyrinthe[X, Y, LaCase.Z-1] = EscalierDescendant) then
      begin
        dec(LaCase.Z);
        ChangeDeZone := True;
      end;
    end else
    begin
      _Z := LaCase.Z;
      Suivant(X, Y, LaCase.Z);
      ChangeDeZone := LaCase.Z <> _Z;
    end;
    LaCase.X     := X div 7; LaCase.Y     := Y div 7;
    LaPosition.X := X mod 7; LaPosition.Y := Y mod 7;
    if ChangeDeZone and Labyrinthe.ActionsZoneExists(LaCase.X, LaCase.Y, LaCase.Z) then
    begin
      X := LaPosition.X;
      Y := LaPosition.Y;
      AvantCaseActive;
      Labyrinthe.ExecuteChangeDeZone(Point3D, LaCase.X, LaCase.Y, LaCase.Z);
      ApresCaseActive(False, False, True);
    end;
    Affiche(True, True, False);
    exit;
  end; //Teleportent
  if Ecran[X, Y] = Ascenseur then
  begin
    OnKeyDown := nil;
    Application.ProcessMessages;
    Sleep(Temporisation);
    Affiche(False, True, False);
    _Z := LaCase.Z;
    BougeAscenseur(LaCase.X*7+X, LaCase.Y*7+Y, LaCase.Z);
    if (LaCase.Z <> _Z) and Labyrinthe.ActionsZoneExists(LaCase.X, LaCase.Y, LaCase.Z) then
    begin
      AvantCaseActive;
      Labyrinthe.ExecuteChangeDeZone(Point3D, LaCase.X, LaCase.Y, LaCase.Z);
      ApresCaseActive(False, False, True);
    end;
    Affiche(False, True, False);
    Application.ProcessMessages;
    Sleep(Temporisation);
    Affiche(True, True, False);
    OnKeyDown := FormKeyDown;
    exit;
  end;  //Ascenseur
  if (Ecran[X, Y] in Bouton)  and (not (Ecran[X,Y] in EpreuvesPerso) or Labyrinthe.Continuer) then //4.2.2
  begin
    OnKeyDown := nil;
    Application.ProcessMessages;
    OnKeyDown := FormKeyDown;
    AvantCaseActive;
    No := CodeToNoBouton(Ecran[X,Y]);
    Labyrinthe.Poursuivre(Labyrinthe.StyleBouton(Ecran[X,Y]) = sDirection);
    Labyrinthe.ExecuteBouton(No, Point3D);
    ApresCaseActive(False, False, True);
    X := LaPosition.X;
    Y := LaPosition.Y;
    if Ecran[X, Y] in Barque then
    begin
      NoBarque := Ecran[X, Y]-192;
      Ecran[X, Y] := Eau;
      //Affiche(True, True, False);
    end;
    Affiche(True, True, False);
    TerminerSiGagne;
    if Labyrinthe.ActiveCaseSuivante then
    begin
      if (not Labyrinthe.Attendu) then Sleep(Temporisation);
      TouchePressee := False;
      goto OnRCommence;
    end;
    exit;
  end; //Bouton
  if Ecran[X, Y] in Barque then
  begin
    NoBarque := Ecran[X, Y]-192;
    Ecran[X, Y] := Eau;
    Affiche(True, True, False);
  end;
  if not (Ecran[X, Y] in Objets) then exit;
  ShowMes('Message', MessageTrouve(Ecran[X, Y]), MB_OK or MB_ICONINFORMATION);
  TrouveObjet(Ecran[X, Y]);
  BarreEtat;
  Ecran[X, Y] := Herbe;
  Affiche(True, True, False);
end;

procedure TFormPrincipale.MenuDescriptionClick(Sender: TObject);
var I : integer;
    Str : string;
begin
  I := 0;
  while I < Labyrinthe.Description.Count do
  begin
    Str := Str+#13+#10+Labyrinthe.Description[I];
    inc(I);
  end;
  Delete(Str, 1, 2);
  if Labyrinthe.Description.Count = 0
  then ShowMes('Description', '(Aucune description.)', MB_OK or MB_ICONINFORMATION)
  else ShowMes('Description', Str, MB_OK or MB_ICONINFORMATION);
end;

procedure TFormPrincipale.MenuPropLabyrintheClick(Sender: TObject);
begin
  FormProprietes.AfficheLabyrinthe;
end;

procedure TFormPrincipale.MenuPropJoueurClick(Sender: TObject);
begin
  FormProprietes.AfficheJoueur;
end;

procedure TFormPrincipale.MenuIndicesClick(Sender: TObject);
begin
  MenuIndices.Checked := not MenuIndices.Checked;
end;

procedure TFormPrincipale.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if TestActions then
    ShellExecute(0, nil, PChar(Dir+'EditActions.exe'),
      PChar('"'+Labyrinthe.FileName+'"'), nil, SW_SHOWNORMAL);
end;

procedure TFormPrincipale.BarreEtat;
begin
  Barre.Panels[0].Text := IntToStr(Bouees) + ' bouée(s)';
  Barre.Panels[1].Text := IntToStr(Planches) + ' planche(s)';
  Barre.Panels[2].Text := IntToStr(ClesArgent) + ' clé(s) d''argent';
  Barre.Panels[3].Text := IntToStr(ClesOr) + ' clé(s) d''or';
end;

procedure TFormPrincipale.MenuRecommencerClick(Sender: TObject);
begin
  Commencer(NomDuFichier);
end;

procedure TFormPrincipale.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if PartieEnCours then
  case ShowDialog('Quitter FunLabyrinthe',
                  'Voulez-vous enregistrer la partie en cours ?',
                  dtConfirmation, dbYesNoCancel) of
    drYes : CanClose := SaveCurrentGame;
    drCancel : CanClose := False;
  end;
end;

end.
