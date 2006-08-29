unit EditLabyrintheMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, Menus, LabyrintheUtils, StdCtrls, Spin, SdDialogs, SvImages,
  ScUtils;

type
  TFormPrincipale = class(TForm)
    BigMenu: TMainMenu;
    MenuFichier: TMenuItem;
    MenuQuitter: TMenuItem;
    Image: TImage;
    Ouvrir: TOpenDialog;
    MenuOuvrir: TMenuItem;
    Sauver: TSaveDialog;
    MenuEnregistrer: TMenuItem;
    MenuNouveau: TMenuItem;
    Horizontal: TScrollBar;
    Vertical: TScrollBar;
    Etage: TSpinButton;
    LabelEtage: TLabel;
    EditEtage: TSpinEdit;
    MenuFermer: TMenuItem;
    MenuEnregSous: TMenuItem;
    MenuAide: TMenuItem;
    MenuRubrAide: TMenuItem;
    Sep2: TMenuItem;
    MenuAPropos: TMenuItem;
    Sep1: TMenuItem;
    MenuDescription: TMenuItem;
    MenuBoutons: TMenuItem;
    MenuEdition: TMenuItem;
    MenuNom: TMenuItem;
    N1: TMenuItem;
    ImageHerbe: TSvDropImage;
    ImageEau: TSvDropImage;
    ImageMur: TSvDropImage;
    ImageTrou: TSvDropImage;
    ImageBlocArgent: TSvDropImage;
    ImageBlocOr: TSvDropImage;
    ImageNord: TSvDropImage;
    ImageEst: TSvDropImage;
    ImageSud: TSvDropImage;
    ImageOuest: TSvDropImage;
    ImageCarrefour: TSvDropImage;
    ImageTeleporteur: TSvDropImage;
    ImageMoulinDirect: TSvDropImage;
    ImageMoulinIndirect: TSvDropImage;
    ImageEscalierM: TSvDropImage;
    ImageEscalierD: TSvDropImage;
    ImageAscenseur: TSvDropImage;
    ImageBouton: TSvDropImage;
    ImageBoutonEnfonce: TSvDropImage;
    ImageBouee: TSvDropImage;
    ImagePlanche: TSvDropImage;
    ImageCleArgent: TSvDropImage;
    ImageCleOr: TSvDropImage;
    ImageFauxMur: TSvDropImage;
    ImageBarque: TSvDropImage;
    ImageDepart: TSvDropImage;
    ImageTresor: TSvDropImage;
    AboutDialog: TSdAboutDialog;
    procedure MenuQuitterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuOuvrirClick(Sender: TObject);
    procedure MenuNouveauClick(Sender: TObject);
    procedure EtageDownClick(Sender: TObject);
    procedure EtageUpClick(Sender: TObject);
    procedure HorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MenuEnregistrerClick(Sender: TObject);
    procedure PoseImage(Sender: TObject; X, Y: Integer);
    procedure EditEtageChange(Sender: TObject);
    procedure MenuFermerClick(Sender: TObject);
    procedure MenuEnregSousClick(Sender: TObject);
    procedure MenuRubrAideClick(Sender: TObject);
    procedure MenuAProposClick(Sender: TObject);
    procedure MenuDescriptionClick(Sender: TObject);
    procedure MenuBoutonsClick(Sender: TObject);
    procedure MenuNomClick(Sender: TObject);
  private
    { Déclarations privées }
    Images : TList;
    function GetEcran(X, Y : integer) : integer;
    procedure SetEcran(X, Y, SC : integer);
  public
    { Déclarations publiques }
    Modifie : boolean;
    NomDuFichier : string;
    procedure LOuvrir(FileName : string);
    procedure Affiche;
    function Enregistrer(FileName : string) : boolean;
    property Ecran[X, Y : integer] : integer read GetEcran write SetEcran;
  end;

const
  BonsChars = ['A'..'Z', 'a'..'z', '0'..'9'];

var
  FormPrincipale : TFormPrincipale;
  Labyrinthe : TLabyrinthe;
  LaCase : T3DPoint;

function GetRect(X, Y : integer) : TRect;
function Montant(X, Y, Z : integer) : boolean;
function CaseToStr(SC : integer) : string;

implementation

uses NewLabDialog, NewLiftDialog, DescriptionDialog, ButtonActions,
  ScrewNumberDialog;

{$R *.DFM}

////////////////////////////////////////
/// Procédures et fonctions globales ///
////////////////////////////////////////

function GetRect(X, Y : integer) : TRect;
begin
  Result := Rect(X*30, Y*30, X*30+30, Y*30+30);
end;

function Montant(X, Y, Z : integer) : boolean;
var Anc, AncX, AncY, AncZ,
    Cols, Rows, Etas : integer;
begin
  Cols := Labyrinthe.Dimensions.Colonnes;
  Rows := Labyrinthe.Dimensions.Lignes;
  Etas := Labyrinthe.Dimensions.Etages;
  AncX := X;
  AncY := Y;
  AncZ := Z;
  Anc := Labyrinthe[AncX, AncY, AncZ];
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
        if Z = Etas+1 then Z := 1;
      end;
    end;
  until Labyrinthe[X, Y, Z] = Anc;
  Result := True;
  if Z > AncZ then exit;
  if (Z = AncZ) and (Y > AncY) then exit;
  if (Z = AncZ) and (Y = AncY) and (X >= AncX) then exit;
  Result := False;
end;

function CaseToStr(SC : integer) : string;
begin
  case SC of
    Herbe          : Result := 'Herbe';
    Eau            : Result := 'Eau';
    Mur            : Result := 'Mur';
    Trou           : Result := 'Trou';
    BlocArgent     : Result := 'Bloc en argent';
    BlocOr         : Result := 'Bloc en or';
    Nord           : Result := 'Flèche nord';
    Est            : Result := 'Flèche est';
    Sud            : Result := 'Flèche sud';
    Ouest          : Result := 'Flèche ouest';
    Carrefour      : Result := 'Carrefour';
    MoulinDirect   : Result := 'Tourniquet Direct';
    MoulinIndirect : Result := 'Tourniquet Indirect';
    FauxMur        : Result := 'Passage secret';
    193..202       : Result := 'Barque n° '+IntToStr(SC-192);
    TeleporteurI   : Result := 'Téléporteur inactif';
    97..126        : Result := 'Téléporteur n° '+IntToStr(SC-96);
    3              : Result := 'Téléporteur';
    71..90         : Result := 'Escalier';
    EscalierM, EscalierMontant    : Result := 'Escalier montant';
    EscalierD, EscalierDescendant : Result := 'Escalier descendant';
    Ascenseur     : Result := 'Ascenseur';
    33..47        : Result := 'Bouton n° '+IntToStr(SC-32);
    161..190      : Result := 'Bouton n° '+IntToStr(SC-145);
    4             : Result := 'Bouton';
    BoutonEnfonce : Result := 'Bouton enfoncé';
    Depart        : Result := 'Départ';
    Vide          : Result := 'Sortie';
    Ciel          : Result := 'Ciel';
    Tresor        : Result := 'Trésor';
    Bouee         : Result := 'Bouée';
    Planche       : Result := 'Planche';
    CleArgent     : Result := 'Clé en argent';
    CleOr         : Result := 'Clé en or';
    else Result := 'Case inconnue';
  end;
end;

//////////////////////////////////////////////////
/// Procédures et fonctions de TFormPrincipale ///
//////////////////////////////////////////////////

function TFormPrincipale.GetEcran(X, Y : integer) : integer;
begin
  Result := Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z];
  if Result in Escalier then
  begin
    if Montant(LaCase.X+X, LaCase.Y+Y, LaCase.Z)
    then Result := 1
    else Result := 2;
  end;
end;

procedure TFormPrincipale.SetEcran(X, Y, SC : integer);
begin
  if Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z] = SC then exit;
  Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z] := SC;
end;

procedure TFormPrincipale.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPrincipale.FormCreate(Sender: TObject);
var B, Pion : TBitmap;
    I : integer;
begin
  Application.HelpFile := Dir+'Labyrinthe.hlp';
  Menu := BigMenu;
  ClientHeight := 228;
  Modifie := False;
  NomDuFichier := '';
  B := TBitmap.Create;
  B.Height := 210;
  B.Width := 210;
  with B.Canvas do
  begin
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Rectangle(0, 0, 210, 210);
  end;
  Image.Picture.Assign(B);
  B.Free;
  Images := TList.Create;
  Images.Add(ImageHerbe);
  Images.Add(ImageEau);
  Images.Add(ImageMur);
  Images.Add(ImageTrou);
  Images.Add(ImageBlocArgent);
  Images.Add(ImageBlocOr);
  Images.Add(ImageNord);
  Images.Add(ImageEst);
  Images.Add(ImageSud);
  Images.Add(ImageOuest);
  Images.Add(ImageCarrefour);
  Images.Add(ImageMoulinDirect);
  Images.Add(ImageMoulinIndirect);
  Images.Add(ImageTeleporteur);
  Images.Add(ImageEscalierM);
  Images.Add(ImageEscalierD);
  Images.Add(ImageAscenseur);
  Images.Add(ImageBouton);
  Images.Add(ImageBoutonEnfonce);
  Images.Add(ImageBouee);
  Images.Add(ImagePlanche);
  Images.Add(ImageCleArgent);
  Images.Add(ImageCleOr);
  Images.Add(ImageFauxMur);
  Images.Add(ImageBarque);
  Images.Add(ImageDepart);
  Images.Add(ImageTresor);
  B := TBitmap.Create;
  B.Height := 30;
  B.Width  := 30;
  for I := 0 to Images.Count-1 do
  begin
    if TSvDropImage(Images[I]).Tag = Depart then
    begin
      Pion := TBitmap.Create;
      Pion.Height := 30; Pion.Width := 30;
      with Pion.Canvas do
      begin
        CopyRect(Rect(0, 0, 30, 30), Cases.Canvas, CaseRect(Depart, sPasBouton));
        Brush.Color := clBlue; Pen.Color := clBlue;
        Ellipse(6, 6, 24, 24);
      end;
      B.Canvas.CopyRect(Rect(0, 0, 30, 30), Pion.Canvas, Rect(0, 0, 30, 30));
      Pion.Free;
    end else B.Canvas.CopyRect(Rect(0, 0, 30, 30), Cases.Canvas, CaseRect(TSvDropImage(Images[I]).Tag, sPasBouton));
    if TSvDropImage(Images[I]).Tag = FauxMur then with B.Canvas do
    begin
      Brush.Color := clWhite;
      Font.Color := clBlack;
      Font.Size := 12;
      Font.Style := [fsBold];
      Font.Name := 'Courier';
      TextOut(10, 8, '!');
      Font.Style := [];
    end;
    TSvDropImage(Images[I]).Picture.Assign(B);
  end;
  B.Free;
  if ParamCount > 0 then LOuvrir(ParamStr(1));
end;

procedure TFormPrincipale.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not Modifie then exit;
  case ShowMes(Application.Title, 'Voulez-vous enregistrer les modifications'+#10+
                  'apportées à '+Labyrinthe.NomLab+' ?',
                  MB_YESNOCANCEL or MB_ICONQUESTION) of
    IDYES    : if not Enregistrer(NomDuFichier) then CanClose := False;
    IDCANCEL : CanClose := False;
  end;
  if CanClose then Images.Free;
end;

procedure TFormPrincipale.LOuvrir(FileName : string);
var I : integer;
begin
  if not FileExists(FileName) then exit;
  try
    Labyrinthe := TLabyrinthe.CreateOpen(FileName);
  except
    on E : EIsSauvegarde do
    begin
      ShowMes('Sauvegarde', E.Message, MB_OK or MB_ICONERROR);
      exit;
    end;
  end;
  if not Labyrinthe.FileInfos.PeutEditer then
  begin
    Labyrinthe.Free;
    ShowMes('Erreur', 'Impossible d''éditer ce labyrinthe'+#10+
               'avec cet éditeur.', MB_OK or MB_ICONERROR);
    exit;
  end;
  NomDuFichier := FileName;
  MenuNouveau.Enabled := False;
  MenuOuvrir.Enabled := False;
  MenuEnregistrer.Enabled := True;
  MenuEnregSous.Enabled := True;
  MenuFermer.Enabled := True;
  MenuEdition.Visible := True;
  Image.Enabled := True;
  if Labyrinthe.Dimensions.Etages > 1 then
  begin
    Etage.Enabled := True;
    EditEtage.Enabled := True;
    EditEtage.MaxValue := Labyrinthe.Dimensions.Etages;
  end;
  for I := 0 to Images.Count-1 do TSvDropImage(Images[I]).Enabled := True;
  if Labyrinthe.Dimensions.Colonnes > 1 then
  begin
    Horizontal.Max := (Labyrinthe.Dimensions.Colonnes-1)*7;
    Horizontal.Enabled := True;
  end;
  if Labyrinthe.Dimensions.Lignes > 1 then
  begin
    Vertical.Max := (Labyrinthe.Dimensions.Lignes-1)*7;
    Vertical.Enabled := True;
  end;
  LaCase.X := 0;
  LaCase.Y := 0;
  LaCase.Z := 1;
  Caption := Application.Title+' - ['+Labyrinthe.NomLab+']';
  Modifie := False;
  Affiche;
end;

procedure TFormPrincipale.Affiche;
var X, Y, N : integer;
    Img : TBitmap;
    StBouton : TStyleBouton;
begin
  Img := TBitmap.Create;
  Img.Height := Image.Picture.Bitmap.Height;
  Img.Width  := Image.Picture.Bitmap.Width;
  with Img.Canvas do
  begin
    for X := 0 to 6 do for Y := 0 to 6 do
    begin
      StBouton := Labyrinthe.StyleBouton(Ecran[X,Y]);
      if (StBouton in StylesPerso) then
        CopyRect(GetRect(X, Y), Labyrinthe.CodeToImage(Ecran[X, Y]).Canvas, Rect(0, 0, 30, 30)) else
        CopyRect(GetRect(X, Y), Cases.Canvas, CaseRect(Ecran[X, Y], Labyrinthe.StyleBouton(Ecran[X, Y])));
      if Ecran[X, Y] = Depart then
      begin
        Brush.Color := clBlue;
        Pen.Color := clBlue;
        Ellipse(X*30+6, Y*30+6, X*30+24, Y*30+24);
      end;
      if Ecran[X, Y] in Bouton then
      begin
        Brush.Color := clWhite;
        Font.Color := clBlack;
        Font.Size := 8;
        Font.Name := 'Arial';
        N := Ecran[X, Y];
        if N <= 127 then dec(N, 32) else dec(N, 145);
        TextOut(X*30+9, Y*30+9, IntToStr0(N, 2));
      end;
      if Ecran[X, Y] in Teleporteur then
      begin
        Brush.Color := clWhite;
        Font.Color := clRed;
        Font.Size := 8;
        Font.Name := 'Arial';
        TextOut(X*30+9, Y*30+9, IntToStr0(Ecran[X, Y]-96, 2));
      end;
      if Ecran[X, Y] in Barque then
      begin
        Brush.Color := clWhite;
        Font.Color := clNavy;
        Font.Size := 8;
        Font.Name := 'Arial';
        TextOut(X*30+9, Y*30+9, IntToStr0(Ecran[X, Y]-192, 2));
      end;
      if Ecran[X, Y] = FauxMur then
      begin
        Brush.Color := clWhite;
        Font.Color := clBlack;
        Font.Size := 12;
        Font.Style := [fsBold];
        Font.Name := 'Courier';
        TextOut(X*30+10, Y*30+8, '!');
        Font.Style := [];
      end;
    end;
  end;
  Image.Picture.Bitmap.Assign(Img);
  Img.Free;
  EditEtage.Value := LaCase.Z;
end;

procedure TFormPrincipale.MenuOuvrirClick(Sender: TObject);
begin
  {$I-}
    MkDir(Dir+'Labyrinthes');
  {$I+}
  Ouvrir.FileName := '';
  Ouvrir.InitialDir := Dir+'Labyrinthes';
  if not Ouvrir.Execute then exit;
  LOuvrir(Ouvrir.FileName);
end;

procedure TFormPrincipale.MenuNouveauClick(Sender: TObject);
var I : integer;
begin
  with FormNouveau do
  begin
    if not QueryInfos then exit;
    Labyrinthe := TLabyrinthe.CreateNew(EditColonnes.Value,
                  EditLignes.Value, EditEtages.Value, Perso);
  end;
  NomDuFichier := '';
  MenuNouveau.Enabled := False;
  MenuOuvrir.Enabled := False;
  MenuEnregistrer.Enabled := True;
  MenuEnregSous.Enabled := True;
  MenuFermer.Enabled := True;
  MenuEdition.Visible := True;
  Image.Enabled := True;
  if Labyrinthe.Dimensions.Etages > 1 then
  begin
    Etage.Enabled := True;
    EditEtage.Enabled := True;
    EditEtage.MaxValue := Labyrinthe.Dimensions.Etages;
  end;
  for I := 0 to Images.Count-1 do TSvDropImage(Images[I]).Enabled := True;
  if Labyrinthe.Dimensions.Colonnes > 1 then
  begin
    Horizontal.Max := (Labyrinthe.Dimensions.Colonnes-1)*7;
    Horizontal.Enabled := True;
  end;
  if Labyrinthe.Dimensions.Lignes > 1 then
  begin
    Vertical.Max := (Labyrinthe.Dimensions.Lignes-1)*7;
    Vertical.Enabled := True;
  end;
  LaCase.X := 0;
  LaCase.Y := 0;
  LaCase.Z := 1;
  Caption := Application.Title+' - ['+Labyrinthe.NomLab+']';
  Modifie := True;
  Affiche;
end;

procedure TFormPrincipale.EtageDownClick(Sender: TObject);
begin
  if LaCase.Z > 1 then
  begin
    dec(LaCase.Z);
    Affiche;
  end;
end;

procedure TFormPrincipale.EtageUpClick(Sender: TObject);
begin
  if LaCase.Z < Labyrinthe.Dimensions.Etages then
  begin
    inc(LaCase.Z);
    Affiche;
  end;
end;

procedure TFormPrincipale.HorizontalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp, scLineDown, scEndScroll : ;
    scPageUp :
    begin
      ScrollPos := LaCase.X - LaCase.X mod 7;
      if ScrollPos = LaCase.X then dec(ScrollPos, 7);
    end;
    scPageDown : ScrollPos := LaCase.X - LaCase.X mod 7 + 7;
    else exit;
  end;
  LaCase.X := ScrollPos;
  Affiche;
end;

procedure TFormPrincipale.VerticalScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp, scLineDown, scEndScroll : ;
    scPageUp :
    begin
      ScrollPos := LaCase.Y - LaCase.Y mod 7;
      if ScrollPos = LaCase.Y then dec(ScrollPos, 7);
    end;
    scPageDown : ScrollPos := LaCase.Y - LaCase.Y mod 7 + 7;
    else exit;
  end;
  LaCase.Y := ScrollPos;
  Affiche;
end;

function TFormPrincipale.Enregistrer(FileName : string) : boolean;
var X, Y, Z : integer;
    OK : boolean;
label Init;
begin
  Ok := False;
  for X := 0 to Labyrinthe.Dimensions.Colonnes*7 - 1 do
    for Y := 0 to Labyrinthe.Dimensions.Lignes*7 - 1 do
      for Z := 1 to Labyrinthe.Dimensions.Etages do
        if Labyrinthe[X, Y, Z] = Depart then OK := True;
  if not OK then
  begin
    ShowMes('Pas de départ', 'Sans départ, impossible d''enregistrer.',
               MB_OK or MB_ICONERROR);
    Result := False;
    exit;
  end;
  if (FileName = '') or ((FileGetAttr(FileName) and faReadOnly) <> 0) then
  begin
Init :
    {$I-}
      MkDir(Dir+'Labyrinthes');
    {$I+}
    Sauver.InitialDir := Dir+'Labyrinthes';
    Sauver.FileName := Labyrinthe.FileName;
    if not Sauver.Execute then
    begin
      Result := False;
      exit;
    end;
    if FileExists(Sauver.FileName) and
       (FileGetAttr(Sauver.FileName) and faReadOnly <> 0) then
    begin
      ShowMes('Lecture seule', 'Ce fichier est en lecture seule.',
              MB_OK or MB_ICONERROR);
      goto Init;
    end;
  end else Sauver.FileName := FileName;
  Labyrinthe.Enregistrer(Sauver.FileName);
  Modifie := False;
  NomDuFichier := Sauver.FileName;
  Caption := Application.Title+' - ['+Labyrinthe.NomLab+']';
  Result := True;
end;

procedure TFormPrincipale.MenuEnregistrerClick(Sender: TObject);
begin
  Enregistrer(NomDuFichier);
end;

procedure TFormPrincipale.PoseImage(Sender: TObject; X, Y: Integer);
var I, SC, SCase, _X, _Y, _Z, Debut, Fin : integer;
    Bool : boolean;
const Escaliers = [EscalierM, EscalierD, EscalierMontant, EscalierDescendant];
begin
  X := X div 30;
  Y := Y div 30;
  SC := (Sender as TSvDropImage).Tag;
  if SC = Ecran[X, Y] then exit;
  if (SC = EscalierMontant) and (LaCase.Z = Labyrinthe.Dimensions.Etages) then
  begin
    ShowMes('Impossible', 'Impossible de placer un escalier'+#10+
            'montant au dernier étage !', MB_OK or MB_ICONERROR);
    exit;
  end;
  if (SC = EscalierDescendant) and (LaCase.Z = 1) then
  begin
    ShowMes('Impossible', 'Impossible de placer un escalier'+#10+
            'descendant au premier étage !', MB_OK or MB_ICONERROR);
    exit;
  end;
  if SC = EscalierMontant then
  begin
    SCase := Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z+1];
    if (SCase = EscalierM) or (SCase = EscalierMontant) then
    begin
      ShowMes('Impossible', 'Escalier montant écrasé à l''étage'+#10+
              'supérieur. Impossible !', MB_OK or MB_ICONERROR);
      exit;
    end;
    if (SCase <> Herbe) and (SCase <> Mur) then
    begin
      if ShowMes(CaseToStr(SCase)+' écrasé(e)',
                 'Attention ! '+CaseToStr(SCase)+' écrasé(e)'+#10+
                 'à l''étage supérieur.'+#10+
                 'Voulez-vous vraiment placer cet escalier ?',
                 MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2)
                 = IDNO then exit;
    end;
  end else if SC = EscalierDescendant then
  begin
    SCase := Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z-1];
    if (SCase = EscalierD) or (SCase = EscalierDescendant) then
    begin
      ShowMes('Impossible', 'Escalier descendant écrasé à l''étage'+#10+
              'inférieur. Impossible !', MB_OK or MB_ICONERROR);
      exit;
    end;
    if (SCase <> Herbe) and (SCase <> Mur) then
    begin
      if ShowMes(CaseToStr(SCase)+' écrasé(e)',
                 'Attention ! '+CaseToStr(SCase)+' écrasé(e)'+#10+
                 'à l''étage inférieur.'+#10+
                 'Voulez-vous vraiment placer cet escalier ?',
                 MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2)
                 = IDNO then exit;
    end;
  end;
  if (Ecran[X, Y] = EscalierM) or (Ecran[X, Y] = EscalierMontant) then
    Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z+1] := Herbe;
  if (Ecran[X, Y] = EscalierD) or (Ecran[X, Y] = EscalierDescendant) then
    Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z-1] := Herbe;
  if (Ecran[X, Y] = Ascenseur) and
     (ShowMes('Destruction de la cage d''ascenseur',
              'Voulez-vous détruire toute'+#10+
              'la cage d''ascenseur ?',
              MB_YESNO or MB_ICONQUESTION) = IDYES) then
  begin
    _Z := LaCase.Z-1;
    while Labyrinthe[LaCase.X+X, LaCase.Y+Y, _Z] = Ascenseur do
    begin
      Labyrinthe[LaCase.X+X, LaCase.Y+Y, _Z] := Herbe;
      dec(_Z);
    end;
    _Z := LaCase.Z+1;
    while Labyrinthe[LaCase.X+X, LaCase.Y+Y, _Z] = Ascenseur do
    begin
      Labyrinthe[LaCase.X+X, LaCase.Y+Y, _Z] := Herbe;
      inc(_Z);
    end;
  end;
  if SC = EscalierMontant then
  begin
    Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z+1] := EscalierDescendant;
  end;
  if SC = EscalierDescendant then
  begin
    Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z-1] := EscalierMontant;
  end;
  if SC = 3 then
  begin
    SC := 96+FormNumeroCase.GetTeleporterNumber;
  end else if SC = 4 then
  begin
    SC := FormNumeroCase.GetButtonNumber;
    if (SC <= 15) then inc(SC, 32) else inc(SC, 145);
  end else if SC = 193 then
  begin
    SC := 192+FormNumeroCase.GetBoatNumber;
  end else if SC = Ascenseur then
  begin
    FormAscenseur.GetDebutFin(Debut, Fin, LaCase.Z, Labyrinthe.Dimensions.Etages);
    Bool := False;
    for I := Debut to Fin do if (I <> LaCase.Z) and
        (Labyrinthe[LaCase.X+X, LaCase.Y+Y, I] <> Herbe) and
        (Labyrinthe[LaCase.X+X, LaCase.Y+Y, I] <> Mur) and
        (Labyrinthe[LaCase.X+X, LaCase.Y+Y, I] <> Ascenseur) then
          Bool := True;
    if Bool and (ShowMes('Case(s) écrasée(s)',
                 'Attention ! Une ou plusieurs case(s)'+#10+
                 'spéciale(s) va (vont) être écrasée(s) !'+#10+
                 'Voulez-vous vraiment placer ces ascenseurs ?',
                 MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2)
                 = IDNO) then exit;
    for I := Debut to Fin do
      Labyrinthe[LaCase.X+X, LaCase.Y+Y, I] := Ascenseur;
  end else if SC = Depart then
  begin
    for _X := 0 to Labyrinthe.Dimensions.Colonnes*7 - 1 do
      for _Y := 0 to Labyrinthe.Dimensions.Lignes*7 - 1 do
        for _Z := 1 to Labyrinthe.Dimensions.Etages do
          if Labyrinthe[_X, _Y, _Z] = Depart then Labyrinthe[_X, _Y, _Z] := Herbe;
  end;
  Ecran[X, Y] := SC;
  Modifie := True;
  Affiche;
end;

procedure TFormPrincipale.EditEtageChange(Sender: TObject);
begin
  LaCase.Z := EditEtage.Value;
  Affiche;
end;

procedure TFormPrincipale.MenuFermerClick(Sender: TObject);
var I : integer;
    B : TBitmap;
begin
  if Modifie then
  case ShowMes(Application.Title, 'Voulez-vous enregistrer les modifications'+#10+
                  'apportées à '+Labyrinthe.NomLab+' ?',
                  MB_YESNOCANCEL or MB_ICONQUESTION) of
    IDYES    : if not Enregistrer(NomDuFichier) then exit;
    IDCANCEL : exit;
    end;
  NomDuFichier := '';
  Labyrinthe.Free;
  for I := 0 to Images.Count - 1 do
    TSvDropImage(Images[I]).Enabled := False;
  EditEtage.OnChange := nil;
  EditEtage.Value := 1;
  EditEtage.OnChange := EditEtageChange;
  EditEtage.Enabled:= False;
  Etage.Enabled := False;
  Horizontal.Position := 0;
  Horizontal.Max := 0;
  Horizontal.Enabled := False;
  Vertical.Position := 0;
  Vertical.Max := 0;
  Vertical.Enabled := False;
  Image.Enabled := False;
  MenuNouveau.Enabled := True;
  MenuOuvrir.Enabled := True;
  MenuEnregistrer.Enabled := False;
  MenuEnregSous.Enabled := False;
  MenuFermer.Enabled := False;
  MenuEdition.Visible := False;
  Modifie := False;
  B := TBitmap.Create;
  B.Height := 210;
  B.Width := 210;
  with B.Canvas do
  begin
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Rectangle(0, 0, 210, 210);
  end;
  Image.Picture.Assign(B);
  B.Free;
  Caption := Application.Title;
end;

procedure TFormPrincipale.MenuEnregSousClick(Sender: TObject);
begin
  Enregistrer('');
end;

procedure TFormPrincipale.MenuRubrAideClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormPrincipale.MenuAProposClick(Sender: TObject);
begin
  AboutDialog.Execute;
end;

procedure TFormPrincipale.MenuDescriptionClick(Sender: TObject);
begin
  with FormDescription do
  begin
    MemoDescription.Lines.Assign(Labyrinthe.Description);
    ActiveControl := MemoDescription;
    if ShowModal = mrOK then
    begin
      Labyrinthe.Description.Assign(MemoDescription.Lines);
      Modifie := True;
    end;
  end;
end;

procedure TFormPrincipale.MenuBoutonsClick(Sender: TObject);
begin
  FormModifieBoutons.EditBoutons;
  Affiche;
end;

procedure TFormPrincipale.MenuNomClick(Sender: TObject);
var S : String;
begin
  S := Labyrinthe.NomLab;
  if InputQuery('Nom du labyrinthe', 'Nom :', S) then
  begin
    Labyrinthe.NomLab := S;
    Modifie := True;
    Labyrinthe.NouveauNom := False;
    Caption := Application.Title+' - ['+Labyrinthe.NomLab+']';
  end;
end;

end.
